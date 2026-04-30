import AppKit
import Metal
import MetalKit
import simd

/// Bottom-anchored audio bars rendered with Metal. The previous CAShapeLayer
/// path swap was already vsync-driven, but Metal lets the bar geometry live
/// in a vertex shader (instanced quads) instead of rebuilding a CGPath on
/// the main thread every frame, and gives us a clean substrate for richer
/// visualizers later (FFT, particles, gradients) without re-architecting.
///
/// Sixteen instanced quads, accent-colored fill, opaque dark background,
/// driven by MTKView's internal CVDisplayLink at the screen's native
/// refresh. Hidden when MIDI mode is on.
final class WaveformView: MTKView {
    weak var menuBand: MenuBandController?

    private static let barCount = 16
    private static let barGapPts: Float = 3   // points; multiplied by contentsScale at draw time
    private static let snapshotSize = 256

    private var samples = [Float](repeating: 0, count: snapshotSize)
    private var smoothedPeak: Float = 0.05
    private var levels = [Float](repeating: 0, count: barCount)

    private var commandQueue: MTLCommandQueue?
    private var pipelineState: MTLRenderPipelineState?
    private var uniforms = BarUniforms()
    /// Per-bar smoothed level — gives the bars visual "ballistics" so they
    /// don't pop instantly between frames. Decay slower than rise so attack
    /// transients punch but releases trail off naturally.
    private var displayLevels = [Float](repeating: 0, count: barCount)
    /// Explicit display link. NSPopover hosts content in an NSPanel whose
    /// runloop coalesces setNeedsDisplay-triggered redraws, and MTKView's
    /// internal CVDisplayLink doesn't fire reliably when the panel isn't
    /// `main`. We drive draws ourselves and call `display()` so the redraw
    /// is synchronous instead of deferred.
    private var displayLink: CVDisplayLink?

    var isLive: Bool = false {
        didSet {
            if isLive {
                startLink()
            } else {
                stopLink()
                for i in 0..<levels.count { levels[i] = 0 }
                for i in 0..<displayLevels.count { displayLevels[i] = 0 }
                display()  // one final paint to clear bars
            }
        }
    }

    private func startLink() {
        stopLink()
        var link: CVDisplayLink?
        CVDisplayLinkCreateWithActiveCGDisplays(&link)
        guard let link = link else { return }
        let opaque = Unmanaged.passUnretained(self).toOpaque()
        CVDisplayLinkSetOutputCallback(link, { _, _, _, _, _, ctx in
            guard let ctx = ctx else { return kCVReturnSuccess }
            let view = Unmanaged<WaveformView>.fromOpaque(ctx).takeUnretainedValue()
            // display() is main-thread-only; hop over and draw synchronously
            // so the redraw can't be coalesced by the popover's runloop.
            DispatchQueue.main.async { view.display() }
            return kCVReturnSuccess
        }, opaque)
        CVDisplayLinkStart(link)
        displayLink = link
    }

    private func stopLink() {
        if let link = displayLink {
            CVDisplayLinkStop(link)
            displayLink = nil
        }
    }

    deinit { stopLink() }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if window == nil { stopLink() }
    }

    init() {
        guard let device = MTLCreateSystemDefaultDevice() else {
            fatalError("MenuBand: no Metal device — every Mac since 2012 has one, "
                       + "so this should not happen on macOS 11+")
        }
        super.init(frame: .zero, device: device)
        wantsLayer = true
        // Note: not using layer.cornerRadius — CAMetalLayer with
        // framebufferOnly = true can't be reliably clipped by a corner
        // mask. Square corners for the visualizer are fine; if we want
        // them rounded later, wrap in a clipping container view.

        // Opaque background — the previous 0.92 alpha was barely a tint and
        // making the layer translucent costs us framebufferOnly + complicates
        // the blend setup. Solid black reads identical at the popover scale.
        clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 1.0)
        framebufferOnly = true
        (layer as? CAMetalLayer)?.isOpaque = true

        // We drive draws via our own CVDisplayLink + display() so MTKView's
        // internal frame timing doesn't fight the popover's runloop mode.
        // `enableSetNeedsDisplay = true` puts MTKView in dirty-rect mode;
        // `isPaused = false` so display() actually paints when called.
        enableSetNeedsDisplay = true

        // Disable vsync on the Metal layer so frames are presented immediately
        // rather than waiting for the next display refresh. Without this, our
        // CVDisplayLink-driven draw calls can stall for up to one refresh
        // interval because the layer tries to sync presentation to the display,
        // creating visible latency between audio input and the rendered
        // waveform — and adding noticeable delay when the popover first
        // appears. Patch contributed by Esteban Uribe.
        if let metalLayer = layer as? CAMetalLayer {
            metalLayer.displaySyncEnabled = false
        }

        isPaused = false
        preferredFramesPerSecond = 0

        commandQueue = device.makeCommandQueue()
        buildPipeline(device: device)
        delegate = self
        applyAccentColor()
    }

    required init(coder: NSCoder) {
        fatalError("WaveformView is code-only; init(coder:) is not supported")
    }

    override var isOpaque: Bool { true }

    private func buildPipeline(device: MTLDevice) {
        do {
            let library = try device.makeLibrary(source: Self.shaderSource, options: nil)
            guard let vfn = library.makeFunction(name: "bar_vertex"),
                  let ffn = library.makeFunction(name: "bar_fragment") else {
                NSLog("MenuBand: visualizer shader functions missing")
                return
            }
            let pd = MTLRenderPipelineDescriptor()
            pd.vertexFunction = vfn
            pd.fragmentFunction = ffn
            pd.colorAttachments[0].pixelFormat = colorPixelFormat
            pipelineState = try device.makeRenderPipelineState(descriptor: pd)
        } catch {
            NSLog("MenuBand: visualizer Metal pipeline failed: \(error)")
        }
    }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        applyAccentColor()
    }

    private func applyAccentColor() {
        let c = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? NSColor.systemTeal
        uniforms.color = SIMD4<Float>(Float(c.redComponent),
                                       Float(c.greenComponent),
                                       Float(c.blueComponent),
                                       1.0)
    }

    // MARK: - Per-frame audio analysis

    private func updateLevels() {
        guard let m = menuBand else {
            for i in 0..<levels.count { levels[i] = 0 }
            for i in 0..<displayLevels.count { displayLevels[i] = 0 }
            return
        }
        m.synthSnapshotWaveform(into: &samples)

        let n = Self.barCount
        let chunkSize = samples.count / n
        var framePeak: Float = 0
        for b in 0..<n {
            var peak: Float = 0
            let base = b * chunkSize
            for i in 0..<chunkSize {
                let a = abs(samples[base + i])
                if a > peak { peak = a }
            }
            levels[b] = peak
            if peak > framePeak { framePeak = peak }
        }

        // Auto-gain — same envelope as the old CALayer path. Snap up on
        // attack, decay slowly on release so a sustained quiet note still
        // pushes bars up.
        if framePeak > smoothedPeak {
            smoothedPeak = framePeak
        } else {
            smoothedPeak = max(0.05, smoothedPeak * 0.92 + framePeak * 0.08)
        }
        let gain = 0.95 / smoothedPeak
        // Direct readout — no inter-frame ballistics. The user prefers
        // raw peak amplitude over smoothed visuals; bars track the
        // analyzed level exactly so transients land within one display
        // frame of the audio that triggered them.
        for b in 0..<n {
            displayLevels[b] = min(1.0, levels[b] * gain)
        }
    }

    // MARK: - Shader source

    private static let shaderSource = """
    #include <metal_stdlib>
    using namespace metal;

    struct Uniforms {
        float viewW;
        float viewH;
        float barW;
        float stride;
        float minHeight;
        float4 color;
    };

    struct VertexOut {
        float4 position [[position]];
    };

    // Two triangles spanning the unit square (CCW), shared across all bar
    // instances. The vertex shader scales each instance into a bar rect
    // and converts pixel space to clip space.
    constant float2 unitQuad[6] = {
        float2(0, 0), float2(1, 0), float2(0, 1),
        float2(0, 1), float2(1, 0), float2(1, 1)
    };

    vertex VertexOut bar_vertex(uint vid [[vertex_id]],
                                 uint iid [[instance_id]],
                                 constant Uniforms &u [[buffer(0)]],
                                 constant float *levels [[buffer(1)]])
    {
        float2 local = unitQuad[vid];
        float barX = float(iid) * u.stride;
        float h = max(u.minHeight, levels[iid] * u.viewH);
        float px = barX + local.x * u.barW;
        float py = local.y * h;
        // Pixel space → clip space ([-1, 1] on both axes).
        float clipX = (px / u.viewW) * 2.0 - 1.0;
        float clipY = (py / u.viewH) * 2.0 - 1.0;
        VertexOut out;
        out.position = float4(clipX, clipY, 0, 1);
        return out;
    }

    fragment float4 bar_fragment(VertexOut in [[stage_in]],
                                  constant Uniforms &u [[buffer(0)]])
    {
        return u.color;
    }
    """
}

private struct BarUniforms {
    var viewW: Float = 0
    var viewH: Float = 0
    var barW: Float = 0
    var stride: Float = 0
    var minHeight: Float = 1.5
    var color: SIMD4<Float> = SIMD4<Float>(0, 1, 1, 1)
}

extension WaveformView: MTKViewDelegate {
    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {}

    func draw(in view: MTKView) {
        updateLevels()

        let drawableSize = view.drawableSize
        let viewW = Float(drawableSize.width)
        let viewH = Float(drawableSize.height)
        let n = Self.barCount
        // Drawable size is in pixels; gap is in points, so scale up by
        // contentsScale (Retina = 2). Without this the gaps look half-width
        // on Retina displays.
        let scale = Float(layer?.contentsScale ?? 2.0)
        let gapPx = Self.barGapPts * scale
        let barW = max(1, (viewW - gapPx * Float(n - 1)) / Float(n))
        let stride = barW + gapPx

        uniforms.viewW = viewW
        uniforms.viewH = viewH
        uniforms.barW = barW
        uniforms.stride = stride
        uniforms.minHeight = 1.5 * scale

        guard let pipeline = pipelineState,
              let queue = commandQueue,
              let descriptor = view.currentRenderPassDescriptor,
              let drawable = view.currentDrawable,
              let cb = queue.makeCommandBuffer(),
              let enc = cb.makeRenderCommandEncoder(descriptor: descriptor) else {
            return
        }

        enc.setRenderPipelineState(pipeline)
        enc.setVertexBytes(&uniforms, length: MemoryLayout<BarUniforms>.size, index: 0)
        enc.setFragmentBytes(&uniforms, length: MemoryLayout<BarUniforms>.size, index: 0)
        displayLevels.withUnsafeBufferPointer { ptr in
            enc.setVertexBytes(ptr.baseAddress!,
                               length: MemoryLayout<Float>.size * n,
                               index: 1)
        }
        enc.drawPrimitives(type: .triangle,
                           vertexStart: 0,
                           vertexCount: 6,
                           instanceCount: n)
        enc.endEncoding()
        cb.present(drawable)
        cb.commit()
    }
}
