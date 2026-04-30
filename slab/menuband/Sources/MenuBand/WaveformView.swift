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
                stopDotMatrix()
                startLink()
            } else {
                stopLink()
                for i in 0..<levels.count { levels[i] = 0 }
                for i in 0..<displayLevels.count { displayLevels[i] = 0 }
                display()  // one final paint to clear bars
            }
        }
    }

    /// When true, the bars stop running the live VU and instead
    /// render the static `dotMasks` pattern (used to spell "MIDI"
    /// while in MIDI mode). Reset masks + uniform when switching
    /// out so live mode resumes cleanly.
    private var dotMatrixActive: Bool = false
    /// Per-bar 32-bit mask. Bit i = whether segment i (0=bottom,
    /// 9=top) is lit. Sized to `barCount`.
    private var dotMasks: [Float] = Array(repeating: 0, count: 32)

    /// Render a static dot-matrix pattern instead of the live VU
    /// bars. Pass nil to clear and return to live mode.
    func setDotMatrix(_ mask: [UInt32]?) {
        if let mask = mask {
            // Encode UInt32 → Float for transport (Float can hold up
            // to 2^24 exactly, and our masks are 10 bits).
            for i in 0..<dotMasks.count {
                dotMasks[i] = Float(i < mask.count ? mask[i] : 0)
            }
            uniforms.dotMatrix = 1
            dotMatrixActive = true
            // Static frame — nudge a single redraw so the pattern
            // appears even when the live link is off.
            display()
        } else {
            stopDotMatrix()
        }
    }

    private func stopDotMatrix() {
        if dotMatrixActive {
            for i in 0..<dotMasks.count { dotMasks[i] = 0 }
            uniforms.dotMatrix = 0
            dotMatrixActive = false
            display()
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
            // Alpha blending — fragment shader emits alpha < 1 for unlit
            // segments so the empty rows of the LED meter dim out instead
            // of staying full-color. Without blending, alpha is ignored
            // and every "off" segment renders at full intensity.
            pd.colorAttachments[0].isBlendingEnabled = true
            pd.colorAttachments[0].rgbBlendOperation = .add
            pd.colorAttachments[0].alphaBlendOperation = .add
            pd.colorAttachments[0].sourceRGBBlendFactor = .sourceAlpha
            pd.colorAttachments[0].sourceAlphaBlendFactor = .sourceAlpha
            pd.colorAttachments[0].destinationRGBBlendFactor = .oneMinusSourceAlpha
            pd.colorAttachments[0].destinationAlphaBlendFactor = .oneMinusSourceAlpha
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

    /// Override the visualizer's base color — used so the LED meter
    /// matches the chosen GM instrument. Top of the bar still brightens
    /// toward white in the shader (hot-zone VU feel), so passing in a
    /// dim mid-tone still reads with peak indication. Pass `nil` to
    /// revert to the system accent color.
    func setBaseColor(_ color: NSColor?) {
        guard let color = color,
              let c = color.usingColorSpace(.sRGB) else {
            applyAccentColor()
            return
        }
        uniforms.color = SIMD4<Float>(Float(c.redComponent),
                                       Float(c.greenComponent),
                                       Float(c.blueComponent),
                                       1.0)
    }

    /// Switch the meter substrate + lit-bar tonality between the
    /// glowing-on-black look (dark mode) and an ink-on-paper look
    /// (light mode). In light mode the clear color flips to a warm
    /// off-white and the shader's hot-zone mix darkens toward black
    /// instead of brightening to white, so peak still reads as
    /// "hotter" without washing out against the light substrate.
    func setLightMode(_ isLight: Bool) {
        if isLight {
            // Warm off-white — closer to a printed page than pure
            // white, so the colored bars don't vibrate against it.
            clearColor = MTLClearColor(red: 0.93, green: 0.92, blue: 0.90, alpha: 1.0)
            uniforms.isLight = 1
        } else {
            clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 1.0)
            uniforms.isLight = 0
        }
        display()
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
        // RMS per bin instead of peak. Peak detection makes bars hop
        // around as zero-crossings drift across chunk boundaries — looks
        // like the spectrum is "rolling." RMS averages within each chunk
        // so a small phase shift barely moves the value, and the bars
        // sit at their true amplitude rather than chasing transients.
        for b in 0..<n {
            var sumSq: Float = 0
            let base = b * chunkSize
            for i in 0..<chunkSize {
                let s = samples[base + i]
                sumSq += s * s
            }
            let rms = (chunkSize > 0) ? sqrtf(sumSq / Float(chunkSize)) : 0
            levels[b] = rms
            if rms > framePeak { framePeak = rms }
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
        // Per-bar temporal smoothing. RMS over a short chunk still
        // wobbles when the chunk is shorter than one period of the note
        // (low pitches especially) — a 30 Hz tone has ~1500 samples per
        // period, but each bar only sees ~32 samples. Without temporal
        // smoothing, those phase-induced wobbles drove the LED segment
        // count up and down frame to frame, reading as the meter
        // "rolling." Asymmetric smoothing (fast attack, slow decay)
        // keeps transient response while killing the ripple.
        let attack: Float = 0.55      // weight on new sample when rising
        let decay:  Float = 0.18      // weight on new sample when falling
        for b in 0..<n {
            let raw = min(1.0, levels[b] * gain)
            let prev = displayLevels[b]
            let alpha = (raw > prev) ? attack : decay
            displayLevels[b] = prev * (1.0 - alpha) + raw * alpha
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
        float dotMatrix;
        float isLight;
    };

    struct VertexOut {
        float4 position [[position]];
        float level;
        float mask;
    };

    // Two triangles spanning the unit square (CCW), shared across all bar
    // instances. The vertex shader scales each instance into a bar rect
    // and converts pixel space to clip space.
    constant float2 unitQuad[6] = {
        float2(0, 0), float2(1, 0), float2(0, 1),
        float2(0, 1), float2(1, 0), float2(1, 1)
    };

    // Segmented LED-meter look. Each bar is rendered as a full-height
    // rect; the fragment shader carves it into stacked segments by
    // level. Tweak NSEG / SEG_GAP to taste — old stereo VU meters
    // typically had 10–14 segments with a small dim row above the lit
    // ones to hint at the headroom.
    constant float NSEG = 10.0;
    constant float SEG_GAP = 0.32;
    constant float UNLIT_ALPHA = 0.12;
    // Hot-zone: above this fraction of the bar height, the lit color
    // brightens toward white so the top of the meter still reads as
    // "peaking" even when the instrument's base color is e.g. a deep
    // navy bass. Linear ramp from HOT_AT → top.
    constant float HOT_AT = 0.70;

    vertex VertexOut bar_vertex(uint vid [[vertex_id]],
                                 uint iid [[instance_id]],
                                 constant Uniforms &u [[buffer(0)]],
                                 constant float *levels [[buffer(1)]],
                                 constant float *masks  [[buffer(2)]])
    {
        float2 local = unitQuad[vid];
        float barX = float(iid) * u.stride;
        // Full-height geometry — fragment shader masks per-segment.
        float h = u.viewH;
        float px = barX + local.x * u.barW;
        float py = local.y * h;
        // Pixel space → clip space ([-1, 1] on both axes).
        float clipX = (px / u.viewW) * 2.0 - 1.0;
        float clipY = (py / u.viewH) * 2.0 - 1.0;
        VertexOut out;
        out.position = float4(clipX, clipY, 0, 1);
        out.level = levels[iid];
        out.mask  = masks[iid];
        return out;
    }

    fragment float4 bar_fragment(VertexOut in [[stage_in]],
                                  constant Uniforms &u [[buffer(0)]])
    {
        // [[position]] gives fragment pixel-space y with origin at TOP.
        // Flip so y01=0 is bottom of the bar (where amplitude starts).
        float y01 = 1.0 - (in.position.y / u.viewH);
        // Inter-segment gap: top fraction of each segment cell stays
        // transparent so the bar reads as a stack rather than a solid.
        float segPos = fract(y01 * NSEG);
        if (segPos > (1.0 - SEG_GAP)) {
            discard_fragment();
        }
        // Bar color = the instrument's chosen base hue, passed in via
        // u.color. In dark mode the top brightens toward white (LED
        // glow); in light mode it darkens toward black (ink saturation
        // at peak) — both read as "this bar is hotter at the top"
        // against their respective substrates.
        float3 base = u.color.rgb;
        float hot = max(0.0, (y01 - HOT_AT) / (1.0 - HOT_AT));
        float3 hotTarget = (u.isLight > 0.5) ? float3(0.0, 0.0, 0.0) : float3(1.0, 1.0, 1.0);
        float3 tier = mix(base, hotTarget, hot * 0.65);
        // Per-segment glow: brighter at the center of each LED cell,
        // falling off toward the gap edges. Reads as a soft bloom on
        // each lit segment without a real blur pass.
        float visibleSegPos = segPos / (1.0 - SEG_GAP);
        float segCenterDist = abs(visibleSegPos - 0.5) * 2.0;
        float bloom = pow(1.0 - segCenterDist, 1.6);
        // Lit = below the level (live VU) OR the corresponding bit
        // is set in this bar's dot-matrix mask. The mask path lets
        // us spell static text out of the LED segments — used in
        // MIDI mode to render "MIDI".
        uint segIndex = uint(floor(y01 * NSEG));
        uint mask = uint(in.mask);
        bool maskLit = (u.dotMatrix > 0.5) && (((mask >> segIndex) & 1u) != 0u);
        bool levelLit = y01 < in.level;
        bool lit = maskLit || levelLit;
        // Bloom direction also flips with the substrate so the
        // per-segment glow reinforces "hotter" instead of fighting it.
        float bloomSign = (u.isLight > 0.5) ? -1.0 : 1.0;
        float3 color = lit ? (tier + bloomSign * bloom * 0.40) : tier;
        // In light mode unlit segments fade toward the substrate (warm
        // off-white) instead of toward black — without this the
        // "off" rows show as faint colored dots, which reads as a row
        // of always-on LEDs rather than empty headroom.
        float unlitAlpha = (u.isLight > 0.5) ? 0.20 : UNLIT_ALPHA;
        float a = lit ? u.color.a : (u.color.a * unlitAlpha);
        return float4(clamp(color, float3(0.0), float3(1.0)), a);
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
    /// Set to 1 when the bars should render the dot-matrix pattern
    /// (see `dotMasks`) instead of the continuous-level VU. Used in
    /// MIDI mode to spell "MIDI" out of the LED segments.
    var dotMatrix: Float = 0
    /// Set to 1 when the popover is in light appearance — flips the
    /// hot-zone mix target from white to black and the unlit fade
    /// target from black to the warm off-white substrate.
    var isLight: Float = 0
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
        dotMasks.withUnsafeBufferPointer { ptr in
            enc.setVertexBytes(ptr.baseAddress!,
                               length: MemoryLayout<Float>.size * n,
                               index: 2)
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
