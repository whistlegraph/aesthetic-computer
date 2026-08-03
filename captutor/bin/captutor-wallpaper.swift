// CaptutorWallpaper — a quiet animated Fuser stage behind every filmed window.

import AppKit
import CoreGraphics
import Metal
import QuartzCore

private struct Particle {
    let x: CGFloat
    let y: CGFloat
    let size: CGFloat
    let phase: CGFloat
    let sway: CGFloat
    let swayRate: CGFloat
    let opacity: CGFloat
    let isSolid: Bool
}

private struct CardState: Decodable, Equatable {
    let phase: String
    let kicker: String?
    let title: String?
    let subtitle: String?
    let footer: String?
    let showMark: Bool?
    let accent: String?

    static let ambient = CardState(
        phase: "ambient", kicker: nil, title: nil, subtitle: nil, footer: nil,
        showMark: nil, accent: nil
    )
}

private func particles(for canvas: CGSize) -> [Particle] {
    // Fixed seed: two takes with the same clock position get the same visual
    // rhythm, and no random source can make a render irreproducible.
    var state: UInt64 = 0xF053_2026_0720
    func unit() -> CGFloat {
        state = state &* 6364136223846793005 &+ 1442695040888963407
        return CGFloat((state >> 33) & 0xFFFF) / CGFloat(0xFFFF)
    }

    let width = max(canvas.width, 1)
    let height = max(canvas.height, 1)
    let verticalPeriod = height * 1.28
    // Roughly 39 marks at the 2560×1440 Stage size. Smaller displays retain a
    // useful floor without becoming crowded; very large displays get a cap.
    let targetCount = max(28, min(46, Int((width * height) / 95_000)))
    var result: [Particle] = []

    for index in 0..<targetCount {
        // Deliberately stepped sizing makes the hierarchy legible: most marks
        // are texture, some are clearly larger, and a few act as anchors.
        let sizeRoll = unit()
        let logoSize: CGFloat
        if sizeRoll < 0.12 {
            logoSize = 76 + unit() * 28
        } else if sizeRoll < 0.42 {
            logoSize = 48 + unit() * 24
        } else {
            logoSize = 26 + unit() * 22
        }
        let sway = 10 + unit() * 28
        let isSolid = index % 8 == 0
        let candidateOpacity: CGFloat = isSolid ? 1 : 0.38 + unit() * 0.34
        let margin = logoSize / 2 + sway + 10

        // Rejection sampling uses each logo's entire horizontal travel envelope,
        // plus a little breathing room. Because every logo rises at one shared
        // rate, a valid arrangement stays valid throughout the vertical loop.
        var accepted: Particle?
        for _ in 0..<240 {
            let availableWidth = max(0, width - margin * 2)
            let x = margin + unit() * availableWidth
            let y = unit() * verticalPeriod
            let candidate = Particle(
                x: x / width,
                y: y / verticalPeriod,
                size: logoSize,
                phase: unit() * .pi * 2,
                sway: sway,
                swayRate: 0.15 + unit() * 0.14,
                opacity: candidateOpacity,
                isSolid: isSolid
            )
            let clearsField = result.allSatisfy { other in
                let otherX = other.x * width
                let otherY = other.y * verticalPeriod
                let dxAfterSway = max(0, abs(x - otherX) - sway - other.sway)
                let rawY = abs(y - otherY)
                let dy = min(rawY, verticalPeriod - rawY)
                let protectedRadius = (logoSize + other.size) / 2 + 18
                return hypot(dxAfterSway, dy) >= protectedRadius
            }
            if clearsField {
                accepted = candidate
                break
            }
        }
        if let accepted { result.append(accepted) }
    }

    return result
}

private final class WallpaperView: NSView {
    private let born = ProcessInfo.processInfo.systemUptime
    private let field: [Particle]
    private let dark: Bool
    private let logo: NSImage
    private let cardURL = FileManager.default.homeDirectoryForCurrentUser
        .appendingPathComponent(".local/share/captutor/wallpaper-card.json")
    private var card = CardState.ambient
    private var cardPayload = Data()
    private var cardChangedAt = ProcessInfo.processInfo.systemUptime
    private var timer: Timer?

    override init(frame frameRect: NSRect) {
        dark = NSApp.effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        logo = WallpaperView.loadLogo(dark: dark)
        field = particles(for: frameRect.size)
        super.init(frame: frameRect)
        wantsLayer = true
        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            self?.reloadCard()
            self?.needsDisplay = true
        }
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

    deinit { timer?.invalidate() }

    override func draw(_ dirtyRect: NSRect) {
        let rect = bounds
        let t = CGFloat(ProcessInfo.processInfo.systemUptime - born)

        (dark ? NSColor.black : NSColor.white).setFill()
        rect.fill()

        // Keep the drifting Fuser marks as the shared visual system behind
        // every phase. Cards soften that field and add type; they never place
        // a separate hero logo in front of the title.
        let fieldOpacityScale: CGFloat = card.phase == "ambient" ? 1 : 0.30
        for particle in field {
            // One shared rise rate keeps the collision-safe layout rigid in Y,
            // while independent sine waves make the marks sway organically.
            let progress = (particle.y * 1.28 + t * 0.014).truncatingRemainder(dividingBy: 1.28)
            let y = (progress - 0.14) * rect.height
            let x = particle.x * rect.width
                + sin(t * particle.swayRate + particle.phase) * particle.sway
            let logoWidth = particle.size
            // Solid anchors stay genuinely full-opacity. The surrounding marks
            // breathe softly, providing the requested opacity shift without
            // making every logo compete with the filmed Fuser window.
            let breath = particle.isSolid
                ? 1
                : 0.58 + 0.42 * (sin(t * 0.32 + particle.phase) + 1) / 2
            drawLogo(
                in: NSRect(x: x - logoWidth / 2, y: y - particle.size / 2,
                           width: logoWidth, height: particle.size),
                opacity: particle.opacity * breath * fieldOpacityScale
            )
        }
        if card.phase != "ambient" {
            drawCard(in: rect, elapsed: ProcessInfo.processInfo.systemUptime - cardChangedAt)
        }
    }

    private func reloadCard() {
        guard let data = try? Data(contentsOf: cardURL), data != cardPayload,
              let decoded = try? JSONDecoder().decode(CardState.self, from: data) else { return }
        cardPayload = data
        card = decoded
        cardChangedAt = ProcessInfo.processInfo.systemUptime
    }

    private func drawCard(in canvas: NSRect, elapsed: TimeInterval) {
        let darkInk = dark ? NSColor.white : NSColor(calibratedWhite: 0.045, alpha: 1)
        func ease(_ delay: TimeInterval, _ duration: TimeInterval) -> CGFloat {
            let raw = CGFloat(min(1, max(0, (elapsed - delay) / duration)))
            return raw * raw * (3 - 2 * raw)
        }
        let titleProgress = ease(0.12, 0.52)

        NSGraphicsContext.saveGraphicsState()
        let portrait = canvas.width < canvas.height
        let titleSize: CGFloat = portrait ? 48 : 62
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current?.cgContext.setAlpha(titleProgress)
        drawCentered(
            card.title ?? "",
            in: NSRect(
                x: canvas.minX + canvas.width * 0.10,
                y: canvas.midY - (portrait ? 125 : 110) - (1 - titleProgress) * 18,
                width: canvas.width * 0.80,
                height: portrait ? 250 : 220
            ),
            font: NSFont.systemFont(ofSize: titleSize, weight: .black), color: darkInk
        )
        NSGraphicsContext.restoreGraphicsState()
        NSGraphicsContext.restoreGraphicsState()
    }

    private func drawCentered(
        _ text: String, in rect: NSRect, font: NSFont, color: NSColor, kern: CGFloat = 0
    ) {
        let paragraph = NSMutableParagraphStyle()
        paragraph.alignment = .center
        paragraph.lineBreakMode = .byWordWrapping
        let attributed = NSAttributedString(string: text, attributes: [
            .font: font, .foregroundColor: color, .paragraphStyle: paragraph, .kern: kern,
        ])
        attributed.draw(with: rect, options: [.usesLineFragmentOrigin, .usesFontLeading])
    }

    private func drawLogo(in rect: NSRect, opacity: CGFloat) {
        // The icon path is extracted verbatim from Fuser's production SVG. Draw
        // it as a vector-backed image so it remains sharp in the 2× Stage capture.
        logo.draw(in: rect, from: .zero, operation: .sourceOver, fraction: opacity)
    }

    private static func loadLogo(dark: Bool) -> NSImage {
        guard let url = Bundle.main.url(forResource: "fuser-thumbnail-logo", withExtension: "svg"),
              let source = try? String(contentsOf: url, encoding: .utf8) else {
            fatalError("Captutor Wallpaper is missing fuser-thumbnail-logo.svg")
        }
        // The production thumbnail contains a wordmark and glow as well as the
        // actual icon. Extract its first path verbatim so the desktop shows only
        // the real mark—never the product name—and theme that mark by contrast.
        guard let groupStart = source.range(of: "<g filter=\"url(#filter1_ddii_0_1)\">")?.upperBound,
              let groupEnd = source.range(of: "</g>", range: groupStart..<source.endIndex)?.lowerBound,
              let pathStart = source.range(of: "<path", range: groupStart..<groupEnd)?.lowerBound,
              let pathEndToken = source.range(of: "/>", range: pathStart..<groupEnd)?.upperBound else {
            fatalError("Captutor Wallpaper could not isolate the Fuser mark")
        }
        let color = dark ? "#FAFAFA" : "#171717"
        let path = String(source[pathStart..<pathEndToken])
            .replacingOccurrences(of: "fill=\"#FAFAFA\"", with: "fill=\"\(color)\"")
            .replacingOccurrences(of: "fill=\"white\"", with: "fill=\"\(color)\"")
        let iconOnly = """
        <svg width="96" height="96" viewBox="45 63 36 36" fill="none" xmlns="http://www.w3.org/2000/svg">
        \(path)
        </svg>
        """
        guard let image = NSImage(data: Data(iconOnly.utf8)) else {
            fatalError("Captutor Wallpaper could not decode the Fuser SVG")
        }
        return image
    }
}

private struct MetaballInstance {
    var placement: SIMD4<Float>
    var motion: SIMD4<Float>
    var appearance: SIMD4<Float>
}

private struct MetaballUniforms {
    var viewport: SIMD2<Float>
    var time: Float
    var dark: Float
    var daylight: SIMD4<Float>
}

// One transparent, instanced Metal pass renders every logo. Only each logo's
// bounding quad is shaded, and the drawable is intentionally sub-Retina; this
// keeps real-time implicit surfaces practical for a quiet desktop backdrop.
private final class MetaballRenderer {
    let layer = CAMetalLayer()
    private let device: MTLDevice
    private let queue: MTLCommandQueue
    private let pipeline: MTLRenderPipelineState
    private let sampleCount: Int
    private var multisampleTexture: MTLTexture?
    private var instances: [MetaballInstance] = []
    private var timer: Timer?
    private var started = ProcessInfo.processInfo.systemUptime
    private var dark = false

    init?() {
        guard let device = MTLCreateSystemDefaultDevice(),
              let queue = device.makeCommandQueue() else { return nil }
        self.device = device
        self.queue = queue
        sampleCount = device.supportsTextureSampleCount(4) ? 4
            : (device.supportsTextureSampleCount(2) ? 2 : 1)
        layer.device = device
        layer.pixelFormat = .bgra8Unorm
        layer.framebufferOnly = true
        layer.isOpaque = false
        layer.maximumDrawableCount = 2
        layer.contentsGravity = .resize

        do {
            let library = try device.makeLibrary(source: Self.shader, options: nil)
            let descriptor = MTLRenderPipelineDescriptor()
            descriptor.vertexFunction = library.makeFunction(name: "metaballVertex")
            descriptor.fragmentFunction = library.makeFunction(name: "metaballFragment")
            descriptor.rasterSampleCount = sampleCount
            descriptor.colorAttachments[0].pixelFormat = layer.pixelFormat
            descriptor.colorAttachments[0].isBlendingEnabled = true
            descriptor.colorAttachments[0].sourceRGBBlendFactor = .sourceAlpha
            descriptor.colorAttachments[0].destinationRGBBlendFactor = .oneMinusSourceAlpha
            descriptor.colorAttachments[0].sourceAlphaBlendFactor = .one
            descriptor.colorAttachments[0].destinationAlphaBlendFactor = .oneMinusSourceAlpha
            pipeline = try device.makeRenderPipelineState(descriptor: descriptor)
        } catch {
            fputs("Captutor metaball shader unavailable: \(error)\n", stderr)
            return nil
        }

        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            self?.draw()
        }
        RunLoop.main.add(timer!, forMode: .common)
    }

    deinit { timer?.invalidate() }

    func setAppearance(dark: Bool) { self.dark = dark }

    func setOpacity(_ opacity: Float) {
        CATransaction.begin()
        CATransaction.setAnimationDuration(0.48)
        layer.opacity = opacity
        CATransaction.commit()
    }

    func setLayout(bounds: CGRect, backingScale: CGFloat,
                   specs: [(CGFloat, CGFloat, CGFloat, TimeInterval, TimeInterval, CGFloat, Int)]) {
        layer.frame = bounds
        // Match the display's backing pixels for crisp foreground silhouettes.
        // Small marks spend fewer shader steps and use softer shading below.
        // A 1× QHD display exposes raymarched contour quantization more readily
        // than Retina. Render those hosts at 1.5× and let Core Animation perform
        // a high-quality downsample; Retina hosts already have enough pixels.
        let renderScale = backingScale < 1.5 ? 1.5 : backingScale
        layer.contentsScale = renderScale
        layer.drawableSize = CGSize(width: max(1, bounds.width * renderScale),
                                    height: max(1, bounds.height * renderScale))
        multisampleTexture = nil
        let logoScale = min(max(bounds.width / 1280, 0.78), 1.30) * renderScale
        instances = specs.enumerated().map { index, spec in
            let opacity: Float = spec.2 > 100 ? 0.72 : (spec.2 > 60 ? 0.58 : 0.48)
            return MetaballInstance(
                placement: SIMD4(Float(spec.0), Float(spec.1), Float(spec.2 * logoScale), opacity),
                motion: SIMD4(Float(spec.3), Float(spec.4), Float(spec.5 * renderScale),
                              spec.2 < 70 ? 1 : (spec.2 < 100 ? 0.52 : 0)),
                appearance: SIMD4(Float(spec.6), index.isMultiple(of: 3) ? -1 : 1,
                                  Float(index) * 0.713, 0)
            )
        }
    }

    private func draw() {
        guard !instances.isEmpty, layer.opacity > 0.001,
              let drawable = layer.nextDrawable(),
              let command = queue.makeCommandBuffer(),
              let encoder = command.makeRenderCommandEncoder(descriptor: renderPass(for: drawable.texture)) else { return }
        let components = Calendar.current.dateComponents([.hour, .minute, .second], from: Date())
        let seconds = Float((components.hour ?? 12) * 3600
                            + (components.minute ?? 0) * 60
                            + (components.second ?? 0))
        let dayPhase = seconds / 86_400
        let sunHeight = max(0, sin((dayPhase - 0.25) * Float.pi * 2))
        let dayProgress = min(max((dayPhase - 0.25) / 0.5, 0), 1)
        let horizonWarmth = sunHeight > 0 ? pow(abs(dayProgress - 0.5) * 2, 1.35) : 0
        var uniforms = MetaballUniforms(
            viewport: SIMD2(Float(layer.drawableSize.width), Float(layer.drawableSize.height)),
            time: Float(ProcessInfo.processInfo.systemUptime - started),
            dark: dark ? 1 : 0,
            daylight: SIMD4(dayPhase, sunHeight, horizonWarmth, dayProgress)
        )
        encoder.setRenderPipelineState(pipeline)
        instances.withUnsafeBytes { bytes in
            encoder.setVertexBytes(bytes.baseAddress!, length: bytes.count, index: 0)
        }
        encoder.setVertexBytes(&uniforms, length: MemoryLayout<MetaballUniforms>.stride, index: 1)
        encoder.setFragmentBytes(&uniforms, length: MemoryLayout<MetaballUniforms>.stride, index: 0)
        encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4,
                               instanceCount: instances.count)
        encoder.endEncoding()
        command.present(drawable)
        command.commit()
    }

    private func renderPass(for texture: MTLTexture) -> MTLRenderPassDescriptor {
        let pass = MTLRenderPassDescriptor()
        if sampleCount > 1 {
            if multisampleTexture?.width != texture.width || multisampleTexture?.height != texture.height {
                let descriptor = MTLTextureDescriptor()
                descriptor.textureType = .type2DMultisample
                descriptor.pixelFormat = layer.pixelFormat
                descriptor.width = texture.width
                descriptor.height = texture.height
                descriptor.sampleCount = sampleCount
                descriptor.storageMode = .private
                descriptor.usage = .renderTarget
                multisampleTexture = device.makeTexture(descriptor: descriptor)
            }
            pass.colorAttachments[0].texture = multisampleTexture
            pass.colorAttachments[0].resolveTexture = texture
            pass.colorAttachments[0].storeAction = .multisampleResolve
        } else {
            pass.colorAttachments[0].texture = texture
            pass.colorAttachments[0].storeAction = .store
        }
        pass.colorAttachments[0].loadAction = .clear
        pass.colorAttachments[0].clearColor = MTLClearColorMake(0, 0, 0, 0)
        return pass
    }

    private static let shader = #"""
    #include <metal_stdlib>
    using namespace metal;

    struct Instance { float4 placement; float4 motion; float4 appearance; };
    struct Uniforms { float2 viewport; float time; float dark; float4 daylight; };
    struct Raster {
        float4 position [[position]];
        float2 local;
        float4 placement;
        float4 motion;
        float4 appearance;
    };

    constant float PI = 3.14159265359;
    constant float TAU = 6.28318530718;
    constant float3 centers[12] = {
        float3( 0.40654,  1.21499,  0.00000), float3( 1.21550,  1.21499,  0.05450),
        float3(-0.40235,  1.21499, -0.01517), float3(-0.40235,  0.40476, -0.05028),
        float3(-1.21530,  0.40867,  0.02924), float3(-1.21139, -0.40476,  0.04214),
        float3(-1.21139, -1.21499, -0.04094), float3(-0.40235, -1.21499, -0.03075),
        float3( 0.40654, -1.21499,  0.04950), float3( 0.40654, -0.40476,  0.01693),
        float3( 1.21550, -0.40476, -0.05421), float3( 1.21550,  0.40476,  0.00153)
    };

    float smoothMin(float a, float b, float k) {
        float h = max(k - abs(a - b), 0.0) / k;
        return min(a, b) - h * h * k * 0.25;
    }

    float field(float3 p) {
        float d = length(p - centers[0]) - 0.355;
        for (uint i = 1; i < 12; ++i)
            d = smoothMin(d, length(p - centers[i]) - 0.355, 0.30);
        return d;
    }

    float3 rotateX(float3 p, float a) {
        float c = cos(a), s = sin(a);
        return float3(p.x, p.y * c - p.z * s, p.y * s + p.z * c);
    }
    float3 rotateY(float3 p, float a) {
        float c = cos(a), s = sin(a);
        return float3(p.x * c + p.z * s, p.y, -p.x * s + p.z * c);
    }
    float3 rotateZ(float3 p, float a) {
        float c = cos(a), s = sin(a);
        return float3(p.x * c - p.y * s, p.x * s + p.y * c, p.z);
    }
    float3 inverseTurn(float3 p, float3 angle) {
        p = rotateZ(p, -angle.z);
        p = rotateY(p, -angle.y);
        return rotateX(p, -angle.x);
    }

    vertex Raster metaballVertex(uint vertexID [[vertex_id]], uint instanceID [[instance_id]],
                                  constant Instance *instances [[buffer(0)]],
                                  constant Uniforms &u [[buffer(1)]]) {
        const float2 corners[4] = { float2(-1,-1), float2(1,-1), float2(-1,1), float2(1,1) };
        Instance item = instances[instanceID];
        float2 corner = corners[vertexID];
        float size = item.placement.z;
        // The diagonal extent of the rotated twelve-node field is larger than
        // its front-facing box. Expand the transparent quad without changing
        // the apparent sculpture scale so no spatial pose can clip internally.
        float quadSize = size * 1.28;
        // A strict sawtooth moves only upward: fully below the screen, through
        // the complete frame, fully beyond the top, then back to the bottom.
        float rise = fract(item.placement.y + u.time / item.motion.x);
        float verticalMargin = quadSize * 0.56;
        float horizontalMargin = quadSize * 0.52 + max(8.0, u.viewport.x * 0.07);
        float2 center = float2(item.placement.x * u.viewport.x,
                               mix(-verticalMargin, u.viewport.y + verticalMargin, rise));
        center.x += sin(u.time * 0.18 + item.placement.y * TAU) * item.motion.z;
        center.x = clamp(center.x, horizontalMargin, u.viewport.x - horizontalMargin);
        float2 ndc = float2(center.x / u.viewport.x * 2.0 - 1.0,
                            center.y / u.viewport.y * 2.0 - 1.0);
        ndc += corner * float2(quadSize / u.viewport.x, quadSize / u.viewport.y);
        Raster out;
        out.position = float4(ndc, 0, 1);
        out.local = corner * 2.18;
        out.placement = item.placement;
        out.motion = item.motion;
        out.appearance = item.appearance;
        return out;
    }

    fragment float4 metaballFragment(Raster in [[stage_in]], uint sampleID [[sample_id]],
                                      constant Uniforms &u [[buffer(0)]]) {
        // Referencing sample_id requests true per-sample implicit-surface
        // coverage instead of merely antialiasing the outer billboard quad.
        float sampleGuard = float(sampleID) * 0.0;
        float direction = in.appearance.y;
        float seed = in.appearance.z;
        float turn = direction * (u.time / in.motion.y * TAU) + in.placement.y * TAU;
        // Rotation is around the spatial vertical axis only. The silhouette and
        // depth change continuously, but the canonical top can never tilt over.
        float3 angle = float3(0.0, turn, 0.0);
        float3 ro = inverseTurn(float3(in.local, 3.0), angle);
        float3 rd = inverseTurn(float3(0, 0, -1), angle);

        // Skip empty corners analytically before entering the implicit field.
        float b = dot(ro, rd);
        float c = dot(ro, ro) - 2.12 * 2.12;
        float discriminant = b * b - c;
        if (discriminant < 0.0) discard_fragment();
        float traveled = max(0.0, -b - sqrt(discriminant));
        float3 p = ro + rd * traveled;
        float blur = in.motion.w;
        float hitEpsilon = mix(0.0025, 0.011, blur);
        uint stepLimit = blur > 0.75 ? 28 : (blur > 0.1 ? 40 : 56);
        bool hit = false;
        for (uint step = 0; step < stepLimit; ++step) {
            float d = field(p);
            if (d < hitEpsilon) { hit = true; break; }
            traveled += max(d * 0.68, hitEpsilon);
            if (traveled > 6.0) break;
            p = ro + rd * traveled;
        }
        if (!hit) discard_fragment();

        float e = mix(0.004, 0.015, blur);
        float3 normal = normalize(float3(
            field(p + float3(e,0,0)) - field(p - float3(e,0,0)),
            field(p + float3(0,e,0)) - field(p - float3(0,e,0)),
            field(p + float3(0,0,e)) - field(p - float3(0,0,e))
        ));
        float3 worldNormal = rotateX(normal, angle.x);
        worldNormal = rotateY(worldNormal, angle.y);
        worldNormal = rotateZ(worldNormal, angle.z);
        // One global light travels across every logo with local solar time.
        float day = u.daylight.y;
        float progress = u.daylight.w;
        float3 sun = normalize(float3(mix(-0.86, 0.86, progress),
                                      0.26 + sin(progress * PI) * 0.72, 0.62));
        float3 moon = normalize(float3(0.62, 0.46, 0.66));
        float dayMix = smoothstep(0.0, 0.12, day);
        float3 light = normalize(mix(moon, sun, dayMix));
        float3 fill = normalize(float3(-light.x * 0.72, -0.24, 0.70));
        float3 sunColor = mix(float3(1.0, 0.97, 0.92), float3(1.0, 0.76, 0.54),
                              u.daylight.z * 0.32);
        float3 lightColor = mix(float3(0.66, 0.78, 1.0), sunColor, dayMix);
        float diffuse = max(dot(worldNormal, light), 0.0);
        float fillLight = max(dot(worldNormal, fill), 0.0);
        float specular = pow(max(dot(worldNormal, normalize(light + float3(0,0,1))), 0.0),
                             mix(52.0, 18.0, blur));
        float rim = pow(1.0 - abs(worldNormal.z), 2.25);
        float3 edgeDirection = normalize(float3(-light.x, light.y * 0.18, -light.z));
        float edgeLight = pow(max(dot(worldNormal, edgeDirection), 0.0), 3.2) * rim;

        int variant = int(in.appearance.x + 0.5);
        float baseValue;
        if (u.dark > 0.5)
            baseValue = variant == 0 ? 0.82 : (variant == 1 ? 0.48 : 0.68);
        else
            baseValue = variant == 0 ? 0.035 : (variant == 1 ? 0.72 : 0.22);
        float illumination = mix(0.86, 1.04, day);
        float lightShape = (0.25 + diffuse * 0.70 + fillLight * 0.18) * illumination;
        lightShape = mix(lightShape, 0.54 + diffuse * 0.36, blur * 0.72);
        float value = baseValue * lightShape + sampleGuard;
        value += rim * mix(u.dark > 0.5 ? 0.42 : 0.30, 0.18, blur)
               + specular * mix(0.92, 0.46, blur);
        float3 color = float3(clamp(value, 0.0, 1.0));
        color += lightColor * (specular * 0.20 + edgeLight * mix(0.20, 0.38, 1.0 - blur));
        color = clamp(color, 0.0, 1.0);
        return float4(color, in.placement.w);
    }
    """#
}

// Dimensional Fuser backdrop. The production SVG's twelve nodes become one
// live smooth-union surface, tumbling through yaw, pitch, and roll in Metal.
private final class FuserDimensionalView: NSView {
    private struct MarkSpec {
        let x: CGFloat
        let phase: CGFloat
        let size: CGFloat
        let riseSeconds: TimeInterval
        let turnSeconds: TimeInterval
        let sway: CGFloat
        let variant: Int
    }

    private static let anchorSpecs = [
        // A fixed staggered 4×4 field. Small phase offsets soften the rows while
        // the shared 48-second rise keeps every protected gap invariant.
        MarkSpec(x: 0.07, phase: 0.04, size: 138, riseSeconds: 48, turnSeconds: 25, sway: 12, variant: 0),
        MarkSpec(x: 0.34, phase: 0.08, size: 48, riseSeconds: 48, turnSeconds: 18, sway: 9, variant: 1),
        MarkSpec(x: 0.61, phase: 0.02, size: 92, riseSeconds: 48, turnSeconds: 22, sway: 11, variant: 2),
        MarkSpec(x: 0.88, phase: 0.06, size: 60, riseSeconds: 48, turnSeconds: 20, sway: 9, variant: 0),

        MarkSpec(x: 0.18, phase: 0.29, size: 52, riseSeconds: 48, turnSeconds: 17, sway: 9, variant: 1),
        MarkSpec(x: 0.45, phase: 0.33, size: 116, riseSeconds: 48, turnSeconds: 28, sway: 12, variant: 2),
        MarkSpec(x: 0.72, phase: 0.27, size: 44, riseSeconds: 48, turnSeconds: 16, sway: 8, variant: 0),
        MarkSpec(x: 0.95, phase: 0.31, size: 82, riseSeconds: 48, turnSeconds: 23, sway: 10, variant: 1),

        MarkSpec(x: 0.06, phase: 0.54, size: 76, riseSeconds: 48, turnSeconds: 21, sway: 10, variant: 2),
        MarkSpec(x: 0.31, phase: 0.58, size: 46, riseSeconds: 48, turnSeconds: 15, sway: 8, variant: 0),
        MarkSpec(x: 0.58, phase: 0.52, size: 146, riseSeconds: 48, turnSeconds: 31, sway: 13, variant: 1),
        MarkSpec(x: 0.84, phase: 0.56, size: 58, riseSeconds: 48, turnSeconds: 19, sway: 9, variant: 2),

        MarkSpec(x: 0.16, phase: 0.79, size: 48, riseSeconds: 48, turnSeconds: 16, sway: 8, variant: 0),
        MarkSpec(x: 0.42, phase: 0.83, size: 104, riseSeconds: 48, turnSeconds: 27, sway: 11, variant: 1),
        MarkSpec(x: 0.68, phase: 0.77, size: 64, riseSeconds: 48, turnSeconds: 20, sway: 9, variant: 2),
        MarkSpec(x: 0.93, phase: 0.81, size: 126, riseSeconds: 48, turnSeconds: 29, sway: 12, variant: 0),
    ]

    // The browser deliberately owns most of the Stage, leaving only narrow
    // bands of wallpaper visible. A sparse field therefore reads as random
    // blobs instead of Fuser. Keep the sixteen large anchors above, then add a
    // deterministic constellation of small complete marks: enough of the
    // twelve-node silhouette is always visible around the window to make the
    // identity instantly legible without competing with the tutorial.
    private static let microSpecs: [MarkSpec] = (0..<96).map { index in
        let column = index % 16
        let row = index / 16
        let jitter = CGFloat((index * 7) % 11) / 11 - 0.5
        let x = (CGFloat(column) + 0.5 + jitter * 0.42) / 16
        let phase = (CGFloat(row) / 6
                     + CGFloat((index * 13) % 29) / 29 * 0.12)
            .truncatingRemainder(dividingBy: 1)
        return MarkSpec(
            x: x,
            phase: phase,
            size: CGFloat(18 + (index * 5) % 19),
            riseSeconds: TimeInterval(34 + (index * 3) % 15),
            turnSeconds: TimeInterval(9 + (index * 5) % 12),
            sway: CGFloat(3 + (index * 2) % 6),
            variant: index % 3
        )
    }

    private static let specs = anchorSpecs + microSpecs

    private let background = CAGradientLayer()
    private let glowA = CAGradientLayer()
    private let glowB = CAGradientLayer()
    private let ambient = CALayer()
    private let metaballRenderer = MetaballRenderer()
    private let cardLayer = CALayer()
    private let cardMark = CALayer()
    private let cardTitle = CATextLayer()
    private let metaballSheets: [CGImage]
    private let cardURL = FileManager.default.homeDirectoryForCurrentUser
        .appendingPathComponent(".local/share/captutor/wallpaper-card.json")
    private var card = CardState.ambient
    private var cardPayload = Data()
    private var timer: Timer?
    private var lastSize = CGSize.zero

    override init(frame frameRect: NSRect) {
        metaballSheets = (0..<3).map(Self.loadMetaballSheet(variant:))
        super.init(frame: frameRect)
        wantsLayer = true
        let root = CALayer()
        root.masksToBounds = true
        layer = root

        background.startPoint = CGPoint(x: 0.04, y: 0.94)
        background.endPoint = CGPoint(x: 0.96, y: 0.06)
        root.addSublayer(background)

        for glow in [glowA, glowB] {
            glow.type = .radial
            glow.startPoint = CGPoint(x: 0.5, y: 0.5)
            glow.endPoint = CGPoint(x: 0.96, y: 0.96)
            glow.locations = [0, 0.48, 1]
            root.addSublayer(glow)
        }
        root.addSublayer(ambient)
        if let metaballRenderer {
            root.addSublayer(metaballRenderer.layer)
        }

        cardLayer.opacity = 0
        cardLayer.addSublayer(cardMark)
        cardTitle.alignmentMode = .center
        cardTitle.isWrapped = true
        cardTitle.contentsScale = NSScreen.main?.backingScaleFactor ?? 2
        cardTitle.font = NSFont.systemFont(ofSize: 60, weight: .black)
        cardLayer.addSublayer(cardTitle)
        root.addSublayer(cardLayer)

        applyAppearance()
        timer = Timer.scheduledTimer(withTimeInterval: 0.20, repeats: true) { [weak self] _ in
            self?.reloadCard()
        }
    }

    required init?(coder: NSCoder) { nil }
    deinit { timer?.invalidate() }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        applyAppearance()
    }

    override func layout() {
        super.layout()
        background.frame = bounds
        let glowSize = max(bounds.width, bounds.height) * 0.82
        glowA.frame = CGRect(x: -glowSize * 0.38, y: bounds.height - glowSize * 0.56,
                             width: glowSize, height: glowSize)
        glowB.frame = CGRect(x: bounds.width - glowSize * 0.62, y: -glowSize * 0.42,
                             width: glowSize, height: glowSize)
        metaballRenderer?.setLayout(bounds: bounds,
                                    backingScale: window?.backingScaleFactor
                                        ?? NSScreen.main?.backingScaleFactor ?? 2,
                                    specs: Self.specs.map {
            ($0.x, $0.phase, $0.size, $0.riseSeconds, $0.turnSeconds, $0.sway, $0.variant)
        })
        cardLayer.frame = bounds
        let portrait = bounds.width < bounds.height
        let markSize: CGFloat = portrait ? 150 : 170
        cardMark.frame = CGRect(x: bounds.midX - markSize / 2,
                                y: bounds.midY + (portrait ? 55 : 25),
                                width: markSize, height: markSize)
        cardTitle.frame = CGRect(x: bounds.width * 0.09,
                                 y: bounds.midY - (portrait ? 230 : 195),
                                 width: bounds.width * 0.82,
                                 height: portrait ? 235 : 210)
        guard bounds.size != lastSize else { return }
        lastSize = bounds.size
        rebuildAmbient()
    }

    private func rebuildAmbient() {
        ambient.sublayers?.forEach { $0.removeFromSuperlayer() }
        ambient.frame = bounds
        guard metaballRenderer == nil else { return }
        let now = CACurrentMediaTime()

        for spec in Self.specs {
            let scale = min(max(bounds.width / 1280, 0.78), 1.30)
            let size = spec.size * scale
            let travel = CALayer()
            travel.bounds = CGRect(x: 0, y: 0, width: size, height: size)
            travel.opacity = spec.size > 100 ? 0.72 : 0.58
            travel.allowsEdgeAntialiasing = true

            let solid = CALayer()
            solid.frame = travel.bounds
            solid.anchorPoint = CGPoint(x: 0.5, y: 0.5)
            solid.position = CGPoint(x: size / 2, y: size / 2)
            travel.addSublayer(solid)

            let turnStart = now - spec.turnSeconds * Double(spec.phase)
            let sheet = metaballSheets[spec.variant]
            let sprite = CALayer()
            sprite.frame = solid.bounds
            sprite.contents = sheet
            sprite.contentsGravity = .resizeAspectFill
            sprite.contentsScale = 2
            solid.addSublayer(sprite)
            let turn = CABasicAnimation(keyPath: "transform.rotation.z")
            turn.fromValue = spec.variant == 1 ? CGFloat.pi * 2 : 0
            turn.toValue = spec.variant == 1 ? 0 : CGFloat.pi * 2
            turn.duration = spec.turnSeconds
            turn.repeatCount = .infinity
            turn.timingFunction = CAMediaTimingFunction(name: .linear)
            turn.beginTime = turnStart
            turn.isRemovedOnCompletion = false
            solid.add(turn, forKey: "metaballTurn")

            let x = bounds.width * spec.x
            // All marks traverse exactly the same vertical span and duration.
            // Their relative layout therefore survives every wrap unchanged.
            let travelMargin = 190 * scale
            let lowY = -travelMargin
            let highY = bounds.height + travelMargin
            let path = CGMutablePath()
            path.move(to: CGPoint(x: x, y: lowY))
            path.addCurve(to: CGPoint(x: x - spec.sway * 0.45, y: bounds.height * 0.44),
                          control1: CGPoint(x: x + spec.sway, y: bounds.height * 0.13),
                          control2: CGPoint(x: x - spec.sway, y: bounds.height * 0.31))
            path.addCurve(to: CGPoint(x: x, y: highY),
                          control1: CGPoint(x: x + spec.sway, y: bounds.height * 0.68),
                          control2: CGPoint(x: x - spec.sway * 0.55, y: bounds.height * 0.88))
            travel.position = CGPoint(x: x, y: lowY + (highY - lowY) * spec.phase)
            let rise = CAKeyframeAnimation(keyPath: "position")
            rise.path = path
            rise.calculationMode = .paced
            rise.duration = spec.riseSeconds
            rise.repeatCount = .infinity
            rise.beginTime = now - spec.riseSeconds * Double(spec.phase)
            rise.isRemovedOnCompletion = false
            travel.add(rise, forKey: "fuserRise")

            ambient.addSublayer(travel)
        }
    }

    private func applyAppearance() {
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let accent = card.accent.flatMap(NSColor.init(hexString:))
            ?? NSColor(hex: 0xA58CBC)
        metaballRenderer?.setAppearance(dark: dark)
        CATransaction.begin()
        CATransaction.setAnimationDuration(0.55)
        background.colors = (dark ? [
            NSColor(hex: 0x050505).mixed(with:accent, amount:0.10),
            NSColor(hex: 0x141414).mixed(with:accent, amount:0.16),
            NSColor(hex: 0x080808).mixed(with:accent, amount:0.08),
        ] : [
            NSColor(hex: 0xFFFFFF).mixed(with:accent, amount:0.07),
            NSColor(hex: 0xECECEC).mixed(with:accent, amount:0.13),
            NSColor(hex: 0xFAFAFA).mixed(with:accent, amount:0.09),
        ]).map(\.cgColor)
        let smoke = NSColor(hex: dark ? 0xFFFFFF : 0x111111)
        let silver = NSColor(hex: dark ? 0xA8A8A8 : 0x8E8E8E)
        glowA.colors = [smoke.withAlphaComponent(dark ? 0.12 : 0.09).cgColor,
                        smoke.withAlphaComponent(dark ? 0.045 : 0.032).cgColor,
                        smoke.withAlphaComponent(0).cgColor]
        glowB.colors = [silver.withAlphaComponent(dark ? 0.10 : 0.08).cgColor,
                        silver.withAlphaComponent(dark ? 0.038 : 0.028).cgColor,
                        silver.withAlphaComponent(0).cgColor]
        cardTitle.foregroundColor = (dark ? NSColor.white : NSColor(hex: 0x171717)).cgColor
        cardMark.contents = metaballSheets[0]
        cardMark.contentsGravity = .resizeAspect
        cardMark.shadowColor = (dark ? NSColor.white : NSColor.black).cgColor
        cardMark.shadowRadius = 28
        cardMark.shadowOpacity = 0.46
        CATransaction.commit()
    }

    private func reloadCard() {
        guard let data = try? Data(contentsOf: cardURL), data != cardPayload,
              let decoded = try? JSONDecoder().decode(CardState.self, from: data) else { return }
        cardPayload = data
        card = decoded
        applyAppearance()
        cardTitle.string = decoded.title ?? ""
        cardMark.opacity = decoded.showMark == false ? 0 : 1
        CATransaction.begin()
        CATransaction.setAnimationDuration(0.48)
        ambient.opacity = decoded.phase == "ambient" ? 1 : 0.10
        metaballRenderer?.setOpacity(decoded.phase == "ambient" ? 1 : 0.10)
        cardLayer.opacity = decoded.phase == "ambient" ? 0 : 1
        CATransaction.commit()
    }

    private static func loadMetaballSheet(variant: Int) -> CGImage {
        guard let url = Bundle.main.url(forResource: "fuser-metaballs-\(variant)", withExtension: "png"),
              let image = NSImage(contentsOf: url),
              let cgImage = image.cgImage(forProposedRect: nil, context: nil, hints: nil) else {
            fatalError("Captutor Wallpaper is missing fuser-metaballs-\(variant).png")
        }
        return cgImage
    }
}

private extension NSColor {
    convenience init(hex: UInt32) {
        self.init(srgbRed: CGFloat((hex >> 16) & 0xFF) / 255,
                  green: CGFloat((hex >> 8) & 0xFF) / 255,
                  blue: CGFloat(hex & 0xFF) / 255,
                  alpha: 1)
    }

    convenience init?(hexString: String) {
        let value = hexString.trimmingCharacters(in: CharacterSet(charactersIn: "#"))
        guard value.count == 6, let hex = UInt32(value, radix:16) else { return nil }
        self.init(hex:hex)
    }

    func mixed(with other: NSColor, amount: CGFloat) -> NSColor {
        let a = usingColorSpace(.sRGB) ?? self
        let b = other.usingColorSpace(.sRGB) ?? other
        let t = min(max(amount, 0), 1)
        return NSColor(srgbRed:a.redComponent + (b.redComponent - a.redComponent) * t,
                       green:a.greenComponent + (b.greenComponent - a.greenComponent) * t,
                       blue:a.blueComponent + (b.blueComponent - a.blueComponent) * t,
                       alpha:a.alphaComponent + (b.alphaComponent - a.alphaComponent) * t)
    }
}

private final class AppDelegate: NSObject, NSApplicationDelegate {
    private var windows: [NSWindow] = []

    func applicationDidFinishLaunching(_ notification: Notification) {
        let args = CommandLine.arguments
        let brandIndex = args.firstIndex(of: "--brand")
        let brand = brandIndex.flatMap { index in
            args.indices.contains(index + 1) ? args[index + 1].lowercased() : nil
        } ?? "fuser"
        let prototype = args.contains("--prototype")
        for screen in NSScreen.screens {
            let window = NSWindow(
                contentRect: screen.frame,
                styleMask: [.borderless],
                backing: .buffered,
                defer: false,
                screen: screen
            )
            window.level = NSWindow.Level(rawValue: prototype
                ? Int(CGWindowLevelForKey(.desktopIconWindow)) - 1
                : Int(CGWindowLevelForKey(.desktopWindow)) + 1)
            window.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle]
            window.ignoresMouseEvents = true
            window.isOpaque = true
            window.hasShadow = false
            if brand == "classic" {
                window.contentView = WallpaperView(frame: NSRect(origin: .zero, size: screen.frame.size))
            } else {
                window.contentView = FuserDimensionalView(frame: NSRect(origin: .zero, size: screen.frame.size))
            }
            window.orderFrontRegardless()
            windows.append(window)
        }
        print("Captutor Wallpaper brand=\(brand) prototype=\(prototype) " +
              "renderer=\(brand == "classic" ? "vector-field" : "realtime-metal-metaballs")")
        fflush(stdout)
    }
}

let app = NSApplication.shared
private let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory)
app.run()
