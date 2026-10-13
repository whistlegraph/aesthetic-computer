import SwiftUI
import CoreImage
import CoreImage.CIFilterBuiltins

/// Photo filters for attachments (Fía): Apple's photo looks plus two in an
/// Aesthetic Computer spirit — Pixel (chunky pixels) and Riso (two-ink print).
enum PhotoFilter: String, CaseIterable, Identifiable {
    // The choir set comes first: built from the app's own paper, inks and daisy.
    case daisyIris = "Daisy Iris"
    case original = "Original", paper = "Paper", daisy = "Daisy", riso = "Riso", risoBlue = "Riso Blue",
         halftone = "Halftone", pixel = "Pixel",
         mono = "Mono", noir = "Noir", warm = "Warm", fade = "Fade", instant = "Instant", chrome = "Chrome"
    var id: String { rawValue }
    static let choirSet: [PhotoFilter] = [.original, .paper, .daisy, .riso, .risoBlue, .halftone, .pixel]
    static let photoSet: [PhotoFilter] = [.mono, .noir, .warm, .fade, .instant, .chrome]

    /// Two inks: dark → `ink`, light → `paper`, posterized like a print.
    private static func duotone(_ input: CIImage, ink: CIColor, paper: CIColor, levels: Float = 4) -> CIImage? {
        let mono = CIFilter.photoEffectNoir(); mono.inputImage = input
        let poster = CIFilter.colorPosterize(); poster.inputImage = mono.outputImage; poster.levels = levels
        let map = CIFilter.falseColor(); map.inputImage = poster.outputImage; map.color0 = ink; map.color1 = paper
        return map.outputImage
    }

    private static let context = CIContext()

    func apply(_ image: UIImage) -> UIImage {
        guard self != .original, let input = CIImage(image: image)?.oriented(.init(image.imageOrientation)) else { return image }
        guard let out = filtered(input), let cg = Self.context.createCGImage(out, from: out.extent) else { return image }
        return UIImage(cgImage: cg)
    }

    // Fía (10-12): "one filter for camera photos: kind of like an iris shot, but
    // it's the outline of the logo (the new daisy)". The photo shows through her
    // hand-drawn daisy (DaisyMask, filled from her outline) with her outline
    // (DaisyOutline) inked on top. Always square. Outside the daisy is clear, so
    // photos land in the chat as flowers (Fía: "images show up as flowers");
    // the live camera fills it with a soft blur of the scene instead.
    // 18 of her hand-drawn flowers (Daisy00…17 Mask/Outline: the outer edge of her
    // stroke only, centers gone). Each camera shot gets a different one.
    nonisolated(unsafe) private static let daisies: [(mask: CIImage, outline: CIImage)] = (0..<64).compactMap { i in
        let n = String(format: "Daisy%02d", i)
        guard let m = UIImage(named: n + "Mask")?.cgImage, let o = UIImage(named: n + "Outline")?.cgImage else { return nil }
        return (CIImage(cgImage: m), CIImage(cgImage: o))
    }
    nonisolated(unsafe) static var daisyIndex = Int.random(in: 0..<max(daisies.count, 1))
    /// Deal the next flower (after each shot), never the same one twice in a row.
    static func nextDaisy() {
        guard daisies.count > 1 else { return }
        var n = daisyIndex
        while n == daisyIndex { n = Int.random(in: 0..<daisies.count) }
        daisyIndex = n
    }
    static func daisy(_ input: CIImage, blurredFrame: Bool = false) -> CIImage? {
        guard !daisies.isEmpty else { return input }
        let (mask, outline) = daisies[daisyIndex % daisies.count]
        let e = input.extent, side = min(e.width, e.height)
        let square = input.cropped(to: CGRect(x: e.midX - side / 2, y: e.midY - side / 2, width: side, height: side))
            .transformed(by: CGAffineTransform(translationX: -(e.midX - side / 2), y: -(e.midY - side / 2)))
        let rect = CGRect(x: 0, y: 0, width: side, height: side)
        let fit = { (img: CIImage) in img.transformed(by: CGAffineTransform(scaleX: side / img.extent.width, y: side / img.extent.height)) }
        let frame: CIImage
        if blurredFrame {
            let blur = CIFilter.gaussianBlur(); blur.inputImage = square.clampedToExtent(); blur.radius = Float(side / 28)
            let soften = CIFilter.colorControls(); soften.inputImage = blur.outputImage?.cropped(to: rect); soften.brightness = 0.06; soften.saturation = 0.8
            frame = soften.outputImage ?? CIImage(color: .clear).cropped(to: rect)
        } else {
            frame = CIImage(color: .clear).cropped(to: rect)
        }
        let blend = CIFilter.blendWithAlphaMask()
        blend.inputImage = square; blend.backgroundImage = frame; blend.maskImage = fit(mask)
        let ink = CIImage(color: CIColor(red: 0.11, green: 0.094, blue: 0.086)).cropped(to: rect)
        let inked = CIFilter.blendWithAlphaMask(); inked.inputImage = ink; inked.backgroundImage = blend.outputImage; inked.maskImage = fit(outline)
        return inked.outputImage?.cropped(to: rect)
    }

    /// The filter on a CIImage (also used live by the camera preview).
    func filtered(_ input: CIImage) -> CIImage? {
        let out: CIImage?
        switch self {
        case .original: out = input
        case .daisyIris: return Self.daisy(input)
        case .paper:
            out = Self.duotone(input, ink: CIColor(red: 0.11, green: 0.094, blue: 0.086), paper: CIColor(red: 0.97, green: 0.95, blue: 0.89), levels: 6)
        case .daisy:
            let warm = CIFilter.colorControls(); warm.inputImage = input; warm.saturation = 1.15; warm.contrast = 1.05
            let wash = CIFilter.colorMonochrome(); wash.inputImage = warm.outputImage
            wash.color = CIColor(red: 1.0, green: 0.86, blue: 0.35); wash.intensity = 0.28
            let vig = CIFilter.vignette(); vig.inputImage = wash.outputImage; vig.intensity = 0.6; vig.radius = 1.6
            out = vig.outputImage
        case .risoBlue:
            out = Self.duotone(input, ink: CIColor(red: 0.24, green: 0.47, blue: 0.86), paper: CIColor(red: 0.99, green: 0.96, blue: 0.9))
        case .halftone:
            let mono = CIFilter.photoEffectNoir(); mono.inputImage = input
            let dots = CIFilter.dotScreen(); dots.inputImage = mono.outputImage
            dots.width = Float(max(input.extent.width, input.extent.height) / 140); dots.angle = 0.6; dots.sharpness = 0.7
            dots.center = .zero
            let map = CIFilter.falseColor(); map.inputImage = dots.outputImage
            map.color0 = CIColor(red: 0.11, green: 0.094, blue: 0.086); map.color1 = CIColor(red: 0.97, green: 0.95, blue: 0.89)
            out = map.outputImage
        case .mono: let f = CIFilter.photoEffectMono(); f.inputImage = input; out = f.outputImage
        case .noir: let f = CIFilter.photoEffectNoir(); f.inputImage = input; out = f.outputImage
        case .warm: let f = CIFilter.sepiaTone(); f.inputImage = input; f.intensity = 0.55; out = f.outputImage
        case .fade: let f = CIFilter.photoEffectFade(); f.inputImage = input; out = f.outputImage
        case .instant: let f = CIFilter.photoEffectInstant(); f.inputImage = input; out = f.outputImage
        case .chrome: let f = CIFilter.photoEffectChrome(); f.inputImage = input; out = f.outputImage
        case .pixel:
            let f = CIFilter.pixellate(); f.inputImage = input.clampedToExtent()
            f.scale = Float(max(input.extent.width, input.extent.height) / 64); f.center = .zero
            out = f.outputImage?.cropped(to: input.extent)
        case .riso:
            out = Self.duotone(input, ink: CIColor(red: 0.82, green: 0.16, blue: 0.22), paper: CIColor(red: 0.99, green: 0.96, blue: 0.9))
        }
        return out?.cropped(to: input.extent)
    }
}

extension CGImagePropertyOrientation {
    init(_ o: UIImage.Orientation) {
        switch o {
        case .up: self = .up; case .down: self = .down; case .left: self = .left; case .right: self = .right
        case .upMirrored: self = .upMirrored; case .downMirrored: self = .downMirrored
        case .leftMirrored: self = .leftMirrored; case .rightMirrored: self = .rightMirrored
        @unknown default: self = .up
        }
    }
}

/// Big preview, swatches underneath; Done bakes the filter into the attachment.
struct FilterSheet: View {
    let original: UIImage
    let onDone: (UIImage, PhotoFilter) -> Void
    @Environment(\.dismiss) private var dismiss
    @State private var chosen: PhotoFilter
    @State private var preview: UIImage
    @State private var swatches: [PhotoFilter: UIImage] = [:]

    init(original: UIImage, current: PhotoFilter, onDone: @escaping (UIImage, PhotoFilter) -> Void) {
        self.original = original; self.onDone = onDone
        _chosen = State(initialValue: current)
        _preview = State(initialValue: current.apply(original))
    }

    var body: some View {
        NavigationStack {
            VStack(spacing: 16) {
                Image(uiImage: preview).resizable().scaledToFit()
                    .clipShape(RoundedRectangle(cornerRadius: 10)).frame(maxHeight: .infinity).padding(.horizontal)
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: 10) {
                        ForEach([PhotoFilter.daisyIris] + PhotoFilter.choirSet + PhotoFilter.photoSet) { f in
                            Button {
                                chosen = f; Trail.send("photo:filter", ["filter": f.rawValue])
                                Task { preview = f.apply(original) }
                            } label: {
                                VStack(spacing: 4) {
                                    Group {
                                        if let s = swatches[f] { Image(uiImage: s).resizable().scaledToFill() } else { Theme.desk }
                                    }
                                    .frame(width: 64, height: 64).clipShape(RoundedRectangle(cornerRadius: 8))
                                    .overlay(RoundedRectangle(cornerRadius: 8).strokeBorder(Theme.ink, lineWidth: chosen == f ? 2.5 : 0))
                                    Text(f.rawValue).font(Theme.type(11, bold: chosen == f)).foregroundStyle(Theme.ink)
                                }
                            }
                            .buttonStyle(.plain)
                        }
                    }
                    .padding(.horizontal)
                }
                .padding(.bottom, 8)
            }
            .background(RuledPaper())
            .navigationTitle("Filter").navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) { Button("Cancel") { dismiss() } }
                ToolbarItem(placement: .confirmationAction) { Button("Done") { onDone(preview, chosen); dismiss() } }
            }
            .task {
                let small = original.preparingThumbnail(of: CGSize(width: 160, height: 160)) ?? original
                for f in PhotoFilter.allCases { swatches[f] = f.apply(small) }
            }
        }
        .tint(Theme.ink)
    }
}
