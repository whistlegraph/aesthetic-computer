import AppKit

// The WaveWizard mascot — illustrated PNG bundled at
// Sources/WaveWizard/Assets/wavewizard-mascot.png. A subtle tint
// overlay reflects the current mood (yellow = recording, cyan =
// listening, green = success, red = error). The PNG was generated
// via bin/gen-mascot.mjs (gpt-image-2, hand-drawn AC style).
final class MascotView: NSView {
    enum Mood { case idle, listening, recording, success, error }
    var mood: Mood = .idle { didSet { needsDisplay = true } }

    private let image: NSImage? = {
        // Try the SwiftPM-generated bundle resource path first.
        let bundle = Bundle.module
        if let url = bundle.url(forResource: "wavewizard-mascot",
                                withExtension: "png",
                                subdirectory: "Assets"),
           let img = NSImage(contentsOf: url) {
            return img
        }
        if let url = bundle.url(forResource: "wavewizard-mascot", withExtension: "png"),
           let img = NSImage(contentsOf: url) {
            return img
        }
        // Last-resort: relative path from the binary (helps when running
        // from .build/release before `swift build` has copied Assets).
        let here = FileManager.default.currentDirectoryPath
        let fallback = "\(here)/Sources/WaveWizard/Assets/wavewizard-mascot.png"
        return NSImage(contentsOfFile: fallback)
    }()

    override var wantsDefaultClipping: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        // Faded watermark: the wizard sits behind the working UI, light
        // opacity, aspect-fit into the bounds. No bordered card, no
        // mood-tint overlay — the controls in front carry the state.
        guard let img = image else { return }
        let imgSize = img.size
        let viewAR = bounds.width / bounds.height
        let imgAR  = imgSize.width / imgSize.height
        // ASPECT-FILL (cover): the image always fills the view with no
        // empty bars — the longer dimension is cropped instead. Flips
        // the previous aspect-fit logic.
        var drawRect = bounds
        if imgAR > viewAR {
            // image is wider than view → match height, overflow width
            let w = bounds.height * imgAR
            drawRect = NSRect(x: (bounds.width - w) * 0.5, y: 0, width: w, height: bounds.height)
        } else {
            // image is taller than view → match width, overflow height
            let h = bounds.width / imgAR
            drawRect = NSRect(x: 0, y: (bounds.height - h) * 0.5, width: bounds.width, height: h)
        }
        let baseAlpha: CGFloat = 0.18
        let alpha: CGFloat
        switch mood {
        case .idle:      alpha = baseAlpha
        case .listening: alpha = baseAlpha + 0.06
        case .recording: alpha = baseAlpha + 0.10
        case .success:   alpha = baseAlpha + 0.04
        case .error:     alpha = baseAlpha + 0.06
        }
        img.draw(in: drawRect, from: .zero, operation: .sourceOver, fraction: alpha)
    }
}
