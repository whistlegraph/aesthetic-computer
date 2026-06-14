// MascotView.swift — the JukeWizard mascot drawn as a faint full-window
// watermark behind the working UI (same treatment as wave/clip-wizard).
// PNG generated via bin/gen-mascot.mjs; degrades to empty if absent.
// Never intercepts mouse events (hitTest → nil) so the controls on top
// stay fully clickable.
import AppKit

final class MascotView: NSView {
    private let image: NSImage? = {
        let bundle = Bundle.module
        if let url = bundle.url(forResource: "jukewizard-mascot", withExtension: "png", subdirectory: "Assets"),
           let img = NSImage(contentsOf: url) { return img }
        if let url = bundle.url(forResource: "jukewizard-mascot", withExtension: "png"),
           let img = NSImage(contentsOf: url) { return img }
        let here = FileManager.default.currentDirectoryPath
        return NSImage(contentsOfFile: "\(here)/juke-wizard/Sources/JukeWizard/Assets/jukewizard-mascot.png")
    }()

    override func hitTest(_ point: NSPoint) -> NSView? { nil }
    override var wantsDefaultClipping: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        guard let img = image else { return }
        let imgAR = img.size.width / img.size.height
        let viewAR = bounds.width / bounds.height
        var drawRect = bounds
        if imgAR > viewAR {
            let w = bounds.height * imgAR
            drawRect = NSRect(x: (bounds.width - w) * 0.5, y: 0, width: w, height: bounds.height)
        } else {
            let h = bounds.width / imgAR
            drawRect = NSRect(x: 0, y: (bounds.height - h) * 0.5, width: bounds.width, height: h)
        }
        img.draw(in: drawRect, from: .zero, operation: .sourceOver, fraction: 0.10)
    }
}
