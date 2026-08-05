import AppKit

/// The shared glass chassis behind Juke's controls. It mirrors Menu Band's
/// popover material while keeping Juke independently buildable as a package.
final class JukeLiquidSurface: NSView {
    private final class PassthroughTintView: NSView {
        override func hitTest(_ point: NSPoint) -> NSView? { nil }
    }

    private let backdrop: NSView
    private let tint = PassthroughTintView()

    override init(frame frameRect: NSRect) {
        if #available(macOS 26.0, *) {
            let glass = NSGlassEffectView()
            glass.style = .clear
            glass.tintColor = .clear
            backdrop = glass
        } else {
            let vibrancy = NSVisualEffectView()
            vibrancy.material = .popover
            vibrancy.blendingMode = .behindWindow
            vibrancy.state = .active
            backdrop = vibrancy
        }

        super.init(frame: frameRect)
        wantsLayer = true
        layer?.backgroundColor = NSColor.clear.cgColor

        // Keep the glass effect at full strength so copy behind the window is
        // refracted instead of competing with Juke. The explicit clear tint
        // above is what lets the desktop's actual color remain present.
        backdrop.alphaValue = 1
        backdrop.frame = bounds
        backdrop.autoresizingMask = [.width, .height]
        addSubview(backdrop)

        tint.wantsLayer = true
        tint.frame = bounds
        tint.autoresizingMask = [.width, .height]
        addSubview(tint)
    }

    required init?(coder: NSCoder) { fatalError() }

    override var isOpaque: Bool { false }

    func setTint(_ color: NSColor) {
        tint.layer?.backgroundColor = color.cgColor
    }
}
