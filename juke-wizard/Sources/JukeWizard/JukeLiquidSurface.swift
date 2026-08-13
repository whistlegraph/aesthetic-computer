import AppKit

/// The shared glass chassis behind Juke's controls. It mirrors Menu Band's
/// popover material while keeping Juke independently buildable as a package.
final class JukeLiquidSurface: NSView {
    private final class PassthroughTintView: NSView {
        override func hitTest(_ point: NSPoint) -> NSView? { nil }
    }

    private let backdrop: NSView
    /// A legibility floor between the glass and the controls. `.clear` glass
    /// refracts the desktop at full strength, and the chassis tint on top of
    /// it is only 4–10% opaque, so track names ended up competing with
    /// whatever wallpaper or window sat behind Juke. This scrim is the one
    /// surface guaranteeing text has something to sit on; it stays
    /// translucent enough that the glass still reads as glass.
    private let substrate = PassthroughTintView()
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

        substrate.wantsLayer = true
        substrate.frame = bounds
        substrate.autoresizingMask = [.width, .height]
        addSubview(substrate)

        tint.wantsLayer = true
        tint.frame = bounds
        tint.autoresizingMask = [.width, .height]
        addSubview(tint)

        applySubstrate()
    }

    required init?(coder: NSCoder) { fatalError() }

    override var isOpaque: Bool { false }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        applySubstrate()
    }

    private func applySubstrate() {
        let dark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        substrate.layer?.backgroundColor = (dark
            ? NSColor(srgbRed: 0.05, green: 0.07, blue: 0.08, alpha: 0.62)
            : NSColor(srgbRed: 0.99, green: 0.99, blue: 1.00, alpha: 0.66)).cgColor
    }

    func setTint(_ color: NSColor) {
        tint.layer?.backgroundColor = color.cgColor
    }
}
