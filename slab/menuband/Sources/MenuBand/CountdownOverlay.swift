import AppKit

/// Big on-screen **3 · 2 · 1** numerals for the record count-in. A full-screen,
/// click-through, all-spaces panel that punches each number in with a quick
/// scale-pop. Kept separate from `FocusFlashOverlay` on purpose: that overlay
/// fades its whole window for the red "charge" glow (max ~0.30 alpha), so the
/// numbers live here at full brightness, layered on top.
///
/// Non-activating + `.screenSaver` level + click-through, so it never steals
/// focus or eats the keystroke driving the count-in — matches FocusFlashOverlay.
final class CountdownOverlay: NSPanel {
    static let shared = CountdownOverlay()
    private let label = CATextLayer()

    private init() {
        super.init(contentRect: .zero,
                   styleMask: [.borderless, .nonactivatingPanel],
                   backing: .buffered,
                   defer: false)
        isOpaque = false
        backgroundColor = .clear
        hasShadow = false
        level = .screenSaver
        ignoresMouseEvents = true
        hidesOnDeactivate = false
        collectionBehavior = [.canJoinAllSpaces, .stationary,
                              .ignoresCycle, .fullScreenAuxiliary]
        let v = NSView()
        v.wantsLayer = true
        label.alignmentMode = .center
        label.foregroundColor = NSColor.white.cgColor
        label.shadowColor = NSColor.black.cgColor
        label.shadowOpacity = 0.5
        label.shadowRadius = 14
        label.shadowOffset = CGSize(width: 0, height: -4)
        label.contentsScale = NSScreen.main?.backingScaleFactor ?? 2
        label.opacity = 0
        v.layer?.addSublayer(label)
        contentView = v
    }

    /// Punch a number on screen (scale-pop + quick fade-in). Stays lit until the
    /// next `show(_:)` or `clear()`. Main-thread only (called from the count-in
    /// work items, which run on the main queue).
    func show(_ n: String) {
        guard let screen = NSScreen.main else { return }
        setFrame(screen.frame, display: true)
        orderFrontRegardless()
        let side = min(screen.frame.width, screen.frame.height)
        let fontSize = side * 0.28
        let bandH = fontSize * 1.25
        label.font = NSFont.systemFont(ofSize: fontSize, weight: .heavy)
        label.fontSize = fontSize
        label.string = n
        // Full-width band so the glyph centers horizontally; the layer scales
        // around its own center (anchorPoint 0.5,0.5) = screen center.
        label.frame = CGRect(x: 0, y: (screen.frame.height - bandH) / 2,
                             width: screen.frame.width, height: bandH)
        label.removeAllAnimations()
        label.opacity = 1
        let pop = CABasicAnimation(keyPath: "transform.scale")
        pop.fromValue = 1.35
        pop.toValue = 1.0
        pop.duration = 0.24
        pop.timingFunction = CAMediaTimingFunction(name: .easeOut)
        let fade = CABasicAnimation(keyPath: "opacity")
        fade.fromValue = 0.0
        fade.toValue = 1.0
        fade.duration = 0.12
        label.add(pop, forKey: "pop")
        label.add(fade, forKey: "in")
    }

    /// Clear the countdown (downbeat reached, or count-in aborted).
    func clear() {
        label.removeAllAnimations()
        label.opacity = 0
        orderOut(nil)
    }
}
