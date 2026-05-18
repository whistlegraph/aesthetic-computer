import AppKit

/// Photo-Booth-style camera flash for arming the board. A
/// full-screen, click-through, all-spaces panel filled with the
/// macOS **system accent color** (lightened toward white so it
/// reads as a luminous flash, not a flat tint). Snaps to peak
/// instantly — like a real strobe — then fades out fast.
///
/// Click-through + non-activating + `.screenSaver` level so it
/// never steals focus or eats the keystroke that triggered it;
/// the panel just paints over everything for a beat and leaves.
final class FocusFlashOverlay: NSPanel {
    static let shared = FocusFlashOverlay()

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
        alphaValue = 0
        collectionBehavior = [.canJoinAllSpaces,
                              .stationary,
                              .ignoresCycle,
                              .fullScreenAuxiliary]
        let v = NSView()
        v.wantsLayer = true
        contentView = v
    }

    /// Quick full-screen color pulse on the screen the user is
    /// looking at. `rising == true` (board START) → saturated blue;
    /// `false` (board STOP) → saturated red. It's a translucent
    /// glow that blends over the system UI — capped well below
    /// opaque so you always see through it — punched on for ~a
    /// frame then fast-faded. Never a stark white-out.
    func flash(rising: Bool) {
        guard let screen = NSScreen.main else { return }
        setFrame(screen.frame, display: true)
        guard let layer = contentView?.layer else { return }

        // Vivid, saturated start/stop colors — not the accent, not
        // washed toward white.
        let color: NSColor = rising
            ? NSColor(srgbRed: 0.06, green: 0.42, blue: 1.0, alpha: 1)   // blue = go
            : NSColor(srgbRed: 1.0,  green: 0.12, blue: 0.16, alpha: 1)  // red  = stop
        layer.backgroundColor = color.cgColor

        // Peak stays a translucent blend (never fully opaque over
        // the UI). Punch to peak instantly, then a fast ease-out
        // fade — reads as a single colored glow frame. Kill any
        // in-flight fade so rapid toggles re-pulse cleanly.
        let peak: CGFloat = 0.42
        layer.removeAllAnimations()
        alphaValue = peak
        orderFrontRegardless()
        NSAnimationContext.runAnimationGroup({ ctx in
            ctx.duration = 0.17
            ctx.timingFunction = CAMediaTimingFunction(name: .easeOut)
            animator().alphaValue = 0
        }, completionHandler: { [weak self] in
            guard let self = self else { return }
            if self.alphaValue <= 0.01 { self.orderOut(nil) }
        })
    }
}
