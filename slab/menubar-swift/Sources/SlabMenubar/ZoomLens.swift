import AppKit
import ApplicationServices
import CoreGraphics

/// ⌃⌃ zooms the screen in on the window under the pointer, centred on it. ⌃⌃
/// again zooms back out.
///
/// This drives macOS's *real* Accessibility Zoom rather than rendering a
/// magnifier of our own, and that is the entire point. A userspace lens has to
/// re-composite the screen every frame (capture → GPU → transform) and hand-draw
/// the cursor from mouse events, and both sit on top of latency the real zoom
/// doesn't have — CloseView lives inside the window server, where the cursor is
/// composited natively and there is no capture round-trip. We built the
/// userspace one first. It was janky, and no amount of polish gets it under that
/// floor.
///
/// So: don't rebuild it, aim it. Two things had to be true, and both were worth
/// establishing by experiment rather than assumption:
///
///   • Synthetic ⌃+scroll — the gesture a human uses — does NOT reach CloseView;
///     it listens below the level `CGEvent.post` injects at. Its *keyboard*
///     shortcut (⌥⌘8) does, because that runs through the ordinary system hotkey
///     path. So the toggle is a synthesized ⌥⌘8, which needs
///     `closeViewHotkeysEnabled` — see `ensureHotkeysEnabled()`.
///
///   • `UAZoomChangeFocus` is the public API for pointing the zoom at a rect.
///     It pans the viewport; it cannot set magnification. So the *framing* is
///     ours (centred on the window you're pointing at) and the *factor* stays
///     whatever the user set in System Settings — which is the right call anyway:
///     it's their preference, not ours to override.
///
/// `UAZoomEnabled()` reads as "currently zoomed in", not "feature available" —
/// verified by watching it flip false→true→false across a toggle. That makes it
/// our state, so the lens stays in sync even when the user zooms by hand.
enum ZoomLens {
    /// kVK_ANSI_8 — with ⌥⌘, CloseView's zoom toggle.
    private static let eightKeyCode: CGKeyCode = 0x1C

    static var isZoomed: Bool { UAZoomEnabled() }

    static func toggle() {
        if isZoomed {
            pressZoomToggle()
            return
        }
        // Grab the window before zooming: once the view magnifies, the pointer's
        // screen position still maps to the same global coordinates, but reading
        // it first keeps the intent obvious.
        let target = windowUnderCursor(excluding: getpid())
        pressZoomToggle()

        guard var rect = target else { return }
        // CloseView needs a moment to actually be zoomed before it will accept a
        // focus change; asking too early is a no-op.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.12) {
            let err = UAZoomChangeFocus(&rect, &rect,
                                        UAZoomChangeFocusType(kUAZoomFocusTypeOther))
            if err != noErr { NSLog("slab zoom lens: UAZoomChangeFocus failed (\(err))") }
        }
    }

    private static func pressZoomToggle() {
        let source = CGEventSource(stateID: .hidSystemState)
        for down in [true, false] {
            guard let event = CGEvent(keyboardEventSource: source,
                                      virtualKey: eightKeyCode, keyDown: down) else { continue }
            event.flags = [.maskAlternate, .maskCommand]
            event.post(tap: .cghidEventTap)
        }
    }

    /// ⌥⌘8 only reaches CloseView if the zoom keyboard shortcuts are switched on,
    /// and they're off by default. Turn them on once, then poke universalaccessd
    /// — it caches the preference and won't notice a write on its own. Only when
    /// we actually changed something: the daemon respawns on demand, but kicking
    /// it on every launch would be gratuitous.
    static func ensureHotkeysEnabled() {
        let domain = "com.apple.universalaccess" as CFString
        let key = "closeViewHotkeysEnabled" as CFString
        CFPreferencesAppSynchronize(domain)
        let current = CFPreferencesCopyAppValue(key, domain) as? NSNumber
        guard current?.boolValue != true else { return }

        CFPreferencesSetAppValue(key, kCFBooleanTrue, domain)
        CFPreferencesAppSynchronize(domain)
        _ = ShellRunner.run("/usr/bin/killall", args: ["universalaccessd"], timeout: 5)
        NSLog("slab zoom lens: enabled Accessibility Zoom keyboard shortcuts (⌥⌘8)")
    }

    /// The topmost ordinary window under the pointer, in CoreGraphics globals —
    /// the same space `UAZoomChangeFocus` wants. `optionOnScreenOnly` hands the
    /// list back already sorted front-to-back, so the first hit wins.
    private static func windowUnderCursor(excluding pid: pid_t) -> CGRect? {
        guard let point = CGEvent(source: nil)?.location,
              let info = CGWindowListCopyWindowInfo(
                [.optionOnScreenOnly, .excludeDesktopElements],
                kCGNullWindowID) as? [[String: Any]] else { return nil }
        for w in info {
            guard let layer = w[kCGWindowLayer as String] as? Int, layer == 0,
                  let owner = w[kCGWindowOwnerPID as String] as? pid_t, owner != pid,
                  let b = w[kCGWindowBounds as String] as? [String: CGFloat],
                  let x = b["X"], let y = b["Y"],
                  let width = b["Width"], let height = b["Height"] else { continue }
            // Anything smaller than this is a shadow, tooltip or other chrome that
            // happens to sit at layer 0 — never what someone means by "this window".
            guard width >= 64, height >= 64 else { continue }
            let rect = CGRect(x: x, y: y, width: width, height: height)
            if rect.contains(point) { return rect }
        }
        return nil
    }
}
