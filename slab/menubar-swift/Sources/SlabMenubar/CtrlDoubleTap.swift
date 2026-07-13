import AppKit
import CoreGraphics

/// Global "tap ⌃ twice, quickly, on its own" detector.
///
/// A `CGEvent.tapCreate` listener, not `NSEvent.addGlobalMonitorForEvents`:
/// the monitor API silently delivers zero events after a codesign rebuild even
/// when `AXIsProcessTrusted()` is true, while `tapCreate` fails loudly (returns
/// nil) so a broken Accessibility grant is visible instead of mysterious. Same
/// lesson MenuBandLauncher learned; see its header.
///
/// "On its own" is the whole difficulty. ⌃ is a chord key — emacs and the shell
/// lean on it constantly — so a naive double-tap would fire on ⌃X ⌃S. Two
/// filters keep it quiet: any *other* modifier held alongside ⌃ disqualifies the
/// press, and any key or click that lands *while* ⌃ is down cancels the pending
/// tap. What survives is a bare ⌃ pressed and released twice with nothing in
/// between, which nothing on macOS otherwise claims.
final class CtrlDoubleTap {
    /// Left and right ⌃. Some keyboards report 59 for both, so we accept either
    /// keycode and never try to tell the sides apart — any two ⌃ taps count.
    private static let leftControlKeyCode: Int64 = 59
    private static let rightControlKeyCode: Int64 = 62
    /// Long enough for a relaxed double-tap, short enough that two unrelated,
    /// incidental ⌃ presses don't pair up.
    private static let window: CFTimeInterval = 0.40

    private let onDoubleTap: () -> Void
    private var tap: CFMachPort?
    private var source: CFRunLoopSource?
    private var lastTapAt: CFTimeInterval = 0

    init(onDoubleTap: @escaping () -> Void) {
        self.onDoubleTap = onDoubleTap
    }

    /// Returns false if the tap couldn't be created — which in practice always
    /// means Accessibility trust is missing or stale for this binary.
    @discardableResult
    func start() -> Bool {
        guard tap == nil else { return true }
        let mask: CGEventMask =
            (1 << CGEventType.flagsChanged.rawValue) |
            (1 << CGEventType.keyDown.rawValue) |
            (1 << CGEventType.leftMouseDown.rawValue) |
            (1 << CGEventType.rightMouseDown.rawValue)

        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon = refcon else { return Unmanaged.passUnretained(event) }
            let me = Unmanaged<CtrlDoubleTap>.fromOpaque(refcon).takeUnretainedValue()
            me.handle(type: type, event: event)
            return Unmanaged.passUnretained(event)
        }

        guard let port = CGEvent.tapCreate(
            tap: .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .listenOnly,
            eventsOfInterest: mask,
            callback: callback,
            userInfo: Unmanaged.passUnretained(self).toOpaque()
        ) else {
            NSLog("slab zoom lens: ⌃⌃ tap creation failed — Accessibility not trusted?")
            return false
        }
        tap = port
        let src = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, port, 0)
        source = src
        // Listen-only, so a slow callback can't stall input — the main runloop
        // is fine here (unlike the lens's consuming pointer tap, which gets its
        // own thread).
        CFRunLoopAddSource(CFRunLoopGetMain(), src, .commonModes)
        CGEvent.tapEnable(tap: port, enable: true)
        return true
    }

    func stop() {
        if let src = source { CFRunLoopRemoveSource(CFRunLoopGetMain(), src, .commonModes) }
        if let port = tap { CGEvent.tapEnable(tap: port, enable: false) }
        source = nil
        tap = nil
    }

    private func handle(type: CGEventType, event: CGEvent) {
        if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
            if let port = tap { CGEvent.tapEnable(tap: port, enable: true) }
            return
        }

        // Anything typed or clicked while ⌃ is held means this ⌃ is a chord's
        // modifier, not a tap. Burn the pending half so ⌃X ⌃S can't pair.
        if type == .keyDown || type == .leftMouseDown || type == .rightMouseDown {
            if event.flags.contains(.maskControl) { lastTapAt = 0 }
            return
        }

        guard type == .flagsChanged else { return }
        let keyCode = event.getIntegerValueField(.keyboardEventKeycode)
        guard keyCode == Self.leftControlKeyCode || keyCode == Self.rightControlKeyCode else { return }

        // .maskControl is set on the press edge and cleared on release; we only
        // want the press.
        guard event.flags.contains(.maskControl) else { return }

        // Bare ⌃ only. A ⌃⌘ press is a chord in progress, and letting it arm a
        // tap would make the *next* bare ⌃ fire the lens.
        let others: CGEventFlags = [.maskShift, .maskAlternate, .maskCommand,
                                    .maskAlphaShift, .maskSecondaryFn]
        guard event.flags.intersection(others).isEmpty else {
            lastTapAt = 0
            return
        }

        let now = CACurrentMediaTime()
        if now - lastTapAt <= Self.window {
            lastTapAt = 0
            DispatchQueue.main.async { [weak self] in self?.onDoubleTap() }
        } else {
            lastTapAt = now
        }
    }
}
