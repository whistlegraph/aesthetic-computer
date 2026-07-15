import AppKit
import ApplicationServices
import Carbon.HIToolbox
import CoreGraphics

/// Lets Terminal/iTerm2 handle their native Command-Plus/Minus font zoom, then
/// restores the focused window's exact pixel frame. Terminal normally expresses
/// its frame in character cells, so changing the font also grows or shrinks the
/// window; Slab's tiled agent wall wants typography and geometry to be separate.
///
/// The event tap is observational: it never consumes or synthesizes a key event.
/// Consequently Command-Plus/Minus keeps its native per-window semantics, and
/// shortcuts in every non-terminal application remain completely untouched.
final class TerminalFontZoomGuard {
    private var tap: CFMachPort?
    private var source: CFRunLoopSource?
    private var frameObserver: AXObserver?
    private var frameObserverSource: CFRunLoopSource?
    private var lockedWindow: AXUIElement?
    private var lockedFrame: CGRect?
    private var lockGeneration = 0

    @discardableResult
    func start() -> Bool {
        guard tap == nil else { return true }
        let mask: CGEventMask = 1 << CGEventType.keyDown.rawValue
        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon else { return Unmanaged.passUnretained(event) }
            let guarder = Unmanaged<TerminalFontZoomGuard>
                .fromOpaque(refcon).takeUnretainedValue()
            guarder.handle(type: type, event: event)
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
            NSLog("slab terminal font zoom: event tap creation failed")
            return false
        }
        tap = port
        let runLoopSource = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, port, 0)
        source = runLoopSource
        CFRunLoopAddSource(CFRunLoopGetMain(), runLoopSource, .commonModes)
        CGEvent.tapEnable(tap: port, enable: true)
        return true
    }

    func stop() {
        endFrameLock()
        if let source { CFRunLoopRemoveSource(CFRunLoopGetMain(), source, .commonModes) }
        if let tap { CGEvent.tapEnable(tap: tap, enable: false) }
        source = nil
        tap = nil
    }

    /// Runs at the head of the session event stream, before Terminal receives
    /// the key. That timing is essential: an NSEvent global monitor fires late
    /// enough that Terminal may already have changed its character-cell frame.
    private func handle(type: CGEventType, event: CGEvent) {
        if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
            if let tap { CGEvent.tapEnable(tap: tap, enable: true) }
            return
        }
        guard type == .keyDown else { return }
        let flags = event.flags
        let keyCode = event.getIntegerValueField(.keyboardEventKeycode)
        guard flags.contains(.maskCommand),
              !flags.contains(.maskAlternate), !flags.contains(.maskControl),
              Self.isFontZoomKey(keyCode),
              let app = NSWorkspace.shared.frontmostApplication,
              let bundle = app.bundleIdentifier,
              bundle == "com.apple.Terminal" || bundle == "com.googlecode.iterm2",
              let window = Self.focusedWindow(pid: app.processIdentifier),
              let frame = Self.frame(of: window)
        else { return }

        beginFrameLock(window: window, frame: frame, pid: app.processIdentifier)
    }

    private static func isFontZoomKey(_ keyCode: Int64) -> Bool {
        keyCode == Int64(kVK_ANSI_Equal)
            || keyCode == Int64(kVK_ANSI_Minus)
            || keyCode == Int64(kVK_ANSI_KeypadPlus)
            || keyCode == Int64(kVK_ANSI_KeypadMinus)
    }

    private static func focusedWindow(pid: pid_t) -> AXUIElement? {
        let app = AXUIElementCreateApplication(pid)
        var ref: CFTypeRef?
        guard AXUIElementCopyAttributeValue(
            app, kAXFocusedWindowAttribute as CFString, &ref
        ) == .success, let ref else { return nil }
        return (ref as! AXUIElement)
    }

    private static func frame(of window: AXUIElement) -> CGRect? {
        var posRef: CFTypeRef?
        var sizeRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(
                window, kAXPositionAttribute as CFString, &posRef) == .success,
              AXUIElementCopyAttributeValue(
                window, kAXSizeAttribute as CFString, &sizeRef) == .success,
              let posRef, let sizeRef,
              CFGetTypeID(posRef) == AXValueGetTypeID(),
              CFGetTypeID(sizeRef) == AXValueGetTypeID()
        else { return nil }
        var origin = CGPoint.zero
        var size = CGSize.zero
        guard AXValueGetValue(posRef as! AXValue, .cgPoint, &origin),
              AXValueGetValue(sizeRef as! AXValue, .cgSize, &size)
        else { return nil }
        return CGRect(origin: origin, size: size)
    }

    private static func setFrame(_ frame: CGRect, of window: AXUIElement) {
        var origin = frame.origin
        var size = frame.size
        if let value = AXValueCreate(.cgPoint, &origin) {
            AXUIElementSetAttributeValue(
                window, kAXPositionAttribute as CFString, value)
        }
        if let value = AXValueCreate(.cgSize, &size) {
            AXUIElementSetAttributeValue(
                window, kAXSizeAttribute as CFString, value)
        }
    }

    /// Hold one window at its pre-shortcut frame while Terminal performs its
    /// character-cell reflow. AX resize/move notifications arrive as Terminal
    /// attempts each geometry write, letting us reject the write immediately
    /// instead of visibly snapping back on a polling schedule.
    private func beginFrameLock(window: AXUIElement, frame: CGRect, pid: pid_t) {
        // Repeated presses (and keyboard auto-repeat) must extend the existing
        // lock. Tearing down/recreating the AX observer on every key leaves a
        // small unguarded gap in which Terminal can resize on the second hit.
        if let currentWindow = lockedWindow,
           frameObserver != nil,
           CFEqual(currentWindow, window) {
            lockGeneration += 1
            scheduleFrameUnlock(generation: lockGeneration)
            return
        }

        endFrameLock()
        lockedWindow = window
        lockedFrame = frame
        lockGeneration += 1
        let generation = lockGeneration

        let callback: AXObserverCallback = { _, _, _, refcon in
            guard let refcon else { return }
            let guarder = Unmanaged<TerminalFontZoomGuard>
                .fromOpaque(refcon).takeUnretainedValue()
            guarder.restoreLockedFrame()
        }
        var observer: AXObserver?
        guard AXObserverCreate(pid, callback, &observer) == .success,
              let observer
        else {
            // AX trust can go stale after a signing change. Keep one final
            // correction as a graceful fallback instead of doing nothing.
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.45) { [weak self] in
                guard self?.lockGeneration == generation else { return }
                self?.restoreLockedFrame()
                self?.endFrameLock()
            }
            return
        }
        frameObserver = observer
        let refcon = Unmanaged.passUnretained(self).toOpaque()
        AXObserverAddNotification(
            observer, window, kAXMovedNotification as CFString, refcon)
        AXObserverAddNotification(
            observer, window, kAXResizedNotification as CFString, refcon)
        let observerSource = AXObserverGetRunLoopSource(observer)
        frameObserverSource = observerSource
        CFRunLoopAddSource(CFRunLoopGetMain(), observerSource, .commonModes)

        scheduleFrameUnlock(generation: generation)
    }

    /// A held key advances the generation for every repeat. Only the most
    /// recent hit may release the observer, after Terminal has gone quiet.
    private func scheduleFrameUnlock(generation: Int) {
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.70) { [weak self] in
            guard self?.lockGeneration == generation else { return }
            self?.restoreLockedFrame()
            self?.endFrameLock()
        }
    }

    private func restoreLockedFrame() {
        guard let window = lockedWindow, let target = lockedFrame else { return }
        if let current = Self.frame(of: window),
           abs(current.minX - target.minX) < 0.5,
           abs(current.minY - target.minY) < 0.5,
           abs(current.width - target.width) < 0.5,
           abs(current.height - target.height) < 0.5 {
            return
        }
        Self.setFrame(target, of: window)
    }

    private func endFrameLock() {
        if let source = frameObserverSource {
            CFRunLoopRemoveSource(CFRunLoopGetMain(), source, .commonModes)
        }
        frameObserverSource = nil
        frameObserver = nil
        lockedWindow = nil
        lockedFrame = nil
    }

    deinit { stop() }
}
