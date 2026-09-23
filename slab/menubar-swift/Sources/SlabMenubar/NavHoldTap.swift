import AppKit
import ApplicationServices
import CoreGraphics

/// Global "bare ⌘⌥ is being held" detector. A `CGEvent.tapCreate` listener
/// for the same reason CtrlDoubleTap is one: `NSEvent` global monitors go
/// silent after codesign rebuilds, while tapCreate fails loudly when the
/// Accessibility grant is broken.
final class NavHoldTap {
    private let onHoldStart: () -> Void
    private let onHoldEnd: () -> Void
    private let onChordKey: () -> Void
    private let onPointerDown: () -> Void
    private var tap: CFMachPort?
    private var source: CFRunLoopSource?
    private var holding = false
    private var releaseTimer: Timer?
    private var holdStartedAt: TimeInterval = 0
    private var generation = 0
    private var requiresRelease = false
    // A remote keyboard can lose both its release event and its modifier state.
    // Require a fresh press after this fail-safe, even if macOS still says held.
    private static let maximumHold: TimeInterval = 30

    init(onHoldStart: @escaping () -> Void,
         onHoldEnd: @escaping () -> Void,
         onChordKey: @escaping () -> Void,
         onPointerDown: @escaping () -> Void) {
        self.onHoldStart = onHoldStart
        self.onHoldEnd = onHoldEnd
        self.onChordKey = onChordKey
        self.onPointerDown = onPointerDown
    }

    @discardableResult
    func start() -> Bool {
        guard tap == nil else { return true }
        guard ProcessInfo.processInfo.environment["SLAB_DISABLE_EVENT_TAPS"] != "1" else {
            NSLog("slab nav hint: event tap disabled for this host")
            return false
        }
        guard AXIsProcessTrusted() else {
            NSLog("slab nav hint: Accessibility not trusted; skipping event tap")
            return false
        }
        let mask: CGEventMask =
            (1 << CGEventType.flagsChanged.rawValue) |
            (1 << CGEventType.keyDown.rawValue) |
            (1 << CGEventType.leftMouseDown.rawValue) |
            (1 << CGEventType.rightMouseDown.rawValue) |
            (1 << CGEventType.otherMouseDown.rawValue)

        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon = refcon else { return Unmanaged.passUnretained(event) }
            let me = Unmanaged<NavHoldTap>.fromOpaque(refcon).takeUnretainedValue()
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
            NSLog("slab nav hint: ⌘⌥ hold tap creation failed — Accessibility not trusted?")
            return false
        }
        tap = port
        let src = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, port, 0)
        source = src
        CFRunLoopAddSource(CFRunLoopGetMain(), src, .commonModes)
        CGEvent.tapEnable(tap: port, enable: true)
        return true
    }

    func stop() {
        setHolding(false)
        requiresRelease = false
        if let src = source { CFRunLoopRemoveSource(CFRunLoopGetMain(), src, .commonModes) }
        if let port = tap { CGEvent.tapEnable(tap: port, enable: false) }
        source = nil
        tap = nil
    }

    private func handle(type: CGEventType, event: CGEvent) {
        if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
            // Releases may have been dropped while the tap was disabled.
            cancelHold()
            if let port = tap { CGEvent.tapEnable(tap: port, enable: true) }
            return
        }
        switch type {
        case .flagsChanged:
            // Bare ⌘⌥ only: ⇧ or ⌃ alongside means some other chord family.
            // Fn is deliberately NOT a disqualifier — some keyboards raise it
            // alongside arrows, and it must not end the hold mid-navigation.
            let bare = Self.isBareChord(event.flags)
            if !bare { requiresRelease = false }
            setHolding(bare && !requiresRelease)
        case .keyDown:
            if event.getIntegerValueField(.keyboardEventKeycode) == 53 {
                cancelHold() // Escape always dismisses; never consume it.
            } else if holding {
                DispatchQueue.main.async(execute: onChordKey)
            }
        case .leftMouseDown, .rightMouseDown, .otherMouseDown:
            // A click while holding ⌘⌥ is some other gesture entirely
            // (Finder's ⌘⌥-drag makes aliases) — stand down for this hold.
            if holding { DispatchQueue.main.async(execute: onPointerDown) }
        default:
            break
        }
    }

    private static func isBareChord(_ flags: CGEventFlags) -> Bool {
        flags.contains(.maskCommand) && flags.contains(.maskAlternate)
            && !flags.contains(.maskShift) && !flags.contains(.maskControl)
    }

    private func setHolding(_ value: Bool) {
        guard value != holding else { return }
        holding = value
        generation += 1
        let revision = generation
        releaseTimer?.invalidate()
        releaseTimer = nil
        if value {
            holdStartedAt = ProcessInfo.processInfo.systemUptime
            let timer = Timer(timeInterval: 0.15, repeats: true) { [weak self] _ in
                // Session state includes Deskflow's injected keys; HID state
                // only sees the keyboard physically attached to this machine.
                self?.reconcile(flags: CGEventSource.flagsState(.combinedSessionState),
                                now: ProcessInfo.processInfo.systemUptime)
            }
            timer.tolerance = 0.03
            releaseTimer = timer
            RunLoop.main.add(timer, forMode: .common)
        }
        DispatchQueue.main.async { [weak self] in
            guard let self, self.generation == revision else { return }
            if self.holding { self.onHoldStart() } else { self.onHoldEnd() }
        }
    }

    private func cancelHold() {
        requiresRelease = true
        setHolding(false)
    }

    private func reconcile(flags: CGEventFlags, now: TimeInterval) {
        guard holding else { return }
        if !Self.isBareChord(flags) {
            setHolding(false)
        } else if now - holdStartedAt >= Self.maximumHold {
            cancelHold()
        }
    }

}
