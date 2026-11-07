import AppKit
import Foundation
import CoreGraphics

// Low-latency global keyboard listener. Runs on a dedicated user-interactive
// thread so key events bypass the main run loop entirely.
final class KeyEventTap {
    fileprivate var lastAlphaShift = false   // Caps Lock state seen by the tap (see the system-defined guard)
    /// Returns true to CONSUME the event (sink it — focused app won't see it),
    /// false to let it propagate normally.
    typealias Handler = (_ keyCode: UInt16, _ isDown: Bool, _ isRepeat: Bool, _ flags: CGEventFlags) -> Bool

    private let lifecycleLock = NSLock()
    private var lifecycleGeneration: UInt64 = 0
    private var tap: CFMachPort?
    private var runLoopSource: CFRunLoopSource?
    private var thread: Thread?
    private var threadRunLoop: CFRunLoop?
    private let handler: Handler
    private let diagnosticSource: SessionDiagnostics.Source
    private let focusedProcessOnly: Bool
    var onFlagsChanged: ((CGEventFlags, CGEventTimestamp) -> Void)?
    var onDiscontinuity: (() -> Void)?
    /// The keyboard's media row — volume, mute, brightness, keyboard light,
    /// play — arrives as NSSystemDefined (type 14, subtype 8), never as a
    /// keyDown, so the key handler above never sees it. Set before `start()`
    /// to receive (keyType, isDown, flags); return true to consume. keyType
    /// is NX_KEYTYPE_*: 0/1 volume, 2/3 brightness, 7 mute, 16–20 transport,
    /// 21/22 keyboard light.
    var onSystemDefined: ((_ keyType: Int, _ isDown: Bool, _ flags: CGEventFlags) -> Bool)?
    private static let systemDefinedType: UInt32 = 14
    private static let auxControlSubtype = 8

    init(source: SessionDiagnostics.Source = .globalKeyboard,
         focusedProcessOnly: Bool = false, handler: @escaping Handler) {
        self.diagnosticSource = source
        self.focusedProcessOnly = focusedProcessOnly
        self.handler = handler
    }

    func start() -> Bool {
        lifecycleLock.lock(); defer { lifecycleLock.unlock() }
        guard tap == nil else { return true }
        lifecycleGeneration &+= 1
        let generation = lifecycleGeneration

        var mask: CGEventMask =
            (1 << CGEventType.keyDown.rawValue) |
            (1 << CGEventType.keyUp.rawValue) |
            (1 << CGEventType.tapDisabledByTimeout.rawValue) |
            (1 << CGEventType.tapDisabledByUserInput.rawValue)
        if onSystemDefined != nil {
            mask |= (1 << CGEventMask(Self.systemDefinedType))
        }
        if onFlagsChanged != nil { mask |= (1 << CGEventType.flagsChanged.rawValue) }

        let opaque = Unmanaged.passRetained(self).toOpaque()

        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon = refcon else { return Unmanaged.passUnretained(event) }
            let me = Unmanaged<KeyEventTap>.fromOpaque(refcon).takeUnretainedValue()

            if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
                me.onDiscontinuity?()
                SessionDiagnostics.shared.record(.tapDisabled, reason: type == .tapDisabledByTimeout ? .timeout : .userInput, source: me.diagnosticSource)
                me.lifecycleLock.lock()
                if let tap = me.tap {
                    CGEvent.tapEnable(tap: tap, enable: true)
                    SessionDiagnostics.shared.record(.tapReenabled, source: me.diagnosticSource, state: CGEvent.tapIsEnabled(tap: tap))
                }
                me.lifecycleLock.unlock()
                return Unmanaged.passUnretained(event)
            }
            if me.focusedProcessOnly {
                // Annotated events carry the destination PID. Never infer
                // focus from stale main-thread NSApp state while UI is busy.
                let target = event.getIntegerValueField(.eventTargetUnixProcessID)
                guard target == Int64(ProcessInfo.processInfo.processIdentifier) else {
                    if target > 0 { me.onDiscontinuity?() }
                    return Unmanaged.passUnretained(event)
                }
            }
            if type == .flagsChanged {
                me.onFlagsChanged?(event.flags, event.timestamp)
                return Unmanaged.passUnretained(event)
            }
            if type == .keyDown || type == .keyUp {
                let kc = UInt16(event.getIntegerValueField(.keyboardEventKeycode))
                let isRepeat = event.getIntegerValueField(.keyboardEventAutorepeat) != 0
                let consume = InputLatencyProbe.withEvent(at: Double(event.timestamp) / 1_000_000_000,
                    down: type == .keyDown, repeated: isRepeat) {
                    me.handler(kc, type == .keyDown, isRepeat, event.flags)
                }
                if consume { return nil }  // sink — don't propagate to focused app
            }
            // NSEvent(cgEvent:) on this tap thread trips AppKit's main-queue assertion
            // when the event carries a Caps Lock transition (TSMSetCapsLockKeyTransitionDetected
            // → ProcessCapsLockSequenceKeyUp; neo crashed mid-piece, TIAT Oct 22). A
            // system-defined event with Caps Lock held, or across a Caps Lock change, is
            // never a media key we care about: let it pass untouched.
            let alpha = event.flags.contains(.maskAlphaShift)
            let alphaChanged = alpha != me.lastAlphaShift
            me.lastAlphaShift = alpha
            if type.rawValue == KeyEventTap.systemDefinedType, let onSystemDefined = me.onSystemDefined,
               !alpha, !alphaChanged,
               let ns = NSEvent(cgEvent: event), ns.subtype.rawValue == KeyEventTap.auxControlSubtype {
                let data1 = ns.data1
                let keyType = (data1 & 0xFFFF0000) >> 16
                let isDown = ((data1 & 0xFF00) >> 8) == 0xA
                if onSystemDefined(keyType, isDown, event.flags) { return nil }
            }
            return Unmanaged.passUnretained(event)
        }

        guard let tap = CGEvent.tapCreate(
            tap: focusedProcessOnly ? .cgAnnotatedSessionEventTap : .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .defaultTap,  // .defaultTap allows consuming events; .listenOnly does not
            eventsOfInterest: mask,
            callback: callback,
            userInfo: opaque
        ) else {
            SessionDiagnostics.shared.record(.tapFailed, source: diagnosticSource)
            Unmanaged<KeyEventTap>.fromOpaque(opaque).release()
            return false
        }
        self.tap = tap

        let source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, tap, 0)
        self.runLoopSource = source

        let thread = Thread { [weak self] in
            guard let self else { return }
            self.lifecycleLock.lock()
            guard self.lifecycleGeneration == generation,
                  let source = self.runLoopSource, let tap = self.tap else {
                self.lifecycleLock.unlock()
                return
            }
            self.threadRunLoop = CFRunLoopGetCurrent()
            CFRunLoopAddSource(CFRunLoopGetCurrent(), source, .commonModes)
            CGEvent.tapEnable(tap: tap, enable: true)
            self.lifecycleLock.unlock()
            SessionDiagnostics.shared.record(.tapStarted, source: self.diagnosticSource)
            CFRunLoopRun()
            CFRunLoopRemoveSource(CFRunLoopGetCurrent(), source, .commonModes)
        }
        thread.qualityOfService = .userInteractive
        thread.name = "MenuBand-KeyTap"
        thread.start()
        self.thread = thread
        return true
    }

    func stop() {
        lifecycleLock.lock()
        let wasStarted = tap != nil
        lifecycleGeneration &+= 1
        if tap != nil { SessionDiagnostics.shared.record(.tapStopped, source: diagnosticSource) }
        if let tap = tap { CGEvent.tapEnable(tap: tap, enable: false) }
        if let rl = threadRunLoop {
            // Also works when stop races the thread just before CFRunLoopRun.
            CFRunLoopPerformBlock(rl, CFRunLoopMode.commonModes.rawValue) { CFRunLoopStop(rl) }
            CFRunLoopWakeUp(rl)
        }
        tap = nil
        runLoopSource = nil
        thread = nil
        threadRunLoop = nil
        lifecycleLock.unlock()
        // The running thread retains self until its callback source is removed.
        if wasStarted { Unmanaged.passUnretained(self).release() }
    }

    deinit { stop() }
}
