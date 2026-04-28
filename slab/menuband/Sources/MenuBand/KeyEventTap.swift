import Foundation
import CoreGraphics

// Low-latency global keyboard listener. Runs on a dedicated user-interactive
// thread so key events bypass the main run loop entirely.
final class KeyEventTap {
    /// Returns true to CONSUME the event (sink it — focused app won't see it),
    /// false to let it propagate normally.
    typealias Handler = (_ keyCode: UInt16, _ isDown: Bool, _ isRepeat: Bool, _ flags: CGEventFlags) -> Bool

    private var tap: CFMachPort?
    private var runLoopSource: CFRunLoopSource?
    private var thread: Thread?
    private var threadRunLoop: CFRunLoop?
    private let handler: Handler

    init(handler: @escaping Handler) {
        self.handler = handler
    }

    func start() -> Bool {
        guard tap == nil else { return true }

        let mask: CGEventMask =
            (1 << CGEventType.keyDown.rawValue) |
            (1 << CGEventType.keyUp.rawValue) |
            (1 << CGEventType.tapDisabledByTimeout.rawValue) |
            (1 << CGEventType.tapDisabledByUserInput.rawValue)

        let opaque = Unmanaged.passRetained(self).toOpaque()

        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon = refcon else { return Unmanaged.passUnretained(event) }
            let me = Unmanaged<KeyEventTap>.fromOpaque(refcon).takeUnretainedValue()

            if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
                if let tap = me.tap { CGEvent.tapEnable(tap: tap, enable: true) }
                return Unmanaged.passUnretained(event)
            }
            if type == .keyDown || type == .keyUp {
                let kc = UInt16(event.getIntegerValueField(.keyboardEventKeycode))
                let isRepeat = event.getIntegerValueField(.keyboardEventAutorepeat) != 0
                let consume = me.handler(kc, type == .keyDown, isRepeat, event.flags)
                if consume { return nil }  // sink — don't propagate to focused app
            }
            return Unmanaged.passUnretained(event)
        }

        guard let tap = CGEvent.tapCreate(
            tap: .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .defaultTap,  // .defaultTap allows consuming events; .listenOnly does not
            eventsOfInterest: mask,
            callback: callback,
            userInfo: opaque
        ) else {
            Unmanaged<KeyEventTap>.fromOpaque(opaque).release()
            return false
        }
        self.tap = tap

        let source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, tap, 0)
        self.runLoopSource = source

        let thread = Thread { [weak self] in
            guard let self = self, let source = self.runLoopSource, let tap = self.tap else { return }
            self.threadRunLoop = CFRunLoopGetCurrent()
            CFRunLoopAddSource(CFRunLoopGetCurrent(), source, .commonModes)
            CGEvent.tapEnable(tap: tap, enable: true)
            CFRunLoopRun()
        }
        thread.qualityOfService = .userInteractive
        thread.name = "MenuBand-KeyTap"
        thread.start()
        self.thread = thread
        return true
    }

    func stop() {
        if let tap = tap { CGEvent.tapEnable(tap: tap, enable: false) }
        if let rl = threadRunLoop { CFRunLoopStop(rl) }
        if tap != nil {
            // Balance the passRetained from start().
            Unmanaged.passUnretained(self).release()
        }
        tap = nil
        runLoopSource = nil
        thread = nil
        threadRunLoop = nil
    }

    deinit { stop() }
}
