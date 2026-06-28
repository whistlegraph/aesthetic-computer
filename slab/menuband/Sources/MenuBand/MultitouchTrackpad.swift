import Foundation

// MultitouchTrackpad — global trackpad-finger tap via Apple's PRIVATE
// MultitouchSupport.framework. Unlike NSTouch (which only reaches the
// frontmost app's first responder), this receives every finger on every
// trackpad regardless of which app is active — exactly what the pitch-bend
// needs, since Menu Band is a non-activating menubar panel that never owns
// activation at touch time.
//
// PRIVATE API — App Store forbidden (Review Guideline 2.5.1 + the App
// Sandbox blocks the multitouch HID device). Hence the whole file is gated
// out of the MAS build; the Developer-ID / direct-download build (no
// sandbox) is the only place it compiles or runs. We dlopen the framework
// rather than link it so there's no symbol dependency unless this path runs.
#if !MAC_APP_STORE

/// One finger as MultitouchSupport reports it. Layout MUST match the
/// framework's `MTTouch`/`Finger` struct byte-for-byte — only `normalized`
/// (0…1 position, origin bottom-left) is consumed here but every field is
/// declared so the offsets line up.
struct MTPoint { var x: Float; var y: Float }
struct MTReadout { var position: MTPoint; var velocity: MTPoint }
struct MTTouch {
    var frame: Int32
    var timestamp: Double
    var identifier: Int32
    var state: Int32          // 1 not-touching … 4 touching … 7 leaving
    var fingerID: Int32
    var handID: Int32
    var normalized: MTReadout
    var size: Float
    var zero1: Int32
    var angle: Float
    var majorAxis: Float
    var minorAxis: Float
    var absolute: MTReadout    // millimetres
    var zero2a: Int32
    var zero2b: Int32
    var zDensity: Float
}

/// C callback shape: `int (*)(MTDeviceRef, MTTouch*, int, double, int)`.
/// The contacts pointer crosses as a raw pointer (a typed Swift-struct
/// pointer isn't C-representable) and is rebound to MTTouch inside.
typealias MTContactCallback = @convention(c) (
    UnsafeMutableRawPointer?, UnsafeMutableRawPointer?, Int32, Double, Int32
) -> Int32

/// Free function so it's a valid C function pointer (no captured context);
/// forwards into the singleton, which the framework can't reach directly.
private func mtFrameCallback(
    _ device: UnsafeMutableRawPointer?,
    _ contacts: UnsafeMutableRawPointer?,
    _ numContacts: Int32,
    _ timestamp: Double,
    _ frame: Int32
) -> Int32 {
    let typed = contacts?.assumingMemoryBound(to: MTTouch.self)
    MultitouchTrackpad.shared.handle(contacts: typed,
                                     count: Int(numContacts),
                                     timestamp: timestamp)
    return 0
}

final class MultitouchTrackpad {
    static let shared = MultitouchTrackpad()

    /// Every finger currently on the trackpad, as absolute normalized points
    /// (0…1 each axis, origin bottom-left). Empty when no finger is down.
    /// Delivered on the MAIN thread (the framework calls back on its own
    /// thread; AppKit/audio state must be touched on main). This is the clean,
    /// focus-independent signal the pitch-bend / fx pad consumes — no pointer
    /// acceleration, unlike the dead NSTouch `TouchSensorView` path.
    var onFrame: (([CGPoint]) -> Void)?

    private var handle: UnsafeMutableRawPointer?
    private var devices: [UnsafeMutableRawPointer] = []
    private var started = false
    private var loggedCount = 0
    private var lastLogStamp: Double = 0

    private typealias CreateListFn = @convention(c) () -> Unmanaged<CFArray>?
    private typealias RegisterFn = @convention(c)
        (UnsafeMutableRawPointer, MTContactCallback) -> Void
    private typealias StartFn = @convention(c)
        (UnsafeMutableRawPointer, Int32) -> Void
    private typealias StopFn = @convention(c) (UnsafeMutableRawPointer) -> Void
    private typealias UnregisterFn = @convention(c) (UnsafeMutableRawPointer) -> Void

    /// Open the private framework, enumerate trackpads, register + start the
    /// frame callback on each. Idempotent. Returns false (and logs) if the
    /// framework or any required symbol is unavailable.
    @discardableResult
    func start() -> Bool {
        guard !started else { return true }
        let path = "/System/Library/PrivateFrameworks/" +
                   "MultitouchSupport.framework/MultitouchSupport"
        guard let h = dlopen(path, RTLD_NOW) else {
            NSLog("MenuBand MTouch: dlopen failed — %s",
                  dlerror().map { String(cString: $0) } ?? "unknown")
            return false
        }
        handle = h
        func sym(_ name: String) -> UnsafeMutableRawPointer? { dlsym(h, name) }
        guard let createSym = sym("MTDeviceCreateList"),
              let registerSym = sym("MTRegisterContactFrameCallback"),
              let startSym = sym("MTDeviceStart") else {
            NSLog("MenuBand MTouch: missing symbol(s) in MultitouchSupport")
            return false
        }
        let createList = unsafeBitCast(createSym, to: CreateListFn.self)
        let register = unsafeBitCast(registerSym, to: RegisterFn.self)
        let startDevice = unsafeBitCast(startSym, to: StartFn.self)

        guard let list = createList()?.takeRetainedValue() else {
            NSLog("MenuBand MTouch: MTDeviceCreateList returned nil")
            return false
        }
        let count = CFArrayGetCount(list)
        for i in 0..<count {
            guard let raw = CFArrayGetValueAtIndex(list, i) else { continue }
            let device = UnsafeMutableRawPointer(mutating: raw)
            register(device, mtFrameCallback)
            startDevice(device, 0)
            devices.append(device)
        }
        started = !devices.isEmpty
        NSLog("MenuBand MTouch: started on %d trackpad device(s)", devices.count)
        return started
    }

    /// Frame callback target. Collects every finger in contact (state 4 ==
    /// "touching") as a normalized point and hands the set to `onFrame` on the
    /// main thread. Throttles logging to ~10 Hz (frames arrive at 60–120 Hz).
    func handle(contacts: UnsafeMutablePointer<MTTouch>?, count: Int, timestamp: Double) {
        var points: [CGPoint] = []
        if let contacts {
            for i in 0..<count where contacts[i].state == 4 {
                let pos = contacts[i].normalized.position
                points.append(CGPoint(x: CGFloat(pos.x), y: CGFloat(pos.y)))
            }
        }
        if loggedCount < 200, !points.isEmpty, timestamp - lastLogStamp > 0.1 {
            lastLogStamp = timestamp
            loggedCount += 1
            let p = points[0]
            NSLog("MenuBand MTouch: x=%.3f y=%.3f  fingers=%d", p.x, p.y, points.count)
        }
        DispatchQueue.main.async { [weak self] in self?.onFrame?(points) }
    }

    func stop() {
        guard let h = handle else { return }
        func sym(_ name: String) -> UnsafeMutableRawPointer? { dlsym(h, name) }
        if let stopSym = sym("MTDeviceStop") {
            let stopDevice = unsafeBitCast(stopSym, to: StopFn.self)
            devices.forEach { stopDevice($0) }
        }
        if let unregSym = sym("MTUnregisterContactFrameCallback") {
            let unregister = unsafeBitCast(unregSym, to: UnregisterFn.self)
            devices.forEach { unregister($0) }
        }
        devices.removeAll()
        started = false
    }
}

#endif
