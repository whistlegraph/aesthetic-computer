import Foundation
import QuartzCore

struct TrackpadContact: Equatable {
    let identifier: Int32
    let point: CGPoint
    let state: Int32

    var isActive: Bool { state == 3 || state == 4 }
    var isBegan: Bool { state == 3 }
}

struct TrackpadContactChanges {
    let active: [TrackpadContact]
    let began: [CGPoint]
    let lifted: [CGPoint]
    let activeByID: [Int32: CGPoint]
    let sameCountReplacement: Bool

    static func resolve(previous: [Int32: CGPoint],
                        contacts: [TrackpadContact]) -> TrackpadContactChanges {
        let active = contacts.filter(\.isActive)
        let current = Dictionary(uniqueKeysWithValues: active.map {
            ($0.identifier, $0.point)
        })
        let began = active.compactMap { contact in
            contact.isBegan || previous[contact.identifier] == nil
                ? contact.point : nil
        }
        let lifted = previous.compactMap { identifier, point in
            current[identifier] == nil ? point : nil
        }
        return TrackpadContactChanges(
            active: active,
            began: began,
            lifted: lifted,
            activeByID: current,
            sameCountReplacement: !began.isEmpty && !lifted.isEmpty
                && previous.count == current.count
        )
    }
}

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
    /// (0…1 each axis, origin bottom-left), plus the device timestamp used for
    /// velocity and latency measurements. Empty when no finger is down.
    /// Delivered on the MAIN thread (the framework calls back on its own
    /// thread; AppKit/audio state must be touched on main). This is the clean,
    /// focus-independent signal the pitch-bend / fx pad consumes — no pointer
    /// acceleration, unlike the dead NSTouch `TouchSensorView` path.
    var onFrame: (([TrackpadContact], Double, Double) -> Void)?

    private var handle: UnsafeMutableRawPointer?
    private var devices: [UnsafeMutableRawPointer] = []
    private var started = false
    private var rangeMinX = Double.greatestFiniteMagnitude
    private var rangeMaxX = -Double.greatestFiniteMagnitude
    private var rangeMinY = Double.greatestFiniteMagnitude
    private var rangeMaxY = -Double.greatestFiniteMagnitude
    private var lastActiveFrameStamp: Double = 0
    private var cadenceSum: Double = 0
    private var cadenceMax: Double = 0
    private var cadenceCount = 0
    private var lastStatsStamp: Double = 0
    private var mainDeliverySamples: [Double] = []
    private var contactBegins = 0

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

    /// Frame callback target. State 3 is the make-contact frame and state 4 is
    /// sustained contact; accepting both avoids waiting an extra hardware
    /// frame before a strike. Hover (2), break (5), linger (6), and out-of-
    /// range (7) never become notes. Raw normalized positions pass through
    /// unchanged—there is no software dead zone, quantization, or edge clamp.
    func handle(contacts: UnsafeMutablePointer<MTTouch>?, count: Int, timestamp: Double) {
        var frameContacts: [TrackpadContact] = []
        if let contacts {
            for i in 0..<count where contacts[i].state >= 3 && contacts[i].state <= 5 {
                let pos = contacts[i].normalized.position
                let contact = TrackpadContact(
                    identifier: contacts[i].identifier,
                    point: CGPoint(x: CGFloat(pos.x), y: CGFloat(pos.y)),
                    state: contacts[i].state
                )
                frameContacts.append(contact)
                if contact.isBegan { contactBegins += 1 }
                if contact.isActive {
                    rangeMinX = min(rangeMinX, Double(pos.x))
                    rangeMaxX = max(rangeMaxX, Double(pos.x))
                    rangeMinY = min(rangeMinY, Double(pos.y))
                    rangeMaxY = max(rangeMaxY, Double(pos.y))
                }
            }
        }
        let points = frameContacts.filter(\.isActive).map(\.point)
        if !points.isEmpty {
            if lastActiveFrameStamp > 0 {
                let interval = timestamp - lastActiveFrameStamp
                if interval > 0, interval < 0.050 {
                    cadenceSum += interval
                    cadenceMax = max(cadenceMax, interval)
                    cadenceCount += 1
                }
            }
            lastActiveFrameStamp = timestamp
        } else {
            // A lift ends the continuous-contact cadence run. The next touch
            // starts a fresh run rather than counting human time between taps.
            lastActiveFrameStamp = 0
        }
        let callbackTime = CACurrentMediaTime()
        let shouldLog = lastStatsStamp == 0 || timestamp - lastStatsStamp >= 2.0
        if shouldLog { lastStatsStamp = timestamp }
        let averageHz = cadenceSum > 0 ? Double(cadenceCount) / cadenceSum : 0
        let maximumGapMs = cadenceMax * 1000
        let range = (rangeMinX, rangeMaxX, rangeMinY, rangeMaxY)
        let begins = contactBegins
        let hasRange = range.0 <= range.1 && range.2 <= range.3
        if shouldLog {
            cadenceSum = 0
            cadenceMax = 0
            cadenceCount = 0
            contactBegins = 0
        }
        DispatchQueue.main.async { [weak self] in
            guard let self else { return }
            let deliveryMs = (CACurrentMediaTime() - callbackTime) * 1000
            self.mainDeliverySamples.append(deliveryMs)
            if self.mainDeliverySamples.count > 256 {
                self.mainDeliverySamples.removeFirst(
                    self.mainDeliverySamples.count - 256
                )
            }
            if shouldLog, hasRange {
                let sorted = self.mainDeliverySamples.sorted()
                let percentile: (Double) -> Double = { fraction in
                    guard !sorted.isEmpty else { return 0 }
                    let index = min(sorted.count - 1,
                                    Int((Double(sorted.count - 1) * fraction).rounded()))
                    return sorted[index]
                }
                NSLog(String(format:
                    "MenuBand MTouch stats: raw x=[%.3f,%.3f] y=[%.3f,%.3f] cadence=%.1fHz maxGap=%.2fms begins=%d main p50/p95/max=%.2f/%.2f/%.2fms",
                    range.0, range.1, range.2, range.3,
                    averageHz, maximumGapMs, begins,
                    percentile(0.50), percentile(0.95), sorted.last ?? 0))
                self.mainDeliverySamples.removeAll(keepingCapacity: true)
            }
            self.onFrame?(frameContacts, timestamp, callbackTime)
        }
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
