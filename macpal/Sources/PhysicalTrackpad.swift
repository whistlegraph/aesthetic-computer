import Foundation

// Global, focus-independent physical trackpad contact frames. Deskflow's
// injected pointer events never appear here, which makes this a reliable
// controller-claim signal on Neo and Blueberry.
#if !MAC_APP_STORE

private struct HandoffMTPoint { var x: Float; var y: Float }
private struct HandoffMTReadout { var position: HandoffMTPoint; var velocity: HandoffMTPoint }
private struct HandoffMTTouch {
    var frame: Int32
    var timestamp: Double
    var identifier: Int32
    var state: Int32
    var fingerID: Int32
    var handID: Int32
    var normalized: HandoffMTReadout
    var size: Float
    var zero1: Int32
    var angle: Float
    var majorAxis: Float
    var minorAxis: Float
    var absolute: HandoffMTReadout
    var zero2a: Int32
    var zero2b: Int32
    var zDensity: Float
}

private typealias HandoffMTCallback = @convention(c) (
    UnsafeMutableRawPointer?, UnsafeMutableRawPointer?, Int32, Double, Int32
) -> Int32

private func handoffMTFrame(
    _ device: UnsafeMutableRawPointer?, _ contacts: UnsafeMutableRawPointer?,
    _ count: Int32, _ timestamp: Double, _ frame: Int32
) -> Int32 {
    let typed = contacts?.assumingMemoryBound(to: HandoffMTTouch.self)
    var point: CGPoint?
    if let typed {
        for i in 0..<Int(count) where typed[i].state == 4 {
            let position = typed[i].normalized.position
            point = CGPoint(x: CGFloat(position.x), y: CGFloat(position.y))
            break
        }
    }
    DispatchQueue.main.async { PhysicalTrackpad.shared.update(point: point) }
    return 0
}

final class PhysicalTrackpad {
    static let shared = PhysicalTrackpad()
    var onTouchBegan: ((CGPoint) -> Void)?
    var onFrame: ((CGPoint?) -> Void)?

    private var framework: UnsafeMutableRawPointer?
    private var devices: [UnsafeMutableRawPointer] = []
    private var wasTouching = false

    private typealias CreateList = @convention(c) () -> Unmanaged<CFArray>?
    private typealias Register = @convention(c) (UnsafeMutableRawPointer, HandoffMTCallback) -> Void
    private typealias Start = @convention(c) (UnsafeMutableRawPointer, Int32) -> Void

    @discardableResult
    func start() -> Bool {
        guard framework == nil else { return !devices.isEmpty }
        let path = "/System/Library/PrivateFrameworks/MultitouchSupport.framework/MultitouchSupport"
        guard let handle = dlopen(path, RTLD_NOW),
              let createSymbol = dlsym(handle, "MTDeviceCreateList"),
              let registerSymbol = dlsym(handle, "MTRegisterContactFrameCallback"),
              let startSymbol = dlsym(handle, "MTDeviceStart") else {
            NSLog("MacPal Deskflow handoff: physical trackpad API unavailable")
            return false
        }
        framework = handle
        let create = unsafeBitCast(createSymbol, to: CreateList.self)
        let register = unsafeBitCast(registerSymbol, to: Register.self)
        let startDevice = unsafeBitCast(startSymbol, to: Start.self)
        guard let list = create()?.takeRetainedValue() else { return false }
        for i in 0..<CFArrayGetCount(list) {
            guard let raw = CFArrayGetValueAtIndex(list, i) else { continue }
            let device = UnsafeMutableRawPointer(mutating: raw)
            register(device, handoffMTFrame)
            startDevice(device, 0)
            devices.append(device)
        }
        NSLog("MacPal Deskflow handoff: watching %d physical trackpad(s)", devices.count)
        return !devices.isEmpty
    }

    fileprivate func update(point: CGPoint?) {
        if let point, !wasTouching { onTouchBegan?(point) }
        onFrame?(point)
        wasTouching = point != nil
    }
}

#endif
