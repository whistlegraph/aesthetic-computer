import Foundation
import CoreGraphics

struct FrameNativeTarget: Equatable {
    let windowID: CGWindowID
    let pid: pid_t
    let bounds: CGRect
}

struct FrameNativeClick: Decodable {
    let observationId: String
    let x: Double
    let y: Double
    let count: Int
    let settleMs: Double

    var verify: FrameNativeVerification? = nil
    var holdMs: Double? = nil
    var drag: FrameNativeDragPath? = nil
    var effectiveHoldMs: Double { holdMs ?? 40 }

    var valid: Bool {
        !observationId.isEmpty && x.isFinite && y.isFinite &&
        (1...3).contains(count) && settleMs.isFinite && (0...1000).contains(settleMs) && effectiveHoldMs.isFinite && (0...1000).contains(effectiveHoldMs) && (drag == nil || (count == 1 && drag!.valid)) && (verify == nil || verify!.valid)
    }
}

struct FrameNativeDragPath: Decodable {
    let x: Double
    let y: Double
    let durationMs: Double
    var releaseMs: Double? = nil
    var effectiveReleaseMs: Double { releaseMs ?? 0 }
    var valid: Bool { x.isFinite && y.isFinite && durationMs.isFinite && (0...2000).contains(durationMs) && effectiveReleaseMs.isFinite && (0...1000).contains(effectiveReleaseMs) }
}

struct FrameNativeVerification: Decodable {
    let x: Double
    let y: Double
    let role: String
    let attribute: String
    let equals: String
    let timeoutMs: Double

    var valid: Bool {
        x.isFinite && y.isFinite && role.hasPrefix("AX") && role.count <= 80 &&
        ["AXValue", "AXTitle", "AXDescription"].contains(attribute) &&
        !equals.isEmpty && equals.utf8.count <= 512 && timeoutMs.isFinite && (1...2000).contains(timeoutMs)
    }
}

// Accessed only on Frame's serial queue. Store the CG geometry associated with
// actual captured pixels, not a caller-supplied rectangle or a cached AX tree.
final class FrameNativeBindings {
    private var bindings: [String: (id: String, target: FrameNativeTarget)] = [:]
    private var order: [String] = []
    private let limit = 32

    func clear(_ session: String) {
        bindings.removeValue(forKey: session)
        order.removeAll { $0 == session }
    }

    func record(session: String, id: String, target: FrameNativeTarget) {
        clear(session)
        if order.count >= limit { clear(order[0]) }
        order.append(session)
        bindings[session] = (id, target)
    }

    func validate(session: String, id: String, current: FrameNativeTarget?,
                  available: Bool, point: CGPoint? = nil) -> String? {
        guard available else { return "Desktop unavailable or permission missing" }
        guard let binding = bindings[session], binding.id == id else {
            return "Observation missing, superseded, or already consumed"
        }
        guard let current, binding.target == current else { return "Native target changed or moved" }
        if let point, !current.bounds.contains(point) { return "Click lies outside the observed window" }
        return nil
    }
}
