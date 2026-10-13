import CoreGraphics

/// A mouse gesture only leaves the grid when its window actually changes frame.
/// Clicks, text selection, and windows disappearing do not suspend tiling.
struct ManualTileOverride {
    private(set) var suspended = false
    private var window: (id: CGWindowID, frame: CGRect)?
    var isTracking: Bool { window != nil }

    mutating func begin(id: CGWindowID, frame: CGRect) {
        window = (id, frame)
    }

    @discardableResult
    mutating func observe(frames: [CGWindowID: CGRect]) -> Bool {
        guard !suspended, let window, let frame = frames[window.id] else { return false }
        let old = window.frame
        guard abs(frame.minX - old.minX) > 2 || abs(frame.minY - old.minY) > 2
            || abs(frame.width - old.width) > 2 || abs(frame.height - old.height) > 2
        else { return false }
        suspended = true
        return true
    }

    mutating func end() { window = nil }
    mutating func resume() { suspended = false; window = nil }
}
