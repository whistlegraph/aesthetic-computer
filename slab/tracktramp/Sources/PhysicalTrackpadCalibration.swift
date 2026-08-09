import AppKit

enum TrackDrumDockLayout {
    /// AppKit owns the sandbox-safe usable-screen calculation. The clearance
    /// TrackDrum adds is only for a Dock that could *appear* at the bottom —
    /// one that is auto-hidden. Reserving it unconditionally stacked a second
    /// Dock's worth of margin on top of the one `visibleFrame` already carves
    /// out, floating the surface high against a Dock that was plainly there.
    static func safeFrame(for screen: NSScreen) -> NSRect {
        var safe = screen.visibleFrame
        let full = screen.frame
        let reveal = min(72, safe.height / 5)
        // A Dock showing along the bottom is already excluded from
        // `visibleFrame`, so the space is spoken for.
        let reserved = safe.minY - full.minY
        if reserved >= reveal { return safe }
        // A Dock showing on either side proves it does not live at the
        // bottom, so no reveal strip can open there.
        if safe.minX > full.minX || safe.maxX < full.maxX { return safe }
        let clearance = reveal - reserved
        safe.origin.y += clearance
        safe.size.height = max(0, safe.height - clearance)
        return safe
    }
}

/// AppKit reports an indirect touch device's physical coordinate range in
/// points through `NSTouch.deviceSize`. That is the public, sandbox-safe source
/// of truth for TrackDrum's one-to-one surface. The fallback is the common
/// built-in MacBook trackpad at 72 points per inch and is replaced by the
/// first real touch frame.
struct PhysicalTrackpadCalibration {
    static let fallbackSurfacePoints = NSSize(
        width: 111.55 * 72 / 25.4,
        height: 67.90 * 72 / 25.4
    )

    let surfacePoints: NSSize
    let source: String

    static func current(deviceSize: NSSize? = nil) -> PhysicalTrackpadCalibration {
        if let deviceSize,
           deviceSize.width >= 100,
           deviceSize.height >= 60,
           deviceSize.width <= 1_000,
           deviceSize.height <= 1_000 {
            return PhysicalTrackpadCalibration(
                surfacePoints: deviceSize,
                source: "NSTouch.deviceSize"
            )
        }
        return PhysicalTrackpadCalibration(
            surfacePoints: fallbackSurfacePoints,
            source: "built-in fallback"
        )
    }
}
