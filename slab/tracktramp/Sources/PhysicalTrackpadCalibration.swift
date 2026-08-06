import AppKit

enum TrackDrumDockLayout {
    /// AppKit owns the sandbox-safe usable-screen calculation. TrackDrum adds
    /// bottom clearance for an auto-hidden Dock without reading Dock settings.
    static func safeFrame(for screen: NSScreen) -> NSRect {
        var safe = screen.visibleFrame
        let reserve = min(72, safe.height / 5)
        safe.origin.y += reserve
        safe.size.height = max(0, safe.height - reserve)
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
