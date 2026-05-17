import AppKit

/// Minimal `NSDraggingSource` for the inline tape eject. The status
/// item button itself doesn't conform to `NSDraggingSource`, and adding
/// a conformance to `NSStatusBarButton` via extension would touch every
/// other drag path that lives on that button. A tiny adapter object
/// keeps the drag isolated to the tape feature.
///
/// We allow `.copy` for external contexts (Finder, Mail, browsers) so
/// the user's recording lands on disk as a duplicate of the temp file
/// — the temp .wav stays in `/tmp` and gets reaped by macOS later, the
/// user's copy lives wherever they dropped it.
final class TapeDragSource: NSObject, NSDraggingSource {
    func draggingSession(_ session: NSDraggingSession,
                         sourceOperationMaskFor context: NSDraggingContext)
        -> NSDragOperation {
        switch context {
        case .outsideApplication:
            return .copy
        case .withinApplication:
            return []
        @unknown default:
            return .copy
        }
    }
}
