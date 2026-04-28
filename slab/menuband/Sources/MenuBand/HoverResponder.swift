import AppKit

// NSResponder shim that forwards tracking-area mouse events to closures.
// NSTrackingArea.owner has to be an NSResponder; this keeps AppDelegate from
// having to subclass NSResponder.
final class HoverResponder: NSResponder {
    var onMove: ((NSEvent) -> Void)?
    var onExit: (() -> Void)?

    override func mouseEntered(with event: NSEvent) { onMove?(event) }
    override func mouseMoved(with event: NSEvent) { onMove?(event) }
    override func mouseExited(with event: NSEvent) { onExit?() }
}
