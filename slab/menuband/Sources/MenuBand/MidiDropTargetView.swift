import AppKit

/// Transparent overlay that accepts a dragged `.mid` / `.midi` file
/// and forwards it to `onDrop`. Installed as a subview of the
/// menubar status item button (and, optionally, the popover content
/// view) so dragging a MIDI file onto Menu Band auto-plays it
/// through the synth.
final class MidiDropTargetView: NSView {

    var onDrop: ((URL) -> Void)?

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        registerForDraggedTypes([.fileURL])
    }

    required init?(coder: NSCoder) { fatalError() }

    /// Pass mouse events through so the parent button still gets
    /// its clicks — we only care about drag operations.
    override func hitTest(_ point: NSPoint) -> NSView? { nil }

    override func draggingEntered(_ sender: NSDraggingInfo) -> NSDragOperation {
        return Self.dragURL(from: sender) != nil ? .copy : []
    }

    override func draggingUpdated(_ sender: NSDraggingInfo) -> NSDragOperation {
        return Self.dragURL(from: sender) != nil ? .copy : []
    }

    override func performDragOperation(_ sender: NSDraggingInfo) -> Bool {
        guard let url = Self.dragURL(from: sender) else { return false }
        onDrop?(url)
        return true
    }

    /// Pull the first `.mid` / `.midi` URL off the drag pasteboard.
    private static func dragURL(from sender: NSDraggingInfo) -> URL? {
        let pb = sender.draggingPasteboard
        guard let urls = pb.readObjects(forClasses: [NSURL.self],
                                         options: nil) as? [URL] else {
            return nil
        }
        return urls.first(where: { url in
            let ext = url.pathExtension.lowercased()
            return ext == "mid" || ext == "midi"
        })
    }
}
