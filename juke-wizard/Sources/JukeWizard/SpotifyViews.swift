import AppKit

final class SpotifyTrackRowView: NSTableCellView {
    static let id = NSUserInterfaceItemIdentifier("spotify-track-row")
    private let titleField = NSTextField(labelWithString: "")
    private let detailField = NSTextField(labelWithString: "")
    private let durationField = NSTextField(labelWithString: "")
    var selected = false { didSet { needsDisplay = true } }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        titleField.font = .systemFont(ofSize: 13, weight: .semibold)
        titleField.lineBreakMode = .byTruncatingTail
        detailField.font = .systemFont(ofSize: 11)
        detailField.textColor = .secondaryLabelColor
        detailField.lineBreakMode = .byTruncatingTail
        durationField.font = .monospacedDigitSystemFont(ofSize: 11, weight: .regular)
        durationField.textColor = .secondaryLabelColor
        durationField.alignment = .right
        addSubview(titleField); addSubview(detailField); addSubview(durationField)
    }
    required init?(coder: NSCoder) { fatalError() }

    func configure(_ track: SpotifyTrackResult) {
        titleField.stringValue = track.title
        detailField.stringValue = [track.artists, track.album].filter { !$0.isEmpty }.joined(separator: " · ")
        durationField.stringValue = JukeController.mmss(track.duration)
    }

    override func layout() {
        super.layout()
        titleField.frame = NSRect(x: 12, y: bounds.height - 23, width: bounds.width - 80, height: 18)
        detailField.frame = NSRect(x: 12, y: 5, width: bounds.width - 80, height: 15)
        durationField.frame = NSRect(x: bounds.width - 63, y: 13, width: 51, height: 16)
    }

    override func draw(_ dirtyRect: NSRect) {
        if selected {
            Palette.teal.withAlphaComponent(0.22).setFill()
            NSBezierPath(roundedRect: bounds.insetBy(dx: 4, dy: 2), xRadius: 6, yRadius: 6).fill()
        }
        super.draw(dirtyRect)
    }
}

final class SpotifyProgressView: NSView {
    var duration: Double = 0 { didSet { needsDisplay = true } }
    var position: Double = 0 { didSet { needsDisplay = true } }
    var onSeek: ((Double) -> Void)?

    override func draw(_ dirtyRect: NSRect) {
        NSColor.black.withAlphaComponent(0.32).setFill()
        NSBezierPath(roundedRect: bounds, xRadius: 6, yRadius: 6).fill()
        let bar = NSRect(x: 10, y: bounds.midY - 3, width: max(0, bounds.width - 20), height: 6)
        NSColor.white.withAlphaComponent(0.16).setFill()
        NSBezierPath(roundedRect: bar, xRadius: 3, yRadius: 3).fill()
        guard duration > 0 else { return }
        var fill = bar
        fill.size.width *= CGFloat(min(1, max(0, position / duration)))
        Palette.teal.setFill()
        NSBezierPath(roundedRect: fill, xRadius: 3, yRadius: 3).fill()
    }

    override func mouseDown(with event: NSEvent) { seek(event) }
    override func mouseDragged(with event: NSEvent) { seek(event) }
    private func seek(_ event: NSEvent) {
        guard duration > 0 else { return }
        let x = convert(event.locationInWindow, from: nil).x
        onSeek?(duration * Double(min(1, max(0, x / max(1, bounds.width)))))
    }
}
