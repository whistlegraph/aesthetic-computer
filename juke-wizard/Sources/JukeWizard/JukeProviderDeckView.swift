import AppKit

/// The main-window deck used by streaming providers whose media cannot leave
/// the service player. It deliberately shares the launch bed's record grammar,
/// but exposes no drag or pop-out affordance.
final class JukeProviderDeckView: NSView {
    private let recordView = JukeProviderRecordView(frame: .zero)
    private let titleLabel = NSTextField(labelWithString: "Choose a track")
    private let artistLabel = NSTextField(labelWithString: "")
    private let albumLabel = NSTextField(labelWithString: "")
    private let fixedLabel = NSTextField(labelWithString: "FIXED TO DECK")
    private let playButton = NSButton(title: "▶", target: nil, action: nil)
    private let timeLabel = NSTextField(labelWithString: "0:00 / 0:00")
    private let progress = SpotifyProgressView(frame: .zero)
    private var accent = Palette.teal

    var onToggle: (() -> Void)?
    var onSeek: ((Double) -> Void)?

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.cornerRadius = 12
        layer?.borderWidth = 1
        titleLabel.font = .systemFont(ofSize: 16, weight: .bold)
        titleLabel.lineBreakMode = .byTruncatingTail
        artistLabel.font = .systemFont(ofSize: 12, weight: .medium)
        artistLabel.textColor = .secondaryLabelColor
        artistLabel.lineBreakMode = .byTruncatingTail
        albumLabel.font = .systemFont(ofSize: 11)
        albumLabel.textColor = .tertiaryLabelColor
        albumLabel.lineBreakMode = .byTruncatingTail
        fixedLabel.font = .monospacedSystemFont(ofSize: 10, weight: .bold)
        fixedLabel.alignment = .center
        fixedLabel.toolTip = "This service keeps playback on the main deck"
        playButton.bezelStyle = .rounded
        playButton.target = self
        playButton.action = #selector(toggle)
        timeLabel.font = .monospacedDigitSystemFont(ofSize: 12, weight: .medium)
        timeLabel.textColor = .secondaryLabelColor
        progress.onSeek = { [weak self] target in self?.onSeek?(target) }
        [recordView, titleLabel, artistLabel, albumLabel, fixedLabel,
         playButton, timeLabel, progress].forEach(addSubview)
        setAccessibilityRole(.group)
        setAccessibilityLabel("Fixed streaming deck")
    }
    required init?(coder: NSCoder) { fatalError() }

    func configure(source: JukeSource) {
        accent = source == .spotify
            ? NSColor(srgbRed: 0.11, green: 0.73, blue: 0.33, alpha: 1)
            : NSColor(srgbRed: 0.98, green: 0.22, blue: 0.35, alpha: 1)
        recordView.accent = accent
        recordView.providerMark = source == .spotify ? "S" : "♪"
        fixedLabel.textColor = accent
        playButton.contentTintColor = accent
        layer?.borderColor = accent.withAlphaComponent(0.55).cgColor
        if titleLabel.stringValue.isEmpty { titleLabel.stringValue = "Choose a track" }
        needsDisplay = true
    }

    func update(title: String, artist: String, album: String, art: NSImage?,
                duration: Double, position: Double, playing: Bool, canSeek: Bool) {
        titleLabel.stringValue = title.isEmpty ? "Choose a track" : title
        artistLabel.stringValue = artist
        albumLabel.stringValue = album
        recordView.art = art
        recordView.isPlaying = playing
        playButton.title = playing ? "❚❚" : "▶"
        playButton.isEnabled = !title.isEmpty
        progress.duration = duration
        progress.position = position
        progress.allowsSeeking = canSeek
        progress.alphaValue = canSeek ? 1 : 0.45
        timeLabel.stringValue = "\(JukeController.mmss(position)) / \(JukeController.mmss(duration))"
    }

    override func layout() {
        let pad: CGFloat = 14
        let top = bounds.height - pad
        titleLabel.frame = NSRect(x: pad, y: top - 24, width: bounds.width - pad * 2, height: 22)
        artistLabel.frame = NSRect(x: pad, y: top - 43, width: bounds.width - pad * 2, height: 17)
        albumLabel.frame = NSRect(x: pad, y: top - 60, width: bounds.width - pad * 2, height: 15)

        let controlsH: CGFloat = 88
        let recordTop = top - 67
        let recordBottom = controlsH + 10
        let diameter = max(90, min(bounds.width - 38, recordTop - recordBottom))
        recordView.frame = NSRect(x: (bounds.width - diameter) / 2,
                                  y: recordBottom + (recordTop - recordBottom - diameter) / 2,
                                  width: diameter, height: diameter)

        fixedLabel.frame = NSRect(x: bounds.midX - 70, y: 66, width: 140, height: 16)
        playButton.frame = NSRect(x: pad, y: 32, width: 45, height: 27)
        progress.frame = NSRect(x: pad + 53, y: 32, width: max(80, bounds.width - pad * 2 - 53), height: 27)
        timeLabel.frame = NSRect(x: pad, y: 9, width: bounds.width - pad * 2, height: 18)
    }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        needsDisplay = true
        recordView.needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let dark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        Palette.deckSurface(accent, dark: dark).withAlphaComponent(0.34).setFill()
        NSBezierPath(roundedRect: bounds, xRadius: 12, yRadius: 12).fill()
    }

    @objc private func toggle() { onToggle?() }
}

final class JukeProviderRecordView: NSView {
    var accent = Palette.teal { didSet { needsDisplay = true } }
    var providerMark = "S" { didSet { needsDisplay = true } }
    var art: NSImage? { didSet { needsDisplay = true } }
    var isPlaying = false { didSet { needsDisplay = true } }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        setAccessibilityRole(.image)
        setAccessibilityLabel("Streaming record fixed to the main deck")
    }
    required init?(coder: NSCoder) { fatalError() }

    override func draw(_ dirtyRect: NSRect) {
        let c = NSPoint(x: bounds.midX, y: bounds.midY)
        let r = max(1, min(bounds.width, bounds.height) / 2 - 6)
        let disc = NSRect(x: c.x - r, y: c.y - r, width: r * 2, height: r * 2)

        NSGraphicsContext.saveGraphicsState()
        let shadow = NSShadow()
        shadow.shadowColor = .black.withAlphaComponent(0.58)
        shadow.shadowBlurRadius = 11
        shadow.shadowOffset = NSSize(width: 0, height: -4)
        shadow.set()
        NSColor(white: 0.025, alpha: 1).setFill()
        NSBezierPath(ovalIn: disc).fill()
        NSGraphicsContext.restoreGraphicsState()

        for groove in stride(from: r * 0.38, through: r * 0.93, by: max(3, r * 0.04)) {
            NSColor(white: 0.22, alpha: 0.58).setStroke()
            let path = NSBezierPath(ovalIn: NSRect(x: c.x - groove, y: c.y - groove,
                                                   width: groove * 2, height: groove * 2))
            path.lineWidth = 0.7
            path.stroke()
        }

        let labelR = r * 0.31
        let labelRect = NSRect(x: c.x - labelR, y: c.y - labelR,
                               width: labelR * 2, height: labelR * 2)
        if let art {
            NSGraphicsContext.saveGraphicsState()
            NSBezierPath(ovalIn: labelRect).addClip()
            art.draw(in: labelRect, from: .zero, operation: .sourceOver, fraction: 1)
            NSGraphicsContext.restoreGraphicsState()
        } else {
            accent.setFill()
            NSBezierPath(ovalIn: labelRect).fill()
            let mark = providerMark as NSString
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: max(20, r * 0.22), weight: .black),
                .foregroundColor: NSColor.white
            ]
            let size = mark.size(withAttributes: attrs)
            mark.draw(at: NSPoint(x: c.x - size.width / 2, y: c.y - size.height / 2),
                      withAttributes: attrs)
        }

        accent.withAlphaComponent(isPlaying ? 1 : 0.46).setStroke()
        let marker = NSBezierPath()
        marker.move(to: NSPoint(x: c.x, y: c.y + r * 0.42))
        marker.line(to: NSPoint(x: c.x, y: c.y + r * 0.87))
        marker.lineWidth = max(2, r * 0.025)
        marker.lineCapStyle = .round
        marker.stroke()
    }
}
