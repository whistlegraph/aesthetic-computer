import AppKit

/// Compact Spotify card shown directly beneath Menu Band's instrument cluster.
/// It deliberately stays small enough for a menu-bar panel: current artwork,
/// track metadata, seekable progress, transport, and catalog search.
final class MenuBandSpotifyPlayerView: NSView, NSSearchFieldDelegate {
    static let preferredSize = NSSize(width: 224, height: 142)

    private weak var menuBand: MenuBandController?
    private let artwork = NSImageView()
    private let titleLabel = NSTextField(labelWithString: "Spotify")
    private let artistLabel = NSTextField(labelWithString: "")
    private let detailLabel = NSTextField(labelWithString: "juked headless player")
    private let timeLabel = NSTextField(labelWithString: "")
    private let progress = MenuBandSpotifyProgressView()
    private let previousButton = NSButton(title: "⏮", target: nil, action: nil)
    private let playButton = NSButton(title: "▶", target: nil, action: nil)
    private let nextButton = NSButton(title: "⏭", target: nil, action: nil)
    private let closeButton = NSButton(title: "×", target: nil, action: nil)
    private let searchField = NSSearchField()
    private var searchResults: [MenuBandSpotifyTrack] = []
    private var representedArtworkURL: URL?
    private var artworkTask: URLSessionDataTask?

    override var intrinsicContentSize: NSSize { Self.preferredSize }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        super.init(frame: NSRect(origin: .zero, size: Self.preferredSize))
        wantsLayer = true
        layer?.cornerRadius = 9
        layer?.borderWidth = 1

        artwork.imageScaling = .scaleProportionallyUpOrDown
        artwork.wantsLayer = true
        artwork.layer?.cornerRadius = 6
        artwork.layer?.masksToBounds = true

        titleLabel.font = .systemFont(ofSize: 13, weight: .bold)
        titleLabel.lineBreakMode = .byTruncatingTail
        artistLabel.font = .systemFont(ofSize: 10.5, weight: .medium)
        artistLabel.textColor = .secondaryLabelColor
        artistLabel.lineBreakMode = .byTruncatingTail
        detailLabel.font = .monospacedDigitSystemFont(ofSize: 9, weight: .regular)
        detailLabel.textColor = .tertiaryLabelColor
        detailLabel.lineBreakMode = .byTruncatingTail
        timeLabel.font = .monospacedDigitSystemFont(ofSize: 8.5, weight: .medium)
        timeLabel.textColor = .tertiaryLabelColor
        timeLabel.alignment = .right

        [previousButton, playButton, nextButton].forEach {
            $0.bezelStyle = .recessed
            $0.controlSize = .small
        }
        previousButton.target = self
        previousButton.action = #selector(previousClicked)
        playButton.target = self
        playButton.action = #selector(playClicked)
        nextButton.target = self
        nextButton.action = #selector(nextClicked)

        closeButton.isBordered = false
        closeButton.font = .systemFont(ofSize: 15, weight: .medium)
        closeButton.contentTintColor = .secondaryLabelColor
        closeButton.target = self
        closeButton.action = #selector(closeClicked)
        closeButton.toolTip = "Pause Spotify and close the player"

        searchField.placeholderString = "Search Spotify"
        searchField.controlSize = .small
        searchField.delegate = self
        searchField.target = self
        searchField.action = #selector(searchSubmitted)

        progress.onSeek = { [weak self] seconds in
            guard
                let self,
                let state = self.menuBand?.spotifyPlayback
            else { return }
            self.menuBand?.seekSpotify(to: seconds, from: state.position)
        }

        [artwork, titleLabel, artistLabel, detailLabel, timeLabel, progress,
         previousButton, playButton, nextButton, closeButton, searchField]
            .forEach(addSubview)
        refresh()
    }

    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        super.layout()
        let width = bounds.width
        artwork.frame = NSRect(x: 7, y: 76, width: 58, height: 58)
        closeButton.frame = NSRect(x: width - 25, y: 116, width: 20, height: 20)
        let textX: CGFloat = 73
        let textWidth = max(40, width - textX - 25)
        titleLabel.frame = NSRect(x: textX, y: 113, width: textWidth, height: 18)
        artistLabel.frame = NSRect(x: textX, y: 95, width: textWidth, height: 15)
        let showsError = menuBand?.spotifyStatusIsError == true
        detailLabel.frame = NSRect(
            x: textX, y: 78,
            width: max(30, width - textX - (showsError ? 7 : 73)), height: 14)
        timeLabel.frame = NSRect(x: width - 73, y: 78,
                                 width: showsError ? 0 : 66, height: 14)
        progress.frame = NSRect(x: 7, y: 61, width: width - 14, height: 10)
        previousButton.frame = NSRect(x: 49, y: 32, width: 38, height: 24)
        playButton.frame = NSRect(x: 93, y: 32, width: 38, height: 24)
        nextButton.frame = NSRect(x: 137, y: 32, width: 38, height: 24)
        searchField.frame = NSRect(x: 7, y: 5, width: width - 14, height: 22)
    }

    override func updateLayer() {
        super.updateLayer()
        let dark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua])
            == .darkAqua
        layer?.backgroundColor = NSColor.systemGreen
            .withAlphaComponent(dark ? 0.11 : 0.08).cgColor
        layer?.borderColor = NSColor.systemGreen
            .withAlphaComponent(dark ? 0.42 : 0.34).cgColor
    }

    func refresh() {
        guard let menuBand else { return }
        let state = menuBand.spotifyPlayback
        titleLabel.stringValue = state?.title ?? "Spotify"
        artistLabel.stringValue = state?.artists ?? ""
        if menuBand.spotifyStatusIsError {
            detailLabel.stringValue = menuBand.spotifyStatus
            timeLabel.stringValue = ""
        } else if let state {
            detailLabel.stringValue = state.album
            timeLabel.stringValue =
                "\(Self.mmss(state.position))/\(Self.mmss(state.duration))"
        } else {
            detailLabel.stringValue = menuBand.spotifyStatus
            timeLabel.stringValue = ""
        }
        detailLabel.textColor = menuBand.spotifyStatusIsError
            ? .systemRed : .tertiaryLabelColor
        playButton.title = state?.isPlaying == true ? "❚❚" : "▶"
        progress.duration = state?.duration ?? 0
        progress.position = state?.position ?? 0
        updateArtwork(state?.artworkURL)
        needsLayout = true
        needsDisplay = true
    }

    private func updateArtwork(_ url: URL?) {
        guard representedArtworkURL != url else { return }
        representedArtworkURL = url
        artworkTask?.cancel()
        artworkTask = nil
        artwork.image = NSImage(
            systemSymbolName: "music.note", accessibilityDescription: "Spotify")
        guard let url else { return }
        artworkTask = URLSession.shared.dataTask(with: url) { [weak self] data, _, _ in
            guard let data, let image = NSImage(data: data) else { return }
            DispatchQueue.main.async {
                guard self?.representedArtworkURL == url else { return }
                self?.artwork.image = image
            }
        }
        artworkTask?.resume()
    }

    @objc private func previousClicked() { menuBand?.previousSpotifyTrack() }
    @objc private func playClicked() { menuBand?.toggleSpotifyPlayback() }
    @objc private func nextClicked() { menuBand?.nextSpotifyTrack() }
    @objc private func closeClicked() { menuBand?.deactivateSpotifyPlayer() }

    @objc private func searchSubmitted() {
        let query = searchField.stringValue
            .trimmingCharacters(in: .whitespacesAndNewlines)
        guard !query.isEmpty else { return }
        searchField.isEnabled = false
        menuBand?.searchSpotify(query) { [weak self] result in
            guard let self else { return }
            self.searchField.isEnabled = true
            switch result {
            case .success(let tracks):
                self.searchResults = tracks
                self.showSearchResults(for: query)
            case .failure:
                self.refresh()
            }
        }
    }

    private func showSearchResults(for query: String) {
        let menu = NSMenu(title: "Spotify results")
        if searchResults.isEmpty {
            let empty = NSMenuItem(
                title: "No Spotify tracks for “\(query)”", action: nil,
                keyEquivalent: "")
            empty.isEnabled = false
            menu.addItem(empty)
        } else {
            for (index, track) in searchResults.prefix(10).enumerated() {
                let detail = track.artists.isEmpty
                    ? track.title : "\(track.title) — \(track.artists)"
                let item = NSMenuItem(
                    title: detail, action: #selector(searchResultClicked(_:)),
                    keyEquivalent: "")
                item.target = self
                item.tag = index
                menu.addItem(item)
            }
        }
        menu.popUp(positioning: nil,
                   at: NSPoint(x: searchField.frame.minX,
                               y: searchField.frame.maxY + 2),
                   in: self)
    }

    @objc private func searchResultClicked(_ sender: NSMenuItem) {
        guard searchResults.indices.contains(sender.tag) else { return }
        menuBand?.playSpotify(searchResults[sender.tag])
    }

    private static func mmss(_ seconds: Double) -> String {
        guard seconds.isFinite, seconds >= 0 else { return "0:00" }
        let total = Int(seconds.rounded(.down))
        return String(format: "%d:%02d", total / 60, total % 60)
    }

    deinit { artworkTask?.cancel() }
}

private final class MenuBandSpotifyProgressView: NSView {
    var duration: Double = 0 { didSet { needsDisplay = true } }
    var position: Double = 0 { didSet { needsDisplay = true } }
    var onSeek: ((Double) -> Void)?

    override func draw(_ dirtyRect: NSRect) {
        let bar = bounds.insetBy(dx: 1, dy: 3)
        NSColor.labelColor.withAlphaComponent(0.13).setFill()
        NSBezierPath(roundedRect: bar, xRadius: 2, yRadius: 2).fill()
        guard duration > 0 else { return }
        var fill = bar
        fill.size.width *= CGFloat(min(1, max(0, position / duration)))
        NSColor.systemGreen.setFill()
        NSBezierPath(roundedRect: fill, xRadius: 2, yRadius: 2).fill()
    }

    override func mouseDown(with event: NSEvent) { seek(event) }
    override func mouseDragged(with event: NSEvent) { seek(event) }

    private func seek(_ event: NSEvent) {
        guard duration > 0 else { return }
        let x = convert(event.locationInWindow, from: nil).x
        let fraction = Double(min(1, max(0, x / max(1, bounds.width))))
        onSeek?(duration * fraction)
    }
}
