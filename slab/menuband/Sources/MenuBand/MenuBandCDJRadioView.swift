import AppKit

/// Menu Band's unified external-audio deck: internet radio and Spotify share
/// one continuous CDJ channel, with one explicit handoff into Piano Sampler.
final class MenuBandCDJRadioView: NSView, NSSearchFieldDelegate {
    static let preferredSize = NSSize(width: 224, height: 170)
    static let radioHeight: CGFloat = 116

    private weak var menuBand: MenuBandController?
    private let artwork = NSImageView()
    private let deckLabel = NSTextField(labelWithString: "CDJ RADIO")
    private let titleLabel = NSTextField(labelWithString: "CDJ Radio")
    private let artistLabel = NSTextField(labelWithString: "")
    private let detailLabel = NSTextField(labelWithString: "juked headless player")
    private let timeLabel = NSTextField(labelWithString: "")
    private let progress = MenuBandSpotifyProgressView()
    private let previousButton = NSButton(title: "⏮", target: nil, action: nil)
    private let playButton = NSButton(title: "▶", target: nil, action: nil)
    private let nextButton = NSButton(title: "⏭", target: nil, action: nil)
    private let sampleButton = NSButton(
        title: "SAMPLE → PIANO", target: nil, action: nil)
    private let closeButton = NSButton(title: "×", target: nil, action: nil)
    private let searchField = NSSearchField()
    private var searchResults: [MenuBandSpotifyTrack] = []
    private var representedArtworkURL: URL?
    private var artworkTask: URLSessionDataTask?
    private var spinTimer: Timer?

    override var intrinsicContentSize: NSSize {
        NSSize(width: Self.preferredSize.width,
               height: menuBand?.cdjRadioSource == .spotify
                    ? Self.preferredSize.height : Self.radioHeight)
    }

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
        artwork.image = MenuBandCDArtworkRenderer.fallback(side: 58)

        deckLabel.font = .systemFont(ofSize: 8, weight: .bold)
        deckLabel.textColor = .secondaryLabelColor
        deckLabel.stringValue = "CDJ RADIO"

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

        sampleButton.bezelStyle = .recessed
        sampleButton.controlSize = .small
        sampleButton.font = .systemFont(ofSize: 9, weight: .semibold)
        sampleButton.target = self
        sampleButton.action = #selector(sampleClicked)
        sampleButton.toolTip = "Capture the latest 2.5 seconds into Menu Band Piano Sampler"

        closeButton.isBordered = false
        closeButton.font = .systemFont(ofSize: 15, weight: .medium)
        closeButton.contentTintColor = .secondaryLabelColor
        closeButton.target = self
        closeButton.action = #selector(closeClicked)
        closeButton.toolTip = "Stop and close CDJ Radio"

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

        [artwork, deckLabel, titleLabel, artistLabel, detailLabel, timeLabel, progress,
         previousButton, playButton, nextButton, sampleButton, closeButton,
         searchField]
            .forEach(addSubview)
        refresh()
    }

    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        super.layout()
        let width = bounds.width
        if menuBand?.cdjRadioSource != .spotify {
            artwork.frame = NSRect(x: 7, y: 51, width: 58, height: 58)
            closeButton.frame = NSRect(x: width - 25, y: 90, width: 20, height: 20)
            let textX: CGFloat = 73
            let textWidth = max(40, width - textX - 25)
            deckLabel.frame = NSRect(x: textX, y: 101, width: textWidth, height: 10)
            titleLabel.frame = NSRect(x: textX, y: 84, width: textWidth, height: 18)
            artistLabel.frame = NSRect(x: textX, y: 66, width: textWidth, height: 15)
            detailLabel.frame = NSRect(x: textX, y: 49, width: width - textX - 7,
                                        height: 14)
            sampleButton.frame = NSRect(x: 49, y: 13, width: 126, height: 26)
            return
        }
        artwork.frame = NSRect(x: 7, y: 104, width: 58, height: 58)
        closeButton.frame = NSRect(x: width - 25, y: 144, width: 20, height: 20)
        let textX: CGFloat = 73
        let textWidth = max(40, width - textX - 25)
        deckLabel.frame = NSRect(x: textX, y: 157, width: textWidth, height: 10)
        titleLabel.frame = NSRect(x: textX, y: 141, width: textWidth, height: 18)
        artistLabel.frame = NSRect(x: textX, y: 123, width: textWidth, height: 15)
        let showsError = menuBand?.spotifyStatusIsError == true
        detailLabel.frame = NSRect(
            x: textX, y: 106,
            width: max(30, width - textX - (showsError ? 7 : 73)), height: 14)
        timeLabel.frame = NSRect(x: width - 73, y: 106,
                                 width: showsError ? 0 : 66, height: 14)
        progress.frame = NSRect(x: 7, y: 89, width: width - 14, height: 10)
        previousButton.frame = NSRect(x: 49, y: 60, width: 38, height: 24)
        playButton.frame = NSRect(x: 93, y: 60, width: 38, height: 24)
        nextButton.frame = NSRect(x: 137, y: 60, width: 38, height: 24)
        sampleButton.frame = NSRect(x: 49, y: 32, width: 126, height: 24)
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
        let spotifySource = menuBand.cdjRadioSource == .spotify
        titleLabel.stringValue = menuBand.cdjRadioTitle
        artistLabel.stringValue = menuBand.cdjRadioSubtitle
        if menuBand.spotifyStatusIsError {
            detailLabel.stringValue = menuBand.spotifyStatus
            timeLabel.stringValue = ""
        } else if spotifySource, let state {
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
        [previousButton, playButton, nextButton, progress, timeLabel,
         searchField].forEach { $0.isHidden = !spotifySource }
        invalidateIntrinsicContentSize()
        updateArtwork(menuBand.cdjRadioArtworkURL)
        updateSpin(menuBand.cdjRadioPlaying)
        needsLayout = true
        needsDisplay = true
    }

    private func updateArtwork(_ url: URL?) {
        guard representedArtworkURL != url else { return }
        representedArtworkURL = url
        artworkTask?.cancel()
        artworkTask = nil
        artwork.image = MenuBandCDArtworkRenderer.fallback(side: 58)
        guard let url else { return }
        artworkTask = URLSession.shared.dataTask(with: url) { [weak self] data, _, _ in
            guard let data, let image = NSImage(data: data) else { return }
            DispatchQueue.main.async {
                guard self?.representedArtworkURL == url else { return }
                self?.artwork.image = MenuBandCDArtworkRenderer.disc(
                    from: image, side: 58)
            }
        }
        artworkTask?.resume()
    }

    @objc private func previousClicked() { menuBand?.previousSpotifyTrack() }
    @objc private func playClicked() { menuBand?.toggleSpotifyPlayback() }
    @objc private func nextClicked() { menuBand?.nextSpotifyTrack() }
    @objc private func sampleClicked() { _ = menuBand?.sampleCDJRadioToPiano() }
    @objc private func closeClicked() { menuBand?.deactivateCDJRadio() }

    private func updateSpin(_ playing: Bool) {
        if playing {
            guard spinTimer == nil else { return }
            let timer = Timer(timeInterval: 1.0 / 30.0, repeats: true) {
                [weak self] _ in
                guard let self else { return }
                self.artwork.frameCenterRotation -= 1.5
                if self.artwork.frameCenterRotation <= -360 {
                    self.artwork.frameCenterRotation += 360
                }
            }
            RunLoop.main.add(timer, forMode: .common)
            spinTimer = timer
        } else {
            spinTimer?.invalidate()
            spinTimer = nil
            artwork.frameCenterRotation = 0
        }
    }

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

    deinit {
        artworkTask?.cancel()
        spinTimer?.invalidate()
    }
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
