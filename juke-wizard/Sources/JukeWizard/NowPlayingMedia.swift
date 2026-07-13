// NowPlayingMedia.swift — the big square in the Winamp-style header. It shows
// the track's cover art large, and when a published track has a video (a local
// reel or the CDN cut) it STREAMS that video here instead — muted, looping,
// aspect-filled — the moving accompaniment while the mp3 plays underneath.
// Replaces the old wizard-mp4 backdrop.
import AppKit
import AVFoundation

final class NowPlayingMedia: NSView {
    private var player: AVQueuePlayer?
    private var looper: AVPlayerLooper?
    private let playerLayer = AVPlayerLayer()
    private var art: NSImage?
    private var currentVideoKey: String?

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        layer?.backgroundColor = NSColor.black.withAlphaComponent(0.28).cgColor
        layer?.cornerRadius = 10
        layer?.masksToBounds = true
        layer?.borderWidth = 1
        layer?.borderColor = NSColor.white.withAlphaComponent(0.10).cgColor
        playerLayer.videoGravity = .resizeAspectFill
        playerLayer.opacity = 0
        layer?.addSublayer(playerLayer)
    }
    required init?(coder: NSCoder) { fatalError() }
    override var isFlipped: Bool { false }

    // Show the cover; stream the video if a published track has one.
    func present(art: NSImage?, videoURL: URL?) {
        self.art = art
        needsDisplay = true
        let key = videoURL?.absoluteString
        if key == currentVideoKey { return }     // already showing this clip
        currentVideoKey = key
        teardownVideo()
        guard let url = videoURL else { return }
        let item = AVPlayerItem(url: url)
        let q = AVQueuePlayer()
        q.isMuted = true                          // audio comes from the mp3
        q.actionAtItemEnd = .advance
        looper = AVPlayerLooper(player: q, templateItem: item)
        player = q
        playerLayer.player = q
        playerLayer.opacity = 1
        q.play()
    }

    func setPaused(_ paused: Bool) {
        guard player != nil else { return }
        paused ? player?.pause() : player?.play()
    }

    private func teardownVideo() {
        player?.pause()
        looper = nil
        player = nil
        playerLayer.player = nil
        playerLayer.opacity = 0
    }

    override func layout() {
        super.layout()
        CATransaction.begin(); CATransaction.setDisableActions(true)
        playerLayer.frame = bounds
        CATransaction.commit()
    }

    override func draw(_ dirtyRect: NSRect) {
        NSColor.black.withAlphaComponent(0.28).setFill()
        dirtyRect.fill()
        guard playerLayer.opacity == 0, let img = art else { return }   // art shows only when no video
        // aspect-fit the cover, centered
        let iar = img.size.width / max(1, img.size.height)
        let var_ = bounds.width / max(1, bounds.height)
        var r = bounds.insetBy(dx: 6, dy: 6)
        if iar > var_ {
            let h = r.width / iar
            r = NSRect(x: r.minX, y: r.midY - h / 2, width: r.width, height: h)
        } else {
            let w = r.height * iar
            r = NSRect(x: r.midX - w / 2, y: r.minY, width: w, height: r.height)
        }
        img.draw(in: r, from: .zero, operation: .sourceOver, fraction: 1)
    }
}
