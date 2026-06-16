// ScrubPlayerView.swift — a bare AVPlayerLayer view with the two
// interactions the wizard wants and nothing else:
//   • single tap-and-drag anywhere scrubs left→right
//   • a thin progress bar pinned along the BOTTOM edge.
// (Copied from ClipWizard — the family's shared preview surface.)
import AppKit
import AVFoundation

final class ScrubPlayerView: NSView {
    private let playerLayer = AVPlayerLayer()
    private let progressTrack = CALayer()
    private let progressFill = CALayer()
    private var timeObserver: Any?
    private var wasPlayingBeforeScrub = false

    var player: AVPlayer? {
        didSet {
            if let old = oldValue, let obs = timeObserver { old.removeTimeObserver(obs) }
            timeObserver = nil
            playerLayer.player = player
            guard let p = player else { return }
            timeObserver = p.addPeriodicTimeObserver(
                forInterval: CMTime(value: 1, timescale: 30), queue: .main
            ) { [weak self] _ in self?.updateProgress() }
        }
    }

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        layer?.backgroundColor = NSColor.black.cgColor
        layer?.cornerRadius = 8
        layer?.masksToBounds = true
        playerLayer.videoGravity = .resizeAspect
        layer?.addSublayer(playerLayer)
        progressTrack.backgroundColor = NSColor.white.withAlphaComponent(0.18).cgColor
        progressFill.backgroundColor = NSColor.systemYellow.cgColor
        layer?.addSublayer(progressTrack)
        layer?.addSublayer(progressFill)
    }

    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        super.layout()
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        playerLayer.frame = bounds
        let barH: CGFloat = 5
        progressTrack.frame = CGRect(x: 0, y: 0, width: bounds.width, height: barH)
        CATransaction.commit()
        updateProgress()
    }

    private func updateProgress() {
        guard let p = player, let item = p.currentItem,
              item.duration.isNumeric, item.duration.seconds > 0 else {
            progressFill.frame = .zero
            return
        }
        let frac = CGFloat(p.currentTime().seconds / item.duration.seconds)
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        progressFill.frame = CGRect(x: 0, y: 0,
                                    width: bounds.width * max(0, min(1, frac)), height: 5)
        CATransaction.commit()
    }

    private func scrub(to event: NSEvent) {
        guard let p = player, let item = p.currentItem,
              item.duration.isNumeric, item.duration.seconds > 0 else { return }
        let x = convert(event.locationInWindow, from: nil).x
        let frac = max(0, min(1, x / bounds.width))
        let t = CMTime(seconds: Double(frac) * item.duration.seconds, preferredTimescale: 600)
        p.seek(to: t, toleranceBefore: .zero, toleranceAfter: .zero)
        updateProgress()
    }

    override func mouseDown(with event: NSEvent) {
        wasPlayingBeforeScrub = (player?.rate ?? 0) > 0
        player?.pause()
        scrub(to: event)
    }

    override func mouseDragged(with event: NSEvent) { scrub(to: event) }

    override func mouseUp(with event: NSEvent) {
        if wasPlayingBeforeScrub { player?.play() }
    }
}
