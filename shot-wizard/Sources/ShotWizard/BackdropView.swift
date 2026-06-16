// BackdropView.swift — faded mascot illustration behind the UI (falls back
// to the static mascot if no backdrop video is present). (From ClipWizard;
// ShotWizard ships without a backdrop loop, so the static fallback draws.)
import AppKit
import AVFoundation

final class BackdropView: NSView {
    private var player: AVQueuePlayer?
    private var looper: AVPlayerLooper?
    private let playerLayer = AVPlayerLayer()
    private let fallbackImage: NSImage?

    override init(frame: NSRect) {
        fallbackImage = {
            let b = Bundle.module
            if let u = b.url(forResource: "shotwizard-mascot", withExtension: "png", subdirectory: "Assets")
                ?? b.url(forResource: "shotwizard-mascot", withExtension: "png"),
               let img = NSImage(contentsOf: u) { return img }
            return nil
        }()
        super.init(frame: frame)
        wantsLayer = true
        setupVideo()
    }
    required init?(coder: NSCoder) { fatalError() }
    override func hitTest(_ point: NSPoint) -> NSView? { nil }

    private func setupVideo() {
        let b = Bundle.module
        guard let url = b.url(forResource: "shotwizard-backdrop", withExtension: "mp4", subdirectory: "Assets")
            ?? b.url(forResource: "shotwizard-backdrop", withExtension: "mp4") else { return }
        let item = AVPlayerItem(url: url)
        let queue = AVQueuePlayer()
        queue.isMuted = true
        looper = AVPlayerLooper(player: queue, templateItem: item)
        player = queue
        playerLayer.player = queue
        playerLayer.videoGravity = .resizeAspectFill
        playerLayer.opacity = 0.16
        layer?.addSublayer(playerLayer)
        queue.play()
    }
    override func viewDidMoveToWindow() { super.viewDidMoveToWindow(); if window != nil { player?.play() } }
    override func layout() {
        super.layout()
        CATransaction.begin(); CATransaction.setDisableActions(true)
        playerLayer.frame = bounds
        CATransaction.commit()
    }
    override func draw(_ dirtyRect: NSRect) {
        guard player == nil, let img = fallbackImage else { return }
        let ar = img.size.width / img.size.height, viewAR = bounds.width / max(1, bounds.height)
        var r = bounds
        if ar > viewAR { let w = bounds.height * ar; r = NSRect(x: (bounds.width - w) * 0.5, y: 0, width: w, height: bounds.height) }
        else { let h = bounds.width / ar; r = NSRect(x: 0, y: (bounds.height - h) * 0.5, width: bounds.width, height: h) }
        img.draw(in: r, from: .zero, operation: .sourceOver, fraction: 0.10)
    }
}
