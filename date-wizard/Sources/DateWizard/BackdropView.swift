// BackdropView.swift — living video backdrop (Seedance loop of the mascot
// illy, bin/gen-backdrop.mjs) drawn aspect-fill at low opacity behind the UI.
// Falls back to the faded static mascot if the video is absent.
import AppKit
import AVFoundation

final class BackdropView: NSView {
    private var player: AVQueuePlayer?
    private var looper: AVPlayerLooper?
    private let playerLayer = AVPlayerLayer()
    private let fallbackImage: NSImage?
    private let idleOpacity: Float = 0.55

    override init(frame: NSRect) {
        fallbackImage = {
            let b = Bundle.module
            if let u = b.url(forResource: "datewizard-mascot", withExtension: "png", subdirectory: "Assets")
                ?? b.url(forResource: "datewizard-mascot", withExtension: "png"),
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
        guard let url = b.url(forResource: "datewizard-backdrop", withExtension: "mp4", subdirectory: "Assets")
            ?? b.url(forResource: "datewizard-backdrop", withExtension: "mp4") else { return }
        let item = AVPlayerItem(url: url)
        let queue = AVQueuePlayer()
        queue.isMuted = true
        looper = AVPlayerLooper(player: queue, templateItem: item)
        player = queue
        playerLayer.player = queue
        playerLayer.videoGravity = .resizeAspectFill
        playerLayer.opacity = idleOpacity
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
        let ar = img.size.width / img.size.height, varAr = bounds.width / max(1, bounds.height)
        var r = bounds
        if ar > varAr { let w = bounds.height * ar; r = NSRect(x: (bounds.width - w) * 0.5, y: 0, width: w, height: bounds.height) }
        else { let h = bounds.width / ar; r = NSRect(x: 0, y: (bounds.height - h) * 0.5, width: bounds.width, height: h) }
        img.draw(in: r, from: .zero, operation: .sourceOver, fraction: 0.10)
    }
}
