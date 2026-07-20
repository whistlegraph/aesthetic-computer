// DockIcon.swift — artwork-backed JukeWizard Dock record. Cover art is
// composed into a classic CD once per track; Core Animation rotates that
// cached image in the Dock compositor, with no per-frame AppKit redraw.
import AppKit
import QuartzCore

private final class DockRecordView: NSView {
    private let recordLayer = CALayer()

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.backgroundColor = NSColor.clear.cgColor
        recordLayer.frame = bounds
        recordLayer.contentsGravity = .resizeAspect
        recordLayer.magnificationFilter = .trilinear
        recordLayer.minificationFilter = .trilinear
        layer?.addSublayer(recordLayer)
    }
    required init?(coder: NSCoder) { fatalError() }

    override func layout() { recordLayer.frame = bounds }

    func set(image: NSImage, playing: Bool, bpm: Double) {
        recordLayer.contents = image.cgImage(forProposedRect: nil, context: nil, hints: nil)
        recordLayer.removeAnimation(forKey: "record-spin")
        recordLayer.transform = CATransform3DIdentity
        guard playing else { return }
        let spin = CABasicAnimation(keyPath: "transform.rotation.z")
        spin.fromValue = 0
        spin.toValue = -Double.pi * 2
        spin.duration = (60.0 / bpm) * 8.0
        spin.repeatCount = .infinity
        spin.timingFunction = CAMediaTimingFunction(name: .linear)
        spin.isRemovedOnCompletion = false
        recordLayer.add(spin, forKey: "record-spin")
    }
}

enum DockIcon {
    private static var fallbackImage: NSImage?
    private static var artwork: NSImage?
    private static var recordImage: NSImage?
    private static var bpm: Double = 120
    private static var isPlaying = false
    private static let recordView = DockRecordView(frame: NSRect(x: 0, y: 0, width: 256, height: 256))

    static func install(prefix: String) {
        apply(prefix: prefix)
        DistributedNotificationCenter.default().addObserver(
            forName: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil, queue: .main
        ) { _ in if artwork == nil { apply(prefix: prefix) } }
    }

    static func setNowPlaying(art: NSImage?, playing: Bool, bpm newBPM: Double? = nil) {
        let artChanged = artwork !== art
        let playingChanged = isPlaying != playing
        let nextBPM = min(200, max(40, newBPM ?? 120))
        let tempoChanged = abs(nextBPM - bpm) > 0.01
        artwork = art
        isPlaying = playing
        bpm = nextBPM
        if artChanged {
            recordImage = art.map { CDArtworkRenderer.disc(from: $0, side: 256, shadow: true) }
        }
        guard artChanged || playingChanged || tempoChanged else { return }
        guard let recordImage else {
            NSApp.dockTile.contentView = nil
            if let fallbackImage { NSApp.applicationIconImage = fallbackImage }
            NSApp.dockTile.display()
            return
        }
        recordView.set(image: recordImage, playing: playing, bpm: bpm)
        NSApp.dockTile.contentView = recordView
        NSApp.dockTile.display()
    }

    private static func apply(prefix: String) {
        let dark = NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let variant = dark ? "dark" : "light"
        let bundle = Bundle.module
        let image = bundle.url(forResource: "\(prefix)-icon-\(variant)",
                               withExtension: "png", subdirectory: "Assets")
            .flatMap { NSImage(contentsOf: $0) }
            ?? bundle.url(forResource: "\(prefix)-icon-\(variant)", withExtension: "png")
                .flatMap { NSImage(contentsOf: $0) }
            ?? bundle.url(forResource: "\(prefix)-mascot", withExtension: "png", subdirectory: "Assets")
                .flatMap { NSImage(contentsOf: $0) }
        if let image {
            fallbackImage = image
            if artwork == nil { NSApp.applicationIconImage = image }
        }
    }
}
