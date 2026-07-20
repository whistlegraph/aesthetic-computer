// MenuBarCD.swift — JukeWizard's presence in the macOS menu bar: a little
// compact disc that lives up top even when the window is closed, and SPINS
// while a track plays — its rate locked to the track's BPM (one revolution
// every two beats, so the speed visibly tracks the tempo). Click it to
// show/hide the JukeWizard window; it sits near DateWizard's wand.
import AppKit

final class MenuBarCD {
    private let statusItem: NSStatusItem
    private let fallbackImage: NSImage
    private var baseImage: NSImage
    private var timer: Timer?
    private var angle: CGFloat = 0            // degrees, clockwise
    private var bpm: Double = 120
    private var playing = false
    private let side: CGFloat = 22
    private let beatsPerRev: Double = 8       // calm turntable pace
    private var currentTitle = ""
    private var currentArtwork: NSImage?
    private var clickGeneration = 0

    var onClick: (() -> Void)?
    var onDoubleClick: (() -> Void)?

    init() {
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        fallbackImage = MenuBarCD.loadCD(side: side)
        baseImage = fallbackImage
        if let b = statusItem.button {
            b.image = baseImage
            b.imagePosition = .imageRight
            b.imageScaling = .scaleProportionallyDown
            b.toolTip = "JukeWizard"
            b.target = self
            b.action = #selector(clicked)
            b.sendAction(on: [.leftMouseUp, .rightMouseUp])
        }
    }

    private static func loadCD(side: CGFloat) -> NSImage {
        let bundle = Bundle.module
        let url = bundle.url(forResource: "jukewizard-cd", withExtension: "png", subdirectory: "Assets")
            ?? bundle.url(forResource: "jukewizard-cd", withExtension: "png")
        let img = (url.flatMap { NSImage(contentsOf: $0) }) ?? NSImage(size: NSSize(width: side, height: side))
        img.size = NSSize(width: side, height: side)
        img.isTemplate = false               // keep the iridescent color in the bar
        return img
    }

    @objc private func clicked() {
        if NSApp.currentEvent?.type == .rightMouseUp {
            onClick?()
            return
        }
        clickGeneration += 1
        let generation = clickGeneration
        if (NSApp.currentEvent?.clickCount ?? 1) >= 2 {
            onDoubleClick?()
        } else {
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.18) { [weak self] in
                guard let self, self.clickGeneration == generation else { return }
                self.onClick?()
            }
        }
    }

    func show(_ popover: NSPopover) {
        guard let button = statusItem.button else { return }
        popover.show(relativeTo: button.bounds, of: button, preferredEdge: .minY)
    }

    func setNowPlaying(title: String, art: NSImage?) {
        let clipped = title.count > 28 ? String(title.prefix(27)) + "…" : title
        let changedTrack = !currentTitle.isEmpty &&
            (clipped != currentTitle || currentArtwork !== art)
        guard clipped != currentTitle || currentArtwork !== art else { return }
        currentTitle = clipped
        currentArtwork = art
        statusItem.button?.title = clipped.isEmpty ? "" : "\(clipped)  "
        statusItem.button?.font = .systemFont(ofSize: 12, weight: .medium)
        baseImage = art.map { CDArtworkRenderer.disc(from: $0, side: side) } ?? fallbackImage
        statusItem.button?.image = angle == 0 ? baseImage : rotated(baseImage, by: angle)
        statusItem.button?.toolTip = clipped.isEmpty ? "JukeWizard" : clipped
        if changedTrack, let button = statusItem.button { MenuBarNoteBurst.emit(from: button) }
    }

    // Feed the current track tempo; clamped to a sane spin range.
    func setBPM(_ b: Double?) {
        let v = b ?? 120
        bpm = min(200, max(40, v.isFinite && v > 0 ? v : 120))
    }

    // Start/stop the spin on a playback-state change (idempotent).
    func setPlaying(_ p: Bool) {
        guard p != playing else { return }
        let resumed = p && !playing
        playing = p
        if p { startSpin() } else { stopSpin() }
        if resumed, let button = statusItem.button { MenuBarNoteBurst.emit(from: button) }
    }

    private func startSpin() {
        timer?.invalidate()
        // ~30 fps; smooth enough for a small bar glyph, cheap to redraw.
        let t = Timer(timeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in self?.tick() }
        RunLoop.main.add(t, forMode: .common)   // keep spinning during menu tracking / resize
        timer = t
    }

    private func stopSpin() {
        timer?.invalidate(); timer = nil
        angle = 0
        statusItem.button?.image = baseImage    // settle upright when paused
    }

    private func tick() {
        // revolutions per second = (bpm/60) / beatsPerRev  →  degrees per frame at 30 fps
        let degPerFrame = 360.0 * (bpm / 60.0) / beatsPerRev / 30.0
        angle -= CGFloat(degPerFrame)           // clockwise, like a turntable
        if angle <= -360 { angle += 360 }
        statusItem.button?.image = rotated(baseImage, by: angle)
    }

    private func rotated(_ img: NSImage, by deg: CGFloat) -> NSImage {
        let size = img.size
        let out = NSImage(size: size)
        out.lockFocus()
        NSGraphicsContext.current?.imageInterpolation = .high
        let t = NSAffineTransform()
        t.translateX(by: size.width / 2, yBy: size.height / 2)
        t.rotate(byDegrees: deg)
        t.translateX(by: -size.width / 2, yBy: -size.height / 2)
        t.concat()
        img.draw(at: .zero, from: NSRect(origin: .zero, size: size),
                 operation: .sourceOver, fraction: 1)
        out.unlockFocus()
        out.isTemplate = false
        return out
    }

}
