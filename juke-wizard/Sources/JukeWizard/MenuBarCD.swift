// MenuBarCD.swift — JukeWizard's presence in the macOS menu bar: a little
// compact disc that lives up top even when the window is closed, and SPINS
// while a track plays — its rate locked to the track's BPM (one revolution
// every two beats, so the speed visibly tracks the tempo). Click it to
// show/hide the JukeWizard window; it sits near DateWizard's wand.
import AppKit

final class MenuBarCD {
    private let statusItem: NSStatusItem
    private let baseImage: NSImage
    private var timer: Timer?
    private var angle: CGFloat = 0            // degrees, clockwise
    private var bpm: Double = 120
    private var playing = false
    private let side: CGFloat = 18
    private let beatsPerRev: Double = 2       // one turn per two beats

    var onClick: (() -> Void)?

    init() {
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        baseImage = MenuBarCD.loadCD(side: side)
        if let b = statusItem.button {
            b.image = baseImage
            b.imageScaling = .scaleProportionallyDown
            b.toolTip = "JukeWizard"
            b.target = self
            b.action = #selector(clicked)
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

    @objc private func clicked() { onClick?() }

    // Feed the current track tempo; clamped to a sane spin range.
    func setBPM(_ b: Double?) {
        let v = b ?? 120
        bpm = min(200, max(40, v.isFinite && v > 0 ? v : 120))
    }

    // Start/stop the spin on a playback-state change (idempotent).
    func setPlaying(_ p: Bool) {
        guard p != playing else { return }
        playing = p
        if p { startSpin() } else { stopSpin() }
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
