// MenuBarCD.swift — JukeWizard's presence in the macOS menu bar: a little
// compact disc that lives up top even when the window is closed, and SPINS
// while a track plays — its rate locked to the track's BPM (one revolution
// every two beats, so the speed visibly tracks the tempo). Click it to
// show/hide the JukeWizard window; it sits near DateWizard's wand.
import AppKit

/// A tiny MenuBand-style keycap: system-accent driven, softly dimensional,
/// and visibly depressed while clicked. Hue offsets give the transport a
/// little rainbow without disconnecting it from the user's chosen accent.
private final class MenuBarKeyButton: NSButton {
    private let hueOffset: CGFloat
    private var hovered = false
    private var hoverTrackingArea: NSTrackingArea?
    var latched = false { didSet { needsDisplay = true } }

    init(_ glyph: String, hueOffset: CGFloat) {
        self.hueOffset = hueOffset
        super.init(frame: .zero)
        title = glyph
        isBordered = false
        focusRingType = .none
        setButtonType(.momentaryChange)
    }
    required init?(coder: NSCoder) { fatalError() }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let hoverTrackingArea { removeTrackingArea(hoverTrackingArea) }
        let tracking = NSTrackingArea(rect: bounds,
                                      options: [.mouseEnteredAndExited, .activeAlways],
                                      owner: self, userInfo: nil)
        addTrackingArea(tracking)
        hoverTrackingArea = tracking
    }

    override func mouseEntered(with event: NSEvent) { hovered = true; needsDisplay = true }
    override func mouseExited(with event: NSEvent) { hovered = false; needsDisplay = true }

    override func highlight(_ flag: Bool) {
        super.highlight(flag)
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let pressed = isHighlighted
        let lit = pressed || latched
        let offset: CGFloat = pressed ? -0.7 : 0
        let inset: CGFloat = pressed ? 2.2 : 1.3
        let rect = bounds.insetBy(dx: inset, dy: 2.2).offsetBy(dx: 0, dy: offset)
        let cap = NSBezierPath(roundedRect: rect, xRadius: 4.2, yRadius: 4.2)
        let accent = shiftedAccent()
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua

        NSGraphicsContext.saveGraphicsState()
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(pressed ? 0.16 : 0.34)
        shadow.shadowOffset = NSSize(width: 0, height: pressed ? 0 : -1)
        shadow.shadowBlurRadius = pressed ? 0 : 1.2
        shadow.set()
        let top = lit
            ? (accent.highlight(withLevel: 0.34) ?? accent)
            : accent.withAlphaComponent(hovered ? 0.42 : (dark ? 0.29 : 0.22))
        let bottom = lit
            ? (accent.shadow(withLevel: 0.24) ?? accent)
            : accent.withAlphaComponent(hovered ? 0.24 : (dark ? 0.14 : 0.10))
        cap.addClip()
        NSGradient(starting: top, ending: bottom)?.draw(in: rect, angle: -90)
        NSGraphicsContext.restoreGraphicsState()

        accent.withAlphaComponent(lit ? 0.95 : (hovered ? 0.78 : 0.55)).setStroke()
        cap.lineWidth = lit ? 1.0 : 0.7
        cap.stroke()

        // A hairline glint makes the cap read as glossy molded plastic.
        let glint = NSBezierPath()
        glint.move(to: NSPoint(x: rect.minX + 4, y: rect.maxY - 1.6))
        glint.line(to: NSPoint(x: rect.maxX - 4, y: rect.maxY - 1.6))
        NSColor.white.withAlphaComponent(lit ? 0.34 : 0.22).setStroke()
        glint.lineWidth = 0.7
        glint.stroke()

        let glyphColor: NSColor = lit
            ? .white
            : (dark ? accent.highlight(withLevel: 0.42) ?? accent : accent.shadow(withLevel: 0.16) ?? accent)
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: title == "❚❚" ? 8.5 : 9.5, weight: .heavy),
            .foregroundColor: glyphColor,
        ]
        let glyph = NSAttributedString(string: title, attributes: attrs)
        let size = glyph.size()
        glyph.draw(at: NSPoint(x: rect.midX - size.width / 2,
                               y: rect.midY - size.height / 2 - 0.5))
    }

    private func shiftedAccent() -> NSColor {
        guard hueOffset != 0,
              let rgb = NSColor.controlAccentColor.usingColorSpace(.sRGB) else {
            return .controlAccentColor
        }
        var hue: CGFloat = 0, saturation: CGFloat = 0, brightness: CGFloat = 0, alpha: CGFloat = 0
        rgb.getHue(&hue, saturation: &saturation, brightness: &brightness, alpha: &alpha)
        hue = (hue + hueOffset).truncatingRemainder(dividingBy: 1)
        if hue < 0 { hue += 1 }
        return NSColor(calibratedHue: hue,
                       saturation: min(1, max(0.48, saturation)),
                       brightness: min(1, max(0.72, brightness)), alpha: alpha)
    }
}

final class MenuBarCD {
    private let statusItem: NSStatusItem
    private let titleButton = NSButton(title: "JukeWizard", target: nil, action: nil)
    private let previousButton = MenuBarKeyButton("⏮", hueOffset: -0.075)
    private let playButton = MenuBarKeyButton("▶", hueOffset: 0)
    private let nextButton = MenuBarKeyButton("⏭", hueOffset: 0.075)
    private let volumeSlider = NSSlider(value: 0.8, minValue: 0, maxValue: 1,
                                        target: nil, action: nil)
    private let discButton = NSButton(title: "", target: nil, action: nil)
    private let fallbackImage: NSImage
    private var baseImage: NSImage
    private var timer: Timer?
    private var angle: CGFloat = 0            // degrees, clockwise
    private var bpm: Double = 120
    private var playing = false
    private let side: CGFloat = 21
    private var titleWidth: CGFloat = 72
    private let beatsPerRev: Double = 8       // calm turntable pace
    private var currentTitle = ""
    private var currentArtwork: NSImage?

    var onOpen: (() -> Void)?
    var onPrevious: (() -> Void)?
    var onTogglePlay: (() -> Void)?
    var onNext: (() -> Void)?
    var onVolumeChanged: ((Float) -> Void)?

    init() {
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        fallbackImage = MenuBarCD.loadCD(side: side)
        baseImage = fallbackImage
        if let b = statusItem.button {
            b.title = ""
            b.image = nil
            b.toolTip = "JukeWizard"
            installControls(in: b)
        }
    }

    private func installControls(in barButton: NSStatusBarButton) {
        titleButton.target = self
        titleButton.action = #selector(openFull)
        titleButton.isBordered = false
        titleButton.font = .systemFont(ofSize: 12, weight: .medium)
        titleButton.alignment = .left
        titleButton.lineBreakMode = .byTruncatingTail

        previousButton.target = self
        previousButton.action = #selector(previous)
        previousButton.toolTip = "Previous"
        playButton.target = self
        playButton.action = #selector(togglePlay)
        playButton.toolTip = "Play / Pause"
        nextButton.target = self
        nextButton.action = #selector(next)
        nextButton.toolTip = "Next"

        volumeSlider.target = self
        volumeSlider.action = #selector(volumeChanged)
        volumeSlider.controlSize = .mini
        // Commit on mouse-up so a drag does not enqueue dozens of Spotify
        // daemon volume commands while the knob is moving.
        volumeSlider.isContinuous = false
        volumeSlider.toolTip = "Volume"

        discButton.target = self
        discButton.action = #selector(openFull)
        discButton.isBordered = false
        discButton.image = baseImage
        discButton.imageScaling = .scaleProportionallyDown
        discButton.toolTip = "Open JukeWizard"

        [titleButton, previousButton, playButton, nextButton, volumeSlider, discButton]
            .forEach(barButton.addSubview)
        layoutControls()
    }

    private func layoutControls() {
        let height = NSStatusBar.system.thickness
        let buttonHeight = min(22, height)
        let y = max(0, (height - buttonHeight) / 2)
        var x: CGFloat = 3
        titleButton.frame = NSRect(x: x, y: y, width: titleWidth, height: buttonHeight)
        x += titleWidth + 2
        for button in [previousButton, playButton, nextButton] {
            button.frame = NSRect(x: x, y: y, width: 23, height: buttonHeight)
            x += 23
        }
        x += 2
        volumeSlider.frame = NSRect(x: x, y: y + 2, width: 50, height: buttonHeight - 4)
        x += 54
        discButton.frame = NSRect(x: x, y: y, width: side + 3, height: buttonHeight)
        x += side + 6
        statusItem.length = x
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

    @objc private func openFull() { onOpen?() }
    @objc private func previous() { onPrevious?() }
    @objc private func togglePlay() { onTogglePlay?() }
    @objc private func next() { onNext?() }
    @objc private func volumeChanged() { onVolumeChanged?(volumeSlider.floatValue) }

    // Kept for the room mixer's anchored popover; the main music controls no
    // longer use a popover.
    func show(_ popover: NSPopover) {
        guard let button = statusItem.button else { return }
        popover.show(relativeTo: button.bounds, of: button, preferredEdge: .minY)
    }

    func setNowPlaying(title: String, art: NSImage?) {
        let clipped = title.count > 24 ? String(title.prefix(23)) + "…" : title
        let changedTrack = !currentTitle.isEmpty &&
            (clipped != currentTitle || currentArtwork !== art)
        guard clipped != currentTitle || currentArtwork !== art else { return }
        currentTitle = clipped
        currentArtwork = art
        let shownTitle = clipped.isEmpty ? "JukeWizard" : clipped
        titleButton.title = shownTitle
        let measured = (shownTitle as NSString).size(withAttributes: [.font: titleButton.font!]).width
        titleWidth = min(154, max(56, ceil(measured) + 8))
        layoutControls()
        baseImage = art.map { CDArtworkRenderer.disc(from: $0, side: side) } ?? fallbackImage
        discButton.image = angle == 0 ? baseImage : rotated(baseImage, by: angle)
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
        playButton.title = p ? "❚❚" : "▶"
        playButton.latched = p
        if p { startSpin() } else { stopSpin() }
        if resumed, let button = statusItem.button { MenuBarNoteBurst.emit(from: button) }
    }

    func setVolume(_ value: Float) {
        volumeSlider.floatValue = max(0, min(1, value))
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
        discButton.image = baseImage           // settle upright when paused
    }

    private func tick() {
        // revolutions per second = (bpm/60) / beatsPerRev  →  degrees per frame at 30 fps
        let degPerFrame = 360.0 * (bpm / 60.0) / beatsPerRev / 30.0
        angle -= CGFloat(degPerFrame)           // clockwise, like a turntable
        if angle <= -360 { angle += 360 }
        discButton.image = rotated(baseImage, by: angle)
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
