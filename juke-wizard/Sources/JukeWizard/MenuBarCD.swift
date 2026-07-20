// MenuBarCD.swift — JukeWizard's presence in the macOS menu bar: a little
// compact disc that lives up top even when the window is closed, and SPINS
// while a track plays — its rate locked to the track's BPM (one revolution
// every two beats, so the speed visibly tracks the tempo). Click it to
// show/hide the JukeWizard window; it sits near DateWizard's wand.
import AppKit

/// A transport glyph embossed directly into the menu-bar faceplate. There is
/// deliberately no button body: the colored symbol itself rises, glows, and
/// presses into the bar like the legends on MenuBand's instrument controls.
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
        let accent = shiftedAccent()
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let glyphColor = lit
            ? (accent.highlight(withLevel: 0.40) ?? accent)
            : hovered
                ? accent
                : (dark ? accent.highlight(withLevel: 0.22) ?? accent
                        : accent.shadow(withLevel: 0.12) ?? accent)
        let shadow = NSShadow()
        shadow.shadowColor = latched
            ? accent.withAlphaComponent(0.90)
            : NSColor.black.withAlphaComponent(pressed ? 0.18 : 0.48)
        shadow.shadowOffset = NSSize(width: 0, height: pressed ? 0 : -1)
        shadow.shadowBlurRadius = latched ? 2.8 : (pressed ? 0 : 0.8)
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: title == "❚❚" ? 9.0 : 10.5, weight: .black),
            .foregroundColor: glyphColor,
            .shadow: shadow,
        ]
        let glyph = NSAttributedString(string: title, attributes: attrs)
        let size = glyph.size()
        let pressY: CGFloat = pressed ? -0.8 : 0.5
        glyph.draw(at: NSPoint(x: bounds.midX - size.width / 2,
                               y: bounds.midY - size.height / 2 + pressY))
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

/// Six discrete volume zones lifted directly from video.mjs's tape wedge.
/// The wedge is the control—there is no surrounding slider or button body.
private final class MenuBarVolumeWedge: NSControl {
    private static let colors: [NSColor] = [
        .systemRed, .systemOrange, .systemYellow,
        .systemGreen, .systemCyan, .systemPink,
    ]
    private(set) var level: Float = 0.8
    private var pressed = false

    override var acceptsFirstResponder: Bool { false }

    func setValue(_ value: Float) {
        level = max(0, min(1, value))
        needsDisplay = true
    }

    override func mouseDown(with event: NSEvent) {
        pressed = true
        updateValue(from: event)
    }

    override func mouseDragged(with event: NSEvent) { updateValue(from: event) }

    override func mouseUp(with event: NSEvent) {
        updateValue(from: event)
        pressed = false
        needsDisplay = true
        sendAction(action, to: target)
    }

    private func updateValue(from event: NSEvent) {
        let x = convert(event.locationInWindow, from: nil).x
        let segment = min(5, max(0, Int((x / max(1, bounds.width)) * 6)))
        level = Float(segment + 1) / 6
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let width = bounds.width
        let height = bounds.height - 3
        let baseY: CGFloat = 1.5
        let litSegments = min(6, max(0, Int(ceil(level * 6))))

        // Recess the rainbow into the menu-bar surface with a tiny dark echo.
        let silhouette = NSBezierPath()
        silhouette.move(to: NSPoint(x: 0, y: baseY - 1))
        silhouette.line(to: NSPoint(x: width, y: baseY - 1))
        silhouette.line(to: NSPoint(x: width, y: baseY + height - 1))
        silhouette.close()
        NSColor.black.withAlphaComponent(pressed ? 0.12 : 0.28).setFill()
        silhouette.fill()

        let segmentWidth = width / 6
        for segment in 0..<6 {
            let x0 = CGFloat(segment) * segmentWidth
            let x1 = CGFloat(segment + 1) * segmentWidth
            let h0 = max(1, height * x0 / width)
            let h1 = max(1, height * x1 / width)
            let bar = NSBezierPath()
            bar.move(to: NSPoint(x: x0 + 0.5, y: baseY))
            bar.line(to: NSPoint(x: x1 - 0.5, y: baseY))
            bar.line(to: NSPoint(x: x1 - 0.5, y: baseY + h1))
            bar.line(to: NSPoint(x: x0 + 0.5, y: baseY + h0))
            bar.close()
            let lit = segment < litSegments
            Self.colors[segment].withAlphaComponent(lit ? 0.95 : 0.20).setFill()
            bar.fill()
            NSColor.white.withAlphaComponent(lit ? 0.30 : 0.08).setStroke()
            bar.lineWidth = 0.5
            bar.stroke()
        }

        // Bright fader notch at the selected quantized boundary.
        let handleX = min(width - 1, max(1, width * CGFloat(level)))
        let handleHeight = max(4, height * CGFloat(level))
        let handle = NSBezierPath()
        handle.move(to: NSPoint(x: handleX, y: baseY - 1))
        handle.line(to: NSPoint(x: handleX, y: baseY + handleHeight + 1))
        NSColor.white.withAlphaComponent(0.92).setStroke()
        handle.lineWidth = 1.2
        handle.stroke()
    }
}

final class MenuBarCD {
    private let statusItem: NSStatusItem
    private let titleButton = NSButton(title: "JukeWizard", target: nil, action: nil)
    private let previousButton = MenuBarKeyButton("⏮", hueOffset: -0.075)
    private let playButton = MenuBarKeyButton("▶", hueOffset: 0)
    private let nextButton = MenuBarKeyButton("⏭", hueOffset: 0.075)
    private let volumeSlider = MenuBarVolumeWedge()
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
    @objc private func volumeChanged() { onVolumeChanged?(volumeSlider.level) }

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
        volumeSlider.setValue(value)
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
