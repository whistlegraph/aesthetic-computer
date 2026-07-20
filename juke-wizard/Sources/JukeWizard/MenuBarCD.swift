// MenuBarCD.swift — JukeWizard's presence in the macOS menu bar: a little
// compact disc that lives up top even when the window is closed, and SPINS
// while a track plays — its rate locked to the track's BPM (one revolution
// every two beats, so the speed visibly tracks the tempo). Click it to
// show/hide the JukeWizard window; it sits near DateWizard's wand.
import AppKit

private final class MenuBarTitleButton: NSButton {
    private var hovered = false
    private var hoverTrackingArea: NSTrackingArea?

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let hoverTrackingArea { removeTrackingArea(hoverTrackingArea) }
        let tracking = NSTrackingArea(rect: .zero,
                                      options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
                                      owner: self, userInfo: nil)
        addTrackingArea(tracking)
        hoverTrackingArea = tracking
    }

    override func resetCursorRects() {
        super.resetCursorRects()
        addCursorRect(bounds, cursor: .pointingHand)
    }

    override func mouseEntered(with event: NSEvent) { hovered = true; needsDisplay = true }
    override func mouseExited(with event: NSEvent) { hovered = false; needsDisplay = true }
    override func highlight(_ flag: Bool) { super.highlight(flag); needsDisplay = true }

    override func draw(_ dirtyRect: NSRect) {
        let pressed = isHighlighted
        let color: NSColor = hovered ? .controlAccentColor : .labelColor
        let shadow = NSShadow()
        shadow.shadowColor = hovered
            ? NSColor.controlAccentColor.withAlphaComponent(0.48)
            : NSColor.black.withAlphaComponent(0.24)
        shadow.shadowOffset = NSSize(width: 0, height: pressed ? 0 : -0.5)
        shadow.shadowBlurRadius = hovered ? 2.0 : 0.4
        let paragraph = NSMutableParagraphStyle()
        paragraph.lineBreakMode = .byTruncatingTail
        paragraph.alignment = .left
        let text = NSAttributedString(string: title, attributes: [
            .font: font ?? NSFont.systemFont(ofSize: 12, weight: .medium),
            .foregroundColor: color,
            .shadow: shadow,
            .paragraphStyle: paragraph,
        ])
        text.draw(in: bounds.insetBy(dx: 1, dy: 3).offsetBy(dx: 0, dy: pressed ? -0.7 : 0))
    }
}

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

    override func resetCursorRects() {
        super.resetCursorRects()
        addCursorRect(bounds, cursor: .pointingHand)
    }

    override func highlight(_ flag: Bool) {
        super.highlight(flag)
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let pressed = isHighlighted
        let lit = pressed || latched
        let accent = shiftedAccent()
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let font = NSFont.systemFont(ofSize: (title == "❚❚" ? 10.8 : 12.8) + (hovered ? 1.0 : 0),
                                     weight: .black)
        let measure = NSAttributedString(string: title, attributes: [.font: font])
        let size = measure.size()
        let pressY: CGFloat = pressed ? -0.8 : 0.5
        let origin = NSPoint(x: bounds.midX - size.width / 2,
                             y: bounds.midY - size.height / 2 + pressY)

        // Hard Riso-style offset first, then an accent enamel keyline.
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(pressed ? 0.24 : 0.68)
        shadow.shadowOffset = NSSize(width: pressed ? 0 : 1, height: pressed ? 0 : -1)
        shadow.shadowBlurRadius = 0
        let outline = (accent.shadow(withLevel: dark ? 0.18 : 0.42) ?? accent)
            .withAlphaComponent(hovered || lit ? 1 : 0.88)
        let outlineGlyph = NSAttributedString(string: title, attributes: [
            .font: font,
            .foregroundColor: NSColor.clear,
            .strokeColor: outline,
            .strokeWidth: 3.2,
            .shadow: shadow,
        ])
        outlineGlyph.draw(at: origin)

        // MenuBand white-key gradient clipped inside the glyph. Hover and
        // playback gently pick up the system accent at the lower edge.
        let whiteHi: NSColor
        let whiteLo: NSColor
        if dark {
            whiteHi = NSColor(srgbRed: 112/255, green: 124/255, blue: 134/255, alpha: 1)
            whiteLo = NSColor(srgbRed: 58/255, green: 68/255, blue: 76/255, alpha: 1)
        } else {
            whiteHi = NSColor(srgbRed: 235/255, green: 242/255, blue: 245/255, alpha: 1)
            whiteLo = NSColor(srgbRed: 195/255, green: 205/255, blue: 210/255, alpha: 1)
        }
        let tintAmount: CGFloat = latched ? 0.72 : (hovered ? 0.62 : 0.10)
        let upper = (hovered || latched)
            ? (whiteHi.blended(withFraction: hovered ? 0.20 : 0.14, of: accent) ?? whiteHi)
            : whiteHi
        let lower = whiteLo.blended(withFraction: tintAmount, of: accent) ?? whiteLo
        let mask = NSImage(size: bounds.size)
        mask.lockFocus()
        NSAttributedString(string: title, attributes: [
            .font: font, .foregroundColor: NSColor.white,
        ]).draw(at: origin)
        mask.unlockFocus()
        let fill = NSImage(size: bounds.size)
        fill.lockFocus()
        NSGradient(starting: upper, ending: lower)?.draw(in: bounds, angle: -90)
        mask.draw(at: .zero, from: NSRect(origin: .zero, size: bounds.size),
                  operation: .destinationIn, fraction: 1)
        fill.unlockFocus()
        fill.draw(at: .zero, from: NSRect(origin: .zero, size: bounds.size),
                  operation: .sourceOver, fraction: 1)
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

/// Six discrete volume zones lifted from video.mjs, stacked vertically for
/// the temporary CD-drag popover. Louder levels climb upward and widen.
private final class MenuBarVerticalVolumeMeter: NSView {
    private static let colors: [NSColor] = [
        .systemRed, .systemOrange, .systemYellow,
        .systemGreen, .systemCyan, .systemPink,
    ]
    private(set) var level: Float = 0.8

    func setValue(_ value: Float) {
        level = max(0, min(1, value))
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let litSegments = min(6, max(0, Int(ceil(level * 6))))
        let gap: CGFloat = 3
        let segmentHeight = (bounds.height - gap * 7) / 6
        for segment in 0..<6 {
            let widthFraction = 0.42 + CGFloat(segment) * 0.105
            let width = (bounds.width - 10) * widthFraction
            let y = gap + CGFloat(segment) * (segmentHeight + gap)
            let rect = NSRect(x: bounds.midX - width / 2, y: y,
                              width: width, height: segmentHeight)
            let bar = NSBezierPath(roundedRect: rect, xRadius: 2.5, yRadius: 2.5)
            let lit = segment < litSegments
            let color = Self.colors[segment]
            let top = lit ? (color.highlight(withLevel: 0.36) ?? color)
                          : color.withAlphaComponent(0.22)
            let bottom = lit ? (color.shadow(withLevel: 0.18) ?? color)
                             : color.withAlphaComponent(0.10)
            NSGradient(starting: top, ending: bottom)?.draw(in: bar, angle: -90)
            NSColor.white.withAlphaComponent(lit ? 0.55 : 0.12).setStroke()
            bar.lineWidth = 0.7
            bar.stroke()
            if segment + 1 == litSegments {
                NSColor.controlAccentColor.setStroke()
                bar.lineWidth = 1.5
                bar.stroke()
            }
        }
    }
}

private final class MenuBarDiscButton: NSButton {
    private var hovered = false
    private var hoverTrackingArea: NSTrackingArea?
    var level: Float = 0.8
    var onClick: (() -> Void)?
    var onVolumeDragBegan: (() -> Void)?
    var onVolumeChanged: ((Float) -> Void)?
    var onVolumeDragEnded: (() -> Void)?
    private var dragStartY: CGFloat = 0
    private var dragStartStep = 1
    private var dragLastStep = 1
    private var draggingVolume = false

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let hoverTrackingArea { removeTrackingArea(hoverTrackingArea) }
        let tracking = NSTrackingArea(rect: .zero,
                                      options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
                                      owner: self, userInfo: nil)
        addTrackingArea(tracking)
        hoverTrackingArea = tracking
    }

    override func resetCursorRects() {
        super.resetCursorRects()
        addCursorRect(bounds, cursor: .pointingHand)
    }

    override func mouseEntered(with event: NSEvent) { hovered = true; needsDisplay = true }
    override func mouseExited(with event: NSEvent) { hovered = false; needsDisplay = true }
    override func highlight(_ flag: Bool) { super.highlight(flag); needsDisplay = true }

    override func mouseDown(with event: NSEvent) {
        dragStartY = NSEvent.mouseLocation.y
        dragStartStep = min(6, max(1, Int((level * 6).rounded())))
        dragLastStep = dragStartStep
        draggingVolume = false
        highlight(true)
    }

    override func mouseDragged(with event: NSEvent) {
        let deltaY = NSEvent.mouseLocation.y - dragStartY
        if !draggingVolume, abs(deltaY) >= 3 {
            draggingVolume = true
            onVolumeDragBegan?()
        }
        if draggingVolume {
            // The record sits against the top screen edge, so upward travel
            // is scarce. Two points per notch still gives all six levels.
            let step = min(6, max(1, dragStartStep + Int(deltaY / 2)))
            if step != dragLastStep {
                dragLastStep = step
                level = Float(step) / 6
                onVolumeChanged?(level)
            }
        }
    }

    override func mouseUp(with event: NSEvent) {
        highlight(false)
        if draggingVolume { onVolumeDragEnded?() }
        else { onClick?() }
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let image else { return }
        let pressed = isHighlighted
        let scale: CGFloat = pressed ? 0.93 : (hovered ? 1.08 : 1)
        let side = min(bounds.width, bounds.height) * scale
        let imageRect = NSRect(x: bounds.midX - side / 2,
                               y: bounds.midY - side / 2 + (pressed ? -0.7 : 0),
                               width: side, height: side)
        if hovered {
            let halo = NSBezierPath(ovalIn: imageRect.insetBy(dx: -1.2, dy: -1.2))
            NSColor.controlAccentColor.withAlphaComponent(0.76).setStroke()
            halo.lineWidth = 1.2
            halo.stroke()
        }
        image.draw(in: imageRect, from: .zero, operation: .sourceOver, fraction: 1,
                   respectFlipped: true, hints: [.interpolation: NSImageInterpolation.high])
    }
}

final class MenuBarCD {
    private let statusItem: NSStatusItem
    private let titleButton = MenuBarTitleButton(title: "JukeWizard", target: nil, action: nil)
    private let previousButton = MenuBarKeyButton("⏮", hueOffset: -0.075)
    private let playButton = MenuBarKeyButton("▶", hueOffset: 0)
    private let nextButton = MenuBarKeyButton("⏭", hueOffset: 0.075)
    private let volumeMeter = MenuBarVerticalVolumeMeter()
    private let discButton = MenuBarDiscButton(title: "", target: nil, action: nil)
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
    private var volumePopover: NSPopover?

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

        discButton.isBordered = false
        discButton.image = baseImage
        discButton.imageScaling = .scaleProportionallyDown
        discButton.toolTip = "Open JukeWizard · drag up/down for volume"
        discButton.onClick = { [weak self] in self?.onOpen?() }
        discButton.onVolumeDragBegan = { [weak self] in self?.setVolumeGestureVisible(true) }
        discButton.onVolumeChanged = { [weak self] value in
            self?.volumeMeter.setValue(value)
            self?.onVolumeChanged?(value)
        }
        discButton.onVolumeDragEnded = { [weak self] in self?.setVolumeGestureVisible(false) }

        [titleButton, previousButton, playButton, nextButton, discButton]
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
            button.frame = NSRect(x: x, y: y, width: 29, height: buttonHeight)
            x += 29
        }
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

    private func setVolumeGestureVisible(_ visible: Bool) {
        if visible {
            guard volumePopover?.isShown != true else { return }
            volumeMeter.frame = NSRect(x: 0, y: 0, width: 48, height: 116)
            let controller = NSViewController()
            controller.view = volumeMeter
            let popover = NSPopover()
            popover.behavior = .applicationDefined
            popover.animates = false
            popover.contentSize = volumeMeter.frame.size
            popover.contentViewController = controller
            volumePopover = popover
            popover.show(relativeTo: discButton.bounds, of: discButton, preferredEdge: .minY)
        } else {
            volumePopover?.close()
            volumePopover = nil
        }
    }

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
        volumeMeter.setValue(value)
        discButton.level = max(0, min(1, value))
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
