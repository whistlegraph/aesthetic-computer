//
//  CollapsedPianoWaveformView.swift
//  MenuBand
//
//  Created by Esteban Uribe on 5/3/26.
//


import AppKit

final class CollapsedPianoWaveformView: NSView {
    private static var shouldUseLiquidGlass: Bool {
        PianoWaveformWindowStyle.shouldUseLiquidGlass
    }

    private weak var menuBand: MenuBandController?
    private let contentContainer = NSView()
    private let waveformContainer = NSView()
    private let waveformClipView = NSView()
    private let waveformView = WaveformView()
    private let heldNotesContainer = NSView()
    private let heldNotesStack = NSStackView()
    private let instrumentRow = NSView()
    private let instrumentArrows = ArrowKeysIndicator()
    private let instrumentLabel = NSTextField(labelWithString: "")
    private var trackingArea: NSTrackingArea?
    private weak var paletteGlassView: NSView?

    var onHoverChanged: ((Bool) -> Void)?
    var onStepBackward: (() -> Void)?
    var onStepForward: (() -> Void)?
    var onStepUp: (() -> Void)?
    var onStepDown: (() -> Void)?

    private static let waveformHeight: CGFloat = 64
    private static let heldNotesRowHeight: CGFloat = 16
    private static let instrumentRowHeight: CGFloat = 22

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        super.init(frame: .zero)
        wantsLayer = true
        layer?.cornerRadius = 10
        layer?.masksToBounds = true

        contentContainer.translatesAutoresizingMaskIntoConstraints = false
        waveformView.menuBand = menuBand
        waveformView.translatesAutoresizingMaskIntoConstraints = false
        waveformView.setSurfaceStyle(.standard)

        waveformContainer.wantsLayer = true
        waveformContainer.translatesAutoresizingMaskIntoConstraints = false
        waveformContainer.layer?.cornerRadius = 8
        waveformContainer.layer?.masksToBounds = false

        waveformClipView.wantsLayer = true
        waveformClipView.translatesAutoresizingMaskIntoConstraints = false
        waveformClipView.layer?.cornerRadius = 8
        waveformClipView.layer?.masksToBounds = true

        heldNotesContainer.translatesAutoresizingMaskIntoConstraints = false
        heldNotesStack.orientation = .horizontal
        heldNotesStack.alignment = .centerY
        heldNotesStack.spacing = 4
        heldNotesStack.translatesAutoresizingMaskIntoConstraints = false

        instrumentRow.translatesAutoresizingMaskIntoConstraints = false
        instrumentRow.wantsLayer = true
        instrumentRow.layer?.cornerRadius = 7
        if #available(macOS 10.15, *) {
            instrumentRow.layer?.cornerCurve = .continuous
        }
        instrumentArrows.translatesAutoresizingMaskIntoConstraints = false
        instrumentArrows.setContentHuggingPriority(.required, for: .horizontal)
        instrumentArrows.setContentCompressionResistancePriority(.required, for: .horizontal)
        instrumentArrows.displayMode = .horizontalPair
        instrumentArrows.style = .prominent
        instrumentArrows.toolTip = "Change instrument"
        instrumentArrows.onClick = { [weak self] direction, isDown in
            guard let self, isDown else { return }
            switch direction {
            case 0:
                self.onStepBackward?()
            case 1:
                self.onStepForward?()
            case 2:
                self.onStepDown?()
            case 3:
                self.onStepUp?()
            default:
                break
            }
        }

        instrumentLabel.translatesAutoresizingMaskIntoConstraints = false
        instrumentLabel.lineBreakMode = .byTruncatingTail
        instrumentLabel.drawsBackground = false
        instrumentLabel.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)

        addSubview(contentContainer)
        contentContainer.addSubview(waveformContainer)
        contentContainer.addSubview(heldNotesContainer)
        contentContainer.addSubview(instrumentRow)
        waveformContainer.addSubview(waveformClipView)
        waveformClipView.addSubview(waveformView)
        heldNotesContainer.addSubview(heldNotesStack)
        instrumentRow.addSubview(instrumentArrows)
        instrumentRow.addSubview(instrumentLabel)
        installLiquidGlassBackgrounds()

        NSLayoutConstraint.activate([
            widthAnchor.constraint(equalToConstant: KeyboardIconRenderer.imageSize.width),
            contentContainer.leadingAnchor.constraint(equalTo: leadingAnchor),
            contentContainer.trailingAnchor.constraint(equalTo: trailingAnchor),
            contentContainer.topAnchor.constraint(equalTo: topAnchor),
            contentContainer.bottomAnchor.constraint(equalTo: bottomAnchor),

            waveformContainer.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor),
            waveformContainer.trailingAnchor.constraint(equalTo: contentContainer.trailingAnchor),
            waveformContainer.topAnchor.constraint(equalTo: contentContainer.topAnchor),
            waveformContainer.heightAnchor.constraint(equalToConstant: Self.waveformHeight),

            waveformClipView.leadingAnchor.constraint(equalTo: waveformContainer.leadingAnchor, constant: 5),
            waveformClipView.trailingAnchor.constraint(equalTo: waveformContainer.trailingAnchor, constant: -5),
            waveformClipView.topAnchor.constraint(equalTo: waveformContainer.topAnchor, constant: 5),
            waveformClipView.bottomAnchor.constraint(equalTo: waveformContainer.bottomAnchor, constant: -5),

            waveformView.leadingAnchor.constraint(equalTo: waveformClipView.leadingAnchor),
            waveformView.trailingAnchor.constraint(equalTo: waveformClipView.trailingAnchor),
            waveformView.topAnchor.constraint(equalTo: waveformClipView.topAnchor),
            waveformView.bottomAnchor.constraint(equalTo: waveformClipView.bottomAnchor),

            heldNotesContainer.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor),
            heldNotesContainer.trailingAnchor.constraint(equalTo: contentContainer.trailingAnchor),
            heldNotesContainer.topAnchor.constraint(equalTo: waveformContainer.bottomAnchor, constant: 2),
            heldNotesContainer.heightAnchor.constraint(equalToConstant: Self.heldNotesRowHeight),

            heldNotesStack.centerXAnchor.constraint(equalTo: heldNotesContainer.centerXAnchor),
            heldNotesStack.centerYAnchor.constraint(equalTo: heldNotesContainer.centerYAnchor),

            instrumentRow.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor),
            instrumentRow.trailingAnchor.constraint(equalTo: contentContainer.trailingAnchor),
            instrumentRow.topAnchor.constraint(equalTo: heldNotesContainer.bottomAnchor, constant: 2),
            instrumentRow.heightAnchor.constraint(equalToConstant: Self.instrumentRowHeight),
            instrumentRow.bottomAnchor.constraint(equalTo: contentContainer.bottomAnchor, constant: -2),

            instrumentArrows.leadingAnchor.constraint(equalTo: instrumentRow.leadingAnchor, constant: 6),
            instrumentArrows.centerYAnchor.constraint(equalTo: instrumentRow.centerYAnchor),

            instrumentLabel.leadingAnchor.constraint(equalTo: instrumentArrows.trailingAnchor, constant: 6),
            instrumentLabel.centerYAnchor.constraint(equalTo: instrumentRow.centerYAnchor),
            instrumentLabel.trailingAnchor.constraint(equalTo: instrumentRow.trailingAnchor, constant: -8),
        ])

        refresh()
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let trackingArea {
            removeTrackingArea(trackingArea)
        }
        let trackingArea = NSTrackingArea(
            rect: bounds,
            options: [.activeAlways, .inVisibleRect, .mouseEnteredAndExited],
            owner: self,
            userInfo: nil
        )
        addTrackingArea(trackingArea)
        self.trackingArea = trackingArea
    }

    override func mouseEntered(with event: NSEvent) {
        onHoverChanged?(true)
        super.mouseEntered(with: event)
    }

    override func mouseExited(with event: NSEvent) {
        onHoverChanged?(false)
        super.mouseExited(with: event)
    }

    func refresh() {
        guard let menuBand else { return }
        let isDark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        waveformView.setLightMode(!isDark)

        let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
        let familyColor = menuBand.midiMode
            ? NSColor.controlAccentColor
            : InstrumentListView.colorForProgram(safe)

        let waveformBackground = isDark
            ? NSColor(white: 0.06, alpha: 1.0)
            : NSColor(white: 0.82, alpha: 1.0)
        let glassWaveformBackground = isDark
            ? NSColor.white.withAlphaComponent(0.04)
            : NSColor.white.withAlphaComponent(0.18)
        let instrumentRowBackground = isDark
            ? NSColor.white.withAlphaComponent(0.08)
            : NSColor.white.withAlphaComponent(0.3)
        waveformContainer.layer?.backgroundColor = NSColor.clear.cgColor
        waveformClipView.layer?.backgroundColor = Self.shouldUseLiquidGlass
            ? glassWaveformBackground.cgColor
            : waveformBackground.cgColor
        waveformContainer.layer?.borderWidth = 1
        waveformContainer.layer?.borderColor = familyColor.withAlphaComponent(
            Self.shouldUseLiquidGlass ? 0.22 : 0.55
        ).cgColor
        instrumentRow.layer?.backgroundColor = Self.shouldUseLiquidGlass
            ? NSColor.clear.cgColor
            : instrumentRowBackground.cgColor
        instrumentRow.layer?.borderWidth = 1
        instrumentRow.layer?.borderColor = familyColor.withAlphaComponent(
            Self.shouldUseLiquidGlass ? 0.22 : 0.35
        ).cgColor
        instrumentLabel.textColor = isDark ? .white : .black
        instrumentArrows.accentColor = familyColor
        instrumentArrows.isDarkAppearance = isDark

        if menuBand.midiMode {
            waveformView.setDotMatrix(MenuBandPopoverViewController.midiDotPattern)
            waveformView.setBaseColor(.controlAccentColor)
        } else {
            waveformView.setDotMatrix(nil)
            waveformView.setBaseColor(familyColor)
        }

        let shadow = NSShadow()
        shadow.shadowColor = familyColor.withAlphaComponent(isDark ? 0.9 : 0.55)
        shadow.shadowOffset = NSSize(width: 0, height: -1)
        shadow.shadowBlurRadius = 3
        let titleFont: NSFont = {
            if let descriptor = AppDelegate.ywftBoldDescriptor,
               let font = NSFont(descriptor: descriptor, size: 14),
               font.familyName == "YWFT Processing" {
                return font
            }
            return NSFont.systemFont(ofSize: 14, weight: .black)
        }()
        instrumentLabel.attributedStringValue = NSAttributedString(
            string: GeneralMIDI.programNames[safe],
            attributes: [
                .font: titleFont,
                .foregroundColor: isDark ? NSColor.white : NSColor.black,
                .shadow: shadow,
            ]
        )

        for view in heldNotesStack.arrangedSubviews {
            heldNotesStack.removeArrangedSubview(view)
            view.removeFromSuperview()
        }
        for name in menuBand.heldNoteNames() {
            heldNotesStack.addArrangedSubview(makeHeldNoteBox(name: name, color: familyColor))
        }

        if Self.shouldUseLiquidGlass, #available(macOS 26.0, *) {
            let paletteTint = familyColor.withAlphaComponent(menuBand.midiMode ? 0.20 : 0.16)
            (paletteGlassView as? NSGlassEffectView)?.style = .regular
            (paletteGlassView as? NSGlassEffectView)?.tintColor = paletteTint
            layer?.backgroundColor = NSColor.clear.cgColor
        } else {
            layer?.backgroundColor = (isDark
                ? NSColor(white: 0.06, alpha: 0.96)
                : NSColor(white: 0.88, alpha: 0.96)).cgColor
            layer?.borderWidth = 1
            layer?.borderColor = familyColor.withAlphaComponent(0.45).cgColor
        }
    }

    func setLive(_ isLive: Bool) {
        waveformView.isLive = isLive
    }

    private func makeHeldNoteBox(name: String, color: NSColor) -> NSView {
        let box = NSView()
        box.translatesAutoresizingMaskIntoConstraints = false
        box.wantsLayer = true
        box.layer?.cornerRadius = 4
        box.layer?.backgroundColor = color.withAlphaComponent(0.85).cgColor

        let label = NSTextField(labelWithString: name)
        label.translatesAutoresizingMaskIntoConstraints = false
        label.font = NSFont.monospacedSystemFont(ofSize: 9, weight: .heavy)
        label.textColor = .black
        box.addSubview(label)

        NSLayoutConstraint.activate([
            label.leadingAnchor.constraint(equalTo: box.leadingAnchor, constant: 5),
            label.trailingAnchor.constraint(equalTo: box.trailingAnchor, constant: -5),
            label.topAnchor.constraint(equalTo: box.topAnchor, constant: 1),
            label.bottomAnchor.constraint(equalTo: box.bottomAnchor, constant: -1),
        ])
        return box
    }

    private func installLiquidGlassBackgrounds() {
        guard Self.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }

        let paletteGlassView = CollapsedPianoWaveformGlassEffectView()
        paletteGlassView.translatesAutoresizingMaskIntoConstraints = false
        paletteGlassView.cornerRadius = 10
        addSubview(paletteGlassView, positioned: .below, relativeTo: contentContainer)
        NSLayoutConstraint.activate([
            paletteGlassView.leadingAnchor.constraint(equalTo: leadingAnchor),
            paletteGlassView.trailingAnchor.constraint(equalTo: trailingAnchor),
            paletteGlassView.topAnchor.constraint(equalTo: topAnchor),
            paletteGlassView.bottomAnchor.constraint(equalTo: bottomAnchor),
        ])
        self.paletteGlassView = paletteGlassView
    }

}

@available(macOS 26.0, *)
private final class CollapsedPianoWaveformGlassEffectView: NSGlassEffectView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}
