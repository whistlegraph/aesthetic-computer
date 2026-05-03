//
//  PianoKeyboardView.swift
//  MenuBand
//
//  Created by Esteban Uribe on 5/3/26.
//


import AppKit

final class PianoKeyboardView: NSView {
    private static let rendererLayout: KeyboardIconRenderer.Layout = .tightActiveRange

    private weak var menuBand: MenuBandController?
    private var trackingArea: NSTrackingArea?
    private var hoveredNote: UInt8?
    private var currentDisplayNote: UInt8?
    private var currentPlayedNote: UInt8?

    private let pianoScale: CGFloat
    private var widthConstraint: NSLayoutConstraint!
    private var heightConstraint: NSLayoutConstraint!

    init(menuBand: MenuBandController, pianoScale: CGFloat) {
        self.menuBand = menuBand
        self.pianoScale = pianoScale
        super.init(frame: NSRect(origin: .zero, size: .zero))
        wantsLayer = true

        let preferredSize = preferredSize()
        widthConstraint = widthAnchor.constraint(equalToConstant: preferredSize.width)
        heightConstraint = heightAnchor.constraint(equalToConstant: preferredSize.height)
        NSLayoutConstraint.activate([widthConstraint, heightConstraint])
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    override var acceptsFirstResponder: Bool { true }
    override var mouseDownCanMoveWindow: Bool { false }

    func refreshLayout() {
        let preferredSize = preferredSize()
        widthConstraint.constant = preferredSize.width
        heightConstraint.constant = preferredSize.height
    }

    private func preferredSize() -> NSSize {
        KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand?.keymap) {
            let piano = KeyboardIconRenderer.pianoImageSize(layout: Self.rendererLayout)
            return NSSize(
                width: piano.width * pianoScale,
                height: piano.height * pianoScale
            )
        }
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let trackingArea = trackingArea {
            removeTrackingArea(trackingArea)
        }
        let area = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .mouseMoved, .activeAlways, .inVisibleRect],
            owner: self,
            userInfo: nil
        )
        addTrackingArea(area)
        trackingArea = area
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        guard let menuBand = menuBand else { return }

        KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand.keymap) {
            KeyboardIconRenderer.activeKeymap = menuBand.keymap
            let image = KeyboardIconRenderer.image(
                litNotes: menuBand.litNotes,
                enabled: menuBand.midiMode,
                typeMode: true,
                hovered: hoveredNote.map { .note($0) },
                includeSettings: false,
                layout: Self.rendererLayout
            )
            image.draw(in: pianoTargetRect())
        }
    }

    override func mouseMoved(with event: NSEvent) {
        updateHover(with: event)
    }

    override func mouseExited(with event: NSEvent) {
        if hoveredNote != nil {
            hoveredNote = nil
            needsDisplay = true
        }
    }

    override func mouseDown(with event: NSEvent) {
        window?.makeKey()
        guard let menuBand = menuBand,
              let point = rendererPoint(from: event),
              let displayNote = KeyboardIconRenderer.withPianoWaveformKeyboard(
                  keymap: menuBand.keymap,
                  { KeyboardIconRenderer.noteAt(point, layout: Self.rendererLayout) }
              ),
              let playedNote = playedNote(for: displayNote, menuBand: menuBand)
        else { return }
        let expression = KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand.keymap) {
            NoteExpression.values(for: displayNote, at: point, layout: Self.rendererLayout)
        }
        currentDisplayNote = displayNote
        currentPlayedNote = playedNote
        hoveredNote = displayNote
        menuBand.startTapNote(
            playedNote,
            velocity: expression.velocity,
            pan: expression.pan,
            displayNote: displayNote
        )
        needsDisplay = true
    }

    override func mouseDragged(with event: NSEvent) {
        guard let menuBand = menuBand,
              let point = rendererPoint(from: event) else { return }
        let hovered = KeyboardIconRenderer.withPianoWaveformKeyboard(
            keymap: menuBand.keymap,
            { KeyboardIconRenderer.noteAt(point, layout: Self.rendererLayout) }
        )
        hoveredNote = hovered

        if hovered != currentDisplayNote {
            if let previous = currentPlayedNote {
                menuBand.stopTapNote(previous)
            }
            if let nextDisplay = hovered,
               let nextPlayed = playedNote(for: nextDisplay, menuBand: menuBand) {
                let expression = KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand.keymap) {
                    NoteExpression.values(for: nextDisplay, at: point, layout: Self.rendererLayout)
                }
                menuBand.startTapNote(
                    nextPlayed,
                    velocity: expression.velocity,
                    pan: expression.pan,
                    displayNote: nextDisplay
                )
                currentPlayedNote = nextPlayed
            } else {
                currentPlayedNote = nil
            }
            currentDisplayNote = hovered
        } else if let current = currentDisplayNote,
                  currentPlayedNote != nil {
            let expression = KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand.keymap) {
                NoteExpression.values(for: current, at: point, layout: Self.rendererLayout)
            }
            if let playedNote = currentPlayedNote {
                menuBand.updateTapPan(playedNote, pan: expression.pan)
            }
        }
        needsDisplay = true
    }

    override func mouseUp(with event: NSEvent) {
        if let note = currentPlayedNote {
            menuBand?.stopTapNote(note)
        }
        currentDisplayNote = nil
        currentPlayedNote = nil
        updateHover(with: event)
    }

    func clearInteraction() {
        currentDisplayNote = nil
        currentPlayedNote = nil
        hoveredNote = nil
        needsDisplay = true
    }

    private func updateHover(with event: NSEvent) {
        guard let point = rendererPoint(from: event) else {
            if hoveredNote != nil {
                hoveredNote = nil
                needsDisplay = true
            }
            return
        }
        let next = KeyboardIconRenderer.withPianoWaveformKeyboard(
            keymap: menuBand?.keymap,
            { KeyboardIconRenderer.noteAt(point, layout: Self.rendererLayout) }
        )
        if next != hoveredNote {
            hoveredNote = next
            needsDisplay = true
        }
    }

    private func rendererPoint(from event: NSEvent) -> NSPoint? {
        let local = convert(event.locationInWindow, from: nil)
        let target = pianoTargetRect()
        let point = NSPoint(
            x: (local.x - target.minX) / pianoScale,
            y: (local.y - target.minY) / pianoScale
        )
        let piano = KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand?.keymap) {
            KeyboardIconRenderer.pianoImageSize(layout: Self.rendererLayout)
        }
        guard point.x >= -KeyboardIconRenderer.whiteW,
              point.x <= piano.width + KeyboardIconRenderer.whiteW,
              point.y >= -piano.height,
              point.y <= piano.height * 2 else { return nil }
        return point
    }

    private func pianoTargetRect() -> NSRect {
        let piano = KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand?.keymap) {
            KeyboardIconRenderer.pianoImageSize(layout: Self.rendererLayout)
        }
        let size = NSSize(width: piano.width * pianoScale, height: piano.height * pianoScale)
        return NSRect(
            x: bounds.midX - size.width / 2,
            y: bounds.midY - size.height / 2,
            width: size.width,
            height: size.height
        )
    }

    private func playedNote(for displayNote: UInt8, menuBand: MenuBandController) -> UInt8? {
        let value = Int(displayNote) + menuBand.octaveShift * 12
        guard value >= 0, value <= 127 else { return nil }
        return UInt8(value)
    }
}
