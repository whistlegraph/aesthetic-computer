// Floating panel that hosts the KidLisp TV.
//
// Skeuomorphic LCD-television look: thick black bezel around a recessed
// screen area, with a subtle glossy inner border + a tiny power LED at
// the bottom-right corner. The framebuffer is upscaled nearest-neighbor
// inside the screen so pixels stay sharp.
//
// Positioning: child window of the popover, anchored directly under
// the LEFT floating panel (PianoWaveformWindow) so the TV reads as part
// of the player palette, not the settings popover. See
// AppDelegate.showPopover / closePopover for lifecycle.

import AppKit

final class KidLispTVPanel: NSPanel {

    let tv: KidLispTVView
    private let bezel: KidLispTVBezelView

    static let bezelThickness: CGFloat = 14
    static let screenAspect: CGFloat = 192.0 / 120.0
    static let panelInset: CGFloat = 3  // gap below the anchor panel

    /// Default piece — a simple amp-reactive twin-line waveform that
    /// spreads from the center as MenuBand's synth output gets louder.
    /// Picked over `$roz` so first-time openers see the audio→visual
    /// loop wired up: tap a piano key, the lines snap apart.
    /// `amp` is the magic var (matches kidlisp.mjs's `amp` global,
    /// ~0–10 scale). Replace via the "$ pieces" chooser on click.
    static let defaultSource: String = """
        wipe black
        ink rainbow
        line 0 (- h/2 (* amp 5)) w (- h/2 (* amp 5))
        line 0 (+ h/2 (* amp 5)) w (+ h/2 (* amp 5))
        """

    init(width: CGFloat) {
        let screenWidth = width - 2 * Self.bezelThickness
        let screenHeight = (screenWidth / Self.screenAspect).rounded()
        let panelHeight = screenHeight + 2 * Self.bezelThickness
        self.tv = KidLispTVView(source: Self.defaultSource,
                                resWidth: 192, resHeight: 120)
        self.bezel = KidLispTVBezelView(frame: NSRect(x: 0, y: 0,
                                                      width: width,
                                                      height: panelHeight))
        super.init(
            contentRect: NSRect(x: 0, y: 0, width: width, height: panelHeight),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: false
        )
        isOpaque = false
        backgroundColor = .clear
        hasShadow = true
        level = .popUpMenu
        animationBehavior = .none
        collectionBehavior = [.transient, .ignoresCycle]
        hidesOnDeactivate = false
        canHide = false
        isMovableByWindowBackground = false
        titleVisibility = .hidden
        titlebarAppearsTransparent = true
        isReleasedWhenClosed = false

        tv.frame = NSRect(x: Self.bezelThickness,
                          y: Self.bezelThickness,
                          width: screenWidth,
                          height: screenHeight)
        let resize: NSView.AutoresizingMask = [.width, .height]
        tv.autoresizingMask = resize
        bezel.addSubview(tv)
        contentView = bezel
    }

    override var canBecomeKey: Bool { false }
    override var canBecomeMain: Bool { false }

    /// Position the panel directly under `anchorFrame` (the left floating
    /// panel's frame), matching its width so the TV reads as a stacked
    /// component.
    func anchor(below anchorFrame: NSRect) {
        let h = frame.height
        let panelFrame = NSRect(
            x: anchorFrame.minX,
            y: anchorFrame.minY - h - Self.panelInset,
            width: anchorFrame.width,
            height: h
        )
        setFrame(panelFrame, display: true)
    }
}

/// The black bezel chrome — draws an LCD-style frame with rounded
/// corners, a recessed screen well, an inner highlight, and a small
/// power LED at the bottom-right. The actual KidLisp framebuffer view
/// is added as a subview centered in the screen well.
final class KidLispTVBezelView: NSView {

    override var isFlipped: Bool { false }  // standard NSView coords for chrome

    override func draw(_ dirtyRect: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        let b = bounds
        let bezel = KidLispTVPanel.bezelThickness

        // Outer bezel — black, slightly rounded.
        let outerPath = NSBezierPath(roundedRect: b, xRadius: 6, yRadius: 6)
        NSColor(white: 0.08, alpha: 1.0).setFill()
        outerPath.fill()

        // Subtle top-edge highlight (glossy plastic feel).
        let highlightRect = NSRect(x: b.minX + 4,
                                    y: b.maxY - 3,
                                    width: b.width - 8,
                                    height: 1.5)
        NSColor(white: 0.28, alpha: 0.7).setFill()
        NSBezierPath(roundedRect: highlightRect, xRadius: 0.75, yRadius: 0.75).fill()

        // Screen well — recessed dark rectangle. The TV view sits on top.
        let wellRect = NSRect(x: b.minX + bezel,
                              y: b.minY + bezel,
                              width: b.width - 2 * bezel,
                              height: b.height - 2 * bezel)
        NSColor.black.setFill()
        NSBezierPath(rect: wellRect).fill()

        // Inset shadow line around the well — sells the recess.
        ctx.saveGState()
        ctx.setStrokeColor(NSColor(white: 0.0, alpha: 0.8).cgColor)
        ctx.setLineWidth(1)
        ctx.stroke(wellRect.insetBy(dx: 0.5, dy: 0.5))
        ctx.restoreGState()

        // Power LED — tiny green dot at bottom-right.
        let ledRadius: CGFloat = 1.5
        let ledRect = NSRect(x: b.maxX - bezel / 2 - ledRadius,
                              y: b.minY + bezel / 2 - ledRadius,
                              width: ledRadius * 2,
                              height: ledRadius * 2)
        NSColor(red: 0.3, green: 1.0, blue: 0.5, alpha: 0.95).setFill()
        NSBezierPath(ovalIn: ledRect).fill()
        // Faint glow ring.
        NSColor(red: 0.3, green: 1.0, blue: 0.5, alpha: 0.2).setFill()
        NSBezierPath(ovalIn: ledRect.insetBy(dx: -2, dy: -2)).fill()
    }
}
