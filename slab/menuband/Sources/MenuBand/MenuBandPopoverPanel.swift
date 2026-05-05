// MenuBandPopoverPanel.swift
//
// Custom popover-styled NSPanel. NSPopover places its arrow at the
// anchor's screen-x and auto-fits the content to the visible screen,
// which means the arrow always lands roughly where the anchor sits
// — when the status item is near the screen edge, the popover content
// shifts inward and the arrow ends up on the inward side of the
// content rather than flush at one corner. This panel decouples the
// two: the window's frame and the arrow tip's screen-x are set
// independently, so the content can sit far from the arrow and the
// arrow can land at any horizontal position on the panel's top edge.
//
// Visually it mimics NSPopover: rounded body + small triangular arrow
// rendered through a single NSVisualEffectView with a CAShapeLayer
// mask, so the liquid-glass material flows continuously from the
// arrow into the body.

import AppKit

final class MenuBandPopoverPanel: NSPanel {
    static let arrowHeight: CGFloat = 11
    static let arrowWidth: CGFloat = 22
    static let cornerRadius: CGFloat = 10

    let chrome: MenuBandPopoverChrome

    init(content: NSView, contentSize: NSSize) {
        let totalSize = NSSize(
            width: contentSize.width,
            height: contentSize.height + Self.arrowHeight
        )
        chrome = MenuBandPopoverChrome(content: content)
        super.init(
            contentRect: NSRect(origin: .zero, size: totalSize),
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
        acceptsMouseMovedEvents = true
        titleVisibility = .hidden
        titlebarAppearsTransparent = true
        isReleasedWhenClosed = false
        contentView = chrome
    }

    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }

    /// Position the panel so that:
    ///   • the panel's top edge sits at `topScreenY` (the menubar bottom),
    ///   • the panel's left edge sits at `leftScreenX`,
    ///   • the arrow tip points at `arrowScreenX`.
    /// `arrowScreenX` may be inside or outside the panel's horizontal
    /// extent; the chrome clamps the rendered arrow to a small inset so
    /// it never falls off the rounded corner radii.
    func position(leftScreenX: CGFloat, topScreenY: CGFloat, arrowScreenX: CGFloat) {
        let frameSize = frame.size
        let panelFrame = NSRect(
            x: leftScreenX,
            y: topScreenY - frameSize.height,
            width: frameSize.width,
            height: frameSize.height
        )
        setFrame(panelFrame, display: true)
        // Arrow position is in chrome-local coords (origin at panel
        // bottom-left). Convert from screen-x.
        let arrowLocalX = arrowScreenX - leftScreenX
        chrome.setArrowOffsetFromLeft(arrowLocalX)
    }
}

final class MenuBandPopoverChrome: NSView {
    private let visualEffect = NSVisualEffectView()
    private let content: NSView
    private let maskLayer = CAShapeLayer()
    private var arrowOffsetFromLeft: CGFloat = MenuBandPopoverPanel.cornerRadius
        + MenuBandPopoverPanel.arrowWidth / 2

    init(content: NSView) {
        self.content = content
        super.init(frame: .zero)
        wantsLayer = true
        layer?.masksToBounds = false

        // The whole panel area (body + arrow) is one continuous
        // visual-effect view. A CAShapeLayer mask carves out the
        // popover silhouette, so the liquid-glass material flows from
        // the arrow tip down into the body without a seam.
        visualEffect.material = .popover
        visualEffect.blendingMode = .behindWindow
        visualEffect.state = .active
        visualEffect.wantsLayer = true
        visualEffect.translatesAutoresizingMaskIntoConstraints = false
        addSubview(visualEffect)

        content.translatesAutoresizingMaskIntoConstraints = false
        addSubview(content)

        NSLayoutConstraint.activate([
            visualEffect.leadingAnchor.constraint(equalTo: leadingAnchor),
            visualEffect.trailingAnchor.constraint(equalTo: trailingAnchor),
            visualEffect.topAnchor.constraint(equalTo: topAnchor),
            visualEffect.bottomAnchor.constraint(equalTo: bottomAnchor),
            // Content sits in the body region (below the arrow strip).
            content.leadingAnchor.constraint(equalTo: leadingAnchor),
            content.trailingAnchor.constraint(equalTo: trailingAnchor),
            content.topAnchor.constraint(
                equalTo: topAnchor,
                constant: MenuBandPopoverPanel.arrowHeight),
            content.bottomAnchor.constraint(equalTo: bottomAnchor),
        ])

        visualEffect.layer?.mask = maskLayer
    }

    required init?(coder: NSCoder) { fatalError() }

    override var isFlipped: Bool { false }

    func setArrowOffsetFromLeft(_ offset: CGFloat) {
        // Clamp so the arrow fits between the rounded body corners.
        let minX = MenuBandPopoverPanel.cornerRadius
            + MenuBandPopoverPanel.arrowWidth / 2 + 2
        let maxX = bounds.width - MenuBandPopoverPanel.cornerRadius
            - MenuBandPopoverPanel.arrowWidth / 2 - 2
        arrowOffsetFromLeft = max(minX, min(maxX, offset))
        rebuildMask()
    }

    override func layout() {
        super.layout()
        rebuildMask()
    }

    private func rebuildMask() {
        let size = bounds.size
        guard size.width > 0, size.height > 0 else { return }

        let arrowH = MenuBandPopoverPanel.arrowHeight
        let arrowW = MenuBandPopoverPanel.arrowWidth
        let radius = MenuBandPopoverPanel.cornerRadius

        // Body rect: the rounded rectangle below the arrow strip.
        // The arrow is drawn as a small triangle attached to the body's
        // top edge, so the mask is one continuous path.
        let bodyTop = size.height - arrowH
        let arrowTipX = max(arrowOffsetFromLeft, radius + arrowW / 2 + 2)

        let path = CGMutablePath()
        // Bottom-left → bottom-right with rounded corners
        path.move(to: CGPoint(x: 0, y: radius))
        path.addArc(tangent1End: CGPoint(x: 0, y: 0),
                    tangent2End: CGPoint(x: radius, y: 0),
                    radius: radius)
        path.addLine(to: CGPoint(x: size.width - radius, y: 0))
        path.addArc(tangent1End: CGPoint(x: size.width, y: 0),
                    tangent2End: CGPoint(x: size.width, y: radius),
                    radius: radius)
        // Right side up to body top
        path.addLine(to: CGPoint(x: size.width, y: bodyTop - radius))
        path.addArc(tangent1End: CGPoint(x: size.width, y: bodyTop),
                    tangent2End: CGPoint(x: size.width - radius, y: bodyTop),
                    radius: radius)
        // Body top edge → arrow base right
        path.addLine(to: CGPoint(x: arrowTipX + arrowW / 2, y: bodyTop))
        // Arrow tip
        path.addLine(to: CGPoint(x: arrowTipX, y: size.height))
        // Arrow base left
        path.addLine(to: CGPoint(x: arrowTipX - arrowW / 2, y: bodyTop))
        // Continue body top edge to top-left corner radius
        path.addLine(to: CGPoint(x: radius, y: bodyTop))
        path.addArc(tangent1End: CGPoint(x: 0, y: bodyTop),
                    tangent2End: CGPoint(x: 0, y: bodyTop - radius),
                    radius: radius)
        path.closeSubpath()

        maskLayer.path = path
        maskLayer.frame = bounds
    }
}
