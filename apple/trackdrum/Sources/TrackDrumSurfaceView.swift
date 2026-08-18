import CoreGraphics
import QuartzCore
import UIKit

/// TrackDrum's palette and pen, shared by the two layers below.
///
/// Every mark in this file is a port of `TrackpadDrumSkinPad.draw` in Menu
/// Band's PitchBendCursor.swift — the same five material bands in the same
/// order, the same snare wires, the same hat teeth, the same chassis rail, the
/// same accent tethers and contact dots, and the same two live layers: the
/// energy field's glow and the membrane lit as a height field. The Metal
/// membrane-warp surface the Mac app lays on top of this chart is NOT ported;
/// this is the vector chart the same app draws underneath it.
enum TrackDrumInk {
    /// macOS resolves `NSColor.controlAccentColor`; iOS has no system accent at
    /// all, so the Mac's default multicolor blue is pinned here.
    static let accent = UIColor(red: 0, green: 0.478, blue: 1, alpha: 1)

    /// The Mac chart is 80 points to one unit of the engine's surface. Every
    /// decoration is quoted in those points and scaled by this.
    static func scale(_ fit: TrackDrumFit) -> CGFloat {
        max(1, fit.pointsPerUnit / 80)
    }

    static func ring(_ distance: Double, _ fit: TrackDrumFit) -> UIBezierPath {
        UIBezierPath(rect: TrackDrumZones.contour(distance, in: fit))
    }
}

/// The playing surface.
///
/// A phone reports at most five simultaneous touches, and that ceiling is
/// audible — the engine reads each finger that is not the striking one as an
/// anchor that tightens the head and damps it, so a chord on a phone is a
/// genuinely different sound from the same chord on a Mac's ten. All five are
/// tracked, not just the newest.
///
/// Every coordinate crossing goes through `fit`. Drawing calls it one way and
/// hit-testing the other, and it is the same function both times.
final class TrackDrumSurfaceView: UIView {
    /// Contacts, the event's timestamp, and the moment we saw it — the same
    /// three the Mac's trackpad callback hands the performer.
    var onContacts: (([TrackpadContact], Double, Double) -> Void)?

    private var fit = TrackDrumFit(bounds: .zero)
    private var identifiers: [ObjectIdentifier: Int32] = [:]
    private var nextIdentifier: Int32 = 1
    private let live = TrackDrumLiveView(frame: .zero)

    private var isDark: Bool { traitCollection.userInterfaceStyle != .light }

    override init(frame: CGRect) {
        super.init(frame: frame)
        // Without this a phone reports exactly one finger, and the anchor
        // behavior the engine is built around never happens.
        isMultipleTouchEnabled = true
        isOpaque = true
        // The chart never changes between layouts, so it rides this view's own
        // layer and the GPU composites it; only `live` is redrawn per frame.
        // Blitting a full-screen bitmap through CoreGraphics at 60 Hz would
        // spend the frame budget on ink that never moved.
        layer.contentsGravity = .resize
        live.isUserInteractionEnabled = false
        addSubview(live)
        // The Mac chart carries a light and a dark palette; follow the phone.
        registerForTraitChanges([UITraitUserInterfaceStyle.self]) {
            (view: Self, _) in view.rebuildChart()
        }
    }

    required init?(coder: NSCoder) { fatalError("TrackDrumSurfaceView is code-only") }

    override func layoutSubviews() {
        super.layoutSubviews()
        fit = TrackDrumFit(bounds: bounds)
        live.frame = bounds
        live.fit = fit
        rebuildChart()
    }

    func update(touches: [CGPoint],
                charges: [TrackpadSurfaceEnergy.Charge],
                membrane: TrackpadMembraneSimulation.Snapshot) {
        live.update(touches: touches, charges: charges, membrane: membrane)
    }

    // MARK: Touches

    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        report(event)
    }

    override func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent?) {
        report(event)
    }

    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        report(event)
    }

    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) {
        report(event)
    }

    /// The engine wants the whole hand at every instant, not the fingers that
    /// happen to have changed — anchors are as much of the sound as the strike.
    private func report(_ event: UIEvent?) {
        guard let all = event?.allTouches else { return }
        var contacts: [TrackpadContact] = []
        for touch in all {
            let key = ObjectIdentifier(touch)
            switch touch.phase {
            case .ended, .cancelled:
                // Dropping the identifier is what reads downstream as a lift.
                identifiers[key] = nil
            case .began:
                identifiers[key] = nextIdentifier
                contacts.append(TrackpadContact(identifier: nextIdentifier,
                                                point: drumPoint(touch),
                                                state: 3))
                nextIdentifier &+= 1
            default:
                guard let identifier = identifiers[key] else { continue }
                contacts.append(TrackpadContact(identifier: identifier,
                                                point: drumPoint(touch),
                                                state: 4))
            }
        }
        onContacts?(contacts,
                    event?.timestamp ?? ProcessInfo.processInfo.systemUptime,
                    CACurrentMediaTime())
    }

    /// The drum reaches every edge, so a touch is only clamped where a finger
    /// straddles the very boundary — never enough to leave a dead ring.
    private func drumPoint(_ touch: UITouch) -> CGPoint {
        let p = fit.drum(from: touch.location(in: self))
        return CGPoint(x: max(0, min(1, p.x)), y: max(0, min(1, p.y)))
    }

    // MARK: The chart

    /// The five bands and their furniture. Zone names are the Mac's: `hatZone`
    /// is the boundary the hat band starts at, and so on inward.
    private func rebuildChart() {
        guard bounds.width > 0, bounds.height > 0 else { return }
        let dark = isDark
        let ink = TrackDrumInk.scale(fit)
        let fit = self.fit
        let bounds = self.bounds
        let hatZone = TrackDrumInk.ring(0.88, fit)
        let rimZone = TrackDrumInk.ring(0.64, fit)
        let snareZone = TrackDrumInk.ring(0.46, fit)
        let kickZone = TrackDrumInk.ring(0.30, fit)

        // The original natural material palette. System accent is a performance
        // signal on the rim and contacts, not the skin's pigment.
        let click = dark ? UIColor(red: 0.48, green: 0.43, blue: 0.34, alpha: 1)
                         : UIColor(red: 0.97, green: 0.91, blue: 0.77, alpha: 1)
        let hat = dark ? UIColor(red: 0.34, green: 0.39, blue: 0.29, alpha: 1)
                       : UIColor(red: 0.72, green: 0.77, blue: 0.62, alpha: 1)
        let rim = dark ? UIColor(red: 0.43, green: 0.34, blue: 0.20, alpha: 1)
                       : UIColor(red: 0.84, green: 0.68, blue: 0.40, alpha: 1)
        let snare = dark ? UIColor(red: 0.43, green: 0.25, blue: 0.17, alpha: 1)
                         : UIColor(red: 0.76, green: 0.45, blue: 0.30, alpha: 1)
        let kick = dark ? UIColor(red: 0.24, green: 0.15, blue: 0.10, alpha: 1)
                        : UIColor(red: 0.48, green: 0.29, blue: 0.19, alpha: 1)

        let chart = UIGraphicsImageRenderer(bounds: bounds).image { context in
            let cg = context.cgContext
            click.setFill(); UIBezierPath(rect: bounds).fill()
            hat.setFill(); hatZone.fill()
            snare.setFill(); rimZone.fill()

            // Dense parallel wires immediately read as the snare material. The
            // inner fills below mask them out of the center.
            cg.saveGState()
            rimZone.addClip()
            let wires = UIBezierPath()
            stride(from: bounds.minX - bounds.height,
                   through: bounds.maxX, by: 6 * ink).forEach { x in
                wires.move(to: CGPoint(x: x, y: bounds.minY))
                wires.addLine(to: CGPoint(x: x + bounds.height, y: bounds.maxY))
            }
            (dark ? UIColor.white : UIColor.black)
                .withAlphaComponent(0.16).setStroke()
            wires.lineWidth = 0.55 * ink
            wires.stroke()
            cg.restoreGState()

            rim.setFill(); snareZone.fill()
            kick.setFill(); kickZone.fill()

            Self.drawTeeth(color: click.withAlphaComponent(0.72),
                           ink: ink, fit: fit, bounds: bounds)

            // The bright rail is the hard chassis click. Quoted as a contour
            // rather than an inset rectangle so the two axes' different scales
            // cannot pull it off the band it belongs to.
            let rail = TrackDrumInk.ring(0.9625, fit)
            (dark ? UIColor.white : UIColor.black)
                .withAlphaComponent(0.62).setStroke()
            rail.lineWidth = 2.2 * ink
            rail.stroke()
        }

        layer.contentsScale = chart.scale
        layer.contents = chart.cgImage
        live.isDark = dark
        live.boundaries = [(hatZone, 0.9 * ink), (rimZone, 2.0 * ink),
                           (snareZone, 0.9 * ink), (kickZone, 1.3 * ink)]
        live.setNeedsDisplay()
    }

    /// Hat teeth span the playable metal band on all four edges. Their depths
    /// are quoted as engine depths, not screen points, so the rim knee cannot
    /// push them out of the band they mark.
    private static func drawTeeth(color: UIColor, ink: CGFloat,
                                  fit: TrackDrumFit, bounds: CGRect) {
        let teeth = UIBezierPath()
        let slant = 2 * ink
        let step = 9 * ink
        // Even on all four edges, because the depth field is.
        let near = fit.screenDepth(fromEngine: 0.065)
        let far = fit.screenDepth(fromEngine: 0.17)
        let (nearY, farY) = (near, far)
        let (nearX, farX) = (near, far)
        stride(from: bounds.minX + step, through: bounds.maxX - step, by: step)
            .forEach { x in
                let lean: CGFloat = Int((x - bounds.minX) / step).isMultiple(of: 2)
                    ? slant : -slant
                teeth.move(to: CGPoint(x: x, y: bounds.maxY - nearY))
                teeth.addLine(to: CGPoint(x: x + lean, y: bounds.maxY - farY))
                teeth.move(to: CGPoint(x: x, y: bounds.minY + nearY))
                teeth.addLine(to: CGPoint(x: x - lean, y: bounds.minY + farY))
            }
        stride(from: bounds.minY + step, through: bounds.maxY - step, by: step)
            .forEach { y in
                let lean: CGFloat = Int((y - bounds.minY) / step).isMultiple(of: 2)
                    ? slant : -slant
                teeth.move(to: CGPoint(x: bounds.minX + nearX, y: y))
                teeth.addLine(to: CGPoint(x: bounds.minX + farX, y: y + lean))
                teeth.move(to: CGPoint(x: bounds.maxX - nearX, y: y))
                teeth.addLine(to: CGPoint(x: bounds.maxX - farX, y: y - lean))
            }
        color.setStroke()
        teeth.lineWidth = 1.25 * ink
        teeth.stroke()
    }
}

/// Everything on the chart that moves: the energy glow, the membrane's
/// lighting, the zone boundaries the lighting washes under, and the contacts.
/// Its own transparent layer, so the bands underneath are never repainted.
final class TrackDrumLiveView: UIView {
    var fit = TrackDrumFit(bounds: .zero)
    var isDark = true
    var boundaries: [(path: UIBezierPath, width: CGFloat)] = []

    private var marks: [CGPoint] = []
    private var charges: [TrackpadSurfaceEnergy.Charge] = []
    private var membrane: TrackpadMembraneSimulation.Snapshot?

    private var ink: CGFloat { TrackDrumInk.scale(fit) }

    override init(frame: CGRect) {
        super.init(frame: frame)
        isOpaque = false
        backgroundColor = .clear
        contentMode = .redraw
        // Everything drawn here is soft — glows, washes, a few thin strokes —
        // so it is rasterized at 2× on a 3× screen. That is a bit under half the
        // pixels per frame, on the same main thread the strikes arrive on.
        contentScaleFactor = min(2, UIScreen.main.scale)
    }

    required init?(coder: NSCoder) { fatalError("TrackDrumLiveView is code-only") }

    func update(touches: [CGPoint],
                charges: [TrackpadSurfaceEnergy.Charge],
                membrane: TrackpadMembraneSimulation.Snapshot) {
        // A still drum costs nothing to leave on screen.
        guard !touches.isEmpty || !marks.isEmpty
                || !charges.isEmpty || !self.charges.isEmpty
                || !membrane.isFlat else { return }
        marks = touches
        self.charges = charges
        self.membrane = membrane
        setNeedsDisplay()
    }

    override func draw(_ rect: CGRect) {
        guard let context = UIGraphicsGetCurrentContext() else { return }
        drawEnergy()
        drawMembraneLighting(in: context)

        (isDark ? UIColor.white : UIColor.black)
            .withAlphaComponent(isDark ? 0.32 : 0.50).setStroke()
        for boundary in boundaries {
            boundary.path.lineWidth = boundary.width
            boundary.path.stroke()
        }

        drawContacts()

        TrackDrumInk.accent.withAlphaComponent(0.95).setStroke()
        let body = UIBezierPath(rect: bounds.insetBy(dx: ink, dy: ink))
        body.lineWidth = 2 * ink
        body.stroke()
    }

    /// The energy field's glow — soft accent circles wherever the surface is
    /// still carrying a charge. Round, because the field they sit in is now
    /// measured evenly; the Mac's 1.64 ovals were compensating for a shape this
    /// edition no longer has.
    private func drawEnergy() {
        for charge in charges where charge.level > 0.01 {
            let center = fit.view(from: CGPoint(x: max(0, min(1, charge.point.x)),
                                                y: max(0, min(1, charge.point.y))))
            // The Mac quotes this radius in chart points; 80 of those are one
            // engine unit, and each axis spends its own points on that unit.
            let radius = CGFloat((10 + charge.level * 23) / 80) * fit.pointsPerUnit
            for layer in stride(from: 6, through: 1, by: -1) {
                let r = radius * CGFloat(layer) / 6
                TrackDrumInk.accent
                    .withAlphaComponent(charge.level * Double(7 - layer) * 0.018)
                    .setFill()
                UIBezierPath(ovalIn: CGRect(x: center.x - r, y: center.y - r,
                                            width: r * 2, height: r * 2)).fill()
            }
        }
    }

    /// Lights the chart as a height field. Geometry never slides: local slope
    /// produces a moving highlight and shadow over the existing ink.
    ///
    /// The Mac rasterizes this in the simulation's own grid and lets AppKit
    /// scale the tiny texture up. Here the grid is laid out in VIEW space
    /// instead — one sample per cell, each pushed back through the fit — because
    /// the phone's mapping is a turn and a stretch, not something an image can
    /// be handed and told to fill a rectangle with.
    private func drawMembraneLighting(in context: CGContext) {
        guard let membrane, !membrane.isFlat else { return }
        let width = max(2, membrane.rows * 2 - 1)
        let height = max(2, membrane.columns * 2 - 1)
        guard let bitmap = CGContext(
            data: nil, width: width, height: height,
            bitsPerComponent: 8, bytesPerRow: width * 4,
            space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue
        ), let data = bitmap.data?.assumingMemoryBound(to: UInt8.self) else { return }
        // Sampling steps in the membrane's own normalized grid, as on the Mac.
        let dx = 1 / CGFloat(membrane.columns * 2 - 2)
        let dy = 1 / CGFloat(membrane.rows * 2 - 2)
        for row in 0..<height {
            for column in 0..<width {
                let point = fit.drum(from: CGPoint(
                    x: bounds.minX + bounds.width * CGFloat(column) / CGFloat(width - 1),
                    y: bounds.minY + bounds.height * CGFloat(row) / CGFloat(height - 1)
                ))
                let left = membrane.height(at: CGPoint(x: point.x - dx, y: point.y))
                let right = membrane.height(at: CGPoint(x: point.x + dx, y: point.y))
                let below = membrane.height(at: CGPoint(x: point.x, y: point.y - dy))
                let above = membrane.height(at: CGPoint(x: point.x, y: point.y + dy))
                // A high, upper-left light reveals slope; a small ambient term
                // makes a held depression retain weight at its center.
                let light = Double((left - right) * 1.9 + (above - below) * 2.7
                                   - membrane.height(at: point) * 0.10)
                let alpha = UInt8(min(46, abs(light) * 210))
                let channel: UInt8 = light >= 0 ? alpha : 0
                let offset = row * width * 4 + column * 4
                data[offset] = channel
                data[offset + 1] = channel
                data[offset + 2] = channel
                data[offset + 3] = alpha
            }
        }
        guard let image = bitmap.makeImage() else { return }
        context.saveGState()
        context.interpolationQuality = .high
        UIImage(cgImage: image).draw(in: bounds)
        context.restoreGState()
    }

    /// Tethers between fingers, and a dot per contact sized by the velocity that
    /// contact would actually strike with.
    private func drawContacts() {
        let mapped = marks.map {
            fit.view(from: CGPoint(x: max(0, min(1, $0.x)), y: max(0, min(1, $0.y))))
        }
        if marks.count > 1 {
            for i in 0..<(marks.count - 1) {
                for j in (i + 1)..<marks.count {
                    let dx = Double(marks[i].x - marks[j].x) * 1.64
                    let dy = Double(marks[i].y - marks[j].y)
                    let proximity = 1.0 - min(1.0, hypot(dx, dy))
                    let tether = UIBezierPath()
                    tether.move(to: mapped[i])
                    tether.addLine(to: mapped[j])
                    TrackDrumInk.accent
                        .withAlphaComponent(0.22 + proximity * 0.50).setStroke()
                    tether.lineWidth = (0.7 + proximity * 1.8) * ink
                    tether.stroke()

                    let knot = (1.2 + proximity * 1.8) * ink
                    let midpoint = CGPoint(x: (mapped[i].x + mapped[j].x) / 2,
                                           y: (mapped[i].y + mapped[j].y) / 2)
                    TrackDrumInk.accent
                        .withAlphaComponent(0.30 + proximity * 0.42).setFill()
                    UIBezierPath(ovalIn: CGRect(x: midpoint.x - knot,
                                                y: midpoint.y - knot,
                                                width: knot * 2,
                                                height: knot * 2)).fill()
                }
            }
        }
        for (index, point) in mapped.enumerated() {
            let anchors = marks.enumerated().compactMap {
                $0.offset == index ? nil : $0.element
            }
            let retained = TrackpadSurfaceEnergy.energy(at: marks[index],
                                                        charges: charges)
            let velocity = MenuBandPercussion.surfaceVelocityEnergy(
                at: marks[index], anchors: anchors, inertia: retained
            )
            let r = CGFloat((2.5 + velocity * 3.2) / 80) * fit.pointsPerUnit
            let dot = UIBezierPath(ovalIn: CGRect(x: point.x - r, y: point.y - r,
                                                  width: r * 2, height: r * 2))
            TrackDrumInk.accent.withAlphaComponent(0.55 + velocity * 0.42).setFill()
            dot.fill()
            UIColor.white.withAlphaComponent(0.85).setStroke()
            dot.lineWidth = 0.8 * ink
            dot.stroke()
        }
    }
}
