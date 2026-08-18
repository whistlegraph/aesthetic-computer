import CoreGraphics
import Foundation

/// Which way the drum is turned to stand up in a portrait phone. Touches and
/// drawing both read this one value, so turning it the other way after holding
/// the thing is a one-word change and nothing else moves.
enum TrackDrumTurn {
    /// The trackpad's right edge lies along the top of the phone.
    case counterclockwise
    /// The trackpad's right edge lies along the bottom of the phone.
    case clockwise
}

/// The drum, re-proportioned for the phone rather than stretched onto it.
///
/// The first version fitted the Mac's 1.64:1 rectangle to the glass and let the
/// two axes scale differently. It filled the screen, but every band came out
/// thicker one way than the other and every circle came out an egg — the drum
/// looked squashed, because it was.
///
/// So the field is built here instead, in the view's own proportions, and
/// measured ISOTROPICALLY: a point's depth is its distance in POINTS from the
/// nearest edge, the same measure on all four sides. Bands of constant depth are
/// therefore rectangles inset evenly all round — 48 points at the top is 48
/// points at the side — and a circle drawn in view points is a circle.
///
/// The engine never learns about any of this. A view point is carried onto the
/// drum's own contour at the matching depth, so `playDrumSkin` receives exactly
/// the normalized drum coordinates it receives on a Mac and sounds the same.
/// Both worlds are rounded-rectangle offset families; what crosses between them
/// is (depth, position around the ring), and both parts are invertible. That is
/// why hit-testing and drawing cannot drift apart: they are this one pair of
/// functions, read in opposite directions.
struct TrackDrumFit {
    /// The engine's own proportions, from MenuBandPercussion's distance field.
    static let halfWidth = 0.82
    static let halfHeight = 0.50

    /// No band should be thinner than a thumb. iOS calls that 44 points; the
    /// four playable rings are raised to it and the core absorbs the cost.
    static let bandFloor: CGFloat = 44
    /// …but never past the point where the core stops being a target itself.
    static let coreFloor: CGFloat = 22

    /// The turn is a property of the instrument, not of any one view.
    static let turn = TrackDrumTurn.counterclockwise

    /// Which way stereo runs across the glass, for the engine's `panAxis`. The
    /// turn stands the drum's own pan axis upright, so left and right have to be
    /// taken from the other one.
    static func panAxis(for turn: TrackDrumTurn) -> (x: Double, y: Double) {
        (x: 0, y: turn == .counterclockwise ? -1 : 1)
    }

    let rect: CGRect
    let turn: TrackDrumTurn
    /// Depth of each band boundary — rim outward-in, ending at the drum's medial
    /// axis. Screen depth in points beside the engine depth it stands for.
    let knots: [(screen: CGFloat, engine: Double)]

    /// How deep the drum goes: half its narrow side, since depth is measured
    /// from the nearest edge either way.
    var reach: CGFloat { min(rect.width, rect.height) / 2 }

    /// View points per unit of the engine's surface, for sizing decorations the
    /// Mac quotes in its own chart's points.
    var pointsPerUnit: CGFloat { reach / CGFloat(Self.halfHeight) }

    init(bounds: CGRect, turn: TrackDrumTurn = TrackDrumFit.turn) {
        rect = bounds
        self.turn = turn

        // Band boundaries as the engine states them, rim inward: the rim
        // itself, then each band's inner edge, ending at the medial axis.
        let depths = [0.0]
            + TrackDrumZones.bands.reversed().dropFirst().map {
                (1 - $0.outer) * Self.halfHeight
            }
            + [Self.halfHeight]
        let reach = min(bounds.width, bounds.height) / 2
        let scale = reach / CGFloat(Self.halfHeight)
        var widths = (1..<depths.count).map {
            CGFloat(depths[$0] - depths[$0 - 1]) * scale
        }
        // The Mac tuned these against 16 cm of trackpad. At phone scale the
        // outer four come out as slivers you would have to aim at, so each is
        // raised to a thumb and the kick core — already the biggest region —
        // gives up the difference.
        let core = widths.count - 1
        for index in 0..<core { widths[index] = max(widths[index], Self.bandFloor) }
        let outer = widths[0..<core].reduce(0, +)
        widths[core] = reach - outer
        if widths[core] < Self.coreFloor {
            // A narrower phone than this was written for: everyone shrinks back
            // toward proportion rather than the core vanishing.
            let available = reach - Self.coreFloor
            for index in 0..<core { widths[index] *= available / outer }
            widths[core] = Self.coreFloor
        }
        var screen: CGFloat = 0
        knots = zip(depths, [0] + widths).map { depth, width in
            screen += width
            return (screen: screen, engine: depth)
        }
    }

    /// Points in from the nearest edge. The same measure on all four sides — the
    /// whole reason the bands read even.
    func depth(at view: CGPoint) -> CGFloat {
        min(min(view.x - rect.minX, rect.maxX - view.x),
            min(view.y - rect.minY, rect.maxY - view.y))
    }

    /// View depth → the engine's own inward depth, and back. Piecewise linear
    /// through the band boundaries, so the floors above cost nothing but a
    /// gentle kink at each ring.
    func engineDepth(fromScreen points: CGFloat) -> Double {
        let points = max(0, min(knots[knots.count - 1].screen, points))
        for index in 1..<knots.count where points <= knots[index].screen {
            let lo = knots[index - 1], hi = knots[index]
            guard hi.screen > lo.screen else { return hi.engine }
            let t = Double((points - lo.screen) / (hi.screen - lo.screen))
            return lo.engine + (hi.engine - lo.engine) * t
        }
        return knots[knots.count - 1].engine
    }

    func screenDepth(fromEngine depth: Double) -> CGFloat {
        let depth = max(0, min(knots[knots.count - 1].engine, depth))
        for index in 1..<knots.count where depth <= knots[index].engine {
            let lo = knots[index - 1], hi = knots[index]
            guard hi.engine > lo.engine else { return hi.screen }
            let t = CGFloat((depth - lo.engine) / (hi.engine - lo.engine))
            return lo.screen + (hi.screen - lo.screen) * t
        }
        return knots[knots.count - 1].screen
    }

    /// View point → normalized drum space.
    func drum(from view: CGPoint) -> CGPoint {
        let u = view.x - rect.midX
        let v = view.y - rect.midY
        let s = depth(at: view)
        // Where the point sits around its own ring: ±1 on whichever axis it is
        // nearest to, somewhere inside ±1 on the other.
        let across = rect.width / 2 - s
        let along = rect.height / 2 - s
        let sideways = across > 0 ? u / across : 0
        let lengthways = along > 0 ? v / along : 0
        // The same place on the drum's ring at the matching depth. The screen's
        // long axis is the drum's long axis; a quarter turn is what makes that
        // true, and the sign is what makes it a turn and not a mirror.
        let depth = engineDepth(fromScreen: s)
        let sign: CGFloat = turn == .counterclockwise ? -1 : 1
        let px = sign * lengthways * CGFloat(Self.halfWidth - depth)
        let py = sign * sideways * CGFloat(Self.halfHeight - depth)
        return CGPoint(x: 0.5 + px / CGFloat(2 * Self.halfWidth),
                       y: 0.5 + py / CGFloat(2 * Self.halfHeight))
    }

    /// Normalized drum space → view point. The exact inverse.
    func view(from drum: CGPoint) -> CGPoint {
        let px = (drum.x - 0.5) * CGFloat(2 * Self.halfWidth)
        let py = (drum.y - 0.5) * CGFloat(2 * Self.halfHeight)
        let depth = min(Self.halfWidth - Double(abs(px)),
                        Self.halfHeight - Double(abs(py)))
        let longExtent = CGFloat(Self.halfWidth - depth)
        let shortExtent = CGFloat(Self.halfHeight - depth)
        let sign: CGFloat = turn == .counterclockwise ? -1 : 1
        let lengthways = longExtent > 0 ? sign * px / longExtent : 0
        let sideways = shortExtent > 0 ? sign * py / shortExtent : 0
        let s = screenDepth(fromEngine: depth)
        return CGPoint(x: rect.midX + sideways * (rect.width / 2 - s),
                       y: rect.midY + lengthways * (rect.height / 2 - s))
    }
}

/// The five concentric instruments, drawn exactly where they are heard.
///
/// Every ring is now simply the view inset evenly — which is the point. The
/// tests push the ring's own corners and edges back through the fit and check
/// them against MenuBandPercussion's field, so "drawn evenly" and "heard
/// correctly" are the same claim and both are checked.
enum TrackDrumZones {
    /// Each band and the distance at which it ends, from `drumSkinZone(at:)`.
    static let bands: [(name: String, outer: Double)] = [
        ("kick", 0.30),
        ("tom", 0.46),
        ("snare", 0.64),
        ("hat", 0.88),
        ("click", 1.0),
    ]

    /// The ring at `distance`, in view coordinates.
    static func contour(_ distance: Double, in fit: TrackDrumFit) -> CGRect {
        let inset = fit.screenDepth(
            fromEngine: (1 - max(0, min(1, distance))) * TrackDrumFit.halfHeight
        )
        return fit.rect.insetBy(dx: inset, dy: inset)
    }
}
