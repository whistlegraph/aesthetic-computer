import CoreGraphics
import Foundation

// Geometry checks for the phone edition. These run on macOS against the REAL
// MenuBandPercussion, so the drawn rings and the heard zones are compared
// against each other rather than against a copy of the same assumption.
//
//   apple/trackdrum/test.sh

var failures = 0

func check(_ label: String, _ passed: Bool, _ detail: @autoclosure () -> String = "") {
    if passed {
        print("  ok   \(label)")
    } else {
        failures += 1
        let extra = detail()
        print("  FAIL \(label)\(extra.isEmpty ? "" : " — \(extra)")")
    }
}

func near(_ a: Double, _ b: Double, _ tolerance: Double = 1e-9) -> Bool {
    abs(a - b) <= tolerance
}

func near(_ a: CGFloat, _ b: CGFloat, _ tolerance: CGFloat = 1e-9) -> Bool {
    abs(a - b) <= tolerance
}

func distance(_ drum: CGPoint) -> Double {
    MenuBandPercussion.roundedTrackpadDistance(sx: Double(drum.x - 0.5) * 2,
                                               sy: Double(drum.y - 0.5) * 2)
}

// An iPhone 14 Pro Max in points — the device this is installed on.
let screen = CGRect(x: 0, y: 0, width: 430, height: 932)
let fit = TrackDrumFit(bounds: screen)

print("the drum fills the glass")
check("no letterbox", fit.rect == screen)
for (label, point) in [("top", CGPoint(x: 215, y: 0)),
                       ("bottom", CGPoint(x: 215, y: 932)),
                       ("left", CGPoint(x: 0, y: 466)),
                       ("right", CGPoint(x: 430, y: 466)),
                       ("corner", CGPoint(x: 0, y: 0))] {
    check("the \(label) edge is the rim", near(distance(fit.drum(from: point)), 1.0, 1e-9),
          "\(distance(fit.drum(from: point)))")
}

print("the bands are even all the way round")
// The whole complaint. A band that measures 48 pt at the top and 33 at the side
// is what "stretched and scaled" looks like; these must come out equal.
for band in TrackDrumZones.bands where band.outer < 1.0 {
    let ring = TrackDrumZones.contour(band.outer, in: fit)
    let top = ring.minY - fit.rect.minY
    let bottom = fit.rect.maxY - ring.maxY
    let left = ring.minX - fit.rect.minX
    let right = fit.rect.maxX - ring.maxX
    check("\(band.name) ring is inset evenly",
          near(top, left, 1e-9) && near(top, right, 1e-9) && near(top, bottom, 1e-9),
          "top \(top), left \(left), right \(right), bottom \(bottom)")
}
// And the depth field itself: equal steps in from any edge read equal.
for step in stride(from: CGFloat(10), through: 200, by: 10) {
    let fromTop = distance(fit.drum(from: CGPoint(x: 215, y: step)))
    let fromSide = distance(fit.drum(from: CGPoint(x: step, y: 466)))
    check("\(Int(step)) pt in reads the same from the top and the side",
          near(fromTop, fromSide, 1e-9), "\(fromTop) vs \(fromSide)")
}

print("every zone is at least a thumb across")
for (index, band) in TrackDrumZones.bands.enumerated() {
    let outerInset = fit.screenDepth(
        fromEngine: (1 - band.outer) * TrackDrumFit.halfHeight)
    let innerInset = index == 0
        ? fit.reach
        : fit.screenDepth(fromEngine: (1 - TrackDrumZones.bands[index - 1].outer)
                                        * TrackDrumFit.halfHeight)
    // The kick is a core, not a ring: its narrowest measure is its full width.
    let across = index == 0 ? (innerInset - outerInset) * 2 : innerInset - outerInset
    check("\(band.name) is \(Int(across.rounded())) pt across", across >= 44,
          "\(across)")
}

print("the transform is one function, read both ways")
for p in [CGPoint(x: 12, y: 40), CGPoint(x: 215, y: 466), CGPoint(x: 428, y: 900),
          CGPoint(x: 1, y: 465), CGPoint(x: 214, y: 100), CGPoint(x: 300, y: 700)] {
    let round = fit.view(from: fit.drum(from: p))
    check("round trip \(p)", near(round.x, p.x, 1e-6) && near(round.y, p.y, 1e-6),
          "\(round)")
}
for p in [CGPoint(x: 0.1, y: 0.2), CGPoint(x: 0.93, y: 0.04), CGPoint(x: 0, y: 1),
          CGPoint(x: 0.97, y: 0.98)] {
    let round = fit.drum(from: fit.view(from: p))
    check("round trip \(p)", near(round.x, p.x, 1e-9) && near(round.y, p.y, 1e-9),
          "\(round)")
}

print("the quarter turn — counterclockwise")
// The drum's right edge ends up along the top of the phone; its far (y = 1)
// edge down the left side.
let corners: [(String, CGPoint, CGPoint)] = [
    ("drum bottom-left → phone bottom-right", CGPoint(x: 0, y: 0),
     CGPoint(x: screen.maxX, y: screen.maxY)),
    ("drum bottom-right → phone top-right", CGPoint(x: 1, y: 0),
     CGPoint(x: screen.maxX, y: screen.minY)),
    ("drum top-left → phone bottom-left", CGPoint(x: 0, y: 1),
     CGPoint(x: screen.minX, y: screen.maxY)),
    ("drum top-right → phone top-left", CGPoint(x: 1, y: 1),
     CGPoint(x: screen.minX, y: screen.minY)),
]
for (label, drum, expected) in corners {
    let got = fit.view(from: drum)
    check(label, near(got.x, expected.x, 1e-9) && near(got.y, expected.y, 1e-9),
          "got \(got), wanted \(expected)")
}

func cross(_ a: CGPoint, _ b: CGPoint, _ c: CGPoint) -> CGFloat {
    (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}
let dA = CGPoint(x: 0.2, y: 0.2), dB = CGPoint(x: 0.8, y: 0.25), dC = CGPoint(x: 0.3, y: 0.7)
check("no mirror — winding survives the y flip",
      cross(dA, dB, dC) * cross(fit.view(from: dA), fit.view(from: dB), fit.view(from: dC)) < 0)

print("stereo runs across the glass, not up it")
// The engine pans on the drum's x axis, which the turn stands upright. The
// phone points `panAxis` at the other one, so left on the glass is left in the
// ears. An un-flipped y would silently reverse this.
func pan(_ viewPoint: CGPoint) -> Double {
    let axis = TrackDrumFit.panAxis(for: fit.turn)
    let drum = fit.drum(from: viewPoint)
    return (Double(drum.x - 0.5) * 2 * axis.x + Double(drum.y - 0.5) * 2 * axis.y) * 0.72
}
check("left of the glass is the left channel", pan(CGPoint(x: 20, y: 466)) < -0.4,
      "\(pan(CGPoint(x: 20, y: 466)))")
check("right of the glass is the right channel", pan(CGPoint(x: 410, y: 466)) > 0.4,
      "\(pan(CGPoint(x: 410, y: 466)))")
check("sliding across sweeps pan monotonically",
      stride(from: CGFloat(2), through: 428, by: 6)
          .map { pan(CGPoint(x: $0, y: 466)) }
          .adjacentPairs().allSatisfy { $0 < $1 })
check("sliding up the glass does not swing the stereo image",
      stride(from: CGFloat(2), through: 930, by: 8)
          .map { abs(pan(CGPoint(x: 215, y: $0))) }.max()! < 1e-9)

print("the other turn is the same instrument, 180° away")
let cw = TrackDrumFit(bounds: screen, turn: .clockwise)
check("clockwise round-trips too",
      near(cw.drum(from: cw.view(from: dA)).x, dA.x, 1e-9)
          && near(cw.drum(from: cw.view(from: dA)).y, dA.y, 1e-9))
check("clockwise is the counterclockwise picture rotated a half turn",
      near(cw.view(from: dA).x, screen.maxX - fit.view(from: dA).x, 1e-6)
          && near(cw.view(from: dA).y, screen.maxY - fit.view(from: dA).y, 1e-6))

print("drawn rings sit on the engine's own contours")
// Every point of a drawn ring, pushed back through the transform, must read the
// ring's distance under MenuBandPercussion's own field. This is the check that
// keeps the picture and the sound from telling different stories.
func perimeter(_ rect: CGRect, per: Int) -> [CGPoint] {
    (0...per).flatMap { step -> [CGPoint] in
        let t = CGFloat(step) / CGFloat(per)
        return [CGPoint(x: rect.minX + rect.width * t, y: rect.minY),
                CGPoint(x: rect.minX + rect.width * t, y: rect.maxY),
                CGPoint(x: rect.minX, y: rect.minY + rect.height * t),
                CGPoint(x: rect.maxX, y: rect.minY + rect.height * t)]
    }
}
for band in TrackDrumZones.bands {
    let worst = perimeter(TrackDrumZones.contour(band.outer, in: fit), per: 60)
        .map { abs(distance(fit.drum(from: $0)) - band.outer) }
        .max() ?? 1
    check("\(band.name) ring is the \(band.outer) contour", worst < 1e-9,
          "off by \(worst)")
}

print("what is drawn is what is played")
var mismatches = 0
var sampled = 0
for row in 0...160 {
    for column in 0...60 {
        let point = CGPoint(x: screen.width * CGFloat(column) / 60,
                            y: screen.height * CGFloat(row) / 160)
        let drum = fit.drum(from: point)
        let d = distance(drum)
        // A sample sitting exactly on a ring is a coin toss between the two
        // sides of `<`, not a mapping error. Step over those.
        guard !TrackDrumZones.bands.contains(where: { abs(d - $0.outer) < 1e-6 })
        else { continue }
        let heard = MenuBandPercussion.drumSkinZone(at: drum).rawValue
        let drawn = TrackDrumZones.bands.first {
            $0.outer < 1.0 && TrackDrumZones.contour($0.outer, in: fit).contains(point)
        }?.name ?? "click"
        sampled += 1
        if drawn != heard { mismatches += 1 }
    }
}
check("every sampled touch lands in the ring it sounds",
      mismatches == 0, "\(mismatches) of \(sampled) disagreed")

print(failures == 0 ? "\n✓ geometry holds" : "\n✗ \(failures) failing")
exit(failures == 0 ? 0 : 1)

extension Array {
    func adjacentPairs() -> [(Element, Element)] {
        guard count > 1 else { return [] }
        return (1..<count).map { (self[$0 - 1], self[$0]) }
    }
}
