import SwiftUI
import UIKit

@MainActor
final class DrawingDraft: ObservableObject {
    @Published var enabled = false
    @Published private(set) var strokes: [[[Double]]] = []
    @Published private(set) var revision = 0
    private(set) var id = UUID().uuidString
    private var startedAt: TimeInterval = 0
    private var aspect = 4.0 / 3.0
    private var active = false
    var hasInk: Bool { !strokes.isEmpty }
    var full: Bool { strokes.count >= 32 || strokes.reduce(0) { $0 + $1.count } >= 1200 }
    func begin(_ point: CGPoint, in size: CGSize, pressure: Double?) {
        guard !full, size.width > 0, size.height > 0 else { return }
        if !hasInk { startedAt = ProcessInfo.processInfo.systemUptime; aspect = size.width / size.height }
        strokes.append([]); active = true
        append(point, in: size, pressure: pressure, force: true)
    }
    func append(_ point: CGPoint, in size: CGSize, pressure: Double?, force: Bool = false) {
        guard active, !strokes.isEmpty, size.width > 0, size.height > 0,
              strokes.reduce(0, { $0 + $1.count }) < 1200 else { return }
        let x = min(1000, max(0, point.x / size.width * 1000)).rounded()
        let y = min(1000, max(0, point.y / size.height * 1000)).rounded()
        let time = min(3600000, max(0, (ProcessInfo.processInfo.systemUptime - startedAt) * 1000)).rounded()
        if !force, let previous = strokes.last?.last,
           hypot(x - previous[0], y - previous[1]) < 3, time - previous[2] < 40 { return }
        var sample = [x, y, time]
        if let pressure { sample.append((min(1, max(0, pressure)) * 1000).rounded()) }
        strokes[strokes.count - 1].append(sample); revision += 1
    }
    func end() { active = false }
    func undo() { active = false; if hasInk { strokes.removeLast(); revision += 1 } }
    func clear() { active = false; strokes = []; revision += 1; id = UUID().uuidString; startedAt = 0 }
    func consume(id: String, revision: Int) { if self.id == id && self.revision == revision { clear() } }
    func payload(speechStart: TimeInterval? = nil) -> [String: Any]? {
        guard hasInk else { return nil }
        var value: [String: Any] = ["schema": "whistlegraph-drawing/v1", "id": id, "revision": revision,
                                    "aspect": aspect, "strokes": strokes]
        if let speechStart { value["speechStartMs"] = ((speechStart - startedAt) * 1000).rounded() }
        return value
    }
}

struct DrawingPad: UIViewRepresentable {
    @ObservedObject var draft: DrawingDraft
    var interactive: Bool
    func makeUIView(context: Context) -> GestureInkView {
        let view = GestureInkView(); view.backgroundColor = .clear; view.isOpaque = false
        view.isMultipleTouchEnabled = false; view.accessibilityIdentifier = "drawing-pad"
        view.accessibilityLabel = "Drawing over the piece"
        view.accessibilityHint = "Draw with a finger or Pencil while talking or typing."
        return view
    }
    func updateUIView(_ view: GestureInkView, context: Context) {
        view.draft = draft; view.isUserInteractionEnabled = interactive; view.setNeedsDisplay()
    }
}

final class GestureInkView: UIView {
    var draft: DrawingDraft?
    private func pressure(_ touch: UITouch) -> Double? {
        guard touch.type == .pencil, touch.maximumPossibleForce > 0 else { return nil }
        return Double(touch.force / touch.maximumPossibleForce)
    }
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        guard let touch = touches.first else { return }
        draft?.begin(touch.location(in: self), in: bounds.size, pressure: pressure(touch)); setNeedsDisplay()
    }
    override func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent?) {
        guard let touch = touches.first else { return }
        draft?.append(touch.location(in: self), in: bounds.size, pressure: pressure(touch)); setNeedsDisplay()
    }
    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        if let touch = touches.first { draft?.append(touch.location(in: self), in: bounds.size, pressure: pressure(touch), force: true) }
        draft?.end(); setNeedsDisplay()
    }
    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) { draft?.end() }
    override func draw(_ rect: CGRect) {
        guard let strokes = draft?.strokes else { return }
        for stroke in strokes {
            guard let first = stroke.first else { continue }
            let path = UIBezierPath(); path.lineCapStyle = .round; path.lineJoinStyle = .round
            let origin = CGPoint(x: first[0] / 1000 * bounds.width, y: first[1] / 1000 * bounds.height)
            path.move(to: origin)
            for p in stroke.dropFirst() { path.addLine(to: CGPoint(x: p[0] / 1000 * bounds.width, y: p[1] / 1000 * bounds.height)) }
            if stroke.count == 1 { path.addLine(to: CGPoint(x: origin.x + 0.1, y: origin.y)) }
            UIColor.black.withAlphaComponent(0.8).setStroke(); path.lineWidth = 7; path.stroke()
            UIColor(red: 1, green: 0.14, blue: 1, alpha: 1).setStroke(); path.lineWidth = 4; path.stroke()
        }
    }
}
