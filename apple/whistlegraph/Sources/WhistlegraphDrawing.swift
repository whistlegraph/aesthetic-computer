import SwiftUI
import UIKit

@MainActor
final class DrawingDraft: ObservableObject {
    @Published var enabled = false { didSet { if enabled != oldValue { DeviceActionLog.shared.record(.drawing, enabled ? .enabled : .disabled) } } }
    @Published private(set) var strokes: [[[Double]]] = []
    @Published private(set) var revision = 0
    private(set) var id = UUID().uuidString
    private var startedAt: TimeInterval = 0
    private var aspect = 4.0 / 3.0
    private var active = false
    var hasInk: Bool { !strokes.isEmpty }
    var full: Bool { strokes.count >= 32 || strokes.reduce(0) { $0 + $1.count } >= 1200 }
    private struct Saved: Codable {
        let id: String
        let revision: Int
        let aspect: Double
        let strokes: [[[Double]]]
    }
    private var draftURL: URL {
        FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("whistlegraph-drawing-draft.json")
    }
    init() {
        guard let data = try? Data(contentsOf: draftURL),
              let saved = try? JSONDecoder().decode(Saved.self, from: data),
              saved.aspect.isFinite, saved.aspect > 0, saved.revision >= 0,
              saved.strokes.count <= 32,
              saved.strokes.reduce(0, { $0 + $1.count }) <= 1200,
              saved.strokes.allSatisfy({ $0.allSatisfy { $0.count >= 3 && $0.allSatisfy(\.isFinite) } }) else { return }
        id = saved.id; revision = saved.revision; aspect = saved.aspect; strokes = saved.strokes
        enabled = !strokes.isEmpty
        startedAt = ProcessInfo.processInfo.systemUptime - (strokes.last?.last?[2] ?? 0) / 1000
    }
    private func persist() {
        do {
            try FileManager.default.createDirectory(at: draftURL.deletingLastPathComponent(), withIntermediateDirectories: true)
            let data = try JSONEncoder().encode(Saved(id: id, revision: revision, aspect: aspect, strokes: strokes))
            try data.write(to: draftURL, options: [.atomic, .completeFileProtectionUntilFirstUserAuthentication])
        } catch { NSLog("Drawing draft save failed: %@", error.localizedDescription) }
    }
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
        strokes[strokes.count - 1].append(sample); revision += 1; persist()
    }
    func end() { active = false; DeviceActionLog.shared.record(.drawing, .add, [.strokes: strokes.count, .points: strokes.reduce(0) { $0 + $1.count }]) }
    func undo() { DeviceActionLog.shared.record(.drawing, .undo); active = false; if hasInk { strokes.removeLast(); revision += 1; persist() } }
    func clear() { DeviceActionLog.shared.record(.drawing, .clear); active = false; strokes = []; revision += 1; id = UUID().uuidString; startedAt = 0; persist() }
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
        view.isMultipleTouchEnabled = false; view.isExclusiveTouch = false; view.isAccessibilityElement = true; view.accessibilityTraits = .allowsDirectInteraction; view.accessibilityIdentifier = "drawing-pad"
        view.accessibilityLabel = "Chalk over the piece"
        view.accessibilityHint = "Add chalk strokes with a finger or Pencil while talking or typing."
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
    static let chalk = UIColor(red: 1, green: 0.55, blue: 0.95, alpha: 1)
    // A dry chalk mark: a powder halo, a dark edge so it reads over any piece,
    // the stick itself, then grain scattered along the line. The grain is
    // seeded per stroke so it never shimmers between redraws.
    override func draw(_ rect: CGRect) {
        guard let strokes = draft?.strokes else { return }
        let chalk = Self.chalk
        for (index, stroke) in strokes.enumerated() {
            guard !stroke.isEmpty else { continue }
            let points = stroke.map { CGPoint(x: $0[0] / 1000 * bounds.width, y: $0[1] / 1000 * bounds.height) }
            let path = UIBezierPath(); path.lineCapStyle = .round; path.lineJoinStyle = .round
            path.move(to: points[0])
            for p in points.dropFirst() { path.addLine(to: p) }
            // Touch-up adds a timed sample even when a held tap never moved.
            if points.allSatisfy({ $0 == points[0] }) { path.addLine(to: CGPoint(x: points[0].x + 0.1, y: points[0].y)) }
            chalk.withAlphaComponent(0.16).setStroke(); path.lineWidth = 12; path.stroke()
            UIColor.black.withAlphaComponent(0.35).setStroke(); path.lineWidth = 6.5; path.stroke()
            chalk.withAlphaComponent(0.9).setStroke(); path.lineWidth = 4.5; path.stroke()
            var seed = UInt32(truncatingIfNeeded: index &* 2_654_435_761 &+ 97)
            func noise() -> CGFloat { seed = seed &* 1_664_525 &+ 1_013_904_223; return CGFloat((seed >> 8) & 0xffff) / 65535 }
            for p in points {
                for _ in 0..<2 {
                    let angle = noise() * .pi * 2, radius = 2 + noise() * 4.5, size = 1.2 + noise() * 1.3
                    let dot = CGRect(x: p.x + cos(angle) * radius - size / 2, y: p.y + sin(angle) * radius - size / 2, width: size, height: size)
                    (noise() < 0.55 ? chalk : UIColor.white).withAlphaComponent(0.22 + noise() * 0.33).setFill()
                    UIBezierPath(ovalIn: dot).fill()
                }
            }
        }
    }
}
