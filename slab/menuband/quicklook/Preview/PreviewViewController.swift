// QuickLook preview for .mbscore: a live piano-roll "graphic score" with a
// playhead that sweeps in real time (looping over the score's duration), notes
// lighting up as it passes and drum hits flashing — so ⌘-Space shows the score
// *playing*, not raw JSON.
import Cocoa
import Quartz

final class PreviewViewController: NSViewController, QLPreviewingController {
    private let roll = ScoreRollView(frame: NSRect(x: 0, y: 0, width: 1040, height: 640))

    override func loadView() { view = roll }

    func preparePreviewOfFile(at url: URL) async throws {
        let data = try Data(contentsOf: url)
        let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any] ?? [:]
        await MainActor.run { self.roll.load(obj) }
    }

    override func viewWillDisappear() { super.viewWillDisappear(); roll.stop() }
}

private func drumLane(_ s: Substring) -> Int? {
    switch s { case "k": return 0; case "s": return 1; case "c": return 2
    case "h": return 3; case "ho": return 4; case "rd": return 5; case "cr": return 6
    default: return nil }
}

final class ScoreRollView: NSView {
    private struct Note { let start, dur: Double; let midi, voice, track: Int }
    private struct Hit { let start: Double; let lane, voice: Int }

    private var notes: [Note] = []
    private var hits: [Hit] = []
    private var total = 1.0          // beats
    private var seconds = 8.0        // wall duration of one pass
    private var title = ""
    private var machines = 1
    private var bpm = 120
    private var startTime = CACurrentMediaTime()
    private var timer: Timer?

    private let palette: [NSColor] = [
        NSColor(srgbRed: 1.0, green: 0.42, blue: 0.62, alpha: 1),
        NSColor(srgbRed: 0.35, green: 0.82, blue: 0.90, alpha: 1),
        NSColor(srgbRed: 0.98, green: 0.80, blue: 0.35, alpha: 1),
        NSColor(srgbRed: 0.55, green: 0.85, blue: 0.55, alpha: 1),
    ]

    override var isFlipped: Bool { false }

    func load(_ obj: [String: Any]) {
        title = (obj["title"] as? String) ?? "Menu Band Score"
        bpm = (obj["bpm"] as? Int) ?? Int((obj["bpm"] as? Double) ?? 120)
        let voices = (obj["voices"] as? [[String: Any]]) ?? []
        machines = (obj["machines"] as? Int) ?? max(1, voices.count)
        notes.removeAll(); hits.removeAll(); total = 1
        for (vi, voice) in voices.enumerated() {
            for (ti, key) in ["notes", "notes2", "notes3", "notes4"].enumerated() {
                guard let spec = voice[key] as? String else { continue }
                var t = 0.0
                for tok in spec.split(separator: ",") {
                    let p = tok.split(separator: ":")
                    guard p.count == 2, let b = Double(p[1]) else { continue }
                    if p[0] == "r" { t += b; continue }
                    if let lane = drumLane(p[0]) { hits.append(Hit(start: t, lane: lane, voice: vi)) }
                    else if let m = Int(p[0]) { notes.append(Note(start: t, dur: b, midi: m, voice: vi, track: ti)) }
                    t += b
                }
                total = max(total, t)
            }
        }
        seconds = max(2, total * 60.0 / Double(max(1, bpm)))
        startTime = CACurrentMediaTime()
        timer?.invalidate()
        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            self?.needsDisplay = true
        }
        RunLoop.main.add(timer!, forMode: .common)
        needsDisplay = true
    }

    func stop() { timer?.invalidate(); timer = nil }
    deinit { timer?.invalidate() }

    override func draw(_ dirty: NSRect) {
        let S = bounds
        NSColor(srgbRed: 0.06, green: 0.07, blue: 0.10, alpha: 1).setFill()
        S.fill()

        let drumStrip: CGFloat = hits.isEmpty ? 0 : 64
        let plot = NSRect(x: 40, y: 70 + drumStrip, width: S.width - 80, height: S.height - 70 - drumStrip - 74)
        let midis = notes.map { $0.midi }
        let minM = (midis.min() ?? 48) - 2, maxM = (midis.max() ?? 84) + 2
        let span = max(1, maxM - minM)
        func px(_ b: Double) -> CGFloat { plot.minX + CGFloat(b / total) * plot.width }
        func py(_ m: Int) -> CGFloat { plot.minY + CGFloat(Double(m - minM) / Double(span)) * plot.height }

        // Playhead position (loops).
        let elapsed = (CACurrentMediaTime() - startTime).truncatingRemainder(dividingBy: seconds)
        let headBeat = elapsed / seconds * total
        let headX = px(headBeat)

        // Octave gridlines.
        NSColor(white: 1, alpha: 0.05).setStroke()
        var m = minM - (minM % 12)
        while m <= maxM { let p = NSBezierPath(); p.move(to: NSPoint(x: plot.minX, y: py(m))); p.line(to: NSPoint(x: plot.maxX, y: py(m))); p.lineWidth = 1; p.stroke(); m += 12 }

        // Notes — brighter when the playhead is inside them (they "sound").
        for n in notes {
            let x0 = px(n.start), x1 = px(n.start + n.dur)
            let live = headBeat >= n.start && headBeat < n.start + n.dur
            let r = NSRect(x: x0, y: py(n.midi) - (live ? 5 : 3.5), width: max(4, x1 - x0 - 1.5), height: live ? 10 : 7)
            let base = palette[n.voice % palette.count]
            let col = live ? (base.blended(withFraction: 0.5, of: .white) ?? base) : base.withAlphaComponent(0.82)
            col.setFill()
            NSBezierPath(roundedRect: r, xRadius: 3, yRadius: 3).fill()
        }

        // Drum hits — flash as the head crosses.
        if !hits.isEmpty {
            let strip = NSRect(x: plot.minX, y: 60, width: plot.width, height: drumStrip - 12)
            for h in hits {
                let rowY = strip.minY + CGFloat(h.lane) * (strip.height / 7) + 3
                let dist = abs(px(h.start) - headX)
                let flash = max(0, 1 - dist / 22)
                let d: CGFloat = (h.lane == 0 ? 7 : 5) + flash * 6
                let c = palette[h.voice % palette.count].blended(withFraction: 0.2 + flash * 0.6, of: .white) ?? .white
                c.withAlphaComponent(0.85).setFill()
                NSBezierPath(ovalIn: NSRect(x: px(h.start) - d / 2, y: rowY - flash * 2, width: d, height: d)).fill()
            }
        }

        // Playhead line + glow.
        NSColor(srgbRed: 1, green: 1, blue: 1, alpha: 0.9).setStroke()
        let head = NSBezierPath()
        head.move(to: NSPoint(x: headX, y: 54)); head.line(to: NSPoint(x: headX, y: S.height - 70))
        head.lineWidth = 2; head.stroke()

        // Title + meta.
        let ps = NSMutableParagraphStyle(); ps.alignment = .center; ps.lineBreakMode = .byTruncatingTail
        NSAttributedString(string: title, attributes: [
            .font: NSFont.systemFont(ofSize: 26, weight: .bold),
            .foregroundColor: NSColor.white, .paragraphStyle: ps,
        ]).draw(in: NSRect(x: 30, y: S.height - 58, width: S.width - 60, height: 34))
        let dots = String(repeating: "●", count: max(1, machines))
        NSAttributedString(string: "\(dots)   \(bpm) bpm   ·   \(Int(total)) beats   ·   \(Int(seconds))s", attributes: [
            .font: NSFont.monospacedSystemFont(ofSize: 14, weight: .medium),
            .foregroundColor: NSColor(white: 1, alpha: 0.5), .paragraphStyle: ps,
        ]).draw(in: NSRect(x: 30, y: 24, width: S.width - 60, height: 22))
    }
}
