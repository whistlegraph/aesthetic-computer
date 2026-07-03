// QuickLook thumbnail for .mbscore — an appearance-aware piano-roll "graphic
// score" card. Generated on demand, so unlike a baked file icon it renders with
// the CURRENT light/dark appearance (light card in light mode, dark in dark).
import AppKit
import QuickLookThumbnailing

final class ThumbnailProvider: QLThumbnailProvider {
    override func provideThumbnail(for request: QLFileThumbnailRequest,
                                   _ handler: @escaping (QLThumbnailReply?, Error?) -> Void) {
        guard let data = try? Data(contentsOf: request.fileURL),
              let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any] else {
            handler(nil, nil); return
        }
        let size = request.maximumSize
        handler(QLThumbnailReply(contextSize: size, currentContextDrawing: {
            ScoreThumb.draw(obj, size: size)
            return true
        }), nil)
    }
}

enum ScoreThumb {
    private static func drumLane(_ s: Substring) -> Int? {
        switch s { case "k": return 0; case "s": return 1; case "c": return 2
        case "h": return 3; case "ho": return 4; case "rd": return 5; case "cr": return 6
        default: return nil }
    }

    private static let palette: [NSColor] = [
        NSColor(srgbRed: 1.0, green: 0.42, blue: 0.62, alpha: 1),
        NSColor(srgbRed: 0.20, green: 0.66, blue: 0.86, alpha: 1),
        NSColor(srgbRed: 0.92, green: 0.66, blue: 0.15, alpha: 1),
        NSColor(srgbRed: 0.35, green: 0.72, blue: 0.42, alpha: 1),
    ]

    static func draw(_ obj: [String: Any], size: CGSize) {
        struct Note { let start, dur: Double; let midi, voice: Int }
        struct Hit { let start: Double; let lane, voice: Int }
        let title = (obj["title"] as? String) ?? "Menu Band Score"
        let voices = (obj["voices"] as? [[String: Any]]) ?? []
        var notes: [Note] = []; var hits: [Hit] = []; var total = 1.0
        for (vi, voice) in voices.enumerated() {
            for key in ["notes", "notes2", "notes3", "notes4"] {
                guard let spec = voice[key] as? String else { continue }
                var t = 0.0
                for tok in spec.split(separator: ",") {
                    let p = tok.split(separator: ":")
                    guard p.count == 2, let b = Double(p[1]) else { continue }
                    if p[0] == "r" { t += b; continue }
                    if let lane = drumLane(p[0]) { hits.append(Hit(start: t, lane: lane, voice: vi)) }
                    else if let m = Int(p[0]) { notes.append(Note(start: t, dur: b, midi: m, voice: vi)) }
                    t += b
                }
                total = max(total, t)
            }
        }
        if total <= 0 { total = 1 }

        let isDark = NSAppearance.currentDrawing().bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let W = size.width, H = size.height
        let inset = W * 0.035
        let card = NSBezierPath(roundedRect: NSRect(x: inset, y: inset, width: W - inset * 2, height: H - inset * 2),
                                xRadius: W * 0.14, yRadius: W * 0.14)
        (isDark ? NSColor(srgbRed: 0.10, green: 0.11, blue: 0.15, alpha: 1)
                : NSColor(srgbRed: 0.97, green: 0.97, blue: 0.98, alpha: 1)).setFill()
        card.fill()
        card.setClip()

        let drumStrip: CGFloat = hits.isEmpty ? 0 : H * 0.13
        let plot = NSRect(x: W * 0.10, y: H * 0.14 + drumStrip, width: W * 0.80, height: H - H * 0.30 - drumStrip)
        let midis = notes.map { $0.midi }
        let minM = (midis.min() ?? 48) - 2, maxM = (midis.max() ?? 84) + 2
        let span = max(1, maxM - minM)
        func px(_ b: Double) -> CGFloat { plot.minX + CGFloat(b / total) * plot.width }
        func py(_ m: Int) -> CGFloat { plot.minY + CGFloat(Double(m - minM) / Double(span)) * plot.height }

        let barH = max(2, H * 0.014)
        for n in notes {
            let x0 = px(n.start), x1 = px(n.start + n.dur)
            let r = NSRect(x: x0, y: py(n.midi) - barH / 2, width: max(barH, x1 - x0 - 1), height: barH)
            palette[n.voice % palette.count].setFill()
            NSBezierPath(roundedRect: r, xRadius: barH * 0.4, yRadius: barH * 0.4).fill()
        }
        if !hits.isEmpty {
            let strip = NSRect(x: plot.minX, y: H * 0.12, width: plot.width, height: drumStrip - H * 0.02)
            let d = max(2, W * 0.012)
            for h in hits {
                let rowY = strip.minY + CGFloat(h.lane) * (strip.height / 7)
                palette[h.voice % palette.count].withAlphaComponent(0.9).setFill()
                NSBezierPath(ovalIn: NSRect(x: px(h.start) - d / 2, y: rowY, width: d, height: d)).fill()
            }
        }

        let ps = NSMutableParagraphStyle(); ps.alignment = .center; ps.lineBreakMode = .byTruncatingTail
        NSAttributedString(string: title, attributes: [
            .font: NSFont.systemFont(ofSize: H * 0.075, weight: .bold),
            .foregroundColor: isDark ? NSColor.white : NSColor(white: 0.12, alpha: 1),
            .paragraphStyle: ps,
        ]).draw(in: NSRect(x: W * 0.08, y: H - H * 0.15, width: W * 0.84, height: H * 0.10))
    }
}
