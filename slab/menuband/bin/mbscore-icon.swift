#!/usr/bin/env swift
// mbscore-icon.swift — render a .mbscore as a piano-roll "graphic score" and
// set it as that file's Finder icon. Usage: swift mbscore-icon.swift a.mbscore …
import AppKit
import Foundation

func drumLane(_ s: Substring) -> Int? {
    switch s { case "k": return 0; case "s": return 1; case "c": return 2
    case "h": return 3; case "ho": return 4; case "rd": return 5; case "cr": return 6
    default: return nil }
}

struct Note { let start, dur: Double; let midi, voice, track: Int }
struct Hit { let start: Double; let lane, voice: Int }

let palette: [NSColor] = [
    NSColor(srgbRed: 1.0, green: 0.42, blue: 0.62, alpha: 1),   // pink
    NSColor(srgbRed: 0.35, green: 0.82, blue: 0.90, alpha: 1),  // teal
    NSColor(srgbRed: 0.98, green: 0.80, blue: 0.35, alpha: 1),  // gold
    NSColor(srgbRed: 0.55, green: 0.85, blue: 0.55, alpha: 1),  // green
]

func render(_ path: String) {
    guard let data = FileManager.default.contents(atPath: path),
          let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
          let voices = obj["voices"] as? [[String: Any]] else {
        FileHandle.standardError.write("skip (unreadable): \(path)\n".data(using: .utf8)!); return
    }
    let title = (obj["title"] as? String) ?? (path as NSString).lastPathComponent
    let bpm = (obj["bpm"] as? Int) ?? Int((obj["bpm"] as? Double) ?? 120)
    let machines = (obj["machines"] as? Int) ?? voices.count

    var notes: [Note] = []; var hits: [Hit] = []; var total = 0.0
    for (vi, voice) in voices.enumerated() {
        for (ti, key) in ["notes", "notes2", "notes3", "notes4"].enumerated() {
            guard let spec = voice[key] as? String else { continue }
            var t = 0.0
            for tok in spec.split(separator: ",") {
                let p = tok.split(separator: ":")
                guard p.count == 2, let beats = Double(p[1]) else { continue }
                if p[0] == "r" { t += beats; continue }
                if let lane = drumLane(p[0]) { hits.append(Hit(start: t, lane: lane, voice: vi)) }
                else if let m = Int(p[0]) { notes.append(Note(start: t, dur: beats, midi: m, voice: vi, track: ti)) }
                t += beats
            }
            total = max(total, t)
        }
    }
    if total <= 0 { total = 1 }

    let S: CGFloat = 512
    let img = NSImage(size: NSSize(width: S, height: S))
    img.lockFocus()
    // Card background with a subtle vertical gradient.
    let card = NSBezierPath(roundedRect: NSRect(x: 18, y: 18, width: S - 36, height: S - 36), xRadius: 72, yRadius: 72)
    NSGradient(starting: NSColor(srgbRed: 0.10, green: 0.11, blue: 0.16, alpha: 1),
               ending: NSColor(srgbRed: 0.05, green: 0.05, blue: 0.08, alpha: 1))?
        .draw(in: card, angle: -90)
    card.setClip()

    let drumStripH: CGFloat = hits.isEmpty ? 0 : 60
    let plot = NSRect(x: 54, y: 96 + drumStripH, width: S - 108, height: S - 96 - drumStripH - 120)
    let midis = notes.map { $0.midi }
    let minM = (midis.min() ?? 48) - 2, maxM = (midis.max() ?? 84) + 2
    let span = max(1, maxM - minM)
    func px(_ b: Double) -> CGFloat { plot.minX + CGFloat(b / total) * plot.width }
    func py(_ m: Int) -> CGFloat { plot.minY + CGFloat(Double(m - minM) / Double(span)) * plot.height }

    // Faint pitch gridlines every octave.
    NSColor(white: 1, alpha: 0.06).setStroke()
    var m = minM - (minM % 12)
    while m <= maxM { let gp = NSBezierPath(); gp.move(to: NSPoint(x: plot.minX, y: py(m))); gp.line(to: NSPoint(x: plot.maxX, y: py(m))); gp.lineWidth = 1; gp.stroke(); m += 12 }

    // Melodic notes as rounded bars, colored by voice, brighter for higher tracks.
    for n in notes {
        let x0 = px(n.start), x1 = px(n.start + n.dur)
        let r = NSRect(x: x0, y: py(n.midi) - 3.5, width: max(4, x1 - x0 - 1.5), height: 7)
        let base = palette[n.voice % palette.count]
        let shade = base.blended(withFraction: CGFloat(n.track) * 0.18, of: .white) ?? base
        shade.withAlphaComponent(0.92).setFill()
        NSBezierPath(roundedRect: r, xRadius: 3, yRadius: 3).fill()
    }

    // Drum hits in a strip beneath the roll, one row per kit piece.
    if !hits.isEmpty {
        let strip = NSRect(x: plot.minX, y: 84, width: plot.width, height: drumStripH - 14)
        for h in hits {
            let rowY = strip.minY + CGFloat(h.lane) * (strip.height / 7) + 3
            let d: CGFloat = h.lane == 0 ? 7 : 5   // kick a touch bigger
            let c = palette[h.voice % palette.count].blended(withFraction: 0.15, of: .white) ?? .white
            c.withAlphaComponent(0.9).setFill()
            NSBezierPath(ovalIn: NSRect(x: px(h.start) - d / 2, y: rowY, width: d, height: d)).fill()
        }
    }

    // Title (top) + meta (bottom).
    let pStyle = NSMutableParagraphStyle(); pStyle.alignment = .center; pStyle.lineBreakMode = .byTruncatingTail
    NSAttributedString(string: title, attributes: [
        .font: NSFont.systemFont(ofSize: 34, weight: .bold),
        .foregroundColor: NSColor.white, .paragraphStyle: pStyle,
    ]).draw(in: NSRect(x: 40, y: S - 92, width: S - 80, height: 44))
    let dots = String(repeating: "●", count: max(1, machines))
    NSAttributedString(string: "\(dots)  \(bpm) bpm  ·  \(Int(total)) beats", attributes: [
        .font: NSFont.monospacedSystemFont(ofSize: 20, weight: .medium),
        .foregroundColor: NSColor(white: 1, alpha: 0.55), .paragraphStyle: pStyle,
    ]).draw(in: NSRect(x: 40, y: 44, width: S - 80, height: 28))

    img.unlockFocus()
    if NSWorkspace.shared.setIcon(img, forFile: path, options: []) {
        print("iconed: \((path as NSString).lastPathComponent)  (\(notes.count) notes, \(hits.count) hits)")
    } else {
        FileHandle.standardError.write("setIcon FAILED: \(path)\n".data(using: .utf8)!)
    }
}

let files = Array(CommandLine.arguments.dropFirst())
if files.isEmpty { print("usage: swift mbscore-icon.swift <file.mbscore> …"); exit(1) }
for f in files { render(f) }
