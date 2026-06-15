// WeekView.swift — custom-drawn NSView showing one week (Sun→Sat) with
// hour rows (6:00–24:00). Today's column is highlighted and a "now" line
// is drawn. Each event renders as a rounded box positioned by start/end.
//
//   AESTHETICAL events : solid fill, editable (click → edit sheet).
//   GOOGLE/feed events : outline + a source-color dot, read-only
//                        (click shows read-only details, NO edit).
//
// A small legend of source colors sits along the bottom.
import AppKit

// A unified, laid-out event the view can draw regardless of source.
struct LaidEvent {
    enum Kind { case aesthetical, feed }
    var kind: Kind
    var uid: String
    var title: String
    var start: Date
    var end: Date
    var allDay: Bool
    var note: String
    var visibility: String       // aesthetical only
    var sourceLabel: String?     // feed only
    var color: NSColor?          // feed source color
}

protocol WeekViewDelegate: AnyObject {
    // Clicking an editable (aesthetical) event opens the editor.
    func weekView(_ view: WeekView, didSelectEditable event: LaidEvent)
    // Clicking a read-only (feed) event shows details.
    func weekView(_ view: WeekView, didSelectReadOnly event: LaidEvent)
    // Clicking an empty slot proposes a new event at that day/hour.
    func weekView(_ view: WeekView, didRequestNewEventAt day: Date, hour: Int)
}

final class WeekView: NSView {
    weak var delegate: WeekViewDelegate?

    // The Sunday that begins the displayed week (local midnight).
    var weekStart: Date = WeekView.startOfWeek(for: Date()) {
        didSet { needsDisplay = true }
    }
    var events: [LaidEvent] = [] { didSet { needsDisplay = true } }

    private let startHour = 6
    private let endHour = 24
    private let headerH: CGFloat = 30
    private let legendH: CGFloat = 26
    private let gutterW: CGFloat = 44

    // Hit-test boxes recomputed each draw.
    private struct HitBox { var rect: NSRect; var event: LaidEvent }
    private var hitBoxes: [HitBox] = []

    override var isFlipped: Bool { true }   // y grows downward — natural for a day grid.

    // ── geometry helpers ─────────────────────────────────────────────

    static func startOfWeek(for date: Date) -> Date {
        var cal = Calendar.current
        cal.firstWeekday = 1 // Sunday
        let comps = cal.dateComponents([.yearForWeekOfYear, .weekOfYear], from: date)
        return cal.date(from: comps) ?? date
    }

    private var hourCount: Int { endHour - startHour }

    private var gridRect: NSRect {
        NSRect(x: gutterW, y: headerH,
               width: bounds.width - gutterW,
               height: bounds.height - headerH - legendH)
    }

    private func columnWidth() -> CGFloat { gridRect.width / 7.0 }
    private func rowHeight() -> CGFloat { gridRect.height / CGFloat(hourCount) }

    // Day index 0..6 for a date relative to weekStart (-1 if outside).
    private func dayIndex(for date: Date) -> Int {
        let cal = Calendar.current
        let days = cal.dateComponents([.day], from: weekStart, to: date).day ?? -1
        return (days >= 0 && days < 7) ? days : -1
    }

    // Fractional hours-from-startHour for a date (clamped to the visible band).
    private func fractionalHour(for date: Date) -> CGFloat {
        let cal = Calendar.current
        let h = cal.component(.hour, from: date)
        let m = cal.component(.minute, from: date)
        let v = CGFloat(h) + CGFloat(m) / 60.0 - CGFloat(startHour)
        return max(0, min(CGFloat(hourCount), v))
    }

    private func dayDate(_ idx: Int) -> Date {
        Calendar.current.date(byAdding: .day, value: idx, to: weekStart) ?? weekStart
    }

    // ── drawing ──────────────────────────────────────────────────────

    override func draw(_ dirtyRect: NSRect) {
        let bg = NSColor.windowBackgroundColor
        bg.setFill()
        bounds.fill()

        drawHeaderAndGrid()
        drawEvents()
        drawNowLine()
        drawLegend()
    }

    private func drawHeaderAndGrid() {
        let grid = gridRect
        let colW = columnWidth()
        let rowH = rowHeight()
        let cal = Calendar.current
        let today = dayIndex(for: Date())

        let dayNames = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

        // Today's column highlight.
        if today >= 0 {
            NSColor.controlAccentColor.withAlphaComponent(0.08).setFill()
            NSRect(x: grid.minX + CGFloat(today) * colW, y: 0,
                   width: colW, height: bounds.height - legendH).fill()
        }

        // Hour grid lines + labels.
        NSColor.separatorColor.withAlphaComponent(0.5).setStroke()
        for r in 0...hourCount {
            let y = grid.minY + CGFloat(r) * rowH
            let line = NSBezierPath()
            line.move(to: NSPoint(x: grid.minX, y: y))
            line.line(to: NSPoint(x: grid.maxX, y: y))
            line.lineWidth = 0.5
            line.stroke()
            if r < hourCount {
                let hour = startHour + r
                let label = String(format: "%02d:00", hour)
                let attrs: [NSAttributedString.Key: Any] = [
                    .font: NSFont.monospacedSystemFont(ofSize: 9, weight: .regular),
                    .foregroundColor: NSColor.secondaryLabelColor,
                ]
                (label as NSString).draw(at: NSPoint(x: 4, y: y + 2), withAttributes: attrs)
            }
        }

        // Day columns + header labels.
        for d in 0...7 {
            let x = grid.minX + CGFloat(d) * colW
            let line = NSBezierPath()
            line.move(to: NSPoint(x: x, y: headerH))
            line.line(to: NSPoint(x: x, y: grid.maxY))
            line.lineWidth = 0.5
            line.stroke()
        }
        for d in 0..<7 {
            let date = dayDate(d)
            let dayNum = cal.component(.day, from: date)
            let isToday = (d == today)
            let header = "\(dayNames[d]) \(dayNum)"
            let style = NSMutableParagraphStyle(); style.alignment = .center
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 11, weight: isToday ? .bold : .medium),
                .foregroundColor: isToday ? NSColor.controlAccentColor : NSColor.labelColor,
                .paragraphStyle: style,
            ]
            (header as NSString).draw(
                in: NSRect(x: grid.minX + CGFloat(d) * colW, y: 8, width: colW, height: 16),
                withAttributes: attrs)
        }
    }

    private func drawEvents() {
        hitBoxes.removeAll()
        let grid = gridRect
        let colW = columnWidth()
        let rowH = rowHeight()
        let inset: CGFloat = 2

        // Stable ordering: aesthetical drawn last so they sit on top.
        let ordered = events.sorted { a, b in
            if a.kind == b.kind { return a.start < b.start }
            return a.kind == .feed   // feed first → aesthetical on top
        }

        for ev in ordered {
            let d = dayIndex(for: ev.start)
            guard d >= 0 else { continue }

            let yTop: CGFloat
            let yBot: CGFloat
            if ev.allDay {
                // All-day events render as a slim band at the very top of the column.
                yTop = grid.minY
                yBot = grid.minY + 14
            } else {
                yTop = grid.minY + fractionalHour(for: ev.start) * rowH
                let endFrac = max(fractionalHour(for: ev.end),
                                  fractionalHour(for: ev.start) + 0.4) // min visible height
                yBot = grid.minY + endFrac * rowH
            }
            let rect = NSRect(x: grid.minX + CGFloat(d) * colW + inset,
                              y: yTop + inset,
                              width: colW - inset * 2,
                              height: max(16, yBot - yTop - inset * 2))
            let path = NSBezierPath(roundedRect: rect, xRadius: 4, yRadius: 4)

            if ev.kind == .aesthetical {
                // Solid fill — editable.
                let fill = (ev.visibility == "public")
                    ? NSColor.systemGreen : NSColor.controlAccentColor
                fill.withAlphaComponent(0.85).setFill()
                path.fill()
                fill.setStroke()
                path.lineWidth = 1
                path.stroke()
                drawEventLabel(ev.title, in: rect, color: .white)
            } else {
                // Outline + hatch + source-color dot — read-only.
                let c = ev.color ?? NSColor.systemGray
                c.withAlphaComponent(0.10).setFill()
                path.fill()
                c.setStroke()
                path.lineWidth = 1
                path.setLineDash([3, 2], count: 2, phase: 0)
                path.stroke()
                // Source dot, top-left inside the box.
                let dot = NSBezierPath(ovalIn: NSRect(x: rect.minX + 4, y: rect.minY + 4, width: 6, height: 6))
                c.setFill(); dot.fill()
                drawEventLabel(ev.title, in: rect.insetBy(dx: 12, dy: 0).offsetBy(dx: 0, dy: 0),
                               color: .labelColor)
            }
            hitBoxes.append(HitBox(rect: rect, event: ev))
        }
    }

    private func drawEventLabel(_ title: String, in rect: NSRect, color: NSColor) {
        let style = NSMutableParagraphStyle()
        style.lineBreakMode = .byTruncatingTail
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 10, weight: .medium),
            .foregroundColor: color,
            .paragraphStyle: style,
        ]
        (title as NSString).draw(
            in: NSRect(x: rect.minX + 4, y: rect.minY + 2,
                       width: rect.width - 6, height: min(rect.height - 2, 28)),
            withAttributes: attrs)
    }

    private func drawNowLine() {
        let now = Date()
        let d = dayIndex(for: now)
        guard d >= 0 else { return }
        let grid = gridRect
        let colW = columnWidth()
        let rowH = rowHeight()
        let y = grid.minY + fractionalHour(for: now) * rowH
        let x0 = grid.minX + CGFloat(d) * colW
        NSColor.systemRed.setStroke()
        let line = NSBezierPath()
        line.move(to: NSPoint(x: x0, y: y))
        line.line(to: NSPoint(x: x0 + colW, y: y))
        line.lineWidth = 1.5
        line.stroke()
        let knob = NSBezierPath(ovalIn: NSRect(x: x0 - 2, y: y - 2, width: 5, height: 5))
        NSColor.systemRed.setFill(); knob.fill()
    }

    private func drawLegend() {
        let y = bounds.height - legendH + 6
        var x: CGFloat = gutterW
        func chip(_ label: String, _ color: NSColor, dashed: Bool) {
            let box = NSRect(x: x, y: y, width: 14, height: 12)
            let path = NSBezierPath(roundedRect: box, xRadius: 2, yRadius: 2)
            if dashed {
                color.withAlphaComponent(0.15).setFill(); path.fill()
                color.setStroke(); path.lineWidth = 1
                path.setLineDash([3, 2], count: 2, phase: 0); path.stroke()
            } else {
                color.withAlphaComponent(0.85).setFill(); path.fill()
            }
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 9),
                .foregroundColor: NSColor.secondaryLabelColor,
            ]
            (label as NSString).draw(at: NSPoint(x: x + 18, y: y), withAttributes: attrs)
            x += 18 + (label as NSString).size(withAttributes: attrs).width + 14
        }
        chip("aesthetical", NSColor.controlAccentColor, dashed: false)
        chip("public", NSColor.systemGreen, dashed: false)
        // One chip per distinct feed source color present this week.
        var seen = Set<String>()
        for ev in events where ev.kind == .feed {
            let label = ev.sourceLabel ?? "calendar"
            if seen.contains(label) { continue }
            seen.insert(label)
            chip(label, ev.color ?? NSColor.systemGray, dashed: true)
        }
    }

    // ── interaction ──────────────────────────────────────────────────

    override func mouseDown(with event: NSEvent) {
        let p = convert(event.locationInWindow, from: nil)
        // Topmost box wins — hitBoxes are in draw order (feed first), so
        // iterate in reverse to prefer aesthetical events on top.
        for box in hitBoxes.reversed() where box.rect.contains(p) {
            if box.event.kind == .aesthetical {
                delegate?.weekView(self, didSelectEditable: box.event)
            } else {
                delegate?.weekView(self, didSelectReadOnly: box.event)
            }
            return
        }
        // Empty slot → propose a new event at that day/hour.
        let grid = gridRect
        guard grid.contains(p) else { return }
        let colW = columnWidth()
        let rowH = rowHeight()
        let d = Int((p.x - grid.minX) / colW)
        let hourOffset = Int((p.y - grid.minY) / rowH)
        guard d >= 0 && d < 7 else { return }
        let day = dayDate(d)
        delegate?.weekView(self, didRequestNewEventAt: day, hour: startHour + hourOffset)
    }
}
