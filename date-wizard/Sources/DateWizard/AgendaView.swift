// AgendaView.swift — the "no day selected" face of the wizard: an ordered,
// scrollable list of everything coming up (rolling window from now forward,
// see WizardController.upcomingDays). Where WeekView is a spatial hour grid,
// this is a chronological agenda — one section per day, each event a row of
//   TIME   ● Title            (source)
// sorted by start. Aesthetical events are editable (solid chip); feed events
// are read-only (dashed chip + source-color dot), matching WeekView's grammar.
//
// The view is a flipped document view inside an NSScrollView; it sizes its own
// height to the content so the scroller appears only when the list overflows.
import AppKit

protocol AgendaViewDelegate: AnyObject {
    func agendaView(_ view: AgendaView, didSelectEditable event: LaidEvent)
    func agendaView(_ view: AgendaView, didSelectReadOnly event: LaidEvent)
}

final class AgendaView: NSView {
    weak var delegate: AgendaViewDelegate?

    // Chronological events to show (already filtered to the upcoming window by
    // the controller). Re-laid on set.
    var events: [LaidEvent] = [] {
        didSet { relayout() }
    }

    // ── layout metrics ────────────────────────────────────────────────
    private let topPad: CGFloat = 8
    private let sectionH: CGFloat = 28      // day header row
    private let rowH: CGFloat = 30
    private let sectionGap: CGFloat = 4
    private let sideInset: CGFloat = 10
    private let timeColW: CGFloat = 78      // right edge of the time column

    override var isFlipped: Bool { true }   // y grows downward — top-to-bottom list.

    // One rendered row: either a day header or an event.
    private enum Item {
        case header(date: Date)
        case event(LaidEvent)
    }
    private var items: [Item] = []

    private struct HitBox { var rect: NSRect; var event: LaidEvent }
    private var hitBoxes: [HitBox] = []

    // Scrim opacity over the living backdrop, matching WeekView so the two read
    // as one surface.
    private let scrimAlpha: CGFloat = 0.24

    // ── build the item list (headers + rows) ──────────────────────────
    private func relayout() {
        let cal = Calendar.current
        let sorted = events.sorted { $0.start < $1.start }
        var built: [Item] = []
        var lastDay: Date?
        for ev in sorted {
            let day = cal.startOfDay(for: ev.start)
            if lastDay == nil || !cal.isDate(day, inSameDayAs: lastDay!) {
                built.append(.header(date: day))
                lastDay = day
            }
            built.append(.event(ev))
        }
        items = built

        // Size the document view to fit; the enclosing scroll view reveals a
        // scroller when this exceeds the visible height.
        let contentH = topPad + items.reduce(0) { acc, item in
            switch item {
            case .header: return acc + sectionH + sectionGap
            case .event:  return acc + rowH
            }
        } + topPad
        let width = enclosingScrollView?.contentSize.width ?? bounds.width
        setFrameSize(NSSize(width: width, height: max(contentH, enclosingScrollView?.contentSize.height ?? contentH)))
        needsDisplay = true
    }

    // Keep our width locked to the scroll view's content width on resize so the
    // rows always span the full pane.
    override func viewDidMoveToSuperview() {
        super.viewDidMoveToSuperview()
        relayout()
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        needsDisplay = true
    }

    // ── drawing ───────────────────────────────────────────────────────
    override func draw(_ dirtyRect: NSRect) {
        NSColor.windowBackgroundColor.withAlphaComponent(scrimAlpha).setFill()
        bounds.fill()

        hitBoxes.removeAll()

        if items.isEmpty {
            drawEmptyState()
            return
        }

        let cal = Calendar.current
        var y = topPad
        for item in items {
            switch item {
            case .header(let date):
                drawHeader(date, atY: y)
                y += sectionH + sectionGap
            case .event(let ev):
                let rect = NSRect(x: sideInset, y: y + 2,
                                  width: bounds.width - sideInset * 2, height: rowH - 4)
                drawRow(ev, in: rect, cal: cal)
                hitBoxes.append(HitBox(rect: rect, event: ev))
                y += rowH
            }
        }
    }

    private func drawEmptyState() {
        let style = NSMutableParagraphStyle(); style.alignment = .center
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.white.withAlphaComponent(0.6)
        shadow.shadowBlurRadius = 1.5
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 13, weight: .medium),
            .foregroundColor: NSColor.secondaryLabelColor,
            .paragraphStyle: style,
            .shadow: shadow,
        ]
        ("Nothing coming up." as NSString).draw(
            in: NSRect(x: 0, y: 40, width: bounds.width, height: 20), withAttributes: attrs)
    }

    private func drawHeader(_ date: Date, atY y: CGFloat) {
        let cal = Calendar.current
        let color = DayPalette.color(for: date)

        // A relative word ("Today"/"Tomorrow") plus the date, colored in the
        // day's ROYGBIV hue so the agenda echoes the strip.
        let dayDelta = cal.dateComponents([.day],
            from: cal.startOfDay(for: Date()), to: cal.startOfDay(for: date)).day ?? 0
        let fmt = DateFormatter(); fmt.dateFormat = "EEEE, MMM d"
        let prefix: String
        switch dayDelta {
        case 0: prefix = "Today · "
        case 1: prefix = "Tomorrow · "
        default: prefix = ""
        }
        let text = "\(prefix)\(fmt.string(from: date))"

        // A small day-color dot, then the label.
        let dotD: CGFloat = 8
        let dotRect = NSRect(x: sideInset, y: y + (sectionH - dotD) / 2 + 1, width: dotD, height: dotD)
        color.withAlphaComponent(0.9).setFill()
        NSBezierPath(ovalIn: dotRect).fill()

        let shadow = NSShadow()
        shadow.shadowColor = NSColor.white.withAlphaComponent(0.5)
        shadow.shadowBlurRadius = 1.5
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 12, weight: .heavy),
            .foregroundColor: color,
            .shadow: shadow,
        ]
        (text as NSString).draw(at: NSPoint(x: dotRect.maxX + 7, y: y + (sectionH - 15) / 2),
                                withAttributes: attrs)

        // A hairline under the header.
        NSColor.separatorColor.withAlphaComponent(0.4).setStroke()
        let line = NSBezierPath()
        line.move(to: NSPoint(x: sideInset, y: y + sectionH))
        line.line(to: NSPoint(x: bounds.width - sideInset, y: y + sectionH))
        line.lineWidth = 0.5
        line.stroke()
    }

    private func drawRow(_ ev: LaidEvent, in rect: NSRect, cal: Calendar) {
        // Time column (left): "all-day", or a compact start time.
        let timeStr: String
        if ev.allDay {
            timeStr = "all-day"
        } else {
            let tf = DateFormatter(); tf.dateFormat = "h:mm a"
            timeStr = tf.string(from: ev.start).lowercased()
        }
        let timeStyle = NSMutableParagraphStyle(); timeStyle.alignment = .right
        let timeShadow = NSShadow()
        timeShadow.shadowColor = NSColor.white.withAlphaComponent(0.5)
        timeShadow.shadowBlurRadius = 1.0
        let timeAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 10, weight: .medium),
            .foregroundColor: NSColor.labelColor,
            .paragraphStyle: timeStyle,
            .shadow: timeShadow,
        ]
        (timeStr as NSString).draw(
            in: NSRect(x: rect.minX, y: rect.midY - 7, width: timeColW - 8, height: 14),
            withAttributes: timeAttrs)

        // Title chip (right of the time column).
        let chip = NSRect(x: rect.minX + timeColW, y: rect.minY,
                          width: rect.maxX - (rect.minX + timeColW), height: rect.height)
        let path = NSBezierPath(roundedRect: chip, xRadius: 5, yRadius: 5)
        let labelColor: NSColor
        var labelInsetX: CGFloat = 9

        if ev.kind == .aesthetical {
            let fill = (ev.visibility == "public") ? NSColor.systemGreen : NSColor.controlAccentColor
            fill.withAlphaComponent(0.85).setFill()
            path.fill()
            labelColor = .white
        } else {
            let c = ev.color ?? NSColor.systemGray
            c.withAlphaComponent(0.12).setFill()
            path.fill()
            c.setStroke()
            path.lineWidth = 1
            path.setLineDash([3, 2], count: 2, phase: 0)
            path.stroke()
            // Source dot.
            let dot = NSBezierPath(ovalIn: NSRect(x: chip.minX + 7, y: chip.midY - 3, width: 6, height: 6))
            c.setFill(); dot.fill()
            labelColor = .labelColor
            labelInsetX = 20
        }

        // Title.
        let tstyle = NSMutableParagraphStyle(); tstyle.lineBreakMode = .byTruncatingTail
        let tAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 11, weight: .medium),
            .foregroundColor: labelColor,
            .paragraphStyle: tstyle,
        ]
        // Reserve room on the right for a source label on feed rows.
        var titleW = chip.width - labelInsetX - 8
        if let src = ev.sourceLabel {
            let sAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 9, weight: .regular),
                .foregroundColor: NSColor.secondaryLabelColor,
            ]
            let sSize = (src as NSString).size(withAttributes: sAttrs)
            let sX = chip.maxX - sSize.width - 8
            (src as NSString).draw(at: NSPoint(x: sX, y: chip.midY - sSize.height / 2), withAttributes: sAttrs)
            titleW = max(20, sX - (chip.minX + labelInsetX) - 8)
        }
        (ev.title as NSString).draw(
            in: NSRect(x: chip.minX + labelInsetX, y: chip.midY - 8, width: titleW, height: 16),
            withAttributes: tAttrs)
    }

    // ── interaction ───────────────────────────────────────────────────
    override func mouseDown(with event: NSEvent) {
        let p = convert(event.locationInWindow, from: nil)
        for box in hitBoxes where box.rect.contains(p) {
            if box.event.kind == .aesthetical {
                delegate?.agendaView(self, didSelectEditable: box.event)
            } else {
                delegate?.agendaView(self, didSelectReadOnly: box.event)
            }
            return
        }
    }
}
