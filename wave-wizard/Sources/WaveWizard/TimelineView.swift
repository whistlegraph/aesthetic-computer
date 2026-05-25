import AppKit

// Horizontal timeline strip of sample states — pending / current / done /
// skipped. Drawn programmatically: a thin track line with circular nodes,
// the current one highlighted, done ones a checkmark, skipped ones a ✕.
final class TimelineView: NSView {
    enum SampleState { case pending, current, done, skipped }
    var sampleNames: [String] = []
    var states: [SampleState] = []

    func update(names: [String], currentIndex: Int, done: Set<Int>, skipped: Set<Int>) {
        sampleNames = names
        var st = [SampleState](repeating: .pending, count: names.count)
        for i in 0..<names.count {
            if done.contains(i)         { st[i] = .done }
            else if skipped.contains(i) { st[i] = .skipped }
            else if i == currentIndex   { st[i] = .current }
        }
        states = st
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let n = sampleNames.count
        guard n > 0 else { return }
        let cellW = bounds.width / CGFloat(n)
        let trackY = bounds.height * 0.62

        // baseline track
        NSColor.tertiaryLabelColor.setStroke()
        let track = NSBezierPath()
        track.move(to: NSPoint(x: cellW * 0.5, y: trackY))
        track.line(to: NSPoint(x: bounds.width - cellW * 0.5, y: trackY))
        track.lineWidth = 1.5
        track.stroke()

        for (i, st) in states.enumerated() {
            let x = cellW * (CGFloat(i) + 0.5)
            let radius: CGFloat = st == .current ? 10 : 7
            let dot = NSBezierPath(ovalIn: NSRect(x: x - radius, y: trackY - radius, width: radius * 2, height: radius * 2))
            switch st {
            case .pending: NSColor.tertiaryLabelColor.setFill()
            case .current: NSColor.systemYellow.setFill()
            case .done:    NSColor.systemGreen.setFill()
            case .skipped: NSColor.systemRed.withAlphaComponent(0.7).setFill()
            }
            dot.fill()

            if st == .current {
                NSColor.systemYellow.withAlphaComponent(0.25).setFill()
                NSBezierPath(ovalIn: NSRect(x: x - radius - 5, y: trackY - radius - 5, width: (radius + 5) * 2, height: (radius + 5) * 2)).fill()
                dot.fill()
            }
            if st == .done {
                NSColor.white.setStroke()
                let check = NSBezierPath()
                check.move(to: NSPoint(x: x - 3.5, y: trackY))
                check.line(to: NSPoint(x: x - 0.5, y: trackY - 3.5))
                check.line(to: NSPoint(x: x + 4, y: trackY + 3.5))
                check.lineWidth = 2
                check.lineCapStyle = .round
                check.lineJoinStyle = .round
                check.stroke()
            }
            if st == .skipped {
                NSColor.white.setStroke()
                let cross = NSBezierPath()
                cross.move(to: NSPoint(x: x - 3, y: trackY - 3))
                cross.line(to: NSPoint(x: x + 3, y: trackY + 3))
                cross.move(to: NSPoint(x: x - 3, y: trackY + 3))
                cross.line(to: NSPoint(x: x + 3, y: trackY - 3))
                cross.lineWidth = 2
                cross.lineCapStyle = .round
                cross.stroke()
            }

            // short label below — last segment after last hyphen
            let short = String(sampleNames[i].split(separator: "-").last ?? Substring(sampleNames[i]))
            let style = NSMutableParagraphStyle(); style.alignment = .center
            let color: NSColor = st == .current ? .labelColor : .secondaryLabelColor
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 10, weight: st == .current ? .semibold : .regular),
                .foregroundColor: color,
                .paragraphStyle: style,
            ]
            (short as NSString).draw(in: NSRect(x: cellW * CGFloat(i), y: 2, width: cellW, height: 14), withAttributes: attrs)
        }
    }
}
