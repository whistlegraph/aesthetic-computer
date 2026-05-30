import AppKit

// 4-light countin indicator: red → orange → yellow → green "GO".
// Wires to a beat-driven controller that calls `light(at:)` once per
// click; draws all four dots inline but darkens the inactive ones so
// the active one pops.
final class TrafficLightView: NSView {
    static let colors: [NSColor] = [
        .systemRed, .systemOrange, .systemYellow, .systemGreen,
    ]
    var activeIdx: Int = -1            // -1 = none lit (idle)

    func light(at idx: Int) {
        activeIdx = idx
        needsDisplay = true
    }

    func clear() {
        activeIdx = -1
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let count = Self.colors.count
        let dotSize = min(bounds.height, bounds.width / CGFloat(count) - 4)
        let gap: CGFloat = (bounds.width - dotSize * CGFloat(count)) / CGFloat(count + 1)
        let y = (bounds.height - dotSize) * 0.5
        for i in 0..<count {
            let x = gap + CGFloat(i) * (dotSize + gap)
            let rect = NSRect(x: x, y: y, width: dotSize, height: dotSize)
            let path = NSBezierPath(ovalIn: rect)
            let isActive = (i == activeIdx)
            let isPast   = (activeIdx >= 0 && i < activeIdx)
            let baseColor = Self.colors[i]
            let fill: NSColor
            if isActive {
                fill = baseColor
            } else if isPast {
                fill = baseColor.withAlphaComponent(0.45)
            } else {
                fill = baseColor.withAlphaComponent(0.18)
            }
            fill.setFill()
            path.fill()
            NSColor.tertiaryLabelColor.withAlphaComponent(0.5).setStroke()
            path.lineWidth = 1
            path.stroke()
            // bright outline on the active dot
            if isActive {
                NSColor.white.withAlphaComponent(0.85).setStroke()
                let glow = NSBezierPath(ovalIn: rect.insetBy(dx: -2, dy: -2))
                glow.lineWidth = 1.5
                glow.stroke()
            }
        }
    }
}
