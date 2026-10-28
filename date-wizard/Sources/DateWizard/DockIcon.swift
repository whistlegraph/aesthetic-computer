// DockIcon.swift — a flat calendar page drawn in code: the month on top,
// today's day number below, redrawn when the day or the system theme changes.
import AppKit

enum DockIcon {
    static func install() {
        apply()
        DistributedNotificationCenter.default().addObserver(
            forName: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil, queue: .main
        ) { _ in apply() }
        NotificationCenter.default.addObserver(
            forName: .NSCalendarDayChanged, object: nil, queue: .main
        ) { _ in apply() }
    }

    private static func apply() {
        let dark = NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        NSApp.applicationIconImage = render(Date(), dark: dark)
    }

    static func render(_ date: Date, dark: Bool) -> NSImage {
        let side: CGFloat = 512
        return NSImage(size: NSSize(width: side, height: side), flipped: false) { _ in
            // macOS icon grid: the tile sits inside a ~10% margin.
            let tile = NSRect(x: 50, y: 50, width: side - 100, height: side - 100)
            let page = dark ? NSColor(white: 0.13, alpha: 1) : NSColor.white
            let ink = dark ? NSColor(white: 0.95, alpha: 1) : NSColor(white: 0.1, alpha: 1)
            let shape = NSBezierPath(roundedRect: tile, xRadius: 92, yRadius: 92)
            page.setFill()
            shape.fill()
            NSColor(white: dark ? 1 : 0, alpha: 0.14).setStroke()
            shape.lineWidth = 4
            shape.stroke()

            let month = DateFormatter(); month.dateFormat = "MMM"
            let day = DateFormatter(); day.dateFormat = "d"
            let center = NSMutableParagraphStyle(); center.alignment = .center

            let monthAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 70, weight: .semibold),
                .foregroundColor: NSColor.systemRed,
                .paragraphStyle: center,
            ]
            (month.string(from: date).uppercased() as NSString).draw(
                in: NSRect(x: tile.minX, y: tile.maxY - 120, width: tile.width, height: 86),
                withAttributes: monthAttrs)

            let dayAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 210, weight: .light),
                .foregroundColor: ink,
                .paragraphStyle: center,
            ]
            (day.string(from: date) as NSString).draw(
                in: NSRect(x: tile.minX, y: tile.minY + 20, width: tile.width, height: 250),
                withAttributes: dayAttrs)
            return true
        }
    }
}
