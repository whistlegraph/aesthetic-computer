// DayDotsView.swift — the seven-day ROYGBIV dot strip inside the wizard
// window, a sibling of the menu bar strip (both draw via DayStrip). The
// focused day is lit; clicking a dot jumps the wizard to that day. Hovering
// previews the dot under the cursor.
import AppKit

final class DayDotsView: NSView {
    /// Fires with the day index (0 = Sun … 6 = Sat) the user clicked.
    var onSelect: ((Int) -> Void)?

    /// The day the wizard is focused on (lit + ringed).
    var selectedIndex: Int? { didSet { if selectedIndex != oldValue { needsDisplay = true } } }
    /// Today's index (faint marker when it isn't the focused day).
    var todayIndex: Int? { didSet { if todayIndex != oldValue { needsDisplay = true } } }

    private var hoveredIndex: Int? { didSet { if hoveredIndex != oldValue { needsDisplay = true } } }
    private var tracking: NSTrackingArea?

    override var isFlipped: Bool { false }   // y-up, matching DayStrip's math.

    override func draw(_ dirtyRect: NSRect) {
        DayStrip.draw(in: bounds, selected: selectedIndex,
                      today: todayIndex, hovered: hoveredIndex)
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let tracking { removeTrackingArea(tracking) }
        let area = NSTrackingArea(rect: bounds,
                                  options: [.mouseMoved, .mouseEnteredAndExited, .activeInActiveApp],
                                  owner: self, userInfo: nil)
        addTrackingArea(area)
        tracking = area
    }

    override func mouseMoved(with event: NSEvent) {
        hoveredIndex = DayStrip.index(atX: convert(event.locationInWindow, from: nil).x, in: bounds)
    }
    override func mouseExited(with event: NSEvent) { hoveredIndex = nil }

    override func mouseDown(with event: NSEvent) {
        let idx = DayStrip.index(atX: convert(event.locationInWindow, from: nil).x, in: bounds)
        onSelect?(idx)
    }
}
