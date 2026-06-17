// DayGlowView.swift — a non-interactive inner radial glow that frames the
// window in the focused day's color, the way the deskflow fullscreen
// highlight hugs the screen edges. Clear through the center (content stays
// readable), the day color easing in toward the borders. Sits on top of
// everything but passes all clicks straight through.
import AppKit

final class DayGlowView: NSView {
    var color: NSColor? { didSet { needsDisplay = true } }

    override var isFlipped: Bool { false }
    override func hitTest(_ point: NSPoint) -> NSView? { nil }   // click-through

    override func draw(_ dirtyRect: NSRect) {
        guard let color else { return }
        // Clear in the middle, color toward the edges — an inner vignette.
        let g = NSGradient(colorsAndLocations:
            (color.withAlphaComponent(0.0), 0.58),
            (color.withAlphaComponent(0.12), 0.86),
            (color.withAlphaComponent(0.34), 1.0))
        let center = NSPoint(x: bounds.midX, y: bounds.midY)
        let radius = hypot(bounds.width, bounds.height) / 2
        g?.draw(fromCenter: center, radius: 0, toCenter: center, radius: radius, options: [])
    }
}
