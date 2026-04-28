import AppKit

// NSStatusBarButton draws a system click/hover pill behind its content.
// Setting `highlightsBy = []` doesn't suppress it because the pill is drawn
// by the cell's bezel routine, not the regular highlight state. Swapping in
// a cell that no-ops the bezel and refuses to enter the highlighted state
// removes the pill while leaving target/action and image drawing intact.
final class NoHighlightStatusBarCell: NSButtonCell {
    override var isHighlighted: Bool {
        get { false }
        set { /* swallow — never enter highlighted state */ }
    }

    override func drawBezel(withFrame frame: NSRect, in controlView: NSView) {
        // Intentionally empty: this is where the menubar pill is painted.
    }

    override func highlight(_ flag: Bool, withFrame cellFrame: NSRect, in controlView: NSView) {
        // Force the cell to redraw without highlight, regardless of input state.
        super.highlight(false, withFrame: cellFrame, in: controlView)
    }

    override func draw(withFrame cellFrame: NSRect, in controlView: NSView) {
        // Bypass the default cell draw (which lays down the bezel/pill first)
        // and just composite the image ourselves, centered in the button.
        guard let image = self.image else { return }
        let imgSize = image.size
        let x = cellFrame.minX + (cellFrame.width - imgSize.width) / 2.0
        let y = cellFrame.minY + (cellFrame.height - imgSize.height) / 2.0
        let target = NSRect(x: x, y: y, width: imgSize.width, height: imgSize.height)
        image.draw(in: target,
                   from: .zero,
                   operation: .sourceOver,
                   fraction: 1.0,
                   respectFlipped: true,
                   hints: nil)
    }
}
