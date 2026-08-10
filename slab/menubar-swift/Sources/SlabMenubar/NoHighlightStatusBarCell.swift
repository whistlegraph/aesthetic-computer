import AppKit

/// Status-item cell that draws only its image. AppKit's ordinary cell paints
/// the rounded hover/pressed pill even when `highlightsBy` is empty.
final class NoHighlightStatusBarCell: NSButtonCell {
    override var isHighlighted: Bool {
        get { false }
        set { }
    }

    override func drawBezel(withFrame frame: NSRect, in controlView: NSView) { }

    override func highlight(_ flag: Bool, withFrame cellFrame: NSRect,
                            in controlView: NSView) {
        super.highlight(false, withFrame: cellFrame, in: controlView)
    }

    override func draw(withFrame cellFrame: NSRect, in controlView: NSView) {
        guard let image else { return }
        let target = NSRect(
            x: cellFrame.midX - image.size.width / 2,
            y: cellFrame.midY - image.size.height / 2,
            width: image.size.width, height: image.size.height)
        image.draw(in: target, from: .zero, operation: .sourceOver,
                   fraction: 1, respectFlipped: true, hints: nil)
    }
}
