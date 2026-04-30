import AppKit

/// Programmatically draws the canonical Ableton logo (the same SVG
/// glyph the kidlisp.com editor uses for its Ableton boot screen):
/// four 3×24 vertical bars on the left, four 24×3 horizontal bars
/// on the right, total 51×24. Returns it as a template-rendered
/// NSImage so AppKit tints it with the surrounding label color.
enum AbletonLogo {
    /// Build a fresh NSImage at the requested height, preserving the
    /// 51:24 aspect ratio. Used for the popover's "Layout" mode
    /// button.
    static func image(height: CGFloat) -> NSImage {
        let aspect: CGFloat = 51.0 / 24.0
        let size = NSSize(width: height * aspect, height: height)
        let img = NSImage(size: size)
        img.lockFocusFlipped(true)
        NSColor.labelColor.setFill()
        let scale = height / 24.0
        // Vertical bars: 3 wide, 24 tall, at x = 0, 6, 12, 18.
        for x in stride(from: 0.0, through: 18.0, by: 6.0) {
            NSRect(x: x * scale, y: 0,
                    width: 3 * scale, height: 24 * scale).fill()
        }
        // Horizontal bars: 24 wide, 3 tall, at y = 0, 7, 14, 21,
        // anchored to x = 27. lockFocusFlipped(true) means y=0 is
        // the TOP of the image — same orientation as the SVG.
        for y in stride(from: 0.0, through: 21.0, by: 7.0) {
            NSRect(x: 27 * scale, y: y * scale,
                    width: 24 * scale, height: 3 * scale).fill()
        }
        img.unlockFocus()
        img.isTemplate = true
        return img
    }
}
