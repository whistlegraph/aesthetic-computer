import AppKit

/// The wooden frame around the piece preview card: the Aesel desktop's
/// `AeselWoodFrame` (itself the Electron spriteland.js tile, shared with the
/// whistlegraph editor's MIME card), drawn here as a band along the card's
/// edge. It is a view rather than a layer so it rides the card's frame through
/// the open and close animation without a second set of geometry, and it takes
/// no clicks — the piece underneath keeps them.
final class PromptPreviewWoodFrame: NSView {
    /// The Aesel desktop pads its preview four points inside the wood.
    static let thickness: CGFloat = 4

    /// The 384×96 three-board tile, drawn once at sprite resolution: boards,
    /// grain ribbons, knots, and the seams between planks.
    private static let tile: NSImage = NSImage(size: NSSize(width: 384, height: 96), flipped: true) { _ in
        guard let context = NSGraphicsContext.current?.cgContext else { return false }
        func fill(_ hex: UInt32, _ x: Int, _ y: Int, _ width: Int, _ height: Int, alpha: CGFloat = 1) {
            context.setFillColor(CGColor(red: CGFloat((hex >> 16) & 255) / 255,
                                         green: CGFloat((hex >> 8) & 255) / 255,
                                         blue: CGFloat(hex & 255) / 255, alpha: alpha))
            context.fill(CGRect(x: x, y: y, width: width, height: height))
        }
        func hash(_ x: Int, _ y: Int) -> Int { Int(UInt32(truncatingIfNeeded: x * 374761393 + y * 668265263) % 997) }
        let palette: [UInt32] = [0x453126, 0x50382a, 0x59402e]
        let ribbons: [(UInt32, CGFloat)] = [(0x2f241a, 85 / 255), (0xac7e45, 40 / 255), (0x1f1816, 56 / 255)]
        for board in 0..<3 {
            let top = board * 32
            fill(palette[board], 0, top, 384, 32)
            for row in 0..<7 {
                let (color, alpha) = ribbons[row % 3]
                for x in stride(from: 0, to: 384, by: 4) {
                    // JavaScript Math.round rounds ties toward positive infinity.
                    let wave = Int(floor(sin(Double(x + board * 61 + row * 19) / 42) * 2 + 0.5)) * 2
                    fill(color, x, top + 3 + row * 4 + wave, 4, row % 3 == 0 ? 2 : 1, alpha: alpha)
                }
            }
            for knot in 0..<3 {
                let x = 42 + knot * 128 + hash(board, knot) % 25
                let y = top + 14 + hash(knot, board) % 7
                for radius in stride(from: 12, through: 3, by: -3) {
                    for dy in -(radius / 3)...(radius / 3) {
                        let width = max(2, radius - abs(dy) * 2)
                        fill(radius % 2 == 1 ? 0x35271f : 0x795437, x - width, y + dy, width * 2, 1)
                    }
                }
            }
            fill(0x241e18, 0, top + 31, 384, 1)
            for x in stride(from: 64 + board * 80, to: 384, by: 192) {
                fill(0x2a201c, x, top, 1, 32)
                fill(0xb38b49, x + 1, top + 1, 1, 30, alpha: 48 / 255)
            }
        }
        return true
    }

    override var isFlipped: Bool { true }
    override func hitTest(_ point: NSPoint) -> NSView? { nil }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setAccessibilityElement(false)
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

    override func draw(_ dirtyRect: NSRect) {
        let t = Self.thickness
        guard bounds.width > t * 2, bounds.height > t * 2 else { return }
        let inner = bounds.insetBy(dx: t, dy: t)
        let band = NSBezierPath(rect: bounds)
        band.append(NSBezierPath(rect: inner))
        band.windingRule = .evenOdd
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current?.patternPhase = convert(NSPoint.zero, to: nil)
        NSColor(patternImage: Self.tile).setFill()
        band.fill()
        NSGraphicsContext.restoreGraphicsState()

        // The desktop's bevel: lit along the top and left, shaded along the
        // bottom and right — on the outside of the wood, and reversed where
        // the wood meets the art, so the picture sits down inside the frame.
        func line(_ points: [NSPoint], _ hex: UInt32) {
            let path = NSBezierPath()
            path.move(to: points[0])
            for point in points.dropFirst() { path.line(to: point) }
            path.lineWidth = 1
            NSColor(srgbRed: CGFloat((hex >> 16) & 255) / 255, green: CGFloat((hex >> 8) & 255) / 255,
                    blue: CGFloat(hex & 255) / 255, alpha: 1).setStroke()
            path.stroke()
        }
        let o = bounds.insetBy(dx: 0.5, dy: 0.5), i = inner.insetBy(dx: -0.5, dy: -0.5)
        line([NSPoint(x: o.minX, y: o.maxY), NSPoint(x: o.minX, y: o.minY), NSPoint(x: o.maxX, y: o.minY)], 0x9e7548)
        line([NSPoint(x: o.maxX, y: o.minY), NSPoint(x: o.maxX, y: o.maxY), NSPoint(x: o.minX, y: o.maxY)], 0x392519)
        line([NSPoint(x: i.minX, y: i.maxY), NSPoint(x: i.minX, y: i.minY), NSPoint(x: i.maxX, y: i.minY)], 0x392519)
        line([NSPoint(x: i.maxX, y: i.minY), NSPoint(x: i.maxX, y: i.maxY), NSPoint(x: i.minX, y: i.maxY)], 0x9e7548)
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        needsDisplay = true
    }
}
