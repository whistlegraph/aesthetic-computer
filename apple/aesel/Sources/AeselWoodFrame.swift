import SwiftUI

/// The original Electron spriteland.js wood tile, drawn once at sprite resolution.
struct AeselWoodFrame: View {
    private static let tile: AeselImage = ApplePlatform.image(size: CGSize(width: 384, height: 96)) {
        #if os(macOS)
        guard let context = NSGraphicsContext.current?.cgContext else { return }
        #else
        guard let context = UIGraphicsGetCurrentContext() else { return }
        #endif
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
    }

    var body: some View {
        Image(aeselImage: Self.tile)
            .resizable(resizingMode: .tile).interpolation(.none)
            .overlay {
                Canvas { context, size in
                    var lit = Path()
                    lit.move(to: CGPoint(x: 0.5, y: size.height - 0.5))
                    lit.addLine(to: CGPoint(x: 0.5, y: 0.5))
                    lit.addLine(to: CGPoint(x: size.width - 0.5, y: 0.5))
                    context.stroke(lit, with: .color(Color(rgb: 0x9e7548)), lineWidth: 1)
                    var shade = Path()
                    shade.move(to: CGPoint(x: size.width - 0.5, y: 0.5))
                    shade.addLine(to: CGPoint(x: size.width - 0.5, y: size.height - 0.5))
                    shade.addLine(to: CGPoint(x: 0.5, y: size.height - 0.5))
                    context.stroke(shade, with: .color(Color(rgb: 0x392519)), lineWidth: 1)
                }
            }
            .accessibilityHidden(true)
    }
}

/// A narrow inner shade gives the artwork depth without another hard outline.
struct AeselPreviewInset: View {
    var body: some View {
        ZStack(alignment: .topLeading) {
            LinearGradient(colors: [.black.opacity(0.22), .clear], startPoint: .top, endPoint: .bottom)
                .frame(height: 3).frame(maxHeight: .infinity, alignment: .top)
            LinearGradient(colors: [.black.opacity(0.16), .clear], startPoint: .leading, endPoint: .trailing)
                .frame(width: 3).frame(maxWidth: .infinity, alignment: .leading)
        }
        .allowsHitTesting(false).accessibilityHidden(true)
    }
}
