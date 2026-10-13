import SwiftUI

/// MacPal's title lettering (macpal/Sources/PalCore.swift): Comic, white over a
/// heavy near-black outline, a hard accent shadow, and each letter bobbed and
/// tilted by the same hash of "char<i>". Outlines and shadows are one layer and
/// fills another on top, so no letter's outline cuts into its neighbour.
struct PalText: View {
    let text: String
    var size: CGFloat = 21
    var accent: Color = Color(hex: 0xA478DF)
    var fill: Color = .white

    private var font: Font { .custom("ComicRelief-Bold", size: size) }
    private var outline: CGFloat { max(1.2, size * 0.075) }

    var body: some View {
        ZStack(alignment: .leading) {
            letters { char in
                ZStack {
                    Text(char).foregroundStyle(accent).offset(x: 2, y: 2)
                    ForEach(0..<8, id: \.self) { i in
                        let angle = Double(i) * .pi / 4
                        Text(char).foregroundStyle(Color(white: 0.08))
                            .offset(x: cos(angle) * outline, y: sin(angle) * outline)
                    }
                }
            }
            letters { char in Text(char).foregroundStyle(fill) }
        }
        .font(font)
        .fixedSize()
        .alignmentGuide(.lastTextBaseline) { _ in size * 0.98 }
        .accessibilityElement()
        .accessibilityLabel(text)
    }

    private func letters<Glyph: View>(@ViewBuilder _ glyph: @escaping (String) -> Glyph) -> some View {
        HStack(spacing: 0) {
            ForEach(Array(text.enumerated()), id: \.offset) { index, char in
                let j = Self.jitter(index)
                glyph(String(char))
                    .rotationEffect(.degrees(-j.rot))
                    .offset(y: -j.dy)
            }
        }
    }

    /// PalCore's jitter: FNV-1a over "char<i>", a bob of ±1.5 pt and a tilt of ±2.4°.
    static func jitter(_ i: Int) -> (dy: CGFloat, rot: Double) {
        var h: UInt32 = 2166136261
        for b in "char\(i)".utf8 { h = (h ^ UInt32(b)) &* 16777619 }
        return (CGFloat(Int(h % 7)) / 2 - 1.5, (Double(Int((h >> 8) % 9)) - 4) * 0.6)
    }
}

extension Color {
    init(hex: UInt32, opacity: Double = 1) {
        self.init(.sRGB, red: Double((hex >> 16) & 0xFF) / 255, green: Double((hex >> 8) & 0xFF) / 255,
                  blue: Double(hex & 0xFF) / 255, opacity: opacity)
    }
    static let paper = Color(hex: 0x171526)
    static let pink = Color(hex: 0xFF91C3)

    /// The type's tint, as on mime.ac: image pink, video purple, audio mint, text and code cyan, HTML peach.
    static func tint(forType type: String) -> Color {
        if type == "text/html" { return Color(hex: 0xFFC585) }
        switch type.split(separator: "/").first {
        case "image": return Color(hex: 0xFF91C5)
        case "video": return Color(hex: 0xC5A1FF)
        case "audio": return Color(hex: 0xA6E6B3)
        case "text", "application": return Color(hex: 0x55CDD9)
        default: return Color(hex: 0xA478DF)
        }
    }

    /// A stable per-author colour, the same palette mime.ac uses (its dark-theme half).
    static func author(_ name: String?) -> Color {
        let palette: [UInt32] = [0xFF91C5, 0x7DE3D7, 0xC5A1FF, 0xFFB18A, 0xA6E6B3, 0x9EBAFF]
        var hash: UInt32 = 0
        for scalar in (name ?? "Anonymous").lowercased().unicodeScalars { hash = hash &* 31 &+ scalar.value }
        return Color(hex: palette[Int(hash % UInt32(palette.count))])
    }
}
