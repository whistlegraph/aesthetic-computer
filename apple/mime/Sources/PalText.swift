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
    var colors: [Color?] = []
    var animated = true
    /// How far each letter sways: 1 is mime.ac's pal-breathe; the card corners swing wider.
    var swing: CGFloat = 1
    /// Letters rise in one after another each time the card comes on screen.
    var entrance = false
    @Environment(\.accessibilityReduceMotion) private var reduced
    @Environment(\.scenePhase) private var scenePhase
    @Environment(\.mimeLettersActive) private var onstage
    @State private var started = Date()

    private var font: Font { .custom("ComicRelief-Bold", size: size) }
    private var outline: CGFloat { max(1.2, size * 0.075) }

    var body: some View {
        TimelineView(.animation(minimumInterval: 1 / 30, paused: !animated || reduced || !onstage || scenePhase != .active)) { timeline in
        let time = animated && !reduced && onstage ? timeline.date.timeIntervalSince(started) : nil
        ZStack(alignment: .leading) {
            letters(time: time) { _, char in
                ZStack {
                    Text(char).foregroundStyle(accent).offset(x: 2, y: 2)
                    ForEach(0..<8, id: \.self) { i in
                        let angle = Double(i) * .pi / 4
                        Text(char).foregroundStyle(Color(white: 0.08))
                            .offset(x: cos(angle) * outline, y: sin(angle) * outline)
                    }
                }
            }
            letters(time: time) { index, char in Text(char).foregroundStyle(index < colors.count ? colors[index] ?? fill : fill) }
        }
        .font(font)
        .fixedSize()
        .alignmentGuide(.lastTextBaseline) { _ in size * 0.98 }
        .accessibilityElement()
        .accessibilityLabel(text)
        }
        .onChange(of: onstage) { _, now in if now { started = Date() } }
        .onAppear { started = Date() }
    }

    private func letters<Glyph: View>(time: Double?, @ViewBuilder _ glyph: @escaping (Int, String) -> Glyph) -> some View {
        HStack(spacing: 0) {
            ForEach(Array(text.enumerated()), id: \.offset) { index, char in
                let j = Self.jitter(index)
                let motion = (time.map { Self.breathe(index, time: $0) } ?? 0) * swing
                let rise = entrance ? Self.rise(index, time: time) : 1
                glyph(index, String(char))
                    .rotationEffect(.degrees(-j.rot + motion * 2 + (1 - rise) * (index.isMultiple(of: 2) ? -18 : 18)))
                    .offset(y: -j.dy + motion * 1.5 + (1 - rise) * size * 0.7)
                    .scaleEffect(0.4 + 0.6 * rise)
                    .opacity(rise)
            }
        }
    }

    /// 0 → 1 as a letter lands, each 35 ms after the one before, with a little overshoot.
    static func rise(_ i: Int, time: Double?) -> Double {
        guard let time else { return 1 }
        let t = min(1, max(0, (time - Double(i) * 0.035) / 0.4))
        let c = 1.7, u = t - 1
        return 1 + (c + 1) * u * u * u + c * u * u
    }

    /// PalCore's jitter: FNV-1a over "char<i>", a bob of ±1.5 pt and a tilt of ±2.4°.
    static func jitter(_ i: Int) -> (dy: CGFloat, rot: Double) {
        let h = hash(i)
        return (CGFloat(Int(h % 7)) / 2 - 1.5, (Double(Int((h >> 8) % 9)) - 4) * 0.6)
    }

    private static func hash(_ i: Int) -> UInt32 {
        var h: UInt32 = 2166136261
        for b in "char\(i)".utf8 { h = (h ^ UInt32(b)) &* 16777619 }
        return h
    }

    /// mime.ac's pal-breathe: identical per-letter period, negative delay and
    /// CSS ease-in-out curve, shared by the outline and fill layers.
    static func breathe(_ i: Int, time: Double) -> Double {
        let h = hash(i), period = 3.1 + Double(h % 23) / 10
        let phase = (max(0, time) + Double(h % 47) / 10).truncatingRemainder(dividingBy: period) / period
        let x = phase < 0.5 ? phase * 2 : (1 - phase) * 2
        var low = 0.0, high = 1.0
        for _ in 0..<12 {
            let t = (low + high) / 2, u = 1 - t
            if 3 * u * u * t * 0.42 + 3 * u * t * t * 0.58 + t * t * t < x { low = t } else { high = t }
        }
        let t = (low + high) / 2
        return (3 * (1 - t) * t * t + t * t * t) * 2 - 1
    }
}

private struct MimeLettersActive: EnvironmentKey { static let defaultValue = true }
extension EnvironmentValues {
    var mimeLettersActive: Bool { get { self[MimeLettersActive.self] } set { self[MimeLettersActive.self] = newValue } }
}

extension Color {
    static func savedLetter(_ rgb: Post.LetterRGB?) -> Color? {
        guard let rgb, [rgb.r, rgb.g, rgb.b].allSatisfy(\.isFinite) else { return nil }
        return Color(red: min(255, max(0, rgb.r)) / 255, green: min(255, max(0, rgb.g)) / 255, blue: min(255, max(0, rgb.b)) / 255)
    }

    /// Matches mime.ac's family/slash/subtype lettering.
    static func letters(forType type: String) -> [Color?] {
        let families: [String: UInt32] = ["text": 0x65DDD6, "image": 0xFF91C5, "video": 0xC5A1FF, "audio": 0xA6E6B3,
            "application": 0x8FB9FF, "x-application": 0x8FB9FF, "font": 0xFFC585, "model": 0xF1D66D, "multipart": 0xB9C4D0]
        let subtypes: [String: UInt32] = ["plain": 0xF7E9C5, "html": 0xFFC585, "javascript": 0xFFE16B, "x-javascript": 0xFFE16B,
            "x-lisp": 0xF5A8ED, "x-lua": 0xA6BCFF, "css": 0xA5C9FF, "json": 0xB8E889, "xml": 0xB8E889,
            "png": 0xFFE8A1, "jpeg": 0xFFC3A1, "gif": 0xE9A7EE, "webp": 0xACE5C2, "svg+xml": 0xFFBC80,
            "mp4": 0xF7B8DC, "webm": 0xB6A8FF, "mpeg": 0xF2D58C, "wav": 0xABE8BD, "ogg": 0xA4DBED,
            "zip": 0xF2CE8B, "pdf": 0xFF9A93, "x-doom-wad": 0xFF947D, "octet-stream": 0xBDC8D6]
        let chars = Array(type), slash = chars.firstIndex(of: "/") ?? -1
        let parts = type.lowercased().split(separator: "/", maxSplits: 1, omittingEmptySubsequences: false)
        let family = String(parts.first ?? ""), subtype = parts.count > 1 ? String(parts[1].split(separator: ";").first ?? "") : ""
        return chars.indices.map { i in Color(hex: i < slash ? families[family] ?? 0xBDC8D6 : i == slash ? 0xFFFFFF : subtypes[subtype] ?? 0xD9C7F3) }
    }
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
