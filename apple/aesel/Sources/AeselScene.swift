import SwiftUI
import UIKit

// The desktop's palette, typefaces and mascot, shared without redraws.
// Title lettering is the desktop's Comic Sans MS Bold. iOS has no Comic Sans,
// so the app bundles Comic Relief Bold, its metric-compatible SIL OFL twin
// (Resources/COMIC-RELIEF-OFL-1.1.txt); Chalkboard SE Bold is the last resort.
//
// Colours follow Slab's theme-by-status: the desktop reads the palettes Slab
// publishes to ~/.local/share/slab/state/theme.json, and the phone carries a
// snapshot of that file (Resources/slab-theme.json) so both wear the same
// cloth for the same status without Slab having to be nearby.
struct Paint: Equatable {
    var bg = Color(rgb: 0x463264)
    var deep = Color(rgb: 0x241d35)
    var ink = Color.white
    var dim = Color(rgb: 0xb394ce)
    var rule = Color.white.opacity(0.12)
    var you = Color(rgb: 0xff0088)
    var ac = Color(rgb: 0xff64ff)
    var edit = Color(rgb: 0x77ff55)
    var bad = Color(rgb: 0xff7777)
    var accent = Color(rgb: 0xff78b2)
    // The preview frame: --aesel-background 75% + accent, edged 65% toward white.
    var frame = Color(rgb: 0x744478)
    var frameEdge = Color(rgb: 0xa485a7)
    // The desktop's --user-ink: foreground 65% toward #c12b87.
    var userInk = Color(rgb: 0xe9b5d5)
    // What the notebook web view is told, as CSS hex.
    var css: [String: String] = ["background": "#463264", "foreground": "#ffffff", "userInk": "#e9b5d5"]

    static let base = Paint()
    static func font(_ size: CGFloat = 17) -> Font {
        .custom("Helvetica", size: size, relativeTo: .body)
    }
    static func title(_ size: CGFloat = 20) -> Font {
        Font(Rock.font(size))
    }

    // MARK: Slab theme-by-status

    private typealias RGB = [Double]
    private struct Palette { var background, foreground, bold, cursor: RGB }
    private static let palettes: [String: Palette] = {
        guard let url = Bundle.main.url(forResource: "slab-theme", withExtension: "json"),
              let data = try? Data(contentsOf: url),
              let document = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              document["version"] as? Int == 1, document["enabled"] as? Bool == true,
              let entries = document["palettes"] as? [String: [String: [Double]]] else { return [:] }
        var result: [String: Palette] = [:]
        for (status, value) in entries {
            guard let background = value["background"], let foreground = value["foreground"],
                  let bold = value["bold"], let cursor = value["cursor"],
                  [background, foreground, bold, cursor].allSatisfy({ $0.count == 3 }) else { continue }
            result[status] = Palette(background: background, foreground: foreground, bold: bold, cursor: cursor)
        }
        return result
    }()

    /// Slab's state names for the phone's session words; the desktop gets
    /// these from Slab directly, the phone derives them from its own turn.
    static func slabStatus(_ status: String, busy: Bool, failed: Bool) -> String {
        if failed { return "stale" }
        if busy { return status == "publishing" || status == "rendering" ? "rendering" : "working" }
        switch status {
        case "ready", "live", "complete", "done": return "complete"
        case "stopped", "interrupted", "cancelled": return "interrupted"
        case "failed", "stale", "error", "not published": return "stale"
        case "awaiting", "waiting": return "awaiting"
        default: return "blank"
        }
    }

    static func slab(status: String, busy: Bool, failed: Bool) -> Paint {
        let name = slabStatus(status, busy: busy, failed: failed)
        guard let palette = palettes[name] ?? palettes["blank"] else { return base }
        var paint = base
        let bg = palette.background, fg = palette.foreground
        paint.bg = color(bg)
        paint.deep = color(mix(bg, [0, 0, 0], 0.45))
        paint.ink = color(fg)
        paint.rule = color(fg).opacity(0.12)
        paint.accent = color(palette.cursor)
        paint.you = color(palette.cursor)
        paint.dim = color(readable([170, 150, 205], on: bg))
        paint.ac = color(readable([255, 100, 255], on: bg))
        paint.edit = color(readable([0, 255, 0], on: bg))
        paint.bad = color(readable([255, 90, 90], on: bg))
        let frame = mix(bg, palette.cursor, 0.25)
        paint.frame = color(frame)
        paint.frameEdge = color(mix(frame, [255, 255, 255], 0.65))
        let userInk = mix(fg, [193, 43, 135], 0.35)
        paint.userInk = color(userInk)
        paint.css = ["background": hex(bg), "foreground": hex(fg), "userInk": hex(userInk)]
        return paint
    }

    private static func color(_ rgb: RGB) -> Color {
        Color(red: rgb[0] / 255, green: rgb[1] / 255, blue: rgb[2] / 255)
    }
    private static func hex(_ rgb: RGB) -> String {
        "#" + rgb.map { String(format: "%02x", Int($0.rounded())) }.joined()
    }
    private static func mix(_ a: RGB, _ b: RGB, _ amount: Double) -> RGB {
        zip(a, b).map { $0 + ($1 - $0) * amount }
    }
    private static func luminance(_ rgb: RGB) -> Double {
        let linear = rgb.map { channel -> Double in
            let c = channel / 255
            return c <= 0.04045 ? c / 12.92 : pow((c + 0.055) / 1.055, 2.4)
        }
        return linear[0] * 0.2126 + linear[1] * 0.7152 + linear[2] * 0.0722
    }
    private static func contrast(_ a: RGB, _ b: RGB) -> Double {
        let x = luminance(a), y = luminance(b)
        return (max(x, y) + 0.05) / (min(x, y) + 0.05)
    }
    // The desktop's `readable`: keep the hue, move only as far toward black or
    // white as WCAG AA (4.5:1) against the background needs.
    private static func readable(_ color: RGB, on background: RGB) -> RGB {
        if contrast(color, background) >= 4.5 { return color }
        let target: Double = contrast([0, 0, 0], background) > contrast([255, 255, 255], background) ? 0 : 255
        for step in 1...100 {
            let candidate = color.map { ($0 + (target - $0) * Double(step) / 100).rounded() }
            if contrast(candidate, background) >= 4.5 { return candidate }
        }
        return [target, target, target]
    }
}

private struct PaintKey: EnvironmentKey { static let defaultValue = Paint.base }
extension EnvironmentValues {
    var paint: Paint {
        get { self[PaintKey.self] }
        set { self[PaintKey.self] = newValue }
    }
}

/// The desktop's native Prox lettering (easel/desktop/native/credit-label.swift),
/// drawn with UIKit: cyan, purple and pink echoes under a thin-outlined face
/// with a hard pink shadow, one cached image per letter so the notebook can
/// tilt and sway each one on its own.
enum Rock {
    static let inset: CGFloat = 9
    private static var cache: [String: (UIImage, CGSize)] = [:]

    static func font(_ size: CGFloat) -> UIFont {
        for name in ["ComicRelief-Bold", "ComicSansMS-Bold", "ChalkboardSE-Bold"] {
            if let font = UIFont(name: name, size: size) { return font }
        }
        return .systemFont(ofSize: size, weight: .heavy)
    }

    /// A letter's image plus its advance box; the image overhangs the box by
    /// `inset` on every side so the echoes and shadow are not clipped.
    static func glyph(_ text: String, size: CGFloat, face: UIColor = .white) -> (image: UIImage, advance: CGSize) {
        let key = "\(text)|\(size)|\(face)"
        if let hit = cache[key] { return hit }
        let font = font(size)
        let advance = (text as NSString).size(withAttributes: [.font: font])
        let bounds = CGSize(width: ceil(advance.width + inset * 2), height: ceil(advance.height + inset * 2))
        let format = UIGraphicsImageRendererFormat.default()
        format.opaque = false
        let image = UIGraphicsImageRenderer(size: bounds, format: format).image { _ in
            let echoes: [(UIColor, CGFloat, CGFloat)] = [
                (.systemCyan.withAlphaComponent(0.28), 4.5, 3),
                (.systemPurple.withAlphaComponent(0.40), 3, 2),
                (.systemPink.withAlphaComponent(0.65), 1.5, 1.5),
            ]
            for (color, x, y) in echoes {
                NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: color])
                    .draw(at: CGPoint(x: inset + x, y: inset + y))
            }
            let shadow = NSShadow()
            shadow.shadowColor = UIColor.systemPink.withAlphaComponent(0.5)
            shadow.shadowBlurRadius = 0
            shadow.shadowOffset = CGSize(width: 1.5, height: 1.5)
            NSAttributedString(string: text, attributes: [
                .font: font,
                .foregroundColor: face.withAlphaComponent(0.94),
                .strokeColor: UIColor(white: 0.08, alpha: 1),
                .strokeWidth: -3.5,
                .shadow: shadow,
            ]).draw(at: CGPoint(x: inset, y: inset))
        }
        if cache.count > 256 { cache.removeAll() }
        cache[key] = (image, advance)
        return (image, advance)
    }
}

extension Color {
    init(rgb: UInt32) {
        self.init(red: Double((rgb >> 16) & 255) / 255,
                  green: Double((rgb >> 8) & 255) / 255,
                  blue: Double(rgb & 255) / 255)
    }
    // "#rrggbb" from the account palette, or nil.
    init?(hex: String) {
        guard hex.count == 7, let rgb = UInt32(hex.dropFirst(), radix: 16) else { return nil }
        self.init(rgb: rgb)
    }
}

extension UIColor {
    // "#rrggbb" from the account palette, or nil.
    convenience init?(hex: String) {
        guard hex.count == 7, let rgb = UInt32(hex.dropFirst(), radix: 16) else { return nil }
        self.init(red: CGFloat((rgb >> 16) & 255) / 255, green: CGFloat((rgb >> 8) & 255) / 255, blue: CGFloat(rgb & 255) / 255, alpha: 1)
    }
}

struct AeselCloth: View {
    @Environment(\.paint) private var paint
    var body: some View { paint.bg.accessibilityHidden(true) }
}

/// The desktop's ruled sheet: one faint line per 24px row, from 10px in to
/// the trailing edge, drawn behind everything so title, prose and the draft
/// all sit on the same paper.
struct AeselRuling: View {
    var spacing: CGFloat = 24
    @Environment(\.paint) private var paint
    var body: some View {
        Canvas { context, size in
            var y = spacing - 0.5
            var path = Path()
            while y < size.height {
                path.move(to: CGPoint(x: 10, y: y))
                path.addLine(to: CGPoint(x: size.width, y: y))
                y += spacing
            }
            context.stroke(path, with: .color(paint.rule), lineWidth: 1)
        }
        .accessibilityHidden(true)
    }
}

struct AeselWordmark: View {
    var text = "aesel"
    @Environment(\.paint) private var paint
    var body: some View {
        let colors: [Color] = [.orange, paint.ac, paint.edit, paint.dim, paint.you]
        HStack(spacing: 0) {
            ForEach(Array(text.enumerated()), id: \.offset) { index, letter in
                Text(String(letter)).foregroundStyle(colors[index % colors.count])
            }
        }
        .font(Paint.title(28))
        .accessibilityElement(children: .ignore)
        .accessibilityLabel("aesel")
    }
}

/// The desktop's piece title: Prox lettering with handle letters tinted by the
/// account palette, every letter leaning by the FNV hash Slab uses (so the
/// phone and the rock agree on each letter's tilt) and swaying on its own
/// 1.8s beat like the desktop's qr-name-wiggle.
struct AeselTitle: View {
    let text: String
    var colors: [String] = []
    var size: CGFloat = 20

    private static func fnv(_ text: String) -> UInt32 {
        var hash: UInt32 = 2166136261
        for byte in text.utf8 { hash = (hash ^ UInt32(byte)) &* 16777619 }
        return hash
    }

    // "@handle" letters wear the account palette; the piece name stays white.
    private func face(_ index: Int, handle: Int) -> UIColor {
        guard index < handle, colors.indices.contains(index), let color = UIColor(hex: colors[index]) else { return .white }
        return color
    }

    var body: some View {
        let handle = text.hasPrefix("@") ? text.prefix(while: { $0 != "/" }).count : 0
        HStack(alignment: .top, spacing: 0) {
            ForEach(Array(text.enumerated()), id: \.offset) { index, letter in
                let hash = Self.fnv("rock\(index)\(text)")
                let face = face(index, handle: handle)
                RockLetter(text: String(letter), size: size, face: face, beat: Double(index) * 0.12)
                    // A new letter is a new view, so the sway never crossfades old ink into new.
                    .id("\(index)|\(letter)|\(face)")
                    .rotationEffect(.degrees(-Double(Int((hash >> 8) % 9) - 4) * 0.9))
                    .offset(y: -(CGFloat(hash % 5) / 2 - 1))
            }
        }
        .accessibilityElement(children: .ignore)
        .accessibilityLabel(text)
    }
}

/// One swaying letter: its image overhangs the advance box by `Rock.inset`
/// so the echoes and shadow stay unclipped, and it rocks on its own beat.
private struct RockLetter: View {
    let text: String
    let size: CGFloat
    let face: UIColor
    let beat: Double
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var swaying = false

    var body: some View {
        let glyph = Rock.glyph(text, size: size, face: face)
        Color.clear
            .frame(width: glyph.advance.width, height: glyph.advance.height)
            .overlay(alignment: .topLeading) {
                Image(uiImage: glyph.image).offset(x: -Rock.inset, y: -Rock.inset)
            }
            .rotationEffect(.degrees(swaying ? 0.8 : -1.2))
            .offset(y: swaying ? 0.8 : -1.2)
            .onAppear {
                guard !reduceMotion else { return }
                withAnimation(.easeInOut(duration: 0.9).repeatForever(autoreverses: true).delay(beat)) { swaying = true }
            }
    }
}

/// The desktop's pencil companion: eight 512px cells on a 4×2 sheet, copied in
/// by bundle-session.sh, drawn smooth and running only while a turn is live.
struct AeselDonkey: View {
    var busy: Bool
    var failed: Bool
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    private static let frames: [UIImage] = {
        guard let url = Bundle.main.url(forResource: "donkey-pencil-run-v2", withExtension: "png", subdirectory: "Session/easel/desktop/assets"),
              let sheet = UIImage(contentsOfFile: url.path)?.cgImage else { return [] }
        let cell = sheet.width / 4
        return (0..<8).compactMap { index in
            sheet.cropping(to: CGRect(x: (index % 4) * cell, y: (index / 4) * sheet.height / 2 + 64, width: cell, height: 416)).map { UIImage(cgImage: $0) }
        }
    }()

    var body: some View {
        TimelineView(.animation(minimumInterval: 0.09, paused: reduceMotion || !busy)) { timeline in
            let index = busy && !reduceMotion ? Int(timeline.date.timeIntervalSinceReferenceDate / 0.09) % 8 : 0
            if Self.frames.indices.contains(index) {
                Image(uiImage: Self.frames[index])
                    .resizable()
                    .scaledToFit()
                    .opacity(failed ? 0.5 : 1)
            }
        }
        .frame(width: 112, height: 112)
        .accessibilityLabel(busy ? "aesel is working" : "aesel the donkey")
    }
}

struct AeselHandle: View {
    let handle: String
    let colors: [String]
    @Environment(\.paint) private var paint

    private var label: AttributedString {
        var result = AttributedString()
        for (index, character) in ("@" + handle).enumerated() {
            var letter = AttributedString(String(character))
            letter.foregroundColor = colors.indices.contains(index) ? Color(hex: colors[index]) ?? paint.ac : paint.ac
            result += letter
        }
        return result
    }

    var body: some View {
        Text(label).lineLimit(1).truncationMode(.tail)
            .accessibilityLabel("@" + handle)
    }
}
