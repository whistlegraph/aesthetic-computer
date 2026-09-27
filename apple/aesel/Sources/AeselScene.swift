import SwiftUI


// The shared renderer's palette, typefaces and mascot, shared without redraws.
// Title lettering is the shared renderer's Comic Sans MS Bold. iOS has no Comic Sans,
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
    // The shared renderer's --user-ink: foreground 65% toward #c12b87.
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

    static func slab(status: String, busy: Bool, failed: Bool, colorScheme: ColorScheme = .dark) -> Paint {
        let name = slabStatus(status, busy: busy, failed: failed)
        let palette = palettes[name] ?? palettes["blank"] ?? Palette(background: [70,50,100], foreground: [255,255,255], bold: [255,255,255], cursor: [255,120,178])
        var paint = base
        // Prompt's purple night / legal-pad daylight; status tints stay subtle.
        let dark = colorScheme == .dark
        let paper: RGB = dark ? [70,50,100] : [252,247,197]
        let bg = mix(paper, palette.cursor, name == "blank" ? 0 : 0.04)
        let fg: RGB = dark ? [255,255,255] : [40,30,90]
        paint.bg = color(bg)
        paint.deep = color(mix(bg, dark ? [0,0,0] : [255,255,255], 0.25))
        paint.ink = color(fg)
        paint.rule = color(fg).opacity(0.12)
        paint.accent = color(readable(palette.cursor, on: bg))
        paint.you = paint.accent
        paint.dim = color(readable([170, 150, 205], on: bg))
        paint.ac = color(readable([255, 100, 255], on: bg))
        paint.edit = color(readable([0, 255, 0], on: bg))
        paint.bad = color(readable([255, 90, 90], on: bg))
        let frame = mix(bg, palette.cursor, 0.25)
        paint.frame = color(frame)
        paint.frameEdge = color(mix(frame, [255, 255, 255], 0.65))
        let userInk = readable(mix(fg, [193, 43, 135], 0.35), on: bg)
        paint.userInk = color(userInk)
        paint.css = ["background": hex(bg), "foreground": hex(fg), "userInk": hex(userInk),
                     "error": hex(readable([255,90,90], on: bg)),
                     "number": hex(readable(dark ? [255,209,124] : [143,61,15], on: bg)),
                     "colorScheme": dark ? "dark" : "light"]
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
    // The shared renderer's `readable`: keep the hue, move only as far toward black or
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

/// The shared renderer's native Prox lettering (easel/shared/native/credit-label.swift),
/// drawn with a light face, a dark edge and tight cyan, purple and pink accents.
/// One cached image per letter lets the notebook
/// tilt and sway each one on its own.
enum Rock {
    static let inset: CGFloat = 4
    private static var cache: [String: (AeselImage, CGSize)] = [:]

    static func font(_ size: CGFloat) -> AeselFont {
        for name in ["ComicRelief-Bold", "ComicSansMS-Bold", "ChalkboardSE-Bold"] {
            if let font = AeselFont(name: name, size: size) { return font }
        }
        return .systemFont(ofSize: size, weight: .heavy)
    }

    /// A letter's image plus its advance box; the image overhangs the box by
    /// `inset` on every side so the echoes and shadow are not clipped.
    static func glyph(_ text: String, size: CGFloat, face: AeselColor = .white) -> (image: AeselImage, advance: CGSize) {
        let key = "\(text)|\(size)|\(face)"
        if let hit = cache[key] { return hit }
        let font = font(size)
        let advance = (text as NSString).size(withAttributes: [.font: font])
        let bounds = CGSize(width: ceil(advance.width + inset * 2), height: ceil(advance.height + inset * 2))
        let image = ApplePlatform.image(size: bounds) {
            for (color, x, y) in [(AeselColor.systemCyan, CGFloat(1.75), CGFloat(1)),
                                  (AeselColor.systemPurple, CGFloat(1), CGFloat(0.75))] {
                NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: color,
                    .strokeColor: color, .strokeWidth: -3])
                    .draw(at: CGPoint(x: inset + x, y: inset + y))
            }
            let shadow = NSShadow()
            shadow.shadowColor = AeselColor.systemPink.withAlphaComponent(0.7)
            shadow.shadowBlurRadius = 0
            shadow.shadowOffset = CGSize(width: 0.75, height: 0.75)
            NSAttributedString(string: text, attributes: [
                .font: font,
                .foregroundColor: face,
                .strokeColor: AeselColor(white: 0.08, alpha: 1),
                .strokeWidth: -3,
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

extension AeselColor {
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

/// The shared renderer's ruled sheet: one faint line per 24px row, from 10px in to
/// the trailing edge, drawn behind everything so title, prose and the draft
/// all sit on the same paper.
private struct UIScaleKey: EnvironmentKey { static let defaultValue: CGFloat = 1 }
extension EnvironmentValues {
    var aeselUIScale: CGFloat {
        get { self[UIScaleKey.self] }
        set { self[UIScaleKey.self] = newValue }
    }
}

struct AeselRuling: View {
    var spacing: CGFloat = 24
    var topInset: CGFloat = 0
    @Environment(\.displayScale) private var displayScale
    @Environment(\.aeselUIScale) private var uiScale
    @Environment(\.paint) private var paint
    var body: some View {
        Canvas { context, size in
            let pixels = displayScale * uiScale
            let hairline = 1 / pixels
            var row = 1
            var path = Path()
            while topInset + CGFloat(row) * spacing < size.height + hairline {
                let y = ((topInset + CGFloat(row) * spacing) * pixels).rounded() / pixels - hairline / 2
                path.move(to: CGPoint(x: 10, y: y))
                path.addLine(to: CGPoint(x: size.width, y: y))
                row += 1
            }
            context.stroke(path, with: .color(paint.rule), lineWidth: hairline)
        }
        .accessibilityHidden(true)
    }
}

struct AeselWordmark: View {
    var text = "aesel"
    @Environment(\.paint) private var paint
    var body: some View {
        let colors: [Color] = [paint.accent, paint.ac, paint.edit, paint.dim, paint.you]
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

/// The shared renderer's piece title: Prox lettering with handle letters tinted by the
/// account palette, every letter leaning by the FNV hash Slab uses (so the
/// phone and the rock agree on each letter's tilt) and swaying on its own
/// 1.8s beat like the shared renderer's qr-name-wiggle.
struct AeselTitle: View {
    let text: String
    var colors: [String] = []
    var size: CGFloat = 20
    var maximumWidth: CGFloat? = nil
    var horizontalInset: CGFloat = 12
    var hoverAnchor: UnitPoint = .leading
    var hoverSound: (() -> Void)? = nil
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var hovered = false

    private static func fnv(_ text: String) -> UInt32 {
        var hash: UInt32 = 2166136261
        for byte in text.utf8 { hash = (hash ^ UInt32(byte)) &* 16777619 }
        return hash
    }

    // Handle letters keep the account palette; the light face has a dark outline.
    private func face(_ index: Int, handle: Int) -> AeselColor {
        guard index < handle, colors.indices.contains(index), let color = AeselColor(hex: colors[index]) else { return .white }
        return color
    }

    var body: some View {
        let handle = text.hasPrefix("@") ? text.prefix(while: { $0 != "/" }).count : 0
        // Include image overhang and sway when fitting a one-line title.
        let width = text.reduce(horizontalInset * 2) { $0 + (String($1) as NSString).size(withAttributes: [.font: Rock.font(size)]).width }
        let limit = maximumWidth.map { max(0, $0) / max(1, width) } ?? CGFloat.greatestFiniteMagnitude
        let restingScale = min(1, limit)
        Color.clear
            .frame(width: width * restingScale, height: size * 1.6)
            .overlay(alignment: .leading) {
                HStack(alignment: .top, spacing: 0) {
                    ForEach(Array(text.enumerated()), id: \.offset) { index, letter in
                        let hash = Self.fnv("rock\(index)\(text)")
                        let face = face(index, handle: handle)
                        RockLetter(text: String(letter), size: size, face: face, beat: Double(index) * 0.12)
                            // A new letter is a new view, so the sway never crossfades old ink into new.
                            .id("\(index)|\(letter)|\(face)")
                            .rotationEffect(.degrees(-Double(Int((hash >> 8) % 9) - 4) * 0.2))
                            .offset(y: -(CGFloat(hash % 5) / 2 - 1) * 0.25)
                    }
                }
                .padding(.horizontal, horizontalInset)
                .fixedSize()
                .scaleEffect(restingScale, anchor: .leading)
                .scaleEffect(hovered && !reduceMotion ? 1.2 : 1, anchor: hoverAnchor)
                .animation(.easeOut(duration: 0.16), value: hovered)
                .allowsHitTesting(false)
            }
        .contentShape(Rectangle())
        .onHover { inside in
            if inside && !hovered { hoverSound?() }
            hovered = inside
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
    let face: AeselColor
    let beat: Double
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    var body: some View {
        let glyph = Rock.glyph(text, size: size, face: face)
        // Time changes only the drawing transform, never the glyph layout.
        TimelineView(.animation(minimumInterval: 1 / 30, paused: reduceMotion)) { timeline in
            let wave = reduceMotion ? 0 : sin((timeline.date.timeIntervalSinceReferenceDate - beat) * .pi / 0.9)
            Color.clear
                .frame(width: glyph.advance.width, height: glyph.advance.height)
                .overlay(alignment: .topLeading) {
                    Image(aeselImage: glyph.image).offset(x: -Rock.inset, y: -Rock.inset)
                }
                .rotationEffect(.degrees(wave * 0.35))
                .offset(y: wave * 0.35)
        }
    }
}

/// The shared renderer's pencil companion: eight 512px cells on a 4×2 sheet, copied in
/// by bundle-session.sh, running while requested and respecting Reduce Motion.
struct AeselDonkey: View {
    var busy: Bool
    var failed: Bool
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var frameIndex = 0
    private static let frames: [AeselImage] = {
        guard let url = Bundle.main.url(forResource: "donkey-pencil-run-v2", withExtension: "png", subdirectory: "Session/easel/shared/assets"),
              let sheet = ApplePlatform.cgImage(at: url) else { return [] }
        let cell = sheet.width / 4
        return (0..<8).compactMap { index in
            sheet.cropping(to: CGRect(x: (index % 4) * cell, y: (index / 4) * sheet.height / 2 + 64, width: cell, height: 416)).map { ApplePlatform.image(cgImage: $0) }
        }
    }()

    var body: some View {
        Group {
            if Self.frames.indices.contains(frameIndex) {
                Image(aeselImage: Self.frames[frameIndex])
                    .resizable()
                    .scaledToFit()
                    .opacity(failed ? 0.5 : 1)
            }
        }
        .frame(width: 112, height: 112)
        .task(id: busy && !reduceMotion) {
            frameIndex = 0
            guard busy && !reduceMotion && !Self.frames.isEmpty else { return }
            // Advance one pose per tick. Deriving the index from wall time
            // skipped poses whenever a redraw crossed a frame boundary.
            while !Task.isCancelled {
                do { try await Task.sleep(for: .milliseconds(90)) }
                catch { return }
                guard !Task.isCancelled else { return }
                frameIndex = (frameIndex + 1) % Self.frames.count
            }
        }
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
