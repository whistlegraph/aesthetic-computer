import AppKit
import QuartzCore

/// The sung line on screen, karaoke-style: every word of the line on one
/// banner, the syllable being sung lit. Lettered like slab's prompt rocks —
/// Comic Sans MS Bold, bubble fill + stroke + hard shadow, each glyph with
/// its own small jitter and sway — so a machine's captions read as the same
/// family as its name. Driven from the play loop: `show` when a line's first
/// syllable is due, `highlight` on every onset, `hide` after its last.
final class LyricCaption {
    static let shared = LyricCaption()

    /// Sim mode: one caption per tile, laid out inside the tile's frame.
    private static var slots: [Int: LyricCaption] = [:]
    static func at(_ slot: SingerFace.SimSlot?) -> LyricCaption {
        guard let slot else { return shared }
        if let c = slots[slot.index] { c.slot = slot; return c }
        let c = LyricCaption(); c.slot = slot; slots[slot.index] = c; return c
    }
    static func hideAll() { shared.hide(line: nil); slots.values.forEach { $0.hide(line: nil) } }
    private var slot: SingerFace.SimSlot?

    private var panel: NSPanel?
    private var shownLine: Int?
    private var syllableGlyphs: [[CaptionGlyphLayer]] = []
    private var litUpTo = -1
    private var ink = NSColor.white
    private var dim = NSColor.white

    static let citrus = NSColor(srgbRed: 0.96, green: 0.77, blue: 0.26, alpha: 1)

    /// "#rrggbb" → color; anything else → citrus, the eldest's.
    static func color(hex: String?) -> NSColor {
        guard var h = hex?.trimmingCharacters(in: .whitespaces), !h.isEmpty else { return citrus }
        if h.hasPrefix("#") { h.removeFirst() }
        guard h.count == 6, let v = UInt32(h, radix: 16) else { return citrus }
        return NSColor(srgbRed: CGFloat((v >> 16) & 0xff) / 255, green: CGFloat((v >> 8) & 0xff) / 255,
                       blue: CGFloat(v & 0xff) / 255, alpha: 1)
    }

    /// The rock lettering: Comic Sans MS Bold, then Chalkboard, then heavy system.
    static func font(_ pt: CGFloat) -> NSFont {
        for n in ["Comic Sans MS Bold", "ComicSansMS-Bold", "Chalkboard SE Bold", "ChalkboardSE-Bold"] {
            if let f = NSFont(name: n, size: pt) { return f }
        }
        return NSFont.systemFont(ofSize: pt, weight: .heavy)
    }

    private func makePanel(on screen: NSScreen, frame: NSRect) -> NSPanel {
        let p = NSPanel(contentRect: frame, styleMask: [.borderless, .nonactivatingPanel],
                        backing: .buffered, defer: true)
        let under: NSWindow.Level = slot == nil ? .screenSaver : .floating
        p.level = NSWindow.Level(rawValue: under.rawValue + 1)   // over the face
        p.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]
        p.isOpaque = false
        p.backgroundColor = .clear
        p.hasShadow = false
        p.ignoresMouseEvents = true
        p.hidesOnDeactivate = false
        p.isReleasedWhenClosed = false
        let v = NSView(frame: NSRect(origin: .zero, size: frame.size))
        v.wantsLayer = true
        p.contentView = v
        return p
    }

    /// Put line `line` up: `tokens` are its words, syllables joined by "-".
    /// `size` 0 = scale with the screen. Bottom-center of the menu-bar screen.
    func show(line: Int, tokens: [String], accent: NSColor, size requested: CGFloat) {
        precondition(Thread.isMainThread)
        guard let screen = NSScreen.screens.first ?? NSScreen.main else { return }
        let stage = slot?.tile(on: screen) ?? screen.frame       // the panel's frame: the tile, or the screen
        let panel = self.panel ?? makePanel(on: screen, frame: stage)
        self.panel = panel
        panel.setFrame(stage, display: false)
        panel.contentView?.frame = NSRect(origin: .zero, size: stage.size)
        guard let root = panel.contentView?.layer else { return }
        root.sublayers?.forEach { $0.removeFromSuperlayer() }
        syllableGlyphs = []
        litUpTo = -1

        // One glyph per character, a space between words, and which
        // syllable each glyph belongs to (-1 = the space).
        var chars: [(ch: Character, syl: Int)] = []
        var nsyl = 0
        for (wi, t) in tokens.enumerated() {
            if wi > 0 { chars.append((" ", -1)) }
            for part in t.split(separator: "-") {
                for c in part { chars.append((c, nsyl)) }
                nsyl += 1
            }
        }
        guard !chars.isEmpty, nsyl > 0 else { return }

        var pt = requested > 0 ? requested
            : slot == nil ? max(22, round(stage.width / 34)) : max(11, round(stage.width / 26))
        let maxWidth = stage.width * 0.86
        var font = LyricCaption.font(pt)
        var widths: [CGFloat] = []
        func measure() -> CGFloat {
            widths = chars.map { ceil((String($0.ch) as NSString).size(withAttributes: [.font: font]).width) }
            return widths.reduce(0, +)
        }
        var total = measure()
        while total > maxWidth && pt > 14 {
            pt = floor(pt * 0.92); font = LyricCaption.font(pt); total = measure()
        }

        let accentRGB = accent.usingColorSpace(.deviceRGB) ?? accent
        let lum = 0.2126 * accentRGB.redComponent + 0.7152 * accentRGB.greenComponent
            + 0.0722 * accentRGB.blueComponent
        ink = lum > 0.58 ? NSColor(deviceWhite: 0.06, alpha: 1) : NSColor(deviceWhite: 0.98, alpha: 1)
        dim = ink.withAlphaComponent(0.38)
        let stroke = lum > 0.58 ? NSColor(deviceWhite: 0.98, alpha: 0.85) : NSColor(white: 0.08, alpha: 1)
        let hardShadow = accentRGB.blended(withFraction: 0.55, of: .black) ?? .black

        let lineH = ceil(pt * 1.5)
        let inset = ceil(pt * 0.6)
        let x0 = round((stage.width - total) / 2)
        let y0 = slot == nil
            ? round(screen.visibleFrame.minY - screen.frame.minY + stage.height * 0.09)
            : round(stage.height * 0.11)

        // no banner — the lettering carries itself (stroke + hard shadow)

        var groups: [[CaptionGlyphLayer]] = Array(repeating: [], count: nsyl)
        var pen = x0
        for (i, c) in chars.enumerated() {
            let w = widths[i]
            defer { pen += w }
            guard c.syl >= 0 else { continue }
            let g = CaptionGlyphLayer()
            g.contentsScale = screen.backingScaleFactor
            g.font = font
            g.strokeColor = stroke
            g.shadowInk = hardShadow
            g.text = String(c.ch)
            g.inset = ceil(pt * 0.25)
            g.bounds = CGRect(x: 0, y: 0, width: w + g.inset * 2, height: lineH + g.inset * 2)
            // MacPal jitter, deterministic per glyph (FNV-1a), like a rock's name.
            var h: UInt32 = 2_166_136_261
            for b in "\(line):\(i):\(c.ch)".utf8 { h = (h ^ UInt32(b)) &* 16_777_619 }
            let jx = (CGFloat(Int(h % 7)) - 3) * pt / 24
            let jy = (CGFloat(Int((h >> 8) % 5)) - 2) * pt / 24
            let rot = (CGFloat(Int((h >> 16) % 9)) - 4) * .pi / 180
            g.position = CGPoint(x: pen + w / 2 + jx, y: y0 + jy)
            g.transform = CATransform3DMakeRotation(rot, 0, 0, 1)
            g.ink = dim
            g.setNeedsDisplay()
            root.addSublayer(g)
            let sway = CABasicAnimation(keyPath: "transform.translation.y")
            sway.fromValue = -1.2
            sway.toValue = 1.2
            sway.duration = 1.6 + Double((h >> 4) % 9) / 10
            sway.autoreverses = true
            sway.repeatCount = .infinity
            sway.timeOffset = Double(h % 100) / 50
            g.add(sway, forKey: "sway")
            groups[c.syl].append(g)
        }
        syllableGlyphs = groups
        shownLine = line

        NSAnimationContext.runAnimationGroup { ctx in
            ctx.duration = 0
            panel.animator().alphaValue = 1
        }
        panel.orderFrontRegardless()
    }

    /// Light syllable `k` of the shown line: it and everything before it turn
    /// full ink, and it pops.
    func highlight(line: Int, syllable k: Int) {
        precondition(Thread.isMainThread)
        guard shownLine == line, k < syllableGlyphs.count else { return }
        if k > litUpTo {
            for i in (litUpTo + 1)...k {
                for g in syllableGlyphs[i] { g.ink = ink; g.setNeedsDisplay() }
            }
            litUpTo = k
        }
        for g in syllableGlyphs[k] {
            let pop = CAKeyframeAnimation(keyPath: "transform.scale")
            pop.values = [1, 1.34, 1.12]
            pop.keyTimes = [0, 0.35, 1]
            pop.duration = 0.32
            pop.timingFunction = CAMediaTimingFunction(name: .easeOut)
            pop.fillMode = .forwards
            pop.isRemovedOnCompletion = false
            g.add(pop, forKey: "pop")
        }
    }

    /// Fade out — only if `line` is still the one showing (nil = whatever is up).
    func hide(line: Int?) {
        precondition(Thread.isMainThread)
        if let line, shownLine != line { return }
        shownLine = nil
        guard let panel else { return }
        NSAnimationContext.runAnimationGroup { ctx in
            ctx.duration = 0.35
            panel.animator().alphaValue = 0
        }
    }
}

/// One character of a caption: bubble lettering (fill + stroke + hard shadow)
/// drawn from an attributed string, so each glyph can jitter and sway alone.
final class CaptionGlyphLayer: CALayer {
    private final class Bitmap: NSObject {
        let image: CGImage
        init(_ image: CGImage) { self.image = image }
    }
    private static let bitmaps: NSCache<NSString, Bitmap> = {
        let cache = NSCache<NSString, Bitmap>()
        cache.totalCostLimit = 16 * 1024 * 1024
        cache.countLimit = 512
        return cache
    }()
    var text = ""
    var font = NSFont.systemFont(ofSize: 12)
    var ink = NSColor.white
    var strokeColor = NSColor.black
    var shadowInk = NSColor.black
    var inset: CGFloat = 0

    // As in Oskiewar's glyph atlas, rasterize text once and let the GPU move
    // its image. Explicit contents avoid replaying CoreGraphics text/shadow
    // display lists as each letter scales and sways over the Metal face.
    override func display() {
        let scale = max(1, contentsScale)
        let width = max(1, Int(ceil(bounds.width * scale)))
        let height = max(1, Int(ceil(bounds.height * scale)))
        let key = "\(text)|\(font.fontName)|\(font.pointSize)|\(bounds)|\(scale)|\(ink)|\(strokeColor)|\(shadowInk)" as NSString
        if let cached = Self.bitmaps.object(forKey: key) { contents = cached.image; return }
        guard let ctx = CGContext(data: nil, width: width, height: height,
                                  bitsPerComponent: 8, bytesPerRow: width * 4,
                                  space: CGColorSpace(name: CGColorSpace.sRGB)!,
                                  bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) else { return }
        ctx.scaleBy(x: scale, y: scale)
        draw(in: ctx)
        guard let image = ctx.makeImage() else { return }
        Self.bitmaps.setObject(Bitmap(image), forKey: key, cost: width * height * 4)
        contents = image
    }

    override func draw(in ctx: CGContext) {
        let sh = NSShadow()
        sh.shadowBlurRadius = 0
        sh.shadowOffset = NSSize(width: 2.5, height: -2.5)
        sh.shadowColor = shadowInk
        let attr = NSAttributedString(string: text, attributes: [
            .font: font, .foregroundColor: ink, .strokeColor: strokeColor,
            .strokeWidth: -4.5, .shadow: sh,
        ])
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
        let size = attr.size()
        attr.draw(at: NSPoint(x: (bounds.width - size.width) / 2, y: (bounds.height - size.height) / 2))
        NSGraphicsContext.restoreGraphicsState()
    }
}
