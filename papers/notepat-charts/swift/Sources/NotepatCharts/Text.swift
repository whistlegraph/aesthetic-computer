import AppKit
import CoreGraphics
import CoreText

// MARK: - Font helpers
//
// Pinned to the AC web fonts shipped under
// `system/public/type/webfonts/`. The build script registers them
// at process start so CoreText resolves them by family name. Falls
// back to system mono / system sans if the bundle isn't there.

// AC web fonts have an empty / mojibake'd name table, so `NSFont(name:)`
// can't find them after the font manager registers them. Bypass the
// name lookup entirely: load the file directly via `CGFont`, build a
// `CTFont` at the requested size, and bridge it to `NSFont`.

private var ywftBoldCGFont: CGFont?
private var ywftLightCGFont: CGFont?
private var berkeleyCGFont: CGFont?
private var fontsLoaded = false

func ensureFontsRegistered() {
    guard !fontsLoaded else { return }
    fontsLoaded = true
    let fontDir = repoRoot() + "/system/public/type/webfonts"
    ywftBoldCGFont  = loadCGFont(at: fontDir + "/ywft-processing-bold.ttf")
    ywftLightCGFont = loadCGFont(at: fontDir + "/ywft-processing-light.ttf")
    // Berkeley Mono only ships as woff2 here; CGFont doesn't read woff2,
    // but `CTFontManagerCreateFontDescriptorsFromURL` does. The font
    // manager registration call below ensures the font is available
    // for descriptor matching by font URL fallback. Most macOS systems
    // also have SF Mono / Menlo as a perfectly serviceable fallback.
    if FileManager.default.fileExists(atPath: fontDir + "/BerkeleyMonoVariable-Regular.woff2") {
        let url = URL(fileURLWithPath: fontDir + "/BerkeleyMonoVariable-Regular.woff2") as CFURL
        var err: Unmanaged<CFError>?
        CTFontManagerRegisterFontsForURL(url, .process, &err)
    }
}

private func loadCGFont(at path: String) -> CGFont? {
    guard FileManager.default.fileExists(atPath: path) else { return nil }
    let url = URL(fileURLWithPath: path) as CFURL
    guard let provider = CGDataProvider(url: url) else { return nil }
    return CGFont(provider)
}

private func nsFont(from cgFont: CGFont?, size: CGFloat, fallback: NSFont) -> NSFont {
    guard let cg = cgFont else { return fallback }
    let ct = CTFontCreateWithGraphicsFont(cg, size, nil, nil)
    return ct as NSFont
}

/// Locate the aesthetic-computer repo root by walking up from this file's
/// directory until we find CLAUDE.md. Works regardless of where `swift run`
/// launches from.
func repoRoot() -> String {
    // Try a few likely starting points (current dir, then SOURCE_FILE).
    let fm = FileManager.default
    var dir = fm.currentDirectoryPath
    for _ in 0..<8 {
        if fm.fileExists(atPath: dir + "/CLAUDE.md")
            && fm.fileExists(atPath: dir + "/papers/notepat-charts") {
            return dir
        }
        let parent = (dir as NSString).deletingLastPathComponent
        if parent == dir { break }
        dir = parent
    }
    // Fallback — hardcoded since the build script is what kicks this off.
    return "/Users/jas/aesthetic-computer"
}

/// Display title font (YWFT Processing Bold).
func displayFont(size: CGFloat) -> NSFont {
    ensureFontsRegistered()
    return nsFont(from: ywftBoldCGFont, size: size,
                  fallback: NSFont.systemFont(ofSize: size, weight: .heavy))
}

/// Light/regular body for subtitles. Falls back to Avenir-style sans.
func displayLightFont(size: CGFloat) -> NSFont {
    ensureFontsRegistered()
    return nsFont(from: ywftLightCGFont, size: size,
                  fallback: NSFont.systemFont(ofSize: size, weight: .light))
}

/// Body copy — a quiet sans, NOT YWFT (which is display-only).
func bodyFont(size: CGFloat, bold: Bool = false) -> NSFont {
    return NSFont.systemFont(ofSize: size,
                             weight: bold ? .semibold : .regular)
}

/// Berkeley Mono — used on every key (QWERTY letter + note name) and
/// for the pretitle / pageno chrome.
func berkeleyMono(size: CGFloat, weight: NSFont.Weight = .regular) -> NSFont {
    ensureFontsRegistered()
    // woff2 isn't readable via CGFont; rely on the manager-registered
    // font being matched by family name, fallback to system mono.
    let candidates = ["Berkeley Mono", "Berkeley Mono Variable", "BerkeleyMono"]
    for name in candidates {
        if let f = NSFont(name: name, size: size) {
            if weight == .bold || weight == .semibold || weight == .heavy {
                return NSFontManager.shared.convert(f, toHaveTrait: .boldFontMask)
            }
            return f
        }
    }
    return NSFont.monospacedSystemFont(ofSize: size, weight: weight)
}

// MARK: - Text helpers

func drawCenteredText(in ctx: CGContext,
                      text: String,
                      center: CGPoint,
                      font: NSFont,
                      color: NSColor) {
    let attrs: [NSAttributedString.Key: Any] = [
        .font: font,
        .foregroundColor: color,
    ]
    let attr = NSAttributedString(string: text, attributes: attrs)
    let size = attr.size()
    let rect = CGRect(x: center.x - size.width / 2,
                      y: center.y - size.height / 2,
                      width: size.width,
                      height: size.height)
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
    attr.draw(in: rect)
    NSGraphicsContext.restoreGraphicsState()
}

/// Draw `text` in a left-aligned box. Returns the y of the bottom of
/// the laid-out content (so callers can stack paragraphs).
@discardableResult
func drawWrappedText(in ctx: CGContext,
                     attr: NSAttributedString,
                     rect: CGRect) -> CGFloat {
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
    let framesetter = CTFramesetterCreateWithAttributedString(attr)
    let path = CGPath(rect: rect, transform: nil)
    let frame = CTFramesetterCreateFrame(framesetter,
                                         CFRange(location: 0, length: attr.length),
                                         path,
                                         nil)
    CTFrameDraw(frame, ctx)
    NSGraphicsContext.restoreGraphicsState()
    return rect.minY
}
