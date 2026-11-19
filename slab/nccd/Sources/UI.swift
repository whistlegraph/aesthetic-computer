// UI.swift — the look of NCCd: a mint sheet, one monospace face, dense rows.
//
// Two screens. The gate asks for the NCC login and the Signal link. The main
// screen is three columns: the rail of circles, the circle's sheet (heading,
// roles, message ledger, composer), and that circle's wiki pages.

import AppKit
import CoreImage

enum Theme {
    // Every colour is a light/dark pair; AppKit picks at draw time.
    static func pair(_ light: String, _ dark: String, alpha: CGFloat = 1) -> NSColor {
        NSColor(name: nil) { ap in
            let isDark = ap.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
            return NSColor(hex: isDark ? dark : light)!.withAlphaComponent(alpha)
        }
    }
    // The window is flat planes of colour, edge to edge: the rail, the sheet
    // (a wash of the open circle's colour), the wiki column. Dark is not black.
    static let mint = pair("BFE8CF", "0B3A4A")      // the rail's plane: mint, or deep teal
    static let paper = pair("F4FBF6", "123F47")     // a neutral sheet (the gate's tiles)
    static let wikiPaper = pair("DCD3FA", "3A2A6E") // the wiki column's plane: lavender, or plum
    static let wikiBand = pair("6B4FD8", "8E6BFF")  // its heading band
    static let chip = pair("FFFFFF", "000000", alpha: 0.22)   // code and table grounds, on any wash

    // A circle's colour as a full plane: mostly the colour's own hue, light
    // enough (or in the dark theme deep enough) to read text on.
    static func wash(_ c: NSColor) -> NSColor {
        NSColor(name: nil) { ap in
            let isDark = ap.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
            var flat = c
            ap.performAsCurrentDrawingAppearance { flat = c.usingColorSpace(.sRGB) ?? c }
            return flat.blended(withFraction: isDark ? 0.66 : 0.74, of: isDark ? NSColor(hex: "0A1C26")! : .white) ?? flat
        }
    }
    static let ink = pair("1B2B22", "E4FBEF")
    static let green = pair("2E7D4F", "12946A")     // heading band
    static let me = pair("2E7D4F", "00D080")        // your own name: the site green
    static let teal = pair("006888", "4FD8FF")      // wordmark and links: the wiki's two logo colours
    // A link must not pass for text on any wash: deep blue, or in the dark theme a warm yellow.
    static let link = pair("0030B0", "FFE066")
    static let word = pair("006888", "00D080")         // the wordmark's NCC: never the donut's cyan (teal, or in the dark the club green)
    static let search = pair("F2A93B", "E0932A")       // the search tab: amber
    static let login = pair("9FB2C8", "5B6E86")        // the login tab: slate
    static let code = pair("F2924A", "C96A22")         // the code tab: the forge's orange
    static let edge: CGFloat = 12                      // the one margin: every plane's contents stand this far in
    static let yellow = NSColor(hex: "F3D34A")!
    static let onYellow = NSColor(hex: "1B2B22")!
    static let faint = pair("1B2B22", "D2F3E1", alpha: 0.62)

    // The club's own colours, for a circle the wiki gives none: site green and
    // cyan, the orange dot of NELA.Computer, the pencil blue, pink, violet.
    static let palette = ["00D080", "00C4FD", "F07C00", "0A84C1", "E0127F", "9932CC"]
    static func circleColor(_ title: String, wiki: String?) -> NSColor {
        if let wiki, let c = NSColor(hex: wiki) { return c }
        let n = title.unicodeScalars.reduce(0) { ($0 &* 31 &+ Int($1.value)) % 9973 }
        return NSColor(hex: palette[n % palette.count])!
    }
    // Dark ink or white, whichever reads better on this colour (by WCAG contrast).
    static func on(_ c: NSColor) -> NSColor {
        guard let s = c.usingColorSpace(.sRGB) else { return .white }
        func lin(_ v: CGFloat) -> CGFloat { v <= 0.03928 ? v / 12.92 : pow((v + 0.055) / 1.055, 2.4) }
        let l = 0.2126 * lin(s.redComponent) + 0.7152 * lin(s.greenComponent) + 0.0722 * lin(s.blueComponent)
        let dark: CGFloat = 0.012            // the ink's own luminance
        return (l + 0.05) / (dark + 0.05) >= 1.05 / (l + 0.05) ? NSColor(hex: "0B1A12")! : .white
    }

    // "system", "light" or "dark"; kept in the app's defaults.
    static func apply(_ mode: String) {
        NSApp.appearance = mode == "light" ? NSAppearance(named: .aqua)
            : mode == "dark" ? NSAppearance(named: .darkAqua) : nil
    }

    // One text size across every view: the tabs, the roles, the messages, the
    // wiki and its headings. Any size asked for between 9 and 13 is the one
    // size (a heading is told apart by weight and colour, not by size).
    static let size: CGFloat = 12
    static func mono(_ size: CGFloat, bold: Bool = false) -> NSFont {
        .monospacedSystemFont(ofSize: (9...13).contains(size) ? Self.size : size, weight: bold ? .bold : .regular)
    }
    static func mono(weight: NSFont.Weight) -> NSFont { .monospacedSystemFont(ofSize: Self.size, weight: weight) }
    // Club artwork is optional and lives outside the repo.
    static func asset(_ name: String) -> NSImage? {
        guard let dir = Config.shared.conf.assetsDir else { return nil }
        return NSImage(contentsOf: Paths.expand(dir).appendingPathComponent(name))
    }

    // A tab's icon: a smooth vector drawing (an SF Symbol), one tone on its
    // ground. The wordmark and the login badges stay in pixels.
    static func tabIcon(_ title: String, size: CGFloat = 13) -> NSImage? {
        let t = title.lowercased()
        let name = t == "home" ? "house.fill" : t == "window" ? "macwindow" : t == "wiki" ? "book.fill"
            : t == "search" ? "magnifyingglass" : t == "login" ? "key.fill" : t == "code" ? "chevron.left.forwardslash.chevron.right"
            : t.contains("steer") ? "steeringwheel" : t.contains("infra") ? "desktopcomputer"
            : t.contains("announce") ? "megaphone.fill" : t.contains("chat") ? "bubble.left.and.bubble.right.fill"
            : t.contains("event") ? "calendar" : t.contains("label") || t.contains("music") ? "music.note" : "circle.dashed"
        let img = (NSImage(systemSymbolName: name, accessibilityDescription: title)
                   ?? NSImage(systemSymbolName: "circle.dashed", accessibilityDescription: title))?
            .withSymbolConfiguration(.init(pointSize: size, weight: .semibold))
        img?.isTemplate = true
        return img
    }
}

// Signal's own logo (the path from the Simple Icons set), where the app
// speaks of Signal: the Link Signal button and the badge at the foot. It is
// Signal's trademark; it is used here to name their service, and NCCd is not
// affiliated with Signal.
enum Brand {
    static let signalPath = "M12 0q-.934 0-1.83.139l.17 1.111a11 11 0 0 1 3.32 0l.172-1.111A12 12 0 0 0 12 0M9.152.34A12 12 0 0 0 5.77 1.742l.584.961a10.8 10.8 0 0 1 3.066-1.27zm5.696 0-.268 1.094a10.8 10.8 0 0 1 3.066 1.27l.584-.962A12 12 0 0 0 14.848.34M12 2.25a9.75 9.75 0 0 0-8.539 14.459c.074.134.1.292.064.441l-1.013 4.338 4.338-1.013a.62.62 0 0 1 .441.064A9.7 9.7 0 0 0 12 21.75c5.385 0 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25m-7.092.068a12 12 0 0 0-2.59 2.59l.909.664a11 11 0 0 1 2.345-2.345zm14.184 0-.664.909a11 11 0 0 1 2.345 2.345l.909-.664a12 12 0 0 0-2.59-2.59M1.742 5.77A12 12 0 0 0 .34 9.152l1.094.268a10.8 10.8 0 0 1 1.269-3.066zm20.516 0-.961.584a10.8 10.8 0 0 1 1.27 3.066l1.093-.268a12 12 0 0 0-1.402-3.383M.138 10.168A12 12 0 0 0 0 12q0 .934.139 1.83l1.111-.17A11 11 0 0 1 1.125 12q0-.848.125-1.66zm23.723.002-1.111.17q.125.812.125 1.66c0 .848-.042 1.12-.125 1.66l1.111.172a12.1 12.1 0 0 0 0-3.662M1.434 14.58l-1.094.268a12 12 0 0 0 .96 2.591l-.265 1.14 1.096.255.36-1.539-.188-.365a10.8 10.8 0 0 1-.87-2.35m21.133 0a10.8 10.8 0 0 1-1.27 3.067l.962.584a12 12 0 0 0 1.402-3.383zm-1.793 3.848a11 11 0 0 1-2.345 2.345l.664.909a12 12 0 0 0 2.59-2.59zm-19.959 1.1L.357 21.48a1.8 1.8 0 0 0 2.162 2.161l1.954-.455-.256-1.095-1.953.455a.675.675 0 0 1-.81-.81l.454-1.954zm16.832 1.769a10.8 10.8 0 0 1-3.066 1.27l.268 1.093a12 12 0 0 0 3.382-1.402zm-10.94.213-1.54.36.256 1.095 1.139-.266c.814.415 1.683.74 2.591.961l.268-1.094a10.8 10.8 0 0 1-2.35-.869zm3.634 1.24-.172 1.111a12.1 12.1 0 0 0 3.662 0l-.17-1.111q-.812.125-1.66.125a11 11 0 0 1-1.66-.125"
    // Authentik's logo (the same set): the club's login is an Authentik page.
    static let authentikPath = "M13.96 9.01h-.84V7.492h-1.234v3.663H5.722c.34.517.538.982.538 1.152 0 .46-1.445 3.059-3.197 3.059C.8 15.427-.745 12.8.372 10.855a3.062 3.062 0 0 1 2.691-1.606c1.04 0 1.971.915 2.557 1.755V6.577a3.773 3.773 0 0 1 3.77-3.769h10.84C22.31 2.808 24 4.5 24 6.577v10.845a3.773 3.773 0 0 1-3.77 3.769h-1.6V17.5h-7.64v3.692h-1.6a3.773 3.773 0 0 1-3.77-3.769v-3.41h12.114v-6.52h-1.59v.893h-.84v-.893H13.96v1.516Zm-9.956 1.845c-.662-.703-1.578-.544-2.209 0-2.105 2.054 1.338 5.553 3.302 1.447a5.395 5.395 0 0 0-1.093-1.447Z"
    static func authentik(size: CGFloat) -> NSImage? { mark(authentikPath, size) }
    private static func mark(_ path: String, _ size: CGFloat) -> NSImage? {
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" width=\"\(size)\" height=\"\(size)\"><path d=\"\(path)\"/></svg>"
        guard let img = NSImage(data: Data(svg.utf8)) else { return nil }
        img.size = NSSize(width: size, height: size)
        img.isTemplate = true
        return img
    }
    // The club's own mark: "NCC" in the wordmark's pixels, one tone (a template).
    static func ncc(cell: CGFloat) -> NSImage {
        let img = NSImage(size: NSSize(width: 18 * cell, height: 7 * cell), flipped: true) { _ in
            NSGraphicsContext.current?.shouldAntialias = false
            NSColor.black.setFill()
            var x0 = 0
            for letter in NCCLogo.letters.prefix(3) {
                for (y, row) in letter.rows.enumerated() {
                    for (x, ch) in row.enumerated() where ch == "X" {
                        NSRect(x: CGFloat(x0 + x) * cell, y: CGFloat(y) * cell, width: cell + 0.2, height: cell + 0.2).fill()
                    }
                }
                x0 += letter.rows[0].count + 1
            }
            return true
        }
        img.isTemplate = true
        return img
    }
    static func signal(size: CGFloat) -> NSImage? {
        let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" width=\"\(size)\" height=\"\(size)\"><path d=\"\(signalPath)\"/></svg>"
        guard let img = NSImage(data: Data(svg.utf8)) else { return Pixel.icon("signal", size: size) }
        img.size = NSSize(width: size, height: size)
        img.isTemplate = true
        return img
    }
}

// Eight-by-eight pixel pictures. An image made here is a template: the view
// that shows it gives it its one colour.
enum Pixel {
    static let art: [String: [String]] = [
        "donut":    [".XXXXXX.", "XXXXXXXX", "XXX..XXX", "XX....XX", "XX....XX", "XXX..XXX", "XXXXXXXX", ".XXXXXX."],
        "bubble":   [".XXXXXX.", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", ".XXXXXX.", "..XX....", ".XX....."],
        "horn":     ["......XX", "....XXXX", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", "....XXXX", ".XX...XX", ".XX....."],
        "computer": ["XXXXXXXX", "X......X", "X......X", "X......X", "XXXXXXXX", "...XX...", "..XXXX..", "XXXXXXXX"],
        "calendar": [".X....X.", "XXXXXXXX", "XXXXXXXX", "X......X", "X.XX.X.X", "X......X", "X.X.XX.X", "XXXXXXXX"],
        "note":     ["...XXXXX", "...XXXXX", "...X...X", "...X...X", "...X...X", ".XXX.XXX", "XXXX.XXX", ".XX...X."],
        "helm":     ["...XX...", "X.XXXX.X", ".XX..XX.", "XX....XX", "XX....XX", ".XX..XX.", "X.XXXX.X", "...XX..."],
        "book":     ["XXX..XXX", "X.XXXX.X", "X..XX..X", "X..XX..X", "X..XX..X", "X..XX..X", "XXXXXXXX", "...XX..."],
        "ring":     ["..XXXX..", ".X....X.", "X......X", "X......X", "X......X", "X......X", ".X....X.", "..XXXX.."],
        "check":    ["........", ".......X", "......XX", "X....XX.", "XX..XX..", ".XXXX...", "..XX....", "........"],
        "box":      ["XXXXXXXX", "X......X", "X......X", "X......X", "X......X", "X......X", "X......X", "XXXXXXXX"],
        "home":     ["...XX...", "..XXXX..", ".XXXXXX.", "XXXXXXXX", ".XXXXXX.", ".XX..XX.", ".XX..XX.", ".XXXXXX."],
        "send":     ["...XX...", "..XXXX..", ".XXXXXX.", "XXXXXXXX", "...XX...", "...XX...", "...XX...", "...XX..."],
        "window":   ["XXXXXXXX", "XXXXXXXX", "X......X", "X......X", "X......X", "X......X", "XXXXXXXX", "........"],
        "n":        ["XX....XX", "XXX...XX", "XXXX..XX", "XX.XX.XX", "XX..XXXX", "XX...XXX", "XX....XX", "........"],
        // Signal's mark, in pixels: a round speech bubble with its tail at the bottom left
        "signal":   ["..XXXX..", ".XXXXXX.", "XXXXXXXX", "XXXXXXXX", "XXXXXXXX", ".XXXXXX.", "XXXXXX..", "XX......"],
    ]
    // `size` should be a multiple of 4 points, so that each of the 8 pixels
    // across is a whole number of device pixels; the picture is then filled
    // with no smoothing, run by run, and no seams show between its squares.
    static func icon(_ name: String, size: CGFloat = 16) -> NSImage? {
        guard let rows = art[name] else { return nil }
        let img = NSImage(size: NSSize(width: size, height: size), flipped: true) { _ in
            let c = size / 8
            NSGraphicsContext.current?.shouldAntialias = false
            NSColor.black.setFill()
            for (y, row) in rows.enumerated() {
                var x = 0
                let cells = Array(row)
                while x < 8 {
                    guard cells[x] == "X" else { x += 1; continue }
                    var end = x
                    while end < 8, cells[end] == "X" { end += 1 }
                    NSRect(x: CGFloat(x) * c, y: CGFloat(y) * c, width: CGFloat(end - x) * c, height: c + 0.25).fill()
                    x = end
                }
            }
            return true
        }
        img.isTemplate = true
        return img
    }
}

func label(_ s: String, _ font: NSFont, _ color: NSColor = Theme.ink) -> NSTextField {
    let l = NSTextField(labelWithString: s)
    l.font = font
    l.textColor = color
    l.lineBreakMode = .byTruncatingTail
    return l
}

// A fine grain, like paper, laid over a plane of flat colour.
enum Grain {
    static let tile: NSImage = {
        let n = 96
        let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: n, pixelsHigh: n, bitsPerSample: 8, samplesPerPixel: 4,
                                   hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
        var seed: UInt64 = 0xD0D0
        for y in 0..<n {
            for x in 0..<n {
                seed = seed &* 6364136223846793005 &+ 1442695040888963407
                let v = CGFloat((seed >> 40) % 1000) / 1000
                // a speck of light or of dark, most of them very faint
                let w: CGFloat = v > 0.5 ? 1 : 0
                rep.setColor(NSColor(deviceRed: w, green: w, blue: w, alpha: abs(v - 0.5) * 0.11), atX: x, y: y)
            }
        }
        let img = NSImage(size: NSSize(width: n / 2, height: n / 2))
        img.addRepresentation(rep)
        return img
    }()
    static func draw(in r: NSRect) {
        NSColor(patternImage: tile).setFill()
        r.fill(using: .sourceOver)
    }
}

final class Fill: NSView {
    var color: NSColor { didSet { needsDisplay = true } }
    var grain = false
    var radius: CGFloat = 0
    init(_ c: NSColor, radius: CGFloat = 0) {
        color = c
        self.radius = radius
        super.init(frame: .zero)
        translatesAutoresizingMaskIntoConstraints = false
    }
    required init?(coder: NSCoder) { fatalError() }
    override func draw(_ r: NSRect) {
        color.setFill()
        NSBezierPath(roundedRect: bounds, xRadius: radius, yRadius: radius).fill()
        if grain { Grain.draw(in: bounds) }
    }
}

func pin(_ v: NSView, to p: NSView, _ inset: CGFloat = 0) {
    v.translatesAutoresizingMaskIntoConstraints = false
    p.addSubview(v)
    NSLayoutConstraint.activate([
        v.leadingAnchor.constraint(equalTo: p.leadingAnchor, constant: inset),
        v.trailingAnchor.constraint(equalTo: p.trailingAnchor, constant: -inset),
        v.topAnchor.constraint(equalTo: p.topAnchor, constant: inset),
        v.bottomAnchor.constraint(equalTo: p.bottomAnchor, constant: -inset),
    ])
}

// A round badge: the member's picture, or their initial on their colour.
func badge(_ p: Person, size: CGFloat) -> NSImage {
    let img = NSImage(size: NSSize(width: size, height: size))
    img.lockFocus()
    let r = NSRect(x: 0, y: 0, width: size, height: size)
    NSBezierPath(ovalIn: r).addClip()
    if let b = p.badge {
        b.draw(in: r, from: .zero, operation: .sourceOver, fraction: 1)
    } else {
        (p.color ?? Theme.green).setFill()
        r.fill()
        let s = NSAttributedString(string: String(p.name.prefix(1)).uppercased(), attributes: [
            .font: Theme.mono(size * 0.55, bold: true), .foregroundColor: NSColor.white])
        let sz = s.size()
        s.draw(at: NSPoint(x: (size - sz.width) / 2, y: (size - sz.height) / 2))
    }
    img.unlockFocus()
    return img
}

// The app's wordmark, "NCCd", in chunky pixels set close together: the club's
// NCC in the wiki's teal and a small d (for donut) in the icing's cyan, with
// sprinkles on it. Each letter stands on a darker copy of itself, a pixel down
// and across. The d rocks a little now and then, and a band of light crosses
// the whole word every few seconds.
final class NCCLogo: NSView {
    static let letters: [(rows: [String], pink: Bool)] = [
        (["XX..XX", "XXX.XX", "XXXXXX", "XXXXXX", "XX.XXX", "XX..XX", "XX..XX"], false),
        ([".XXXX", "XXXXX", "XX...", "XX...", "XX...", "XXXXX", ".XXXX"], false),
        ([".XXXX", "XXXXX", "XX...", "XX...", "XX...", "XXXXX", ".XXXX"], false),
        (["...XX", "...XX", ".XXXX", "XXXXX", "XX.XX", "XXXXX", ".XXXX"], true),
    ]
    static let columns = 24, rows = 7          // 6 + 5 + 5 + 5 letters and three gaps of one
    static var teal: NSColor { Theme.teal }

    // Some of the word's letters, drawn in pixels.
    private final class Word: NSView {
        let cell: CGFloat, parts: [(rows: [String], pink: Bool)]
        init(cell: CGFloat, parts: [(rows: [String], pink: Bool)]) {
            self.cell = cell
            self.parts = parts
            super.init(frame: .zero)
            wantsLayer = true
        }
        required init?(coder: NSCoder) { fatalError() }
        var pixels: [(rect: NSRect, pink: Bool)] {
            var out: [(NSRect, Bool)] = []
            var x0 = 0
            let top = (CGFloat(NCCLogo.rows) + 0.5) * cell
            for letter in parts {
                for (y, row) in letter.rows.enumerated() {
                    for (x, ch) in row.enumerated() where ch == "X" {
                        out.append((NSRect(x: CGFloat(x0 + x) * cell, y: top - CGFloat(y + 1) * cell, width: cell, height: cell), letter.pink))
                    }
                }
                x0 += letter.rows[0].count + 1
            }
            return out
        }
        override func draw(_ r: NSRect) {
            NSGraphicsContext.current?.shouldAntialias = false
            let all = pixels
            let drop = cell * 0.5
            for (rect, pink) in all {                // the copy underneath, darker
                ((pink ? Donut.icing : Theme.word).blended(withFraction: 0.55, of: .black) ?? .black).setFill()
                rect.offsetBy(dx: drop, dy: -drop).fill()
            }
            // The d's bowl is the donut: icing, with sprinkles. Its stem (the two
            // rows above the bowl) is a deeper hue and bare, so the donut stands out.
            // Also bare and deeper are the bowl's two right corners, where the stem
            // meets it: without them the icing is a ring, a circle of pixels.
            let top = (CGFloat(NCCLogo.rows) + 0.5) * cell
            func isStem(_ rect: NSRect) -> Bool {
                let col = Int((rect.minX / cell).rounded()), row = Int(((top - rect.maxY) / cell).rounded())
                return row < 2 || (col == 4 && (row == 2 || row == 6))
            }
            for (rect, pink) in all {
                (!pink ? Theme.word : isStem(rect) ? Donut.stem : Donut.icing).setFill()
                rect.insetBy(dx: -0.2, dy: -0.2).fill()
            }
            for (n, (rect, pink)) in all.enumerated() where pink && !isStem(rect) && (n * 7 + 3) % 3 == 0 {
                NSColor(hex: Theme.palette[(n * 5 + 1) % Theme.palette.count])!.setFill()
                let w = cell * 0.5, across = (n % 2 == 0)
                NSRect(x: rect.minX + cell * (n % 3 == 0 ? 0.15 : 0.4), y: rect.minY + cell * (n % 5 == 0 ? 0.5 : 0.2),
                       width: across ? w : w / 2, height: across ? w / 2 : w).fill()
            }
        }
    }

    private let cell: CGFloat
    private let ncc: Word, d: Word
    private let shine = CAGradientLayer()

    // `scale` is the size of one pixel of the word, in points.
    init(scale: CGFloat) {
        cell = scale
        ncc = Word(cell: scale, parts: Array(Self.letters.prefix(3)))
        d = Word(cell: scale, parts: [Self.letters[3]])
        super.init(frame: .zero)
        translatesAutoresizingMaskIntoConstraints = false
        let high = (CGFloat(Self.rows) + 0.5) * cell
        widthAnchor.constraint(equalToConstant: (CGFloat(Self.columns) + 0.5) * cell).isActive = true
        heightAnchor.constraint(equalToConstant: high).isActive = true
        wantsLayer = true
        ncc.frame = NSRect(x: 0, y: 0, width: 18.5 * cell, height: high)
        d.frame = NSRect(x: 19 * cell, y: 0, width: 5.5 * cell, height: high)
        addSubview(ncc)
        addSubview(d)
        // the d rocks on its foot, a few quick swings and then a rest
        let rock = CAKeyframeAnimation(keyPath: "transform.rotation.z")
        rock.values = [0, 0.09, -0.07, 0.05, -0.03, 0]
        rock.duration = 0.7
        let pause = CAAnimationGroup()
        pause.animations = [rock]
        pause.duration = 4.2
        pause.repeatCount = .infinity
        d.layer?.add(pause, forKey: "rock")
        // the light: a slanted band that crosses the word, cut to the letters' own shape
        shine.colors = [NSColor.clear.cgColor, NSColor(white: 1, alpha: 0.75).cgColor, NSColor.clear.cgColor]
        shine.startPoint = CGPoint(x: 0, y: 0.35)
        shine.endPoint = CGPoint(x: 1, y: 0.65)
        shine.locations = [-0.3, -0.15, 0]
        let mask = CAShapeLayer()
        let path = CGMutablePath()
        for p in ncc.pixels { path.addRect(p.rect) }
        for p in d.pixels { path.addRect(p.rect.offsetBy(dx: 19 * cell, dy: 0)) }
        mask.path = path
        shine.mask = mask
        let sweep = CABasicAnimation(keyPath: "locations")
        sweep.fromValue = [-0.3, -0.15, 0]
        sweep.toValue = [1, 1.15, 1.3]
        sweep.duration = 1.1
        let rest = CAAnimationGroup()
        rest.animations = [sweep]
        rest.duration = 5.5                     // one pass, then a rest
        rest.repeatCount = .infinity
        shine.add(rest, forKey: "shine")
    }
    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        super.layout()
        if shine.superlayer == nil { layer?.addSublayer(shine) }
        shine.frame = bounds
        shine.mask?.frame = bounds
    }
    override func hitTest(_ point: NSPoint) -> NSView? { nil }      // the tab it sits on takes the click
}

// The folder-tab shape every tab of the rail shares: rounded at its outer
// (left) corners, flush with the pane at its right, a line of light along the
// top and of shade along the foot. A tab that is not open stands a little
// proud, like a button: lighter at the top, darker at the foot, and in the
// pane's shadow at its right end.
enum Tab {
    // `top`: the tabs stand in a row along the top of the window, like a
    // browser's or a file folder's: rounded at their upper corners, flush
    // with the pane at their foot. Otherwise they stand down the side.
    static func draw(in bounds: NSRect, fill: NSColor, open: Bool, top: Bool = false, hover: Bool = false) {
        let r: CGFloat = top ? 7 : 5
        let body = top ? NSRect(x: 0, y: 0, width: bounds.width, height: bounds.height - 3)
                       : NSRect(x: 6, y: 0, width: bounds.width - 6, height: bounds.height)
        let path = NSBezierPath()
        if top {
            path.move(to: NSPoint(x: body.minX, y: body.minY))
            path.line(to: NSPoint(x: body.minX, y: body.maxY - r))
            path.appendArc(withCenter: NSPoint(x: body.minX + r, y: body.maxY - r), radius: r, startAngle: 180, endAngle: 90, clockwise: true)
            path.line(to: NSPoint(x: body.maxX - r, y: body.maxY))
            path.appendArc(withCenter: NSPoint(x: body.maxX - r, y: body.maxY - r), radius: r, startAngle: 90, endAngle: 0, clockwise: true)
            path.line(to: NSPoint(x: body.maxX, y: body.minY))
        } else {
            path.move(to: NSPoint(x: body.maxX, y: body.minY))
            path.line(to: NSPoint(x: body.minX + r, y: body.minY))
            path.appendArc(withCenter: NSPoint(x: body.minX + r, y: body.minY + r), radius: r, startAngle: 270, endAngle: 180, clockwise: true)
            path.line(to: NSPoint(x: body.minX, y: body.maxY - r))
            path.appendArc(withCenter: NSPoint(x: body.minX + r, y: body.maxY - r), radius: r, startAngle: 180, endAngle: 90, clockwise: true)
            path.line(to: NSPoint(x: body.maxX, y: body.maxY))
        }
        path.close()
        fill.setFill()
        path.fill()
        NSGraphicsContext.saveGraphicsState()
        path.addClip()
        Grain.draw(in: body)
        if hover, !open {                       // under the pointer, a tab that is not open lights up
            NSColor(white: 1, alpha: 0.22).setFill()
            body.fill(using: .sourceOver)
        }
        if !open {
            NSGradient(colors: [NSColor(white: 0, alpha: 0.14), NSColor(white: 1, alpha: 0.12)])?.draw(in: body, angle: 90)
            // a tab that is not open lies in the pane's shadow, at the edge where it meets the pane
            if top {
                NSGradient(colors: [NSColor(white: 0, alpha: 0.22), NSColor(white: 0, alpha: 0)])?
                    .draw(in: NSRect(x: body.minX, y: body.minY, width: body.width, height: 8), angle: 90)
            } else {
                NSGradient(colors: [NSColor(white: 0, alpha: 0), NSColor(white: 0, alpha: 0.2)])?
                    .draw(in: NSRect(x: body.maxX - 10, y: body.minY, width: 10, height: body.height), angle: 0)
            }
        }
        NSColor(white: 1, alpha: open ? 0.5 : 0.34).setFill()
        NSRect(x: body.minX, y: body.maxY - 1, width: body.width, height: 1).fill()
        if !top {
            NSColor(white: 0, alpha: open ? 0.08 : 0.26).setFill()
            NSRect(x: body.minX, y: body.minY, width: body.width, height: 1).fill()
        }
        NSGraphicsContext.restoreGraphicsState()
    }
}

// The unibar: with the tabs "on top", the top of the window is one bar. It
// says where you are (the open place's mark and name, in that place's colour)
// and how much is unread elsewhere. Tap it anywhere and the list of every
// place drops down under it; tap a place to go there.
final class Unibar: NSView {
    var onClick: (() -> Void)?
    var title = "donut" { didSet { needsDisplay = true } }
    var glyph: NSImage? { didSet { needsDisplay = true } }
    var color: NSColor = Donut.icing { didSet { needsDisplay = true } }
    var unread = 0 { didSet { needsDisplay = true } }
    var open = false { didSet { needsDisplay = true } }
    // Until one login is done there is nowhere to go: no arrow, and a press does nothing.
    var locked = false { didSet { needsDisplay = true; window?.invalidateCursorRects(for: self) } }
    // While a login is under way the bar is the way back out of it.
    var backAction: (() -> Void)? { didSet { needsDisplay = true; window?.invalidateCursorRects(for: self) } }
    private var hover = false { didSet { if hover != oldValue { needsDisplay = true } } }

    override func draw(_ dirty: NSRect) {
        Tab.draw(in: NSRect(x: -10, y: 0, width: bounds.width + 20, height: bounds.height + 13), fill: color, open: true, top: true)
        if let _ = backAction {
            if hover { NSColor(white: 1, alpha: 0.16).setFill(); bounds.fill(using: .sourceOver) }
            // a large arrow, and no word
            let arrow = NSImage(systemSymbolName: "arrow.left", accessibilityDescription: "Back")?
                .withSymbolConfiguration(.init(pointSize: 20, weight: .heavy))
            if let arrow {
                let tinted = NSImage(size: arrow.size, flipped: false) { r in
                    arrow.draw(in: r)
                    Theme.ink.set()
                    r.fill(using: .sourceAtop)
                    return true
                }
                tinted.draw(in: NSRect(x: 12, y: bounds.midY - arrow.size.height / 2, width: arrow.size.width, height: arrow.size.height))
            }
            return
        }
        if (hover || open) && !locked {
            NSColor(white: 1, alpha: 0.16).setFill()
            bounds.fill(using: .sourceOver)
        }
        let on = Theme.ink
        var x: CGFloat = 12
        let mid = bounds.midY
        if let glyph {
            let size = glyph.size
            let tinted = NSImage(size: size, flipped: false) { r in
                glyph.draw(in: r)
                on.set()
                r.fill(using: .sourceAtop)
                return true
            }
            tinted.draw(in: NSRect(x: x, y: mid - size.height / 2, width: size.width, height: size.height))
            x += size.width + 8
        }
        if !title.isEmpty {
            let name = NSAttributedString(string: title, attributes: [.font: Theme.mono(weight: .heavy), .foregroundColor: on])
            name.draw(at: NSPoint(x: x, y: mid - name.size().height / 2))
            x += name.size().width + 8
        }
        if !locked {
            let arrow = NSAttributedString(string: open ? "▴" : "▾", attributes: [.font: Theme.mono(11, bold: true), .foregroundColor: on])
            arrow.draw(at: NSPoint(x: x, y: mid - arrow.size().height / 2))
        }
        if unread > 0 {
            let n = NSAttributedString(string: "\(unread)", attributes: [.font: Theme.mono(11, bold: true), .foregroundColor: Theme.onYellow])
            let pill = NSRect(x: bounds.maxX - n.size().width - 26, y: mid - 9, width: n.size().width + 16, height: 18)
            Theme.yellow.setFill()
            NSBezierPath(roundedRect: pill, xRadius: 9, yRadius: 9).fill()
            n.draw(at: NSPoint(x: pill.minX + 8, y: pill.midY - n.size().height / 2))
        }
    }
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect], owner: self))
    }
    override func mouseEntered(with event: NSEvent) { hover = true }
    override func mouseExited(with event: NSEvent) { hover = false }
    override func mouseDown(with event: NSEvent) {
        if let backAction { return backAction() }
        if !locked { onClick?() }
    }
    override func resetCursorRects() { if !locked || backAction != nil { addCursorRect(bounds, cursor: .pointingHand) } }
}

// The first tab of the rail: the wordmark, with a house to its left. It is the
// way to the donut, and when the donut is open it is the donut's own tab, in
// the colour of the donut's pane.
final class LogoTab: NSView {
    private let word = NCCLogo(scale: 3.5)
    private let home = NSImageView()
    var onClick: (() -> Void)?
    var selected = false { didSet { needsDisplay = true; home.contentTintColor = selected ? Theme.ink : Theme.faint } }
    var onTop = false { didSet { tallness.constant = onTop ? 48 : Self.tall; arrange() } }
    var small = false { didSet { arrange() } }                  // no room for the word: the house alone
    private var afterHome: NSLayoutConstraint!, atEdge: NSLayoutConstraint!
    // Down the side the house stands left of the word. On top the word stands alone
    // (the bar beside it says where you are), and in a narrow window the house stands for it.
    private func arrange() {
        word.isHidden = false
        home.isHidden = true                 // no house: the wordmark itself is the way home
        afterHome.isActive = false
        atEdge.isActive = false
        (home.isHidden ? atEdge : afterHome)?.isActive = true
        needsDisplay = true
    }
    var barColor: NSColor? { didSet { needsDisplay = true } }   // on top: the wordmark's end of the unibar
    private var tallness: NSLayoutConstraint!
    static let tall: CGFloat = 46

    init() {
        super.init(frame: .zero)
        translatesAutoresizingMaskIntoConstraints = false
        home.image = Theme.tabIcon("home", size: 12)
        home.imageScaling = .scaleNone
        home.translatesAutoresizingMaskIntoConstraints = false
        addSubview(home)
        addSubview(word)
        tallness = heightAnchor.constraint(equalToConstant: Self.tall)
        NSLayoutConstraint.activate([
            tallness,
            home.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 15),
            home.centerYAnchor.constraint(equalTo: centerYAnchor),
            home.widthAnchor.constraint(equalToConstant: 15), home.heightAnchor.constraint(equalToConstant: 14),
            word.centerYAnchor.constraint(equalTo: centerYAnchor, constant: -1),
        ])
        afterHome = word.leadingAnchor.constraint(equalTo: home.trailingAnchor, constant: 7)
        atEdge = word.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 12)
        afterHome.isActive = true
        home.contentTintColor = Theme.faint
    }
    required init?(coder: NSCoder) { fatalError() }
    override func draw(_ dirty: NSRect) {
        // on top it is the left end of one unbroken bar: no corners, no gap
        Tab.draw(in: onTop ? NSRect(x: -10, y: 0, width: bounds.width + 20, height: bounds.height + 10) : bounds,
                 fill: barColor ?? (selected ? Theme.wash(Donut.icing) : Donut.icing.withAlphaComponent(0.3)),
                 open: selected, top: onTop, hover: hover)
    }
    // The pointer over the tab.
    private var hover = false { didSet { if hover != oldValue { needsDisplay = true } } }
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect], owner: self))
    }
    override func mouseEntered(with event: NSEvent) { hover = true }
    override func mouseExited(with event: NSEvent) { hover = false }

    override func mouseDown(with event: NSEvent) { onClick?() }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
}

// ─── login panel ────────────────────────────────────────────────────────────
// The Login tab: two large buttons, one for each login, each in its service's
// own colour, side by side in a wide window and one over the other in a narrow
// one. The whole pane is the button: its mark, and under it one line that says
// what a press does and what that opens ("Link Signal to chat"), or once in,
// the way out and who is signed in. Signal's pane also shows the QR code to
// scan while a link is being made.

// A label and a button that say when their words change (the panes show them).
final class Words: NSTextField {
    var onChange: (() -> Void)?
    override var stringValue: String { didSet { onChange?() } }
}
final class Deed: NSButton {
    var onChange: (() -> Void)?
    override var title: String { didSet { onChange?() } }
    override var isEnabled: Bool { didSet { onChange?() } }
}

// One of the two large buttons.
final class BigButton: NSView {
    private let fill: NSColor
    private let line = NSTextField(labelWithString: "")
    private let stack = NSStackView()
    var onClick: (() -> Void)?
    var enabled = true { didSet { needsDisplay = true } }
    private var hover = false { didSet { if hover != oldValue { needsDisplay = true } } }
    private var down = false { didSet { needsDisplay = true } }
    private var shift: NSLayoutConstraint!
    private let logo = NSImageView()
    private(set) var hosted: NSView?
    private var below: NSLayoutConstraint!, centred: NSLayoutConstraint!
    private var qrSlot: NSView?
    private var hostHead: NSView?
    var busy: Bool { hosted != nil || code.image != nil }        // a login is in progress in this pane
    // A strip: the other pane's login has the room, and this one shows its one line.
    var compact = false { didSet { if compact != oldValue { arrange() } } }
    private func arrange() {
        let flow = !compact
        hosted?.isHidden = !flow
        hostHead?.isHidden = !flow
        code.isHidden = !flow || code.image == nil
        codeLine.isHidden = code.isHidden
        logo.isHidden = compact || busy
        stack.isHidden = flow && busy
        for v in stack.arrangedSubviews where v !== line { v.isHidden = compact || v === qrSlot }     // a strip is its one line
        below.isActive = false
        centred.isActive = false
        (compact ? centred : below)?.isActive = true
        needsDisplay = true
    }

    // The login itself, inside this pane (nil: the button again). While it is
    // here the pane is not a button: what is in it takes the presses.
    func host(_ v: NSView?) {
        hosted?.removeFromSuperview()
        hosted = v
        arrange()
        hostHead?.removeFromSuperview()
        hostHead = nil
        guard let v else { return }
        // over the form: whose login this is (the club's, by Authentik)
        let glyph = NSImageView()
        glyph.image = Brand.authentik(size: 18)
        glyph.contentTintColor = .white
        let words = NSTextField(labelWithString: "NCC login · authentik")
        words.font = Theme.mono(11, bold: true)
        words.textColor = .white
        let head = NSStackView(views: [glyph, words])
        head.spacing = 6
        head.translatesAutoresizingMaskIntoConstraints = false
        addSubview(head)
        hostHead = head
        v.translatesAutoresizingMaskIntoConstraints = false
        addSubview(v)
        NSLayoutConstraint.activate([
            head.centerXAnchor.constraint(equalTo: centerXAnchor), head.topAnchor.constraint(equalTo: topAnchor, constant: 10),
            v.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 8), v.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -8),
            v.topAnchor.constraint(equalTo: head.bottomAnchor, constant: 4), v.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -(lip + 8)),
        ])
    }

    // The code to scan fills the pane (nil: the button again): as large as the
    // pane lets it be, whole at any window size, with one line under it.
    private let code = NSImageView()
    private let codeLine = NSTextField(labelWithString: "")
    func showCode(_ image: NSImage?, _ words: String = "") {
        if code.superview == nil {
            code.imageScaling = .scaleProportionallyUpOrDown
            code.translatesAutoresizingMaskIntoConstraints = false
            code.setContentCompressionResistancePriority(.init(1), for: .vertical)
            code.setContentCompressionResistancePriority(.init(1), for: .horizontal)
            codeLine.font = Theme.mono(11)
            codeLine.textColor = .white
            codeLine.alignment = .center
            codeLine.lineBreakMode = .byTruncatingTail
            codeLine.translatesAutoresizingMaskIntoConstraints = false
            addSubview(code)
            addSubview(codeLine)
            NSLayoutConstraint.activate([
                code.topAnchor.constraint(equalTo: topAnchor, constant: 10),
                code.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 10), code.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -10),
                code.bottomAnchor.constraint(equalTo: codeLine.topAnchor, constant: -6),
                codeLine.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 8), codeLine.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -8),
                codeLine.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -(lip + 7)),
            ])
        }
        code.image = image
        codeLine.stringValue = words
        arrange()
    }

    init(color: NSColor, mark: NSImage?, under: [NSView]) {
        fill = color
        super.init(frame: .zero)
        wantsLayer = true
        layer?.masksToBounds = true                  // in a very small pane the contents are cut, never squeezed
        logo.image = mark
        logo.imageScaling = .scaleNone
        logo.contentTintColor = .white
        logo.translatesAutoresizingMaskIntoConstraints = false
        addSubview(logo)
        line.alignment = .center
        line.lineBreakMode = .byTruncatingTail
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 9
        stack.addArrangedSubview(line)
        under.forEach { stack.addArrangedSubview($0) }
        stack.translatesAutoresizingMaskIntoConstraints = false
        addSubview(stack)
        // The mark stands at the same height in both panes, whatever is under it.
        NSLayoutConstraint.activate([
            logo.widthAnchor.constraint(equalToConstant: 112), logo.heightAnchor.constraint(equalToConstant: 48),
            logo.centerXAnchor.constraint(equalTo: centerXAnchor),
            { shift = logo.centerYAnchor.constraint(equalTo: centerYAnchor, constant: -26); return shift }(),
            stack.centerXAnchor.constraint(equalTo: centerXAnchor),
        ])
        // the line never runs off the pane: it is cut short with "…" instead
        stack.widthAnchor.constraint(lessThanOrEqualTo: widthAnchor, constant: -20).isActive = true
        line.setContentCompressionResistancePriority(.init(200), for: .horizontal)
        line.maximumNumberOfLines = 1
        line.cell?.usesSingleLineMode = true
        line.cell?.wraps = false
        below = stack.topAnchor.constraint(equalTo: logo.bottomAnchor, constant: 12)
        centred = stack.centerYAnchor.constraint(equalTo: centerYAnchor, constant: -lip / 2 + 1)
        below.isActive = true
    }
    required init?(coder: NSCoder) { fatalError() }

    // "Link Signal" in bold and "to chat" after it, on one line.
    func say(_ deed: String, _ what: String) {
        let s = NSMutableAttributedString(string: deed, attributes: [.font: Theme.mono(13, bold: true), .foregroundColor: NSColor.white])
        if !what.isEmpty {
            s.append(NSAttributedString(string: " " + what, attributes: [.font: Theme.mono(13), .foregroundColor: NSColor(white: 1, alpha: 0.86)]))
        }
        line.attributedStringValue = s
    }

    // A raised key: a face that is lighter at the top than at the foot, a line
    // of light along its upper edge, and a thick dark lip under it. Pressed, the
    // face goes down on to the lip and the light goes out.
    private let lip: CGFloat = 7
    override func draw(_ dirty: NSRect) {
        let r: CGFloat = 8
        let sunk = (down && enabled) || (busy && !compact)       // with its login in it, the key stays down
        (fill.blended(withFraction: 0.5, of: .black) ?? fill).setFill()          // the lip
        NSBezierPath(roundedRect: bounds, xRadius: r, yRadius: r).fill()
        let face = NSRect(x: 0, y: sunk ? 2 : lip, width: bounds.width, height: bounds.height - lip)
        let shape = NSBezierPath(roundedRect: face, xRadius: r, yRadius: r)
        fill.setFill()
        shape.fill()
        NSGraphicsContext.saveGraphicsState()
        shape.addClip()
        Grain.draw(in: face)
        NSGradient(colors: [NSColor(white: 0, alpha: sunk ? 0.2 : 0.16), NSColor(white: 1, alpha: sunk ? 0 : 0.16)])?.draw(in: face, angle: 90)
        if hover && enabled && !sunk && hosted == nil { NSColor(white: 1, alpha: 0.12).setFill(); face.fill(using: .sourceOver) }
        if !enabled { NSColor(white: 0, alpha: 0.25).setFill(); face.fill(using: .sourceOver) }
        NSGraphicsContext.restoreGraphicsState()
        if !sunk {                                                                // the light along the upper edge
            NSColor(white: 1, alpha: 0.45).setStroke()
            let edge = NSBezierPath(roundedRect: face.insetBy(dx: 1, dy: 1), xRadius: r - 1, yRadius: r - 1)
            NSGraphicsContext.saveGraphicsState()
            NSBezierPath(rect: NSRect(x: 0, y: face.maxY - r, width: bounds.width, height: r)).addClip()
            edge.lineWidth = 1.5
            edge.stroke()
            NSGraphicsContext.restoreGraphicsState()
        }
        // what is on the key goes down with it
        let dy: CGFloat = sunk ? -(lip - 2) / 2 : 0
        if shift?.constant != dy - 26 + lip / 2 { shift?.constant = dy - 26 + lip / 2 }
    }
    // The whole pane is the button: a press anywhere in it, on the mark or the words too, is the pane's.
    override func hitTest(_ point: NSPoint) -> NSView? {
        if hosted != nil, !compact { return super.hitTest(point) }
        return frame.contains(point) ? self : nil
    }
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect], owner: self))
    }
    override func mouseEntered(with event: NSEvent) { hover = true }
    override func mouseExited(with event: NSEvent) { hover = false; down = false }
    override func mouseDown(with event: NSEvent) { if enabled || compact, hosted == nil || compact { down = true } }
    override func mouseUp(with event: NSEvent) {
        let was = down
        down = false
        if was, enabled || compact, bounds.contains(convert(event.locationInWindow, from: nil)) { onClick?() }
    }
    override func resetCursorRects() { if enabled { addCursorRect(bounds, cursor: .pointingHand) } }
}

final class LinkPanel: NSView {
    // What each pane says and does is set through these (they are not shown themselves).
    let status = Words(labelWithString: "")
    let button = Deed(title: "Link Signal", target: nil, action: nil)
    let nccButton = Deed(title: "Sign into NCC", target: nil, action: nil)
    let nccStatus = Words(labelWithString: "for wiki & code")
    let note = label("", Theme.mono(11), NSColor(white: 1, alpha: 0.85))
    private var signalPane: BigButton!, nccPane: BigButton!

    init() {
        super.init(frame: .zero)
        translatesAutoresizingMaskIntoConstraints = false
        setContentHuggingPriority(.init(1), for: .vertical)          // the panel takes the whole sheet
        note.maximumNumberOfLines = 3
        note.lineBreakMode = .byWordWrapping
        note.alignment = .center
        // A fixed width: a wrapping label whose width is left to the layout
        // measures itself again on every pass, and AppKit stops the app.
        note.preferredMaxLayoutWidth = 240
        note.widthAnchor.constraint(equalToConstant: 240).isActive = true

        signalPane = BigButton(color: NSColor(hex: "3A76F0")!, mark: Brand.signal(size: 44), under: [note])
        nccPane = BigButton(color: NSColor(hex: "006888")!, mark: Brand.ncc(cell: 6), under: [])
        signalPane.onClick = { [weak self] in self?.press(self?.button) }
        nccPane.onClick = { [weak self] in self?.press(self?.nccButton) }
        for w in [status, nccStatus] { w.onChange = { [weak self] in self?.sync() } }
        for d in [button, nccButton] { d.onChange = { [weak self] in self?.sync() } }
        // The panes are placed by hand (layout, below), not by a stack: every
        // state, and every way back from one, gives each pane its exact frame.
        for pane in [signalPane!, nccPane!] {
            pane.translatesAutoresizingMaskIntoConstraints = true
            pane.autoresizingMask = []
            addSubview(pane)
        }
        sync()
    }
    required init?(coder: NSCoder) { fatalError() }

    // One login at a time: the one in progress has the whole sheet and the
    // other pane is out of sight. With none in progress the two share the
    // sheet: side by side when it is wide for its height, else one over the other.
    var onFlow: ((Bool) -> Void)?            // a login started (true) or there is none now (false)
    private var focus: BigButton? {
        didSet {
            guard focus !== oldValue else { return }
            needsLayout = true
            onFlow?(focus != nil)
        }
    }
    var flowing: Bool { focus != nil }
    private func settle() { if let f = focus, !f.busy { focus = nil } }      // the login ended

    override func layout() {
        super.layout()
        // as far from the bar above as from the bar below, and as from the sides
        let room = NSRect(x: 0, y: 7, width: bounds.width, height: max(0, bounds.height - 12))
        signalPane.isHidden = focus != nil && focus !== signalPane
        nccPane.isHidden = focus != nil && focus !== nccPane
        if let focus { focus.frame = room; return }
        let gap: CGFloat = 8
        if room.width >= 520 && room.width > room.height {
            let w = ((room.width - gap) / 2).rounded(.down)
            signalPane.frame = NSRect(x: room.minX, y: room.minY, width: w, height: room.height)
            nccPane.frame = NSRect(x: room.maxX - w, y: room.minY, width: w, height: room.height)
        } else {
            let h = ((room.height - gap) / 2).rounded(.down)
            signalPane.frame = NSRect(x: room.minX, y: room.maxY - h, width: room.width, height: h)      // Signal's on top
            nccPane.frame = NSRect(x: room.minX, y: room.minY, width: room.width, height: h)
        }
    }
    override func setFrameSize(_ size: NSSize) {
        super.setFrameSize(size)
        needsLayout = true
    }

    // The NCC login runs in NCC's own pane (nil: the button again).
    func showNCC(_ web: NSView?) {
        nccPane.host(web)
        if web != nil { focus = nccPane } else { settle() }
    }
    var nccWeb: NSView? { nccPane.hosted }

    private func press(_ b: NSButton?) {
        guard let b, b.isEnabled, let action = b.action else { return }
        NSApp.sendAction(action, to: b.target, from: b)
    }
    private func sync() {
        signalPane?.say(button.title.trimmingCharacters(in: .whitespaces), status.stringValue)
        signalPane?.enabled = button.isEnabled
        nccPane?.say(nccButton.title.trimmingCharacters(in: .whitespaces), nccStatus.stringValue)
        nccPane?.enabled = nccButton.isEnabled
    }

    // Point at the logins: the two panes blink (a greyed-out tab was pressed).
    func flash() {
        for pane in [signalPane!, nccPane!] {
            NSAnimationContext.runAnimationGroup({ ctx in
                ctx.duration = 0.12
                pane.animator().alphaValue = 0.35
            }, completionHandler: {
                NSAnimationContext.runAnimationGroup { ctx in
                    ctx.duration = 0.3
                    pane.animator().alphaValue = 1
                }
            })
        }
    }

    // The moment Link Signal is pressed: the pane is already laid out as it will
    // be with the code in it (an empty white square, one line under it), so
    // nothing jumps when the code arrives a second later.
    func waitForCode() {
        let side: CGFloat = 330
        let blank = NSImage(size: NSSize(width: side, height: side))
        blank.lockFocus()
        NSColor.white.setFill()
        NSRect(x: 0, y: 0, width: side, height: side).fill()
        let words = NSAttributedString(string: "making your code…", attributes: [
            .font: NSFont.monospacedSystemFont(ofSize: 15, weight: .medium), .foregroundColor: NSColor(white: 0, alpha: 0.45)])
        words.draw(at: NSPoint(x: (side - words.size().width) / 2, y: (side - words.size().height) / 2))
        blank.unlockFocus()
        signalPane.showCode(blank, "Signal on your phone → Settings → Linked devices → scan")
        focus = signalPane
    }

    // The code to scan, in Signal's pane (nil: the button again).
    func showQR(_ uri: String?) {
        guard let uri, let f = CIFilter(name: "CIQRCodeGenerator") else { signalPane.showCode(nil); return settle() }
        f.setValue(Data(uri.utf8), forKey: "inputMessage")
        f.setValue("M", forKey: "inputCorrectionLevel")
        guard let out = f.outputImage?.transformed(by: CGAffineTransform(scaleX: 10, y: 10)) else { return }
        // on white, with a white margin round it, so it scans on any pane colour
        let rep = NSCIImageRep(ciImage: out)
        let pad: CGFloat = 24
        let img = NSImage(size: NSSize(width: rep.size.width + 2 * pad, height: rep.size.height + 2 * pad))
        img.lockFocus()
        NSColor.white.setFill()
        NSRect(origin: .zero, size: img.size).fill()
        rep.draw(in: NSRect(x: pad, y: pad, width: rep.size.width, height: rep.size.height))
        img.unlockFocus()
        signalPane.showCode(img, "Signal on your phone → Settings → Linked devices → scan")
        focus = signalPane
    }
}

// ─── window shell ────────────────────────────────────────────────────────────
// The window wears the standard macOS title bar. The shell holds the screen
// under it.

final class Shell: NSView {
    private var screen: NSView?
    func set(_ v: NSView) {
        subviews.forEach { $0.removeFromSuperview() }
        v.translatesAutoresizingMaskIntoConstraints = true
        v.autoresizingMask = [.width, .height]
        v.frame = bounds
        addSubview(v)
        screen = v
    }
    override func layout() {
        super.layout()
        screen?.frame = bounds
    }
}

// The list that "@" opens over the composer: the circle's members, a name a row.
final class MentionList: NSView {
    private var items: [(name: String, id: String)] = []
    private var at = 0
    private var hue = Theme.green
    var onPick: (((name: String, id: String)) -> Void)?
    var picked: (name: String, id: String)? { items.indices.contains(at) ? items[at] : nil }
    static let row: CGFloat = 20
    override var isFlipped: Bool { true }

    func show(_ list: [(name: String, id: String)], color: NSColor) {
        items = list
        at = min(at, max(0, list.count - 1))
        hue = color
        let widest = list.map { ($0.name as NSString).size(withAttributes: [.font: Theme.mono(11, bold: true)]).width }.max() ?? 80
        setFrameSize(NSSize(width: max(150, widest + 34), height: CGFloat(list.count) * Self.row + 6))
        needsDisplay = true
    }
    func move(_ by: Int) {
        guard !items.isEmpty else { return }
        at = (at + by + items.count) % items.count
        needsDisplay = true
    }
    override func draw(_ dirty: NSRect) {
        Theme.pair("FFFFFF", "061820").setFill()
        NSBezierPath(roundedRect: bounds, xRadius: 6, yRadius: 6).fill()
        NSColor(white: 0, alpha: 0.25).setStroke()
        NSBezierPath(roundedRect: bounds.insetBy(dx: 0.5, dy: 0.5), xRadius: 6, yRadius: 6).stroke()
        for (i, m) in items.enumerated() {
            let r = NSRect(x: 3, y: 3 + CGFloat(i) * Self.row, width: bounds.width - 6, height: Self.row)
            if i == at {
                hue.setFill()
                NSBezierPath(roundedRect: r, xRadius: 4, yRadius: 4).fill()
            }
            ("@" + m.name as NSString).draw(at: NSPoint(x: r.minX + 8, y: r.minY + 3), withAttributes: [
                .font: Theme.mono(11, bold: i == at), .foregroundColor: i == at ? Theme.on(hue) : Theme.ink])
        }
    }
    override func mouseDown(with event: NSEvent) {
        let i = Int((convert(event.locationInWindow, from: nil).y - 3) / Self.row)
        if items.indices.contains(i) { onPick?(items[i]) }
    }
}

// A seam: a row of stitches, as on cloth. Each stitch is a short rounded
// thread, lit along its top, with the dark of the hole it goes into at each
// end and a soft shadow under it, so it sits on the plane, not in it.
final class Hairline: NSView {
    static let tall: CGFloat = 7
    override func draw(_ dirty: NSRect) {
        let mid = bounds.midY, long: CGFloat = 9, gap: CGFloat = 6, thick: CGFloat = 2.4
        var thread = Theme.word, light = NSColor.white
        effectiveAppearance.performAsCurrentDrawingAppearance {
            thread = Theme.word.usingColorSpace(.sRGB) ?? thread
            light = thread.blended(withFraction: 0.55, of: .white) ?? light
        }
        var x: CGFloat = 4
        while x + long < bounds.width {
            // the holes
            NSColor(white: 0, alpha: 0.35).setFill()
            NSBezierPath(ovalIn: NSRect(x: x - 1.6, y: mid - 1.3, width: 2.6, height: 2.6)).fill()
            NSBezierPath(ovalIn: NSRect(x: x + long - 1, y: mid - 1.3, width: 2.6, height: 2.6)).fill()
            // the shadow under the thread
            NSColor(white: 0, alpha: 0.22).setFill()
            NSBezierPath(roundedRect: NSRect(x: x, y: mid - thick / 2 - 1.2, width: long, height: thick), xRadius: thick / 2, yRadius: thick / 2).fill()
            // the thread, and the light along its top
            thread.setFill()
            NSBezierPath(roundedRect: NSRect(x: x, y: mid - thick / 2, width: long, height: thick), xRadius: thick / 2, yRadius: thick / 2).fill()
            light.setFill()
            NSBezierPath(roundedRect: NSRect(x: x + 1, y: mid + thick / 2 - 1, width: long - 2, height: 0.8), xRadius: 0.4, yRadius: 0.4).fill()
            x += long + gap
        }
    }
    override func hitTest(_ point: NSPoint) -> NSView? { nil }
}

// A filled shape that takes a click.
final class PressFill: NSView {
    var color: NSColor { didSet { needsDisplay = true } }
    private let radius: CGFloat
    var onClick: (() -> Void)?
    init(_ c: NSColor, radius: CGFloat) {
        color = c
        self.radius = radius
        super.init(frame: .zero)
    }
    required init?(coder: NSCoder) { fatalError() }
    override func draw(_ r: NSRect) {
        color.setFill()
        NSBezierPath(roundedRect: bounds, xRadius: radius, yRadius: radius).fill()
        if hover {                               // under the pointer it lights up
            NSColor(white: 1, alpha: 0.25).setFill()
            NSBezierPath(roundedRect: bounds, xRadius: radius, yRadius: radius).fill()
        }
    }
    private var hover = false { didSet { if hover != oldValue { needsDisplay = true } } }
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect], owner: self))
    }
    override func mouseEntered(with event: NSEvent) { hover = true }
    override func mouseExited(with event: NSEvent) { hover = false }
    override func mouseDown(with event: NSEvent) { onClick?() }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
}

// A stack that takes a click.
final class Press: NSStackView {
    var onClick: (() -> Void)?
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect], owner: self))
    }
    override func mouseEntered(with event: NSEvent) { alphaValue = 0.65 }     // under the pointer it gives a little
    override func mouseExited(with event: NSEvent) { alphaValue = 1 }
    override func mouseDown(with event: NSEvent) { onClick?() }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
}

// ─── rail ────────────────────────────────────────────────────────────────────

// One row of the rail is a folder tab, and its look says what is open:
//   open here      the tab is the colour of the pane it opens, and runs into
//                  it with no edge between them: the tab and its folder are one
//   open elsewhere the circle's full colour and a small window mark (it has a
//                  window of its own)
//   not open       the circle's colour, faded and shaded, as a tab that lies
//                  behind
// Every tab runs flush to the pane (Tab.draw).
final class RailRow: NSView {
    private let name: NSTextField
    private let icon = NSImageView()
    private let mark = NSImageView()
    private let count = label("", Theme.mono(9, bold: true), Theme.onYellow)
    private let countBg = Fill(Theme.yellow, radius: 8)
    let wash: NSColor                  // the circle's colour
    let pane: NSColor                  // the colour of the pane this tab opens
    let title: String
    var glyph: NSImage? { icon.image }
    private var height: NSLayoutConstraint!
    var onClick: (() -> Void)?
    var menuProvider: (() -> NSMenu?)?
    var tag_ = -1
    var selected = false { didSet { paint() } }
    var windowed = false { didSet { paint() } }
    var unread = 0 { didSet { count.stringValue = "\(unread)"; countBg.isHidden = unread == 0 } }
    var tall: CGFloat = 24 { didSet { height.constant = tall } }
    // The tabs are along the top of the window: closer set, so more of them fit on a line.
    // Greyed out: this place needs a login that is not done. A press goes to the logins.
    var enabled = true { didSet { paint() } }
    var onTop = false {
        didSet {
            inner.edgeInsets = NSEdgeInsets(top: 0, left: onTop ? 10 : 16, bottom: onTop ? 3 : 0, right: onTop ? 9 : 10)
            inner.spacing = onTop ? 5 : 7
            needsDisplay = true
        }
    }
    // How much room there is for names: 0 every tab shows its name, 1 only the open tab, 2 none.
    var compact = 0 { didSet { paint() } }
    // How wide the tab is with its name, or with only its icon (and its unread count).
    // (Measured from the tab's own contents, with the name in its heaviest weight, so a name is never cut short.)
    func width(named: Bool) -> CGFloat {
        let was = name.font
        name.font = Theme.mono(weight: .heavy)
        let hidden = name.isHidden
        name.isHidden = !named
        let w = inner.fittingSize.width
        name.isHidden = hidden
        name.font = was
        return ceil(w) + 4
    }
    private var inner: NSStackView!

    init(title: String, icon image: NSImage?, color: NSColor, pane: NSColor) {
        wash = color
        self.pane = pane
        self.title = title
        name = label(title, Theme.mono(11))           // as it is written, not in capitals
        super.init(frame: .zero)
        icon.image = image
        mark.image = Theme.tabIcon("window", size: 10)
        for (v, size) in [(icon, 18), (mark, 13)] as [(NSImageView, CGFloat)] {
            v.imageScaling = .scaleNone
            v.widthAnchor.constraint(equalToConstant: size).isActive = true
            v.heightAnchor.constraint(equalToConstant: size).isActive = true
        }
        pin(count, to: countBg, 0)
        count.alignment = .center
        countBg.widthAnchor.constraint(greaterThanOrEqualToConstant: 22).isActive = true
        countBg.heightAnchor.constraint(equalToConstant: 16).isActive = true
        countBg.isHidden = true
        let row = NSStackView(views: [icon, name, NSView(), mark, countBg])
        row.spacing = 7
        row.edgeInsets = NSEdgeInsets(top: 0, left: 16, bottom: 0, right: 10)
        name.setContentCompressionResistancePriority(.required, for: .horizontal)
        count.setContentCompressionResistancePriority(.required, for: .horizontal)
        inner = row
        pin(row, to: self)
        height = heightAnchor.constraint(equalToConstant: tall)
        height.isActive = true
        paint()
    }
    required init?(coder: NSCoder) { fatalError() }

    private var open: Bool { selected || windowed }
    private func paint() {
        let on = !enabled ? Theme.faint : selected ? Theme.ink : windowed ? Theme.on(wash) : Theme.ink
        name.font = Theme.mono(weight: selected ? .heavy : open ? .bold : .medium)
        name.textColor = on
        icon.contentTintColor = on
        mark.contentTintColor = on
        mark.isHidden = !windowed
        name.isHidden = compact == 2 || (compact == 1 && !selected)
        needsDisplay = true
    }

    override func draw(_ dirty: NSRect) {
        Tab.draw(in: bounds, fill: !enabled ? NSColor.gray.withAlphaComponent(0.22) : selected ? pane : windowed ? wash : wash.withAlphaComponent(0.42),
                 open: selected, top: onTop, hover: hover && enabled)
    }
    // The pointer over the tab.
    private var hover = false { didSet { if hover != oldValue { needsDisplay = true } } }
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect], owner: self))
    }
    override func mouseEntered(with event: NSEvent) { hover = true }
    override func mouseExited(with event: NSEvent) { hover = false }

    override func viewDidChangeEffectiveAppearance() { paint() }
    override func mouseDown(with event: NSEvent) { onClick?() }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
    override func menu(for event: NSEvent) -> NSMenu? { menuProvider?() }
}

// ─── main ────────────────────────────────────────────────────────────────────

final class MainView: NSView, NSTextFieldDelegate, NSTextViewDelegate {
    private let rail = NSStackView()
    private let logo = LogoTab()
    private var diagramHeight: NSLayoutConstraint!
    private var wikiShown = ""         // the heading of the tree in the wiki pane
    private var docKey = ""            // the page in the wiki pane, so a redraw keeps its place
    private var lastWikiRender: (() -> Void)?
    private var wikiRenderedWidth: CGFloat = 0
    private let solo: Bool             // a circle in a window of its own: no rail, no wiki
    // The wiki pane is the member's to open and shut (the WIKI row of the rail).
    // The wiki tab: open, or not. Picking a circle or the donut shuts it.
    private var shownIndex = -1
    var paneTab = -2 { didSet { paintTabs() } }       // which tab has the wiki's plane: the wiki (-2) or the code (-5)
    var wikiOpen = false { didSet { if wikiOpen != oldValue { fold(width: bounds.width); needsLayout = true } } }
    private var sheetFromRail: NSLayoutConstraint!, sheetFromEdge: NSLayoutConstraint!
    private var sheetToWiki: NSLayoutConstraint!, sheetToEdge: NSLayoutConstraint!
    var menuFor: ((Int) -> NSMenu?)?
    private let foot = NSStackView()      // the radio and the two logins, at the foot of the rail
    private let radioIcon = NSImageView()
    private let radioName = label(Radio.name, Theme.mono(11, bold: true))
    private let radioNow = label("", Theme.mono(11), Theme.faint)
    private var radioRow: Press!
    var onRadio: (() -> Void)?
    let link = LinkPanel()                // shown on the sheet while Signal is not linked
    private let radioBar = Fill(Theme.pair("FFFFFF", "061820", alpha: 0.55), radius: 6)    // in the Netlabel tab: the station
    private let searchBar = Fill(Theme.pair("FFFFFF", "061820", alpha: 0.78), radius: 7)
    private let searchField = NSTextField()
    private let barSearch = NSTextField()          // on top: the search is in the task bar, at its left
    private let barSearchBox = Fill(Theme.pair("FFF3D0", "2B2410"), radius: 0)      // full bleed: it is the bar, on a warm ground of its own
    private let barGlass = NSImageView()
    var onSearchText: ((String) -> Void)?
    private var loginWeb: NSView?         // the club's login page, in the wiki pane
    private var wikiPane: NSView!
    private var linkShown = false
    // The zoom (⌘+, ⌘−, ⌘0), as in a browser: what is in the pane, the
    // messages and the wiki with their pictures, is drawn larger or smaller
    // and wraps again to fit. The tabs and the bars stay as they are.
    var zoom: CGFloat = 1 {
        didSet {
            for sv in [scroll, wikiScroll] {
                sv.allowsMagnification = true
                sv.minMagnification = 0.5
                sv.maxMagnification = 3
                sv.magnification = zoom
            }
            fitText()
            DispatchQueue.main.async { [weak self] in        // the text is laid out again at its new width
                self?.fitText()
                self?.lastRender?()
                self?.lastWikiRender?()
            }
        }
    }

    // A text is as wide as its pane shows of it. Zoomed in, the pane shows
    // less, so the text is narrower and wraps sooner; nothing runs off the edge.
    private func fitText() {
        for (t, sv) in [(text, scroll), (wikiText, wikiScroll)] {
            let w = sv.contentView.bounds.width
            if w > 0, abs(t.frame.width - w) > 0.5 { t.setFrameSize(NSSize(width: w, height: t.frame.height)) }
        }
    }

    // Where the tabs are: down the side, or along the top like a browser's (View menu).
    var tabsOnTop = UserDefaults.standard.bool(forKey: "tabsOnTop") { didSet { if tabsOnTop != oldValue { applyTabs() } } }
    private var top: Bool { tabsOnTop && !solo }
    private var sideSet: [NSLayoutConstraint] = [], topSet: [NSLayoutConstraint] = []
    private var logoWide: NSLayoutConstraint!
    private var topHeight: NSLayoutConstraint!
    private let unibar = Unibar()              // on top: the one bar
    private let drop = Fill(Theme.mint)        // the list of places that drops down from it
    private let dropStack = NSStackView()
    private let scrim = PressFill(NSColor(white: 0, alpha: 0.28), radius: 0)     // behind the list: a tap on it shuts the list
    private var dropOpen = false {
        didSet {
            drop.isHidden = !dropOpen
            scrim.isHidden = !dropOpen
            unibar.open = dropOpen
            diagram.alphaValue = dropOpen ? 0 : 1        // the 3D donut steps out from under the list
        }
    }
    private var lastSections: [(String, [(Int, String, NSColor?)])] = []
    private let taskBar = Fill(Theme.mint)     // along the bottom, when the tabs are along the top
    private let topLine = Hairline(), bottomLine = Hairline()
    private let footGap = NSView()             // pushes the logins to the right end of the task bar
    private var badgeNames: [NSView] = []      // the words in the login badges
    private var topLevel = -1
    private var rowWide: [NSLayoutConstraint] = []
    private var railHeads: [NSView] = []       // the section headings of the rail
    private var railEnds: [NSView] = []        // the last row of each section
    private var railTop: NSLayoutConstraint!
    private var whoLabels: [NSView] = []       // the names beside the login badges
    private var density = -1
    private let title = label("", Theme.mono(11, bold: true), .white)
    private let band = Fill(Theme.green)
    private let sheet = Fill(Theme.paper)
    private let wikiBand = Fill(Theme.wikiBand)
    private let diagram = CircleDiagram()
    private let roles = NSTextField(labelWithString: "")
    private let text = NSTextView()
    private let scroll = NSScrollView()
    private let field = NSTextField()
    private let composer = Fill(Theme.pair("FFFFFF", "061820", alpha: 0.78), radius: 7)
    private let sendButton = PressFill(Theme.green, radius: 13)
    private let mentions = MentionList()
    private var chosen: [(name: String, id: String)] = []       // the members picked from the list in this draft
    // The members of the open circle, for the list that "@" opens.
    var mentionable: [(name: String, id: String)] = []
    private var pendingRanges: [NSRange] = []                    // the words of messages still on their way
    private var glow: Timer?
    private var glowPhase = 0.0
    private var sendArrow: NSImageView?
    private let status = label("", Theme.mono(9), Theme.faint)
    private let wikiHead = label("WIKI", Theme.mono(10, bold: true), .white)
    private let wikiText = NSTextView()
    private let wikiScroll = NSScrollView()
    private var rows: [RailRow] = []
    static let size: CGFloat = Theme.size
    private var lastRender: (() -> Void)?
    private var renderedWidth: CGFloat = 0
    private var resizeWork: DispatchWorkItem?

    private var railWidth: NSLayoutConstraint!
    private var wikiWidth: NSLayoutConstraint!
    private var railView: NSView!
    private var wikiView: NSView!

    // The wiki is a tab like any circle: when it is open it has the whole
    // sheet, and the circle that was there is out of sight until its tab is
    // picked again. Under 470 points wide the rail folds away too.
    //
    // A pane that folds away is hidden and steps out of the chain of panes; it
    // is never squeezed to no width, because its own insides cannot be laid
    // out in nothing (AppKit then goes round until it stops the app).
    private func fold(width w: CGFloat) {
        guard w > 0 else { return }
        let showRail = !solo && (top || w >= 470)
        let showWiki = !solo && wikiOpen
        let beside = showRail && !top               // the rail stands beside the sheet (not over it)
        let railW: CGFloat = beside ? 158 : 0
        railView.isHidden = !showRail
        wikiView.isHidden = !showWiki
        sheet.isHidden = showWiki
        if beside { sheetFromEdge.isActive = false; sheetFromRail.isActive = true }
        else { sheetFromRail.isActive = false; sheetFromEdge.isActive = true }
        fitTop(width: w)
        sheetToWiki.isActive = false
        sheetToEdge.isActive = true
        if showWiki { wikiWidth.constant = w - railW }
        paintTabs()
    }

    // On top: the wordmark gives way to its house in a narrow window, and the
    // task bar along the bottom keeps its words while there is room for them.
    private func fitTop(width w: CGFloat) {
        guard top else {
            if topLevel != -1 { topLevel = -1; logo.small = false; applyFoot(words: true, names: true) }
            return
        }
        topLevel = 1
        let small = false                    // the wordmark always shows: it is the donut's own name
        logo.small = small
        logoWide.constant = small ? 46 : 118
        topHeight.constant = 48
        applyFoot(words: w >= 330, names: w >= 520)
    }

    // The radio and the logins in words, or as marks alone; with or without who is signed in.
    private func applyFoot(words: Bool, names: Bool) {
        badgeNames.forEach { $0.isHidden = !words }
        if top { whoLabels.forEach { $0.isHidden = !names } }
    }

    // Which tab is the open one: the wiki's when the wiki is open, else the sheet's.
    private func paintTabs() {
        let wiki = !solo && wikiOpen
        for r in rows { r.selected = wiki ? r.tag_ == paneTab : r.tag_ == shownIndex }
        logo.selected = !wiki && shownIndex == -1
        if top {                                    // the bar says where we are, in that place's colour
            // You are in NCCd (the donut: the wordmark alone), or in NCCd and one more place.
            // (The logins are NCCd's own first page, so there the bar is blank too.)
            let here = rows.first { $0.selected && $0.tag_ != -1 && $0.tag_ != -4 }
            if let here {
                unibar.title = here.title
                unibar.glyph = here.glyph
            } else if wiki {                    // the wiki's plane with no tab of its own yet (the login)
                unibar.title = paneTab == -5 ? "Code" : "Wiki"
                unibar.glyph = Theme.tabIcon(unibar.title)
            } else {
                unibar.title = ""
                unibar.glyph = nil
            }
            unibar.color = here?.pane ?? (wiki ? Theme.wikiPaper : shownIndex == -4 ? Theme.wash(Theme.login) : Theme.wash(Donut.icing))
            unibar.unread = rows.filter { !$0.selected }.reduce(0) { $0 + $1.unread }
            logo.selected = true                    // the wordmark's end of the bar is the bar's colour too
            logo.barColor = unibar.color
        } else {
            logo.barColor = nil
        }
    }

    // The size that counts is the bounds' (the zoom gives the view bounds that are not its frame's).
    override func setFrameSize(_ size: NSSize) {
        super.setFrameSize(size)
        fold(width: bounds.width)
        fitRail(height: bounds.height)
    }
    override func setBoundsSize(_ size: NSSize) {
        super.setBoundsSize(size)
        fold(width: size.width)
        fitRail(height: size.height)
    }

    // Pictures are sized to the column, so a new width lays the ledger out again.
    // The rail fits its window at any height, and nothing in it ever overlaps:
    //   0  roomy: the wordmark, headings, tall rows, a name beside each login
    //   1  close: no headings, short rows, no names beside the logins
    //   2  tight: no wordmark either
    // It is worked out when the size or the rail's contents change, never
    // inside a layout pass.
    private func fitRail(height h: CGFloat) {
        guard h > 0 else { return }
        let n = CGFloat(rows.count), heads = CGFloat(railHeads.count), ends = CGFloat(railEnds.count)
        let logoH = LogoTab.tall
        func need(_ d: Int) -> CGFloat {
            (d < 2 ? 8 + logoH + 8 : 6) + n * (d == 0 ? 25 : 21) + (d == 0 ? heads * 17 + ends * 10 : ends * 4) + 2 * 19 + 6 + 22
        }
        if top {                                    // along the top: one height, no headings, no names by the logins
            guard density != 9 else { return }
            density = 9
            logo.isHidden = false
            return
        }
        let d = need(0) <= h ? 0 : need(1) <= h ? 1 : 2
        guard d != density else { return }
        density = d
        rows.forEach { $0.tall = d == 0 ? 24 : 20 }
        railHeads.forEach { $0.isHidden = d > 0 }
        railEnds.forEach { rail.setCustomSpacing(d == 0 ? 10 : 4, after: $0) }
        logo.isHidden = d == 2
        railTop.constant = d < 2 ? 8 + logoH + 8 : 6
        whoLabels.forEach { $0.isHidden = d > 0 }
    }

    override func layout() {
        defer { fitText() }
        fitText()
        // The donut has the sheet to itself.
        // the donut has the sheet to itself, or shares it with what is under it
        let tall = linkShown ? max(120, (bounds.height * 0.48).rounded())
            : max(120, bounds.height - (top ? topHeight.constant + 38 : 0) - 24)
        if diagramHeight.constant != tall { diagramHeight.constant = tall }
        super.layout()
        let sheetMoved = lastRender != nil && abs((text.textContainer?.size.width ?? 0) - renderedWidth) > 24
        let wikiMoved = lastWikiRender != nil && abs((wikiText.textContainer?.size.width ?? 0) - wikiRenderedWidth) > 24
        guard sheetMoved || wikiMoved else { return }
        resizeWork?.cancel()
        let work = DispatchWorkItem { [weak self] in
            if sheetMoved { self?.lastRender?() }
            if wikiMoved { self?.lastWikiRender?() }
        }
        resizeWork = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.2, execute: work)
    }

    var onSelect: ((Int) -> Void)?
    var onSend: ((String, [String]) -> Void)?      // the words, and their mentions ("start:length:member")
    var onDoc: ((String) -> Void)?     // a wiki page was picked
    var onBack: (() -> Void)?          // leave the page, back to messages

    init(solo: Bool = false) {
        self.solo = solo
        super.init(frame: .zero)

        // Three flat planes, edge to edge: the rail, the sheet, the wiki column.
        let railBox = Fill(Theme.mint)
        railBox.grain = true
        sheet.grain = true
        rail.orientation = .vertical
        rail.alignment = .leading
        rail.spacing = 1                   // a hairline of the rail's own plane between the bands
        rail.translatesAutoresizingMaskIntoConstraints = false
        foot.orientation = .horizontal
        foot.alignment = .leading
        foot.spacing = 6
        foot.translatesAutoresizingMaskIntoConstraints = false
        logo.onClick = { [weak self] in self?.onSelect?(-1) }
        railBox.addSubview(logo)
        railBox.addSubview(rail)
        unibar.translatesAutoresizingMaskIntoConstraints = false
        unibar.onClick = { [weak self] in self?.dropOpen.toggle() }
        railBox.addSubview(unibar)
        // The radio and the logins. Down the side they are the foot of the rail;
        // with the tabs along the top they are a task bar along the bottom.
        taskBar.grain = true
        taskBar.isHidden = true
        addSubview(taskBar)
        addSubview(foot)
        footGap.setContentHuggingPriority(.init(1), for: .horizontal)
        // the radio: a press plays the station, another stops it
        radioIcon.imageScaling = .scaleNone
        radioIcon.widthAnchor.constraint(equalToConstant: 14).isActive = true
        radioNow.lineBreakMode = .byTruncatingTail
        radioNow.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        radioRow = Press(views: [radioIcon, radioName, radioNow])
        radioRow.spacing = 6
        radioRow.onClick = { [weak self] in self?.onRadio?() }
        setRadio(playing: false, now: "")
        addSubview(railBox)

        // sheet: a band across the top in the circle's colour, the rest a wash of it
        addSubview(sheet)
        title.lineBreakMode = .byTruncatingTail
        band.translatesAutoresizingMaskIntoConstraints = false
        sheet.addSubview(band)
        band.isHidden = true
        roles.lineBreakMode = .byTruncatingTail
        roles.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)

        Self.dress(text, scroll)
        text.delegate = self
        // In the ledger a link is underlined too, every line of it, however far it wraps.
        text.linkTextAttributes = [.foregroundColor: Theme.link, .cursor: NSCursor.pointingHand,
                                   .underlineStyle: NSUnderlineStyle.single.rawValue]
        Self.dress(wikiText, wikiScroll)
        wikiText.delegate = self

        // The composer: a bar with room in it, flat, on a lighter ground than
        // the sheet, with a send button in the circle's colour.
        field.font = Theme.mono(12)
        field.isBordered = false
        field.drawsBackground = false
        field.focusRingType = .none
        field.lineBreakMode = .byTruncatingHead
        field.delegate = self
        field.target = self
        field.action = #selector(send)
        field.translatesAutoresizingMaskIntoConstraints = false
        sendButton.translatesAutoresizingMaskIntoConstraints = false
        composer.addSubview(field)
        composer.addSubview(sendButton)
        // The one icon that is not pixels: a round, smooth arrow.
        let arrow = NSImageView()
        arrow.image = NSImage(systemSymbolName: "arrow.up", accessibilityDescription: "Send")?
            .withSymbolConfiguration(.init(pointSize: 13, weight: .black))
        arrow.imageScaling = .scaleNone
        arrow.translatesAutoresizingMaskIntoConstraints = false
        sendButton.addSubview(arrow)
        sendArrow = arrow
        sendButton.onClick = { [weak self] in self?.send() }
        NSLayoutConstraint.activate([
            composer.heightAnchor.constraint(equalToConstant: 34),
            field.leadingAnchor.constraint(equalTo: composer.leadingAnchor, constant: 11),
            field.centerYAnchor.constraint(equalTo: composer.centerYAnchor),
            field.trailingAnchor.constraint(equalTo: sendButton.leadingAnchor, constant: -8),
            sendButton.trailingAnchor.constraint(equalTo: composer.trailingAnchor, constant: -5),
            sendButton.centerYAnchor.constraint(equalTo: composer.centerYAnchor),
            sendButton.widthAnchor.constraint(equalToConstant: 26),
            sendButton.heightAnchor.constraint(equalToConstant: 26),
            arrow.centerXAnchor.constraint(equalTo: sendButton.centerXAnchor),
            arrow.centerYAnchor.constraint(equalTo: sendButton.centerYAnchor),
        ])

        diagram.translatesAutoresizingMaskIntoConstraints = false
        diagramHeight = diagram.heightAnchor.constraint(equalToConstant: 226)
        // Weaker than the window's hold on its own size: the donut takes the
        // height it is offered and never pushes the window taller.
        diagramHeight.priority = NSLayoutConstraint.Priority(490)
        diagramHeight.isActive = true
        diagram.isHidden = true
        diagram.onPick = { [weak self] i, at in self?.pick(i, at: at) }
        link.isHidden = true
        // the radio, in a bar of its own (shown in the Netlabel tab)
        radioBar.isHidden = true
        radioRow.translatesAutoresizingMaskIntoConstraints = false
        radioBar.addSubview(radioRow)
        // the search field (shown in the Search tab)
        searchBar.isHidden = true
        searchField.font = Theme.mono(12)
        searchField.isBordered = false
        searchField.drawsBackground = false
        searchField.focusRingType = .none
        searchField.placeholderString = "search the circles, the chats and the wiki"
        searchField.delegate = self
        searchField.translatesAutoresizingMaskIntoConstraints = false
        searchBar.addSubview(searchField)
        barSearch.font = .monospacedSystemFont(ofSize: 15, weight: .regular)     // larger than the one size: it is the bar
        barSearch.isBordered = false
        barSearch.drawsBackground = false
        barSearch.focusRingType = .none
        barSearch.placeholderString = "Search NCCd"
        // pressed while it is greyed out: on to the logins
        barSearchBox.addGestureRecognizer(NSClickGestureRecognizer(target: self, action: #selector(searchPressed)))
        barSearch.delegate = self
        barSearch.translatesAutoresizingMaskIntoConstraints = false
        barSearch.setContentCompressionResistancePriority(.init(1), for: .horizontal)
        barSearchBox.addSubview(barSearch)
        barGlass.image = Theme.tabIcon("search", size: 14)
        barGlass.imageScaling = .scaleNone
        barGlass.contentTintColor = Theme.word          // the wordmark's green
        barGlass.translatesAutoresizingMaskIntoConstraints = false
        barSearchBox.addSubview(barGlass)
        // It takes all of the task bar that the badges leave: it is the one thing
        // in the bar that stretches. (Never by a wide width of its own: that
        // would push the window wider.)
        barSearchBox.setContentHuggingPriority(.init(1), for: .horizontal)
        barSearchBox.setContentCompressionResistancePriority(.init(1), for: .horizontal)
        NSLayoutConstraint.activate([
            barSearchBox.widthAnchor.constraint(greaterThanOrEqualToConstant: 50),
            barSearchBox.heightAnchor.constraint(equalToConstant: 38),
            barGlass.leadingAnchor.constraint(equalTo: barSearchBox.leadingAnchor, constant: Theme.edge),
            barGlass.centerYAnchor.constraint(equalTo: barSearchBox.centerYAnchor),
            barGlass.widthAnchor.constraint(equalToConstant: 17),
            barSearch.leadingAnchor.constraint(equalTo: barGlass.trailingAnchor, constant: 5),
            barSearch.trailingAnchor.constraint(equalTo: barSearchBox.trailingAnchor, constant: -5),
            barSearch.centerYAnchor.constraint(equalTo: barSearchBox.centerYAnchor),
        ])
        NSLayoutConstraint.activate([
            radioBar.heightAnchor.constraint(equalToConstant: 28),
            radioRow.leadingAnchor.constraint(equalTo: radioBar.leadingAnchor, constant: 10),
            radioRow.trailingAnchor.constraint(lessThanOrEqualTo: radioBar.trailingAnchor, constant: -10),
            radioRow.centerYAnchor.constraint(equalTo: radioBar.centerYAnchor),
            searchBar.heightAnchor.constraint(equalToConstant: 34),
            searchField.leadingAnchor.constraint(equalTo: searchBar.leadingAnchor, constant: 11),
            searchField.trailingAnchor.constraint(equalTo: searchBar.trailingAnchor, constant: -11),
            searchField.centerYAnchor.constraint(equalTo: searchBar.centerYAnchor),
        ])
        status.isHidden = true
        let col = NSStackView(views: [searchBar, radioBar, roles, diagram, link, scroll, status, composer])
        col.orientation = .vertical
        col.alignment = .leading
        col.spacing = 2
        col.translatesAutoresizingMaskIntoConstraints = false
        sheet.addSubview(col)
        for v in [searchBar, radioBar, diagram, link, scroll, composer] as [NSView] { v.widthAnchor.constraint(equalTo: col.widthAnchor).isActive = true }
        col.setCustomSpacing(5, after: scroll)
        scroll.setContentHuggingPriority(.defaultLow, for: .vertical)

        // wiki: its own band, its own plane
        let wiki = Fill(Theme.wikiPaper)
        wiki.grain = true
        addSubview(wiki)
        wikiBand.translatesAutoresizingMaskIntoConstraints = false
        wiki.addSubview(wikiBand)
        wikiBand.isHidden = true
        wikiScroll.translatesAutoresizingMaskIntoConstraints = false
        wiki.addSubview(wikiScroll)

        railWidth = railBox.widthAnchor.constraint(equalToConstant: 158)
        wikiWidth = wiki.widthAnchor.constraint(equalToConstant: 224)
        railView = railBox
        wikiView = wiki
        wikiPane = wiki
        sheetFromRail = sheet.leadingAnchor.constraint(equalTo: railBox.trailingAnchor)
        sheetFromEdge = sheet.leadingAnchor.constraint(equalTo: leadingAnchor)
        sheetToWiki = sheet.trailingAnchor.constraint(equalTo: wiki.leadingAnchor)
        sheetToEdge = sheet.trailingAnchor.constraint(equalTo: trailingAnchor)
        sheetFromRail.isActive = !solo
        sheetFromEdge.isActive = solo
        sheetToEdge.isActive = true
        railBox.isHidden = solo
        wiki.isHidden = true
        railTop = rail.topAnchor.constraint(equalTo: railBox.topAnchor, constant: 56)
        // the tabs down the side
        sideSet = [
            railBox.bottomAnchor.constraint(equalTo: bottomAnchor),
            railWidth,
            logo.trailingAnchor.constraint(equalTo: railBox.trailingAnchor),
            logo.topAnchor.constraint(equalTo: railBox.topAnchor, constant: 8),
            rail.leadingAnchor.constraint(equalTo: railBox.leadingAnchor),
            rail.trailingAnchor.constraint(equalTo: railBox.trailingAnchor),
            railTop,
            foot.leadingAnchor.constraint(equalTo: railBox.leadingAnchor, constant: Theme.edge),
            foot.trailingAnchor.constraint(lessThanOrEqualTo: railBox.trailingAnchor, constant: -Theme.edge),
            foot.bottomAnchor.constraint(equalTo: railBox.bottomAnchor, constant: -10),
            sheet.topAnchor.constraint(equalTo: topAnchor),
            wiki.topAnchor.constraint(equalTo: topAnchor),
            sheet.bottomAnchor.constraint(equalTo: bottomAnchor),
            wiki.bottomAnchor.constraint(equalTo: bottomAnchor),
        ]
        // the tabs along the top: the wordmark's tab, the tabs in a row, the radio and the logins at the right
        topSet = [
            railBox.trailingAnchor.constraint(equalTo: trailingAnchor),
            { topHeight = railBox.heightAnchor.constraint(equalToConstant: 48); return topHeight }(),
            { logoWide = logo.widthAnchor.constraint(equalToConstant: 146); return logoWide }(),
            logo.topAnchor.constraint(equalTo: railBox.topAnchor),
            unibar.leadingAnchor.constraint(equalTo: logo.trailingAnchor),
            unibar.trailingAnchor.constraint(equalTo: railBox.trailingAnchor),
            unibar.topAnchor.constraint(equalTo: railBox.topAnchor),
            unibar.bottomAnchor.constraint(equalTo: railBox.bottomAnchor),
            foot.leadingAnchor.constraint(equalTo: taskBar.leadingAnchor),
            foot.trailingAnchor.constraint(equalTo: taskBar.trailingAnchor),
            foot.centerYAnchor.constraint(equalTo: taskBar.centerYAnchor),
            sheet.topAnchor.constraint(equalTo: railBox.bottomAnchor),
            wiki.topAnchor.constraint(equalTo: railBox.bottomAnchor),
            sheet.bottomAnchor.constraint(equalTo: taskBar.topAnchor),
            wiki.bottomAnchor.constraint(equalTo: taskBar.topAnchor),
        ]
        NSLayoutConstraint.activate([
            railBox.leadingAnchor.constraint(equalTo: leadingAnchor),
            railBox.topAnchor.constraint(equalTo: topAnchor),
            logo.leadingAnchor.constraint(equalTo: railBox.leadingAnchor),
            taskBar.leadingAnchor.constraint(equalTo: leadingAnchor),
            taskBar.trailingAnchor.constraint(equalTo: trailingAnchor),
            taskBar.bottomAnchor.constraint(equalTo: bottomAnchor),
            taskBar.heightAnchor.constraint(equalToConstant: 38),
            band.leadingAnchor.constraint(equalTo: sheet.leadingAnchor),
            band.trailingAnchor.constraint(equalTo: sheet.trailingAnchor),
            band.topAnchor.constraint(equalTo: sheet.topAnchor),
            band.heightAnchor.constraint(equalToConstant: 0),      // no title strip: the window's title says where we are
            col.leadingAnchor.constraint(equalTo: sheet.leadingAnchor, constant: Theme.edge),
            col.trailingAnchor.constraint(equalTo: sheet.trailingAnchor, constant: -Theme.edge),
            col.topAnchor.constraint(equalTo: band.bottomAnchor, constant: 7),
            col.bottomAnchor.constraint(equalTo: sheet.bottomAnchor, constant: -5),
            wiki.trailingAnchor.constraint(equalTo: trailingAnchor),
            wikiWidth,
            wikiBand.leadingAnchor.constraint(equalTo: wiki.leadingAnchor),
            wikiBand.trailingAnchor.constraint(equalTo: wiki.trailingAnchor),
            wikiBand.topAnchor.constraint(equalTo: wiki.topAnchor),
            wikiBand.heightAnchor.constraint(equalToConstant: 0),       // no band: the tab says this is the wiki
            wikiScroll.leadingAnchor.constraint(equalTo: wiki.leadingAnchor, constant: 4),
            wikiScroll.trailingAnchor.constraint(equalTo: wiki.trailingAnchor, constant: -2),
            wikiScroll.topAnchor.constraint(equalTo: wikiBand.bottomAnchor, constant: 3),
            wikiScroll.bottomAnchor.constraint(equalTo: wiki.bottomAnchor, constant: -4),
        ])
        // the dropdown: over everything under the bar, with a scrim behind it
        scrim.wantsLayer = true            // layers, so that they lie over the 3D donut (which is one)
        drop.wantsLayer = true
        scrim.translatesAutoresizingMaskIntoConstraints = false
        scrim.onClick = { [weak self] in self?.dropOpen = false }
        addSubview(scrim)
        drop.grain = true
        addSubview(drop)
        dropStack.orientation = .vertical
        dropStack.alignment = .leading
        dropStack.spacing = 1
        dropStack.translatesAutoresizingMaskIntoConstraints = false
        drop.addSubview(dropStack)
        NSLayoutConstraint.activate([
            scrim.leadingAnchor.constraint(equalTo: leadingAnchor), scrim.trailingAnchor.constraint(equalTo: trailingAnchor),
            scrim.topAnchor.constraint(equalTo: railBox.bottomAnchor), scrim.bottomAnchor.constraint(equalTo: bottomAnchor),
            drop.leadingAnchor.constraint(equalTo: leadingAnchor), drop.trailingAnchor.constraint(equalTo: trailingAnchor),
            drop.topAnchor.constraint(equalTo: railBox.bottomAnchor),
            dropStack.leadingAnchor.constraint(equalTo: drop.leadingAnchor), dropStack.trailingAnchor.constraint(equalTo: drop.trailingAnchor, constant: -8),
            dropStack.topAnchor.constraint(equalTo: drop.topAnchor, constant: 8), dropStack.bottomAnchor.constraint(equalTo: drop.bottomAnchor, constant: -10),
        ])
        // Hairlines: one under the top bar and one over the bottom bar, a dotted
        // line in the wordmark's green, a pixel high.
        for (line, bar, under) in [(topLine, railBox as NSView, true), (bottomLine, taskBar as NSView, false)] {
            line.translatesAutoresizingMaskIntoConstraints = false
            addSubview(line, positioned: .above, relativeTo: nil)
            NSLayoutConstraint.activate([
                line.leadingAnchor.constraint(equalTo: leadingAnchor), line.trailingAnchor.constraint(equalTo: trailingAnchor),
                line.heightAnchor.constraint(equalToConstant: Hairline.tall),
                // the seam sits across the edge where the two planes meet
                line.centerYAnchor.constraint(equalTo: under ? bar.bottomAnchor : bar.topAnchor),
            ])
        }
        dropOpen = false
        applyTabs()
        // a login under way: the bar is "← back" (down the side, where there is no bar, a press on the wordmark goes back)
        link.onFlow = { [weak self] on in
            guard let self else { return }
            self.dropOpen = false
            self.unibar.backAction = on ? { [weak self] in self?.onLoginBack?() } : nil
        }
    }
    var onLoginBack: (() -> Void)?
    required init?(coder: NSCoder) { fatalError() }

    // Put the tabs where the member wants them: down the side, or along the top.
    private func applyTabs() {
        NSLayoutConstraint.deactivate(sideSet + topSet + rowWide)
        NSLayoutConstraint.activate(top ? topSet : sideSet)
        rail.isHidden = top
        unibar.isHidden = !top
        dropOpen = false
        logo.onClick = { [weak self] in
            guard let self else { return }
            self.dropOpen = false
            self.onSelect?(-1)                  // the wordmark is the way home, in either arrangement
        }
        if !lastSections.isEmpty { setRail(lastSections) }      // the tabs are made again where they now stand
        foot.orientation = .horizontal
        foot.alignment = top ? .centerY : .leading
        foot.spacing = top ? 12 : 6
        taskBar.isHidden = !top
        topLine.isHidden = !top
        bottomLine.isHidden = !top
        footGap.isHidden = true
        barSearchBox.isHidden = !top
        logo.onTop = top
        density = -1
        fitRail(height: bounds.height)
        fold(width: bounds.width)
        needsLayout = true
    }

    private static func dress(_ t: NSTextView, _ s: NSScrollView) {
        t.isEditable = false
        t.drawsBackground = false
        t.textContainerInset = NSSize(width: 2, height: 2)
        t.autoresizingMask = []              // its width is set by fitText: what the pane shows, zoom and all
        t.isVerticallyResizable = true
        t.textContainer?.widthTracksTextView = true
        t.linkTextAttributes = [.foregroundColor: Theme.link, .cursor: NSCursor.pointingHand]
        s.documentView = t
        s.hasVerticalScroller = true
        s.drawsBackground = false
    }

    // A member's wiki colour, lifted toward white in the dark theme so a deep
    // blue name still reads on a near-black sheet.
    static func legible(_ c: NSColor?) -> NSColor {
        guard let c else { return Theme.ink }
        return NSColor(name: nil) { ap in
            ap.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua ? (c.blended(withFraction: 0.45, of: .white) ?? c) : c
        }
    }

    // The foot of the rail: the two logins. Each is a badge in its service's
    // own colour with its mark in pixels, then a tick and who is signed in, or
    // what to do to sign in. They are separate: Signal opens the circles, NCC
    // opens the wiki.
    var onAccount: ((String) -> Void)?
    func setAccounts(ncc: String?, signal: String?) {
        foot.arrangedSubviews.forEach { $0.removeFromSuperview() }
        foot.addArrangedSubview(barSearchBox)
        barSearchBox.isHidden = !top
        foot.addArrangedSubview(footGap)
        footGap.isHidden = true                     // on top the search fills the bar; down the side nothing needs pushing
        whoLabels = []
        badgeNames = []
        func badge(_ name: String, logo: String, color: NSColor, who: String?, ask: String) -> NSView {
            let chip = Fill(who == nil ? color.withAlphaComponent(0.5) : color, radius: 3)
            let glyph = NSImageView()
            glyph.image = logo == "signal" ? Brand.signal(size: 11) : Pixel.icon(logo, size: 8)
            glyph.imageScaling = .scaleNone
            glyph.contentTintColor = .white
            let row = NSStackView(views: [glyph])         // small: the mark alone (the Login tab has the words)
            row.edgeInsets = NSEdgeInsets(top: 3, left: 4, bottom: 3, right: 4)
            pin(row, to: chip)
            // no tick beside it: a badge is full colour when its login is done, faded when it is not
            let line = Press(views: [chip])
            line.spacing = 3
            line.toolTip = who.map { name + " · " + $0 } ?? ask
            line.onClick = { [weak self] in self?.onAccount?(name) }
            return line
        }
        // (No badges: the Login tab says which logins are done.)
        _ = badge
        density = -1
        fitRail(height: bounds.height)
        topLevel = -1
        fitTop(width: bounds.width)
    }

    func setStatus(_ s: String) { status.stringValue = s; status.isHidden = s.isEmpty }

    // The station's bar, in the Netlabel tab.
    func showRadio(_ on: Bool) { radioBar.isHidden = !on }

    // The Search tab: its field, and what was found (in the ledger's place).
    // (On top the field is in the task bar, so the sheet has no field of its own.)
    func showSearch(_ on: Bool) {
        searchBar.isHidden = !on || top
        guard on else { return }
        if !top { window?.makeFirstResponder(searchField) }
        else if barSearch.currentEditor() == nil { window?.makeFirstResponder(barSearch) }
    }
    private var searchOn = true
    @objc private func searchPressed() {
        if searchOn { window?.makeFirstResponder(barSearch) } else { onAccount?("") }
    }
    // The search is greyed out until there is something to search: one login, at least.
    func setSearchEnabled(_ on: Bool) {
        unibar.locked = !on                  // the same rule: nothing to open until one login is done
        if !on { dropOpen = false }
        barSearch.isEnabled = on
        searchField.isEnabled = on
        barSearchBox.alphaValue = on ? 1 : 0          // not there until there is something to search: an empty bar
        searchOn = on
    }
    var searchText: String {
        get { top ? barSearch.stringValue : searchField.stringValue }
        set { searchField.stringValue = newValue; barSearch.stringValue = newValue }
    }

    struct Found { let link: URL; let place: String; let color: NSColor?; let when: String; let who: String; let text: String }
    func renderSearch(query: String, found: [Found], note: String) {
        lastRender = nil
        pendingRanges = []
        glimmer()
        let font = Theme.mono(11), bold = Theme.mono(11, bold: true)
        let out = NSMutableAttributedString(string: note + "\n", attributes: [.font: font, .foregroundColor: Theme.faint])
        let para = NSMutableParagraphStyle()
        para.paragraphSpacingBefore = 7
        para.headIndent = 14
        let q = query.lowercased()
        for f in found {
            let line = NSMutableAttributedString(string: " " + f.place + " ", attributes: [
                .font: bold, .foregroundColor: f.color.map(Theme.on) ?? .white, .backgroundColor: f.color ?? Theme.wikiBand, .link: f.link])
            line.append(NSAttributedString(string: "  " + [f.when, f.who].filter { !$0.isEmpty }.joined(separator: "  ") + "\n", attributes: [
                .font: font, .foregroundColor: Theme.faint]))
            // the words, with what was looked for in bold
            let words = f.text.replacingOccurrences(of: "\n", with: " ")
            // (no link on the words themselves: a link's colour would hide what was found; the place is the link)
            let body = NSMutableAttributedString(string: words + "\n", attributes: [.font: font, .foregroundColor: Theme.ink])
            let ns = words.lowercased() as NSString
            var from = 0
            while !q.isEmpty, from < ns.length {
                let r = ns.range(of: q, options: [], range: NSRange(location: from, length: ns.length - from))
                guard r.location != NSNotFound, r.location + r.length <= body.length else { break }
                body.addAttributes([.font: bold, .backgroundColor: Theme.yellow, .foregroundColor: Theme.onYellow], range: r)
                from = r.location + r.length
            }
            line.append(body)
            line.addAttribute(.paragraphStyle, value: para, range: NSRange(location: 0, length: line.length))
            out.append(line)
        }
        fitText()
        text.textStorage?.setAttributedString(out)
        text.scrollToBeginningOfDocument(nil)
    }

    func setRadio(playing: Bool, now: String) {
        radioIcon.image = NSImage(systemSymbolName: playing ? "pause.fill" : "play.fill", accessibilityDescription: playing ? "Stop" : "Play")?
            .withSymbolConfiguration(.init(pointSize: 11, weight: .bold))
        radioIcon.contentTintColor = playing ? Theme.me : Theme.ink
        radioName.textColor = playing ? Theme.me : Theme.ink
        radioNow.stringValue = playing ? now : "95.9 FM"
        radioRow.toolTip = playing && !now.isEmpty ? Radio.name + " · " + now : Radio.name + " 95.9 FM, community radio"
    }

    // A circle picked on the donut is zoomed into. Its messages, words and
    // pictures and all, are already inside its token, very small; the view
    // moves in on the token until what was inside it fills the sheet, and the
    // donut grows past the edges and is gone.
    func pick(_ index: Int, at: NSPoint?) {
        guard let at, !diagram.isHidden, sheet.bounds.width > 0 else { onSelect?(index); return }
        let p = sheet.convert(at, from: diagram)
        let size = sheet.bounds.size
        func resolved(_ c: NSColor) -> NSColor {
            var out = c
            effectiveAppearance.performAsCurrentDrawingAppearance { out = c.usingColorSpace(.sRGB) ?? c }
            return out
        }
        // the donut as it stands, with its names, on the sheet's present colour
        let ground = resolved(sheet.color)
        let donut = NSImage(size: size)
        donut.lockFocus()
        ground.setFill()
        NSRect(origin: .zero, size: size).fill()
        let pane = sheet.convert(diagram.bounds, from: diagram)
        diagram.shot().draw(in: pane)
        for v in diagram.overlays {
            guard let r = v.bitmapImageRepForCachingDisplay(in: v.bounds) else { continue }
            v.cacheDisplay(in: v.bounds, to: r)
            r.draw(in: sheet.convert(v.bounds, from: v))
        }
        donut.unlockFocus()

        onSelect?(index)                         // the circle's sheet is drawn now
        sheet.layoutSubtreeIfNeeded()
        guard let rep = sheet.bitmapImageRepForCachingDisplay(in: sheet.bounds) else { return }
        sheet.cacheDisplay(in: sheet.bounds, to: rep)
        let chat = NSImage(size: size)
        chat.addRepresentation(rep)

        let cover = NSView(frame: sheet.bounds)
        cover.autoresizingMask = [.width, .height]
        cover.wantsLayer = true
        cover.layer?.masksToBounds = true
        sheet.addSubview(cover, positioned: .above, relativeTo: nil)
        // both pictures turn about the picked point
        let anchor = CGPoint(x: p.x / size.width, y: p.y / size.height)
        func layer(_ image: NSImage) -> CALayer {
            let l = CALayer()
            l.contents = image
            l.anchorPoint = anchor
            l.frame = cover.bounds
            cover.layer?.addSublayer(l)
            return l
        }
        let far = layer(donut), near = layer(chat)
        // the sheet inside the token: a circle about the point, as wide as the
        // whole sheet when the zoom is done and as small as the token at its start
        let reach = hypot(max(p.x, size.width - p.x), max(p.y, size.height - p.y)) + 24
        let small: CGFloat = 26 / reach
        let lens = CAShapeLayer()
        lens.path = CGPath(ellipseIn: CGRect(x: p.x - reach, y: p.y - reach, width: 2 * reach, height: 2 * reach), transform: nil)
        near.mask = lens
        let rim = CAShapeLayer()                 // the token's own edge, growing with it
        rim.path = lens.path
        rim.fillColor = nil
        rim.strokeColor = resolved(band.color).cgColor
        rim.lineWidth = 5
        near.addSublayer(rim)

        let time = 1.15
        let curve = CAMediaTimingFunction(controlPoints: 0.6, 0, 0.25, 1)
        func animate(_ l: CALayer, _ key: String, _ from: Any, _ to: Any) {
            let a = CABasicAnimation(keyPath: key)
            a.fromValue = from
            a.toValue = to
            a.duration = time
            a.timingFunction = curve
            l.add(a, forKey: key)
        }
        animate(near, "transform.scale", small, 1)
        animate(far, "transform.scale", 1, 1 / small * 0.35)       // the donut grows past the edges
        animate(far, "opacity", 1, 0.2)
        animate(rim, "lineWidth", 5 / small, 0)
        far.opacity = 0.2
        rim.lineWidth = 0
        DispatchQueue.main.asyncAfter(deadline: .now() + time) { cover.removeFromSuperview() }
    }

    // The link panel on the sheet, under the donut (Signal is not linked).
    func showLink(_ on: Bool) {
        linkShown = on
        link.isHidden = !on
        scroll.isHidden = on
        needsLayout = true
    }

    // The club's login page in the wiki pane, in place of the pages (nil: the pages again).
    func showWikiLogin(_ web: NSView?) {
        loginWeb?.removeFromSuperview()
        loginWeb = web
        wikiScroll.isHidden = web != nil
        guard let web else { return }
        web.translatesAutoresizingMaskIntoConstraints = false
        wikiPane.addSubview(web)
        NSLayoutConstraint.activate([
            web.leadingAnchor.constraint(equalTo: wikiPane.leadingAnchor), web.trailingAnchor.constraint(equalTo: wikiPane.trailingAnchor),
            web.topAnchor.constraint(equalTo: wikiBand.bottomAnchor), web.bottomAnchor.constraint(equalTo: wikiPane.bottomAnchor)])
    }
    var wikiLoginView: NSView? { loginWeb ?? (link.isHiddenOrHasHiddenAncestor ? nil : link.nccWeb) }

    // sections: [(heading, [(index, title, colour)])]
    func setRail(_ sections: [(String, [(Int, String, NSColor?)])]) {
        lastSections = sections
        // In the dropdown the donut is a place like any other, and it comes first.
        rail.arrangedSubviews.forEach { $0.removeFromSuperview() }
        dropStack.arrangedSubviews.forEach { $0.removeFromSuperview() }
        rows = []
        rowWide = []
        railHeads = []
        railEnds = []
        density = -1
        for (heading, items) in sections where !items.isEmpty {
            if !heading.isEmpty {
                let h = label(heading, Theme.mono(9, bold: true), Theme.faint)
                let box = NSView()                      // the heading, set in from the edge like the names
                h.translatesAutoresizingMaskIntoConstraints = false
                box.addSubview(h)
                NSLayoutConstraint.activate([
                    h.leadingAnchor.constraint(equalTo: box.leadingAnchor, constant: 12),
                    h.topAnchor.constraint(equalTo: box.topAnchor), h.bottomAnchor.constraint(equalTo: box.bottomAnchor)])
                rail.addArrangedSubview(box)
                rail.setCustomSpacing(5, after: box)
                railHeads.append(box)
            }
            for (index, title, color) in items {
                let hue = color ?? Donut.icing
                let row = RailRow(title: title, icon: index == -1 ? nil : Theme.tabIcon(title), color: hue,
                                  pane: index == -2 ? Theme.wikiPaper : Theme.wash(hue))
                row.unread = unreadBy[index] ?? 0
                row.enabled = !disabledTabs.contains(index)
                row.onClick = { [weak self] in
                    guard let self else { return }
                    self.dropOpen = false
                    if self.disabledTabs.contains(index) { self.onAccount?("") } else { self.onSelect?(index) }
                }
                row.menuProvider = { [weak self] in self?.menuFor?(index) }
                row.tag_ = index
                row.tall = density > 0 ? 20 : 24
                if top {                                     // on top the places are a list that drops down from the bar
                    row.tall = 30
                    dropStack.addArrangedSubview(row)
                    row.widthAnchor.constraint(equalTo: dropStack.widthAnchor).isActive = true
                } else {
                    rail.addArrangedSubview(row)
                    let wide = row.widthAnchor.constraint(equalTo: rail.widthAnchor)     // down the side, as wide as the rail
                    rowWide.append(wide)
                    wide.isActive = true
                }
                rows.append(row)
            }
            if let last = rows.last { railEnds.append(last) }
        }
        fitRail(height: bounds.height)
        paintTabs()
    }

    private var unreadBy: [Int: Int] = [:]      // kept here too, so tabs that are made again keep their counts
    func setUnread(_ index: Int, _ n: Int) {
        unreadBy[index] = n
        guard let row = rows.first(where: { $0.tag_ == index }), row.unread != n else { return }
        row.unread = n
        if top { paintTabs() }                        // the bar's count of what is unread elsewhere
    }
    // The tabs in the order they stand, by index (the wiki's is -2).
    var tabOrder: [Int] { rows.filter { $0.enabled }.map { $0.tag_ } }
    var disabledTabs: Set<Int> = []        // set before setRail: the places whose login is not done
    func setWindowed(_ open: Set<Int>) { for r in rows where r.tag_ >= 0 { r.windowed = open.contains(r.tag_) } }

    // One heading line, one roles line: "ROLE Name · ROLE Name".
    func show(index: Int, heading: String, color: NSColor? = nil, diagram items: [DiagramItem]? = nil,
              roleLine: [(String, Person)], canSend: Bool, placeholder: String) {
        shownIndex = index
        paintTabs()
        title.stringValue = heading
        let hue = color ?? Donut.icing           // home wears the icing
        band.color = hue
        sheet.color = Theme.wash(hue)            // the sheet is a wash of the circle's colour
        title.textColor = Theme.on(hue)
        diagram.isHidden = items == nil
        if let items { diagram.items = items }
        // The roles, each its own block: the role on a dark ground, then the
        // member's name on the member's own colour.
        let line = NSMutableAttributedString()
        for (i, (role, p)) in roleLine.enumerated() {
            if i > 0 { line.append(NSAttributedString(string: "  ", attributes: [.font: Theme.mono(10)])) }
            let own = p.color ?? hue
            line.append(NSAttributedString(string: " " + role.uppercased() + " ", attributes: [
                .font: Theme.mono(9, bold: true), .foregroundColor: NSColor.white,
                .backgroundColor: NSColor(white: 0.08, alpha: 0.78)]))
            line.append(NSAttributedString(string: " " + p.name + " ", attributes: [
                .font: Theme.mono(10, bold: true), .foregroundColor: Theme.on(own), .backgroundColor: own]))
        }
        roles.attributedStringValue = line
        roles.isHidden = roleLine.isEmpty
        field.isEnabled = canSend
        field.placeholderString = placeholder
        composer.isHidden = !canSend && placeholder.isEmpty   // the donut has nothing to write to
        sendButton.color = hue                               // the send button wears the circle's colour
        sendButton.alphaValue = canSend ? 1 : 0.35
        sendArrow?.contentTintColor = Theme.on(hue)
        // (on the donut, the wordmark is the open tab: paintTabs)
    }

    // The ledger, one row per message: time, name, text. The text hangs under
    // its own column, so a long message stays one block.
    func render(_ msgs: [Msg], reactions: [Int64: [String: Set<String>]], people: People, me: String) {
        lastRender = { [weak self] in self?.render(msgs, reactions: reactions, people: people, me: me) }
        renderedWidth = text.textContainer?.size.width ?? 0
        let font = Theme.mono(Self.size), bold = Theme.mono(Self.size, bold: true)
        let ch = ("0" as NSString).size(withAttributes: [.font: font]).width
        let nameCol = 6 * ch, textCol = 16 * ch
        let row = NSMutableParagraphStyle()
        row.tabStops = [NSTextTab(textAlignment: .left, location: nameCol), NSTextTab(textAlignment: .left, location: textCol)]
        row.headIndent = textCol
        row.lineBreakMode = .byWordWrapping
        let under = NSMutableParagraphStyle()
        under.firstLineHeadIndent = textCol
        under.headIndent = textCol
        let dayStyle = NSMutableParagraphStyle()
        dayStyle.paragraphSpacingBefore = 4
        let dayFmt = DateFormatter()
        dayFmt.dateFormat = "EEE d MMM yyyy"
        let timeFmt = DateFormatter()
        timeFmt.dateFormat = "HH:mm"
        let out = NSMutableAttributedString()

        var lastDay = "", lastAuthor = ""
        var waiting: [NSRange] = []
        for m in msgs {
            let date = Date(timeIntervalSince1970: Double(m.ts) / 1000)
            let day = dayFmt.string(from: date).uppercased()
            if day != lastDay {
                out.append(NSAttributedString(string: "── \(day) ──\n", attributes: [
                    .font: Theme.mono(9, bold: true), .foregroundColor: Theme.faint, .paragraphStyle: dayStyle]))
                lastDay = day
                lastAuthor = ""
            }
            let p = people.person(m.author, fallback: m.author == me ? "me" : m.name)
            if let q = m.quote, !q.isEmpty {
                let short = q.replacingOccurrences(of: "\n", with: " ")
                out.append(NSAttributedString(string: "› " + (short.count > 90 ? String(short.prefix(90)) + "…" : short) + "\n", attributes: [
                    .font: Theme.mono(Self.size - 1), .foregroundColor: Theme.faint, .paragraphStyle: under]))
            }
            let line = NSMutableAttributedString(string: timeFmt.string(from: date) + "\t", attributes: [
                .font: font, .foregroundColor: Theme.faint])
            // The name shows when the speaker changes.
            let name = m.author == lastAuthor ? "" : String(p.name.prefix(9))
            line.append(NSAttributedString(string: name + "\t", attributes: [
                .font: bold, .foregroundColor: m.author == me ? Theme.me : Self.legible(p.color)]))
            lastAuthor = m.author
            // History rows carry mention placeholders with no names; drop them.
            let bodyText = m.body.replacingOccurrences(of: "\u{FFFC}", with: "").trimmingCharacters(in: .whitespaces)
            // a message still on its way to Signal: its words glimmer until it lands
            if m.kind == "pending" {
                waiting.append(NSRange(location: out.length + line.length, length: (bodyText as NSString).length))
            }
            line.append(NSAttributedString(string: bodyText, attributes: [.font: font, .foregroundColor: Theme.ink]))
            // Media sits under the text, in the text column, scaled to fit it.
            let room = max(120, (text.textContainer?.size.width ?? 400) - textCol - 12)
            let newline = { if line.length > 0, !line.string.hasSuffix("\t") { line.append(NSAttributedString(string: "\n")) } }
            // One picture shows large. Several sit side by side as tiles of one
            // height, wrapping like words.
            let files = Array((m.files ?? []).enumerated())
            let visuals = files.filter { Media.thumb($0.element) != nil }
            let tiled = visuals.count > 1
            if !visuals.isEmpty { newline() }
            for (n, (_, path)) in visuals.enumerated() {
                guard let img = Media.thumb(path) else { continue }
                let url = URL(fileURLWithPath: path)
                var size = tiled ? Media.fit(img.size, maxWidth: min(room, 190), maxHeight: 112)
                                 : Media.fit(img.size, maxWidth: min(room, 380), maxHeight: 260)
                if tiled, size.height > 0, size.height < 112 {   // bring every tile to the row height
                    let k = min(112 / size.height, min(room, 190) / size.width)
                    size = NSSize(width: (size.width * k).rounded(), height: (size.height * k).rounded())
                }
                let video = Media.isVideo(path)
                let att = NSTextAttachment()
                att.image = video ? Media.withPlayMark(img, size: size) : img
                att.bounds = NSRect(origin: .zero, size: size)
                let pic = NSMutableAttributedString(attachment: att)
                pic.addAttribute(.link, value: url, range: NSRange(location: 0, length: pic.length))
                line.append(pic)
                if tiled, n < visuals.count - 1 {
                    line.append(NSAttributedString(string: " ", attributes: [.font: Theme.mono(4)]))
                }
                if video, !tiled {
                    let facts = [Media.duration(path), Media.sizeLabel(path)].filter { !$0.isEmpty }.joined(separator: " · ")
                    line.append(NSAttributedString(string: "\nvideo " + facts, attributes: [
                        .font: Theme.mono(Self.size - 2), .foregroundColor: Theme.faint]))
                }
            }
            for (i, path) in files where Media.thumb(path) == nil {
                let shown = (m.fileNames?.indices.contains(i) == true && !m.fileNames![i].isEmpty ? m.fileNames![i] : nil)
                    ?? (path as NSString).lastPathComponent
                newline()
                let missing = !FileManager.default.fileExists(atPath: path)
                line.append(NSAttributedString(string: "[" + shown + "]", attributes: [
                    .font: font, .link: URL(fileURLWithPath: path)]))
                line.append(NSAttributedString(string: " " + (missing ? "not downloaded" : Media.sizeLabel(path)), attributes: [
                    .font: Theme.mono(Self.size - 2), .foregroundColor: Theme.faint]))
            }
            // Link previews: a small card with the page's picture, title and first words.
            for p in m.previews ?? [] {
                guard let link = URL(string: p.url) else { continue }
                newline()
                if let path = p.image, let img = Media.thumb(path, maxPixel: 480) {
                    let att = NSTextAttachment()
                    att.image = img
                    att.bounds = NSRect(origin: .zero, size: Media.fit(img.size, maxWidth: min(room, 220), maxHeight: 120))
                    let pic = NSMutableAttributedString(attachment: att)
                    pic.addAttribute(.link, value: link, range: NSRange(location: 0, length: pic.length))
                    line.append(pic)
                    line.append(NSAttributedString(string: "\n"))
                }
                let title = p.title.isEmpty ? (link.host ?? p.url) : p.title
                line.append(NSAttributedString(string: "▍" + title, attributes: [.font: bold, .link: link]))
                let host = link.host?.replacingOccurrences(of: "www.", with: "") ?? ""
                let words = p.description.replacingOccurrences(of: "\n", with: " ")
                let blurb = [host, words.count > 150 ? String(words.prefix(150)) + "…" : words].filter { !$0.isEmpty }.joined(separator: " · ")
                if !blurb.isEmpty {
                    line.append(NSAttributedString(string: "\n▍" + blurb, attributes: [
                        .font: Theme.mono(Self.size - 1), .foregroundColor: Theme.faint]))
                }
            }
            if let r = reactions[m.ts] {
                let marks = r.filter { !$0.value.isEmpty }.sorted { $0.key < $1.key }
                    .map { $0.value.count > 1 ? "\($0.key)\($0.value.count)" : $0.key }.joined(separator: " ")
                if !marks.isEmpty {
                    line.append(NSAttributedString(string: "  " + marks, attributes: [
                        .font: Theme.mono(Self.size - 1), .foregroundColor: Theme.faint]))
                }
            }
            line.append(NSAttributedString(string: "\n"))
            // The first paragraph carries the time and name columns; every later
            // one (more text, media, previews) starts in the text column.
            let whole = NSRange(location: 0, length: line.length)
            let firstEnd = (line.string as NSString).range(of: "\n").location + 1
            line.addAttribute(.paragraphStyle, value: under, range: whole)
            line.addAttribute(.paragraphStyle, value: row, range: NSRange(location: 0, length: min(firstEnd, line.length)))
            out.append(line)
        }
        fitText()
        text.textStorage?.setAttributedString(out)
        Self.linkify(text)
        pendingRanges = waiting
        glimmer()
        toNewest()
        // and once more when this pass of layout is done: the text's height is only then final
        DispatchQueue.main.async { [weak self] in self?.toNewest() }
    }

    // The words of a message on its way: a wave of light runs along them, a
    // character at a time, with a flicker, until the ledger is drawn again
    // without them (the message landed, or went back to the composer).
    private func glimmer() {
        glow?.invalidate()
        glow = nil
        guard !pendingRanges.isEmpty else { return }
        let t = Timer(timeInterval: 1.0 / 18, repeats: true) { [weak self] _ in
            guard let self, let lm = self.text.layoutManager, let length = self.text.textStorage?.length else { return }
            self.glowPhase += 0.42
            var ink = Theme.ink, lit = self.band.color
            self.effectiveAppearance.performAsCurrentDrawingAppearance {
                ink = Theme.ink.usingColorSpace(.sRGB) ?? ink
                lit = (self.band.color.usingColorSpace(.sRGB) ?? lit).blended(withFraction: 0.35, of: .white) ?? lit
            }
            for r in self.pendingRanges where r.location + r.length <= length {
                for i in 0..<r.length {
                    var f = 0.5 + 0.5 * sin(self.glowPhase - Double(i) * 0.55)
                    if (i &* 31 &+ Int(self.glowPhase * 3)) % 11 == 0 { f *= 0.35 }          // the flicker
                    let c = (ink.blended(withFraction: CGFloat(f), of: lit) ?? ink).withAlphaComponent(0.55 + 0.45 * CGFloat(f))
                    lm.addTemporaryAttribute(.foregroundColor, value: c, forCharacterRange: NSRange(location: r.location + i, length: 1))
                }
            }
        }
        RunLoop.main.add(t, forMode: .common)
        glow = t
    }

    // Show the newest message: lay the whole ledger out, then go to its end.
    private func toNewest() {
        fitText()
        guard let container = text.textContainer else { return }
        text.layoutManager?.ensureLayout(for: container)
        text.sizeToFit()
        text.scrollToEndOfDocument(nil)
        scroll.reflectScrolledClipView(scroll.contentView)
    }

    // Nothing on the sheet (the donut has it to itself).
    func renderBlank() {
        lastRender = nil
        pendingRanges = []
        glimmer()
        text.textStorage?.setAttributedString(NSAttributedString())
    }

    // A wiki page in the wiki pane, as typed text (Markdown.swift). A link to
    // another wiki page opens it here. `image` hands over the pictures fetched
    // so far; the page is drawn again as the rest arrive, in place.
    func renderWikiDoc(title: String, markdown: String, url: URL?, wikiBase: String,
                       image: @escaping (String) -> NSImage? = { _ in nil }) {
        lastWikiRender = { [weak self] in
            self?.renderWikiDoc(title: title, markdown: markdown, url: url, wikiBase: wikiBase, image: image)
        }
        wikiRenderedWidth = wikiText.textContainer?.size.width ?? 0
        wikiHead.stringValue = "WIKI"
        wikiShown = ""
        let font = Theme.mono(Self.size)
        let out = NSMutableAttributedString(string: "← pages", attributes: [
            .font: Theme.mono(10, bold: true), .link: URL(string: "ncc://back")!])
        if let url {
            out.append(NSAttributedString(string: "    "))
            // The member's default browser, by its own icon.
            if let app = NSWorkspace.shared.urlForApplication(toOpen: url) {
                let att = NSTextAttachment()
                att.image = NSWorkspace.shared.icon(forFile: app.path)
                att.bounds = NSRect(x: 0, y: -3, width: 14, height: 14)
                let pic = NSMutableAttributedString(attachment: att)
                pic.addAttribute(.link, value: url, range: NSRange(location: 0, length: pic.length))
                out.append(pic)
                out.append(NSAttributedString(string: " ", attributes: [.font: Theme.mono(10)]))
            }
            out.append(NSAttributedString(string: "open in browser", attributes: [.font: Theme.mono(10, bold: true), .link: url]))
        }
        out.append(NSAttributedString(string: "\n\n" + title.uppercased() + "\n", attributes: [
            .font: Theme.mono(13, bold: true), .foregroundColor: Theme.ink]))
        out.append(NSAttributedString(string: "\n", attributes: [.font: Theme.mono(4)]))
        let width = max(160, (wikiText.textContainer?.size.width ?? 300) - 10)
        let ch = ("0" as NSString).size(withAttributes: [.font: font]).width
        let md = Markdown(font: font, size: Self.size, wikiBase: wikiBase, columns: Int(width / ch),
                          image: image, maxImage: NSSize(width: min(width, 560), height: 420))
        out.append(md.render(markdown))
        // The same page drawn again (a picture came in, the pane changed width) keeps its place.
        let key = title + "\n" + (url?.absoluteString ?? "")
        let keep = key == docKey ? wikiScroll.contentView.bounds.origin : nil
        docKey = key
        fitText()
        wikiText.textStorage?.setAttributedString(out)
        Self.linkify(wikiText)
        if let keep {
            wikiText.layoutManager?.ensureLayout(for: wikiText.textContainer!)
            wikiScroll.contentView.scroll(to: keep)
            wikiScroll.reflectScrolledClipView(wikiScroll.contentView)
        } else {
            wikiText.scrollToBeginningOfDocument(nil)
        }
    }

    // The wiki column: the page tree, as its editors arranged it.
    func renderWiki(heading: String, rows: [WikiRow], note: String?, open: String? = nil) {
        lastWikiRender = nil
        docKey = ""
        wikiHead.stringValue = heading.uppercased()
        let font = Theme.mono(Self.size)
        let ch = ("0" as NSString).size(withAttributes: [.font: font]).width
        let out = NSMutableAttributedString()
        if let note {
            out.append(NSAttributedString(string: note + "\n", attributes: [.font: font, .foregroundColor: Theme.faint]))
        }
        for r in rows {
            let style = NSMutableParagraphStyle()
            style.firstLineHeadIndent = CGFloat(r.depth) * 2 * ch
            style.headIndent = CGFloat(r.depth) * 2 * ch + 2 * ch
            let line: NSMutableAttributedString
            if let id = r.id {
                let here = id == open        // the page on the sheet: bold, with a mark
                line = NSMutableAttributedString(string: here ? "▸ " : "· ", attributes: [
                    .font: font, .foregroundColor: here ? Theme.teal : Theme.faint])
                line.append(NSAttributedString(string: r.title + "\n", attributes: [
                    .font: here ? Theme.mono(Self.size, bold: true) : font, .link: URL(string: "ncc://doc/" + id)!]))
            } else {
                style.paragraphSpacingBefore = 5
                line = NSMutableAttributedString(string: r.title.uppercased() + "\n", attributes: [
                    .font: Theme.mono(9, bold: true), .foregroundColor: Theme.faint])
            }
            line.addAttribute(.paragraphStyle, value: style, range: NSRange(location: 0, length: line.length))
            out.append(line)
        }
        let same = wikiHead.stringValue == wikiShown
        let keep = wikiScroll.contentView.bounds.origin
        wikiShown = wikiHead.stringValue
        wikiText.textStorage?.setAttributedString(out)
        if same {                            // the same tree with another page marked: stay put
            wikiText.layoutManager?.ensureLayout(for: wikiText.textContainer!)
            wikiScroll.contentView.scroll(to: keep)
            wikiScroll.reflectScrolledClipView(wikiScroll.contentView)
        } else {
            wikiText.scrollToBeginningOfDocument(nil)
        }
    }

    // Plain http(s) addresses in the text become links.
    private static func linkify(_ t: NSTextView) {
        guard let storage = t.textStorage,
              let det = try? NSDataDetector(types: NSTextCheckingResult.CheckingType.link.rawValue) else { return }
        let s = storage.string
        for m in det.matches(in: s, range: NSRange(location: 0, length: (s as NSString).length)) {
            if let u = m.url, storage.attribute(.link, at: m.range.location, effectiveRange: nil) == nil {
                storage.addAttribute(.link, value: u, range: m.range)
            }
        }
    }

    func textView(_ textView: NSTextView, clickedOnLink link: Any, at charIndex: Int) -> Bool {
        guard let url = link as? URL, url.scheme == "ncc" else { return false }
        if url.host == "back" { onBack?() }
        if url.host == "doc" { onDoc?(url.lastPathComponent) }
        if url.host == "circle", let i = Int(url.lastPathComponent) { onSelect?(i) }       // a message that was found
        return true
    }

    override func cancelOperation(_ sender: Any?) {
        if dropOpen { dropOpen = false } else { onBack?() }
    }

    @objc private func send() {
        if !mentions.isHidden, let pick = mentions.picked { return choose(pick) }     // Return takes the name in the list
        let s = field.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !s.isEmpty, field.isEnabled else { return }
        // Each "@Name" that was picked from the list goes as a real Signal
        // mention: where it starts, how long it is (UTF-16), and who it is.
        let ns = s as NSString
        var marks: [String] = []
        for m in chosen {
            var from = 0
            while from < ns.length {
                let r = ns.range(of: "@" + m.name, options: [], range: NSRange(location: from, length: ns.length - from))
                guard r.location != NSNotFound else { break }
                marks.append("\(r.location):\(r.length):\(m.id)")
                from = r.location + r.length
            }
        }
        onSend?(s, marks)
    }

    // The "@word" the caret is in, and where it starts.
    private func mentionToken() -> (query: String, range: NSRange)? {
        guard let editor = field.currentEditor() else { return nil }
        let ns = field.stringValue as NSString
        let caret = min(editor.selectedRange.location, ns.length)
        var at = caret - 1
        while at >= 0 {
            let ch = ns.substring(with: NSRange(location: at, length: 1))
            if ch == "@" { break }
            if ch == " " || ch == "\n" { return nil }
            at -= 1
        }
        guard at >= 0, at == 0 || ns.substring(with: NSRange(location: at - 1, length: 1)) == " " else { return nil }
        return (ns.substring(with: NSRange(location: at + 1, length: caret - at - 1)), NSRange(location: at, length: caret - at))
    }

    private func updateMentions() {
        guard let token = mentionToken(), !mentionable.isEmpty else { return mentions.isHidden = true }
        let q = token.query.lowercased()
        let hits = mentionable.filter { q.isEmpty || $0.name.lowercased().hasPrefix(q) }
            + mentionable.filter { !q.isEmpty && !$0.name.lowercased().hasPrefix(q) && $0.name.lowercased().contains(q) }
        guard !hits.isEmpty else { return mentions.isHidden = true }
        mentions.show(Array(hits.prefix(7)), color: band.color)
        mentions.onPick = { [weak self] m in self?.choose(m) }
        if mentions.superview == nil { sheet.addSubview(mentions) }
        let box = sheet.convert(composer.bounds, from: composer)
        mentions.frame.origin = NSPoint(x: box.minX + 6, y: box.maxY + 4)
        mentions.isHidden = false
    }

    private func choose(_ m: (name: String, id: String)) {
        guard let token = mentionToken(), let editor = field.currentEditor() else { return }
        let ns = field.stringValue as NSString
        field.stringValue = ns.replacingCharacters(in: token.range, with: "@" + m.name + " ")
        editor.selectedRange = NSRange(location: token.range.location + (m.name as NSString).length + 2, length: 0)
        if !chosen.contains(where: { $0.id == m.id }) { chosen.append(m) }
        mentions.isHidden = true
    }

    // While the list is open: the arrows move in it, Return or Tab takes a name, Esc shuts it.
    func control(_ control: NSControl, textView: NSTextView, doCommandBy selector: Selector) -> Bool {
        guard control === field, !mentions.isHidden else { return false }
        switch selector {
        case #selector(NSResponder.moveUp(_:)): mentions.move(-1); return true
        case #selector(NSResponder.moveDown(_:)): mentions.move(1); return true
        case #selector(NSResponder.insertTab(_:)), #selector(NSResponder.insertNewline(_:)):
            if let pick = mentions.picked { choose(pick) }
            return true
        case #selector(NSResponder.cancelOperation(_:)): mentions.isHidden = true; return true
        default: return false
        }
    }

    func clearField() { field.stringValue = ""; chosen = []; mentions.isHidden = true }

    // The wordmark hops: a press on it had nowhere to go.
    func bounceLogo() {
        logo.wantsLayer = true
        let hop = CAKeyframeAnimation(keyPath: "transform.translation.y")
        hop.values = [0, 9, 0, 4, 0, 1.5, 0]
        hop.keyTimes = [0, 0.22, 0.48, 0.64, 0.8, 0.9, 1]
        hop.duration = 0.5
        hop.timingFunction = CAMediaTimingFunction(name: .easeOut)
        logo.layer?.add(hop, forKey: "hop")
    }

    // Test aid: open the unibar's dropdown.
    func testDrop() { dropOpen = true }

    // Test aid: pick the first circle on the donut, as a click on its token would.
    func testPick() { diagram.testPick() }

    // Test aid: the 3D donut as a picture with the views over it, and where it sits (nil when hidden).
    func donutShot() -> (NSImage, [NSView], NSView)? {
        diagram.isHiddenOrHasHiddenAncestor || diagram.alphaValue == 0 || diagram.window == nil ? nil : (diagram.shot(), diagram.overlays, diagram)
    }

    // The words in the composer, and whether the member has stopped typing
    // (an update restarts the app only in a pause).
    var draft: String {
        get { field.stringValue }
        set { field.stringValue = newValue }
    }
    private var lastTyped = Date.distantPast
    var quiet: Bool { Date().timeIntervalSince(lastTyped) > 6 }
    func controlTextDidChange(_ n: Notification) {
        if let f = n.object as? NSTextField, f === searchField || f === barSearch { return onSearchText?(f.stringValue) ?? () }
        lastTyped = Date()
        updateMentions()
    }
    func focusField() { window?.makeFirstResponder(field) }
}
