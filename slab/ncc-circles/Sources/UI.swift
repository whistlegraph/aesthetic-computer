// UI.swift — the look: a mint sheet, one monospace face, dense rows.
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
    static let mint = pair("CFEBD9", "07100C")      // the ground: mint sheet, or the site's near-black
    static let paper = pair("F4FBF6", "0F1B16")     // a sheet lying on it
    static let ink = pair("1B2B22", "D2F3E1")
    static let green = pair("2E7D4F", "0E6B45")     // heading band
    static let me = pair("2E7D4F", "00D080")        // your own name: the site green
    static let teal = pair("006888", "00C4FD")      // wordmark and links: the wiki's two logo colours
    static let bar = pair("006888", "062B36")       // the title bar: the wiki's teal, apart from the sheet
    static let onBar = pair("FFFFFF", "00C4FD")
    static let yellow = NSColor(hex: "F3D34A")!
    static let onYellow = NSColor(hex: "1B2B22")!
    static let faint = pair("1B2B22", "D2F3E1", alpha: 0.55)

    // The club's own colours, for a circle the wiki gives none: site green and
    // cyan, the orange dot of NELA.Computer, the pencil blue, pink, violet.
    static let palette = ["00D080", "00C4FD", "F07C00", "0A84C1", "E0127F", "9932CC"]
    static func circleColor(_ title: String, wiki: String?) -> NSColor {
        if let wiki, let c = NSColor(hex: wiki) { return c }
        let n = title.unicodeScalars.reduce(0) { ($0 &* 31 &+ Int($1.value)) % 9973 }
        return NSColor(hex: palette[n % palette.count])!
    }
    // Dark ink or white, whichever reads on this colour.
    static func on(_ c: NSColor) -> NSColor {
        guard let s = c.usingColorSpace(.sRGB) else { return .white }
        let l = 0.2126 * s.redComponent + 0.7152 * s.greenComponent + 0.0722 * s.blueComponent
        return l > 0.55 ? NSColor(hex: "10231A")! : .white
    }

    // "system", "light" or "dark"; kept in the app's defaults.
    static func apply(_ mode: String) {
        NSApp.appearance = mode == "light" ? NSAppearance(named: .aqua)
            : mode == "dark" ? NSAppearance(named: .darkAqua) : nil
    }

    static func mono(_ size: CGFloat, bold: Bool = false) -> NSFont {
        .monospacedSystemFont(ofSize: size, weight: bold ? .bold : .regular)
    }
    // Club artwork is optional and lives outside the repo.
    static func asset(_ name: String) -> NSImage? {
        guard let dir = Config.shared.conf.assetsDir else { return nil }
        return NSImage(contentsOf: Paths.expand(dir).appendingPathComponent(name))
    }

    static func icon(for title: String) -> NSImage? {
        let t = title.lowercased()
        let pick: (String, String) =
            t == "start" ? ("nela-magnifier-cut.png", "house") :
            t.contains("steer") ? ("nela-gavel-cut.png", "person.3") :
            t.contains("infra") ? ("nela-computer-cut.png", "server.rack") :
            t.contains("announce") ? ("nela-bulb-cut.png", "megaphone") :
            t.contains("chat") ? ("nela-pen-cut.png", "bubble.left.and.bubble.right") :
            ("nela-key-cut.png", "circle.dashed")
        return asset(pick.0) ?? NSImage(systemSymbolName: pick.1, accessibilityDescription: title)
    }
}

func label(_ s: String, _ font: NSFont, _ color: NSColor = Theme.ink) -> NSTextField {
    let l = NSTextField(labelWithString: s)
    l.font = font
    l.textColor = color
    l.lineBreakMode = .byTruncatingTail
    return l
}

final class Fill: NSView {
    var color: NSColor { didSet { needsDisplay = true } }
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

// The club wordmark, drawn the way the wiki and the login page draw it: "NCC"
// set in Unicode block characters, one teal colour. Each cell is cut as a path
// so the blocks meet cleanly at any size.
final class NCCLogo: NSView {
    private static let rows: [[(Int, Character)]] = [
        [(0, "▛"), (1, "▚"), (5, "▌"), (9, "▞"), (10, "▔"), (11, "▔"), (12, "▚"), (16, "▞"), (17, "▔"), (18, "▔"), (19, "▚")],
        [(0, "▌"), (2, "▚"), (5, "▌"), (8, "▞"), (15, "▞")],
        [(0, "▌"), (3, "▚"), (5, "▌"), (8, "▚"), (15, "▚")],
        [(0, "▌"), (4, "▚"), (5, "▌"), (9, "▚"), (10, "▂"), (11, "▂"), (12, "▞"), (16, "▚"), (17, "▂"), (18, "▂"), (19, "▞")],
    ]
    static var teal: NSColor { Theme.teal }
    private let scale: CGFloat
    private let color: NSColor

    init(scale: CGFloat, color: NSColor = Theme.teal) {
        self.scale = scale
        self.color = color
        super.init(frame: .zero)
        translatesAutoresizingMaskIntoConstraints = false
        widthAnchor.constraint(equalToConstant: 200 * scale).isActive = true
        heightAnchor.constraint(equalToConstant: 64 * scale).isActive = true
    }
    required init?(coder: NSCoder) { fatalError() }
    override var isFlipped: Bool { true }

    override func draw(_ r: NSRect) {
        color.setFill()
        let w = 10 * scale, h = 16 * scale
        for (row, cells) in Self.rows.enumerated() {
            for (col, ch) in cells {
                let x = CGFloat(col) * w, y = CGFloat(row) * h
                // Each block character as rects in unit cell space (x, y, w, h).
                let parts: [(CGFloat, CGFloat, CGFloat, CGFloat)]
                switch ch {
                case "▛": parts = [(0, 0, 1, 0.5), (0, 0.5, 0.5, 0.5)]
                case "▚": parts = [(0, 0, 0.5, 0.5), (0.5, 0.5, 0.5, 0.5)]
                case "▞": parts = [(0.5, 0, 0.5, 0.5), (0, 0.5, 0.5, 0.5)]
                case "▌": parts = [(0, 0, 0.5, 1)]
                case "▔": parts = [(0, 0, 1, 0.125)]
                case "▂": parts = [(0, 0.75, 1, 0.25)]
                default: parts = []
                }
                for (px, py, pw, ph) in parts {
                    NSRect(x: x + px * w, y: y + py * h, width: pw * w, height: ph * h).fill()
                }
            }
        }
    }
}

// ─── gate ────────────────────────────────────────────────────────────────────

final class GateView: NSView {
    let authStatus = label("", Theme.mono(12))
    let authButton = NSButton(title: "Sign in with NCC", target: nil, action: nil)
    let signalStatus = label("", Theme.mono(12))
    let signalButton = NSButton(title: "Link this Mac", target: nil, action: nil)
    let qr = NSImageView()
    let note = label("", Theme.mono(11), Theme.faint)

    init() {
        super.init(frame: .zero)
        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 14

        let logo = NCCLogo(scale: 1.6)
        stack.addArrangedSubview(logo)
        stack.setCustomSpacing(20, after: logo)
        stack.addArrangedSubview(label("CIRCLES", Theme.mono(22, bold: true), NCCLogo.teal))
        stack.setCustomSpacing(26, after: stack.arrangedSubviews.last!)

        stack.addArrangedSubview(step("1 · NCC LOGIN", authStatus, authButton))
        stack.addArrangedSubview(step("2 · SIGNAL", signalStatus, signalButton))

        qr.imageScaling = .scaleProportionallyUpOrDown
        qr.widthAnchor.constraint(equalToConstant: 220).isActive = true
        qr.heightAnchor.constraint(equalToConstant: 220).isActive = true
        qr.isHidden = true
        stack.addArrangedSubview(qr)
        note.maximumNumberOfLines = 3
        note.lineBreakMode = .byWordWrapping
        note.alignment = .center
        note.preferredMaxLayoutWidth = 460
        stack.addArrangedSubview(note)

        stack.translatesAutoresizingMaskIntoConstraints = false
        addSubview(stack)
        NSLayoutConstraint.activate([
            stack.centerXAnchor.constraint(equalTo: centerXAnchor),
            stack.centerYAnchor.constraint(equalTo: centerYAnchor),
        ])
    }
    required init?(coder: NSCoder) { fatalError() }

    private func step(_ title: String, _ status: NSTextField, _ button: NSButton) -> NSView {
        let tile = Fill(Theme.paper, radius: 6)
        let row = NSStackView(views: [label(title, Theme.mono(12, bold: true)), status, NSView(), button])
        row.spacing = 12
        status.textColor = Theme.faint
        status.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        button.bezelStyle = .rounded
        button.font = Theme.mono(12)
        pin(row, to: tile, 12)
        tile.widthAnchor.constraint(equalToConstant: 520).isActive = true
        return tile
    }

    func showQR(_ uri: String?) {
        guard let uri, let f = CIFilter(name: "CIQRCodeGenerator") else { qr.isHidden = true; return }
        f.setValue(Data(uri.utf8), forKey: "inputMessage")
        f.setValue("M", forKey: "inputCorrectionLevel")
        guard let out = f.outputImage?.transformed(by: CGAffineTransform(scaleX: 8, y: 8)) else { return }
        let rep = NSCIImageRep(ciImage: out)
        let img = NSImage(size: rep.size)
        img.addRepresentation(rep)
        qr.image = img
        qr.isHidden = false
    }
}

// ─── window shell ────────────────────────────────────────────────────────────
// A title bar in the club's teal across the top (the traffic lights sit on
// it), with the wordmark and the name of what is open; the screen goes below.

final class Shell: NSView {
    private let bar = Fill(Theme.bar)
    private let titleLabel = label("", Theme.mono(11, bold: true), Theme.onBar)
    private let slot = NSView()
    static let barHeight: CGFloat = 28

    init() {
        super.init(frame: .zero)
        addSubview(bar)
        slot.translatesAutoresizingMaskIntoConstraints = false
        addSubview(slot)
        let logo = NCCLogo(scale: 0.3, color: Theme.onBar)
        bar.addSubview(logo)
        titleLabel.translatesAutoresizingMaskIntoConstraints = false
        bar.addSubview(titleLabel)
        NSLayoutConstraint.activate([
            bar.leadingAnchor.constraint(equalTo: leadingAnchor), bar.trailingAnchor.constraint(equalTo: trailingAnchor),
            bar.topAnchor.constraint(equalTo: topAnchor), bar.heightAnchor.constraint(equalToConstant: Self.barHeight),
            logo.leadingAnchor.constraint(equalTo: bar.leadingAnchor, constant: 76),
            logo.centerYAnchor.constraint(equalTo: bar.centerYAnchor),
            titleLabel.leadingAnchor.constraint(equalTo: logo.trailingAnchor, constant: 10),
            titleLabel.centerYAnchor.constraint(equalTo: bar.centerYAnchor),
            titleLabel.trailingAnchor.constraint(lessThanOrEqualTo: bar.trailingAnchor, constant: -10),
            slot.leadingAnchor.constraint(equalTo: leadingAnchor), slot.trailingAnchor.constraint(equalTo: trailingAnchor),
            slot.topAnchor.constraint(equalTo: bar.bottomAnchor), slot.bottomAnchor.constraint(equalTo: bottomAnchor),
        ])
    }
    required init?(coder: NSCoder) { fatalError() }

    func setTitle(_ s: String) { titleLabel.stringValue = s }
    func set(_ v: NSView) {
        slot.subviews.forEach { $0.removeFromSuperview() }
        pin(v, to: slot)
    }
    var current: NSView? { slot.subviews.first }
}

// ─── circle diagram ──────────────────────────────────────────────────────────
// The club at a glance, on START. The circles are peers: they sit evenly on
// one ring, none above another, and the commons are the ground they all stand
// on. A disc is sized by its members and takes its circle's colour. Click one
// to open it.

struct DiagramItem {
    let index: Int
    let title: String
    let commons: Bool
    let color: NSColor
    let members: Int
    let unread: Int
}

final class CircleDiagram: NSView {
    var items: [DiagramItem] = [] { didSet { needsDisplay = true } }
    var onPick: ((Int) -> Void)?
    private var hits: [(NSRect, Int)] = []
    override var isFlipped: Bool { true }

    override func draw(_ dirty: NSRect) {
        hits = []
        let circles = items.filter { !$0.commons }, commons = items.filter { $0.commons }
        guard !items.isEmpty else { return }
        let w = bounds.width, h = bounds.height
        let groundH: CGFloat = commons.isEmpty ? 0 : 40
        let most = CGFloat(max(circles.map { $0.members }.max() ?? 1, 1))
        func radius(_ it: DiagramItem) -> CGFloat { 14 + 11 * sqrt(CGFloat(max(it.members, 1)) / most) }

        // Centres, evenly round an ellipse. The first angle is offset by half a
        // step so that no single circle sits alone at the top.
        var centre: [Int: NSPoint] = [:]
        let mid = NSPoint(x: w / 2, y: (h - groundH) / 2)
        let rx = min(w * 0.24, 200), ry = max(20, (h - groundH) / 2 - 32)
        var side: [Int: CGFloat] = [:]      // which way a disc faces out of the ring: -1 left, 1 right, 0 down
        let n = CGFloat(max(circles.count, 1))
        for (i, it) in circles.enumerated() {
            let a = (-90 + 180 / n + CGFloat(i) * 360 / n) * .pi / 180
            centre[it.index] = circles.count == 1 ? mid : NSPoint(x: mid.x + rx * cos(a), y: mid.y + ry * sin(a))
            side[it.index] = circles.count == 1 ? 0 : (cos(a) > 0.2 ? 1 : cos(a) < -0.2 ? -1 : 0)
        }

        // the ring that joins them
        if circles.count > 1 {
            Theme.faint.setStroke()
            let ring = NSBezierPath(ovalIn: NSRect(x: mid.x - rx, y: mid.y - ry, width: 2 * rx, height: 2 * ry))
            ring.lineWidth = 1.2
            ring.stroke()
        }

        // the commons: one band of ground with a chip for each
        if !commons.isEmpty {
            let ground = NSRect(x: 6, y: h - groundH, width: w - 12, height: groundH - 4)
            Theme.mint.setFill()
            NSBezierPath(roundedRect: ground, xRadius: 6, yRadius: 6).fill()
            let tag = NSAttributedString(string: "COMMONS", attributes: [.font: Theme.mono(9, bold: true), .foregroundColor: Theme.faint])
            tag.draw(at: NSPoint(x: ground.minX + 8, y: ground.midY - tag.size().height / 2))
            var x = ground.minX + 8 + tag.size().width + 12
            for it in commons {
                let words = NSAttributedString(string: it.title.uppercased() + (it.members > 0 ? "  \(it.members)" : ""), attributes: [
                    .font: Theme.mono(10, bold: true), .foregroundColor: Theme.on(it.color)])
                let chip = NSRect(x: x, y: ground.midY - 11, width: words.size().width + 16, height: 22)
                it.color.setFill()
                NSBezierPath(roundedRect: chip, xRadius: 4, yRadius: 4).fill()
                words.draw(at: NSPoint(x: chip.minX + 8, y: chip.midY - words.size().height / 2))
                drawUnread(it.unread, at: NSPoint(x: chip.maxX - 4, y: chip.minY - 5))
                hits.append((chip, it.index))
                x = chip.maxX + 10
            }
        }

        // the circles
        for it in circles {
            guard let c = centre[it.index] else { continue }
            let r = radius(it)
            let disc = NSRect(x: c.x - r, y: c.y - r, width: 2 * r, height: 2 * r)
            Theme.paper.setFill()
            NSBezierPath(ovalIn: disc.insetBy(dx: -2, dy: -2)).fill()     // a gap where a link meets the disc
            it.color.setFill()
            NSBezierPath(ovalIn: disc).fill()
            let count = NSAttributedString(string: "\(it.members)", attributes: [
                .font: Theme.mono(11, bold: true), .foregroundColor: Theme.on(it.color)])
            if it.members > 0 { count.draw(at: NSPoint(x: c.x - count.size().width / 2, y: c.y - count.size().height / 2)) }
            let name = NSAttributedString(string: it.title.uppercased(), attributes: [
                .font: Theme.mono(10, bold: true), .foregroundColor: Theme.ink])
            // the name goes on the outer side of the ring, clear of the other discs
            let out = side[it.index] ?? 0
            let at = out > 0 ? NSPoint(x: c.x + r + 7, y: c.y - name.size().height / 2)
                : out < 0 ? NSPoint(x: c.x - r - 7 - name.size().width, y: c.y - name.size().height / 2)
                : NSPoint(x: c.x - name.size().width / 2, y: c.y + r + 3)
            name.draw(at: at)
            drawUnread(it.unread, at: NSPoint(x: c.x + r * 0.6, y: c.y - r - 2))
            hits.append((disc.union(NSRect(origin: at, size: name.size())), it.index))
        }
    }

    private func drawUnread(_ n: Int, at p: NSPoint) {
        guard n > 0 else { return }
        let s = NSAttributedString(string: "\(n)", attributes: [.font: Theme.mono(9, bold: true), .foregroundColor: Theme.onYellow])
        let box = NSRect(x: p.x, y: p.y, width: max(18, s.size().width + 8), height: 12)
        Theme.yellow.setFill()
        NSBezierPath(roundedRect: box, xRadius: 6, yRadius: 6).fill()
        s.draw(at: NSPoint(x: box.midX - s.size().width / 2, y: box.midY - s.size().height / 2))
    }

    override func mouseDown(with event: NSEvent) {
        let p = convert(event.locationInWindow, from: nil)
        if let hit = hits.first(where: { $0.0.contains(p) }) { onPick?(hit.1) }
    }
}

// ─── rail ────────────────────────────────────────────────────────────────────

final class RailRow: NSView {
    private let bg = Fill(.clear, radius: 4)
    private let count = label("", Theme.mono(9, bold: true), Theme.onYellow)
    private let countBg = Fill(Theme.yellow, radius: 6)
    var onClick: (() -> Void)?
    var tag_ = -1
    var selected = false { didSet { bg.color = selected ? Theme.paper : .clear } }
    var unread = 0 { didSet { count.stringValue = "\(unread)"; countBg.isHidden = unread == 0 } }

    init(title: String, icon: NSImage?, color: NSColor? = nil) {
        super.init(frame: .zero)
        pin(bg, to: self)
        let chip = Fill(color ?? .clear, radius: 2)    // the circle's own colour, down the left edge
        chip.widthAnchor.constraint(equalToConstant: 4).isActive = true
        chip.heightAnchor.constraint(equalToConstant: 14).isActive = true
        let iv = NSImageView()
        iv.image = icon
        iv.contentTintColor = Theme.ink
        iv.imageScaling = .scaleProportionallyUpOrDown
        iv.widthAnchor.constraint(equalToConstant: 15).isActive = true
        iv.heightAnchor.constraint(equalToConstant: 15).isActive = true
        pin(count, to: countBg, 0)
        count.alignment = .center
        countBg.widthAnchor.constraint(greaterThanOrEqualToConstant: 20).isActive = true
        countBg.heightAnchor.constraint(equalToConstant: 12).isActive = true
        countBg.isHidden = true
        let row = NSStackView(views: [chip, iv, label(title.uppercased(), Theme.mono(11, bold: true)), NSView(), countBg])
        row.spacing = 5
        pin(row, to: self, 3)
        heightAnchor.constraint(equalToConstant: 22).isActive = true
    }
    required init?(coder: NSCoder) { fatalError() }
    override func mouseDown(with event: NSEvent) { onClick?() }
}

// ─── main ────────────────────────────────────────────────────────────────────

final class MainView: NSView, NSTextFieldDelegate, NSTextViewDelegate {
    private let rail = NSStackView()
    private let whoLabel = label("", Theme.mono(9), Theme.faint)
    private let title = label("", Theme.mono(11, bold: true), .white)
    private let band = Fill(Theme.green, radius: 3)
    private let diagram = CircleDiagram()
    private let roles = NSTextField(labelWithString: "")
    private let text = NSTextView()
    private let scroll = NSScrollView()
    private let field = NSTextField()
    private let status = label("", Theme.mono(9), Theme.faint)
    private let wikiHead = label("WIKI", Theme.mono(10, bold: true), Theme.faint)
    private let wikiText = NSTextView()
    private let wikiScroll = NSScrollView()
    private var rows: [RailRow] = []
    static let size: CGFloat = 11
    private var lastRender: (() -> Void)?
    private var renderedWidth: CGFloat = 0
    private var resizeWork: DispatchWorkItem?

    private var railWidth: NSLayoutConstraint!
    private var wikiWidth: NSLayoutConstraint!
    private var railView: NSView!
    private var wikiView: NSView!

    // In a narrow window (a cell of the slab tiler) the side columns fold away:
    // first the wiki, then the rail, leaving the sheet.
    private func fold() {
        let w = bounds.width
        let showWiki = w >= 760, showRail = w >= 470
        if wikiView.isHidden == showWiki {
            wikiView.isHidden = !showWiki
            wikiWidth.constant = showWiki ? 236 : 0
        }
        if railView.isHidden == showRail {
            railView.isHidden = !showRail
            railWidth.constant = showRail ? 132 : 0
        }
    }

    // Pictures are sized to the column, so a new width lays the ledger out again.
    override func layout() {
        fold()
        super.layout()
        guard let again = lastRender, abs((text.textContainer?.size.width ?? 0) - renderedWidth) > 24 else { return }
        resizeWork?.cancel()
        let work = DispatchWorkItem { again() }
        resizeWork = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.2, execute: work)
    }

    var onSelect: ((Int) -> Void)?
    var onSend: ((String) -> Void)?
    var onDoc: ((String) -> Void)?     // a wiki page was picked
    var onBack: (() -> Void)?          // leave the page, back to messages

    init() {
        super.init(frame: .zero)

        // rail
        let railBox = NSView()
        rail.orientation = .vertical
        rail.alignment = .leading
        rail.spacing = 1
        railBox.translatesAutoresizingMaskIntoConstraints = false
        rail.translatesAutoresizingMaskIntoConstraints = false
        whoLabel.translatesAutoresizingMaskIntoConstraints = false
        whoLabel.maximumNumberOfLines = 2
        railBox.addSubview(rail)
        railBox.addSubview(whoLabel)
        addSubview(railBox)

        // sheet
        let sheet = Fill(Theme.paper, radius: 5)
        addSubview(sheet)
        title.lineBreakMode = .byTruncatingTail
        pin(title, to: band, 3)
        band.heightAnchor.constraint(equalToConstant: 20).isActive = true
        roles.lineBreakMode = .byTruncatingTail
        roles.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)

        Self.dress(text, scroll)
        text.delegate = self
        Self.dress(wikiText, wikiScroll)
        wikiText.delegate = self

        field.font = Theme.mono(Self.size)
        field.isBordered = true
        field.bezelStyle = .squareBezel
        field.focusRingType = .none
        field.delegate = self
        field.target = self
        field.action = #selector(send)

        diagram.translatesAutoresizingMaskIntoConstraints = false
        diagram.heightAnchor.constraint(equalToConstant: 226).isActive = true
        diagram.isHidden = true
        diagram.onPick = { [weak self] i in self?.onSelect?(i) }
        let col = NSStackView(views: [band, roles, diagram, scroll, status, field])
        col.orientation = .vertical
        col.alignment = .leading
        col.spacing = 3
        pin(col, to: sheet, 5)
        for v in [band, diagram, scroll, field] as [NSView] { v.widthAnchor.constraint(equalTo: col.widthAnchor).isActive = true }
        scroll.setContentHuggingPriority(.defaultLow, for: .vertical)

        // wiki
        let wiki = Fill(Theme.paper, radius: 5)
        addSubview(wiki)
        let wcol = NSStackView(views: [wikiHead, wikiScroll])
        wcol.orientation = .vertical
        wcol.alignment = .leading
        wcol.spacing = 3
        pin(wcol, to: wiki, 5)
        wikiScroll.widthAnchor.constraint(equalTo: wcol.widthAnchor).isActive = true
        wikiScroll.setContentHuggingPriority(.defaultLow, for: .vertical)

        railWidth = railBox.widthAnchor.constraint(equalToConstant: 132)
        wikiWidth = wiki.widthAnchor.constraint(equalToConstant: 236)
        railView = railBox
        wikiView = wiki
        NSLayoutConstraint.activate([
            railBox.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 8),
            railBox.topAnchor.constraint(equalTo: topAnchor, constant: 8),
            railBox.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -6),
            railWidth,
            rail.leadingAnchor.constraint(equalTo: railBox.leadingAnchor),
            rail.trailingAnchor.constraint(equalTo: railBox.trailingAnchor),
            rail.topAnchor.constraint(equalTo: railBox.topAnchor),
            whoLabel.leadingAnchor.constraint(equalTo: railBox.leadingAnchor, constant: 3),
            whoLabel.trailingAnchor.constraint(equalTo: railBox.trailingAnchor),
            whoLabel.bottomAnchor.constraint(equalTo: railBox.bottomAnchor),
            sheet.leadingAnchor.constraint(equalTo: railBox.trailingAnchor, constant: 6),
            sheet.topAnchor.constraint(equalTo: topAnchor, constant: 6),
            sheet.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -6),
            wiki.leadingAnchor.constraint(equalTo: sheet.trailingAnchor, constant: 4),
            wiki.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -6),
            wiki.topAnchor.constraint(equalTo: sheet.topAnchor),
            wiki.bottomAnchor.constraint(equalTo: sheet.bottomAnchor),
            wikiWidth,
        ])
    }
    required init?(coder: NSCoder) { fatalError() }

    private static func dress(_ t: NSTextView, _ s: NSScrollView) {
        t.isEditable = false
        t.drawsBackground = false
        t.textContainerInset = NSSize(width: 2, height: 2)
        t.autoresizingMask = [.width]
        t.isVerticallyResizable = true
        t.textContainer?.widthTracksTextView = true
        t.linkTextAttributes = [.foregroundColor: NCCLogo.teal, .cursor: NSCursor.pointingHand]
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

    func setWho(_ s: String) { whoLabel.stringValue = s }
    func setStatus(_ s: String) { status.stringValue = s; status.isHidden = s.isEmpty }

    // sections: [(heading, [(index, title, colour)])]
    func setRail(_ sections: [(String, [(Int, String, NSColor?)])]) {
        rail.arrangedSubviews.forEach { $0.removeFromSuperview() }
        rows = []
        for (heading, items) in sections where !items.isEmpty {
            if !heading.isEmpty {
                let h = label(heading, Theme.mono(9, bold: true), Theme.faint)
                rail.addArrangedSubview(h)
                rail.setCustomSpacing(3, after: h)
            }
            for (index, title, color) in items {
                let row = RailRow(title: title, icon: Theme.icon(for: title), color: color)
                row.onClick = { [weak self] in self?.onSelect?(index) }
                row.tag_ = index
                rail.addArrangedSubview(row)
                row.widthAnchor.constraint(equalTo: rail.widthAnchor).isActive = true
                rows.append(row)
            }
            rail.setCustomSpacing(9, after: rail.arrangedSubviews.last!)
        }
    }

    func setUnread(_ index: Int, _ n: Int) { rows.first { $0.tag_ == index }?.unread = n }

    // One heading line, one roles line: "ROLE Name · ROLE Name".
    func show(index: Int, heading: String, color: NSColor? = nil, diagram items: [DiagramItem]? = nil,
              roleLine: [(String, Person)], canSend: Bool, placeholder: String) {
        for r in rows { r.selected = r.tag_ == index }
        title.stringValue = heading
        band.color = color ?? Theme.green        // the heading band wears the circle's colour
        title.textColor = color.map(Theme.on) ?? .white
        diagram.isHidden = items == nil
        if let items { diagram.items = items }
        let line = NSMutableAttributedString()
        for (i, (role, p)) in roleLine.enumerated() {
            if i > 0 { line.append(NSAttributedString(string: " · ", attributes: [.font: Theme.mono(10), .foregroundColor: Theme.faint])) }
            line.append(NSAttributedString(string: role.uppercased() + " ", attributes: [
                .font: Theme.mono(9, bold: true), .foregroundColor: Theme.faint]))
            line.append(NSAttributedString(string: p.name, attributes: [
                .font: Theme.mono(10, bold: true), .foregroundColor: Self.legible(p.color)]))
        }
        roles.attributedStringValue = line
        roles.isHidden = roleLine.isEmpty
        field.isEnabled = canSend
        field.placeholderString = placeholder
        field.isHidden = !canSend && placeholder.isEmpty   // START has nothing to write to
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
        text.textStorage?.setAttributedString(out)
        Self.linkify(text)
        text.scrollToEndOfDocument(nil)
    }

    // A wiki page in place of the ledger: its markdown as typed text, headings
    // in teal, links live. A link to another wiki page opens it here.
    func renderDoc(title: String, markdown: String, url: URL?, back: String?, wikiBase: String) {
        lastRender = nil
        let font = Theme.mono(Self.size)
        let out = NSMutableAttributedString()
        if let back {
            out.append(NSAttributedString(string: "← " + back, attributes: [
                .font: Theme.mono(10, bold: true), .link: URL(string: "ncc://back")!]))
            out.append(NSAttributedString(string: "    "))
        }
        if let url {
            out.append(NSAttributedString(string: "open in wiki ↗", attributes: [.font: Theme.mono(10, bold: true), .link: url]))
        }
        out.append(NSAttributedString(string: (out.length > 0 ? "\n\n" : "") + title.uppercased() + "\n", attributes: [
            .font: Theme.mono(13, bold: true), .foregroundColor: Theme.ink]))
        let para = NSMutableParagraphStyle()
        para.paragraphSpacing = 1
        let headStyle = NSMutableParagraphStyle()
        headStyle.paragraphSpacingBefore = 9
        headStyle.paragraphSpacing = 2
        var blank = false
        for raw in markdown.components(separatedBy: "\n") {
            let trimmed = raw.trimmingCharacters(in: .whitespaces)
            if trimmed.isEmpty || trimmed == "\\" {      // collapse runs of empty lines
                if !blank { out.append(NSAttributedString(string: "\n", attributes: [.font: Theme.mono(4)])) }
                blank = true
                continue
            }
            blank = false
            if raw.hasPrefix("#") {
                let words = raw.drop { $0 == "#" || $0 == " " }.uppercased()
                out.append(NSAttributedString(string: "▍" + words + "\n", attributes: [
                    .font: Theme.mono(Self.size, bold: true), .foregroundColor: Theme.teal, .paragraphStyle: headStyle]))
            } else {
                let line = Self.inline(raw, font: font, wikiBase: wikiBase)
                line.append(NSAttributedString(string: "\n"))
                line.addAttribute(.paragraphStyle, value: para, range: NSRange(location: 0, length: line.length))
                out.append(line)
            }
        }
        text.textStorage?.setAttributedString(out)
        Self.linkify(text)
        text.scrollToBeginningOfDocument(nil)
    }

    // Markdown links in one line: [words](address) and <address>.
    private static func inline(_ s: String, font: NSFont, wikiBase: String) -> NSMutableAttributedString {
        let out = NSMutableAttributedString()
        let plain: [NSAttributedString.Key: Any] = [.font: font, .foregroundColor: Theme.ink]
        guard let re = try? NSRegularExpression(pattern: #"\[([^\]]+)\]\(([^)\s]+)[^)]*\)|<(https?://[^>\s]+)>"#) else {
            return NSMutableAttributedString(string: s, attributes: plain)
        }
        let ns = s as NSString
        var at = 0
        for m in re.matches(in: s, range: NSRange(location: 0, length: ns.length)) {
            out.append(NSAttributedString(string: ns.substring(with: NSRange(location: at, length: m.range.location - at)), attributes: plain))
            let bare = m.range(at: 3).location != NSNotFound
            let words = ns.substring(with: m.range(at: bare ? 3 : 1))
            let address = ns.substring(with: m.range(at: bare ? 3 : 2))
            var target: URL?
            if address.hasPrefix("/doc/") {                 // another wiki page: open it in the sheet
                target = URL(string: "ncc://doc/" + (address as NSString).lastPathComponent)
            } else if address.hasPrefix("/") {
                target = URL(string: wikiBase + address)
            } else {
                target = URL(string: address)
            }
            var attrs = plain
            if let target { attrs[.link] = target }
            out.append(NSAttributedString(string: words, attributes: attrs))
            at = m.range.location + m.range.length
        }
        out.append(NSAttributedString(string: ns.substring(from: at), attributes: plain))
        return out
    }

    // The wiki column: the page tree, as its editors arranged it.
    func renderWiki(heading: String, rows: [WikiRow], note: String?) {
        wikiHead.stringValue = heading.uppercased()
        let font = Theme.mono(10)
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
                line = NSMutableAttributedString(string: "· ", attributes: [.font: font, .foregroundColor: Theme.faint])
                line.append(NSAttributedString(string: r.title + "\n", attributes: [
                    .font: font, .link: URL(string: "ncc://doc/" + id)!]))
            } else {
                style.paragraphSpacingBefore = 5
                line = NSMutableAttributedString(string: r.title.uppercased() + "\n", attributes: [
                    .font: Theme.mono(9, bold: true), .foregroundColor: Theme.faint])
            }
            line.addAttribute(.paragraphStyle, value: style, range: NSRange(location: 0, length: line.length))
            out.append(line)
        }
        wikiText.textStorage?.setAttributedString(out)
        wikiText.scrollToBeginningOfDocument(nil)
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
        return true
    }

    override func cancelOperation(_ sender: Any?) { onBack?() }

    @objc private func send() {
        let s = field.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !s.isEmpty, field.isEnabled else { return }
        onSend?(s)
    }

    func clearField() { field.stringValue = "" }
    func focusField() { window?.makeFirstResponder(field) }
}
