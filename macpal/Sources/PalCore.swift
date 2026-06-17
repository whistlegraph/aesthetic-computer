// PalCore — the shared heart of MacPal: a friendly little character who lives
// in the corner of your screen.
//
// This is the avatar and nothing domain-specific: a glyph that floats, blinks
// and leans on its own, springs when you hover, squashes when you press, and
// rides along when you drag (snapping to the nearest corner with a clink).
// Click it and it ESCAPES — collapses into a micro chip tucked into its corner,
// name beside it; click again and it tinks back. Corner, color, and collapsed
// state persist across launches. It also speaks the Menu Band "sing" protocol
// (see noteSignalDir below).
//
// Everything that differs between the distributable star (Fía) and the fuser
// machines' badge (neo/panda/chicken/blueberry) is a PalPlugin stacked beneath
// the name. The profile chosen in MacPal.swift decides the PalConfig + plugins.
//
// Built from one swiftc invocation over Sources/*.swift (see build.sh).

import AppKit

// ── shared helpers (visible to every Sources/*.swift file) ────────────────────
let accent = NSColor.controlAccentColor

func hexColor(_ v: UInt32) -> NSColor {
    NSColor(calibratedRed: CGFloat((v >> 16) & 0xff) / 255,
            green: CGFloat((v >> 8) & 0xff) / 255,
            blue: CGFloat(v & 0xff) / 255, alpha: 1)
}

func playfulFont(_ pt: CGFloat, bold: Bool) -> NSFont {
    let names = bold
        ? ["Comic Sans MS Bold", "ComicSansMS-Bold", "Chalkboard SE Bold", "ChalkboardSE-Bold"]
        : ["Comic Sans MS", "Chalkboard SE"]
    for n in names { if let f = NSFont(name: n, size: pt) { return f } }
    let base = NSFont(name: "Comic Sans MS", size: pt) ?? NSFont.systemFont(ofSize: pt, weight: .heavy)
    return bold ? NSFontManager.shared.convert(base, toHaveTrait: .boldFontMask) : base
}

func resource(_ name: String, _ ext: String) -> String? {
    Bundle.main.path(forResource: name, ofType: ext)
}

// ── profile configuration ─────────────────────────────────────────────────
// Seeded once in MacPal.swift from the launch profile. Replaces what used to be
// file-scope globals so the same controller drives the star and the badge.
struct PalConfig {
    var profile: String                  // "star" | "fuser"
    var palName: String                  // "star", or a machine name like "panda"
    var palEmoji: String                 // fallback if the glyph art can't load
    var supportDir: String               // per-profile state home
    var noteSignalDir: String            // Menu Band "now playing" dir (shared protocol)
    var factoryCorner: String            // where it sits before any drag
    var fixedCorner: String?             // fuser pins a corner; star is draggable (nil)
    var marginX: CGFloat                 // resting gap from the screen's side
    var marginY: CGFloat                 // resting gap from the screen's top/bottom
    var draggable: Bool                   // star drags to corners; badge stays put
    var colorTogglable: Bool             // star: gold/silver right-click toggle
    var registersLoginItem: Bool         // star adds itself to Login Items once
    var showsAbout: Bool                  // star shows the About panel
    var aboutDedication: String?         // the dedication line in About
    // 3D avatar (e.g. the blueberry). When set, the pal renders a SceneKit
    // model instead of the flat SVG/emoji glyph. `model3DPath` nil (or missing)
    // → a procedural placeholder berry, so the 3D path is always live.
    var model3D: Bool = false
    var model3DPath: String? = nil
    // When true, the right-click menu offers a 3D ⇄ 2D avatar toggle (the
    // blueberry can show its SceneKit self or a flat thumbnail). Persisted in
    // avatar2DFlag.
    var avatarTogglable: Bool = false
    // Glyph resolution. Given the active color ("gold"/"silver"/""), return the
    // absolute pose paths (home · blink · lean) and the open-mouth "sing" pose.
    // Star reads bundle Resources; the badge reads files staged in its home dir.
    var posePaths: (_ color: String) -> [String]
    var singPath: (_ color: String) -> String?

    var cornerFile: String { supportDir + "/corner" }
    var collapsedFlag: String { supportDir + "/collapsed" }
    var colorFile: String { supportDir + "/color" }
    var avatar2DFlag: String { supportDir + "/avatar2d" }   // present ⇒ 2D mode
    var noteSignalFile: String { noteSignalDir + "/note" }
}

// ── plugin protocol ─────────────────────────────────────────────────────────
// A plugin owns whatever stacks BENEATH the name. The controller owns the glyph,
// the name, collapse, drag, and the sing easter egg; it asks each plugin for its
// reserved height, lets it place its rows, and ticks it on the shared timer.
protocol PalPlugin: AnyObject {
    func attach(to c: PalController)                       // build subviews once
    func stackHeight(in c: PalController) -> CGFloat       // height to reserve under the name (full mode)
    func layoutRows(in c: PalController, originY: CGFloat) // place rows upward from originY; may append liveRects
    func setCollapsed(_ collapsed: Bool)                   // hide/show rows
    func tick()                                            // periodic refresh (polling)
    func menuItems(for c: PalController) -> [NSMenuItem]   // contributed right-click items
}
extension PalPlugin {
    func stackHeight(in c: PalController) -> CGFloat { 0 }
    func layoutRows(in c: PalController, originY: CGFloat) {}
    func setCollapsed(_ collapsed: Bool) {}
    func tick() {}
    func menuItems(for c: PalController) -> [NSMenuItem] { [] }
}

// ── windows & views ───────────────────────────────────────────────────────
// A Spotlight-style non-activating panel: clicking the pal never steals focus.
final class KeyWindow: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }
}

// Click-through everywhere except the registered live rects (the pal + any
// plugin rows that need the mouse, like the fuser pane).
final class PassThroughView: NSView {
    var liveRects: [NSRect] = []
    override func hitTest(_ p: NSPoint) -> NSView? {
        for r in liveRects where r.contains(p) { return super.hitTest(p) }
        return nil
    }
}

// The pal is also his own handle: press-and-move drags the whole window, a still
// press stays a click. Past a 4px slop the drag is handed to the window server
// via performDrag (app-side per-event moves stutter on a transparent window).
// The server swallows the mouseUp, so the drop is detected by polling
// pressedMouseButtons.
final class ClickView: NSView {
    var onClick: () -> Void = {}
    var onPress: () -> Void = {}
    var onDragStart: () -> Void = {}
    var onRightClick: (NSEvent) -> Void = { _ in }
    var draggable = true
    override func rightMouseDown(with e: NSEvent) { onRightClick(e) }
    private var dragging = false
    private var downAt = NSPoint.zero
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
    override func mouseDown(with e: NSEvent) {
        dragging = false
        downAt = NSEvent.mouseLocation
        onPress()
    }
    override func mouseDragged(with e: NSEvent) {
        guard draggable, !dragging else { return }
        let p = NSEvent.mouseLocation
        guard abs(p.x - downAt.x) > 4 || abs(p.y - downAt.y) > 4 else { return }
        dragging = true
        onDragStart()
        window?.performDrag(with: e)
    }
    override func mouseUp(with e: NSEvent) {
        if !dragging { onClick() }
        dragging = false
    }
}

// ── per-character name label ──────────────────────────────────────────────
// Each character is its own CALayer, hand-placed with a deterministic static
// jitter (baseline nudge + tilt). At idle they drift on a slow, subtle wave;
// bounce() fires a full-energy hop (used on expand / new affirmation / new
// fuser output).
final class CharLayer: CALayer {
    var attr: NSAttributedString?
    var inset: CGFloat = 0
    override func draw(in ctx: CGContext) {
        guard let attr = attr else { return }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
        attr.draw(at: NSPoint(x: inset, y: inset))
        NSGraphicsContext.restoreGraphicsState()
    }
}

final class WiggleLabel: NSView {
    private var chars: [CharLayer] = []
    private var advances: [CGFloat] = []
    private var widths: [CGFloat] = []
    private var textWidth: CGFloat = 0
    private let inset: CGFloat = 10

    init() {
        super.init(frame: .zero)
        layer = CALayer()
        wantsLayer = true
        layer?.masksToBounds = false
    }
    required init?(coder: NSCoder) { fatalError("no nib") }

    private func jitter(_ i: Int) -> (dy: CGFloat, rot: CGFloat) {
        var h: UInt32 = 2166136261
        for b in "char\(i)".utf8 { h = (h ^ UInt32(b)) &* 16777619 }
        let dy = CGFloat(Int(h % 7)) / 2 - 1.5
        let rot = (CGFloat(Int((h >> 8) % 9)) - 4) * 0.6 * .pi / 180
        return (dy, rot)
    }

    func setText(_ s: String, font: NSFont, fill: NSColor,
                 stroke: NSColor, strokeW: CGFloat) {
        chars.forEach { $0.removeFromSuperlayer() }
        chars = []; advances = []; widths = []; textWidth = 0
        let sh = NSShadow()
        sh.shadowColor = accent; sh.shadowBlurRadius = 0
        sh.shadowOffset = NSSize(width: 2, height: 2)
        let scale = NSScreen.main?.backingScaleFactor ?? 2
        for ch in s {
            let a = NSAttributedString(string: String(ch), attributes: [
                .font: font, .foregroundColor: fill,
                .strokeColor: stroke, .strokeWidth: -strokeW,
                .shadow: sh,
            ])
            let size = a.size()
            let l = CharLayer()
            l.attr = a; l.inset = inset
            l.contentsScale = scale
            l.bounds = CGRect(x: 0, y: 0,
                              width: size.width + inset * 2,
                              height: size.height + inset * 2)
            l.anchorPoint = CGPoint(x: 0.5, y: 0.5)
            let j = jitter(chars.count)
            var t = CATransform3DMakeTranslation(0, j.dy, 0)
            t = CATransform3DRotate(t, j.rot, 0, 0, 1)
            l.transform = t
            layer?.addSublayer(l)
            chars.append(l)
            advances.append(textWidth)
            widths.append(size.width)
            textWidth += size.width
            l.setNeedsDisplay()
        }
        layoutChars()
        startWiggle()
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        layoutChars()
    }

    private func layoutChars() {
        CATransaction.begin(); CATransaction.setDisableActions(true)
        let x0 = (bounds.width - textWidth) / 2
        for (i, l) in chars.enumerated() {
            l.position = CGPoint(x: x0 + advances[i] + widths[i] / 2,
                                 y: bounds.height / 2)
        }
        CATransaction.commit()
    }

    private func ease(_ n: Int) -> [CAMediaTimingFunction] {
        Array(repeating: CAMediaTimingFunction(name: .easeInEaseOut), count: n)
    }

    private func startWiggle() {
        let t0 = CACurrentMediaTime()
        for (i, l) in chars.enumerated() {
            let begin = t0 + Double(i) * 0.12
            let y = CAKeyframeAnimation(keyPath: "transform.translation.y")
            y.values = [0, 1.2, -0.8, 0]
            y.keyTimes = [0, 0.25, 0.75, 1]
            y.timingFunctions = ease(3)
            y.duration = 1.8; y.repeatCount = .infinity
            y.beginTime = begin; y.isAdditive = true
            l.add(y, forKey: "wiggleY")
            let r = CAKeyframeAnimation(keyPath: "transform.rotation.z")
            r.values = [0, 1.2 * Double.pi / 180, -0.8 * Double.pi / 180, 0]
            r.keyTimes = [0, 0.25, 0.75, 1]
            r.timingFunctions = ease(3)
            r.duration = 1.8; r.repeatCount = .infinity
            r.beginTime = begin; r.isAdditive = true
            l.add(r, forKey: "wiggleR")
        }
    }

    func bounce() {
        let t0 = CACurrentMediaTime()
        for (i, l) in chars.enumerated() {
            let begin = t0 + Double(i) * 0.05
            let y = CAKeyframeAnimation(keyPath: "transform.translation.y")
            y.values = [0, 8, 0]
            y.keyTimes = [0, 0.5, 1]
            y.timingFunctions = ease(2)
            y.duration = 0.6; y.beginTime = begin; y.isAdditive = true
            l.add(y, forKey: "bounceY")
            let s = CAKeyframeAnimation(keyPath: "transform.scale")
            s.values = [0, 0.2, 0]
            s.keyTimes = [0, 0.5, 1]
            s.timingFunctions = ease(2)
            s.duration = 0.6; s.beginTime = begin; s.isAdditive = true
            l.add(s, forKey: "bounceS")
        }
    }
}

// ── the pal ────────────────────────────────────────────────────────────────
final class PalController: NSObject {
    let config: PalConfig
    var plugins: [PalPlugin] = []

    var window: NSWindow!
    var content: PassThroughView!
    var glyphView: NSView!
    var clickCatcher: ClickView!
    var glyphW: CGFloat = 128
    var glyphH: CGFloat = 116
    var glyphImages: [NSImage] = []
    var glyphBitmaps: [NSBitmapImageRep] = []   // rasterized poses, for alpha hit-tests
    var glyphState = 0
    let nameLabel = WiggleLabel()
    let microLabel = NSTextField(labelWithString: "")
    var draggingBadge = false
    var hovering = false
    // Menu Band "sing" easter egg state.
    var singImage: NSImage?            // open-mouth pose for the current color
    var lastNoteSeq = -1               // last seq seen in the note signal (-1 = unread)
    var singUntil: CFTimeInterval = 0  // hold the open mouth until this time
    var showingSing = false

    var starColor: String
    var collapsed: Bool
    var corner: String
    var use3D: Bool            // render the SceneKit avatar vs the flat glyph
    let W: CGFloat = 160        // full-size window width (the star); plugins may widen via plugin metrics
    let micro: CGFloat = 34     // collapsed glyph size
    var marginX: CGFloat { config.marginX }
    var marginY: CGFloat { config.marginY }

    init(config: PalConfig) {
        self.config = config
        try? FileManager.default.createDirectory(atPath: config.supportDir, withIntermediateDirectories: true)
        try? FileManager.default.createDirectory(atPath: config.noteSignalDir, withIntermediateDirectories: true)
        if config.colorTogglable {
            let s = ((try? String(contentsOfFile: config.colorFile, encoding: .utf8)) ?? "")
                .trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
            starColor = s == "silver" ? "silver" : "gold"
        } else {
            starColor = ""
        }
        collapsed = FileManager.default.fileExists(atPath: config.collapsedFlag)
        // 3D by default when a model is configured; the menu can flip to 2D.
        use3D = config.model3D && !FileManager.default.fileExists(atPath: config.avatar2DFlag)
        if let fixed = config.fixedCorner,
           ["TL", "TR", "BL", "BR"].contains(fixed.uppercased()) {
            corner = fixed.uppercased()
        } else {
            let saved = ((try? String(contentsOfFile: config.cornerFile, encoding: .utf8)) ?? "")
                .trimmingCharacters(in: .whitespacesAndNewlines).uppercased()
            corner = ["TL", "TR", "BL", "BR"].contains(saved) ? saved : config.factoryCorner
        }
        super.init()
    }

    func styleField(_ f: NSTextField) {
        f.isBordered = false; f.drawsBackground = false; f.alignment = .center
        let sh = NSShadow()
        sh.shadowColor = accent; sh.shadowBlurRadius = 0
        sh.shadowOffset = NSSize(width: 2, height: 2)
        f.shadow = sh
    }

    // Build the glyph view for the current avatar mode — reusable so the 3D⇄2D
    // menu toggle can swap it live. The 3D pal gets a bigger frame so it reads.
    func makeGlyphView() -> NSView {
        if use3D, let path = config.model3DPath {
            glyphW = 188; glyphH = 188
            let v = Pal3DView(frame: NSRect(x: (fullWidth - glyphW) / 2, y: 0, width: glyphW, height: glyphH),
                              objPath: path, tint: accent)
            v.wantsLayer = true
            return v
        }
        glyphW = 128; glyphH = 116
        if let img = glyphImages.first {
            let iv = NSImageView(frame: NSRect(x: (fullWidth - glyphW) / 2, y: 0, width: glyphW, height: glyphH))
            iv.image = img
            iv.imageScaling = .scaleProportionallyUpOrDown
            iv.wantsLayer = true
            let sh = NSShadow()
            sh.shadowColor = accent; sh.shadowBlurRadius = 0
            sh.shadowOffset = NSSize(width: 2, height: -2)
            iv.shadow = sh
            DispatchQueue.main.async { [weak self] in self?.scheduleGlyphPose() }
            return iv
        }
        glyphH = 76
        let e = NSTextField(labelWithString: config.palEmoji)
        e.font = NSFont.systemFont(ofSize: 60); e.alignment = .center
        e.isBordered = false; e.drawsBackground = false
        return e
    }

    // Swap the avatar in place (3D ⇄ 2D) without relaunching.
    func rebuildGlyph() {
        let old = glyphView
        let nv = makeGlyphView()
        glyphView = nv
        if let cc = clickCatcher {
            content.addSubview(nv, positioned: .below, relativeTo: cc)
        } else {
            content.addSubview(nv)
        }
        old?.removeFromSuperview()
        layout()
    }

    func build() {
        let win = KeyWindow(contentRect: NSRect(x: 0, y: 0, width: W, height: 158),
                            styleMask: [.borderless, .nonactivatingPanel],
                            backing: .buffered, defer: false)
        win.isOpaque = false; win.backgroundColor = .clear; win.level = .floating
        win.ignoresMouseEvents = false; win.hasShadow = false
        win.acceptsMouseMovedEvents = true
        win.hidesOnDeactivate = false
        win.becomesKeyOnlyIfNeeded = true
        win.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]

        content = PassThroughView(frame: NSRect(x: 0, y: 0, width: W, height: 158))
        content.wantsLayer = true

        loadGlyphs()
        glyphView = makeGlyphView()
        content.addSubview(glyphView)

        nameLabel.setText(config.palName, font: playfulFont(29, bold: true),
                          fill: .white, stroke: NSColor(white: 0.08, alpha: 1), strokeW: 4)
        styleField(microLabel)
        microLabel.attributedStringValue = NSAttributedString(string: config.palName, attributes: [
            .font: playfulFont(14, bold: true),
            .foregroundColor: NSColor.white,
            .strokeColor: NSColor(white: 0.08, alpha: 1),
            .strokeWidth: -3.0,
        ])
        microLabel.isHidden = true
        content.addSubview(nameLabel)
        content.addSubview(microLabel)

        // Plugins build their rows before the click-catcher so they sit beneath it.
        for p in plugins { p.attach(to: self) }

        clickCatcher = ClickView(frame: .zero)
        clickCatcher.draggable = config.draggable
        clickCatcher.onClick = { [weak self] in self?.toggleCollapsed() }
        clickCatcher.onPress = { [weak self] in self?.pressPop() }
        clickCatcher.onRightClick = { [weak self] e in self?.showContextMenu(e) }
        clickCatcher.onDragStart = { [weak self] in
            self?.draggingBadge = true
            Timer.scheduledTimer(withTimeInterval: 0.03, repeats: true) { [weak self] t in
                guard let self = self else { t.invalidate(); return }
                if NSEvent.pressedMouseButtons & 1 == 0 {
                    t.invalidate()
                    self.snapToNearestCorner()
                }
            }
        }
        content.addSubview(clickCatcher)

        win.contentView = content
        win.alphaValue = 0.97
        self.window = win
        startCursorWatch()
        layout()
        win.orderFrontRegardless()

        // 20Hz tick keeps the click-through flag fresh and drives the note poll.
        Timer.scheduledTimer(withTimeInterval: 0.05, repeats: true) { [weak self] _ in
            self?.updateClickability()
            self?.tickNotes()
        }
        // Plugins poll on their own cadence off a 1s tick (cheap; each gates itself).
        Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] _ in
            self?.plugins.forEach { $0.tick() }
        }
        plugins.forEach { $0.tick() }   // first fill immediately
        NotificationCenter.default.addObserver(self, selector: #selector(layout),
            name: NSApplication.didChangeScreenParametersNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(accentChanged),
            name: NSColor.systemColorsDidChangeNotification, object: nil)
    }

    @objc func accentChanged() {
        nameLabel.setText(config.palName, font: playfulFont(29, bold: true),
                          fill: .white, stroke: NSColor(white: 0.08, alpha: 1), strokeW: 4)
        glyphView?.needsDisplay = true
        microLabel.needsDisplay = true
    }

    // ── glyph art ─────────────────────────────────────────────────────────
    // NSImage renders the SVG natively — no WKWebView (its SafeBrowsing init can
    // deadlock the runloop, and its clipped box crops the drop shadow).
    func loadGlyphs() {
        glyphImages = config.posePaths(starColor).compactMap { NSImage(contentsOfFile: $0) }
        glyphBitmaps = glyphImages.compactMap {
            $0.tiffRepresentation.flatMap { NSBitmapImageRep(data: $0) }
        }
        singImage = config.singPath(starColor).flatMap { NSImage(contentsOfFile: $0) }
    }

    func setStarColor(_ c: String) {
        guard config.colorTogglable, c != starColor, c == "gold" || c == "silver" else { return }
        starColor = c
        try? c.write(toFile: config.colorFile, atomically: true, encoding: .utf8)
        loadGlyphs()
        glyphState = min(glyphState, max(0, glyphImages.count - 1))
        if let iv = glyphView as? NSImageView, !glyphImages.isEmpty {
            iv.image = glyphImages[glyphState]
            if let l = iv.layer {   // a little squash sells the change
                let s = CAKeyframeAnimation(keyPath: "transform.scale")
                s.values = [0, 0.08, 0]; s.keyTimes = [0, 0.45, 1]
                s.duration = 0.3; s.isAdditive = true
                l.add(s, forKey: "colorPop")
            }
        }
        nameLabel.bounce()
        let snd = NSSound(named: "Tink"); snd?.volume = 0.4; snd?.play()
    }

    // ── right-click menu ──────────────────────────────────────────────────
    // Plugins contribute items (fuser: Overtime); the star adds About + color +
    // Quit. A fuser badge with no About just shows Overtime · Quit.
    func showContextMenu(_ e: NSEvent) {
        let menu = NSMenu()
        var items = plugins.flatMap { $0.menuItems(for: self) }
        if config.showsAbout {
            let about = NSMenuItem(title: "About MacPal", action: #selector(showAbout), keyEquivalent: "")
            about.target = self
            items.append(about)
        }
        if config.colorTogglable {
            let gold = NSMenuItem(title: "Gold Star", action: #selector(pickGold), keyEquivalent: "")
            gold.target = self; gold.state = starColor == "gold" ? .on : .off
            let silver = NSMenuItem(title: "Silver Star", action: #selector(pickSilver), keyEquivalent: "")
            silver.target = self; silver.state = starColor == "silver" ? .on : .off
            items.append(gold); items.append(silver)
        }
        if config.avatarTogglable {
            let m3 = NSMenuItem(title: "3D Blueberry", action: #selector(use3DAvatar), keyEquivalent: "")
            m3.target = self; m3.state = use3D ? .on : .off
            let m2 = NSMenuItem(title: "2D Blueberry", action: #selector(use2DAvatar), keyEquivalent: "")
            m2.target = self; m2.state = use3D ? .off : .on
            items.append(m3); items.append(m2)
        }
        items.forEach { menu.addItem($0) }
        if !items.isEmpty { menu.addItem(.separator()) }
        let quit = NSMenuItem(title: "Quit MacPal", action: #selector(quit), keyEquivalent: "q")
        quit.target = self
        menu.addItem(quit)
        NSMenu.popUpContextMenu(menu, with: e, for: clickCatcher)
    }

    @objc func showAbout() { AboutWindow.show(config: config) }
    @objc func pickGold() { setStarColor("gold") }
    @objc func pickSilver() { setStarColor("silver") }
    @objc func use3DAvatar() { setAvatar3D(true) }
    @objc func use2DAvatar() { setAvatar3D(false) }
    @objc func quit() { NSApp.terminate(nil) }

    // Swap between the SceneKit pal and the flat thumbnail; the choice persists.
    func setAvatar3D(_ on: Bool) {
        guard config.avatarTogglable, on != use3D else { return }
        use3D = on
        if on { try? FileManager.default.removeItem(atPath: config.avatar2DFlag) }
        else { FileManager.default.createFile(atPath: config.avatar2DFlag, contents: nil) }
        rebuildGlyph()
        nameLabel.bounce()
        let s = NSSound(named: "Tink"); s?.volume = 0.4; s?.play()
    }

    // ── Menu Band "sing" easter egg ───────────────────────────────────────
    func tickNotes() {
        let now = CACurrentMediaTime()
        if showingSing, now >= singUntil {
            showingSing = false
            if let iv = glyphView as? NSImageView, !glyphImages.isEmpty {
                iv.image = glyphImages[min(glyphState, glyphImages.count - 1)]
            }
        }
        guard let raw = try? String(contentsOfFile: config.noteSignalFile, encoding: .utf8) else { return }
        let parts = raw.trimmingCharacters(in: .whitespacesAndNewlines)
            .split(separator: " ", maxSplits: 1)
        guard let first = parts.first, let seq = Int(first) else { return }
        if lastNoteSeq < 0 { lastNoteSeq = seq; return }
        guard seq != lastNoteSeq else { return }
        let burst = seq > lastNoteSeq ? min(seq - lastNoteSeq, 3) : 1
        lastNoteSeq = seq
        let name = parts.count > 1 ? String(parts[1]) : ""
        for k in 0..<burst {
            DispatchQueue.main.asyncAfter(deadline: .now() + Double(k) * 0.07) {
                [weak self] in self?.sing(name)
            }
        }
    }

    func sing(_ name: String) {
        singUntil = CACurrentMediaTime() + 0.26
        if let pal = glyphView as? Pal3DView { pal.sing(); pal.playClip("cheer") }   // 3D pal cheers to the music
        if let iv = glyphView as? NSImageView, let s = singImage {
            iv.image = s
            showingSing = true
            if let l = iv.layer {
                let pop = CAKeyframeAnimation(keyPath: "transform.scale")
                pop.values = [0, 0.10, 0]; pop.keyTimes = [0, 0.4, 1]
                pop.duration = 0.26; pop.isAdditive = true
                l.add(pop, forKey: "singPop")
            }
        }
        if !collapsed { floatNote(name) }
    }

    // A musical glyph rises out of the mouth, drifting + fading + spinning a
    // touch, tinted by the note's pitch class.
    func floatNote(_ name: String) {
        let glyphs = ["♪", "♫", "♩", "♬"]
        let pick = glyphs[((name.hashValue % glyphs.count) + glyphs.count) % glyphs.count]
        // In 3D the note is spawned inside the SceneKit scene (a CALayer overlay
        // would render behind the metal view).
        if let pal = glyphView as? Pal3DView {
            pal.floatNote(pick, color: noteTint(name), up: !corner.hasPrefix("T"))
            return
        }
        guard let host = content.layer else { return }
        let t = CATextLayer()
        t.string = pick
        t.font = NSFont.boldSystemFont(ofSize: 19)
        t.fontSize = 19
        t.foregroundColor = noteTint(name).cgColor
        t.alignmentMode = .center
        t.zPosition = 1000   // float ABOVE the 3D SCNView's metal layer, not behind it
        t.contentsScale = NSScreen.main?.backingScaleFactor ?? 2
        t.shadowColor = NSColor.black.cgColor
        t.shadowOffset = CGSize(width: 1, height: -1)
        t.shadowOpacity = 0.45
        t.shadowRadius = 0.5
        t.bounds = CGRect(x: 0, y: 0, width: 26, height: 26)
        let gf = glyphView.frame
        let startX = gf.midX + CGFloat.random(in: -7...7)
        let startY = gf.midY - 4 + CGFloat.random(in: -3...3)
        t.position = CGPoint(x: startX, y: startY)
        t.opacity = 0
        host.addSublayer(t)
        let dx = CGFloat.random(in: -14...14)
        let downward = collapsed && corner.hasPrefix("T")
        let rise = CGFloat.random(in: 34...48) * (downward ? -1 : 1)
        let move = CABasicAnimation(keyPath: "position")
        move.fromValue = NSValue(point: t.position)
        move.toValue = NSValue(point: CGPoint(x: startX + dx, y: startY + rise))
        let fade = CAKeyframeAnimation(keyPath: "opacity")
        fade.values = [0, 1, 1, 0]; fade.keyTimes = [0, 0.18, 0.65, 1]
        let scale = CAKeyframeAnimation(keyPath: "transform.scale")
        scale.values = [0.3, 1.18, 1.0]; scale.keyTimes = [0, 0.32, 1]
        let spin = CABasicAnimation(keyPath: "transform.rotation.z")
        spin.fromValue = 0; spin.toValue = Double.random(in: -0.5...0.5)
        let grp = CAAnimationGroup()
        grp.animations = [move, fade, scale, spin]
        grp.duration = 0.95
        grp.timingFunction = CAMediaTimingFunction(name: .easeOut)
        grp.isRemovedOnCompletion = false
        grp.fillMode = .forwards
        CATransaction.begin()
        CATransaction.setCompletionBlock { t.removeFromSuperlayer() }
        t.add(grp, forKey: "float")
        CATransaction.commit()
    }

    func noteTint(_ name: String) -> NSColor {
        let map: [Character: UInt32] = [
            "C": 0xff5d5d, "D": 0xffa53d, "E": 0xffe14d, "F": 0x6ce06c,
            "G": 0x4dd0ff, "A": 0x6b8cff, "B": 0xd08aff,
        ]
        if let c = name.uppercased().first, let v = map[c] { return hexColor(v) }
        return accent
    }

    // He lingers in his home pose, pops to a blink/lean variant for a beat
    // (occasionally chaining), and settles back — a little squash sells it.
    func scheduleGlyphPose() {
        guard glyphImages.count > 1 else { return }
        let hold = glyphState == 0 ? Double.random(in: 2.2...5.0)
                                   : Double.random(in: 0.45...1.0)
        DispatchQueue.main.asyncAfter(deadline: .now() + hold) { [weak self] in
            guard let self = self, let iv = self.glyphView as? NSImageView else { return }
            if CACurrentMediaTime() < self.singUntil { self.scheduleGlyphPose(); return }
            if self.glyphState == 0 {
                self.glyphState = Int.random(in: 1..<self.glyphImages.count)
            } else if self.glyphImages.count > 2, Int.random(in: 0..<4) == 0 {
                self.glyphState = self.glyphState == 1 ? 2 : 1
            } else {
                self.glyphState = 0
            }
            iv.image = self.glyphImages[self.glyphState]
            if let l = iv.layer {
                let s = CAKeyframeAnimation(keyPath: "transform.scale")
                s.values = [0, 0.04, 0]; s.keyTimes = [0, 0.45, 1]
                s.duration = 0.26
                s.isAdditive = true
                l.add(s, forKey: "posePop")
            }
            self.scheduleGlyphPose()
        }
    }

    func toggleCollapsed() {
        collapsed.toggle()
        if collapsed { FileManager.default.createFile(atPath: config.collapsedFlag, contents: nil) }
        else { try? FileManager.default.removeItem(atPath: config.collapsedFlag) }
        let s = NSSound(named: collapsed ? "Pop" : "Tink")
        s?.volume = 0.5
        s?.play()
        plugins.forEach { $0.setCollapsed(collapsed) }
        layout()
        if !collapsed { nameLabel.bounce() }
    }

    // ── per-pixel click-through ───────────────────────────────────────────
    func glyphHit(_ p: NSPoint) -> Bool {
        guard let iv = glyphView as? NSImageView, let img = iv.image else { return true }
        let rep = glyphState < glyphBitmaps.count ? glyphBitmaps[glyphState] : glyphBitmaps.first
        guard let rep = rep else { return true }
        let b = iv.bounds, isz = img.size
        guard isz.width > 0, isz.height > 0, b.width > 0, b.height > 0 else { return true }
        let s = min(b.width / isz.width, b.height / isz.height)
        let fit = NSRect(x: (b.width - isz.width * s) / 2,
                         y: (b.height - isz.height * s) / 2,
                         width: isz.width * s, height: isz.height * s)
        guard fit.contains(p) else { return false }
        let px = Int((p.x - fit.minX) / fit.width * CGFloat(rep.pixelsWide))
        let py = Int((1 - (p.y - fit.minY) / fit.height) * CGFloat(rep.pixelsHigh))
        guard px >= 0, px < rep.pixelsWide, py >= 0, py < rep.pixelsHigh else { return false }
        return (rep.colorAt(x: px, y: py)?.alphaComponent ?? 0) > 0.08
    }

    func startCursorWatch() {
        let mask: NSEvent.EventTypeMask = [.mouseMoved, .leftMouseDragged, .leftMouseDown]
        NSEvent.addGlobalMonitorForEvents(matching: mask) { [weak self] e in
            guard let self = self else { return }
            self.updateClickability()
            if e.type == .leftMouseDown, let window = self.window {
                let p = window.convertPoint(fromScreen: NSEvent.mouseLocation)
                if self.glyphView.frame.contains(p),
                   self.glyphHit(self.glyphView.convert(p, from: nil)) {
                    self.pressPop()
                    self.toggleCollapsed()
                }
            }
        }
        NSEvent.addLocalMonitorForEvents(matching: mask) { [weak self] e in
            self?.updateClickability()
            return e
        }
    }

    func updateClickability() {
        guard !draggingBadge, let window = window else { return }
        let m = NSEvent.mouseLocation
        var live = false
        if window.frame.contains(m) {
            let p = window.convertPoint(fromScreen: m)
            if collapsed {
                live = microChipRect.contains(p)
            } else if glyphView.frame.contains(p) {
                live = glyphHit(glyphView.convert(p, from: nil))
            } else {
                // A plugin row under the mouse (e.g. the fuser pane) keeps the
                // window live so it can be scrolled / selected.
                for r in content.liveRects where r != glyphView.frame && r.contains(p) { live = true }
            }
            window.ignoresMouseEvents = !live
        }
        setHover(live && (collapsed
            || glyphView.frame.contains(window.convertPoint(fromScreen: m))))
    }

    func setHover(_ h: Bool) {
        guard hovering != h else { return }
        hovering = h
        if h { (glyphView as? Pal3DView)?.playClip("wave") }   // 3D pal waves hello
        guard let l = glyphView.layer else { return }
        let s: CGFloat = h ? 1.12 : 1.0
        let target = CATransform3DMakeScale(s, s, 1)
        let a = CASpringAnimation(keyPath: "transform")
        a.fromValue = l.presentation()?.transform ?? l.transform
        a.toValue = target
        a.damping = 11; a.stiffness = 170; a.mass = 0.7; a.initialVelocity = 6
        a.duration = a.settlingDuration
        l.add(a, forKey: "hoverScale")
        l.transform = target
        if h {
            let snd = NSSound(named: "Tink")
            snd?.volume = 0.22
            snd?.play()
        }
    }

    func pressPop() {
        guard let l = glyphView.layer else { return }
        let a = CAKeyframeAnimation(keyPath: "transform.scale")
        a.values = [0, -0.10, -0.05]
        a.keyTimes = [0, 0.6, 1]
        a.duration = 0.1
        a.isAdditive = true
        l.add(a, forKey: "press")
    }

    func snapToNearestCorner() {
        guard draggingBadge else { return }
        draggingBadge = false
        guard let screen = window.screen ?? NSScreen.main ?? NSScreen.screens.first else { return }
        let f = screen.frame
        let mid = NSPoint(x: window.frame.midX, y: window.frame.midY)
        corner = (mid.y > f.midY ? "T" : "B") + (mid.x > f.midX ? "R" : "L")
        try? corner.write(toFile: config.cornerFile, atomically: true, encoding: .utf8)
        let dropped = window.frame
        layout()
        let target = window.frame
        window.setFrame(dropped, display: false)
        NSAnimationContext.runAnimationGroup { ctx in
            ctx.duration = 0.25
            ctx.timingFunction = CAMediaTimingFunction(name: .easeOut)
            window.animator().setFrame(target, display: true)
        }
        let s = NSSound(named: "Bottle")
        s?.volume = 0.4
        s?.play()
    }

    var microChipRect = NSRect.zero

    // The full window width: the star is W; a plugin (the fuser pane) may want
    // more, so plugins can raise it via `preferredWidth`.
    var fullWidth: CGFloat {
        plugins.map { ($0 as? WidthHinting)?.preferredWidth ?? W }.max() ?? W
    }

    @objc func layout() {
        guard !draggingBadge else { return }
        guard let screen = window.screen ?? NSScreen.main ?? NSScreen.screens.first else {
            DispatchQueue.main.asyncAfter(deadline: .now() + 2) { [weak self] in self?.layout() }
            return
        }
        let f = screen.frame

        if collapsed {
            let gap: CGFloat = -3
            let nat = microLabel.intrinsicContentSize
            let lblW = ceil(nat.width) + 10
            let lblH = ceil(nat.height) + 8
            let totalW = micro + gap + lblW
            let vf = screen.visibleFrame
            var o = NSPoint(x: f.minX + 6, y: vf.maxY - micro - 1)
            switch corner {
            case "TR": o = NSPoint(x: f.maxX - totalW - 6, y: vf.maxY - micro - 1)
            case "BL": o = NSPoint(x: f.minX + 6, y: vf.minY + 6)
            case "BR": o = NSPoint(x: f.maxX - totalW - 6, y: vf.minY + 6)
            default: break
            }
            window.setFrame(NSRect(origin: o, size: NSSize(width: totalW, height: micro)),
                            display: true)
            content.frame = NSRect(x: 0, y: 0, width: totalW, height: micro)
            let rightAligned = corner.hasSuffix("R")
            glyphView.frame = NSRect(x: rightAligned ? totalW - micro : 0, y: 0,
                                     width: micro, height: micro)
            if let e = glyphView as? NSTextField { e.font = NSFont.systemFont(ofSize: 24) }
            nameLabel.isHidden = true
            microLabel.isHidden = false
            microLabel.frame = NSRect(x: rightAligned ? totalW - micro - gap - lblW : micro + gap,
                                      y: (micro - lblH) / 2 - 3, width: lblW, height: lblH)
            microChipRect = NSRect(x: 0, y: 0, width: totalW, height: micro)
            clickCatcher.frame = microChipRect
            content.liveRects = [microChipRect]
            plugins.forEach { $0.setCollapsed(true) }
            if let l = glyphView.layer {
                l.anchorPoint = CGPoint(x: 0.5, y: 0.5)
                l.position = CGPoint(x: glyphView.frame.midX, y: glyphView.frame.midY)
            }
            return
        }

        microLabel.isHidden = true
        nameLabel.isHidden = false
        plugins.forEach { $0.setCollapsed(false) }
        if let e = glyphView as? NSTextField { e.font = NSFont.systemFont(ofSize: 60) }

        let win = fullWidth
        // Stack, bottom-up: [plugin rows] · name · glyph.
        let stackH = plugins.reduce(CGFloat(0)) { $0 + $1.stackHeight(in: self) }
        let nameY: CGFloat = 12 + stackH
        let glyphY = nameY + 44 + 4
        let totalH = glyphY + glyphH + 8
        var o = NSPoint(x: f.minX + marginX, y: f.maxY - totalH - marginY)   // TL
        switch corner {
        case "TR": o = NSPoint(x: f.maxX - win - marginX, y: f.maxY - totalH - marginY)
        case "BL": o = NSPoint(x: f.minX + marginX, y: f.minY + marginY)
        case "BR": o = NSPoint(x: f.maxX - win - marginX, y: f.minY + marginY)
        default: break
        }
        window.setFrame(NSRect(origin: o, size: NSSize(width: win, height: totalH)), display: true)
        content.frame = NSRect(x: 0, y: 0, width: win, height: totalH)
        let gw: CGFloat = (glyphView is NSImageView) ? glyphW : win
        glyphView.frame = NSRect(x: (win - gw) / 2, y: glyphY, width: gw, height: glyphH)
        nameLabel.frame = NSRect(x: 0, y: nameY, width: win, height: 44)
        clickCatcher.frame = glyphView.frame
        content.liveRects = [glyphView.frame]
        // Plugins lay their rows out bottom-up from y=12 and append any live rects.
        var cursor: CGFloat = 12
        for p in plugins {
            p.layoutRows(in: self, originY: cursor)
            cursor += p.stackHeight(in: self)
        }
        if let l = glyphView.layer {
            l.anchorPoint = CGPoint(x: 0.5, y: 0.5)
            l.position = CGPoint(x: glyphView.frame.midX, y: glyphView.frame.midY)
        }
    }
}

// A plugin that wants a wider window than the star's default declares it.
protocol WidthHinting { var preferredWidth: CGFloat { get } }

// ── About window ───────────────────────────────────────────────────────────
// A small transparent-titlebar window with a centered masthead — glyph icon,
// bold "MacPal", a dedication, then the version + copyright footer. The "í" in
// the dedication wears the live system accent, tying it to the pal's theme.
final class AboutWindow: NSObject, NSWindowDelegate {
    static var shared: AboutWindow?
    let window: NSWindow
    let config: PalConfig
    private var accentObserver: NSObjectProtocol?

    static func show(config: PalConfig) {
        let a = shared ?? AboutWindow(config: config)
        shared = a
        NSApp.activate(ignoringOtherApps: true)
        a.window.center()
        a.window.makeKeyAndOrderFront(nil)
    }

    init(config: PalConfig) {
        self.config = config
        window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 300, height: 300),
                          styleMask: [.titled, .closable, .fullSizeContentView],
                          backing: .buffered, defer: false)
        super.init()
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        window.isMovableByWindowBackground = true
        window.isReleasedWhenClosed = false
        window.level = .floating
        window.delegate = self
        buildContent()
        accentObserver = NotificationCenter.default.addObserver(
            forName: NSColor.systemColorsDidChangeNotification, object: nil, queue: .main
        ) { [weak self] _ in self?.buildContent() }
    }

    func windowWillClose(_ notification: Notification) {
        if let o = accentObserver { NotificationCenter.default.removeObserver(o) }
        AboutWindow.shared = nil
    }

    private func buildContent() {
        let content = NSView()
        window.contentView = content

        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 8
        stack.edgeInsets = NSEdgeInsets(top: 22, left: 28, bottom: 22, right: 28)
        stack.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            stack.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            stack.topAnchor.constraint(equalTo: content.topAnchor),
            stack.bottomAnchor.constraint(equalTo: content.bottomAnchor),
        ])

        if let path = config.posePaths(config.colorTogglable ? "gold" : "").first,
           let star = NSImage(contentsOfFile: path) {
            let iv = NSImageView()
            iv.image = star
            iv.wantsLayer = true
            iv.imageScaling = .scaleProportionallyUpOrDown
            iv.translatesAutoresizingMaskIntoConstraints = false
            iv.setContentHuggingPriority(.required, for: .vertical)
            iv.setContentCompressionResistancePriority(.required, for: .vertical)
            iv.widthAnchor.constraint(equalToConstant: 92).isActive = true
            iv.heightAnchor.constraint(equalToConstant: 92).isActive = true
            stack.addArrangedSubview(iv)
            stack.setCustomSpacing(10, after: iv)
        }

        let name = NSTextField(labelWithString: "MacPal")
        name.font = NSFont.systemFont(ofSize: 20, weight: .bold)
        name.alignment = .center
        stack.addArrangedSubview(name)

        let tagline = NSTextField(labelWithString: "a friendly little helper")
        tagline.font = NSFont.systemFont(ofSize: 11)
        tagline.textColor = .secondaryLabelColor
        tagline.alignment = .center
        stack.addArrangedSubview(tagline)
        stack.setCustomSpacing(16, after: tagline)

        if let line = config.aboutDedication {
            let para = NSMutableParagraphStyle()
            para.alignment = .center; para.lineBreakMode = .byWordWrapping
            let dedication = NSMutableAttributedString(string: line, attributes: [
                .font: NSFont.systemFont(ofSize: 13),
                .foregroundColor: NSColor.labelColor,
                .paragraphStyle: para,
            ])
            let ns = line as NSString
            let fia = ns.range(of: "Fía")
            if fia.location != NSNotFound {
                let iRange = NSRange(location: fia.location + 1, length: 1)   // the 'í'
                dedication.addAttribute(.foregroundColor, value: NSColor.controlAccentColor, range: iRange)
                dedication.addAttribute(.font, value: NSFont.systemFont(ofSize: 14, weight: .heavy), range: iRange)
            }
            let handle = ns.range(of: "@jeffrey")
            if handle.location != NSNotFound {
                dedication.addAttribute(.font, value: NSFont.systemFont(ofSize: 13, weight: .semibold), range: handle)
            }
            let ded = NSTextField(wrappingLabelWithString: "")
            ded.attributedStringValue = dedication
            ded.alignment = .center
            ded.translatesAutoresizingMaskIntoConstraints = false
            ded.preferredMaxLayoutWidth = 244
            ded.widthAnchor.constraint(equalToConstant: 244).isActive = true
            stack.addArrangedSubview(ded)
            stack.setCustomSpacing(20, after: ded)
        }

        let version = (Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String) ?? "?"
        let versionLabel = NSTextField(labelWithString: "Version \(version)")
        versionLabel.font = NSFont.systemFont(ofSize: 11)
        versionLabel.textColor = .secondaryLabelColor
        versionLabel.alignment = .center
        stack.addArrangedSubview(versionLabel)
        stack.setCustomSpacing(4, after: versionLabel)

        let copyright = NSTextField(labelWithString: "© 2026 Aesthetic, Inc.")
        copyright.font = NSFont.systemFont(ofSize: 11)
        copyright.textColor = .tertiaryLabelColor
        copyright.alignment = .center
        stack.addArrangedSubview(copyright)

        content.layoutSubtreeIfNeeded()
        window.setContentSize(NSSize(width: 300, height: ceil(content.fittingSize.height)))
    }
}
