// Donut.swift — the donut: the app's home screen.
//
// The club's circles are peers. They sit evenly on one ring, none above
// another, so the ring is shown as what it looks like: a donut, in three
// dimensions (SceneKit), rocking gently. Each chat is a flat token lying on
// the icing, with its name on it:
//   - its size is the circle's members;
//   - its colour and its beat are how recently the circle spoke: a circle with
//     a message in the last hour beats fast, one from today beats slowly, and
//     an older one lies still and fades toward grey over two weeks.
// The commons are not on the ring; they stand beside it. Click a token, a name
// or a commons chip to open it.

import AppKit
import SceneKit

enum Donut {
    static let dough = Theme.pair("E6B873", "D9A562")
    static let icing = Theme.pair("3CCBF0", "6FE3FF")       // cyan: the app's own colour
    static let stem = Theme.pair("1E9FDB", "3AB4EE")        // the d's stem in the wordmark: a deeper blue than the icing
    static let hole: CGFloat = 0.40          // the hole's radius, as a share of the donut's

    // The flat mark (the wordmark's last letter): a ring of dough, and on it a
    // ring of icing with a wavy edge. The hole is open to what lies under it.
    static func draw(at c: NSPoint, radius R: CGFloat) {
        func ring(_ outer: @escaping (CGFloat) -> CGFloat, _ inner: @escaping (CGFloat) -> CGFloat) -> NSBezierPath {
            let p = NSBezierPath()
            for radius in [outer, inner] {
                for step in 0...120 {
                    let a = CGFloat(step) / 120 * 2 * .pi
                    let pt = NSPoint(x: c.x + radius(a) * cos(a), y: c.y + radius(a) * sin(a))
                    step == 0 ? p.move(to: pt) : p.line(to: pt)
                }
                p.close()
            }
            p.windingRule = .evenOdd
            return p
        }
        dough.setFill()
        ring({ _ in R }, { _ in R * hole }).fill()
        icing.setFill()
        ring({ a in R * (0.905 + 0.035 * sin(11 * a)) }, { a in R * (hole + 0.095 + 0.022 * sin(7 * a + 1)) }).fill()
    }
}

struct DiagramItem {
    let index: Int
    let title: String
    let commons: Bool
    let color: NSColor
    let members: Int
    let unread: Int
    let lastActive: Int64        // unix ms of the newest message, 0 when there is none
    var who = ""                 // the newest message: who, what, how long ago
    var text = ""
    var when = ""
}

// A name on a token: flat colour, click to open.
private final class Tag: NSView {
    var onClick: (() -> Void)?
    private let text = NSTextField(labelWithString: "")
    init(_ s: NSAttributedString, wash: NSColor?, tight: Bool = false) {
        super.init(frame: .zero)
        wantsLayer = true
        layer?.backgroundColor = wash?.cgColor
        text.attributedStringValue = s
        text.sizeToFit()
        let padX: CGFloat = wash == nil ? 0 : tight ? 2 : 6, padY: CGFloat = wash == nil ? 0 : tight ? 1 : 3
        text.frame.origin = NSPoint(x: padX, y: padY)
        addSubview(text)
        frame.size = NSSize(width: text.frame.width + 2 * padX, height: text.frame.height + 2 * padY)
    }
    required init?(coder: NSCoder) { fatalError() }
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect], owner: self))
    }
    override func mouseEntered(with event: NSEvent) { layer?.borderWidth = 2; layer?.borderColor = NSColor.white.cgColor }   // under the pointer: a white edge
    override func mouseExited(with event: NSEvent) { layer?.borderWidth = 0 }
    override func mouseDown(with event: NSEvent) { onClick?() }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
}

// The newest message of a chat, floating off its token: who and how long ago,
// then the words, on a wash of the chat's colour. Click to go into the chat.
private final class Bubble: NSView {
    var onClick: (() -> Void)?
    let item: DiagramItem
    private var hover = false { didSet { if hover != oldValue { needsDisplay = true } } }
    override var isFlipped: Bool { true }
    init(_ item: DiagramItem) {
        self.item = item
        super.init(frame: .zero)
        wantsLayer = true
    }
    required init?(coder: NSCoder) { fatalError() }
    static let tall: CGFloat = 36
    override func draw(_ dirty: NSRect) {
        let small = NSFont.monospacedSystemFont(ofSize: 10, weight: .regular), bold = NSFont.monospacedSystemFont(ofSize: 10, weight: .bold)
        Theme.wash(item.color).setFill()
        NSBezierPath(roundedRect: bounds, xRadius: 6, yRadius: 6).fill()
        (hover ? item.color.withAlphaComponent(0.35) : NSColor.clear).setFill()
        NSBezierPath(roundedRect: bounds, xRadius: 6, yRadius: 6).fill()
        item.color.setFill()                    // the chat's colour down the side that faces its token
        NSBezierPath(roundedRect: NSRect(x: 0, y: 0, width: 4, height: bounds.height), xRadius: 2, yRadius: 2).fill()
        let cut = NSMutableParagraphStyle()
        cut.lineBreakMode = .byTruncatingTail
        let top = NSMutableAttributedString(string: item.who, attributes: [.font: bold, .foregroundColor: Theme.ink, .paragraphStyle: cut])
        top.append(NSAttributedString(string: "  " + item.when, attributes: [.font: small, .foregroundColor: Theme.faint, .paragraphStyle: cut]))
        var room = bounds.width - 16
        if item.unread > 0 {
            let n = NSAttributedString(string: " \(item.unread) ", attributes: [.font: bold, .foregroundColor: Theme.onYellow, .backgroundColor: Theme.yellow])
            n.draw(at: NSPoint(x: bounds.maxX - n.size().width - 6, y: 4))
            room -= n.size().width + 6
        }
        top.draw(in: NSRect(x: 10, y: 4, width: max(10, room), height: 14))
        NSAttributedString(string: item.text.replacingOccurrences(of: "\n", with: " "), attributes: [
            .font: small, .foregroundColor: Theme.ink, .paragraphStyle: cut]).draw(in: NSRect(x: 10, y: 18, width: bounds.width - 16, height: 14))
    }
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

final class CircleDiagram: NSView {
    var onPick: ((Int, NSPoint?) -> Void)?       // which, and where it was picked (in this view), when it has a place
    var items: [DiagramItem] = [] { didSet { if signature(items) != signature(oldValue) { build() } } }

    private let scn = SCNView()
    private let rocking = SCNNode()                 // the donut and its tokens; this is what rocks
    private let camera = SCNNode()
    private let strings = CAShapeLayer()            // the lines from the tokens to their bubbles
    private var balls: [(node: SCNNode, tag: Tag, bubble: Bubble?, radius: CGFloat)] = []
    private var timer: Timer?

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        scn.backgroundColor = .clear
        scn.antialiasingMode = .multisampling4X
        scn.preferredFramesPerSecond = 30
        scn.autoresizingMask = [.width, .height]
        scn.frame = bounds
        scn.scene = makeScene()
        addSubview(scn)
        strings.fillColor = nil
        strings.lineWidth = 1.5
        strings.lineDashPattern = [3, 3]
        layer?.addSublayer(strings)
    }
    required init?(coder: NSCoder) { fatalError() }

    // What a rebuild depends on (unread counts, recency and the newest words included).
    private func signature(_ items: [DiagramItem]) -> String {
        items.map { "\($0.index)\($0.title)\($0.members)\($0.unread)\($0.lastActive / 600_000)\($0.color.description)\($0.who)\($0.text.prefix(60))\($0.when)" }.joined(separator: "|")
    }

    // ─── the scene ───────────────────────────────────────────────────────────

    private static let ring: CGFloat = 1.0, pipe: CGFloat = 0.43

    private func material(_ c: NSColor, shine: CGFloat) -> SCNMaterial {
        let m = SCNMaterial()
        m.lightingModel = .blinn
        m.diffuse.contents = c
        m.specular.contents = NSColor(white: 1, alpha: shine)
        m.shininess = 0.35 + shine
        return m
    }

    private func makeScene() -> SCNScene {
        let scene = SCNScene()
        camera.camera = SCNCamera()
        camera.camera?.fieldOfView = 30
        camera.position = SCNVector3(0, 4.1, 4.5)
        camera.look(at: SCNVector3(0, 0.08, 0))
        scene.rootNode.addChildNode(camera)

        let ambient = SCNNode()
        ambient.light = SCNLight()
        ambient.light?.type = .ambient
        ambient.light?.intensity = 620
        scene.rootNode.addChildNode(ambient)
        let key = SCNNode()
        key.light = SCNLight()
        key.light?.type = .directional
        key.light?.intensity = 780
        key.eulerAngles = SCNVector3(-1.05, -0.5, 0)
        scene.rootNode.addChildNode(key)

        // dough, and icing: a slightly fatter, flattened ring lifted so that only its top shows
        let dough = SCNTorus(ringRadius: Self.ring, pipeRadius: Self.pipe)
        dough.ringSegmentCount = 96
        dough.pipeSegmentCount = 48
        dough.materials = [material(Donut.dough, shine: 0.08)]
        rocking.addChildNode(SCNNode(geometry: dough))
        let icing = SCNTorus(ringRadius: Self.ring, pipeRadius: Self.pipe * 1.05)
        icing.ringSegmentCount = 96
        icing.pipeSegmentCount = 48
        icing.materials = [material(Donut.icing, shine: 0.75)]
        let icingNode = SCNNode(geometry: icing)
        icingNode.scale = SCNVector3(1, 0.62, 1)
        icingNode.position = SCNVector3(0, Self.pipe * 0.42, 0)
        rocking.addChildNode(icingNode)

        // sprinkles on the icing, in the club's colours
        var seed: UInt64 = 0x00C0FFEE
        func rnd() -> CGFloat {
            seed = seed &* 6364136223846793005 &+ 1442695040888963407
            return CGFloat((seed >> 33) % 10_000) / 10_000
        }
        for n in 0..<70 {
            let s = SCNCapsule(capRadius: 0.014, height: 0.085)
            s.materials = [material(NSColor(hex: Theme.palette[n % Theme.palette.count])!, shine: 0.5)]
            let node = SCNNode(geometry: s)
            let a = rnd() * 2 * .pi, tilt = (rnd() - 0.5) * 1.5          // round the ring, across its top
            let reach = Self.ring + Self.pipe * 1.02 * sin(tilt)
            node.position = SCNVector3(reach * cos(a), Self.pipe * 0.42 + Self.pipe * 0.655 * cos(tilt), reach * sin(a))
            node.eulerAngles = SCNVector3(.pi / 2 + (rnd() - 0.5) * 0.5, rnd() * .pi, tilt)
            rocking.addChildNode(node)
        }

        scene.rootNode.addChildNode(rocking)
        // It does not spin (the messages that float off it must stay readable): it rocks a little, like a thing afloat.
        let rock = SCNAction.sequence([.rotateBy(x: 0.06, y: 0.1, z: 0.03, duration: 3.4), .rotateBy(x: -0.06, y: -0.1, z: -0.03, duration: 3.4)])
        rock.timingMode = .easeInEaseOut
        rocking.runAction(.repeatForever(rock))
        return scene
    }

    // ─── the tokens, their names, the messages that float off them ───────────

    private func build() {
        balls.forEach { $0.node.removeFromParentNode(); $0.tag.removeFromSuperview(); $0.bubble?.removeFromSuperview() }
        balls = []
        let most = CGFloat(max(items.map { $0.members }.max() ?? 1, 1))
        let n = CGFloat(max(items.count, 1))
        let now = Int64(Date().timeIntervalSince1970 * 1000)

        for (i, it) in items.enumerated() {
            // size: members. No wider than its share of the ring.
            let r = min(0.2 + 0.2 * sqrt(CGFloat(max(it.members, 1)) / most), Self.ring * sin(.pi / max(n, 2)) * 0.8)
            // colour and beat: recency.
            let age = it.lastActive > 0 ? Double(now - it.lastActive) / 3_600_000 : 1e6       // hours
            let stale = CGFloat(min(1, max(0, (age - 24) / (24 * 13))))
            let shown = (it.color.usingColorSpace(.sRGB) ?? it.color).blended(withFraction: 0.72 * stale, of: NSColor(white: 0.62, alpha: 1)) ?? it.color
            // A token: a thin flat disc in one colour, on a white one a little wider.
            let flat = SCNMaterial()
            flat.lightingModel = .constant
            flat.diffuse.contents = shown
            let disc = SCNCylinder(radius: r, height: 0.035)
            disc.radialSegmentCount = 64
            disc.materials = [flat]
            let rim = SCNMaterial()
            rim.lightingModel = .constant
            rim.diffuse.contents = NSColor.white
            let under = SCNCylinder(radius: r * 1.1, height: 0.03)
            under.radialSegmentCount = 64
            under.materials = [rim]
            let node = SCNNode(geometry: disc)
            let base = SCNNode(geometry: under)
            base.position = SCNVector3(0, -0.012, 0)
            node.addChildNode(base)
            let a = (-90 + 180 / n + CGFloat(i) * 360 / n) * .pi / 180
            node.position = SCNVector3(Self.ring * cos(a), Self.pipe * 0.42 + Self.pipe * 0.655 + 0.03, Self.ring * sin(a))
            node.name = "\(it.index)"
            base.name = node.name
            if age < 24 {                                   // a beat: fast inside the hour, slow inside the day
                let d = age < 1 ? 0.42 : 1.5, k: CGFloat = age < 1 ? 1.14 : 1.06
                let beat = SCNAction.sequence([.scale(to: k, duration: d), .scale(to: 1, duration: d)])
                beat.timingMode = .easeInEaseOut
                node.runAction(.repeatForever(beat))
            }
            rocking.addChildNode(node)

            // The names grow and shrink with the donut, so they stay on their tokens
            // and off each other (one of the texts that is not the one size).
            let small = NSFont.monospacedSystemFont(ofSize: nameSize, weight: .bold)
            let words = NSAttributedString(string: it.title.uppercased(), attributes: [.font: small, .foregroundColor: Theme.on(shown)])
            let tag = Tag(words, wash: shown, tight: true)
            tag.onClick = { [weak self, weak tag] in
                guard let self else { return }
                self.onPick?(it.index, tag.map { NSPoint(x: $0.frame.midX, y: $0.frame.midY) })
            }
            addSubview(tag)
            var bubble: Bubble?
            if !it.text.isEmpty || !it.who.isEmpty {
                let b = Bubble(it)
                b.onClick = { [weak self, weak tag] in
                    guard let self else { return }
                    self.onPick?(it.index, tag.map { NSPoint(x: $0.frame.midX, y: $0.frame.midY) })
                }
                addSubview(b)
                bubble = b
            }
            balls.append((node, tag, bubble, r))
        }
        needsLayout = true
        place()
    }

    // The size of the names on the tokens, by the donut's own size.
    private var nameSize: CGFloat { max(7, min(12, (min(bounds.height, bounds.width * 0.5) / 30).rounded())) }
    private var builtAt: CGFloat = 0

    override func layout() {
        super.layout()
        strings.frame = bounds
        // The donut keeps to the middle half of the view's width, so the messages have the sides.
        // (SceneKit sizes the scene by the view's height; a view that is tall for its width moves the camera back.)
        let wide = 1.12 * bounds.height, room = max(120, bounds.width * 0.46)
        let back = max(1, wide / room)
        camera.position = SCNVector3(0, 4.1 * back, 4.5 * back)
        if builtAt != nameSize, !items.isEmpty { builtAt = nameSize; build() }
        place()
    }

    // Keep each name on its token, and float each message off its token: to the
    // right of the donut for a token on its right half, to the left for one on
    // its left, one under another, with a dashed string back to the token.
    private func place() {
        guard !balls.isEmpty, bounds.width > 0 else { return }
        let centre = scn.projectPoint(SCNVector3(0, 0, 0))
        var left: [(Bubble, NSPoint)] = [], right: [(Bubble, NSPoint)] = []
        var reachX: CGFloat = 0
        for (node, tag, bubble, _) in balls {
            let p = scn.projectPoint(node.presentation.worldPosition)
            let size = tag.frame.size
            tag.frame.origin = NSPoint(x: min(max(2, p.x - size.width / 2), bounds.width - size.width - 2),
                                       y: min(max(2, p.y - size.height / 2), bounds.height - size.height - 2))
            reachX = max(reachX, abs(p.x - centre.x))
            if let bubble { p.x >= centre.x ? right.append((bubble, NSPoint(x: p.x, y: p.y))) : left.append((bubble, NSPoint(x: p.x, y: p.y))) }
        }
        let edge = reachX * 1.42 + 18                          // clear of the donut's side
        let path = CGMutablePath()
        for (side, list) in [(-1.0, left), (1.0, right)] as [(CGFloat, [(Bubble, NSPoint)])] {
            let space = side < 0 ? centre.x - edge - 6 : bounds.width - (centre.x + edge) - 6
            let width = min(250, space)
            let sorted = list.sorted { $0.1.y > $1.1.y }       // top to bottom
            // one under another, about their tokens' heights, never overlapping and never off the view
            var ys = sorted.map { $0.1.y - Bubble.tall / 2 }
            for i in ys.indices.dropFirst() { ys[i] = min(ys[i], ys[i - 1] - Bubble.tall - 5) }
            if let low = ys.last, low < 4 { let up = 4 - low; ys = ys.map { $0 + up } }
            if let high = ys.first, high + Bubble.tall > bounds.height - 4 { let down = high + Bubble.tall - bounds.height + 4; ys = ys.map { $0 - down } }
            for (i, (bubble, token)) in sorted.enumerated() {
                bubble.isHidden = width < 84                   // no room at the side: the tokens stand alone
                guard !bubble.isHidden else { continue }
                let x = side < 0 ? centre.x - edge - width : centre.x + edge
                bubble.frame = NSRect(x: x, y: ys[i], width: width, height: Bubble.tall)
                path.move(to: token)
                path.addLine(to: NSPoint(x: side < 0 ? x + width : x, y: ys[i] + Bubble.tall / 2))
            }
        }
        var ink = Theme.faint
        effectiveAppearance.performAsCurrentDrawingAppearance { ink = Theme.faint.usingColorSpace(.sRGB) ?? ink }
        strings.strokeColor = ink.cgColor
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        strings.path = path
        CATransaction.commit()
    }

    // The donut moves only while it is on screen.
    private func run() {
        let on = window != nil && !isHiddenOrHasHiddenAncestor
        scn.isPlaying = on
        timer?.invalidate()
        timer = nil
        guard on else { return }
        let t = Timer(timeInterval: 1.0 / 30, repeats: true) { [weak self] _ in self?.place() }
        RunLoop.main.add(t, forMode: .common)
        timer = t
    }
    override func viewDidMoveToWindow() { run() }
    override func viewDidHide() { run() }
    override func viewDidUnhide() { run() }

    override func mouseDown(with event: NSEvent) {
        let p = scn.convert(event.locationInWindow, from: nil)
        for hit in scn.hitTest(p, options: [.searchMode: SCNHitTestSearchMode.all.rawValue]) {
            if let name = hit.node.name, let index = Int(name) {
                return onPick?(index, convert(event.locationInWindow, from: nil)) ?? ()
            }
        }
    }

    // Test aid: pick the first token, as a click on it would.
    func testPick() {
        guard let first = items.first, let tag = balls.first?.tag else { return }
        onPick?(first.index, NSPoint(x: tag.frame.midX, y: tag.frame.midY))
    }

    // Test aid: the scene as a picture (a window snapshot cannot see Metal), and the views drawn over it.
    func shot() -> NSImage { scn.snapshot() }
    var overlays: [NSView] { balls.map { $0.tag } + balls.compactMap { $0.bubble }.filter { !$0.isHidden } }
}
