// Donut.swift — the donut: the app's home screen.
//
// The club's circles are peers. They sit evenly on one ring, none above
// another, so the ring is shown as what it looks like: a donut, in three
// dimensions (SceneKit), turning slowly. Each circle is a flat token lying on
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
    static let icing = Theme.pair("FF8FC7", "FF6FB8")
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
}

// A name beside a ball, or a commons chip: flat colour, click to open.
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
    override func mouseDown(with event: NSEvent) { onClick?() }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
}

final class CircleDiagram: NSView {
    var onPick: ((Int, NSPoint?) -> Void)?       // which, and where it was picked (in this view), when it has a place
    var items: [DiagramItem] = [] { didSet { if signature(items) != signature(oldValue) { build() } } }

    private let scn = SCNView()
    private let turning = SCNNode()                 // the donut and its balls; this is what turns
    private var balls: [(node: SCNNode, tag: Tag, radius: CGFloat)] = []
    private var chips: [Tag] = []
    private var timer: Timer?

    override init(frame: NSRect) {
        super.init(frame: frame)
        scn.backgroundColor = .clear
        scn.antialiasingMode = .multisampling4X
        scn.preferredFramesPerSecond = 30
        scn.autoresizingMask = [.width, .height]
        scn.frame = bounds
        scn.scene = makeScene()
        addSubview(scn)
    }
    required init?(coder: NSCoder) { fatalError() }

    // What a rebuild depends on (unread counts and recency included).
    private func signature(_ items: [DiagramItem]) -> String {
        items.map { "\($0.index)\($0.title)\($0.members)\($0.unread)\($0.lastActive / 600_000)\($0.color.description)" }.joined(separator: "|")
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
        let cam = SCNNode()
        cam.camera = SCNCamera()
        cam.camera?.fieldOfView = 30
        cam.position = SCNVector3(0, 4.1, 4.5)
        cam.look(at: SCNVector3(0, 0.08, 0))
        scene.rootNode.addChildNode(cam)

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
        turning.addChildNode(SCNNode(geometry: dough))
        let icing = SCNTorus(ringRadius: Self.ring, pipeRadius: Self.pipe * 1.05)
        icing.ringSegmentCount = 96
        icing.pipeSegmentCount = 48
        icing.materials = [material(Donut.icing, shine: 0.75)]
        let icingNode = SCNNode(geometry: icing)
        icingNode.scale = SCNVector3(1, 0.62, 1)
        icingNode.position = SCNVector3(0, Self.pipe * 0.42, 0)
        turning.addChildNode(icingNode)

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
            turning.addChildNode(node)
        }

        scene.rootNode.addChildNode(turning)
        turning.runAction(.repeatForever(.rotateBy(x: 0, y: -2 * .pi, z: 0, duration: 70)))
        let rock = SCNAction.sequence([.rotateBy(x: 0.07, y: 0, z: 0.03, duration: 3.2), .rotateBy(x: -0.07, y: 0, z: -0.03, duration: 3.2)])
        rock.timingMode = .easeInEaseOut
        scene.rootNode.childNodes.last?.parent?.runAction(.repeatForever(rock))
        return scene
    }

    // ─── the balls, their names, the commons ─────────────────────────────────

    private func build() {
        balls.forEach { $0.node.removeFromParentNode(); $0.tag.removeFromSuperview() }
        chips.forEach { $0.removeFromSuperview() }
        balls = []
        chips = []
        let circles = items.filter { !$0.commons }
        let most = CGFloat(max(circles.map { $0.members }.max() ?? 1, 1))
        let n = CGFloat(max(circles.count, 1))
        let now = Int64(Date().timeIntervalSince1970 * 1000)

        for (i, it) in circles.enumerated() {
            // size: members. No wider than its share of the ring.
            let r = min(0.2 + 0.2 * sqrt(CGFloat(max(it.members, 1)) / most), Self.ring * sin(.pi / max(n, 2)) * 0.8)
            // light: recency.
            let age = it.lastActive > 0 ? Double(now - it.lastActive) / 3_600_000 : 1e6       // hours
            let stale = CGFloat(min(1, max(0, (age - 24) / (24 * 13))))
            let shown = (it.color.usingColorSpace(.sRGB) ?? it.color).blended(withFraction: 0.72 * stale, of: NSColor(white: 0.62, alpha: 1)) ?? it.color
            // A token, not a ball: a thin flat disc in one colour, on a white one a little wider.
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
            turning.addChildNode(node)

            // its name, small, on the token: a chip of the same colour
            let on = Theme.on(shown)
            let words = NSMutableAttributedString(string: it.title.uppercased(), attributes: [
                .font: Theme.mono(9, bold: true), .foregroundColor: on])
            if it.unread > 0 {
                words.append(NSAttributedString(string: " "))
                words.append(NSAttributedString(string: " \(it.unread) ", attributes: [
                    .font: Theme.mono(9, bold: true), .foregroundColor: Theme.onYellow, .backgroundColor: Theme.yellow]))
            }
            let tag = Tag(words, wash: shown, tight: true)
            tag.onClick = { [weak self, weak tag] in
                guard let self else { return }
                self.onPick?(it.index, tag.map { NSPoint(x: $0.frame.midX, y: $0.frame.midY) })
            }
            addSubview(tag)
            balls.append((node, tag, r))
        }

        // the commons: flat chips down the side
        for it in items.filter({ $0.commons }) {
            let on = Theme.on(it.color)
            let words = NSMutableAttributedString(string: it.title.uppercased(), attributes: [
                .font: Theme.mono(11, bold: true), .foregroundColor: on])
            if it.members > 0 {
                words.append(NSAttributedString(string: " \(it.members)", attributes: [.font: Theme.mono(10), .foregroundColor: on.withAlphaComponent(0.7)]))
            }
            if it.unread > 0 {
                words.append(NSAttributedString(string: " "))
                words.append(NSAttributedString(string: " \(it.unread) ", attributes: [
                    .font: Theme.mono(10, bold: true), .foregroundColor: Theme.onYellow, .backgroundColor: Theme.yellow]))
            }
            let chip = Tag(words, wash: it.color)
            chip.onClick = { [weak self, weak chip] in
                guard let self else { return }
                self.onPick?(it.index, chip.map { NSPoint(x: $0.frame.midX, y: $0.frame.midY) })
            }
            addSubview(chip)
            chips.append(chip)
        }
        needsLayout = true
        place()
    }

    override func layout() {
        super.layout()
        // The commons stand to the right of the donut, one above the other.
        let wide = chips.map { $0.frame.width }.max() ?? 0
        var y = bounds.midY + (CGFloat(chips.count) * 24 - 4) / 2 - 20
        for chip in chips {
            chip.frame.size.width = wide
            chip.frame.origin = NSPoint(x: bounds.maxX - wide - 10, y: y)
            y -= 24
        }
        place()
    }

    // Keep each name on its token as the donut turns.
    private func place() {
        guard !balls.isEmpty, bounds.width > 0 else { return }
        for (node, tag, _) in balls {
            let p = scn.projectPoint(node.presentation.worldPosition)
            let size = tag.frame.size
            tag.frame.origin = NSPoint(x: min(max(2, p.x - size.width / 2), bounds.width - size.width - 2),
                                       y: min(max(2, p.y - size.height / 2), bounds.height - size.height - 2))
        }
    }

    // The donut turns only while it is on screen.
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
        guard let first = items.first(where: { !$0.commons }), let tag = balls.first?.tag else { return }
        onPick?(first.index, NSPoint(x: tag.frame.midX, y: tag.frame.midY))
    }

    // Test aid: the scene as a picture (a window snapshot cannot see Metal), and the views drawn over it.
    func shot() -> NSImage { scn.snapshot() }
    var overlays: [NSView] { balls.map { $0.tag } + chips }
}
