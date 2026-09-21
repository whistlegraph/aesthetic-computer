import AppKit

/// The singer's face takes the whole display: the screen fills with the
/// member's color and becomes a face — two big eyes that blink and drift, a
/// mouth that opens on every sung syllable in the vowel's shape, thick black
/// lines that boil (fresh jitter ten times a second), a squash on each onset.
/// Old-cartoon timing, no avatar: the machine IS the face. The caption sits
/// under the mouth. Driven from the play loop beside the captions.
final class SingerFace {
    static let shared = SingerFace()

    /// The simulator: `sim=i/n` on a play payload puts member i of n in a
    /// small tile instead of the whole display, so one Menu Band can stand
    /// in for the whole band (every member's face, caption and voice on one
    /// machine) before the real bodies are spread across the room.
    struct SimSlot: Hashable {
        let index: Int, count: Int
        /// "i/n" → slot; anything else → nil (the full display).
        init?(_ raw: String?) {
            guard let raw, !raw.isEmpty, raw != "0" else { return nil }
            let p = raw.split(separator: "/")
            guard p.count == 2, let i = Int(p[0]), let n = Int(p[1]), n > 0, i >= 0, i < n else { return nil }
            index = i; count = n
        }
        /// Tiles side by side along the bottom-right of the menu-bar screen,
        /// 16:10, each at most 520 pt wide.
        func tile(on screen: NSScreen) -> NSRect {
            let vf = screen.visibleFrame
            let margin: CGFloat = 16, gap: CGFloat = 12
            let w = min(520, floor((vf.width - margin * 2 - gap * CGFloat(count - 1)) / CGFloat(count)))
            let h = floor(w * 0.625)
            let x = vf.maxX - margin - (w + gap) * CGFloat(count - index) + gap
            return NSRect(x: x, y: vf.minY + margin, width: w, height: h)
        }
        /// Stereo place for the slot's voice: left to right across the band.
        var pan: Float { count < 2 ? 0 : -0.75 + 1.5 * Float(index) / Float(count - 1) }
    }
    private static var slots: [Int: SingerFace] = [:]
    /// The face for a payload: `shared` for the full display, one per tile in sim mode.
    static func at(_ slot: SimSlot?) -> SingerFace {
        guard let slot else { return shared }
        if let f = slots[slot.index] { f.slot = slot; return f }
        let f = SingerFace(); f.slot = slot; slots[slot.index] = f; return f
    }
    static func hideAll() { shared.hide(); slots.values.forEach { $0.hide() } }

    private var slot: SimSlot?
    private var panel: NSPanel?
    private var view: SingerFaceView?

    func show(member: String, accent: NSColor) {
        precondition(Thread.isMainThread)
        guard let screen = NSScreen.screens.first ?? NSScreen.main else { return }
        let frame = slot?.tile(on: screen) ?? screen.frame
        let panel = self.panel ?? makePanel(frame)
        self.panel = panel
        panel.setFrame(frame, display: false)
        let v = view ?? SingerFaceView(frame: NSRect(origin: .zero, size: frame.size))
        view = v
        v.frame = NSRect(origin: .zero, size: frame.size)
        if panel.contentView !== v { panel.contentView = v }
        v.member = member
        v.accent = accent
        v.label = slot == nil ? nil : member
        v.start()
        NSAnimationContext.runAnimationGroup { ctx in
            ctx.duration = 0.3
            panel.animator().alphaValue = 1
        }
        panel.orderFrontRegardless()
    }

    func onset(_ syllable: String, hold: Double) {
        precondition(Thread.isMainThread)
        view?.onset(syllable, hold: hold)
    }

    /// Live level from the singer's audio tap: the jaw follows the sound.
    func meter(rms: CGFloat, zcr: CGFloat) {
        view?.meter(rms: rms, zcr: zcr)
    }

    func hide() {
        precondition(Thread.isMainThread)
        guard let panel, let view else { return }
        NSAnimationContext.runAnimationGroup({ ctx in
            ctx.duration = 0.5
            panel.animator().alphaValue = 0
        }, completionHandler: { [weak view, weak panel] in
            if panel?.alphaValue == 0 { view?.stop() }
        })
    }

    private func makePanel(_ frame: NSRect) -> NSPanel {
        let p = NSPanel(contentRect: frame, styleMask: [.borderless, .nonactivatingPanel],
                        backing: .buffered, defer: true)
        p.level = slot == nil ? .screenSaver : .floating   // a tile floats over the desk, not the screen saver
        p.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]
        p.isOpaque = false
        p.backgroundColor = .clear
        p.hasShadow = false
        p.ignoresMouseEvents = true
        p.hidesOnDeactivate = false
        p.isReleasedWhenClosed = false
        p.alphaValue = 0
        return p
    }
}

final class SingerFaceView: NSView {
    var member = "neo"
    var accent = NSColor.orange
    var label: String?                    // sim tile: the member's name in the corner

    private var timer: Timer?
    private var boil: UInt64 = 1
    private var lastBoil = Date.distantPast
    private var mouthOpen: CGFloat = 0
    private var mouthShape = 0            // 0 neutral · 1 wide (a) · 2 spread (e, i) · 3 small round (oo, u) · 4 round open (o, aw)
    private var mouthUntil = Date.distantPast
    private var lipsShutUntil = Date.distantPast   // m / b / p: lips pressed at the onset
    private var liveRms: CGFloat = 0
    private var liveZcr: CGFloat = 0
    private var lastMeter = Date.distantPast
    private var squash: CGFloat = 0
    private var lean: CGFloat = 1
    private var blinkUntil = Date.distantPast
    private var nextBlink = Date()
    private var gaze = CGPoint.zero
    private var gazeTarget = CGPoint.zero
    private var nextGaze = Date()
    private var bob: CGFloat = 0

    override var isFlipped: Bool { false }

    private var link: AnyObject?          // CADisplayLink on macOS 14+
    private var drawMs = 0.0, drawN = 0, lastDrawReport = Date()

    /// Native display link (macOS 14+): one tick per display refresh, 60 or
    /// 120 Hz, suspended off-screen — the cartoon runs on the panel's own
    /// clock, not a timer. The view is layer-backed so the drawn frame
    /// composites on the GPU.
    func start() {
        guard link == nil else { return }
        wantsLayer = true
        layerContentsRedrawPolicy = .onSetNeedsDisplay
        nextBlink = Date().addingTimeInterval(1.5)
        ticks = 0
        if #available(macOS 14.0, *) {
            let l = displayLink(target: self, selector: #selector(displayTick(_:)))
            l.add(to: .main, forMode: .common)
            link = l
            NSLog("🎭 face: display link armed")
            // If the link never fires (off-screen, or a quirk), fall back to
            // a 60 Hz timer after a second rather than freezing the face.
            DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
                guard let self, self.link != nil, self.ticks < 10 else { return }
                NSLog("🎭 face: display link silent (%d ticks) — 60 Hz timer fallback", self.ticks)
                self.startTimer()
            }
        } else {
            startTimer()
        }
    }

    private func startTimer() {
        guard timer == nil else { return }
        let t = Timer(timeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in self?.tick() }
        RunLoop.main.add(t, forMode: .common)
        timer = t
    }

    private var ticks = 0
    @objc private func displayTick(_ sender: Any) { tick() }

    func stop() {
        if #available(macOS 14.0, *) { (link as? CADisplayLink)?.invalidate() }
        link = nil
        timer?.invalidate()
        timer = nil
    }

    /// The mouth's FORM from the syllable's spelling (its jaw comes from the
    /// audio): oo/u → small round, ee/i/e/y → spread, o/aw/au → round open,
    /// a → wide open. A syllable starting with m/b/p presses the lips first.
    /// Preston Blair's set, from spelling: 1 A/I wide · 2 E spread ·
    /// 3 W/OO small round · 4 O round open · 5 F/V teeth on lip · 6 L/D/N/T
    /// tongue · 0 neutral. M/B/P is the lips-shut flash at the onset.
    static func mouthClass(_ syl: String) -> Int {
        let s = syl.lowercased()
        let vowels = s.filter { "aeiouy".contains($0) }
        if vowels.isEmpty {
            if s.hasPrefix("f") || s.hasPrefix("v") { return 5 }
            return 6
        }
        if s.hasPrefix("f") || s.hasPrefix("v"), s.count <= 2 { return 5 }
        if s.contains("oo") || s.contains("ou") || s.contains("ew") || s.hasSuffix("u") || s.hasPrefix("w") || s.hasPrefix("qu") { return 3 }
        if s.contains("aw") || s.contains("au") || s.contains("o") { return 4 }
        if s.contains("ee") || s.contains("ea") || s.contains("i") || s.contains("y") || s.contains("e") { return 2 }
        if s.contains("a") { return 1 }
        return 0
    }

    func onset(_ syllable: String, hold: Double) {
        mouthShape = SingerFaceView.mouthClass(syllable)
        if let f = syllable.lowercased().first, "mbp".contains(f) {
            lipsShutUntil = Date().addingTimeInterval(0.07)
        }
        mouthUntil = Date().addingTimeInterval(max(0.12, hold * 0.8))
        squash = 1
        lean = lean > 0 ? -1 : 1
    }

    func meter(rms: CGFloat, zcr: CGFloat) {
        liveRms = rms
        liveZcr = zcr
        lastMeter = Date()
    }

    private func tick() {
        ticks += 1
        let now = Date()
        if now.timeIntervalSince(lastBoil) > 0.1 { boil &+= 1; lastBoil = now }
        if now > nextBlink {
            blinkUntil = now.addingTimeInterval(0.13)
            nextBlink = now.addingTimeInterval(2.2 + Double(boil % 30) / 10)
        }
        if now > nextGaze {
            gazeTarget = CGPoint(x: CGFloat(Int(boil % 5)) / 2 - 1, y: CGFloat(Int((boil >> 3) % 3)) / 2 - 0.5)
            nextGaze = now.addingTimeInterval(1.2 + Double((boil >> 5) % 20) / 10)
        }
        gaze.x += (gazeTarget.x - gaze.x) * 0.18
        gaze.y += (gazeTarget.y - gaze.y) * 0.18
        squash *= 0.78
        // Jaw from the audio: fast attack, softer release. With no meter for
        // half a second (no tap yet) fall back to the onset-and-decay pulse.
        let metered = now.timeIntervalSince(lastMeter) < 0.5
        let target: CGFloat = metered
            ? min(1, max(0, (liveRms - 0.012) / 0.16))
            : (now < mouthUntil ? 0.85 : 0)
        mouthOpen += (target - mouthOpen) * (target > mouthOpen ? 0.65 : 0.3)
        if now < lipsShutUntil { mouthOpen = 0 }
        bob += 1.0 / 60.0
        needsDisplay = true
    }

    /// Deterministic jitter in [-1, 1] for point `i` of the current boil.
    private func j(_ i: Int, _ salt: Int = 0) -> CGFloat {
        var h: UInt64 = 1_469_598_103_934_665_603 ^ boil
        for b in [UInt64(i), UInt64(salt), 0x9E37] { h = (h ^ b) &* 1_099_511_628_211 }
        return CGFloat(Int(h % 2001)) / 1000 - 1
    }

    /// A lumpy closed blob through jittered points on an ellipse.
    private func blob(cx: CGFloat, cy: CGFloat, rx: CGFloat, ry: CGFloat, lump: CGFloat, salt: Int, n: Int = 26) -> NSBezierPath {
        var pts: [CGPoint] = []
        for i in 0..<n {
            let a = CGFloat(i) / CGFloat(n) * 2 * .pi
            let k = 1 + lump * j(i, salt)
            pts.append(CGPoint(x: cx + cos(a) * rx * k, y: cy + sin(a) * ry * k))
        }
        let p = NSBezierPath()
        let mid = { (a: CGPoint, b: CGPoint) in CGPoint(x: (a.x + b.x) / 2, y: (a.y + b.y) / 2) }
        p.move(to: mid(pts[n - 1], pts[0]))
        for i in 0..<n {
            let m = mid(pts[i], pts[(i + 1) % n])
            p.curve(to: m, controlPoint1: pts[i], controlPoint2: pts[i])
        }
        p.close()
        return p
    }

    private func ink(_ p: NSBezierPath, width: CGFloat, fill: NSColor?) {
        if let fill { fill.setFill(); p.fill() }
        p.lineWidth = width
        p.lineJoinStyle = .round
        p.lineCapStyle = .round
        NSColor.black.setStroke()
        p.stroke()
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        let t0 = CACurrentMediaTime()
        defer {
            drawMs += (CACurrentMediaTime() - t0) * 1000; drawN += 1
            if Date().timeIntervalSince(lastDrawReport) > 5 {
                let secs = Date().timeIntervalSince(lastDrawReport)
                NSLog("🎭 face: %.1f ms/frame · %.0f draws/s · %.0f ticks/s", drawMs / Double(max(1, drawN)),
                      Double(drawN) / secs, Double(ticks) / secs)
                drawMs = 0; drawN = 0; ticks = 0; lastDrawReport = Date()
            }
        }
        let W = bounds.width, H = bounds.height
        let isBB = member == "blueberry"
        let line = W * 0.011

        // the screen is the face
        accent.setFill()
        bounds.fill()

        ctx.saveGState()
        // squash on the onset around the face's center, leaning left then right
        ctx.translateBy(x: W / 2, y: H * 0.5 + sin(bob * 1.1) * H * 0.006)
        ctx.rotate(by: squash * lean * 0.025)
        ctx.scaleBy(x: 1 + 0.05 * squash, y: 1 - 0.08 * squash)
        ctx.translateBy(x: -W / 2, y: -H * 0.5)

        // eyes
        let blinking = Date() < blinkUntil
        let ey = H * 0.64
        let ex = W * (isBB ? 0.2 : 0.19)
        let ew = W * (isBB ? 0.1 : 0.105)
        let eh = H * (isBB ? 0.15 : 0.19)
        for side in [-1, 1] as [CGFloat] {
            let cx = W / 2 + side * ex + W * 0.003 * j(Int(side) + 10, 4)
            let cy = ey + H * 0.004 * j(Int(side) + 12, 5)
            if blinking {
                let shut = NSBezierPath()
                shut.move(to: CGPoint(x: cx - ew, y: cy))
                shut.curve(to: CGPoint(x: cx + ew, y: cy), controlPoint1: CGPoint(x: cx - ew * 0.4, y: cy - eh * 0.35), controlPoint2: CGPoint(x: cx + ew * 0.4, y: cy - eh * 0.35))
                ink(shut, width: line, fill: nil)
            } else {
                let eye = blob(cx: cx, cy: cy, rx: ew, ry: eh, lump: 0.025, salt: Int(side) + 20, n: 18)
                ink(eye, width: line, fill: NSColor(white: 0.97, alpha: 1))
                let pr = isBB ? ew * 0.42 : ew * 0.4
                let px = cx + gaze.x * ew * 0.35, py = cy + gaze.y * eh * 0.3
                NSColor.black.setFill()
                NSBezierPath(ovalIn: CGRect(x: px - pr, y: py - pr, width: pr * 2, height: pr * 2)).fill()
                NSColor.white.setFill()
                NSBezierPath(ovalIn: CGRect(x: px - pr * 0.15, y: py + pr * 0.3, width: pr * 0.5, height: pr * 0.5)).fill()
                if isBB {
                    // heavy lids: the top of each eye in the face's own color
                    ctx.saveGState(); eye.addClip()
                    accent.setFill()
                    NSBezierPath(rect: CGRect(x: cx - ew * 1.2, y: cy + eh * 0.08, width: ew * 2.4, height: eh * 1.2)).fill()
                    ctx.restoreGState()
                    let edge = NSBezierPath()
                    edge.move(to: CGPoint(x: cx - ew, y: cy + eh * 0.08))
                    edge.line(to: CGPoint(x: cx + ew, y: cy + eh * 0.08))
                    ink(edge, width: line, fill: nil)
                }
            }
            // brow
            let brow = NSBezierPath()
            let by = cy + eh * (isBB ? 1.25 : 1.45) + H * 0.005 * j(Int(side) + 30, 6)
            brow.move(to: CGPoint(x: cx - ew * 0.95, y: by - (isBB ? 0 : eh * 0.12)))
            if isBB {
                brow.line(to: CGPoint(x: cx + ew * 0.95, y: by))
            } else {
                brow.curve(to: CGPoint(x: cx + ew * 0.95, y: by - eh * 0.12), controlPoint1: CGPoint(x: cx - ew * 0.3, y: by + eh * 0.3), controlPoint2: CGPoint(x: cx + ew * 0.3, y: by + eh * 0.3))
            }
            ink(brow, width: line * (isBB ? 1.5 : 1.1), fill: nil)
        }

        // mouth — above the caption line
        let my = H * 0.33
        let mw = W * (isBB ? 0.13 : 0.16)
        if mouthOpen < 0.08 {
            let smile = NSBezierPath()
            let w = mw * (isBB ? 1.0 : 1.25)
            smile.move(to: CGPoint(x: W / 2 - w, y: my + H * 0.03))
            smile.curve(to: CGPoint(x: W / 2 + w, y: my + H * 0.03), controlPoint1: CGPoint(x: W / 2 - w * 0.4, y: my - H * (isBB ? 0.01 : 0.06)), controlPoint2: CGPoint(x: W / 2 + w * 0.4, y: my - H * (isBB ? 0.01 : 0.06)))
            ink(smile, width: line * 1.2, fill: nil)
        } else {
            var w = mw, h = H * 0.14 * mouthOpen
            var teeth = true, tongue = false, bite = false
            switch mouthShape {
            case 1: w *= 1.15                                   // A/I — wide open
            case 2: w *= 1.45; h *= 0.5                         // E — spread
            case 3: w *= 0.5; h *= 0.9; teeth = false           // W/OO — small round
            case 4: w *= 0.78; h *= 1.2; teeth = false          // O — round open
            case 5: w *= 1.2; h *= 0.35; bite = true            // F/V — teeth on the lip
            case 6: w *= 0.9; h *= 0.55; tongue = true          // L/D/N/T — tongue up
            default: w *= 0.85; h *= 0.7
            }
            if liveZcr > 0.22 && mouthOpen < 0.6 { w *= 1.2; h *= 0.45 }   // a hiss — teeth
            h = max(h, H * 0.015)
            let mouth = blob(cx: W / 2, cy: my, rx: w, ry: h, lump: 0.05, salt: 40, n: 16)
            ink(mouth, width: line * 1.2, fill: NSColor(srgbRed: 0.1, green: 0.03, blue: 0.05, alpha: 1))
            ctx.saveGState(); mouth.addClip()
            if teeth {
                NSColor(white: 0.96, alpha: 1).setFill()
                NSBezierPath(rect: CGRect(x: W / 2 - w, y: my + h * (bite ? 0.1 : 0.45), width: w * 2, height: h * (bite ? 0.95 : 0.6))).fill()
            }
            if tongue {
                NSColor(srgbRed: 0.85, green: 0.25, blue: 0.35, alpha: 1).setFill()
                NSBezierPath(ovalIn: CGRect(x: W / 2 - w * 0.45, y: my - h * 0.3, width: w * 0.9, height: h * 1.0)).fill()
            } else if !bite {
                NSColor(srgbRed: 0.85, green: 0.25, blue: 0.35, alpha: 1).setFill()
                NSBezierPath(ovalIn: CGRect(x: W / 2 - w * 0.5, y: my - h * 1.25, width: w, height: h * 0.9)).fill()
            }
            ctx.restoreGState()
        }
        ctx.restoreGState()

        if let label {
            // the tile's name, top-left, in the rock lettering, in the face's ink
            let lum = (accent.usingColorSpace(.deviceRGB)).map { 0.2126 * $0.redComponent + 0.7152 * $0.greenComponent + 0.0722 * $0.blueComponent } ?? 0.5
            let ink = lum > 0.58 ? NSColor(deviceWhite: 0.06, alpha: 0.9) : NSColor(deviceWhite: 0.98, alpha: 0.9)
            let pt = max(11, round(W / 22))
            let attr = NSAttributedString(string: label, attributes: [.font: LyricCaption.font(pt), .foregroundColor: ink])
            attr.draw(at: NSPoint(x: W * 0.035, y: H - pt * 1.6))
            NSColor(white: 0, alpha: 0.35).setStroke()
            let edge = NSBezierPath(rect: bounds.insetBy(dx: 0.5, dy: 0.5)); edge.lineWidth = 1; edge.stroke()
        }
    }
}
