import AppKit
import MetalKit

/// The singer's face takes the whole display: the screen fills with the
/// member's color and becomes a face — two big eyes that blink and drift, a
/// mouth that opens on every sung syllable in the vowel's shape, thick black
/// lines that boil (fresh jitter ten times a second), a squash on each onset.
/// Old-cartoon timing, no avatar: the machine IS the face. The caption sits
/// under the mouth. Driven from the play loop beside the captions.
final class SingerFace {
    static let shared = SingerFace()
    static var fullscreenVisible: Bool {
        shared.panel?.isVisible == true && (shared.panel?.alphaValue ?? 0) > 0.01
    }

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
    private var performances: [ObjectIdentifier: (SingerArticulation, () -> Double?)] = [:]
    private var performanceEpoch = 0.0, performanceBpm = 88.0, expression = 0.0

    func configure(epoch: Double, bpm: Double, expression: Double) {
        performanceEpoch = epoch; performanceBpm = bpm; self.expression = expression
        view?.configure(epoch: epoch, bpm: bpm, expression: expression)
    }

    func setExpression(_ value: Double) {
        expression = min(1,max(0,value))
        view?.setExpression(expression)
    }

    func inhale() -> CGFloat {
        var breath = 0.0
        for (a, clock) in performances.values {
            guard let t = clock(), let first = a.cues.first else { continue }
            let lead = first.start-t
            if lead > 0 && lead < 0.38 { breath = max(breath, sin(.pi*(1-lead/0.38))) }
        }
        return CGFloat(breath)
    }

    func follow(_ articulation: SingerArticulation, slot: ObjectIdentifier, clock: @escaping () -> Double?) {
        performances[slot] = (articulation, clock)
    }

    func clearArticulation() {
        performances.removeAll()
    }

    /// Preroll from a new phrase must not close the still-singing old phrase.
    func mouthPose() -> SingerMouthPose? {
        guard !performances.isEmpty else { return nil }
        var selected: (pose: SingerMouthPose, energy: Double)?
        for (a, clock) in performances.values {
            guard let t = clock(), a.active(at: t) else { continue }
            let energy = a.level(at: max(0, t))
            if selected == nil || energy > selected!.energy { selected = (a.pose(at: t), energy) }
        }
        return selected?.pose ?? SingerViseme.rest.pose
    }

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
        v.articulationPose = { [weak self] in self?.mouthPose() }
        v.articulationBreath = { [weak self] in self?.inhale() ?? 0 }
        v.configure(epoch: performanceEpoch, bpm: performanceBpm, expression: expression)
        v.start()
        NSAnimationContext.runAnimationGroup { ctx in
            ctx.duration = 0.12
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
    var articulationPose: (() -> SingerMouthPose?)?
    var previewPose: SingerMouthPose?      // deterministic render inspection
    var previewEffort: CGFloat?          // expression render inspection
    var articulationBreath: (() -> CGFloat)?
    private var drawnPose: SingerMouthPose?
    private var performanceEpoch = 0.0, performanceBpm = 88.0
    private var expression: CGFloat = 0
    private var effort: CGFloat = 0, breath: CGFloat = 0, beatPulse: CGFloat = 0
    private var lastMusicalBeat = -1
    private var lastTick = CACurrentMediaTime()
    private var cameraPhase = 0.0
    private var livingPhase = 0.0
    private var vitality: CGFloat = 0

    func setExpression(_ value: Double) { expression = CGFloat(min(1,max(0,value))) }

    func configure(epoch: Double, bpm: Double, expression: Double) {
        performanceEpoch = epoch; performanceBpm = max(1,bpm)
        self.expression = CGFloat(min(1,max(0,expression)))
        lastMusicalBeat = -1; lastTick = CACurrentMediaTime()
    }

    func previewExpression(beat: Double, effort: CGFloat, breath: CGFloat) {
        cameraPhase = beat; previewEffort = effort; self.breath = breath
        blinkUntil = beat >= 0 && Int(beat) % 4 == 3 && beat.truncatingRemainder(dividingBy: 1) < 0.14
            ? Date().addingTimeInterval(1) : .distantPast
    }

    private var timer: Timer?
    private var boil: UInt64 = 1
    private var boilBlend: CGFloat = 0
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

    private var metalFace: SingerFaceMetalView?
    private var canvas: SingerFaceCanvas?
    private var link: AnyObject?          // CADisplayLink on macOS 14+
    private var drawMs = 0.0, drawN = 0, lastDrawReport = Date()

    /// Native display clock, explicit Metal presentation. AppKit is retained
    /// for offline reference images and machines without a Metal surface.
    func start() {
        guard link == nil && metalFace == nil && timer == nil else { return }
        if let surface = SingerFaceMetalView(metalFrame: bounds) {
            surface.autoresizingMask = [.width, .height]
            surface.animate = { [weak self] in self?.tick() }
            surface.paint = { [weak self] canvas in self?.drawArtwork(canvas) }
            metalFace = surface
            addSubview(surface)
            if surface.start() {
                NSLog("🎭 face: Oskiewar-style Metal triangle renderer armed")
                return
            }
            surface.removeFromSuperview(); metalFace = nil
        }
        NSLog("🎭 face: Metal unavailable; AppKit fallback")
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
        metalFace?.stop()
        metalFace?.removeFromSuperview()
        metalFace = nil
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
        let tickTime = CACurrentMediaTime()
        let dt = max(0.001,min(0.08,tickTime-lastTick)); lastTick = tickTime
        let beat = (now.timeIntervalSince1970-performanceEpoch)*performanceBpm/60
        cameraPhase = beat
        let active = max(0, (effort - 0.035) / 0.965)
        vitality = active*active*(3-2*active)
        if expression > 0 && beat >= 0 {
            let b = Int(floor(beat))
            if b != lastMusicalBeat {
                lastMusicalBeat = b
                beatPulse = b % 4 == 0 ? 1 : 0.36
                // Blinks punctuate the backbeat and phrase turns, instead of
                // a random blink cutting straight through every held vowel.
                if vitality > 0.08 && (b % 4 == 3 || b % 8 == 0) { blinkUntil = now.addingTimeInterval(0.095) }
            }
        }
        livingPhase += dt * Double(expression > 0 ? vitality * 14 : 10)
        let phase = livingPhase
        boil = UInt64(phase)
        let fraction = CGFloat(phase - floor(phase))
        boilBlend = fraction*fraction*(3-2*fraction)
        if expression == 0 && now > nextBlink {
            blinkUntil = now.addingTimeInterval(0.13)
            nextBlink = now.addingTimeInterval(2.2 + Double(boil % 30) / 10)
        }
        if now > nextGaze {
            gazeTarget = CGPoint(x: CGFloat(Int(boil % 5)) / 2 - 1, y: CGFloat(Int((boil >> 3) % 3)) / 2 - 0.5)
            nextGaze = now.addingTimeInterval(1.2 + Double((boil >> 5) % 20) / 10)
        }
        if expression > 0 {
            gazeTarget = CGPoint(x: sin(beat * .pi / 4) * 0.85 * Double(vitality), y: 0.05 + Double(vitality)*0.35 + Double(breath)*0.55)
        }
        let response = CGFloat(1-exp(-dt/0.045))
        gaze.x += (gazeTarget.x - gaze.x) * response
        gaze.y += (gazeTarget.y - gaze.y) * response
        squash *= CGFloat(exp(-dt/0.067))
        beatPulse *= CGFloat(exp(-dt/0.12))
        // Jaw from the audio: fast attack, softer release. With no meter for
        // half a second (no tap yet) fall back to the onset-and-decay pulse.
        let metered = now.timeIntervalSince(lastMeter) < 0.5
        let target: CGFloat = metered
            ? min(1, max(0, (liveRms - 0.012) / 0.16))
            : (now < mouthUntil ? 0.85 : 0)
        mouthOpen += (target - mouthOpen) * (target > mouthOpen ? 0.65 : 0.3)
        if now < lipsShutUntil { mouthOpen = 0 }
        drawnPose = previewPose ?? articulationPose?()
        if let pose = drawnPose { mouthOpen = CGFloat(pose.jaw * (1 - pose.seal)) }
        let sounding = metered ? min(1,max(0,(liveRms-0.003)/0.12)) : 0
        effort += (sounding-effort)*CGFloat(1-exp(-dt/(sounding>effort ? 0.045 : 0.20)))
        let incoming = articulationBreath?() ?? 0
        breath += (incoming-breath)*CGFloat(1-exp(-dt/0.028))
        if expression > 0, breath > 0.15, let pose = drawnPose, pose.seal > 0.95, pose.press == 0 {
            var inhale = SingerViseme.oh.pose
            inhale.jaw = Double(breath)*0.22; inhale.width = 0.65
            drawnPose = inhale
        }
        bob += dt * (expression > 0 ? vitality : 1)
        if metalFace == nil { needsDisplay = true }
    }

    /// Deterministic jitter in [-1, 1] for point `i` of the current boil.
    private func j(_ i: Int, _ salt: Int = 0) -> CGFloat {
        func value(_ seed: UInt64) -> CGFloat {
            var h: UInt64 = 1_469_598_103_934_665_603 ^ seed
            for b in [UInt64(i), UInt64(salt), 0x9E37] { h = (h ^ b) &* 1_099_511_628_211 }
            return CGFloat(Int(h % 2001)) / 1000 - 1
        }
        let a = value(boil), b = value(boil &+ 1)
        return a + (b-a)*boilBlend
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
        canvas?.ink(p, width: width, fill: fill)
    }

    override func draw(_ dirtyRect: NSRect) {
        guard metalFace == nil, let context = NSGraphicsContext.current?.cgContext else { return }
        drawArtwork(SingerFaceCanvas(context: context))
    }

    func metalPreview() -> SingerFaceCanvas {
        let canvas = SingerFaceCanvas()
        drawArtwork(canvas)
        return canvas
    }

    private func drawArtwork(_ ctx: SingerFaceCanvas) {
        canvas = ctx
        defer { canvas = nil }
        let t0 = CACurrentMediaTime()
        defer {
            drawMs += (CACurrentMediaTime() - t0) * 1000; drawN += 1
            if metalFace == nil && Date().timeIntervalSince(lastDrawReport) > 5 {
                let secs = Date().timeIntervalSince(lastDrawReport)
                NSLog("🎭 face: %.1f ms/frame · %.0f draws/s · %.0f ticks/s", drawMs / Double(max(1, drawN)),
                      Double(drawN) / secs, Double(ticks) / secs)
                drawMs = 0; drawN = 0; ticks = 0; lastDrawReport = Date()
            }
        }
        let W = bounds.width, H = bounds.height
        let isBB = member == "blueberry"
        let isFrisbee = member == "frisbee"
        let line = W * 0.011
        let mood: CGFloat = previewEffort == nil ? expression : 1
        let energy = previewEffort ?? effort

        let v = max(0,(energy-0.035)/0.965)
        let life = v*v*(3-2*v)
        let motion = mood * max(life,breath*0.18)
        // Soft singing comes closer: a small, slow breath remains visible
        // while the larger gestures settle. Quiet is attentive, not absent.
        let softBreath = (1-life) * CGFloat(0.5+0.5*sin(cameraPhase * .pi/4))
        let base = accent.usingColorSpace(.sRGB) ?? accent
        let cool = NSColor(srgbRed:base.redComponent*0.86+0.06,green:base.greenComponent*0.88+0.04,
                           blue:min(1,base.blueComponent*0.90+0.045),alpha:1)
        let warm = NSColor(srgbRed:min(1,base.redComponent*0.72+0.42),
                           green:min(1,base.greenComponent*0.8+0.30),
                           blue:min(1,base.blueComponent*0.66+0.20),alpha:1)
        let litEdge = cool.blended(withFraction:life,of:warm) ?? base
        let edge = base.blended(withFraction:mood,of:litEdge) ?? base
        ctx.fill(NSBezierPath(rect: bounds), color: base)
        ctx.edgeLight(in:bounds,center:base,edge:edge)

        ctx.saveGState()
        // A small coordinated camera drift follows the common musical clock.
        // Breathing lifts the whole face before the next phrase; accents give
        // it a brief push toward the listener rather than a constant wobble.
        ctx.translateBy(x: W/2, y: H/2)
        let sway = CGFloat(sin(cameraPhase * .pi / (isBB ? 8 : isFrisbee ? 3 : 6)))
        let nod = isBB ? CGFloat(0.5-0.5*cos(cameraPhase * .pi))*life*0.014 : 0
        let bounce = isFrisbee ? CGFloat(abs(sin(cameraPhase * .pi/2)))*life*0.012 : 0
        ctx.translateBy(x: W * (isBB ? 0.014 : isFrisbee ? 0.035 : 0.026) * motion * sway,
                        y: H * mood * (breath*0.014 + life*0.016 + softBreath*0.003 - nod + bounce))
        ctx.rotate(by: motion * (isBB ? 0.012 : isFrisbee ? 0.04 : 0.025) * sway)
        let zoom = 1 + mood * (0.055*(1-life) + 0.04*life + 0.018*beatPulse*life + 0.008*breath + softBreath*0.003)
        ctx.scaleBy(x: zoom, y: zoom)
        ctx.translateBy(x: -W/2, y: -H/2)
        // squash on the onset around the face's center, leaning left then right
        ctx.translateBy(x: W / 2, y: H * 0.5 + sin(bob * 1.1) * H * 0.018 * motion)
        ctx.rotate(by: squash * lean * (isBB ? 0.025 : isFrisbee ? 0.065 : 0.045) * motion)
        ctx.scaleBy(x: 1 + (isBB ? 0.055 : 0.09) * squash * motion, y: 1 - (isBB ? 0.075 : 0.12) * squash * motion)
        ctx.translateBy(x: -W / 2, y: -H * 0.5)

        // Soft blush builds with vocal effort, never replacing member color.
        for side in [-1,1] as [CGFloat] {
            let cheek = NSBezierPath(ovalIn: CGRect(x: W/2 + side*W*0.265 - W*0.105,
                y: H*0.39, width: W*0.21, height: H*(0.12 + breath*0.018)))
            let red = NSColor(srgbRed: 1, green: 0.12, blue: 0.18,
                              alpha: mood*(0.055 + 0.55*life + 0.08*breath))
            ctx.gradient(in: cheek, color: red)
            if isFrisbee {
                // A small constellation of freckles stays readable when quiet.
                for dot in 0..<3 {
                    let x = W/2 + side*W*(0.265 + CGFloat(dot-1)*0.028)
                    let y = H*(0.447 + (dot == 1 ? 0.013 : 0))
                    ctx.fill(NSBezierPath(ovalIn:CGRect(x:x-W*0.005,y:y-H*0.007,width:W*0.01,height:H*0.014)),
                             color:NSColor(srgbRed:0.43,green:0.12,blue:0.19,alpha:0.6))
                }
            } else if isBB {
                let dimple = NSBezierPath()
                let x = W/2 + side*W*0.255
                dimple.move(to:CGPoint(x:x-side*W*0.014,y:H*0.39))
                dimple.curve(to:CGPoint(x:x+side*W*0.006,y:H*0.345),
                    controlPoint1:CGPoint(x:x+side*W*0.015,y:H*0.385),
                    controlPoint2:CGPoint(x:x+side*W*0.02,y:H*0.36))
                ink(dimple,width:line*0.45,fill:nil)
            }
        }

        // eyes
        let blinking = Date() < blinkUntil
        let ey = H * 0.61
        let ex = W * (isBB ? 0.21 : isFrisbee ? 0.19 : 0.175)
        let ew = W * (isBB ? 0.126 : isFrisbee ? 0.106 : 0.088)
        let eh = H * (isBB ? 0.127 : isFrisbee ? 0.15 : 0.155) * (1 + mood*(life*0.18-(1-life)*0.12-breath*0.05))
        for side in [-1, 1] as [CGFloat] {
            let cx = W / 2 + side * ex + W * 0.003 * j(Int(side) + 10, 4)
            let cy = ey + H * 0.004 * j(Int(side) + 12, 5) + (isBB ? 0 : side*H*(isFrisbee ? 0.007 : 0.014))
            if blinking {
                let shut = NSBezierPath()
                shut.move(to: CGPoint(x: cx - ew, y: cy))
                shut.curve(to: CGPoint(x: cx + ew, y: cy), controlPoint1: CGPoint(x: cx - ew * 0.4, y: cy - eh * 0.35), controlPoint2: CGPoint(x: cx + ew * 0.4, y: cy - eh * 0.35))
                ink(shut, width: line, fill: nil)
            } else {
                let eye = blob(cx: cx, cy: cy, rx: ew, ry: eh, lump: 0.025, salt: Int(side) + 20, n: 18)
                ink(eye, width: line, fill: NSColor(white: 0.97, alpha: 1))
                let pr = ew * (isBB ? 0.34 : isFrisbee ? 0.47 : 0.43)
                let px = cx + gaze.x * ew * 0.35, py = cy + gaze.y * eh * 0.3
                ctx.fill(NSBezierPath(ovalIn: CGRect(x: px - pr, y: py - pr, width: pr * 2, height: pr * 2)), color: .black)
                ctx.fill(NSBezierPath(ovalIn: CGRect(x: px - pr * 0.15, y: py + pr * 0.3, width: pr * 0.5, height: pr * 0.5)), color: .white)
                if isBB {
                    // heavy lids: the top of each eye in the face's own color
                    ctx.saveGState(); ctx.clip(eye)
                    let lid = eh * (0.2 + mood*energy*0.3)
                    ctx.fill(NSBezierPath(rect: CGRect(x: cx - ew * 1.2, y: cy + lid, width: ew * 2.4, height: eh * 1.2)), color: accent)
                    ctx.restoreGState()
                    let edge = NSBezierPath()
                    edge.move(to: CGPoint(x: cx - ew, y: cy + lid))
                    edge.line(to: CGPoint(x: cx + ew, y: cy + lid))
                    ink(edge, width: line, fill: nil)
                }
            }
            // brow
            let brow = NSBezierPath()
            let lift = mood * H * (0.035*life - 0.012*(1-life) + 0.012*breath + 0.012*beatPulse*life)
            let curiosity = !isBB && !isFrisbee ? side*H*0.018 : 0
            let by = min(H*0.835, cy + eh * (isBB ? 1.25 : 1.39) + lift + curiosity) + H * 0.005 * j(Int(side) + 30, 6)
            brow.move(to: CGPoint(x: cx - ew * 0.95, y: by - (isBB ? 0 : eh * 0.12)))
            if isBB {
                brow.curve(to: CGPoint(x: cx + ew * 0.95, y: by), controlPoint1: CGPoint(x: cx-ew*0.3,y:by+lift), controlPoint2: CGPoint(x:cx+ew*0.3,y:by+lift))
            } else if isFrisbee {
                brow.curve(to: CGPoint(x: cx + ew * 0.95, y: by - eh * 0.12), controlPoint1: CGPoint(x: cx - ew * 0.3, y: by + eh * 0.3), controlPoint2: CGPoint(x: cx + ew * 0.3, y: by + eh * 0.3))
            } else {
                brow.line(to:CGPoint(x:cx-ew*0.12,y:by+eh*0.17))
                brow.line(to:CGPoint(x:cx+ew*0.95,y:by-eh*0.06))
            }
            ink(brow, width: line * (isBB ? 1.65 : isFrisbee ? 0.85 : 1.2), fill: nil)
        }

        // Compact nose silhouettes: angular curiosity, a broad bass nose,
        // and a tiny upturned button. Leave clear space above the visemes.
        let nose = NSBezierPath()
        if isBB {
            nose.move(to:CGPoint(x:W*0.471,y:H*0.46))
            nose.curve(to:CGPoint(x:W*0.529,y:H*0.46),controlPoint1:CGPoint(x:W*0.477,y:H*0.484),controlPoint2:CGPoint(x:W*0.523,y:H*0.484))
        } else if isFrisbee {
            nose.move(to:CGPoint(x:W*0.491,y:H*0.464))
            nose.curve(to:CGPoint(x:W*0.512,y:H*0.469),controlPoint1:CGPoint(x:W*0.50,y:H*0.452),controlPoint2:CGPoint(x:W*0.508,y:H*0.456))
        } else {
            nose.move(to:CGPoint(x:W*0.501,y:H*0.49))
            nose.line(to:CGPoint(x:W*0.486,y:H*0.46))
            nose.line(to:CGPoint(x:W*0.508,y:H*0.461))
        }
        ink(nose,width:line*0.52,fill:nil)

        // mouth — above the caption line
        let pose = previewPose ?? drawnPose
        let openness = pose.map { CGFloat($0.jaw * (1 - $0.seal)) } ?? mouthOpen
        let my = H * 0.33
        let mw = W * (isBB ? 0.18 : isFrisbee ? 0.16 : 0.145)
        if openness < 0.08 {
            let smile = NSBezierPath()
            let press = CGFloat(pose?.press ?? 0)
            let w = mw * CGFloat(pose?.width ?? (isBB ? 1.0 : 1.25))
            let corners = my + H * 0.03 * (1 - press)
            smile.move(to: CGPoint(x: W / 2 - w, y: corners))
            smile.curve(to: CGPoint(x: W / 2 + w, y: corners + (!isBB && !isFrisbee ? H*0.018*(1-press) : 0)), controlPoint1: CGPoint(x: W / 2 - w * 0.4, y: my - H * (isBB ? 0.012 : 0.025) * (1 - press)), controlPoint2: CGPoint(x: W / 2 + w * 0.4, y: my - H * 0.025 * (1 - press)))
            ink(smile, width: line * 1.2, fill: nil)
            if press > 0.25 {
                let lower = NSBezierPath()
                lower.move(to: CGPoint(x: W / 2 - w * 0.65, y: my - H * 0.025))
                lower.curve(to: CGPoint(x: W / 2 + w * 0.65, y: my - H * 0.025), controlPoint1: CGPoint(x: W / 2 - w * 0.3, y: my - H * 0.042), controlPoint2: CGPoint(x: W / 2 + w * 0.3, y: my - H * 0.042))
                ink(lower, width: line * 0.5 * press, fill: nil)
            }
        } else {
            var w = mw, h = H * 0.17 * openness
            var teeth = true, tongue = false, bite = false
            if let pose {
                w *= CGFloat(pose.width)
                teeth = pose.teeth > 0.1
                tongue = pose.tongue > 0.5
                bite = pose.bite > 0.5
            } else { switch mouthShape {
            case 1: w *= 1.15                                   // A/I — wide open
            case 2: w *= 1.45; h *= 0.5                         // E — spread
            case 3: w *= 0.5; h *= 0.9; teeth = false           // W/OO — small round
            case 4: w *= 0.78; h *= 1.2; teeth = false          // O — round open
            case 5: w *= 1.2; h *= 0.35; bite = true            // F/V — teeth on the lip
            case 6: w *= 0.9; h *= 0.55; tongue = true          // L/D/N/T — tongue up
            default: w *= 0.85; h *= 0.7
            } }
            if pose == nil && liveZcr > 0.22 && mouthOpen < 0.6 { w *= 1.2; h *= 0.45 }
            h = max(h, H * 0.015)
            // Rounded vowels keep full cheeks; spread vowels pull the corners.
            // The upper lip stays steadier while the lower jaw opens on an arc.
            let mouth = NSBezierPath()
            let rounded = CGFloat(pose?.round ?? 0.2)
            let top = h * 0.62, bottom = h * 1.38
            mouth.move(to: CGPoint(x: W/2-w, y: my))
            mouth.curve(to: CGPoint(x: W/2+w, y: my), controlPoint1: CGPoint(x: W/2-w*(0.55+rounded*0.35), y: my+top*1.4), controlPoint2: CGPoint(x: W/2+w*(0.55+rounded*0.35), y: my+top*1.4))
            mouth.curve(to: CGPoint(x: W/2-w, y: my), controlPoint1: CGPoint(x: W/2+w*0.85, y: my-bottom*1.3), controlPoint2: CGPoint(x: W/2-w*0.85, y: my-bottom*1.3))
            mouth.close()
            ink(mouth, width: line * 1.2, fill: NSColor(srgbRed: 0.1, green: 0.03, blue: 0.05, alpha: 1))
            ctx.saveGState(); ctx.clip(mouth)
            if teeth {
                let depth = CGFloat(pose?.teeth ?? 0.7)
                ctx.fill(NSBezierPath(roundedRect: CGRect(x: W / 2 - w * 0.86, y: my + top - h * (bite ? 0.9 : 0.48 * depth), width: w * 1.72, height: h), xRadius: w * 0.12, yRadius: h * 0.18), color: NSColor(white: 0.96, alpha: 1))
                if (pose?.teeth ?? 0) >= 0.99 && !bite {
                    ctx.fill(NSBezierPath(rect: CGRect(x: W / 2 - w, y: my - bottom, width: w * 2, height: h * 0.35)), color: NSColor(white: 0.96, alpha: 1))
                }
            }
            if tongue {
                let reach = CGFloat(pose?.tongue ?? 1)
                ctx.fill(NSBezierPath(roundedRect: CGRect(x: W / 2 - w * 0.28, y: my - bottom, width: w * 0.56, height: h * (1.1 + reach * 0.7)), xRadius: w * 0.24, yRadius: h * 0.4), color: NSColor(srgbRed: 0.85, green: 0.25, blue: 0.35, alpha: 1))
            } else if !bite && (pose == nil || (pose?.tongue ?? 0) > 0.02) {
                let amount = CGFloat(pose?.tongue ?? 0.5)
                ctx.fill(NSBezierPath(ovalIn: CGRect(x: W / 2 - w * 0.5, y: my - bottom * 1.1, width: w, height: h * (0.35 + amount))), color: NSColor(srgbRed: 0.85, green: 0.25, blue: 0.35, alpha: 1))
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
            ctx.text(attr, at: NSPoint(x: W * 0.035, y: H - pt * 1.6))
            ctx.ink(NSBezierPath(rect: bounds.insetBy(dx: 0.5, dy: 0.5)), width: 1, fill: nil)
        }
    }
}
