import AppKit

/// The jam wall — Menu Band's full-screen visualizer, a lava lamp split the
/// way the keyboard is: melody on the left, percussion on the right.
///
/// Nothing is drawn as a line or a grid. Every note is a soft blob of its
/// pitch-class color (notepat's rainbow) that wells up on the left the frame
/// the key goes down — low notes near the floor, high notes near the ceiling
/// — swells while it's held, then drifts upward and cools out over a couple
/// of seconds. Every drum is a burst on the right in the kit's color: a kick
/// blooms big and low and white, a hat sparks small and violet up top, a
/// crash washes the whole side. Blobs add into one field, so neighbors melt
/// together, and the master level breathes a glow up the seam. Chords read
/// as a cluster of color; a groove reads as weather.
///
/// Rendered as a metaball field at a quarter of the screen's resolution and
/// scaled up through Core Graphics' bilinear filter — the upscale IS the
/// glow — so a laptop pays ~100k cells a frame and the headless
/// `--render-visualizer` capture (cacheDisplay) renders the real thing.
final class JamWallView: NSView {
    weak var menuBand: MenuBandController?

    /// The active voice's family color — the seam's breath and the faint
    /// ambient under everything, fed by the overlay.
    var tintColor: NSColor = .controlAccentColor {
        didSet {
            tint = Self.components(tintColor)
            needsDisplay = true
        }
    }
    private var tint: (r: Float, g: Float, b: Float) = (0.4, 0.5, 1)

    /// When false the wall stops sampling the synth and holds still.
    var isLive: Bool = true {
        didSet {
            guard isLive != oldValue, window != nil else { return }
            if isLive { start() } else { stop() }
        }
    }

    var onClick: (() -> Void)?

    override func mouseDown(with event: NSEvent) {
        if let onClick { onClick() } else { super.mouseDown(with: event) }
    }

    // MARK: - Field

    /// Screen points per field cell.
    private static let cell: CGFloat = 4
    private var fw = 0
    private var fh = 0
    private var field: [Float] = []     // fw × fh × RGB, linear light
    private var pixels: [UInt8] = []    // fw × fh × RGBA
    private var rebuildField = true

    // MARK: - Blobs

    private enum Kind { case note, drum, seam }
    private struct Blob {
        var kind: Kind
        var x: Float            // field cells
        var y: Float
        var vy: Float           // cells / s (lava rises)
        var r: Float            // radius, cells
        var rGrowth: Float      // cells / s while held (notes) or dying (drums)
        var stretchX: Float     // >1 squashes the blob horizontally (seam)
        var color: (r: Float, g: Float, b: Float)
        var energy: Float       // brightness; >1 overdrives to white
        var tau: Float          // decay time once released, s
        var held: Bool
        var midi: UInt8?
        var phase: Float
        var sway: Float         // cells
    }
    private var blobs: [Blob] = []
    private static let maxBlobs = 64

    /// How long each drum's burst lingers — the drum's own tail.
    private static let drumTau: [Float] = [
        0.45,  // kick
        1.70,  // crash
        0.50,  // snare
        0.90,  // splash
        0.40,  // clap
        0.30,  // snap
        0.50,  // cowbell
        0.24,  // hatClosed
        0.32,  // block
        0.75,  // hatOpen
        0.55,  // tambo
        1.30,  // ride
    ]
    /// Where each drum lives on the right side (0–1 of the half, bottom-up
    /// y): kick on the floor, cymbals up by the ceiling, the rest between.
    private static let drumHome: [(x: Float, y: Float, size: Float)] = [
        (0.42, 0.20, 1.45),  // kick
        (0.80, 0.86, 1.60),  // crash
        (0.60, 0.46, 1.10),  // snare
        (0.22, 0.84, 1.00),  // splash
        (0.30, 0.42, 0.85),  // clap
        (0.72, 0.30, 0.55),  // snap
        (0.16, 0.56, 0.70),  // cowbell
        (0.56, 0.74, 0.55),  // hatClosed
        (0.84, 0.50, 0.65),  // block
        (0.48, 0.80, 0.80),  // hatOpen
        (0.14, 0.26, 0.75),  // tambo
        (0.86, 0.70, 1.25),  // ride
    ]

    // MARK: - Palette

    private static func rgb(_ r: Int, _ g: Int, _ b: Int) -> NSColor {
        NSColor(srgbRed: CGFloat(r) / 255, green: CGFloat(g) / 255, blue: CGFloat(b) / 255, alpha: 1)
    }

    /// notepat's rainbow for the naturals; a sharp is the blend of its
    /// neighbors, a shade paler, so it reads as "between" rather than as the
    /// black key the chord readout draws. (The beat bus borrows both
    /// palettes for the desktop's vibe color.)
    static let melodyColors: [NSColor] = (0..<12).map { pc -> NSColor in
        if NoteColors.isAccidental(pitchClass: pc) {
            let lo = NoteColors.color(pitchClass: pc - 1)
            let hi = NoteColors.color(pitchClass: pc + 1)
            let mix = lo.blended(withFraction: 0.5, of: hi) ?? lo
            return mix.blended(withFraction: 0.25, of: .white) ?? mix
        }
        return NoteColors.color(pitchClass: pc)
    }.map { $0.usingColorSpace(.sRGB) ?? $0 }

    /// One color per drum, in `MenuBandPercussion.Drum` raw order — the DMX
    /// fixture palette brought indoors.
    static let drumColors: [NSColor] = [
        rgb(255, 255, 255),  // kick
        rgb(235, 210, 255),  // crash
        rgb(255, 60, 60),    // snare
        rgb(120, 170, 255),  // splash
        rgb(255, 170, 40),   // clap
        rgb(255, 240, 80),   // snap
        rgb(255, 110, 200),  // cowbell
        rgb(170, 90, 255),   // hatClosed
        rgb(80, 230, 120),   // block
        rgb(200, 160, 255),  // hatOpen
        rgb(60, 230, 230),   // tambo
        rgb(110, 120, 255),  // ride
    ]

    private static func components(_ c: NSColor) -> (r: Float, g: Float, b: Float) {
        let s = c.usingColorSpace(.sRGB) ?? c
        return (Float(s.redComponent), Float(s.greenComponent), Float(s.blueComponent))
    }
    private static let melodyRGB = melodyColors.map(components)
    private static let drumRGB = drumColors.map(components)

    // MARK: - Clock + level

    private var now: Double = 0
    private var lastTickTime: Double = 0
    private var timer: Timer?
    private var displayLinkObj: Any?
    private var samples = [Float](repeating: 0, count: 2048)
    private var level: Float = 0
    private var idleTicks = 0
    private var seed: UInt32 = 0x9E3779B9

    /// Deterministic jitter so the headless capture is reproducible.
    private func rand() -> Float {
        seed = seed &* 1664525 &+ 1013904223
        return Float(seed >> 8) / Float(1 << 24)
    }

    // MARK: - Lifecycle

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
    }
    required init?(coder: NSCoder) { fatalError() }

    override var isOpaque: Bool { true }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if window != nil, isLive { start() } else { stop() }
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        rebuildField = true
    }

    deinit {
        timer?.invalidate()
        if #available(macOS 14.0, *), let link = displayLinkObj as? CADisplayLink {
            link.invalidate()
        }
    }

    private func start() {
        guard timer == nil, displayLinkObj == nil else { return }
        menuBand?.setWaveformCaptureEnabled(true)
        lastTickTime = CACurrentMediaTime()
        now = lastTickTime
        if #available(macOS 14.0, *) {
            let link = displayLink(target: self, selector: #selector(displayTick(_:)))
            link.preferredFrameRateRange = CAFrameRateRange(minimum: 30, maximum: 60, preferred: 60)
            link.add(to: .main, forMode: .common)
            displayLinkObj = link
        } else {
            let t = Timer(timeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in self?.tick() }
            t.tolerance = 1.0 / 90.0
            RunLoop.main.add(t, forMode: .common)
            timer = t
        }
    }

    private func stop() {
        guard timer != nil || displayLinkObj != nil else { return }
        timer?.invalidate(); timer = nil
        if #available(macOS 14.0, *), let link = displayLinkObj as? CADisplayLink {
            link.invalidate()
        }
        displayLinkObj = nil
        menuBand?.setWaveformCaptureEnabled(false)
    }

    @available(macOS 14.0, *)
    @objc private func displayTick(_ link: CADisplayLink) { tick() }

    private func ensureField() {
        let w = max(1, Int(bounds.width / Self.cell))
        let h = max(1, Int(bounds.height / Self.cell))
        guard rebuildField || w != fw || h != fh else { return }
        rebuildField = false
        // Keep the blobs where they were, proportionally, across a resize.
        if fw > 0, fh > 0 {
            let sx = Float(w) / Float(fw), sy = Float(h) / Float(fh)
            for i in blobs.indices {
                blobs[i].x *= sx; blobs[i].y *= sy
                blobs[i].r *= min(sx, sy)
            }
        }
        fw = w
        fh = h
        field = .init(repeating: 0, count: w * h * 3)
        pixels = .init(repeating: 255, count: w * h * 4)
    }

    // MARK: - Events (main thread)

    func noteOn(_ midi: UInt8, velocity: UInt8) {
        ensureField()
        let v = Float(velocity) / 127
        let pc = Int(midi) % 12
        // Low notes well up near the floor, high notes near the ceiling;
        // the same pitch class returns to roughly the same column so a
        // repeated note breathes in place instead of scattering.
        let pitch = min(1, max(0, (Float(midi) - 45) / 42))
        let home = Float((pc * 7) % 12) / 11
        let x = Float(fw) * (0.06 + 0.37 * home) + (rand() - 0.5) * Float(fw) * 0.05
        let y = Float(fh) * (0.10 + 0.72 * pitch) + (rand() - 0.5) * Float(fh) * 0.06
        let base = Float(min(fw, fh)) * (0.11 + 0.08 * v)
        // Born at nearly full size and over-bright: the key-down is the
        // frame you see it, not the start of a fade-in.
        let blob = Blob(kind: .note, x: x, y: y,
                        vy: Float(fh) * (0.010 + 0.012 * rand()),
                        r: base * 0.85, rGrowth: base * 0.9,
                        stretchX: 1,
                        color: Self.melodyRGB[pc],
                        energy: 1.6, tau: 1.1, held: true, midi: midi,
                        phase: rand() * 6.283, sway: Float(fw) * 0.012)
        push(blob)
        idleTicks = 0
        needsDisplay = true
    }

    func noteOff(_ midi: UInt8) {
        for i in blobs.indices where blobs[i].kind == .note && blobs[i].midi == midi && blobs[i].held {
            blobs[i].held = false
        }
    }

    func releaseAllNotes() {
        for i in blobs.indices where blobs[i].kind == .note { blobs[i].held = false }
    }

    func drumHit(_ drum: MenuBandPercussion.Drum, velocity: UInt8) {
        ensureField()
        let i = drum.rawValue
        let v = Float(velocity) / 127
        let home = Self.drumHome[i]
        let half = Float(fw) * 0.5
        let x = half + half * (0.06 + 0.88 * home.x) + (rand() - 0.5) * half * 0.10
        let y = Float(fh) * home.y + (rand() - 0.5) * Float(fh) * 0.08
        let base = Float(min(fw, fh)) * 0.16 * home.size * (0.6 + 0.6 * v)
        let blob = Blob(kind: .drum, x: x, y: y,
                        vy: Float(fh) * 0.02,
                        r: base, rGrowth: base * 0.6,
                        stretchX: 1,
                        color: Self.drumRGB[i],
                        energy: 1.3 + 0.7 * v, tau: Self.drumTau[i], held: false, midi: nil,
                        phase: rand() * 6.283, sway: 0)
        push(blob)
        idleTicks = 0
        needsDisplay = true
    }

    private func push(_ blob: Blob) {
        if blobs.count >= Self.maxBlobs {
            // Drop the dimmest unheld blob to make room.
            if let i = blobs.indices.filter({ !blobs[$0].held }).min(by: { blobs[$0].energy < blobs[$1].energy }) {
                blobs.remove(at: i)
            } else {
                blobs.removeFirst()
            }
        }
        blobs.append(blob)
    }

    // MARK: - Model

    private func tick() {
        ensureField()
        let t = CACurrentMediaTime()
        let dt = min(0.25, max(0.0005, t - lastTickTime))
        lastTickTime = t
        var rms: Float = 0
        if let menuBand {
            menuBand.synthSnapshotWaveform(into: &samples)
            var sumSq: Float = 0
            for s in samples { sumSq += s * s }
            rms = (sumSq / Float(samples.count)).squareRoot()
        }
        let busy = step(now: t, dt: dt, level: rms)
        // A dark wall is a still picture: repaint at 6 fps so the first note
        // is still caught, without burning the field for nothing.
        idleTicks = busy ? 0 : idleTicks + 1
        if busy || idleTicks % 10 == 0 { needsDisplay = true }
    }

    /// Advance the lamp by `dt`. Returns whether anything is still glowing.
    @discardableResult
    private func step(now: Double, dt: Double, level rms: Float) -> Bool {
        self.now = now
        let dtf = Float(dt)
        // Master level, lifted the way the scope lifts it; the seam breathes it.
        let target = min(1, (rms * 2.2).squareRoot())
        if target > level { level = target } else { level += (target - level) * min(1, dtf * 6) }

        for i in blobs.indices {
            var b = blobs[i]
            b.y += b.vy * dtf
            if b.kind == .note {
                b.x += cosf(Float(now) * 0.6 + b.phase) * b.sway * dtf
                if b.held {
                    // The over-bright attack settles into a sustain that
                    // breathes; the blob keeps swelling the longer the key
                    // is down.
                    let breath = 0.85 + 0.12 * sinf(Float(now) * 2.1 + b.phase)
                    b.energy += (breath - b.energy) * min(1, dtf * 7)
                    b.r = min(b.r + b.rGrowth * dtf * 0.45, b.rGrowth * 1.9)
                } else {
                    b.energy *= expf(-dtf / b.tau)
                    b.r += b.rGrowth * dtf * 0.12   // cools and spreads
                }
            } else if b.kind == .drum {
                b.energy *= expf(-dtf / b.tau)
                b.r += b.rGrowth * dtf * 0.9       // bloom outward as it dies
            }
            blobs[i] = b
        }
        blobs.removeAll { ($0.kind == .note && !$0.held && $0.energy < 0.015)
            || ($0.kind == .drum && $0.energy < 0.02)
            || $0.y - $0.r > Float(fh) * 1.1 }
        return !blobs.isEmpty || level > 0.01
    }

    // MARK: - Capture

    /// Pose the lamp with a believable jam for the HEADLESS capture
    /// (`--render-visualizer`), where no audio engine runs and nothing would
    /// ever light: a left-hand phrase, a kick/snare/hat pattern on the
    /// right, the seam mid-breath. Deterministic.
    func seedSyntheticJam() {
        rebuildField = true
        ensureField()
        seed = 0x9E3779B9
        let dt = 1.0 / 60.0
        var t = 0.0
        let phrase: [(Double, UInt8, Double)] = [   // (onset, midi, length)
            (0.2, 60, 0.9), (0.8, 64, 0.5), (1.2, 67, 0.5), (1.6, 71, 1.4),
            (2.4, 69, 0.4), (2.7, 67, 0.4), (3.0, 64, 1.1), (3.6, 72, 0.5),
            (3.95, 71, 0.4), (4.3, 67, 1.6), (4.3, 60, 1.6), (4.3, 64, 1.6),
        ]
        var pattern: [(Double, MenuBandPercussion.Drum, UInt8)] = []
        var beat = 0.25
        var n = 0
        while beat < 5.0 {
            pattern.append((beat, n % 2 == 0 ? .kick : .snare, n % 2 == 0 ? 118 : 104))
            pattern.append((beat + 0.25, .hatClosed, 80))
            if n % 4 == 3 { pattern.append((beat + 0.375, .clap, 96)) }
            if n == 7 { pattern.append((beat, .crash, 110)) }
            beat += 0.5
            n += 1
        }
        var pi = 0, di = 0
        var offs: [(Double, UInt8)] = []
        while t < 5.0 {
            while pi < phrase.count, phrase[pi].0 <= t {
                noteOn(phrase[pi].1, velocity: 104)
                offs.append((phrase[pi].0 + phrase[pi].2, phrase[pi].1))
                pi += 1
            }
            offs.removeAll { off in
                if off.0 <= t { noteOff(off.1); return true }
                return false
            }
            while di < pattern.count, pattern[di].0 <= t {
                drumHit(pattern[di].1, velocity: pattern[di].2)
                di += 1
            }
            step(now: t, dt: dt, level: Float(0.10 + 0.14 * abs(sin(t * 7.3))))
            t += dt
        }
        needsDisplay = true
    }

    // MARK: - Drawing

    override func draw(_ dirtyRect: NSRect) {
        let bounds = self.bounds
        guard bounds.width > 6, bounds.height > 6,
              let ctx = NSGraphicsContext.current?.cgContext else { return }
        ensureField()
        renderField()
        guard let image = makeImage() else { return }
        ctx.interpolationQuality = .high
        // The field is bottom-up (row 0 is the floor); CGImage rows are
        // top-down, so flip once here and the model keeps its gravity.
        ctx.saveGState()
        ctx.translateBy(x: 0, y: bounds.height)
        ctx.scaleBy(x: 1, y: -1)
        ctx.draw(image, in: bounds)
        ctx.restoreGState()
    }

    /// Accumulate every blob into the linear-light field, then tone-map to
    /// pixels. Blobs add, so overlaps go brighter and eventually white, the
    /// way light does.
    private func renderField() {
        let w = fw, h = fh
        let ambient = (tint.r * 0.030, tint.g * 0.030, tint.b * 0.030)
        for i in 0..<(w * h) {
            field[i * 3] = ambient.0
            field[i * 3 + 1] = ambient.1
            field[i * 3 + 2] = ambient.2
        }
        // The seam: a tall soft column of the voice color, as bright as the
        // music is loud.
        var all = blobs
        all.append(Blob(kind: .seam, x: Float(w) * 0.5, y: Float(h) * 0.5, vy: 0,
                        r: Float(h) * 0.62, rGrowth: 0, stretchX: 5.5,
                        color: tint, energy: 0.10 + 0.55 * level, tau: 1, held: false,
                        midi: nil, phase: 0, sway: 0))
        for b in all {
            let wobble = b.kind == .seam ? 1 : 1 + 0.07 * sinf(Float(now) * 1.3 + b.phase)
            let r = b.r * wobble
            guard r > 0.5, b.energy > 0.001 else { continue }
            // Blobs are two lobes: the body and a satellite that slowly
            // orbits, so nothing is a perfect disc.
            let lobes: [(Float, Float, Float, Float)] = b.kind == .seam
                ? [(b.x, b.y, r, b.energy)]
                : [(b.x, b.y, r, b.energy),
                   (b.x + cosf(Float(now) * 0.5 + b.phase) * r * 0.45,
                    b.y + sinf(Float(now) * 0.37 + b.phase) * r * 0.45,
                    r * 0.72, b.energy * 0.7)]
            for (cx, cy, lr, e) in lobes {
                let rx = lr / b.stretchX
                let x0 = max(0, Int(cx - rx) - 1), x1 = min(w - 1, Int(cx + rx) + 1)
                let y0 = max(0, Int(cy - lr) - 1), y1 = min(h - 1, Int(cy + lr) + 1)
                guard x0 <= x1, y0 <= y1 else { continue }
                let inv = 1 / (lr * lr)
                let cr = b.color.r * e, cg = b.color.g * e, cb = b.color.b * e
                for y in y0...y1 {
                    let dy = Float(y) - cy
                    let dy2 = dy * dy
                    var idx = (y * w + x0) * 3
                    for x in x0...x1 {
                        let dx = (Float(x) - cx) * b.stretchX
                        let d2 = (dx * dx + dy2) * inv
                        if d2 < 1 {
                            let f0 = 1 - d2
                            let f = f0 * f0 * f0   // soft shoulder, long foot
                            field[idx] += cr * f
                            field[idx + 1] += cg * f
                            field[idx + 2] += cb * f
                        }
                        idx += 3
                    }
                }
            }
        }
        // Tone map: a filmic-ish knee so overlaps bloom toward white instead
        // of clipping per channel, on top of the dark glass.
        let glass: (Float, Float, Float) = (0.035, 0.045, 0.042)
        for i in 0..<(w * h) {
            let r = field[i * 3], g = field[i * 3 + 1], b = field[i * 3 + 2]
            let lum = 0.30 * r + 0.59 * g + 0.11 * b
            let white = max(0, 1 - expf(-max(0, lum - 0.9) * 1.4))   // overdrive → white
            func map(_ v: Float, _ base: Float) -> UInt8 {
                let m = 1 - expf(-v * 1.35)
                let out = base + (1 - base) * (m + (1 - m) * white)
                return UInt8(max(0, min(255, out * 255 + 0.5)))
            }
            pixels[i * 4] = map(r, glass.0)
            pixels[i * 4 + 1] = map(g, glass.1)
            pixels[i * 4 + 2] = map(b, glass.2)
            pixels[i * 4 + 3] = 255
        }
    }

    private func makeImage() -> CGImage? {
        let w = fw, h = fh
        guard w > 0, h > 0 else { return nil }
        let data = Data(pixels)
        guard let provider = CGDataProvider(data: data as CFData) else { return nil }
        return CGImage(width: w, height: h, bitsPerComponent: 8, bitsPerPixel: 32,
                       bytesPerRow: w * 4, space: CGColorSpace(name: CGColorSpace.sRGB)!,
                       bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.noneSkipLast.rawValue),
                       provider: provider, decode: nil, shouldInterpolate: true,
                       intent: .defaultIntent)
    }
}
