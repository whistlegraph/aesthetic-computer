import AppKit

/// A per-prompt "sigil": a deterministic curvy blob, uniquely multi-coloured
/// from the prompt text, that each session's terminal wears (top-right, under
/// the title bar) as a glanceable identity. The same prompt always draws the
/// same shape *and* the same colours, so prompts are tellable apart at a
/// glance and re-recognizable across the wall.
///
/// Division of labour: the SHAPE and COLOUR encode *which prompt* (hashed
/// here, baked into a still image). The MOTION — a slow continuous spin whose
/// speed and direction the overlay drives separately — encodes the session's
/// STATUS (working spins faster clockwise, calm states drift, attention
/// reverses). So the polygon answers "how many / what state", and the sigil
/// answers "which prompt, and — by how it moves — what state" one level down.
enum SigilRenderer {
    /// Deterministic 64-bit seed for a prompt. FNV-1a over the normalized
    /// prompt text — the same hash family `TitleEmoji` uses, chosen because
    /// Swift's `hashValue` is seed-randomized per launch and a sigil must
    /// survive a menubar restart unchanged. Identical prompts hash alike on
    /// purpose: a recognizable prompt should wear a recognizable mark.
    static func seed(for prompt: String) -> UInt64 {
        var h: UInt64 = 0xcbf2_9ce4_8422_2325
        let norm = prompt.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        for b in norm.utf8 {
            h ^= UInt64(b)
            h = h &* 0x1_0000_0001_b3
        }
        return h == 0 ? 0x9e37_79b9_7f4a_7c15 : h
    }

    /// The rock's pet name: 3–6 pronounceable characters, deterministic from
    /// a caller-chosen identity seed. Session rocks use the session id here,
    /// independently of the evolving prompt seed that shapes their texture.
    /// Alternating consonant/vowel starting on a
    /// consonant (CVC … CVCVCV), which lands on sayable pebble-names like
    /// "gop", "miva", "tazok".
    static func name(seed: UInt64) -> String {
        // Salted so the name draws don't mirror the shape's first draws.
        var rng = SplitMix64(seed ^ 0x5e5a_11ed_000b_b1e5)
        let consonants = Array("bdfgklmnprstvz")
        let vowels = Array("aeiou")
        let len = rng.int(3, 6)
        var out = ""
        for i in 0..<len {
            let set = i % 2 == 0 ? consonants : vowels
            out.append(set[rng.int(0, set.count - 1)])
        }
        return out
    }

    /// A sticky pet name for one session/thread. Keep subject text out of this
    /// seed: the rock may visually evolve with new prompts, but its spoken and
    /// fleet-visible handle must remain recognizable for the session lifetime.
    static func name(forSessionId sessionId: String) -> String {
        name(seed: seed(for: sessionId))
    }

    /// Loopboys keep the pet name stored on their contact route when a CLI
    /// wrapper is reopened around the same provider thread. Ordinary prompts
    /// still derive their name from the ephemeral Slab session id.
    static func name(for session: ClaudeSession) -> String {
        if let data = FileManager.default.contents(atPath: Paths.loopboyConfig),
           let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
           let loops = obj["loops"] as? [String: Any] {
            for value in loops.values {
                guard let loop = value as? [String: Any],
                      (loop["sessionId"] as? String) == session.sessionId,
                      let routeName = loop["name"] as? String,
                      !routeName.isEmpty else { continue }
                return routeName
            }
        }
        return name(forSessionId: session.sessionId)
    }

    /// SplitMix64 — a tiny, well-distributed PRNG. Seeding it from the
    /// prompt's FNV hash turns that single hash into the stream of independent
    /// draws the shape + palette want, all deterministically.
    private struct SplitMix64 {
        var state: UInt64
        init(_ seed: UInt64) { state = seed }
        mutating func next() -> UInt64 {
            state = state &+ 0x9e37_79b9_7f4a_7c15
            var z = state
            z = (z ^ (z >> 30)) &* 0xbf58_476d_1ce4_e5b9
            z = (z ^ (z >> 27)) &* 0x94d0_49bb_1331_11eb
            return z ^ (z >> 31)
        }
        mutating func unit() -> CGFloat { CGFloat(next() >> 11) / CGFloat(UInt64(1) << 53) }
        mutating func int(_ lo: Int, _ hi: Int) -> Int { lo + Int(next() % UInt64(hi - lo + 1)) }
    }

    /// Render the still sigil for `seed`: a curvy blob silhouette filled with
    /// hashed sedimentary strata — discrete banded layers, occasional mineral
    /// veins, gentle geological folding and a per-sigil bedding dip, so each
    /// prompt reads as its own little computed rock cross-section rather than a
    /// smooth colour-field. Transparent background, no outline, no centre dot.
    /// `size` is the point size of the square badge, drawn @2x for crisp curves
    /// on retina; the blob's max radius stays under `size/2` so it never clips
    /// as the overlay spins it about its centre.
    static func image(seed: UInt64, dark: Bool, size: CGFloat = 56) -> NSImage {
        let scale: CGFloat = 2
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil,
            pixelsWide: Int(size * scale),
            pixelsHigh: Int(size * scale),
            bitsPerSample: 8,
            samplesPerPixel: 4,
            hasAlpha: true,
            isPlanar: false,
            colorSpaceName: .deviceRGB,
            bytesPerRow: 0,
            bitsPerPixel: 0
        ) else { return NSImage(size: NSSize(width: size, height: size)) }
        rep.size = NSSize(width: size, height: size)

        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
        defer { NSGraphicsContext.restoreGraphicsState() }

        let cx = size / 2, cy = size / 2
        let outer = size * 0.42   // max extent < size/2 so a spinning rock never clips

        var rng = SplitMix64(seed)

        // ── Character rolls: the rotation-invariant fingerprint. These are the
        // cues that survive the badge's spin (proportion, lamination, colour
        // mood, bedding dip) — pushed hard so no two prompts read alike.
        let aspect: CGFloat = 0.55 + 0.45 * rng.unit()   // thin shard … round cobble
        let lumps = rng.unit()                           // 0 smooth … 1 lumpy
        let rMin: CGFloat = 0.78 - 0.42 * lumps          // lumpier ⇒ deeper notches
        let dip = (rng.unit() - 0.5) * 2.2               // ±~63° bedding: flat … near-vertical
        // Always vivid — never grey. And themed to the appearance: light-mode
        // rocks ride bright, dark-mode rocks ride deep, so the wall of rocks
        // matches the screen.
        let satMood: CGFloat = 0.66 + 0.34 * rng.unit()  // saturated … fully saturated
        let briMood: CGFloat = dark ? (0.34 + 0.24 * rng.unit())
                                    : (0.66 + 0.26 * rng.unit())

        // ── Silhouette: hashed radii smoothed into an organic closed curve
        // (Catmull-Rom), then squished on one axis by `aspect` so the specimen
        // is anything from a thin shard to a round cobble.
        let n = rng.int(5, 9)
        let rot0 = rng.unit() * 2 * .pi
        var pts: [NSPoint] = []
        pts.reserveCapacity(n)
        for k in 0..<n {
            let r = outer * (rMin + (1 - rMin) * rng.unit())
            let theta = rot0 + 2 * .pi * CGFloat(k) / CGFloat(n)
            let x = cx + (r * cos(theta)) * aspect   // squish horizontally
            let y = cy + r * sin(theta)
            pts.append(NSPoint(x: x, y: y))
        }
        smoothClosedPath(through: pts).addClip()

        // ── Palette: three hue anchors fanned from a hashed base (not one hue
        // + complement), plus a rare complementary mineral vein. `satMood`
        // shifts the whole rock from grey shale to vivid ore.
        let h0 = rng.unit()
        let fan = 0.10 + 0.40 * rng.unit()
        func hue(_ i: Int) -> CGFloat { (h0 + fan * CGFloat(i)).truncatingRemainder(dividingBy: 1) }
        let veinHue = (h0 + 0.5).truncatingRemainder(dividingBy: 1)

        // Bedding dips in a rotated frame so the layers tilt as a set; the clip
        // (set above) stays fixed.
        let xf = NSAffineTransform()
        xf.translateX(by: cx, yBy: cy)
        xf.rotate(byRadians: dip)
        xf.translateX(by: -cx, yBy: -cy)
        xf.concat()

        // Over-extend the bedding box so the dipped bands fully cover the blob.
        let ext = outer * 1.9
        let xL = cx - ext, xR = cx + ext
        let yBot = cy - ext, yTop = cy + ext

        // Shared fold: every bed boundary rides the same sine — flat-bedded for
        // some rocks, strongly folded for others.
        let amp = ext * (0.10 * rng.unit())
        let freq = (0.6 + 2.2 * rng.unit()) * .pi / ext
        let phase = rng.unit() * 2 * .pi
        func wave(_ x: CGFloat) -> CGFloat { amp * sin(freq * x + phase) }

        // ── A few major beds set large-scale colour regions; the rock is then
        // filled with MANY fine striations whose colour follows the bed they
        // land in but carry per-striation brightness grain — so it reads like
        // real laminated rock (fine, firm layering) instead of flat bands.
        let bedCount = rng.int(3, 6)
        var bedHSB: [(h: CGFloat, s: CGFloat, b: CGFloat)] = []
        var bedEdge: [CGFloat] = []           // cumulative top fraction per bed
        let weights: [CGFloat] = (0..<bedCount).map { _ in 0.5 + rng.unit() }
        let wsum = weights.reduce(0, +)
        var acc: CGFloat = 0
        for i in 0..<bedCount {
            let darkBed = (i % 2 == 0)
            let s = max(0, min(1, satMood + (rng.unit() - 0.5) * 0.16))
            let b = max(0.16, min(1, briMood * (darkBed ? 0.68 : 1.06) + (rng.unit() - 0.5) * 0.10))
            bedHSB.append((hue(rng.int(0, 2)), s, b))
            acc += weights[i] / wsum
            bedEdge.append(acc)
        }
        func bedIndex(_ frac: CGFloat) -> Int {
            for i in 0..<bedCount where frac <= bedEdge[i] { return i }
            return bedCount - 1
        }

        // Fine striations: thin folded lines (~1px), brightness-jittered for
        // grain, with the odd brighter mineral fleck.
        let span = yTop - yBot
        let striation = max(0.7, span * 0.012)
        var y = yBot
        while y < yTop {
            let bed = bedHSB[bedIndex((y - yBot) / span)]
            let col: NSColor
            if rng.unit() < 0.09 {
                // Neon spot striation: a high-chroma electric fleck (hue spun
                // off the vein) — pure punch for at-a-glance identifiability.
                col = NSColor(deviceHue: (veinHue + 0.5 * rng.unit()).truncatingRemainder(dividingBy: 1.0),
                              saturation: 1.0, brightness: 1.0, alpha: 1.0)
            } else {
                let jb = (rng.unit() - 0.5) * 0.20            // brightness grain
                let jh = (rng.unit() - 0.5) * 0.015           // faint hue drift
                col = NSColor(deviceHue: (bed.h + jh + 1).truncatingRemainder(dividingBy: 1.0),
                              saturation: bed.s,
                              brightness: max(0.10, min(1, bed.b + jb)), alpha: 1.0)
            }
            // +0.5 overlap so adjacent striations leave no seam.
            fillBand(yLo: y, yHi: y + striation + 0.5, xL: xL, xR: xR, wave: wave, color: col)
            y += striation
        }

        // ── Gashes: a few cross-cutting fractures slashed across the rock —
        // neon or quartz-white, at hashed angles/offsets. Two near-perpendicular
        // gashes read as a cross-mark. Pure identity hardware (and they survive
        // the spin, so they help tell rocks apart at a glance).
        let gashes = rng.int(0, 3)
        for _ in 0..<gashes {
            let ang = rng.unit() * 2 * .pi
            let off = (rng.unit() - 0.5) * outer * 1.3
            let dx = cos(ang), dy = sin(ang)
            let nx = -dy, ny = dx                          // perpendicular offset dir
            let g = NSBezierPath()
            g.move(to: NSPoint(x: cx + nx * off - dx * ext, y: cy + ny * off - dy * ext))
            g.line(to: NSPoint(x: cx + nx * off + dx * ext, y: cy + ny * off + dy * ext))
            g.lineWidth = 0.7 + 1.4 * rng.unit()
            g.lineCapStyle = .round
            let col = rng.unit() < 0.5
                ? NSColor(deviceHue: rng.unit(), saturation: 1.0, brightness: 1.0, alpha: 0.95)   // neon
                : NSColor(deviceWhite: dark ? 0.96 : 0.12, alpha: 0.9)                              // quartz / fault
            col.setStroke()
            g.stroke()
        }

        let img = NSImage(size: NSSize(width: size, height: size))
        img.addRepresentation(rep)
        img.isTemplate = false
        return img
    }

    /// A full-rect strata texture for wrapping onto the 3D rock mesh — same
    /// hashed beds / neon spots / gashes as the flat sigil, but drawn edge to
    /// edge with FLAT (un-dipped) horizontal bands so the left/right edges
    /// match and the texture tiles seamlessly around the rock's longitude; the
    /// bands then read as latitude strata layers on the sphere.
    static func texture(seed: UInt64, dark: Bool, side: CGFloat = 128) -> NSImage {
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: Int(side), pixelsHigh: Int(side),
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0
        ) else { return NSImage(size: NSSize(width: side, height: side)) }
        rep.size = NSSize(width: side, height: side)
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
        defer { NSGraphicsContext.restoreGraphicsState() }

        var rng = SplitMix64(seed)
        let satMood: CGFloat = 0.70 + 0.30 * rng.unit()
        let briMood: CGFloat = dark ? (0.34 + 0.24 * rng.unit()) : (0.66 + 0.26 * rng.unit())
        let h0 = rng.unit()
        // Wide hue fan → rocks span a real chunk of the wheel (more multi-hued,
        // more tellable apart) rather than close monochrome bands.
        let fan = 0.14 + 0.62 * rng.unit()
        func hue(_ i: Int) -> CGFloat { (h0 + fan * CGFloat(i)).truncatingRemainder(dividingBy: 1) }
        let veinHue = (h0 + 0.5).truncatingRemainder(dividingBy: 1)

        // Fewer beds, one boosted to dominate → each rock has a clear dominant
        // colour with a couple of supporting areas.
        let bedCount = rng.int(2, 5)
        var bedHSB: [(h: CGFloat, s: CGFloat, b: CGFloat)] = []
        var bedEdge: [CGFloat] = []
        var weights: [CGFloat] = (0..<bedCount).map { _ in 0.4 + 0.8 * rng.unit() }
        weights[rng.int(0, bedCount - 1)] += 2.4
        let wsum = weights.reduce(0, +)
        var acc: CGFloat = 0
        for i in 0..<bedCount {
            let darkBed = (i % 2 == 0)
            let s = max(0, min(1, satMood + (rng.unit() - 0.5) * 0.16))
            let b = max(0.16, min(1, briMood * (darkBed ? 0.68 : 1.06) + (rng.unit() - 0.5) * 0.10))
            bedHSB.append((hue(rng.int(0, 2)), s, b))
            acc += weights[i] / wsum
            bedEdge.append(acc)
        }
        func bedIndex(_ frac: CGFloat) -> Int {
            for i in 0..<bedCount where frac <= bedEdge[i] { return i }
            return bedCount - 1
        }

        let flat: (CGFloat) -> CGFloat = { _ in 0 }
        // Thick bands → big bold areas of colour (not fine grain). A little
        // brightness drift keeps each block from being dead-flat; neon streaks
        // are rare so they punctuate rather than fleck.
        let striation = max(2.0, side * 0.05)
        var y: CGFloat = 0
        while y < side {
            let bed = bedHSB[bedIndex(y / side)]
            let jb = (rng.unit() - 0.5) * 0.08
            let col = NSColor(deviceHue: bed.h, saturation: bed.s,
                              brightness: max(0.10, min(1, bed.b + jb)), alpha: 1)
            fillBand(yLo: y, yHi: y + striation + 0.5, xL: 0, xR: side, wave: flat, color: col)
            y += striation
        }
        _ = veinHue

        // Spotting: scattered bright / neon speckles (mineral inclusions) over
        // the big colour areas — lively, and strong per-rock identity.
        let spots = rng.int(40, 110)
        for _ in 0..<spots {
            let sx = rng.unit() * side, sy = rng.unit() * side
            let r = 0.6 + 1.8 * rng.unit()
            let col = rng.unit() < 0.6
                ? NSColor(deviceHue: rng.unit(), saturation: 1, brightness: 1, alpha: 0.9)
                : NSColor(deviceWhite: dark ? 0.92 : 0.10, alpha: 0.8)
            col.setFill()
            NSBezierPath(ovalIn: NSRect(x: sx - r, y: sy - r, width: 2 * r, height: 2 * r)).fill()
        }

        let gashes = rng.int(0, 2)
        for _ in 0..<gashes {
            let gy = rng.unit() * side
            let slope = (rng.unit() - 0.5) * 0.6
            let g = NSBezierPath()
            g.move(to: NSPoint(x: 0, y: gy))
            g.line(to: NSPoint(x: side, y: gy + slope * side))
            g.lineWidth = 1 + 2 * rng.unit()
            g.lineCapStyle = .round
            let col = rng.unit() < 0.5
                ? NSColor(deviceHue: rng.unit(), saturation: 1, brightness: 1, alpha: 0.9)
                : NSColor(deviceWhite: dark ? 0.96 : 0.12, alpha: 0.85)
            col.setStroke()
            g.stroke()
        }

        let img = NSImage(size: NSSize(width: side, height: side))
        img.addRepresentation(rep)
        img.isTemplate = false
        return img
    }

    /// Fill one stratum between two parallel folded boundaries (bottom `yLo`,
    /// top `yHi`), both riding `wave`, across `xL…xR`. Sampling the wave along
    /// the width turns each bed into a gently folded layer.
    private static func fillBand(
        yLo: CGFloat, yHi: CGFloat, xL: CGFloat, xR: CGFloat,
        wave: (CGFloat) -> CGFloat, color: NSColor
    ) {
        let p = NSBezierPath()
        let step: CGFloat = 2
        // Bottom edge, left → right.
        p.move(to: NSPoint(x: xL, y: yLo + wave(xL)))
        var x = xL
        while x <= xR { p.line(to: NSPoint(x: x, y: yLo + wave(x))); x += step }
        p.line(to: NSPoint(x: xR, y: yLo + wave(xR)))
        // Up the right side and back along the top edge, right → left.
        p.line(to: NSPoint(x: xR, y: yHi + wave(xR)))
        x = xR
        while x >= xL { p.line(to: NSPoint(x: x, y: yHi + wave(x))); x -= step }
        p.line(to: NSPoint(x: xL, y: yHi + wave(xL)))
        p.close()
        color.setFill()
        p.fill()
    }

    /// Build a smooth closed bezier through `pts` using a uniform Catmull-Rom
    /// spline (tension 1/6), converted to cubic segments. Indices wrap so the
    /// curve closes seamlessly — the source of the sigils' organic curviness.
    private static func smoothClosedPath(through pts: [NSPoint]) -> NSBezierPath {
        let path = NSBezierPath()
        let n = pts.count
        guard n >= 3 else {
            // Degenerate: just connect what we have.
            if let first = pts.first { path.move(to: first) }
            for p in pts.dropFirst() { path.line(to: p) }
            path.close()
            return path
        }
        path.move(to: pts[0])
        for i in 0..<n {
            let p0 = pts[(i - 1 + n) % n]
            let p1 = pts[i]
            let p2 = pts[(i + 1) % n]
            let p3 = pts[(i + 2) % n]
            let c1 = NSPoint(x: p1.x + (p2.x - p0.x) / 6.0, y: p1.y + (p2.y - p0.y) / 6.0)
            let c2 = NSPoint(x: p2.x - (p3.x - p1.x) / 6.0, y: p2.y - (p3.y - p1.y) / 6.0)
            path.curve(to: p2, controlPoint1: c1, controlPoint2: c2)
        }
        path.close()
        return path
    }
}
