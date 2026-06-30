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
    static func image(seed: UInt64, size: CGFloat = 56) -> NSImage {
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
        let satMood: CGFloat = 0.28 + 0.58 * rng.unit()  // shale-grey … vivid mineral
        let briMood: CGFloat = 0.52 + 0.30 * rng.unit()

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

        // Band regime is bimodal: a few thick beds OR many fine laminations —
        // two visibly different rock textures. Beds alternate light/dark for
        // the stratified look; veins are rare and thin.
        let layers = rng.unit() < 0.5 ? rng.int(4, 8) : rng.int(15, 28)
        var beds: [(color: NSColor, weight: CGFloat)] = []
        beds.reserveCapacity(layers)
        for i in 0..<layers {
            if rng.unit() < 0.12 {
                let c = NSColor(deviceHue: veinHue,
                                saturation: min(1, satMood + 0.30),
                                brightness: min(1, briMood + 0.25), alpha: 1.0)
                beds.append((c, 0.20 + 0.15 * rng.unit()))                    // vein
            } else {
                let dark = (i % 2 == 0)
                let bri = max(0.18, min(1, briMood * (dark ? 0.66 : 1.06) + (rng.unit() - 0.5) * 0.12))
                let sat = max(0.0, min(1, satMood + (rng.unit() - 0.5) * 0.18))
                let c = NSColor(deviceHue: hue(rng.int(0, 2)),
                                saturation: sat, brightness: bri, alpha: 1.0)
                beds.append((c, 0.5 + 1.4 * rng.unit()))                      // bed
            }
        }
        let wsum = beds.reduce(0) { $0 + $1.weight }

        var y = yBot
        for bed in beds {
            let h = (bed.weight / wsum) * (yTop - yBot)
            fillBand(yLo: y, yHi: y + h, xL: xL, xR: xR, wave: wave, color: bed.color)
            y += h
        }

        let img = NSImage(size: NSSize(width: size, height: size))
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
