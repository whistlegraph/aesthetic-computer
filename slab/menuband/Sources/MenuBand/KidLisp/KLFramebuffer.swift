// CPU framebuffer + immediate-mode primitives.
//
// Pixel format: RGBA8, row-major. (0,0) is top-left.
// This is the layer the Metal blit will consume — keep it
// Foundation-only so it can run from a CLI without AppKit.

import Foundation

final class KLFramebuffer {
    let width: Int
    let height: Int
    var pixels: [UInt8]   // length = width * height * 4, RGBA

    init(width: Int, height: Int) {
        self.width = max(1, width)
        self.height = max(1, height)
        self.pixels = [UInt8](repeating: 0, count: self.width * self.height * 4)
    }

    @inline(__always)
    private func index(_ x: Int, _ y: Int) -> Int {
        return (y * width + x) * 4
    }

    /// Standard "over" composite: `dest = src.rgb * α + dest.rgb * (1-α)`.
    /// The framebuffer stays at alpha=255 throughout — partial alpha lives
    /// only on the incoming ink color. This matches kidlisp.mjs's draw
    /// semantics for `ink … N` where N is alpha.
    @inline(__always)
    func setPixel(_ x: Int, _ y: Int, _ c: KLColor) {
        guard x >= 0, y >= 0, x < width, y < height else { return }
        let i = index(x, y)
        if c.a == 255 {
            pixels[i] = c.r
            pixels[i + 1] = c.g
            pixels[i + 2] = c.b
            pixels[i + 3] = 255
            return
        }
        let alpha = Double(c.a) / 255.0
        let inv = 1.0 - alpha
        let dr = Double(pixels[i]),     sr = Double(c.r)
        let dg = Double(pixels[i + 1]), sg = Double(c.g)
        let db = Double(pixels[i + 2]), sb = Double(c.b)
        pixels[i]     = UInt8(max(0.0, min(255.0, sr * alpha + dr * inv)))
        pixels[i + 1] = UInt8(max(0.0, min(255.0, sg * alpha + dg * inv)))
        pixels[i + 2] = UInt8(max(0.0, min(255.0, sb * alpha + db * inv)))
        pixels[i + 3] = 255   // canvas stays opaque
    }

    func wipe(_ c: KLColor) {
        let total = width * height
        var i = 0
        for _ in 0..<total {
            pixels[i] = c.r
            pixels[i + 1] = c.g
            pixels[i + 2] = c.b
            pixels[i + 3] = c.a
            i += 4
        }
    }

    /// Bresenham line. Matches graph.mjs `line()` integer floor behavior.
    func line(_ x0In: Int, _ y0In: Int, _ x1In: Int, _ y1In: Int, _ c: KLColor) {
        var x0 = x0In, y0 = y0In
        let x1 = x1In, y1 = y1In
        let dx = abs(x1 - x0)
        let dy = -abs(y1 - y0)
        let sx = x0 < x1 ? 1 : -1
        let sy = y0 < y1 ? 1 : -1
        var err = dx + dy
        while true {
            setPixel(x0, y0, c)
            if x0 == x1 && y0 == y1 { break }
            let e2 = 2 * err
            if e2 >= dy { err += dy; x0 += sx }
            if e2 <= dx { err += dx; y0 += sy }
        }
    }

    /// Separable Gaussian blur of pixel radius `r`. Kernel = 2r+1, σ = r/3.
    /// Identity at r ≤ 0. Mirrors graph.mjs Gaussian variant (the
    /// `applyHorizontalBlur` / `applyVerticalBlur` pair) so visuals stay
    /// consistent across runtimes.
    func blur(radius r: Int) {
        guard r > 0 else { return }
        let w = width, h = height
        let kernelSize = min(2 * r + 1, 15)
        let radius = kernelSize / 2
        let sigma = max(0.25, Double(radius) / 3.0)
        var weights = [Double](repeating: 0, count: kernelSize)
        var wsum = 0.0
        for i in 0..<kernelSize {
            let x = Double(i - radius)
            let v = exp(-(x * x) / (2 * sigma * sigma))
            weights[i] = v
            wsum += v
        }
        for i in 0..<kernelSize { weights[i] /= wsum }

        var tmp = [UInt8](repeating: 0, count: w * h * 4)
        // Horizontal pass: pixels -> tmp
        for y in 0..<h {
            for x in 0..<w {
                var rs = 0.0, gs = 0.0, bs = 0.0, asum = 0.0
                for k in 0..<kernelSize {
                    let xx = min(max(x + k - radius, 0), w - 1)
                    let i = (y * w + xx) * 4
                    let wk = weights[k]
                    rs += Double(pixels[i]) * wk
                    gs += Double(pixels[i + 1]) * wk
                    bs += Double(pixels[i + 2]) * wk
                    asum += Double(pixels[i + 3]) * wk
                }
                let oi = (y * w + x) * 4
                tmp[oi] = UInt8(max(0.0, min(255.0, rs)))
                tmp[oi + 1] = UInt8(max(0.0, min(255.0, gs)))
                tmp[oi + 2] = UInt8(max(0.0, min(255.0, bs)))
                tmp[oi + 3] = UInt8(max(0.0, min(255.0, asum)))
            }
        }
        // Vertical pass: tmp -> pixels
        for y in 0..<h {
            for x in 0..<w {
                var rs = 0.0, gs = 0.0, bs = 0.0, asum = 0.0
                for k in 0..<kernelSize {
                    let yy = min(max(y + k - radius, 0), h - 1)
                    let i = (yy * w + x) * 4
                    let wk = weights[k]
                    rs += Double(tmp[i]) * wk
                    gs += Double(tmp[i + 1]) * wk
                    bs += Double(tmp[i + 2]) * wk
                    asum += Double(tmp[i + 3]) * wk
                }
                let oi = (y * w + x) * 4
                pixels[oi] = UInt8(max(0.0, min(255.0, rs)))
                pixels[oi + 1] = UInt8(max(0.0, min(255.0, gs)))
                pixels[oi + 2] = UInt8(max(0.0, min(255.0, bs)))
                pixels[oi + 3] = UInt8(max(0.0, min(255.0, asum)))
            }
        }
    }

    /// Filled circle, midpoint algorithm.
    func circle(_ cx: Int, _ cy: Int, _ r: Int, _ c: KLColor) {
        guard r > 0 else { setPixel(cx, cy, c); return }
        for dy in -r...r {
            let span = Int(Double(r * r - dy * dy).squareRoot().rounded())
            let y = cy + dy
            for dx in -span...span {
                setPixel(cx + dx, y, c)
            }
        }
    }

    /// Wrap-around scroll by float pixels. Fractional offsets snap to int.
    func scroll(dx: Double, dy: Double) {
        let w = width, h = height
        let ix = ((Int(dx.rounded()) % w) + w) % w
        let iy = ((Int(dy.rounded()) % h) + h) % h
        if ix == 0 && iy == 0 { return }
        var dst = [UInt8](repeating: 0, count: w * h * 4)
        for y in 0..<h {
            let srcY = ((y - iy) % h + h) % h
            for x in 0..<w {
                let srcX = ((x - ix) % w + w) % w
                let si = (srcY * w + srcX) * 4
                let di = (y * w + x) * 4
                dst[di]     = pixels[si]
                dst[di + 1] = pixels[si + 1]
                dst[di + 2] = pixels[si + 2]
                dst[di + 3] = pixels[si + 3]
            }
        }
        pixels = dst
    }

    /// Push RGB values toward / away from 128 by `factor`. Alpha untouched.
    func contrast(factor f: Double) {
        guard f > 0 else { return }
        let w = width, h = height
        var i = 0
        for _ in 0..<(w * h) {
            for c in 0..<3 {
                let v = Double(pixels[i + c])
                let stretched = (v - 128.0) * f + 128.0
                pixels[i + c] = UInt8(max(0.0, min(255.0, stretched)))
            }
            i += 4
        }
    }

    /// Vortex spin (matches graph.mjs §spinBlockBased). `integerSteps` is
    /// the already-accumulated whole-pixel step count — the evaluator does
    /// the fractional accumulation. Per-pixel angle change = `steps /
    /// distance_from_center`, so near-center pixels twist more than edges,
    /// producing spiral arms. Source coords wrap (modulo) for the tiled-
    /// tunnel look $roz depends on.
    func spinVortex(integerSteps: Double) {
        guard integerSteps != 0 else { return }
        let w = width, h = height
        let cx = Double(w) / 2.0, cy = Double(h) / 2.0
        var dst = [UInt8](repeating: 0, count: w * h * 4)
        let twoPi = 2.0 * .pi
        for y in 0..<h {
            let dy = Double(y) - cy
            let dy2 = dy * dy
            for x in 0..<w {
                let dx = Double(x) - cx
                let d2 = dx * dx + dy2
                let di = (y * w + x) * 4
                if d2 < 1.0 {
                    let si = (y * w + x) * 4
                    dst[di]     = pixels[si]
                    dst[di + 1] = pixels[si + 1]
                    dst[di + 2] = pixels[si + 2]
                    dst[di + 3] = pixels[si + 3]
                    continue
                }
                let dist = d2.squareRoot()
                let angle = atan2(dy, dx)
                var srcAngle = angle - integerSteps / dist
                srcAngle -= twoPi * floor(srcAngle / twoPi)
                if srcAngle < 0 { srcAngle += twoPi }
                let sxf = cx + dist * cos(srcAngle)
                let syf = cy + dist * sin(srcAngle)
                var sx = Int(sxf.rounded())
                var sy = Int(syf.rounded())
                sx = ((sx % w) + w) % w
                sy = ((sy % h) + h) % h
                let si = (sy * w + sx) * 4
                dst[di]     = pixels[si]
                dst[di + 1] = pixels[si + 1]
                dst[di + 2] = pixels[si + 2]
                dst[di + 3] = pixels[si + 3]
            }
        }
        pixels = dst
    }

    /// Center-anchored zoom with wrap (matches graph.mjs §zoom). The
    /// evaluator throttles call-rate via an accumulator; this method just
    /// applies the given scale once. Wrap (not clamp) makes repeated
    /// zoom-in calls produce the recursive-tunnel pattern that $roz uses.
    func zoom(scale: Double, anchorX: Double = 0.5, anchorY: Double = 0.5) {
        guard scale > 0, scale != 1.0 else { return }
        let w = width, h = height
        let ax = Double(w) * anchorX
        let ay = Double(h) * anchorY
        let inv = 1.0 / scale
        var dst = [UInt8](repeating: 0, count: w * h * 4)
        for y in 0..<h {
            let syf = (Double(y) - ay) * inv + ay
            var sy = Int(syf.rounded())
            sy = ((sy % h) + h) % h
            for x in 0..<w {
                let sxf = (Double(x) - ax) * inv + ax
                var sx = Int(sxf.rounded())
                sx = ((sx % w) + w) % w
                let di = (y * w + x) * 4
                let si = (sy * w + sx) * 4
                dst[di]     = pixels[si]
                dst[di + 1] = pixels[si + 1]
                dst[di + 2] = pixels[si + 2]
                dst[di + 3] = pixels[si + 3]
            }
        }
        pixels = dst
    }

    /// Vertical fade: top row is `colors.first`, bottom row is `colors.last`,
    /// intermediates equally spaced. Used by `fade:r-g-b` shorthand.
    func wipeFade(_ colors: [KLColor]) {
        guard !colors.isEmpty else { return }
        guard colors.count > 1 else { wipe(colors[0]); return }
        let stops = colors.count - 1
        let h = height
        for y in 0..<h {
            let t = Double(y) / Double(max(1, h - 1))
            let f = t * Double(stops)
            let i0 = min(Int(f), stops)
            let i1 = min(i0 + 1, stops)
            let frac = f - Double(i0)
            let c0 = colors[i0], c1 = colors[i1]
            let inv: Double = 1.0 - frac
            let r0d: Double = Double(c0.r), r1d: Double = Double(c1.r)
            let g0d: Double = Double(c0.g), g1d: Double = Double(c1.g)
            let b0d: Double = Double(c0.b), b1d: Double = Double(c1.b)
            let a0d: Double = Double(c0.a), a1d: Double = Double(c1.a)
            let r = UInt8(max(0.0, min(255.0, inv * r0d + frac * r1d)))
            let g = UInt8(max(0.0, min(255.0, inv * g0d + frac * g1d)))
            let b = UInt8(max(0.0, min(255.0, inv * b0d + frac * b1d)))
            let a = UInt8(max(0.0, min(255.0, inv * a0d + frac * a1d)))
            let row = KLColor(r, g, b, a)
            for x in 0..<width { setPixel(x, y, row) }
        }
    }

    /// Serialize to binary PPM (P6) — opaque, no alpha. Enough for golden frame diffs.
    func writePPM(to url: URL) throws {
        var header = "P6\n\(width) \(height)\n255\n".data(using: .ascii)!
        var body = Data(capacity: width * height * 3)
        var i = 0
        for _ in 0..<(width * height) {
            body.append(pixels[i])
            body.append(pixels[i + 1])
            body.append(pixels[i + 2])
            i += 4
        }
        header.append(body)
        try header.write(to: url)
    }
}
