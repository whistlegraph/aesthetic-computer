// Compile with FrameVisualControls.swift; no app, screen permissions, or input.
import CoreGraphics
import Foundation

@main struct FrameVisualControlsTests {
    static func check(_ ok: Bool, _ message: String) {
        if !ok { fatalError(message) }
    }
    static func image(dark: Bool = false, extra: Bool = false, width: Int = 512) -> CGImage {
        let ctx = CGContext(data: nil, width: width, height: 320, bitsPerComponent: 8,
            bytesPerRow: width * 4, space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue)!
        ctx.setFillColor(gray: dark ? 0 : 1, alpha: 1)
        ctx.fill(CGRect(x: 0, y: 0, width: width, height: 320))
        ctx.setFillColor(gray: dark ? 1 : 0, alpha: 1)
        ctx.fill(CGRect(x: 80, y: 80, width: 28, height: 28))
        ctx.fillEllipse(in: CGRect(x: 240, y: 160, width: 32, height: 32))
        if extra { ctx.fill(CGRect(x: 360, y: 60, width: 24, height: 24)) }
        return ctx.makeImage()!
    }
    static func equal(_ a: [[String: Any]], _ b: [[String: Any]]) -> Bool {
        NSDictionary(dictionary: ["controls": a]).isEqual(to: ["controls": b])
    }
    static func main() {
        for dark in [false, true] {
            let detector = FrameVisualControls(), original = image(dark: dark)
            let first = detector.controls(original, scale: 1, origin: .zero, focus: nil)
            check(!detector.cacheHit && first.count >= 2, "must detect both fixture shapes on either background")
            for (x, y) in [(94, 226), (256, 144)] {
                check(first.contains { abs(($0["cx"] as! Int) - x) <= 2 && abs(($0["cy"] as! Int) - y) <= 2 },
                      "shape centers must map from image coordinates to global top-left points")
            }
            let repeated = detector.controls(image(dark: dark), scale: 1, origin: .zero, focus: nil)
            check(detector.cacheHit && equal(first, repeated), "identical pixels must reuse identical controls")
            let moved = detector.controls(original, scale: 1, origin: CGPoint(x: 200, y: 400), focus: nil)
            check(detector.cacheHit, "origin does not invalidate normalized shapes")
            for (before, after) in zip(first, moved) {
                check((after["cx"] as! Int) == (before["cx"] as! Int) + 200, "global x must move")
                check((after["cy"] as! Int) == (before["cy"] as! Int) + 400, "global y must move")
            }
            let focus = CGPoint(x: first.last!["cx"] as! Int, y: first.last!["cy"] as! Int)
            let ranked = detector.controls(original, scale: 1, origin: .zero, focus: focus)
            check(detector.cacheHit && (ranked.first!["distance"] as! Int) == 0, "focus ranking must refresh on a cache hit")
            let scaled = detector.controls(original, scale: 2, origin: .zero, focus: nil)
            let freshScaled = FrameVisualControls().controls(original, scale: 2, origin: .zero, focus: nil)
            check(detector.cacheHit && equal(scaled, freshScaled), "scale must be recomputed")
            let changed = image(dark: dark, extra: true)
            let next = detector.controls(changed, scale: 1, origin: .zero, focus: nil)
            check(!detector.cacheHit && next.count > first.count, "a new control must invalidate cached pixels")
            check(equal(next, FrameVisualControls().controls(changed, scale: 1, origin: .zero, focus: nil)), "changed pixels must match a fresh scan")
            _ = detector.controls(image(dark: dark, width: 420), scale: 1, origin: .zero, focus: nil)
            check(!detector.cacheHit, "new dimensions must invalidate")
        }
        let fixture = image(), detector = FrameVisualControls()
        var cold: [Double] = [], warm: [Double] = []
        for _ in 0..<5 {
            var start = Date()
            _ = FrameVisualControls().controls(fixture, scale: 1, origin: .zero, focus: nil)
            cold.append(Date().timeIntervalSince(start) * 1000)
            _ = detector.controls(fixture, scale: 1, origin: .zero, focus: nil)
            start = Date()
            _ = detector.controls(fixture, scale: 1, origin: .zero, focus: nil)
            warm.append(Date().timeIntervalSince(start) * 1000)
        }
        print("PASS: light/dark shapes, exact invalidation, origin, scale, focus, dimensions")
        print("fixture median ms: uncached=\(cold.sorted()[2]), cached=\(warm.sorted()[2])")
    }
}
