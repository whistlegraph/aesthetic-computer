import CoreGraphics
import Foundation
import Vision

// Used only on FrameCapture's serial queue. Cache one bounded analysis buffer,
// never the screenshot, OCR, AX tree, pointer position, or session baseline.
final class FrameVisualControls {
    private var pixels: Data?
    private var dimensions = CGSize.zero
    private var boxes: [CGRect] = []
    private(set) var cacheHit = false

    func controls(_ cg: CGImage, scale: Double, origin: CGPoint,
                  focus: CGPoint?) -> [[String: Any]] {
        cacheHit = false
        let w = min(512, cg.width)
        let h = max(1, Int(Double(cg.height) * Double(w) / Double(cg.width)))
        guard let ctx = CGContext(data: nil, width: w, height: h, bitsPerComponent: 8,
            bytesPerRow: w * 4, space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) else { return [] }
        ctx.interpolationQuality = .medium
        ctx.draw(cg, in: CGRect(x: 0, y: 0, width: w, height: h))
        guard let data = ctx.data, let scan = ctx.makeImage() else { return [] }
        let current = Data(bytes: data, count: ctx.bytesPerRow * h)
        let size = CGSize(width: w, height: h)
        // Exact byte equality, including image dimensions. Changed analysis pixels
        // always invalidate; no age limit or approximate/perceptual matching.
        if dimensions == size, pixels == current {
            cacheHit = true
        } else {
            let dark = VNDetectContoursRequest()
            dark.contrastAdjustment = 1.5
            dark.detectsDarkOnLight = true
            let light = VNDetectContoursRequest()
            light.contrastAdjustment = 1.5
            light.detectsDarkOnLight = false
            do { try VNImageRequestHandler(cgImage: scan, options: [:]).perform([dark, light]) }
            catch { pixels = nil; boxes = []; return [] }
            guard let darkResult = dark.results?.first, let lightResult = light.results?.first
            else { pixels = nil; boxes = []; return [] }
            boxes = [darkResult, lightResult].flatMap { $0.topLevelContours }
                .map { $0.normalizedPath.boundingBox }
            pixels = current
            dimensions = size
        }
        let scanScale = scale * Double(w) / Double(cg.width)
        let W = Double(w) / scanScale, H = Double(h) / scanScale
        var out: [[String: Any]] = []
        for b in boxes {
            let x = origin.x + b.minX * W
            let y = origin.y + (1 - b.maxY) * H
            let bw = b.width * W, bh = b.height * H
            let ratio = bw / max(bh, 0.1)
            guard bw >= 10, bh >= 10, bw <= 46, bh <= 46, ratio >= 0.65, ratio <= 1.5 else { continue }
            let cx = x + bw / 2, cy = y + bh / 2
            let distance = focus.map { hypot(cx - $0.x, cy - $0.y) } ?? 0
            let duplicate = out.contains { abs(($0["cx"] as? Int ?? 0) - Int(cx)) < 3 && abs(($0["cy"] as? Int ?? 0) - Int(cy)) < 3 }
            if !duplicate { out.append(["kind": "compact-control", "cx": Int(cx), "cy": Int(cy),
                "r": [Int(x), Int(y), Int(bw), Int(bh)], "distance": Int(distance)]) }
        }
        return out.sorted { ($0["distance"] as? Int ?? 0) < ($1["distance"] as? Int ?? 0) }.prefix(24).map { $0 }
    }
}
