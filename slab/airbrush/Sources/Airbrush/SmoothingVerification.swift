import AppKit
import AirbrushCore

enum SmoothingVerification {
    static func run(path: String) throws {
        let painter = try PaintRenderer()
        var brush = Brush(); brush.marker = true; brush.radius = 1.5
        brush.hardness = 1; brush.flow = 1; brush.scatter = 0
        brush.red = 0.06; brush.green = 0.06; brush.blue = 0.06
        var gray = brush; gray.radius = 0.7; gray.red = 0.68; gray.green = 0.66; gray.blue = 0.63
        let size = Point(900, 550)
        var document = Painting()
        for (row, mode) in [StrokeSmoothing.balanced, .string12].enumerated() {
            let raw = (0...960).map { i -> Point in
                let t = Double(i) / 960
                return Point(-350 + t * 700, Double(row) * 220 - 110 + sin(t * .pi * 4) * 65 + sin(t * .pi * 170) * 3)
            }
            func stroke(_ points: [Point], _ brush: Brush) -> Stroke {
                let first = Sample(point: points[0], brush: brush)
                var sampler = StrokeSampler(first: first), samples = [first]
                for (i, point) in points.dropFirst().enumerated() {
                    samples += sampler.append(Sample(point: point, brush: brush), at: Double(i + 1) / 240)
                }
                return Stroke(seed: 42, samples: samples)
            }
            var stabilizer = StrokeStabilizer(point: raw[0], time: 0, mode: mode)
            var filtered = [raw[0]]
            for i in 1..<raw.count { filtered.append(stabilizer.update(raw[i], at: Double(i) / 240)) }
            filtered += stabilizer.finish()
            document.strokes += [stroke(raw, gray), stroke(filtered, brush)]
        }
        let decoded = try Painting.decode(document.encoded())
        let live = document.strokes.flatMap(Spray.replay)
        let replayed = decoded.strokes.flatMap(Spray.replay)
        precondition(live == replayed, "Filtered paths changed when reopened")
        let pixels = try painter.snapshot(dabs: live, camera: Camera(), size: size, width: 900, height: 550)
        let replayPixels = try painter.snapshot(dabs: replayed, camera: Camera(), size: size, width: 900, height: 550)
        precondition(pixels == replayPixels)
        try PaintRenderer.png(pixels, width: 900, height: 550).write(to: URL(fileURLWithPath: path))
        print("PASS smoothed marker pixel replay; gray = raw input, black = filtered; top = adaptive, bottom = string12; \(path)")
    }
}
