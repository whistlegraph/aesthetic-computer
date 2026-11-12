import AppKit
import MetalKit
import AirbrushCore

enum CacheVerification {
    struct Frame {
        let cpuMS: Double, gpuMS: Double
        let metrics: PaintTileCache.Metrics
    }
    private static func check(_ condition: @autoclosure () -> Bool,
                              _ message: @autoclosure () -> String = "", line: Int = #line) {
        if !condition() { fputs("FAIL cache verification line \(line): \(message())\n", stderr); exit(1) }
    }
    static func run(directory: String) throws {
        let output = URL(fileURLWithPath: directory)
        try FileManager.default.createDirectory(at: output, withIntermediateDirectories: true)
        let painter = try PaintRenderer()
        let width = 768, height = 512
        let size = Point(Double(width), Double(height))
        let descriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .bgra8Unorm, width: width, height: height, mipmapped: false)
        descriptor.storageMode = .shared
        descriptor.usage = [.renderTarget, .shaderRead]
        let texture = painter.device.makeTexture(descriptor: descriptor)!
        func frame(_ cache: PaintTileCache, _ camera: Camera = Camera()) throws -> Frame {
            let command = painter.queue.makeCommandBuffer()!
            let pass = MTLRenderPassDescriptor()
            pass.colorAttachments[0].texture = texture
            let start = CACurrentMediaTime()
            cache.encode(camera: camera, size: size, pixels: CGSize(width: width, height: height), pass: pass, command: command)
            let cpu = (CACurrentMediaTime() - start) * 1000
            command.commit(); command.waitUntilCompleted()
            if let error = command.error { throw error }
            let m = cache.metrics
            check(m.queryWork <= PaintTileCache.workBudget && m.deposits <= PaintTileCache.depositBudget)
            check(m.fragmentArea <= PaintTileCache.fragmentBudget && m.passes <= PaintTileCache.passBudget)
            check(m.resident <= PaintTileCache.maximumTiles)
            return Frame(cpuMS: cpu, gpuMS: (command.gpuEndTime - command.gpuStartTime) * 1000, metrics: m)
        }
        func settle(_ cache: PaintTileCache, _ camera: Camera = Camera()) throws -> Int {
            var frames = 0
            repeat { _ = try frame(cache, camera); frames += 1; check(frames < 10_000, "Cache failed to converge") }
            while cache.needsRefinement
            return frames
        }
        func pixels() -> Data {
            var data = Data(count: width * height * 4)
            data.withUnsafeMutableBytes {
                texture.getBytes($0.baseAddress!, bytesPerRow: width * 4,
                    from: MTLRegionMake2D(0, 0, width, height), mipmapLevel: 0)
            }
            return data
        }
        func difference(_ a: Data, _ b: Data) -> (mean: Double, maximum: Int) {
            var total: Int = 0, maximum = 0
            for (x, y) in zip(a, b) { let d = abs(Int(x) - Int(y)); total += d; maximum = max(maximum, d) }
            return (Double(total) / Double(a.count), maximum)
        }
        let cache = PaintTileCache(painter: painter)
        let demo = AirbrushApp.demo().strokes.flatMap(Spray.replay)
        cache.append(demo)
        let initialFrames = try settle(cache)
        let tiled = pixels()
        let reference = try painter.snapshot(dabs: demo, camera: Camera(), size: size, width: width, height: height)
        let delta = difference(tiled, reference)
        check(delta.mean < 0.2 && delta.maximum <= 4, "Tile seam/order mismatch: \(delta)")
        try PaintRenderer.png(tiled, width: width, height: height).write(to: output.appendingPathComponent("cached.png"))
        try PaintRenderer.png(reference, width: width, height: height).write(to: output.appendingPathComponent("reference.png"))

        var brush = Brush(); brush.radius = 35; brush.red = 0.9; brush.green = 0.3; brush.blue = 0.1
        let addition = (0..<120).flatMap { Spray.dabs(Sample(point: Point(Double($0) - 60, 0), brush: brush), seed: 7, index: $0) }
        cache.append(addition)
        _ = try settle(cache)
        let afterAppend = pixels()
        let appendedReference = try painter.snapshot(dabs: demo + addition, camera: Camera(), size: size, width: width, height: height)
        check(difference(afterAppend, appendedReference).mean < 0.2, "Incremental painting changed compositing order")
        // Undo invalidates every retained LOD and the fallback, then reuses the
        // retained prefix of the spatial index. No removed mark can reappear.
        cache.truncate(to: demo.count)
        _ = try settle(cache)
        check(pixels() == tiled, "Undo cache invalidation resurrected paint")

        var camera = Camera()
        camera.zoom = 2
        _ = try settle(cache, camera)
        let zoomReference = try painter.snapshot(dabs: demo, camera: camera, size: size, width: width, height: height)
        check(difference(pixels(), zoomReference).mean < 0.2, "Zoom LOD mismatch")
        // Move across sufficient disjoint regions to exercise the LRU cap.
        for i in 0..<32 {
            camera.center = Point(Double(i) * 5000, Double(i % 3) * 3000)
            _ = try settle(cache, camera)
        }
        _ = try settle(cache)
        check(pixels() == tiled, "Evicted tiles did not reconstruct deterministically")

        // Regression: inspect unfinished frames, not only settled output.
        let progressive = PaintTileCache(painter: painter)
        _ = try frame(progressive)
        let paper = pixels()
        var oldBrush = Brush()
        oldBrush.radius = 40; oldBrush.scatter = 0; oldBrush.flow = 0.15
        oldBrush.red = 0.1; oldBrush.green = 0.2; oldBrush.blue = 0.8
        let old = (0..<20_000).flatMap {
            Spray.dabs(Sample(point: Point(100, 80), brush: oldBrush), seed: 2, index: $0)
        }
        progressive.replace(old)
        let first = try frame(progressive)
        check(first.metrics.pending && first.metrics.partialTiles > 0 && pixels() != paper,
                     "Unfinished tiles are hidden until replay completes")
        var liveBrush = oldBrush
        liveBrush.hardness = 1; liveBrush.flow = 1
        liveBrush.red = 1; liveBrush.green = 0; liveBrush.blue = 0
        let live = (0..<24).flatMap {
            Spray.dabs(Sample(point: Point(100, 80), brush: liveBrush), seed: 3, index: $0)
        }
        progressive.append(live, live: true)
        let liveFrame = try frame(progressive)
        let livePixels = pixels(), center = ((height / 2 + 80) * width + width / 2 + 100) * 4
        check(liveFrame.metrics.pending && liveFrame.metrics.liveDeposits > 0,
                     "Fresh paint waited behind historical replay")
        check(Int(livePixels[center + 2]) > Int(livePixels[center]) + 50,
                     "Fresh red paint is not visible above the old blue prefix")
        try PaintRenderer.png(livePixels, width: width, height: height).write(to: output.appendingPathComponent("live-during-refinement.png"))
        _ = try settle(progressive)
        let progressiveReference = try painter.snapshot(dabs: old + live, camera: Camera(), size: size, width: width, height: height)
        check(difference(pixels(), progressiveReference).maximum <= 4,
                     "Live preview merge changed the final stroke order")
        // New, uncached LODs must also separate paint arriving before admission.
        var coldCamera = Camera(); coldCamera.zoom = 2
        progressive.append(live, live: true)
        let coldLive = try frame(progressive, coldCamera)
        check(coldLive.metrics.pending && coldLive.metrics.liveDeposits > 0)
        _ = try settle(progressive, coldCamera)
        let coldReference = try painter.snapshot(dabs: old + live + live, camera: coldCamera, size: size, width: width, height: height)
        check(difference(pixels(), coldReference).maximum <= 4)

        // A large-world painting must remain visible beyond the former zoom
        // cutoff. Scale the scene and inverse-scale the camera together.
        let wide = PaintTileCache(painter: painter)
        let expanded = AirbrushApp.demo().strokes.map { stroke in
            Stroke(seed: stroke.seed, samples: stroke.samples.map { sample in
                var brush = sample.brush; brush.radius *= 1e12
                return Sample(point: sample.point * 1e12, brush: brush)
            })
        }.flatMap(Spray.replay)
        var wideCamera = Camera(); wideCamera.zoom = 1e-12
        wide.append(expanded); _ = try settle(wide, wideCamera)
        let wideReference = try painter.snapshot(dabs: expanded, camera: wideCamera, size: size, width: width, height: height)
        check(pixels() != paper && difference(pixels(), wideReference).mean < 1,
              "Extreme zoom lost the painting or chose the wrong LOD")

        var markerBrush = Brush(); markerBrush.marker = true
        markerBrush.radius = 2; markerBrush.hardness = 1; markerBrush.flow = 1; markerBrush.scatter = 0
        markerBrush.red = 0.06; markerBrush.green = 0.06; markerBrush.blue = 0.06
        let start = Sample(point: Point(-200, 0), brush: markerBrush)
        var markerSampler = StrokeSampler(first: start)
        let markerSamples = [start] + markerSampler.append(Sample(point: Point(200, 0), brush: markerBrush), at: Spray.step)
        let markerDabs = Spray.replay(Stroke(seed: 42, samples: markerSamples))
        let markerCache = PaintTileCache(painter: painter)
        markerCache.append(markerDabs, live: true); _ = try settle(markerCache)
        let markerPixels = pixels()
        for x in (width / 2 - 199)...(width / 2 + 199) {
            check(markerPixels[(height / 2 * width + x) * 4] < 25, "Fast marker stroke has a pale gap")
            check(markerPixels[((height / 2 + 4) * width + x) * 4] > 200, "Marker stroke is too wide")
        }
        try PaintRenderer.png(markerPixels, width: width, height: height).write(to: output.appendingPathComponent("marker.png"))

        var reports: [[String: Any]] = []
        for knots in [160, 16_000] {
            var brush = Brush(); brush.radius = 12; brush.scatter = 0.9; brush.flow = 0.15
            let deposits = (0..<knots).flatMap { i -> [Dab] in
                let x = Spray.unit(seed: 72, index: UInt64(i * 2)) * 300 - 150
                let y = Spray.unit(seed: 72, index: UInt64(i * 2 + 1)) * 180 - 90
                return Spray.dabs(Sample(point: Point(x, y), brush: brush), seed: 17, index: i)
            }
            let dense = PaintTileCache(painter: painter)
            dense.append(deposits)
            let coldFrames = try settle(dense)
            var frames: [Frame] = []
            for _ in 0..<30 {
                let f = try frame(dense)
                check(f.metrics.queryWork == 0 && f.metrics.deposits == 0 && f.metrics.passes == 0,
                             "A warm frame replayed stroke history")
                frames.append(f)
            }
            let rawStart = CACurrentMediaTime()
            _ = try painter.snapshot(dabs: deposits, camera: Camera(), size: size, width: width, height: height)
            let rawMS = (CACurrentMediaTime() - rawStart) * 1000
            let cpu = frames.map(\.cpuMS).sorted(), gpu = frames.map(\.gpuMS).sorted()
            var appendMaxDeposits = 0, appendMaxWork = 0
            var appendCPU: [Double] = []
            for i in 0..<30 {
                let fresh = Spray.dabs(Sample(point: Point(), brush: brush), seed: 81, index: i)
                dense.append(fresh, live: true)
                let f = try frame(dense)
                // Only four neighboring tiles intersect this brush. A new
                // emission must not replay any of their historical deposits.
                check(f.metrics.deposits <= fresh.count * 4 && !dense.needsRefinement,
                             "Live painting replayed old deposits or accumulated a backlog")
                appendMaxDeposits = max(appendMaxDeposits, f.metrics.deposits)
                appendMaxWork = max(appendMaxWork, f.metrics.queryWork)
                appendCPU.append(f.cpuMS)
            }
            appendCPU.sort()
            reports.append(["deposits": deposits.count, "coldRefinementFrames": coldFrames,
                "warmCPUmedianMS": cpu[15], "warmCPUp95MS": cpu[28], "warmGPUmedianMS": gpu[15],
                "warmGPUp95MS": gpu[28], "warmQuads": frames[0].metrics.quads,
                "warmHistoryTests": 0, "warmDeposits": 0, "rawReplayWallMS": rawMS,
                "paintingCPUp95MS": appendCPU[28], "paintingMaxDeposits": appendMaxDeposits,
                "paintingMaxQueryWork": appendMaxWork])
            // Pan through already cached tiles and return through LOD changes;
            // every frame, including misses, obeys the same work limits.
            var moving = Camera()
            for i in 0..<40 {
                moving.center = Point(Double(i - 20) * 12, 0)
                moving.zoom = pow(2, Double(i % 20) / 10)
                _ = try frame(dense, moving)
            }
            _ = try settle(dense, moving)
        }
        let result: [String: Any] = ["device": painter.device.name, "viewport": [width, height],
            "comparisonMeanByteError": delta.mean, "comparisonMaximumByteError": delta.maximum,
            "demoRefinementFrames": initialFrames, "maximumResidentTiles": PaintTileCache.maximumTiles,
            "workPerFrame": PaintTileCache.workBudget, "depositsPerFrame": PaintTileCache.depositBudget,
            "fragmentAreaPerFrame": PaintTileCache.fragmentBudget, "benchmarks": reports]
        let report = try JSONSerialization.data(withJSONObject: result, options: [.prettyPrinted, .sortedKeys])
        try report.write(to: output.appendingPathComponent("benchmark.json"))
        print(String(decoding: report, as: UTF8.self))
        print("PASS partial tile visibility, live paint during cold replay, exact live merge, cached/reference pixels, incremental append, undo, zoom LOD, eviction replay, bounded cold work, and density-independent warm work")
    }
}
