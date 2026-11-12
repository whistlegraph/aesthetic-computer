import XCTest
@testable import AirbrushCore

final class PaintTests: XCTestCase {
    func sample(_ x: Double = 0, hardness: Double = 0.3) -> Sample {
        var brush = Brush()
        brush.hardness = hardness
        return Sample(point: Point(x, 20), brush: brush)
    }
    func testSeedKnownAnswerAndRandomAccess() {
        // Published SplitMix64 zero-state vector. Locks the saved v1 algorithm.
        XCTAssertEqual(Spray.bits(seed: 0, index: 0), 0xe220a8397b1dcdaf)
        XCTAssertEqual(Spray.bits(seed: 0, index: 1), 0x6e789e6aa1b965f4)
        let stroke = Stroke(seed: 42, samples: (0..<20).map { sample(Double($0)) })
        let whole = Spray.replay(stroke)
        let reversedChunks = stroke.samples.indices.reversed().map {
            Spray.dabs(stroke.samples[$0], seed: stroke.seed, index: $0)
        }.reversed().flatMap { $0 }
        XCTAssertEqual(whole, reversedChunks)
        XCTAssertNotEqual(whole, Spray.replay(Stroke(seed: 43, samples: stroke.samples)))
    }
    func testSaveOpenPreservesSeedAndExactScatter() throws {
        var p = Painting()
        p.strokes = [Stroke(seed: UInt64.max, samples: [sample(), sample(10, hardness: 1)])]
        let reopened = try Painting.decode(p.encoded())
        XCTAssertEqual(p, reopened)
        XCTAssertEqual(Spray.replay(p.strokes[0]), Spray.replay(reopened.strokes[0]))
    }
    func testSamplingIndependentOfEventBatching() {
        var coarse = StrokeSampler(first: sample())
        let a = coarse.append(sample(120), at: 1)
        var fine = StrokeSampler(first: sample())
        var b: [Sample] = []
        for i in 1...60 { b += fine.append(sample(Double(i) * 2), at: Double(i) / 60) }
        XCTAssertEqual(a.count, 120)
        XCTAssertEqual(a.count, b.count)
        for (x, y) in zip(a, b) {
            XCTAssertEqual(x.point.x, y.point.x, accuracy: 1e-10)
            XCTAssertEqual(x.brush, y.brush)
        }
    }
    func testStationaryPaintAndHardnessPreserveScatterLocations() {
        var sampler = StrokeSampler(first: sample())
        XCTAssertEqual(sampler.append(sample(), at: 0.5).count, 60)
        let soft = Spray.dabs(sample(hardness: 0), seed: 9, index: 2)
        let hard = Spray.dabs(sample(hardness: 1), seed: 9, index: 2)
        XCTAssertEqual(soft.map(\.point), hard.map(\.point))
        XCTAssertEqual(soft.map(\.radius), hard.map(\.radius))
        XCTAssertTrue(soft.allSatisfy { $0.brush.hardness == 0 })
        XCTAssertTrue(hard.allSatisfy { $0.brush.hardness == 1 })
    }
    func testScatterBoundsAndZeroFlow() {
        var s = sample()
        s.brush.scatter = 1
        for i in 0..<100 {
            for dab in Spray.dabs(s, seed: 123, index: i) {
                let d = dab.point - s.point
                XCTAssertLessThanOrEqual(sqrt(d.x * d.x + d.y * d.y) + dab.radius, s.brush.radius + 1e-10)
            }
        }
        s.brush.flow = 0
        XCTAssertTrue(Spray.dabs(s, seed: 123, index: 0).allSatisfy { $0.opacity == 0 })
    }
    func testZoomAnchorAndRoundTrip() {
        var camera = Camera()
        camera.center = Point(1e9, -1e9)
        let size = Point(800, 600), anchor = Point(133, 421)
        let before = camera.world(anchor, size: size)
        camera.magnify(20, anchor: anchor, size: size)
        let after = camera.world(anchor, size: size)
        XCTAssertEqual(before.x, after.x, accuracy: 1e-6)
        XCTAssertEqual(before.y, after.y, accuracy: 1e-6)
        let roundTrip = camera.screen(after, size: size)
        XCTAssertEqual(roundTrip.x, anchor.x, accuracy: 1e-5)
        XCTAssertEqual(roundTrip.y, anchor.y, accuracy: 1e-5)
    }
    func testZoomPastOldLimitAndSaveLargeWorldBrush() throws {
        var camera = Camera(); camera.zoom = 1e-6
        let size = Point(800, 600), anchor = Point(120, 210)
        let before = camera.world(anchor, size: size)
        camera.magnify(1e-6, anchor: anchor, size: size)
        XCTAssertEqual(camera.zoom, 1e-12)
        let anchored = camera.screen(before, size: size)
        XCTAssertEqual(anchored.x, anchor.x, accuracy: 1e-6)
        XCTAssertEqual(anchored.y, anchor.y, accuracy: 1e-6)
        var document = Painting(); document.camera = camera
        var brush = Brush(); brush.radius = 512 / camera.zoom
        document.strokes = [Stroke(seed: 42, samples: [Sample(point: camera.world(Point(10, 10), size: size), brush: brush)])]
        XCTAssertEqual(try Painting.decode(document.encoded()).strokes, document.strokes)
    }
    func testMarkerIsDenseContinuousAndDoesNotBuildUpAtRest() throws {
        var brush = Brush(); brush.marker = true; brush.radius = 2
        brush.hardness = 1; brush.flow = 1; brush.scatter = 0
        let first = Sample(point: Point(), brush: brush)
        var sampler = StrokeSampler(first: first)
        let samples = [first] + sampler.append(Sample(point: Point(400, 0), brush: brush), at: Spray.step)
        XCTAssertEqual(samples.last?.point, Point(400, 0))
        for pair in zip(samples, samples.dropFirst()) {
            XCTAssertLessThanOrEqual(pair.1.point.x - pair.0.point.x, 1.00001)
        }
        XCTAssertTrue(Spray.replay(Stroke(seed: 42, samples: samples)).allSatisfy { $0.opacity > 0.999 })
        XCTAssertTrue(sampler.append(samples.last!, at: Spray.step * 2).isEmpty)
        var document = Painting(); document.version = 1
        document.strokes = [Stroke(seed: 42, samples: samples)]
        let decoded = try Painting.decode(document.encoded())
        XCTAssertEqual(decoded.version, 2)
        XCTAssertEqual(decoded.strokes.flatMap(Spray.replay), document.strokes.flatMap(Spray.replay))
        // Documents predating marker support still decode to the old exposure.
        var old = Painting(); old.version = 1
        var legacy = brush; legacy.marker = nil
        old.strokes = [Stroke(seed: 42, samples: [Sample(point: Point(), brush: legacy)])]
        let reopened = try Painting.decode(old.encoded())
        XCTAssertEqual(reopened.version, 1)
        XCTAssertLessThan(Spray.replay(reopened.strokes[0])[0].opacity, 0.1)
    }
    func testRejectUnknownAlgorithmAndInvalidBrush() throws {
        var p = Painting()
        p.version = 3
        XCTAssertThrowsError(try Painting.decode(p.encoded()))
        p.version = 1
        var s = sample()
        s.brush.radius = -1
        p.strokes = [Stroke(seed: 0, samples: [s])]
        XCTAssertThrowsError(try Painting.decode(p.encoded()))
    }
}
