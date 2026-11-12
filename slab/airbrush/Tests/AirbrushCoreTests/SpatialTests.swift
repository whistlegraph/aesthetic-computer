import XCTest
@testable import AirbrushCore

final class SpatialTests: XCTestCase {
    func dabs(_ count: Int) -> [Dab] {
        var brush = Brush(); brush.radius = 3; brush.scatter = 0
        return (0..<count).flatMap { i in
            Spray.dabs(Sample(point: Point(Double(i % 100) * 10 - 500, Double(i / 100) * 10 - 500), brush: brush), seed: 42, index: i)
        }
    }
    func drain(_ cursor: SpatialDabs.Cursor, budget: Int = 17) -> [Dab] {
        var result: [Dab] = []
        var iterations = 0
        while !cursor.done {
            var work = budget
            result += cursor.take(work: &work)
            XCTAssertGreaterThanOrEqual(work, 0)
            iterations += 1
            XCTAssertLessThan(iterations, 100_000)
        }
        return result
    }
    func testSpatialQueryMatchesOrderedBruteForceAcrossBudgets() {
        let source = dabs(10_000), store = SpatialDabs()
        store.append(contentsOf: source)
        let rect = PaintRect(-73, -31, 201, 123)
        let expected = source.filter {
            PaintRect($0.point.x - $0.radius, $0.point.y - $0.radius, $0.radius * 2, $0.radius * 2).intersects(rect)
        }
        XCTAssertEqual(drain(store.query(rect), budget: 1), expected)
        XCTAssertEqual(drain(store.query(rect), budget: 1000), expected)
    }
    func testCursorSnapshotAndSuffixDoNotDoublePaintOnAppend() {
        let store = SpatialDabs(), source = dabs(1000), rect = PaintRect(-1000, -1000, 2000, 2000)
        store.append(contentsOf: Array(source.prefix(300)))
        let beforeAppend = store.query(rect)
        store.append(contentsOf: Array(source.dropFirst(300)))
        XCTAssertEqual(drain(store.query(rect, after: 150, before: 300)), Array(source[150..<300]))
        XCTAssertEqual(drain(beforeAppend), Array(source.prefix(300)))
        XCTAssertEqual(drain(store.query(rect, after: 300)), Array(source.dropFirst(300)))
    }
    func testTruncateThenAppendDoesNotResurrectUndonePaint() {
        let store = SpatialDabs(), source = dabs(1000)
        store.append(contentsOf: source)
        store.truncate(to: 71)
        store.append(contentsOf: Array(source.suffix(9)))
        XCTAssertEqual(drain(store.query(PaintRect(-1000, -1000, 2000, 2000))), Array(source.prefix(71)) + Array(source.suffix(9)))
    }
    func testDistantHistorySkipsInOneBudgetUnit() {
        let store = SpatialDabs(); store.append(contentsOf: dabs(100_000))
        let query = store.query(PaintRect(1e9, 1e9, 10, 10))
        var work = 1
        XCTAssertEqual(query.take(work: &work), [])
        XCTAssertTrue(query.done)
    }
    func testLODAndNegativeTilesStayBounded() {
        XCTAssertEqual(PaintTileKey(level: 0, x: -1, y: -3).parent, PaintTileKey(level: -1, x: -1, y: -2))
        var camera = Camera()
        for zoom in [1e-12, 1e-6, 0.5, 1, 2, 1e6, 1e12] {
            camera.zoom = zoom
            let keys = PaintTileKey.visible(camera: camera, size: Point(7680, 4320), backingScale: 2)
            XCTAssertLessThanOrEqual(keys.count, 128)
        }
        camera.zoom = 1e-12
        let distant = PaintTileKey.visible(camera: camera, size: Point(800, 600), backingScale: 2)
        XCTAssertLessThan(distant[0].level, -30)
        camera.center = Point(1e15, -1e15)
        XCTAssertLessThanOrEqual(PaintTileKey.visible(camera: camera, size: Point(1000, 1000), backingScale: 4).count, 128)
    }
}
