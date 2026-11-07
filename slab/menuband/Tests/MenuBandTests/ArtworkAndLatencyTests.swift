import XCTest
import AppKit
import CRealtimeAudio
@testable import MenuBand

final class ArtworkAndLatencyTests: XCTestCase {
    private func bitmap(size: NSSize, scale: CGFloat = 2, flipped: Bool = false,
                        draw: () -> Void) -> NSBitmapImageRep {
        let rep = NSBitmapImageRep(bitmapDataPlanes: nil,
            pixelsWide: Int(ceil(size.width * scale)), pixelsHigh: Int(ceil(size.height * scale)),
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
        NSGraphicsContext.saveGraphicsState()
        let cg = NSGraphicsContext(bitmapImageRep: rep)!.cgContext
        cg.scaleBy(x: scale, y: scale)
        if flipped { cg.translateBy(x: 0, y: size.height); cg.scaleBy(x: 1, y: -1) }
        NSGraphicsContext.current = NSGraphicsContext(cgContext: cg, flipped: flipped)
        draw()
        NSGraphicsContext.restoreGraphicsState()
        return rep
    }

    private func assertSimilar(_ a: NSBitmapImageRep, _ b: NSBitmapImageRep,
                               name: String, tolerance: Double = 3) throws {
        XCTAssertEqual(a.pixelsWide, b.pixelsWide)
        XCTAssertEqual(a.pixelsHigh, b.pixelsHigh)
        var total = 0.0
        for y in 0..<a.pixelsHigh {
            for x in 0..<a.pixelsWide {
                let ca = a.colorAt(x: x, y: y)!.usingColorSpace(.deviceRGB)!
                let cb = b.colorAt(x: x, y: y)!.usingColorSpace(.deviceRGB)!
                // Compare composited channels: RGB in transparent pixels is immaterial.
                total += abs(ca.redComponent * ca.alphaComponent - cb.redComponent * cb.alphaComponent)
                    + abs(ca.greenComponent * ca.alphaComponent - cb.greenComponent * cb.alphaComponent)
                    + abs(ca.blueComponent * ca.alphaComponent - cb.blueComponent * cb.alphaComponent)
                    + abs(ca.alphaComponent - cb.alphaComponent)
            }
        }
        let error = total * 255 / Double(a.pixelsWide * a.pixelsHigh * 4)
        print("ARTWORK \(name) mean pixel error \(error)/255")
        XCTAssertLessThan(error, tolerance, name)
        for (suffix, rep) in [("reference", a), ("cached", b)] {
            try rep.representation(using: .png, properties: [:])!.write(to:
                URL(fileURLWithPath: "/tmp/menuband-\(name)-\(suffix).png"))
        }
    }

    func testTimingPipelinePublishesConcurrentSamplesAndAccountsForDrops() {
        let p = mb_latency_create()!
        defer { mb_latency_destroy(p) }
        let stop = DispatchSemaphore(value: 0), finished = DispatchSemaphore(value: 0)
        DispatchQueue.global().async {
            while stop.wait(timeout: .now()) != .success {
                mb_latency_begin_render(p)
                mb_latency_end_render(p, 10_000, true)
                Thread.sleep(forTimeInterval: 0.0001)
            }
            mb_latency_begin_render(p)
            mb_latency_end_render(p, 10_000, true)
            finished.signal()
        }
        var received: [Double] = []
        var result = MBLatencySample()
        for i in 1...4096 {
            let t = Double(i)
            _ = mb_latency_stage(p, MBLatencySample(source: 1, event: t,
                captured: t, queued: t, staged: t, rendered: 0))
            while mb_latency_pop(p, &result) { received.append(result.event) }
            if i % 64 == 0 { Thread.sleep(forTimeInterval: 0.0001) }
        }
        stop.signal()
        XCTAssertEqual(finished.wait(timeout: .now() + 5), .success)
        while mb_latency_pop(p, &result) { received.append(result.event) }
        XCTAssertGreaterThan(received.count, 0)
        XCTAssertEqual(received, received.sorted())
        XCTAssertEqual(Set(received).count, received.count)
        XCTAssertEqual(received.count + Int(mb_latency_take_drops(p)), 4096)
    }

    func testCellArtworkMatchesUncachedAtBothAppearances() throws {
        let app = NSApplication.shared
        let appearance = app.appearance
        defer { app.appearance = appearance }
        for dark in [false, true] {
            app.appearance = NSAppearance(named: dark ? .darkAqua : .aqua)
            let view = InstrumentListView(frame: .zero)
            view.selectedProgram = 40
            var a: NSBitmapImageRep!, b: NSBitmapImageRep!
            app.effectiveAppearance.performAsCurrentDrawingAppearance {
                view.artworkCachingEnabled = false
                a = bitmap(size: view.bounds.size, flipped: true) { view.draw(view.bounds) }
                view.artworkCachingEnabled = true
                b = bitmap(size: view.bounds.size, flipped: true) { view.draw(view.bounds) }
                let count = view.cachedCellRasterizations
                _ = bitmap(size: view.bounds.size, flipped: true) { view.draw(view.bounds) }
                XCTAssertEqual(view.cachedCellRasterizations, count)
            }
            try assertSimilar(a, b, name: dark ? "grid-dark" : "grid-light")
        }
    }

    func testPianoArtworkMatchesUncachedWithLiveHighlights() throws {
        let app = NSApplication.shared
        let appearance = app.appearance
        let percussion = KeyboardIconRenderer.percussionRightActive
        defer {
            app.appearance = appearance
            KeyboardIconRenderer.artworkCachingEnabled = true
            KeyboardIconRenderer.percussionRightActive = percussion
        }
        for dark in [false, true] {
            app.appearance = NSAppearance(named: dark ? .darkAqua : .aqua)
            for drums in [false, true] {
                KeyboardIconRenderer.percussionRightActive = drums
                func draw(_ cache: Bool) -> NSBitmapImageRep {
                    KeyboardIconRenderer.artworkCachingEnabled = cache
                    let image = KeyboardIconRenderer.image(litNotes: [60, 64, 67], enabled: false,
                        typeMode: true, letterAlpha: { _ in 0.8 }, includeSettings: false)
                    return bitmap(size: image.size) { image.draw(at: .zero, from: .zero,
                        operation: .sourceOver, fraction: 1) }
                }
                var a: NSBitmapImageRep!, b: NSBitmapImageRep!
                app.effectiveAppearance.performAsCurrentDrawingAppearance { a = draw(false); b = draw(true) }
                try assertSimilar(a, b, name: "piano-\(dark ? "dark" : "light")-\(drums ? "drums" : "keys")")
            }
        }
    }

    func testTimingWaitsForBlockThatBeginsAfterSubmission() {
        let p = mb_latency_create()!
        defer { mb_latency_destroy(p) }
        let first = MBLatencySample(source: 1, event: 1, captured: 1.001,
                                    queued: 1.003, staged: 1.004, rendered: 0)
        XCTAssertTrue(mb_latency_stage(p, first))
        mb_latency_begin_render(p)
        var second = first; second.source = 2; second.staged = 1.005
        XCTAssertTrue(mb_latency_stage(p, second))
        mb_latency_end_render(p, 1.006, true)
        var result = MBLatencySample()
        XCTAssertTrue(mb_latency_pop(p, &result))
        XCTAssertEqual(result.source, 1)
        XCTAssertEqual(result.rendered - result.event, 0.006, accuracy: 0.000001)
        XCTAssertFalse(mb_latency_pop(p, &result))
        mb_latency_begin_render(p)
        mb_latency_end_render(p, 1.012, true)
        XCTAssertTrue(mb_latency_pop(p, &result))
        XCTAssertEqual(result.source, 2)
        XCTAssertEqual(result.rendered, 1.012)
        XCTAssertEqual(mb_latency_take_drops(p), 0)
    }

    func testTimingOverflowAndRenderErrorsDropMeasurementsOnly() {
        let p = mb_latency_create()!
        defer { mb_latency_destroy(p) }
        let sample = MBLatencySample(source: 1, event: 1, captured: 1, queued: 1, staged: 1, rendered: 0)
        for _ in 0..<256 { XCTAssertTrue(mb_latency_stage(p, sample)) }
        XCTAssertFalse(mb_latency_stage(p, sample))
        mb_latency_begin_render(p)
        mb_latency_end_render(p, 2, false)
        XCTAssertEqual(mb_latency_take_drops(p), 257)
        var result = MBLatencySample()
        XCTAssertFalse(mb_latency_pop(p, &result))
        XCTAssertTrue(mb_latency_stage(p, sample))
        mb_latency_begin_render(p)
        mb_latency_end_render(p, 2, true)
        XCTAssertTrue(mb_latency_pop(p, &result))
        XCTAssertEqual(result.rendered, 2)
    }
}
