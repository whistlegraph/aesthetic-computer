// Standalone checks for CLT-only Macs without XCTest. Compiles the production
// sources directly; never opens an audio device or launches Menu Band.
import AppKit
import AVFoundation

private func check(_ condition: @autoclosure () -> Bool, _ message: String) {
    precondition(condition(), message)
}

@main
enum AudioSmoothnessChecks {
    static func main() throws {
        var cursor = MonitorReadCursor()
        let partial = cursor.plan(read: 100, written: 108, frames: 16, lead: 32)
        check(partial.count == 8 && partial.underrun, "underrun read unpublished samples")
        check(cursor.plan(read: 108, written: 140, frames: 16, lead: 32).count == 0, "missing recovery cushion")
        let ready = cursor.plan(read: 108, written: 172, frames: 16, lead: 32)
        check(ready.start == 124 && ready.count == 16, "recovery did not restore target lead")
        for read in [1 << 24, (1 << 24) + 1, 1 << 30, (1 << 30) + 17] {
            let plan = cursor.plan(read: read, written: read + 32, frames: 16, lead: 32)
            check(plan.start == read && plan.count == 16, "long-running cursor lost precision")
        }
        var written = 1 << 30, read = written - 32
        for tick in 0..<100_000 {
            let frames = [16, 32, 64, 128][(tick / 1000) % 4]
            written += tick % 17 < 3 ? 0 : (tick % 17 == 3 ? frames * 4 : frames)
            let plan = cursor.plan(read: read, written: written, frames: frames, lead: max(32, frames))
            check(plan.start >= read && plan.start + plan.count <= written && plan.count <= frames,
                  "jitter read stale/unwritten frames")
            read = plan.start + plan.count
        }
        let large = cursor.plan(read: 0, written: 4096, frames: 512, lead: 32)
        check(large.count == 512, "larger callback returned a short block")
        print("PASS: underrun recovery, long uptime, 100,000 jitter cycles, callback-size changes")

        var devices: [UInt32] = [], busy: [Bool] = [], results: [Bool] = []
        var finishes: [(Bool) -> Void] = []
        let resets = InterfaceResetCoordinator(perform: { device, done in
            devices.append(device); finishes.append(done)
        }, busyChanged: { busy.append($0) })
        for _ in 0..<20 { resets.request(87) { results.append($0) } }
        resets.request(88) { results.append($0) }
        check(devices == [87] && busy == [true] && results.isEmpty, "overlapping reset escaped coalescing")
        finishes[0](false)
        check(devices == [87, 88] && busy == [true], "failure stranded a reset or cleared busy early")
        finishes[1](true)
        check(results == Array(repeating: false, count: 20) + [true] && busy == [true, false], "reset callbacks lost")
        print("PASS: reset coalescing, serialization, failure recovery, busy state")

        let tape = MenuBandTape()
        let format = AVAudioFormat(standardFormatWithSampleRate: 44_100, channels: 2)!
        let signal = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 44_100)!
        signal.frameLength = 44_100
        for ch in 0..<2 { signal.floatChannelData![ch].initialize(repeating: 0.25, count: 44_100) }
        tape.record(micAlreadyInMix: true, mix: ["mic": 0.5])
        check(tape.snapshotForExport() == nil, "live storage shared with export")
        for _ in 0..<91 { tape.ingestSynth(signal); tape.ingestTones(signal) }
        tape.stop()
        let handoffStart = ProcessInfo.processInfo.systemUptime
        let snapshot = tape.snapshotForExport()!
        let handoffMS = (ProcessInfo.processInfo.systemUptime - handoffStart) * 1000
        check(snapshot === tape.snapshotForExport(), "snapshot was not reused")
        check(snapshot.durationSeconds == 90, "long take lost frames")
        tape.record(micAlreadyInMix: false, mix: ["mic": 1])
        for ch in 0..<2 { signal.floatChannelData![ch].update(repeating: 0.75, count: 44_100) }
        tape.ingestSynth(signal)
        check(tape.takeID != snapshot.takeID && snapshot.micWasInMix && !tape.micWasInMix,
              "new take changed export identity or mic routing")

        var done = false, ticks = 0, maxGap = 0.0, lastTick = ProcessInfo.processInfo.systemUptime
        let timer = Timer.scheduledTimer(withTimeInterval: 0.01, repeats: true) { _ in
            let now = ProcessInfo.processInfo.systemUptime
            maxGap = max(maxGap, now - lastTick); lastTick = now; ticks += 1
        }
        MenuBandTape.exportQueue.async {
            let started = ProcessInfo.processInfo.systemUptime
            let take = snapshot.eject()!
            defer {
                try? FileManager.default.removeItem(at: take.file)
                if let stems = take.stems { try? FileManager.default.removeItem(at: stems) }
            }
            do {
                let file = try AVAudioFile(forReading: take.file)
                let head = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: 256)!
                try file.read(into: head)
                check(abs(head.floatChannelData![0][100] - 0.25) < 0.001, "next take overwrote frozen audio")
                let data = try Data(contentsOf: take.stems!.appendingPathComponent("mix.json"))
                let mix = try JSONSerialization.jsonObject(with: data) as! [String: Any]
                check((mix["start"] as? [String: Double])?["mic"] == 0.5, "next take overwrote frozen mix metadata")
                let elapsed = ProcessInfo.processInfo.systemUptime - started
                DispatchQueue.main.async {
                    check(tape.state == .recording, "old queued auto-stop stopped a new take")
                    tape.stop()
                    print(String(format: "PASS: 90-second frozen export; handoff %.3f ms; worker %.3f s; main ticks %d; max tick gap %.1f ms",
                                 handoffMS, elapsed, ticks, maxGap * 1000))
                    done = true
                }
            } catch { fatalError("export check failed: \(error)") }
        }
        let deadline = Date().addingTimeInterval(30)
        while !done && Date() < deadline { RunLoop.current.run(until: Date().addingTimeInterval(0.01)) }
        timer.invalidate()
        check(done && ticks > 0, "background export stalled main or never completed")

        // Fulfill a file promise after releasing the drag's original owner.
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: root) }
        let input = root.appendingPathComponent("source.wav"), output = root.appendingPathComponent("promised.wav")
        try Data("frozen take".utf8).write(to: input)
        var promise: TapeFilePromise? = TapeFilePromise(name: "promised.wav") { finish in finish(input) }
        let provider = promise!.provider(fileType: "com.microsoft.waveform-audio")
        weak var retained = promise
        promise = nil
        check(retained != nil, "promise delegate died before drop")
        var promised = false
        retained!.filePromiseProvider(provider, writePromiseTo: output) { error in
            check(error == nil, "file promise failed")
            DispatchQueue.main.async { promised = true }
        }
        let promiseDeadline = Date().addingTimeInterval(10)
        while !promised && Date() < promiseDeadline { RunLoop.current.run(until: Date().addingTimeInterval(0.01)) }
        check(promised, "file promise timed out")
        let copied = try Data(contentsOf: output), original = try Data(contentsOf: input)
        check(copied == original, "file promise changed contents")
        print("PASS: asynchronous file promise, delegate lifetime, copied contents")
    }
}
