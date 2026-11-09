import Foundation
import QuartzCore
import CRealtimeAudio

struct PerformanceInputTiming {
    enum Source: UInt32, CaseIterable {
        case keyboardTap = 1, trackpad = 2, keyboardAppKit = 3
        var label: String {
            switch self {
            case .keyboardTap: return "keyboard_tap"
            case .keyboardAppKit: return "keyboard_appkit"
            case .trackpad: return "trackpad_callback"
            }
        }
    }
    let source: Source
    let event, captured: Double
    var queued: Double = 0
}

/// Explicit, bounded, local-only summaries. The output-render callback only
/// transfers fixed C records; a utility timer computes percentiles and writes.
final class InputLatencyProbe {
    static let preferenceKey = "MBInputLatencyDiagnosticsEnabled"
    static let shared = InputLatencyProbe()
    private static let eventKey = "MenuBand.inputTiming"
    let enabled: Bool
    private let probe: OpaquePointer
    private let producerLock = NSLock()
    private var timer: DispatchSourceTimer?
    private var loggedStart = false
    private let log = AudioDiagnosticLog(directory: FileManager.default.homeDirectoryForCurrentUser
        .appendingPathComponent("Library/Logs/MenuBand"), maximumBytes: 2 * 1024 * 1024,
        name: "input-latency")

    private init() {
        enabled = UserDefaults.standard.bool(forKey: Self.preferenceKey)
        probe = mb_latency_create()!
        guard enabled else { return }
        let timer = DispatchSource.makeTimerSource(queue:
            DispatchQueue(label: "menuband.input-latency", qos: .utility))
        timer.schedule(deadline: .now() + 1, repeating: 1, leeway: .milliseconds(100))
        timer.setEventHandler { [weak self] in self?.writeSummary() }
        self.timer = timer
        timer.resume()
    }

    static var capturedEvent: PerformanceInputTiming? {
        guard shared.enabled else { return nil }
        return Thread.current.threadDictionary[eventKey] as? PerformanceInputTiming
    }

    static func withEvent<T>(at timestamp: Double, down: Bool, repeated: Bool,
                             source: PerformanceInputTiming.Source = .keyboardAppKit,
                             _ work: () -> T) -> T {
        guard shared.enabled, down, !repeated, timestamp > 0 else { return work() }
        let dictionary = Thread.current.threadDictionary
        let previous = dictionary[eventKey]
        dictionary[eventKey] = PerformanceInputTiming(source: source,
            event: timestamp, captured: CACurrentMediaTime())
        defer { dictionary[eventKey] = previous }
        return work()
    }

    func stage(_ input: PerformanceInputTiming) {
        guard enabled else { return }
        let sample = MBLatencySample(source: input.source.rawValue, event: input.event,
            captured: input.captured, queued: input.queued, staged: CACurrentMediaTime(), rendered: 0)
        producerLock.lock()
        _ = mb_latency_stage(probe, sample)
        producerLock.unlock()
    }

    // Render thread: fixed-capacity C handoff; no locks, dispatch, file IO or arrays.
    func beginRender() { if enabled { mb_latency_begin_render(probe) } }
    func endRender(valid: Bool) {
        if enabled { mb_latency_end_render(probe, CACurrentMediaTime(), valid) }
    }

    private func writeSummary() {
        if !loggedStart {
            try? log.append(["schema": 2, "event": "session_started",
                "boundary": "next_output_block_after_submission", "includesSpeakerLatency": false])
            loggedStart = true
        }
        var samples: [MBLatencySample] = []
        var sample = MBLatencySample()
        while mb_latency_pop(probe, &sample) { samples.append(sample) }
        let drops = mb_latency_take_drops(probe)
        guard !samples.isEmpty || drops > 0 else { return }
        func distribution(_ values: [Double]) -> [String: Double] {
            let sorted = values.sorted()
            guard !sorted.isEmpty else { return [:] }
            func percentile(_ p: Double) -> Double {
                sorted[min(sorted.count - 1, Int(ceil(Double(sorted.count) * p)) - 1)] * 1000
            }
            return ["p50": percentile(0.50), "p95": percentile(0.95), "max": sorted.last! * 1000]
        }
        var groups: [[String: Any]] = []
        for source in PerformanceInputTiming.Source.allCases {
            let rows = samples.filter { $0.source == source.rawValue }
            guard !rows.isEmpty else { continue }
            groups.append([
                "source": source.label,
                "n": rows.count,
                "captureMs": distribution(rows.map { $0.captured - $0.event }),
                "queueMs": distribution(rows.map { $0.queued - $0.captured }),
                "controlMs": distribution(rows.map { $0.staged - $0.queued }),
                "nextRenderMs": distribution(rows.map { $0.rendered - $0.staged }),
                "inputToNextRenderMs": distribution(rows.map { $0.rendered - $0.event })
            ])
        }
        try? log.append(["schema": 2, "event": "input_latency", "groups": groups,
            "droppedMeasurements": drops,
            "boundary": "next_output_block_after_submission",
            "includesSpeakerLatency": false])
    }

    deinit { timer?.cancel(); mb_latency_destroy(probe) }
}
