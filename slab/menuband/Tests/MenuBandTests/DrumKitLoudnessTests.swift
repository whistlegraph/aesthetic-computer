import XCTest
@testable import MenuBand

/// Every genre-kit pad should land on its role's K-weighted loudness (the
/// Menu Band membrane's), so arrowing between kits never jumps in level.
/// Also prints a table — loudness, peak, brightness, decay — for tuning.
final class DrumKitLoudnessTests: XCTestCase {
    private typealias P = MenuBandPercussion

    private func render(_ voices: [P.Voice], seconds: Double = 1.5) -> [Double] {
        let rate = 48_000.0
        var out = [Double](repeating: 0, count: Int(rate * seconds))
        for var v in voices {
            for i in out.indices {
                if v.elapsed >= v.duration + v.delay { break }
                out[i] += P.nextSample(&v, pitch: 1, dt: 1 / rate)
                    * Double(v.gainL + v.gainR) * 0.5
            }
        }
        return out
    }

    /// Zero-crossings per second: a cheap brightness proxy.
    private func brightness(_ x: [Double]) -> Double {
        var crossings = 0
        let n = min(x.count, 4_800)   // first 100 ms
        for i in 1..<n where (x[i - 1] < 0) != (x[i] < 0) { crossings += 1 }
        return Double(crossings) / (Double(n) / 48_000) / 2
    }

    /// Time (ms) until the 5 ms RMS falls 20 dB under its maximum.
    private func decayMs(_ x: [Double]) -> Double {
        let hop = 240
        var rms: [Double] = []
        var i = 0
        while i + hop <= x.count {
            let slice = x[i..<(i + hop)]
            rms.append(sqrt(slice.reduce(0) { $0 + $1 * $1 } / Double(hop)))
            i += hop
        }
        guard let peak = rms.max(), peak > 0,
              let top = rms.firstIndex(of: peak) else { return 0 }
        let floor = peak * 0.1
        let end = rms[top...].firstIndex { $0 < floor } ?? rms.count
        return Double(end) * 5
    }

    func testEveryPadMatchesItsRoleLoudness() {
        let perc = P()
        let targets = perc.roleTargets()
        var report = "\nkit       pad      role   target  matched  peak   Hz≈   decay\n"
        for kit in P.DrumKit.allCases {
            guard let spec = P.spec(for: kit) else { continue }
            let gains = perc.padGains(for: kit)
            for (index, pad) in spec.pads.enumerated() {
                let voices = perc.padVoices(pad, gain: gains[index], pan: 0, jitter: false)
                let matched = perc.averagedLoudness(of: pad) + 20 * log10(gains[index])
                let target = targets[pad.role]! + pad.trimDB
                let audio = render(voices)
                let peak = audio.map(abs).max() ?? 0
                report += String(format: "%-9@ %-8@ %-6@ %6.1f  %6.1f  %5.2f %6.0f %5.0fms\n",
                                  kit.label as NSString, pad.name as NSString,
                                  "\(pad.role)" as NSString, target, matched, peak,
                                  brightness(audio), decayMs(audio))
                XCTAssertEqual(matched, target, accuracy: 0.1,
                               "\(kit.label) \(pad.name) is off its role loudness")
            }
        }
        report += "\nMenu Band membrane references:\n"
        for (role, target) in targets.sorted(by: { $0.value > $1.value }) {
            report += String(format: "  %-6@ %6.1f\n", "\(role)" as NSString, target)
        }
        print(report)
    }
}
