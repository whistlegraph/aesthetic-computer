import Foundation
import AVFoundation

/// Listens to the microphone and estimates the tempo (BPM) of music playing
/// in the room, then reports a smoothed, stable estimate via `onTempo`.
///
/// Pipeline: a mic tap feeds short frames; per hop we take a half-wave
/// rectified log-energy *flux* (a cheap onset-novelty curve — kicks/snares
/// spike it). The novelty ring is autocorrelated over lags spanning
/// 70–160 BPM; the strongest lag (gently biased toward mid-tempo to fight
/// octave errors) is the beat period. Estimates are median-smoothed and only
/// reported when stable and meaningfully changed, so the engine's loop
/// follows the room without jittering.
///
/// Uses its own AVAudioEngine input tap — independent of the sampler's — and
/// relies on menuband's existing microphone permission.
final class MenuBandMicTempo {
    /// Called on the main thread with a confident, smoothed BPM.
    var onTempo: ((Double) -> Void)?

    private let aue = AVAudioEngine()
    private(set) var running = false

    private var sr: Double = 48000
    private let hop = 512
    private var prevLogE: Double = -10

    private let cap = 1100                 // ~12 s of novelty @ ~94 Hz
    private var novelty: [Float]
    private var head = 0
    private var filled = 0
    private let lock = NSLock()
    private var carry: [Float] = []        // samples not yet grouped into a hop

    private var analyzeTimer: Timer?
    private var recent: [Double] = []
    private var lastReported: Double = 0

    private let bpmMin = 70.0
    private let bpmMax = 160.0

    init() { novelty = [Float](repeating: 0, count: cap) }

    func start() {
        guard !running else { return }
        let input = aue.inputNode
        let fmt = input.inputFormat(forBus: 0)
        sr = fmt.sampleRate > 0 ? fmt.sampleRate : 48000
        input.installTap(onBus: 0, bufferSize: 4096, format: fmt) { [weak self] buf, _ in
            self?.process(buf)
        }
        do {
            try aue.start()
            running = true
        } catch {
            running = false
            input.removeTap(onBus: 0)
            return
        }
        let t = Timer(timeInterval: 0.75, repeats: true) { [weak self] _ in self?.analyze() }
        RunLoop.main.add(t, forMode: .common)
        analyzeTimer = t
    }

    func stop() {
        guard running else { return }
        aue.inputNode.removeTap(onBus: 0)
        aue.stop()
        analyzeTimer?.invalidate(); analyzeTimer = nil
        running = false
        lock.lock()
        novelty = [Float](repeating: 0, count: cap); head = 0; filled = 0
        lock.unlock()
        recent.removeAll(); carry.removeAll(); prevLogE = -10; lastReported = 0
    }

    private func process(_ buf: AVAudioPCMBuffer) {
        guard let ch = buf.floatChannelData?[0] else { return }
        let n = Int(buf.frameLength)
        carry.append(contentsOf: UnsafeBufferPointer(start: ch, count: n))
        var offset = 0
        while carry.count - offset >= hop {
            var sum: Float = 0
            for i in 0..<hop { let s = carry[offset + i]; sum += s * s }
            let rms = (sum / Float(hop)).squareRoot()
            let logE = Double(log(Double(rms) + 1e-6))
            let flux = max(0, logE - prevLogE)        // onset novelty
            prevLogE = logE
            lock.lock()
            novelty[head] = Float(flux)
            head = (head + 1) % cap
            filled = min(filled + 1, cap)
            lock.unlock()
            offset += hop
        }
        if offset > 0 { carry.removeFirst(offset) }
    }

    private func analyze() {
        lock.lock()
        let count = filled
        var nov = [Float](repeating: 0, count: count)
        for i in 0..<count { nov[i] = novelty[(head - count + i + cap * 2) % cap] }
        lock.unlock()
        guard count > 256 else { return }            // need a few seconds first

        let mean = nov.reduce(0, +) / Float(count)
        for i in 0..<count { nov[i] -= mean }

        let noveltyRate = sr / Double(hop)
        let lagMin = Int(noveltyRate * 60.0 / bpmMax)
        let lagMax = Int(noveltyRate * 60.0 / bpmMin)
        guard lagMax < count, lagMin >= 1 else { return }

        var bestLag = lagMin
        var bestCorr = -Double.greatestFiniteMagnitude
        for lag in lagMin...lagMax {
            var c = 0.0
            var i = 0
            while i + lag < count { c += Double(nov[i]) * Double(nov[i + lag]); i += 1 }
            let bpm = 60.0 * noveltyRate / Double(lag)
            let pref = exp(-pow((bpm - 120.0) / 90.0, 2))   // mild mid-tempo bias
            c *= (0.5 + 0.5 * pref)
            if c > bestCorr { bestCorr = c; bestLag = lag }
        }
        guard bestCorr > 0 else { return }               // no periodicity worth trusting

        var bpm = 60.0 * noveltyRate / Double(bestLag)
        while bpm < bpmMin { bpm *= 2 }
        while bpm > bpmMax { bpm /= 2 }

        recent.append(bpm)
        if recent.count > 5 { recent.removeFirst() }
        let sorted = recent.sorted()
        let med = sorted[sorted.count / 2]
        let spread = (sorted.last ?? med) - (sorted.first ?? med)
        if recent.count >= 3, spread < 6, abs(med - lastReported) >= 2 {
            lastReported = med
            let out = (med * 10).rounded() / 10
            DispatchQueue.main.async { [weak self] in self?.onTempo?(out) }
        }
    }
}
