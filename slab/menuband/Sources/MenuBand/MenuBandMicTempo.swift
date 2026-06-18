import Foundation
import AVFoundation
import CoreAudio

/// Listens to the room and (a) estimates tempo and (b) detects **kick onsets**
/// in real time so a follower can hit *on* the song's beats, not just at its
/// tempo.
///
/// Onset path: the mic is low-passed (~150 Hz) to emphasize the kick, energy
/// flux gives an onset-novelty curve, and an adaptive threshold + refractory
/// window fire `onOnset` the instant a kick-like transient arrives. A small
/// hardware IO buffer keeps latency low. Tempo path: the same novelty ring is
/// autocorrelated (70–160 BPM) for `onTempo`.
final class MenuBandMicTempo {
    var onTempo: ((Double) -> Void)?      // smoothed BPM (main thread)
    var onOnset: (() -> Void)?            // a kick-like onset just happened (main thread)
    var onBeat: (() -> Void)?             // a phase-locked beat tick (main thread)

    // Phase-locked beat clock: a steady pulse at the detected period whose
    // phase is nudged toward detected onsets. Emits onBeat for an even,
    // consistent kick that rides the song instead of chasing raw transients.
    private var period = 0.0
    private var lastBeat = 0.0
    private var clockRunning = false
    private let latencyComp = 0.02        // s, push beats early to offset input lag

    private let aue = AVAudioEngine()
    private(set) var running = false
    private var sr = 44100.0
    private let hop = 256                  // novelty frame (~6 ms @ 44.1k)

    // kick-band emphasis + flux
    private var lpf = 0.0
    private var lpAlpha = 0.02
    private var prevLogE = -12.0
    private var carry: [Float] = []

    // adaptive onset detector
    private var fluxMean = 0.0
    private var fluxVar = 0.0
    private var lastOnset = 0.0
    private let refractory = 0.11          // s — no two kicks closer than this

    // tempo ring + autocorrelation
    private let cap = 1600
    private var novelty: [Float]
    private var head = 0, filled = 0
    private let lock = NSLock()
    private var analyzeTimer: Timer?
    private var recent: [Double] = []
    private var lastReported = 0.0
    private let bpmMin = 70.0, bpmMax = 160.0

    init() { novelty = [Float](repeating: 0, count: cap) }

    func start() {
        guard !running else { return }
        let input = aue.inputNode
        setSmallIOBuffer()                 // ask CoreAudio for low latency
        let fmt = input.inputFormat(forBus: 0)
        sr = fmt.sampleRate > 0 ? fmt.sampleRate : 44100
        lpAlpha = 1 - exp(-2 * Double.pi * 90 / sr)   // ~90 Hz: kick fundamental, reject snare
        input.installTap(onBus: 0, bufferSize: 256, format: fmt) { [weak self] b, _ in
            self?.process(b)
        }
        do {
            try aue.start()
            running = true
            NSLog("🎧 mic: listening sr=\(Int(sr)) hop=\(hop)")
        } catch {
            running = false
            input.removeTap(onBus: 0)
            NSLog("🎧 mic: FAILED to start — \(error.localizedDescription)")
            return
        }
        let t = Timer(timeInterval: 0.7, repeats: true) { [weak self] _ in self?.analyze() }
        RunLoop.main.add(t, forMode: .common)
        analyzeTimer = t
    }

    func stop() {
        guard running else { return }
        aue.inputNode.removeTap(onBus: 0)
        aue.stop()
        analyzeTimer?.invalidate(); analyzeTimer = nil
        running = false
        clockRunning = false; period = 0
        lock.lock(); novelty = [Float](repeating: 0, count: cap); head = 0; filled = 0; lock.unlock()
        recent.removeAll(); carry.removeAll(); prevLogE = -12; fluxMean = 0; fluxVar = 0
    }

    /// Best-effort: shrink the input device IO buffer so the tap fires sooner.
    private func setSmallIOBuffer() {
        var dev = AudioDeviceID(0)
        var size = UInt32(MemoryLayout<AudioDeviceID>.size)
        var addr = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDefaultInputDevice,
            mScope: kAudioObjectPropertyScopeGlobal, mElement: kAudioObjectPropertyElementMain)
        guard AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &addr, 0, nil, &size, &dev) == noErr else { return }
        var frames = UInt32(256)
        var faddr = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyBufferFrameSize,
            mScope: kAudioObjectPropertyScopeGlobal, mElement: kAudioObjectPropertyElementMain)
        AudioObjectSetPropertyData(dev, &faddr, 0, nil, UInt32(MemoryLayout<UInt32>.size), &frames)
    }

    // MARK: - Phase-locked beat clock
    private func startClockIfNeeded() {
        guard onBeat != nil, !clockRunning, period > 0 else { return }
        clockRunning = true
        lastBeat = CACurrentMediaTime()
        NSLog("🎧 beat clock: start @ \(Int(60/period)) bpm")
        scheduleNextBeat()
    }

    private func scheduleNextBeat() {
        guard clockRunning, period > 0 else { return }
        let now = CACurrentMediaTime()
        var target = lastBeat + period
        if target <= now { target = now + 0.005; lastBeat = target - period }
        DispatchQueue.main.asyncAfter(deadline: .now() + (target - now)) { [weak self] in
            guard let self = self, self.clockRunning else { return }
            self.lastBeat = target
            self.onBeat?()
            self.scheduleNextBeat()
        }
    }

    /// PLL: pull the beat grid toward an onset, but only if the onset lands
    /// near an expected beat (so off-beat snares/claps are ignored).
    private func correctPhase(at onsetTime: Double) {
        guard clockRunning, period > 0 else { return }
        let t = onsetTime - latencyComp
        let k = ((t - lastBeat) / period).rounded()
        let err = t - (lastBeat + k * period)
        if abs(err) < period * 0.28 {
            lastBeat += 0.15 * err
        }
    }

    private func process(_ b: AVAudioPCMBuffer) {
        guard let ch = b.floatChannelData?[0] else { return }
        carry.append(contentsOf: UnsafeBufferPointer(start: ch, count: Int(b.frameLength)))
        var off = 0
        while carry.count - off >= hop {
            var sum = 0.0
            for i in 0..<hop {
                let s = Double(carry[off + i])
                lpf += lpAlpha * (s - lpf)      // kick-band low-pass
                sum += lpf * lpf
            }
            let energy = sum / Double(hop)
            let logE = log(energy + 1e-9)
            let flux = max(0.0, logE - prevLogE)
            prevLogE = logE

            lock.lock()
            novelty[head] = Float(flux); head = (head + 1) % cap; filled = min(filled + 1, cap)
            lock.unlock()

            // adaptive onset: flux well above its running mean+std, debounced.
            let d = flux - fluxMean
            fluxMean += 0.04 * d
            fluxVar = 0.96 * (fluxVar + 0.04 * d * d)
            let std = (max(fluxVar, 1e-12)).squareRoot()
            let now = CACurrentMediaTime()
            if flux > fluxMean + 1.7 * std, flux > 0.015, now - lastOnset > refractory {
                lastOnset = now
                let t = now
                DispatchQueue.main.async { [weak self] in
                    self?.correctPhase(at: t)     // nudge the beat clock toward this onset
                    self?.onOnset?()
                }
            }
            off += hop
        }
        if off > 0 { carry.removeFirst(off) }
    }

    private func analyze() {
        lock.lock()
        let count = filled
        var nov = [Float](repeating: 0, count: count)
        for i in 0..<count { nov[i] = novelty[(head - count + i + cap * 2) % cap] }
        lock.unlock()
        guard count > 256 else { return }
        let mean = nov.reduce(0, +) / Float(count)
        for i in 0..<count { nov[i] -= mean }
        let noveltyRate = sr / Double(hop)
        let lagMin = Int(noveltyRate * 60.0 / bpmMax)
        let lagMax = Int(noveltyRate * 60.0 / bpmMin)
        guard lagMax < count, lagMin >= 1 else { return }
        var bestLag = lagMin, bestCorr = -Double.greatestFiniteMagnitude
        for lag in lagMin...lagMax {
            var c = 0.0, i = 0
            while i + lag < count { c += Double(nov[i]) * Double(nov[i + lag]); i += 1 }
            let bpm = 60.0 * noveltyRate / Double(lag)
            c *= 0.5 + 0.5 * exp(-pow((bpm - 120.0) / 90.0, 2))
            if c > bestCorr { bestCorr = c; bestLag = lag }
        }
        guard bestCorr > 0 else { return }
        var bpm = 60.0 * noveltyRate / Double(bestLag)
        while bpm < bpmMin { bpm *= 2 }
        while bpm > bpmMax { bpm /= 2 }
        recent.append(bpm); if recent.count > 5 { recent.removeFirst() }
        let sorted = recent.sorted(); let med = sorted[sorted.count / 2]
        let spread = (sorted.last ?? med) - (sorted.first ?? med)
        if recent.count >= 3, spread < 8 {
            period = 60.0 / med                         // drive the beat clock
            DispatchQueue.main.async { [weak self] in self?.startClockIfNeeded() }
            if abs(med - lastReported) >= 2 {
                lastReported = med
                let out = (med * 10).rounded() / 10
                NSLog("🎧 mic tempo: \(Int(out)) bpm")
                DispatchQueue.main.async { [weak self] in self?.onTempo?(out) }
            }
        }
    }
}
