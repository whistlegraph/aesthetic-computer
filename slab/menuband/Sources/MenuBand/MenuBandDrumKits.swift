import CoreGraphics
import Foundation
import QuartzCore

/// Drum-machine kits for the TrackDrum skin. Menu Band's own membrane stays
/// in MenuBandPercussion; a kit here is five recipes (layers of swept
/// oscillators, filtered noise and the 808 metal bank), one per concentric
/// zone of the membrane — kick, tom, snare, hat, click — blended across the
/// same contours so the trackpad plays the same way in every kit.
///
/// Every pad is loudness-matched at runtime: its recipe is rendered offline
/// through the same `nextSample` the audio thread uses, K-weighted
/// (ITU-R BS.1770), and trimmed so it lands on the loudness the Menu Band
/// membrane has for the same role. Switching kits changes the sound, not the
/// level.
extension MenuBandPercussion {

    // MARK: Recipe

    struct Layer {
        var wave: Wave
        var freq: Double
        var level: Double
        /// Exponential decay time constant (s); the voice lasts ~5·tau.
        var tau: Double
        var attack: Double = 0.0005
        /// Pitch starts `sweep` Hz above `freq` and falls with `sweepTau`.
        var sweep: Double = 0
        var sweepTau: Double = 0.02
        var filter: NoiseFilter = .lowpass
        var q: Double = 0.707
        var drive: Double = 0
        var delay: Double = 0
        var metalScale: Double = 1
    }

    enum Role { case kick, tom, snare, hat, click }

    struct KitPad {
        let name: String
        let role: Role
        /// Deliberate offset from the role's matched loudness, in dB.
        var trimDB: Double = 0
        let layers: [Layer]
    }

    /// Pads in zone order, center out: kick, tom, snare, hat, click.
    struct KitSpec {
        let pads: [KitPad]
    }

    static func spec(for kit: DrumKit) -> KitSpec? {
        switch kit {
        case .menuBand: return nil
        case .electro: return electro
        }
    }

    // MARK: Electro — 909 / 606, concentric rings (continuous morph)
    // 909 kick chirp (Tune = 30–120 ms), 909 snare tones at 180/330 Hz,
    // Simmons-style toms with a zap, 606 hat, 808 cowbell.

    private static let electro = KitSpec(pads: [
        KitPad(name: "909", role: .kick, layers: [
            // ~230 → 52 Hz chirp, a hard HP'd noise click, light drive.
            Layer(wave: .sine, freq: 52, level: 1.0, tau: 0.10, sweep: 180, sweepTau: 0.018, drive: 1.5),
            Layer(wave: .noise, freq: 3000, level: 0.45, tau: 0.0012, filter: .highpass, q: 0.7),
            Layer(wave: .square, freq: 1000, level: 0.12, tau: 0.0005),
        ]),
        KitPad(name: "SIMMONS", role: .tom, layers: [
            // Simmons tom: f0 × 2 → f0 over ~200 ms, plus a zap on top and
            // a 1.5 kHz stick.
            Layer(wave: .sine, freq: 125, level: 1.0, tau: 0.10, sweep: 125, sweepTau: 0.06),
            Layer(wave: .triangle, freq: 160, level: 0.25, tau: 0.018, sweep: 2600, sweepTau: 0.012),
            Layer(wave: .noise, freq: 1500, level: 0.25, tau: 0.003, filter: .bandpass, q: 1.2),
        ]),
        KitPad(name: "SNARE", role: .snare, layers: [
            // 909 snare: 180/330 Hz tones dropping ~15%, noise band 1–8 kHz
            // longer and brighter than the 808's, and a clap flam on top.
            Layer(wave: .sine, freq: 180, level: 0.55, tau: 0.03, sweep: 30, sweepTau: 0.015),
            Layer(wave: .sine, freq: 330, level: 0.30, tau: 0.018, sweep: 50, sweepTau: 0.015),
            Layer(wave: .noise, freq: 3500, level: 0.85, tau: 0.05, filter: .bandpass, q: 0.35),
            Layer(wave: .noise, freq: 1200, level: 0.55, tau: 0.003, filter: .bandpass, q: 2.0),
            Layer(wave: .noise, freq: 1200, level: 0.55, tau: 0.003, filter: .bandpass, q: 2.0, delay: 0.009),
            Layer(wave: .noise, freq: 1200, level: 0.55, tau: 0.003, filter: .bandpass, q: 2.0, delay: 0.018),
        ]),
        KitPad(name: "606", role: .hat, layers: [
            // 606 hat: the metal bank pitched up, high-passed at 8 kHz, tight.
            Layer(wave: .metal, freq: 8000, level: 1.0, tau: 0.008, filter: .highpass, q: 0.8, metalScale: 1.5),
            Layer(wave: .noise, freq: 10_000, level: 0.2, tau: 0.005, filter: .highpass),
        ]),
        KitPad(name: "BELL", role: .click, layers: [
            // 808 cowbell: 540 + 800 Hz squares through a 2.3 kHz band-pass,
            // a fast 15 ms drop then a longer tail.
            Layer(wave: .square, freq: 540, level: 0.9, tau: 0.005, filter: .bandpass, q: 1.5),
            Layer(wave: .square, freq: 800, level: 0.9, tau: 0.005, filter: .bandpass, q: 1.5),
            Layer(wave: .square, freq: 540, level: 0.35, tau: 0.07, filter: .bandpass, q: 1.5),
            Layer(wave: .square, freq: 800, level: 0.35, tau: 0.07, filter: .bandpass, q: 1.5),
        ]),
    ])

    // MARK: Playing a kit

    /// Zone weights for kick, tom, snare, hat, click — the membrane's own
    /// contours, so a kit blends exactly where the default does.
    func ringWeights(at point: CGPoint) -> [Double] {
        let sx = Double(point.x - 0.5) * 2
        let sy = Double(point.y - 0.5) * 2
        let r = Self.roundedTrackpadDistance(sx: sx, sy: sy)
        let edge = smoothstep(0.62, 0.70, r)
        let outerClick = smoothstep(0.88, 0.965, r)
        return [
            1.0 - smoothstep(0.23, 0.31, r),
            smoothstep(0.23, 0.31, r) * (1.0 - smoothstep(0.40, 0.48, r)),
            smoothstep(0.40, 0.48, r) * (1.0 - edge),
            edge * (1.0 - outerClick),
            outerClick,
        ]
    }

    /// Build a pad's voices. `tension` raises tonal pitch (resting fingers
    /// tighten the head) and `damping` shortens decays.
    func padVoices(_ pad: KitPad, gain: Double, pan: Double,
                   tension: Double = 1, damping: Double = 1,
                   jitter: Bool = true) -> [Voice] {
        pad.layers.map { layer in
            let tonal = layer.wave != .noise && layer.wave != .metal
            let tune = tonal ? tension : 1
            let tau = layer.tau * damping
            let length = layer.attack + tau * 5.5
            let level = layer.level * gain * (jitter ? rj(1, 0.06) : 1)
            var voice = makeVoice(layer.wave, layer.freq * tune, length, level,
                                  layer.attack, min(0.012, length * 0.2),
                                  pan + (jitter ? rn(-0.02, 0.02) : 0))
            voice.tau = tau
            voice.sweep = layer.sweep * (tonal ? tension : 1)
            voice.sweepTau = layer.sweepTau
            voice.drive = layer.drive
            voice.delay = layer.delay
            voice.metalScale = layer.metalScale
            voice.filter = layer.filter
            voice.q = layer.q
            if layer.wave == .noise || layer.wave == .metal {
                if jitter { voice.freq *= rj(1, 0.04) }
                setupNoiseFilter(&voice)
            } else if layer.filter == .bandpass {
                // Tonal waves take the filter only when asked: the 808 bell's
                // shared ~2.3 kHz band-pass over its 540/800 Hz squares.
                voice.filtered = true
                var shaped = voice
                shaped.freq = 2_300
                setupNoiseFilter(&shaped)
                voice.nb0 = shaped.nb0; voice.nb1 = shaped.nb1; voice.nb2 = shaped.nb2
                voice.na1 = shaped.na1; voice.na2 = shaped.na2
            }
            return voice
        }
    }

    func playKit(_ kit: DrumKit, strike: CGPoint, anchors: [CGPoint],
                 velocity: UInt8) {
        guard let spec = Self.spec(for: kit) else { return }
        let gains = padGains(for: kit)
        let v = max(0.1, min(2.2, Double(velocity) / 100.0))
        let sx = Double(strike.x - 0.5) * 2
        let sy = Double(strike.y - 0.5) * 2
        let pan = stereo(sx, sy, 0.72)
        let tension = 1.0 + min(0.30, Double(anchors.count) * 0.07)
        let damping = 1.0 - min(0.55, Double(anchors.count) * 0.12)
        var voices: [Voice] = []
        let weights = ringWeights(at: strike)
        for (index, weight) in weights.enumerated()
        where weight > 0.02 && index < spec.pads.count {
            voices += padVoices(spec.pads[index], gain: gains[index] * v * weight,
                                pan: pan, tension: tension, damping: damping)
        }
        let top = weights.indices.max { weights[$0] < weights[$1] } ?? 0

        let pulseDrum: Drum
        switch spec.pads[min(top, spec.pads.count - 1)].role {
        case .kick: pulseDrum = .kick
        case .tom: pulseDrum = .block
        case .snare: pulseDrum = .snare
        case .hat: pulseDrum = .hatClosed
        case .click: pulseDrum = .cowbell
        }
        let now = CACurrentMediaTime()
        lock.lock()
        pending.append(contentsOf: voices)
        pulses[pulseDrum.rawValue] = DrumPulse(
            at: now, level: min(1.0, Double(velocity) / 127.0)
        )
        pendingStageTime = now
        lock.unlock()
    }

    // MARK: Loudness matching

    /// Per-pad linear gains that put every pad on its role's reference
    /// loudness. Computed once per kit per sample rate, off the audio thread.
    func padGains(for kit: DrumKit) -> [Double] {
        guard let spec = Self.spec(for: kit) else { return [] }
        Self.gainLock.lock()
        defer { Self.gainLock.unlock() }
        let key = "\(kit.rawValue)@\(Int(sampleRate))"
        if let cached = Self.gainCache[key] { return cached }
        let targets = roleTargets()
        let gains = spec.pads.map { pad -> Double in
            let measured = averagedLoudness(of: pad)
            let target = targets[pad.role] ?? -20
            return pow(10, (target + pad.trimDB - measured) / 20)
        }
        Self.gainCache[key] = gains
        return gains
    }

    /// Noise layers differ render to render; average the energy of a few so
    /// a pad's trim doesn't inherit one lucky or unlucky seed.
    func averagedLoudness(of pad: KitPad, renders: Int = 8) -> Double {
        var energy = 0.0
        for render in 0..<renders {
            let voices = Self.seeded(padVoices(pad, gain: 1, pan: 0, jitter: false), render)
            energy += pow(10, loudness(of: voices) / 10)
        }
        return 10 * log10(energy / Double(renders))
    }

    /// Fixed noise seeds, so a measurement is reproducible.
    static func seeded(_ voices: [Voice], _ render: Int) -> [Voice] {
        voices.enumerated().map { index, voice in
            var copy = voice
            copy.seed = UInt32(truncatingIfNeeded: 0x9E37_79B9 &* (render * 64 + index + 1))
            if copy.seed == 0 { copy.seed = 1 }
            return copy
        }
    }

    private static let gainLock = NSLock()
    private static var gainCache: [String: [Double]] = [:]

    /// Reference loudness per role: the Menu Band membrane struck at the
    /// middle of each zone at velocity 100.
    func roleTargets() -> [Role: Double] {
        let probes: [(Role, CGFloat)] = [
            (.kick, 0.0), (.tom, 0.35), (.snare, 0.55), (.hat, 0.78), (.click, 0.94),
        ]
        var targets: [Role: Double] = [:]
        for (role, radius) in probes {
            // roundedTrackpadDistance ≈ |sy| near the vertical center line.
            let point = CGPoint(x: 0.5, y: 0.5 + radius / 2)
            var energy = 0.0
            for render in 0..<8 {
                let voices = Self.seeded(
                    membraneVoices(strike: point, anchors: [], velocity: 100).0, render)
                energy += pow(10, loudness(of: voices) / 10)
            }
            targets[role] = 10 * log10(energy / 8)
        }
        return targets
    }

    /// K-weighted loudness (dB, relative) of a set of voices over the 400 ms
    /// after onset — BS.1770's momentary window, which suits one drum hit.
    func loudness(of voices: [Voice]) -> Double {
        let rate = sampleRate
        let frames = Int(rate * 0.4)
        var buffer = [Double](repeating: 0, count: frames)
        let dt = 1.0 / rate
        for var voice in voices {
            for i in 0..<frames {
                if voice.elapsed >= voice.duration + voice.delay { break }
                buffer[i] += Self.nextSample(&voice, pitch: 1, dt: dt)
                    * Double(voice.gainL + voice.gainR) * 0.5
            }
        }
        var shelf = Biquad.kShelf(rate)
        var highpass = Biquad.kHighpass(rate)
        var energy = 0.0
        for sample in buffer {
            let y = highpass.process(shelf.process(sample))
            energy += y * y
        }
        return 10 * log10(max(1e-12, energy / Double(frames)))
    }

    /// BS.1770 K-weighting stages (pyloudnorm's parameterization, so they
    /// hold at 44.1k and 96k as well as 48k).
    struct Biquad {
        var b0 = 1.0, b1 = 0.0, b2 = 0.0, a1 = 0.0, a2 = 0.0
        var x1 = 0.0, x2 = 0.0, y1 = 0.0, y2 = 0.0

        mutating func process(_ x: Double) -> Double {
            let y = b0 * x + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
            x2 = x1; x1 = x; y2 = y1; y1 = y
            return y
        }

        static func kShelf(_ rate: Double) -> Biquad {
            let gain = 3.99984385397, q = 0.7071752369554193, fc = 1681.9744509555319
            let a = pow(10, gain / 40), w = 2 * .pi * fc / rate
            let alpha = sin(w) / (2 * q), c = cos(w), sa = 2 * sqrt(a) * alpha
            let a0 = (a + 1) - (a - 1) * c + sa
            return Biquad(b0: a * ((a + 1) + (a - 1) * c + sa) / a0,
                          b1: -2 * a * ((a - 1) + (a + 1) * c) / a0,
                          b2: a * ((a + 1) + (a - 1) * c - sa) / a0,
                          a1: 2 * ((a - 1) - (a + 1) * c) / a0,
                          a2: ((a + 1) - (a - 1) * c - sa) / a0)
        }

        static func kHighpass(_ rate: Double) -> Biquad {
            let q = 0.5003270373253953, fc = 38.13547087613982
            let w = 2 * .pi * fc / rate
            let alpha = sin(w) / (2 * q), c = cos(w), a0 = 1 + alpha
            return Biquad(b0: (1 + c) / 2 / a0, b1: -(1 + c) / a0,
                          b2: (1 + c) / 2 / a0,
                          a1: -2 * c / a0, a2: (1 - alpha) / a0)
        }
    }
}
