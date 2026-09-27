import Foundation

/// A cartoon mouth chart: independent jaw, corners, lip seal, teeth and tongue.
/// The poses are key drawings; continuous interpolation supplies breakdowns.
enum SingerViseme: String, CaseIterable {
    case rest, closed, ah, eh, ee, oh, oo, fv, l, th, sz, sh, r, tdn, hum

    var pose: SingerMouthPose {
        switch self {
        case .rest:   return .init(width: 0.95, jaw: 0, seal: 1)
        case .closed: return .init(width: 0.90, jaw: 0.08, seal: 1, press: 1)
        case .hum:    return .init(width: 0.80, jaw: 0.06, seal: 1, press: 0.55)
        case .ah:     return .init(width: 1.15, jaw: 1.00, teeth: 0.35, tongue: 0.22)
        case .eh:     return .init(width: 1.27, jaw: 0.62, teeth: 0.65, tongue: 0.15)
        case .ee:     return .init(width: 1.48, jaw: 0.34, teeth: 0.95, tongue: 0.10)
        case .oh:     return .init(width: 0.74, jaw: 0.90, round: 0.8)
        case .oo:     return .init(width: 0.43, jaw: 0.43, round: 1)
        case .fv:     return .init(width: 1.12, jaw: 0.23, teeth: 1, bite: 1)
        case .l:      return .init(width: 1.04, jaw: 0.68, teeth: 0.45, tongue: 1)
        case .th:     return .init(width: 1.17, jaw: 0.29, teeth: 0.70, tongue: 0.8)
        case .sz:     return .init(width: 1.29, jaw: 0.19, teeth: 1)
        case .sh:     return .init(width: 0.71, jaw: 0.36, round: 0.7, teeth: 0.7)
        case .r:      return .init(width: 0.68, jaw: 0.48, round: 0.65, tongue: 0.2)
        case .tdn:    return .init(width: 1.07, jaw: 0.31, teeth: 0.65, tongue: 0.85)
        }
    }
}

struct SingerMouthPose {
    var width: Double
    var jaw: Double
    var seal: Double = 0
    var round: Double = 0
    var teeth: Double = 0
    var tongue: Double = 0
    var bite: Double = 0
    var press: Double = 0

    func blended(to b: Self, by raw: Double) -> Self {
        let u = min(1, max(0, raw))
        func mix(_ a: Double, _ b: Double) -> Double { a + (b - a) * u }
        return .init(width: mix(width,b.width), jaw: mix(jaw,b.jaw),
                     seal: mix(seal,b.seal), round: mix(round,b.round),
                     teeth: mix(teeth,b.teeth), tongue: mix(tongue,b.tongue),
                     bite: mix(bite,b.bite), press: mix(press,b.press))
    }
}

struct SingerArticulation {
    struct Unit {
        let syllable: String
        let start: Double, vowelStart: Double, vowelEnd: Double, end: Double
    }
    struct Cue {
        let start: Double, end: Double
        let shape: SingerViseme
    }
    let cues: [Cue]
    let energy: [Double]
    let duration: Double
    static let energyRate = 120.0
    static let anticipation = 0.040

    /// Reload a renderer's exported exposure sheet for visual regression QA.
    init(cues: [Cue], energy: [Double], duration: Double) {
        self.cues = cues; self.energy = energy; self.duration = duration
    }

    /// Text supplies sound identity; the singer's measured vowel warp supplies
    /// time. This lexicon deliberately handles our wordless syllables. General
    /// prose uses a spelling approximation, not a claimed phoneme recognizer.
    static func sounds(_ raw: String) -> (SingerViseme?, SingerViseme, SingerViseme?) {
        let s = raw.lowercased().filter { $0.isLetter }
        if s.isEmpty { return (nil, .rest, nil) }
        if s.allSatisfy({ $0 == "m" || $0 == "h" }) { return (nil, .hum, nil) }
        func consonant(_ s: String) -> SingerViseme? {
            if s.hasPrefix("th") { return .th }
            if s.hasPrefix("sh") || s.hasPrefix("ch") || s.hasPrefix("j") { return .sh }
            guard let c = s.first else { return nil }
            if "mbp".contains(c) { return .closed }
            if "fv".contains(c) { return .fv }
            if "tdn".contains(c) { return .tdn }
            if "sz".contains(c) { return .sz }
            if c == "l" { return .l }
            if c == "r" { return .r }
            if c == "w" || s.hasPrefix("qu") { return .oo }
            return nil
        }
        let vowels = "aeiouy"
        let onset = consonant(s)
        let vowel: SingerViseme
        if s.contains("oo") || s.contains("ou") || s.contains("ew") || s == "du" { vowel = .oo }
        else if s.contains("ee") || s.contains("ea") || s.contains("ie") || s.hasSuffix("i") { vowel = .ee }
        else if s.contains("aw") || s.contains("au") || s.contains("o") { vowel = .oh }
        else if s.contains("a") || s.contains("u") { vowel = .ah }
        else if s.contains("e") || s.contains("i") || s.contains("y") { vowel = .eh }
        else { vowel = onset ?? .rest }
        var tail = ""
        for c in s.reversed() { if vowels.contains(c) { break }; tail.insert(c, at: tail.startIndex) }
        // Terminal h in wah/ah/ooh is breath, not another mouth target.
        let coda = tail.isEmpty || tail == "h" ? nil : consonant(tail)
        return (onset, vowel, coda)
    }

    init(units: [Unit], samples: [Float], sampleRate: Double) {
        let duration = Double(samples.count) / sampleRate
        self.duration = duration
        var out: [Cue] = []
        func append(_ shape: SingerViseme, _ a: Double, _ b: Double) {
            let lo = max(0, min(duration, a)), hi = max(0, min(duration, b))
            if hi > lo { out.append(.init(start: lo, end: hi, shape: shape)) }
        }
        for unit in units {
            let (onset, vowel, coda) = Self.sounds(unit.syllable)
            if vowel == .hum { append(.hum, unit.start, unit.end); continue }
            if let onset { append(onset, min(unit.start, unit.vowelStart - 0.065), unit.vowelStart) }
            else { append(vowel, unit.start, unit.vowelStart) }
            let release = coda == nil ? unit.vowelEnd : max(unit.vowelStart, min(unit.vowelEnd, unit.end - 0.065))
            append(vowel, unit.vowelStart, release)
            append(coda ?? vowel, release, unit.end)
        }
        cues = out.sorted { $0.start < $1.start }
        let count = Int(ceil(duration * Self.energyRate))
        var rms = [Double](repeating: 0, count: count)
        for i in 0..<count {
            let a = Int(Double(i) / Self.energyRate * sampleRate)
            let b = min(samples.count, Int(Double(i + 1) / Self.energyRate * sampleRate))
            if b <= a { continue }
            var sum = 0.0
            for j in a..<b { sum += Double(samples[j]) * Double(samples[j]) }
            rms[i] = sqrt(sum / Double(b - a))
        }
        // Normalize articulation per phrase: a quiet backing singer still
        // enunciates. The actual samples retain the score's acoustic dynamics.
        let sorted = rms.sorted()
        let reference = max(0.015, sorted.isEmpty ? 0 : sorted[min(sorted.count - 1, Int(Double(sorted.count) * 0.85))])
        energy = rms.map { min(1, max(0, ($0 - 0.002) / reference)) }
    }

    func level(at time: Double) -> Double {
        guard time >= 0, time < duration, !energy.isEmpty else { return 0 }
        let f = time * Self.energyRate, a = min(energy.count - 1, Int(f))
        let b = min(energy.count - 1, a + 1)
        return energy[a] + (energy[b] - energy[a]) * (f - Double(a))
    }

    /// Shape changes anticipate the audio by one 24 fps frame. Transition
    /// widths are bounded so a short consonant still gets a decisive key pose.
    func pose(at time: Double) -> SingerMouthPose {
        let t = time + Self.anticipation
        guard let i = cues.lastIndex(where: { $0.start <= t && t < $0.end }) else {
            if let last = cues.last(where: { $0.end <= t }), t - last.end < 0.06 {
                let u = (t - last.end) / 0.06
                return last.shape.pose.blended(to: SingerViseme.rest.pose, by: u * u * (3 - 2 * u))
            }
            return SingerViseme.rest.pose
        }
        let cue = cues[i]
        var p = cue.shape.pose
        let transition = min(0.055, (cue.end - cue.start) * 0.3)
        if transition > 0, t < cue.start + transition {
            let prev = i > 0 && cue.start - cues[i-1].end < 0.025 ? cues[i-1].shape : .rest
            let u = (t - cue.start) / transition
            p = prev.pose.blended(to: p, by: u * u * (3 - 2 * u))
        }
        // Hold the vowel silhouette; energy adds restrained jaw motion rather
        // than collapsing every quiet syllable into a generic closed mouth.
        let e = level(at: max(0, time))
        if p.seal < 0.8 { p.jaw *= 0.72 + 0.28 * sqrt(e) }
        return p
    }

    func active(at time: Double) -> Bool {
        let t = time + Self.anticipation
        return cues.contains { $0.start <= t && t < $0.end + 0.06 }
    }
}
