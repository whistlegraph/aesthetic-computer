import Foundation

/// Continuous listening noise, independent of the pitched ⇧Space instrument.
/// All three colors stay warm so switching blends rather than restarting.
enum MenuBandAirColor: String, CaseIterable {
    case cabin, brown, white
    var title: String { rawValue.capitalized }
}

struct MenuBandAirNoise {
    private var seed: UInt64 = 0x91E10DA5C79E7B1D
    private var brown: Float = 0
    private var low: Float = 0
    private var mid: Float = 0
    private var dc: Float = 0
    private var whiteWeight: Float = 0
    private var brownWeight: Float = 0
    private var cabinWeight: Float = 1
    private var phase: Double = 0
    private var filterA: Float = 0
    private var filterB: Float = 0
    private var filterCoefficient: Float = 0
    private var targetCoefficient: Float = 0
    private var coefficientStep: Float = 0
    private var lastCutoff: Double = 0
    private var lastRate: Double = 0

    static let defaultCutoff: Double = 160
    static func cutoff(at position: Double) -> Double { 60 * pow(100, max(0, min(1, position))) }
    static func position(for cutoff: Double) -> Double { log(max(60, min(6000, cutoff)) / 60) / log(100) }

    mutating func next(color: MenuBandAirColor, sampleRate: Double,
                       cutoff: Double = MenuBandAirNoise.defaultCutoff) -> Float {
        let cutoff = cutoff.isFinite ? max(60, min(6000, cutoff)) : Self.defaultCutoff
        if cutoff != lastCutoff || sampleRate != lastRate {
            targetCoefficient = Float(1 - exp(-2 * Double.pi * min(cutoff, sampleRate * 0.45) / sampleRate))
            coefficientStep = 1 / Float(sampleRate * 0.06)
            if lastRate == 0 { filterCoefficient = targetCoefficient }
            lastCutoff = cutoff; lastRate = sampleRate
        }
        filterCoefficient += coefficientStep * (targetCoefficient - filterCoefficient)
        // Deterministic, allocation-free noise on the audio callback.
        seed ^= seed << 13; seed ^= seed >> 7; seed ^= seed << 17
        let white = Float(seed >> 40) / Float(0xFFFFFF) * 2 - 1
        let rate = Float(44100 / max(8000, sampleRate))
        brown += min(1, 0.004 * rate) * (white - brown)
        dc += min(1, 0.0007 * rate) * (brown - dc)
        low += min(1, 0.017 * rate) * (white - low)
        mid += min(1, 0.085 * rate) * (white - mid)
        phase += 2 * Double.pi * 0.13 / sampleRate
        if phase >= 2 * Double.pi { phase -= 2 * Double.pi }
        let dark = (brown - dc) * 12
        let cabin = (dark * 0.65 + low * 2.5 + mid * 0.35) * (0.96 + 0.04 * Float(sin(phase)))
        let blend = min(1, 1 / Float(sampleRate * 0.06))
        whiteWeight += blend * ((color == .white ? 1 : 0) - whiteWeight)
        brownWeight += blend * ((color == .brown ? 1 : 0) - brownWeight)
        cabinWeight += blend * ((color == .cabin ? 1 : 0) - cabinWeight)
        let mixed = tanh(white * whiteWeight + dark * brownWeight + cabin * cabinWeight)
        // Two gentle low-pass stages remove hiss without resonant gain boosts.
        filterA += filterCoefficient * (mixed - filterA)
        filterB += filterCoefficient * (filterA - filterB)
        return filterB
    }
}
