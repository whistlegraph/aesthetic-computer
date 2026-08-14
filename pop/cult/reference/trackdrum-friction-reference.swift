import Foundation

// The friction voice lifted verbatim out of MenuBandPercussion.swift, plus the
// position->material mapping from setDrumSkinScratch. No GUI, no engine: it
// renders the same maths to a raw f32 file so a JS port can be diffed against
// the real thing instead of against a reading of the source.

func xorshift(_ s: inout UInt32) -> UInt32 {
    s ^= s << 13; s ^= s >> 17; s ^= s << 5
    return s
}
func smoothstep(_ a: Double, _ b: Double, _ v: Double) -> Double {
    let u = min(1, max(0, (v - a) / (b - a))); return u * u * (3 - 2 * u)
}
func mix(_ a: Double, _ b: Double, _ amount: Double) -> Double { a + (b - a) * amount }

func roundedTrackpadDistance(sx: Double, sy: Double) -> Double {
    let halfWidth = 0.82, halfHeight = 0.50, corner = 0.055
    let px = abs(sx) * halfWidth, py = abs(sy) * halfHeight
    let qx = px - (halfWidth - corner), qy = py - (halfHeight - corner)
    let outside = hypot(max(qx, 0), max(qy, 0))
    let inside = min(max(qx, qy), 0)
    let signedDistance = outside + inside - corner
    let inwardDepth = max(0, -signedDistance)
    return min(1, max(0, 1 - inwardDepth / 0.5))
}

let sampleRate = 48000.0
let seconds = 2.0
let n = Int(sampleRate * seconds)
var out = [Float](repeating: 0, count: n)
var seed: UInt32 = 0x9e3779b9
var noiseState = 0.0, slowNoiseState = 0.0, phase = 0.0, level = 0.0

// A finger dragging left->right across the middle of the pad, then easing off.
for i in 0..<n {
    let u = Double(i) / sampleRate
    let x = min(1.0, u / 1.2)
    let point = CGPoint(x: 0.12 + 0.76 * x, y: 0.5)
    let speed = (u < 1.2) ? 0.9 : 0.0            // normalized pad-lengths/sec

    let sx = Double(point.x - 0.5) * 2, sy = Double(point.y - 0.5) * 2
    let distance = roundedTrackpadDistance(sx: sx, sy: sy)
    let tension = 1.0
    let levelScale = 0.052, levelCeiling = 0.14
    let energyGain = 0.76
    let target = min(levelCeiling, max(0, speed) * levelScale) * energyGain

    let toSnare = smoothstep(0.23, 0.31, distance)
    let toRim = smoothstep(0.40, 0.48, distance)
    let toHat = smoothstep(0.62, 0.70, distance)
    let toClick = smoothstep(0.88, 0.965, distance)
    var materialCutoff = mix(175, 430, toSnare)
    materialCutoff = mix(materialCutoff, 680, toRim)
    materialCutoff = mix(materialCutoff, 1_250, toHat)
    materialCutoff = mix(materialCutoff, 2_050, toClick)
    let cutoff = materialCutoff * (0.88 + tension * 0.12)
    var resonance = mix(mix(mix(mix(48, 90, toSnare), 185, toRim), 360, toHat), 560, toClick) * tension
    let pathVariation = 1.0 + 0.055 * sin((sx * 2.7 + sy * 3.9) * .pi)
    resonance *= pathVariation
    let roughness = 0.55

    let attackA = 1.0 - exp(-1.0 / (sampleRate * 0.0025))
    let releaseA = 1.0 - exp(-1.0 / (sampleRate * 0.10))
    let filterA = 1.0 - exp(-2.0 * .pi * cutoff / sampleRate)
    let slowFilterA = 1.0 - exp(-2.0 * .pi * max(35.0, cutoff * 0.18) / sampleRate)

    let smoothing = target > level ? attackA : releaseA
    level += smoothing * (target - level)
    let white = Double(xorshift(&seed)) / Double(UInt32.max) * 2.0 - 1.0
    noiseState += filterA * (white - noiseState)
    slowNoiseState += slowFilterA * (white - slowNoiseState)
    let frictionBand = noiseState - slowNoiseState
    let pitchMotion = 1.0 + tanh(frictionBand * 8.0) * 0.055
    phase += resonance * pitchMotion / sampleRate
    if phase >= 1 { phase -= floor(phase) }
    let carrier = sin(2.0 * .pi * phase)
    let gnarl = tanh(frictionBand * (5.0 + roughness * 5.0))
    let texture = gnarl * 0.44
        + carrier * (0.08 + abs(gnarl) * (0.42 + roughness * 0.30))
    out[i] = Float(texture * level)
}

// report what the real voice actually reaches
let peak = out.map { abs($0) }.max() ?? 0
FileHandle.standardError.write("reference peak \(peak)  (level ceiling 0.14 * 0.76)\n".data(using: .utf8)!)
out.withUnsafeBufferPointer { buf in
    FileManager.default.createFile(atPath: "ref.f32", contents: Data(buffer: buf))
}
