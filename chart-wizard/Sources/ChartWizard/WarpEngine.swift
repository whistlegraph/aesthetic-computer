// WarpEngine.swift — hear the drag immediately.
//
// The real render is halo3's: WORLD analysis, a frame-axis warp where
// vowels absorb the stretch and consonants ride near 1:1, the pitch snap,
// the halos. It takes half a minute and it is the thing that ships. This
// is not that. This is WSOLA on the raw slice — overlap-add with a
// correlation search so the grains line up — assembled onto the beat grid
// with a kick, in a few milliseconds, so a boundary can be judged by ear
// while the mouse is still down. Timing is what it has to be honest
// about, and timing is exactly what WSOLA preserves.
import AVFoundation

final class WarpEngine {
    private let engine = AVAudioEngine()
    private let player = AVAudioPlayerNode()
    private var source: [Float] = []
    private var sr: Double = 44100
    private(set) var isPlaying = false
    private var startHostTime: AVAudioTime?
    private var renderedFrames: AVAudioFrameCount = 0

    init() {
        engine.attach(player)
        engine.connect(player, to: engine.mainMixerNode,
                       format: AVAudioFormat(standardFormatWithSampleRate: 44100, channels: 1))
    }

    func load(wav: URL) throws {
        let file = try AVAudioFile(forReading: wav)
        sr = file.processingFormat.sampleRate
        guard let buf = AVAudioPCMBuffer(pcmFormat: file.processingFormat,
                                         frameCapacity: AVAudioFrameCount(file.length)) else { return }
        try file.read(into: buf)
        let n = Int(buf.frameLength)
        var mono = [Float](repeating: 0, count: n)
        if let ch = buf.floatChannelData {
            let chans = Int(buf.format.channelCount)
            for i in 0..<n {
                var s: Float = 0
                for c in 0..<chans { s += ch[c][i] }
                mono[i] = s / Float(chans)
            }
        }
        source = mono
    }

    // ── WSOLA ────────────────────────────────────────────────────────
    // Grains of `win` samples are laid down every `synHop`; each is pulled
    // from around its ideal analysis position, but shifted by up to
    // ±`search` samples to wherever it correlates best with what is
    // already in the output. That search is the whole trick: without it a
    // stretched vowel phases against itself and buzzes.
    private func wsola(_ x: ArraySlice<Float>, toLength out: Int) -> [Float] {
        let src = Array(x)
        guard out > 0 else { return [] }
        guard src.count > 8 else { return [Float](repeating: 0, count: out) }
        let rate = Double(src.count) / Double(out)
        if abs(rate - 1.0) < 0.01 && src.count >= out { return Array(src[0..<out]) }

        let win = max(64, min(Int(0.030 * sr), src.count / 2))
        let synHop = win / 2
        let search = max(8, Int(0.006 * sr))
        var window = [Float](repeating: 0, count: win)
        for i in 0..<win { window[i] = 0.5 - 0.5 * cos(2 * .pi * Float(i) / Float(win)) }

        var y = [Float](repeating: 0, count: out + win)
        var norm = [Float](repeating: 0, count: out + win)
        var anaPos = 0.0
        var synPos = 0
        var prevTail = [Float](repeating: 0, count: synHop)

        while synPos < out {
            var best = Int(anaPos.rounded())
            if synPos > 0 {
                // line the next grain up with the tail we just wrote
                var bestScore = -Float.greatestFiniteMagnitude
                let lo = max(0, Int(anaPos.rounded()) - search)
                let hi = min(src.count - win - 1, Int(anaPos.rounded()) + search)
                if lo <= hi {
                    for cand in stride(from: lo, through: hi, by: 2) {
                        var score: Float = 0
                        for k in stride(from: 0, to: synHop, by: 2) {
                            score += prevTail[k] * src[cand + k]
                        }
                        if score > bestScore { bestScore = score; best = cand }
                    }
                }
            }
            best = min(max(0, best), max(0, src.count - win - 1))
            for k in 0..<win where synPos + k < y.count {
                y[synPos + k] += src[best + k] * window[k]
                norm[synPos + k] += window[k]
            }
            for k in 0..<synHop {
                let idx = best + synHop + k
                prevTail[k] = idx < src.count ? src[idx] : 0
            }
            synPos += synHop
            anaPos += Double(synHop) * rate
            if anaPos >= Double(src.count - win) { anaPos = Double(max(0, src.count - win - 1)) }
        }
        for i in 0..<y.count where norm[i] > 1e-6 { y[i] /= norm[i] }
        return Array(y[0..<out])
    }

    /// Assemble every unit onto the grid, plus the four-on-the-floor the
    /// words are being regulated against — the kick is not decoration
    /// here, it is the ruler.
    func assemble(units: [Unit], spb: Double, leadIn: Double,
                  totalBeats: Double, kick: Bool = true) -> AVAudioPCMBuffer? {
        let total = Int(((totalBeats + 2) * spb) * sr)
        guard total > 0,
              let fmt = AVAudioFormat(standardFormatWithSampleRate: sr, channels: 1),
              let buf = AVAudioPCMBuffer(pcmFormat: fmt, frameCapacity: AVAudioFrameCount(total))
        else { return nil }
        buf.frameLength = AVAudioFrameCount(total)
        guard let out = buf.floatChannelData?[0] else { return nil }
        for i in 0..<total { out[i] = 0 }

        for u in units {
            let a = Int(u.src0 * sr), b = Int(u.src1 * sr)
            guard a >= 0, b > a, b <= source.count else { continue }
            let target = Int(u.dur * spb * sr)
            guard target > 0 else { continue }
            let warped = wsola(source[a..<b], toLength: target)
            let at = Int((u.beat * spb - leadIn * spb) * sr)
            // 5 ms raised-cosine seams, the lane's rule everywhere else too
            let fade = min(Int(0.005 * sr), warped.count / 2)
            for (k, v) in warped.enumerated() {
                let i = at + k
                guard i >= 0, i < total else { continue }
                var g: Float = 1
                if k < fade { g = 0.5 - 0.5 * cos(.pi * Float(k) / Float(fade)) }
                else if k >= warped.count - fade {
                    g = 0.5 - 0.5 * cos(.pi * Float(warped.count - k) / Float(fade))
                }
                out[i] += v * g
            }
        }

        if kick {
            let beats = Int(totalBeats.rounded(.up))
            let len = Int(0.09 * sr)
            for beat in 0...max(0, beats) {
                let at = Int(Double(beat) * spb * sr)
                for k in 0..<len {
                    let i = at + k
                    guard i >= 0, i < total else { break }
                    let t = Double(k) / sr
                    let env = exp(-t * 38.0)
                    let f = 118.0 * exp(-t * 26.0) + 44.0
                    out[i] += Float(sin(2 * .pi * f * t) * env * 0.30)
                }
            }
        }
        return buf
    }

    /// Play from a beat by slicing the assembled buffer — scheduleBuffer
    /// has no start offset, and copying a few hundred KB is free next to
    /// re-warping every unit.
    func play(_ buf: AVAudioPCMBuffer, fromBeat beat: Double = 0, spb: Double = 0.5) {
        stop()
        do { try engine.start() } catch { return }
        let skip = Int(max(0, beat * spb * sr))
        guard skip < Int(buf.frameLength) else { return }
        let piece: AVAudioPCMBuffer
        if skip == 0 {
            piece = buf
        } else {
            let n = AVAudioFrameCount(Int(buf.frameLength) - skip)
            guard let cut = AVAudioPCMBuffer(pcmFormat: buf.format, frameCapacity: n),
                  let src = buf.floatChannelData?[0], let dst = cut.floatChannelData?[0]
            else { return }
            cut.frameLength = n
            for i in 0..<Int(n) { dst[i] = src[skip + i] }
            piece = cut
        }
        player.scheduleBuffer(piece, at: nil, options: []) { [weak self] in
            DispatchQueue.main.async { self?.isPlaying = false }
        }
        player.play()
        isPlaying = true
    }

    func stop() {
        player.stop()
        isPlaying = false
    }

    /// Where the playhead is, in seconds, for the roll to draw.
    var elapsed: Double {
        guard isPlaying,
              let node = player.lastRenderTime,
              let t = player.playerTime(forNodeTime: node) else { return 0 }
        return Double(t.sampleTime) / t.sampleRate
    }
}
