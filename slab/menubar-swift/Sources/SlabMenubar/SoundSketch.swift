import Foundation
import AVFoundation
import Accelerate

/// What the sound card draws, measured from the file itself: an overview of
/// its loudness in columns, and the spectrum in sixteen bands twenty times a
/// second, so the bars that move while it plays are the record's own and
/// never an animation standing in for it. Any format AVFoundation reads
/// (mp3, wav, aiff, m4a). Values are quantized to one base-36 character each,
/// which keeps a three-minute song's sketch near sixty kilobytes of page.
struct SoundSketch {
    static let columns = 480
    static let bands = 16
    static let fps = 20

    let duration: Double
    /// `columns` characters of peak level, then `columns` of RMS.
    let peaks: String
    let rms: String
    /// `frames × bands` characters, frame-major.
    let spectrum: String
    let frames: Int

    private static let digits = Array("0123456789abcdefghijklmnopqrstuvwxyz")
    private static func q(_ x: Float) -> Character { digits[max(0, min(35, Int((x * 35).rounded())))] }

    init?(url: URL, maxSeconds: Double = 20 * 60) {
        guard let file = try? AVAudioFile(forReading: url) else { return nil }
        let sr = file.processingFormat.sampleRate, total = Int(file.length)
        guard sr > 0, total > 0, Double(total) / sr <= maxSeconds else { return nil }
        // One mono line: the channels averaged, read in blocks.
        var mono = [Float](repeating: 0, count: total)
        let block: AVAudioFrameCount = 1 << 16
        guard let buffer = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: block) else { return nil }
        var at = 0
        while at < total {
            do { try file.read(into: buffer, frameCount: min(block, AVAudioFrameCount(total - at))) } catch { break }
            let n = Int(buffer.frameLength), ch = Int(buffer.format.channelCount)
            guard n > 0, let data = buffer.floatChannelData else { break }
            for c in 0..<ch {
                mono.withUnsafeMutableBufferPointer { dst in
                    vDSP_vsma(data[c], 1, [1 / Float(ch)], dst.baseAddress! + at, 1, dst.baseAddress! + at, 1, vDSP_Length(n))
                }
            }
            at += n
        }
        let count = at
        guard count > 0 else { return nil }
        duration = Double(count) / sr

        // The overview: peak and RMS per column, against the loudest column.
        var peakCol = [Float](repeating: 0, count: Self.columns), rmsCol = peakCol
        mono.withUnsafeBufferPointer { p in
            for x in 0..<Self.columns {
                let a = x * count / Self.columns, b = max(a + 1, (x + 1) * count / Self.columns)
                var pk: Float = 0, ms: Float = 0
                vDSP_maxmgv(p.baseAddress! + a, 1, &pk, vDSP_Length(b - a))
                vDSP_measqv(p.baseAddress! + a, 1, &ms, vDSP_Length(b - a))
                peakCol[x] = pk; rmsCol[x] = sqrt(ms)
            }
        }
        let top = max(peakCol.max() ?? 0, 1e-6)
        peaks = String(peakCol.map { Self.q($0 / top) })
        rms = String(rmsCol.map { Self.q(min(1, $0 / top * 1.6)) })

        // The spectrum: a 2048-point Hann FFT every 1/fps second, folded into
        // log-spaced bands from 40 Hz to 16 kHz, -66…0 dB against the loudest.
        let n = 2048, log2n = vDSP_Length(11), hop = sr / Double(Self.fps)
        frames = max(1, Int(duration * Double(Self.fps)))
        guard let setup = vDSP_create_fftsetup(log2n, FFTRadix(kFFTRadix2)) else { return nil }
        defer { vDSP_destroy_fftsetup(setup) }
        var window = [Float](repeating: 0, count: n)
        vDSP_hann_window(&window, vDSP_Length(n), Int32(vDSP_HANN_NORM))
        let edges: [Int] = (0...Self.bands).map { k in
            let f = 40 * pow(16000.0 / 40, Double(k) / Double(Self.bands))
            return max(1, min(n / 2 - 1, Int(f / sr * Double(n))))
        }
        var levels = [Float](repeating: 0, count: frames * Self.bands)
        var frame = [Float](repeating: 0, count: n), re = [Float](repeating: 0, count: n / 2), im = re, mag = re
        for i in 0..<frames {
            let start = Int(Double(i) * hop) - n / 2
            for j in 0..<n { let s = start + j; frame[j] = s >= 0 && s < count ? mono[s] * window[j] : 0 }
            re.withUnsafeMutableBufferPointer { r in im.withUnsafeMutableBufferPointer { m in
                var split = DSPSplitComplex(realp: r.baseAddress!, imagp: m.baseAddress!)
                frame.withUnsafeBufferPointer { f in
                    f.baseAddress!.withMemoryRebound(to: DSPComplex.self, capacity: n / 2) { vDSP_ctoz($0, 2, &split, 1, vDSP_Length(n / 2)) }
                }
                vDSP_fft_zrip(setup, &split, 1, log2n, FFTDirection(FFT_FORWARD))
                vDSP_zvmags(&split, 1, &mag, 1, vDSP_Length(n / 2))
            } }
            mag.withUnsafeBufferPointer { m in
                for k in 0..<Self.bands {
                    let a = edges[k], b = max(a + 1, edges[k + 1])
                    var mean: Float = 0
                    vDSP_meanv(m.baseAddress! + a, 1, &mean, vDSP_Length(b - a))
                    levels[i * Self.bands + k] = 10 * log10(mean + 1e-12)
                }
            }
        }
        let loud = levels.max() ?? 0
        spectrum = String(levels.map { Self.q(max(0, ($0 - loud + 66) / 66)) })
    }
}
