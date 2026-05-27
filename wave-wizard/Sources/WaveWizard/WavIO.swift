import Foundation

// Minimal mono 16-bit PCM WAV reader / writer + trim + normalize.
// Matches the same shape as pop/hellsine/bin/cut-shakes.mjs so trimmed
// samples are interchangeable across the toolchain.
enum WavIO {

    static func load(path: String) -> (samples: [Float], sr: Int)? {
        guard let buf = try? Data(contentsOf: URL(fileURLWithPath: path)) else { return nil }
        var dOff = 0
        var dLen = 0
        var sr = 48000
        var bits = 16
        var channels = 1
        var pos = 12
        while pos + 8 <= buf.count {
            let id = String(data: buf.subdata(in: pos..<pos+4), encoding: .ascii) ?? ""
            let sz = Int(buf.uInt32LE(at: pos + 4))
            if id == "fmt " {
                channels = Int(buf.uInt16LE(at: pos + 10))
                sr = Int(buf.uInt32LE(at: pos + 12))
                bits = Int(buf.uInt16LE(at: pos + 22))
            } else if id == "data" {
                dOff = pos + 8; dLen = sz
                break
            }
            pos += 8 + sz + (sz & 1)
        }
        guard dOff > 0 else { return nil }
        let bytesPerSample = bits / 8
        let frameSize = bytesPerSample * channels
        let frames = dLen / frameSize
        var out = [Float](repeating: 0, count: frames)
        for i in 0..<frames {
            var acc: Float = 0
            for c in 0..<channels {
                let o = dOff + i * frameSize + c * bytesPerSample
                if bits == 16 {
                    let v = Int16(bitPattern: buf.uInt16LE(at: o))
                    acc += Float(v) / 32768.0
                } else if bits == 32 {
                    let v = Int32(bitPattern: buf.uInt32LE(at: o))
                    acc += Float(v) / Float(Int32.max)
                }
            }
            out[i] = acc / Float(channels)
        }
        return (out, sr)
    }

    static func write(path: String, samples: [Float], sr: Int) {
        let n = samples.count
        let dataLen = n * 2
        var buf = Data(capacity: 44 + dataLen)
        buf.append("RIFF".data(using: .ascii)!)
        buf.append(UInt32(36 + dataLen).leBytes)
        buf.append("WAVE".data(using: .ascii)!)
        buf.append("fmt ".data(using: .ascii)!)
        buf.append(UInt32(16).leBytes)
        buf.append(UInt16(1).leBytes)        // PCM
        buf.append(UInt16(1).leBytes)        // mono
        buf.append(UInt32(sr).leBytes)
        buf.append(UInt32(sr * 2).leBytes)
        buf.append(UInt16(2).leBytes)
        buf.append(UInt16(16).leBytes)
        buf.append("data".data(using: .ascii)!)
        buf.append(UInt32(dataLen).leBytes)
        for s in samples {
            let v = max(-1, min(1, s))
            let i = Int16(round(v * 32767))
            buf.append(UInt16(bitPattern: i).leBytes)
        }
        try? buf.write(to: URL(fileURLWithPath: path))
    }

    /// Trim to the actual hit (peak-relative threshold), pad head/tail,
    /// declick fade, normalize to `normDb` dBFS. Returns true on success.
    @discardableResult
    static func trimNormalize(
        path: String,
        thresholdPctOfPeak: Double = 0.08,
        padHeadMs: Double = 8,
        padTailMs: Double = 80,
        fadeMs: Double = 3,
        normalizeDb: Double = -1.0
    ) -> Bool {
        guard let (s, sr) = load(path: path) else { return false }
        if s.isEmpty { return false }
        var peak: Float = 0
        for v in s { peak = max(peak, abs(v)) }
        if peak < 1e-4 { return false }
        let th = peak * Float(thresholdPctOfPeak)
        var a = 0, b = s.count
        while a < b && abs(s[a]) < th { a += 1 }
        while b > a && abs(s[b - 1]) < th { b -= 1 }
        a = max(0, a - Int(Double(sr) * padHeadMs / 1000))
        b = min(s.count, b + Int(Double(sr) * padTailMs / 1000))
        var out = Array(s[a..<b])
        var outPeak: Float = 0
        for v in out { outPeak = max(outPeak, abs(v)) }
        let normPeak = Float(pow(10.0, normalizeDb / 20.0))
        let gain: Float = outPeak > 0 ? normPeak / outPeak : 1
        let fadeN = Int(Double(sr) * fadeMs / 1000)
        for k in 0..<out.count {
            var v = out[k] * gain
            if k < fadeN { v *= Float(k) / Float(fadeN) }
            let tailDist = out.count - 1 - k
            if tailDist < fadeN { v *= Float(tailDist) / Float(fadeN) }
            out[k] = v
        }
        write(path: path, samples: out, sr: sr)
        return true
    }
}

private extension Data {
    func uInt32LE(at o: Int) -> UInt32 {
        UInt32(self[o]) | UInt32(self[o + 1]) << 8 |
        UInt32(self[o + 2]) << 16 | UInt32(self[o + 3]) << 24
    }
    func uInt16LE(at o: Int) -> UInt16 {
        UInt16(self[o]) | UInt16(self[o + 1]) << 8
    }
}

private extension UInt32 {
    var leBytes: Data {
        var v = self.littleEndian
        return Data(bytes: &v, count: 4)
    }
}
private extension UInt16 {
    var leBytes: Data {
        var v = self.littleEndian
        return Data(bytes: &v, count: 2)
    }
}
