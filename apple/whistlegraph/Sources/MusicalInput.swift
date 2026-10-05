import Foundation
import AVFoundation

// Streaming adaptation of MenuBandSampleVoice.detectFundamental and
// MenuBandMicTempo's log-energy onset novelty. Inference never runs here.
final class MusicalInput {
    private let queue = DispatchQueue(label: "computer.walkieware.sound", qos: .userInitiated)
    private let recording = UtteranceRecording()
    private let speechPCM = SpeechPCM()
    var onPCM: ((Data) -> Void)?
    private var carry: [Double] = []
    private var offset = 0
    private var decimationSum = 0.0
    private var decimationCount = 0
    private var rate = 8000.0
    private var frames: [[String: Double]] = []
    private var onsets: [Double] = []
    private var lastEnergy = -20.0
    private var lastOnset = -1000.0
    var onUpdate: ((Double?, Double) -> Void)?
    var onObservation: (([String: Any]) -> Void)?
    func feed(_ buffer: AVAudioPCMBuffer) {
        guard let channel = buffer.floatChannelData?[0], buffer.frameLength > 0 else { return }
        let step = max(1, Int(buffer.format.sampleRate / 8000))
        let samples = (0..<Int(buffer.frameLength)).map { Double(channel[$0]) }
        let originalRate = buffer.format.sampleRate
        let sampleRate = originalRate / Double(step)
        queue.async { [self] in
            guard frames.count < 1500 else { return }
            recording.append(samples, rate: originalRate)
            onPCM?(speechPCM.append(samples, rate: originalRate))
            rate = sampleRate
            // Keep decimation phase across input buffers; average before sampling.
            for sample in samples {
                decimationSum += sample; decimationCount += 1
                if decimationCount == step {
                    carry.append(decimationSum / Double(step)); decimationSum = 0; decimationCount = 0
                }
            }
            while carry.count >= 512 && frames.count < 1500 {
                let values = Array(carry.prefix(512))
                let measurement = Self.measure(values, rate: rate)
                let at = Double(offset) / rate * 1000
                var frame = ["atMs": at, "rms": measurement.rms]
                if let hz = measurement.pitch { frame["pitchHz"] = hz }
                let energy = log(measurement.rms * measurement.rms + 1e-9)
                if measurement.rms > 0.012 && energy - lastEnergy > 1.8 && at - lastOnset > 120 {
                    onsets.append(at); lastOnset = at
                }
                lastEnergy = energy; frames.append(frame)
                if frames.count % 3 == 0 { onUpdate?(measurement.pitch, measurement.rms) }
                if frames.count % 16 == 0 { onObservation?(observation()) }
                carry.removeFirst(256); offset += 256
            }
        }
    }
    private func observation() -> [String: Any] {
        let strideSize = max(1, Int(ceil(Double(frames.count) / 128)))
        let timeline = stride(from: 0, to: frames.count, by: strideSize).map { frames[$0] }
        let audible = frames.filter { ($0["rms"] ?? 0) > 0.012 }.count
        return ["schema":"walkieware-sound/v1", "durationMs":Double(offset + carry.count) / rate * 1000,
                "audibleMs":Double(audible * 256) / rate * 1000, "frames":timeline,
                "onsetsMs":Array(onsets.prefix(128)), "analysis":"dominant monophonic pitch and energy onsets; estimates, not note or beat transcription"]
    }
    func finish(_ completion: @escaping ([String: Any]) -> Void) {
        queue.async { [self] in
            var result = observation()
            if let id = recording.finish() { result["recordingID"] = id }
            completion(result)
        }
    }
    static func measure(_ samples: [Double], rate: Double) -> (pitch: Double?, rms: Double) {
        let n = samples.count
        guard n >= 128, rate > 0 else { return (nil,0) }
        let mean = samples.reduce(0,+) / Double(n)
        let rms = sqrt(samples.reduce(0) { $0 + ($1-mean)*($1-mean) } / Double(n))
        guard rms > 0.008 else { return (nil,rms) }
        let data = samples.enumerated().map { i,v in (v-mean) * (0.5-0.5*cos(2 * .pi * Double(i)/Double(n-1))) }
        let energy = data.reduce(0) { $0+$1*$1 }
        let lo = max(2,Int(rate/2000)), hi = min(n/2,Int(rate/70))
        guard hi > lo+2, energy > 1e-9 else { return (nil,rms) }
        var corr = [Double](repeating:0,count:hi+1)
        for lag in lo...hi { for i in 0..<(n-lag) { corr[lag] += data[i]*data[i+lag] }; corr[lag] /= energy }
        for lag in (lo+1)..<hi where corr[lag] > 0.6 && corr[lag] > corr[lag-1] && corr[lag] >= corr[lag+1] {
            let denom = corr[lag-1]-2*corr[lag]+corr[lag+1]
            let shift = abs(denom)>1e-9 ? 0.5*(corr[lag-1]-corr[lag+1])/denom : 0
            return (rate/(Double(lag)+shift),rms)
        }
        return (nil,rms)
    }
}
