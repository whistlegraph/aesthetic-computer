// beat-listener — hear the room, publish its beat for the wallpaper to follow.
//
//   swiftc -O -framework AVFoundation -framework Accelerate beat-listener.swift -o beat-listener
//   ./beat-listener [--device <name substring>] [--quiet]
//
// Opens the default (or named) audio input, tracks an onset-strength envelope
// at ~86 Hz, estimates tempo by autocorrelation over the last eight seconds
// (60–180 BPM), and twice a second writes ~/.local/share/slab/wallpaper/beat.json
// {bpm, beatAt, energy, updatedAt} and posts
// computer.aesthetic.slab.beat.changed with the same payload. When the room is
// quiet it publishes energy 0 so followers settle. Audio never leaves memory.
import AVFoundation
import Accelerate
import Foundation

let args = CommandLine.arguments
func flag(_ k: String) -> String? { if let i = args.firstIndex(of: k), i + 1 < args.count { return args[i + 1] }; return nil }
let quiet = args.contains("--quiet")
let outPath = NSString(string: "~/.local/share/slab/wallpaper/beat.json").expandingTildeInPath
let note = Notification.Name("computer.aesthetic.slab.beat.changed")

let engine = AVAudioEngine()
if let wanted = flag("--device") {
    // Pick an input by name via Core Audio so a USB mic can be preferred.
    var address = AudioObjectPropertyAddress(mSelector: kAudioHardwarePropertyDevices,
                                             mScope: kAudioObjectPropertyScopeGlobal,
                                             mElement: kAudioObjectPropertyElementMain)
    var size: UInt32 = 0
    AudioObjectGetPropertyDataSize(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, &size)
    var ids = [AudioDeviceID](repeating: 0, count: Int(size) / MemoryLayout<AudioDeviceID>.size)
    AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, &size, &ids)
    for id in ids {
        var nameAddress = AudioObjectPropertyAddress(mSelector: kAudioObjectPropertyName,
                                                     mScope: kAudioObjectPropertyScopeGlobal,
                                                     mElement: kAudioObjectPropertyElementMain)
        var name: CFString = "" as CFString
        var nameSize = UInt32(MemoryLayout<CFString>.size)
        guard AudioObjectGetPropertyData(id, &nameAddress, 0, nil, &nameSize, &name) == noErr else { continue }
        if (name as String).localizedCaseInsensitiveContains(wanted) {
            var deviceID = id
            if let unit = engine.inputNode.audioUnit {
                AudioUnitSetProperty(unit, kAudioOutputUnitProperty_CurrentDevice,
                                     kAudioUnitScope_Global, 0, &deviceID, UInt32(MemoryLayout<AudioDeviceID>.size))
            }
            if !quiet { print("input: \(name)") }
            break
        }
    }
}

let input = engine.inputNode
let format = input.outputFormat(forBus: 0)
guard format.channelCount > 0 else { fputs("no audio input device on this machine\n", stderr); exit(1) }
let hop = 512
let hopRate = format.sampleRate / Double(hop)          // ~86–94 envelopes per second
let window = Int(hopRate * 8)                          // eight seconds of onset history
var onsets = [Float](repeating: 0, count: window)
var head = 0
var previousEnergy: Float = 0
var smoothedEnergy: Float = 0
var loudness: Float = 0
var lastOnsetTime = Date().timeIntervalSince1970
var lastOnsetStrength: Float = 0
let lock = NSLock()

input.installTap(onBus: 0, bufferSize: AVAudioFrameCount(hop * 4), format: format) { buffer, _ in
    guard let data = buffer.floatChannelData?[0] else { return }
    let n = Int(buffer.frameLength)
    var offset = 0
    lock.lock(); defer { lock.unlock() }
    while offset + hop <= n {
        var rms: Float = 0
        vDSP_rmsqv(data + offset, 1, &rms, vDSP_Length(hop))
        offset += hop
        // Onset strength: the rise of a lightly smoothed energy envelope.
        smoothedEnergy += (rms - smoothedEnergy) * 0.5
        let rise = max(smoothedEnergy - previousEnergy, 0)
        previousEnergy = smoothedEnergy
        loudness += (rms - loudness) * 0.02
        onsets[head] = rise
        head = (head + 1) % window
        // A beat candidate: a rise well above the recent mean rise.
        var mean: Float = 0
        vDSP_meanv(onsets, 1, &mean, vDSP_Length(window))
        if rise > mean * 3, rise > lastOnsetStrength * 0.6 || Date().timeIntervalSince1970 - lastOnsetTime > 0.25 {
            lastOnsetTime = Date().timeIntervalSince1970
            lastOnsetStrength = rise
        }
        lastOnsetStrength *= 0.995
    }
}

/// Tempo from the autocorrelation of the onset envelope, 60–180 BPM.
func estimateBPM() -> (bpm: Double, confidence: Double) {
    let x = onsets
    var mean: Float = 0
    vDSP_meanv(x, 1, &mean, vDSP_Length(window))
    var centered = [Float](repeating: 0, count: window)
    var negMean = -mean
    vDSP_vsadd(x, 1, &negMean, &centered, 1, vDSP_Length(window))
    var zero: Float = 0
    vDSP_dotpr(centered, 1, centered, 1, &zero, vDSP_Length(window))
    guard zero > 0 else { return (0, 0) }
    let minLag = Int(hopRate * 60 / 180), maxLag = Int(hopRate * 60 / 60)
    var ac = [Float](repeating: 0, count: maxLag + 2)
    for lag in (minLag - 1)...(maxLag + 1) where lag > 0 {
        var r: Float = 0
        centered.withUnsafeBufferPointer { p in
            vDSP_dotpr(p.baseAddress!, 1, p.baseAddress! + lag, 1, &r, vDSP_Length(window - lag))
        }
        ac[lag] = r / Float(window - lag)
    }
    var best = (lag: 0, value: Float(0))
    for lag in minLag...maxLag {
        // A faint preference for faster lags breaks exact octave ties only.
        let weighted = ac[lag] * (1 + 0.02 * Float(maxLag - lag) / Float(maxLag))
        if weighted > best.value { best = (lag, weighted) }
    }
    guard best.lag > 0 else { return (0, 0) }
    // Parabolic interpolation between neighbouring lags: the envelope rate is
    // coarse (~90–190 Hz), so the integer lag alone quantises the tempo.
    let l = best.lag, y0 = ac[l - 1], y1 = ac[l], y2 = ac[l + 1]
    let denom = y0 - 2 * y1 + y2
    let shift = denom != 0 ? max(-0.5, min(0.5, 0.5 * (y0 - y2) / denom)) : 0
    let lag = Double(l) + Double(shift)
    let confidence = Double(ac[l] / (zero / Float(window)))
    return (60 * hopRate / lag, min(max(confidence, 0), 1))
}

func publish() {
    lock.lock()
    let (bpm, confidence) = estimateBPM()
    let energy = min(Double(loudness) * 12, 1)
    let beatAt = lastOnsetTime
    lock.unlock()
    let live = energy > 0.04 && confidence > 0.08 && bpm > 0
    let payload: [String: Any] = [
        "bpm": live ? (bpm * 10).rounded() / 10 : 0,
        "beatAt": beatAt,
        "energy": live ? (energy * 100).rounded() / 100 : 0,
        "confidence": (confidence * 100).rounded() / 100,
        "updatedAt": Date().timeIntervalSince1970,
    ]
    try? FileManager.default.createDirectory(atPath: (outPath as NSString).deletingLastPathComponent,
                                             withIntermediateDirectories: true)
    if let data = try? JSONSerialization.data(withJSONObject: payload, options: [.sortedKeys]) {
        try? data.write(to: URL(fileURLWithPath: outPath), options: .atomic)
    }
    DistributedNotificationCenter.default().postNotificationName(
        note, object: nil, userInfo: payload, deliverImmediately: true)
    if !quiet {
        print(String(format: "bpm %5.1f  energy %.2f  confidence %.2f%@", payload["bpm"] as! Double,
                     payload["energy"] as! Double, confidence, live ? "" : "  (quiet)"))
        fflush(stdout)
    }
}

do { try engine.start() } catch { fputs("could not start audio input: \(error)\n", stderr); exit(1) }
if !quiet { print("listening at \(Int(format.sampleRate)) Hz · publishing \(outPath)") }
let timer = Timer(timeInterval: 0.5, repeats: true) { _ in publish() }
RunLoop.main.add(timer, forMode: .common)
RunLoop.main.run()
