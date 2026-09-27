// singrender — Menu Band's singer, offline. Renders a sung `.play` payload
// line by line to WAV files with no audio device, no window, no Menu Band:
// the same SungLine → AVSpeech → CSinger path the app runs at a downbeat,
// so a file made here IS what the room would hear. Built for the
// intelligibility loop (grants/…/macneopolitan/bin/hear.mjs): render, run
// the words back through Whisper, score, change the core, repeat.
//
//   singrender --kv "lyrics=…;notes=…;singVoice=Noelle;…" --bpm 100 --out DIR
//              [--spoken] [--rate 0.42] [--fs 44100]
//
// Writes DIR/line-NN.wav (+ line-NN.txt: the words), DIR/spoken-NN.wav with
// --spoken (the TTS source, the ceiling), and a JSON manifest on stdout.
import AVFoundation
import Foundation

var args = Array(CommandLine.arguments.dropFirst())
func opt(_ name: String) -> String? {
    guard let i = args.firstIndex(of: name), i + 1 < args.count else { return nil }
    let v = args[i + 1]; args.removeSubrange(i...(i + 1)); return v
}
func flag(_ name: String) -> Bool {
    guard let i = args.firstIndex(of: name) else { return false }
    args.remove(at: i); return true
}
guard let kv = opt("--kv"), let out = opt("--out") else {
    FileHandle.standardError.write("usage: singrender --kv 'k=v;…' --bpm N --out DIR [--spoken] [--rate R] [--fs HZ]\n".data(using: .utf8)!)
    exit(2)
}
let bpm = Double(opt("--bpm") ?? "") ?? 100
let wantSpoken = flag("--spoken")
if let r = opt("--rate"), let f = Float(r) { MenuBandSinger.speechRate = f }
let fs = Double(opt("--fs") ?? "") ?? 44_100

var info: [String: String] = [:]
for pair in kv.split(separator: ";") {
    guard let eq = pair.firstIndex(of: "=") else { continue }
    info[String(pair[..<eq])] = String(pair[pair.index(after: eq)...])
}
guard let sung = SungLine(info: info, bpm: bpm) else {
    FileHandle.standardError.write("singrender: payload has no lyrics/notes\n".data(using: .utf8)!); exit(2)
}
try? FileManager.default.createDirectory(atPath: out, withIntermediateDirectories: true)
let format = AVAudioFormat(commonFormat: .pcmFormatFloat32, sampleRate: fs, channels: 1, interleaved: false)!
let singer = MenuBandSinger()

func writeWav(_ buf: AVAudioPCMBuffer, to path: String) throws {
    let settings: [String: Any] = [
        AVFormatIDKey: kAudioFormatLinearPCM, AVSampleRateKey: buf.format.sampleRate,
        AVNumberOfChannelsKey: 1, AVLinearPCMBitDepthKey: 16, AVLinearPCMIsFloatKey: false,
        AVLinearPCMIsBigEndianKey: false, AVLinearPCMIsNonInterleaved: false,
    ]
    let file = try AVAudioFile(forWriting: URL(fileURLWithPath: path), settings: settings,
                               commonFormat: .pcmFormatFloat32, interleaved: false)
    try file.write(from: buf)
}

// AVSpeechSynthesizer.write delivers its buffers through the main run loop,
// so the work runs off-main and the main thread just spins the loop —
// exactly the shape Menu Band has (render queue + app run loop).
var manifest: [[String: Any]] = []
let lines = sung.splitLines()
DispatchQueue.global(qos: .userInitiated).async {
for (i, line) in lines.enumerated() {
    let tag = String(format: "%02d", i + 1)
    var entry: [String: Any] = ["line": i + 1, "text": line.spoken, "lyrics": line.lyrics, "notes": line.notes]
    let t0 = Date()
    if let r = singer.renderSync(line, into: format) {
        let wav = "\(out)/line-\(tag).wav"
        do { try writeWav(r.buffer, to: wav) } catch { entry["error"] = "\(error)" }
        try? line.spoken.write(toFile: "\(out)/line-\(tag).txt", atomically: true, encoding: .utf8)
        entry["wav"] = wav; entry["duration"] = r.duration; entry["peak"] = r.peak
        entry["notesUsed"] = r.notesUsed; entry["noteCount"] = r.noteCount
        entry["spanOffset"] = r.spanOffset
        if let a = r.articulation {
            entry["mouthCues"] = a.cues.map { ["start": $0.start, "end": $0.end, "shape": $0.shape.rawValue] as [String: Any] }
        }
    } else { entry["error"] = "render gave nothing" }
    entry["renderMs"] = Int(Date().timeIntervalSince(t0) * 1000)
    if wantSpoken, let sp = singer.speechOnly(line) {
        let f = AVAudioFormat(commonFormat: .pcmFormatFloat32, sampleRate: Double(sp.fs), channels: 1, interleaved: false)!
        if let b = AVAudioPCMBuffer(pcmFormat: f, frameCapacity: AVAudioFrameCount(sp.pcm.count)) {
            b.frameLength = AVAudioFrameCount(sp.pcm.count)
            for (k, v) in sp.pcm.enumerated() { b.floatChannelData![0][k] = Float(v) }
            let wav = "\(out)/spoken-\(tag).wav"
            try? writeWav(b, to: wav)
            entry["spoken"] = wav; entry["spokenDuration"] = Double(sp.pcm.count) / Double(sp.fs)
        }
    }
    manifest.append(entry)
}
let json = try! JSONSerialization.data(withJSONObject: ["bpm": bpm, "voice": sung.voice, "lines": manifest], options: [.prettyPrinted, .sortedKeys])
FileHandle.standardOutput.write(json)
FileHandle.standardOutput.write("\n".data(using: .utf8)!)
exit(0)
}
dispatchMain()
