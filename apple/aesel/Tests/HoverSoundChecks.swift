// swiftc -parse-as-library apple/aesel/Sources/AeselHoverSound.swift apple/aesel/Tests/HoverSoundChecks.swift -o /tmp/aesel-hover-sound-check
import Foundation
import AVFoundation

@main struct HoverSoundChecks {
    static func main() throws {
        func samples(_ revision: Int = 0, _ control: String = "title", _ time: Double = 100) -> [Float] {
            let buffer = AeselHoverSound.buffer(project: "@jeffrey/lepere", revision: revision, control: control, at: time)!
            return Array(UnsafeBufferPointer(start: buffer.floatChannelData![0], count: Int(buffer.frameLength)))
        }
        let original = samples()
        precondition(original == samples(), "Same seed must reproduce the sound")
        precondition(original != samples(1), "Revision must change the voice")
        precondition(original != samples(0, "version"), "Title and version must have distinct voices")
        precondition(original != samples(0, "title", 101), "Time must vary the timbre")
        precondition(original.allSatisfy { $0.isFinite && abs($0) < 1 })
        let duration = Double(original.count) / 44100
        precondition((0.075...0.165).contains(duration))
        let rms = sqrt(original.reduce(0.0) { $0 + Double($1 * $1) } / Double(original.count))
        precondition(rms > 0.01 && rms < 0.3)
        for control in ["title", "version"] {
            let buffer = AeselHoverSound.buffer(project: "@jeffrey/lepere", revision: 0, control: control, at: 100)!
            let file = try AVAudioFile(forWriting: URL(fileURLWithPath: "/tmp/aesel-hover-\(control).wav"), settings: buffer.format.settings)
            try file.write(from: buffer)
        }
        print("Prox synthesis: repeatable seeds, distinct title/revision/time voices, bounded \(Int(duration * 1000))ms signal; RMS \(rms)")
    }
}
