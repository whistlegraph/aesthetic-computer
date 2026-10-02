import AVFoundation
import SwiftUI

/// AC's compkey sample and QWERTY tuning from disks/prompt.mjs.
/// Bundled sample: https://assets.aesthetic.computer/sounds/AeCo_compkey.m4a
@MainActor final class PromptKeySounds: ObservableObject {
    private let engine = AVAudioEngine()
    private var sample: AVAudioPCMBuffer?
    private var voices: [(player: AVAudioPlayerNode, speed: AVAudioUnitVarispeed)] = []
    private var cursor = 0
    private static let notes: [Character: (semitone: Int, pan: Float)] = [
        "c": (0, -0.7), "v": (1, -0.5), "d": (2, -0.6), "s": (3, -0.4),
        "e": (4, -0.5), "f": (5, -0.4), "w": (6, -0.3), "g": (7, -0.3),
        "r": (8, -0.2), "a": (9, -0.2), "q": (10, -0.1), "b": (11, -0.1),
        "h": (12, 0.1), "t": (13, 0.1), "i": (14, 0.2), "y": (15, 0.2),
        "j": (16, 0.3), "k": (17, 0.4), "u": (18, 0.3), "l": (19, 0.5),
        "o": (20, 0.4), "m": (21, 0.6), "p": (22, 0.5), "n": (23, 0.7),
        "z": (-2, -0.9), "x": (-1, -0.8)
    ]

    func prepare() {
        guard sample == nil,
              let url = Bundle.main.url(forResource: "AeCo_compkey", withExtension: "m4a", subdirectory: "Web") else { return }
        do {
            let file = try AVAudioFile(forReading: url)
            guard let buffer = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: AVAudioFrameCount(file.length)) else { return }
            try file.read(into: buffer)
            sample = buffer
            // Overlap quick keystrokes without cutting off the preceding click.
            for _ in 0..<8 {
                let player = AVAudioPlayerNode(), speed = AVAudioUnitVarispeed()
                engine.attach(player); engine.attach(speed)
                engine.connect(player, to: speed, format: buffer.format)
                engine.connect(speed, to: engine.mainMixerNode, format: buffer.format)
                voices.append((player, speed))
            }
            engine.prepare()
        } catch { print("Could not load AC key sound: \(error.localizedDescription)") }
    }

    func play(key: Character?) {
        guard ButtonSounds.enabled, let sample, !voices.isEmpty else { return }
        do {
            if !engine.isRunning {
                // Keyboard feedback respects silent mode and mixes with the piece.
                try AVAudioSession.sharedInstance().setCategory(.ambient, mode: .default)
                try AVAudioSession.sharedInstance().setActive(true)
                try engine.start()
            }
            let voice = voices[cursor]
            cursor = (cursor + 1) % voices.count
            let note = key.flatMap { String($0).lowercased().first }.flatMap { Self.notes[$0] }
            voice.player.stop()
            voice.speed.rate = note.map { Float(pow(2, Double($0.semitone - 11) / 12)) } ?? 1
            voice.player.pan = note?.pan ?? 0
            voice.player.volume = Float.random(in: 0.2...0.6)
            voice.player.scheduleBuffer(sample)
            voice.player.play()
        } catch { print("Could not play AC key sound: \(error.localizedDescription)") }
    }

    func stop() {
        voices.forEach { $0.player.stop() }
        engine.stop()
        // The preview shares this audio session; leave it active.
    }
}
