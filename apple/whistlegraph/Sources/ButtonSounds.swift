import AudioToolbox
import AVFoundation
import UIKit

// Every control in the app answers with a short synthesized cue and a matching
// haptic. The cues are tiny WAVs written once into Caches and played through
// the system sound API, so they follow the silent switch and ringer volume and
// never take over the audio session the microphone or the narrator is using.
// A "Button sounds" toggle in the account sheet silences the audio; haptics
// stay, since they disturb no one.
@MainActor enum ButtonSounds {
    static let settingKey = "walkieware-sounds"

    enum Cue: String, CaseIterable {
        case press, release, sent, stop, tick, pop, play, key, error
    }

    static var enabled: Bool { UserDefaults.standard.object(forKey: settingKey) as? Bool ?? true }

    static func play(_ cue: Cue) {
        haptic(cue)
        guard enabled, let id = soundID(cue) else { return }
        AudioServicesPlaySystemSound(id)
    }

    // MARK: haptics

    private static let light = UIImpactFeedbackGenerator(style: .light)
    private static let medium = UIImpactFeedbackGenerator(style: .medium)
    private static let rigid = UIImpactFeedbackGenerator(style: .rigid)
    private static let selection = UISelectionFeedbackGenerator()
    private static let notice = UINotificationFeedbackGenerator()

    private static func haptic(_ cue: Cue) {
        switch cue {
        case .press: medium.impactOccurred()
        case .release: light.impactOccurred(intensity: 0.6)
        case .sent: notice.notificationOccurred(.success)
        case .stop: rigid.impactOccurred()
        case .tick, .key: selection.selectionChanged()
        case .pop, .play: light.impactOccurred()
        case .error: notice.notificationOccurred(.error)
        }
    }

    // MARK: sounds

    private static var ids: [Cue: SystemSoundID] = [:]

    private static func soundID(_ cue: Cue) -> SystemSoundID? {
        if let id = ids[cue] { return id }
        guard let notes = score(cue) else { return nil }
        do {
            let url = try file(for: cue, notes: notes)
            var id: SystemSoundID = 0
            guard AudioServicesCreateSystemSoundID(url as CFURL, &id) == kAudioServicesNoError else { return nil }
            ids[cue] = id
            return id
        } catch { return nil }
    }

    /// One cue is a few notes: (hertz at start, hertz at end, seconds, gain).
    private static func score(_ cue: Cue) -> [(Double, Double, Double, Double)]? {
        switch cue {
        case .press: return [(620, 880, 0.06, 0.5)]
        case .release: return nil
        case .sent: return [(880, 880, 0.05, 0.45), (1320, 1320, 0.07, 0.4)]
        case .stop: return [(260, 180, 0.11, 0.6)]
        case .tick: return [(1500, 1500, 0.018, 0.35)]
        case .pop: return [(720, 440, 0.045, 0.45)]
        case .play: return [(660, 660, 0.045, 0.4), (990, 990, 0.045, 0.4), (1320, 1320, 0.08, 0.4)]
        case .key: return [(2300, 1900, 0.014, 0.3)]
        case .error: return [(200, 170, 0.09, 0.55), (170, 150, 0.12, 0.5)]
        }
    }

    private static let rate = 22050.0

    private static func file(for cue: Cue, notes: [(Double, Double, Double, Double)]) throws -> URL {
        let directory = FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0].appendingPathComponent("walkieware-sounds", isDirectory: true)
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        let url = directory.appendingPathComponent(cue.rawValue + "-v1.wav")
        if FileManager.default.fileExists(atPath: url.path) { return url }
        var samples: [Int16] = []
        for (from, to, seconds, gain) in notes {
            let count = Int(seconds * rate)
            var phase = 0.0
            for i in 0..<count {
                let t = Double(i) / Double(count)
                let hz = from + (to - from) * t
                phase += 2 * .pi * hz / rate
                // A sine with a whisper of octave gives the blip some body; a
                // 2 ms attack and an exponential tail keep it from clicking.
                let tone = sin(phase) + 0.18 * sin(2 * phase)
                let attack = min(1, Double(i) / (0.002 * rate))
                let decay = exp(-5.5 * t)
                samples.append(Int16(max(-1, min(1, tone * attack * decay * gain)) * 32767))
            }
            samples.append(contentsOf: [Int16](repeating: 0, count: Int(0.012 * rate)))
        }
        try wav(samples).write(to: url, options: .atomic)
        return url
    }

    private static func wav(_ samples: [Int16]) -> Data {
        var data = Data()
        func put32(_ v: UInt32) { withUnsafeBytes(of: v.littleEndian) { data.append(contentsOf: $0) } }
        func put16(_ v: UInt16) { withUnsafeBytes(of: v.littleEndian) { data.append(contentsOf: $0) } }
        let bytes = UInt32(samples.count * 2)
        data.append(contentsOf: Array("RIFF".utf8)); put32(36 + bytes); data.append(contentsOf: Array("WAVE".utf8))
        data.append(contentsOf: Array("fmt ".utf8)); put32(16); put16(1); put16(1); put32(UInt32(rate)); put32(UInt32(rate) * 2); put16(2); put16(16)
        data.append(contentsOf: Array("data".utf8)); put32(bytes)
        samples.withUnsafeBufferPointer { buffer in
            for sample in buffer { put16(UInt16(bitPattern: sample)) }
        }
        return data
    }
}
