import AVFoundation
import Foundation

/// Small, deterministic records for learning the physical behavior of a deck.
///
/// Primpats are rendered locally and contain no samples, network requests, or
/// user data. Each eight-second record contains a whole number of cycles so a
/// looping deck can cross its boundary without a phase discontinuity.
enum DJPrimpats {
    enum Waveform: String, CaseIterable, Codable {
        case sine
    }

    struct Metadata: Hashable {
        let id: String
        let title: String
        let frequency: Double
        let waveform: Waveform
        let duration: Double
        let bpm: Int
        let key: String
    }

    struct Record {
        let track: Track
        let metadata: Metadata

        var title: String { metadata.title }
        var frequency: Double { metadata.frequency }
        var waveform: Waveform { metadata.waveform }
    }

    private static let sampleRate = 48_000.0
    private static let amplitude: Float = 0.55
    private static let cacheVersion = "v3"
    private static let renderLock = NSLock()

    /// One octave of white piano keys. Rendering chooses an imperceptibly
    /// adjusted sample-exact frequency so every eight-second record loops on a
    /// zero-phase boundary while retaining the equal-tempered musical pitch.
    static let catalog: [Metadata] = [
        metadata(id: "sine-c4", frequency: 261.6256, key: "C4"),
        metadata(id: "sine-d4", frequency: 293.6648, key: "D4"),
        metadata(id: "sine-e4", frequency: 329.6276, key: "E4"),
        metadata(id: "sine-f4", frequency: 349.2282, key: "F4"),
        metadata(id: "sine-g4", frequency: 391.9954, key: "G4"),
        metadata(id: "sine-a4", frequency: 440.0000, key: "A4"),
        metadata(id: "sine-b4", frequency: 493.8833, key: "B4"),
        metadata(id: "sine-c5", frequency: 523.2511, key: "C5"),
    ]

    /// Render the catalog as scratch-ready Tracks. Failed cache entries are
    /// omitted so one unwritable file never prevents the remaining records.
    static func make() -> [Record] {
        catalog.compactMap { metadata in
            guard let url = render(metadata) else { return nil }
            let track = Track(url: url, lane: "primpats", title: metadata.title)
            track.meta = TrackMeta(
                artist: "Menu Band Juke",
                backend: "Primpats local \(metadata.waveform.rawValue) synthesis · \(frequencyLabel(metadata.frequency)) Hz",
                status: "PRIMPAT",
                updated: nil,
                revisions: nil,
                bytes: fileSize(at: url),
                durationSec: metadata.duration,
                bpm: metadata.bpm,
                key: metadata.key,
                releaseDate: nil,
                art: nil,
                media: nil,
                links: nil
            )
            return Record(track: track, metadata: metadata)
        }
    }

    /// Convenience API for consumers that only need the existing Track type.
    static func makeTracks() -> [Track] {
        make().map(\.track)
    }

    /// Recover primpat metadata after a Track has passed through a deck queue.
    static func metadata(for track: Track) -> Metadata? {
        let id = track.url.deletingPathExtension().lastPathComponent
        return catalog.first { $0.id == id }
    }

    private static func metadata(id: String, frequency: Double, key: String) -> Metadata {
        Metadata(
            id: id,
            title: "Primpats · \(key) · Sine \(frequencyLabel(frequency)) Hz",
            frequency: frequency,
            waveform: .sine,
            duration: 8,
            bpm: 120,
            key: key
        )
    }

    private static func frequencyLabel(_ frequency: Double) -> String {
        let hundredths = Int((frequency * 100).rounded())
        if hundredths.isMultiple(of: 100) { return String(hundredths / 100) }
        if hundredths.isMultiple(of: 10) { return "\(hundredths / 100).\((hundredths % 100) / 10)" }
        return "\(hundredths / 100).\(String(format: "%02d", hundredths % 100))"
    }

    private static func render(_ metadata: Metadata) -> URL? {
        renderLock.lock()
        defer { renderLock.unlock() }

        let fm = FileManager.default
        guard let caches = fm.urls(for: .cachesDirectory, in: .userDomainMask).first else { return nil }
        let directory = caches
            .appendingPathComponent("computer.aesthetic.jukewizard", isDirectory: true)
            .appendingPathComponent("primpats", isDirectory: true)
            .appendingPathComponent(cacheVersion, isDirectory: true)
        do {
            try fm.createDirectory(at: directory, withIntermediateDirectories: true)
        } catch {
            return nil
        }

        let url = directory.appendingPathComponent(metadata.id).appendingPathExtension("wav")
        let expectedFrames = AVAudioFramePosition(sampleRate * metadata.duration)
        if audioIsUsable(at: url, expectedFrames: expectedFrames) { return url }
        if fm.fileExists(atPath: url.path) { try? fm.removeItem(at: url) }

        let temporaryURL = directory
            .appendingPathComponent(".\(metadata.id)-rendering")
            .appendingPathExtension("wav")
        if fm.fileExists(atPath: temporaryURL.path) { try? fm.removeItem(at: temporaryURL) }

        let frames = Int(expectedFrames)
        let cycles = max(1, Int((metadata.frequency * metadata.duration).rounded()))
        let renderedFrequency = Double(cycles) * sampleRate / Double(frames)
        guard let format = AVAudioFormat(
            commonFormat: .pcmFormatFloat32,
            sampleRate: sampleRate,
            channels: 2,
            interleaved: false
        ), let buffer = AVAudioPCMBuffer(
            pcmFormat: format,
            frameCapacity: AVAudioFrameCount(frames)
        ), let channels = buffer.floatChannelData else { return nil }

        buffer.frameLength = AVAudioFrameCount(frames)
        let radiansPerFrame = 2 * Double.pi * renderedFrequency / sampleRate
        for frame in 0..<frames {
            let sample = amplitude * Float(sin(Double(frame) * radiansPerFrame))
            channels[0][frame] = sample
            channels[1][frame] = sample
        }

        do {
            var settings = format.settings
            settings[AVLinearPCMIsNonInterleaved] = false
            do {
                let file = try AVAudioFile(forWriting: temporaryURL, settings: settings)
                try file.write(from: buffer)
            }
            try fm.moveItem(at: temporaryURL, to: url)
            guard audioIsUsable(at: url, expectedFrames: expectedFrames) else {
                try? fm.removeItem(at: url)
                return nil
            }
            return url
        } catch {
            try? fm.removeItem(at: temporaryURL)
            try? fm.removeItem(at: url)
            return nil
        }
    }

    private static func audioIsUsable(at url: URL, expectedFrames: AVAudioFramePosition) -> Bool {
        guard let file = try? AVAudioFile(forReading: url) else { return false }
        return file.length == expectedFrames && file.processingFormat.channelCount == 2
    }

    private static func fileSize(at url: URL) -> Int? {
        let values = try? url.resourceValues(forKeys: [.fileSizeKey])
        return values?.fileSize
    }
}
