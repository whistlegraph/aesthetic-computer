import Foundation

/// Minimal Standard MIDI File (format 0) writer — turns a take's captured note
/// events into a `.mid` that Ableton (or any DAW) can drop onto a MIDI track
/// to re-trigger the performance. Fixed 120 BPM / 480 PPQ; event times are
/// real seconds converted to ticks, so the timing matches what was played.
enum MidiFile {
    struct Event {
        var time: TimeInterval   // seconds from the first note
        var note: UInt8
        var velocity: UInt8
        var on: Bool
        var channel: UInt8
    }

    private static let ppq = 480
    private static let ticksPerSecond = Double(ppq) * 2.0   // 120 BPM → 0.5 s/qn

    /// Build the `.mid` bytes. Times are normalized so the first event is 0.
    static func data(events: [Event]) -> Data {
        var track = Data()

        // Tempo meta (500000 µs/quarter = 120 BPM) at delta 0.
        track.append(contentsOf: [0x00, 0xFF, 0x51, 0x03, 0x07, 0xA1, 0x20])

        let sorted = events.sorted { $0.time < $1.time }
        let base = sorted.first?.time ?? 0
        var lastTick = 0
        for e in sorted {
            let tick = Int(((e.time - base) * ticksPerSecond).rounded())
            let delta = max(0, tick - lastTick)
            lastTick = tick
            track.append(varLen(delta))
            let status: UInt8 = (e.on ? 0x90 : 0x80) | (e.channel & 0x0F)
            track.append(contentsOf: [status, e.note & 0x7F, e.velocity & 0x7F])
        }
        // End of track.
        track.append(contentsOf: [0x00, 0xFF, 0x2F, 0x00])

        var out = Data()
        // MThd — format 0, 1 track, division = PPQ.
        out.append(contentsOf: Array("MThd".utf8))
        out.append(be32(6))
        out.append(be16(0))
        out.append(be16(1))
        out.append(be16(UInt16(ppq)))
        // MTrk.
        out.append(contentsOf: Array("MTrk".utf8))
        out.append(be32(UInt32(track.count)))
        out.append(track)
        return out
    }

    // MIDI variable-length quantity (7 bits/byte, high bit = continue).
    private static func varLen(_ value: Int) -> Data {
        var v = UInt32(max(0, value))
        var bytes = [UInt8(v & 0x7F)]
        v >>= 7
        while v > 0 {
            bytes.insert(UInt8((v & 0x7F) | 0x80), at: 0)
            v >>= 7
        }
        return Data(bytes)
    }

    private static func be16(_ v: UInt16) -> Data { Data([UInt8(v >> 8), UInt8(v & 0xFF)]) }
    private static func be32(_ v: UInt32) -> Data {
        Data([UInt8(v >> 24), UInt8((v >> 16) & 0xFF), UInt8((v >> 8) & 0xFF), UInt8(v & 0xFF)])
    }
}
