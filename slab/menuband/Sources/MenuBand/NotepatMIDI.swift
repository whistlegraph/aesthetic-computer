import Foundation
import CoreMIDI

// Notepat keyboard layout → semitone offset from middle C (C4 = MIDI 60).
// Mirrors NOTE_TO_KEYBOARD_KEY in system/public/aesthetic.computer/disks/notepat.mjs
//   "-X" = octave below, "+X" = octave above, "++X" = two octaves above.
// Indexed by Carbon kVK_ANSI_* virtual key codes for zero-allocation lookup.
enum Keymap: String { case notepat, ableton }

enum NotepatLayout {
    // Pre-built dense lookup: index = virtual key code (0-127), value = semitone or Int8.min if unmapped.
    static let semitoneByKeyCode: [Int8] = {
        var table = [Int8](repeating: Int8.min, count: 128)
        // (kVK_ANSI_*, semitone offset from middle C)
        let mapping: [(UInt16, Int8)] = [
            (6,  -2),  // Z   -a#
            (7,  -1),  // X   -b
            (8,   0),  // C    c
            (9,   1),  // V    c#
            (2,   2),  // D    d
            (1,   3),  // S    d#
            (14,  4),  // E    e
            (3,   5),  // F    f
            (13,  6),  // W    f#
            (5,   7),  // G    g
            (15,  8),  // R    g#
            (0,   9),  // A    a
            (12, 10),  // Q    a#
            (11, 11),  // B    b
            (4,  12),  // H   +c
            (17, 13),  // T   +c#
            (34, 14),  // I   +d
            (16, 15),  // Y   +d#
            (38, 16),  // J   +e
            (40, 17),  // K   +f
            (32, 18),  // U   +f#
            (37, 19),  // L   +g
            (31, 20),  // O   +g#
            (46, 21),  // M   +a
            (35, 22),  // P   +a#
            (45, 23),  // N   +b
            (41, 24),  // ;  ++c
            (39, 25),  // '  ++c#
            (30, 26),  // ]  ++d
        ]
        for (kc, st) in mapping { table[Int(kc)] = st }
        return table
    }()

    /// Ableton Live's qwerty keymap (M to enable in Live). Home row a..k → C..C
    /// across an octave; w/e/t/y/u → black keys; o/p add another octave.
    static let semitoneByKeyCodeAbleton: [Int8] = {
        var t = [Int8](repeating: Int8.min, count: 128)
        let mapping: [(UInt16, Int8)] = [
            (0,   0),   // A   C
            (13,  1),   // W   C#
            (1,   2),   // S   D
            (14,  3),   // E   D#
            (2,   4),   // D   E
            (3,   5),   // F   F
            (17,  6),   // T   F#
            (5,   7),   // G   G
            (16,  8),   // Y   G#
            (4,   9),   // H   A
            (32, 10),   // U   A#
            (38, 11),   // J   B
            (40, 12),   // K   C+1
            (31, 13),   // O   C#+1
            (37, 14),   // L   D+1
            (35, 15),   // P   D#+1
            (41, 16),   // ;   E+1
        ]
        for (kc, st) in mapping { t[Int(kc)] = st }
        return t
    }()

    @inline(__always)
    static func midiNote(forKeyCode keyCode: UInt16,
                         octaveShift: Int,
                         keymap: Keymap = .notepat) -> UInt8? {
        guard keyCode < 128 else { return nil }
        let table = (keymap == .ableton) ? semitoneByKeyCodeAbleton : semitoneByKeyCode
        let semitone = table[Int(keyCode)]
        if semitone == Int8.min { return nil }
        let value = 60 + Int(semitone) + (octaveShift * 12)
        guard value >= 0, value <= 127 else { return nil }
        return UInt8(value)
    }
}

final class NotepatMIDI {
    private var client: MIDIClientRef = 0
    private var source: MIDIEndpointRef = 0
    private var started = false

    deinit { stop() }

    func start() {
        guard !started else { return }
        let clientName = "Notepat" as CFString
        let status = MIDIClientCreate(clientName, nil, nil, &client)
        guard status == noErr else {
            NSLog("Notepat MIDIClientCreate failed: \(status)")
            return
        }
        let sourceName = "Notepat (slab)" as CFString
        let srcStatus = MIDISourceCreate(client, sourceName, &source)
        guard srcStatus == noErr else {
            NSLog("Notepat MIDISourceCreate failed: \(srcStatus)")
            MIDIClientDispose(client)
            client = 0
            return
        }
        started = true
    }

    func stop() {
        // Best-effort: send all-notes-off so Ableton doesn't hang on stuck notes.
        if started {
            sendAllNotesOff()
            if source != 0 { MIDIEndpointDispose(source) }
            if client != 0 { MIDIClientDispose(client) }
            source = 0
            client = 0
            started = false
        }
    }

    func noteOn(_ note: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard started else { return }
        send([0x90 | (channel & 0x0F), note & 0x7F, velocity & 0x7F])
    }

    func noteOff(_ note: UInt8, channel: UInt8 = 0) {
        guard started else { return }
        send([0x80 | (channel & 0x0F), note & 0x7F, 0])
    }

    /// Send a Control Change message. CC 10 = pan (0=left, 64=center, 127=right).
    func sendCC(_ cc: UInt8, value: UInt8, channel: UInt8 = 0) {
        guard started else { return }
        send([0xB0 | (channel & 0x0F), cc & 0x7F, value & 0x7F])
    }

    func sendAllNotesOff(channel: UInt8 = 0) {
        guard started else { return }
        // CC 123 = All Notes Off
        send([0xB0 | (channel & 0x0F), 123, 0])
    }

    // MARK: - Internal

    private func send(_ bytes: [UInt8]) {
        var packetList = MIDIPacketList()
        let packet = MIDIPacketListInit(&packetList)
        // Timestamp 0 → "deliver as soon as possible, no scheduling." For live
        // play this is lower latency than scheduling against mach_absolute_time().
        bytes.withUnsafeBufferPointer { buf in
            _ = MIDIPacketListAdd(&packetList, MemoryLayout<MIDIPacketList>.size, packet, 0, bytes.count, buf.baseAddress!)
        }
        let status = MIDIReceived(source, &packetList)
        if status != noErr {
            NSLog("Notepat MIDIReceived failed: \(status)")
        }
    }
}
