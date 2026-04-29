import Foundation
import CoreMIDI

// MenuBand keyboard layout → semitone offset from middle C (C4 = MIDI 60).
// Mirrors NOTE_TO_KEYBOARD_KEY in system/public/aesthetic.computer/disks/notepat.mjs
//   "-X" = octave below, "+X" = octave above, "++X" = two octaves above.
// Indexed by Carbon kVK_ANSI_* virtual key codes for zero-allocation lookup.
// Keymap raw values are persisted in UserDefaults — keep the `notepat` raw
// value stable so existing settings continue to load.
enum Keymap: String { case notepat, ableton }

enum MenuBandLayout {
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

final class MenuBandMIDI {
    private var client: MIDIClientRef = 0
    private var source: MIDIEndpointRef = 0
    // `started` = virtual port is published (lifetime bit; once true, stays
    // true until process exit). `enabled` = outbound sends allowed (the
    // toggle). Splitting these is what makes the MIDI on/off toggle safe:
    // disposing the virtual source at runtime while a DAW holds a connection
    // historically hung the main thread inside MIDIEndpointDispose /
    // MIDIClientDispose (see /tmp/menuband-debug.log — process died right
    // after "disableMIDIMode: calling midi.stop()"). Keeping the port alive
    // for the app's lifetime sidesteps that path entirely; the OS reclaims
    // it on process exit.
    private var started = false
    private var enabled = false
    private var loopbackPort: MIDIPortRef = 0
    private var loopbackHandler: ((UInt8, UInt8) -> Void)?

    deinit {
        // Don't dispose CoreMIDI here — see the `started`/`enabled` comment.
        // Best we can do is flush any held notes so DAWs don't get stuck.
        if enabled { sendAllNotesOff() }
    }

    /// Subscribe to our own virtual source from the same process. The
    /// callback fires for each noteOn (status 0x9X) — used by the
    /// controller to verify the port works without a DAW in the loop.
    func startLoopback(onNoteOn: @escaping (UInt8, UInt8) -> Void) {
        guard started else {
            debugLog("startLoopback skipped — MIDI not started")
            return
        }
        loopbackHandler = onNoteOn
        if loopbackPort == 0 {
            let st = MIDIInputPortCreateWithBlock(client, "loopback" as CFString, &loopbackPort) { [weak self] listPtr, _ in
                let pkt = listPtr.pointee
                var p = pkt.packet
                for _ in 0..<pkt.numPackets {
                    let len = Int(p.length)
                    var bytes: [UInt8] = []
                    withUnsafePointer(to: &p.data) { ptr in
                        ptr.withMemoryRebound(to: UInt8.self, capacity: len) { b in
                            for i in 0..<len { bytes.append(b[i]) }
                        }
                    }
                    if bytes.count >= 3, (bytes[0] & 0xF0) == 0x90, bytes[2] != 0 {
                        self?.loopbackHandler?(bytes[1], bytes[2])
                    }
                    p = MIDIPacketNext(&p).pointee
                }
            }
            if st != noErr {
                debugLog("MIDIInputPortCreateWithBlock failed: \(st)")
                return
            }
        }
        let cstat = MIDIPortConnectSource(loopbackPort, source, nil)
        if cstat != noErr {
            debugLog("MIDIPortConnectSource failed: \(cstat)")
        } else {
            debugLog("startLoopback connected")
        }
    }

    func stopLoopback() {
        // Belt-and-suspenders: only disconnect if both port and source are
        // still live. The port is owned by the client and dies with it, so
        // calling MIDIPortDisconnectSource on a stale handle crashes.
        if loopbackPort != 0, source != 0, started {
            MIDIPortDisconnectSource(loopbackPort, source)
        }
        loopbackHandler = nil
    }

    /// Idempotent: publishes the virtual source on first call, then enables
    /// outbound sends. Subsequent calls just re-enable. Pairs with `stop()`,
    /// which only flips `enabled` — the port stays published for the app's
    /// lifetime.
    func start() {
        if !started {
            let clientName = "Menu Band" as CFString
            let status = MIDIClientCreate(clientName, nil, nil, &client)
            guard status == noErr else {
                NSLog("MenuBand MIDIClientCreate failed: \(status)")
                return
            }
            let sourceName = "Menu Band" as CFString
            let srcStatus = MIDISourceCreate(client, sourceName, &source)
            guard srcStatus == noErr else {
                NSLog("MenuBand MIDISourceCreate failed: \(srcStatus)")
                MIDIClientDispose(client)
                client = 0
                return
            }
            started = true
            debugLog("midi.start: virtual source published")
        }
        enabled = true
        debugLog("midi.start: enabled")
    }

    /// Disable outbound sends and flush stuck notes. Does NOT dispose the
    /// virtual source — runtime CoreMIDI teardown is the bug we're avoiding.
    func stop() {
        guard started else { return }
        sendAllNotesOff()
        enabled = false
        debugLog("midi.stop: disabled (port stays published)")
    }

    func noteOn(_ note: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard enabled else {
            debugLog("midi.noteOn dropped (enabled=false) note=\(note)")
            return
        }
        debugLog("midi.noteOn note=\(note) ch=\(channel)")
        send([0x90 | (channel & 0x0F), note & 0x7F, velocity & 0x7F])
    }

    func noteOff(_ note: UInt8, channel: UInt8 = 0) {
        guard enabled else { return }
        send([0x80 | (channel & 0x0F), note & 0x7F, 0])
    }

    /// Send a Control Change message. CC 10 = pan (0=left, 64=center, 127=right).
    func sendCC(_ cc: UInt8, value: UInt8, channel: UInt8 = 0) {
        guard enabled else { return }
        send([0xB0 | (channel & 0x0F), cc & 0x7F, value & 0x7F])
    }

    /// Emergency flush — gated on the published port (`started`), not the
    /// toggle (`enabled`), so `stop()` can call it after flipping `enabled`
    /// off and still get the CC#123 out.
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
            NSLog("MenuBand MIDIReceived failed: \(status)")
        }
    }
}
