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

    /// Pan (MIDI 0–127, 64 = center) per QWERTY position. Derived from
    /// notepat native's `getPanForQwertyKey` — physical keyboard
    /// column maps to stereo placement so left-hand keys sit left,
    /// right-hand keys sit right. Rows offset like a real keyboard
    /// (top row no offset, middle row 0.5 col over, bottom row 1 col
    /// over). PAN_RANGE keeps the spread inside ±0.9 so nothing is
    /// hard-panned to a single ear.
    static let panByKeyCode: [UInt8] = {
        var t = [UInt8](repeating: 64, count: 128)
        // (row, col → keyCode mapping, mirrors notepat.mjs QWERTY rows)
        let rows: [[UInt16]] = [
            // Row 0: q w e r t y u i o p ]
            [12, 13, 14, 15, 17, 16, 32, 34, 31, 35, 30],
            // Row 1: a s d f g h j k l ; '
            [0,  1,  2,  3,  5,  4,  38, 40, 37, 41, 39],
            // Row 2: z x c v b n m   (skipping the modifier bookends)
            [6,  7,  8,  9,  11, 45, 46],
        ]
        let rowOffsets: [Double] = [0.0, 0.5, 1.0]
        // Match notepat: MAX_SPAN = max(row.length + offset). Rows are
        // 11, 11.5, 8 here (we drop control / alt vs the JS source) so
        // MAX_SPAN = 11.5.
        let maxSpan: Double = 11.5
        let panRange: Double = 0.9
        for r in 0..<rows.count {
            for (col, kc) in rows[r].enumerated() where kc < 128 {
                let x = Double(col) + rowOffsets[r]
                let normalized = x / (maxSpan - 1)
                let pan = (normalized * 2 - 1) * panRange
                let midi = max(0, min(127, Int((((pan + 1) / 2) * 127.0).rounded())))
                t[Int(kc)] = UInt8(midi)
            }
        }
        return t
    }()

    /// Look up the pan (MIDI 0–127) for a given hardware key code.
    /// Returns 64 (center) for keys that aren't in the QWERTY pan
    /// table (modifiers, function keys, etc.).
    @inline(__always)
    static func panForKeyCode(_ keyCode: UInt16) -> UInt8 {
        guard keyCode < 128 else { return 64 }
        return panByKeyCode[Int(keyCode)]
    }

    /// Display-note (in the menubar piano's clamped C4–C5 window, MIDI
    /// 60–83) that splits the keyboard into a left half (lower octave,
    /// 60–71) and a right half (upper octave, 72–83). Left shift lingers
    /// + uppercases notes below this; right shift, at or above it. A
    /// single value keeps the audio linger decision and the menubar
    /// uppercase-cue in lockstep.
    static let lingerSplitMidi = 72

    /// Hardware key codes used as octave-down / octave-up shifters in
    /// the given keymap. Notepat reserves , (43) and . (47); Ableton
    /// remaps to z (6) and x (7) because those sit unmapped in Live's
    /// M-mode layout (where comma and period are right next to mapped
    /// notes and would steal accidental presses).
    @inline(__always)
    static func octaveKeyCodes(for keymap: Keymap) -> (down: UInt16, up: UInt16) {
        switch keymap {
        case .ableton: return (6, 7)
        case .notepat: return (43, 47)
        }
    }

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

    /// The key's intrinsic semitone offset from middle C (before any octave
    /// shift), or nil if unmapped. C = 0, +C (the right-hand octave) = 12.
    /// The right-hand percussion split uses this: keys at semitone ≥ 12 are
    /// the right hand, and `semitone % 12` picks their drum (octave-
    /// invariant, matching notepat's drum wave).
    @inline(__always)
    static func semitone(forKeyCode keyCode: UInt16,
                         keymap: Keymap = .notepat) -> Int? {
        guard keyCode < 128 else { return nil }
        let table = (keymap == .ableton) ? semitoneByKeyCodeAbleton : semitoneByKeyCode
        let s = table[Int(keyCode)]
        return s == Int8.min ? nil : Int(s)
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
            // Stable identity across launches. Without these, CoreMIDI
            // mints a fresh kMIDIPropertyUniqueID per process; DAWs cache
            // their per-input "Track / Sync / Remote" toggles by UID, so
            // every reinstall or relaunch reads as a new device and the
            // user has to re-enable Track in Live's MIDI prefs to hear
            // notes. Pinning UID + manufacturer + model means Ableton's
            // routing survives reinstalls.
            // UID is a 32-bit signed int.
            //   • 0x4D424E44 "MBND" — original
            //   • 0x4D424E45 "MBNE" — bump for Live 12.3.8 stale cache
            //   • 0x4D424E46 "MBNF" — current; bump for Live 12.4 +
            //                          rebuild-shifted-UID symptom (Live's
            //                          MidiInDevicePreferences entry was
            //                          wedged onto a transient per-process
            //                          UID that CoreMIDI minted before our
            //                          pin landed)
            // Bump again the next time a DAW's per-port cache gets wedged.
            let pinnedUID = Int32(bitPattern: 0x4D424E46)
            let uidStatus = MIDIObjectSetIntegerProperty(source,
                                                         kMIDIPropertyUniqueID,
                                                         pinnedUID)
            if uidStatus != noErr {
                NSLog("MenuBand MIDI: pin UID failed status=\(uidStatus) (CoreMIDI may keep its auto-minted UID)")
            }
            // Read back and log the UID we actually got so a future
            // "Live can't see Menu Band" can be diagnosed straight
            // from /tmp/menuband-debug.log — if this doesn't match
            // pinnedUID, CoreMIDI rejected our pin and Live is
            // tracking a transient ID instead.
            var actualUID: Int32 = 0
            MIDIObjectGetIntegerProperty(source, kMIDIPropertyUniqueID, &actualUID)
            NSLog("MenuBand MIDI: virtual source UID actual=0x\(String(actualUID, radix: 16)) pinned=0x\(String(pinnedUID, radix: 16))")
            MIDIObjectSetStringProperty(source, kMIDIPropertyManufacturer,
                                        "aesthetic.computer" as CFString)
            MIDIObjectSetStringProperty(source, kMIDIPropertyModel,
                                        "Menu Band" as CFString)
            // Mark the source as a hardware-style endpoint that Live
            // surfaces in its MIDI prefs panel under "Input". Without
            // these, Live treats the source as "generic" and hides
            // it behind the "Show Hidden" toggle, which the user
            // typically doesn't think to check.
            MIDIObjectSetIntegerProperty(source, kMIDIPropertyTransmitsNotes, 1)
            MIDIObjectSetIntegerProperty(source, kMIDIPropertyTransmitsProgramChanges, 1)
            MIDIObjectSetIntegerProperty(source, kMIDIPropertyMaxTransmitChannels, 16)
            // Explicitly visible — CoreMIDI defaults to non-hidden,
            // but a misconfigured DAW filter can read a missing
            // property as "use the cached/default" and Live's been
            // observed to hide undeclared sources. Setting Hidden=0
            // forces it into the visible list. Same logic for
            // Private/Offline which sometimes get inherited from
            // the (nil) Device parent.
            MIDIObjectSetIntegerProperty(source, kMIDIPropertyPrivate, 0)
            MIDIObjectSetIntegerProperty(source, kMIDIPropertyOffline, 0)
            // Embedded-entity name so Live's display reads "Menu Band"
            // and not "Untitled Source" / "Menu Band - port 1".
            MIDIObjectSetStringProperty(source, kMIDIPropertyDisplayName,
                                        "Menu Band" as CFString)
            started = true
            debugLog("midi.start: virtual source published")
        }
        enabled = true
        debugLog("midi.start: enabled")
        // Announce a 12-semitone pitch-bend range on every melodic
        // channel via RPN 0 (Pitch Bend Sensitivity). The trackpad
        // accumulator scales ±1 = ±12 semitones; without this the
        // receiver (Ableton et al.) interprets our full-scale 14-bit
        // bend as ±2 semitones, so dragging across an octave on the
        // trackpad sounded like a quarter-tone wobble in Live.
        for ch: UInt8 in 0..<16 {
            send([0xB0 | ch, 101, 0])    // RPN MSB = 0
            send([0xB0 | ch, 100, 0])    // RPN LSB = 0  (Pitch Bend Sensitivity)
            send([0xB0 | ch, 6, 12])     // Data Entry MSB = 12 semitones
            send([0xB0 | ch, 38, 0])     // Data Entry LSB = 0 cents
            send([0xB0 | ch, 101, 127])  // RPN Null (MSB)
            send([0xB0 | ch, 100, 127])  // RPN Null (LSB)
        }
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

    /// Send a Program Change so a connected DAW retargets its instrument
    /// when the user picks a new GM voice in the popover. The endpoint
    /// advertises `kMIDIPropertyTransmitsProgramChanges`, but without
    /// this method nothing actually emits the PC — the DAW track stays
    /// on whatever instrument it was last assigned and the local synth
    /// voice silently drifts out of sync with what's recording.
    /// Optionally precede the PC with a Bank Select MSB+LSB pair so
    /// receivers that respect bank routing (e.g. multi-bank ROMplers)
    /// land on the right program — defaults map to the GM Melodic bank.
    func sendProgramChange(_ program: UInt8,
                           channel: UInt8 = 0,
                           bankMSB: UInt8? = nil,
                           bankLSB: UInt8? = nil) {
        guard enabled else { return }
        let ch = channel & 0x0F
        if let msb = bankMSB {
            send([0xB0 | ch, 0,  msb & 0x7F])
        }
        if let lsb = bankLSB {
            send([0xB0 | ch, 32, lsb & 0x7F])
        }
        send([0xC0 | ch, program & 0x7F])
    }

    /// Send a 14-bit pitch-bend message. `value` is signed:
    /// -8192 (full down) … 0 (center) … +8191 (full up). Default
    /// MIDI bend range is ±2 semitones; receivers can configure
    /// wider ranges via RPN if needed.
    func sendPitchBend(value: Int16, channel: UInt8 = 0) {
        guard enabled else { return }
        let v = max(-8192, min(8191, Int(value))) + 8192
        send([0xE0 | (channel & 0x0F), UInt8(v & 0x7F), UInt8((v >> 7) & 0x7F)])
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
        // CRITICAL — three things have to be right for MIDIPacketListAdd
        // to actually insert the packet (it silently no-ops on failure
        // and Live then sees the source but never receives notes):
        //   • `var packet` (not `let`) — must be reassigned from the
        //     return of MIDIPacketListAdd, which advances the cursor.
        //     With `let` the second call (or even the first under some
        //     Swift import paths) writes onto the wrong slot.
        //   • Pass a real buffer size (1024) for `listSize`, not
        //     `MemoryLayout<MIDIPacketList>.size`. The latter is the
        //     Swift-reported struct stride and CoreMIDI compared it
        //     against the bytes needed for the packet INCLUDING its
        //     header, often rounding our struct-sized value below
        //     threshold and rejecting the add.
        //   • `mach_absolute_time()` as the timestamp. `0` is documented
        //     as "now" but CoreMIDI on macOS 14+ treats it as past-due
        //     in some routing contexts (notably Live's MIDI input
        //     thread) and drops the packet entirely.
        // This three-bug combo was the entire reason Menu Band's
        // notes reached the source but never reached Live's track.
        var packetList = MIDIPacketList()
        var packet = MIDIPacketListInit(&packetList)
        bytes.withUnsafeBufferPointer { buf in
            packet = MIDIPacketListAdd(&packetList, 1024, packet,
                                       mach_absolute_time(),
                                       bytes.count, buf.baseAddress!)
        }
        let status = MIDIReceived(source, &packetList)
        if status != noErr {
            NSLog("MenuBand MIDIReceived failed: \(status)")
        }
    }
}
