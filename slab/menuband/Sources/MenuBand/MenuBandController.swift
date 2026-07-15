import AppKit
import ApplicationServices
import CoreGraphics


extension Float {
    /// Multiplies while capping infinite overflow to the nearest finite Float magnitude.
    func finiteMagnitudeMultiply(_ value: Float) -> Float {
        let product = self * value

        if product == .infinity {
            return .greatestFiniteMagnitude
        }

        if product == -.infinity {
            return -.greatestFiniteMagnitude
        }
        
        return product
    }
    
    /// Restricts the value to the provided lower and upper bounds.
    func safeClamped(_ minValue: Float, _ maxValue: Float) -> Float {
        min(max(self, minValue), maxValue)
    }
}

final class MenuBandController {
    private let midi = MenuBandMIDI()
    private let synth = MenuBandSynth()
    /// 90-second tape that captures synth output + mic together. Wired
    /// up in `bootstrap` after `synth.start()` so the player node has
    /// a running engine to attach to.
    let tape = MenuBandTape()
    private let tapeMicPinReason = "tape-record"
    private let tapeWaveformPinReason = "tape"
    private var keyTap: KeyEventTap?
    private var heldNotes: [UInt16: UInt8] = [:]
    /// Chord EXTENSION voices for a held key, keyed by interval (semitones
    /// above the root) → voice. The root itself (interval 0) always lives in
    /// `heldNotes`/`heldKeyChannel` and is NEVER stored here, so a live chord
    /// morph only ever adds / removes / swaps these extension tones — the root
    /// keeps sounding untouched. Because the 5th (interval 7) is shared by
    /// major/minor/sus, morphing between those qualities re-voices ONLY the
    /// third (4 → 3 → 2); the root and 5th sustain through the change, so it
    /// reads as the chord swapping its third, not a retrigger of the whole
    /// stack. The matching key-up releases the root (normal single-note path)
    /// plus every extension here together (lingering like Shift when armed).
    private var morphExt: [UInt16: [Int: (note: UInt8, channel: UInt8, display: UInt8)]] = [:]
    /// Synth channel each held keystroke is voicing on. Mirrors
    /// `tapNoteChannel` for the menubar-tap path: round-robin across
    /// melodic channels 0–7 lets fast same-key retriggers overlap as
    /// distinct voices instead of voice-stealing on channel 0. Same lock.
    private var heldKeyChannel: [UInt16: UInt8] = [:]
    /// Display-note (clamped into the menubar piano's C4–C5 range) for
    /// each held key. Lets keyUp remove the visually-lit cell even when
    /// the audio note was octave-shifted out of the visible range.
    private var heldKeyDisplayNote: [UInt16: UInt8] = [:]
    /// Per-key linger flag captured at keyDown. We sample shift state
    /// once on press so a release-shift-mid-hold still rings out the
    /// note that was started under shift.
    private var heldKeyLinger: [UInt16: Bool] = [:]
    /// Live chord-morph bookkeeping. While a note key is physically held,
    /// pressing / releasing ⌘ / ⌥ re-voices that key between a single note
    /// and a triad WITHOUT a fresh keystroke — the morph is driven by
    /// `.flagsChanged` (see `morphHeldKeys`), so it works the same in TYPE
    /// mode and quiet-focus. We capture the root pitch + octave shift + the
    /// user's linger intent ONCE at press; the morph re-voices from those so
    /// the chord stays anchored to the originally-pressed key no matter how
    /// the octave shifts mid-hold. `morphQuality` is the CURRENT sounding
    /// shape (0 = single, 1 = major, 2 = minor, 3 = sus) — compared against
    /// the modifier-derived desired shape so an idempotent flagsChange (e.g.
    /// Shift toggling) doesn't needlessly retrigger.
    private var morphRoot: [UInt16: UInt8] = [:]
    private var morphShift: [UInt16: Int] = [:]
    private var morphLinger: [UInt16: Bool] = [:]
    private var morphQuality: [UInt16: Int] = [:]
    /// Enter-latch (port of notepat-native): while Return is held, any
    /// note pressed is *latched* — its key-up is swallowed so the note
    /// sustains indefinitely. Backspace pops the most-recent latched
    /// note. Latched notes stay in `heldNotes`/`litNotes`, so the
    /// menubar keyboard visualizer shows them held with no extra wiring.
    private var enterLatchHeld: Bool = false
    /// Keys pressed during the current Enter-hold whose key-up should
    /// latch (not release) the note.
    private var latchArmedKeys: Set<UInt16> = []

    /// Key codes currently held by a POINTER (click/drag on a QWERTY keycap),
    /// not by a physical key. They live in `heldNotes` like any other held key
    /// — they play the same notes — but they must never count as *keyboard*
    /// notes. See `keyboardNotesHeld`: counting them arms the trackpad
    /// pitch-bend, which calls `CGAssociateMouseAndMouseCursorPosition(0)` and
    /// pins the cursor, so the drag you are currently making across the caps
    /// dies under your hand. Same shape as `latchArmedKeys` — in `heldNotes`,
    /// deliberately not "physically held".
    private var pointerHeldKeys: Set<UInt16> = []
    /// LIFO stack of currently-latched keyCodes — Backspace unlatches
    /// the top one.
    private var latchedKeys: [UInt16] = []
    /// Percussion-split: which drum-voice group each held right-hand key
    /// started, so key-up releases exactly that hit (hi-hat down/up sounds).
    private var heldDrumKeys: [UInt16: UInt64] = [:]
    /// Display note each held drum key lit, so key-up extinguishes the
    /// right menubar cell (drum keys stay accent-lit for the whole hold).
    private var heldDrumDisplay: [UInt16: UInt8] = [:]
    private let heldLock = NSLock()

    /// Refcount of held drum lights per display note (a display note can be
    /// held by more than one source — keyboard + click). Main-thread only.
    private var drumLitRefs: [UInt8: Int] = [:]

    /// Which shift key (if any) is arming linger, and over which half of
    /// the keyboard. Left shift lingers only the left half (lower octave,
    /// notes < `lingerSplitMidi`); right shift only the right half. Caps
    /// lock — or both shifts at once — latches the whole board. Each note
    /// still pans by its own column, so a left-half note naturally rings
    /// out to the left without any forced pan.
    enum LingerSide {
        case none      // not lingering
        case left      // left shift  → linger left half only
        case right     // right shift → linger right half only
        case neutral   // caps lock / both shifts → linger whole board

        var isLingering: Bool { self != .none }
    }

    // Device-dependent modifier bits (IOLLEvent / NSEvent legacy). These
    // distinguish the physical left vs right shift — the device-independent
    // `.shift` mask collapses both into one bit. Present in the raw value
    // of both CGEventFlags and NSEvent.ModifierFlags.
    private static let kLeftShiftBit: UInt64 = 0x0000_0002
    private static let kRightShiftBit: UInt64 = 0x0000_0004

    /// Resolve which shift side is arming linger from the raw modifier
    /// bits. Falls back to `.neutral` when shift/caps is on but the
    /// device bits are unavailable, so behavior degrades to the old
    /// natural-pan linger rather than dropping the hold entirely.
    private static func lingerSide(rawFlags: UInt64,
                                   shiftDown: Bool,
                                   capsOn: Bool) -> LingerSide {
        let left = (rawFlags & kLeftShiftBit) != 0
        let right = (rawFlags & kRightShiftBit) != 0
        if left && !right { return .left }
        if right && !left { return .right }
        if shiftDown || capsOn { return .neutral }
        return .none
    }

    private let midiModeKey = "notepat.midiMode"
    private let typeModeKey = "notepat.typeMode"

    /// True while ~ (Shift+`) is held — the next note key records a per-key
    /// sample into that key instead of playing it.
    private var perKeySampleArmed = false
    /// The MIDI note currently capturing a per-key sample (nil = none).
    private var perKeySampleRecordingMidi: UInt8? = nil
    private let octaveShiftKey = "notepat.octaveShift"
    private let melodicProgramKey = "notepat.melodicProgram"
    private let keymapKey = "notepat.keymap"
    private let percussionLeftKey = KeyboardIconRenderer.percussionLeftDefaultsKey
    private let percussionRightKey = KeyboardIconRenderer.percussionRightDefaultsKey
    private let masterVolumeKey = "notepat.masterVolume"
    /// Active instrument backend: `"gm"` for the General MIDI bank, or
    /// `"gb"` for a GarageBand sampler patch. Default is GM. Stored as a
    /// string so future backends (Logic, EXS3rd-party, etc.) can be
    /// added without breaking older saved values.
    private let instrumentBackendKey = "notepat.instrumentBackend"
    private let radioStationKey = "notepat.radioStation"
    /// File URL string of the GarageBand patch the user picked. Empty
    /// when no GB patch has been selected yet (we'll fall back to the
    /// first scanned patch when the backend is GarageBand and this is
    /// missing).
    private let garageBandPatchPathKey = "notepat.garageBandPatchPath"

    // Visual state — accessed only on the main thread.
    private(set) var litNotes: Set<UInt8> = []
    /// Notes currently lit because they're being auto-played from a
    /// dragged-in PDF. Rendered in red by KeyboardIconRenderer to
    /// distinguish "the score is playing me back" from "I'm pressing
    /// keys right now." Disjoint from `litNotes` because the source
    /// of truth differs (synth callback vs user input pipeline).
    private(set) var playbackLitNotes: Set<UInt8> = []
    private var litDownAt: [UInt8: CFTimeInterval] = [:]
    private var playbackChannel: [UInt8: UInt8] = [:]
    private let minVisibleSeconds: CFTimeInterval = 0.18

    var onChange: (() -> Void)?
    var onOctaveLimitNudge: ((Int) -> Void)?
    var onLitChanged: (() -> Void)?
    var onInstrumentVisualChange: (() -> Void)?
    private(set) var sampleInputLevel: Float = 0

    /// Last MIDI note actually played (mouse tap or keyboard). Used by
    /// the instrument preview / audition path so the "test" note that
    /// plays when picking a voice matches whatever the user last
    /// touched, instead of always defaulting to middle C.
    /// Defaults to 60 (C4) on a fresh session.
    private(set) var lastPlayedNote: UInt8 = 60

    /// Format a MIDI note as the user's preferred name pattern —
    /// "<octave><pitch class>" like 4C, 5D#, 3G. C4 (MIDI 60) is the
    /// reference octave.
    static func noteName(_ midi: UInt8) -> String {
        let pitches = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
        let octave = Int(midi) / 12 - 1
        let pc = Int(midi) % 12
        return "\(octave)\(pitches[pc])"
    }

    /// Snapshot of hardware key codes currently held. Used by the
    /// popover's QWERTY layout view to highlight which physical
    /// keys are sounding right now. Includes both note-mapped
    /// keys (heldNotes) and control keys like the digit row /
    /// octave keys (heldControlKeys) so the keymap visualization
    /// reflects every kind of key the user is pressing.
    func heldKeyCodes() -> Set<UInt16> {
        if let posed = captureHeldKeyCodes { return posed }
        heldLock.lock(); defer { heldLock.unlock() }
        return Set(heldNotes.keys).union(heldControlKeys)
    }

    /// Key codes a promo capture has posed as "held". nil in the shipping app,
    /// where held state comes from the keyboard.
    private var captureHeldKeyCodes: Set<UInt16>?

    /// Pose the controller as though `notes` were being held, so a headless
    /// render lights the big piano AND the QWERTY map from a score instead of
    /// a live keyboard. The key codes are the keymap's own inverse — whichever
    /// physical key would have sounded that note. `spaceHeld` lights the space
    /// bar too, for the reverse-playback demo.
    func captureHold(notes: Set<UInt8>, spaceHeld: Bool = false) {
        litNotes = notes
        var codes = Set<UInt16>()
        for keyCode in UInt16(0)..<128 {
            if let midi = MenuBandLayout.midiNote(forKeyCode: keyCode,
                                                 octaveShift: octaveShift,
                                                 keymap: keymap), notes.contains(midi) {
                codes.insert(keyCode)
            }
        }
        if spaceHeld { codes.insert(49) }   // kVK_Space
        captureHeldKeyCodes = codes
    }

    /// Pose the spacebar reverse-replay state for a headless render — the scope
    /// draws its frozen columns with an orange playhead when `isRewinding`.
    func captureReverse(_ active: Bool) { isRewinding = active }

    /// Hardware key codes for non-note keys we want to light up
    /// on the QWERTY visualization (digits 0-9, octave keys, the
    /// backtick sample-record key, etc.).
    private var heldControlKeys: Set<UInt16> = []

    /// True when at least one note is sounding from a hardware
    /// keypress. Mouse taps on the menubar piano use `tapHeld`,
    /// which is intentionally excluded — the pitch-bend cursor
    /// lock + trackpad-Y bending are keyboard-only features so
    /// the user can still drag the mouse across menubar keys.
    var keyboardNotesHeld: Bool {
        heldLock.lock(); defer { heldLock.unlock() }
        // Latched notes (Enter-latch) live in `heldNotes` but their
        // physical key is UP — they must NOT count as "held" for the
        // trackpad pitch-bend lock, or the bend cursor would stay
        // locked open forever after a latch. Count only keys whose
        // physical key is still down (i.e. not latch-armed).
        // Pointer-held caps are excluded for the same reason: the QWERTY map
        // routes clicks through the keyboard path so they play identical
        // notes, but a mouse-driven note must not lock the cursor — that is
        // the very drag the user is in the middle of.
        let melodicHeld = heldNotes.keys.contains {
            !latchArmedKeys.contains($0) && !pointerHeldKeys.contains($0)
        }
        // Held percussion keys count too — drumming engages the same
        // trackpad pitch-bend gesture so the kit can be bent live.
        let drumHeld = heldDrumKeys.keys.contains { !pointerHeldKeys.contains($0) }
        return melodicHeld || drumHeld
    }

    /// Fires whenever an outbound MIDI noteOn lands on the
    /// virtual MIDI source. AppDelegate uses this to flash the
    /// Ableton-style square activity indicator in the chip.
    var onMIDIEvent: (() -> Void)?

    /// Internal helper — wraps midi.noteOn with the activity hook.
    fileprivate func midiNoteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        self.midi.noteOn(midi, velocity: velocity, channel: channel)
        let cb = onMIDIEvent
        if Thread.isMainThread { cb?() } else { DispatchQueue.main.async { cb?() } }
    }

    /// Snapshot of currently-held MIDI note names for popover display.
    /// Empty when nothing is sounding.
    func heldNoteNames() -> [String] {
        let sorted = litNotes.sorted()
        return sorted.map { Self.noteName($0) }
    }

    /// Held-note entries for the popover / panel display: each entry
    /// carries the bare pitch-class name (no octave) plus the
    /// keyboard key letter that's mapped to it under the active
    /// keymap. Empty when nothing is sounding.
    struct HeldNoteEntry {
        let midi: UInt8
        let pitchClass: String
        let keyLabel: String?
    }

    func heldNoteEntries() -> [HeldNoteEntry] {
        let pitches = Self.pitchClassNames
        let labels = KeyboardIconRenderer.labelByMidi
        return litNotes.sorted().map { midi in
            let pc = Int(midi) % 12
            return HeldNoteEntry(
                midi: midi,
                pitchClass: pitches[pc],
                keyLabel: labels[Int(midi)]
            )
        }
    }

    /// One possible chord matching the currently held notes. `missing`
    /// lists pitch classes (0..11) the user still needs to add to
    /// finish the chord; empty when the chord is fully held.
    struct ChordCandidate {
        let name: String
        let rootPitchClass: Int
        let pitchClasses: Set<Int>
        let missingPitchClasses: [Int]
        let missingNoteNames: [String]
        var isComplete: Bool { missingPitchClasses.isEmpty }
    }

    /// Pitch-class names indexed 0=C..11=B. Shared across the chord
    /// readout APIs so display strings stay consistent.
    static let pitchClassNames = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

    /// Chord-pattern table — intervals from root + display suffix.
    /// Covers common triads + 6/7ths so casual menubar playing gets a
    /// useful readout without dragging in a full chord-theory engine.
    private static let chordPatterns: [(intervals: Set<Int>, suffix: String)] = [
        ([0, 4, 7],         ""),
        ([0, 3, 7],         "m"),
        ([0, 3, 6],         "dim"),
        ([0, 4, 8],         "aug"),
        ([0, 2, 7],         "sus2"),
        ([0, 5, 7],         "sus4"),
        ([0, 4, 7, 10],     "7"),
        ([0, 4, 7, 11],     "maj7"),
        ([0, 3, 7, 10],     "m7"),
        ([0, 3, 7, 11],     "mMaj7"),
        ([0, 3, 6, 9],      "dim7"),
        ([0, 3, 6, 10],     "m7♭5"),
        ([0, 4, 8, 10],     "aug7"),
        ([0, 4, 7, 9],      "6"),
        ([0, 3, 7, 9],      "m6"),
    ]

    /// Best-guess chord name from the currently-held pitch classes.
    /// Returns nil when fewer than 3 pitch classes are held or nothing
    /// matches a known shape exactly.
    func currentChordName() -> String? {
        let pcs = Set(litNotes.map { Int($0) % 12 })
        guard pcs.count >= 3 else { return nil }
        for root in pcs {
            let intervals = Set(pcs.map { ($0 - root + 12) % 12 })
            for (pat, suffix) in Self.chordPatterns where pat == intervals {
                return "\(Self.pitchClassNames[root])\(suffix)"
            }
        }
        return nil
    }

    /// Pitch classes (0..11) reachable in the active keymap, octave-
    /// independent. Used to filter chord suggestions down to ones the
    /// user could actually finish playing on the current QWERTY layout.
    func keymapPitchClasses() -> Set<Int> {
        let table = (keymap == .ableton)
            ? MenuBandLayout.semitoneByKeyCodeAbleton
            : MenuBandLayout.semitoneByKeyCode
        var pcs: Set<Int> = []
        for st in table where st != Int8.min {
            pcs.insert(((Int(st) % 12) + 12) % 12)
        }
        return pcs
    }

    /// Possible chord completions for the currently-held notes. Each
    /// candidate carries its full pitch-class set and the notes still
    /// needed to finish the chord. Filters to chords whose missing
    /// notes are reachable on the active keymap so the suggestions stay
    /// actionable. Sorted: complete first, then by ascending missing-
    /// note count, triads before 7ths, alphabetical tiebreaker.
    func chordCandidates(maxResults: Int = 6) -> [ChordCandidate] {
        let heldPCs = Set(litNotes.map { Int($0) % 12 })
        guard !heldPCs.isEmpty else { return [] }
        let availablePCs = keymapPitchClasses()
        var out: [ChordCandidate] = []
        for root in 0..<12 {
            for (intervals, suffix) in Self.chordPatterns {
                let pcs = Set(intervals.map { (root + $0) % 12 })
                if !heldPCs.isSubset(of: pcs) { continue }
                let missing = pcs.subtracting(heldPCs)
                if !missing.isSubset(of: availablePCs) { continue }
                let sortedMissing = missing.sorted()
                let names = sortedMissing.map { Self.pitchClassNames[$0] }
                let name = "\(Self.pitchClassNames[root])\(suffix)"
                out.append(ChordCandidate(
                    name: name,
                    rootPitchClass: root,
                    pitchClasses: pcs,
                    missingPitchClasses: sortedMissing,
                    missingNoteNames: names
                ))
            }
        }
        out.sort { a, b in
            if a.isComplete != b.isComplete { return a.isComplete && !b.isComplete }
            if a.missingPitchClasses.count != b.missingPitchClasses.count {
                return a.missingPitchClasses.count < b.missingPitchClasses.count
            }
            if a.pitchClasses.count != b.pitchClasses.count {
                return a.pitchClasses.count < b.pitchClasses.count
            }
            return a.name < b.name
        }
        if out.count > maxResults { out = Array(out.prefix(maxResults)) }
        return out
    }

    var midiMode: Bool {
        UserDefaults.standard.bool(forKey: midiModeKey)
    }

    var typeMode: Bool {
        UserDefaults.standard.bool(forKey: typeModeKey)
    }

    /// Audition the currently-loaded melodic program through the local
    /// synth, regardless of MIDI mode. Used by the instrument-list click
    /// handler so the user *always* hears their instrument pick, even when
    /// MIDI is on (which normally silences the local synth and routes to
    /// the DAW). Plays middle-C at velocity 100 for ~700 ms then releases.
    /// Forward the synth's tap-ring snapshot to callers (the WaveformView).
    /// Routing through the controller keeps MenuBandSynth private to the
    /// rest of the app while still letting the popover wire up live audio.
    func synthSnapshotWaveform(into dest: inout [Float]) {
        synth.snapshotWaveform(into: &dest)
    }

    /// Multiple surfaces can show the live waveform at once (popover,
    /// floating palette, menubar strip). Keep synth capture alive until the
    /// last consumer turns itself off, otherwise one view hiding can blank
    /// another view that's still visible.
    private var waveformCaptureClients = 0

    func setWaveformCaptureEnabled(_ enabled: Bool) {
        if enabled {
            waveformCaptureClients += 1
            if waveformCaptureClients == 1 {
                synth.setWaveformCaptureEnabled(true)
            }
        } else {
            waveformCaptureClients = max(0, waveformCaptureClients - 1)
            if waveformCaptureClients == 0 {
                synth.setWaveformCaptureEnabled(false)
            }
        }
    }

    // Held preview note for sonic-browse hover over the instrument map.
    // Continuous tone — switching cells stops the old note + starts a new
    // one in the new program. Hover-out releases. Silent in MIDI mode
    // (DAW is the audio path then; we still apply the program change so
    // it's correct when the user toggles MIDI off).
    private var previewNote: UInt8?
    private var previewProgram: UInt8?
    /// Pending noteOn dispatched after a setMelodicProgram swap. Held so
    /// a fast hover sequence (cell A → cell B → cell C in <70 ms) cancels
    /// the not-yet-fired note for B before C's load even starts.
    private var pendingPreviewWork: DispatchWorkItem?
    private let previewLoadDelay: TimeInterval = 0.07

    /// Hover-preview a program in the instrument map. Pass nil when the
    /// hover ends to release the held note and restore the committed
    /// program.
    func setInstrumentPreview(_ program: UInt8?) {
        // Always cancel any pending preview noteOn from a prior call —
        // each hover/click target gets a fresh 70 ms scheduling slot.
        pendingPreviewWork?.cancel()
        pendingPreviewWork = nil
        if let prev = previewNote {
            synth.noteOff(prev, channel: 0)
            previewNote = nil
        }
        guard let prog = program else {
            // Hover ended — flip back to the committed program so the
            // menubar piano still plays whatever the user actually picked.
            previewProgram = nil
            synth.setMelodicProgram(melodicProgram)
            onInstrumentVisualChange?()
            return
        }
        previewProgram = prog
        synth.setMelodicProgram(prog)
        onInstrumentVisualChange?()
        guard !midiMode else { return }
        let note = lastPlayedNote
        previewNote = note
        // When the synth supports instant program changes (MIDISynth backend
        // ready), fire noteOn immediately — the user's mouseDown becomes an
        // audible click with no perceptible delay. Sampler fallback still
        // needs the ~70 ms swap-settle window: AVAudioUnitSampler briefly
        // drops scheduled notes during `loadSoundBankInstrument`, so an
        // immediate noteOn falls into that gap and goes silent.
        if synth.supportsInstantProgramChange {
            synth.noteOn(note, velocity: 75, channel: 0)
            return
        }
        let work = DispatchWorkItem { [weak self] in
            guard let self = self,
                  self.previewNote == note,
                  !self.midiMode else { return }
            self.synth.noteOn(note, velocity: 75, channel: 0)
        }
        pendingPreviewWork = work
        DispatchQueue.main.asyncAfter(deadline: .now() + previewLoadDelay,
                                      execute: work)
    }

    // MARK: - Octave hold-to-ramp

    /// The keyCode currently driving the octave-hold timer. nil when
    /// no octave key is held.
    private var octaveHoldKey: UInt16?
    private var octaveHoldTimer: Timer?
    private static let octaveHoldDelay: TimeInterval    = 0.28
    private static let octaveHoldInterval: TimeInterval = 0.16

    /// Apply a single octave step + UI refresh. Clamps to ±4
    /// octaves; no-ops at the limit so a clamped key press is
    /// silent. The old percussive click was tied to held-key
    /// auto-repeat — both removed at jas's request so `,` / `.`
    /// behaves like a discrete one-press-one-step toggle.
    private func octaveStepOnce(delta: Int) {
        let next = max(-4, min(4, octaveShift + delta))
        if next == octaveShift {
            onOctaveLimitNudge?(delta)
            return
        }
        octaveShift = next
    }

    private func startOctaveHold(keyCode: UInt16, delta: Int) {
        octaveHoldKey = keyCode
        octaveHoldTimer?.invalidate()
        octaveHoldTimer = Timer.scheduledTimer(
            withTimeInterval: Self.octaveHoldDelay, repeats: false
        ) { [weak self] _ in
            guard let self = self,
                  self.octaveHoldKey == keyCode else { return }
            // After the initial hesitation, fire on a steady tempo
            // until the user lets go.
            self.octaveHoldTimer = Timer.scheduledTimer(
                withTimeInterval: Self.octaveHoldInterval, repeats: true
            ) { [weak self] _ in
                guard let self = self,
                      self.octaveHoldKey == keyCode else { return }
                self.octaveStepOnce(delta: delta)
            }
        }
    }

    private func stopOctaveHold(forKeyCode keyCode: UInt16) {
        guard octaveHoldKey == keyCode else { return }
        octaveHoldTimer?.invalidate()
        octaveHoldTimer = nil
        octaveHoldKey = nil
    }

    /// Tiny percussive click for octave shifts. Uses a high-mid drum
    /// (Tambourine, GM key 54) on the drum channel so it reads as
    /// punctual rather than tonal — but the velocity is mapped from
    /// the new octave so each step has its own audible weight,
    /// scaling brighter / sharper as you move up.
    private func playOctaveClick(for newShift: Int) {
        // Velocity range so all octaves are distinct but none are
        // jarring: −4 → ~50, 0 → ~85, +4 → ~120.
        let v = max(40, min(127, 85 + newShift * 9))
        synth.noteOn(54, velocity: UInt8(v), channel: 9)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) { [weak self] in
            self?.synth.noteOff(54, channel: 9)
        }
    }

    func auditionCurrentProgram() {
        let note = lastPlayedNote
        debugLog("audition: synth.noteOn \(note) (program \(melodicProgram))")
        synth.noteOn(note, velocity: 100, channel: 0)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.7) { [weak self] in
            self?.synth.noteOff(note, channel: 0)
        }
    }

    /// Right-⌘ quiet-focus toggle cue. No chord, no preview note —
    /// just two things:
    ///
    ///  1. A fixed two-note notification **bell** (`FocusCueBeep`),
    ///     on its own engine, so it never changes regardless of the
    ///     selected instrument — the dependable identity of "board
    ///     on" / "board off."
    ///  2. The **whole keyboard wipes** — a lit front sweeps across
    ///     EVERY key like a cover sliding off (on) / back on (off).
    ///
    /// (The click + camera flash are fired separately by the
    /// AppDelegate on the right-⌘ down edge.)
    func playFocusCue(rising: Bool) {
        // Fixed-identity bell — independent of instrument.
        FocusCueBeep.shared.play(rising: rising)
        // Whole keyboard flashes at once (no sweep), same for both.
        flashAllKeys()
    }

    private var keyFlashToken = 0

    /// Flash the ENTIRE keyboard on, then off — every key at once,
    /// no sweep / row pattern. Same gesture for enable and disable.
    /// Drives `litNotes` wholesale (the set real playing uses) then
    /// clears, handing the lights back to live play. A token guards
    /// against a stale clear wiping a newer flash on rapid toggles.
    func flashAllKeys() {
        let lo = KeyboardIconRenderer.firstMidi
        let hi = KeyboardIconRenderer.lastMidi
        guard hi >= lo else { return }
        keyFlashToken &+= 1
        let token = keyFlashToken
        var all = Set<UInt8>()
        for m in lo...hi { all.insert(UInt8(m)) }
        if all != litNotes {
            litNotes = all
            onLitChanged?()
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) { [weak self] in
            guard let self = self, self.keyFlashToken == token else { return }
            if !self.litNotes.isEmpty {
                self.litNotes.removeAll()
                self.onLitChanged?()
            }
        }
    }

    var keymap: Keymap {
        get {
            let raw = UserDefaults.standard.string(forKey: keymapKey) ?? ""
            // Default = .notepat (2 octaves). Pointer mode (typeMode off)
            // uses this keymap purely for piano range, so a fresh install
            // boots showing the full 2-octave range. Picking Ableton in the
            // popover narrows to 1 octave + Live's M-mode letters.
            return Keymap(rawValue: raw) ?? .notepat
        }
        set {
            UserDefaults.standard.set(newValue.rawValue, forKey: keymapKey)
            onChange?()
        }
    }

    var octaveShift: Int {
        get { UserDefaults.standard.integer(forKey: octaveShiftKey) }
        set {
            UserDefaults.standard.set(newValue, forKey: octaveShiftKey)
            // Held notes are stored by their actual (already octave-baked)
            // MIDI value, so we DON'T release them here — a note you're
            // holding keeps sounding at its original pitch when you bump the
            // octave; only the NEXT press uses the new octave. (Their key-up
            // still releases the correct stored note.)
            onChange?()
        }
    }

    var octaveShiftLabel: String {
        if octaveShift == 0 { return "0" }
        return octaveShift > 0 ? "+\(octaveShift)" : "\(octaveShift)"
    }

    /// Sided percussion. Each half of the board latches to the AC-native
    /// drum kit independently: tap `[` for the LEFT half, `]` for the
    /// RIGHT half (both → whole board). A latched half fires drums instead
    /// of melodic notes; the other half stays melodic. Off by default.
    /// Toggling a side releases held notes and, when BOTH go off, silences
    /// any ringing drums.
    var percussionLeft: Bool {
        get { UserDefaults.standard.bool(forKey: percussionLeftKey) }
        set {
            UserDefaults.standard.set(newValue, forKey: percussionLeftKey)
            applyPercussionSideEffects()
        }
    }
    var percussionRight: Bool {
        get { UserDefaults.standard.bool(forKey: percussionRightKey) }
        set {
            UserDefaults.standard.set(newValue, forKey: percussionRightKey)
            applyPercussionSideEffects()
        }
    }

    /// Whole-board convenience used by the About-window checkbox and the
    /// global ⌘⌃⌥D shortcut: reads as on if EITHER side is latched, and
    /// setting it flips both sides together.
    var percussionSplit: Bool {
        get { percussionLeft || percussionRight }
        set {
            UserDefaults.standard.set(newValue, forKey: percussionLeftKey)
            UserDefaults.standard.set(newValue, forKey: percussionRightKey)
            applyPercussionSideEffects()
        }
    }

    /// Shared side effects for any percussion-state change: drop held notes
    /// so nothing hangs across the melodic↔drum flip, silence drums once
    /// both sides are off, and keep the engine warm while any side drums.
    private func applyPercussionSideEffects() {
        releaseAllHeldNotes()
        if !percussionLeft && !percussionRight { synth.percussion.silence() }
        synth.keepEngineWarm = percussionLeft || percussionRight
        onChange?()
    }

    func togglePercussionSplit() { percussionSplit.toggle() }
    func togglePercussionLeft() { percussionLeft.toggle() }
    func togglePercussionRight() { percussionRight.toggle() }

    /// Re-apply side effects after state changed out of band (e.g. the
    /// About-window checkbox writes UserDefaults directly). The getters
    /// already reflect the new values.
    func reloadPercussionSplit() {
        applyPercussionSideEffects()
    }

    /// Forward the About-window "Use AC OS MIDI" flag to the synth so
    /// melodic notes route through (or away from) the native gm_synth
    /// voices. Re-reads UserDefaults so the toggle is the source of truth.
    func reloadUseACMIDI() {
        let on = UserDefaults.standard.bool(
            forKey: MenuBandSynth.useACMIDIDefaultsKey)
        synth.setUseACMIDI(on)
    }

    /// Audible confirmation for a percussion toggle — a kick when armed, a
    /// closed-hat tick when disarmed — so the gesture lands by ear. `pan`
    /// places the cue on the side that toggled (32 left, 96 right, 64 both).
    func playPercussionToggleCue(on: Bool, pan: UInt8 = 64) {
        synth.playPercussion(on ? .kick : .hatClosed,
                             velocity: on ? 100 : 80, pan: pan)
    }

    /// Whether `keyCode` currently fires a drum: the half of the board it
    /// sits on is latched to percussion. Drum keys ignore the chord
    /// modifiers — ⌘/⌥ + drum key plays the drum, not a triad — so the
    /// drum hand keeps working while the other hand holds chords.
    private func percussionActive(forKeyCode keyCode: UInt16) -> Bool {
        guard percussionLeft || percussionRight else { return false }
        let shift = octaveShift
        guard let note = MenuBandLayout.midiNote(forKeyCode: keyCode,
                                                 octaveShift: shift,
                                                 keymap: keymap) else { return false }
        let dn = max(60, min(83, Int(note) - shift * 12))
        return dn < MenuBandLayout.lingerSplitMidi ? percussionLeft : percussionRight
    }

    /// Key/click-down for a split drum — returns the group token to release
    /// on up. Used by both the keyboard split and menubar-piano clicks.
    @discardableResult
    func percussionNoteOn(_ drum: MenuBandPercussion.Drum,
                          velocity: UInt8, pan: UInt8, accent: Bool = false) -> UInt64 {
        synth.percussionNoteOn(drum, velocity: velocity, pan: pan, accent: accent)
    }

    /// Drum trigger→render latency (ms) for the debug latency readout.
    func percussionTriggerHandoffMs() -> Double { synth.percussionTriggerHandoffMs() }

    /// Key/click-up for a split drum (hi-hat foot-pedal release).
    func percussionNoteOff(_ group: UInt64) {
        synth.percussionNoteOff(group)
    }

    /// Live per-pitch-class drum hit pulses driving the menubar key vibe.
    func percussionPulses() -> [MenuBandPercussion.DrumPulse] {
        synth.percussionPulses()
    }

    // MARK: - Live engine primitives
    // Low-level voice control for `MenuBandEngine` (the conductible
    // drone/arp/drum loop). These bypass the round-robin tap path and address
    // explicit channels so the engine can hold a sustained chord on one bank
    // while crossfading a new chord onto another. GM channel 9 stays reserved
    // for percussion.

    /// Hold a sustained melodic voice on an explicit channel (no auto-off).
    func engineVoiceOn(_ note: UInt8, channel: UInt8, velocity: UInt8) {
        synth.setPan(64, channel: channel)
        synth.noteOn(note, velocity: velocity, channel: channel)
    }

    /// Release a sustained engine voice.
    func engineVoiceOff(_ note: UInt8, channel: UInt8) {
        synth.noteOff(note, channel: channel)
    }

    /// Per-channel Expression (CC 11) — the engine's crossfade lever.
    func engineExpression(_ value: UInt8, channel: UInt8) {
        synth.sendExpression(value: value, channel: channel)
    }

    /// One-shot percussion hit for the engine's drum loop.
    func engineDrum(_ drum: MenuBandPercussion.Drum, velocity: UInt8) {
        _ = synth.percussionNoteOn(drum, velocity: velocity, pan: 64, accent: false)
    }

    /// Set the melodic patch the engine's pad + arp voices speak through.
    func engineSetProgram(_ program: UInt8) {
        synth.setMelodicProgram(program)
    }

    /// True when the given menubar display note is a latched drum key. Each
    /// half is independent: the left half (`< lingerSplitMidi`) follows
    /// `percussionLeft`, the right half follows `percussionRight` — the same
    /// split the keyboard path uses (`percussionActive(forKeyCode:)`). The old
    /// `percussionSplit && displayNote >= lingerSplitMidi` form only ever
    /// matched the RIGHT half, so clicking left-octave menubar keys fell
    /// through to melodic notes instead of firing the latched drum.
    func isPercussionDisplayNote(_ displayNote: UInt8) -> Bool {
        Int(displayNote) < MenuBandLayout.lingerSplitMidi ? percussionLeft : percussionRight
    }

    /// Drum for a menubar display note (octave-invariant pitch class).
    func percussionDrum(forDisplayNote displayNote: UInt8) -> MenuBandPercussion.Drum {
        .forPitchClass(Int(displayNote) % 12)
    }

    /// Briefly light a menubar display cell for a one-shot drum hit, then
    /// extinguish it. Drums have no key-up sustain, so the off is self-
    /// scheduled after a short fixed flash. Lit-state is main-thread only,
    /// so hop there when called from the global CGEventTap thread.
    /// Light a menubar cell for a held drum key in the accent color and
    /// keep it lit until `drumLitOff` — so long as the key is down the cell
    /// stays "hold green." Refcounted so two sources on the same cell don't
    /// extinguish early. Main-thread only.
    func drumLitOn(_ displayNote: UInt8) {
        let work = { [weak self] in
            guard let self = self else { return }
            self.drumLitRefs[displayNote, default: 0] += 1
            self.litDownAt[displayNote] = CACurrentMediaTime()
            if self.litNotes.insert(displayNote).inserted { self.onLitChanged?() }
        }
        if Thread.isMainThread { work() } else { DispatchQueue.main.async(execute: work) }
    }

    /// Release a held drum light; extinguishes once the last holder lets go
    /// (unless a melodic note still holds the same cell).
    func drumLitOff(_ displayNote: UInt8) {
        let work = { [weak self] in
            guard let self = self else { return }
            let n = (self.drumLitRefs[displayNote] ?? 1) - 1
            if n > 0 { self.drumLitRefs[displayNote] = n; return }
            self.drumLitRefs.removeValue(forKey: displayNote)
            self.heldLock.lock()
            let heldByMelodic = self.heldKeyDisplayNote.values.contains(displayNote)
            self.heldLock.unlock()
            if !heldByMelodic {
                self.litDownAt.removeValue(forKey: displayNote)
                if self.litNotes.remove(displayNote) != nil { self.onLitChanged?() }
            }
        }
        if Thread.isMainThread { work() } else { DispatchQueue.main.async(execute: work) }
    }

    var playableNoteRangeLabel: String {
        let upper = keymap == .ableton ? 76 : 83
        let lowerNote = UInt8(max(0, min(127, 60 + octaveShift * 12)))
        let upperNote = UInt8(max(0, min(127, upper + octaveShift * 12)))
        return "\(Self.noteName(lowerNote))-\(Self.noteName(upperNote))"
    }

    var octaveContextLabel: String {
        "Octave \(octaveShiftLabel) \(playableNoteRangeLabel)"
    }

    /// Trackpad key-tap haptics. Toggleable again, from Settings — 1.5.4 had
    /// deleted the switch because it was wedged into the keymap's instrument
    /// title row with nowhere sane to live, not because the setting was
    /// unwanted. Still gated by hardware support (`MenuBandHaptics.isAvailable`)
    /// at the call site; on a Mac without a Force Touch trackpad the Settings
    /// row disables itself and this value is moot.
    private static let hapticsKey = "MBHaptics"
    var hapticsEnabled: Bool {
        get {
            if UserDefaults.standard.object(forKey: Self.hapticsKey) == nil { return true }
            return UserDefaults.standard.bool(forKey: Self.hapticsKey)
        }
        set { UserDefaults.standard.set(newValue, forKey: Self.hapticsKey) }
    }

    var melodicProgram: UInt8 {
        let raw = UserDefaults.standard.integer(forKey: melodicProgramKey)
        return UInt8(max(0, min(127, raw)))
    }

    /// Persistent master output gain, 0.0…1.0. Default 1.0 (full volume).
    /// Lives on the pre-limiter sum bus inside the synth so every backend
    /// scales together — drag the popover slider and the whole mix moves.
    var masterVolume: Float {
        get {
            if UserDefaults.standard.object(forKey: masterVolumeKey) == nil {
                return 1.0
            }
            let raw = UserDefaults.standard.double(forKey: masterVolumeKey)
            return Float(max(0.0, min(1.0, raw)))
        }
        set {
            let clamped = max(0, min(1, newValue))
            UserDefaults.standard.set(Double(clamped), forKey: masterVolumeKey)
            synth.setMasterVolume(clamped)
        }
    }

    var effectiveMelodicProgram: UInt8 {
        previewProgram ?? melodicProgram
    }

    func setMelodicProgram(_ program: UInt8) {
        UserDefaults.standard.set(Int(program), forKey: melodicProgramKey)
        previewProgram = nil
        // Picking a GM program implicitly switches us back to the GM
        // backend — the user's "Instrument" pick lives on the GM grid,
        // so committing one means GM is now the active source.
        UserDefaults.standard.set("gm", forKey: instrumentBackendKey)
        // Capture currently-held notes BEFORE the program change so
        // we can morph them onto the new instrument: held keys keep
        // sounding, just on the new patch. Apple's MIDISynth
        // applies program changes to subsequent noteOns only;
        // already-triggered voices keep their original sample.
        // Re-attacking the held notes after the program change is
        // the simplest path to "the held note swaps voice."
        let heldTaps: [(note: UInt8, channel: UInt8)] = tapNoteChannel.map { ($0.key, $0.value) }
        let heldKeys: [(note: UInt8, channel: UInt8)] = heldNotes.compactMap { kc, note in
            guard let ch = heldKeyChannel[kc] else { return nil }
            return (note, ch)
        }
        synth.setMelodicProgram(program)
        // Tell the external MIDI side too — without this the DAW (Live,
        // Logic, etc.) keeps playing whatever instrument was last on the
        // track and the local synth voice silently drifts out of sync
        // with what's recording. GM Melodic bank (MSB 0x79, LSB 0x00)
        // makes receivers that respect bank routing pick the same
        // patch the local MIDISynth just loaded.
        midi.sendProgramChange(program,
                               channel: 0,
                               bankMSB: 0x79,
                               bankLSB: 0x00)
        if !midiMode {
            for tap in heldTaps {
                synth.noteOff(tap.note, channel: tap.channel)
                synth.noteOn(tap.note, velocity: 100, channel: tap.channel)
            }
            for held in heldKeys {
                synth.noteOff(held.note, channel: held.channel)
                synth.noteOn(held.note, velocity: 100, channel: held.channel)
            }
        }
        onChange?()
        onInstrumentVisualChange?()
    }

    /// Apply a normalized pitch-bend (-1…+1) to every channel that
    /// currently has a held tap or keyboard note. Maps the
    /// normalized amount to the full ±8192 14-bit MIDI range, which
    /// at the GM default ±2-semitone receiver gives ±2 semitones
    /// of bend. Internal synth + MIDI out are both updated so a
    /// DAW receiving the MIDI sees the same bend as the user
    /// hears.
    /// Trackpad bend → semitone scale. ±1 of bendAmount = one
    /// octave. Picked over the old ±2 semitones so dramatic pitch
    /// shifts are reachable inside a single trackpad throw; the
    /// MIDI side announces the same range via RPN 0/0 = 12 on
    /// `midi.start()` so receivers (Live) scale matched.
    static let bendSemitonesPerUnit: CGFloat = 12

    /// Master ambience knob (trackpad X-axis on the bend gesture).
    /// Forwards straight to the synth's global reverb, so it colors
    /// every backend equally and is independent of which instrument
    /// is selected. 0 = dry / up front, 1 = big room.
    func setSpace(amount: Float) {
        synth.setSpace(amount)
    }

    /// Master echo knob (⌥Option + trackpad X on the bend gesture).
    /// Forwards straight to the synth's global delay so it colors
    /// every backend equally, independent of the selected instrument.
    func setEcho(amount: Float) {
        synth.setEcho(amount)
        // Mirror onto the spacebar tape playback so a mouse swipe while space
        // is held echoes the reverse audio too (its own insert — see synth).
        synth.setRewindEcho(amount: amount)
    }

    /// Speak a language's own name through the fx bus — the About-window
    /// flat-map language picker calls this on each pick, so the spoken
    /// name picks up whatever bend/space/echo the last gesture left on.
    func speakLanguageName(_ text: String, languageCode: String) {
        synth.speak(text, languageCode: languageCode)
    }

    /// One random drum hit for the About-window card-flip easter egg.
    func playEasterEggDrum() {
        synth.playEasterEggDrum()
    }

    /// Spacebar reverse-replay (notepat-native parity). Plays the most-recent
    /// few seconds of what just sounded, backwards. The synth keeps a rolling
    /// ring of its post-FX output at all times; this snapshots + reverses it.
    /// Bounced to the main thread because `playKeyEvent` runs on the
    /// KeyEventTap background thread and reverse playback mutates the engine
    /// graph + schedules a player buffer (both main-thread-only).
    /// True while the spacebar reverse-replay is sounding — drives the
    /// popover waveform strip's direction indicator (◀ reverse vs ▶ live).
    private(set) var isRewinding = false

    func rewind() {
        isRewinding = true
        DispatchQueue.main.async { [weak self] in
            self?.synth.playReverse()
        }
    }

    /// Spacebar released — stop reverse playback, banking the playhead so
    /// the next press resumes from the same reverse point (deeper into the
    /// tape). Playing a note resets that cursor to the live head.
    func rewindRelease() {
        isRewinding = false
        DispatchQueue.main.async { [weak self] in
            self?.synth.releaseReverse()
        }
    }

    /// Reverse window length (seconds) — for the waveform strip's scrub.
    var rewindWindowSeconds: Double { synth.rewindWindowSeconds }

    /// Reverse playback progress (0…1) or nil — for the strip's playhead.
    func rewindProgress() -> Double? { synth.rewindProgress() }

    func setBend(amount: Float, allChannels: Bool = false) {
        // No clamp here — the trackpad accumulator can swing past
        // ±1 and the sample voice can vari-speed an arbitrary
        // amount; the MIDI value saturates naturally inside
        // `sendPitchBend` (14-bit signed limit, receiver-side bend
        // range determines audible cap).
        let result = amount.finiteMagnitudeMultiply(8192)
        let value = Int16(result.safeClamped(Float(Int16.min), Float(Int16.max)))
        // Remember the current bend so a freshly-allocated round-robin
        // channel can be set to it at noteOn time (see currentBendValue).
        currentBendValue = value
        // Channels currently sounding via either input path. Use
        // sets to dedupe — same channel can host both a tap and a
        // keyboard note when the user is using both at once.
        var channels: Set<UInt8> = []
        if allChannels {
            // Shift-held performance move: warp EVERYTHING that could
            // be sounding right now — held keys, taps, AND lingering
            // doppler tails that have since rotated onto other
            // melodic channels. Broadcast to every channel except 9
            // (the GM drum kit), which would just detune into mush.
            for ch: UInt8 in 0...15 where ch != 9 { channels.insert(ch) }
        } else {
            for ch in tapNoteChannel.values { channels.insert(ch) }
            for ch in heldKeyChannel.values { channels.insert(ch) }
            if channels.isEmpty {
                // Send to channel 0 as a fallback so MIDI listeners
                // that pre-route on a fixed channel still get the
                // bend even when nothing is held locally.
                channels.insert(0)
            }
        }
        debugLog("setBend amt=\(amount) value=\(value) channels=\(channels) midiMode=\(midiMode)")
        if !midiMode {
            for ch in channels { synth.sendPitchBend(value: value, channel: ch) }
            // Sample voice runs through AVAudioUnitTimePitch and
            // ignores MIDI pitch-bend — route the signed amount
            // separately so trackpad pitch-bend slides the looping
            // sample alongside the MIDISynth-based voices.
            synth.setSamplePitchBend(amount: amount)
            // NOTE: percussion is intentionally NOT pitch-bent — the drums
            // stay dry/unaffected by the trackpad gesture (they also bypass
            // the master echo/reverb/proximity via `postFxMixer`).
            // KPBJ radio is also a varispeed backend (ignores MIDI
            // pitch-bend) — route the signed bend so the live stream
            // slides in pitch with everything else.
            synth.setRadioPitchBend(amount: amount)
            // AC GM synth re-derives increments from the live frequency, so
            // it ignores MIDI pitch-bend too — route the signed amount.
            synth.setGMPitchBend(amount: amount)
        }
        // Speech easter-egg voice slides too — applied regardless of
        // midiMode since it always sounds locally through the fx bus.
        synth.setSpeechPitchBend(amount: amount)
        // Spacebar tape playback bends too (its own source-side pitch insert),
        // so moving the mouse while space is held slides the reverse audio.
        synth.setRewindBend(amount: amount)
        for ch in channels { midi.sendPitchBend(value: value, channel: ch) }
    }

    /// Snap pitch-bend back to center (value 0). Convenience so the
    /// trackpad rubber-band animation has a clean `setBend(amount: 0)`
    /// equivalent at the call site.
    func clearBend() {
        setBend(amount: 0)
    }

    func stepMelodicProgram(delta: Int) {
        let next = max(0, min(127, Int(melodicProgram) + delta))
        guard next != Int(melodicProgram) else { return }
        setMelodicProgram(UInt8(next))
    }

    /// Arrow-key entry point: step the program AND fire a short
    /// audition blip in the newly-selected voice so the user can
    /// scan instruments by ear, just like dragging through the
    /// palette grid. Skipped when the step gets clamped (already
    /// at slot 0 / 127) so the user doesn't hear repeated blips
    /// on the same voice when holding the arrow past the end.
    func stepMelodicProgramWithBlip(delta: Int) {
        let prev = melodicProgram
        stepMelodicProgram(delta: delta)
        guard prev != melodicProgram else { return }
        firePreviewBlip()
    }

    /// Tail-released audition note used by arrow-key stepping. The
    /// preview note matches the user's most recent played pitch —
    /// same convention as the palette's hover-preview path.
    private var previewBlipWork: DispatchWorkItem?
    private static let previewBlipDuration: TimeInterval = 0.30

    private func firePreviewBlip() {
        guard !midiMode else { return }
        let note = lastPlayedNote
        // Cancel any in-flight blip release and silence the prior
        // note before retriggering so rapid arrow auto-repeats
        // produce a continuous audition stream, not stacked notes.
        previewBlipWork?.cancel()
        synth.noteOff(note, channel: 0)
        synth.noteOn(note, velocity: 75, channel: 0)
        let work = DispatchWorkItem { [weak self] in
            self?.synth.noteOff(note, channel: 0)
        }
        previewBlipWork = work
        DispatchQueue.main.asyncAfter(deadline: .now() + Self.previewBlipDuration,
                                      execute: work)
    }

    func stepOctave(delta: Int) {
        octaveStepOnce(delta: delta)
    }

    /// Wood-block tick for an octave-swipe detent (swipe path only — key
    /// presses stay silent per jas's earlier ask). `up` pans the tick.
    func playOctaveTick(up: Bool) {
        synth.playOctaveTick(up: up)
    }

    // MARK: - Instrument backend (GM vs GarageBand)

    enum InstrumentBackend: String { case gm, garageBand = "gb", kpbj = "kpbj", sample = "sample" }

    var instrumentBackend: InstrumentBackend {
        let raw = UserDefaults.standard.string(forKey: instrumentBackendKey) ?? "gm"
        return InstrumentBackend(rawValue: raw) ?? .gm
    }

    /// Currently-selected GarageBand patch URL, or nil if none picked
    /// yet (or the previously-saved patch is no longer on disk).
    var garageBandPatchURL: URL? {
        guard let path = UserDefaults.standard.string(forKey: garageBandPatchPathKey),
              !path.isEmpty,
              FileManager.default.fileExists(atPath: path) else { return nil }
        return URL(fileURLWithPath: path)
    }

    /// Switch the active backend to GarageBand and load the given patch.
    func setGarageBandPatch(_ url: URL) {
        UserDefaults.standard.set("gb", forKey: instrumentBackendKey)
        UserDefaults.standard.set(url.path, forKey: garageBandPatchPathKey)
        synth.setGarageBandPatch(at: url)
        onChange?()
    }

    /// Switch the active backend to the live KPBJ.FM radio stream
    /// (conceptually "voice −1"). The piano keys play the live audio
    /// pitched by 2^((note−60)/12) — middle C is unpitched, every other
    /// note is varispeed-shifted off the same buffer. Disabling restores
    /// whichever GM program was last selected.
    func setRadioBackend(_ enabled: Bool) {
        if enabled {
            UserDefaults.standard.set("kpbj", forKey: instrumentBackendKey)
            synth.setRadioStation(radioStation)  // tune to the saved station
            synth.setRadioBackend(true)
        } else {
            UserDefaults.standard.set("gm", forKey: instrumentBackendKey)
            synth.setRadioBackend(false)
            // Reload whatever GM voice was last picked so the user lands
            // back on a familiar instrument instead of silence.
            synth.setMelodicProgram(melodicProgram)
        }
        onChange?()
        onInstrumentVisualChange?()
    }

    func toggleRadioBackend() {
        setRadioBackend(instrumentBackend != .kpbj)
    }

    /// The radio station the "voice −1" backend is tuned to (persisted).
    var radioStation: RadioStation {
        get { RadioStation.by(id: UserDefaults.standard.string(forKey: radioStationKey) ?? "kpbj") }
        set {
            UserDefaults.standard.set(newValue.id, forKey: radioStationKey)
            synth.setRadioStation(newValue)
            onChange?()
            onInstrumentVisualChange?()
        }
    }

    /// Pick a station AND make the radio the active backend — the action
    /// behind a station cell in the chooser and the `-kpbj` / `-nts1` /
    /// `-nts2` typed commands. Tuning while already on radio just retunes.
    func selectRadioStation(_ station: RadioStation) {
        UserDefaults.standard.set(station.id, forKey: radioStationKey)
        synth.setRadioStation(station)
        if instrumentBackend != .kpbj {
            setRadioBackend(true)   // engages radio + applies this station
        } else {
            onChange?()
            onInstrumentVisualChange?()
        }
    }

    // MARK: - Plugins (third-party AU instruments)

    /// Window controller for the AU picker. Held weakly so reopening
    /// reuses the same window if it's still on screen.
    private var pluginPicker: AUPluginPickerController?

    /// Open (or focus) the Plugins picker. Wired to the About window's
    /// "Plugins…" link so users can drop a third-party AU in for testing.
    func presentPluginPicker() {
        if let existing = pluginPicker {
            existing.window?.makeKeyAndOrderFront(nil)
            NSApp.activate(ignoringOtherApps: true)
            return
        }
        let ctrl = AUPluginPickerController(
            currentInstrument: synth.pluginInstrument,
            onLoad: { [weak self] avUnit in
                self?.synth.setPluginInstrument(avUnit)
                self?.onChange?()
            },
            onUnload: { [weak self] in
                self?.synth.setPluginInstrument(nil)
                self?.onChange?()
            }
        )
        pluginPicker = ctrl
        ctrl.present()
    }

    /// Public read of "is the synth currently capturing input?". The
    /// AppDelegate uses this to flip the menubar icon to its red
    /// "recording active" state and to switch the VU meter source from
    /// synth-output RMS to mic-input RMS.
    var sampleRecordingActive: Bool { synth.sampleRecording }

    var sampleVoiceHasRecording: Bool { synth.hasSampleRecording }

    var audioRoutingContextLabel: String? {
        if midiMode { return "Local synth muted - MIDI OUT" }
        if instrumentBackend == .sample && !sampleVoiceHasRecording {
            return "No sample yet - hold ` to record"
        }
        return nil
    }

    var voiceContextLabel: String {
        if midiMode { return "0 MIDI OUT" }
        switch instrumentBackend {
        case .sample:
            return sampleVoiceHasRecording ? "Sample Voice" : "Sample Voice - no recording"
        case .kpbj:
            return "\(radioStation.name) radio"
        case .garageBand:
            return garageBandPatchURL?.deletingPathExtension().lastPathComponent ?? "GarageBand patch"
        case .gm:
            let safe = max(0, min(127, Int(effectiveMelodicProgram)))
            return String(format: "%03d %@", safe + 1, GeneralMIDI.programName(safe))
        }
    }

    /// Forward an RMS callback to the underlying sample voice's input
    /// tap. Called once per recording block (~93 ms) on the main queue
    /// with that block's RMS. AppDelegate hooks this up in
    /// `applicationDidFinishLaunching` and stashes the latest value
    /// for the visualizer animation tick to read on its next frame.
    func setSampleLevelHandler(_ handler: ((Float) -> Void)?) {
        synth.onSampleLevel = { [weak self] level in
            self?.sampleInputLevel = level
            handler?(level)
        }
    }

    /// Switch the active backend to the user-recorded sample voice.
    /// The `MenuBandSampleVoice` instance lives on the synth — it
    /// owns the recording buffer + per-note player pool. Disabling
    /// restores whichever GM voice was last selected (same shape as
    /// `setRadioBackend(false)`).
    func setSampleBackend(_ enabled: Bool) {
        if enabled {
            UserDefaults.standard.set("sample", forKey: instrumentBackendKey)
            synth.setSampleBackend(true)
        } else {
            UserDefaults.standard.set("gm", forKey: instrumentBackendKey)
            synth.setSampleBackend(false)
            // Reload whatever GM voice was last picked so the user
            // lands back on a familiar instrument instead of silence.
            synth.setMelodicProgram(melodicProgram)
        }
        onChange?()
        onInstrumentVisualChange?()
    }


    // MARK: - Tape transport
    //
    // Thin facade over `MenuBandTape` that adds two pieces of glue the
    // tape itself doesn't know about:
    //
    //   • Hot-mic pinning — REC must keep the input tap delivering
    //     frames even when the sample-voice backtick gate is closed.
    //   • Waveform-tap pinning — REC + PLAY must keep the synth's
    //     mainMixer tap installed so output flows into `ingestSynth`.
    //
    // Pin reasons are released in `handleTapeChange()` when the tape
    // transitions back to `.idle`.

    /// Toggle the tape between IDLE and RECORDING. On first-ever REC
    /// press, surface the macOS mic permission prompt; the recording
    /// is started for real once the user grants access.
    func toggleTapeRecording() {
        if tape.state == .recording {
            tape.stop()
            return
        }
        let auth = MenuBandSampleVoice.micAuthorizationStatus()
        switch auth {
        case .authorized:
            beginTapeRecording()
        case .notDetermined:
            MenuBandSampleVoice.requestMicAccess { [weak self] granted in
                guard granted else { return }
                self?.beginTapeRecording()
            }
        case .denied, .restricted:
            // No mic available — record synth output only. The mic
            // indicator on the inline widget will read as "off".
            beginTapeRecording()
        @unknown default:
            beginTapeRecording()
        }
    }

    private func beginTapeRecording() {
        // Pin both the hot mic and the synth waveform tap before we
        // flip the tape state, so the first audio block after REC
        // already lands in the buffer.
        synth.addWaveformTapPin(tapeWaveformPinReason)
        synth.pinHotMic(reason: tapeMicPinReason)
        tape.record()
    }

    func stopTape() { tape.stop() }
    func playTape() {
        // Need the waveform tap pinned during playback so the live
        // visualizer in the popover shows tape output — the player
        // node feeds into the same mixer the tap reads from. (Cheap
        // to pin redundantly; reasons set is set-typed.)
        synth.addWaveformTapPin(tapeWaveformPinReason)
        tape.play()
    }
    func pauseTape()  { tape.pause() }
    func rewindTape() { tape.rewind() }
    func tapeSeekToEnd() { tape.seekToEnd() }
    func clearTape() { tape.clear() }

    /// Drag-out path. Returns the on-disk 4-channel `.wav` URL for
    /// the current recording (synth L/R on channels 1-2, mic on
    /// channels 3-4) with the cassette cover-art icon applied.
    /// AppDelegate's status-button drag source rides this URL on the
    /// pasteboard so dropping the cassette onto the Desktop yields a
    /// single multi-track WAV that DAWs can split into stems.
    func ejectTape() -> URL? { tape.eject()?.file }

    /// True while a tape recording is rolling — the global record shortcut
    /// keys off this so it can't stack recordings.
    var isTapeRecording: Bool { tape.state == .recording }

    /// Audio-only recording for the global record gesture: pin ONLY the synth
    /// waveform tap, never the mic — so it captures the instrument with no
    /// microphone permission prompt. The mic stem stays silent in the WAV.
    func startSynthOnlyRecording() {
        guard tape.state != .recording else { return }
        synth.addWaveformTapPin(tapeWaveformPinReason)
        tape.record()
    }

    /// Instant transport stop — just flips the tape state. The heavy WAV
    /// render happens later in `saveStoppedTakeToDesktop()` off the main
    /// thread, so Escape feels immediate.
    func stopTapeNow() {
        if tape.state == .recording { tape.stop() }
    }

    /// Play one melodic note for `duration` seconds — drives the synth
    /// directly (bypassing the keyboard) so an `.mbscore` can auto-perform a
    /// take for testing the record → DMG pipeline.
    func playTestNote(midi: UInt8, velocity: UInt8 = 100, duration: TimeInterval) {
        synth.noteOn(midi, velocity: velocity, channel: 0)
        DispatchQueue.main.asyncAfter(deadline: .now() + duration) { [weak self] in
            self?.synth.noteOff(midi, channel: 0)
        }
    }

    /// Render the stopped take and wrap it into a per-take **DMG "record
    /// release"** on the Desktop: our logo as the volume icon, the take's WAV
    /// (with its generative album-art icon) inside, and that album art on the
    /// .dmg file itself. HEAVY (audio conversion + drawing + hdiutil) — call
    /// OFF the main thread. Returns the .dmg URL, or nil if empty.
    @discardableResult
    func saveStoppedTakeToDesktop() -> URL? {
        guard let take = tape.eject() else { return nil }
        let src = take.file
        // The album-art icon eject() stamped on the WAV → the .dmg file icon.
        let cover = NSWorkspace.shared.icon(forFile: src.path)
        let name = src.deletingPathExtension().lastPathComponent
        // Include the sidecar .mid (editable notes for Ableton) in the release.
        let extras = [take.midi].compactMap { $0 }
        return TakeDMG.build(wav: src, extras: extras, name: name, coverIcon: cover)
    }

    /// State-change observer. Drops the pins whenever the tape goes
    /// back to idle so the synth engine can suspend on inactivity.
    private func handleTapeChange() {
        if tape.state == .idle {
            synth.unpinHotMic(reason: tapeMicPinReason)
            synth.removeWaveformTapPin(tapeWaveformPinReason)
        }
        onChange?()
    }

    func bootstrap() {
        // Built-in synth is always live. TYPE mode and MIDI mode are now
        // independent toggles: TYPE controls global keyboard capture + key
        // letter overlays; MIDI controls the virtual MIDI port output to
        // external DAWs. MIDI mode defaults to *off* for fresh installs —
        // most users hear the synth first; DAW routing is the opt-in path
        // they pick after they know they want it. Existing users' choice
        // is preserved (only the default for first-launch changes).
        // Default instrument: Whistle (GM 078) — playful + breathy, more
        // distinctive than acoustic grand on a fresh install. Users can
        // still pick anything from the popover map.
        if UserDefaults.standard.object(forKey: melodicProgramKey) == nil {
            UserDefaults.standard.set(78, forKey: melodicProgramKey)
        }
        synth.start()
        synth.setMelodicProgram(melodicProgram)
        // Tape wiring: attach the player to the synth's pre-limiter
        // sum bus, fork the existing audio taps into the tape's
        // ingestion methods, and listen for state changes so we can
        // unpin the hot mic / waveform tap when REC ends.
        synth.attachTape(tape)
        synth.onWaveformBuffer = { [weak self] buffer in
            self?.tape.ingestSynth(buffer)
        }
        synth.onMicInputBuffer = { [weak self] buffer in
            self?.tape.ingestMic(buffer)
        }
        // Capture the note performance as MIDI alongside the audio, so a take
        // carries editable notes (a .mid drops onto an Ableton MIDI track).
        synth.onNoteEvent = { [weak self] note, vel, on, ch in
            self?.tape.ingestNote(note, velocity: vel, on: on, channel: ch)
        }
        // Block-based observer so the controller (a plain Swift class,
        // not NSObject) can register without inheriting from
        // NSObject. Token-less because the controller lives for the
        // app's full duration.
        NotificationCenter.default.addObserver(
            forName: .menuBandTapeChanged,
            object: tape,
            queue: .main
        ) { [weak self] _ in
            self?.handleTapeChange()
        }
        // Restore the user's last master-volume setting before any notes
        // sound. Default of 1.0 means a fresh install plays at full
        // volume (matches previous behaviour); explicit lower picks
        // survive the relaunch.
        synth.setMasterVolume(masterVolume)
        // Restore radio backend if it was active in the previous session.
        // Done after setMelodicProgram so the GM voice is primed underneath
        // — toggling radio off later returns the user to that voice.
        if instrumentBackend == .kpbj {
            synth.setRadioBackend(true)
        }
        // Sample backend doesn't survive relaunch — there's no
        // recording on disk yet, so fall back to GM. Persist the
        // reset so a subsequent voice picker click doesn't re-flip.
        if instrumentBackend == .sample {
            UserDefaults.standard.set("gm", forKey: instrumentBackendKey)
        }
        if UserDefaults.standard.object(forKey: midiModeKey) == nil {
            UserDefaults.standard.set(false, forKey: midiModeKey)
        }
        if typeMode { enableTypeMode(promptForPermission: false) }
        if midiMode { enableMIDIMode() }

        // GarageBand integration deprecated for now — see
        // GarageBandLibrary.swift / GarageBandPatchView.swift for the
        // dormant scaffolding.
        // (enableMIDIMode triggers a loopback self-test; result lands in
        // /tmp/menuband-debug.log and updates the popover's status row.)
    }

    func toggleMIDIMode() {
        debugLog("toggleMIDIMode (currently \(midiMode))")
        if midiMode { disableMIDIMode() } else { enableMIDIMode() }
        debugLog("toggleMIDIMode DONE (now \(midiMode))")
    }

    func toggleTypeMode() {
        if typeMode {
            disableTypeMode(playFeedback: true)
        } else {
            enableTypeMode(promptForPermission: true, playFeedback: true)
        }
    }

    func disableTypeModeForFocusCapture() {
        guard typeMode else { return }
        disableTypeMode()
    }

    func shutdown() {
        disableTypeMode()
        disableMIDIMode()
        synth.stop()
    }

    // MARK: - MIDI virtual port (Ableton-facing output)

    private func enableMIDIMode() {
        midi.start()
        synth.panic()  // DAW takes over from internal synth
        UserDefaults.standard.set(true, forKey: midiModeKey)
        onChange?()
        // Self-test runs once per session — first enable. Skipping it on
        // every toggle removes the 50–800ms perceived lag from the popover
        // switch.
        if !loopbackTestRunOnce {
            loopbackTestRunOnce = true
            runMIDILoopbackTest()
        }
    }

    // Last loopback test result, surfaced to the popover as a status line.
    enum MIDISelfTest {
        case unknown
        case running
        case ok(latencyMs: Int)
        case failed
    }
    private(set) var midiSelfTest: MIDISelfTest = .unknown
    var onSelfTestChanged: (() -> Void)?

    /// `true` once the loopback self-test has run successfully for this app
    /// session. We skip subsequent runs on toggle so the switch flips
    /// instantly — the test exists to surface CoreMIDI hiccups, and once
    /// we've confirmed the port works there's no value in re-checking.
    private var loopbackTestRunOnce = false

    /// Sends a single test note out the virtual port and listens on the
    /// process's own input port for it to come back. If it loops back within
    /// the window, the DAW-facing port is functional. If not, it's blocked
    /// or the source is broken. Result lands on `midiSelfTest`.
    private func runMIDILoopbackTest() {
        midiSelfTest = .running
        onSelfTestChanged?()
        let testNote: UInt8 = 60
        let testVelocity: UInt8 = 1     // velocity 1 — inaudible enough for a DAW
        let sentAt = CACurrentMediaTime()
        midi.startLoopback { [weak self] note, velocity in
            guard let self = self else { return }
            if note == testNote {
                let dt = Int((CACurrentMediaTime() - sentAt) * 1000)
                debugLog("MIDI loopback: received note=\(note) vel=\(velocity) latency=\(dt)ms")
                // CRITICAL: stopLoopback() calls MIDIPortDisconnectSource on
                // the same source whose callback we're inside right now —
                // CoreMIDI does not allow that, the disconnect blocks the
                // delivery thread (see usleep loop in
                // LocalMIDIReceiverList::ReceiverConnectEndpoint) and the
                // ensuing thread storm crashes the main thread with a wild
                // timer fire (Code Signature Invalid SIGKILL). Defer the
                // disconnect to the main run-loop, which runs *after* this
                // callback returns.
                DispatchQueue.main.async {
                    self.midiSelfTest = .ok(latencyMs: dt)
                    self.onSelfTestChanged?()
                    self.midi.stopLoopback()
                }
            }
        }
        // Send the test note
        DispatchQueue.global(qos: .userInteractive).asyncAfter(deadline: .now() + .milliseconds(50)) { [weak self] in
            self?.midi.noteOn(testNote, velocity: testVelocity, channel: 0)
            self?.midi.noteOff(testNote, channel: 0)
        }
        // Timeout
        DispatchQueue.main.asyncAfter(deadline: .now() + .milliseconds(800)) { [weak self] in
            guard let self = self else { return }
            if case .running = self.midiSelfTest {
                debugLog("MIDI loopback: TIMEOUT — no note received within 800ms")
                self.midi.stopLoopback()
                self.midiSelfTest = .failed
                self.onSelfTestChanged?()
            }
        }
    }

    private func disableMIDIMode() {
        debugLog("disableMIDIMode: calling midi.stop()")
        midi.stop()
        debugLog("disableMIDIMode: midi.stop() returned")
        UserDefaults.standard.set(false, forKey: midiModeKey)
        midiSelfTest = .unknown
        onSelfTestChanged?()
        onChange?()
        debugLog("disableMIDIMode: complete")
    }

    // MARK: - TYPE mode (global keyboard capture + letter overlays)

    private func enableTypeMode(promptForPermission: Bool, playFeedback: Bool = false) {
        #if MAC_APP_STORE
        // Mac App Store build: global keystroke capture (CGEventTap) is
        // forbidden by the App Sandbox, so there is no global TYPE mode.
        // Typing-to-play is served instead by LocalKeyCapture while Menu
        // Band is the focused app (armed on piano-click / popover-open);
        // the selected keymap is still honored via handleLocalKey. Keep
        // typeMode persisted false so the UI never claims global capture.
        UserDefaults.standard.set(false, forKey: typeModeKey)
        return
        #else
        if !ensureAccessibility(prompt: promptForPermission) {
            UserDefaults.standard.set(false, forKey: typeModeKey)
            onChange?()
            return
        }
        let tap = KeyEventTap { [weak self] keyCode, isDown, isRepeat, flags in
            return self?.handleKey(keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, flags: flags) ?? false
        }
        if !tap.start() {
            NSLog("MenuBand: CGEventTap creation failed (likely missing Accessibility permission)")
            UserDefaults.standard.set(false, forKey: typeModeKey)
            onChange?()
            return
        }
        keyTap = tap
        UserDefaults.standard.set(true, forKey: typeModeKey)
        installClickAwayMonitor()
        if playFeedback {
            NSSound(named: NSSound.Name("Submarine"))?.play()
        }
        onChange?()
        #endif
    }

    private func disableTypeMode(playFeedback: Bool = false) {
        removeClickAwayMonitor()
        keyTap?.stop()
        keyTap = nil
        releaseAllHeldNotes()
        voiceDigitBuffer = ""
        UserDefaults.standard.set(false, forKey: typeModeKey)
        if playFeedback {
            NSSound(named: NSSound.Name("Pop"))?.play()
        }
        onChange?()
    }

    private func handleSampleRecordKey(isDown: Bool, isRepeat: Bool, chromatic: Bool = false, source: String) -> Bool {
        if isDown && isRepeat { return true }
        NSLog("MenuBand SampleVoice: backtick \(isDown ? "down" : "up") chromatic=\(chromatic) via \(source)")
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            if isDown {
                // "Home" gesture: clear all per-key custom samples, then
                // record a fresh global sample while held. ⌃ picks chromatic
                // (pitch-corrected) vs the default normal (C4 = raw) mode.
                self.synth.clearPerKeySamples()
                self.synth.startSampleRecording(chromatic: chromatic)
                // Nudge the AppDelegate so the menubar icon immediately
                // picks up the red "REC" tint on the chip.
                self.onInstrumentVisualChange?()
            } else {
                let usable = self.synth.stopSampleRecording()
                if usable {
                    self.setSampleBackend(true)
                } else {
                    // Recording was too short / discarded — still need to
                    // repaint so the icon drops the red tint.
                    self.onInstrumentVisualChange?()
                }
            }
        }
        return true
    }

    // MARK: - Click-away auto-disable

    private var clickAwayMonitor: Any?
    private var appActivationObserver: Any?

    private func installClickAwayMonitor() {
        // Global mouse-down monitor: fires for events going to any OTHER app.
        // A click outside our menubar item exits TYPE mode so we don't keep
        // sinking keystrokes after the user moves on.
        if clickAwayMonitor == nil {
            clickAwayMonitor = NSEvent.addGlobalMonitorForEvents(
                matching: [.leftMouseDown, .rightMouseDown, .otherMouseDown]
            ) { [weak self] _ in
                DispatchQueue.main.async { self?.disableTypeMode() }
            }
        }
        // App-activation observer: belt-and-suspenders for cmd-tab and
        // dock/Spotlight-launched activations that NSEvent global monitors
        // sometimes miss.
        if appActivationObserver == nil {
            appActivationObserver = NSWorkspace.shared.notificationCenter.addObserver(
                forName: NSWorkspace.didActivateApplicationNotification,
                object: nil, queue: .main
            ) { [weak self] note in
                guard let info = note.userInfo,
                      let app = info[NSWorkspace.applicationUserInfoKey] as? NSRunningApplication,
                      app.bundleIdentifier != Bundle.main.bundleIdentifier
                else { return }
                self?.disableTypeMode()
            }
        }
    }

    private func removeClickAwayMonitor() {
        if let m = clickAwayMonitor { NSEvent.removeMonitor(m) }
        clickAwayMonitor = nil
        if let o = appActivationObserver {
            NSWorkspace.shared.notificationCenter.removeObserver(o)
        }
        appActivationObserver = nil
    }

    // Drums (low GM percussion notes) route to GM channel 10 (index 9) so the
    // built-in DLS synth picks the standard drum kit instead of piano.
    @inline(__always)
    private func channel(for note: UInt8) -> UInt8 {
        note < UInt8(KeyboardIconRenderer.firstMidi) ? 9 : 0
    }

    // MARK: - Menubar tap / drag-to-play

    private var tapHeld: Set<UInt8> = []  // notes currently held by mouse drag (main thread)
    private var tapNoteChannel: [UInt8: UInt8] = [:]  // active channel per held note
    private var tapDisplayNote: [UInt8: UInt8] = [:]  // visible key to light for a held tap note
    private var tapLinger: [UInt8: UInt8] = [:]       // notes started under shift → noteOn velocity (drives doppler)
    // Live notes round-robin across channels 0-3; doppler retriggers
    // use their own 4-7 cursor. The split guarantees a live press
    // cannot ever land on a channel that a doppler voice is on, so
    // the live note's noteOff never cuts a still-ringing doppler tail
    // (and vice versa). 4 voices per side is enough for typical chord
    // play and ~7 doppler retriggers with their growing intervals.
    private var melodicVoiceCursor: UInt8 = 0         // round-robin 0..3 for live
    private var dopplerVoiceCursor: UInt8 = 4         // round-robin 4..7 for doppler retriggers
    /// Last pitch-bend value pushed via `setBend` (centered = 0). Each
    /// freshly-allocated round-robin channel is set to this at noteOn so
    /// a new note never inherits a STALE bend left on that channel by an
    /// earlier gesture — otherwise repeatedly pressing one key cycles
    /// through 0…3 and audibly "oscillates" in pitch. When a bend is
    /// active the new note correctly joins it; when idle it's centered.
    private var currentBendValue: Int16 = 0
    /// Center the pitch bend on a channel about to host a fresh note,
    /// applying the current bend (0 when idle). No-op in MIDI mode (the
    /// external DAW owns channel state there).
    private func primeChannelBend(_ channel: UInt8) {
        guard !midiMode else { return }
        synth.sendPitchBend(value: currentBendValue, channel: channel)
    }

    /// Linger tail length — how long after key release we hold the
    /// note before sending the cleanup noteOff. Long enough that the
    /// synth's natural release envelope can decay to silence (~3-5s on
    /// most GM voices), short enough that an external DAW doesn't
    /// accumulate hung notes when the user shift-spams keys.
    private static let lingerTailSeconds: TimeInterval = 4.0

    // Doppler retrigger tuning for staccato linger. Each retrigger
    // fires the same midi note on a fresh channel at decaying velocity
    // and slowly-growing intervals. The user can keep playing live
    // notes over the tail because retriggers always use a rotating
    // channel via `nextMelodicChannel()` — they never pin the user's
    // active voices.
    private static let dopplerInitialDelay: TimeInterval = 0.30
    private static let dopplerIntervalGrowth: Double = 1.18
    private static let dopplerVelocityDecay: Double = 0.78
    private static let dopplerVelocityFloor: Double = 8.0
    private static let dopplerMaxSteps: Int = 14
    private static let dopplerNoteOffWindow: TimeInterval = 0.40

    @inline(__always)
    private func nextMelodicChannel() -> UInt8 {
        let c = melodicVoiceCursor
        melodicVoiceCursor = (melodicVoiceCursor &+ 1) & 0x03
        return c
    }

    @inline(__always)
    private func nextDopplerChannel() -> UInt8 {
        let c = dopplerVoiceCursor
        dopplerVoiceCursor = ((dopplerVoiceCursor &+ 1 - 4) & 0x03) + 4
        return c
    }

    /// Begin holding a note from a menubar mouse interaction. Velocity (0–127)
    /// and pan (0–127, 64=center) are passed in by the caller — the drag
    /// handler computes them from the cursor's relative position inside the
    /// hovered key, giving expressive control: y closer to vertical center =
    /// louder, x within key = stereo pan.
    func startTapNote(_ midiNote: UInt8,
                      velocity: UInt8 = 100,
                      pan: UInt8 = 64,
                      displayNote: UInt8? = nil,
                      linger: Bool = false) {
        debugLog("startTapNote midi=\(midiNote) midiMode=\(midiMode) linger=\(linger)")
        lastPlayedNote = midiNote
        if tapHeld.contains(midiNote) { return }
        tapHeld.insert(midiNote)
        let visualNote = displayNote ?? midiNote
        tapDisplayNote[midiNote] = visualNote
        if linger { tapLinger[midiNote] = velocity }
        // Tapped notes are always melodic — they originate from the piano
        // UI and get octave-shifted by the caller. Never route to channel 9
        // (GM drums); the old check (`midiNote < firstMidi`) conflated
        // "below the visible display range" with "is a drum."
        let synthCh: UInt8 = nextMelodicChannel()
        let midiCh: UInt8 = 0
        tapNoteChannel[midiNote] = synthCh
        // Reuse a recently-faded channel? Cancel its in-flight
        // Expression ramp + reset to 127 so this attack starts at
        // full volume. Otherwise the new note inherits the prior
        // fade's running value and "sustains in" only as the ramp
        // tail completes.
        cancelLingerFade(channel: synthCh)
        midi.sendCC(10, value: pan, channel: midiCh)
        // Mirror the keyboard path: also send pan to the local synth's
        // per-channel state so synth state stays symmetric across input
        // methods. Without this, channels written by keyboard carry over
        // their pan into subsequent taps on the same channel.
        if !midiMode {
            synth.setPan(pan, channel: synthCh)
            // Prime the channel's bend (current bend, or centered when idle)
            // so this tap can't inherit a stale bend on this round-robin slot.
            primeChannelBend(synthCh)
            synth.noteOn(midiNote, velocity: velocity, channel: synthCh)
        }
        midiNoteOn(midiNote, velocity: velocity, channel: midiCh)
        // Lit state is main-thread-only; update synchronously so the menubar
        // redraws within the same runloop pass as the click. Dispatching async
        // pushed the redraw past the event-tracking loop's next spin and the
        // blink wasn't visible.
        let setLit = { [weak self] in
            guard let self = self else { return }
            self.litDownAt[visualNote] = CACurrentMediaTime()
            if self.litNotes.insert(visualNote).inserted {
                self.onLitChanged?()
            }
        }
        if Thread.isMainThread { setLit() } else { DispatchQueue.main.async(execute: setLit) }
    }

    /// While dragging, update pan in real time as the cursor slides within
    /// the held note. Doesn't retrigger the note.
    func updateTapPan(_ midiNote: UInt8, pan: UInt8) {
        guard tapHeld.contains(midiNote) else { return }
        midi.sendCC(10, value: pan, channel: 0)
    }

    /// Release a note previously started with `startTapNote`.
    func stopTapNote(_ midiNote: UInt8) {
        guard tapHeld.contains(midiNote) else { return }
        tapHeld.remove(midiNote)
        let visualNote = tapDisplayNote.removeValue(forKey: midiNote) ?? midiNote
        let lingerVelocity = tapLinger.removeValue(forKey: midiNote)
        let synthCh = tapNoteChannel.removeValue(forKey: midiNote) ?? channel(for: midiNote)
        if let v = lingerVelocity {
            releaseLingering(midiNote: midiNote, synthChannel: synthCh, midiChannel: 0,
                             isDrum: false, originalVelocity: v)
        } else {
            if !midiMode {
                synth.noteOff(midiNote, channel: synthCh)
            }
            midi.noteOff(midiNote, channel: 0)
        }
        // Release the visual immediately on mouse-up. The earlier
        // minVisibleSeconds floor read as visual lag — the user
        // perceives it as the key sticking down past the click. Snap-up
        // matches the keyboard path now.
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.litDownAt.removeValue(forKey: visualNote)
            if self.litNotes.remove(visualNote) != nil {
                self.onLitChanged?()
            }
        }
    }

    // MARK: - Playback notes (PDF drag-in auto-play)
    //
    // Keep a separate channel so a dragged-in score's playback
    // can light up the menubar piano in red without touching the
    // normal `litNotes` user-input set. Sound emits the same way
    // a tap would; only the visual state is parallel.

    // [v1 cutoff] startPlaybackNote / stopPlaybackNote /
    // releaseAllPlaybackNotes removed — only the deleted SheetMusicView
    // (PDF/MIDI score auto-playback) drove them. `playbackLitNotes` stays
    // declared because the menubar renderer + AppDelegate still read it
    // (it simply stays empty until score playback returns post-v1).

    // MARK: - Linger / bell-ring tail

    /// Shared release path for any note that was started under shift.
    /// Sustained voices keep their original noteOn alive (the synth's
    /// release envelope rings out) and get a cleanup noteOff scheduled
    /// for the end of the tail window. Staccato voices close the
    /// original immediately and trigger a doppler retrigger tail on
    /// fresh round-robin channels — this is what lets the user keep
    /// playing live notes over the linger without stealing voices.
    /// Drums always skip the synth.noteOff (existing convention) so
    /// the kit sample plays through.
    /// Semitone intervals (ABOVE the root, root excluded) for a chord quality.
    /// 0 = single (no extensions), 1 = major, 2 = minor, 3 = sus2, 4 = aug,
    /// 5 = dim, 6 = sus4. The 5th (7) is shared by major/minor/sus on
    /// purpose — see `setChordVoices`; aug raises it (8), dim lowers it (6).
    private static func chordIntervals(quality: Int) -> [Int] {
        switch quality {
        case 1: return [4, 7]   // major third + fifth
        case 2: return [3, 7]   // minor third + fifth
        case 3: return [2, 7]   // sus2 (second) + fifth
        case 4: return [4, 8]   // augmented: major third + raised fifth
        case 5: return [3, 6]   // diminished: minor third + lowered fifth
        case 6: return [5, 7]   // sus4 (fourth) + fifth
        default: return []      // single note: root only
        }
    }

    /// Re-voice a held key's chord EXTENSIONS to match `quality`, by diffing
    /// the desired intervals against what's already sounding. Shared intervals
    /// (the 5th across major/minor/sus) are left untouched — so flipping
    /// quality only retriggers the tone that actually changes (the third),
    /// and the root (in `heldNotes`) is never touched at all. That's the
    /// "swap the third, don't retrigger the chord" feel. `pan`/`shift` mirror
    /// the press-time voice so added tones sit where the root sits.
    private func setChordVoices(keyCode: UInt16, rootNote: UInt8, shift: Int,
                               quality: Int) {
        let desired = Set(Self.chordIntervals(quality: quality))
        let pan = MenuBandLayout.panForKeyCode(keyCode)
        heldLock.lock()
        var ext = morphExt[keyCode] ?? [:]
        let current = Set(ext.keys)
        var removedDisplays: [UInt8] = []
        var addedDisplays: [UInt8] = []
        // Remove intervals no longer wanted (hard-cut: a morph should leave a
        // clean stack; the eventual key-up handles the lingering tail).
        for iv in current.subtracting(desired) {
            if let v = ext.removeValue(forKey: iv) {
                if !midiMode { synth.noteOff(v.note, channel: v.channel) }
                midi.noteOff(v.note)
                removedDisplays.append(v.display)
            }
        }
        // Add newly-wanted intervals on fresh round-robin channels.
        for iv in desired.subtracting(current) {
            let vNote = Int(rootNote) + iv
            guard vNote <= 127 else { continue }
            let note = UInt8(vNote)
            let ch = nextMelodicChannel()
            let display = UInt8(max(60, min(83, Int(note) - shift * 12)))
            if !midiMode { synth.setPan(pan, channel: ch) }
            cancelLingerFade(channel: ch)
            primeChannelBend(ch)
            if !midiMode { synth.noteOn(note, velocity: 100, channel: ch) }
            midiNoteOn(note, velocity: 100, channel: 0)
            ext[iv] = (note, ch, display)
            addedDisplays.append(display)
        }
        if ext.isEmpty { morphExt.removeValue(forKey: keyCode) }
        else { morphExt[keyCode] = ext }
        heldLock.unlock()
        guard !removedDisplays.isEmpty || !addedDisplays.isEmpty else { return }
        let relight = { [weak self] in
            guard let self = self else { return }
            var changed = false
            for d in removedDisplays {
                // Keep a cell lit if the root (or another voice) still uses it.
                let stillHeld = self.heldKeyDisplayNote.values.contains(d)
                    || self.morphExt.values.contains { $0.values.contains { $0.display == d } }
                if !stillHeld {
                    self.litDownAt.removeValue(forKey: d)
                    if self.litNotes.remove(d) != nil { changed = true }
                }
            }
            for d in addedDisplays {
                self.litDownAt[d] = CACurrentMediaTime()
                if self.litNotes.insert(d).inserted { changed = true }
            }
            if changed { self.onLitChanged?() }
        }
        if Thread.isMainThread { relight() } else { DispatchQueue.main.async(execute: relight) }
    }

    /// Release a held key's chord extension voices (root is released by the
    /// normal single-note path). Lingers each like Shift when `linger`. Returns
    /// the display cells that were extinguished so the caller can repaint.
    @discardableResult
    private func releaseMorphExt(keyCode: UInt16, linger: Bool) -> [UInt8] {
        heldLock.lock()
        let ext = morphExt.removeValue(forKey: keyCode)
        heldLock.unlock()
        guard let ext = ext, !ext.isEmpty else { return [] }
        var displays: [UInt8] = []
        for (_, v) in ext {
            if linger {
                releaseLingering(midiNote: v.note, synthChannel: v.channel,
                                 midiChannel: 0, isDrum: false, originalVelocity: 100)
            } else {
                if !midiMode { synth.noteOff(v.note, channel: v.channel) }
                midi.noteOff(v.note)
            }
            displays.append(v.display)
        }
        let extinguish = { [weak self] in
            guard let self = self else { return }
            var changed = false
            for d in displays {
                let stillHeld = self.heldKeyDisplayNote.values.contains(d)
                if !stillHeld {
                    self.litDownAt.removeValue(forKey: d)
                    if self.litNotes.remove(d) != nil { changed = true }
                }
            }
            if changed { self.onLitChanged?() }
        }
        if Thread.isMainThread { extinguish() } else { DispatchQueue.main.async(execute: extinguish) }
        return displays
    }

    // MARK: Live chord morph

    /// Forget the morph intent for a key (called on physical key-up). Guarded
    /// by `heldLock` because the morph dictionaries are written from the global
    /// tap's background thread (keyDown) and read on main (the flagsChanged
    /// morph monitor).
    private func clearMorphState(_ keyCode: UInt16) {
        heldLock.lock()
        morphRoot.removeValue(forKey: keyCode)
        morphShift.removeValue(forKey: keyCode)
        morphLinger.removeValue(forKey: keyCode)
        morphQuality.removeValue(forKey: keyCode)
        heldLock.unlock()
    }

    /// Record the press-time morph intent for a key (called from both keyDown
    /// branches). `quality`: 0 = single, 1 = major, 2 = minor, 3 = sus.
    private func setMorphState(_ keyCode: UInt16, root: UInt8, shift: Int,
                              linger: Bool, quality: Int) {
        heldLock.lock()
        morphRoot[keyCode] = root
        morphShift[keyCode] = shift
        morphLinger[keyCode] = linger
        morphQuality[keyCode] = quality
        heldLock.unlock()
    }

    /// Modifier-derived chord shape: 0 = single (no modifier), 1 = major (⌘),
    /// 2 = minor (⌥), 3 = sus2 (⌘⌥), 4 = augmented (⌃), 5 = diminished (⌥⌃),
    /// 6 = sus4 (⌘⌥⌃). The grammar: ⌃ alters the shape you're holding —
    /// major's fifth goes UP (aug), minor's fifth goes DOWN (dim), sus2's
    /// second lifts to a fourth (sus4). One finger per classic triad family,
    /// one combo each for the rest. Mirrors the keyDown chord scheme so a
    /// held note and a freshly-pressed chord agree on what each modifier means.
    private static func chordQuality(modifier: Bool, minor: Bool, sus: Bool, aug: Bool = false) -> Int {
        guard modifier else { return 0 }
        if sus { return aug ? 6 : 3 }
        if aug { return minor ? 5 : 4 }
        return minor ? 2 : 1
    }

    /// Re-voice every physically-held note key to match the current ⌘/⌥/⌃
    /// state. Driven by the AppDelegate's `.flagsChanged` monitors: while you
    /// hold a letter, tapping ⌘ blooms it into a major triad, adding ⌥ swings
    /// it to sus2 (⌃ on top lifts that to sus4), dropping ⌘ leaves minor,
    /// ⌃ alone raises the fifth (aug), ⌥⌃ lowers it instead (dim),
    /// releasing everything collapses back to the lone note — all live, with
    /// the key still down. Idempotent per key (skips when the shape is
    /// unchanged) so the stream of flagsChanged events that a single modifier
    /// press emits doesn't machine-gun retriggers.
    func morphHeldKeys(chordModifier: Bool, chordMinor: Bool, chordSus: Bool, chordAug: Bool = false) {
        let desired = Self.chordQuality(modifier: chordModifier,
                                        minor: chordMinor, sus: chordSus,
                                        aug: chordAug)
        // Snapshot under the lock — the per-key revoice helpers below take
        // `heldLock` themselves (NSLock isn't recursive), so we must NOT hold
        // it across them. Copying the intent dictionaries up front also gives
        // a stable view while we mutate them.
        heldLock.lock()
        let roots = morphRoot
        let shifts = morphShift
        let lingers = morphLinger
        let qualities = morphQuality
        heldLock.unlock()
        for (keyCode, root) in roots {
            let current = qualities[keyCode] ?? 0
            if current == desired { continue }
            let shift = shifts[keyCode] ?? 0
            // The root stays put in `heldNotes` — only the chord extensions
            // change. setChordVoices diffs intervals so the 5th sustains and
            // just the third swaps (or both extensions drop when collapsing to
            // a single note). No retrigger of the root, ever.
            setChordVoices(keyCode: keyCode, rootNote: root, shift: shift,
                           quality: desired)
            // Only commit the new shape if the key is still held — a key-up
            // that raced in between (global tap is on a bg thread) cleared the
            // intent, and we must not resurrect morphQuality for a dead key.
            heldLock.lock()
            if morphRoot[keyCode] != nil { morphQuality[keyCode] = desired }
            heldLock.unlock()
        }
        _ = lingers   // press-time linger is applied at key-up, not on morph
    }

    /// Play a lone melodic root note for `keyCode`. Used by the chord keyDown
    /// path to establish the root in `heldNotes` before `setChordVoices` adds
    /// the extensions — so a chord that's pressed all-at-once shares the exact
    /// same root-in-`heldNotes` structure a morph relies on. Records
    /// `heldKeyLinger` so the eventual key-up rings (or cuts) like any note.
    private func playSingleVoice(keyCode: UInt16, note: UInt8, shift: Int, linger: Bool) {
        let synthCh = nextMelodicChannel()
        let displayNote = UInt8(max(60, min(83, Int(note) - shift * 12)))
        heldLock.lock()
        heldNotes[keyCode] = note
        heldKeyChannel[keyCode] = synthCh
        heldKeyDisplayNote[keyCode] = displayNote
        heldKeyLinger[keyCode] = linger
        heldLock.unlock()
        lastPlayedNote = note
        let pan = MenuBandLayout.panForKeyCode(keyCode)
        if !midiMode { synth.setPan(pan, channel: synthCh) }
        cancelLingerFade(channel: synthCh)
        primeChannelBend(synthCh)
        midi.sendCC(10, value: pan, channel: 0)
        if !midiMode { synth.noteOn(note, velocity: 100, channel: synthCh) }
        midiNoteOn(note, velocity: 100, channel: 0)
        let setLit = { [weak self] in
            guard let self = self else { return }
            self.litDownAt[displayNote] = CACurrentMediaTime()
            if self.litNotes.insert(displayNote).inserted { self.onLitChanged?() }
        }
        if Thread.isMainThread { setLit() } else { DispatchQueue.main.async(execute: setLit) }
    }

    private func releaseLingering(midiNote: UInt8,
                                  synthChannel: UInt8,
                                  midiChannel: UInt8,
                                  isDrum: Bool,
                                  originalVelocity: UInt8) {
        if isDrum {
            // Drums: original noteOn already plays through; no synth
            // noteOff. Just delay the MIDI noteOff so external DAWs
            // see a clean pair without truncating the kit sample.
            let n = midiNote, mc = midiChannel
            DispatchQueue.main.asyncAfter(deadline: .now() + Self.lingerTailSeconds) { [weak self] in
                self?.midi.noteOff(n, channel: mc)
            }
            return
        }
        let category = GeneralMIDI.lingerCategory(for: melodicProgram)
        switch category {
        case .sustained:
            // Sustained patches (organ, pad, brass, choir) hold at
            // full volume until noteOff — the synth's release
            // envelope barely tapers them, so simply deferring the
            // noteOff produced a flat hold followed by a hard cut.
            // Run an explicit volume envelope from 127 → 0 over the
            // linger tail so the user hears every voice fade, then
            // send the cleanup noteOff at the end and reset
            // Expression to 127 so the next note starts at full
            // volume.
            scheduleSustainedLingerFade(midiNote: midiNote,
                                        synthChannel: synthChannel,
                                        midiChannel: midiChannel)
        case .staccato:
            // Close the original cleanly — the staccato sample is
            // already mostly decayed by the time the user releases
            // the key, so the noteOff is mostly bookkeeping. Then
            // schedule the doppler-style retrigger tail.
            if !midiMode { synth.noteOff(midiNote, channel: synthChannel) }
            midi.noteOff(midiNote, channel: midiChannel)
            scheduleStaccatoDoppler(midiNote: midiNote,
                                    midiChannel: midiChannel,
                                    startVelocity: originalVelocity)
        }
    }

    /// In-flight Expression ramp work items per synth channel, so a
    /// new noteOn arriving on a channel mid-fade can cancel the
    /// remaining ramp (and reset Expression to 127) BEFORE playing
    /// — otherwise the new note starts at the fade's current value
    /// and you hear it "sustain into" full volume only as the prior
    /// fade naturally completes. Drove the user's "sustain comes
    /// after the press" complaint.
    private var lingerFadeWork: [UInt8: [DispatchWorkItem]] = [:]

    /// The still-ringing sustained-linger note on each synth channel,
    /// tracked for the lifetime of its fade. A sustained linger keeps
    /// its original noteOn alive while Expression ramps 127→0, so the
    /// note is physically still sounding (just inaudible) until the
    /// tail's cleanup noteOff. If a NEW note reuses this channel before
    /// then, `cancelLingerFade` resets the channel volume back to 127 —
    /// which, without first stopping the old note, snaps the faded-out
    /// note back to full volume (the "it plays again after one press"
    /// bug). We noteOff the tracked note before the volume reset.
    private var lingeringNoteByChannel: [UInt8: UInt8] = [:]

    /// Cancel an in-flight Expression fade on `synthChannel` (if
    /// any) and immediately restore Expression to 127 so the next
    /// note on the channel plays at full volume. Call before every
    /// melodic noteOn that might reuse a recently-faded channel.
    private func cancelLingerFade(channel synthChannel: UInt8) {
        if let items = lingerFadeWork.removeValue(forKey: synthChannel) {
            for w in items { w.cancel() }
        }
        // Kill the still-ringing lingering note on this channel BEFORE
        // restoring the volume — otherwise resetting Expression/Volume
        // to 127 resurrects the faded-out note at full gain.
        if let stale = lingeringNoteByChannel.removeValue(forKey: synthChannel) {
            if !midiMode { synth.noteOff(stale, channel: synthChannel) }
            // Lingering notes are always melodic → outbound MIDI channel 0
            // (matches midiNoteOn / releaseLingering's midiChannel: 0).
            midi.noteOff(stale, channel: 0)
        }
        if !midiMode {
            synth.sendExpression(value: 127, channel: synthChannel)
            synth.resetSampleChannelVolumes(channel: synthChannel)
        }
        // CC11 (Expression) AND CC7 (channel Volume) — Expression is
        // the MIDI-correct envelope-fade CC, Volume is the brute-
        // force one that every DAW instrument always respects.
        // Sending both means the linger fade reaches Live whether
        // the active patch maps Expression to its amp envelope or
        // ignores it entirely.
        midi.sendCC(11, value: 127, channel: synthChannel)
        midi.sendCC(7, value: 127, channel: synthChannel)
    }

    /// Run a smooth Expression-controlled fade over `lingerTailSeconds`
    /// on the channel holding `midiNote`, then send the cleanup
    /// noteOff and restore Expression to 127 so subsequent notes on
    /// the channel come back at full volume. Routes both the
    /// MIDISynth path (CC 11) and the sample-voice path (direct
    /// player volume) so every melodic backend audibly fades — same
    /// experience whether the user is on a GM organ, a pad, or
    /// their own recorded sample.
    private func scheduleSustainedLingerFade(midiNote: UInt8,
                                             synthChannel: UInt8,
                                             midiChannel: UInt8) {
        // Mark this note as ringing-but-fading on its channel so a new
        // note that reuses the channel kills it (see cancelLingerFade)
        // instead of snapping it back to full volume.
        lingeringNoteByChannel[synthChannel] = midiNote
        // 100 hops over 10 s = every 100 ms. Linear ramp:
        // Ableton's stock devices (and most third-party plugins)
        // map CC7/CC11 to a near-linear audible volume curve, so
        // a linear CC drop reads as a steady, audible fade.
        // Squared/ease-out curves dropped to "almost silent" by
        // the midpoint and read as a sharp early cutoff with a
        // long inaudible tail.
        let steps = 100
        let stepInterval = Self.lingerTailSeconds / TimeInterval(steps)
        var items: [DispatchWorkItem] = []
        for i in 0..<steps {
            let t = Float(i + 1) / Float(steps)
            let remaining = 1.0 - t
            let value = UInt8(max(0, min(127, Int((127.0 * remaining).rounded()))))
            let work = DispatchWorkItem { [weak self] in
                guard let self = self else { return }
                if !self.midiMode {
                    self.synth.sendExpression(value: value, channel: synthChannel)
                    self.synth.setSampleNoteVolume(midi: midiNote,
                                                   channel: synthChannel,
                                                   value: value)
                }
                // Belt + suspenders for DAW instruments: ramp both
                // CC11 (Expression — correct envelope mapping) AND
                // CC7 (Volume — universal). Live's stock devices
                // (Operator/Wavetable/Drum Rack) all respond to one
                // or both; sending both means the SHIFT-held tail
                // audibly fades regardless of which mapping the
                // active patch uses.
                self.midi.sendCC(11, value: value, channel: midiChannel)
                self.midi.sendCC(7,  value: value, channel: midiChannel)
            }
            items.append(work)
            DispatchQueue.main.asyncAfter(deadline: .now() + stepInterval * Double(i + 1),
                                          execute: work)
        }
        // Final cleanup noteOff lands a comfortable margin AFTER
        // the last ramp tick so the synth sees CC=0 land cleanly
        // before the noteOff arrives — and CRITICALLY we do NOT
        // snap CC11/CC7 back to 127 here. That snap-back was the
        // "sharp cutoff" the user heard: noteOff with the release
        // envelope still ringing, then instantly setting Volume
        // back to 127 punched the residual tail through at full
        // gain. The reset is now deferred to `cancelLingerFade`,
        // which fires only when a NEW note starts on this channel
        // — by which point any release envelope has run its
        // natural course and a fresh attack is what we're scaling.
        let tail = DispatchWorkItem { [weak self] in
            guard let self = self else { return }
            if !self.midiMode { self.synth.noteOff(midiNote, channel: synthChannel) }
            self.midi.noteOff(midiNote, channel: midiChannel)
            // Fade finished and the note is now off — it can no longer be
            // resurrected, so stop tracking it (only clear if this same
            // note is still the one registered; a newer note may have
            // taken the slot). Don't clear lingerFadeWork[synthChannel]
            // here — leave it so a future noteOn knows the channel is in a
            // faded state and runs cancelLingerFade to restore CC=127.
            if self.lingeringNoteByChannel[synthChannel] == midiNote {
                self.lingeringNoteByChannel.removeValue(forKey: synthChannel)
            }
        }
        items.append(tail)
        // 200 ms breathing room past the last ramp tick. AVAudio's
        // dispatch queue and CoreMIDI both tolerate this fine; the
        // delay is below the perceptual threshold for "the note
        // ended" because volume is already 0 from the ramp.
        DispatchQueue.main.asyncAfter(
            deadline: .now() + Self.lingerTailSeconds + 0.2,
            execute: tail)
        // If a prior fade was still in flight on this channel,
        // cancel it before swapping the tracker — otherwise the old
        // ramp keeps stepping down Expression while the new note
        // tries to ring out.
        if let prev = lingerFadeWork[synthChannel] {
            for w in prev { w.cancel() }
        }
        lingerFadeWork[synthChannel] = items
    }

    /// Fire the same midiNote N times on rotating melodic channels,
    /// each at a smaller velocity and slightly later than the last.
    /// Stops when velocity drops below the floor or after maxSteps.
    /// Each retrigger is paired with its own noteOff so MIDI consumers
    /// get clean event pairs and our synth voice budget recycles.
    private func scheduleStaccatoDoppler(midiNote: UInt8,
                                         midiChannel: UInt8,
                                         startVelocity: UInt8) {
        var step = 0
        var interval = Self.dopplerInitialDelay
        var velocity = Double(startVelocity)
        var cumulative: TimeInterval = 0
        while step < Self.dopplerMaxSteps {
            velocity *= Self.dopplerVelocityDecay
            if velocity < Self.dopplerVelocityFloor { break }
            cumulative += interval
            let v = UInt8(max(1, min(127, Int(velocity.rounded()))))
            let triggerAt = cumulative
            DispatchQueue.main.asyncAfter(deadline: .now() + triggerAt) { [weak self] in
                guard let self = self else { return }
                let ch = self.nextDopplerChannel()
                self.primeChannelBend(ch)
                if !self.midiMode { self.synth.noteOn(midiNote, velocity: v, channel: ch) }
                self.midiNoteOn(midiNote, velocity: v, channel: midiChannel)
                DispatchQueue.main.asyncAfter(deadline: .now() + Self.dopplerNoteOffWindow) { [weak self] in
                    guard let self = self else { return }
                    if !self.midiMode { self.synth.noteOff(midiNote, channel: ch) }
                    self.midi.noteOff(midiNote, channel: midiChannel)
                }
            }
            interval *= Self.dopplerIntervalGrowth
            step += 1
        }
    }

    // MARK: - Permissions

    @discardableResult
    private func ensureAccessibility(prompt: Bool) -> Bool {
        // Always pass false here so the *system* modal doesn't fire. We show
        // our own alert that explains the situation and offers a "Reset grant"
        // escape hatch — the common cause of repeated prompts is a stale TCC
        // entry from a previous (ad-hoc / unsigned) build whose code signing
        // identity no longer matches the current bundle.
        let key = kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String
        let trusted = AXIsProcessTrustedWithOptions([key: false] as CFDictionary)
        if !trusted && prompt {
            DispatchQueue.main.async { self.presentAccessibilityAlert() }
        }
        return trusted
    }

    private func presentAccessibilityAlert() {
        let alert = NSAlert()
        alert.messageText = "Menu Band needs Accessibility access"
        alert.informativeText = """
            To capture keystrokes globally, Menu Band must be enabled in \
            System Settings → Privacy & Security → Accessibility.

            If you've already enabled it but this keeps prompting, the TCC \
            entry is likely stale (left over from a previous build). Use \
            "Reset & Re-prompt" to clear it and grant fresh.
            """
        alert.alertStyle = .informational
        alert.addButton(withTitle: "Open Settings")
        alert.addButton(withTitle: "Reset & Re-prompt")
        alert.addButton(withTitle: "Cancel")
        NSApp.activate(ignoringOtherApps: true)
        let resp = alert.runModal()
        switch resp {
        case .alertFirstButtonReturn:
            if let url = URL(string: "x-apple.systempreferences:com.apple.preference.security?Privacy_Accessibility") {
                NSWorkspace.shared.open(url)
            }
        case .alertSecondButtonReturn:
            // Run `tccutil reset Accessibility <bundleID>` and then ask the
            // *system* to prompt fresh. This creates a new TCC entry tied to
            // our current code-signing identity.
            let bundleID = Bundle.main.bundleIdentifier ?? "computer.aestheticcomputer.menuband"
            let task = Process()
            task.launchPath = "/usr/bin/tccutil"
            task.arguments = ["reset", "Accessibility", bundleID]
            try? task.run()
            task.waitUntilExit()
            // Now request the system prompt explicitly.
            let key = kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String
            _ = AXIsProcessTrustedWithOptions([key: true] as CFDictionary)
        default:
            break
        }
    }

    // MARK: - Voice-by-digit picker (TYPE mode)

    /// Buffer of digit keystrokes typed since the last note play / reset.
    /// Each digit press updates the GM program live (clamped to 0–127);
    /// after 3 digits the next press starts a fresh sequence so the user
    /// can keep typing without an explicit "clear." Resets when any note
    /// key is played, when TYPE mode is disabled, when the buffer
    /// reaches its 3-digit cap, OR when more than
    /// `voiceDigitFlushInterval` elapses since the previous digit —
    /// the type-ahead idiom: a quick burst "1""2""8" still picks voice
    /// 128, but a deliberate single tap after a pause picks that one
    /// digit instead of extending the stale buffer (which read as
    /// "number keys aren't switching the instrument").
    private var voiceDigitBuffer: String = ""
    /// Wall-clock of the last accepted digit press. A gap longer than
    /// `voiceDigitFlushInterval` means the user is picking a new voice,
    /// not continuing a multi-digit number.
    private var voiceDigitLastPress: CFTimeInterval = 0
    /// True after the `-` key was pressed and the next digit will be
    /// read as part of a negative voice slot (currently only `-1`
    /// = KPBJ radio). Same `voiceDigitFlushInterval` timeout as the
    /// digit buffer so a stray `-` doesn't hijack the next typed
    /// voice number.
    private var voiceDigitNegative: Bool = false
    /// Letters typed after a `-` accumulate here so a negative voice can be
    /// picked by NAME instead of slot number — e.g. `-kpbj`, `-nts1`,
    /// `-nts2` each tune the radio backend ("voice −1") to that station.
    /// Cleared on `-`, on a match, on divergence from any known name, and on
    /// the same `voiceDigitFlushInterval` staleness window as the digits.
    private var voiceCommandBuffer: String = ""
    /// Negative voice names recognized after a `-` — one per radio station
    /// id. Matched as the buffer grows so it can bail the moment it diverges
    /// from every known name.
    private static let negativeVoiceNames: Set<String> =
        Set(RadioStation.all.map { $0.id })
    /// Burst window for multi-digit voice entry. Tuned so "128" typed
    /// at a normal pace stays one number, while a re-pick after a beat
    /// starts clean.
    private static let voiceDigitFlushInterval: CFTimeInterval = 0.8

    /// Lowercase letter on a hardware key cap, or nil for non-letter keys.
    /// Used to capture typed voice names like `-kpbj`. ANSI layout.
    @inline(__always)
    private static func letterForKeyCode(_ kc: UInt16) -> Character? {
        switch kc {
        case 0: return "a"; case 1: return "s"; case 2: return "d"
        case 3: return "f"; case 4: return "h"; case 5: return "g"
        case 6: return "z"; case 7: return "x"; case 8: return "c"
        case 9: return "v"; case 11: return "b"; case 12: return "q"
        case 13: return "w"; case 14: return "e"; case 15: return "r"
        case 16: return "y"; case 17: return "t"; case 31: return "o"
        case 32: return "u"; case 34: return "i"; case 35: return "p"
        case 37: return "l"; case 38: return "j"; case 40: return "k"
        case 45: return "n"; case 46: return "m"
        default: return nil
        }
    }

    /// Map a hardware key code to the digit on its key cap, or nil for
    /// non-digit keys. Covers the top number row only — keypad digits
    /// have separate codes and are intentionally skipped (most laptops
    /// don't have one and we don't want a numpad press to silently
    /// repurpose itself).
    @inline(__always)
    private static func digitForKeyCode(_ kc: UInt16) -> Int? {
        switch kc {
        case 29: return 0
        case 18: return 1
        case 19: return 2
        case 20: return 3
        case 21: return 4
        case 23: return 5
        case 22: return 6
        case 26: return 7
        case 28: return 8
        case 25: return 9
        default: return nil
        }
    }

    // MARK: - Key handling (runs on KeyEventTap background thread)
    // Returns true to CONSUME the key event (sink it from the focused app);
    // false to let it pass through.

    private func handleKey(keyCode: UInt16, isDown: Bool, isRepeat: Bool, flags: CGEventFlags) -> Bool {
        // Escape exits TYPE mode (and consumes the keystroke).
        if isDown && keyCode == 53 {  // kVK_Escape
            DispatchQueue.main.async { [weak self] in
                self?.disableTypeMode(playFeedback: true)
            }
            return true
        }
        let hasMod = flags.contains(.maskCommand) || flags.contains(.maskControl) || flags.contains(.maskAlternate)
        // Linger armed when EITHER shift is currently held OR caps lock
        // is on. Caps lock latches the mode so the user can play a
        // long ambient passage without having to keep shift down. The
        // shift *side* now pans the lingering note (left shift → left,
        // right shift → right); caps keeps the natural per-key pan.
        let side = Self.lingerSide(rawFlags: flags.rawValue,
                                   shiftDown: flags.contains(.maskShift),
                                   capsOn: flags.contains(.maskAlphaShift))
        return playKeyEvent(keyCode: keyCode, isDown: isDown, isRepeat: isRepeat,
                            hasModifier: hasMod, lingerSide: side,
                            chordModifier: flags.contains(.maskCommand) || flags.contains(.maskAlternate) || flags.contains(.maskControl),
                            chordMinor: flags.contains(.maskAlternate),
                            chordSus: flags.contains(.maskCommand) && flags.contains(.maskAlternate),
                            chordAug: flags.contains(.maskControl),
                            control: flags.contains(.maskControl))
    }

    /// Sandbox-friendly key path: same note logic as the global tap, but
    /// driven by a local NSEvent monitor on the AppDelegate's invisible
    /// capture panel. No TYPE-mode escape semantics — this path activates
    /// and deactivates with the panel's key-window state, so an "exit"
    /// keystroke isn't needed.
    @discardableResult
    /// `fromPointer` marks a press driven by a click/drag on a QWERTY keycap
    /// rather than a physical key. It plays the identical note; it only keeps
    /// the key out of `keyboardNotesHeld` so the trackpad pitch-bend (which
    /// pins and hides the cursor) stays a keyboard-only feature.
    func handleLocalKey(keyCode: UInt16, isDown: Bool, isRepeat: Bool,
                        flags: NSEvent.ModifierFlags, fromPointer: Bool = false) -> Bool {
        if fromPointer {
            heldLock.lock()
            if isDown { pointerHeldKeys.insert(keyCode) }
            else { pointerHeldKeys.remove(keyCode) }
            heldLock.unlock()
        }
        // ⌘ / ⌥ / ⌃ are the chord modifiers here (⌘ = major, ⌥ = minor,
        // ⌘+⌥ = sus, ⌃ = augmented). This path is only live while
        // quiet-focus is armed, and the user often armed it with right-⌘ —
        // which they're still holding when they start playing, so right-⌘+f
        // intentionally blooms F into an F-major chord (the chord block
        // in playKeyEvent consumes modifier+note before the passthrough gate
        // below, so it never reaches the system as a shortcut).
        // Modified NON-note keys (⌘-Tab, ⌃-arrow, …) still pass through.
        let hasMod = flags.contains(.command) || flags.contains(.control) || flags.contains(.option)
        // Caps lock latches linger so the user can play hands-free
        // without holding shift; shift held still works as a momentary.
        // The shift side pans the lingering note (left → left, right →
        // right); caps lock keeps the note's natural per-key pan.
        let side = Self.lingerSide(rawFlags: UInt64(flags.rawValue),
                                   shiftDown: flags.contains(.shift),
                                   capsOn: flags.contains(.capsLock))
        return playKeyEvent(keyCode: keyCode, isDown: isDown, isRepeat: isRepeat,
                            hasModifier: hasMod, lingerSide: side,
                            chordModifier: flags.contains(.command) || flags.contains(.option) || flags.contains(.control),
                            chordMinor: flags.contains(.option),
                            chordSus: flags.contains(.command) && flags.contains(.option),
                            chordAug: flags.contains(.control),
                            control: flags.contains(.control))
    }

    /// Shared note logic for both the global CGEventTap path and the
    /// local NSEvent panel path. Returns true if the keystroke was
    /// consumed (mapped to a note); false if it should pass through.
    /// `linger` engages bell-ring mode: the note is held by the synth
    /// past key-up so it rings on its release envelope (sustained
    /// voices) rather than cutting on release.
    @discardableResult
    private func playKeyEvent(keyCode: UInt16, isDown: Bool, isRepeat: Bool, hasModifier: Bool, lingerSide: LingerSide = .none, chordModifier: Bool = false, chordMinor: Bool = false, chordSus: Bool = false, chordAug: Bool = false, control: Bool = false) -> Bool {
        // Modifier-chord: holding ⌘/⌥/⌃ + a note key plays & HOLDS that
        // key's triad (the pressed note is the root), lingering on release
        // just like Shift. ⌘ = major, ⌥ = minor, ⌘+⌥ = sus, ⌃ = augmented
        // (the raised-fifth opposite of minor). Chords are global — no
        // left/right sidedness. The release is checked on EVERY key-up (not
        // gated by the modifier still being held) so letting go of the
        // modifier first can't strand a sounding chord.
        if !isDown {
            // End of a physical hold: drop the morph intent so a future ⌘/⌥
            // tap can't resurrect this key. The root + any chord extensions
            // are released below by the normal single-note key-up path (the
            // root lives in `heldNotes`; `releaseMorphExt` clears the rest).
            clearMorphState(keyCode)
        }
        // Percussion-latched keys are chord-immune: when this key's half of
        // the board is drums, ⌘/⌥ fall through so the drum block below fires
        // the drum instead of blooming a triad.
        if chordModifier, isDown, !percussionActive(forKeyCode: keyCode) {
            let shift = octaveShift
            if let root = MenuBandLayout.midiNote(forKeyCode: keyCode,
                                                  octaveShift: shift, keymap: keymap) {
                // CONSUME the whole keystroke — including auto-repeat — so a
                // held ⌘/Option + note combo can never reach the focused
                // app and fire (or repeat-fire) its shortcut (⌘c → Copy,
                // ⌘s → Save) or insert an Option-glyph (⌥a → å). Only the
                // first down triggers the chord.
                if !isRepeat {
                    // Root into `heldNotes` (so it stays put through morphs and
                    // the key-up releases it), then layer the chord extensions
                    // on top — the exact structure a held note morphs into.
                    playSingleVoice(keyCode: keyCode, note: root, shift: shift,
                                    linger: lingerSide.isLingering)
                    let quality = Self.chordQuality(modifier: true, minor: chordMinor,
                                                    sus: chordSus, aug: chordAug)
                    setChordVoices(keyCode: keyCode, rootNote: root, shift: shift,
                                   quality: quality)
                    // Anchor the morph so releasing/adding ⌘/⌥ while the key is
                    // still down re-voices between major/minor/sus/single.
                    setMorphState(keyCode, root: root, shift: shift,
                                  linger: lingerSide.isLingering, quality: quality)
                }
                return true
            }
            // Not a note key — fall through so real ⌘/Option shortcuts
            // that don't collide with a note (⌘-Tab, ⌥-arrow, …) still
            // pass through to the system.
        }

        // Modifier combos pass through so ⌃-c, ⌘-tab etc. work as usual —
        // but ONLY gate this on key-DOWN. A key-up must ALWAYS reach the
        // release path below, even while ⌘/Option is still held; otherwise
        // letting go of a note with a chord modifier down strands the
        // sounding note/chord (the note-off would never fire).
        // …except a chord modifier on a percussion-latched key, which must
        // reach the drum block below (the drum hand stays live while the
        // other hand holds ⌘/⌥ chords on the melodic half).
        if hasModifier && isDown
            && !(chordModifier && percussionActive(forKeyCode: keyCode)) { return false }

        // Brackets latch the sided percussion split: `[` (33) flips the
        // LEFT half of the board to drums, `]` (30) the RIGHT half — `]`
        // gave up its old ++d note for this. Bare presses only (a modified
        // bracket like ⌘-[ passed through above). Consumed in both
        // directions; the toggle cue pans to the side that flipped.
        if keyCode == 33 /* [ */ || keyCode == 30 /* ] */ {
            if isDown && !isRepeat {
                let left = keyCode == 33
                DispatchQueue.main.async { [weak self] in
                    guard let self = self else { return }
                    if left { self.togglePercussionLeft() }
                    else { self.togglePercussionRight() }
                    self.playPercussionToggleCue(
                        on: left ? self.percussionLeft : self.percussionRight,
                        pan: left ? 32 : 96)
                }
            }
            return true
        }

        // Spacebar (keyCode 49) = reverse-replay (notepat-native parity).
        // Plain space rewinds the most-recent audio; we intercept it BEFORE
        // it can fall through to the note/typing path so it never leaks a
        // note or a literal space character. Trigger on the first key-down
        // (ignore auto-repeat); consume key-up too so it stays swallowed.
        if keyCode == 49 /* kVK_Space */ {
            // Hold-to-reverse with a persistent cursor: the first press
            // rewinds from "now"; release banks the playhead; the next press
            // RESUMES from that same reverse point. Playing a note re-anchors
            // the cursor at the live head (see MenuBandRewindVoice).
            if isDown {
                if !isRepeat { rewind() }
            } else {
                rewindRelease()
            }
            return true
        }

        // Enter-latch (notepat-native parity). Return (keyCode 36) is the
        // latch modifier: while it's held, notes you press get latched on
        // release so they sustain indefinitely. Backspace (51) pops the
        // most-recent latched note. Both keys are consumed so they never
        // leak to the focused app.
        if keyCode == 36 /* kVK_Return */ {
            enterLatchHeld = isDown
            if !isDown { latchArmedKeys.removeAll() }
            return true
        }
        if keyCode == 51 /* kVK_Delete (Backspace) */ {
            // Plain Backspace pops the most-recent latched note (LIFO);
            // Shift+Backspace clears them all at once.
            if isDown && !isRepeat {
                if lingerSide.isLingering { unlatchAll() }
                else { unlatchMostRecent() }
            }
            return true
        }

        // Arrow keys step through the GM instrument grid: ←/→ by one
        // slot, ↑/↓ by one row (= InstrumentListView.cols). Mirrors
        // the floating panel's arrow cluster clicks. Each step also
        // fires a short audition blip — same sound the palette plays
        // while the user drags through cells — so the user can scan
        // voices by ear, not just by name. Auto-repeat is allowed so
        // holding an arrow streams through voices.
        switch keyCode {
        case 123:
            if isDown {
                DispatchQueue.main.async { [weak self] in
                    self?.stepMelodicProgramWithBlip(delta: -1)
                }
            }
            return true
        case 124:
            if isDown {
                DispatchQueue.main.async { [weak self] in
                    self?.stepMelodicProgramWithBlip(delta: +1)
                }
            }
            return true
        case 125:
            if isDown {
                let cols = InstrumentListView.cols
                DispatchQueue.main.async { [weak self] in
                    self?.stepMelodicProgramWithBlip(delta: cols)
                }
            }
            return true
        case 126:
            if isDown {
                let cols = InstrumentListView.cols
                DispatchQueue.main.async { [weak self] in
                    self?.stepMelodicProgramWithBlip(delta: -cols)
                }
            }
            return true
        default:
            break
        }

        // Minus key (`-`, keyCode 27) primes a negative voice slot. The
        // trigger is `-1` OR a voice name like `-kpbj`, NOT a bare `-`.
        // Either selects the live KPBJ radio backend ("voice −1": the
        // piano plays the live stream pitched per note, stalls fading into
        // AM-style static). `-1` toggles; `-<name>` accumulates the letters
        // that follow and selects by callsign. Standalone `-` is a no-op so
        // the negative-prefix UX matches how positive voices are typed
        // (the rest of the sequence commits the pick). Consumed in both
        // directions so the key never leaks to the focused app.
        if keyCode == 27 {
            if isDown && !isRepeat {
                voiceDigitBuffer = ""
                voiceCommandBuffer = ""
                voiceDigitNegative = true
                voiceDigitLastPress = CACurrentMediaTime()
            }
            return true
        }

        // Backtick (`, keyCode 50) is the microphone sampler trigger:
        // hold the key to record a clip from the default input device,
        // release to switch the active voice to that clip. Subsequent
        // notes pitch-shift the recording with duration preserved
        // (TimePitch, cents = (midi−60)×100). Pressing any number key flips back to a
        // GM voice (`setMelodicProgram` exits sample mode internally).
        if keyCode == 50 {
            // ~ (Shift+`) ARMS per-key recording while held — it does NOT
            // record the global sample. Plain ` records the global sample
            // (and clears per-key customs) — the "Home" gesture.
            if lingerSide != .none {
                perKeySampleArmed = isDown
                if !isDown, let m = perKeySampleRecordingMidi {
                    // ~ released mid per-key capture — finalize it.
                    if synth.stopSampleRecording() { setSampleBackend(true) }
                    perKeySampleRecordingMidi = nil
                    _ = m
                }
                onInstrumentVisualChange?()
                return true
            }
            // Plain ` = normal global sample (C4 = raw); ⌃+` = chromatic
            // (pitch-corrected) global sample. Two modes of the Sample voice.
            return handleSampleRecordKey(isDown: isDown, isRepeat: isRepeat,
                                         chromatic: control,
                                         source: typeMode ? "type" : "local")
        }

        // ~ held + a note key = record a per-key sample into that key
        // (anchored to its pitch), instead of playing the note.
        if perKeySampleArmed,
           let note = MenuBandLayout.midiNote(forKeyCode: keyCode,
                                              octaveShift: octaveShift,
                                              keymap: keymap) {
            if isDown && !isRepeat {
                perKeySampleRecordingMidi = note
                synth.startSampleRecording(forKey: note)
                onInstrumentVisualChange?()
            } else if !isDown && perKeySampleRecordingMidi == note {
                if synth.stopSampleRecording() { setSampleBackend(true) }
                perKeySampleRecordingMidi = nil
                onInstrumentVisualChange?()
            }
            return true
        }

        // Number-row digits 0–9 select a voice using the chooser
        // grid's 1-based numbering: 0 / 00 / 000 is the MIDI
        // passthrough slot, "1" picks GM program 0 (Acoustic Grand,
        // displayed as voice 1), …, "128" picks program 127. Picking
        // a non-zero voice forces the backend back to internal-synth
        // playback so the user can sweep out of MIDI mode by typing.
        // 3-digit cap means the 4th press starts a fresh sequence.
        // Down-events only.
        if let digit = Self.digitForKeyCode(keyCode) {
            // Track the digit's press/release in the control-keys
            // set so the QWERTY visualization can light it up
            // alongside note-mapped keys. Posting onLitChanged
            // triggers refreshHeldNotes → qwertyMap.litKeyCodes.
            heldLock.lock()
            let inserted = isDown
                ? heldControlKeys.insert(keyCode).inserted
                : heldControlKeys.remove(keyCode) != nil
            heldLock.unlock()
            if inserted {
                let notify: () -> Void = { [weak self] in
                    self?.onLitChanged?()
                }
                if Thread.isMainThread {
                    notify()
                } else {
                    DispatchQueue.main.async(execute: notify)
                }
            }
            if isDown && !isRepeat {
                // Start fresh if the buffer hit its cap OR enough time
                // passed since the last digit that this is plainly a
                // new pick, not the next digit of a longer number.
                let now = CACurrentMediaTime()
                let staleGap = now - voiceDigitLastPress
                    > Self.voiceDigitFlushInterval
                // Negative-voice slot, primed by a preceding `-`.
                // `-1` is the only negative voice today (KPBJ radio);
                // it toggles, so a second `-1` switches back to the
                // last GM voice. Other digits after `-` are no-ops —
                // we consume them so they don't quietly pick a GM
                // voice the user wasn't aiming for.
                if voiceDigitNegative && !staleGap {
                    voiceDigitNegative = false
                    voiceDigitLastPress = now
                    if digit == 1 {
                        DispatchQueue.main.async { [weak self] in
                            self?.toggleRadioBackend()
                        }
                    }
                    return true
                }
                // A stale `-` doesn't get to hijack the next typed
                // voice number — clear the prefix and fall through to
                // normal positive-digit handling.
                voiceDigitNegative = false
                if voiceDigitBuffer.count >= 3 || staleGap {
                    voiceDigitBuffer = ""
                }
                voiceDigitLastPress = now
                voiceDigitBuffer.append(String(digit))
                let buffer = voiceDigitBuffer
                DispatchQueue.main.async { [weak self] in
                    guard let self = self, let v = Int(buffer) else { return }
                    if v == 0 {
                        if !self.midiMode { self.toggleMIDIMode() }
                        return
                    }
                    // Typing a non-zero voice number is the user's
                    // explicit "play this voice locally" gesture —
                    // exit MIDI mode so the local synth is audible.
                    // (`0` is the inverse gesture, kept above.)
                    if self.midiMode { self.toggleMIDIMode() }
                    let program = UInt8(max(0, min(127, v - 1)))
                    self.setMelodicProgram(program)
                }
            }
            return true
        }

        // Negative voice by NAME: once `-` has primed negative mode, the
        // letters that follow spell a voice callsign — `-kpbj` selects the
        // KPBJ radio. We accumulate letters and match against the known
        // names as the buffer grows, bailing the moment it can't be any of
        // them. The letter keys are consumed (no note plays) only while a
        // name is still plausibly being typed; a stale `-` or a divergent
        // letter releases them back to normal note play.
        if voiceDigitNegative, isDown, !isRepeat,
           let ch = Self.letterForKeyCode(keyCode) {
            let now = CACurrentMediaTime()
            if now - voiceDigitLastPress > Self.voiceDigitFlushInterval {
                // Stale prefix — abandon the capture and let this key play
                // as a normal note (fall through to the handlers below).
                voiceDigitNegative = false
                voiceCommandBuffer = ""
            } else {
                voiceDigitLastPress = now
                voiceCommandBuffer.append(ch)
                let token = voiceCommandBuffer
                let isPrefix = Self.negativeVoiceNames.contains { $0.hasPrefix(token) }
                if Self.negativeVoiceNames.contains(token) {
                    // Complete match → tune the radio to that station and
                    // engage it (idempotent: a name selects, it doesn't
                    // toggle off like `-1` does).
                    voiceDigitNegative = false
                    voiceCommandBuffer = ""
                    let station = RadioStation.by(id: token)
                    DispatchQueue.main.async { [weak self] in
                        self?.selectRadioStation(station)
                    }
                } else if !isPrefix {
                    // Diverged from every known name → stop capturing.
                    voiceDigitNegative = false
                    voiceCommandBuffer = ""
                }
                return true  // consumed the letter while capturing a name
            }
        }

        // Octave shift: notepat uses , (43) / . (47); Ableton uses z (6) /
        // x (7) since those are unmapped in Live's M-mode keymap and the
        // comma/period live next to mapped notes there. Acts globally —
        // works the same in TYPE mode and via the popover's local-key
        // forwarding. Hold-to-ramp: a single tap moves one octave;
        // holding the key sets up our own metronome (~280 ms initial
        // delay, then ~160 ms between repeats) so the octave notches
        // predictably while the key is held — independent of the OS's
        // key-repeat settings.
        let (octDownKC, octUpKC) = MenuBandLayout.octaveKeyCodes(for: keymap)
        if keyCode == octDownKC || keyCode == octUpKC {
            let delta = (keyCode == octDownKC) ? -1 : +1
            // One press = one octave step. Auto-repeat (system AND
            // our own hold timer) is disabled per jas's preference:
            // chained octaves jumped the keyboard further than
            // intended on a single sustained press.
            if isDown && !isRepeat {
                octaveStepOnce(delta: delta)
            }
            return true
        }

        let shift = octaveShift // a single UserDefaults read; cheap

        // Right-hand percussion split: when armed, upper-octave keys
        // (intrinsic semitone ≥ 12) fire the AC-native drum kit instead of
        // a melodic note — octave-invariant, so the drum is picked by
        // `semitone % 12`. One-shot: triggers on key-down, ignores linger,
        // and consumes the key-up with no melodic note ever stored.
        // Sided percussion: this key fires a drum (not a melodic note) when
        // the HALF of the board it sits on is latched — left half (display
        // note < lingerSplitMidi) follows `percussionLeft`, right half
        // follows `percussionRight`. Drum chosen by pitch class. One-shot:
        // triggers on key-down, consumes key-up, never stores a melodic note.
        let percDisplayNote: UInt8? = MenuBandLayout
            .midiNote(forKeyCode: keyCode, octaveShift: shift, keymap: keymap)
            .map { UInt8(max(60, min(83, Int($0) - shift * 12))) }
        let percActiveForKey: Bool = {
            // Chord modifiers (⌘/⌥) don't disqualify a drum key — drums are
            // chord-immune. Other modifiers (⌃) still pass the key through.
            guard !hasModifier || chordModifier, let dn = percDisplayNote else { return false }
            return Int(dn) < MenuBandLayout.lingerSplitMidi ? percussionLeft : percussionRight
        }()
        if percActiveForKey,
           let semi = MenuBandLayout.semitone(forKeyCode: keyCode, keymap: keymap) {
            if isDown {
                if !isRepeat {
                    let drum = MenuBandPercussion.Drum.forPitchClass(semi % 12)
                    let pan = MenuBandLayout.panForKeyCode(keyCode)
                    // Shift (the linger arm) ACCENTS the drum — a harder,
                    // more intense hit (the percussion analogue of linger).
                    let accent = lingerSide.isLingering
                    let group = synth.percussionNoteOn(drum, velocity: 100, pan: pan,
                                                       accent: accent)
                    let dn = percDisplayNote
                    heldLock.lock()
                    heldDrumKeys[keyCode] = group
                    if let dn { heldDrumDisplay[keyCode] = dn }
                    heldLock.unlock()
                    if let dn { drumLitOn(dn) }
                }
            } else {
                heldLock.lock()
                let group = heldDrumKeys.removeValue(forKey: keyCode)
                let dn = heldDrumDisplay.removeValue(forKey: keyCode)
                heldLock.unlock()
                if let group { synth.percussionNoteOff(group) }
                if let dn { drumLitOff(dn) }
            }
            return true
        }

        if isDown {
            if isRepeat { return true }  // consume repeats but don't retrigger
            guard let note = MenuBandLayout.midiNote(forKeyCode: keyCode, octaveShift: shift, keymap: keymap) else {
                // Unmapped key: still consume in TYPE mode so it doesn't leak
                // through to the focused app.
                return true
            }
            // Match the menubar-tap path's voice allocation: round-robin
            // across 8 melodic channels so rapid retriggers don't stomp
            // each other on channel 0. Without this, keyboard play sounds
            // noticeably different from mouse play (more voice-stealing,
            // sharper attack tail-off). MIDI out still goes to channel 0
            // so DAW tracks listening on one channel get every note.
            let synthCh = nextMelodicChannel()
            let displayNote: UInt8 = {
                let v = Int(note) - shift * 12
                let clamped = max(60, min(83, v))
                return UInt8(clamped)
            }()
            // Sided linger: left shift rings out only the left half of the
            // board (display note below the split), right shift only the
            // right half; caps lock / both shifts ring the whole board.
            // Keying off the display note keeps which notes linger in sync
            // with which letters the menubar uppercases.
            let linger: Bool = {
                switch lingerSide {
                case .none: return false
                case .neutral: return true
                case .left: return Int(displayNote) < MenuBandLayout.lingerSplitMidi
                case .right: return Int(displayNote) >= MenuBandLayout.lingerSplitMidi
                }
            }()
            heldLock.lock()
            let prevNote = heldNotes[keyCode]
            let prevCh = heldKeyChannel[keyCode]
            let prevDisplay = heldKeyDisplayNote[keyCode]
            heldNotes[keyCode] = note
            heldKeyChannel[keyCode] = synthCh
            heldKeyDisplayNote[keyCode] = displayNote
            heldKeyLinger[keyCode] = linger
            // Morph anchor: a plain note press starts as a single voice, but
            // tapping ⌘/⌥ while it's still held blooms it into a chord live.
            morphRoot[keyCode] = note
            morphShift[keyCode] = shift
            morphLinger[keyCode] = linger
            morphQuality[keyCode] = 0
            heldLock.unlock()
            if let prevNote = prevNote {
                if !midiMode { synth.noteOff(prevNote, channel: prevCh ?? 0) }
                midi.noteOff(prevNote)
            }
            // A note press confirms the picked voice — clear so the next
            // digit starts a fresh sequence instead of extending the old.
            voiceDigitBuffer = ""
            lastPlayedNote = note
            // Stereo pan from the qwerty key's physical column —
            // mirrors notepat native, so left-hand keys play left and
            // right-hand keys play right. CC10 to both the local
            // synth (MIDISynth backend) and the outbound MIDI port,
            // sent BEFORE noteOn so the new note is panned from the
            // first sample.
            let pan = MenuBandLayout.panForKeyCode(keyCode)
            if !midiMode { synth.setPan(pan, channel: synthCh) }
            // Cancel any leftover linger fade on this channel so the
            // new note attacks at full volume (see startTapNote for
            // the parallel call site / rationale).
            cancelLingerFade(channel: synthCh)
            // Center (or current-bend) this channel BEFORE the noteOn so it
            // can't inherit a stale bend left by an earlier gesture on the
            // same round-robin slot.
            primeChannelBend(synthCh)
            midi.sendCC(10, value: pan, channel: 0)
            if !midiMode { synth.noteOn(note, velocity: 100, channel: synthCh) }
            midiNoteOn(note, velocity: 100, channel: 0)
            // The menubar piano renders a fixed C4–C5 window; the audio
            // path plays at the user's full octave-shifted pitch. To keep
            // the visual lit state honest, mark the *display* note (note
            // minus the octave delta, clamped to the visible range) as
            // pressed — keyboard 'z' at octaveShift=3 plays MIDI 96 but
            // lights up the visible C4 (MIDI 60). Lit state must update
            // synchronously when called on main (the local-capture path)
            // so the menubar redraws within the same runloop pass as the
            // keystroke; the global tap runs on a CGEventTap background
            // thread, hop to main there.
            let setLit = { [weak self] in
                guard let self = self else { return }
                if let prevDisplay = prevDisplay, prevDisplay != displayNote {
                    self.litDownAt.removeValue(forKey: prevDisplay)
                    if self.litNotes.remove(prevDisplay) != nil {
                        self.onLitChanged?()
                    }
                }
                self.litDownAt[displayNote] = CACurrentMediaTime()
                if self.litNotes.insert(displayNote).inserted {
                    self.onLitChanged?()
                }
            }
            if Thread.isMainThread { setLit() } else { DispatchQueue.main.async(execute: setLit) }
            // If Enter is held while this note is pressed, arm it so its
            // upcoming key-up latches (sustains) instead of releasing.
            if enterLatchHeld {
                latchArmedKeys.insert(keyCode)
                if !latchedKeys.contains(keyCode) { latchedKeys.append(keyCode) }
            }
            return true
        } else {
            // Latched key: swallow the key-up so the note keeps ringing.
            // It stays in heldNotes / litNotes (visualizer shows it held)
            // until Backspace pops it via unlatchKey(_:).
            if latchArmedKeys.contains(keyCode) {
                // The physical key is now UP even though the note keeps
                // sounding — poke the lit-changed hook so AppDelegate
                // re-evaluates `keyboardNotesHeld` and releases the
                // trackpad pitch-bend lock (latched ≠ physically held).
                let notify: () -> Void = { [weak self] in self?.onLitChanged?() }
                if Thread.isMainThread { notify() }
                else { DispatchQueue.main.async(execute: notify) }
                return true
            }
            heldLock.lock()
            let note = heldNotes.removeValue(forKey: keyCode)
            let synthCh = heldKeyChannel.removeValue(forKey: keyCode) ?? 0
            let displayNote = heldKeyDisplayNote.removeValue(forKey: keyCode)
            let wasLinger = heldKeyLinger.removeValue(forKey: keyCode) ?? false
            heldLock.unlock()
            // Release any chord extensions stacked on this key (⌘/⌥ morph),
            // ringing them out with the same linger the root gets.
            releaseMorphExt(keyCode: keyCode, linger: wasLinger)
            guard let releasedNote = note else { return true }  // consume the up too
            if wasLinger {
                // Keyboard always plays melodic (QWERTY notes start at
                // C4); originalVelocity is fixed at 100 in the keyDown
                // branch. Funnel through the shared sustained/staccato
                // splitter so the doppler tail engages on plucked GM
                // voices automatically.
                releaseLingering(midiNote: releasedNote,
                                 synthChannel: synthCh,
                                 midiChannel: 0,
                                 isDrum: false,
                                 originalVelocity: 100)
            } else {
                if !midiMode { synth.noteOff(releasedNote, channel: synthCh) }
                midi.noteOff(releasedNote)
            }
            // Extinguish the *display* lit cell, not the played pitch —
            // the played pitch may be far outside the visible range when
            // octaveShift > 0, but the lit highlight always lives in 60–83.
            // No minVisibleSeconds delay: release the visual the instant
            // the key is released for a snappy press-and-up feel.
            let visualNote = displayNote ?? releasedNote
            let extinguish = { [weak self] in
                guard let self = self else { return }
                self.litDownAt.removeValue(forKey: visualNote)
                if self.litNotes.remove(visualNote) != nil {
                    self.onLitChanged?()
                }
            }
            if Thread.isMainThread { extinguish() } else { DispatchQueue.main.async(execute: extinguish) }
            return true
        }
    }

    /// Plain Backspace: release the most-recently latched note (LIFO).
    /// No-op when nothing is latched.
    private func unlatchMostRecent() {
        guard let keyCode = latchedKeys.popLast() else { return }
        unlatchKey(keyCode)
    }

    /// Shift+Backspace: release ALL currently-latched notes at once.
    private func unlatchAll() {
        // Snapshot — unlatchKey mutates `latchedKeys` as it goes.
        for keyCode in latchedKeys { unlatchKey(keyCode) }
        latchedKeys.removeAll()
    }

    /// Release a single latched key — mirrors the normal key-up branch
    /// of `playKeyEvent` (linger-aware note-off + extinguish the lit
    /// cell) but is driven by Backspace rather than the physical key-up.
    private func unlatchKey(_ keyCode: UInt16) {
        latchArmedKeys.remove(keyCode)
        latchedKeys.removeAll { $0 == keyCode }
        heldLock.lock()
        let note = heldNotes.removeValue(forKey: keyCode)
        let synthCh = heldKeyChannel.removeValue(forKey: keyCode) ?? 0
        let displayNote = heldKeyDisplayNote.removeValue(forKey: keyCode)
        let wasLinger = heldKeyLinger.removeValue(forKey: keyCode) ?? false
        heldLock.unlock()
        clearMorphState(keyCode)
        releaseMorphExt(keyCode: keyCode, linger: wasLinger)
        guard let releasedNote = note else { return }
        if wasLinger {
            releaseLingering(midiNote: releasedNote, synthChannel: synthCh,
                             midiChannel: 0, isDrum: false, originalVelocity: 100)
        } else {
            if !midiMode { synth.noteOff(releasedNote, channel: synthCh) }
            midi.noteOff(releasedNote)
        }
        let visualNote = displayNote ?? releasedNote
        let extinguish = { [weak self] in
            guard let self = self else { return }
            self.litDownAt.removeValue(forKey: visualNote)
            if self.litNotes.remove(visualNote) != nil { self.onLitChanged?() }
        }
        if Thread.isMainThread { extinguish() } else { DispatchQueue.main.async(execute: extinguish) }
    }

    /// Hard stop for the fleet Stop: release every held note and silence all
    /// sounding voices — melodic and percussion.
    func panic() {
        releaseAllHeldNotes()
        synth.percussion.silence()
        synth.panic()
    }

    func releaseAllHeldNotes() {
        heldLock.lock()
        let noteSnapshot = heldNotes
        let chanSnapshot = heldKeyChannel
        heldNotes.removeAll()
        heldKeyChannel.removeAll()
        heldKeyDisplayNote.removeAll()
        heldKeyLinger.removeAll()
        // Drop latch bookkeeping too — a blanket release means no note
        // is held anymore, latched or otherwise.
        latchArmedKeys.removeAll()
        latchedKeys.removeAll()
        enterLatchHeld = false
        let chordSnapshot = morphExt
        morphExt.removeAll()
        morphRoot.removeAll()
        morphShift.removeAll()
        morphLinger.removeAll()
        morphQuality.removeAll()
        let drumSnapshot = heldDrumKeys
        let drumDisplaySnapshot = heldDrumDisplay
        heldDrumKeys.removeAll()
        heldDrumDisplay.removeAll()
        heldLock.unlock()
        // Silence any held chord-extension voices (panic() below also catches
        // these, but be explicit so MIDI listeners get clean note-offs). The
        // roots ride along in `noteSnapshot` and are released further down.
        for (_, ext) in chordSnapshot {
            for (_, v) in ext {
                synth.noteOff(v.note, channel: v.channel)
                midi.noteOff(v.note)
            }
        }
        // Release any held drum voices (open-hat foot pedal up) so a held
        // key doesn't ring forever across an octave change / split toggle,
        // and extinguish their hold lights.
        for (_, group) in drumSnapshot { synth.percussionNoteOff(group) }
        for (_, dn) in drumDisplaySnapshot { drumLitOff(dn) }
        let tapSnapshot = tapHeld
        let tapChanSnapshot = tapNoteChannel
        tapHeld.removeAll()
        tapNoteChannel.removeAll()
        tapDisplayNote.removeAll()
        for (keyCode, note) in noteSnapshot {
            let ch = chanSnapshot[keyCode] ?? 0
            synth.noteOff(note, channel: ch)
            midi.noteOff(note)
        }
        for note in tapSnapshot {
            let synthCh = tapChanSnapshot[note] ?? channel(for: note)
            synth.noteOff(note, channel: synthCh)
            midi.noteOff(note, channel: 0)
        }
        midi.sendAllNotesOff()
        synth.panic()
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            if !self.litNotes.isEmpty {
                self.litNotes.removeAll()
                self.litDownAt.removeAll()
                self.onLitChanged?()
            }
        }
    }
}
