import AppKit
import ApplicationServices
import CoreGraphics

final class MenuBandController {
    private let midi = MenuBandMIDI()
    private let synth = MenuBandSynth()
    private var keyTap: KeyEventTap?
    private var heldNotes: [UInt16: UInt8] = [:]
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
    private let heldLock = NSLock()

    private let midiModeKey = "notepat.midiMode"
    private let typeModeKey = "notepat.typeMode"
    private let octaveShiftKey = "notepat.octaveShift"
    private let melodicProgramKey = "notepat.melodicProgram"
    private let keymapKey = "notepat.keymap"
    /// Active instrument backend: `"gm"` for the General MIDI bank, or
    /// `"gb"` for a GarageBand sampler patch. Default is GM. Stored as a
    /// string so future backends (Logic, EXS3rd-party, etc.) can be
    /// added without breaking older saved values.
    private let instrumentBackendKey = "notepat.instrumentBackend"
    /// File URL string of the GarageBand patch the user picked. Empty
    /// when no GB patch has been selected yet (we'll fall back to the
    /// first scanned patch when the backend is GarageBand and this is
    /// missing).
    private let garageBandPatchPathKey = "notepat.garageBandPatchPath"

    // Visual state — accessed only on the main thread.
    private(set) var litNotes: Set<UInt8> = []
    private var litDownAt: [UInt8: CFTimeInterval] = [:]
    private let minVisibleSeconds: CFTimeInterval = 0.18

    var onChange: (() -> Void)?
    var onLitChanged: (() -> Void)?
    var onInstrumentVisualChange: (() -> Void)?

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
        heldLock.lock(); defer { heldLock.unlock() }
        return Set(heldNotes.keys).union(heldControlKeys)
    }

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
        return !heldNotes.isEmpty
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

    /// Apply a single octave step + percussive click + UI refresh.
    /// Clamps to ±4 octaves; no-ops once the user is at the limit so
    /// the click stops giving false feedback while held against the
    /// stop.
    private func octaveStepOnce(delta: Int) {
        let next = max(-4, min(4, octaveShift + delta))
        if next == octaveShift { return }
        octaveShift = next
        playOctaveClick(for: next)
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
            releaseAllHeldNotes()
            onChange?()
        }
    }

    var melodicProgram: UInt8 {
        let raw = UserDefaults.standard.integer(forKey: melodicProgramKey)
        return UInt8(max(0, min(127, raw)))
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
    func setBend(amount: Float) {
        let clamped = max(-1, min(1, amount))
        let value = Int16(clamped * 8192)
        // Channels currently sounding via either input path. Use
        // sets to dedupe — same channel can host both a tap and a
        // keyboard note when the user is using both at once.
        var channels: Set<UInt8> = []
        for ch in tapNoteChannel.values { channels.insert(ch) }
        for ch in heldKeyChannel.values { channels.insert(ch) }
        if channels.isEmpty {
            // Send to channel 0 as a fallback so MIDI listeners
            // that pre-route on a fixed channel still get the
            // bend even when nothing is held locally.
            channels.insert(0)
        }
        debugLog("setBend amt=\(clamped) value=\(value) channels=\(channels) midiMode=\(midiMode)")
        if !midiMode {
            for ch in channels { synth.sendPitchBend(value: value, channel: ch) }
        }
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

    func stepOctave(delta: Int) {
        octaveStepOnce(delta: delta)
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

    /// Forward an RMS callback to the underlying sample voice's input
    /// tap. Called once per recording block (~93 ms) on the main queue
    /// with that block's RMS. AppDelegate hooks this up in
    /// `applicationDidFinishLaunching` and stashes the latest value
    /// for the visualizer animation tick to read on its next frame.
    func setSampleLevelHandler(_ handler: ((Float) -> Void)?) {
        synth.onSampleLevel = handler
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

    /// Linger tail length — how long after key release we hold the
    /// note before sending the cleanup noteOff. Long enough that the
    /// synth's natural release envelope can decay to silence (~3-5s on
    /// most GM voices), short enough that an external DAW doesn't
    /// accumulate hung notes when the user shift-spams keys.
    private static let lingerTailSeconds: TimeInterval = 6.0

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
        // Drum routing is decided by the *visual* key (the cell the user
        // tapped), not the post-octave played pitch. Without this, a
        // melodic tap at octaveShift < 0 plays below MIDI 60 and the old
        // played-pitch test mis-routed it to channel 9 (drum kit) — so
        // the same key on the QWERTY (which has no isDrum check) and the
        // on-screen piano played different patches. The menubar piano's
        // visible range is melodic-only (60–83), so visual-based routing
        // keeps both input methods consistent at any octave.
        let isDrum = visualNote < UInt8(KeyboardIconRenderer.firstMidi)
        // Synth: rotate across 8 channels so rapid same-note taps overlap
        // (different channels = different voices, no stealing). MIDI: always
        // land on channel 1 (drums on 10) so an Ableton track listening on
        // a single channel actually receives every note.
        let synthCh: UInt8 = isDrum ? 9 : nextMelodicChannel()
        let midiCh: UInt8 = isDrum ? 9 : 0
        tapNoteChannel[midiNote] = synthCh
        midi.sendCC(10, value: pan, channel: midiCh)
        // Mirror the keyboard path: also send pan to the local synth's
        // per-channel state so synth state stays symmetric across input
        // methods. Without this, channels written by keyboard carry over
        // their pan into subsequent taps on the same channel.
        if !midiMode && !isDrum { synth.setPan(pan, channel: synthCh) }
        if !midiMode { synth.noteOn(midiNote, velocity: velocity, channel: synthCh) }
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
        // Mirror startTapNote: drum routing follows the *visual* key,
        // not the played pitch.
        let visualNote = tapDisplayNote[midiNote] ?? midiNote
        let midiCh: UInt8 = visualNote < UInt8(KeyboardIconRenderer.firstMidi) ? 9 : 0
        midi.sendCC(10, value: pan, channel: midiCh)
    }

    /// Release a note previously started with `startTapNote`.
    func stopTapNote(_ midiNote: UInt8) {
        guard tapHeld.contains(midiNote) else { return }
        tapHeld.remove(midiNote)
        let visualNote = tapDisplayNote.removeValue(forKey: midiNote) ?? midiNote
        let lingerVelocity = tapLinger.removeValue(forKey: midiNote)
        let synthCh = tapNoteChannel.removeValue(forKey: midiNote) ?? channel(for: midiNote)
        // Drum routing follows the visual key (see startTapNote).
        let isDrum = visualNote < UInt8(KeyboardIconRenderer.firstMidi)
        let midiCh: UInt8 = isDrum ? 9 : 0
        // Drums are one-shot percussion: do NOT send synth.noteOff. Letting
        // the sample play through is what makes rapid taps overlap correctly
        // instead of cutting each other off. The MIDI port still sends noteOff
        // so external sequencers (Ableton drum racks) get a clean event pair.
        // Internal synth is silent in MIDI mode anyway.
        if let v = lingerVelocity {
            releaseLingering(midiNote: midiNote, synthChannel: synthCh, midiChannel: midiCh,
                             isDrum: isDrum, originalVelocity: v)
        } else {
            if !isDrum && !midiMode {
                synth.noteOff(midiNote, channel: synthCh)
            }
            midi.noteOff(midiNote, channel: midiCh)
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
            // Skip the immediate noteOff so the synth's release
            // envelope rings out. Cleanup pair lands at +tail.
            let n = midiNote, ch = synthChannel, mc = midiChannel
            DispatchQueue.main.asyncAfter(deadline: .now() + Self.lingerTailSeconds) { [weak self] in
                guard let self = self else { return }
                if !self.midiMode { self.synth.noteOff(n, channel: ch) }
                self.midi.noteOff(n, channel: mc)
            }
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
    /// key is played, when TYPE mode is disabled, or when the buffer
    /// reaches its 3-digit cap.
    private var voiceDigitBuffer: String = ""

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
        // long ambient passage without having to keep shift down.
        let linger = flags.contains(.maskShift) || flags.contains(.maskAlphaShift)
        return playKeyEvent(keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, hasModifier: hasMod, linger: linger)
    }

    /// Sandbox-friendly key path: same note logic as the global tap, but
    /// driven by a local NSEvent monitor on the AppDelegate's invisible
    /// capture panel. No TYPE-mode escape semantics — this path activates
    /// and deactivates with the panel's key-window state, so an "exit"
    /// keystroke isn't needed.
    @discardableResult
    func handleLocalKey(keyCode: UInt16, isDown: Bool, isRepeat: Bool, flags: NSEvent.ModifierFlags) -> Bool {
        let hasMod = flags.contains(.command) || flags.contains(.control) || flags.contains(.option)
        // Caps lock latches linger so the user can play hands-free
        // without holding shift; shift held still works as a momentary.
        let linger = flags.contains(.shift) || flags.contains(.capsLock)
        return playKeyEvent(keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, hasModifier: hasMod, linger: linger)
    }

    /// Shared note logic for both the global CGEventTap path and the
    /// local NSEvent panel path. Returns true if the keystroke was
    /// consumed (mapped to a note); false if it should pass through.
    /// `linger` engages bell-ring mode: the note is held by the synth
    /// past key-up so it rings on its release envelope (sustained
    /// voices) rather than cutting on release.
    @discardableResult
    private func playKeyEvent(keyCode: UInt16, isDown: Bool, isRepeat: Bool, hasModifier: Bool, linger: Bool = false) -> Bool {
        // Modifier combos pass through so cmd-c, cmd-tab etc. work as usual.
        if hasModifier { return false }

        // Minus key (`-`, keyCode 27) toggles the live KPBJ radio backend
        // — conceptually "voice −1": the piano plays the live stream
        // pitched by 2^((note−60)/12), with stalls fading into AM-style
        // static. Consumed in both directions so it never leaks to the
        // focused app, but only the down-event flips the mode.
        if keyCode == 27 {
            if isDown && !isRepeat {
                DispatchQueue.main.async { [weak self] in
                    self?.toggleRadioBackend()
                }
            }
            return true
        }

        // Backtick (`, keyCode 50) is the microphone sampler trigger:
        // hold the key to record a clip from the default input device,
        // release to switch the active voice to that clip. Subsequent
        // notes play the recording back at varispeed (rate =
        // 2^((midi−60)/12)). Pressing any number key flips back to a
        // GM voice (`setMelodicProgram` exits sample mode internally).
        if keyCode == 50 {
            if isDown && !isRepeat {
                DispatchQueue.main.async { [weak self] in
                    guard let self = self else { return }
                    self.synth.startSampleRecording()
                    // Nudge the AppDelegate so the menubar icon
                    // immediately picks up the red "REC" tint on the
                    // chip — sampleRecordingActive flipped, but the
                    // icon won't repaint without a callback.
                    self.onInstrumentVisualChange?()
                }
            } else if !isDown {
                DispatchQueue.main.async { [weak self] in
                    guard let self = self else { return }
                    let usable = self.synth.stopSampleRecording()
                    if usable {
                        self.setSampleBackend(true)
                    } else {
                        // Recording was too short / discarded — still
                        // need to repaint so the icon drops the red
                        // tint. setSampleBackend would have done this
                        // for us in the usable branch.
                        self.onInstrumentVisualChange?()
                    }
                }
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
                if voiceDigitBuffer.count >= 3 { voiceDigitBuffer = "" }
                voiceDigitBuffer.append(String(digit))
                let buffer = voiceDigitBuffer
                DispatchQueue.main.async { [weak self] in
                    guard let self = self, let v = Int(buffer) else { return }
                    if v == 0 {
                        if !self.midiMode { self.toggleMIDIMode() }
                        return
                    }
                    if self.midiMode { self.toggleMIDIMode() }
                    let program = UInt8(max(0, min(127, v - 1)))
                    self.setMelodicProgram(program)
                }
            }
            return true
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
            if isDown {
                if isRepeat { return true }   // we drive our own repeats
                octaveStepOnce(delta: delta)
                startOctaveHold(keyCode: keyCode, delta: delta)
            } else {
                stopOctaveHold(forKeyCode: keyCode)
            }
            return true
        }

        let shift = octaveShift // a single UserDefaults read; cheap

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
            heldLock.lock()
            let prevNote = heldNotes[keyCode]
            let prevCh = heldKeyChannel[keyCode]
            let prevDisplay = heldKeyDisplayNote[keyCode]
            heldNotes[keyCode] = note
            heldKeyChannel[keyCode] = synthCh
            heldKeyDisplayNote[keyCode] = displayNote
            heldKeyLinger[keyCode] = linger
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
            return true
        } else {
            heldLock.lock()
            let note = heldNotes.removeValue(forKey: keyCode)
            let synthCh = heldKeyChannel.removeValue(forKey: keyCode) ?? 0
            let displayNote = heldKeyDisplayNote.removeValue(forKey: keyCode)
            let wasLinger = heldKeyLinger.removeValue(forKey: keyCode) ?? false
            heldLock.unlock()
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

    func releaseAllHeldNotes() {
        heldLock.lock()
        let noteSnapshot = heldNotes
        let chanSnapshot = heldKeyChannel
        heldNotes.removeAll()
        heldKeyChannel.removeAll()
        heldKeyDisplayNote.removeAll()
        heldKeyLinger.removeAll()
        heldLock.unlock()
        let tapSnapshot = tapHeld
        let tapChanSnapshot = tapNoteChannel
        let tapDisplaySnapshot = tapDisplayNote
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
            // Drum routing follows the visual key (see startTapNote).
            let visualNote = tapDisplaySnapshot[note] ?? note
            let isDrum = visualNote < UInt8(KeyboardIconRenderer.firstMidi)
            let midiCh: UInt8 = isDrum ? 9 : 0
            if !isDrum {
                synth.noteOff(note, channel: synthCh)
            }
            midi.noteOff(note, channel: midiCh)
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
