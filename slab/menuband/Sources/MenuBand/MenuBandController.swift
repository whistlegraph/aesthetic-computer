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
    private let heldLock = NSLock()

    private let midiModeKey = "notepat.midiMode"
    private let typeModeKey = "notepat.typeMode"
    private let octaveShiftKey = "notepat.octaveShift"
    private let melodicProgramKey = "notepat.melodicProgram"
    private let keymapKey = "notepat.keymap"
    private let mutedKey = "notepat.muted"
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

    var midiMode: Bool {
        UserDefaults.standard.bool(forKey: midiModeKey)
    }

    var typeMode: Bool {
        UserDefaults.standard.bool(forKey: typeModeKey)
    }

    /// When on, the local synth is silent — note triggers still update lit
    /// state and (in MIDI mode) still send out the virtual port, but the
    /// built-in sampler/MIDISynth doesn't sound. Independent of MIDI mode
    /// so a user can keep MIDI off and still mute the local synth.
    var muted: Bool {
        UserDefaults.standard.bool(forKey: mutedKey)
    }

    func toggleMuted() {
        let now = !muted
        UserDefaults.standard.set(now, forKey: mutedKey)
        if now {
            // Cut anything currently sounding so a long-tail note doesn't
            // hang past the moment the user hit mute.
            synth.panic()
        }
        onChange?()
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

    // Held preview note for sonic-browse hover over the instrument map.
    // Continuous tone — switching cells stops the old note + starts a new
    // one in the new program. Hover-out releases. Silent in MIDI mode
    // (DAW is the audio path then; we still apply the program change so
    // it's correct when the user toggles MIDI off).
    private var previewNote: UInt8?
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
            synth.setMelodicProgram(melodicProgram)
            return
        }
        synth.setMelodicProgram(prog)
        guard !midiMode, !muted else { return }
        let note: UInt8 = 60
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

    func auditionCurrentProgram() {
        guard !muted else { return }
        let note: UInt8 = 60
        debugLog("audition: synth.noteOn 60 (program \(melodicProgram))")
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

    func setMelodicProgram(_ program: UInt8) {
        UserDefaults.standard.set(Int(program), forKey: melodicProgramKey)
        // Picking a GM program implicitly switches us back to the GM
        // backend — the user's "Instrument" pick lives on the GM grid,
        // so committing one means GM is now the active source.
        UserDefaults.standard.set("gm", forKey: instrumentBackendKey)
        synth.setMelodicProgram(program)
        onChange?()
    }

    // MARK: - Instrument backend (GM vs GarageBand)

    enum InstrumentBackend: String { case gm, garageBand = "gb" }

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
    private var melodicVoiceCursor: UInt8 = 0         // round-robin across 8 melodic channels

    @inline(__always)
    private func nextMelodicChannel() -> UInt8 {
        let c = melodicVoiceCursor
        melodicVoiceCursor = (melodicVoiceCursor &+ 1) & 0x07
        return c
    }

    /// Begin holding a note from a menubar mouse interaction. Velocity (0–127)
    /// and pan (0–127, 64=center) are passed in by the caller — the drag
    /// handler computes them from the cursor's relative position inside the
    /// hovered key, giving expressive control: y closer to vertical center =
    /// louder, x within key = stereo pan.
    func startTapNote(_ midiNote: UInt8, velocity: UInt8 = 100, pan: UInt8 = 64) {
        debugLog("startTapNote midi=\(midiNote) midiMode=\(midiMode)")
        if tapHeld.contains(midiNote) { return }
        tapHeld.insert(midiNote)
        let isDrum = midiNote < UInt8(KeyboardIconRenderer.firstMidi)
        // Synth: rotate across 8 channels so rapid same-note taps overlap
        // (different channels = different voices, no stealing). MIDI: always
        // land on channel 1 (drums on 10) so an Ableton track listening on
        // a single channel actually receives every note.
        let synthCh: UInt8 = isDrum ? 9 : nextMelodicChannel()
        let midiCh: UInt8 = isDrum ? 9 : 0
        tapNoteChannel[midiNote] = synthCh
        midi.sendCC(10, value: pan, channel: midiCh)
        if !midiMode && !muted { synth.noteOn(midiNote, velocity: velocity, channel: synthCh) }
        midi.noteOn(midiNote, velocity: velocity, channel: midiCh)
        // Lit state is main-thread-only; update synchronously so the menubar
        // redraws within the same runloop pass as the click. Dispatching async
        // pushed the redraw past the event-tracking loop's next spin and the
        // blink wasn't visible.
        let setLit = { [weak self] in
            guard let self = self else { return }
            self.litDownAt[midiNote] = CACurrentMediaTime()
            if self.litNotes.insert(midiNote).inserted {
                self.onLitChanged?()
            }
        }
        if Thread.isMainThread { setLit() } else { DispatchQueue.main.async(execute: setLit) }
    }

    /// While dragging, update pan in real time as the cursor slides within
    /// the held note. Doesn't retrigger the note.
    func updateTapPan(_ midiNote: UInt8, pan: UInt8) {
        guard tapHeld.contains(midiNote) else { return }
        let midiCh: UInt8 = midiNote < UInt8(KeyboardIconRenderer.firstMidi) ? 9 : 0
        midi.sendCC(10, value: pan, channel: midiCh)
    }

    /// Release a note previously started with `startTapNote`.
    func stopTapNote(_ midiNote: UInt8) {
        guard tapHeld.contains(midiNote) else { return }
        tapHeld.remove(midiNote)
        let synthCh = tapNoteChannel.removeValue(forKey: midiNote) ?? channel(for: midiNote)
        let isDrum = midiNote < UInt8(KeyboardIconRenderer.firstMidi)
        let midiCh: UInt8 = isDrum ? 9 : 0
        // Drums are one-shot percussion: do NOT send synth.noteOff. Letting
        // the sample play through is what makes rapid taps overlap correctly
        // instead of cutting each other off. The MIDI port still sends noteOff
        // so external sequencers (Ableton drum racks) get a clean event pair.
        // Internal synth is silent in MIDI mode anyway.
        if !isDrum && !midiMode {
            synth.noteOff(midiNote, channel: synthCh)
        }
        midi.noteOff(midiNote, channel: midiCh)
        // Release the visual immediately on mouse-up. The earlier
        // minVisibleSeconds floor read as visual lag — the user
        // perceives it as the key sticking down past the click. Snap-up
        // matches the keyboard path now.
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.litDownAt.removeValue(forKey: midiNote)
            if self.litNotes.remove(midiNote) != nil {
                self.onLitChanged?()
            }
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
        return playKeyEvent(keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, hasModifier: hasMod)
    }

    /// Sandbox-friendly key path: same note logic as the global tap, but
    /// driven by a local NSEvent monitor on the AppDelegate's invisible
    /// capture panel. No TYPE-mode escape semantics — this path activates
    /// and deactivates with the panel's key-window state, so an "exit"
    /// keystroke isn't needed.
    @discardableResult
    func handleLocalKey(keyCode: UInt16, isDown: Bool, isRepeat: Bool, flags: NSEvent.ModifierFlags) -> Bool {
        let hasMod = flags.contains(.command) || flags.contains(.control) || flags.contains(.option)
        return playKeyEvent(keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, hasModifier: hasMod)
    }

    /// Shared note logic for both the global CGEventTap path and the
    /// local NSEvent panel path. Returns true if the keystroke was
    /// consumed (mapped to a note); false if it should pass through.
    @discardableResult
    private func playKeyEvent(keyCode: UInt16, isDown: Bool, isRepeat: Bool, hasModifier: Bool) -> Bool {
        // Modifier combos pass through so cmd-c, cmd-tab etc. work as usual.
        if hasModifier { return false }

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
            heldLock.unlock()
            if let prevNote = prevNote {
                if !midiMode { synth.noteOff(prevNote, channel: prevCh ?? 0) }
                midi.noteOff(prevNote)
            }
            if !midiMode && !muted { synth.noteOn(note, velocity: 100, channel: synthCh) }
            midi.noteOn(note)
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
            heldLock.unlock()
            guard let releasedNote = note else { return true }  // consume the up too
            if !midiMode { synth.noteOff(releasedNote, channel: synthCh) }
            midi.noteOff(releasedNote)
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

    private func releaseAllHeldNotes() {
        heldLock.lock()
        let noteSnapshot = heldNotes
        let chanSnapshot = heldKeyChannel
        heldNotes.removeAll()
        heldKeyChannel.removeAll()
        heldKeyDisplayNote.removeAll()
        heldLock.unlock()
        for (keyCode, note) in noteSnapshot {
            let ch = chanSnapshot[keyCode] ?? 0
            synth.noteOff(note, channel: ch)
            midi.noteOff(note)
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
