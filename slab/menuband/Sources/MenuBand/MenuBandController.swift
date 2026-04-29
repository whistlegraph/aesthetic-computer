import AppKit
import ApplicationServices
import CoreGraphics

final class MenuBandController {
    private let midi = MenuBandMIDI()
    private let synth = MenuBandSynth()
    private var keyTap: KeyEventTap?
    private var heldNotes: [UInt16: UInt8] = [:]
    private let heldLock = NSLock()

    private let midiModeKey = "notepat.midiMode"
    private let typeModeKey = "notepat.typeMode"
    private let octaveShiftKey = "notepat.octaveShift"
    private let melodicProgramKey = "notepat.melodicProgram"
    private let keymapKey = "notepat.keymap"

    // Visual state — accessed only on the main thread.
    private(set) var litNotes: Set<UInt8> = []
    private var litDownAt: [UInt8: CFTimeInterval] = [:]
    private let minVisibleSeconds: CFTimeInterval = 0.08

    var onChange: (() -> Void)?
    var onLitChanged: (() -> Void)?

    var midiMode: Bool {
        UserDefaults.standard.bool(forKey: midiModeKey)
    }

    var typeMode: Bool {
        UserDefaults.standard.bool(forKey: typeModeKey)
    }

    // Hover-preview overlay. The popover's input segmented control sets these
    // when the user hovers a segment so the menubar piano can render that
    // mode's range/labels and the local key monitor can play notes through
    // its keymap — all without committing to UserDefaults. Cleared on hover
    // exit. Effective getters fall through to the real values when nil.
    private var previewTypeMode: Bool?
    private var previewKeymap: Keymap?

    var effectiveTypeMode: Bool { previewTypeMode ?? typeMode }
    var effectiveKeymap: Keymap { previewKeymap ?? keymap }

    /// True only while the popover is hovering Notepat or Ableton — i.e. a
    /// preview is active AND it's a typing mode (not Pointer). Used to gate
    /// the popover's local key monitor so demo keystrokes are consumed
    /// only when actually previewing.
    var isHoveringTypingMode: Bool { previewTypeMode == true }

    func setHoverPreview(typeMode tm: Bool, keymap km: Keymap) {
        previewTypeMode = tm
        previewKeymap = km
        onChange?()
    }

    func clearHoverPreview() {
        guard previewTypeMode != nil || previewKeymap != nil else { return }
        previewTypeMode = nil
        previewKeymap = nil
        onChange?()
    }

    /// Local-key demo while the popover is hovering an input mode. Maps the
    /// keystroke through the preview (or real) keymap and feeds it through
    /// `startTapNote`/`stopTapNote` so it sounds + lights up + sends MIDI
    /// just like a real tap.
    func previewPlayKey(keyCode: UInt16, isDown: Bool) {
        let km = previewKeymap ?? keymap
        guard let note = MenuBandLayout.midiNote(forKeyCode: keyCode,
                                                 octaveShift: octaveShift,
                                                 keymap: km) else { return }
        if isDown {
            startTapNote(note)
        } else {
            stopTapNote(note)
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
        synth.setMelodicProgram(program)
        // Any in-flight preview is no longer relevant — drop its held note +
        // saved-program so the next hover restarts cleanly.
        if let prev = previewNote {
            synth.noteOff(prev)
            previewNote = nil
        }
        previewSavedProgram = nil
        onChange?()
    }

    // MARK: - Instrument hover preview (popover flat-map)

    private var previewSavedProgram: UInt8?
    private var previewNote: UInt8?
    private let previewVelocity: UInt8 = 80

    /// Hover-preview a program in the instrument flat-map. Pass nil to stop
    /// preview and restore the user's committed program. Plays a held
    /// middle-C note in the previewed program through the local synth so
    /// the user can audition without committing. Silent when MIDI mode is
    /// on (DAW is in charge of audio).
    func setInstrumentPreview(_ program: UInt8?) {
        // Stop any previously playing preview note immediately.
        if let prev = previewNote {
            synth.noteOff(prev)
            previewNote = nil
        }

        guard let prog = program else {
            // Hover ended — restore the user's committed program.
            if let saved = previewSavedProgram {
                synth.setMelodicProgram(saved)
                previewSavedProgram = nil
            }
            return
        }

        // Save the user's committed program once so we can restore on exit.
        if previewSavedProgram == nil {
            previewSavedProgram = melodicProgram
        }

        synth.setMelodicProgram(prog)
        // Don't add audio in MIDI mode — DAW is hearing user input.
        guard !midiMode else { return }
        let note: UInt8 = 60
        synth.noteOn(note, velocity: previewVelocity, channel: 0)
        previewNote = note
    }


    func bootstrap() {
        // Built-in synth is always live. TYPE mode and MIDI mode are now
        // independent toggles: TYPE controls global keyboard capture + key
        // letter overlays; MIDI controls the virtual MIDI port output to
        // external DAWs. MIDI mode defaults to *off* for fresh installs —
        // most users hear the synth first; DAW routing is the opt-in path
        // they pick after they know they want it. Existing users' choice
        // is preserved (only the default for first-launch changes).
        synth.start()
        synth.setMelodicProgram(melodicProgram)
        if UserDefaults.standard.object(forKey: midiModeKey) == nil {
            UserDefaults.standard.set(false, forKey: midiModeKey)
        }
        if typeMode { enableTypeMode(promptForPermission: false) }
        if midiMode { enableMIDIMode() }
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
        runMIDILoopbackTest()
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
        if !midiMode { synth.noteOn(midiNote, velocity: velocity, channel: synthCh) }
        midi.noteOn(midiNote, velocity: velocity, channel: midiCh)
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.litDownAt[midiNote] = CACurrentMediaTime()
            if self.litNotes.insert(midiNote).inserted {
                self.onLitChanged?()
            }
        }
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
        // Visual cleanup deferred so we don't pay image-render cost in the
        // mouse-up handler.
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            let downAt = self.litDownAt.removeValue(forKey: midiNote) ?? 0
            let held = CACurrentMediaTime() - downAt
            let extinguish = { [weak self] in
                guard let self = self else { return }
                if self.litNotes.remove(midiNote) != nil {
                    self.onLitChanged?()
                }
            }
            if held < self.minVisibleSeconds {
                DispatchQueue.main.asyncAfter(deadline: .now() + (self.minVisibleSeconds - held)) {
                    extinguish()
                }
            } else {
                extinguish()
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
        // Modifier combos pass through so cmd-c, cmd-tab etc. work as usual.
        if flags.contains(.maskCommand) || flags.contains(.maskControl) || flags.contains(.maskAlternate) { return false }

        let shift = octaveShift // a single UserDefaults read; cheap

        if isDown {
            if isRepeat { return true }  // consume repeats but don't retrigger
            guard let note = MenuBandLayout.midiNote(forKeyCode: keyCode, octaveShift: shift, keymap: keymap) else {
                // Unmapped key: still consume in TYPE mode so it doesn't leak
                // through to the focused app.
                return true
            }
            heldLock.lock()
            let prev = heldNotes[keyCode]
            heldNotes[keyCode] = note
            heldLock.unlock()
            if let prev = prev {
                if !midiMode { synth.noteOff(prev) }
                midi.noteOff(prev)
            }
            if !midiMode { synth.noteOn(note) }
            midi.noteOn(note)
            DispatchQueue.main.async { [weak self] in
                guard let self = self else { return }
                self.litDownAt[note] = CACurrentMediaTime()
                if self.litNotes.insert(note).inserted {
                    self.onLitChanged?()
                }
            }
            return true
        } else {
            heldLock.lock()
            let note = heldNotes.removeValue(forKey: keyCode)
            heldLock.unlock()
            guard let releasedNote = note else { return true }  // consume the up too
            if !midiMode { synth.noteOff(releasedNote) }
            midi.noteOff(releasedNote)
            DispatchQueue.main.async { [weak self] in
                guard let self = self else { return }
                let downAt = self.litDownAt.removeValue(forKey: releasedNote) ?? 0
                let held = CACurrentMediaTime() - downAt
                let extinguish = { [weak self] in
                    guard let self = self else { return }
                    if self.litNotes.remove(releasedNote) != nil {
                        self.onLitChanged?()
                    }
                }
                if held < self.minVisibleSeconds {
                    DispatchQueue.main.asyncAfter(deadline: .now() + (self.minVisibleSeconds - held)) {
                        extinguish()
                    }
                } else {
                    extinguish()
                }
            }
            return true
        }
    }

    private func releaseAllHeldNotes() {
        heldLock.lock()
        let snapshot = heldNotes
        heldNotes.removeAll()
        heldLock.unlock()
        for (_, note) in snapshot {
            synth.noteOff(note)
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
