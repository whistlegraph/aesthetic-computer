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

    var keymap: Keymap {
        get {
            let raw = UserDefaults.standard.string(forKey: keymapKey) ?? ""
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
        onChange?()
    }


    func bootstrap() {
        // Built-in synth is always live. TYPE mode and MIDI mode are now
        // independent toggles: TYPE controls global keyboard capture + key
        // letter overlays; MIDI controls the virtual MIDI port output to
        // external DAWs.
        synth.start()
        synth.setMelodicProgram(melodicProgram)
        if typeMode { enableTypeMode(promptForPermission: false) }
        if midiMode { enableMIDIMode() }
    }

    func toggleMIDIMode() {
        if midiMode { disableMIDIMode() } else { enableMIDIMode() }
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
    }

    private func disableMIDIMode() {
        midi.stop()
        UserDefaults.standard.set(false, forKey: midiModeKey)
        onChange?()
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
        alert.messageText = "MenuBand needs Accessibility access"
        alert.informativeText = """
            To capture keystrokes globally, MenuBand must be enabled in \
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
