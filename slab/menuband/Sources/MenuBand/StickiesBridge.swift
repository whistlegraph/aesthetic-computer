import AppKit
import ApplicationServices
import Carbon.HIToolbox

/// Bridges macOS Stickies → Menu Band live note playback. A sticky whose
/// body contains an `mb<N>` token becomes an instrument: every character
/// typed after that token plays a note through the same keymap the
/// physical keyboard uses, while the text lands in the sticky unchanged.
///
/// Why Accessibility and not a CGEventTap:
///   • A tap sees every keystroke in every app — far more reach than
///     this feature needs, and it can silently swallow the user's
///     typing if the callback misbehaves.
///   • `.defaultTap` (the only mode that can suppress auto-repeat) is
///     killed by the system whenever the callback overruns its timeout,
///     and it never comes back without an explicit re-enable.
///   • The App Sandbox forbids event taps outright.
///
/// Observing `kAXValueChangedNotification` on the focused sticky sidesteps
/// all three: we learn what was typed by diffing the note's text, and we
/// never stand between the keyboard and Stickies.
///
/// The trade is that AX reports text, not keys. There is no key-up, so a
/// note is attacked and then released on a fixed timer (`noteDuration`)
/// rather than on the user's release. Holding a key auto-repeats into a
/// trill instead of a sustain. Uppercase letters carry the linger
/// envelope — the case survives in the text where a modifier flag would
/// not.
final class StickiesBridge {
    private weak var menuBand: MenuBandController?

    /// Observer on the Stickies process. Recreated if Stickies restarts.
    private var observer: AXObserver?
    /// The app-level element; carries the focus-changed notification.
    private var appElement: AXUIElement?
    /// The text area we're currently watching for value changes. Swapped
    /// whenever the user clicks into a different sticky.
    private var watchedElement: AXUIElement?

    /// Text of `watchedElement` as of the last notification. The diff
    /// against the incoming value is what tells us which character the
    /// user just typed. `nil` means "not primed yet" — the first read
    /// after attaching establishes the baseline and plays nothing.
    private var lastText: String?

    /// Notes currently sounding, keyed by the keyCode we attacked them
    /// with, along with the pending release. A retrigger of the same key
    /// cancels and fires its release early so the synth sees a clean
    /// down/up pair.
    private var pendingReleases: [UInt16: DispatchWorkItem] = [:]

    /// How long a typed note sounds before its synthetic key-up. AX gives
    /// us no release event, so this stands in for one.
    private static let noteDuration: TimeInterval = 0.2

    /// Memoized GM program from the most recent completed `mb<N>` token,
    /// so we only call `setMelodicProgram` on an actual change — it
    /// gracefully morphs held notes, and re-applying it would re-attack.
    private var lastAppliedProgram: UInt8?

    /// Polls for Accessibility trust after we've prompted for it. The
    /// grant lands out-of-band — the user ticks a box in System Settings
    /// while we're already running — and without this the bridge would
    /// stay idle until the next relaunch.
    private var trustTimer: Timer?

    /// `defaults write computer.aestheticcomputer.menuband stickies.debug -bool YES`
    /// to trace what the bridge hears. The bridge is invisible when it
    /// works and was, for a while, invisible when it didn't.
    private static let debugLogging =
        UserDefaults.standard.bool(forKey: "stickies.debug")

    /// Maps a character to the key that types it on the user's current
    /// layout. Built once from the active keyboard layout so a Dvorak or
    /// AZERTY typist gets the note their key would have played. Rebuilt
    /// when the input source changes.
    private var keyCodeForCharacter: [Character: UInt16] = [:]

    /// Whether the bridge is listening. Persisted so the user's choice
    /// survives a relaunch. Defaults to ON — the feature needs no
    /// configuration beyond Accessibility permission.
    var isEnabled: Bool {
        get {
            if UserDefaults.standard.object(forKey: "stickies.bridgeEnabled") == nil {
                return true
            }
            return UserDefaults.standard.bool(forKey: "stickies.bridgeEnabled")
        }
        set {
            UserDefaults.standard.set(newValue, forKey: "stickies.bridgeEnabled")
            if newValue { start() } else { stop() }
        }
    }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
    }

    deinit { stop() }

    // MARK: - Lifecycle

    /// Attach to Stickies if it's running, and keep watching so we attach
    /// when it launches and drop cleanly when it quits.
    ///
    /// Requires Accessibility permission. Note that a locally-built,
    /// ad-hoc-signed bundle loses its grant on every rebuild — TCC pins
    /// the old code requirement, System Settings still shows the toggle
    /// on, and the AX calls just fail. We prompt rather than fail mute.
    func start() {
        guard observer == nil, trustTimer == nil else { return }
        // Prompts on the first call. The user then grants (or doesn't) at
        // their leisure, so poll rather than giving up — a bridge that
        // needs a relaunch to notice its own permission is a bridge that
        // looks broken.
        guard ensureAccessibilityTrust() else {
            NSLog("StickiesBridge: awaiting Accessibility permission")
            trustTimer = Timer.scheduledTimer(withTimeInterval: 1,
                                              repeats: true) { [weak self] timer in
                guard AXIsProcessTrusted() else { return }
                timer.invalidate()
                self?.trustTimer = nil
                self?.beginObserving()
            }
            return
        }
        beginObserving()
    }

    private func beginObserving() {
        rebuildKeyboardLayout()
        NotificationCenter.default.addObserver(
            self, selector: #selector(inputSourceChanged),
            name: NSTextInputContext.keyboardSelectionDidChangeNotification,
            object: nil)
        let ws = NSWorkspace.shared.notificationCenter
        ws.addObserver(self, selector: #selector(appLaunched(_:)),
                       name: NSWorkspace.didLaunchApplicationNotification, object: nil)
        ws.addObserver(self, selector: #selector(appTerminated(_:)),
                       name: NSWorkspace.didTerminateApplicationNotification, object: nil)
        attachToStickies()
        NSLog("StickiesBridge: started (AX observer)")
    }

    func stop() {
        trustTimer?.invalidate()
        trustTimer = nil
        NotificationCenter.default.removeObserver(self)
        NSWorkspace.shared.notificationCenter.removeObserver(self)
        detachFromStickies()
        releaseAllSoundingNotes()
    }

    private func ensureAccessibilityTrust() -> Bool {
        let key = kAXTrustedCheckOptionPrompt.takeUnretainedValue()
        return AXIsProcessTrustedWithOptions([key: true] as CFDictionary)
    }

    @objc private func inputSourceChanged() { rebuildKeyboardLayout() }

    @objc private func appLaunched(_ note: Notification) {
        guard bundleID(from: note) == "com.apple.Stickies" else { return }
        attachToStickies()
    }

    @objc private func appTerminated(_ note: Notification) {
        guard bundleID(from: note) == "com.apple.Stickies" else { return }
        detachFromStickies()
        releaseAllSoundingNotes()
    }

    private func bundleID(from note: Notification) -> String? {
        (note.userInfo?[NSWorkspace.applicationUserInfoKey]
            as? NSRunningApplication)?.bundleIdentifier
    }

    // MARK: - Observer wiring

    private func attachToStickies() {
        guard observer == nil,
              let stickies = NSWorkspace.shared.runningApplications.first(
                  where: { $0.bundleIdentifier == "com.apple.Stickies" })
        else { return }

        var obs: AXObserver?
        let callback: AXObserverCallback = { _, element, notification, refcon in
            guard let refcon = refcon else { return }
            let bridge = Unmanaged<StickiesBridge>.fromOpaque(refcon)
                .takeUnretainedValue()
            switch notification as String {
            case kAXFocusedUIElementChangedNotification:
                bridge.watchFocusedElement()
            case kAXValueChangedNotification:
                bridge.valueChanged(element)
            default:
                break
            }
        }
        guard AXObserverCreate(stickies.processIdentifier, callback, &obs) == .success,
              let obs = obs else {
            NSLog("StickiesBridge: AXObserverCreate failed")
            return
        }
        observer = obs
        let app = AXUIElementCreateApplication(stickies.processIdentifier)
        appElement = app
        let refcon = Unmanaged.passUnretained(self).toOpaque()
        AXObserverAddNotification(obs, app,
                                  kAXFocusedUIElementChangedNotification as CFString,
                                  refcon)
        CFRunLoopAddSource(CFRunLoopGetMain(),
                           AXObserverGetRunLoopSource(obs), .defaultMode)
        watchFocusedElement()
    }

    private func detachFromStickies() {
        if let obs = observer {
            if let app = appElement {
                AXObserverRemoveNotification(
                    obs, app, kAXFocusedUIElementChangedNotification as CFString)
            }
            if let watched = watchedElement {
                AXObserverRemoveNotification(
                    obs, watched, kAXValueChangedNotification as CFString)
            }
            CFRunLoopRemoveSource(CFRunLoopGetMain(),
                                  AXObserverGetRunLoopSource(obs), .defaultMode)
        }
        observer = nil
        appElement = nil
        watchedElement = nil
        lastText = nil
        lastAppliedProgram = nil
    }

    /// Point the value-changed notification at whichever sticky now has
    /// focus. Called on every focus change; also once at attach time.
    private func watchFocusedElement() {
        guard let obs = observer, let app = appElement else { return }
        if let previous = watchedElement {
            AXObserverRemoveNotification(
                obs, previous, kAXValueChangedNotification as CFString)
        }
        watchedElement = nil
        lastText = nil
        // A fresh sticky is a fresh instrument context — forget the last
        // program so the new note's `mbN` re-applies rather than being
        // memoized away.
        lastAppliedProgram = nil

        var focusedRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(
                app, kAXFocusedUIElementAttribute as CFString, &focusedRef) == .success,
              let focused = focusedRef else { return }
        let element = focused as! AXUIElement
        // Only a text element carries a string value; clicking a sticky's
        // title bar focuses something we can't diff.
        guard stringValue(of: element) != nil else { return }
        let refcon = Unmanaged.passUnretained(self).toOpaque()
        guard AXObserverAddNotification(
                obs, element, kAXValueChangedNotification as CFString,
                refcon) == .success else { return }
        watchedElement = element
        lastText = stringValue(of: element)
    }

    // MARK: - The diff

    /// One text change in the focused sticky. Everything the bridge does
    /// hangs off comparing the new body to the old one.
    private func valueChanged(_ element: AXUIElement) {
        guard let newText = stringValue(of: element) else { return }
        let previous = lastText
        lastText = newText
        // Baseline not established yet (we just attached) — nothing to
        // diff against, so nothing to play.
        guard let old = previous else { return }
        // Exactly one character appeared. Deletions, replacements, and
        // pastes all fall out here: a paste would otherwise burst dozens
        // of notes at once, and backspacing should never sound.
        guard newText.count == old.count + 1,
              let (character, insertIndex) = singleInsertion(old: old, new: newText)
        else { return }

        // Arm against the text as it stood BEFORE the character landed,
        // with the cursor where the character was about to go. That's the
        // same vantage the old event-tap had, so `mb6` still types itself
        // into the sticky silently before the notes begin.
        let head = String(old.prefix(insertIndex))
        let context = inlineContext(head: head)
        if Self.debugLogging {
            NSLog("StickiesBridge: typed '\(character)' armed=\(context.armed) program=\(context.program.map(String.init) ?? "-")")
        }
        guard context.armed else { return }

        if let program = context.program, program != lastAppliedProgram {
            lastAppliedProgram = program
            menuBand?.setMelodicProgram(program)
        }
        play(character)
    }

    /// The character added between `old` and `new`, and the offset it was
    /// inserted at. Found by walking in from both ends: whatever the
    /// common prefix and common suffix don't cover is the insertion.
    /// Returns nil when the edit isn't a clean single-character insert.
    private func singleInsertion(old: String, new: String) -> (Character, Int)? {
        let oldChars = Array(old), newChars = Array(new)
        var prefix = 0
        while prefix < oldChars.count, oldChars[prefix] == newChars[prefix] {
            prefix += 1
        }
        var suffix = 0
        while suffix < oldChars.count - prefix,
              oldChars[oldChars.count - 1 - suffix] == newChars[newChars.count - 1 - suffix] {
            suffix += 1
        }
        guard prefix + suffix == oldChars.count else { return nil }
        return (newChars[prefix], prefix)
    }

    // MARK: - Arming

    /// What the text before the cursor says about whether to play, and on
    /// which instrument. Driven by `mb<N>` tokens inline in the body, so
    ///   `mb5 hhg mb6 hhih`
    /// plays the first run on instrument 5 and the second on 6.
    private struct InlineContext {
        /// True when the keystroke should reach the synth: at least one
        /// completed `mbN` token precedes it, and the cursor isn't inside
        /// a token still being typed.
        let armed: Bool
        /// GM program (0-based) from the most recent completed `mbN`.
        let program: UInt8?
    }

    private func inlineContext(head: String) -> InlineContext {
        let lower = head.lowercased()
        // Walk back to the start of the word the cursor sits in.
        var tokenStart = lower.endIndex
        while tokenStart > lower.startIndex {
            let prev = lower.index(before: tokenStart)
            if lower[prev].isWhitespace { break }
            tokenStart = prev
        }
        let currentToken = lower[tokenStart...]
        let completedHead = lower[..<tokenStart]

        // Is the cursor inside a token that is still becoming `mbN`? The
        // user passes through `m` → `mb` → `mb1` → `mb10` on the way, and
        // none of those keystrokes should sound. (Digits especially: they
        // toggle MIDI mode in Menu Band's keymap.)
        let inConfig: Bool
        if currentToken.hasPrefix("mb") {
            inConfig = currentToken.dropFirst(2).allSatisfy { $0.isNumber }
        } else {
            inConfig = currentToken == "m"
        }

        // The last completed `mbN` before the cursor wins.
        var program: UInt8?
        var sawAnyMB = false
        for token in completedHead.split(whereSeparator: { $0.isWhitespace }) {
            if let parsed = Self.parseMBToken(token) {
                program = parsed
                sawAnyMB = true
            }
        }
        return InlineContext(armed: sawAnyMB && !inConfig, program: program)
    }

    /// `mb1`…`mb128` → 0-based GM program. Anything else (`mbox`, `mb0`,
    /// `mb999`) is just a word.
    private static func parseMBToken(_ token: Substring) -> UInt8? {
        guard token.hasPrefix("mb") else { return nil }
        let rest = token.dropFirst(2)
        guard !rest.isEmpty, rest.allSatisfy({ $0.isNumber }),
              let n = Int(rest), n >= 1, n <= 128 else { return nil }
        return UInt8(n - 1)
    }

    // MARK: - Sounding a character

    /// Attack the note this character maps to, and schedule its release.
    /// Uppercase engages linger (the bell-ring envelope) via caps-lock —
    /// caps keeps each key's natural pan, where shift would demand a
    /// left/right side we can't recover from text.
    private func play(_ character: Character) {
        let lowered = Character(character.lowercased())
        guard let keyCode = keyCodeForCharacter[lowered],
              let menuBand = menuBand else {
            if Self.debugLogging {
                NSLog("StickiesBridge: '\(character)' has no key on this layout")
            }
            return
        }
        if Self.debugLogging {
            NSLog("StickiesBridge: play '\(character)' → keyCode \(keyCode)")
        }
        let flags: NSEvent.ModifierFlags = character.isUppercase ? [.capsLock] : []

        // Retrigger of a still-sounding key: release it first so the synth
        // sees down/up/down rather than two stacked attacks.
        if let pending = pendingReleases.removeValue(forKey: keyCode) {
            pending.cancel()
            _ = menuBand.handleLocalKey(keyCode: keyCode, isDown: false,
                                        isRepeat: false, flags: [])
        }
        _ = menuBand.handleLocalKey(keyCode: keyCode, isDown: true,
                                    isRepeat: false, flags: flags)

        let release = DispatchWorkItem { [weak self] in
            guard let self = self else { return }
            self.pendingReleases.removeValue(forKey: keyCode)
            _ = self.menuBand?.handleLocalKey(keyCode: keyCode, isDown: false,
                                              isRepeat: false, flags: [])
        }
        pendingReleases[keyCode] = release
        DispatchQueue.main.asyncAfter(deadline: .now() + Self.noteDuration,
                                      execute: release)
    }

    /// Fire every pending release now. Used when the bridge stops or
    /// Stickies quits — otherwise a note attacked microseconds earlier
    /// would ring forever with no key-up coming.
    private func releaseAllSoundingNotes() {
        let pending = pendingReleases
        pendingReleases.removeAll()
        for (keyCode, work) in pending {
            work.cancel()
            _ = menuBand?.handleLocalKey(keyCode: keyCode, isDown: false,
                                         isRepeat: false, flags: [])
        }
    }

    // MARK: - Layout

    /// Invert the active keyboard layout into character → keyCode. Menu
    /// Band's keymap is indexed by hardware keyCode, but AX hands us the
    /// character that key produced, so we have to go back the other way.
    private func rebuildKeyboardLayout() {
        keyCodeForCharacter.removeAll()
        guard let source = TISCopyCurrentASCIICapableKeyboardLayoutInputSource()?
                .takeRetainedValue(),
              let raw = TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData)
        else { return }
        let data = Unmanaged<CFData>.fromOpaque(raw).takeUnretainedValue() as Data
        let kbdType = UInt32(LMGetKbdType())

        data.withUnsafeBytes { buffer in
            guard let layout = buffer.baseAddress?
                    .assumingMemoryBound(to: UCKeyboardLayout.self) else { return }
            // 0…127 covers every hardware key the keymap can name.
            for keyCode in UInt16(0)..<128 {
                var deadKeyState: UInt32 = 0
                var length = 0
                var chars = [UniChar](repeating: 0, count: 4)
                let status = UCKeyTranslate(
                    layout, keyCode, UInt16(kUCKeyActionDown), 0, kbdType,
                    OptionBits(kUCKeyTranslateNoDeadKeysBit),
                    &deadKeyState, chars.count, &length, &chars)
                guard status == noErr, length == 1,
                      let scalar = Unicode.Scalar(chars[0]) else { continue }
                let character = Character(scalar)
                // First key wins: the number row should claim a digit
                // before the numpad does.
                if keyCodeForCharacter[character] == nil {
                    keyCodeForCharacter[character] = keyCode
                }
            }
        }
    }

    // MARK: - AX helpers

    private func stringValue(of element: AXUIElement) -> String? {
        var value: CFTypeRef?
        guard AXUIElementCopyAttributeValue(
                element, kAXValueAttribute as CFString, &value) == .success
        else { return nil }
        return value as? String
    }
}
