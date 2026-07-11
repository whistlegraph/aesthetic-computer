import AVFoundation
import AppKit
import Speech

extension Notification.Name {
    /// Posted by the About-window checkbox when voice squawk is switched
    /// on or off; AppDelegate registers/unregisters the ⌘⌃⌥` hotkey and the
    /// popover shows/hides its MIC cell.
    static let menuBandSquawkEnabledChanged =
        Notification.Name("MenuBandSquawkEnabledChanged")
    /// Posted by any UI affordance (e.g. the popover MIC cell) that wants to
    /// start/stop squawk without owning the engine. AppDelegate toggles.
    static let menuBandSquawkToggleRequested =
        Notification.Name("MenuBandSquawkToggleRequested")
    /// Posted by AppDelegate when listening starts (`object: true`) or stops
    /// (`object: false`) so UI can reflect the live state.
    static let menuBandSquawkStateChanged =
        Notification.Name("MenuBandSquawkStateChanged")
}

/// Squawk — on-device voice dictation for Menu Band.
///
/// Press the squawk hotkey (⌘⌃⌥`) → this starts listening, transcribes
/// your speech locally with Apple's `Speech` framework, and — when you
/// press again or pause — types the finished text into whatever app is
/// frontmost. Built so you can talk into a terminal / Claude Code without
/// lifting your hands to the trackpad, but it types into any focused text
/// field the same way.
///
/// It is deliberately self-contained: the recognizer, its own audio
/// engine, and the keystroke injector all live here so nothing else in the
/// app has to know how squawk works. The rest of Menu Band only calls
/// `toggle()` and reads `onStateChange` for a status cue.
///
/// ```
/// mic ──► AVAudioEngine (own instance) ──tap──► SFSpeechRecognitionRequest
///                                                      │
///                                        partial + final transcriptions
///                                                      │
///                                            onFinalText (default: type
///                                            into the focused app)
/// ```
///
/// **Why a separate `AVAudioEngine`.** `MenuBandSampleVoice` already taps
/// the mic `inputNode` on bus 0 for the sampler; running squawk on its
/// own engine keeps the two from fighting over the same tap.
///
/// **Sandbox note.** Keystroke injection (`CGEvent`) is forbidden in the
/// App Store sandbox, exactly like the Notepat/Ableton typing modes, so the
/// type-into-focused-app path is gated `#if !MAC_APP_STORE`. The MAS build
/// still transcribes; it just can't type into other apps.
final class MenuBandSquawk {

    /// Off by default. The About-window "Advanced" checkbox flips this and
    /// the hotkey only registers while it's true, so the mic stays dormant
    /// for anyone who doesn't want it.
    static let enabledDefaultsKey = "MenuBandVoiceSquawkEnabled"

    static var isEnabled: Bool {
        // Squawk needs keystroke injection to be useful (it types the
        // transcription into the frontmost app), which the App Store sandbox
        // forbids. Rather than ship a Speech-Recognition capability that can
        // transcribe but do nothing, Squawk is direct-download only: force it
        // off in the MAS build so the hotkey never registers, the 🦜 cell
        // never shows, and the recognizer is never invoked.
        #if MAC_APP_STORE
        return false
        #else
        return UserDefaults.standard.bool(forKey: enabledDefaultsKey)
        #endif
    }

    /// Fired on the main thread whenever listening starts (`true`) or stops
    /// (`false`). Drives any status cue (menubar glyph, beep, mic-cell fill).
    var onStateChange: ((Bool) -> Void)?

    /// Fired on the main thread with the finished transcript. Defaults to
    /// typing it into the focused app; swap it to route the text somewhere
    /// else — e.g. queue it as a prompt for the LLM window. Empty
    /// transcripts are never delivered.
    var onFinalText: ((String) -> Void)?

    private(set) var isListening = false

    // MARK: - Recognition state (recreated per session)

    private let recognizer: SFSpeechRecognizer?
    private var request: SFSpeechAudioBufferRecognitionRequest?
    private var task: SFSpeechRecognitionTask?
    private let engine = AVAudioEngine()

    /// The most recent transcription seen this session. On stop we inject
    /// this immediately rather than waiting for the (sometimes slow) final
    /// callback, so the text lands the instant you release.
    private var latestTranscript = ""
    private var didDeliver = false
    /// Desired state, set the instant `start()`/`stop()` are called. Push-to-
    /// talk releases can arrive before the async auth callback opens the
    /// session; this lets a release that lands mid-authorization cancel the
    /// pending start instead of stranding a live mic.
    private var wantsListening = false

    init(localeIdentifier: String = "en-US") {
        recognizer = SFSpeechRecognizer(locale: Locale(identifier: localeIdentifier))
        onFinalText = { text in MenuBandSquawk.typeIntoFocusedApp(text) }
    }

    // MARK: - Public control

    /// Start if idle, finalize + inject if already listening. This is what
    /// the global hotkey calls.
    func toggle() {
        if isListening { stop() } else { start() }
    }

    /// Ask for the two permissions squawk needs (speech + mic), then
    /// begin a fresh recognition session. Safe to call when already
    /// listening (no-op).
    func start() {
        guard !isListening else { return }
        guard recognizer != nil else {
            NSLog("MenuBand Squawk: no recognizer for locale — is the language pack installed?")
            return
        }
        wantsListening = true
        authorize { [weak self] granted in
            guard let self, granted else {
                NSLog("MenuBand Squawk: permission denied (speech or mic)")
                return
            }
            // The user may have already released (push-to-talk) while the
            // permission round-trip was in flight — honor that.
            guard self.wantsListening else { return }
            self.beginSession()
        }
    }

    /// Stop listening, deliver whatever we have, and tear the session down.
    func stop() {
        wantsListening = false
        guard isListening else { return }
        isListening = false

        // Detach the mic first so no more audio arrives, then close the
        // request so the recognizer flushes its final result.
        engine.inputNode.removeTap(onBus: 0)
        engine.stop()
        request?.endAudio()

        deliverIfNeeded()

        task?.finish()
        task = nil
        request = nil

        DispatchQueue.main.async { self.onStateChange?(false) }
    }

    // MARK: - Session

    private func beginSession() {
        guard let recognizer, recognizer.isAvailable else {
            NSLog("MenuBand Squawk: recognizer unavailable")
            return
        }

        latestTranscript = ""
        didDeliver = false

        let request = SFSpeechAudioBufferRecognitionRequest()
        request.shouldReportPartialResults = true
        // Prefer offline transcription when the on-device model is present —
        // no network, no account, nothing leaves the machine. Falls back to
        // Apple's servers only if the local model isn't installed.
        request.requiresOnDeviceRecognition = recognizer.supportsOnDeviceRecognition
        self.request = request

        let input = engine.inputNode
        let format = input.outputFormat(forBus: 0)
        input.installTap(onBus: 0, bufferSize: 1024, format: format) { [weak self] buffer, _ in
            self?.request?.append(buffer)
        }

        engine.prepare()
        do {
            try engine.start()
        } catch {
            NSLog("MenuBand Squawk: engine start failed: \(error)")
            input.removeTap(onBus: 0)
            self.request = nil
            return
        }

        task = recognizer.recognitionTask(with: request) { [weak self] result, error in
            guard let self else { return }
            if let result {
                self.latestTranscript = result.bestTranscription.formattedString
                if result.isFinal { self.deliverIfNeeded() }
            }
            if error != nil {
                // The recognizer errors out normally on endAudio; deliver
                // whatever we captured rather than dropping it.
                self.deliverIfNeeded()
            }
        }

        isListening = true
        DispatchQueue.main.async { self.onStateChange?(true) }
        NSLog("MenuBand Squawk: listening (onDevice=\(request.requiresOnDeviceRecognition))")
    }

    /// Hand the transcript to `onFinalText` exactly once per session.
    private func deliverIfNeeded() {
        guard !didDeliver else { return }
        let text = latestTranscript.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        didDeliver = true
        NSLog("MenuBand Squawk: delivering \"\(text)\"")
        DispatchQueue.main.async { self.onFinalText?(text) }
    }

    // MARK: - Authorization

    /// Speech recognition and microphone are separate grants; we need both.
    private func authorize(_ completion: @escaping (Bool) -> Void) {
        SFSpeechRecognizer.requestAuthorization { speechStatus in
            guard speechStatus == .authorized else {
                DispatchQueue.main.async { completion(false) }
                return
            }
            switch AVCaptureDevice.authorizationStatus(for: .audio) {
            case .authorized:
                DispatchQueue.main.async { completion(true) }
            case .notDetermined:
                AVCaptureDevice.requestAccess(for: .audio) { micOK in
                    DispatchQueue.main.async { completion(micOK) }
                }
            default:
                DispatchQueue.main.async { completion(false) }
            }
        }
    }

    // MARK: - Text injection

    /// Type `text` into the frontmost app as synthesized Unicode key events —
    /// the same mechanism the Notepat/Ableton typing modes use, so it lands
    /// in any focused text field (terminal included) without touching the
    /// pasteboard.
    ///
    /// No-op in the App Store sandbox, which forbids posting events to other
    /// apps. There the transcript can still be routed in-app (e.g. to the
    /// LLM window) via a custom `onFinalText`.
    static func typeIntoFocusedApp(_ text: String) {
        #if MAC_APP_STORE
        NSLog("MenuBand Squawk: keystroke injection unavailable in sandbox")
        #else
        let source = CGEventSource(stateID: .combinedSessionState)
        // Post in small UTF-16 chunks — long single-event strings are
        // dropped by some apps.
        let units = Array(text.utf16)
        let chunkSize = 16
        var i = 0
        while i < units.count {
            let slice = Array(units[i..<min(i + chunkSize, units.count)])
            for keyDown in [true, false] {
                guard let event = CGEvent(keyboardEventSource: source,
                                          virtualKey: 0, keyDown: keyDown) else { continue }
                event.keyboardSetUnicodeString(stringLength: slice.count, unicodeString: slice)
                event.post(tap: .cghidEventTap)
            }
            i += chunkSize
        }
        #endif
    }
}
