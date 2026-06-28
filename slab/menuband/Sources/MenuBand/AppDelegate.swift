import AppKit
import AVFoundation
import Carbon

extension Notification.Name {
    /// Posted whenever the set of currently-sounding notes changes, so the
    /// About-window icon's live key glow can refresh.
    static let menuBandLitNotesChanged = Notification.Name("menuBandLitNotesChanged")
}

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem!
    private let menuBand = MenuBandController()
    /// Live conductible drone/arp/drum loop (see MenuBandEngine + the
    /// `engine.*` distributed-notification handlers).
    private lazy var engine = MenuBandEngine(menuBand: menuBand)
    /// Mic tempo follower — steers the engine's BPM to the room (engine.listen).
    private var micTempo: MenuBandMicTempo?
    /// Text-to-speech voice for the `say` hook (machines talking to each other).
    private let speechSynth = AVSpeechSynthesizer()
    /// Peer-to-peer link to other Menu Bands (Bluetooth + peer Wi-Fi, no LAN).
    private let fleet = MenuBandFleet()
    private let hoverResponder = HoverResponder()
    /// Bridges typing in the macOS Stickies app to Menu Band note
    /// playback when the focused sticky matches the trigger color.
    /// Initialized lazily so it can capture `menuBand` after that
    /// stored property is fully set.
    private lazy var stickiesBridge = StickiesBridge(menuBand: menuBand)
    private var hoveredElement: KeyboardIconRenderer.HitResult? = nil
    /// ~10 Hz ticker that drives the REC dot's blink + elapsed timer while a
    /// tape is recording, and tears down to the idle dot when it stops.
    private var recTimer: Timer?
    /// Wall-clock moment recording actually began (0 = not yet). The REC
    /// timer counts from here rather than the tape's buffer duration, which
    /// can read stale on the UI tick.
    private var recStartTime: CFTimeInterval = 0
    /// Cached MP3 export, keyed by the source WAV's path. Every eject path
    /// (REC-dot stop-and-drop, popover EJECT button, cassette drag-out)
    /// funnels through `exportTapeMP3()` so they all yield the same
    /// shareable MP3 with cover art; the transcode runs once per take and
    /// is reused. A fresh take always produces a new (collision-suffixed)
    /// WAV name, so a stale entry simply never matches — no explicit
    /// invalidation needed. Access is serialized by `tapeExportLock`.
    private var cachedTapeMP3: (wavPath: String, mp3: URL)?
    /// Serializes the ffmpeg encode + cache read/write so an in-flight
    /// pre-warm and a follow-up drag/eject can't launch two encoders
    /// writing the same temp MP3. Held only around CPU/cache work — never
    /// across a hop to the main thread — so it can't deadlock.
    private let tapeExportLock = NSLock()
    /// Tracks the tape's recording state across `onChange` ticks so we can
    /// fire a single MP3 pre-warm on the recording→idle edge — covering
    /// every stop path (icon REC dot, popover transport, keyboard, 90 s
    /// auto-stop) from one place.
    private var tapeWasRecording = false
    private var trackingArea: NSTrackingArea?
    private var typeModeHotkey: GlobalHotkey?
    private var focusCaptureHotkey: GlobalHotkey?
    private var pianoWaveformHotkey: GlobalHotkey?
    private var exitFocusHotkey: GlobalHotkey?
    private var layoutToggleHotkey: GlobalHotkey?
    private var percussionToggleHotkey: GlobalHotkey?
    private var popoverPanel: MenuBandPopoverPanel?
    private var popoverVC: MenuBandPopoverViewController?
    /// Indirect-touch sensor embedded in the popover so trackpad
    /// pitch-bend still engages while the popover is the key window
    /// (the off-screen capture sensor only sees fingers when IT is key,
    /// which it isn't once the popover takes focus). First-responder of
    /// the popover panel; mouse passes straight through (hitTest → nil).
    private var popoverTouchSensor: TouchSensorView?
    // [v1 cutoff] KidLisp TV panel + its `$` piece chooser cache + amp
    // provider state removed. The shared KidLispState + the About-opened
    // aesthetic.computer web window survive on their own.

    private var isPopoverPanelShown: Bool { popoverPanel?.isVisible == true }
    private lazy var pianoWaveformWindowDelegate = PianoWaveformWindowDelegate(menuBand: menuBand)
    private var appBeforePopover: NSRunningApplication?
    private var appBeforeFocusCapture: NSRunningApplication?
    private var focusCaptureArmedByShortcut = false

    /// Periodic check that the status item is actually visible in the
    /// menu bar. macOS silently hides items when there's no room (notch +
    /// many menubar apps). When that happens we shrink the layout —
    /// full piano → 1 octave → compact chip — until something fits.
    private var visibilityTimer: Timer?
    /// Live drag-source for the inline tape eject. NSStatusBarButton
    /// doesn't conform to NSDraggingSource so we hand a small adapter
    /// in; this ivar keeps it alive for the duration of the drag.
    private var tapeDragSource: TapeDragSource?
    /// 24fps redraw timer for the in-chip mini visualizer. Drives the
    /// per-bar sine wiggle + smooths the activity level so bars move
    /// continuously instead of snapping on note events.
    private var visualizerAnimTimer: Timer?
    /// Last-published values for the icon's animated state. The tick
    /// only calls `updateIcon()` when these move past a small
    /// perceptual epsilon — when the synth is silent and nothing is
    /// flashing, the status item stops repainting and idle CPU
    /// drops to near zero instead of burning ~24 redraws per second.
    private var visualizerLastDrawnLevel: CGFloat = -1
    private var visualizerLastDrawnMidiFlash: CGFloat = -1
    private var visualizerLastDrawnMetroFlash: CGFloat = -1
    private var visualizerSmoothedLevel: CGFloat = 0
    /// Running adaptive peak — mirrors the main waveform's auto-gain
    /// (`smoothedPeak` in WaveformView) so the menubar bars normalize
    /// to whatever the loudest recent sample was instead of sitting
    /// flat for quiet voices. Snaps up instantly on louder peaks,
    /// decays slowly so a sustained quiet note still lights the bars.
    private var visualizerSmoothedPeak: Float = 0.05
    /// Frames remaining in the post-attack "hold" window. While > 0,
    /// the bars stay pinned to whatever level the last attack reached
    /// instead of decaying — gives a more meter-like feel where the
    /// needle hangs at peak briefly before falling back. Combined
    /// with the slow release alpha below, the bars stay readable for
    /// ~700ms after the user lifts off the keys.
    private var visualizerHoldFrames: Int = 0
    /// Reused sample buffer for the RMS meter. 256 frames at 44.1kHz
    /// ≈ 5.8ms of audio — small window keeps the menubar bars
    /// snappy on note attack rather than smearing across the prior
    /// 20+ ms.
    private var visualizerSampleBuffer = [Float](repeating: 0, count: 256)
    /// Fast-responding bass / mid / treble bar levels (0..1) for the chip's
    /// 3-bar spectrum meter, plus a shared adaptive peak for normalization
    /// and the last-drawn snapshot for the dirty check.
    private var visualizerBars: [CGFloat] = [0, 0, 0]
    /// Per-band adaptive peaks so each bar normalizes to its OWN dynamics —
    /// otherwise bass dominates the shared gain and mid/treble stay stubby,
    /// making the three bars look lopsided instead of evenly lively.
    private var visualizerBandPeaks: [Float] = [0.05, 0.05, 0.05]
    private var visualizerLastDrawnBars: [CGFloat] = [-1, -1, -1]
    private var percLatLogCounter = 0
    /// Latest mic-input RMS published by `MenuBandSampleVoice`'s tap
    /// while the user is recording (key `). Drives the menubar VU
    /// bars when the sample-voice backend is capturing audio. The
    /// visualizer animation tick reads this on every frame and
    /// substitutes it for the synth-output RMS while
    /// `menuBand.sampleRecordingActive` is true.
    private var latestMicLevel: Float = 0
    /// Consecutive checks where macOS reports the compact status item
    /// hidden. This can be transient during SystemUIServer layout, so
    /// we track it for logs/retries instead of surfacing a modal.
    private var compactHiddenCheckCount = 0

    /// Click-away monitor active while the popover is shown. Catches clicks
    /// on OTHER apps and dismisses the popover. Clicks on our status-item
    /// button stay in-app and route through `statusClicked`, which only
    /// closes the popover when the user clicks the settings chip — piano
    /// taps keep the popover open.
    private var clickAwayMonitor: Any?

    /// Global + local .flagsChanged monitors that drive the shift →
    /// uppercase-label cue. Stored so we can clean them up if needed
    /// (NSEvent monitors are otherwise leaked on app exit, which is
    /// fine here but we keep the references for symmetry).
    private var globalShiftMonitor: Any?
    private var localShiftMonitor: Any?
    /// Live chord-morph .flagsChanged monitors — while a note key is held,
    /// pressing/releasing ⌘/⌥ re-voices it (single ↔ major/minor/sus) via
    /// `menuBand.morphHeldKeys`. Rides the same global+local stream so it
    /// works in TYPE mode and quiet-focus alike.
    private var globalChordMorphMonitor: Any?
    private var localChordMorphMonitor: Any?
    /// Double-tap right-⌘ → toggle Menu Band focus. A bare modifier
    /// can't be a Carbon hotkey, so this rides the same global/local
    /// .flagsChanged stream the shift monitor uses. Why double-tap:
    /// a single right-⌘ is too easy to hit by accident (and burns the
    /// key as a plain modifier system-wide). The double-tap is a
    /// deliberate gesture that leaves single ⌘ free for normal Mac
    /// chording.
    private var rightCmdMonitorGlobal: Any?
    private var rightCmdMonitorLocal: Any?
    /// Right Command's .flagsChanged virtual keycode (left ⌘ is 55).
    private static let rightCommandKeyCode: UInt16 = 54
    /// Wall time of the last bare right-⌘ DOWN edge. Reset to 0 once
    /// a pair is consumed or a chord/other-mod press interrupts the
    /// sequence, so the next tap starts a fresh window.
    private var lastRightCmdPressAt: CFTimeInterval = 0
    /// Max gap between the two bare right-⌘ presses that count as a
    /// double-tap. ~300 ms matches macOS's own "press ⌘ twice" feel.
    private static let rightCommandDoubleTapWindow: CFTimeInterval = 0.30
    private var popoverEscMonitor: Any?

    /// Sandbox-friendly local key capture. Armed when the user clicks the
    /// menubar piano (without opening the popover). The companion ghost
    /// timer paints letter labels on the menubar keys briefly when armed
    /// or when an actual key is pressed — visual signal that "you're
    /// capturing, type now."
    private let localCapture = LocalKeyCapture()
    // Bluetooth game controller (Xbox / DualSense / generic) → notes + all
    // controls. Lazy so `menuBand` is available to hand it. See GamepadManager.
    private lazy var gamepad = GamepadManager(controller: menuBand)
    /// Retained `ProcessInfo` activity token. Held for the app's
    /// lifetime so macOS treats Menu Band as a latency-critical
    /// instrument: no App Nap, no timer coalescing, no background
    /// throttling of the audio + gesture timers when the system is
    /// under load. Idle cost is negligible — it disables throttling,
    /// it doesn't consume CPU.
    private var playPrivilegeActivity: NSObjectProtocol?
    private var ghostUntil: CFTimeInterval = 0
    private var ghostRefreshTimer: Timer?

    /// Letter-wave state. Pivot is the most recently lit display note;
    /// `phaseStartedAt` is when the current direction began; `fadingIn`
    /// indicates direction. The renderer queries per-cell alpha derived
    /// from these — fade-in ripples outward from the pivot, fade-out
    /// retreats inward (far cells fade first, the pivot last).
    private var letterPivot: UInt8 = 60
    private var letterFadeTimer: Timer?

    // Menubar-icon "activity" animation: a brief horizontal slide of
    // the piano + a flash of the music-note glyph whenever the user
    // shifts octave or plays a note. Driven by a single 60 Hz timer
    // that runs only while either effect is in progress.
    private var lastKnownOctaveShift: Int = 0
    private var lastLitCount: Int = 0
    /// Monotonic note counter for the desktop-badge easter egg: each fresh
    /// note bumps it and writes "<seq> <noteName>" to the badge's signal file,
    /// which makes the blueberry sticker open its mouth and float a note out.
    /// No-ops on machines without the badge installed.
    private var badgeNoteSeq: Int = 0
    private var slideDirection: Int = 0
    private var slideStartedAt: CFTimeInterval = 0
    private var slideIsLimitNudge = false
    private var flashStrength: CGFloat = 0
    private var flashStartedAt: CFTimeInterval = 0
    private var iconAnimTimer: Timer?
    private static let slideDuration: CFTimeInterval = 0.34
    private static let limitNudgeDistance: CGFloat = 16
    private static let flashDuration: CFTimeInterval = 0.18
    /// Per-MIDI displayed alpha. Each tick we smooth this value
    /// toward the cell's target — so transitions are organic
    /// regardless of when keys are pressed (no snap when the user
    /// plays a fresh key while letters are still fading out).
    private var letterAlphas: [UInt8: CGFloat] = [:]
    /// MIDI notes whose attack wave has already touched them. Once a
    /// cell is reached it stays reached for the duration of the
    /// session — pivot changes mid-wave don't dim already-bright
    /// cells back to 0.
    private var letterReached: Set<UInt8> = []
    /// Wallclock time the current attack wave started — i.e. the
    /// first key press of this session. Reset after the wave fully
    /// retreats to 0.
    private var letterAttackStartedAt: CFTimeInterval = 0
    // Slower wave so neighboring cells trickle into view as the user
    // plays — the ghost letters build up across the whole board over a
    // second-ish of activity instead of all snapping in at once. Fade-
    // out is also softer so when playing stops, the letters settle out
    // gracefully rather than blinking off.
    private static let letterWaveStep: CFTimeInterval = 0.06    // 60 ms per cell of distance
    private static let letterFadeInDur: CFTimeInterval = 0.18
    private static let letterFadeOutDur: CFTimeInterval = 0.32

    /// Held so the KVO observation on NSApp.effectiveAppearance
    /// stays alive for the lifetime of the app delegate. Fires
    /// `systemAppearanceChanged()` whenever the system flips
    /// dark↔light.
    private var appearanceObservation: NSKeyValueObservation?
    /// Polling fallback for theme changes — KVO and the system
    /// distributed notification can occasionally drop the event
    /// for LSUIElement apps without a visible window.
    private var appearancePollTimer: Timer?
    private var lastObservedDarkMode: Bool = false

    // MARK: - Pitch-bend (trackpad gesture while playing)
    /// Current bend amount in [-1, 1] (mapped to ±2 semitones via
    /// the GM default bend range). 0 = no bend.
    private var bendAmount: Float = 0
    /// Current "space" amount in [0, 1], the reverb half of the
    /// bipolar X axis (negative side). 0 = dry/up-front, 1 = big
    /// room. Eases back to 0 alongside the bend spring on release.
    private var spaceAmount: Float = 0
    /// Current echo amount in [0, 1], the delay half of the bipolar
    /// X axis (positive side). Held with the other fx after release
    /// (see `startFxRelease`).
    private var echoAmount: Float = 0
    /// Bipolar X-axis driver in [-1, +1] for the chart's puck. Right
    /// (positive) feeds `echoAmount`, left (negative) feeds
    /// `spaceAmount`; center is 0 = no fx. Lets a single horizontal
    /// swipe pick either effect — the puck visibly slides past
    /// center to the side the user dragged.
    private var fxX: Float = 0
    /// Post-release "dead zone": all fx hold here fully engaged for
    /// `fxHoldDuration`, so resuming play within the window keeps the
    /// sound intact. Fires into `startFxRamp` only if nothing resumes.
    private var fxHoldTimer: Timer?
    /// Linear ramp timer — eases bend/space/echo to 0 over
    /// `fxRampDuration` once the hold expires. Replaces the old
    /// spring/exponential so the fx never cut off sharply.
    private var fxRampTimer: Timer?
    /// Fx values snapshotted at the instant the ramp begins, so the
    /// linear interpolation has a fixed origin.
    private var fxRampFromBend: Float = 0
    private var fxRampFromSpace: Float = 0
    private var fxRampFromEcho: Float = 0
    private var fxRampFromX: Float = 0
    private var fxRampStart: Date?
    // MARK: Source-side bend smoothing
    /// The trackpad gesture writes its latest accumulated pitch-bend TARGET
    /// here; `bendEaseTimer` then slews `bendAmount` (the value actually sent
    /// to the synths) toward it at a bounded rate. Without this, one trackpad
    /// `.mouseMoved` event jumps `bendAmount` in a single step and EVERY
    /// backend re-pitches at the next render quantum — the DLS MIDISynth, the
    /// varispeed sample/radio voices, and even the GM synth (whose own ~4 ms
    /// internal glide is too fast to bridge the ~10 ms between frames). That
    /// per-frame discontinuity is the "pop" heard while sliding fast. Easing
    /// at the source keeps each step well under a quarter-tone so nothing
    /// clicks.
    private var bendGestureTarget: Float = 0
    private var bendEaseTimer: Timer?
    /// Whether the in-flight ease should broadcast to all channels (Shift).
    private var bendEaseAllChannels: Bool = false
    /// True while one or more fingers rest on the trackpad (fed by
    /// LocalKeyCapture's touch sensor). The bend holds — and survives
    /// note changes / legato — for as long as this is true; it only
    /// springs back once the last finger lifts.
    private var trackpadTouchActive = false
    /// Tracks the last lit-note count we observed in `onLitChanged`
    /// so we can detect the all-notes-released edge and trigger
    /// both the bend rubber-band and the cursor pop.
    private var pitchBendCursorPushed = false
    /// Mirrors `CGAssociateMouseAndMouseCursorPosition(0)` state.
    /// Avoids spurious calls on every onLitChanged tick — only
    /// flips on the keyboard-held edges.
    private var pitchBendCursorLocked = false
    /// Sticky pitch-bend MODE. Once the gesture engages (hold a key +
    /// swipe), pitch stays latched: ALL subsequent trackpad movement
    /// bends, with no note required, until the user hits Esc or Menu
    /// Band loses focus. Releasing keys no longer ends it — only those
    /// two explicit exits do.
    private var pitchBendModeLatched = false
    /// Floating window that draws the bend wheel above every app.
    /// Used in lockstep with `CGDisplayHideCursor` so the real
    /// system cursor is invisible during pitch bend — that's what
    /// stops the cross-app flicker (other apps' cursorUpdate
    /// handlers can't fight a cursor that isn't being drawn).
    private var pitchBendOverlay: PitchBendCursorOverlayWindow?
    /// True while the system cursor is hidden via CGDisplayHideCursor.
    /// CGDisplayHide/Show calls are reference-counted — unbalanced
    /// hides leak across app sessions, so this guards a 1-to-1
    /// pairing.
    private var pitchBendSystemCursorHidden = false
    /// Screen position the cursor was at when the bend lock
    /// engaged. The overlay window anchors here for the duration
    /// of the gesture (the real cursor is detached by
    /// CGAssociateMouseAndMouseCursorPosition so position doesn't
    /// drift).
    private var pitchBendLockScreenPoint: NSPoint = .zero
    /// Short grace timer that ends the pitch-bend graphic after the last
    /// keyboard note lifts. A small delay so a fast legato note change
    /// (release one key, press the next) doesn't flicker the overlay.
    private var pitchBendEndTimer: Timer?
    /// While the pitch graph is open, releasing the last note doesn't kill
    /// the bend immediately — for `pitchBendReleaseGrace` seconds, trackpad
    /// movement re-engages the slide with no key held. Each move pushes this
    /// deadline forward; when movement stops past it, the session ends.
    private var pitchBendReleaseGraceUntil: Date?
    /// Local + global NSEvent monitors for trackpad scroll events.
    /// Held so we can teardown if needed (we don't, but per-instance
    /// retention is cleaner than globals).
    private var bendScrollLocal: Any?
    private var bendScrollGlobal: Any?
    private var octaveScrollLocal: Any?
    /// Running accumulator for the in-flight two-finger swipe.
    /// Reset on `.began` so consecutive gestures don't pile up,
    /// but the remainder within one gesture is preserved so a
    /// fast flick can shift multiple octaves.
    private var octaveScrollAccum: CGFloat = 0
    /// Pixels of accumulated swipe travel per octave step. Tuned
    /// low so a short flick on the menubar icon shows immediate
    /// movement — the first ~9 pixels of any swipe already step
    /// once. Combined with momentum-decay filtering, that gives a
    /// responsive "incremental" feel without overshooting.
    private static let octaveScrollPxPerStep: CGFloat = 9
    /// After the last note/finger releases, ALL fx hold fully
    /// engaged for this long. Replaying within the window cancels
    /// the release outright — the sound never even starts to fade.
    private static let fxHoldDuration: TimeInterval = 3.5
    /// Post-release "catch" window for the pitch graph: after the last
    /// keyboard note lifts, trackpad movement keeps bending for this long
    /// (re-armed on each move) so a fast release-then-swipe still grabs the
    /// graph instead of dropping the gesture. 250–500ms reads as deliberate
    /// without lingering.
    private static let pitchBendReleaseGrace: TimeInterval = 0.4
    /// Once the hold expires, bend/space/echo ramp LINEARLY to 0
    /// over this long — a gentle glide off, never a sharp cutoff.
    private static let fxRampDuration: TimeInterval = 1.0
    /// Trackpad-points-per-unit-bend. 80pt of vertical drag pulls
    /// to a full ±1 (= ±2 semitones at default GM range), so a
    /// modest two-finger flick reads as a noticeable bend.
    private static let bendSensitivityPerPoint: Float = 1.0 / 80.0
    /// Max bend magnitude in `bendAmount` units (1 unit = one octave via
    /// `bendSemitonesPerUnit`). The accumulator clamps here so there's no
    /// wind-up past the edge, and the overlay puck normalizes against it so
    /// the grid edge IS the cap. ±2 = two octaves down / up — which is also
    /// AVAudioUnitTimePitch's hard limit (±2400 cents) for the radio voice,
    /// so the radio reaches its true floor/ceiling at the grid edges.
    private static let bendRange: Float = 2.0
    /// Max bend slew, in `bendAmount` units per second, while easing the
    /// applied bend toward the gesture target (see `bendGestureTarget`). The
    /// full ±range (4 units peak-to-peak) resolves in ~⅓ s and a typical
    /// one-octave flick in ~85 ms — sized so each ~8 ms ease tick moves pitch
    /// by well under a quarter-tone, small enough that no backend clicks at
    /// the step yet fast enough to still read as immediate under the finger.
    private static let bendSlewPerSecond: Float = 12.0
    /// Ease tick rate (Hz). Fine-grained relative to a CoreAudio render
    /// quantum so the synths see a continuous slide, not a staircase.
    private static let bendEaseHz: Double = 120.0
    /// Trackpad-points-per-unit-space (X-axis). Deliberately less
    /// touchy than pitch: ~200pt of horizontal travel sweeps fully
    /// dry → full room, so it's a controllable ambience wash rather
    /// than a hair-trigger.
    private static let spaceSensitivityPerPoint: Float = 1.0 / 200.0
    /// Trackpad-points-per-unit-echo (⌥Option + X-axis). Matched to
    /// the space sensitivity so the two horizontal modes feel the
    /// same under the finger — only the modifier changes the target.
    private static let echoSensitivityPerPoint: Float = 1.0 / 200.0
    /// Echo half of the horizontal fx axis (swipe RIGHT). When false the
    /// X axis is clamped to its left/space (reverb) half only; flip to
    /// `false` again to temporarily disable echo.
    private static let fxEchoEnabled = true

    func applicationDidFinishLaunching(_ notification: Notification) {
        debugLog("applicationDidFinishLaunching pid=\(ProcessInfo.processInfo.processIdentifier)")
        // Claim latency-critical scheduling for the whole session so a
        // busy system (a release compile, ffmpeg, etc.) can't starve
        // the audio render + bend/decay timers mid-play. Pairs with
        // ProcessType=Interactive in the launchd plist.
        playPrivilegeActivity = ProcessInfo.processInfo.beginActivity(
            options: [.userInitiated, .latencyCritical],
            reason: "Menu Band live instrument audio"
        )
        Self.registerBundledFonts()
        // Apply forceLayout *before* statusItem creation so the initial
        // status-item length matches the pinned layout's imageSize.
        // Otherwise the icon flashes the default `.full` width until
        // the next updateIcon() catches up.
        applyForcedLayoutIfAny()
        // Mirror the tape-deck feature flag from UserDefaults into the
        // renderer BEFORE the status item is sized — otherwise the bar
        // would briefly reserve room for the deck and then snap back.
        KeyboardIconRenderer.tapeFeatureEnabled = UserDefaults.standard
            .bool(forKey: KeyboardIconRenderer.tapeFeatureDefaultsKey)
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(handleTapeFeatureToggled(_:)),
            name: .menuBandTapeFeatureChanged,
            object: nil
        )
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(handlePercussionSplitToggled(_:)),
            name: .menuBandPercussionSplitChanged,
            object: nil
        )
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(handleUseACMIDIToggled(_:)),
            name: .menuBandUseACMIDIChanged,
            object: nil
        )
        Timer.scheduledTimer(withTimeInterval: 5.0, repeats: true) { _ in
            debugLog("heartbeat")
        }
        menuBand.onChange = { [weak self] in
            DispatchQueue.main.async {
                guard let self = self else { return }
                // Pre-warm the MP3 export on the recording→idle edge so the
                // first drag-out / EJECT after a take feels instant. One
                // hook covers all stop paths; the lock in exportTapeMP3
                // makes overlap with an explicit stop-and-drop harmless.
                let recordingNow = self.menuBand.tape.state == .recording
                if self.tapeWasRecording && !recordingNow
                    && self.menuBand.tape.hasRecording {
                    self.prewarmTapeMP3()
                }
                self.tapeWasRecording = recordingNow
                // Trigger the slide + flash whenever the octave shift
                // changes — acts as the visual feedback for the
                // ,/. keys (or popover stepper). Direction matches
                // the change so up=right, down=left.
                let cur = self.menuBand.octaveShift
                if cur != self.lastKnownOctaveShift {
                    // Octave UP → piano slides LEFT (the camera is
                    // panning right toward higher notes). Octave DOWN
                    // → piano slides RIGHT (camera pans left). Reads
                    // like the player physically dragged the
                    // keyboard sideways under their hand.
                    let dir = (cur > self.lastKnownOctaveShift) ? -1 : 1
                    self.lastKnownOctaveShift = cur
                    self.kickIconAnim(slide: dir, flash: 1.0)
                }
                self.pushStaffPitchShift()
                self.updateIcon()
                self.updatePianoWaveformWindow()
                // Refresh the popover too so live state changes
                // (octave shift via , / . , MIDI mode flip, etc.)
                // reflect immediately while the popover is open.
                if self.isPopoverPanelShown {
                    self.popoverVC?.syncFromController()
                }
            }
        }
        menuBand.onOctaveLimitNudge = { [weak self] delta in
            DispatchQueue.main.async {
                guard let self = self else { return }
                let dir = delta > 0 ? -1 : 1
                self.kickIconAnim(slide: dir, flash: 0.55, limitNudge: true)
            }
        }
        menuBand.onLitChanged = { [weak self] in
            guard let self = self else { return }
            // Subtle flash on every fresh note hit so the icon
            // pulses with playing activity. Only on count
            // increment — releases don't re-fire the flash.
            let prev = self.lastLitCount
            let cur = self.menuBand.litNotes.count
            if cur > prev {
                self.kickIconAnim(slide: 0, flash: 0.7)
                // Replaying within the post-release window: freeze the
                // fx exactly where they are (mid-hold or mid-ramp) so
                // the sound continues seamlessly and never resets.
                self.cancelFxRelease()
                // Ping the desktop badge so the blueberry sings (no-op if absent).
                let topName = self.menuBand.litNotes.max().map(MenuBandController.noteName) ?? ""
                self.emitBadgeNote(topName)
            }
            // Pitch-bend cursor lifecycle — only engages when
            // the user is playing via the KEYBOARD (not mouse
            // taps). Locking the cursor on a mouse-tapped note
            // would freeze drag mid-stroke on the menubar
            // piano, so we gate on `keyboardNotesHeld`. Trackpad
            // pitch-bend is a keyboard-mode feature: hold a
            // letter → cursor locks → swipe to bend → release
            // letter → cursor unlocks + spring rubber-band.
            // Once latched, pitch-bend MODE persists regardless of which
            // notes are (or aren't) held — only Esc / focus loss ends it
            // (see endPitchBendSession callers). So we no longer end the
            // session here on note release; releasing keys just leaves
            // the bend where it is and keeps the mode live.
            let kbHeld = self.menuBand.keyboardNotesHeld
            if kbHeld {
                // A keyboard note is held — keep (or arm) the pitch-bend
                // graphic and cancel any pending teardown left over from a
                // momentary gap (fast legato note changes).
                self.pitchBendEndTimer?.invalidate()
                self.pitchBendEndTimer = nil
                self.pitchBendReleaseGraceUntil = nil
                if !self.pitchBendCursorLocked {
                    CGAssociateMouseAndMouseCursorPosition(0)
                    self.pitchBendCursorLocked = true
                    self.pitchBendModeLatched = true
                    // Snapshot the cursor's screen position so the
                    // overlay window can anchor there for the duration
                    // of the gesture. NSEvent.mouseLocation is in
                    // bottom-left-origin screen coords — same space
                    // NSWindow.setFrameOrigin expects.
                    self.pitchBendLockScreenPoint = NSEvent.mouseLocation
                }
            } else if self.pitchBendModeLatched {
                // No keyboard note held anymore. The pitch-shift graphic is
                // a hold-to-bend visual, so tear it down once the last key
                // lifts — after a short grace so a quick legato hand-off
                // doesn't flicker it. (Shift stays the deliberate exception:
                // it keeps bending still-ringing notes with no key down.)
                self.scheduleGraphicEndIfNoKeyHeld()
            }
            self.lastLitCount = cur
            self.updateIcon()
            self.popoverVC?.refreshHeldNotes()
            // Drive the About-window icon's live key glow (if open).
            NotificationCenter.default.post(name: .menuBandLitNotesChanged,
                                            object: nil)
            self.updatePianoWaveformWindow()
            // Refresh the pitch-bend pad so the puck's key-held highlight
            // tracks note presses even without trackpad motion (no-op when
            // the overlay isn't visible).
            self.updatePitchBendOverlayImage()
        }
        menuBand.onInstrumentVisualChange = { [weak self] in
            DispatchQueue.main.async {
                guard let self = self else { return }
                self.updateIcon()
                self.popoverVC?.refreshInstrumentVisuals()
                self.updatePianoWaveformWindow()
            }
        }
        menuBand.onMIDIEvent = {
            // Spike the square indicator to full on every
            // outbound noteOn; the visualizer animation tick
            // decays it back toward zero.
            KeyboardIconRenderer.midiActivityFlash = 1
            // [v1 cutoff] KidLisp TV amp-envelope stamp removed with the TV.
        }
        menuBand.bootstrap()
        // Subscribe to mic RMS during sample-voice recording. The
        // sample voice's input tap fires this on the main queue with
        // each block's RMS [0, 1]. We just stash it; the visualizer
        // animation tick (`startVisualizerAnimation`) picks it up the
        // next frame and substitutes it for the synth-output RMS
        // while recording is active.
        menuBand.setSampleLevelHandler { [weak self] lvl in
            self?.latestMicLevel = lvl
        }
        lastKnownOctaveShift = menuBand.octaveShift
        pianoWaveformWindowDelegate.onDismiss = { [weak self] in
            self?.updateIcon()
            self?.popoverVC?.syncFromController()
            self?.updatePianoWaveformWindowSuppression()
        }
        // While the popover is on screen, the floating panel's
        // collapsed frame snaps right-aligned to the popover's
        // left edge. The closure returns nil when the popover
        // isn't visible, falling the panel back to the menubar
        // status-item anchor.
        pianoWaveformWindowDelegate.popoverFrameProvider = { [weak self] in
            guard let self = self, self.isPopoverPanelShown else { return nil }
            return self.popoverPanel?.frame
        }
        pianoWaveformWindowDelegate.isPianoFocusActive = { [weak self] in
            guard let self = self else { return false }
            return self.localCapture.isArmed || self.pianoWaveformWindowDelegate.isKeyboardFocused
        }
        pianoWaveformWindowDelegate.onFocusRelease = { [weak self] in
            self?.finishPianoWaveformKeyboardFocus()
        }
        pianoWaveformWindowDelegate.onToggleKeymap = { [weak self] in
            self?.toggleKeyboardLayoutShortcut()
        }

        // Magnify the icon to fill the bar's usable height. The status
        // button centers (doesn't downscale) its image, so we scale up only
        // as far as the thickness allows — bigger on roomy/notched bars,
        // 1× on a tight bar. Hit-testing compensates via the same factor.
        let barThickness = NSStatusBar.system.thickness
        let baseIconH = KeyboardIconRenderer.baseImageSize.height
        if baseIconH > 0 {
            KeyboardIconRenderer.iconScale =
                max(1.0, min(1.6, (barThickness - 0.5) / baseIconH))
        }
        statusItem = NSStatusBar.system.statusItem(withLength: KeyboardIconRenderer.imageSize.width)
        debugLog("statusItem created, button=\(statusItem.button != nil) length=\(statusItem.length)")
        if let button = statusItem.button {
            let cell = NoHighlightStatusBarCell()
            cell.imagePosition = .imageOnly
            cell.isBordered = false
            cell.highlightsBy = []
            button.cell = cell
            button.imagePosition = .imageOnly
            button.target = self
            button.action = #selector(statusClicked(_:))
            button.sendAction(on: [.leftMouseDown, .rightMouseDown])
            button.isBordered = false

            hoverResponder.onMove = { [weak self] ev in self?.handleHover(event: ev) }
            hoverResponder.onExit = { [weak self] in self?.handleHoverExit() }
            let area = NSTrackingArea(
                rect: button.bounds,
                options: [.mouseMoved, .mouseEnteredAndExited,
                          .activeAlways, .inVisibleRect],
                owner: hoverResponder, userInfo: nil
            )
            button.addTrackingArea(area)
            trackingArea = area
            // Drag-drop a .mid / .midi file onto the menubar icon
            // to play it back through the synth as if a player
            // piano was striking the keys. A Menu Band-authored
            // .pdf works the same way — its embedded MusicXML is
            // round-tripped through Verovio to produce playable
            // MIDI, the popover opens to show the staff, and the
            // synth plays the score back. Drop target is a
            // transparent subview that fills the button so the
            // button still receives clicks normally.
            let dropTarget = MidiDropTargetView(frame: button.bounds)
            dropTarget.autoresizingMask = [.width, .height]
            dropTarget.onDrop = { [weak self] url in
                let ext = url.pathExtension.lowercased()
                if ext == "pdf" {
                    self?.handlePDFFileDrop(url: url)
                } else {
                    self?.handleMidiFileDrop(url: url)
                }
            }
            button.addSubview(dropTarget)
        }
        updateIcon()

        // Pre-build the waveform strip panel so the first note press
        // doesn't stall on panel + Metal pipeline construction.
        pianoWaveformWindowDelegate.reposition(statusItemButton: statusItem.button)
        pianoWaveformWindowDelegate.onStepBackward = { [weak self] in
            self?.menuBand.stepMelodicProgram(delta: -1)
        }
        pianoWaveformWindowDelegate.onStepForward = { [weak self] in
            self?.menuBand.stepMelodicProgram(delta: +1)
        }
        pianoWaveformWindowDelegate.onStepUp = { [weak self] in
            self?.menuBand.stepMelodicProgram(delta: -InstrumentListView.cols)
        }
        pianoWaveformWindowDelegate.onStepDown = { [weak self] in
            self?.menuBand.stepMelodicProgram(delta: +InstrumentListView.cols)
        }
        // The collapsed picker's "Keymap" button opens the full-screen
        // keymap view (large piano + QWERTY + mode toggle).
        pianoWaveformWindowDelegate.onOpenKeymap = { [weak self] in
            self?.pianoWaveformWindowDelegate.showExpandedForPopover()
        }
        pianoWaveformWindowDelegate.warmUp()

        // Four independent global controls: show the floating piano,
        // focus its keys, exit that focus, and switch the keyboard layout.
        // [v1] The double right-⌘ tap (startRightCommandTapMonitor) is the
        // ONLY system-wide key. It already arms AND disarms focus capture,
        // so the focus/exit global hotkeys are redundant; the piano-waveform
        // hotkey is dead (that overlay is retired); and layout + percussion
        // toggles still fire from the LOCAL `localCapture.onKey` handler —
        // `[`/`]` latch the percussion halves in-band. All other global
        // RegisterEventHotKey bindings stay unregistered:
        // _ = registerFocusCaptureHotkey(MenuBandShortcutPreferences.focusShortcut)
        // _ = registerPianoWaveformHotkey(MenuBandShortcutPreferences.playPaletteShortcut)
        // _ = registerExitFocusHotkey(MenuBandShortcutPreferences.exitFocusShortcut)
        // registerLayoutToggleHotkey()
        // registerPercussionToggleHotkey()

        // Start the Stickies bridge — listens for keystrokes when
        // the macOS Stickies app is frontmost and the focused
        // sticky matches the configured trigger color, then
        // forwards them through the same keymap the physical
        // keyboard uses. Requires Accessibility permission; on
        // first launch the system will prompt.
        #if !MAC_APP_STORE
        // The Stickies bridge taps global keystrokes and drives the
        // Stickies app via Apple Events — both forbidden by the App
        // Sandbox, so it's absent from the Mac App Store build. (Gating
        // the access here also means the lazy `stickiesBridge` is never
        // constructed in that build.)
        if stickiesBridge.isEnabled {
            stickiesBridge.start()
        }
        #endif

        // Dev affordance: post the
        // `computer.aestheticcomputer.menuband.showPopover`
        // distributed notification to toggle the popover from the
        // shell. Used during iteration to flash the popover open
        // after rebuild without hunting for the menubar item.
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(handleShowPopoverNotification(_:)),
            name: NSNotification.Name("computer.aestheticcomputer.menuband.showPopover"),
            object: nil
        )

        // Sibling remote: toggle the popover's instrument-chart
        // disclosure (same path as pressing the instrument name).
        // Lets the shell exercise the expand/collapse resize without
        // clicking.
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(handleToggleChartNotification(_:)),
            name: NSNotification.Name("computer.aestheticcomputer.menuband.toggleChart"),
            object: nil
        )

        // Sibling remote: open the About window directly. Lets the
        // shell verify (or screenshot) that the About panel renders
        // correctly without first walking through the popover.
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(handleShowAboutNotification(_:)),
            name: NSNotification.Name("computer.aestheticcomputer.menuband.showAbout"),
            object: nil
        )

        // Sibling remote: open the full-screen keymap view directly (same
        // path the popover's "Keymap" footer button drives). Lets the shell
        // verify / screenshot the expanded keymap screen without clicking.
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(handleShowKeymapNotification(_:)),
            name: NSNotification.Name("computer.aestheticcomputer.menuband.showKeymap"),
            object: nil
        )

        // Sibling remote: autoplay a melody. Switches to the GM program
        // named in userInfo["program"] (default Whistle, 78) and plays the
        // note sequence in userInfo["notes"] through the real tap path, so
        // the audio, the lit menubar keys, and the popover waveform all
        // animate exactly as if a human were playing. Lets the fleet drive
        // a machine to literally perform a refrain over ssh — no keyboard,
        // no AX, just one posted notification. See `handlePlayNotification`.
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(handlePlayNotification(_:)),
            name: NSNotification.Name("computer.aestheticcomputer.menuband.play"),
            object: nil
        )

        // Live engine: a conductible drone/arp/drum loop that runs
        // indefinitely and morphs on command (see MenuBandEngine). Four
        // verbs — start / chord / pattern / stop — let the fleet evolve a
        // piece of music over ssh without ever stopping the sound.
        for verb in ["start", "chord", "pattern", "stop", "listen"] {
            DistributedNotificationCenter.default().addObserver(
                self,
                selector: #selector(handleEngineNotification(_:)),
                name: NSNotification.Name("computer.aestheticcomputer.menuband.engine.\(verb)"),
                object: nil
            )
        }

        // Text-to-speech: let one machine speak (machines chatting before a
        // duet, count-ins, narration). Synced via startEpoch like everything
        // else. See handleSayNotification.
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(handleSayNotification(_:)),
            name: NSNotification.Name("computer.aestheticcomputer.menuband.say"),
            object: nil
        )

        // Multipeer fleet: play a received part, and expose two local triggers
        // — `fleet.play` (conduct self + relay the peer's part over MC) and
        // `fleet.status` (speak the connection state, for testing).
        //
        // GATED OUT of the Mac App Store build for v1: MultipeerConnectivity
        // advertising would trigger a Local Network permission prompt and add
        // peer-to-peer review surface, and the sandboxed behavior isn't verified
        // on a signed build yet. Never starting it leaves the MCSession/advertiser
        // constructed but inert (no advertise, no prompt). Ship the fleet as a
        // fast-follow once the signed-build Multipeer path + lazy prompt-on-use
        // start are verified. (The direct-download build keeps the fleet live.)
        #if !MAC_APP_STORE
        fleet.onMessage = { [weak self] msg in
            guard (msg["t"] as? String) == "play" else { return }
            var info: [String: String] = [:]
            for (k, v) in msg { if let s = v as? String { info[k] = s } }
            self?.playFromInfo(info)
        }
        fleet.start()
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(handleFleetPlayNotification(_:)),
            name: NSNotification.Name("computer.aestheticcomputer.menuband.fleet.play"),
            object: nil
        )
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(handleFleetStatusNotification(_:)),
            name: NSNotification.Name("computer.aestheticcomputer.menuband.fleet.status"),
            object: nil
        )
        #endif

        // Trackpad pitch-bend: while local capture is armed (the
        // user is playing notes via the keyboard), single-finger
        // trackpad cursor-Y movement bends the pitch of every
        // sounding channel. Stop moving for ~120ms and the spring
        // rubber-bands the bend back to center. Local + global
        // monitors so the gesture works whether the app is focused
        // or the user is typing through global capture into another
        // app.
        // Only listen for cursor moves WITHOUT a held mouse
        // button — a click-and-drag (e.g. drag across menubar
        // piano keys) would otherwise be interpreted as a bend
        // gesture. Trackpad pitch-bend is a single-finger
        // hover/swipe motion and exclusively generates
        // `.mouseMoved` events.
        bendScrollLocal = NSEvent.addLocalMonitorForEvents(
            matching: [.mouseMoved]
        ) { [weak self] event in
            self?.handlePitchBendCursorMove(event: event)
            return event
        }
        bendScrollGlobal = NSEvent.addGlobalMonitorForEvents(
            matching: [.mouseMoved]
        ) { [weak self] event in
            self?.handlePitchBendCursorMove(event: event)
        }

        // Two-finger trackpad swipe → octave step. Captures the
        // event app-wide so a swipe over the menubar icon, the
        // popover, or the floating piano panel all step octaves
        // regardless of which subview happens to be under the
        // cursor. Filtering by `eventWindowIsOurs` keeps the
        // monitor from intercepting scrolls in unrelated AppKit
        // windows (the About panel, the Aesthetic web window, etc).
        octaveScrollLocal = NSEvent.addLocalMonitorForEvents(
            matching: [.scrollWheel]
        ) { [weak self] event in
            return self?.handleOctaveScroll(event: event) ?? event
        }

        // System dark/light flips after sleep used to leave Menu Band
        // half-redrawn — the menubar icon redrawn fresh, but the
        // popover + floating panel kept stale colors. Two-pronged
        // listener: KVO on NSApp.effectiveAppearance covers the
        // canonical AppKit hand-off, and the lower-level
        // distributed notification (`AppleInterfaceThemeChanged`)
        // catches the case where the system flip happens while the
        // app is suspended (KVO can fire before our views are
        // back). Both funnel through `systemAppearanceChanged()`
        // which forces a top-to-bottom retint.
        // Belt-and-suspenders: poll the system appearance every
        // 0.6s and force a retint when it changes. KVO and the
        // distributed notification both occasionally miss flips
        // for LSUIElement (menubar) apps that don't have a
        // visible window — this guarantees the icon + popover +
        // panel catch up even if those paths drop the event.
        lastObservedDarkMode = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        appearancePollTimer = Timer.scheduledTimer(withTimeInterval: 0.6,
                                                     repeats: true) { [weak self] _ in
            guard let self = self else { return }
            let nowDark = NSApp.effectiveAppearance.bestMatch(
                from: [.aqua, .darkAqua]) == .darkAqua
            if nowDark != self.lastObservedDarkMode {
                self.lastObservedDarkMode = nowDark
                self.systemAppearanceChanged()
            }
        }
        appearanceObservation = NSApp.observe(\.effectiveAppearance,
                                              options: [.old, .new]) { [weak self] _, _ in
            DispatchQueue.main.async { self?.systemAppearanceChanged() }
        }
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(systemAppearanceChangedNotification(_:)),
            name: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil
        )
        // Sleep/wake recovery: when the system suspends, KVO +
        // distributed-notification + the polling timer can all
        // miss the theme flip that happened during sleep. Reset
        // our cached state on wake and force a retint so the
        // popover comes back in sync with the system theme.
        NSWorkspace.shared.notificationCenter.addObserver(
            self,
            selector: #selector(systemDidWake(_:)),
            name: NSWorkspace.didWakeNotification,
            object: nil
        )

        // Local key capture wiring. Routes keys to the same note logic the
        // global tap uses, with the ghost-label flash on every press so the
        // user sees the layout dynamically appear while typing.
        localCapture.onKey = { [weak self] keyCode, isDown, isRepeat, flags in
            guard let self = self else { return false }
            // Escape disarms capture explicitly. Useful when the user
            // wants to release focus without clicking another app. Also
            // the explicit exit for latched pitch-bend mode.
            if isDown && keyCode == 53 /* kVK_Escape */ {
                if self.pitchBendModeLatched { self.endPitchBendSession() }
                // Esc is now the UNFOCUS gesture — it shows the same red
                // flash + falling-bell cue the right-⌘ double-tap used to
                // play on disarm (⌘⌘ is arm-only now).
                FocusFlashOverlay.shared.flash(rising: false)
                self.menuBand.playFocusCue(rising: false)
                self.localCapture.disarm(reason: .cancelled)
                return true
            }
            if isDown && MenuBandShortcutPreferences.layoutShortcut.matches(
                keyCode: UInt32(keyCode),
                modifiers: MenuBandShortcut.carbonModifiers(from: flags)
            ) {
                self.toggleKeyboardLayoutShortcut()
                return true
            }
            if isDown && MenuBandShortcut.defaultPercussionToggle.matches(
                keyCode: UInt32(keyCode),
                modifiers: MenuBandShortcut.carbonModifiers(from: flags)
            ) {
                self.togglePercussionSplitFromShortcut()
                return true
            }
            // [v1 cutoff] Spacebar no longer toggles a metronome (removed);
            // it falls through to the normal note/key path.
            // Arrow keys always step the GM program — the chooser
            // grid lives in the floating panel that pairs with the
            // popover, so when either is open the user expects ←/→/
            // ↑/↓ to drive the bee-vision center. Each press both
            // commits the new program AND auditions a preview note
            // so the user hears the new voice without manually
            // pressing a piano key.
            // Arrow keys are unmapped — they pass through to the
            // focused app like any other navigation key. The
            // keyboard easter egg for stepping instruments has
            // been retired (use the on-screen ArrowKeysIndicator
            // chevrons in the floating panel for mouse-driven
            // stepping).
            // Keep the popover's touch sensor as first responder while
            // the user plays — a stray mouse click on a popover control
            // could otherwise move first-responder and stop indirect
            // touches from routing to the sensor mid-phrase, silently
            // breaking trackpad pitch-bend. Cheap to re-assert; only
            // acts when it isn't already first responder.
            if isDown, let sensor = self.popoverTouchSensor,
               sensor.window?.firstResponder !== sensor {
                sensor.window?.makeFirstResponder(sensor)
            }
            let consumed = self.menuBand.handleLocalKey(
                keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, flags: flags
            )
            if consumed && isDown {
                // Note plays only — the floating panel is reserved
                // for explicit triggers (LED chip / gear popover).
                // Earlier this called `showIfNeeded()` so a typed
                // letter would auto-open the panel; the surprise
                // pop-up was not what the user wanted while playing.
                // Use the most-recent lit display note as the wave pivot
                // so the ripple emanates from whichever key the user just
                // played. `litNotes` is updated synchronously on this
                // thread, so it already contains the new note.
                let pivot = self.menuBand.litNotes.max() ?? 60
                DispatchQueue.main.async { self.extendGhost(0.4, pivot: pivot) }
            }
            return consumed
        }
        localCapture.onCaptureEnd = { [weak self] reason in
            // Focus lost (user clicked another app). Drop the ghost and
            // any held notes so we don't leave anything hanging.
            self?.finishLocalCapture(reason: reason)
        }
        localCapture.onTrackpadTouchActiveChanged = { [weak self] active in
            self?.setTrackpadTouchActive(active)
        }

        // Bluetooth game controller. The Menu button toggles the popover
        // (showPopover already closes it when shown). The gamepad config UI
        // now lives in the full-screen Keymap overlay and self-refreshes off
        // the GameController connect/disconnect notifications, so no popover
        // callback is needed here.
        gamepad.onTogglePopover = { [weak self] in self?.showPopover() }
        gamepad.start()

        // Pre-instance the popover VC + force its view to load now so the
        // first click pops it instantly. The actual NSPanel host is
        // built lazily in `showPopover()` (panel position depends on
        // the floating piano panel's frame, which only exists once the
        // status item is on screen).
        installPopoverVC()

        // Language change → rebuild the popover with the new translations.
        // Cheaper than walking every label with a setter, and means future
        // strings just have to live in `Localization.tables` to participate.
        NotificationCenter.default.addObserver(
            forName: Localization.didChange,
            object: nil, queue: .main
        ) { [weak self] _ in
            self?.rebuildPopoverForLanguageChange()
        }
        NotificationCenter.default.addObserver(
            forName: .menuBandMicPermissionAlertWillShow,
            object: nil, queue: .main
        ) { [weak self] _ in
            self?.prepareForModalAlert()
        }

        startAdaptiveLayoutChecks()
        startShiftStateMonitors()
        startChordMorphMonitors()
        startRightCommandTapMonitor()
        startVisualizerAnimation()

        // Retint the running app's icon (About panel + Dock) to the
        // user's accent color. Sets NSApp.applicationIconImage rather
        // than mutating the bundle on disk — writing FinderInfo xattrs
        // to the bundle invalidated the signature and SIGKILLed the
        // process on lazy code-page loads (notably during PDF drag).
        // Refreshed whenever the accent changes.
        IconTinter.applyTintedIcon()
        NotificationCenter.default.addObserver(
            forName: NSColor.systemColorsDidChangeNotification,
            object: nil, queue: .main
        ) { _ in
            IconTinter.applyTintedIcon()
        }

        // `--focus-on-launch` is set by MenuBandLauncher when it
        // wakes MenuBand from a double-tap ⌘⌘. The shortcut should
        // not just launch the app — it should land in the same
        // popover-open + focus-armed state the in-process double-tap
        // handler produces. Fire that handler once the run loop has
        // settled (the status item button isn't positioned yet inside
        // applicationDidFinishLaunching, so showPopover bails without
        // the deferred dispatch).
        if CommandLine.arguments.contains("--focus-on-launch") {
            debugLog("--focus-on-launch flag detected; arming focus after run-loop settles")
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.4) { [weak self] in
                self?.toggleQuietFocusFromRightCommand()
            }
        }
    }

    // MARK: - Popover lifecycle

    /// Build a fresh `MenuBandPopoverViewController`, hand it our callbacks,
    /// and install it as the popover's content. Called once at launch and
    /// again whenever the language flips so every label rebuilds against
    /// the new translation table without us having to track each one.
    private func installPopoverVC() {
        let vc = MenuBandPopoverViewController()
        vc.menuBand = menuBand
        vc.onFocusShortcutChange = { [weak self] shortcut in
            self?.applyFocusShortcut(shortcut) ?? false
        }
        vc.onFocusShortcutRecordingChanged = { [weak self] isRecording in
            self?.setShortcutRecording(isRecording)
        }
        vc.onPlayPaletteToggle = { [weak self] in
            self?.togglePianoWaveformFromCommand()
        }
        vc.onPlayPaletteShortcutChange = { [weak self] shortcut in
            self?.applyPlayPaletteShortcut(shortcut) ?? false
        }
        vc.onPlayPaletteShortcutRecordingChanged = { [weak self] isRecording in
            self?.setShortcutRecording(isRecording)
        }
        vc.isPlayPaletteShown = { [weak self] in
            self?.pianoWaveformWindowDelegate.isShown ?? false
        }
        // Click on the mini visualizer strip → hide the popover
        // (without dismissing the floating panel — `closePopover`'s
        // default tears down both) and transition the floating
        // panel into Esteban's full-screen liquid expanded view.
        vc.onMiniVisualizerExpand = { [weak self] in
            guard let self = self else { return }
            if self.isPopoverPanelShown {
                self.closePopover(dismissFloatingPanel: false)
            }
            self.pianoWaveformWindowDelegate.showExpandedForPopover()
        }
        popoverVC = vc
        _ = vc.view
    }

    /// Swap the popover's content for a freshly-built VC so every string
    /// re-reads from the current locale. Preserves whether the popover was
    /// open — re-shows it relative to the status item if so.
    private func rebuildPopoverForLanguageChange() {
        let wasShown = isPopoverPanelShown
        if wasShown { closePopover() }
        installPopoverVC()
        popoverVC?.syncFromController()
        if wasShown { showPopover() }
    }

    private func prepareForModalAlert() {
        if isPopoverPanelShown {
            closePopover()
        }
        pianoWaveformWindowDelegate.dismiss(reason: .programmatic)
    }

    // MARK: - Global shortcuts

    private func registerTypeModeHotkey() {
        let hotkey = GlobalHotkey(
            signature: OSType(0x4E544B59),  // 'NTKY'
            id: 1
        ) { [weak self] in
            self?.menuBand.toggleTypeMode()
        }
        let shortcut = MenuBandShortcut.typeMode
        if hotkey.register(keyCode: shortcut.keyCode, modifiers: shortcut.modifiers) {
            typeModeHotkey = hotkey
        }
    }

    private func registerLayoutToggleHotkey() {
        let hotkey = GlobalHotkey(
            signature: OSType(0x4D424C54),  // 'MBLT'
            id: 1
        ) { [weak self] in
            self?.toggleKeyboardLayoutShortcut()
        }
        let shortcut = MenuBandShortcutPreferences.layoutShortcut
        if hotkey.register(
            keyCode: shortcut.keyCode,
            modifiers: shortcut.modifiers
        ) {
            layoutToggleHotkey = hotkey
        }
    }

    private func registerPercussionToggleHotkey() {
        let hotkey = GlobalHotkey(
            signature: OSType(0x4D425052),  // 'MBPR'
            id: 1
        ) { [weak self] in
            self?.togglePercussionSplitFromShortcut()
        }
        let shortcut = MenuBandShortcut.defaultPercussionToggle
        if hotkey.register(keyCode: shortcut.keyCode, modifiers: shortcut.modifiers) {
            percussionToggleHotkey = hotkey
        }
    }

    private func togglePercussionSplitFromShortcut() {
        menuBand.togglePercussionSplit()
        // Audible cue so the toggle lands without looking at the menubar.
        menuBand.playPercussionToggleCue(on: menuBand.percussionSplit)
    }

    @discardableResult
    private func registerPianoWaveformHotkey(_ shortcut: MenuBandShortcut) -> Bool {
        let hotkey = GlobalHotkey(
            signature: OSType(0x4D425050),  // 'MBPP'
            id: 1
        ) { [weak self] in
            self?.togglePianoWaveformFromShortcut()
        }
        guard hotkey.register(keyCode: shortcut.keyCode, modifiers: shortcut.modifiers) else {
            return false
        }
        pianoWaveformHotkey = hotkey
        return true
    }

    @discardableResult
    private func registerFocusCaptureHotkey(_ shortcut: MenuBandShortcut) -> Bool {
        let hotkey = GlobalHotkey(
            signature: OSType(0x4D42464B),  // 'MBFK'
            id: 1
        ) { [weak self] in
            self?.toggleFocusCaptureFromShortcut()
        }
        guard hotkey.register(keyCode: shortcut.keyCode, modifiers: shortcut.modifiers) else {
            return false
        }
        focusCaptureHotkey = hotkey
        return true
    }

    @discardableResult
    private func registerExitFocusHotkey(_ shortcut: MenuBandShortcut) -> Bool {
        let hotkey = GlobalHotkey(
            signature: OSType(0x4D425846),  // 'MBXF'
            id: 1
        ) { [weak self] in
            self?.exitPianoFocusFromShortcut()
        }
        guard hotkey.register(keyCode: shortcut.keyCode, modifiers: shortcut.modifiers) else {
            return false
        }
        exitFocusHotkey = hotkey
        localCapture.cancelShortcut = shortcut
        return true
    }

    private func shortcutConflictsWithAssignedRole(
        _ shortcut: MenuBandShortcut,
        excluding current: MenuBandShortcut
    ) -> Bool {
        [
            MenuBandShortcutPreferences.focusShortcut,
            MenuBandShortcutPreferences.playPaletteShortcut,
            MenuBandShortcutPreferences.exitFocusShortcut,
            MenuBandShortcutPreferences.layoutShortcut
        ].contains { $0 != current && $0 == shortcut }
    }

    private func applyFocusShortcut(_ shortcut: MenuBandShortcut) -> Bool {
        guard shortcut.isValidForRecording,
              !shortcut.isReservedForTypeMode,
              !shortcutConflictsWithAssignedRole(shortcut, excluding: MenuBandShortcutPreferences.focusShortcut) else {
            return false
        }
        let previous = MenuBandShortcutPreferences.focusShortcut
        focusCaptureHotkey?.unregister()
        focusCaptureHotkey = nil
        guard registerFocusCaptureHotkey(shortcut) else {
            _ = registerFocusCaptureHotkey(previous)
            return false
        }
        MenuBandShortcutPreferences.focusShortcut = shortcut
        return true
    }

    private func applyPlayPaletteShortcut(_ shortcut: MenuBandShortcut) -> Bool {
        guard shortcut.isValidForRecording,
              !shortcut.isReservedForTypeMode,
              !shortcutConflictsWithAssignedRole(shortcut, excluding: MenuBandShortcutPreferences.playPaletteShortcut) else {
            return false
        }
        let previous = MenuBandShortcutPreferences.playPaletteShortcut
        pianoWaveformHotkey?.unregister()
        pianoWaveformHotkey = nil
        guard registerPianoWaveformHotkey(shortcut) else {
            _ = registerPianoWaveformHotkey(previous)
            return false
        }
        MenuBandShortcutPreferences.playPaletteShortcut = shortcut
        return true
    }

    private func setShortcutRecording(_ isRecording: Bool) {
        if isRecording {
            typeModeHotkey?.unregister()
            typeModeHotkey = nil
            focusCaptureHotkey?.unregister()
            focusCaptureHotkey = nil
            pianoWaveformHotkey?.unregister()
            pianoWaveformHotkey = nil
            exitFocusHotkey?.unregister()
            exitFocusHotkey = nil
            layoutToggleHotkey?.unregister()
            layoutToggleHotkey = nil
        } else {
            if typeModeHotkey == nil { registerTypeModeHotkey() }
            if focusCaptureHotkey == nil {
                _ = registerFocusCaptureHotkey(MenuBandShortcutPreferences.focusShortcut)
            }
            if pianoWaveformHotkey == nil {
                _ = registerPianoWaveformHotkey(MenuBandShortcutPreferences.playPaletteShortcut)
            }
            if exitFocusHotkey == nil {
                _ = registerExitFocusHotkey(MenuBandShortcutPreferences.exitFocusShortcut)
            }
            if layoutToggleHotkey == nil { registerLayoutToggleHotkey() }
        }
    }

    private func togglePianoWaveformFromShortcut() {
        if pianoWaveformWindowDelegate.isShown {
            pianoWaveformWindowDelegate.toggleFromShortcut()
            updatePianoWaveformWindowSuppression()
            return
        }
        beginPianoWaveformPresentation()
        pianoWaveformWindowDelegate.toggleFromShortcut()
        updatePianoWaveformWindowSuppression()
    }

    private func togglePianoWaveformFromCommand() {
        let appToRestore = appBeforePopover
        beginPianoWaveformPresentation()
        appBeforePopover = nil
        pianoWaveformWindowDelegate.showFromCommand(restoringTo: appToRestore)
        updatePianoWaveformWindowSuppression()
    }

    private func beginPianoWaveformPresentation() {
        closePopover()
        if localCapture.isArmed {
            localCapture.disarm(reason: .resignedKey)
        }
        if menuBand.typeMode {
            menuBand.disableTypeModeForFocusCapture()
        }
        updatePianoWaveformWindowSuppression()
    }

    private func toggleFocusCaptureFromShortcut() {
        // Focus is separate from show/hide: K brings the expanded piano
        // forward and leaves it ready for keyboard play. E exits focus.
        let popoverWasOpen = isPopoverPanelShown
        closePopover()
        if menuBand.typeMode {
            menuBand.disableTypeModeForFocusCapture()
        }
        if popoverWasOpen {
            DispatchQueue.main.async { [weak self] in
                self?.pianoWaveformWindowDelegate.showExpandedForPopover()
            }
        } else {
            pianoWaveformWindowDelegate.showExpandedForPopover()
        }
    }

    /// Right-⌘ tap toggle: arm/disarm keyboard play WITHOUT bringing
    /// up the expanded piano overlay (that's what ⌃⌥⌘K does). Just
    /// readies the menubar instrument for typing, with a little chime
    /// so the silent focus change is felt.
    private func toggleQuietFocusFromRightCommand() {
        // Right-⌘ double-tap is FOCUS-ONLY now: it arms (or re-arms) quiet
        // focus and is inert once already focused — unfocusing is Esc's job
        // (see the localCapture Esc handler). Keeps the gesture a pure
        // "bring me into play" action that can't accidentally drop focus.
        guard !localCapture.isArmed else { return }
        // Arm focus capture WITHOUT touching the popover — the blue glow +
        // rising bell are the "it landed" cue. keepPopoverOpen:true leaves an
        // already-open popover as-is rather than closing it.
        beginFocusCaptureFromShortcut(keepPopoverOpen: true)
        FocusFlashOverlay.shared.flash(rising: true)
        menuBand.playFocusCue(rising: true)
    }

    private func exitPianoFocusFromShortcut() {
        if localCapture.isArmed {
            localCapture.disarm(reason: .cancelled)
            return
        }
        finishPianoWaveformKeyboardFocus()
    }

    /// Legacy focus-capture path — preserved as a stub so the
    /// existing `focusCaptureArmedByShortcut` callers keep
    /// compiling. The shortcut itself was repurposed above.
    private func _unusedLegacyFocusCapture() {
        if pianoWaveformWindowDelegate.isKeyboardFocused {
            finishPianoWaveformKeyboardFocus()
            return
        }
        if localCapture.isArmed, focusCaptureArmedByShortcut {
            localCapture.disarm(reason: .cancelled)
            return
        }
        beginFocusCaptureFromShortcut()
    }

    private func beginFocusCaptureFromShortcut(keepPopoverOpen: Bool = false) {
        let frontmost = NSWorkspace.shared.frontmostApplication
        if frontmost?.bundleIdentifier == Bundle.main.bundleIdentifier {
            appBeforeFocusCapture = nil
        } else {
            appBeforeFocusCapture = frontmost
        }
        if !keepPopoverOpen {
            closePopover()
        }
        if menuBand.typeMode {
            menuBand.disableTypeModeForFocusCapture()
        }
        focusCaptureArmedByShortcut = true
        localCapture.arm()
        updateIcon()
        updatePianoWaveformWindow()
    }

    private func finishLocalCapture(reason: LocalKeyCapture.EndReason) {
        let shouldRestoreFocus = focusCaptureArmedByShortcut && reason == .cancelled
        focusCaptureArmedByShortcut = false
        // Menu Band lost key focus → exit latched pitch mode too.
        if pitchBendModeLatched { endPitchBendSession() }
        menuBand.releaseAllHeldNotes()
        ghostUntil = 0
        ghostRefreshTimer?.invalidate()
        ghostRefreshTimer = nil
        updateIcon()
        updatePianoWaveformWindow()
        if shouldRestoreFocus {
            restorePreviousAppFocus()
        }
        appBeforeFocusCapture = nil
    }

    private func restorePreviousAppFocus() {
        guard let app = appBeforeFocusCapture,
              !app.isTerminated,
              app.bundleIdentifier != Bundle.main.bundleIdentifier else { return }
        app.activate(options: [.activateIgnoringOtherApps])
    }

    private func finishPianoWaveformKeyboardFocus() {
        menuBand.releaseAllHeldNotes()
        pianoWaveformWindowDelegate.clearInteraction()
        pianoWaveformWindowDelegate.releaseKeyboardFocus()
        updateIcon()
        updatePianoWaveformWindow()
    }

    private func toggleKeyboardLayoutShortcut() {
        menuBand.keymap = (menuBand.keymap == .ableton) ? .notepat : .ableton
    }

    /// Drag-and-drop entry point — replays the file's notes
    /// through `startTapNote` / `stopTapNote` so the popover
    /// staff + the menubar icon light up alongside the audio,
    /// like a player piano performing the file.
    private func handleMidiFileDrop(url: URL) {
        // Cancel any previous playback and silence held notes
        // before kicking off the new file — superseding playback
        // shouldn't leave stuck notes from the old one.
        MidiFilePlayer.stop()
        menuBand.releaseAllHeldNotes()
        let started = MidiFilePlayer.play(
            url: url,
            onNoteOn: { [weak self] midi, velocity in
                self?.menuBand.startTapNote(midi, velocity: velocity,
                                            pan: 0, displayNote: midi)
            },
            onNoteOff: { [weak self] midi in
                self?.menuBand.stopTapNote(midi)
            },
            onFinish: { [weak self] in
                self?.menuBand.releaseAllHeldNotes()
            }
        )
        if !started {
            NSLog("MenuBand: MIDI drop \(url.lastPathComponent) had no playable events")
        }
    }

    /// [v1 cutoff] Menu Band PDF-score import rode on the Verovio
    /// SheetMusicView, which was removed for v1. Dropping a `.pdf`
    /// now just logs and is ignored; MIDI-file drops still play.
    /// Score import returns post-v1 with the reintegrated staff.
    private func handlePDFFileDrop(url: URL) {
        NSLog("MenuBand: PDF drop \(url.lastPathComponent) ignored — score import is not in v1")
    }

    // MARK: - Adaptive menubar layout

    /// `true` when the status item button is laid out on a real screen.
    /// macOS leaves the button's window assigned but with `screen == nil`
    /// when there's no room for it in the menu bar.
    private func isStatusItemVisible() -> Bool {
        guard let button = statusItem.button else { return false }
        guard let window = button.window else { return false }
        if window.screen == nil { return false }
        return NSScreen.screens.contains { $0.frame.intersects(window.frame) }
    }

    /// `forceLayout` UserDefaults key: lets the user pin the layout to
    /// `full` / `fullSlim` / `oneOctave` / `compact` regardless of how
    /// much menubar room exists. Intended both as a dev/QA aid and as
    /// a preference for users who like a specific footprint.
    ///
    ///   defaults write computer.aestheticcomputer.menuband forceLayout fullSlim
    ///   launchctl kickstart -k gui/$(id -u)/computer.aestheticcomputer.menuband
    ///
    /// Clear with `defaults delete ... forceLayout`.
    /// 24fps redraw loop driving the mini visualizer's continuous
    /// motion. Level is exponentially smoothed toward the live note
    /// count so attacks ramp up over a couple frames and releases
    /// glide back to idle. Phase is just CACurrentMediaTime — the
    /// renderer uses it to spread bar wiggles by a per-bar offset.
    /// Suppressed when the popover or palette is open (no point
    /// burning frames on a hidden meter).
    private func startVisualizerAnimation() {
        visualizerAnimTimer?.invalidate()
        menuBand.setWaveformCaptureEnabled(true)
        // 24fps is enough for the tiny menubar VU meter and avoids
        // spending a full CPU core on synchronous status-item redraws.
        // Bars are ALWAYS animating (popover or palette open or not);
        // when silent they fall to a flat/short floor instead of
        // vanishing, so the menubar still looks "alive."
        let frameRate: CGFloat = 24
        let timer = Timer(timeInterval: TimeInterval(1.0 / frameRate), repeats: true) { [weak self] _ in
            guard let self = self else { return }
            // Two RMS sources, one consumer:
            //   • While the user is recording (holding `), the bars
            //     pulse with mic-input level — published by
            //     `MenuBandSampleVoice`'s input tap into
            //     `latestMicLevel`.
            //   • Otherwise (the normal case), the bars pulse with
            //     the synth's own pre-limiter mixer output, computed
            //     from the 256-sample waveform tap below.
            let rms: Float
            if self.menuBand.sampleRecordingActive {
                rms = self.latestMicLevel
            } else {
                self.menuBand.synthSnapshotWaveform(into: &self.visualizerSampleBuffer)
                var sumSq: Float = 0
                for s in self.visualizerSampleBuffer { sumSq += s * s }
                rms = sqrt(sumSq / Float(self.visualizerSampleBuffer.count))
            }

            // Adaptive auto-gain — same envelope as WaveformView's
            // `smoothedPeak`. Snap up the moment we see a louder
            // sample so transients are captured at full scale; bleed
            // down slowly so a sustained quiet sound still pushes the
            // bars near the top once the peak settles.
            if rms > self.visualizerSmoothedPeak {
                self.visualizerSmoothedPeak = rms
            } else {
                self.visualizerSmoothedPeak =
                    max(0.05, self.visualizerSmoothedPeak * 0.92 + rms * 0.08)
            }
            let gain = 0.95 / self.visualizerSmoothedPeak
            let target = CGFloat(min(1.0, rms * gain))

            // Hold-then-decay envelope: snap-up on attack, hang at
            // peak for ~250ms, then bleed down slowly (~480ms half-
            // life). Total tail ≈ 700ms before bars settle to the
            // silent floor — gives the meter a satisfying "needle
            // hangs" feel instead of cutting back to flat the
            // instant a key releases.
            let holdFramesAtPeak = Int(round(frameRate * 0.25))
            let releaseAlpha: CGFloat = 0.056 // ~480ms half-life at 24fps
            if target >= self.visualizerSmoothedLevel {
                self.visualizerSmoothedLevel = target
                self.visualizerHoldFrames = holdFramesAtPeak
            } else if self.visualizerHoldFrames > 0 {
                self.visualizerHoldFrames -= 1
            } else {
                self.visualizerSmoothedLevel +=
                    (target - self.visualizerSmoothedLevel) * releaseAlpha
            }
            KeyboardIconRenderer.miniVisualizerLevel = self.visualizerSmoothedLevel
            KeyboardIconRenderer.miniVisualizerPhase = CACurrentMediaTime()
            // MIDI activity square decays toward 0 each tick. Got
            // a fresh hit? `onMIDIEvent` already spiked it to 1.
            KeyboardIconRenderer.midiActivityFlash *= 0.86
            if KeyboardIconRenderer.midiActivityFlash < 0.01 {
                KeyboardIconRenderer.midiActivityFlash = 0
            }
            // Metronome blink — popover's metronome spikes this to
            // 1 on each beat; decay slow enough that the yellow
            // tint reads as a clear pulse, not a flicker.
            KeyboardIconRenderer.metronomeFlash *= 0.88
            if KeyboardIconRenderer.metronomeFlash < 0.01 {
                KeyboardIconRenderer.metronomeFlash = 0
            }
            // Three real bars — split the live waveform into bass / mid /
            // treble via one-pole filters, RMS each, normalize to a shared
            // adaptive peak, and smooth with INSTANT attack + fast release
            // so the meter is honest and time-accurate (no decorative
            // wiggle). While recording, the mic level fans across the bars.
            var bass: Float = 0, mid: Float = 0, treble: Float = 0
            if self.menuBand.sampleRecordingActive {
                bass = self.latestMicLevel
                mid = self.latestMicLevel * 0.8
                treble = self.latestMicLevel * 0.6
            } else {
                var lpLow: Float = 0, lpMid: Float = 0
                let aLow: Float = 0.035, aMid: Float = 0.22
                var bSq: Float = 0, mSq: Float = 0, tSq: Float = 0
                for s in self.visualizerSampleBuffer {
                    lpLow += aLow * (s - lpLow)
                    lpMid += aMid * (s - lpMid)
                    let b = lpLow
                    let m = lpMid - lpLow
                    let t = s - lpMid
                    bSq += b * b; mSq += m * m; tSq += t * t
                }
                let n = Float(self.visualizerSampleBuffer.count)
                bass = (bSq / n).squareRoot()
                mid = (mSq / n).squareRoot()
                treble = (tSq / n).squareRoot()
            }
            // Per-band adaptive gain: normalize each band to ITS OWN recent
            // peak so bass (lower absolute energy) still swings full-height
            // instead of staying a stub — all three bars read evenly.
            let bandVals: [Float] = [bass, mid, treble]
            var bandTargets: [CGFloat] = [0, 0, 0]
            // Noise gate: below this the signal is effectively silence, so
            // skip the per-band gain (which would otherwise amplify the
            // noise floor into twitching bars) and let them fall to the
            // flat baseline.
            let gateOpen = max(bass, max(mid, treble)) > 0.004
            for i in 0..<3 where gateOpen {
                if bandVals[i] > self.visualizerBandPeaks[i] {
                    self.visualizerBandPeaks[i] = bandVals[i]
                } else {
                    self.visualizerBandPeaks[i] =
                        max(0.015, self.visualizerBandPeaks[i] * 0.95 + bandVals[i] * 0.05)
                }
                let gain = 0.95 / self.visualizerBandPeaks[i]
                bandTargets[i] = CGFloat(min(1, bandVals[i] * gain))
            }
            for i in 0..<3 {
                if bandTargets[i] >= self.visualizerBars[i] {
                    self.visualizerBars[i] = bandTargets[i]      // instant attack
                } else {
                    self.visualizerBars[i] += (bandTargets[i] - self.visualizerBars[i]) * 0.4
                }
            }
            KeyboardIconRenderer.miniVisualizerBars = self.visualizerBars
            var barsDirty = false
            for i in 0..<3 where abs(self.visualizerBars[i] - self.visualizerLastDrawnBars[i]) > 0.02 {
                barsDirty = true
            }

            // Percussion vibe: push the live per-drum hit pulses so the
            // right-hand keys shake/blink with each hit. Keep repainting
            // while any pulse is still fresh.
            var percActive = false
            if self.menuBand.percussionSplit {
                let now = CACurrentMediaTime()
                let pulses = self.menuBand.percussionPulses()
                KeyboardIconRenderer.drumPulses = pulses
                KeyboardIconRenderer.drumPulseNow = now
                for p in pulses where p.at > 0 && (now - p.at) < 0.45 && p.level > 0.01 {
                    percActive = true
                    break
                }
                // Surface the drum trigger→render latency ~1×/sec while
                // playing so we can verify it stays at/below one buffer.
                if percActive {
                    self.percLatLogCounter += 1
                    if self.percLatLogCounter % 24 == 0 {
                        NSLog(String(format: "MenuBand perc latency: trigger→render %.2f ms",
                                     self.menuBand.percussionTriggerHandoffMs()))
                    }
                }
            } else if !KeyboardIconRenderer.drumPulses.isEmpty {
                KeyboardIconRenderer.drumPulses = []
            }

            // Only repaint the status item when one of the animated
            // signals has actually moved a visible amount. The phase
            // value updates every tick but the per-bar wiggle is too
            // subtle to see when the levels are flat (silent floor),
            // so the eye won't notice the skipped frame — but
            // skipping is the difference between idle CPU at ~0%
            // versus a steady drain from constant menubar repaints.
            let level = self.visualizerSmoothedLevel
            let midi = CGFloat(KeyboardIconRenderer.midiActivityFlash)
            let metro = CGFloat(KeyboardIconRenderer.metronomeFlash)
            let levelStep: CGFloat = 0.01
            let flashStep: CGFloat = 0.02
            let dirty = percActive || barsDirty
                || abs(level - self.visualizerLastDrawnLevel) > levelStep
                || abs(midi - self.visualizerLastDrawnMidiFlash) > flashStep
                || abs(metro - self.visualizerLastDrawnMetroFlash) > flashStep
            if dirty {
                self.visualizerLastDrawnLevel = level
                self.visualizerLastDrawnMidiFlash = midi
                self.visualizerLastDrawnMetroFlash = metro
                self.visualizerLastDrawnBars = self.visualizerBars
                self.updateIcon()
            }
        }
        RunLoop.main.add(timer, forMode: .common)
        visualizerAnimTimer = timer
    }

    /// Watch shift state globally + locally so the menubar piano can
    /// uppercase its letter labels while the user holds shift. The
    /// uppercase letters are the visual cue that linger / bell-ring
    /// mode is armed; lowercase = normal.
    /// Map the current modifier mask to which keyboard half should render
    /// uppercase. Mirrors `MenuBandController.lingerSide`: the left/right
    /// shift are told apart by the device-dependent bits (0x2 / 0x4) the
    /// plain `.shift` mask collapses. A single shift sides; caps lock or
    /// both shifts arm the whole board.
    private static func uppercaseSide(for flags: NSEvent.ModifierFlags)
        -> KeyboardIconRenderer.UppercaseSide {
        let raw = UInt64(flags.rawValue)
        let left = (raw & 0x0000_0002) != 0
        let right = (raw & 0x0000_0004) != 0
        if left && !right { return .left }
        if right && !left { return .right }
        if flags.contains(.shift) || flags.contains(.capsLock) { return .all }
        return .none
    }

    private func startShiftStateMonitors() {
        let handler: (NSEvent) -> Void = { [weak self] event in
            guard let self = self else { return }
            // Caps lock latches the mode; shift is the momentary. The
            // shift *side* picks which half uppercases (mirrors the sided
            // linger); caps / both shifts uppercase the whole board.
            let caps = event.modifierFlags.contains(.capsLock)
            let side = Self.uppercaseSide(for: event.modifierFlags)
            var dirty = false
            if KeyboardIconRenderer.labelsUppercaseSide != side {
                KeyboardIconRenderer.labelsUppercaseSide = side
                dirty = true
            }
            if KeyboardIconRenderer.lingerCapsLatched != caps {
                KeyboardIconRenderer.lingerCapsLatched = caps
                dirty = true
            }
            if dirty {
                self.updateIcon()
                // Staff view's note letters honor labelsUppercase too;
                // repaint held notes so capitalization flips along
                // with the menubar piano + qwerty map.
                self.popoverVC?.refreshHeldNotes()
            }
        }
        globalShiftMonitor = NSEvent.addGlobalMonitorForEvents(
            matching: .flagsChanged
        ) { event in handler(event) }
        localShiftMonitor = NSEvent.addLocalMonitorForEvents(
            matching: .flagsChanged
        ) { event in handler(event); return event }
        // Initial-state sync: .flagsChanged only fires on changes, so
        // a caps-lock already on at launch wouldn't paint uppercase
        // until something toggles. Read the current modifier mask once
        // at startup to seed the renderer correctly.
        let initial = NSEvent.modifierFlags
        let initialCaps = initial.contains(.capsLock)
        let initialSide = Self.uppercaseSide(for: initial)
        var initialDirty = false
        if KeyboardIconRenderer.labelsUppercaseSide != initialSide {
            KeyboardIconRenderer.labelsUppercaseSide = initialSide
            initialDirty = true
        }
        if KeyboardIconRenderer.lingerCapsLatched != initialCaps {
            KeyboardIconRenderer.lingerCapsLatched = initialCaps
            initialDirty = true
        }
        if initialDirty { updateIcon() }
    }

    /// Live chord morph: while a note key is physically held, watch ⌘/⌥ and
    /// re-voice the held key between a single note and a triad. The chord
    /// scheme matches the keyDown path — ⌘ = major, ⌥ = minor, ⌘+⌥ = sus —
    /// so a note that started plain and one that started chorded morph the
    /// same way (⌃ is chord-inert; its shortcuts stay system shortcuts).
    /// Both a global monitor (TYPE mode / background apps) and a local one
    /// (quiet-focus, Menu Band frontmost) feed the same handler; the
    /// controller no-ops when nothing is held, so wiring both is harmless.
    private func startChordMorphMonitors() {
        let handler: (NSEvent) -> Void = { [weak self] event in
            guard let self = self else { return }
            let flags = event.modifierFlags
            let cmd = flags.contains(.command)
            let opt = flags.contains(.option)
            self.menuBand.morphHeldKeys(chordModifier: cmd || opt,
                                        chordMinor: opt,
                                        chordSus: cmd && opt)
        }
        globalChordMorphMonitor = NSEvent.addGlobalMonitorForEvents(
            matching: .flagsChanged
        ) { event in handler(event) }
        localChordMorphMonitor = NSEvent.addLocalMonitorForEvents(
            matching: .flagsChanged
        ) { event in handler(event); return event }
    }

    /// Double-tap right-⌘ → toggle focus capture. Two bare right-⌘
    /// presses within `rightCommandDoubleTapWindow` arm / disarm
    /// the menubar piano for typing; a single tap does nothing, so
    /// right ⌘ stays free as a normal modifier for the rest of the
    /// system. Trade-off vs. the old single-press flow: you can't
    /// do `right-⌘+f` to arm + play F in one motion — you tap twice
    /// first, then play. That's the price of not arming on every
    /// stray ⌘. The toggle fires a dry click on the qualifying
    /// second press so the user feels the gesture land.
    ///
    /// A bare modifier isn't a valid Carbon hotkey, so we ride the
    /// same global/local .flagsChanged stream the shift monitor
    /// already has permission for — nothing new.
    private func startRightCommandTapMonitor() {
        let handler: (NSEvent) -> Void = { [weak self] event in
            guard let self = self else { return }
            guard event.type == .flagsChanged,
                  event.keyCode == Self.rightCommandKeyCode else { return }
            // .flagsChanged fires on press AND release for the same
            // physical key — `.command` is set on the down edge,
            // cleared on the up edge. We only count down edges.
            let isDown = event.modifierFlags.contains(.command)
            guard isDown else { return }
            // Bare right-⌘ only. If anything else is held (⇧/⌥/⌃/
            // capsLock, or a chord like ⌘⇧), this isn't a double-tap
            // candidate — reset the window so a chord can't form
            // half of a future double-tap.
            let mask = event.modifierFlags.intersection(.deviceIndependentFlagsMask)
            guard mask == .command else {
                self.lastRightCmdPressAt = 0
                return
            }
            let now = CACurrentMediaTime()
            if now - self.lastRightCmdPressAt <= Self.rightCommandDoubleTapWindow {
                // Pair completed — consume it so a third press
                // starts a fresh window instead of chaining toggles.
                self.lastRightCmdPressAt = 0
                FocusCueBeep.shared.click()
                self.toggleQuietFocusFromRightCommand()
            } else {
                self.lastRightCmdPressAt = now
            }
        }
        rightCmdMonitorGlobal = NSEvent.addGlobalMonitorForEvents(
            matching: [.flagsChanged, .keyDown]
        ) { handler($0) }
        rightCmdMonitorLocal = NSEvent.addLocalMonitorForEvents(
            matching: [.flagsChanged, .keyDown]
        ) { handler($0); return $0 }
    }

    private func applyForcedLayoutIfAny() {
        let raw = UserDefaults.standard.string(forKey: "forceLayout") ?? ""
        guard !raw.isEmpty,
              let layout = KeyboardIconRenderer.DisplayLayout(rawValue: raw) else {
            KeyboardIconRenderer.forceLayout = nil
            return
        }
        KeyboardIconRenderer.forceLayout = layout
        KeyboardIconRenderer.displayLayout = layout
        debugLog("forceLayout pinned to \(raw)")
    }

    private func startAdaptiveLayoutChecks() {
        // Initial fit pass — give the system a beat to lay out before
        // probing visibility.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4) { [weak self] in
            self?.adaptLayoutForAvailableSpace()
        }
        // Periodic re-check. Was 6s — bumped to 2s so when an app
        // quits and frees menubar room, we re-expand within a
        // couple seconds instead of feeling stuck in thin-keys
        // mode for half a minute.
        visibilityTimer?.invalidate()
        visibilityTimer = Timer.scheduledTimer(withTimeInterval: 2.0, repeats: true) { [weak self] _ in
            self?.adaptLayoutForAvailableSpace()
        }
    }

    private func adaptLayoutForAvailableSpace() {
        // `forceLayout` used to hard-pin and disable adapt entirely
        // — that left the icon stuck in whatever layout was chosen
        // even after the user freed up menubar space. Treat it as
        // a CEILING instead: the adapter can shrink below it under
        // pressure but never expands past it.
        let ceiling: KeyboardIconRenderer.DisplayLayout? =
            KeyboardIconRenderer.forceLayout
        let current = KeyboardIconRenderer.displayLayout
        let visible = isStatusItemVisible()

        if !visible {
            // Shrink to the next-smaller layout. If we're already at
            // .compact and STILL not visible, keep retrying quietly.
            // SystemUIServer can report a compact status item as
            // hidden during transient relayout even when the menu bar
            // has room; a modal here reads as accusatory and is often
            // simply wrong.
            if let smaller = current.smaller {
                compactHiddenCheckCount = 0
                debugLog("statusItem hidden — shrinking \(current) → \(smaller)")
                KeyboardIconRenderer.displayLayout = smaller
                updateIcon()
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) { [weak self] in
                    self?.adaptLayoutForAvailableSpace()
                }
            } else {
                compactHiddenCheckCount += 1
                if compactHiddenCheckCount == 1 || compactHiddenCheckCount % 10 == 0 {
                    debugLog("statusItem hidden at compact; retrying quietly check=\(compactHiddenCheckCount)")
                }
                updateIcon()
            }
            return
        }

        compactHiddenCheckCount = 0

        // Visible. If we previously shrunk, try to expand back. Set the
        // larger layout, force a layout, then re-check; revert if we
        // lost the slot.
        guard let bigger = current.larger else { return }
        // Respect the ceiling — never expand past forceLayout.
        if let ceiling = ceiling, bigger.adaptiveRank > ceiling.adaptiveRank {
            return
        }
        KeyboardIconRenderer.displayLayout = bigger
        updateIcon()
        // Verify after 0.6s — was 0.25s, but the menubar's slot
        // recompute is async and 0.25s sometimes caught a transient
        // "no screen" state that wasn't real, causing us to revert
        // and feel stuck in the smaller layout. 0.6s gives the OS
        // a beat to settle without making the user wait.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) { [weak self] in
            guard let self = self else { return }
            if !self.isStatusItemVisible() {
                debugLog("expand \(current) → \(bigger) didn't fit; reverting")
                KeyboardIconRenderer.displayLayout = current
                self.updateIcon()
            } else {
                debugLog("statusItem expanded \(current) → \(bigger)")
                self.adaptLayoutForAvailableSpace()
            }
        }
    }

    private func alertNoMenuBarSpace() {
        let alert = NSAlert()
        alert.messageText = L("alert.noMenuBarSpace.title")
        alert.informativeText = L("alert.noMenuBarSpace.body")
        alert.alertStyle = .informational
        alert.addButton(withTitle: L("alert.ok"))
        NSApp.activate(ignoringOtherApps: true)
        alert.runModal()
    }

    func applicationWillTerminate(_ notification: Notification) {
        pianoWaveformWindowDelegate.dismiss(reason: .programmatic)
        menuBand.shutdown()
        // If we crashed (or were killed) mid-pitch-bend, the system
        // cursor stays hidden across sessions because CGDisplayHide/Show
        // are reference-counted globally. Restore here so the user never
        // ends up with an invisible cursor on next launch.
        showSystemCursorIfNeeded()
        pitchBendOverlay?.dismiss()
    }

    /// YWFT Processing descriptors built straight from the .ttf URLs.
    /// Both bundled cuts share the PostScript name "YWFT-Processing",
    /// so any name-based lookup (`NSFont(name:)` or
    /// `NSFontDescriptor(fontAttributes: [.family: ...])` + symbolic
    /// traits) silently returns the wrong cut or falls through to the
    /// system font without nil-ing. Loading the descriptor directly
    /// from the file URL is the only way to be sure a given draw call
    /// gets that exact .ttf.
    static var ywftBoldDescriptor: NSFontDescriptor?
    static var ywftRegularDescriptor: NSFontDescriptor?

    /// Register the YWFT Processing font files we ship in the SPM
    /// bundle so AppKit can find them by PostScript name, AND cache
    /// per-cut descriptors built directly from the .ttf URLs.
    /// Registration alone is unreliable for these specific files
    /// because both cuts share a PostScript name (the original 0.7/0.8
    /// title-rendering bug), so callers should prefer the cached
    /// descriptors. Called once at launch.
    /// Internal (not private) so the headless capture entry points
    /// (PopoverCLI/AboutCLI/JamCLI) can register the bundled YWFT Processing
    /// font too — they build the view tree without going through
    /// applicationDidFinishLaunching, so without this call the instrument
    /// title falls back to system font in App Store screenshots.
    static func registerBundledFonts() {
        let bundle = Bundle.appResources
        for name in ["ywft-processing-regular", "ywft-processing-bold"] {
            guard let url = bundle.url(forResource: name, withExtension: "ttf") else {
                NSLog("MenuBand: bundled font missing — \(name).ttf")
                continue
            }
            var error: Unmanaged<CFError>?
            if !CTFontManagerRegisterFontsForURL(url as CFURL, .process, &error) {
                NSLog("MenuBand: font register failed for \(name): \(error?.takeRetainedValue().localizedDescription ?? "?")")
            }
            guard let descs = CTFontManagerCreateFontDescriptorsFromURL(url as CFURL) as? [NSFontDescriptor],
                  let desc = descs.first else {
                NSLog("MenuBand: no descriptor parsed for \(name).ttf")
                continue
            }
            if name.hasSuffix("bold") {
                ywftBoldDescriptor = desc
            } else {
                ywftRegularDescriptor = desc
            }
        }
        if ywftBoldDescriptor == nil {
            NSLog("MenuBand: ywft-processing-bold descriptor unavailable — title will fall back to system font")
        }
    }

    // MARK: - Icon animation (slide + flash)

    /// Kick off the icon's transient animation. `slide` ∈ {-1, 0, +1}:
    /// −1 scrolls the piano left, +1 right, 0 leaves it alone. `flash`
    /// is the peak brightness boost on the music-note glyph (0…1).
    /// Either parameter can re-trigger an already-running animation
    /// — most recent value wins.
    private func kickIconAnim(slide: Int, flash: CGFloat, limitNudge: Bool = false) {
        let now = CACurrentMediaTime()
        if slide != 0 {
            slideDirection = slide
            slideStartedAt = now
            slideIsLimitNudge = limitNudge
        }
        if flash > 0.001 {
            flashStrength = max(flashStrength, flash)
            flashStartedAt = now
        }
        startIconAnimTimerIfNeeded()
        updateIcon()
    }

    private func startIconAnimTimerIfNeeded() {
        guard iconAnimTimer == nil else { return }
        iconAnimTimer = Timer.scheduledTimer(
            withTimeInterval: 1.0/60.0, repeats: true
        ) { [weak self] _ in
            self?.tickIconAnim()
        }
    }

    private func tickIconAnim() {
        let now = CACurrentMediaTime()
        var slideActive = false
        var flashActive = false
        if slideDirection != 0 {
            if (now - slideStartedAt) >= Self.slideDuration {
                slideDirection = 0
                slideIsLimitNudge = false
            } else {
                slideActive = true
            }
        }
        if flashStrength > 0.01 {
            if (now - flashStartedAt) >= Self.flashDuration {
                flashStrength = 0
            } else {
                flashActive = true
            }
        }
        if !slideActive && !flashActive {
            iconAnimTimer?.invalidate()
            iconAnimTimer = nil
        }
        updateIcon()
    }

    /// Computed slide offset for the current frame. The status item acts as
    /// a fixed viewport over a longer piano strip; changing octaves scrolls
    /// the keys by exactly one octave underneath that mask.
    private func currentSlideOffset() -> CGFloat {
        guard slideDirection != 0 else { return 0 }
        let elapsed = CACurrentMediaTime() - slideStartedAt
        if elapsed >= Self.slideDuration { return 0 }
        let t = elapsed / Self.slideDuration  // 0…1
        let dir = CGFloat(slideDirection)
        if slideIsLimitNudge {
            let phase = CGFloat(t)
            return sin(.pi * phase) * Self.limitNudgeDistance * dir
        }
        let w = KeyboardIconRenderer.octaveSlideDistance
        let phase = CGFloat(t)
        let eased = 1 - pow(1 - phase, 3)
        return (eased - 1) * w * dir
    }

    /// Computed flash strength for the current frame. Linear decay
    /// from the peak value down to 0 over `flashDuration`.
    private func currentFlashStrength() -> CGFloat {
        guard flashStrength > 0.01 else { return 0 }
        let elapsed = CACurrentMediaTime() - flashStartedAt
        if elapsed >= Self.flashDuration { return 0 }
        let t = CGFloat(elapsed / Self.flashDuration)
        return flashStrength * (1 - t)
    }

    func updateIcon() {
        guard let button = statusItem.button else { return }
        // Keep the renderer's drum-zone coloring in sync with the live split.
        KeyboardIconRenderer.percussionLeftActive = menuBand.percussionLeft
        KeyboardIconRenderer.percussionRightActive = menuBand.percussionRight
        // Use *effective* keymap/typeMode so popover hover-preview can
        // override the live state without writing to UserDefaults. The
        // renderer keeps `imageSize` constant across keymaps (always the
        // 2-octave Notepat layout area) — Ableton is drawn with negative
        // space on the right — so the status item slot never resizes and
        // the popover anchor stays put.
        //
        // Ghost-label flash: when the user clicks the menubar piano (or
        // types while armed), letters render on the keys for ~0.5–0.7 s.
        // It's a temporal "you're capturing right now" hint, not a
        // permanent overlay.
        KeyboardIconRenderer.activeKeymap = menuBand.keymap
        // Sync the playing flag so the chip's linger fermata can hide
        // when the user is just resting on shift between phrases.
        KeyboardIconRenderer.playingActive = !menuBand.litNotes.isEmpty
        // Sample-voice recording state — the renderer reads this to
        // tint the chip + VU bars red while the user is holding the
        // record key.
        KeyboardIconRenderer.recordingActive = menuBand.sampleRecordingActive
        // Tape state — refreshed every paint so the cassette REC dot,
        // mic LED, fill bar and playhead track the controller in real
        // time. Position fractions are computed off the tape's own
        // duration accessors; phase is wall-clock so the reels keep
        // turning smoothly between paint ticks.
        let tape = menuBand.tape
        let fillFrac = CGFloat(tape.durationSeconds / MenuBandTape.maxDurationSeconds)
        let playFrac = CGFloat(tape.positionSeconds / max(0.001, tape.durationSeconds))
        KeyboardIconRenderer.tapeRecording = (tape.state == .recording)
        KeyboardIconRenderer.tapePlaying = (tape.state == .playing)
        KeyboardIconRenderer.tapeFillFraction = min(1, max(0, fillFrac))
        KeyboardIconRenderer.tapePlayheadFraction = min(1, max(0, playFrac))
        KeyboardIconRenderer.tapeMicHot =
            (tape.state == .recording) &&
            (MenuBandSampleVoice.micAuthorizationStatus() == .authorized)
        KeyboardIconRenderer.tapePhase = CACurrentMediaTime()
        // Note: miniVisualizerLevel + miniVisualizerPhase are driven by
        // the visualizerAnimTimer (24fps smoothed VU motion), not from
        // here — overriding them on every note-event redraw would
        // erase the smoothing.
        // Voice-subscript override: backends that don't map to a GM
        // program get a glyph instead of a digit. Sample voice → "`"
        // (the record key); KPBJ radio → its CALLSIGN ("kpbj") so the
        // station keeps its identity in the menubar instead of an opaque
        // "−1" slot number. The badge auto-widens to fit (voiceBadgeDigits
        // below), same as 3-digit GM numbers. Everything else falls
        // through to the 1-based GM program number.
        let voiceLabel: String?
        switch menuBand.instrumentBackend {
        case .sample: voiceLabel = "`"
        case .kpbj:   voiceLabel = menuBand.radioStation.label
        default:      voiceLabel = nil
        }
        // Reserve badge width for the actual subscript so 3-digit GM
        // numbers (100–128) don't crop. Must be set BEFORE sizing the slot.
        let badgeText = menuBand.midiMode
            ? "M"
            : (voiceLabel ?? String(Int(menuBand.effectiveMelodicProgram) + 1))
        KeyboardIconRenderer.voiceBadgeDigits = badgeText.count
        statusItem.length = KeyboardIconRenderer.imageSize.width
        button.image = KeyboardIconRenderer.image(
            litNotes: menuBand.litNotes,
            playbackLitNotes: menuBand.playbackLitNotes,
            enabled: menuBand.midiMode,
            typeMode: menuBand.typeMode,
            melodicProgram: menuBand.effectiveMelodicProgram,
            voiceLabel: voiceLabel,
            hovered: hoveredElement,
            letterAlpha: { [weak self] midi in
                self?.letterAlpha(for: midi) ?? 0
            },
            slideOffsetX: currentSlideOffset(),
            settingsFlash: currentFlashStrength()
        )
        let layoutName = menuBand.keymap == .ableton ? "Ableton" : "Notepat"
        let routing = menuBand.audioRoutingContextLabel.map { " - \($0)" } ?? ""
        button.toolTip = "\(menuBand.voiceContextLabel) - \(menuBand.octaveContextLabel) - \(layoutName) layout\(routing)"
        // Force a synchronous redraw — the click drag-loop runs the runloop
        // in `eventTracking` mode and has been swallowing the next CA flush
        // until mouseUp. Without this, key blinks and hover highlights only
        // appeared after the user released the mouse.
        button.needsDisplay = true
        button.displayIfNeeded()
    }

    /// Extend the letter ghost duration and pivot the wave at the given
    /// display note. Each keypress restarts (or extends) the fade-in
    /// from that pivot; when the ghost expires the wave reverses outward
    /// (far cells fade out first). The animation tick drives 60 Hz
    /// redraws while the fade is in progress, then auto-stops.
    /// Schedule / extend the letter ghost. Each press advances the
    /// attack wave (cells become "reached" as the wave passes them)
    /// and resets the release deadline. No timed curves — per-cell
    /// alpha is smoothed toward its target on every tick, so any
    /// pivot change or fresh press while letters are decaying just
    /// nudges the targets and the physics handles the rest. No snaps.
    private func extendGhost(_ duration: CFTimeInterval, pivot: UInt8? = nil) {
        let now = CACurrentMediaTime()
        // Fresh attack: when the previous wave fully decayed back to
        // 0, restart the wave clock so cell-distance delays are
        // measured from the new press.
        let stable = letterAlphas.allSatisfy { $0.value < 0.01 }
        if stable {
            letterAttackStartedAt = now
            letterReached.removeAll()
        }
        if let pivot = pivot { letterPivot = pivot }
        // Hold the ghost open long enough for the wave to fully fill,
        // so distant cells get to 1.0 before any release wave kicks
        // in.
        let maxDist: CFTimeInterval = 24
        let fillDur = maxDist * Self.letterWaveStep + 0.10
        let phaseElapsed = now - letterAttackStartedAt
        let untilFull = max(0, fillDur - phaseElapsed)
        let effective = max(duration, untilFull + 0.05)
        ghostUntil = max(ghostUntil, now + effective)
        startLetterFadeTickIfNeeded()
        updateIcon()
    }

    /// Legacy API — left as a no-op since the physics-based model
    /// has no explicit fade-out phase. The release behavior happens
    /// automatically when `now > ghostUntil` and the per-cell tick
    /// flips targets to 0 with the reverse-distance delay.
    private func startLetterFadeOut() {}

    private func startLetterFadeTickIfNeeded() {
        guard letterFadeTimer == nil else { return }
        letterFadeTimer = Timer.scheduledTimer(withTimeInterval: 1.0/60.0, repeats: true) { [weak self] _ in
            self?.tickLetterFade()
        }
    }

    private func tickLetterFade() {
        let now = CACurrentMediaTime()
        let attackPhase = now - letterAttackStartedAt
        let maxDist: CFTimeInterval = 24
        // Pivot's release start time. Beyond this wallclock, the
        // release wave begins; far cells flip to target=0 first.
        let releaseAnchor = max(ghostUntil,
                                letterAttackStartedAt + maxDist * Self.letterWaveStep)

        // 1. Walk the attack wave forward — any cell whose distance
        //    is now within the wave's reach joins `letterReached`.
        for midi in 0...127 where !letterReached.contains(UInt8(midi)) {
            let dist = CFTimeInterval(abs(Int(midi) - Int(letterPivot)))
            if attackPhase >= dist * Self.letterWaveStep {
                letterReached.insert(UInt8(midi))
            }
        }

        // 2. Compute target alpha per cell, then smooth toward it.
        //    Asymmetric rates: faster attack so playing reads as
        //    immediate, slower release so decay feels like it has
        //    weight. Rates are per-frame (60 Hz tick).
        let attackRate: CGFloat = 0.30
        let decayRate:  CGFloat = 0.045
        let isReleasing = now > ghostUntil
        // Track whether any cell actually moved this tick. `updateIcon` is an
        // expensive CoreGraphics redraw of the whole keyboard glyph; the per-
        // cell arithmetic below is cheap. While the letters are HELD lit
        // (all reached, ghost not yet expired) every cell sits pinned at its
        // target and nothing moves — but the timer must keep ticking to catch
        // the release transition (`now > ghostUntil`). So we keep ticking but
        // only redraw when there's motion. This is what kept the menubar icon
        // pinned at ~50% CPU: 60 Hz identical redraws during a static hold.
        var moved = false
        for midi in 0...127 {
            let key = UInt8(midi)
            let cur = letterAlphas[key] ?? 0
            let target: CGFloat
            if !letterReached.contains(key) {
                target = 0
            } else if !isReleasing {
                target = 1
            } else {
                // Release wave: far cells (from pivot) drop first.
                let dist = CFTimeInterval(abs(Int(midi) - Int(letterPivot)))
                let releaseDelay = (maxDist - dist) * Self.letterWaveStep
                let cellReleaseAt = releaseAnchor + releaseDelay
                target = (now < cellReleaseAt) ? 1 : 0
            }
            let rate: CGFloat = (target > cur) ? attackRate : decayRate
            let next = cur + (target - cur) * rate
            // Skip near-zero noise so we can detect the all-stable
            // state and stop the tick.
            let clamped: CGFloat = abs(next) < 0.001 ? 0 : next
            if abs(clamped - cur) > 0.0005 { moved = true }
            letterAlphas[key] = clamped
        }

        // 3. Park the timer when everything has fully decayed and the
        //    ghost is past — saves CPU while idle.
        let fullyDecayed = letterAlphas.values.allSatisfy { $0 < 0.01 }
        if isReleasing && fullyDecayed {
            letterAlphas.removeAll()
            letterReached.removeAll()
            letterFadeTimer?.invalidate()
            letterFadeTimer = nil
            updateIcon()   // draw the final cleared frame once
            return
        }
        // Only repaint when a cell actually moved — a held, fully-lit ghost
        // costs zero redraws until the release wave starts.
        if moved { updateIcon() }
    }

    /// Per-cell alpha based on radial distance from the pivot. Fade-in
    /// starts at the pivot and ripples outward; fade-out starts at the
    /// outermost cell and retreats toward the pivot.
    private func letterAlpha(for midi: UInt8) -> CGFloat {
        if focusCaptureArmedByShortcut { return 1.0 }
        return letterAlphas[midi] ?? 0
    }

    // MARK: - Hover

    private var lastHoverLogTime: TimeInterval = 0
    private func handleHover(event: NSEvent) {
        let now = CACurrentMediaTime()
        if now - lastHoverLogTime > 1.0 {
            lastHoverLogTime = now
            debugLog("handleHover (1Hz throttle)")
        }
        handleHoverInner(event: event)
    }

    private func handleHoverInner(event: NSEvent) {
        guard let button = statusItem.button else { return }
        let imgSize = KeyboardIconRenderer.imageSize
        let bb = button.bounds
        let xOff = (bb.width - imgSize.width) / 2.0
        let yOff = (bb.height - imgSize.height) / 2.0
        let local = button.convert(event.locationInWindow, from: nil)
        let yLocal = button.isFlipped ? (bb.height - local.y) : local.y
        let pt = NSPoint(x: local.x - xOff, y: yLocal - yOff)
        let result = KeyboardIconRenderer.hit(at: pt)
        if hoveredElement != result {
            hoveredElement = result
            updateIcon()
        }
        // Hover intentionally does NOT reveal the pitch strip - the
        // cursor merely passing over the menubar item should never pop
        // a GUI. The floating piano/waveform panel appears only via an
        // explicit press or the settings popover.
    }

    private func handleHoverExit() {
        if hoveredElement != nil {
            hoveredElement = nil
            updateIcon()
        }
        // Pointer left the menubar item → hide the hover-revealed strip
        // (deferred while the pointer is over the panel itself).
        pianoWaveformWindowDelegate.scheduleHideFromHover()
    }

    // MARK: - Click + drag

    @objc private func statusClicked(_ sender: Any?) {
        guard let button = statusItem.button else { return }
        let event = NSApp.currentEvent
        let isRight = event?.type == .rightMouseDown
        let isCtrl = event?.modifierFlags.contains(.control) ?? false
        debugLog("statusClicked type=\(event?.type.rawValue.description ?? "nil") isRight=\(isRight) isCtrl=\(isCtrl) midiMode=\(menuBand.midiMode)")

        if hoveredElement != nil {
            hoveredElement = nil
            updateIcon()
        }

        if isRight || isCtrl {
            showPopover()
            return
        }
        guard let downEvent = event, downEvent.type == .leftMouseDown else { return }

        let imgSize = KeyboardIconRenderer.imageSize
        let bb = button.bounds
        let xOff = (bb.width - imgSize.width) / 2.0
        let yOff = (bb.height - imgSize.height) / 2.0
        func imagePoint(from windowPoint: NSPoint) -> NSPoint {
            let local = button.convert(windowPoint, from: nil)
            let yLocal = button.isFlipped ? (bb.height - local.y) : local.y
            return NSPoint(x: local.x - xOff, y: yLocal - yOff)
        }

        let initialHitPt = imagePoint(from: downEvent.locationInWindow)
        let initial = KeyboardIconRenderer.hit(at: initialHitPt)
        debugLog("hit pt=(\(initialHitPt.x),\(initialHitPt.y)) -> \(String(describing: initial))")
        let startDisplayNote: UInt8
        switch initial {
        case .openSettings:
            showPopover()
            return
        case .openVisualizer:
            // The mini-visualizer is a child of the music-note chip,
            // not a separate trigger. Clicking it routes to the same
            // popover-open path so the panels only ever open from a
            // single deliberate action — the music-note icon — and
            // never from a stray tap on the LED bars.
            showPopover()
            return
        case .tape:
            handleTapeMouseDown(button: button,
                                downEvent: downEvent,
                                imagePoint: imagePoint)
            return
        case .tapeRew:
            menuBand.rewindTape()
            updateIcon()
            return
        case .tapeStop:
            menuBand.stopTape()  // recording→idle edge pre-warms the MP3 cache
            updateIcon()
            return
        case .tapePlay:
            menuBand.playTape()
            updateIcon()
            return
        case .tapeFfwd:
            menuBand.tapeSeekToEnd()
            updateIcon()
            return
        case .recDot:
            // Far-left red dot: press to start recording a tape (dot becomes
            // a live timer); press again to stop and drop the take onto the
            // Desktop with its tape-art icon.
            if menuBand.tape.state == .recording {
                menuBand.stopTape()
                stopRecTicker()
                dropTapeOnDesktop()
            } else {
                menuBand.toggleTapeRecording()  // handles mic-auth + start
                startRecTicker()
            }
            updateIcon()
            return
        case .tapeRec:
            menuBand.toggleTapeRecording()
            updateIcon()
            return
        case .tapeEject:
            // Drop the finished take onto the Desktop as a shareable MP3
            // (cover art embedded) — the same artifact the cassette
            // drag-out and the REC-dot stop produce. The cassette body
            // still owns the live drag-to-anywhere gesture; this button is
            // the "didn't think to drag it" fallback. Stop any in-flight
            // recording first so the export sees a finalized buffer.
            let stateBefore = menuBand.tape.state
            let hasRec = menuBand.tape.hasRecording
            let dur = menuBand.tape.durationSeconds
            NSLog("MenuBand: eject button pressed — state=\(stateBefore) hasRecording=\(hasRec) duration=\(dur)s")
            if stateBefore == .recording {
                menuBand.stopTape()
                stopRecTicker()
            }
            dropTapeOnDesktop()  // export MP3 + reveal (or beep if empty)
            updateIcon()
            return
        case .note(let n):
            startDisplayNote = n
        case .none:
            return
        }

        let initialPt = imagePoint(from: downEvent.locationInWindow)
        let initialShift = downEvent.modifierFlags.contains(.shift)
            || downEvent.modifierFlags.contains(.capsLock)

        // Percussion split: a click on a right-hand key plays the drum kit
        // instead of a melodic note — same split the keyboard uses. Track
        // whichever voice is live (melodic note vs. drum group) so a drag
        // across keys and the mouse-up release the correct one.
        var currentDisplay: UInt8?
        var currentPlayed: UInt8?
        var currentDrumGroup: UInt64?
        var currentDrumDisplay: UInt8?

        func stopCurrentVoice() {
            if let c = currentPlayed { menuBand.stopTapNote(c); currentPlayed = nil }
            if let g = currentDrumGroup { menuBand.percussionNoteOff(g); currentDrumGroup = nil }
            if let d = currentDrumDisplay { menuBand.drumLitOff(d); currentDrumDisplay = nil }
        }
        func startVoice(_ display: UInt8, at pt: NSPoint, shift: Bool) {
            let (v, p) = NoteExpression.values(for: display, at: pt)
            if menuBand.isPercussionDisplayNote(display) {
                // Shift-click accents the drum (a harder hit).
                currentDrumGroup = menuBand.percussionNoteOn(
                    menuBand.percussionDrum(forDisplayNote: display),
                    velocity: v, pan: p, accent: shift)
                menuBand.drumLitOn(display)
                currentDrumDisplay = display
            } else if let played = playedNote(for: display) {
                menuBand.startTapNote(played, velocity: v, pan: p,
                                      displayNote: display, linger: shift)
                currentPlayed = played
            }
            currentDisplay = display
        }

        startVoice(startDisplayNote, at: initialPt, shift: initialShift)
        // Menubar piano taps play the note ONLY — no floating
        // panel pop-up. Reserve the panel for the popover-paired
        // flow and the explicit LED-chip / shortcut entry points.
        // Arm sandbox-friendly local capture on a real piano click;
        // skip arming when global TYPE mode is already on so the
        // global tap doesn't double-trigger.
        if !menuBand.typeMode {
            localCapture.arm()
            // Refresh the panel's lit state in case it's already
            // visible (popover-paired), but do not call
            // `updatePianoWaveformWindow()` — that path runs
            // `showIfNeeded()` which would auto-open the panel
            // here, which is exactly what the user doesn't want.
            pianoWaveformWindowDelegate.refresh()
        }
        while let next = NSApp.nextEvent(
            matching: [.leftMouseDragged, .leftMouseUp],
            until: .distantFuture,
            inMode: .eventTracking,
            dequeue: true
        ) {
            if next.type == .leftMouseUp {
                stopCurrentVoice()
                break
            }
            let pt = imagePoint(from: next.locationInWindow)
            let hoveredDisplay = KeyboardIconRenderer.noteAt(pt)
            if hoveredDisplay != currentDisplay {
                stopCurrentVoice()
                if let nxtDisplay = hoveredDisplay {
                    // Sample shift+capslock state per-note during the drag
                    // so the user can shift-press, release shift mid-drag,
                    // and still get linger from a latched caps lock.
                    let shiftNow = next.modifierFlags.contains(.shift)
                        || next.modifierFlags.contains(.capsLock)
                    startVoice(nxtDisplay, at: pt, shift: shiftNow)
                } else {
                    currentDisplay = nil
                }
            } else if let c = currentPlayed, let display = currentDisplay {
                let (_, p) = NoteExpression.values(for: display, at: pt)
                menuBand.updateTapPan(c, pan: p)
            }
        }
    }

    private func playedNote(for displayNote: UInt8) -> UInt8? {
        let value = Int(displayNote) + menuBand.octaveShift * 12
        guard value >= 0, value <= 127 else { return nil }
        return UInt8(value)
    }

    // MARK: - Tape mouse routing
    //
    // Mouse-down on the inline cassette enters a small dispatcher that
    // distinguishes between a click (toggle REC) and a drag (eject the
    // current recording onto the desktop). Same mouse-tracking trick as
    // SheetMusicView's PDF pull-out — we run our own modal-ish event
    // loop until either `mouseUp` (click) or a meaningful drag delta
    // arrives (eject).

    /// Start the REC dot's live timer: each tick mirrors the tape's record
    /// state + elapsed time into the renderer and repaints the icon. Stops
    /// itself if the tape leaves the recording state for any reason.
    private func startRecTicker() {
        recTimer?.invalidate()
        recStartTime = 0
        // 20 Hz so the bubble morph + blink read smoothly.
        let t = Timer(timeInterval: 0.05, repeats: true) { [weak self] _ in
            guard let self = self else { return }
            let recording = self.menuBand.tape.state == .recording
            KeyboardIconRenderer.recActive = recording
            if recording {
                if self.recStartTime == 0 { self.recStartTime = CACurrentMediaTime() }
                KeyboardIconRenderer.recElapsed = CACurrentMediaTime() - self.recStartTime
            } else if self.recStartTime != 0 {
                // Recording ended on its own (hit the 90 s ceiling) — tear
                // the ticker down. The recording→idle edge (handled in
                // onChange) warms the MP3 cache for a snappy drag-out.
                self.stopRecTicker()
                return
            }
            self.updateIcon()
        }
        RunLoop.main.add(t, forMode: .common)
        recTimer = t
    }

    private func stopRecTicker() {
        recTimer?.invalidate()
        recTimer = nil
        recStartTime = 0
        KeyboardIconRenderer.recActive = false
        KeyboardIconRenderer.recElapsed = 0
        updateIcon()
    }

    /// Stop-and-drop: export the finished take to a shareable MP3 (cover
    /// art embedded), then move it onto the Desktop and reveal it. Runs the
    /// transcode off the main thread; falls back to the raw WAV if ffmpeg
    /// isn't available or the encode fails.
    private func dropTapeOnDesktop() {
        guard menuBand.tape.hasRecording else {
            NSSound.beep()
            return
        }
        guard let desktop = FileManager.default.urls(
            for: .desktopDirectory, in: .userDomainMask).first else {
            if let f = exportTapeMP3() {
                NSWorkspace.shared.activateFileViewerSelecting([f])
            }
            return
        }
        // Export off the main thread (ffmpeg blocks), reveal on completion.
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            guard let file = self?.exportTapeMP3() else { return }
            DispatchQueue.main.async {
                self?.revealOnDesktop(file, desktop: desktop)
            }
        }
    }

    /// Eject the current take and return a shareable stereo MP3 with the
    /// cassette cover art embedded + stamped as the file icon. Falls back
    /// to the raw 4-channel WAV if ffmpeg is missing or the encode fails.
    /// The transcode result is cached per take, so the three eject paths
    /// (stop-and-drop, EJECT button, cassette drag-out) share one encode.
    /// Safe to call on the main thread (drag-out needs the file
    /// synchronously) or a background queue (stop-and-drop).
    @discardableResult
    private func exportTapeMP3() -> URL? {
        // 1. Main-thread work first, outside the lock: `tape.eject()` renders
        //    the WAV + stamps its icon via NSWorkspace, and we grab that
        //    cover for the encoder. Doing this before locking keeps the lock
        //    free of any main-thread hop (no deadlock when a background
        //    pre-warm holds it while the main thread also wants to export).
        let prep: (wav: URL, cover: NSImage)? = onMain {
            guard let wav = self.menuBand.ejectTape() else { return nil }
            return (wav, NSWorkspace.shared.icon(forFile: wav.path))
        }
        guard let prep = prep else { return nil }

        // 2. Serialize cache lookup + encode. A second caller blocks here
        //    until the first finishes, then hits the cache instead of
        //    launching a duplicate ffmpeg against the same temp file.
        tapeExportLock.lock()
        if let c = cachedTapeMP3, c.wavPath == prep.wav.path,
           FileManager.default.fileExists(atPath: c.mp3.path) {
            tapeExportLock.unlock()
            return c.mp3
        }
        let encoded = transcodeToMP3(wav: prep.wav, cover: prep.cover)
        if let mp3 = encoded { cachedTapeMP3 = (prep.wav.path, mp3) }
        tapeExportLock.unlock()

        // 3. Stamp the cover as the file icon on main, outside the lock.
        guard let mp3 = encoded else {
            return prep.wav  // no encoder / encode failed — the WAV still plays
        }
        onMain { NSWorkspace.shared.setIcon(prep.cover, forFile: mp3.path, options: []) }
        return mp3
    }

    /// Kick off the MP3 transcode for the just-finished take on a
    /// background queue so the cache is warm by the time the user reaches
    /// for drag-out or EJECT — keeps those paths feeling instant. No-op if
    /// there's nothing recorded or the cache is already populated.
    private func prewarmTapeMP3() {
        guard menuBand.tape.hasRecording else { return }
        DispatchQueue.global(qos: .utility).async { [weak self] in
            self?.exportTapeMP3()
        }
    }

    /// Transcode the 4-channel tape WAV to a stereo MP3, embedding `cover`
    /// (the cassette art) as the ID3 attached picture. Pure CPU/IO — no
    /// NSWorkspace, no main-thread hops — so it's safe to call while
    /// holding `tapeExportLock`. The caller stamps the file icon. Returns
    /// nil if no ffmpeg is found or the encode fails.
    private func transcodeToMP3(wav: URL, cover: NSImage) -> URL? {
        guard let ffmpeg = Self.ffmpegPath() else { return nil }
        // Write the cover to a temp PNG for ffmpeg to embed.
        let tmp = FileManager.default.temporaryDirectory
        let coverURL = tmp.appendingPathComponent("ac-tape-cover-\(UUID().uuidString).png")
        let haveCover = writePNG(cover, to: coverURL)
        defer { try? FileManager.default.removeItem(at: coverURL) }
        let mp3 = tmp.appendingPathComponent(
            wav.deletingPathExtension().lastPathComponent + ".mp3")
        try? FileManager.default.removeItem(at: mp3)

        var args = ["-y", "-i", wav.path]
        if haveCover {
            args += ["-i", coverURL.path, "-map", "0:a:0", "-map", "1:v:0",
                     "-c:v", "copy", "-disposition:v:0", "attached_pic",
                     "-metadata:s:v", "title=Album cover",
                     "-metadata:s:v", "comment=Cover (front)"]
        } else {
            args += ["-map", "0:a:0"]
        }
        // Down-mix the 4-channel take to stereo MP3 at 192kbps + ID3v2.
        args += ["-c:a", "libmp3lame", "-b:a", "192k", "-ac", "2",
                 "-id3v2_version", "3", mp3.path]

        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: ffmpeg)
        proc.arguments = args
        proc.standardOutput = nil
        proc.standardError = nil
        do { try proc.run(); proc.waitUntilExit() }
        catch {
            NSLog("MenuBand: ffmpeg launch failed: \(error)")
            return nil
        }
        guard proc.terminationStatus == 0,
              FileManager.default.fileExists(atPath: mp3.path) else {
            NSLog("MenuBand: mp3 encode failed (status \(proc.terminationStatus))")
            return nil
        }
        return mp3
    }

    /// Run `work` on the main thread and return its result, executing
    /// inline if already on main (so callers on either thread are safe and
    /// can't deadlock on `DispatchQueue.main.sync`).
    @discardableResult
    private func onMain<T>(_ work: () -> T) -> T {
        if Thread.isMainThread { return work() }
        return DispatchQueue.main.sync(execute: work)
    }

    /// Locate a usable ffmpeg (Homebrew arm64 / Intel). The launch-agent's
    /// PATH may not include Homebrew, so probe absolute paths.
    private static func ffmpegPath() -> String? {
        for p in ["/opt/homebrew/bin/ffmpeg", "/usr/local/bin/ffmpeg"] {
            if FileManager.default.isExecutableFile(atPath: p) { return p }
        }
        return nil
    }

    /// Write an NSImage to a PNG file (best available representation).
    private func writePNG(_ image: NSImage, to url: URL) -> Bool {
        guard let tiff = image.tiffRepresentation,
              let rep = NSBitmapImageRep(data: tiff),
              let png = rep.representation(using: .png, properties: [:]) else { return false }
        do { try png.write(to: url); return true }
        catch { NSLog("MenuBand: cover PNG write failed: \(error)"); return false }
    }

    /// Move a freshly-made tape onto the Desktop (collision-safe) and reveal
    /// it in Finder; reveal in place if the move fails.
    private func revealOnDesktop(_ src: URL, desktop: URL) {
        let dest = uniqueDestinationOnDesktop(
            in: desktop, preferredName: src.lastPathComponent)
        do {
            try FileManager.default.moveItem(at: src, to: dest)
            NSWorkspace.shared.activateFileViewerSelecting([dest])
        } catch {
            NSLog("MenuBand: tape move failed: \(error)")
            NSWorkspace.shared.activateFileViewerSelecting([src])
        }
    }

    /// Pick a non-colliding filename inside `dir` for the freshly
    /// ejected WAV. macOS' move would fail (or silently overwrite) if
    /// a tape with the same name is already on the Desktop.
    private func uniqueDestinationOnDesktop(in dir: URL,
                                            preferredName: String) -> URL {
        var candidate = dir.appendingPathComponent(preferredName)
        if !FileManager.default.fileExists(atPath: candidate.path) {
            return candidate
        }
        let base = (preferredName as NSString).deletingPathExtension
        let ext  = (preferredName as NSString).pathExtension
        var n = 2
        while true {
            let name = ext.isEmpty
                ? "\(base) \(n)"
                : "\(base) \(n).\(ext)"
            candidate = dir.appendingPathComponent(name)
            if !FileManager.default.fileExists(atPath: candidate.path) {
                return candidate
            }
            n += 1
        }
    }

    private func handleTapeMouseDown(button: NSStatusBarButton,
                                     downEvent: NSEvent,
                                     imagePoint: (NSPoint) -> NSPoint) {
        let startPt = imagePoint(downEvent.locationInWindow)
        while let next = NSApp.nextEvent(
            matching: [.leftMouseDragged, .leftMouseUp],
            until: .distantFuture,
            inMode: .eventTracking,
            dequeue: true
        ) {
            if next.type == .leftMouseUp {
                // Click without drag → no-op. REC moved out to the
                // dedicated red transport button; the cassette body is
                // now a pure drag-source.
                return
            }
            // .leftMouseDragged
            let p = imagePoint(next.locationInWindow)
            if abs(p.x - startPt.x) > 5 || abs(p.y - startPt.y) > 5 {
                if startTapeEjectDrag(button: button, event: next) {
                    return
                }
                // Drag-out failed (no recording, write error). Wait
                // for mouseUp so we don't leave a stuck drag state.
            }
        }
    }

    /// Begin a drag session whose pasteboard item is the freshly
    /// exported MP3. Mirrors `SheetMusicView.beginPDFDrag` — eager
    /// render to a temp file BEFORE calling `beginDraggingSession`,
    /// then animate the cassette "ejecting" downward so the user
    /// sees the artifact leave the menubar. `exportTapeMP3()` is
    /// usually a cache hit (pre-warmed on stop), so the synchronous
    /// call here returns immediately; on a cold cache it transcodes
    /// inline, and the cassette-eject animation masks the brief wait.
    @discardableResult
    private func startTapeEjectDrag(button: NSStatusBarButton,
                                    event: NSEvent) -> Bool {
        guard menuBand.tape.hasRecording else {
            NSLog("MenuBand: tape eject ignored — no recording")
            return false
        }
        // The shareable MP3 (or the raw WAV if ffmpeg is unavailable).
        guard let file = exportTapeMP3() else {
            NSLog("MenuBand: tape eject failed to write file")
            return false
        }
        let dragItem = NSDraggingItem(pasteboardWriter: file as NSURL)
        // Drag preview = the exact icon the export stamped on the
        // file. Reading it back through `NSWorkspace` rather than
        // re-rendering keeps the preview and the on-disk icon
        // identical, so the user sees the same cassette artwork
        // riding the cursor that they'll see in Finder after drop.
        let previewIcon = NSWorkspace.shared.icon(forFile: file.path)
        let previewSize = NSSize(width: 96, height: 96)
        let local = button.convert(event.locationInWindow, from: nil)
        let dragFrame = NSRect(x: local.x - previewSize.width / 2,
                                y: local.y - previewSize.height / 2,
                                width: previewSize.width,
                                height: previewSize.height)
        dragItem.setDraggingFrame(dragFrame, contents: previewIcon)
        let source = TapeDragSource()
        tapeDragSource = source
        button.beginDraggingSession(with: [dragItem], event: event, source: source)
        return true
    }

    // MARK: - Popover

    /// Close the popover and tear down the click-away monitor. Called from
    /// `statusClicked` (settings-chip toggle) and from the click-away
    /// monitor itself when the user clicks anywhere outside our app.
    /// Closes with a short fade-out (~140 ms) so the popover dissolves
    /// the way standard macOS menu pop-ups do, instead of cutting away.
    private func closePopover(dismissFloatingPanel: Bool = true) {
        // Tear down monitors immediately so a stray click during the
        // fade can't re-trigger close.
        if let m = clickAwayMonitor { NSEvent.removeMonitor(m); clickAwayMonitor = nil }
        if let m = popoverEscMonitor { NSEvent.removeMonitor(m); popoverEscMonitor = nil }
        appBeforePopover = nil
        let panelToFade = popoverPanel
        // Capture the CURRENT content view now. The fade's completion
        // runs ~0.14s later, by which point a language-change rebuild
        // may have already swapped `popoverVC` to a freshly-shown VC —
        // dereferencing `popoverVC` in the completion would then yank
        // the NEW popover's view out of its panel and blank it. Pin the
        // old view here so we only detach what we're actually fading.
        let fadingContentView = popoverVC?.view
        // Drop the reference now so isPopoverPanelShown becomes
        // false synchronously — prevents toggle paths from racing
        // with the in-flight fade.
        popoverPanel = nil
        // The popover's touch sensor goes away with it. Clear the
        // resting-finger flag so a held bend doesn't strand once the
        // sensor that was reporting it is gone (the capture-panel sensor
        // takes over again if the user keeps playing with notes held).
        popoverTouchSensor?.removeFromSuperview()
        popoverTouchSensor = nil
        if !menuBand.keyboardNotesHeld { setTrackpadTouchActive(false) }
        // [v1 cutoff] KidLisp TV panel removed — no child panel to tear down.
        if let panel = panelToFade {
            NSAnimationContext.runAnimationGroup({ ctx in
                ctx.duration = 0.14
                ctx.timingFunction = CAMediaTimingFunction(name: .easeOut)
                panel.animator().alphaValue = 0
            }, completionHandler: {
                fadingContentView?.removeFromSuperview()
                panel.orderOut(nil)
                // Reset alpha so the next show isn't invisible.
                panel.alphaValue = 1
            })
        }
        // Floating window normally pairs with the popover — fade
        // alongside. Skip when caller is about to switch the panel
        // into expanded mode (mini-visualizer click), where we want
        // the panel to STAY UP and just transition presentation.
        if dismissFloatingPanel {
            pianoWaveformWindowDelegate.dismiss(reason: .programmatic)
        }
        updatePianoWaveformWindowSuppression()
    }

    /// Distributed-notification entry point for the dev affordance
    /// registered in `applicationDidFinishLaunching`. Always *opens*
    /// the popover (never closes) — repeated triggers from a shell
    /// rebuild loop should leave it visible, not flicker shut.
    @objc private func handleShowPopoverNotification(_ note: Notification) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            if !self.isPopoverPanelShown {
                self.showPopover()
            }
        }
    }

    @objc private func handleToggleChartNotification(_ note: Notification) {
        DispatchQueue.main.async { [weak self] in
            self?.popoverVC?.debugToggleChart()
        }
    }

    /// Remote entry point for opening the About window. Mirrors the
    /// showPopover dev affordance — used by tooling/screenshots to
    /// verify chrome that lives only in the About panel (the
    /// crash-report send button, the language picker, the plugins
    /// chip) without driving the menubar.
    @objc private func handleTapeFeatureToggled(_ note: Notification) {
        let enabled = UserDefaults.standard
            .bool(forKey: KeyboardIconRenderer.tapeFeatureDefaultsKey)
        KeyboardIconRenderer.tapeFeatureEnabled = enabled
        // Status item width changes with the flag — resize the slot
        // before redrawing so the icon doesn't get cropped/scaled.
        statusItem.length = KeyboardIconRenderer.imageSize.width
        updateIcon()
    }

    @objc private func handlePercussionSplitToggled(_ note: Notification) {
        menuBand.reloadPercussionSplit()
    }

    @objc private func handleUseACMIDIToggled(_ note: Notification) {
        menuBand.reloadUseACMIDI()
    }

    @objc private func handleShowKeymapNotification(_ note: Notification) {
        DispatchQueue.main.async { [weak self] in
            self?.pianoWaveformWindowDelegate.showExpandedForPopover()
        }
    }

    /// Remote autoplay. `userInfo` (all string-valued, since it crosses a
    /// process boundary via DistributedNotificationCenter):
    ///   - `program`: GM melodic program 0–127 (default 78 = Whistle).
    ///   - `bpm`:     tempo in beats/min (default 132).
    ///   - `velocity`: 1–127 (default 100).
    ///   - `notes`:   comma-separated `sym:beats` tokens. `sym` is a MIDI
    ///                note ("67"), a drum letter (k/s/h/ho/c/rd/cr — kick,
    ///                snare, closed/open hat, clap, ride, crash), or `r` for a
    ///                rest. e.g. "67:1,72:1,r:1" or "k:1,h:0.5,s:1,h:0.5".
    ///                Beats are fractional-friendly ("60:0.5").
    ///   - `notes2`/`notes3`/`notes4`: optional extra voices played in
    ///                parallel from the same start — one per drum so a kit can
    ///                groove under a melody. Each honors `velocity2`/`3`/`4`.
    ///   - `startEpoch`: optional Unix time (seconds, fractional ok) to begin
    ///                playback at. Lets two NTP-synced machines start on the
    ///                exact same beat for call-and-response across the fleet —
    ///                the sequence waits until that wall-clock instant
    ///                regardless of when the notification arrived. If absent
    ///                or already past, playback starts right away.
    /// Each note rides `startTapNote`/`stopTapNote` so it lights the keys
    /// and feeds the waveform — the machine performs, it doesn't just emit
    /// audio. A bare post (no userInfo) plays the default whistle refrain.
    @objc private func handlePlayNotification(_ note: Notification) {
        playFromInfo(note.userInfo as? [String: String] ?? [:])
    }

    /// Parse a play spec (from a distributed notification or a fleet message)
    /// and schedule it. Keys are documented on handlePlayNotification.
    func playFromInfo(_ info: [String: String]) {
        let program = UInt8(info["program"] ?? "") ?? 78
        let bpm = Double(info["bpm"] ?? "") ?? 132
        let velocity = UInt8(info["velocity"] ?? "") ?? 100
        // Shared start instant (see `startEpoch` above). 0.2s minimum so a
        // local post still gets a beat for the program change to settle.
        var leadIn = 0.2
        if let epoch = Double(info["startEpoch"] ?? "") {
            leadIn = max(0.2, epoch - Date().timeIntervalSince1970)
        }
        let beat = 60.0 / max(1, bpm)
        // Up to four parallel voices share the same start instant: `notes`
        // plus `notes2`/`notes3`/`notes4`. Each can carry its own
        // `velocity`/`velocity2`/… (falling back to the shared velocity).
        // That lets ONE post lay a drum kit — kick, snare, hat on separate
        // tracks — optionally under a melody. A bare post (no `notes`) plays
        // the default whistle refrain.
        var tracks: [(spec: String, vel: UInt8)] = []
        for suffix in ["", "2", "3", "4"] {
            guard let s = info["notes" + suffix] else { continue }
            let v = UInt8(info["velocity" + suffix] ?? "") ?? velocity
            tracks.append((s, v))
        }
        if tracks.isEmpty {
            tracks.append(("67:1,72:1,76:1,79:1,81:2,79:1,76:1,74:1,76:1,79:1,76:1,72:3,r:1",
                           velocity))
        }

        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.menuBand.setMelodicProgram(program)

            for track in tracks {
                var t = leadIn  // wait for the shared start instant (or 0.2s)
                let vel = track.vel
                for token in track.spec.split(separator: ",") {
                    let parts = token.split(separator: ":")
                    guard parts.count == 2, let beats = Double(parts[1]) else { continue }
                    let dur = beats * beat
                    let sym = parts[0]
                    if sym == "r" { t += dur; continue }   // rest
                    let onAt = t
                    if let drum = AppDelegate.drum(for: sym) {
                        // Percussion one-shot on menuband's native drum engine
                        // (GM-independent; kick/snare/hat just hit and ring).
                        DispatchQueue.main.asyncAfter(deadline: .now() + onAt) { [weak self] in
                            _ = self?.menuBand.percussionNoteOn(drum, velocity: vel,
                                                                pan: 64, accent: false)
                        }
                    } else if let midi = UInt8(sym) {
                        // Melodic note via the real tap path (lights keys + waveform).
                        // Hold for most of the slot; a small gap keeps repeated
                        // notes articulated instead of slurring into one.
                        let offAt = t + dur * 0.9
                        DispatchQueue.main.asyncAfter(deadline: .now() + onAt) { [weak self] in
                            self?.menuBand.startTapNote(midi, velocity: vel)
                        }
                        DispatchQueue.main.asyncAfter(deadline: .now() + offAt) { [weak self] in
                            self?.menuBand.stopTapNote(midi)
                        }
                    }
                    t += dur
                }
            }
        }
    }

    /// Map a `play` drum token to a kit voice. `k` kick · `s` snare ·
    /// `h` closed hat · `ho` open hat · `c` clap · `rd` ride · `cr` crash.
    private static func drum(for sym: Substring) -> MenuBandPercussion.Drum? {
        switch sym {
        case "k":  return .kick
        case "s":  return .snare
        case "h":  return .hatClosed
        case "ho": return .hatOpen
        case "c":  return .clap
        case "rd": return .ride
        case "cr": return .crash
        default:   return nil
        }
    }

    /// Single entry point for all `engine.*` verbs. `userInfo` (string-valued):
    ///   start   — `bpm`, `chord` ("60,64,67"), `program`, `arp` ("0,1,2,1"
    ///             chord-tone indices, -1 = rest), `step` (beats/step),
    ///             `drums` ("k,h,s,h,…", empty = rest). Begins the loop and
    ///             holds the chord as a pad.
    ///   chord   — `chord` + `glide` (s): crossfade-morph the held chord; the
    ///             arpeggio follows the new tones automatically.
    ///   pattern — any of `arp`/`drums`/`bpm`/`step`: reshape the loop live
    ///             without stopping the sound.
    ///   stop    — `fade` (s): fade the pad out and halt the loop.
    @objc private func handleEngineNotification(_ note: Notification) {
        let info = note.userInfo as? [String: String] ?? [:]
        let verb = note.name.rawValue.components(separatedBy: ".").last ?? ""
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            switch verb {
            case "start":
                self.engine.start(
                    bpm: Double(info["bpm"] ?? "") ?? 110,
                    chord: AppDelegate.parseBytes(info["chord"]),
                    program: UInt8(info["program"] ?? "") ?? 89,
                    arp: AppDelegate.parseInts(info["arp"]),
                    stepBeats: Double(info["step"] ?? "") ?? 0.5,
                    drums: AppDelegate.parseSteps(info["drums"]))
            case "chord":
                self.engine.morph(toChord: AppDelegate.parseBytes(info["chord"]),
                                  glide: Double(info["glide"] ?? "") ?? 1.5)
            case "pattern":
                if let a = info["arp"]   { self.engine.setArp(AppDelegate.parseInts(a)) }
                if let d = info["drums"] { self.engine.setDrums(AppDelegate.parseSteps(d)) }
                if let b = Double(info["bpm"] ?? "")  { self.engine.setBPM(b) }
                if let s = Double(info["step"] ?? "") { self.engine.setStepBeats(s) }
            case "stop":
                self.engine.stop(fade: Double(info["fade"] ?? "") ?? 0.6)
            case "listen":
                // Mic-driven tempo follow: detect the room's BPM and steer the
                // engine's loop to it. `on=0/false/off` stops listening.
                let off = ["0", "false", "off"].contains((info["on"] ?? "1").lowercased())
                if off {
                    self.micTempo?.stop()
                } else {
                    if self.micTempo == nil {
                        let mt = MenuBandMicTempo()
                        mt.onTempo = { [weak self] bpm in self?.engine.setBPM(bpm) }
                        self.micTempo = mt
                    }
                    // `kick=1`: hit a kick on every detected onset (beat follow),
                    // so the machine locks to the *phase* of the room's beat,
                    // not just its tempo.
                    if ["1", "true"].contains((info["kick"] ?? "").lowercased()) {
                        let v = UInt8(info["kickvel"] ?? "") ?? 122
                        // Kick fires from the phase-locked beat clock (steady +
                        // aligned), not raw onsets (jittery).
                        self.micTempo?.onBeat = { [weak self] in
                            self?.menuBand.engineDrum(.kick, velocity: v)
                        }
                    } else {
                        self.micTempo?.onBeat = nil
                    }
                    self.micTempo?.start()
                }
            default:
                break
            }
        }
    }

    /// Speak text via AVSpeechSynthesizer. userInfo: `text` (required),
    /// `voice` (a voice name like "Daniel"/"Samantha", an AVSpeech identifier,
    /// or a BCP-47 language), `rate` (0–1, default ~0.5), `pitch` (0.5–2),
    /// `volume` (0–1), `startEpoch` (UTC instant to speak at, for synced
    /// cross-machine dialogue/count-ins).
    @objc private func handleSayNotification(_ note: Notification) {
        let info = note.userInfo as? [String: String] ?? [:]
        guard let text = info["text"], !text.isEmpty else { return }
        let rate = Float(info["rate"] ?? "") ?? AVSpeechUtteranceDefaultSpeechRate
        let pitch = Float(info["pitch"] ?? "") ?? 1.0
        let volume = Float(info["volume"] ?? "") ?? 1.0
        let voiceName = info["voice"]
        var leadIn = 0.0
        if let epoch = Double(info["startEpoch"] ?? "") {
            leadIn = max(0, epoch - Date().timeIntervalSince1970)
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + leadIn) { [weak self] in
            let u = AVSpeechUtterance(string: text)
            u.rate = rate
            u.pitchMultiplier = max(0.5, min(2.0, pitch))
            u.volume = max(0, min(1, volume))
            if let name = voiceName, let v = AppDelegate.speechVoice(named: name) {
                u.voice = v
            }
            self?.speechSynth.speak(u)
        }
    }

    /// Conductor trigger (local): play `notesSelf` here and relay `notesPeer`
    /// to the connected Multipeer peer, both at one shared instant (peer's
    /// clock = mine + the measured offset). userInfo: program/bpm/velocity/lead
    /// + notesSelf + notesPeer. No ssh, no LAN — the relay rides Bluetooth/AWDL.
    @objc private func handleFleetPlayNotification(_ note: Notification) {
        let info = note.userInfo as? [String: String] ?? [:]
        let lead = Double(info["lead"] ?? "") ?? 1.2
        let startLocal = Date().timeIntervalSince1970 + lead
        var mine = info
        mine["notes"] = info["notesSelf"] ?? ""
        mine["startEpoch"] = String(format: "%.3f", startLocal)
        mine.removeValue(forKey: "notesSelf")
        mine.removeValue(forKey: "notesPeer")
        playFromInfo(mine)
        if let off = fleet.firstPeerOffset {
            fleet.send([
                "t": "play",
                "program": info["program"] ?? "78",
                "bpm": info["bpm"] ?? "132",
                "velocity": info["velocity"] ?? "100",
                "notes": info["notesPeer"] ?? "",
                "startEpoch": String(format: "%.3f", startLocal + off),
            ])
        }
    }

    /// Speak the fleet connection state — audible test feedback over ssh.
    @objc private func handleFleetStatusNotification(_ note: Notification) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            let peers = self.fleet.connectedPeers.map { $0.displayName }
            let txt: String
            if peers.isEmpty {
                txt = "No peers connected yet."
            } else {
                let off = Int((self.fleet.firstPeerOffset ?? 0) * 1000)
                txt = "Connected to \(peers.joined(separator: ", ")). Offset \(off) milliseconds."
            }
            self.speechSynth.speak(AVSpeechUtterance(string: txt))
        }
    }

    /// Resolve a `say` voice: exact AVSpeech identifier, then voice name
    /// (case-insensitive, exact then contains), then a BCP-47 language.
    private static func speechVoice(named s: String) -> AVSpeechSynthesisVoice? {
        let all = AVSpeechSynthesisVoice.speechVoices()
        if let v = all.first(where: { $0.identifier == s }) { return v }
        let lower = s.lowercased()
        if let v = all.first(where: { $0.name.lowercased() == lower }) { return v }
        if let v = all.first(where: { $0.name.lowercased().contains(lower) }) { return v }
        return AVSpeechSynthesisVoice(language: s)
    }

    private static func parseBytes(_ s: String?) -> [UInt8] {
        (s ?? "").split(separator: ",").compactMap {
            UInt8($0.trimmingCharacters(in: .whitespaces))
        }
    }
    private static func parseInts(_ s: String?) -> [Int] {
        (s ?? "").split(separator: ",").compactMap {
            Int($0.trimmingCharacters(in: .whitespaces))
        }
    }
    /// Comma-split keeping empties so a `drums` pattern preserves its rests.
    private static func parseSteps(_ s: String?) -> [String] {
        guard let s = s, !s.isEmpty else { return [] }
        return s.split(separator: ",", omittingEmptySubsequences: false)
            .map { $0.trimmingCharacters(in: .whitespaces) }
    }

    @objc private func handleShowAboutNotification(_ note: Notification) {
        debugLog("handleShowAboutNotification received")
        DispatchQueue.main.async { [weak self] in
            guard let self = self else {
                debugLog("handleShowAboutNotification: self gone")
                return
            }
            guard let vc = self.popoverVC else {
                debugLog("handleShowAboutNotification: popoverVC nil")
                return
            }
            // showAboutPanel rebuilds the AboutWindowController each
            // call, so repeated triggers are safe — they swap a fresh
            // window in and toss the previous one.
            debugLog("handleShowAboutNotification: presenting About")
            vc.showAboutPanel(nil)
        }
    }

    private func showPopover() {
        debugLog("showPopover entry; isPopoverPanelShown=\(isPopoverPanelShown)")
        // Safety: clear any stranded pitch-bend cursor-hide. CGDisplayHide/
        // Show are reference-counted and an interrupted bend (focus lost
        // mid-gesture) can leak a hide that leaves the cursor invisible —
        // restore it whenever the popover opens so the pointer is always
        // present over the popover.
        showSystemCursorIfNeeded()
        guard let button = statusItem.button,
              let buttonWindow = button.window else {
            debugLog("showPopover: no button/window — bail")
            return
        }
        // If we slept since the last open, the cached theme may be
        // stale — re-evaluate against the live system appearance
        // and trigger a retint pass before the popover is shown so
        // the colors are fresh on this open.
        let nowDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        if nowDark != lastObservedDarkMode {
            lastObservedDarkMode = nowDark
            systemAppearanceChanged()
        }
        popoverVC?.syncFromController()
        if isPopoverPanelShown {
            closePopover()
        } else {
            guard let vc = popoverVC else { return }
            // Force the VC's view to lay out so we have its real
            // preferredContentSize before stuffing it into the panel.
            _ = vc.view
            vc.view.layoutSubtreeIfNeeded()
            let contentSize = vc.preferredContentSize.width > 0
                ? vc.preferredContentSize
                : vc.view.fittingSize
            // Hand the floating-panel delegate the popover's width
            // so its predicted (pre-show) anchor lines up with the
            // live frame — keeps the panel from hopping the first
            // time the popover opens.
            pianoWaveformWindowDelegate.popoverPreferredWidth = contentSize.width

            let panel = MenuBandPopoverPanel(
                content: vc.view,
                contentSize: contentSize)

            // When the popover is key (quiet-focus + popover open), it — not
            // the invisible capture panel — sees key-equivalents. Route ⌘/⌥+
            // letter chords through the same local note path so they chord
            // consistently and shadow Cut/Copy/etc. for mapped note keys
            // (⌘ = major, ⌥ = minor, ⌘+⌥ = sus).
            panel.keyEquivalentHandler = { [weak self] event in
                guard let self = self else { return false }
                let flags = event.modifierFlags
                guard flags.contains(.command) || flags.contains(.control)
                    || flags.contains(.option) else { return false }
                return self.menuBand.handleLocalKey(
                    keyCode: event.keyCode, isDown: true,
                    isRepeat: event.isARepeat, flags: flags)
            }

            // Grow / shrink the whole popover when the notation staff
            // slides in or out (metronome on/off), keeping the top edge
            // pinned under the menubar.
            vc.onRequestResize = { [weak panel] size in
                // Snap (not animate) the panel to its new height: the top
                // edge stays pinned under the menubar and the BOTTOM drops
                // to reserve the picker's space instantly, so the content
                // above never reflow-bounces while a frame animation catches
                // up. The picker fills the new bottom space in place.
                panel?.resizeContent(to: size, animated: false)
            }

            // Show the floating piano FIRST so its frame is known and
            // we can flush the popover's left edge against the
            // floating panel's right edge.
            pianoWaveformWindowDelegate.showCollapsedForPopover()
            updatePianoWaveformWindowSuppression()

            // Compute screen positions:
            //  • leftScreenX  = right edge of the floating piano panel
            //                  (so the popover sits flush against it)
            //  • topScreenY   = bottom of the menubar
            //  • arrowScreenX = horizontal center of the gear/note icon
            let imgSize = KeyboardIconRenderer.imageSize
            let bb = button.bounds
            let xOff = (bb.width - imgSize.width) / 2.0
            let latch = KeyboardIconRenderer.settingsRectPublic
            // latch is in base coords; the displayed icon is magnified, so
            // scale the gear center to land the popover arrow correctly.
            let gearLocal = NSPoint(x: xOff + latch.midX * KeyboardIconRenderer.iconScale, y: 0)
            let gearWindow = button.convert(gearLocal, to: nil)
            let gearScreen = buttonWindow.convertPoint(toScreen: gearWindow)
            let buttonScreenFrame = buttonWindow.convertToScreen(button.frame)
            let topScreenY = buttonScreenFrame.minY

            // Center the popover under the gear icon — the arrow tip
            // lands at the popover's horizontal midpoint and the
            // popover extends evenly to either side. Pairs with
            // the floating panel's snug-left placement so the icon
            // sits right at the seam between panel and popover.
            let leftScreenX: CGFloat = gearScreen.x - contentSize.width / 2

            panel.position(
                leftScreenX: leftScreenX,
                topScreenY: topScreenY,
                arrowScreenX: gearScreen.x)

            // Pair-and-show: panel goes up first, then we record the
            // ownership reference + arm focus/click-away monitors.
            let frontmost = NSWorkspace.shared.frontmostApplication
            appBeforePopover = frontmost?.bundleIdentifier == Bundle.main.bundleIdentifier
                ? nil
                : frontmost
            NSApp.activate(ignoringOtherApps: true)
            panel.makeKeyAndOrderFront(nil)
            popoverPanel = panel
            debugLog("popover panel ordered front; visible=\(panel.isVisible) frame=\(panel.frame)")

            // Now that the popover's live frame is available
            // (popoverFrameProvider can read popoverPanel.frame),
            // re-anchor the floating piano panel so it lands at
            // the right spot on first show. Without this nudge the
            // panel uses the *predicted* popover frame — which is
            // close but not exact — and only snaps to the live
            // position on the first refresh trigger (e.g. a
            // keypress). User noticed: "when I press a keyboard
            // key the liquid popover is in the right place" —
            // fixing the cause here.
            pianoWaveformWindowDelegate.refresh()

            // [v1 cutoff] KidLisp TV panel removed — the popover no longer
            // spawns a child LCD screen / `$` piece chooser. The shared
            // KidLispState + floating aesthetic.computer web window (opened
            // from About) survive independently.

            // Arm local key capture so arrow keys + spacebar reach
            // our handler while the popover is up. The InstrumentList
            // used to be in the popover and grabbed keys via its
            // first-responder; once it moved to the floating panel
            // nothing was capturing arrows for the controller.
            if !localCapture.isArmed { localCapture.arm() }
            DispatchQueue.main.async { [weak panel] in
                panel?.makeKey()
            }

            // Embed a trackpad-touch sensor in the popover (the now-key
            // window) so resting fingers are detected here and the
            // trackpad pitch-bend gesture engages while the popover is
            // open. Made first responder so indirect touches route to it;
            // it never steals the mouse (hitTest → nil), so the keys,
            // buttons, and chooser grid underneath stay fully clickable.
            let sensor = TouchSensorView(frame: panel.chrome.bounds)
            sensor.autoresizingMask = [.width, .height]
            sensor.onActiveChanged = { [weak self] active in
                self?.setTrackpadTouchActive(active)
            }
            panel.chrome.addSubview(sensor)
            panel.makeFirstResponder(sensor)
            popoverTouchSensor = sensor
            // Click-away monitor: clicks on OTHER apps close the popover.
            // In-app clicks (status item button, the popover itself) don't
            // fire global monitors and therefore don't dismiss — that's how
            // we keep the popover open while the user taps menubar piano
            // keys.
            if clickAwayMonitor == nil {
                clickAwayMonitor = NSEvent.addGlobalMonitorForEvents(
                    matching: [.leftMouseDown, .rightMouseDown]
                ) { [weak self] _ in
                    self?.closePopover()
                }
            }
            // Esc closes the popover when it has key focus.
            if popoverEscMonitor == nil {
                popoverEscMonitor = NSEvent.addLocalMonitorForEvents(
                    matching: [.keyDown]
                ) { [weak self] event in
                    if event.keyCode == 53 /* kVK_Escape */ {
                        self?.closePopover()
                        return nil
                    }
                    return event
                }
            }
        }
    }

    // MARK: - Menubar waveform strip

    // MARK: - Octave swipe

    /// Two-finger trackpad scroll → octave step, dispatched
    /// app-wide via local NSEvent monitor. Filters to scroll
    /// events whose window is one of ours (menubar status item,
    /// popover panel, floating piano panel). Returns nil to
    /// consume the event when it's been claimed for octave
    /// stepping; returns the event unchanged otherwise so
    /// scrollable views (instrument list, etc) still get their
    /// natural scroll.
    private func handleOctaveScroll(event: NSEvent) -> NSEvent? {
        guard eventIsOverOurChrome(event) else { return event }
        // Ignore the post-fling momentum decay so a single flick
        // doesn't keep stepping past the user's intended landing
        // octave.
        if event.momentumPhase != [] { return event }
        if event.phase == .began { octaveScrollAccum = 0 }
        let dx = event.scrollingDeltaX
        let dy = event.scrollingDeltaY
        let delta = abs(dy) >= abs(dx) ? dy : dx
        if delta == 0 { return event }
        octaveScrollAccum += delta
        var stepped = false
        // Direction is FLIPPED from the raw scroll delta: a natural-scroll
        // swipe UP should raise the octave, but scrollingDeltaY is positive
        // when content moves up. Each real step (octave actually changed —
        // not clamped at ±4) fires a wood-block tick so the swipe has detents.
        while octaveScrollAccum >= Self.octaveScrollPxPerStep {
            let before = menuBand.octaveShift
            menuBand.stepOctave(delta: -1)
            if menuBand.octaveShift != before { menuBand.playOctaveTick(up: false) }
            octaveScrollAccum -= Self.octaveScrollPxPerStep
            stepped = true
        }
        while octaveScrollAccum <= -Self.octaveScrollPxPerStep {
            let before = menuBand.octaveShift
            menuBand.stepOctave(delta: 1)
            if menuBand.octaveShift != before { menuBand.playOctaveTick(up: true) }
            octaveScrollAccum += Self.octaveScrollPxPerStep
            stepped = true
        }
        // Consume the event so AppKit doesn't double-handle it
        // (e.g. the instrument list trying to scroll inside the
        // popover during a swipe meant for octave shifting).
        _ = stepped
        return nil
    }

    /// True when the scroll event originated over the menubar
    /// status item only. Popover + floating piano panel are
    /// explicitly EXCLUDED so their internal scroll regions
    /// (instrument list, sheet music PDF, etc) keep their own
    /// scroll behavior — octave swipe is reserved for the
    /// menubar piano icon itself.
    private func eventIsOverOurChrome(_ event: NSEvent) -> Bool {
        guard let window = event.window else { return false }
        return window === statusItem.button?.window
    }

    // MARK: - Pitch-bend gesture

    /// Central sink for "is a finger resting on the trackpad right now."
    /// Fed by BOTH the off-screen key-capture sensor panel (active when
    /// no other window is key — e.g. playing with the popover closed)
    /// AND the in-popover sensor (active while the popover is the key
    /// window). Either source flipping the flag lets the trackpad
    /// pitch-bend gesture engage; whichever currently owns the key
    /// window's responder chain is the one delivering indirect touches.
    func setTrackpadTouchActive(_ active: Bool) {
        trackpadTouchActive = active
        guard !active else { return }
        // Last finger lifted off the trackpad. If no notes are held
        // either, this IS the end of the gesture — close out the cursor
        // lock and let the fx ramp naturally. If notes ARE still held,
        // the user is mid-phrase and just resting their finger; leave
        // bend/space/echo where they set them so pitch + ambience don't
        // drift on their own. The next finger touch (or note release)
        // is the right time to change them.
        if !menuBand.keyboardNotesHeld {
            endPitchBendSession()
        }
    }

    private func handlePitchBendCursorMove(event: NSEvent) {
        // Single-finger trackpad gesture: normally active while the
        // user is holding a KEYBOARD note. Mouse-tapped piano notes
        // don't engage pitch-bend so the user can drag across menubar
        // piano keys with the mouse normally. Shift held lets the
        // wheel engage even with no key physically down — so you can
        // grab still-ringing lingering notes and warp the whole sound.
        // Shift also broadcasts the bend to ALL playing channels
        // (see setBend allChannels).
        //
        // Additionally, while the chart overlay is still visible
        // (post-release spring-back, fx-hold), a trackpad-driven
        // mouseMoved re-engages the slide without requiring a
        // keyboard note. We gate on trackpadTouchActive (NSTouch
        // begun) so a passing MOUSE move can't reactivate the bend
        // — only an actual finger on the trackpad.
        let shift = event.modifierFlags.contains(.shift)
        // Engage ONLY while the user is actively holding a KEYBOARD note
        // (or Shift, for warping still-ringing tails). This is the gesture:
        // hold a letter key → the cursor locks (see onLitChanged) → move
        // the pointer to bend → release the key → bend ends + cursor
        // unlocks. Gating on `keyboardNotesHeld` (NOT on a latched flag
        // that lingers after release) is what keeps the ORIGINAL bug fixed:
        // clicking the menubar piano keys with the MOUSE plays via the tap
        // channels — never `keyboardNotesHeld` — so a plain mouse move can
        // never hide the cursor or bend. (An earlier attempt gated on an
        // NSTouch `trackpadTouchActive` sensor, but indirect-touch delivery
        // proved unreliable through the popover's key window, so the bend
        // never engaged at all — confirmed in the field with touch=false on
        // every move while keyHeld=true.)
        // While the pitch graph is still up after a release, a short "catch"
        // window lets trackpad movement re-engage the bend with no key held
        // — so a quick release-then-swipe grabs the graph instead of the
        // gesture dropping. Gated on `pitchBendCursorPushed` (the overlay is
        // genuinely open) so a plain mouse move on the menubar piano — which
        // never opens the graph — still can't reactivate the bend.
        let inReleaseGrace = pitchBendCursorPushed
            && (pitchBendReleaseGraceUntil.map { $0.timeIntervalSinceNow > 0 } ?? false)
        // While the spacebar tape is reverse-playing, a mouse move bends + echoes
        // the playback itself (the fx route onto the rewind voice's own inserts),
        // so engage the gesture even with no key held.
        guard menuBand.keyboardNotesHeld || shift || inReleaseGrace
                || menuBand.isRewinding else { return }
        let dy = Float(event.deltaY)
        let dx = Float(event.deltaX)
        guard dy != 0 || dx != 0 else { return }
        // Keep the catch window alive while the finger is still moving in the
        // post-release grace (no key, no Shift). Stopping past the window
        // lets `armPitchBendGraceCheck` tear the session down.
        if inReleaseGrace && !menuBand.keyboardNotesHeld && !shift {
            pitchBendReleaseGraceUntil = Date().addingTimeInterval(Self.pitchBendReleaseGrace)
        }
        // Negate so swipe UP on trackpad → pitch UP (NSEvent.deltaY
        // is positive when the cursor moves DOWN on screen).
        let bendDelta = -dy * Self.bendSensitivityPerPoint
        // Horizontal is a single BIPOLAR fx axis: center = 0 = no fx,
        // right = echo (trailing delay), left = space (reverb).
        // One swipe direction picks one effect; center kills both.
        // The puck's X position visualises this signed value directly.
        // ⌥Option lets you fine-tune the same axis at lower
        // sensitivity for precise hold-and-tweak.
        let xSens: Float = event.modifierFlags.contains(.option)
            ? Self.echoSensitivityPerPoint * Float(0.3)
            : Self.echoSensitivityPerPoint
        // [temp] When echo is disabled, cap the axis at 0 so a RIGHT
        // swipe can't wind up any echo (and the puck won't drift into a
        // dead right half); the LEFT/space half still runs normally.
        let fxMax: Float = Self.fxEchoEnabled ? 1 : 0
        fxX = max(Float(-1), min(fxMax, fxX + dx * xSens))
        echoAmount = Self.fxEchoEnabled ? max(Float(0), fxX) : 0
        spaceAmount = max(Float(0), -fxX)
        cancelFxRelease()
        // Clamp the accumulator to ±bendRange so it can't wind up past the
        // edge: at the top/bottom, the instant you move the other way it
        // pulls straight back (no dead travel unwinding old overshoot). The
        // overlay puck normalizes against the same range, so the grid edge
        // IS the cap. ±bendRange = two octaves down / up.
        //
        // Accumulate onto the TARGET, not the applied value, and let the
        // ease timer slew `bendAmount` toward it (see `bendGestureTarget`).
        // When the easer isn't already running we're (re)starting a slide —
        // sync the target to the value currently sounding so the accumulator
        // continues from there (handles a fresh grab AND a re-grab during the
        // post-release spring-back, where `cancelFxRelease` above just froze
        // `bendAmount` mid-glide).
        if bendEaseTimer == nil { bendGestureTarget = bendAmount }
        bendGestureTarget = max(-Self.bendRange,
                                min(Self.bendRange, bendGestureTarget + bendDelta))
        bendEaseAllChannels = shift
        startBendEase()
        menuBand.setSpace(amount: spaceAmount)
        menuBand.setEcho(amount: echoAmount)
        pushStaffPitchShift()
        if !pitchBendCursorPushed {
            // Hide the real system cursor and show the floating
            // overlay wheel — the overlay paints over every app, so
            // there's nothing for other apps' cursorUpdate handlers
            // to flicker against. NSCursor.push of the neutral
            // cursor is kept as a fallback for any in-app surface
            // that does its own NSCursor stack manipulation.
            PitchBendCursor.neutral.push()
            pitchBendCursorPushed = true
            hideSystemCursorIfNeeded()
            showPitchBendOverlay()
        }
        updatePitchBendOverlayImage()
        // No idle timeout: a finger resting still on the trackpad
        // emits no .mouseMoved deltas, so any silence-based timer
        // would release the fx while the user is deliberately
        // holding them. The post-release sequence is driven solely
        // by keyboard note release (see onLitChanged →
        // startFxRelease), so the fx hold wherever they're left
        // until the note lifts.
        debugLog("bend cursor dy=\(dy) lit=\(menuBand.litNotes.count) amt=\(bendAmount)")
    }

    /// [v1 cutoff] Previously forwarded the effective pitch shift
    /// (octave + bend) into the popover's staff so it slid up/down.
    /// The staff was removed for v1; kept as a no-op so the bend /
    /// octave call sites stay intact for the post-v1 single-column
    /// pitch indicator.
    private func pushStaffPitchShift() {}

    /// 60Hz timer used to overwrite the floating panel's
    /// `cursorUpdate`-driven cursor sets while pitch-bend is locked.
    /// Without this the bend wheel visibly flickers because
    /// AppKit's cursor stack races our explicit `set()` calls.
    private var pitchBendCursorPinTimer: Timer?

    private func startPitchBendCursorPin() {
        pitchBendCursorPinTimer?.invalidate()
        let timer = Timer(timeInterval: 1.0 / 60.0,
                          repeats: true) { [weak self] _ in
            guard let self = self,
                  self.pitchBendCursorLocked,
                  self.pitchBendCursorPushed else { return }
            PitchBendCursor.cursor(forBend: self.bendAmount,
                                   echo: self.echoAmount).set()
        }
        timer.tolerance = 1.0 / 120.0
        RunLoop.main.add(timer, forMode: .common)
        pitchBendCursorPinTimer = timer
    }

    private func stopPitchBendCursorPin() {
        pitchBendCursorPinTimer?.invalidate()
        pitchBendCursorPinTimer = nil
    }

    private func hideSystemCursorIfNeeded() {
        guard !pitchBendSystemCursorHidden else { return }
        CGDisplayHideCursor(CGMainDisplayID())
        pitchBendSystemCursorHidden = true
    }

    private func showSystemCursorIfNeeded() {
        guard pitchBendSystemCursorHidden else { return }
        CGDisplayShowCursor(CGMainDisplayID())
        pitchBendSystemCursorHidden = false
    }

    private func ensurePitchBendOverlay() -> PitchBendCursorOverlayWindow {
        if let existing = pitchBendOverlay { return existing }
        let overlay = PitchBendCursorOverlayWindow()
        pitchBendOverlay = overlay
        return overlay
    }

    /// XY-pad image for the floating overlay. The chart is a
    /// frozen modulation pad at the lock point; the puck inside
    /// rides up/down with bend and right with echo. Both axes
    /// read at once and the chart never moves, so the user has
    /// a stable reference frame for the whole gesture and the
    /// post-release spring-back.
    private func currentFxCursorImage() -> NSImage {
        // Pass the bipolar fxX so the puck slides both sides of
        // center — positive (right) is echo, negative (left) is the
        // closer/tinier proximity filter. Normalize bend by bendRange so
        // the puck reaches the grid edge exactly at the ±bendRange cap
        // (PitchBendCursor clamps the normalized value to ±1 internally).
        PitchBendCursor.image(forBend: bendAmount / Self.bendRange, echo: fxX,
                              keyDown: menuBand.keyboardNotesHeld)
    }

    private func showPitchBendOverlay() {
        let overlay = ensurePitchBendOverlay()
        overlay.show(image: currentFxCursorImage(),
                     atScreenPoint: safePitchBendAnchor(for: pitchBendLockScreenPoint))
    }

    /// The XY-pad chart normally anchors directly under the frozen
    /// cursor. But if that point is up in the system menu bar — or the
    /// chart (centered on the point) would poke up across the menu bar
    /// and over the Menu Band keys — drop it just below the menu bar
    /// while staying under the cursor's horizontal position, so the grid
    /// stays local to the mouse without crossing the menu bar or
    /// overlapping the keys. The bend gesture reads relative trackpad
    /// motion, not the chart position, so this is purely visual.
    private func safePitchBendAnchor(for point: NSPoint) -> NSPoint {
        let screen = NSScreen.screens.first { NSMouseInRect(point, $0.frame, false) }
            ?? NSScreen.main
            ?? NSScreen.screens.first
        guard let screen else { return point }
        let visible = screen.visibleFrame
        // Chart top edge in bottom-left-origin coords = point.y + half.
        // The menu bar occupies the band above `visible.maxY`.
        let halfHeight = PitchBendCursor.cursorSize.height / 2
        let halfWidth = PitchBendCursor.cursorSize.width / 2
        if point.y + halfHeight > visible.maxY {
            // Tuck the chart just under the menu bar, keeping it under
            // the cursor's x (clamped so it stays fully on screen).
            let gap: CGFloat = 8
            let y = visible.maxY - halfHeight - gap
            let x = min(max(point.x, visible.minX + halfWidth),
                        visible.maxX - halfWidth)
            return NSPoint(x: x, y: y)
        }
        return point
    }

    private func updatePitchBendOverlayImage() {
        guard let overlay = pitchBendOverlay, overlay.isVisible else { return }
        // Chart is frozen at the lock point; only the puck inside
        // moves, so a single image swap each tick is enough.
        overlay.update(image: currentFxCursorImage())
    }

    /// Cancel any in-flight post-release sequence (hold OR ramp),
    /// freezing the fx wherever they currently sit. Called both when
    /// the gesture resumes and when a fresh note is replayed.
    private func cancelFxRelease() {
        fxHoldTimer?.invalidate()
        fxHoldTimer = nil
        fxRampTimer?.invalidate()
        fxRampTimer = nil
        fxRampStart = nil
    }

    /// Slew `bendAmount` toward `bendGestureTarget` at `bendSlewPerSecond`,
    /// pushing each small step to every backend so the pitch slides instead
    /// of jumping (see `bendGestureTarget` for why). Idempotent — a running
    /// easer just keeps tracking the moving target; it self-cancels once it
    /// reaches the target.
    private func startBendEase() {
        guard bendEaseTimer == nil else { return }
        let dt = 1.0 / Self.bendEaseHz
        let maxStep = Self.bendSlewPerSecond * Float(dt)
        let timer = Timer(timeInterval: dt, repeats: true) { [weak self] t in
            guard let self = self else { t.invalidate(); return }
            let diff = self.bendGestureTarget - self.bendAmount
            if abs(diff) <= maxStep {
                self.bendAmount = self.bendGestureTarget
            } else {
                self.bendAmount += diff > 0 ? maxStep : -maxStep
            }
            self.menuBand.setBend(amount: self.bendAmount,
                                  allChannels: self.bendEaseAllChannels)
            self.updatePitchBendOverlayImage()
            if self.bendAmount == self.bendGestureTarget {
                t.invalidate()
                self.bendEaseTimer = nil
            }
        }
        RunLoop.main.add(timer, forMode: .common)
        bendEaseTimer = timer
    }

    /// Stop the bend easer and snap the target to wherever the applied bend
    /// currently sits — used when the post-release ramp takes over driving
    /// `bendAmount` so the two don't fight over it.
    private func stopBendEase() {
        bendEaseTimer?.invalidate()
        bendEaseTimer = nil
        bendGestureTarget = bendAmount
    }

    /// Write "<seq> <noteName>" to the desktop-badge note signal file so the
    /// blueberry sticker opens its mouth + floats a note. Fire-and-forget on a
    /// utility queue; silently does nothing on machines without the badge dir.
    private func emitBadgeNote(_ name: String) {
        badgeNoteSeq &+= 1
        let seq = badgeNoteSeq
        let dir = NSString(string: "~/.local/share/desktop-badge").expandingTildeInPath
        DispatchQueue.global(qos: .utility).async {
            var isDir: ObjCBool = false
            guard FileManager.default.fileExists(atPath: dir, isDirectory: &isDir),
                  isDir.boolValue else { return }
            try? "\(seq) \(name)".write(toFile: dir + "/note",
                                        atomically: true, encoding: .utf8)
        }
    }

    /// Tear down the pitch-bend cursor lock + floating overlay and
    /// start the post-release hold→linear-ramp on the fx. Shared by
    /// the two end-of-gesture triggers: all keyboard notes released
    /// with no finger on the trackpad, or the trackpad finger
    /// lifting. Idempotent.
    /// Arm (or re-arm) the short grace timer that tears the pitch-bend
    /// graphic down once the last keyboard note has lifted. Cancelled if a
    /// note comes back (see onLitChanged). Shift is the deliberate
    /// exception — it keeps bending still-ringing notes with no key down.
    private func scheduleGraphicEndIfNoKeyHeld() {
        pitchBendReleaseGraceUntil = Date().addingTimeInterval(Self.pitchBendReleaseGrace)
        armPitchBendGraceCheck()
    }

    /// Fires when the post-release catch window elapses with no further
    /// trackpad movement — movement (see `handlePitchBendCursorMove`) pushes
    /// `pitchBendReleaseGraceUntil` forward, so the check reschedules itself
    /// for the remainder. A held note or Shift cancels the teardown.
    private func armPitchBendGraceCheck() {
        pitchBendEndTimer?.invalidate()
        let remaining = max(0.05, pitchBendReleaseGraceUntil?.timeIntervalSinceNow ?? 0)
        let t = Timer(timeInterval: remaining, repeats: false) { [weak self] _ in
            guard let self = self else { return }
            let shift = NSEvent.modifierFlags.contains(.shift)
            if self.menuBand.keyboardNotesHeld || shift {
                self.pitchBendEndTimer = nil
                return
            }
            // A move during the window pushed the deadline out — wait the
            // remainder before tearing the graph down.
            if let until = self.pitchBendReleaseGraceUntil,
               until.timeIntervalSinceNow > 0 {
                self.armPitchBendGraceCheck()
                return
            }
            self.pitchBendEndTimer = nil
            self.pitchBendReleaseGraceUntil = nil
            self.endPitchBendSession()
        }
        RunLoop.main.add(t, forMode: .common)
        pitchBendEndTimer = t
    }

    private func endPitchBendSession() {
        pitchBendEndTimer?.invalidate()
        pitchBendEndTimer = nil
        pitchBendReleaseGraceUntil = nil
        // Stop the bend easer so it can't keep nudging pitch after teardown;
        // the fx spring-back (startFxRelease below) owns `bendAmount` now.
        stopBendEase()
        // Clear the latched mode regardless of cursor-lock state so an
        // Esc / focus-loss always fully exits, even if the lock flag was
        // somehow already cleared.
        pitchBendModeLatched = false
        guard pitchBendCursorLocked else {
            // Mode was latched but cursor not currently locked — still
            // make sure the overlay is gone and fx spring back.
            pitchBendOverlay?.dismiss()
            return
        }
        stopPitchBendCursorPin()
        if pitchBendCursorPushed {
            NSCursor.pop()
            pitchBendCursorPushed = false
        }
        // Hide the chart the instant the key lifts — the user asked for
        // no lingering graph. The fx (bend/space/echo) still spring back
        // audibly via `startFxRelease`; only the on-screen overlay goes
        // away immediately. The real system cursor comes back now.
        showSystemCursorIfNeeded()
        CGAssociateMouseAndMouseCursorPosition(1)
        pitchBendCursorLocked = false
        pitchBendOverlay?.dismiss()
        startFxRelease()
    }

    /// Begin the post-release sequence: a `fxHoldDuration` dead zone
    /// where bend/space/echo stay fully engaged (so resuming play
    /// keeps the sound intact), then a linear glide to neutral.
    /// Replaying cancels the whole thing via `cancelFxRelease`
    /// (from `onLitChanged` on a fresh note, and from the gesture
    /// handler when the swipe resumes).
    private func startFxRelease() {
        cancelFxRelease()
        // Nothing engaged → no fade needed, stay idle. The overlay
        // (kept alive through `endPitchBendSession` for the ramp
        // visualisation) has nothing to animate, so tear it down now.
        if bendAmount == 0 && spaceAmount == 0 && echoAmount == 0 && fxX == 0 {
            pitchBendOverlay?.dismiss()
            return
        }
        let hold = Timer(timeInterval: Self.fxHoldDuration, repeats: false) {
            [weak self] _ in self?.startFxRamp()
        }
        RunLoop.main.add(hold, forMode: .common)
        fxHoldTimer = hold
    }

    /// Linear glide of bend/space/echo to neutral over
    /// `fxRampDuration`. No easing — a straight ramp, never a sharp
    /// cutoff. A fresh note mid-ramp cancels it (cancelFxRelease),
    /// freezing the fx wherever the glide had reached.
    private func startFxRamp() {
        fxHoldTimer?.invalidate()
        fxHoldTimer = nil
        // The ramp drives `bendAmount` directly from here on; hand it off so
        // the gesture easer (which may still be settling toward the target)
        // doesn't fight the glide to neutral.
        stopBendEase()
        fxRampFromBend = bendAmount
        fxRampFromSpace = spaceAmount
        fxRampFromEcho = echoAmount
        fxRampFromX = fxX
        fxRampStart = Date()
        let timer = Timer(timeInterval: 1.0 / 60.0,
                          repeats: true) { [weak self] timer in
            guard let self = self, let start = self.fxRampStart else {
                timer.invalidate(); return
            }
            // start is in the past, so -timeIntervalSinceNow = elapsed.
            let elapsed = -start.timeIntervalSinceNow
            let p = Float(min(1, elapsed / Self.fxRampDuration))
            let k = 1 - p
            self.bendAmount = self.fxRampFromBend * k
            self.spaceAmount = self.fxRampFromSpace * k
            self.echoAmount = self.fxRampFromEcho * k
            self.fxX = self.fxRampFromX * k
            // allChannels so lingering / shift-bent voices on other
            // channels un-bend with everything else, not just held.
            self.menuBand.setBend(amount: self.bendAmount, allChannels: true)
            self.menuBand.setSpace(amount: self.spaceAmount)
            self.menuBand.setEcho(amount: self.echoAmount)
            self.pushStaffPitchShift()
            // Floating overlay follows the glide so the wheel
            // un-stretches in lockstep with the audio.
            self.updatePitchBendOverlayImage()
            if p >= 1 {
                self.bendAmount = 0
                self.spaceAmount = 0
                self.echoAmount = 0
                self.fxX = 0
                self.menuBand.setBend(amount: 0, allChannels: true)
                self.menuBand.setSpace(amount: 0)
                self.menuBand.setEcho(amount: 0)
                self.pushStaffPitchShift()
                self.updatePitchBendOverlayImage()
                timer.invalidate()
                self.fxRampTimer = nil
                self.fxRampStart = nil
                // Ramp completed → tear down the chart (kept
                // alive by endPitchBendSession purely so the user
                // could watch the puck slide back to center).
                self.pitchBendOverlay?.dismiss()
            }
        }
        timer.tolerance = 1.0 / 120.0
        RunLoop.main.add(timer, forMode: .common)
        fxRampTimer = timer
    }

    @objc private func systemAppearanceChangedNotification(_ note: Notification) {
        // The distributed notification can land on a background
        // thread; bounce to main before hitting any AppKit state.
        DispatchQueue.main.async { [weak self] in
            self?.systemAppearanceChanged()
        }
    }

    @objc private func systemDidWake(_ note: Notification) {
        // After sleep/wake the polling timer + KVO observers can
        // miss the theme flip that happened while we were
        // suspended, leaving the popover painted in the old theme.
        // Reset our cached `lastObservedDarkMode` so the next poll
        // tick treats the current state as fresh, and run a retint
        // immediately in case the theme changed during sleep.
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            let nowDark = NSApp.effectiveAppearance.bestMatch(
                from: [.aqua, .darkAqua]) == .darkAqua
            self.lastObservedDarkMode = nowDark
            self.systemAppearanceChanged()
        }
    }

    /// Coalesce + propagate a full retint pass after the system
    /// flips dark↔light. Touches every surface that caches
    /// theme-aware colors: the menubar status item, the popover
    /// (if visible), the floating panel, and any open auxiliary
    /// windows (About / AC web). Idempotent — safe to call from
    /// both the KVO and the distributed-notification path.
    private func systemAppearanceChanged() {
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        debugLog("systemAppearanceChanged → isDark=\(isDark)")
        // Re-render the menubar status item so the keyboard's
        // off-white / slate body, the chromatic stripes, and the
        // labels all swap to the new theme atomically.
        updateIcon()
        // Popover background — NSVisualEffectView with `.popover`
        // material can latch onto the appearance it had at
        // construction time. Nudge the chrome's visual-effect
        // view explicitly so the glass tint flips with the system
        // theme even if the panel is currently showing.
        popoverPanel?.appearance = nil
        popoverPanel?.contentView?.appearance = nil
        popoverPanel?.chrome.refreshAppearance()
        // Force the popover's MenuBandPopoverRootView through its
        // explicit retint path (rebuilds layer-painted CGColors
        // that don't auto-track NSColor changes).
        if let popoverVC, popoverVC.isViewLoaded {
            popoverVC.forceAppearanceRetint()
            invalidateDisplayRecursively(from: popoverVC.view)
        }
        // Floating panel — refresh its cached gradients + glass
        // tints, plus force a full needsDisplay so any subview
        // that draws via NSColor names gets re-evaluated.
        pianoWaveformWindowDelegate.refreshAppearance()
        // Walk every open NSWindow's content view so layer-backed
        // child views (About chips, AC web glass, etc) repaint
        // even when their parent didn't propagate the change.
        for window in NSApp.windows {
            window.appearance = nil   // re-inherit from NSApp
            if let root = window.contentView {
                invalidateDisplayRecursively(from: root)
            }
            // Belt + suspenders: explicit display() so layer-backed
            // CALayers redraw this runloop pass, not next frame.
            window.contentView?.displayIfNeeded()
        }
    }

    /// Walk a view tree, marking every NSView dirty so layer-
    /// backed and dynamic-NSColor draws re-resolve on the new
    /// effectiveAppearance. Cheaper than `display()` (defers to
    /// the next runloop pass) but exhaustive enough that no
    /// stale-tinted child can hide.
    private func invalidateDisplayRecursively(from view: NSView) {
        view.needsDisplay = true
        for sub in view.subviews {
            invalidateDisplayRecursively(from: sub)
        }
    }

    private func updatePianoWaveformWindow() {
        pianoWaveformWindowDelegate.refresh()
        guard pianoWaveformWindowDelegate.isCollapsedState else { return }
        // Show is intentionally NOT triggered here — `onChange`
        // fires for every menubar piano tap, and auto-opening on
        // a click was the source of the surprise pop-up. The panel
        // stays a deliberate-trigger surface (LED chip, gear
        // popover, typed-key showIfNeeded). All this path does is
        // manage hide-timer state for an already-visible panel.
        if menuBand.litNotes.isEmpty && !isPopoverPanelShown {
            // Auto-hide the collapsed strip only when it's
            // standalone. While the popover is up the panel is
            // paired with it and must stay visible.
            pianoWaveformWindowDelegate.scheduleHide()
        } else {
            pianoWaveformWindowDelegate.cancelPendingHide()
        }
    }

    private func updatePianoWaveformWindowSuppression() {
        pianoWaveformWindowDelegate.isCollapsedPresentationSuppressed =
            pianoWaveformWindowDelegate.isDocked && (isPopoverPanelShown || pianoWaveformWindowDelegate.isShown)
    }

    // [v1 cutoff] KidLisp TV chooser (currentSynthAmp / KidLispChooserItem /
    // showKidLispChooser / buildKidLispChooserMenu / loadKidLispPiece /
    // resetKidLispPiece / applyKidLisp / reloadKidLispChooser /
    // fetchKidLispChooserPieces) removed with the TV panel. The shared
    // KidLispState + the About-opened aesthetic.computer web window remain.
}
