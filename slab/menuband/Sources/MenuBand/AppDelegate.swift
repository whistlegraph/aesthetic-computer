import AppKit
import Carbon

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem!
    private let menuBand = MenuBandController()
    private let hoverResponder = HoverResponder()
    private var hoveredElement: KeyboardIconRenderer.HitResult? = nil
    private var trackingArea: NSTrackingArea?
    private var typeModeHotkey: GlobalHotkey?
    private var focusCaptureHotkey: GlobalHotkey?
    private var pianoWaveformHotkey: GlobalHotkey?
    private var layoutToggleHotkey: GlobalHotkey?
    private var popoverPanel: MenuBandPopoverPanel?
    private var popoverVC: MenuBandPopoverViewController?

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
    /// 24fps redraw timer for the in-chip mini visualizer. Drives the
    /// per-bar sine wiggle + smooths the activity level so bars move
    /// continuously instead of snapping on note events.
    private var visualizerAnimTimer: Timer?
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
    /// Latest mic-input RMS published by `MenuBandSampleVoice`'s tap
    /// while the user is recording (key `). Drives the menubar VU
    /// bars when the sample-voice backend is capturing audio. The
    /// visualizer animation tick reads this on every frame and
    /// substitutes it for the synth-output RMS while
    /// `menuBand.sampleRecordingActive` is true.
    private var latestMicLevel: Float = 0
    /// Set to true once we've shown the "no room even for compact" alert
    /// so we don't spam the user every check.
    private var hasAlertedNoSpace = false

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
    private var popoverEscMonitor: Any?

    /// Sandbox-friendly local key capture. Armed when the user clicks the
    /// menubar piano (without opening the popover). The companion ghost
    /// timer paints letter labels on the menubar keys briefly when armed
    /// or when an actual key is pressed — visual signal that "you're
    /// capturing, type now."
    private let localCapture = LocalKeyCapture()
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
    private var slideDirection: Int = 0
    private var slideStartedAt: CFTimeInterval = 0
    private var flashStrength: CGFloat = 0
    private var flashStartedAt: CFTimeInterval = 0
    private var iconAnimTimer: Timer?
    private static let slideDuration: CFTimeInterval = 0.34
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
    /// Velocity component of the spring rubber-band that snaps the
    /// bend back to 0 when the user lifts their fingers.
    private var bendVelocity: Float = 0
    /// Active rubber-band timer, retained so we can cancel it when
    /// a new gesture starts mid-decay.
    private var bendDecayTimer: Timer?
    /// Tracks the last lit-note count we observed in `onLitChanged`
    /// so we can detect the all-notes-released edge and trigger
    /// both the bend rubber-band and the cursor pop.
    private var pitchBendCursorPushed = false
    /// Mirrors `CGAssociateMouseAndMouseCursorPosition(0)` state.
    /// Avoids spurious calls on every onLitChanged tick — only
    /// flips on the keyboard-held edges.
    private var pitchBendCursorLocked = false
    /// Fires when no cursor-Y delta has come in for the idle
    /// window — synthesizes a "finger lifted off the trackpad"
    /// edge. mouseMoved has no .ended phase, so we infer it from
    /// silence and start the rubber-band.
    private var bendIdleTimer: Timer?
    /// Local + global NSEvent monitors for trackpad scroll events.
    /// Held so we can teardown if needed (we don't, but per-instance
    /// retention is cleaner than globals).
    private var bendScrollLocal: Any?
    private var bendScrollGlobal: Any?
    /// Spring constants for the rubber-band decay. Stiffness too
    /// high snaps too fast; damping below ~2*sqrt(stiffness) leaves
    /// some bounce so the snap feels rubbery rather than mechanical.
    private static let bendSpringStiffness: Float = 80
    private static let bendSpringDamping: Float = 9
    /// Trackpad-points-per-unit-bend. 80pt of vertical drag pulls
    /// to a full ±1 (= ±2 semitones at default GM range), so a
    /// modest two-finger flick reads as a noticeable bend.
    private static let bendSensitivityPerPoint: Float = 1.0 / 80.0

    func applicationDidFinishLaunching(_ notification: Notification) {
        debugLog("applicationDidFinishLaunching pid=\(ProcessInfo.processInfo.processIdentifier)")
        Self.registerBundledFonts()
        // Apply forceLayout *before* statusItem creation so the initial
        // status-item length matches the pinned layout's imageSize.
        // Otherwise the icon flashes the default `.full` width until
        // the next updateIcon() catches up.
        applyForcedLayoutIfAny()
        Timer.scheduledTimer(withTimeInterval: 5.0, repeats: true) { _ in
            debugLog("heartbeat")
        }
        menuBand.onChange = { [weak self] in
            DispatchQueue.main.async {
                guard let self = self else { return }
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
        menuBand.onLitChanged = { [weak self] in
            guard let self = self else { return }
            // Subtle flash on every fresh note hit so the icon
            // pulses with playing activity. Only on count
            // increment — releases don't re-fire the flash.
            let prev = self.lastLitCount
            let cur = self.menuBand.litNotes.count
            if cur > prev {
                self.kickIconAnim(slide: 0, flash: 0.7)
            }
            // Pitch-bend cursor lifecycle — only engages when
            // the user is playing via the KEYBOARD (not mouse
            // taps). Locking the cursor on a mouse-tapped note
            // would freeze drag mid-stroke on the menubar
            // piano, so we gate on `keyboardNotesHeld`. Trackpad
            // pitch-bend is a keyboard-mode feature: hold a
            // letter → cursor locks → swipe to bend → release
            // letter → cursor unlocks + spring rubber-band.
            let kbHeld = self.menuBand.keyboardNotesHeld
            if kbHeld && !self.pitchBendCursorLocked {
                CGAssociateMouseAndMouseCursorPosition(0)
                self.pitchBendCursorLocked = true
            } else if !kbHeld && self.pitchBendCursorLocked {
                if self.pitchBendCursorPushed {
                    NSCursor.pop()
                    self.pitchBendCursorPushed = false
                }
                CGAssociateMouseAndMouseCursorPosition(1)
                self.pitchBendCursorLocked = false
                self.startBendDecay()
            }
            self.lastLitCount = cur
            self.updateIcon()
            self.popoverVC?.refreshHeldNotes()
            self.updatePianoWaveformWindow()
        }
        menuBand.onInstrumentVisualChange = { [weak self] in
            DispatchQueue.main.async {
                guard let self = self else { return }
                self.updateIcon()
                self.updatePianoWaveformWindow()
            }
        }
        menuBand.onMIDIEvent = {
            // Spike the square indicator to full on every
            // outbound noteOn; the visualizer animation tick
            // decays it back toward zero.
            KeyboardIconRenderer.midiActivityFlash = 1
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
            self?.localCapture.isArmed ?? false
        }
        pianoWaveformWindowDelegate.onFocusRelease = { [weak self] in
            self?.finishPianoWaveformKeyboardFocus()
        }
        pianoWaveformWindowDelegate.onToggleKeymap = { [weak self] in
            self?.toggleKeyboardLayoutShortcut()
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
        pianoWaveformWindowDelegate.warmUp()

        // Type-mode (cmd-ctrl-opt-P) and floating-piano (cmd-ctrl-opt-
        // space) shortcuts retired — they were undocumented power-user
        // affordances and overlap with the menubar piano + popover
        // flow. Layout toggle stays; focus shortcut (K) is repurposed
        // below to open the expanded floating panel centered.
        _ = registerFocusCaptureHotkey(MenuBandShortcutPreferences.focusShortcut)
        registerLayoutToggleHotkey()

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

        // Local key capture wiring. Routes keys to the same note logic the
        // global tap uses, with the ghost-label flash on every press so the
        // user sees the layout dynamically appear while typing.
        localCapture.onKey = { [weak self] keyCode, isDown, isRepeat, flags in
            guard let self = self else { return false }
            // Escape disarms capture explicitly. Useful when the user
            // wants to release focus without clicking another app.
            if isDown && keyCode == 53 /* kVK_Escape */ {
                NSSound(named: NSSound.Name("Tink"))?.play()
                self.localCapture.disarm(reason: .cancelled)
                return true
            }
            if isDown && MenuBandShortcut.layoutToggle.matches(
                keyCode: UInt32(keyCode),
                modifiers: MenuBandShortcut.carbonModifiers(from: flags)
            ) {
                self.toggleKeyboardLayoutShortcut()
                return true
            }
            // Spacebar toggles the metronome whenever the popover is
            // open. Consumed in both directions so the keystroke
            // never falls through to the focused app or to the note
            // path (where space would otherwise behave like an
            // unmapped key consume).
            if keyCode == 49 /* kVK_Space */, self.isPopoverPanelShown {
                if isDown && !isRepeat {
                    self.popoverVC?.toggleMetronome()
                }
                return true
            }
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

        startAdaptiveLayoutChecks()
        startShiftStateMonitors()
        startVisualizerAnimation()

        // Retint the bundle's Finder icon to the user's accent color.
        // Stored as an xattr on the bundle folder, so the signed payload
        // isn't modified. Refreshed whenever the accent changes.
        IconTinter.applyTintedIcon()
        NotificationCenter.default.addObserver(
            forName: NSColor.systemColorsDidChangeNotification,
            object: nil, queue: .main
        ) { _ in
            IconTinter.applyTintedIcon()
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
        if hotkey.register(
            keyCode: MenuBandShortcut.layoutToggle.keyCode,
            modifiers: MenuBandShortcut.layoutToggle.modifiers
        ) {
            layoutToggleHotkey = hotkey
        }
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
        localCapture.cancelShortcut = shortcut
        return true
    }

    private func applyFocusShortcut(_ shortcut: MenuBandShortcut) -> Bool {
        guard shortcut.isValidForRecording,
              !shortcut.isReservedForTypeMode,
              shortcut != MenuBandShortcutPreferences.playPaletteShortcut else {
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
              shortcut != MenuBandShortcutPreferences.focusShortcut else {
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
        // Cmd-Ctrl-Opt-K now opens (or toggles) the expanded
        // floating piano centered on the active display. With the
        // popover closed, `popoverFrameProvider` returns nil and
        // `expandedFrame` falls back to a `centeredOrigin`, so the
        // panel pops up dead-center.
        let popoverWasOpen = isPopoverPanelShown
        let panelWasOpen = pianoWaveformWindowDelegate.isShown
        closePopover()
        if panelWasOpen {
            pianoWaveformWindowDelegate.dismiss(reason: .programmatic)
            return
        }
        // Defer the panel open by one runloop tick so AppKit can finish
        // tearing down the popover window first. Without this, the popover
        // and the expanded panel both render on screen simultaneously
        // (both wear liquid-glass material → reads as "two large popovers").
        if popoverWasOpen {
            DispatchQueue.main.async { [weak self] in
                self?.pianoWaveformWindowDelegate.showExpandedForPopover()
            }
        } else {
            pianoWaveformWindowDelegate.showExpandedForPopover()
        }
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

    private func beginFocusCaptureFromShortcut() {
        let frontmost = NSWorkspace.shared.frontmostApplication
        if frontmost?.bundleIdentifier == Bundle.main.bundleIdentifier {
            appBeforeFocusCapture = nil
        } else {
            appBeforeFocusCapture = frontmost
        }
        closePopover()
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
        // 120fps tick — half the perceptual latency of the prior 60fps
        // path. Combined with the 256-sample RMS window (~5.8ms),
        // adaptive auto-gain (matches the main visualizer's envelope),
        // and a snap-instant attack, the bars now move within ~10ms
        // of a note hitting the synth and reach full height even for
        // quiet voices. Bars are ALWAYS animating (popover or palette
        // open or not); when silent they fall to a flat/short floor
        // instead of vanishing, so the menubar always looks "alive."
        let timer = Timer(timeInterval: 1.0 / 120.0, repeats: true) { [weak self] _ in
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
            let holdFramesAtPeak = 30        // 250ms at 120fps
            let releaseAlpha: CGFloat = 0.012 // ~480ms half-life
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
            self.updateIcon()
        }
        RunLoop.main.add(timer, forMode: .common)
        visualizerAnimTimer = timer
    }

    /// Watch shift state globally + locally so the menubar piano can
    /// uppercase its letter labels while the user holds shift. The
    /// uppercase letters are the visual cue that linger / bell-ring
    /// mode is armed; lowercase = normal.
    private func startShiftStateMonitors() {
        let handler: (NSEvent) -> Void = { [weak self] event in
            guard let self = self else { return }
            // Caps lock latches the mode; shift is the momentary. Either
            // arms linger and shows uppercase labels.
            let caps = event.modifierFlags.contains(.capsLock)
            let armed = event.modifierFlags.contains(.shift) || caps
            var dirty = false
            if KeyboardIconRenderer.labelsUppercase != armed {
                KeyboardIconRenderer.labelsUppercase = armed
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
        let initialArmed = initial.contains(.shift) || initialCaps
        var initialDirty = false
        if KeyboardIconRenderer.labelsUppercase != initialArmed {
            KeyboardIconRenderer.labelsUppercase = initialArmed
            initialDirty = true
        }
        if KeyboardIconRenderer.lingerCapsLatched != initialCaps {
            KeyboardIconRenderer.lingerCapsLatched = initialCaps
            initialDirty = true
        }
        if initialDirty { updateIcon() }
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
        // Periodic re-check. Menubar real-estate changes throughout the
        // day as apps come and go; 6s polling is cheap and lets us both
        // shrink (when squeezed) and re-expand (when room opens up).
        visibilityTimer?.invalidate()
        visibilityTimer = Timer.scheduledTimer(withTimeInterval: 6.0, repeats: true) { [weak self] _ in
            self?.adaptLayoutForAvailableSpace()
        }
    }

    private func adaptLayoutForAvailableSpace() {
        // Forced layouts disable auto-resize entirely — the renderer
        // stays pinned to whatever the user (or QA) chose.
        if KeyboardIconRenderer.forceLayout != nil { return }
        let current = KeyboardIconRenderer.displayLayout
        let visible = isStatusItemVisible()

        if !visible {
            // Shrink to the next-smaller layout. If we're already at
            // .compact and STILL not visible, alert the user once.
            if let smaller = current.smaller {
                debugLog("statusItem hidden — shrinking \(current) → \(smaller)")
                KeyboardIconRenderer.displayLayout = smaller
                updateIcon()
            } else if !hasAlertedNoSpace {
                hasAlertedNoSpace = true
                DispatchQueue.main.async { [weak self] in self?.alertNoMenuBarSpace() }
            }
            return
        }

        // Visible. If we previously shrunk, try to expand back. Set the
        // larger layout, force a layout, then re-check; revert if we
        // lost the slot.
        guard let bigger = current.larger else { return }
        KeyboardIconRenderer.displayLayout = bigger
        updateIcon()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) { [weak self] in
            guard let self = self else { return }
            if !self.isStatusItemVisible() {
                debugLog("expand \(current) → \(bigger) didn't fit; reverting")
                KeyboardIconRenderer.displayLayout = current
                self.updateIcon()
            } else {
                debugLog("statusItem expanded \(current) → \(bigger)")
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
    private static func registerBundledFonts() {
        let bundle = Bundle.module
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
    private func kickIconAnim(slide: Int, flash: CGFloat) {
        let now = CACurrentMediaTime()
        if slide != 0 {
            slideDirection = slide
            slideStartedAt = now
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

    /// Computed slide offset for the current frame. Two-phase
    /// "masked scroll" so the whole board reads as physically
    /// scrolling past the menubar slot:
    ///   phase 1: image slides off in `slideDirection` until it's
    ///            fully off the slot (offset = ±imageWidth)
    ///   phase 2: image enters from the OPPOSITE side and settles
    ///            back at offset 0
    /// Result: instead of a back-and-forth shove, the user gets a
    /// physical sense of the piano scrolling — like dragging a long
    /// keyboard sideways and seeing one octave's worth slide past.
    private func currentSlideOffset() -> CGFloat {
        guard slideDirection != 0 else { return 0 }
        let elapsed = CACurrentMediaTime() - slideStartedAt
        if elapsed >= Self.slideDuration { return 0 }
        let t = elapsed / Self.slideDuration  // 0…1
        let w = KeyboardIconRenderer.imageSize.width
        let dir = CGFloat(slideDirection)
        if t < 0.5 {
            // Linear slide off — keeps speed constant so the scroll
            // reads as a real surface moving past.
            let phase = CGFloat(t / 0.5)
            return phase * w * dir
        } else {
            // Re-entry from the opposite side, easing into rest.
            let phase = CGFloat((t - 0.5) / 0.5)
            return -(1 - phase) * w * dir
        }
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
        // Note: miniVisualizerLevel + miniVisualizerPhase are driven by
        // the visualizerAnimTimer (24fps smoothed VU motion), not from
        // here — overriding them on every note-event redraw would
        // erase the smoothing.
        statusItem.length = KeyboardIconRenderer.imageSize.width
        // While the sample backend is active, the voice subscript
        // shows the literal `\`` glyph instead of a numeric program
        // slot — visually marks "this voice is your recording, not a
        // GM patch." Otherwise the existing 1-based digit logic
        // continues to render the GM program number.
        let voiceLabel: String? = (menuBand.instrumentBackend == .sample) ? "`" : nil
        button.image = KeyboardIconRenderer.image(
            litNotes: menuBand.litNotes,
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
            letterAlphas[key] = abs(next) < 0.001 ? 0 : next
        }

        // 3. Park the timer when everything has fully decayed and the
        //    ghost is past — saves CPU while idle.
        let fullyDecayed = letterAlphas.values.allSatisfy { $0 < 0.01 }
        if isReleasing && fullyDecayed {
            letterAlphas.removeAll()
            letterReached.removeAll()
            letterFadeTimer?.invalidate()
            letterFadeTimer = nil
        }
        updateIcon()
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
    }

    private func handleHoverExit() {
        if hoveredElement != nil {
            hoveredElement = nil
            updateIcon()
        }
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
        case .note(let n):
            startDisplayNote = n
        case .none:
            return
        }

        guard let startNote = playedNote(for: startDisplayNote) else { return }

        let initialPt = imagePoint(from: downEvent.locationInWindow)
        let (vel0, pan0) = NoteExpression.values(for: startDisplayNote, at: initialPt)
        let initialShift = downEvent.modifierFlags.contains(.shift)
            || downEvent.modifierFlags.contains(.capsLock)
        menuBand.startTapNote(
            startNote,
            velocity: vel0,
            pan: pan0,
            displayNote: startDisplayNote,
            linger: initialShift
        )
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
        var currentDisplay: UInt8? = startDisplayNote
        var currentPlayed: UInt8? = startNote
        while let next = NSApp.nextEvent(
            matching: [.leftMouseDragged, .leftMouseUp],
            until: .distantFuture,
            inMode: .eventTracking,
            dequeue: true
        ) {
            if next.type == .leftMouseUp {
                if let c = currentPlayed { menuBand.stopTapNote(c) }
                break
            }
            let pt = imagePoint(from: next.locationInWindow)
            let hoveredDisplay = KeyboardIconRenderer.noteAt(pt)
            if hoveredDisplay != currentDisplay {
                if let prev = currentPlayed { menuBand.stopTapNote(prev) }
                if let nxtDisplay = hoveredDisplay,
                   let nxtPlayed = playedNote(for: nxtDisplay) {
                    let (v, p) = NoteExpression.values(for: nxtDisplay, at: pt)
                    // Sample shift+capslock state per-note during the drag
                    // so the user can shift-press, release shift mid-drag,
                    // and still get linger from a latched caps lock.
                    let shiftNow = next.modifierFlags.contains(.shift)
                        || next.modifierFlags.contains(.capsLock)
                    menuBand.startTapNote(
                        nxtPlayed,
                        velocity: v,
                        pan: p,
                        displayNote: nxtDisplay,
                        linger: shiftNow
                    )
                    currentPlayed = nxtPlayed
                } else {
                    currentPlayed = nil
                }
                currentDisplay = hoveredDisplay
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

    // MARK: - Popover

    /// Close the popover and tear down the click-away monitor. Called from
    /// `statusClicked` (settings-chip toggle) and from the click-away
    /// monitor itself when the user clicks anywhere outside our app.
    /// Closes with a short fade-out (~140 ms) so the popover dissolves
    /// the way standard macOS menu pop-ups do, instead of cutting away.
    private func closePopover() {
        // Tear down monitors immediately so a stray click during the
        // fade can't re-trigger close.
        if let m = clickAwayMonitor { NSEvent.removeMonitor(m); clickAwayMonitor = nil }
        if let m = popoverEscMonitor { NSEvent.removeMonitor(m); popoverEscMonitor = nil }
        appBeforePopover = nil
        let panelToFade = popoverPanel
        // Drop the reference now so isPopoverPanelShown becomes
        // false synchronously — prevents toggle paths from racing
        // with the in-flight fade.
        popoverPanel = nil
        if let panel = panelToFade {
            NSAnimationContext.runAnimationGroup({ ctx in
                ctx.duration = 0.14
                ctx.timingFunction = CAMediaTimingFunction(name: .easeOut)
                panel.animator().alphaValue = 0
            }, completionHandler: { [weak self] in
                self?.popoverVC?.view.removeFromSuperview()
                panel.orderOut(nil)
                // Reset alpha so the next show isn't invisible.
                panel.alphaValue = 1
            })
        }
        // Floating window pairs with the popover — fade alongside.
        pianoWaveformWindowDelegate.dismiss(reason: .programmatic)
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

    private func showPopover() {
        guard let button = statusItem.button,
              let buttonWindow = button.window else { return }
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
            let gearLocal = NSPoint(x: xOff + latch.midX, y: 0)
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

            // Arm local key capture so arrow keys + spacebar reach
            // our handler while the popover is up. The InstrumentList
            // used to be in the popover and grabbed keys via its
            // first-responder; once it moved to the floating panel
            // nothing was capturing arrows for the controller.
            if !localCapture.isArmed { localCapture.arm() }
            DispatchQueue.main.async { [weak panel] in
                panel?.makeKey()
            }
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

    // MARK: - Pitch-bend gesture

    private func handlePitchBendCursorMove(event: NSEvent) {
        // Single-finger trackpad gesture: only active while the
        // user is holding a KEYBOARD note. Mouse-tapped piano
        // notes don't engage pitch-bend so the user can drag
        // across menubar piano keys with the mouse normally.
        guard menuBand.keyboardNotesHeld else { return }
        let dy = Float(event.deltaY)
        guard dy != 0 else { return }
        // Negate so swipe UP on trackpad → pitch UP (NSEvent.deltaY
        // is positive when the cursor moves DOWN on screen).
        let bendDelta = -dy * Self.bendSensitivityPerPoint
        cancelBendDecay()
        bendAmount += bendDelta
        bendAmount = max(-1, min(1, bendAmount))
        menuBand.setBend(amount: bendAmount)
        pushStaffPitchShift()
        if !pitchBendCursorPushed {
            PitchBendCursor.neutral.push()
            pitchBendCursorPushed = true
        }
        PitchBendCursor.cursor(forBend: bendAmount).set()
        // Re-arm the idle-detection timer; if 150 ms goes by
        // without another delta we treat the finger as lifted
        // and start the spring rubber-band.
        bendIdleTimer?.invalidate()
        bendIdleTimer = Timer.scheduledTimer(withTimeInterval: 0.15,
                                              repeats: false) { [weak self] _ in
            self?.startBendDecay()
        }
        debugLog("bend cursor dy=\(dy) lit=\(menuBand.litNotes.count) amt=\(bendAmount)")
    }

    /// Forward the current effective pitch shift (octave + bend)
    /// into the popover's staff view so the staff visibly slides
    /// up/down on every shift. ±1 of bendAmount = ±2 semitones; 1
    /// octave = 12 semitones. The staff eases its displayed shift
    /// toward this target so changes glide instead of snapping.
    private func pushStaffPitchShift() {
        let bendSemitones = CGFloat(bendAmount) * 2
        let octaveSemitones = CGFloat(menuBand.octaveShift) * 12
        popoverVC?.staffView?.targetPitchShiftSemitones = bendSemitones + octaveSemitones
    }

    private func cancelBendDecay() {
        bendDecayTimer?.invalidate()
        bendDecayTimer = nil
        bendIdleTimer?.invalidate()
        bendIdleTimer = nil
        bendVelocity = 0
    }

    private func startBendDecay() {
        cancelBendDecay()
        bendDecayTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0,
                                                repeats: true) { [weak self] timer in
            guard let self = self else { timer.invalidate(); return }
            // Spring step — Hooke's law plus viscous damping. With
            // stiffness=80 and damping=9, ratio is ~0.5 so the bend
            // overshoots zero a little before settling, which reads
            // as the rubbery snap the user asked for.
            let dt: Float = 1.0 / 60.0
            let force = -Self.bendSpringStiffness * self.bendAmount
                - Self.bendSpringDamping * self.bendVelocity
            self.bendVelocity += force * dt
            self.bendAmount += self.bendVelocity * dt
            self.menuBand.setBend(amount: self.bendAmount)
            self.pushStaffPitchShift()
            // Cursor follows the spring so the wheel un-stretches
            // in lockstep with the audio bend — the visual is
            // always in sync with what the user hears.
            if self.pitchBendCursorPushed {
                PitchBendCursor.cursor(forBend: self.bendAmount).set()
            }
            if abs(self.bendAmount) < 0.001 && abs(self.bendVelocity) < 0.001 {
                self.bendAmount = 0
                self.bendVelocity = 0
                self.menuBand.setBend(amount: 0)
                if self.pitchBendCursorPushed {
                    PitchBendCursor.neutral.set()
                }
                timer.invalidate()
                self.bendDecayTimer = nil
            }
        }
    }

    @objc private func systemAppearanceChangedNotification(_ note: Notification) {
        // The distributed notification can land on a background
        // thread; bounce to main before hitting any AppKit state.
        DispatchQueue.main.async { [weak self] in
            self?.systemAppearanceChanged()
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
}
