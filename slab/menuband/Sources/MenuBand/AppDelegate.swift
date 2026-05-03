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
    private var playPaletteHotkey: GlobalHotkey?
    private var layoutToggleHotkey: GlobalHotkey?
    private let popover = NSPopover()
    private var popoverVC: MenuBandPopoverViewController?
    private lazy var pianoWaveformPalette = UnifiedPianoWaveformPalette(menuBand: menuBand)
    private var floatingPlayPalette: UnifiedPianoWaveformPalette { pianoWaveformPalette }
    private var waveformStrip: UnifiedPianoWaveformPalette { pianoWaveformPalette }
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
                self.updateIcon()
                self.floatingPlayPalette.refresh()
                // Refresh the popover too so live state changes
                // (octave shift via , / . , MIDI mode flip, etc.)
                // reflect immediately while the popover is open.
                if self.popover.isShown {
                    self.popoverVC?.syncFromController()
                }
            }
        }
        menuBand.onLitChanged = { [weak self] in
            guard let self = self else { return }
            // Subtle flash on every fresh note hit so the icon
            // pulses with playing activity. Only on count
            // increment — releases don't re-fire the flash.
            let cur = self.menuBand.litNotes.count
            if cur > self.lastLitCount {
                self.kickIconAnim(slide: 0, flash: 0.7)
            }
            self.lastLitCount = cur
            self.updateIcon()
            self.popoverVC?.refreshHeldNotes()
            self.floatingPlayPalette.refresh()
            self.waveformStrip.refresh()
            self.updateWaveformStrip()
        }
        menuBand.onInstrumentVisualChange = { [weak self] in
            DispatchQueue.main.async {
                guard let self = self else { return }
                self.updateIcon()
                self.floatingPlayPalette.refresh()
                self.waveformStrip.refreshAppearance()
            }
        }
        menuBand.bootstrap()
        lastKnownOctaveShift = menuBand.octaveShift
        floatingPlayPalette.onDismiss = { [weak self] in
            self?.updateIcon()
            self?.popoverVC?.syncFromController()
            self?.updateWaveformStripSuppression()
        }
        floatingPlayPalette.isPianoFocusActive = { [weak self] in
            self?.localCapture.isArmed ?? false
        }
        floatingPlayPalette.onFocusRelease = { [weak self] in
            self?.finishFloatingPaletteKeyboardFocus()
        }
        floatingPlayPalette.onToggleKeymap = { [weak self] in
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
        waveformStrip.reposition(statusItemButton: statusItem.button)
        waveformStrip.onStepBackward = { [weak self] in
            self?.menuBand.stepMelodicProgram(delta: -1)
        }
        waveformStrip.onStepForward = { [weak self] in
            self?.menuBand.stepMelodicProgram(delta: +1)
        }
        waveformStrip.onStepUp = { [weak self] in
            self?.menuBand.stepMelodicProgram(delta: -InstrumentListView.cols)
        }
        waveformStrip.onStepDown = { [weak self] in
            self?.menuBand.stepMelodicProgram(delta: +InstrumentListView.cols)
        }
        waveformStrip.warmUp()

        registerTypeModeHotkey()
        _ = registerFocusCaptureHotkey(MenuBandShortcutPreferences.focusShortcut)
        _ = registerPlayPaletteHotkey(MenuBandShortcutPreferences.playPaletteShortcut)
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
            if self.popover.isShown == false && self.floatingPlayPalette.isShown == false {
                switch keyCode {
                case 123: // kVK_LeftArrow
                    if isDown {
                        self.waveformStrip.registerArrowInput()
                        if !isRepeat { self.menuBand.stepMelodicProgram(delta: -1) }
                    }
                    return true
                case 124: // kVK_RightArrow
                    if isDown {
                        self.waveformStrip.registerArrowInput()
                        if !isRepeat { self.menuBand.stepMelodicProgram(delta: +1) }
                    }
                    return true
                case 125: // kVK_DownArrow
                    if isDown {
                        self.waveformStrip.registerArrowInput()
                        if !isRepeat { self.menuBand.stepMelodicProgram(delta: +InstrumentListView.cols) }
                    }
                    return true
                case 126: // kVK_UpArrow
                    if isDown {
                        self.waveformStrip.registerArrowInput()
                        if !isRepeat { self.menuBand.stepMelodicProgram(delta: -InstrumentListView.cols) }
                    }
                    return true
                default:
                    break
                }
            }
            let consumed = self.menuBand.handleLocalKey(
                keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, flags: flags
            )
            if consumed && isDown {
                if !self.menuBand.litNotes.isEmpty {
                    self.waveformStrip.showIfNeeded()
                }
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

        // Pre-instance the popover + force its view to load now so the
        // first click pops it instantly. With `animates = false` the
        // open/close has no transition — it's a snap, much more "playable"
        // for quickly toggling between the menubar piano and the picker.
        installPopoverVC()
        // .applicationDefined: never auto-close. We manage closing manually
        // so clicking a menubar piano key (which would normally count as
        // "outside" the popover under .transient) doesn't dismiss the
        // popover while the user is playing.
        popover.behavior = .applicationDefined
        popover.animates = false

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
        vc.popover = popover
        vc.onFocusShortcutChange = { [weak self] shortcut in
            self?.applyFocusShortcut(shortcut) ?? false
        }
        vc.onFocusShortcutRecordingChanged = { [weak self] isRecording in
            self?.setShortcutRecording(isRecording)
        }
        vc.onPlayPaletteToggle = { [weak self] in
            self?.togglePlayPaletteFromCommand()
        }
        vc.onPlayPaletteShortcutChange = { [weak self] shortcut in
            self?.applyPlayPaletteShortcut(shortcut) ?? false
        }
        vc.onPlayPaletteShortcutRecordingChanged = { [weak self] isRecording in
            self?.setShortcutRecording(isRecording)
        }
        vc.isPlayPaletteShown = { [weak self] in
            self?.floatingPlayPalette.isShown ?? false
        }
        popoverVC = vc
        popover.contentViewController = vc
        _ = vc.view
    }

    /// Swap the popover's content for a freshly-built VC so every string
    /// re-reads from the current locale. Preserves whether the popover was
    /// open — re-shows it relative to the status item if so.
    private func rebuildPopoverForLanguageChange() {
        let wasShown = popover.isShown
        if wasShown { popover.performClose(nil) }
        installPopoverVC()
        popoverVC?.syncFromController()
        if wasShown, let button = statusItem.button {
            popover.show(relativeTo: button.bounds,
                         of: button,
                         preferredEdge: .minY)
        }
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
    private func registerPlayPaletteHotkey(_ shortcut: MenuBandShortcut) -> Bool {
        let hotkey = GlobalHotkey(
            signature: OSType(0x4D425050),  // 'MBPP'
            id: 1
        ) { [weak self] in
            self?.togglePlayPaletteFromShortcut()
        }
        guard hotkey.register(keyCode: shortcut.keyCode, modifiers: shortcut.modifiers) else {
            return false
        }
        playPaletteHotkey = hotkey
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
        playPaletteHotkey?.unregister()
        playPaletteHotkey = nil
        guard registerPlayPaletteHotkey(shortcut) else {
            _ = registerPlayPaletteHotkey(previous)
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
            playPaletteHotkey?.unregister()
            playPaletteHotkey = nil
            layoutToggleHotkey?.unregister()
            layoutToggleHotkey = nil
        } else {
            if typeModeHotkey == nil { registerTypeModeHotkey() }
            if focusCaptureHotkey == nil {
                _ = registerFocusCaptureHotkey(MenuBandShortcutPreferences.focusShortcut)
            }
            if playPaletteHotkey == nil {
                _ = registerPlayPaletteHotkey(MenuBandShortcutPreferences.playPaletteShortcut)
            }
            if layoutToggleHotkey == nil { registerLayoutToggleHotkey() }
        }
    }

    private func togglePlayPaletteFromShortcut() {
        if floatingPlayPalette.isShown {
            floatingPlayPalette.toggleFromShortcut()
            updateWaveformStripSuppression()
            return
        }
        beginFloatingPlayPalette()
        floatingPlayPalette.toggleFromShortcut()
        updateWaveformStripSuppression()
    }

    private func togglePlayPaletteFromCommand() {
        let appToRestore = appBeforePopover
        beginFloatingPlayPalette()
        appBeforePopover = nil
        floatingPlayPalette.showFromCommand(restoringTo: appToRestore)
        updateWaveformStripSuppression()
    }

    private func beginFloatingPlayPalette() {
        closePopover()
        if localCapture.isArmed {
            localCapture.disarm(reason: .resignedKey)
        }
        if menuBand.typeMode {
            menuBand.disableTypeModeForFocusCapture()
        }
        updateWaveformStripSuppression()
    }

    private func toggleFocusCaptureFromShortcut() {
        if floatingPlayPalette.isKeyboardFocused {
            finishFloatingPaletteKeyboardFocus()
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
        floatingPlayPalette.refresh()
    }

    private func finishLocalCapture(reason: LocalKeyCapture.EndReason) {
        let shouldRestoreFocus = focusCaptureArmedByShortcut && reason == .cancelled
        focusCaptureArmedByShortcut = false
        menuBand.releaseAllHeldNotes()
        ghostUntil = 0
        ghostRefreshTimer?.invalidate()
        ghostRefreshTimer = nil
        updateIcon()
        floatingPlayPalette.refresh()
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

    private func finishFloatingPaletteKeyboardFocus() {
        menuBand.releaseAllHeldNotes()
        floatingPlayPalette.clearInteraction()
        floatingPlayPalette.releaseKeyboardFocus()
        updateIcon()
        floatingPlayPalette.refresh()
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
            self.menuBand.synthSnapshotWaveform(into: &self.visualizerSampleBuffer)
            var sumSq: Float = 0
            for s in self.visualizerSampleBuffer { sumSq += s * s }
            let rms = sqrt(sumSq / Float(self.visualizerSampleBuffer.count))

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
            let armed = event.modifierFlags.contains(.shift)
                || event.modifierFlags.contains(.capsLock)
            if KeyboardIconRenderer.labelsUppercase != armed {
                KeyboardIconRenderer.labelsUppercase = armed
                self.updateIcon()
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
        let initialArmed = initial.contains(.shift) || initial.contains(.capsLock)
        if KeyboardIconRenderer.labelsUppercase != initialArmed {
            KeyboardIconRenderer.labelsUppercase = initialArmed
            updateIcon()
        }
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
        floatingPlayPalette.dismiss(reason: .programmatic)
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

    private func updateIcon() {
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
        // Note: miniVisualizerLevel + miniVisualizerPhase are driven by
        // the visualizerAnimTimer (24fps smoothed VU motion), not from
        // here — overriding them on every note-event redraw would
        // erase the smoothing.
        statusItem.length = KeyboardIconRenderer.imageSize.width
        button.image = KeyboardIconRenderer.image(
            litNotes: menuBand.litNotes,
            enabled: menuBand.midiMode,
            typeMode: menuBand.typeMode,
            melodicProgram: menuBand.effectiveMelodicProgram,
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
            // Mini-visualizer click → "big overlay" (the floating play
            // palette window). Same target the popover's WaveformView
            // routes to, so the user gets one entry point regardless of
            // where they tap.
            floatingPlayPalette.show()
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
        waveformStrip.showIfNeeded()
        // Arm sandbox-friendly local capture on a real piano click. We
        // skip arming when global TYPE mode is already on — the global
        // tap is already handling keys, doubling up would re-trigger
        // every note. No letter flash on click: the label overlay is
        // reserved for actual key presses, so the menubar stays clean
        // when you're just tapping the piano with the mouse.
        if !menuBand.typeMode {
            localCapture.arm()
            floatingPlayPalette.refresh()
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
                    waveformStrip.showIfNeeded()
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
    private func closePopover() {
        if popover.isShown {
            popover.performClose(nil)
        }
        if let m = clickAwayMonitor { NSEvent.removeMonitor(m); clickAwayMonitor = nil }
        if let m = popoverEscMonitor { NSEvent.removeMonitor(m); popoverEscMonitor = nil }
        appBeforePopover = nil
        updateWaveformStripSuppression()
    }

    /// Distributed-notification entry point for the dev affordance
    /// registered in `applicationDidFinishLaunching`. Always *opens*
    /// the popover (never closes) — repeated triggers from a shell
    /// rebuild loop should leave it visible, not flicker shut.
    @objc private func handleShowPopoverNotification(_ note: Notification) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            if !self.popover.isShown {
                self.showPopover()
            }
        }
    }

    private func showPopover() {
        guard let button = statusItem.button else { return }
        // popoverVC is pre-built in applicationDidFinishLaunching so the first
        // open is instant — no lazy view inflation here.
        popoverVC?.syncFromController()
        if popover.isShown {
            closePopover()
        } else {
            let imgSize = KeyboardIconRenderer.imageSize
            let bb = button.bounds
            let xOff = (bb.width - imgSize.width) / 2.0
            let yOff = (bb.height - imgSize.height) / 2.0
            let latch = KeyboardIconRenderer.settingsRectPublic
            let anchor = NSRect(
                x: xOff + latch.minX,
                y: yOff + latch.minY,
                width: latch.width,
                height: latch.height
            )
            // Activate the app + make the popover key so hover and clicks
            // register immediately. NSStatusItem popovers don't pull focus
            // by default; without this you have to click into the popover
            // once before its controls react.
            let frontmost = NSWorkspace.shared.frontmostApplication
            appBeforePopover = frontmost?.bundleIdentifier == Bundle.main.bundleIdentifier
                ? nil
                : frontmost
            NSApp.activate(ignoringOtherApps: true)
            popover.show(relativeTo: anchor, of: button, preferredEdge: .minY)
            updateWaveformStripSuppression()
            DispatchQueue.main.async {
                self.popover.contentViewController?.view.window?.makeKey()
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

    private func updateWaveformStrip() {
        guard waveformStrip.isCollapsedState else { return }
        if !menuBand.litNotes.isEmpty {
            waveformStrip.showIfNeeded()
        } else {
            waveformStrip.scheduleHide()
        }
    }

    private func updateWaveformStripSuppression() {
        waveformStrip.suppressed = waveformStrip.isDocked && (popover.isShown || floatingPlayPalette.isShown)
    }
}
