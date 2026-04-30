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
    private let popover = NSPopover()
    private var popoverVC: MenuBandPopoverViewController?
    private var appBeforeFocusCapture: NSRunningApplication?
    private var focusCaptureArmedByShortcut = false

    /// Periodic check that the status item is actually visible in the
    /// menu bar. macOS silently hides items when there's no room (notch +
    /// many menubar apps). When that happens we shrink the layout —
    /// full piano → 1 octave → compact chip — until something fits.
    private var visibilityTimer: Timer?
    /// Set to true once we've shown the "no room even for compact" alert
    /// so we don't spam the user every check.
    private var hasAlertedNoSpace = false

    /// Click-away monitor active while the popover is shown. Catches clicks
    /// on OTHER apps and dismisses the popover. Clicks on our status-item
    /// button stay in-app and route through `statusClicked`, which only
    /// closes the popover when the user clicks the settings chip — piano
    /// taps keep the popover open.
    private var clickAwayMonitor: Any?
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
        }
        menuBand.bootstrap()

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

        registerTypeModeHotkey()
        _ = registerFocusCaptureHotkey(MenuBandShortcutPreferences.focusShortcut)

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
            let consumed = self.menuBand.handleLocalKey(
                keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, flags: flags
            )
            if consumed && isDown {
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
        let vc = MenuBandPopoverViewController()
        vc.menuBand = menuBand
        vc.popover = popover
        vc.onFocusShortcutChange = { [weak self] shortcut in
            self?.applyFocusShortcut(shortcut) ?? false
        }
        vc.onFocusShortcutRecordingChanged = { [weak self] isRecording in
            self?.setShortcutRecording(isRecording)
        }
        popoverVC = vc
        popover.contentViewController = vc
        // .applicationDefined: never auto-close. We manage closing manually
        // so clicking a menubar piano key (which would normally count as
        // "outside" the popover under .transient) doesn't dismiss the
        // popover while the user is playing.
        popover.behavior = .applicationDefined
        popover.animates = false
        _ = vc.view

        startAdaptiveLayoutChecks()

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
        guard shortcut.isValidForRecording, !shortcut.isReservedForTypeMode else {
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

    private func setShortcutRecording(_ isRecording: Bool) {
        if isRecording {
            typeModeHotkey?.unregister()
            typeModeHotkey = nil
            focusCaptureHotkey?.unregister()
            focusCaptureHotkey = nil
        } else {
            if typeModeHotkey == nil { registerTypeModeHotkey() }
            if focusCaptureHotkey == nil {
                _ = registerFocusCaptureHotkey(MenuBandShortcutPreferences.focusShortcut)
            }
        }
    }

    private func toggleFocusCaptureFromShortcut() {
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
    }

    private func finishLocalCapture(reason: LocalKeyCapture.EndReason) {
        let shouldRestoreFocus = focusCaptureArmedByShortcut && reason == .cancelled
        focusCaptureArmedByShortcut = false
        menuBand.releaseAllHeldNotes()
        ghostUntil = 0
        ghostRefreshTimer?.invalidate()
        ghostRefreshTimer = nil
        updateIcon()
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
        alert.messageText = "Menu Band can't fit in your menu bar"
        alert.informativeText = """
            There's no room in your menu bar — even for the compact icon. \
            Try quitting an app that puts items in the menu bar (slack, \
            dropbox, etc.), or use Bartender / Hidden Bar to manage them.

            Menu Band will keep trying every few seconds.
            """
        alert.alertStyle = .informational
        alert.addButton(withTitle: "OK")
        NSApp.activate(ignoringOtherApps: true)
        alert.runModal()
    }

    func applicationWillTerminate(_ notification: Notification) {
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
        statusItem.length = KeyboardIconRenderer.imageSize.width
        button.image = KeyboardIconRenderer.image(
            litNotes: menuBand.litNotes,
            enabled: menuBand.midiMode,
            typeMode: menuBand.typeMode,
            melodicProgram: menuBand.melodicProgram,
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
        let startNote: UInt8
        switch initial {
        case .openSettings:
            showPopover()
            return
        case .note(let n):
            startNote = n
        case .none:
            return
        }

        let initialPt = imagePoint(from: downEvent.locationInWindow)
        let (vel0, pan0) = expression(for: startNote, at: initialPt)
        menuBand.startTapNote(startNote, velocity: vel0, pan: pan0)
        // Arm sandbox-friendly local capture on a real piano click. We
        // skip arming when global TYPE mode is already on — the global
        // tap is already handling keys, doubling up would re-trigger
        // every note. No letter flash on click: the label overlay is
        // reserved for actual key presses, so the menubar stays clean
        // when you're just tapping the piano with the mouse.
        if !menuBand.typeMode {
            localCapture.arm()
        }
        var current: UInt8? = startNote
        while let next = NSApp.nextEvent(
            matching: [.leftMouseDragged, .leftMouseUp],
            until: .distantFuture,
            inMode: .eventTracking,
            dequeue: true
        ) {
            if next.type == .leftMouseUp {
                if let c = current { menuBand.stopTapNote(c) }
                break
            }
            let pt = imagePoint(from: next.locationInWindow)
            let hovered = KeyboardIconRenderer.noteAt(pt)
            if hovered != current {
                if let prev = current { menuBand.stopTapNote(prev) }
                if let nxt = hovered {
                    let (v, p) = expression(for: nxt, at: pt)
                    menuBand.startTapNote(nxt, velocity: v, pan: p)
                }
                current = hovered
            } else if let c = current {
                let (_, p) = expression(for: c, at: pt)
                menuBand.updateTapPan(c, pan: p)
            }
        }
    }

    private func expression(for midiNote: UInt8, at pt: NSPoint) -> (UInt8, UInt8) {
        guard let rect = KeyboardIconRenderer.keyRect(for: midiNote) else {
            return (100, 64)
        }
        let xRel = max(0, min(1, (pt.x - rect.minX) / rect.width))
        let yRel = max(0, min(1, (pt.y - rect.minY) / rect.height))
        let pan = UInt8(max(0, min(127, Int(round(xRel * 127)))))
        let yDist = abs(yRel - 0.5) * 2.0
        let vMin: Double = 60, vMax: Double = 120
        let vel = vMax - (vMax - vMin) * yDist
        let velocity = UInt8(max(1, min(127, Int(round(vel)))))
        return (velocity, pan)
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
            NSApp.activate(ignoringOtherApps: true)
            popover.show(relativeTo: anchor, of: button, preferredEdge: .minY)
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
}
