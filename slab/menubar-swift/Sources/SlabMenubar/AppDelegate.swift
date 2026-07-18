import AppKit
import Carbon.HIToolbox

final class AppDelegate: NSObject, NSApplicationDelegate, NSMenuDelegate {
    private var statusItem: NSStatusItem!
    /// One stable menu instance owned for the app's lifetime. We rebuild its
    /// *contents* lazily in `menuNeedsUpdate(_:)` rather than swapping the
    /// object on a timer — swapping `statusItem.menu` mid-track is what made
    /// the dropdown / submenus hitch under the cursor.
    private let menu = NSMenu()
    /// Set while a background `StateSnapshot.gather()` is in flight so an
    /// overlapping 2 s tick doesn't pile up worker threads (a stalled
    /// `tailscale status` can run right up to its 2 s timeout).
    private var gathering = false
    /// Subject keys we've already fired a `slab-wallpaper` gen for, so a slow
    /// generation isn't re-kicked every 2 s tick. Guarded by `wallpaperLock`
    /// (touched from the off-main resolve).
    private var kickedWallpapers = Set<String>()
    /// At most one `slab-wallpaper` gen may run at a time. Every status flip and
    /// every retitled session mints a fresh key, and a gen takes minutes on a
    /// loaded box — detached spawns outran their own renders and piled up
    /// (8 nodes × ~16 MB on neo, self-amplifying into swap). A request landing
    /// mid-render coalesces into `pendingWallpaper`, replacing whatever was
    /// waiting: only the newest state is worth drawing.
    private var wallpaperBusy = false
    private var pendingWallpaper: (key: String, subject: String, status: String)?
    private let wallpaperLock = NSLock()
    /// `slab-wallpaper path` is a pure hash of (subject, status) plus an
    /// existence check, so once it names a real file that answer is permanent —
    /// but we were forking a node to re-ask it for every session on every 2 s
    /// tick. Seven sessions is ~3.5 node spawns a second, forever, and it scales
    /// with the one number that keeps growing. Remember the answers instead.
    private var wallpaperPaths: [String: String] = [:]
    /// Keys whose render hasn't landed yet still answer "" — those we re-ask, but
    /// on a slow clock, not every tick. A finished gen clears its stamp so the
    /// fresh picture is picked up on the next pass rather than after the wait.
    private var wallpaperProbedAt: [String: Date] = [:]
    private let wallpaperProbeRetry: TimeInterval = 20
    /// One-shot guard so the status-defaults gen is kicked once per launch.
    private var defaultsKicked = false
    private var refreshTimer: Timer?
    /// Dedicated contact-awareness clock. iMessage must not depend on the
    /// heavier fleet snapshot completing; a stalled system probe should never
    /// make Slab miss an incoming message.
    private var imsgTimer: Timer?
    private var animTimer: Timer?
    /// Drives the attention-blink on `.complete` / `.awaiting` terminal
    /// backgrounds — both states need the user's eyes, and a static blue vs.
    /// orange is too easy to confuse. Only runs while at least one session is
    /// in an attention state; toggles `blinkPhase` and re-applies decor each
    /// tick so the bg alternates between its base and pulse palettes.
    private var blinkTimer: Timer?
    private var blinkPhase: Bool = false
    /// Live-updates the pop-render progress bars while the menu is open.
    /// Re-reads ~/.ac-pop-renders/ (cheap — small dir of small JSON files)
    /// and rewrites each tagged row's title in place. Scheduled in
    /// `.common` mode so it actually fires while NSMenu tracks events;
    /// invalidated in `menuDidClose`.
    private var popRenderTimer: Timer?
    /// `sessionId → "<state>|<subject>"` of the last theme/title we pushed
    /// to Terminal, so the per-tick refresh only fires osascript when
    /// something actually changed.
    private var lastTerminalDecor: [String: String] = [:]
    /// Last desktop-picture path slab pushed via System Events (the tint PNG,
    /// or "" meaning "restored to the user's original"), so the per-tick
    /// refresh only re-sets the wallpaper when the aggregate status changed.
    private var lastDesktopTint: String?
    /// True once we've saved the user's pre-slab desktop picture to
    /// `Paths.desktopOriginalFile`; until then we never overwrite it.
    private var desktopOriginalCaptured = false
    /// Base font size from the most recent `tileNow()` pass. `applyTerminalDecor`
    /// scales typography off this — `.awaiting` ("orange") tiles get bumped
    /// up so focus reads typographically while the cell geometry stays put.
    private var lastTiledFontSize: Int?
    /// True while the windows are scattered (⌘⌥S) rather than tiled. The 0.6s
    /// decor refresh pins the terminal font to `lastTiledFontSize`, which would
    /// otherwise stomp the tiny scatter font right back to the tile size every
    /// tick — so while this is set, `applyTerminalDecor` pins `scatterFontSize`
    /// instead. Any tile pass clears it.
    private var scatterMode = false
    /// Count of AC Electron (preview) windows seen last tick — when it changes
    /// we auto re-pack the grid so previews slot in with the terminals.
    private var lastAcWindowCount = 0
    private var rainbowPhase: CGFloat = 0
    private var rotationPhase: CGFloat = 0
    private var mailTickCount = 0
    private var mailPending = false
    private var mailSyncing = false
    private var mailStatus = "—"
    /// iMessage bridge state. Polled faster than mail (a chat wants low
    /// latency) but still off-main; the helper itself rings the bell when a
    /// NEW inbound arrives. `imsgUnread` is exposed so the theme-by-status
    /// pipeline can treat "she texted" as a first-class status accent.
    private var imsgPending = false
    private var imsgStatus = "—"
    private var imsgConfigured = false
    private var imsgUnread = 0
    private var signalPending = false
    private var signalStatus = "Signal: —"
    private var signalConfigured = false
    private var signalUnread = 0
    /// Keeps a just-arrived message visible to Slab even if Messages marks it
    /// read before our next snapshot. Unread messages keep the signal alive;
    /// this short edge pulse covers the actual arrival moment.
    private var imsgArrivalVisibleUntil = Date.distantPast
    /// Asana task state. Polled off-main on a slow cadence (tasks don't change
    /// second-to-second); the helper itself talks to the Asana REST API and
    /// the token lives only in the untracked config (see slab-public-repo PII).
    private var asanaTickCount = 29   // primed so the first tick loads tasks
    private var asanaPending = false
    private var asanaState = AsanaState()
    /// Deploy status. Polled off-main on a slow cadence via the helper, which
    /// reads Cloudflare "Workers Builds" GitHub check-runs. Repos + token live
    /// only in the untracked deploy-status config (see slab-public-repo PII).
    private var deployTickCount = 28   // offset from asana so polls don't collide
    private var deployPending = false
    private var deployState = DeployStatusState()
    private var state = StateSnapshot()
    private let passphraseServer = PassphraseServer()
    /// System-wide ⌘⌥T → re-tile agent terminals. Kept alive for the app's
    /// lifetime; unregistered in `applicationWillTerminate`.
    private var tileHotkey: GlobalHotkey?
    /// System-wide ⌘⌥S → scatter every session into tiny confetti windows.
    /// A Carbon global hotkey, not just a menu keyEquivalent: a status-bar
    /// app's menu shortcuts only fire when it's frontmost, so ⌘⌥S would
    /// otherwise be swallowed by whatever terminal has focus (see tileHotkey).
    private var scatterHotkey: GlobalHotkey?
    /// System-wide ⌃⌥⌘A → toggle Dark Mode across this host + tailscale macs.
    private var appearanceHotkey: GlobalHotkey?
    /// System-wide ⌘⌥← → ↑ ↓ → walk focus spatially across the tiled terminal
    /// wall (see WindowNav). Four hotkeys, one per arrow; held for the app's
    /// lifetime and unregistered in `applicationWillTerminate`.
    private var navHotkeys: [GlobalHotkey] = []

    /// Keeps the focused terminal's pixel frame fixed while its native ⌘+/-
    /// command changes only that window's font zoom. This watches Terminal and
    /// iTerm2 themselves, so Claude and Codex windows behave identically.
    private var terminalFontZoomGuard: TerminalFontZoomGuard?

    /// ⌃⌃ → magnify the window under the pointer. Not a `GlobalHotkey`: Carbon
    /// can only register a keycode+modifier chord, and a bare modifier tapped
    /// twice isn't one. See CtrlDoubleTap.
    private var zoomLensTap: CtrlDoubleTap?

    /// Macs (beyond this host) to flip when going dark/light. ssh aliases that
    /// resolve on the LAN/tailnet; unreachable ones are skipped silently.
    private static let appearanceHosts = ["panda", "chicken", "blueberry"]

    func applicationDidFinishLaunching(_ notification: Notification) {
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        statusItem.button?.imagePosition = .imageLeading

        // Assign the menu once. `menuNeedsUpdate(_:)` fills it the instant
        // before each open, so it's always fresh without a timer ever
        // mutating it while it's on screen.
        menu.autoenablesItems = false
        menu.delegate = self
        statusItem.menu = menu

        do {
            try passphraseServer.start()
        } catch {
            NSLog("slab passphrase server failed to start: \(error)")
        }

        refresh()
        let timer = Timer.scheduledTimer(withTimeInterval: 2.0, repeats: true) { [weak self] _ in
            self?.refresh()
        }
        refreshTimer = timer
        RunLoop.main.add(timer, forMode: .common)

        // Contact awareness has its own lightweight clock instead of riding
        // the fleet refresh. Poll once at launch, then every three seconds.
        refreshImsgCount()
        refreshSignalCount()
        let contactTimer = Timer.scheduledTimer(withTimeInterval: 3.0, repeats: true) {
            [weak self] _ in
            self?.refreshImsgCount()
            self?.refreshSignalCount()
        }
        imsgTimer = contactTimer
        RunLoop.main.add(contactTimer, forMode: .common)

        // Title-component hygiene for the Slab-* Terminal profiles: the
        // working-dir and active-process checkboxes are NOT scriptable and
        // a running Terminal serves profiles from memory (and clobbers
        // external plist writes on its next save). The only safe window is
        // while Terminal is down — patch at launch if it isn't running,
        // and again every time it quits.
        NSWorkspace.shared.notificationCenter.addObserver(
            self, selector: #selector(appDidTerminate(_:)),
            name: NSWorkspace.didTerminateApplicationNotification, object: nil)
        if NSRunningApplication.runningApplications(
            withBundleIdentifier: "com.apple.Terminal").isEmpty {
            DispatchQueue.global(qos: .utility).async {
                Self.patchTerminalTitleComponents()
            }
        }

        // Global ⌘⌥T re-tiles claude terminals — same payload as the menu's
        // "Tile now" item, no need to open the dropdown.
        let hotkey = GlobalHotkey { [weak self] in self?.tileNow() }
        if hotkey.register(keyCode: UInt32(kVK_ANSI_T),
                           modifiers: UInt32(cmdKey | optionKey)) {
            tileHotkey = hotkey
        }

        // Global ⌘⌥S scatters the session windows — same payload as the menu's
        // "Scatter now" item. Distinct id (3) so it has its own hotkey slot.
        let scatterHK = GlobalHotkey(id: 3) { [weak self] in self?.scatterNow() }
        if scatterHK.register(keyCode: UInt32(kVK_ANSI_S),
                              modifiers: UInt32(cmdKey | optionKey)) {
            scatterHotkey = scatterHK
        }

        // Global ⌃⌥⌘A toggles Dark Mode on this host + the tailscale macs.
        // Distinct id so it can't collide with the tiling hotkey's slot.
        let appHotkey = GlobalHotkey(id: 2) { [weak self] in self?.toggleAppearance() }
        if appHotkey.register(keyCode: UInt32(kVK_ANSI_A),
                              modifiers: UInt32(controlKey | optionKey | cmdKey)) {
            appearanceHotkey = appHotkey
        }

        // Global ⌘⌥←/→/↑/↓ walk focus across the tiled terminal wall spatially:
        // each arrow raises the Claude terminal window in that direction. ⌘⌥
        // (not bare ⌥) matches the tile/scatter family and leaves Option+arrow
        // word-navigation intact. Distinct ids (4–7) so each arrow owns its slot.
        let navBindings: [(UInt32, UInt32, WindowNav.Direction)] = [
            (4, UInt32(kVK_LeftArrow),  .left),
            (5, UInt32(kVK_RightArrow), .right),
            (6, UInt32(kVK_UpArrow),    .up),
            (7, UInt32(kVK_DownArrow),  .down),
        ]
        for (hkId, key, dir) in navBindings {
            let hk = GlobalHotkey(id: hkId) { WindowNav.jump(dir) }
            if hk.register(keyCode: key, modifiers: UInt32(cmdKey | optionKey)) {
                navHotkeys.append(hk)
            }
        }

        // Terminal.app sizes windows in character cells, so its native font
        // zoom also changes the pixel frame. Preserve the frame around that
        // native action; the terminal remains responsible for its per-window
        // zoom state, and Slab only prevents the geometry side effect.
        let fontGuard = TerminalFontZoomGuard()
        if fontGuard.start() { terminalFontZoomGuard = fontGuard }

        // ⌃⌃ zooms in on the window under the pointer; moving onto another
        // window follows and reframes it; ⌃⌃ again zooms back out.
        // The tap listens always — the flag is checked at fire time, not here, so
        // toggling the feature from the menu doesn't need to tear a tap down.
        let lensTap = CtrlDoubleTap(
            onDoubleTap: { [weak self] in
                guard let self = self, self.state.zoomLens else { return }
                ZoomLens.toggle()
            },
            onPointerMove: { [weak self] point in
                guard let self = self, self.state.zoomLens else { return }
                ZoomLens.followCursor(to: point)
            })
        if lensTap.start() { zoomLensTap = lensTap }

        // setDesktopImageURL only writes the wallpaper on the active Space of
        // each screen — macOS gives every Space its own wallpaper slot. To
        // keep all Spaces synced to the current tint we re-apply on each
        // Space switch; the memo gate is dropped so the same color re-paints
        // the newly visible Space.
        NSWorkspace.shared.notificationCenter.addObserver(
            self,
            selector: #selector(activeSpaceDidChange),
            name: NSWorkspace.activeSpaceDidChangeNotification,
            object: nil)

        // Fleet "frame" capture: watch for an SSH-dropped request file and
        // answer with pixels + OCR + AX + window state. Only spins a file
        // watcher — never touches ScreenCaptureKit until a frame is actually
        // requested, so no Screen Recording prompt at launch (lazy grant).
        FrameCapture.shared.start()

        // `reel` — the moving-picture sibling of frame. Same lazy-grant file
        // watcher; hardware-encodes an SCStream straight to mp4 when asked.
        if #available(macOS 15.0, *) { ScreenRecord.shared.start() }

        // Advertised ledger: serve this machine's handles over the tailnet and
        // cache peers' ledgers, so `host:name` references resolve O(1) without
        // an SSH crawl. Overlay stays local — this is a data channel only.
        LedgerStore.shared.start()
    }

    func applicationWillTerminate(_ notification: Notification) {
        tileHotkey?.unregister()
        scatterHotkey?.unregister()
        appearanceHotkey?.unregister()
        navHotkeys.forEach { $0.unregister() }
        terminalFontZoomGuard?.stop()
        imsgTimer?.invalidate()
        zoomLensTap?.stop()
        // Compositor zoom outlives us — never quit leaving the screen magnified.
        if ZoomLens.isZoomed { ZoomLens.zoomOut() }
        passphraseServer.stop()
        LedgerStore.shared.stop()
        NSWorkspace.shared.notificationCenter.removeObserver(self)
    }

    @objc private func activeSpaceDidChange(_ note: Notification) {
        // Force a re-paint of the now-visible Space (per-screen). Clearing
        // the memo is enough: applyDesktopTint takes both branches (tint or
        // restore) from `lastDesktopTint`, so nil-ing it covers either.
        lastDesktopTint = nil
        applyDesktopTint()
    }

    // MARK: - Refresh

    private func refresh() {
        // `StateSnapshot.gather()` forks `ioreg` / `pmset` / `tailscale` and
        // blocks on each — never on the main thread, or the menu can't open
        // and submenu tracking stalls while a tick is in flight. Gather on a
        // background queue; touch UI only back on main.
        if gathering { return }
        gathering = true
        DispatchQueue.global(qos: .utility).async { [weak self] in
            guard let self = self else { return }
            var snapshot = StateSnapshot.gather()
            // Resolve each session's wallpaper here (off-main): only instant
            // cache probes + fire-and-forget gen kicks, so the main thread
            // never waits on node/network (see slab-menubar-perf).
            snapshot.claudeSessions = self.resolveWallpapers(snapshot.claudeSessions)
            // Sticky per-session title emoji — assigned here (off-main,
            // serialized by the `gathering` guard) so decor + menu read one
            // consistent mark.
            snapshot.claudeSessions = TitleEmoji.assign(snapshot.claudeSessions)
            // Publish this machine's ledger + refresh the peer cache (throttled
            // inside; peer GETs are async URLSession — never block this queue).
            LedgerStore.shared.tick(sessions: snapshot.claudeSessions,
                                    peers: snapshot.tailnetPeers)
            // RENDERING overlay — a session whose turn is done but whose
            // launched render (a ~/.ac-pop-renders heartbeat tagged with its
            // sessionId) is still running shows pink `rendering` instead of
            // idle slate, so a backgrounded build never reads as "not
            // working on anything". Attention states (awaiting/interrupted)
            // still win; a genuinely working session stays green.
            let renderSids = Set(snapshot.popRenders.map(\.sessionId).filter { !$0.isEmpty })
            if !renderSids.isEmpty {
                snapshot.claudeSessions = snapshot.claudeSessions.map { s in
                    var s = s
                    if s.state == .complete, renderSids.contains(s.sessionId) {
                        s.state = .rendering
                    }
                    return s
                }
            }
            DispatchQueue.main.async {
                self.gathering = false
                self.state = snapshot
                // gather() doesn't know about iMessage; fold the cached poll
                // result in here so the icon + decor read one consistent
                // picture. This awareness is independent of theme-by-status:
                // the menubar should notice an arrival even on an unthemed
                // wall. Unread keeps it present; the edge pulse catches a
                // message that another device marks read almost immediately.
                self.applyInputNotificationState()

                self.updateIcon()
                self.updateAnimTimer()
                self.updateBlinkTimer()

                self.mailTickCount += 1
                if self.mailTickCount >= 15 && !self.mailPending && !self.mailSyncing {
                    self.mailTickCount = 0
                    self.refreshMailCount()
                }

                // Asana tasks change on a human cadence, not a chat one —
                // poll every ~30 ticks (the REST round-trip is the cost here).
                self.asanaTickCount += 1
                if self.asanaTickCount >= 30 && !self.asanaPending {
                    self.asanaTickCount = 0
                    self.refreshAsana()
                }

                // Deploy status — changes on a merge cadence; poll ~every 30
                // ticks via the helper (GitHub check-runs round-trip).
                self.deployTickCount += 1
                if self.deployTickCount >= 30 && !self.deployPending {
                    self.deployTickCount = 0
                    self.refreshDeploy()
                }

                // No menu rebuild here — it's lazy via menuNeedsUpdate(_:).
                // Consume any "open this PDF / video" asks (tiny main-thread
                // stats; see PdfViewer.swift / VideoViewer.swift contracts).
                // The lookup hands each viewer chip the sticky TitleEmoji of
                // the Claude session that asked (state is main-thread here).
                let emojiFor: (String) -> String = { sid in
                    self.state.claudeSessions.first(where: { $0.sessionId == sid })?.emoji ?? ""
                }
                PdfViewer.shared.consumeRequests(emojiFor: emojiFor)
                VideoViewer.shared.consumeRequests(emojiFor: emojiFor)
                // Auto-tile: when the number of AC Electron preview windows
                // changes (a slab-web preview opened or closed), re-pack the
                // grid so the buffer fits in with the terminals — no manual
                // ⌘⌥T. Only meaningful when AX-trusted (else count stays 0).
                if AXTiler.trusted {
                    let acCount = AXTiler.windows(bundleId: "computer.aesthetic.app",
                                                  requireStandardSubrole: false).count
                    if acCount != self.lastAcWindowCount {
                        self.lastAcWindowCount = acCount
                        self.tileNowImpl(resetZoom: true)
                    }
                }
                // Pulled `frame` screenshots open in FramePreview, badged with
                // the source machine name (see FramePreview.swift / `frame
                // --preview`). Machine identity leads, so no session emoji here.
                FramePreview.shared.consumeRequests()
                // Groups of images (e.g. App Store screenshots) open as a tiled
                // wall of chromeless glass panels — Preview-free review. See
                // ImageGroupPreview.swift / `slab-images`.
                ImageGroupPreview.shared.consumeRequests()
                // Groups of audio files open as a tiled jukebox wall — cover
                // art + metadata + waveform, one tile playing at a time. See
                // AudioGroupPreview.swift / `slab-audio`.
                AudioGroupPreview.shared.consumeRequests(emojiFor: emojiFor)
                self.applyTerminalDecor()
                self.applyDesktopTint()
                // Per-prompt sigil badges — one little hashed star pinned to
                // each session's terminal top-right, so prompts are
                // distinguishable at the per-window grain (theme-by-status
                // colours the window; this shapes the prompt). Membership +
                // re-render happen here; the controller's own timer keeps each
                // badge glued to its window between refreshes.
                PromptSigilOverlayController.shared.sync(
                    sessions: self.state.claudeSessions,
                    enabled: self.state.promptSigils)
            }
        }
    }

    // MARK: - NSMenuDelegate

    /// Rebuild the menu contents right before it displays, from the most
    /// recent cached snapshot. Pure in-memory work (sub-millisecond), so the
    /// dropdown pops instantly and stays smooth while tracking.
    func menuNeedsUpdate(_ menu: NSMenu) {
        guard menu === self.menu else { return }
        MenuBuilder.populate(
            menu,
            state: state,
            mailStatus: mailStatus,
            imsgStatus: imsgStatus,
            imsgConfigured: imsgConfigured,
            signalStatus: signalStatus,
            signalConfigured: signalConfigured,
            asana: asanaState,
            deploy: deployState,
            target: self
        )
    }

    /// Once the menu is on screen, tick the pop-render progress bars
    /// live. NSMenu's default mode pauses Timers (and `menuNeedsUpdate`
    /// only fires once per open), so we add this one to `.common` modes
    /// and rewrite titles in place rather than restructuring the menu.
    func menuWillOpen(_ menu: NSMenu) {
        guard menu === self.menu else { return }
        popRenderTimer?.invalidate()
        guard !state.popRenders.isEmpty else { return }
        let t = Timer(timeInterval: 0.5, repeats: true) { [weak self] _ in
            self?.tickPopRenders()
        }
        popRenderTimer = t
        RunLoop.main.add(t, forMode: .common)
    }

    func menuDidClose(_ menu: NSMenu) {
        guard menu === self.menu else { return }
        popRenderTimer?.invalidate()
        popRenderTimer = nil
    }

    private func tickPopRenders() {
        // Cheap pop-only refresh: skip the full gather() (ioreg / pmset /
        // tailscale) and just re-read the heartbeat dir. Safe on main —
        // a few KB of JSON in a tiny directory.
        let fresh = StateSnapshot.readPopRenders()
        state.popRenders = fresh
        MenuBuilder.updatePopRenders(in: menu, state: state)
    }

    private func updateIcon() {
        guard let button = statusItem.button else { return }
        // Don't tint our color image — contentTintColor would otherwise
        // recolor non-template images on macOS 10.14+.
        button.contentTintColor = nil
        button.image = IconRenderer.image(for: state, phase: rainbowPhase, rotation: rotationPhase)
        // Keep message state out of the status item so its dot/count never
        // obscures or stretches the Slab bars. It remains visible in the menu.
        // Agent counts stay separate with a `+`.
        var tails: [String] = []
        if state.claudeSessions.isEmpty && state.totalActive > 0 {
            tails.append("+\(state.totalActive)")
        } else if state.activeSubagents > 0 {
            tails.append("+\(state.activeSubagents)")
        }
        button.title = tails.isEmpty ? "" : " " + tails.joined(separator: " ")
    }

    private func updateAnimTimer() {
        // Run the animation timer whenever the polygon icon is showing, so
        // we can drive both the active-session rotation/pulse and the
        // all-stale blink. Rotation only advances while at least one
        // session is non-stale; pulse phase always advances (it drives both
        // awaiting brightness and stale blink). When the polygon goes away
        // entirely, stop the timer and reset phases.
        if !state.claudeSessions.isEmpty || state.idleResting {
            if animTimer == nil {
                let t = Timer.scheduledTimer(withTimeInterval: 0.08, repeats: true) { [weak self] _ in
                    guard let self = self else { return }
                    // Idle rests at a gentle hue flow; active states cycle
                    // faster (the awaiting throb reads off this phase).
                    let rainbowStep: CGFloat = self.state.idleResting ? 0.006 : 0.025
                    self.rainbowPhase = (self.rainbowPhase + rainbowStep).truncatingRemainder(dividingBy: 1.0)
                    if self.state.anyActive {
                        let rotSpeed = 0.004 + 0.012 * CGFloat(self.state.awaitingCount)
                        self.rotationPhase = (self.rotationPhase + rotSpeed)
                            .truncatingRemainder(dividingBy: .pi * 2)
                    } else if self.state.idleResting {
                        // Slow spin — ~50 s per revolution. Enticing, not busy.
                        self.rotationPhase = (self.rotationPhase + 0.010)
                            .truncatingRemainder(dividingBy: .pi * 2)
                    }
                    self.updateIcon()
                }
                RunLoop.main.add(t, forMode: .common)
                animTimer = t
            }
        } else if let t = animTimer {
            t.invalidate()
            animTimer = nil
            rainbowPhase = 0
            rotationPhase = 0
        }
    }

    /// Start a slow blink (0.6 s toggle → 1.2 s period) whenever any session
    /// sits in an attention state (`.complete` / `.awaiting`). Each tick
    /// flips `blinkPhase` and re-applies decor — `applyTerminalDecor` folds
    /// the phase into the dedup key only for attention states, so other
    /// sessions' osascripts don't re-fire. Stop the timer when no attention
    /// state remains, snapping the palette back to its base.
    private func updateBlinkTimer() {
        // Disabled for now — too distracting on the wall. Re-enable by
        // flipping this to `true`. statusDecor still accepts a `blink` arg
        // so this only needs to come back on at the timer level.
        let blinkEnabled = false
        let needsBlink = blinkEnabled && state.themeByStatus && state.claudeSessions.contains {
            $0.state == .complete || $0.state == .awaiting || $0.state == .interrupted
        }
        if needsBlink {
            if blinkTimer == nil {
                let t = Timer.scheduledTimer(withTimeInterval: 0.6, repeats: true) { [weak self] _ in
                    guard let self = self else { return }
                    self.blinkPhase.toggle()
                    self.applyTerminalDecor()
                }
                RunLoop.main.add(t, forMode: .common)
                blinkTimer = t
            }
        } else if let t = blinkTimer {
            t.invalidate()
            blinkTimer = nil
            if blinkPhase {
                blinkPhase = false
                applyTerminalDecor()
            }
        }
    }

    private func refreshMailCount() {
        guard let mu = Tools.resolve("mu") else {
            mailStatus = "mu not found"
            return
        }
        mailPending = true
        DispatchQueue.global(qos: .utility).async { [weak self] in
            let out = ShellRunner.output(mu, args: ["find", "flag:unread", "AND", "NOT", "flag:trashed"], timeout: 5) ?? ""
            let count = out.split(separator: "\n").filter { !$0.isEmpty }.count
            let status = count > 0 ? "\(count) unread" : "no unread"
            DispatchQueue.main.async {
                self?.mailStatus = status
                self?.mailPending = false
            }
        }
    }

    /// Pull the iMessage summary off-main. The helper always exits 0 for
    /// `status`, prints one JSON line, and rings the bell itself when a NEW
    /// inbound arrives — so this never blocks the tick and the alert fires
    /// even while the menu is closed.
    private func refreshImsgCount() {
        let helper = Paths.imsgHelper
        guard FileManager.default.isExecutableFile(atPath: helper) else {
            imsgStatus = "iMessage: helper missing"
            imsgConfigured = false
            imsgUnread = 0
            return
        }
        imsgPending = true
        DispatchQueue.global(qos: .utility).async { [weak self] in
            let out = ShellRunner.run(helper, args: ["status"], timeout: 8).output
            let line = out.split(separator: "\n").last.map(String.init) ?? ""
            var label = "iMessage: —"
            var configured = false
            var unread = 0
            var newSinceLast = false
            var helperError: String?
            var lastText = ""
            if let data = line.data(using: .utf8),
               let obj = try? JSONSerialization.jsonObject(with: data)
                   as? [String: Any] {
                label = (obj["label"] as? String) ?? label
                configured = (obj["configured"] as? Bool) ?? false
                unread = (obj["unread"] as? Int) ?? 0
                newSinceLast = (obj["newSinceLast"] as? Bool) ?? false
                helperError = obj["error"] as? String
                if let last = obj["last"] as? [String: Any] {
                    lastText = (last["text"] as? String) ?? ""
                }
            }
            DispatchQueue.main.async {
                guard let self = self else { return }
                if let helperError = helperError {
                    NSLog("💬 [imsg] watcher error: \(helperError)")
                }
                let wasWaiting = self.state.messageWaiting
                self.imsgStatus = label
                self.imsgConfigured = configured
                self.imsgUnread = unread
                if newSinceLast {
                    self.imsgArrivalVisibleUntil = Date().addingTimeInterval(15)
                    self.bumpBoundProx(displayLabel: label, message: lastText)
                }
                self.applyInputNotificationState()
                self.imsgPending = false
                self.updateIcon()
                self.updateAnimTimer()
                if wasWaiting != self.state.messageWaiting {
                    self.applyTerminalDecor()
                }
            }
        }
    }

    /// Signal mirrors the iMessage poll, but keeps its own ingestion cursor.
    /// The icon folds both counts together while the menu preserves the
    /// per-channel breakdown.
    private func refreshSignalCount() {
        let helper = Paths.signalHelper
        guard FileManager.default.isExecutableFile(atPath: helper) else {
            signalStatus = "Signal: helper missing"
            signalConfigured = false
            signalUnread = 0
            applyInputNotificationState()
            return
        }
        guard !signalPending else { return }
        signalPending = true
        DispatchQueue.global(qos: .utility).async { [weak self] in
            let out = ShellRunner.run(helper, args: ["status"], timeout: 8).output
            let line = out.split(separator: "\n").last.map(String.init) ?? ""
            var label = "Signal: —"
            var configured = false
            var unread = 0
            var newSinceLast = false
            var helperError: String?
            if let data = line.data(using: .utf8),
               let obj = try? JSONSerialization.jsonObject(with: data)
                   as? [String: Any] {
                label = (obj["label"] as? String) ?? label
                configured = (obj["configured"] as? Bool) ?? false
                unread = (obj["unread"] as? Int) ?? 0
                newSinceLast = (obj["newSinceLast"] as? Bool) ?? false
                helperError = obj["error"] as? String
            }
            DispatchQueue.main.async {
                guard let self = self else { return }
                if let helperError = helperError {
                    NSLog("💬 [signal] watcher error: \(helperError)")
                }
                let wasWaiting = self.state.messageWaiting
                self.signalStatus = label
                self.signalConfigured = configured
                self.signalUnread = unread
                if newSinceLast {
                    self.imsgArrivalVisibleUntil = Date().addingTimeInterval(15)
                }
                self.applyInputNotificationState()
                self.signalPending = false
                self.updateIcon()
                self.updateAnimTimer()
                if wasWaiting != self.state.messageWaiting {
                    self.applyTerminalDecor()
                }
            }
        }
    }

    private func applyInputNotificationState() {
        state.inputNotificationCount = imsgUnread + signalUnread
        state.messageWaiting = state.inputNotificationCount > 0
            || Date() < imsgArrivalVisibleUntil
    }

    /// Poke the prox explicitly assigned to iMessage awareness and optionally
    /// submit a small steering prompt to its live TTY. This is deliberately
    /// opt-in via an untracked binding file; Slab never guesses which agent to
    /// wake and never sends a message back to the contact.
    private func bumpBoundProx(displayLabel: String, message: String) {
        guard let data = FileManager.default.contents(atPath: Paths.imsgProxBinding),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let sid = obj["sessionId"] as? String, !sid.isEmpty else { return }
        let wake = (obj["wake"] as? Bool) ?? false
        LedgerStore.shared.pokeLocal(sessionId: sid, by: "slab:imessage")
        guard wake, let tty = ttyForSession(sid), !tty.isEmpty else { return }

        let clean = message.replacingOccurrences(of: "\n", with: " ")
            .trimmingCharacters(in: .whitespacesAndNewlines)
        let excerpt = String(clean.prefix(240))
        let prompt = excerpt.isEmpty
            ? "Slab received a new iMessage from Alex. Check Alex's latest messages and handle the request."
            : "Slab received a new iMessage from Alex: \(excerpt) — check Alex's latest messages and handle the request."
        wakeTerminal(tty: tty, prompt: prompt)
        NSLog("💬 [imsg] poked + woke prox \(sid.prefix(8)) on \(tty) (\(displayLabel))")
    }

    private func ttyForSession(_ sid: String) -> String? {
        if let tty = state.claudeSessions.first(where: { $0.sessionId == sid })?.tty,
           !tty.isEmpty { return tty }
        for dir in [Paths.activePromptsDir, Paths.awaitingPromptsDir] {
            let path = "\(dir)/\(sid)"
            guard let data = FileManager.default.contents(atPath: path),
                  let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let tty = obj["tty"] as? String, !tty.isEmpty else { continue }
            return tty
        }
        return nil
    }

    private func wakeTerminal(tty: String, prompt: String) {
        func esc(_ s: String) -> String {
            s.replacingOccurrences(of: "\\", with: "\\\\")
                .replacingOccurrences(of: "\"", with: "\\\"")
        }
        let t = esc((tty as NSString).lastPathComponent)
        let p = esc(prompt)
        let script = """
        tell application "Terminal"
            repeat with w in windows
                repeat with tabRef in tabs of w
                    try
                        if (tty of tabRef) ends with "\(t)" then
                            do script "\(p)" in tabRef
                            return "terminal"
                        end if
                    end try
                end repeat
            end repeat
        end tell
        try
            tell application id "com.googlecode.iterm2"
                repeat with w in windows
                    repeat with tabRef in tabs of w
                        repeat with sessionRef in sessions of tabRef
                            if (tty of sessionRef) ends with "\(t)" then
                                tell sessionRef to write text "\(p)"
                                return "iterm"
                            end if
                        end repeat
                    end repeat
                end repeat
            end tell
        end try
        return "tty-not-found"
        """
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    /// Pull the Asana task tree off-main via `slab/bin/asana status`. The
    /// helper always exits 0 and prints one JSON line: `{configured, label,
    /// projects:[{name, tasks:[{name,url,due,overdue,today}]}]}`. We decode it
    /// into `asanaState` for the lazy menu rebuild. Slow cadence, so the 8 s
    /// timeout is comfortable.
    private func refreshAsana() {
        let helper = Paths.asanaHelper
        guard FileManager.default.isExecutableFile(atPath: helper) else {
            asanaState = AsanaState(configured: false, label: "Asana: helper missing")
            return
        }
        asanaPending = true
        DispatchQueue.global(qos: .utility).async { [weak self] in
            let out = ShellRunner.run(helper, args: ["status"], timeout: 12).output
            let line = out.split(separator: "\n").last.map(String.init) ?? ""
            let parsed = Self.parseAsana(line)
            DispatchQueue.main.async {
                self?.asanaState = parsed
                self?.asanaPending = false
            }
        }
    }

    /// Decode one JSON status line into an AsanaState. Defensive: any missing
    /// field collapses to an empty/unconfigured state rather than throwing.
    private static func parseAsana(_ line: String) -> AsanaState {
        guard let data = line.data(using: .utf8),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else { return AsanaState(configured: false, label: "Asana: —") }
        var s = AsanaState()
        s.configured = (obj["configured"] as? Bool) ?? false
        s.label = (obj["label"] as? String) ?? s.label
        s.user = (obj["user"] as? String) ?? ""
        s.count = (obj["count"] as? Int) ?? 0
        let projects = (obj["projects"] as? [[String: Any]]) ?? []
        s.projects = projects.map { p in
            let tasks = (p["tasks"] as? [[String: Any]]) ?? []
            return AsanaProject(
                name: (p["name"] as? String) ?? "Project",
                tasks: tasks.map { t in
                    AsanaTask(
                        name: (t["name"] as? String) ?? "(untitled)",
                        url: (t["url"] as? String) ?? "",
                        due: (t["due"] as? String) ?? "",
                        overdue: (t["overdue"] as? Bool) ?? false,
                        today: (t["today"] as? Bool) ?? false)
                })
        }
        return s
    }

    // MARK: - Menu actions

    /// Force an immediate Asana re-poll from the submenu's "Refresh now".
    @objc func refreshAsanaNow() {
        asanaTickCount = 0
        if !asanaPending { refreshAsana() }
    }

    /// Open Asana "My Tasks" in the browser.
    @objc func openAsana() {
        ShellRunner.runAsync(Paths.asanaHelper, args: ["open"])
    }

    /// Open one task's permalink (carried on the menu item's representedObject).
    @objc func openAsanaTask(_ sender: NSMenuItem) {
        guard let url = sender.representedObject as? String, !url.isEmpty,
              let u = URL(string: url) else { return }
        NSWorkspace.shared.open(u)
    }

    /// OVERTIME toggle — third surface for the same flag file the badge's
    /// right-click and the remote `overtime … on` flip. Raising the flag
    /// kickstarts the worker LaunchAgent so pickup is immediate instead of
    /// waiting out its StartInterval.
    @objc func toggleOvertime() {
        let fm = FileManager.default
        if fm.fileExists(atPath: Paths.overtimeFlag) {
            try? fm.removeItem(atPath: Paths.overtimeFlag)
            try? fm.removeItem(atPath: Paths.overtimeStatus)
        } else {
            fm.createFile(atPath: Paths.overtimeFlag, contents: nil)
            try? "idle — overtime on".write(toFile: Paths.overtimeStatus,
                                            atomically: true, encoding: .utf8)
            ShellRunner.runShellAsync(
                "launchctl kickstart gui/$(id -u)/computer.aesthetic.overtimeworker")
        }
    }

    /// Ensure the (untracked) Asana config exists, then open it for editing.
    @objc func openAsanaConfig() {
        ShellRunner.runAsync(Paths.asanaHelper, args: ["config"])
        let path = Paths.asanaConfig
        // Give the helper a beat to drop the stub, then open in the editor.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
            NSWorkspace.shared.open(URL(fileURLWithPath: path))
        }
    }

    /// Pull deploy status off-main via `slab/bin/deploy-status status`. The
    /// helper exits 0 with one JSON line: `{configured,label,target,envs:[…]}`
    /// where each env carries its rolled-up state + per-worker apps. We decode
    /// it into `deployState` for the lazy menu rebuild.
    private func refreshDeploy() {
        let helper = Paths.deployStatusHelper
        guard FileManager.default.isExecutableFile(atPath: helper) else {
            deployState = DeployStatusState(configured: false, label: "Deploy: helper missing")
            return
        }
        deployPending = true
        DispatchQueue.global(qos: .utility).async { [weak self] in
            let out = ShellRunner.run(helper, args: ["status"], timeout: 15).output
            let line = out.split(separator: "\n").last.map(String.init) ?? ""
            let parsed = Self.parseDeploy(line)
            DispatchQueue.main.async {
                self?.deployState = parsed
                self?.deployPending = false
            }
        }
    }

    /// Decode one JSON status line into a DeployStatusState. Defensive: any
    /// missing field collapses to an empty/unconfigured state, never throws.
    private static func parseDeploy(_ line: String) -> DeployStatusState {
        guard let data = line.data(using: .utf8),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else { return DeployStatusState(configured: false, label: "Deploy: —") }
        var s = DeployStatusState()
        s.configured = (obj["configured"] as? Bool) ?? false
        s.label = (obj["label"] as? String) ?? s.label
        s.target = (obj["target"] as? String) ?? ""
        s.url = (obj["url"] as? String) ?? ""
        let envs = (obj["envs"] as? [[String: Any]]) ?? []
        s.envs = envs.map { e in
            let apps = (e["apps"] as? [[String: Any]]) ?? []
            return DeployEnv(
                env: (e["env"] as? String) ?? "?",
                state: (e["state"] as? String) ?? "none",
                sha: (e["sha"] as? String) ?? "",
                message: (e["message"] as? String) ?? "",
                apps: apps.map { a in
                    DeployApp(
                        name: (a["name"] as? String) ?? "?",
                        state: (a["state"] as? String) ?? "none",
                        url: (a["url"] as? String) ?? "")
                },
                url: (e["url"] as? String) ?? "")
        }
        return s
    }

    /// Force an immediate deploy re-poll from the submenu's "Refresh now".
    @objc func refreshDeployNow() {
        deployTickCount = 0
        if !deployPending { refreshDeploy() }
    }

    /// Open the commit / build log carried on the item's representedObject
    /// (falls back to the repo URL).
    @objc func openDeploy(_ sender: NSMenuItem) {
        let url = (sender.representedObject as? String) ?? deployState.url
        guard !url.isEmpty, let u = URL(string: url) else { return }
        NSWorkspace.shared.open(u)
    }

    /// Ensure the (untracked) deploy-status config exists, then open it.
    @objc func openDeployConfig() {
        ShellRunner.runAsync(Paths.deployStatusHelper, args: ["config"])
        let path = Paths.deployStatusConfig
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
            NSWorkspace.shared.open(URL(fileURLWithPath: path))
        }
    }

    @objc func openImsg() {
        ShellRunner.runAsync(Paths.imsgHelper, args: ["open"])
    }

    /// Open a fresh iTerm2 window running the dependency-free live client.
    /// `exec` replaces the shell so closing the pane just ends the client.
    @objc func openImsgTail() {
        let helper = Paths.imsgHelper
        let script = """
        tell application id "com.googlecode.iterm2"
            activate
            create window with default profile
            tell current session of current window to write text "exec '\(helper)' tail"
        end tell
        """
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    @objc func replyImsg() {
        let alert = NSAlert()
        alert.messageText = "Reply"
        alert.informativeText = "Send an iMessage to your configured contact."
        alert.addButton(withTitle: "Send")
        alert.addButton(withTitle: "Cancel")
        let field = NSTextField(frame: NSRect(x: 0, y: 0, width: 280, height: 24))
        alert.accessoryView = field
        NSApp.activate(ignoringOtherApps: true)
        alert.window.initialFirstResponder = field
        guard alert.runModal() == .alertFirstButtonReturn else { return }
        let text = field.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        ShellRunner.runAsync(Paths.imsgHelper, args: ["send", text])
    }

    @objc func openImsgConfig() {
        // `config` creates the untracked stub if absent, then open it.
        ShellRunner.run(Paths.imsgHelper, args: ["config"])
        ShellRunner.run("/usr/bin/open", args: ["-t", Paths.imsgConfig])
    }

    @objc func acknowledgeImsg() {
        ShellRunner.runAsync(Paths.imsgHelper, args: ["ack"]) { [weak self] in
            DispatchQueue.main.async { self?.refreshImsgCount() }
        }
    }

    @objc func openSignal() {
        ShellRunner.runAsync(Paths.signalHelper, args: ["open"])
    }

    @objc func openSignalConfig() {
        ShellRunner.run(Paths.signalHelper, args: ["config"])
        ShellRunner.run("/usr/bin/open", args: ["-t", Paths.signalConfig])
    }

    @objc func acknowledgeSignal() {
        ShellRunner.runAsync(Paths.signalHelper, args: ["ack"]) { [weak self] in
            DispatchQueue.main.async { self?.refreshSignalCount() }
        }
    }

    // MARK: - Deskflow KVM

    /// Read the launchd agent label from the untracked deskflow config.
    /// Machine role/label/agent live there, never in tracked code.
    private func deskflowAgent() -> String? {
        guard let data = FileManager.default.contents(atPath: Paths.deskflowConfig),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let agent = obj["agent"] as? String, !agent.isEmpty
        else { return nil }
        return agent
    }

    private func deskflowPlistPath(_ agent: String) -> String {
        "\(Paths.home)/Library/LaunchAgents/\(agent).plist"
    }

    @objc func deskflowStart() {
        guard let agent = deskflowAgent() else { return }
        ShellRunner.runAsync("/bin/launchctl",
                             args: ["bootstrap", "gui/\(getuid())", deskflowPlistPath(agent)])
    }

    @objc func deskflowStop() {
        guard let agent = deskflowAgent() else { return }
        ShellRunner.runAsync("/bin/launchctl", args: ["bootout", "gui/\(getuid())/\(agent)"])
    }

    @objc func deskflowRestart() {
        guard let agent = deskflowAgent() else { return }
        // bootstrap first in case it isn't loaded, then kickstart -k to
        // force a fresh respawn of the managed process.
        let cmd = "/bin/launchctl bootstrap gui/\(getuid()) \(deskflowPlistPath(agent)) 2>/dev/null; "
                + "/bin/launchctl kickstart -k gui/\(getuid())/\(agent)"
        ShellRunner.runShellAsync(cmd)
    }

    @objc func openDeskflowLog() {
        ShellRunner.run("/usr/bin/open", args: ["-a", "Console", Paths.deskflowLog])
    }

    @objc func openDeskflowConfig() {
        // Write an untracked stub if absent, then open it for editing.
        let path = Paths.deskflowConfig
        if !FileManager.default.fileExists(atPath: path) {
            let dir = (path as NSString).deletingLastPathComponent
            try? FileManager.default.createDirectory(
                atPath: dir, withIntermediateDirectories: true)
            let stub = """
            {
              "enabled": false,
              "role": "server",
              "label": "Deskflow",
              "agent": "computer.aesthetic.deskflow"
            }
            """
            try? stub.write(toFile: path, atomically: true, encoding: .utf8)
        }
        ShellRunner.run("/usr/bin/open", args: ["-t", path])
    }

    @objc func openDaemonLog() {
        ShellRunner.run("/usr/bin/open", args: ["-a", "Console", Paths.lidLog])
    }

    @objc func openSoundsFolder() {
        ShellRunner.run("/usr/bin/open", args: [Paths.soundsDir])
    }

    @objc func openActivityMonitor() {
        ShellRunner.run("/usr/bin/open", args: ["-a", "Activity Monitor"])
    }

    /// Open iTerm2 running the RFA ("request for audio") wizard for a /pop
    /// lane — a per-note loop that plays the pitch, shows the word, records
    /// you singing it, then recompiles the track + plays it back. It's
    /// interactive (stdin keep/redo), so it needs a real terminal; `exec`
    /// replaces the shell so closing the pane ends the session cleanly.
    @objc func requestForAudio(_ sender: NSMenuItem) {
        let lane = (sender.representedObject as? String) ?? "hum"
        let repoEsc = Paths.acRepo.replacingOccurrences(of: "'", with: "'\\''")
        let cmd = "cd '\(repoEsc)' && exec node pop/bin/rfa.mjs --track \(lane)"
        let script = """
        tell application id "com.googlecode.iterm2"
            activate
            create window with default profile
            tell current session of current window to write text "\(cmd)"
        end tell
        """
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    /// Spawn the recorder. Refresh immediately + after a short delay so the
    /// menu flips to "◉ Recording call…" without waiting for the slow tick
    /// (the state file appears within ~300ms of the script starting).
    @objc func startCall() {
        ShellRunner.runAsync(Paths.slabCallRecord, args: ["start"]) { [weak self] in
            DispatchQueue.main.async { self?.refresh() }
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4) { [weak self] in
            self?.refresh()
        }
    }

    @objc func stopCall() {
        ShellRunner.runAsync(Paths.slabCallRecord, args: ["stop"]) { [weak self] in
            DispatchQueue.main.async { self?.refresh() }
        }
    }

    /// Reveal ~/Documents/Shelf/meetings — where slab-call-record writes
    /// the raw WAVs before they get ingested into the meetings/ dir.
    @objc func openMeetingsShelf() {
        let shelf = "\(Paths.home)/Documents/Shelf/meetings"
        let fm = FileManager.default
        if !fm.fileExists(atPath: shelf) {
            try? fm.createDirectory(atPath: shelf,
                                    withIntermediateDirectories: true)
        }
        ShellRunner.run("/usr/bin/open", args: [shelf])
    }

    @objc func openMeetingsDir() {
        ShellRunner.run("/usr/bin/open", args: [Paths.meetingsDir])
    }

    /// Close the frontmost Terminal.app or iTerm2 window. Slab is an
    /// accessory app (no dock icon, no activation), so opening the menu
    /// doesn't change the frontmost application — `NSWorkspace.frontmostApplication`
    /// still reports whichever terminal the user was just in. If auto-tile
    /// is on, the remaining windows re-tile after the close.
    @objc func closeFrontTerminal() {
        let bundle = NSWorkspace.shared.frontmostApplication?.bundleIdentifier
        let script: String
        switch bundle {
        case "com.googlecode.iterm2":
            // iTerm2 uses `current window`. Saving prompt is suppressed
            // by closing without a session-end question — iTerm2 honors
            // its own "Quit when all windows are closed" pref.
            script = """
            tell application id \"com.googlecode.iterm2\"
                if (count of windows) > 0 then close current window
            end tell
            """
        case "com.apple.Terminal":
            // Terminal.app prompts if a process is still running unless
            // we ask it not to. `saving no` silences the dialog so the
            // close completes synchronously and re-tile fires cleanly.
            script = """
            tell application \"Terminal\"
                if (count of windows) > 0 then close front window saving no
            end tell
            """
        default:
            // Front app is neither terminal — no-op rather than guessing.
            notify(title: "slab", subtitle: "Close terminal",
                   body: "Frontmost app isn't Terminal or iTerm2 — nothing to close.")
            return
        }
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script]) { [weak self] in
            DispatchQueue.main.async {
                guard let self = self else { return }
                if self.state.autoTile {
                    // A tile is a normalization boundary: remaining windows
                    // return to one grid-derived font size after the close.
                    self.tileNowImpl(resetZoom: true)
                }
                self.refresh()
            }
        }
    }

    /// Tidy every Stickies note into a tight top-left stack, collapsed.
    /// Rather than hand-roll the geometry, we drive Stickies' own built-in
    /// "Window ▸ Arrange By" command: first set its "When Arranging" mode to
    /// "Collapse All" (a persistent radio choice), then invoke "Arrange By ▸
    /// Color", which packs the notes into a flush column in the top-left and
    /// collapses each to its title bar. Native, idempotent, and it leaves the
    /// notes exactly where Stickies' own "Arrange" would.
    @objc func stackStickies() {
        let script = """
        tell application "Stickies" to activate
        tell application "System Events" to tell process "Stickies"
            set abMenu to menu 1 of menu item "Arrange By" of menu 1 of menu bar item "Window" of menu bar 1
            -- Collapse-on-arrange mode (persists across runs).
            click menu item "Collapse All" of abMenu
            delay 0.2
            -- Perform the arrange — packs top-left + collapses.
            click menu item "Color" of abMenu
        end tell
        """
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    @objc func reloadDaemon() {
        DispatchQueue.global(qos: .userInitiated).async {
            ShellRunner.run("/bin/launchctl", args: ["unload", Paths.daemonPlist])
            ShellRunner.run("/bin/launchctl", args: ["load", Paths.daemonPlist])
        }
    }

    @objc func showAboutSlab() {
        SlabAboutWindow.show()
    }

    @objc func quitMenubar() {
        DispatchQueue.global(qos: .userInitiated).async {
            ShellRunner.run("/bin/launchctl", args: ["unload", Paths.menubarPlist])
            DispatchQueue.main.async {
                NSApp.terminate(nil)
            }
        }
    }

    @objc func toggleStayAwake() {
        let arg = state.sleepDisabled ? "auto" : "awake"
        ShellRunner.runAsync(Paths.claudeSleep, args: [arg]) { [weak self] in
            DispatchQueue.main.async { self?.refresh() }
        }
    }

    @objc func sleepNow() {
        ShellRunner.runAsync(Paths.claudeSleep, args: ["now"])
    }

    /// Kick off the currently-selected screen saver now (the Slab Status
    /// saver if it's chosen in System Settings). Launching ScreenSaverEngine
    /// directly is the only path that still works on macOS 26 — the old
    /// `System Events`/`start current screen saver` verb was removed.
    @objc func startScreensaver() {
        ShellRunner.runAsync(
            "/usr/bin/open",
            args: ["-a", "/System/Library/CoreServices/ScreenSaverEngine.app"]
        )
    }

    @objc func toggleMute() {
        let path = Paths.muteFlag
        let fm = FileManager.default
        if fm.fileExists(atPath: path) {
            try? fm.removeItem(atPath: path)
        } else {
            let dir = (path as NSString).deletingLastPathComponent
            try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
            fm.createFile(atPath: path, contents: nil)
            // Stop any in-flight ambient pad / chime so toggling on shuts up
            // the speakers immediately instead of waiting for the next Stop.
            // Routes through slab-fade-ambient so we only silence slab's own
            // afplay processes, not every afplay on the system.
            ShellRunner.runAsync("\(Paths.slabBin)/slab-fade-ambient",
                                 args: ["--kill-slab-afplay"])
        }
        refresh()
    }

    @objc func syncBoth() { syncMail(account: nil) }
    @objc func syncAcMail() { syncMail(account: "ac-mail") }
    @objc func syncJasMail() { syncMail(account: "jas-mail") }
    @objc func syncSotceMail() { syncMail(account: "sotce-mail") }
    @objc func syncQuiltnetMail() { syncMail(account: "quiltnet-mail") }

    /// Sync an extra mail account whose mbsync channel name is carried on the
    /// menu item's representedObject, so client account names live in untracked
    /// config (see Paths.mailAccountsConfig) rather than tracked code.
    @objc func syncMailFromMenuItem(_ sender: NSMenuItem) {
        guard let account = sender.representedObject as? String, !account.isEmpty else { return }
        syncMail(account: account)
    }

    @objc func openSyncLog() {
        if FileManager.default.fileExists(atPath: Paths.mailSyncLog) {
            ShellRunner.run("/usr/bin/open", args: ["-a", "Console", Paths.mailSyncLog])
        } else {
            notify(title: "slab", subtitle: "Mail sync", body: "No sync log yet.")
        }
    }

    @objc func focusClaudeSession(_ sender: NSMenuItem) {
        guard let tty = sender.representedObject as? String, !tty.isEmpty else { return }
        // tty here is a short name like "ttys003"; AppleScript's `tty of tab`
        // returns "/dev/ttys003", so suffix-match on the bare name.
        let escaped = tty.replacingOccurrences(of: "\"", with: "\\\"")
        // iTerm2's `tty of session` returns "/dev/ttysNNN"; we hold the bare
        // name. Selecting session→tab→window focuses the right pane and
        // raises its window; `activate` brings iTerm2 forward.
        let script = """
        tell application id "com.googlecode.iterm2"
            activate
            set targetTTY to "\(escaped)"
            repeat with w in windows
                repeat with t in tabs of w
                    repeat with s in sessions of t
                        try
                            if (tty of s) ends with targetTTY then
                                select s
                                select t
                                select w
                                return
                            end if
                        end try
                    end repeat
                end repeat
            end repeat
        end tell
        """
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    /// Menu items "All Macs → Dark/Light" — representedObject is "dark"/"light".
    @objc func setAppearance(_ sender: NSMenuItem) {
        guard let mode = sender.representedObject as? String else { return }
        applyAppearance(mode)
    }

    /// Global ⌃⌥⌘A — flip to the opposite of this host's current mode.
    @objc func toggleAppearance() {
        let isDark = UserDefaults.standard.string(forKey: "AppleInterfaceStyle") == "Dark"
        applyAppearance(isDark ? "light" : "dark")
    }

    /// Set macOS Dark Mode on this host and each reachable tailscale mac. Every
    /// call is dispatched async (no local shell), so the menu never blocks and
    /// an offline/asleep host just times out its ssh quietly.
    private func applyAppearance(_ mode: String) {
        let val = (mode == "dark") ? "true" : "false"
        let osa = "tell application \"System Events\" to tell appearance preferences to set dark mode to \(val)"
        // Local host (neo).
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", osa])
        // Remotes: pass the remote command as a single ssh arg (no local shell),
        // so the AppleScript's double quotes survive untouched on the far end.
        for host in Self.appearanceHosts {
            ShellRunner.runAsync("/usr/bin/ssh",
                                 args: ["-o", "ConnectTimeout=4", "-o", "BatchMode=yes",
                                        host, "osascript -e '\(osa)'"])
        }
    }

    @objc func sshPeer(_ sender: NSMenuItem) {
        guard let host = sender.representedObject as? String else { return }
        let safeHost = host.replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")
        let script = """
        tell application id "com.googlecode.iterm2"
            activate
            create window with default profile
            tell current session of current window to write text "ssh \(safeHost)"
        end tell
        """
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    /// Open N fresh Terminal windows, each resuming one of the most-recently-
    /// modified Claude sessions on disk that isn't already live. Built for
    /// the "Terminal.app crashed and took every Claude with it" case.
    @objc func restoreRecentThreads(_ sender: NSMenuItem) {
        guard let n = sender.representedObject as? Int, n > 0 else { return }
        let liveIds = Set(state.claudeSessions.map { $0.sessionId })
        let tile = state.autoTile
        let textSize = state.textSize
        // Snapshot screen geometry on main; NSScreen reads off-main can
        // return nil on first hit and were the silent failure mode for the
        // first auto-tile pass.
        let geom = tile ? Self.screenGeom() : nil
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            let entries = ClaudeHistoryReader.recent(limit: n, excluding: liveIds)
            if entries.isEmpty {
                DispatchQueue.main.async {
                    self?.notify(title: "slab", subtitle: "Restore threads", body: "No restorable Claude sessions found.")
                }
                return
            }
            let layout = geom.flatMap { Self.computeTileLayout(count: entries.count, geom: $0, size: textSize) }
            let term = Self.preferredTerminalApp()
            for (i, entry) in entries.enumerated() {
                let cell = layout?.cellAt(index: i)
                Self.openTerminalRunningClaude(
                    cwd: entry.cwd,
                    sessionId: entry.sessionId,
                    bounds: cell?.bounds,
                    fontSize: layout?.fontSize,
                    app: term
                )
            }
        }
    }

    /// Stop every live local agent session, then relaunch it in a fresh
    /// terminal, resuming the provider thread. This reloads current CLI config
    /// (including Codex sandbox/approval policy) without losing conversation.
    @objc func restartAllActive() {
        let sessions = state.claudeSessions.filter { !$0.isRemote }
        if sessions.isEmpty { return }
        let legacyCodexCount = sessions.filter {
            $0.agentType == "codex" && $0.providerSessionId.isEmpty
        }.count
        let alert = NSAlert()
        alert.messageText = "Refresh all local agent sessions?"
        var detail = "This will stop \(sessions.count) running Claude/Codex session\(sessions.count == 1 ? "" : "s"), reload current configuration, and resume each thread in a fresh terminal. In-flight work will be interrupted."
        if legacyCodexCount > 0 {
            detail += " \(legacyCodexCount) Codex session\(legacyCodexCount == 1 ? "" : "s") predates refresh tracking and will reopen as a new thread this once."
        }
        alert.informativeText = detail
        alert.alertStyle = .warning
        alert.addButton(withTitle: "Refresh All")
        alert.addButton(withTitle: "Cancel")
        NSApp.activate(ignoringOtherApps: true)
        guard alert.runModal() == .alertFirstButtonReturn else { return }

        // Snapshot what we need before SIGTERM races the active-prompts
        // janitor — once the pid dies, the marker file gets reaped.
        let payloads = sessions.map {
            (sid: $0.sessionId, providerSid: $0.providerSessionId,
             agent: $0.agentType, cwd: $0.cwd, pid: $0.claudePid)
        }
        let geom = state.autoTile ? Self.screenGeom() : nil
        let layout = geom.flatMap { Self.computeTileLayout(count: payloads.count, geom: $0, size: state.textSize) }
        DispatchQueue.global(qos: .userInitiated).async {
            // Resolve the target terminal before the SIGTERM — once the old
            // windows tear down, the open-app detection would skew.
            let term = Self.preferredTerminalApp()
            for p in payloads where p.pid > 0 {
                kill(pid_t(p.pid), SIGTERM)
            }
            // Brief grace period so the old TTY/process tree tears down
            // before the resumed session tries to grab the keyring etc.
            Thread.sleep(forTimeInterval: 0.5)
            for (i, p) in payloads.enumerated() where !p.cwd.isEmpty {
                let cell = layout?.cellAt(index: i)
                Self.openTerminalRunningAgent(
                    cwd: p.cwd,
                    sessionId: p.sid,
                    providerSessionId: p.providerSid,
                    agentType: p.agent,
                    bounds: cell?.bounds,
                    fontSize: layout?.fontSize,
                    app: term
                )
            }
        }
    }

    @objc func toggleAutoTile() {
        let path = Paths.autoTileFlag
        let fm = FileManager.default
        if fm.fileExists(atPath: path) {
            try? fm.removeItem(atPath: path)
        } else {
            let dir = (path as NSString).deletingLastPathComponent
            try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
            fm.createFile(atPath: path, contents: nil)
        }
        refresh()
    }

    @objc func setTextNear() {
        let path = Paths.nearTextFlag
        let dir = (path as NSString).deletingLastPathComponent
        try? FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
        FileManager.default.createFile(atPath: path, contents: nil)
        try? FileManager.default.removeItem(atPath: Paths.tinyTextFlag)
        // Update the live snapshot synchronously: `refresh()` re-derives
        // textSize off a *background* gather that posts back later, so the
        // immediate `tileNow()` would otherwise tile at the previous size.
        state.textSize = .near
        refresh()
        tileNow()
    }

    @objc func setTextTiny() {
        let path = Paths.tinyTextFlag
        let dir = (path as NSString).deletingLastPathComponent
        try? FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
        try? FileManager.default.removeItem(atPath: Paths.nearTextFlag)
        state.textSize = .tiny
        refresh()
        tileNow()
    }

    @objc func setTextFar() {
        try? FileManager.default.removeItem(atPath: Paths.nearTextFlag)
        try? FileManager.default.removeItem(atPath: Paths.tinyTextFlag)
        state.textSize = .far
        refresh()
        tileNow()
    }

    @objc func toggleThemeByStatus() {
        let path = Paths.themeByStatusFlag
        let fm = FileManager.default
        if fm.fileExists(atPath: path) {
            try? fm.removeItem(atPath: path)
            // Forget what we pushed so re-enabling later starts from a
            // clean slate (no "we already themed this session" memory
            // surviving across an off→on cycle).
            lastTerminalDecor.removeAll()
        } else {
            let dir = (path as NSString).deletingLastPathComponent
            try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
            fm.createFile(atPath: path, contents: nil)
        }
        refresh()
        applyTerminalDecor()
    }

    /// Toggle the "force bright" override: status themes use the light
    /// palettes regardless of the macOS Auto-appearance schedule, so the
    /// wall stays sunlight-readable after the system flips Dark for the
    /// evening. Clears the decor + desktop memos so the flip repaints every
    /// live session (and the desktop tint) on the very next tick instead of
    /// waiting for each one to change state.
    @objc func toggleForceBright() {
        let path = Paths.forceBrightFlag
        let fm = FileManager.default
        if fm.fileExists(atPath: path) {
            try? fm.removeItem(atPath: path)
        } else {
            let dir = (path as NSString).deletingLastPathComponent
            try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
            fm.createFile(atPath: path, contents: nil)
        }
        lastTerminalDecor.removeAll()
        lastDesktopTint = ""
        refresh()
        applyTerminalDecor()
    }

    /// Toggle the per-prompt sigil badges. When on, each live session's
    /// terminal window wears a little hashed star (SigilRenderer) in its
    /// top-right corner so prompts are glanceably distinct. Off tears every
    /// badge down via the controller (the `enabled: false` branch of sync);
    /// `refresh()` re-reads the flag and drives the controller on the next
    /// tick, but we call sync directly here too so the toggle is instant.
    @objc func togglePromptSigils() {
        // On by default; the marker exists only when explicitly disabled.
        let path = Paths.promptSigilsDisabledFlag
        let fm = FileManager.default
        let nowOn: Bool
        if fm.fileExists(atPath: path) {
            try? fm.removeItem(atPath: path)   // remove disable → ON
            nowOn = true
        } else {
            let dir = (path as NSString).deletingLastPathComponent
            try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
            fm.createFile(atPath: path, contents: nil)   // create disable → OFF
            nowOn = false
        }
        state.promptSigils = nowOn
        PromptSigilOverlayController.shared.sync(
            sessions: state.claudeSessions, enabled: nowOn)
        refresh()
    }

    /// Toggle the ⌃⌃ zoom lens. The tap keeps listening either way — this only
    /// flips the flag it consults — so turning the lens back on is instant and
    /// doesn't risk a tap that fails to re-arm. If the lens is up when it's
    /// switched off, drop it now rather than stranding the pointer.
    @objc func toggleZoomLens() {
        // On by default; the marker exists only when explicitly disabled.
        let path = Paths.zoomLensDisabledFlag
        let fm = FileManager.default
        let nowOn: Bool
        if fm.fileExists(atPath: path) {
            try? fm.removeItem(atPath: path)
            nowOn = true
        } else {
            let dir = (path as NSString).deletingLastPathComponent
            try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
            fm.createFile(atPath: path, contents: nil)
            nowOn = false
        }
        state.zoomLens = nowOn
        if !nowOn, ZoomLens.isZoomed { ZoomLens.toggle() }   // don't strand a zoom
        refresh()
    }

    /// Toggle the "spawn in iTerm2" preference: when on, restore-threads /
    /// restart-all open sessions in iTerm2 (the only terminal that shows the
    /// tiled topic wallpapers) instead of Terminal.app. A pure spawn-target
    /// preference — existing windows are untouched until you restart them.
    @objc func togglePreferIterm() {
        let path = Paths.preferItermFlag
        let fm = FileManager.default
        if fm.fileExists(atPath: path) {
            try? fm.removeItem(atPath: path)
        } else {
            let dir = (path as NSString).deletingLastPathComponent
            try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
            fm.createFile(atPath: path, contents: nil)
        }
        refresh()
    }

    /// Effective dark-mode decision for status theming: the macOS appearance
    /// unless the "force bright" override is on, in which case theming is
    /// pinned to the light (sunlight-readable) palettes.
    private func effectiveDark() -> Bool {
        state.forceBright ? false : Self.isDarkAppearance()
    }

    private func stateName(_ s: ClaudeSession.State) -> String {
        switch s {
        case .blank:       return "blank"
        case .working:     return "working"
        case .rendering:   return "rendering"
        case .complete:    return "complete"
        case .awaiting:    return "awaiting"
        case .interrupted: return "interrupted"
        case .stale:       return "stale"
        }
    }

    /// Off-main: pick each session's wallpaper from cache only (instant
    /// `slab-wallpaper path` probe + status-default file check), and queue a
    /// gen for anything not yet cached (`kickWallpaper` — one at a time). Never
    /// blocks on node/network beyond the ~50 ms probe (see slab-menubar-perf).
    private func resolveWallpapers(_ sessions: [ClaudeSession]) -> [ClaudeSession] {
        let bin = Paths.slabWallpaper
        let fm = FileManager.default
        guard fm.isExecutableFile(atPath: bin) else { return sessions }

        // Status-default set: kick once per launch, detached.
        wallpaperLock.lock()
        let kickDefaults = !defaultsKicked
        if kickDefaults { defaultsKicked = true }
        wallpaperLock.unlock()
        if kickDefaults { ShellRunner.runAsync(bin, args: ["defaults"]) }

        var out: [ClaudeSession] = []
        out.reserveCapacity(sessions.count)
        for var s in sessions {
            let st = stateName(s.state)
            let summary = s.titleString
            var pick = ""
            if !summary.isEmpty {
                pick = wallpaperPath(subject: summary, status: st, fm: fm)
            }
            if pick.isEmpty {
                let def = "\(Paths.wallpaperStatusDir)/\(st).jpg"
                if fm.fileExists(atPath: def) { pick = def }
            }
            s.wallpaper = pick

            if !summary.isEmpty { kickWallpaper(subject: summary, status: st) }
            out.append(s)
        }
        return out
    }

    /// The cached answer if we have one, else a throttled `slab-wallpaper path`.
    /// Off-main (called from `resolveWallpapers`).
    private func wallpaperPath(subject: String, status: String, fm: FileManager) -> String {
        let key = "\(status)\u{1}\(subject)"

        wallpaperLock.lock()
        if let known = wallpaperPaths[key] {
            wallpaperLock.unlock()
            // The file can be swept out from under us; a stale name is worse
            // than a fresh probe, so drop it and let the retry path re-ask.
            if fm.fileExists(atPath: known) { return known }
            wallpaperLock.lock()
            wallpaperPaths[key] = nil
        }
        if let last = wallpaperProbedAt[key], Date().timeIntervalSince(last) < wallpaperProbeRetry {
            wallpaperLock.unlock()
            return ""
        }
        wallpaperProbedAt[key] = Date()
        wallpaperLock.unlock()

        let probe = ShellRunner.output(
            Paths.slabWallpaper, args: ["path", "subject", subject, status], timeout: 4
        )?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        guard !probe.isEmpty, fm.fileExists(atPath: probe) else { return "" }

        wallpaperLock.lock()
        wallpaperPaths[key] = probe
        wallpaperProbedAt[key] = nil
        wallpaperLock.unlock()
        return probe
    }

    /// Queue a subject gen: new if unseen, otherwise nothing. Coalescing — a
    /// request that arrives while a gen runs displaces the one waiting, and the
    /// displaced key leaves the once-only set so it can be asked for again if
    /// that state comes back.
    private func kickWallpaper(subject: String, status: String) {
        let key = "\(status)\u{1}\(subject)"
        wallpaperLock.lock()
        guard kickedWallpapers.insert(key).inserted else { wallpaperLock.unlock(); return }
        if wallpaperBusy {
            if let stale = pendingWallpaper { kickedWallpapers.remove(stale.key) }
            pendingWallpaper = (key, subject, status)
            wallpaperLock.unlock()
            return
        }
        wallpaperBusy = true
        wallpaperLock.unlock()
        runWallpaper(subject, status)
    }

    /// One gen, then drain whatever coalesced behind it. The 180 s cap matches
    /// slab-wallpaper's own stale-lock reclaim — past that the render is wedged,
    /// and a wedged render must die rather than hold the slot forever.
    private func runWallpaper(_ subject: String, _ status: String) {
        DispatchQueue.global(qos: .utility).async { [weak self] in
            _ = ShellRunner.run(
                Paths.slabWallpaper, args: ["subject", subject, status], timeout: 180
            )
            guard let self = self else { return }
            self.wallpaperLock.lock()
            // The picture this gen just drew is the one the probe was waiting
            // for: forget the throttle so the next tick asks once and caches it.
            self.wallpaperProbedAt["\(status)\u{1}\(subject)"] = nil
            let next = self.pendingWallpaper
            self.pendingWallpaper = nil
            if next == nil { self.wallpaperBusy = false }
            self.wallpaperLock.unlock()
            if let next = next { self.runWallpaper(next.subject, next.status) }
        }
    }

    /// RGB triple in AppleScript colorspace (0–65535 per channel).
    typealias RGB = (Int, Int, Int)
    /// One coordinated per-status palette: bg + foreground + bold + cursor,
    /// so the whole window shifts as a single tone.
    struct Palette { let bg: RGB?; let text: RGB?; let bold: RGB?; let cursor: RGB? }

    /// Single source of truth for the per-status palette + status glyph,
    /// shared by the iTerm2 live-property path and the Terminal.app
    /// settings-set provisioning path. Light mode = a genuinely *bright*
    /// page — a near-white background washed with the status hue, plus dark
    /// saturated ink — so every status (not just blank) stays readable in
    /// direct sunlight outdoors. Dark mode = the same hue dimmed to a
    /// genuine dark page (never collapsed to black) with bright ink, so
    /// status stays legible across windows at night.
    /// Per-status palette + glyph, agent-aware. Claude uses the base palette;
    /// Codex gets a slightly cooler sibling of the SAME state color (a small
    /// pull toward blue) so you can tell agents apart at a glance without
    /// breaking the state-as-color language (green=working, slate=complete…).
    static func statusDecor(
        for state: ClaudeSession.State, dark: Bool, blink: Bool = false,
        agentType: String = "claude"
    ) -> (palette: Palette, glyph: String) {
        // Codex completion is a stronger attention cue than Claude's calm
        // slate: coral/red, distinct from approval/elicitation amber.
        if agentType == "codex", state == .complete {
            if dark {
                return blink
                    ? (Palette(bg: (23500, 3200, 4200), text: (65535, 48000, 46000),
                               bold: (65535, 57000, 55000), cursor: (65535, 15000, 14000)), "✓ complete")
                    : (Palette(bg: (17000, 1900, 3000), text: (65535, 45000, 43000),
                               bold: (65535, 55000, 53000), cursor: (65535, 11000, 10000)), "✓ complete")
            }
            return blink
                ? (Palette(bg: (65535, 39000, 38000), text: (30000, 1200, 1800),
                           bold: (21000, 300, 800), cursor: (62000, 5000, 5000)), "✓ complete")
                : (Palette(bg: (65535, 45500, 44000), text: (30000, 1200, 1800),
                           bold: (21000, 300, 800), cursor: (62000, 5000, 5000)), "✓ complete")
        }
        let base = baseStatusDecor(for: state, dark: dark, blink: blink)
        guard agentType == "codex" else { return base }
        return (palette: codexTint(base.palette, dark: dark), glyph: base.glyph)
    }

    /// The Codex variant: a mild, uniform cool shift (−R, +B) on every channel
    /// of the palette. Keeps state identity (a working page stays green-ish,
    /// just cooler/tealer) while reading as "not Claude". Awaiting is left
    /// nearly untouched so its urgency amber doesn't go muddy.
    static func codexTint(_ p: Palette, dark: Bool) -> Palette {
        func t(_ c: RGB?) -> RGB? {
            guard let c = c else { return nil }
            let r = Int((Double(c.0) * 0.86).rounded())
            let g = c.1
            let b = min(65535, Int((Double(c.2) * 1.14).rounded()) + 2600)
            return (r, g, b)
        }
        return Palette(bg: t(p.bg), text: t(p.text), bold: t(p.bold), cursor: t(p.cursor))
    }

    private static func baseStatusDecor(
        for state: ClaudeSession.State, dark: Bool, blink: Bool = false
    ) -> (palette: Palette, glyph: String) {
        switch state {
        // Blank = pure macOS appearance (white in light, black in dark) so a
        // fresh window reads as a blank page until the first prompt fires.
        case .blank:
            return dark
                ? (Palette(bg: (0, 0, 0), text: (60000, 60000, 60000),
                           bold: (65535, 65535, 65535), cursor: (50000, 50000, 50000)), "")
                : (Palette(bg: (65535, 65535, 65535), text: (8000, 8000, 8000),
                           bold: (0, 0, 0), cursor: (20000, 20000, 20000)), "")
        // Working = green (active/healthy). Light: bright mint page, deep
        // forest ink — the hue lives in the wash + ink, not a dark field.
        case .working:
            return dark
                ? (Palette(bg: (600, 12000, 3200), text: (40000, 64000, 47000),
                           bold: (54000, 65535, 58000), cursor: (14000, 62000, 30000)), "● working")
                : (Palette(bg: (39000, 64000, 45000), text: (1200, 22000, 6000),
                           bold: (400, 13000, 3000), cursor: (1000, 53000, 11000)), "● working")
        // Rendering = pink (turn done, but a launched render is still
        // cooking — between working-green and awaiting-amber). Light: bright
        // rose page, deep magenta ink; dark: deep rose page, bright pink ink.
        case .rendering:
            return dark
                ? (Palette(bg: (9000, 1800, 5800), text: (62000, 44000, 54000),
                           bold: (65535, 56000, 62000), cursor: (58000, 18000, 40000)), "◐ rendering")
                : (Palette(bg: (65535, 54500, 60000), text: (26000, 2500, 15000),
                           bold: (17000, 1000, 9500), cursor: (54000, 6000, 31000)), "◐ rendering")
        // Complete = slate (turn done — "look when ready"). Blink: a small,
        // same-hue nudge so the window breathes rather than flashes. Direction
        // flips by appearance — dark mode lifts toward brighter slate, bright
        // mode dips toward deeper, more saturated blue (it's already near
        // white, so brighter has nowhere to go).
        case .complete:
            if blink {
                return dark
                    ? (Palette(bg: (5000, 8000, 18500), text: (46000, 51000, 64000),
                               bold: (58000, 61000, 65535), cursor: (24000, 40000, 64000)), "✓ complete")
                    : (Palette(bg: (37000, 45000, 64000), text: (5000, 12000, 35000),
                               bold: (2000, 6000, 25000), cursor: (8000, 22000, 63000)), "✓ complete")
            }
            return dark
                ? (Palette(bg: (2200, 4200, 14000), text: (46000, 51000, 64000),
                           bold: (58000, 61000, 65535), cursor: (24000, 40000, 64000)), "✓ complete")
                : (Palette(bg: (42000, 50000, 65535), text: (5000, 12000, 35000),
                           bold: (2000, 6000, 25000), cursor: (8000, 22000, 63000)), "✓ complete")
        // Awaiting = warm amber (needs input, focus pop). Blink: same
        // subtle-nudge rule — dark mode warms slightly brighter, bright mode
        // saturates slightly deeper. Still distinguishable from complete's
        // blue pulse without strobing the room.
        case .awaiting:
            if blink {
                return dark
                    ? (Palette(bg: (24000, 13500, 1200), text: (65535, 58000, 38000),
                               bold: (65535, 65535, 50000), cursor: (65535, 46000, 10000)), "◉ awaiting")
                    : (Palette(bg: (65535, 54000, 36000), text: (26000, 13000, 0),
                               bold: (18000, 8000, 0), cursor: (65535, 35000, 0)), "◉ awaiting")
            }
            return dark
                ? (Palette(bg: (19000, 10500, 900), text: (65535, 58000, 38000),
                           bold: (65535, 65535, 50000), cursor: (65535, 46000, 10000)), "◉ awaiting")
                : (Palette(bg: (65535, 59000, 45000), text: (26000, 13000, 0),
                           bold: (18000, 8000, 0), cursor: (65535, 35000, 0)), "◉ awaiting")
        // Interrupted = muted violet (Esc'd, idle at the prompt — distinct
        // from working-green, complete-slate, awaiting-amber). Blink nudges
        // a touch brighter so a cut-off thread reads as "needs a nudge".
        case .interrupted:
            if blink {
                return dark
                    ? (Palette(bg: (12000, 4000, 18000), text: (54000, 46000, 64000),
                               bold: (60000, 54000, 65535), cursor: (46000, 26000, 65535)), "✕ interrupted")
                    : (Palette(bg: (54000, 47000, 64000), text: (22000, 8000, 34000),
                               bold: (14000, 2000, 26000), cursor: (40000, 12000, 65535)), "✕ interrupted")
            }
            return dark
                ? (Palette(bg: (8500, 2800, 13500), text: (54000, 46000, 64000),
                           bold: (60000, 54000, 65535), cursor: (40000, 22000, 60000)), "✕ interrupted")
                : (Palette(bg: (58000, 52000, 65535), text: (22000, 8000, 34000),
                           bold: (14000, 2000, 26000), cursor: (34000, 12000, 60000)), "✕ interrupted")
        // Stale = deep red (process dead, escalate).
        case .stale:
            return dark
                ? (Palette(bg: (17000, 1400, 2600), text: (65535, 42000, 42000),
                           bold: (65535, 55000, 55000), cursor: (65535, 20000, 20000)), "○ stale")
                : (Palette(bg: (65535, 54000, 54000), text: (26000, 1500, 4000),
                           bold: (18000, 0, 2000), cursor: (60000, 4000, 7000)), "○ stale")
        }
    }

    /// Terminal.app settings-set name for a status × appearance. Terminal
    /// can't set ad-hoc per-window RGB like iTerm2, so slab provisions one
    /// named profile per combo and just switches a tab's `current settings`.
    static func profileName(
        for state: ClaudeSession.State, dark: Bool, blink: Bool = false,
        agentType: String = "claude"
    ) -> String {
        let s: String
        switch state {
        case .blank:    s = "blank"
        case .working:  s = "working"
        case .rendering: s = "rendering"
        case .complete: s = "complete"
        case .awaiting: s = "awaiting"
        // No dedicated Terminal.app settings set for interrupted — reuse the
        // calm "complete" (slate) profile so Terminal users still leave green.
        // iTerm2 + the menubar polygon use the distinct violet palette above.
        case .interrupted: s = "complete"
        case .stale:    s = "stale"
        }
        // Codex gets its own settings-set family so its slightly-cooler
        // palette is provisioned + switched independently of Claude's.
        let agentSuffix = agentType == "codex" ? "-codex" : ""
        let base = "Slab-\(s)-\(dark ? "dark" : "light")\(agentSuffix)"
        // Only attention states ever pulse; the suffix keeps the alt
        // settings set distinct so Terminal.app can flip between two
        // provisioned profiles per tick.
        return blink && (state == .complete || state == .awaiting)
            ? "\(base)-pulse"
            : base
    }

    /// Push the per-status palette + custom title to each live Claude
    /// session, in iTerm2 *and* Terminal.app. Called from `refresh()`, but
    /// only emits an osascript when something actually changed since the
    /// last pass (state flipped, subject moved, appearance flipped, or a new
    /// session appeared). iTerm2 gets ad-hoc per-session colors + wallpaper;
    /// Terminal.app gets a provisioned `Slab-<state>-<appearance>` settings
    /// set switched per tab (it has no per-window RGB / bg-image scripting).
    /// Both blocks are `is running`-guarded so a non-running terminal is a
    /// cheap no-op and is never launched.
    private func applyTerminalDecor() {
        guard state.themeByStatus else { return }
        struct Assignment {
            let tty: String; let palette: Palette; let title: String
            let wallpaper: String; let profile: String
        }
        var changes: [Assignment] = []
        var seen = Set<String>()
        let darkAppearance = effectiveDark()
        // Opt-in diagnostic (off unless the flag file exists): `touch
        // $SLAB_HOME/state/decor-debug` to log what the agent actually
        // computes (appearance, per-session state/tty, change set) to stderr
        // and dump the emitted AppleScript to /tmp/slab-decor-*.scpt. Zero
        // cost when the flag is absent; invaluable when this AppleScript-
        // fragile pipeline misbehaves on a new macOS / terminal version.
        let decorDebug = FileManager.default.fileExists(
            atPath: "\(Paths.slabHome)/state/decor-debug")
        func dlog(_ m: String) {
            guard decorDebug else { return }
            FileHandle.standardError.write(Data("[decor] \(m)\n".utf8))
        }
        dlog("dark=\(darkAppearance) forceBright=\(state.forceBright) "
            + "themeByStatus=\(state.themeByStatus) "
            + "sessions=\(state.claudeSessions.count) "
            + "withTty=\(state.claudeSessions.filter { !$0.tty.isEmpty }.count)")
        for s in state.claudeSessions {
            dlog("  sid=\(s.sessionId.prefix(8)) state=\(s.state) "
                + "tty=\"\(s.tty)\"")
        }
        for s in state.claudeSessions where !s.tty.isEmpty {
            seen.insert(s.sessionId)
            // Pulse only the attention states — every other state holds steady
            // so working/blank/stale sessions don't churn osascript on the
            // 0.6 s blink tick.
            let isAttention = (s.state == .complete || s.state == .awaiting || s.state == .interrupted)
            let blink = isAttention && blinkPhase
            let decor = Self.statusDecor(for: s.state, dark: darkAppearance, blink: blink, agentType: s.agentType)
            var palette = decor.palette
            let glyph = decor.glyph
            // She texted (theme-by-status on): fold a shared magenta accent
            // into every themed page so the wall reads "look here" without
            // losing each session's state identity. The cursor goes full
            // message-hue (its blink becomes the cue); bold + bg only warm.
            if state.messageWaiting {
                func blend(_ c: RGB?, _ m: RGB, _ f: Double) -> RGB? {
                    guard let c = c else { return nil }
                    func g(_ a: Int, _ b: Int) -> Int {
                        Int((Double(a) * (1 - f) + Double(b) * f).rounded())
                    }
                    return (g(c.0, m.0), g(c.1, m.1), g(c.2, m.2))
                }
                let msg: RGB = (62000, 9000, 38000)
                let msgBright: RGB = (65535, 40000, 55000)
                palette = Palette(
                    bg:     blend(palette.bg, msg, 0.10),
                    text:   palette.text,
                    bold:   blend(palette.bold, msgBright, 0.35),
                    cursor: msg)
            }
            // Blank windows get an empty custom title so Terminal shows just
            // its default tty/process line — no "● working · …" badge while
            // the page is meant to look blank. Every other window leads with
            // its sticky session emoji — the anchor that survives retiles.
            let emojiPrefix = s.emoji.isEmpty ? "" : "\(s.emoji) "
            let title = (s.state == .blank) ? "" : "\(emojiPrefix)\(glyph) · \(s.titleString)"
            // Terminal.app profile name. The "-msg" suffix gives the "she
            // texted" magenta-tinted palette its own settings set so Terminal
            // windows show the accent too (their colors come from the profile,
            // not ad-hoc RGB). Provisioned below from this Assignment.palette.
            let profile = Self.profileName(for: s.state, dark: darkAppearance, blink: blink, agentType: s.agentType)
                + (state.messageWaiting ? "-msg" : "")
            func keyOf(_ c: RGB?) -> String { c.map { "\($0.0),\($0.1),\($0.2)" } ?? "-" }
            let key = [
                keyOf(palette.bg),
                keyOf(palette.text),
                keyOf(palette.bold),
                keyOf(palette.cursor),
                state.messageWaiting ? "msg" : "-",
                title,
                profile,
                s.wallpaper.isEmpty ? "-" : s.wallpaper,
                // Pulse phase joins the key only for attention states so
                // working/blank tiles don't churn — see `updateBlinkTimer`.
                blink ? "pulse" : "-",
            ].joined(separator: "|")
            if lastTerminalDecor[s.sessionId] == key { continue }
            lastTerminalDecor[s.sessionId] = key
            changes.append(Assignment(
                tty: s.tty, palette: palette, title: title,
                wallpaper: s.wallpaper, profile: profile))
        }
        // Reap entries for sessions that disappeared since last tick — they
        // either died or got reaped by the janitor; either way our memo is
        // stale.
        for sid in lastTerminalDecor.keys where !seen.contains(sid) {
            lastTerminalDecor.removeValue(forKey: sid)
        }
        dlog("changes=\(changes.count) "
            + changes.map { "[\($0.tty) bg=\($0.palette.bg.map { "\($0)" } ?? "-")]" }
                .joined(separator: " "))
        if changes.isEmpty { return }

        func rgbStr(_ c: RGB) -> String { "{\(c.0), \(c.1), \(c.2)}" }
        func esc(_ v: String) -> String {
            v.replacingOccurrences(of: "\\", with: "\\\\")
                .replacingOccurrences(of: "\"", with: "\\\"")
        }

        // Two independent osascript invocations so a compile/resolve
        // failure in one terminal can't kill the other (one shared script
        // is fatal: a bad `tell application "iTerm2"` term aborts the whole
        // thing, Terminal block included — that was the original bug).

        // ── iTerm2: ad-hoc per-session color + name + bg image ──────────
        // Referenced by bundle id: the app registers as "iTerm", so the
        // by-name `application "iTerm2"` term fails to *compile* on many
        // installs. Skipped entirely unless iTerm2 is installed; the inner
        // `if it is running` keeps it from ever auto-launching iTerm2.
        let itermInstalled = NSWorkspace.shared.urlForApplication(
            withBundleIdentifier: "com.googlecode.iterm2") != nil
        if itermInstalled {
            var it: [String] = [
                "tell application id \"com.googlecode.iterm2\"",
                "  if it is running then",
                "    repeat with w in windows",
                "      repeat with t in tabs of w",
                "        repeat with s in sessions of t",
                "          try",
                "            set ttyName to tty of s",
            ]
            for a in changes {
                let escTty = esc(a.tty)
                it.append("            if ttyName ends with \"\(escTty)\" then")
                if let bg = a.palette.bg {
                    it.append("              set background color of s to \(rgbStr(bg))")
                }
                if let text = a.palette.text {
                    it.append("              set foreground color of s to \(rgbStr(text))")
                }
                if let bold = a.palette.bold {
                    it.append("              set bold color of s to \(rgbStr(bold))")
                }
                if let cursor = a.palette.cursor {
                    it.append("              set cursor color of s to \(rgbStr(cursor))")
                }
                it.append("              set name of s to \"\(esc(a.title))\"")
                it.append("              set background image of s to \"\(esc(a.wallpaper))\"")
                it.append("            end if")
            }
            it.append(contentsOf: [
                "          end try",
                "        end repeat",
                "      end repeat",
                "    end repeat",
                "  end if",
                "end tell",
            ])
            let itScript = it.joined(separator: "\n")
            if decorDebug {
                try? itScript.write(toFile: "/tmp/slab-decor-iterm.scpt",
                                    atomically: true, encoding: .utf8)
            }
            ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", itScript])
        }

        // ── Terminal.app: provisioned settings sets switched per tab ────
        // Terminal has no per-window RGB or bg-image scripting, so slab
        // provisions one named settings set per distinct status×appearance
        // (idempotent: reuse if present, always re-push colors so a palette
        // tweak propagates) and switches each matching tab's
        // `current settings`. Font is copied from the user's default profile
        // so only colors change. (Terminal applies a profile's window size
        // on switch — if tiled windows jump, that's the known Terminal-only
        // tradeoff of per-window theming here.)
        var profileOrder: [String] = []
        var profilePalette: [String: Palette] = [:]
        for a in changes where profilePalette[a.profile] == nil {
            profileOrder.append(a.profile)
            profilePalette[a.profile] = a.palette
        }
        var tm: [String] = [
            "if application \"Terminal\" is running then",
            "  tell application \"Terminal\"",
        ]
        for name in profileOrder {
            let pal = profilePalette[name]!
            let n = esc(name)
            tm.append("    set slabE to (every settings set whose name is \"\(n)\")")
            tm.append("    if (count of slabE) is 0 then")
            tm.append("      set slabSS to (make new settings set with properties {name:\"\(n)\"})")
            tm.append("    else")
            tm.append("      set slabSS to item 1 of slabE")
            tm.append("    end if")
            tm.append("    try")
            tm.append("      set font name of slabSS to font name of default settings")
            tm.append("    end try")
            // Keep the family from the user's default, but pin the size so a
            // re-theme on the blink tick doesn't bounce the window back to the
            // un-tiled 12pt. In scatter mode that pin is the tiny scatter font
            // (otherwise the refresh would undo the shrink every tick); tiled
            // otherwise; default before the first tile of the session.
            let decorFont = scatterMode ? Self.scatterFontSize : lastTiledFontSize
            tm.append("    try")
            if let f = decorFont {
                tm.append("      set font size of slabSS to \(f)")
            } else {
                tm.append("      set font size of slabSS to font size of default settings")
            }
            tm.append("    end try")
            // Close windows without the "terminate running processes?" modal:
            // `clean commands` is Terminal's allowlist of processes ignored when
            // deciding whether to warn on close. Include shells + dev runtimes so
            // Slab can close terminals (dev servers, REPLs) without a popover.
            tm.append("    try")
            tm.append("      set clean commands of slabSS to {\"screen\", \"tmux\", \"less\", \"more\", \"view\", \"mandoc\", \"tail\", \"log\", \"top\", \"htop\", \"bash\", \"zsh\", \"sh\", \"fish\", \"node\", \"npm\", \"pnpm\", \"yarn\", \"bun\", \"deno\", \"turbo\", \"vite\", \"tsx\", \"ts-node\", \"nodemon\", \"esbuild\", \"git\", \"ssh\", \"python\", \"python3\", \"ruby\", \"claude\", \"codex\", \"codex-slab\"}")
            tm.append("    end try")
            // A fresh `make new settings set` inherits Terminal's FACTORY
            // title components (working dir + process + size all on), not
            // the user's default profile — so a themed window read
            // "aesthetic-computer — <title> — claude — 80×24". Turn off
            // every component AppleScript can reach; the two it can't
            // (working dir + active process) are plist-only and handled by
            // `patchTerminalTitleComponents()` when Terminal next quits.
            tm.append("    try")
            tm.append("      set title displays window size of slabSS to false")
            tm.append("      set title displays device name of slabSS to false")
            tm.append("      set title displays shell path of slabSS to false")
            tm.append("      set title displays settings name of slabSS to false")
            tm.append("    end try")
            if let bg = pal.bg {
                tm.append("    try")
                tm.append("      set background color of slabSS to \(rgbStr(bg))")
                tm.append("    end try")
            }
            if let text = pal.text {
                tm.append("    try")
                tm.append("      set normal text color of slabSS to \(rgbStr(text))")
                tm.append("    end try")
            }
            if let bold = pal.bold {
                tm.append("    try")
                tm.append("      set bold text color of slabSS to \(rgbStr(bold))")
                tm.append("    end try")
            }
            if let cursor = pal.cursor {
                tm.append("    try")
                tm.append("      set cursor color of slabSS to \(rgbStr(cursor))")
                tm.append("    end try")
            }
        }
        tm.append(contentsOf: [
            "    repeat with w in windows",
            "      repeat with t in tabs of w",
            "        try",
            "          set ttyName to tty of t",
        ])
        for a in changes {
            let escTty = esc(a.tty)
            tm.append("          if ttyName ends with \"\(escTty)\" then")
            tm.append("            set current settings of t to settings set \"\(esc(a.profile))\"")
            if a.title.isEmpty {
                tm.append("            set title displays custom title of t to false")
            } else {
                tm.append("            set custom title of t to \"\(esc(a.title))\"")
                tm.append("            set title displays custom title of t to true")
            }
            tm.append("          end if")
        }
        tm.append(contentsOf: [
            "        end try",
            "      end repeat",
            "    end repeat",
            "  end tell",
            "end if",
        ])
        let tmScript = tm.joined(separator: "\n")
        if decorDebug {
            try? tmScript.write(toFile: "/tmp/slab-decor-terminal.scpt",
                                atomically: true, encoding: .utf8)
            dlog("wrote /tmp/slab-decor-*.scpt iterm=\(itermInstalled) "
                + "profiles=\(profileOrder.count)")
        }
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", tmScript])
    }

    // MARK: - Desktop tint (status wallpaper)

    /// Set the macOS desktop picture to a flat solid color: the **average
    /// of every live session's status color**, then pushed *darker* (dark
    /// mode) or *lighter* (light mode) than the tasks themselves so the
    /// desktop stays recessive behind the themed terminals. Recomputed
    /// whenever any task changes — a new average → a new memo key →
    /// reapply. Uses the in-process `NSWorkspace` desktop-image API, NOT
    /// System Events osascript (`get/set picture of desktop` returns
    /// `missing value` on current macOS, and NSWorkspace needs no
    /// Automation TCC). The user's pre-slab wallpaper is captured once
    /// before the first overwrite and restored whenever theme-by-status is
    /// off / no sessions are live. Decision runs on the main hop (NSScreen
    /// is main-affine); the render + per-screen set are dispatched off-main
    /// so the 2 s tick never stalls (see slab-menubar-perf).
    private func applyDesktopTint() {
        // Always runs, even when theming is off: this is how a wallpaper the
        // user picks (while theme-by-status is disabled) gets remembered as
        // the restore target.
        captureOriginalIfNeeded()
        let sessions = state.claudeSessions
        guard state.themeByStatus, !sessions.isEmpty else {
            // No live sessions (or theme-by-status off): keep a SOLID color —
            // a time-of-day idle tone — never the user's photo wallpaper.
            // (jeffrey: always solid, never default back to the photo.)
            applyIdleTint()
            return
        }
        let dark = effectiveDark()  // override-aware; NSApp/NSScreen → main only
        // Component-wise mean of every session's status page color.
        var sr = 0, sg = 0, sb = 0
        for s in sessions {
            let c = Self.statusDecor(for: s.state, dark: dark, agentType: s.agentType).palette.bg
                ?? (0, 0, 0)
            sr += c.0; sg += c.1; sb += c.2
        }
        let n = sessions.count
        let avg = (sr / n, sg / n, sb / n)
        // Keep the desktop off the tasks' own brightness: darker in dark
        // mode, lighter in light mode, so terminals always sit above it.
        func adjust(_ v: Int) -> Int {
            dark ? Int(Double(v) * 0.55)
                 : v + Int(Double(65535 - v) * 0.55)
        }
        let color = (adjust(avg.0), adjust(avg.1), adjust(avg.2))
        let name = "avg-\(dark ? "dark" : "light")"
        // Memo on the resolved color so any task change (→ new average)
        // re-applies, and an unchanged average doesn't even dispatch.
        let memoKey = "\(name)|\(color.0),\(color.1),\(color.2)"
        if lastDesktopTint == memoKey { return }
        lastDesktopTint = memoKey
        let screens = NSScreen.screens
        DispatchQueue.global(qos: .utility).async {
            guard let path = DesktopTint.ensure(name: name, color: color)
            else { return }
            let url = URL(fileURLWithPath: path)
            for s in screens {
                try? NSWorkspace.shared.setDesktopImageURL(url, for: s, options: [:])
            }
        }
    }

    /// Idle desktop tone when no Claude session is live (or theme-by-status is
    /// off): a flat color that drifts with time of day — warm dawn, cool
    /// morning, bright midday, gold afternoon, amber dusk, deep-indigo night —
    /// dimmed in dark mode. Slab keeps the desktop a solid color at all times
    /// and never reverts to the user's photo wallpaper.
    private func idleDesktopColor(dark: Bool) -> (Int, Int, Int) {
        let hour = Calendar.current.component(.hour, from: Date())
        let b: (Int, Int, Int)   // base tone, 0–255
        switch hour {
        case 5..<8:   b = (224, 178, 168)  // dawn — warm rose
        case 8..<11:  b = (176, 200, 232)  // morning — cool sky
        case 11..<15: b = (208, 214, 220)  // midday — bright neutral
        case 15..<18: b = (228, 198, 150)  // afternoon — gold
        case 18..<21: b = (206, 150, 116)  // dusk — amber
        default:      b = (44, 54, 92)     // night — deep indigo
        }
        let s = dark ? 0.42 : 1.0          // dim for dark mode, full in light
        func c(_ v: Int) -> Int { min(65535, Int(Double(v) * 257.0 * s)) }
        return (c(b.0), c(b.1), c(b.2))
    }

    /// Apply the time-of-day idle tone (memoized like applyDesktopTint, so it
    /// only re-sets when the resolved color actually changes — e.g. crossing
    /// a time-of-day boundary or an appearance flip).
    private func applyIdleTint() {
        let dark = effectiveDark()
        let color = idleDesktopColor(dark: dark)
        let name = "idle-\(dark ? "dark" : "light")"
        let memoKey = "idle|\(name)|\(color.0),\(color.1),\(color.2)"
        if lastDesktopTint == memoKey { return }
        lastDesktopTint = memoKey
        let screens = NSScreen.screens
        DispatchQueue.global(qos: .utility).async {
            guard let path = DesktopTint.ensure(name: name, color: color)
            else { return }
            let url = URL(fileURLWithPath: path)
            for s in screens {
                try? NSWorkspace.shared.setDesktopImageURL(url, for: s, options: [:])
            }
        }
    }

    /// Save the current desktop picture path to `Paths.desktopOriginalFile`
    /// so it can be restored when theming is off / no sessions are live.
    /// Cheap in-process NSWorkspace read; skipped once captured, or if the
    /// current picture is already one of slab's tint PNGs (so we never
    /// record our own image as "the original"). Re-attempts every tick
    /// while uncaptured, so a wallpaper the user sets later still sticks.
    private func captureOriginalIfNeeded() {
        guard !desktopOriginalCaptured else { return }
        if FileManager.default.fileExists(atPath: Paths.desktopOriginalFile) {
            desktopOriginalCaptured = true
            return
        }
        guard let scr = NSScreen.main,
              let cur = NSWorkspace.shared.desktopImageURL(for: scr),
              !cur.path.hasPrefix(Paths.desktopWallpaperDir)
        else { return }
        let dir = (Paths.desktopOriginalFile as NSString).deletingLastPathComponent
        try? FileManager.default.createDirectory(
            atPath: dir, withIntermediateDirectories: true)
        try? cur.path.write(toFile: Paths.desktopOriginalFile,
                            atomically: true, encoding: .utf8)
        desktopOriginalCaptured = true
    }

    private func restoreDesktopWallpaper() {
        guard let raw = try? String(
                contentsOfFile: Paths.desktopOriginalFile, encoding: .utf8)
        else { return }
        let orig = raw.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !orig.isEmpty,
              FileManager.default.fileExists(atPath: orig) else { return }
        let url = URL(fileURLWithPath: orig)
        let screens = NSScreen.screens  // main-affine; we're on the main hop
        DispatchQueue.global(qos: .utility).async {
            for s in screens {
                try? NSWorkspace.shared.setDesktopImageURL(url, for: s, options: [:])
            }
        }
    }

    /// Which terminal app to spawn restored sessions into. Honor whatever
    /// the user actually has open: if only one of iTerm2 / Terminal.app has
    /// windows, use it; if both, defer to the frontmost one; if neither,
    /// fall back to iTerm2 (the port default). `windowCount`'s `is running`
    /// probe never launches an app, so this is side-effect free.
    private static func preferredTerminalApp() -> String {
        // "Make iTerm2 the home": when the user has pinned iTerm2 (and it's
        // installed), every slab-spawned session goes there — even while
        // they're typing in a Terminal window — so the iTerm2-only tiled
        // topic wallpapers are the default surface again.
        let itermInstalled = NSWorkspace.shared.urlForApplication(
            withBundleIdentifier: "com.googlecode.iterm2") != nil
        if itermInstalled,
           FileManager.default.fileExists(atPath: Paths.preferItermFlag) {
            return "iTerm2"
        }
        let iterm = windowCount(app: "iTerm2") > 0
        let term = windowCount(app: "Terminal") > 0
        if iterm && !term { return "iTerm2" }
        if term && !iterm { return "Terminal" }
        if iterm && term {
            let front = ShellRunner.run(
                "/usr/bin/osascript",
                args: ["-e", "tell application \"System Events\" to name of first application process whose frontmost is true"],
                timeout: 5
            ).output.trimmingCharacters(in: .whitespacesAndNewlines)
            return front == "Terminal" ? "Terminal" : "iTerm2"
        }
        return "iTerm2"
    }

    /// Spawn a new window in `app` (iTerm2 or Terminal.app) that cd's into
    /// `cwd` and resumes the given session. Optionally sets window bounds so
    /// the caller can tile. Single-quoted shell args are escaped so a path
    /// with apostrophes can't break out. `fontSize` is accepted for call-
    /// site compatibility but ignored: neither path drives per-window font
    /// via AppleScript here (tiling is by pixel bounds only for now).
    private static func openTerminalRunningClaude(
        cwd: String,
        sessionId: String,
        bounds: (left: Int, top: Int, right: Int, bottom: Int)? = nil,
        fontSize: Int? = nil,
        app: String = "iTerm2"
    ) {
        openTerminalRunningAgent(cwd: cwd, sessionId: sessionId,
            providerSessionId: sessionId, agentType: "claude", bounds: bounds,
            fontSize: fontSize, app: app)
    }

    /// Provider-aware sibling used by Refresh Sessions. Codex is launched via
    /// its Slab wrapper so the replacement remains visible as a prompt rock.
    private static func openTerminalRunningAgent(
        cwd: String,
        sessionId: String,
        providerSessionId: String,
        agentType: String,
        bounds: (left: Int, top: Int, right: Int, bottom: Int)? = nil,
        fontSize: Int? = nil,
        app: String = "iTerm2"
    ) {
        _ = fontSize
        let safeCwd = cwd.replacingOccurrences(of: "'", with: "'\\''")
        let shellCmd: String
        if agentType == "codex" {
            if providerSessionId.isEmpty {
                shellCmd = "cd '\(safeCwd)' && codex-slab"
            } else {
                let safeSid = providerSessionId.replacingOccurrences(of: "'", with: "'\\''")
                shellCmd = "cd '\(safeCwd)' && codex-slab resume '\(safeSid)'"
            }
        } else {
            let safeSid = sessionId.replacingOccurrences(of: "'", with: "'\\''")
            shellCmd = "cd '\(safeCwd)' && claude -r '\(safeSid)'"
        }
        let escapedCmd = shellCmd.replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")

        var lines: [String]
        if app == "Terminal" {
            // `do script` with no `in` target opens a fresh window running
            // the command; the new window becomes `front window`.
            lines = [
                "tell application \"Terminal\"",
                "    activate",
                "    do script \"\(escapedCmd)\"",
            ]
            if let b = bounds {
                lines.append("    set bounds of front window to {\(b.left), \(b.top), \(b.right), \(b.bottom)}")
            }
        } else {
            lines = [
                "tell \(appSpecifier("iTerm2"))",
                "    activate",
                "    create window with default profile",
                "    tell current session of current window to write text \"\(escapedCmd)\"",
            ]
            if let b = bounds {
                lines.append("    set bounds of current window to {\(b.left), \(b.top), \(b.right), \(b.bottom)}")
            }
        }
        lines.append("end tell")
        let script = lines.joined(separator: "\n")
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    // MARK: - Spatial locality

    /// Assign windows to target cells so each lands in the cell NEAREST its
    /// current position — the anti-teleport rule for both tile and scatter: a
    /// window in the top-left stays top-left instead of jumping to wherever its
    /// z-order index (tile) or a random shuffle (scatter) would have sent it.
    ///
    /// Returns `pick`, length == cellCenters.count, where `pick[c]` is the
    /// index of the window assigned to cell `c` (or -1 if that cell goes empty,
    /// which happens only when there are more cells than windows — scatter's
    /// slack cells). Global-greedy over all (window, cell) pairs sorted by
    /// distance: repeatedly take the closest still-unclaimed pair. O(n² log n)
    /// with n = window count, which is a dozen at most — trivial.
    static func localityAssignment(windowCenters: [CGPoint],
                                   cellCenters: [CGPoint]) -> [Int] {
        var pairs: [(d: Double, w: Int, c: Int)] = []
        pairs.reserveCapacity(windowCenters.count * cellCenters.count)
        for w in windowCenters.indices {
            for c in cellCenters.indices {
                let dx = windowCenters[w].x - cellCenters[c].x
                let dy = windowCenters[w].y - cellCenters[c].y
                pairs.append((dx * dx + dy * dy, w, c))
            }
        }
        pairs.sort { $0.d < $1.d }
        var pick = Array(repeating: -1, count: cellCenters.count)
        var wTaken = Set<Int>()
        var cTaken = Set<Int>()
        for p in pairs {
            if wTaken.contains(p.w) || cTaken.contains(p.c) { continue }
            pick[p.c] = p.w
            wTaken.insert(p.w)
            cTaken.insert(p.c)
            if wTaken.count == windowCenters.count { break }
        }
        return pick
    }

    // MARK: - Auto tile layout

    /// One cell in the tiled grid, in AppleScript's top-left-origin pixel
    /// coordinates (Terminal.app's `bounds` property uses this space).
    /// Per-side gutter between tiled windows (and at the edge of the visible
    /// frame). Total inter-window gap is 2*gutter; total edge gap = gutter.
    static let tileGutter = 2

    /// Row-balanced rect packing. Windows are distributed across `rowCounts`
    /// rows (top→bottom); a row with fewer windows simply makes each of its
    /// panels wider, so the layout always fills the visible frame edge-to-
    /// edge with **no empty cells** — never a square grid with holes.
    /// Row heights are equal; column widths vary per row. Integer-division
    /// remainder is absorbed by snapping the last row/column to the exact
    /// screen edge so there is no dead strip on the right or bottom.
    struct TileLayout {
        let rowCounts: [Int]  // windows per row, top row first; sum == count
        let originX: Int      // visible-frame left edge (AS coords)
        let originY: Int      // visible-frame top edge (below menu bar)
        let width: Int        // full visible width to fill
        let height: Int       // full visible height to fill
        let fontSize: Int

        var rows: Int { rowCounts.count }

        struct Cell {
            let bounds: (left: Int, top: Int, right: Int, bottom: Int)
        }

        func cellAt(index: Int) -> Cell {
            // Walk the rows to find which one this flat index lands in.
            var rem = max(0, index)
            var row = rowCounts.count - 1
            for (r, c) in rowCounts.enumerated() {
                if rem < c { row = r; break }
                rem -= c
            }
            let colsInRow = max(1, rowCounts[row])
            let col = min(rem, colsInRow - 1)

            // Equal-height rows; last row snaps to the bottom edge.
            let top = originY + row * height / rows
            let bottom = (row == rows - 1)
                ? originY + height
                : originY + (row + 1) * height / rows
            // Equal-width columns within the row; last col snaps to the
            // right edge so a sparse row stretches full width.
            let left = originX + col * width / colsInRow
            let right = (col == colsInRow - 1)
                ? originX + width
                : originX + (col + 1) * width / colsInRow

            return Cell(bounds: (
                left + tileGutter, top + tileGutter,
                right - tileGutter, bottom - tileGutter
            ))
        }
    }

    /// Snapshot of the main display's geometry, in AppleScript's top-left-
    /// origin pixel coordinates. Captured on the main thread so layout math
    /// can run safely off-main.
    struct ScreenGeom {
        let originX: Int  // visible-frame left edge (AS coords)
        let originY: Int  // visible-frame top edge (below menu bar, AS coords)
        let width: Int
        let height: Int
    }

    static func screenGeom() -> ScreenGeom? {
        guard let screen = NSScreen.main else { return nil }
        let visible = screen.visibleFrame
        let fullHeight = screen.frame.height
        let asTopOfVisible = Int(fullHeight - (visible.origin.y + visible.size.height))
        return ScreenGeom(
            originX: Int(visible.origin.x),
            originY: asTopOfVisible,
            width: Int(visible.size.width),
            height: Int(visible.size.height)
        )
    }

    /// Pack `count` windows into a row-balanced layout that fills the whole
    /// visible frame — no holes, ever. Pick a near-square row count, then
    /// spread the windows across those rows as evenly as possible; rows that
    /// come up a window short just get wider panels. Font size scales down
    /// for denser layouts so even N=10 stays legible.
    private static func computeTileLayout(count: Int, geom: ScreenGeom, size: TextSize = .far) -> TileLayout? {
        guard count > 0 else { return nil }

        // Near-square: same density feel as the old grid, but the per-row
        // counts below let sparse rows stretch instead of leaving holes.
        let approxCols = max(1, Int(ceil(Double(count).squareRoot())))
        let rows = max(1, Int(ceil(Double(count) / Double(approxCols))))

        // Distribute `count` across `rows` as evenly as possible. The first
        // `rem` rows carry one extra window; the lighter (wider-panel) rows
        // fall to the bottom, where a stretched window reads naturally.
        let base = count / rows
        let rem = count % rows
        let rowCounts = (0..<rows).map { $0 < rem ? base + 1 : base }
        let maxCols = rowCounts.max() ?? 1  // narrowest cells live here

        // Font is sized for the tightest cell: full-width / widest row, and
        // equal row height. A char cell is ~fontSize*0.6 wide, *1.2 tall in
        // Menlo/SF Mono; target ~26 rows × ~84 cols of monospace.
        let innerW = max(1, geom.width / maxCols - 2 * tileGutter)
        let innerH = max(1, geom.height / rows - 2 * tileGutter)
        let fontByH = Double(innerH) / (26.0 * 1.2)
        let fontByW = Double(innerW) / (84.0 * 0.6)
        let raw = min(fontByH, fontByW)
        // Far = legible from a typical sitting distance (the auto-fit
        // baseline). Near ≈ 75% — denser when sitting close. Tiny ≈ 60%
        // — tightest pack. The floors are the load-bearing rule: a tiled
        // wall must ALWAYS stay readable, so even the densest pack never
        // drops below ~10pt (Monaspace Argon's comfortable lower bound).
        let scale: Double
        let floor: Int
        switch size {
        case .far:  scale = 1.0;   floor = 10
        case .near: scale = 0.85;  floor = 9
        case .tiny: scale = 0.7;   floor = 8
        }
        let fontSize = max(floor, Int((raw * scale).rounded()))

        return TileLayout(
            rowCounts: rowCounts,
            originX: geom.originX,
            originY: geom.originY,
            width: geom.width,
            height: geom.height,
            fontSize: fontSize
        )
    }

    // MARK: - Scatter (the opposite of tile)

    /// Scatter's font is deliberately tiny — the whole point is to shrink each
    /// session to a little confetti window and let the desktop show through.
    /// 6pt is about as small as Terminal.app renders while a couple of words
    /// stay distinguishable; the window shrinks to match once the font lands.
    static let scatterFontSize = 7
    /// Hard cap on a scattered window's edge so a sparse scatter (few windows,
    /// huge cells) still reads as "tiny confetti" instead of a loose tile.
    static let scatterMaxWin = 290

    /// Where each window lands in a scatter, in AppleScript top-left pixels.
    /// Unlike TileLayout there is no fill invariant: windows are small and the
    /// gaps between them are the feature (more visible desktop).
    struct ScatterLayout {
        let frames: [(left: Int, top: Int, right: Int, bottom: Int)]
        let fontSize: Int
    }

    /// Jittered-grid scatter: carve the visible frame into cells (near-square,
    /// screen-aspect-aware), then drop one small window into a random spot
    /// *inside* a cell. Because every window stays within its own cell and is
    /// strictly smaller than it, windows can never overlap — and the leftover
    /// slack in each cell is the desktop showing through.
    ///
    /// `centers` are the windows' CURRENT centers (top-left-origin px; nil when
    /// AX couldn't read one). Each window is assigned to the cell NEAREST its
    /// current center, so scatter keeps a window roughly where it already was
    /// instead of teleporting it — the confetti look comes from the shrink +
    /// per-cell jitter, not from flinging windows across the screen. Returns
    /// nil when nothing is open. `frames[i]` corresponds to `centers[i]`.
    private static func computeScatterLayout(centers: [CGPoint?], geom: ScreenGeom) -> ScatterLayout? {
        let count = centers.count
        guard count > 0 else { return nil }

        // Cells sized to the screen aspect so they stay roughly square — a
        // square cell gives jitter room in both axes.
        let aspect = Double(geom.width) / Double(max(1, geom.height))
        var cols = max(1, Int((Double(count) * aspect).squareRoot().rounded()))
        var rows = Int(ceil(Double(count) / Double(cols)))
        while cols * rows < count { cols += 1; rows = Int(ceil(Double(count) / Double(cols))) }

        let cellW = geom.width / cols
        let cellH = geom.height / rows

        // Window strictly smaller than its cell: ~66% leaves desktop slack for
        // the jitter to move within, and guarantees non-overlap. Capped so even
        // a 3-window scatter on a big screen stays little.
        let winW = max(130, min(scatterMaxWin, Int(Double(cellW) * 0.66)))
        let winH = max(90, min(Int(Double(scatterMaxWin) * 0.72), Int(Double(cellH) * 0.66)))

        // Center of every cell in the grid (there may be more cells than
        // windows — the slack ones stay empty).
        let cellCenters: [CGPoint] = (0..<(cols * rows)).map { idx in
            let cx = idx % cols, cy = idx / cols
            let cellLeft = geom.originX + cx * geom.width / cols
            let cellTop = geom.originY + cy * geom.height / rows
            return CGPoint(x: Double(cellLeft) + Double(cellW) / 2,
                           y: Double(cellTop) + Double(cellH) / 2)
        }
        // Fill unknown centers with the same-index cell center so they still
        // participate deterministically.
        let wCenters: [CGPoint] = centers.enumerated().map { i, c in
            c ?? cellCenters[min(i, cellCenters.count - 1)]
        }
        // Nearest-cell assignment: pick[c] = window index in cell c (or -1).
        let pick = localityAssignment(windowCenters: wCenters, cellCenters: cellCenters)

        var frames = [(left: Int, top: Int, right: Int, bottom: Int)](
            repeating: (0, 0, 0, 0), count: count)
        for c in 0..<cellCenters.count {
            let wi = pick[c]
            guard wi >= 0 else { continue }
            let cx = c % cols, cy = c / cols
            let cellLeft = geom.originX + cx * geom.width / cols
            let cellTop = geom.originY + cy * geom.height / rows
            // Slack computed from the floored cellW/cellH, so left+winW+slack
            // never crosses the (>= cellW-wide) cell's right/bottom edge.
            let slackX = max(0, cellW - winW)
            let slackY = max(0, cellH - winH)
            let ox = slackX > 0 ? Int.random(in: 0...slackX) : 0
            let oy = slackY > 0 ? Int.random(in: 0...slackY) : 0
            let left = cellLeft + ox
            let top = cellTop + oy
            frames[wi] = (left, top, left + winW, top + winH)
        }
        return ScatterLayout(frames: frames, fontSize: scatterFontSize)
    }

    /// Tile every currently-open iTerm2 *and* Terminal.app agent window into one
    /// shared grid. Independent of the auto-tile flag — this is the
    /// "I forgot to enable it" / "I want to re-pack what's open" button.
    /// iTerm2 windows fill the grid first, then Terminal.app windows. The pass
    /// is deliberately agent-agnostic: Claude and Codex hosts share the same
    /// geometry and Far/Near/Tiny font calculation.
    /// Terminal gets bounds only — AppleScript can't set per-session decor
    /// or wallpaper on Terminal.app, so those windows tile but stay
    /// un-themed (the iTerm2-only port intentionally dropped Terminal decor).
    /// Menu / hotkey entry point: an explicit tile, which DOES reset each
    /// Terminal window's font zoom so the grid-derived size actually lands.
    @objc func tileNow() { tileNowImpl(resetZoom: true) }

    /// `resetZoom`: when true, drive View ▸ Default Font Size on every
    /// Terminal window so a live window adopts the new profile font (a
    /// per-window zoom otherwise silently overrides it — see
    /// slab-terminal-font-zoom). It steals focus for ~0.35 s/window, so the
    /// frequent auto-retile-after-close path passes false (fresh/unchanged
    /// windows already render their profile font).
    ///
    /// Fast path: windows snap into the grid via the in-process AX API
    /// (see AXTiler — milliseconds, no focus steal); the Terminal font pass
    /// is an async catch-up that only runs when the grid font actually
    /// changed (or on an explicit resetZoom tile). Falls back to the legacy
    /// osascript path when Accessibility trust is missing.
    func tileNowImpl(resetZoom: Bool) {
        guard let geom = Self.screenGeom() else { return }
        scatterMode = false  // tiling supersedes a prior scatter; restore tile font
        let textSize = state.textSize
        guard AXTiler.trusted else {
            tileNowLegacy(resetZoom: resetZoom, geom: geom, textSize: textSize)
            return
        }
        let prevFont = lastTiledFontSize
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            guard let pass = Self.axTilePass(geom: geom, textSize: textSize) else { return }
            DispatchQueue.main.async {
                self?.lastTiledFontSize = pass.fontSize
                // Reset decor memo so the next refresh re-themes every
                // window from scratch (a re-pack invalidates prior placement).
                self?.lastTerminalDecor.removeAll()
            }
            // Geometry is already done — the grid snapped above. Terminal
            // text size catches up asynchronously, and only when needed:
            // the profile-font write + Default-Font-Size menu dance is the
            // slow, focus-stealing part of the old tiler.
            guard pass.nTerm > 0, resetZoom || prevFont != pass.fontSize else { return }
            // Set the shared profile size first. An explicit/automatic tile is
            // also a normalization boundary: clear Terminal's invisible
            // per-window Cmd +/- override via View ▸ Default Font Size so all
            // Claude and Codex panes actually render at the same size.
            var lines: [String] = [
                "tell application \"Terminal\"",
                "  set _slabIds to id of (every window whose miniaturized is false)",
                "  repeat with _w in (every window whose miniaturized is false)",
                "    try",
                "      set font size of current settings of _w to \(pass.fontSize)",
                "    end try",
                "  end repeat",
                "end tell",
            ]
            if resetZoom {
                lines.append(contentsOf: [
                    "tell application \"Terminal\" to activate",
                    "repeat with _wid in _slabIds",
                    "  try",
                    "    tell application \"Terminal\" to set index of (first window whose id is (contents of _wid)) to 1",
                    "    delay 0.04",
                    "    tell application \"System Events\" to tell process \"Terminal\" to click menu item \"Default Font Size\" of menu 1 of menu bar item \"View\" of menu bar 1",
                    "  end try",
                    "end repeat",
                ])
            }
            ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", lines.joined(separator: "\n")]) {
                // Terminal sizes by character CELLS, so the font change reflows
                // each window to a new pixel size — and that reflow can land a
                // beat AFTER osascript returns, undoing a single re-pin (window
                // gets "stuck" mid-resize). Re-pin quickly across two short
                // settles so the geometry is always the LAST thing to apply and
                // wins. The windows already snapped instantly in the first pass
                // above; these are tiny AX corrections (sub-ms, no focus steal),
                // so it stays snappy while resolving cleanly after the reflow.
                Self.axTilePass(geom: geom, textSize: textSize)
                for delay in [0.06, 0.16] {
                    DispatchQueue.main.asyncAfter(deadline: .now() + delay) {
                        Self.axTilePass(geom: geom, textSize: textSize)
                    }
                }
            }
        }
    }

    /// Scatter every open session into tiny confetti windows spread across the
    /// desktop — the inverse of Tile. Same window set (iTerm2 + Terminal.app +
    /// AC preview panes), but instead of packing them edge-to-edge it shrinks
    /// each to a little window, drops the font to `scatterFontSize`, and jitters
    /// them into non-overlapping spots so the desktop shows through the gaps.
    ///
    /// The layout is computed ONCE (it's randomized — recomputing per re-pin
    /// would make windows jump), and the same frames are re-applied across a
    /// few settles because a Terminal window only reaches its tiny pixel size
    /// AFTER its font shrinks and the grid reflows (same reflow race the tiler
    /// fights). Requires Accessibility trust; without it we no-op rather than
    /// fall back, since scatter has no legacy osascript path.
    @objc func scatterNow() {
        guard let geom = Self.screenGeom() else { return }
        guard AXTiler.trusted else {
            NSLog("🎲 [scatter] skipped — Accessibility not trusted")
            return
        }
        // Enter scatter mode so the decor refresh pins the tiny font instead of
        // bouncing it back to the tile size every 0.6s tick.
        scatterMode = true
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            let iterm = AXTiler.windows(bundleId: "com.googlecode.iterm2")
            let term = AXTiler.windows(bundleId: "com.apple.Terminal")
            // AC Electron preview windows are frameless (non-standard subrole).
            let acpane = AXTiler.windows(bundleId: "computer.aesthetic.app",
                                         requireStandardSubrole: false)
            let all = iterm + term + acpane
            NSLog("🎲 [scatter] iterm=\(iterm.count) term=\(term.count) acpane=\(acpane.count)")
            // Read each window's current center so scatter can keep it near its
            // present spot instead of flinging it across the screen.
            let centers: [CGPoint?] = all.map { AXTiler.center($0) }
            guard !all.isEmpty,
                  let layout = Self.computeScatterLayout(centers: centers, geom: geom)
            else { return }

            // Pin the fixed, pre-randomized frames — reused verbatim by every
            // re-pin below so the confetti stays put across the font reflow.
            let apply = {
                for (i, w) in all.enumerated() {
                    let f = layout.frames[i]
                    AXTiler.setFrame(w, left: f.left, top: f.top,
                                     right: f.right, bottom: f.bottom)
                }
            }
            apply()  // iTerm2 + AC panes shrink instantly; terminals wait for the font.

            DispatchQueue.main.async {
                // A scatter re-lays everything, so forget prior decor placement.
                self?.lastTerminalDecor.removeAll()
            }
            guard term.count > 0 else { return }

            // Drop each Terminal window's font to the tiny scatter size in ONE
            // fast bulk pass — no `activate`, no per-window z-order reshuffle,
            // no View ▸ Default Font Size menu dance, no delays. That dance is
            // what made the tiler feel heavy (it steals focus and touches each
            // window serially); scatter skips it because setting the settings
            // font alone reflows a normal window, and the scatterMode decor pin
            // enforces the tiny size on the next tick for any window that lags.
            let lines: [String] = [
                "tell application \"Terminal\"",
                "  repeat with _w in (every window whose miniaturized is false)",
                "    try",
                "      set font size of current settings of _w to \(layout.fontSize)",
                "    end try",
                "  end repeat",
                "end tell",
            ]
            ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", lines.joined(separator: "\n")]) {
                // Shrinking the font reflows each window to a new pixel size a
                // beat after osascript returns; re-pin twice, quickly, so the
                // tiny frame is the last thing applied and wins.
                apply()
                for delay in [0.06, 0.16] {
                    DispatchQueue.main.asyncAfter(deadline: .now() + delay) { apply() }
                }
            }
        }
    }

    /// One AX tile sweep: enumerate both terminals' tileable windows
    /// (iTerm2 fills the grid first, matching the legacy order), compute
    /// the layout, and pin every frame. Returns nil when nothing is open.
    private struct AXPass { let nIterm: Int; let nTerm: Int; let fontSize: Int }
    @discardableResult
    private static func axTilePass(geom: ScreenGeom, textSize: TextSize) -> AXPass? {
        let iterm = AXTiler.windows(bundleId: "com.googlecode.iterm2")
        let term = AXTiler.windows(bundleId: "com.apple.Terminal")
        // AC Electron preview windows (slab-web) are created frame:false, so
        // macOS reports a non-standard window subrole — accept any subrole here
        // so these frameless previews pack into the same grid as the terminals.
        let acpane = AXTiler.windows(bundleId: "computer.aesthetic.app", requireStandardSubrole: false)
        let acProcs = NSRunningApplication.runningApplications(withBundleIdentifier: "computer.aesthetic.app").count
        NSLog("🧩 [tile] trusted=\(AXTiler.trusted) acProcs=\(acProcs) iterm=\(iterm.count) term=\(term.count) acpane=\(acpane.count)")
        let all = iterm + term + acpane
        guard !all.isEmpty,
              let layout = computeTileLayout(count: all.count, geom: geom, size: textSize)
        else { return nil }
        // Assign windows to grid cells by spatial locality so a window keeps
        // its region instead of jumping to wherever its app+z-order index fell.
        // Cell centers (top-left-origin px) from the layout; window centers from
        // AX, falling back to the same-index cell center when AX can't read one.
        let cellCenters: [CGPoint] = (0..<all.count).map { i in
            let b = layout.cellAt(index: i).bounds
            return CGPoint(x: Double(b.left + b.right) / 2, y: Double(b.top + b.bottom) / 2)
        }
        let windowCenters: [CGPoint] = all.enumerated().map { i, w in
            AXTiler.center(w) ?? cellCenters[i]
        }
        let pick = localityAssignment(windowCenters: windowCenters, cellCenters: cellCenters)
        for c in 0..<cellCenters.count {
            let wi = pick[c]
            guard wi >= 0 else { continue }
            let cell = layout.cellAt(index: c).bounds
            AXTiler.setFrame(all[wi], left: cell.left, top: cell.top,
                             right: cell.right, bottom: cell.bottom)
        }
        return AXPass(nIterm: iterm.count, nTerm: term.count, fontSize: layout.fontSize)
    }

    /// The pre-AX tiler, kept verbatim as the no-Accessibility fallback:
    /// three osascript spawns (two count probes + one bounds script).
    private func tileNowLegacy(resetZoom: Bool, geom: ScreenGeom, textSize: TextSize) {
        DispatchQueue.global(qos: .userInitiated).async {
            // Size the grid to everything on screen across both apps. The
            // `is running` guard never launches a quit Terminal.app, so a
            // non-running Terminal contributes zero cells.
            let nIterm = Self.windowCount(app: "iTerm2")
            let nTerm = Self.windowCount(app: "Terminal")
            let n = nIterm + nTerm
            guard n > 0 else { return }
            guard let layout = Self.computeTileLayout(count: n, geom: geom, size: textSize) else { return }

            // Reset decor memo so the next refresh re-themes every iTerm2
            // window from scratch (a re-pack invalidates prior placement).
            DispatchQueue.main.async { [weak self] in
                self?.lastTiledFontSize = layout.fontSize
                self?.lastTerminalDecor.removeAll()
            }

            // Pure pixel bounds for both apps — no font/bounds dance.
            // AppleScript `bounds` is {left, top, right, bottom} for both
            // iTerm2 and Terminal, so the same cell math applies to each.
            var lines: [String] = []
            if nIterm > 0 {
                lines.append("tell \(Self.appSpecifier("iTerm2"))")
                lines.append("    activate")
                for i in 0..<nIterm {
                    let cell = layout.cellAt(index: i)
                    lines.append("    set bounds of window \(i + 1) to {\(cell.bounds.left), \(cell.bounds.top), \(cell.bounds.right), \(cell.bounds.bottom)}")
                }
                lines.append("end tell")
            }
            if nTerm > 0 {
                // Terminal.app auto-fit, three ordered passes in ONE osascript
                // so the sequence is deterministic. Windows are addressed by
                // id (captured up front) because pass 2 reorders the list.
                //  1. set each window's profile font to the grid-derived size
                //  2. View ▸ Default Font Size on each window (System Events) —
                //     the ONLY reliable way to make a LIVE window adopt a new
                //     font. A per-window zoom (View ▸ Bigger/Smaller) silently
                //     overrides the profile font and is invisible to
                //     AppleScript, so pass 1 alone is a no-op on already-open
                //     windows (see slab-terminal-font-zoom). Skipped when
                //     resetZoom is false (frequent auto-retile path).
                //  3. re-pin pixel bounds LAST so the reflow can't fight the
                //     cell geometry.
                // Minimized windows are excluded (matches windowCount): they
                // neither consume a cell nor get moved.
                lines.append("tell application \"Terminal\"")
                lines.append("    set _slabIds to id of (every window whose miniaturized is false)")
                lines.append("    repeat with _wid in _slabIds")
                lines.append("      try")
                lines.append("        set font size of current settings of (first window whose id is (contents of _wid)) to \(layout.fontSize)")
                lines.append("      end try")
                lines.append("    end repeat")
                lines.append("end tell")
                if resetZoom {
                    lines.append("tell application \"Terminal\" to activate")
                    lines.append("repeat with _wid in _slabIds")
                    lines.append("  try")
                    lines.append("    tell application \"Terminal\" to set index of (first window whose id is (contents of _wid)) to 1")
                    lines.append("    delay 0.25")
                    lines.append("    tell application \"System Events\" to tell process \"Terminal\" to click menu item \"Default Font Size\" of menu 1 of menu bar item \"View\" of menu bar 1")
                    lines.append("    delay 0.1")
                    lines.append("  end try")
                    lines.append("end repeat")
                }
                lines.append("tell application \"Terminal\"")
                for j in 0..<nTerm {
                    let cell = layout.cellAt(index: nIterm + j)
                    lines.append("    try")
                    lines.append("      set bounds of (first window whose id is (item \(j + 1) of _slabIds)) to {\(cell.bounds.left), \(cell.bounds.top), \(cell.bounds.right), \(cell.bounds.bottom)}")
                    lines.append("    end try")
                }
                lines.append("end tell")
            }
            guard !lines.isEmpty else { return }
            let script = lines.joined(separator: "\n")
            ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
        }
    }

    /// Count windows of `app` via AppleScript, returning 0 if the app isn't
    /// running. `application "X" is running` is a no-op probe — it never
    /// launches the app — so the cross-app tiler can safely ask about a
    /// Terminal.app the user may have quit.
    /// AppleScript application specifier for `app`. iTerm2 MUST be addressed
    /// by **bundle id**: on installs where the app registers as `iTerm.app`
    /// (the common case), the by-name `application "iTerm2"` term fails to
    /// resolve with -1728, which silently zeroed every window count, spawn,
    /// and tile pass — the bug that made the iTerm2 topic wallpapers vanish.
    /// Terminal.app resolves fine by name. Single source of truth so no
    /// AppleScript path can regress to the broken by-name form again.
    static func appSpecifier(_ app: String) -> String {
        app == "iTerm2"
            ? "application id \"com.googlecode.iterm2\""
            : "application \"\(app)\""
    }

    private static func windowCount(app: String) -> Int {
        // Fast path: count in-process via AX (no osascript spawn, no Apple
        // Events round-trip). NSRunningApplication returning nothing for a
        // quit app keeps the never-launches guarantee.
        if AXTiler.trusted {
            let bid = app == "iTerm2" ? "com.googlecode.iterm2" : "com.apple.Terminal"
            return AXTiler.windows(bundleId: bid).count
        }
        let spec = appSpecifier(app)
        // Terminal exposes `miniaturized`, so minimized windows are excluded
        // from the tile grid (they shouldn't consume a cell). iTerm2 doesn't
        // expose it via AppleScript, so fall back to counting all windows.
        let countExpr = app == "iTerm2"
            ? "count windows"
            : "count (windows whose miniaturized is false)"
        let script = """
        if \(spec) is running then
          tell \(spec) to \(countExpr)
        else
          0
        end if
        """
        let out = ShellRunner.run(
            "/usr/bin/osascript",
            args: ["-e", script],
            timeout: 5
        ).output.trimmingCharacters(in: .whitespacesAndNewlines)
        return max(0, Int(out) ?? 0)
    }

    private func syncMail(account: String?) {
        mailSyncing = true
        mailStatus = "syncing…"
        // Menu is already dismissed (an item was clicked); the next open
        // rebuilds from `mailStatus` via menuNeedsUpdate(_:).

        let target = account ?? "-a"
        let log = Paths.mailSyncLog.replacingOccurrences(of: "\"", with: "\\\"")
        let logDir = (Paths.mailSyncLog as NSString).deletingLastPathComponent
        try? FileManager.default.createDirectory(atPath: logDir, withIntermediateDirectories: true)

        let command = "mbsync \(target) >> \"\(log)\" 2>&1 && mu index --quiet >> \"\(log)\" 2>&1"
        ShellRunner.runShellAsync(command) { [weak self] in
            DispatchQueue.main.async {
                self?.mailSyncing = false
                self?.refreshMailCount()
            }
        }
    }

    /// Terminal quit → patch the Slab profiles' unscriptable title keys.
    /// Delayed a beat so Terminal's own final prefs flush lands first
    /// (whoever writes last wins, and we want it to be us).
    @objc private func appDidTerminate(_ note: Notification) {
        guard let app = note.userInfo?[NSWorkspace.applicationUserInfoKey]
                as? NSRunningApplication,
              app.bundleIdentifier == "com.apple.Terminal" else { return }
        DispatchQueue.global(qos: .utility).asyncAfter(deadline: .now() + 2) {
            Self.patchTerminalTitleComponents()
        }
    }

    /// Force the title bar of every Slab-* Terminal profile down to just
    /// the custom title: the "Working directory or document", "Active
    /// process name", and "Window size" components have no AppleScript
    /// terms, so they can only be cleared in the prefs plist — and only
    /// while Terminal is NOT running (it serves profiles from memory and
    /// rewrites the whole domain on save, clobbering external edits).
    /// Idempotent; no-ops when Terminal is up or nothing needs changing.
    private static func patchTerminalTitleComponents() {
        guard NSRunningApplication.runningApplications(
            withBundleIdentifier: "com.apple.Terminal").isEmpty else { return }
        let domain = "com.apple.Terminal" as CFString
        guard var ws = CFPreferencesCopyAppValue("Window Settings" as CFString, domain)
                as? [String: Any] else { return }
        var dirty = false
        for (name, value) in ws where name.hasPrefix("Slab") {
            guard var prof = value as? [String: Any] else { continue }
            for key in ["ShowRepresentedURLInTitle",
                        "ShowActiveProcessInTitle",
                        "ShowDimensionsInTitle",
                        "ShowTTYNameInTitle"] where (prof[key] as? Bool) != false {
                prof[key] = false
                dirty = true
            }
            ws[name] = prof
        }
        guard dirty else { return }
        CFPreferencesSetAppValue("Window Settings" as CFString, ws as CFDictionary, domain)
        CFPreferencesAppSynchronize(domain)
    }

    /// True when the system is currently in Dark mode. Reads NSApp's
    /// effectiveAppearance so live appearance flips (Auto / sunset switch /
    /// manual toggle) propagate without an explicit observer — the next
    /// refresh tick (~2s) recomputes the blank-window bg key and repaints.
    static func isDarkAppearance() -> Bool {
        let appearance = NSApp.effectiveAppearance
        let match = appearance.bestMatch(from: [.darkAqua, .aqua])
        return match == .darkAqua
    }

    // MARK: - Background overlay (experimental)

    /// Toggle a translucent tint overlay over the focused Terminal window.
    /// Proof-of-concept: AppleScript can't set Terminal's own bg image or
    /// transparency, so Slab paints over the window via a borderless NSWindow
    /// tracked to its bounds. Click again on the same window to turn off.
    @objc func toggleBackgroundOverlay() {
        let dark = Self.isDarkAppearance()
        // Cool indigo tint dark mode, warm peach in light mode — visible on
        // both default profiles without obliterating the text underneath.
        let tint = dark
            ? NSColor(red: 0.30, green: 0.45, blue: 0.95, alpha: 1.0)
            : NSColor(red: 1.00, green: 0.80, blue: 0.50, alpha: 1.0)
        BackgroundOverlayController.shared.toggleFrontTerminal(tint: tint, alpha: 0.35)
    }

    @objc func clearBackgroundOverlays() {
        BackgroundOverlayController.shared.clearAll()
    }

    private func notify(title: String, subtitle: String?, body: String) {
        let alert = NSAlert()
        alert.messageText = title
        var info = body
        if let subtitle = subtitle, !subtitle.isEmpty {
            info = "\(subtitle)\n\n\(body)"
        }
        alert.informativeText = info
        alert.alertStyle = .informational
        alert.addButton(withTitle: "OK")
        NSApp.activate(ignoringOtherApps: true)
        alert.runModal()
    }
}
