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
    /// Subject keys we've already fired a detached `slab-wallpaper` gen for,
    /// so a slow generation isn't re-kicked every 2 s tick. Guarded by
    /// `wallpaperLock` (touched from the off-main resolve).
    private var kickedWallpapers = Set<String>()
    private let wallpaperLock = NSLock()
    /// One-shot guard so the status-defaults gen is kicked once per launch.
    private var defaultsKicked = false
    private var refreshTimer: Timer?
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
    private var imsgTickCount = 0
    private var imsgPending = false
    private var imsgStatus = "—"
    private var imsgConfigured = false
    private var imsgUnread = 0
    private var state = StateSnapshot()
    private let passphraseServer = PassphraseServer()
    /// System-wide ⌘⌥T → re-tile claude terminals. Kept alive for the app's
    /// lifetime; unregistered in `applicationWillTerminate`.
    private var tileHotkey: GlobalHotkey?

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

        // Global ⌘⌥T re-tiles claude terminals — same payload as the menu's
        // "Tile now" item, no need to open the dropdown.
        let hotkey = GlobalHotkey { [weak self] in self?.tileNow() }
        if hotkey.register(keyCode: UInt32(kVK_ANSI_T),
                           modifiers: UInt32(cmdKey | optionKey)) {
            tileHotkey = hotkey
        }

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
    }

    func applicationWillTerminate(_ notification: Notification) {
        tileHotkey?.unregister()
        passphraseServer.stop()
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
            DispatchQueue.main.async {
                self.gathering = false
                self.state = snapshot
                // gather() doesn't know about iMessage; fold the cached poll
                // result in here so the icon + decor read one consistent
                // picture. Gated on theme-by-status per the accent's contract.
                self.state.messageWaiting =
                    self.imsgUnread > 0 && snapshot.themeByStatus

                self.updateIcon()
                self.updateAnimTimer()
                self.updateBlinkTimer()

                self.mailTickCount += 1
                if self.mailTickCount >= 15 && !self.mailPending && !self.mailSyncing {
                    self.mailTickCount = 0
                    self.refreshMailCount()
                }

                // Chat wants lower latency than mail — poll ~10 s. The helper
                // detects new inbound and rings the bell itself; we only pull
                // the summary back for the menu label + theme accent.
                self.imsgTickCount += 1
                if self.imsgTickCount >= 5 && !self.imsgPending {
                    self.imsgTickCount = 0
                    self.refreshImsgCount()
                }

                // No menu rebuild here — it's lazy via menuNeedsUpdate(_:).
                self.applyTerminalDecor()
                self.applyDesktopTint()
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
        // When the polygon icon is showing, the edge count communicates the
        // number — only show a numeric tail for subagents (which the polygon
        // doesn't represent) or the legacy fallback states.
        if state.claudeSessions.isEmpty && state.totalActive > 0 {
            button.title = " \(state.totalActive)"
        } else if state.activeSubagents > 0 {
            button.title = " +\(state.activeSubagents)"
        } else {
            button.title = ""
        }
    }

    private func updateAnimTimer() {
        // Run the animation timer whenever the polygon icon is showing, so
        // we can drive both the active-session rotation/pulse and the
        // all-stale blink. Rotation only advances while at least one
        // session is non-stale; pulse phase always advances (it drives both
        // awaiting brightness and stale blink). When the polygon goes away
        // entirely, stop the timer and reset phases.
        if !state.claudeSessions.isEmpty || state.messageWaiting {
            if animTimer == nil {
                let t = Timer.scheduledTimer(withTimeInterval: 0.08, repeats: true) { [weak self] _ in
                    guard let self = self else { return }
                    self.rainbowPhase = (self.rainbowPhase + 0.025).truncatingRemainder(dividingBy: 1.0)
                    if self.state.anyActive {
                        let rotSpeed = 0.004 + 0.012 * CGFloat(self.state.awaitingCount)
                        self.rotationPhase = (self.rotationPhase + rotSpeed)
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
            $0.state == .complete || $0.state == .awaiting
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
            if let data = line.data(using: .utf8),
               let obj = try? JSONSerialization.jsonObject(with: data)
                   as? [String: Any] {
                label = (obj["label"] as? String) ?? label
                configured = (obj["configured"] as? Bool) ?? false
                unread = (obj["unread"] as? Int) ?? 0
            }
            DispatchQueue.main.async {
                self?.imsgStatus = label
                self?.imsgConfigured = configured
                self?.imsgUnread = unread
                self?.imsgPending = false
            }
        }
    }

    // MARK: - Menu actions

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

    @objc func openDaemonLog() {
        ShellRunner.run("/usr/bin/open", args: ["-a", "Console", Paths.lidLog])
    }

    @objc func openSoundsFolder() {
        ShellRunner.run("/usr/bin/open", args: [Paths.soundsDir])
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
                    self.tileNow()
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

    /// Stop every live Claude session, then relaunch each in a fresh Terminal
    /// window with `--resume`. Confirms first because it kills in-flight work.
    @objc func restartAllActive() {
        let sessions = state.claudeSessions
        if sessions.isEmpty { return }
        let alert = NSAlert()
        alert.messageText = "Restart all active Claude sessions?"
        alert.informativeText = "This will stop \(sessions.count) running session\(sessions.count == 1 ? "" : "s") and relaunch each in a fresh Terminal window. In-flight work will be interrupted."
        alert.alertStyle = .warning
        alert.addButton(withTitle: "Restart All")
        alert.addButton(withTitle: "Cancel")
        NSApp.activate(ignoringOtherApps: true)
        guard alert.runModal() == .alertFirstButtonReturn else { return }

        // Snapshot what we need before SIGTERM races the active-prompts
        // janitor — once the pid dies, the marker file gets reaped.
        let payloads = sessions.map { (sid: $0.sessionId, cwd: $0.cwd, pid: $0.claudePid) }
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
                Self.openTerminalRunningClaude(
                    cwd: p.cwd,
                    sessionId: p.sid,
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
        refresh()
        tileNow()
    }

    @objc func setTextTiny() {
        let path = Paths.tinyTextFlag
        let dir = (path as NSString).deletingLastPathComponent
        try? FileManager.default.createDirectory(atPath: dir, withIntermediateDirectories: true)
        FileManager.default.createFile(atPath: path, contents: nil)
        try? FileManager.default.removeItem(atPath: Paths.nearTextFlag)
        refresh()
        tileNow()
    }

    @objc func setTextFar() {
        try? FileManager.default.removeItem(atPath: Paths.nearTextFlag)
        try? FileManager.default.removeItem(atPath: Paths.tinyTextFlag)
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
        case .blank:    return "blank"
        case .working:  return "working"
        case .complete: return "complete"
        case .awaiting: return "awaiting"
        case .stale:    return "stale"
        }
    }

    /// Off-main: pick each session's wallpaper from cache only (instant
    /// `slab-wallpaper path` probe + status-default file check), and fire
    /// one-time detached generators for anything not yet cached. Never
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
                let probe = ShellRunner.output(
                    bin, args: ["path", "subject", summary, st], timeout: 4
                )?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
                if !probe.isEmpty, fm.fileExists(atPath: probe) { pick = probe }
            }
            if pick.isEmpty {
                let def = "\(Paths.wallpaperStatusDir)/\(st).jpg"
                if fm.fileExists(atPath: def) { pick = def }
            }
            s.wallpaper = pick

            if !summary.isEmpty {
                let key = "\(st)\u{1}\(summary)"
                wallpaperLock.lock()
                let fresh = kickedWallpapers.insert(key).inserted
                wallpaperLock.unlock()
                if fresh {
                    ShellRunner.runAsync(bin, args: ["subject", summary, st])
                }
            }
            out.append(s)
        }
        return out
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
    static func statusDecor(
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
                ? (Palette(bg: (900, 8000, 2400), text: (42000, 62000, 48000),
                           bold: (56000, 65535, 60000), cursor: (24000, 56000, 34000)), "● working")
                : (Palette(bg: (55000, 65535, 58000), text: (2500, 20000, 8000),
                           bold: (1000, 13000, 4000), cursor: (4000, 42000, 15000)), "● working")
        // Complete = slate (turn done — "look when ready"). Blink: a small,
        // same-hue nudge so the window breathes rather than flashes. Direction
        // flips by appearance — dark mode lifts toward brighter slate, bright
        // mode dips toward deeper, more saturated blue (it's already near
        // white, so brighter has nowhere to go).
        case .complete:
            if blink {
                return dark
                    ? (Palette(bg: (4500, 6500, 11500), text: (48000, 52000, 62000),
                               bold: (60000, 62000, 65535), cursor: (32000, 42000, 57000)), "✓ complete")
                    : (Palette(bg: (50000, 54000, 64000), text: (8000, 14000, 30000),
                               bold: (3000, 7000, 21000), cursor: (15000, 25000, 52000)), "✓ complete")
            }
            return dark
                ? (Palette(bg: (2800, 4000, 7500), text: (48000, 52000, 62000),
                           bold: (60000, 62000, 65535), cursor: (32000, 42000, 57000)), "✓ complete")
                : (Palette(bg: (56000, 59000, 65535), text: (8000, 14000, 30000),
                           bold: (3000, 7000, 21000), cursor: (15000, 25000, 52000)), "✓ complete")
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
        for state: ClaudeSession.State, dark: Bool, blink: Bool = false
    ) -> String {
        let s: String
        switch state {
        case .blank:    s = "blank"
        case .working:  s = "working"
        case .complete: s = "complete"
        case .awaiting: s = "awaiting"
        case .stale:    s = "stale"
        }
        let base = "Slab-\(s)-\(dark ? "dark" : "light")"
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
            let isAttention = (s.state == .complete || s.state == .awaiting)
            let blink = isAttention && blinkPhase
            let decor = Self.statusDecor(for: s.state, dark: darkAppearance, blink: blink)
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
            // the page is meant to look blank.
            let title = (s.state == .blank) ? "" : "\(glyph) · \(s.titleString)"
            // Terminal.app profile name. The "-msg" suffix gives the "she
            // texted" magenta-tinted palette its own settings set so Terminal
            // windows show the accent too (their colors come from the profile,
            // not ad-hoc RGB). Provisioned below from this Assignment.palette.
            let profile = Self.profileName(for: s.state, dark: darkAppearance, blink: blink)
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
            tm.append("    try")
            tm.append("      set font size of slabSS to font size of default settings")
            tm.append("    end try")
            // Close windows without the "terminate running processes?" modal:
            // `clean commands` is Terminal's allowlist of processes ignored when
            // deciding whether to warn on close. Include shells + dev runtimes so
            // Slab can close terminals (dev servers, REPLs) without a popover.
            tm.append("    try")
            tm.append("      set clean commands of slabSS to {\"screen\", \"tmux\", \"less\", \"more\", \"view\", \"mandoc\", \"tail\", \"log\", \"top\", \"htop\", \"bash\", \"zsh\", \"sh\", \"fish\", \"node\", \"npm\", \"pnpm\", \"yarn\", \"bun\", \"deno\", \"turbo\", \"vite\", \"tsx\", \"ts-node\", \"nodemon\", \"esbuild\", \"git\", \"ssh\", \"python\", \"python3\", \"ruby\", \"claude\"}")
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
            let c = Self.statusDecor(for: s.state, dark: dark).palette.bg
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
        _ = fontSize
        let safeCwd = cwd.replacingOccurrences(of: "'", with: "'\\''")
        let safeSid = sessionId.replacingOccurrences(of: "'", with: "'\\''")
        let shellCmd = "cd '\(safeCwd)' && claude -r '\(safeSid)'"
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
        // Menlo/SF Mono; target ~24 rows × ~80 cols of monospace.
        let innerW = max(1, geom.width / maxCols - 2 * tileGutter)
        let innerH = max(1, geom.height / rows - 2 * tileGutter)
        let fontByH = Double(innerH) / (24.0 * 1.2)
        let fontByW = Double(innerW) / (80.0 * 0.6)
        let raw = min(fontByH, fontByW)
        // Far = legible from a typical sitting distance (the auto-fit
        // baseline). Near ≈ 60% — denser when sitting close. Tiny ≈ 40%
        // — for cramming many panes at the edge of legibility.
        let scale: Double
        let floor: Int
        switch size {
        case .far:  scale = 1.0;  floor = 8
        case .near: scale = 0.6;  floor = 6
        case .tiny: scale = 0.4;  floor = 4
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

    /// Tile every currently-open iTerm2 *and* Terminal.app window into one
    /// shared grid. Independent of the auto-tile flag — this is the
    /// "I forgot to enable it" / "I want to re-pack what's open" button.
    /// iTerm2 windows fill the grid first, then Terminal.app windows.
    /// Terminal gets bounds only — AppleScript can't set per-session decor
    /// or wallpaper on Terminal.app, so those windows tile but stay
    /// un-themed (the iTerm2-only port intentionally dropped Terminal decor).
    @objc func tileNow() {
        guard let geom = Self.screenGeom() else { return }
        let textSize = state.textSize
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
                lines.append("tell application \"Terminal\"")
                // Tile only non-minimized windows (matches windowCount): a
                // minimized window must neither consume a cell nor be moved.
                lines.append("    set _slabVis to (every window whose miniaturized is false)")
                for j in 0..<nTerm {
                    let cell = layout.cellAt(index: nIterm + j)
                    lines.append("    try")
                    lines.append("      set bounds of item \(j + 1) of _slabVis to {\(cell.bounds.left), \(cell.bounds.top), \(cell.bounds.right), \(cell.bounds.bottom)}")
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
