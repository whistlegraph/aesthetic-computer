import AppKit

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
    /// `sessionId → "<state>|<subject>"` of the last theme/title we pushed
    /// to Terminal, so the per-tick refresh only fires osascript when
    /// something actually changed.
    private var lastTerminalDecor: [String: String] = [:]
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
    private var state = StateSnapshot()
    private let passphraseServer = PassphraseServer()

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
    }

    func applicationWillTerminate(_ notification: Notification) {
        passphraseServer.stop()
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

                self.updateIcon()
                self.updateAnimTimer()

                self.mailTickCount += 1
                if self.mailTickCount >= 15 && !self.mailPending && !self.mailSyncing {
                    self.mailTickCount = 0
                    self.refreshMailCount()
                }

                // No menu rebuild here — it's lazy via menuNeedsUpdate(_:).
                self.applyTerminalDecor()
            }
        }
    }

    // MARK: - NSMenuDelegate

    /// Rebuild the menu contents right before it displays, from the most
    /// recent cached snapshot. Pure in-memory work (sub-millisecond), so the
    /// dropdown pops instantly and stays smooth while tracking.
    func menuNeedsUpdate(_ menu: NSMenu) {
        guard menu === self.menu else { return }
        MenuBuilder.populate(menu, state: state, mailStatus: mailStatus, target: self)
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
        if !state.claudeSessions.isEmpty {
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

    // MARK: - Menu actions

    @objc func openDaemonLog() {
        ShellRunner.run("/usr/bin/open", args: ["-a", "Console", Paths.lidLog])
    }

    @objc func openSoundsFolder() {
        ShellRunner.run("/usr/bin/open", args: [Paths.soundsDir])
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
        tell application "iTerm2"
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
        tell application "iTerm2"
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
        let near = state.nearText
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
            let layout = geom.flatMap { Self.computeTileLayout(count: entries.count, geom: $0, near: near) }
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
        let layout = geom.flatMap { Self.computeTileLayout(count: payloads.count, geom: $0, near: state.nearText) }
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
        refresh()
        tileNow()
    }

    @objc func setTextFar() {
        try? FileManager.default.removeItem(atPath: Paths.nearTextFlag)
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

    /// Push the per-status palette + custom title + wallpaper to each iTerm2
    /// session matching a live Claude session. Called from `refresh()`, but
    /// only emits an osascript when something actually changed since the
    /// last pass (state flipped, subject moved, wallpaper landed, or a new
    /// session appeared). iTerm2 exposes these as per-session properties, so
    /// nothing resets font/window geometry — no save/restore, no flicker.
    private func applyTerminalDecor() {
        guard state.themeByStatus else { return }
        // Each state ships a coordinated palette (bg + foreground + bold +
        // cursor) so the whole window shifts as one tone. RGB triples are
        // AppleScript colorspace 0–65535.
        typealias RGB = (Int, Int, Int)
        struct Palette { let bg: RGB?; let text: RGB?; let bold: RGB?; let cursor: RGB? }
        struct Assignment { let tty: String; let palette: Palette; let title: String; let wallpaper: String }
        var changes: [Assignment] = []
        var seen = Set<String>()
        let darkAppearance = Self.isDarkAppearance()
        for s in state.claudeSessions where !s.tty.isEmpty {
            seen.insert(s.sessionId)
            let palette: Palette
            let glyph: String
            switch s.state {
            // Blank = pure macOS appearance (white in light mode, black in
            // dark mode) so a fresh window reads as a blank page until the
            // first prompt fires. Text/cursor flip with appearance so
            // default ANSI output stays legible on either page color.
            case .blank:
                if darkAppearance {
                    palette = Palette(
                        bg:     (0, 0, 0),
                        text:   (60000, 60000, 60000),
                        bold:   (65535, 65535, 65535),
                        cursor: (50000, 50000, 50000))
                } else {
                    palette = Palette(
                        bg:     (65535, 65535, 65535),
                        text:   (8000,  8000,  8000),
                        bold:   (0,     0,     0),
                        cursor: (20000, 20000, 20000))
                }
                glyph = ""
            // Working = green (active/healthy): pale mint text on dark forest.
            case .working:
                palette = Palette(
                    bg:     (1500,  14000, 4000),
                    text:   (42000, 60000, 46000),
                    bold:   (55000, 65535, 58000),
                    cursor: (22000, 55000, 32000))
                glyph = "● working"
            // Complete = slate (turn done, calm "look when ready"):
            // pale lavender text on deep slate.
            case .complete:
                palette = Palette(
                    bg:     (5000,  7000,  12000),
                    text:   (46000, 50000, 60000),
                    bold:   (58000, 60000, 65535),
                    cursor: (30000, 40000, 55000))
                glyph = "✓ complete"
            // Awaiting = warm amber (needs input, focus pop): cream text on
            // amber so the warm tone reads coherently rather than fighting
            // the default light gray.
            case .awaiting:
                palette = Palette(
                    bg:     (32000, 18000, 1500),
                    text:   (65535, 58000, 38000),
                    bold:   (65535, 65535, 50000),
                    cursor: (65535, 45000, 8000))
                glyph = "◉ awaiting"
            // Stale = deep red (process dead, escalate): pale rose text on
            // deep red — readable but unmistakably alert.
            case .stale:
                palette = Palette(
                    bg:     (30000, 2500,  4000),
                    text:   (65535, 42000, 42000),
                    bold:   (65535, 55000, 55000),
                    cursor: (65535, 18000, 18000))
                glyph = "○ stale"
            }
            // Blank windows get an empty custom title so Terminal shows just
            // its default tty/process line — no "● working · …" badge while
            // the page is meant to look blank.
            let title = (s.state == .blank) ? "" : "\(glyph) · \(s.titleString)"
            func keyOf(_ c: RGB?) -> String { c.map { "\($0.0),\($0.1),\($0.2)" } ?? "-" }
            let key = [
                keyOf(palette.bg),
                keyOf(palette.text),
                keyOf(palette.bold),
                keyOf(palette.cursor),
                title,
                s.wallpaper.isEmpty ? "-" : s.wallpaper,
            ].joined(separator: "|")
            if lastTerminalDecor[s.sessionId] == key { continue }
            lastTerminalDecor[s.sessionId] = key
            changes.append(Assignment(tty: s.tty, palette: palette, title: title, wallpaper: s.wallpaper))
        }
        // Reap entries for sessions that disappeared since last tick — they
        // either died or got reaped by the janitor; either way our memo is
        // stale.
        for sid in lastTerminalDecor.keys where !seen.contains(sid) {
            lastTerminalDecor.removeValue(forKey: sid)
        }
        if changes.isEmpty { return }

        // One osascript pass that walks every iTerm2 window→tab→session
        // once and applies every change in this batch. These are all
        // per-session properties — none reset font or window geometry
        // (proven), so there's nothing to save/restore and nothing to
        // flicker. RGB triples are AppleScript colorspace 0–65535.
        var lines = [
            "tell application \"iTerm2\"",
            "    repeat with w in windows",
            "        repeat with t in tabs of w",
            "            repeat with s in sessions of t",
            "                try",
            "                    set ttyName to tty of s",
        ]
        func rgbStr(_ c: RGB) -> String { "{\(c.0), \(c.1), \(c.2)}" }
        func esc(_ v: String) -> String {
            v.replacingOccurrences(of: "\\", with: "\\\\")
                .replacingOccurrences(of: "\"", with: "\\\"")
        }
        for a in changes {
            // tty of session is "/dev/ttysNNN"; we record "ttysNNN".
            let escTty = esc(a.tty)
            lines.append("                    if ttyName ends with \"\(escTty)\" then")
            if let bg = a.palette.bg {
                lines.append("                        set background color of s to \(rgbStr(bg))")
            }
            if let text = a.palette.text {
                lines.append("                        set foreground color of s to \(rgbStr(text))")
            }
            if let bold = a.palette.bold {
                lines.append("                        set bold color of s to \(rgbStr(bold))")
            }
            if let cursor = a.palette.cursor {
                lines.append("                        set cursor color of s to \(rgbStr(cursor))")
            }
            lines.append("                        set name of s to \"\(esc(a.title))\"")
            // Per-session wallpaper. Empty path clears any prior image so a
            // session that lost its wallpaper falls back to the flat palette.
            lines.append("                        set background image of s to \"\(esc(a.wallpaper))\"")
            lines.append("                    end if")
        }
        lines.append(contentsOf: [
            "                end try",
            "            end repeat",
            "        end repeat",
            "    end repeat",
            "end tell",
        ])
        let script = lines.joined(separator: "\n")
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    /// Which terminal app to spawn restored sessions into. Honor whatever
    /// the user actually has open: if only one of iTerm2 / Terminal.app has
    /// windows, use it; if both, defer to the frontmost one; if neither,
    /// fall back to iTerm2 (the port default). `windowCount`'s `is running`
    /// probe never launches an app, so this is side-effect free.
    private static func preferredTerminalApp() -> String {
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
                "tell application \"iTerm2\"",
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
    private static func computeTileLayout(count: Int, geom: ScreenGeom, near: Bool = false) -> TileLayout? {
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
        // Near = "I'm sitting close, give me density" → 60% of the
        // legible-from-typical-distance size, floored at 6pt.
        let scaled = near ? raw * 0.6 : raw
        let fontSize = max(near ? 6 : 8, Int(scaled.rounded()))

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
        let near = state.nearText
        DispatchQueue.global(qos: .userInitiated).async {
            // Size the grid to everything on screen across both apps. The
            // `is running` guard never launches a quit Terminal.app, so a
            // non-running Terminal contributes zero cells.
            let nIterm = Self.windowCount(app: "iTerm2")
            let nTerm = Self.windowCount(app: "Terminal")
            let n = nIterm + nTerm
            guard n > 0 else { return }
            guard let layout = Self.computeTileLayout(count: n, geom: geom, near: near) else { return }

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
                lines.append("tell application \"iTerm2\"")
                lines.append("    activate")
                for i in 0..<nIterm {
                    let cell = layout.cellAt(index: i)
                    lines.append("    set bounds of window \(i + 1) to {\(cell.bounds.left), \(cell.bounds.top), \(cell.bounds.right), \(cell.bounds.bottom)}")
                }
                lines.append("end tell")
            }
            if nTerm > 0 {
                lines.append("tell application \"Terminal\"")
                for j in 0..<nTerm {
                    let cell = layout.cellAt(index: nIterm + j)
                    lines.append("    set bounds of window \(j + 1) to {\(cell.bounds.left), \(cell.bounds.top), \(cell.bounds.right), \(cell.bounds.bottom)}")
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
    private static func windowCount(app: String) -> Int {
        let script = """
        if application "\(app)" is running then
          tell application "\(app)" to count windows
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
