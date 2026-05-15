import AppKit

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem!
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
        state = StateSnapshot.gather()

        updateIcon()
        updateAnimTimer()

        mailTickCount += 1
        if mailTickCount >= 15 && !mailPending && !mailSyncing {
            mailTickCount = 0
            refreshMailCount()
        }

        statusItem.menu = MenuBuilder.build(state: state, mailStatus: mailStatus, target: self)

        applyTerminalDecor()
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
        let script = """
        tell application "Terminal"
            activate
            set targetTTY to "\(escaped)"
            repeat with w in windows
                set tabIndex to 0
                repeat with t in tabs of w
                    set tabIndex to tabIndex + 1
                    try
                        if (tty of t) ends with targetTTY then
                            set selected of t to true
                            set index of w to 1
                            return
                        end if
                    end try
                end repeat
            end repeat
        end tell
        """
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    @objc func sshPeer(_ sender: NSMenuItem) {
        guard let host = sender.representedObject as? String else { return }
        let script = "tell application \"Terminal\" to do script \"ssh \(host)\""
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
            for (i, entry) in entries.enumerated() {
                let cell = layout?.cellAt(index: i)
                Self.openTerminalRunningClaude(
                    cwd: entry.cwd,
                    sessionId: entry.sessionId,
                    bounds: cell?.bounds,
                    fontSize: layout?.fontSize
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
                    fontSize: layout?.fontSize
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

    /// Push the per-status Terminal profile + custom title to each window
    /// matching a live Claude session. Called from `refresh()`, but only
    /// emits an osascript when something actually changed since the last
    /// pass (state flipped, subject moved, or new session appeared).
    private func applyTerminalDecor() {
        guard state.themeByStatus else { return }
        // Each state ships a coordinated palette (bg + normal text + bold
        // text + cursor) so the whole window shifts as one tone — not a
        // new background fighting Pro's default light text. ANSI table
        // colors live on the profile (`settings set`) and applying a
        // profile resets font + window size, so we touch only the per-tab
        // knobs AppleScript exposes directly. RGB triples are AppleScript
        // colorspace 0–65535.
        typealias RGB = (Int, Int, Int)
        struct Palette { let bg: RGB?; let text: RGB?; let bold: RGB?; let cursor: RGB? }
        struct Assignment { let tty: String; let palette: Palette; let title: String; let fontSize: Int? }
        var changes: [Assignment] = []
        var seen = Set<String>()
        // Typographic gradient by state — green smallest (calm), red largest
        // (escalate). The base size comes from the most recent tile pass, so
        // the global Near/Far toggle naturally shifts every tier in lockstep.
        // Tweak these freely — the goal is that a sweep across the wall reads
        // its priority just from the size.
        //   working   (green)  1.00× — head-down, default
        //   complete  (slate)  1.10× — turn done, gentle "look when ready"
        //   awaiting  (orange) 1.25× — paused for input, focus here
        //   stale     (red)    1.45× — process dead, biggest call for cleanup
        let baseFont = self.lastTiledFontSize
        func sizeFor(_ scale: Double) -> Int? {
            baseFont.map { Int((Double($0) * scale).rounded()) }
        }
        let darkAppearance = Self.isDarkAppearance()
        for s in state.claudeSessions where !s.tty.isEmpty {
            seen.insert(s.sessionId)
            let palette: Palette
            let glyph: String
            let fontSize: Int?
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
                fontSize = sizeFor(1.00)
            // Working = green (active/healthy): pale mint text on dark forest.
            case .working:
                palette = Palette(
                    bg:     (1500,  14000, 4000),
                    text:   (42000, 60000, 46000),
                    bold:   (55000, 65535, 58000),
                    cursor: (22000, 55000, 32000))
                glyph = "● working"
                fontSize = sizeFor(1.00)
            // Complete = slate (turn done, calm "look when ready"):
            // pale lavender text on deep slate.
            case .complete:
                palette = Palette(
                    bg:     (5000,  7000,  12000),
                    text:   (46000, 50000, 60000),
                    bold:   (58000, 60000, 65535),
                    cursor: (30000, 40000, 55000))
                glyph = "✓ complete"
                fontSize = sizeFor(1.10)
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
                fontSize = sizeFor(1.25)
            // Stale = deep red (process dead, escalate): pale rose text on
            // deep red — readable but unmistakably alert.
            case .stale:
                palette = Palette(
                    bg:     (30000, 2500,  4000),
                    text:   (65535, 42000, 42000),
                    bold:   (65535, 55000, 55000),
                    cursor: (65535, 18000, 18000))
                glyph = "○ stale"
                fontSize = sizeFor(1.45)
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
                fontSize.map(String.init) ?? "-",
            ].joined(separator: "|")
            if lastTerminalDecor[s.sessionId] == key { continue }
            lastTerminalDecor[s.sessionId] = key
            changes.append(Assignment(tty: s.tty, palette: palette, title: title, fontSize: fontSize))
        }
        // Reap entries for sessions that disappeared since last tick — they
        // either died or got reaped by the janitor; either way our memo is
        // stale.
        for sid in lastTerminalDecor.keys where !seen.contains(sid) {
            lastTerminalDecor.removeValue(forKey: sid)
        }
        if changes.isEmpty { return }

        // One osascript pass that walks every window×tab once and applies
        // every change in this batch. We only touch the per-tab color knobs
        // and `custom title` — none reset font or window size, so there's
        // nothing to save/restore and nothing to flicker.
        var lines = [
            "tell application \"Terminal\"",
            "    repeat with w in windows",
            "        repeat with t in tabs of w",
            "            try",
            "                set ttyName to tty of t",
        ]
        func rgbStr(_ c: RGB) -> String { "{\(c.0), \(c.1), \(c.2)}" }
        for a in changes {
            let escTty = a.tty.replacingOccurrences(of: "\"", with: "\\\"")
            let escTitle = a.title
                .replacingOccurrences(of: "\\", with: "\\\\")
                .replacingOccurrences(of: "\"", with: "\\\"")
            lines.append("                if ttyName ends with \"\(escTty)\" then")
            if let bg = a.palette.bg {
                lines.append("                    set background color of t to \(rgbStr(bg))")
            }
            if let text = a.palette.text {
                lines.append("                    set normal text color of t to \(rgbStr(text))")
            }
            if let bold = a.palette.bold {
                lines.append("                    set bold text color of t to \(rgbStr(bold))")
            }
            if let cursor = a.palette.cursor {
                lines.append("                    set cursor color of t to \(rgbStr(cursor))")
            }
            lines.append("                    set custom title of t to \"\(escTitle)\"")
            // Font sets snap Terminal to the row/col grid, so save the window's
            // bounds and restore them — typography changes per state, geometry
            // stays put with the tile.
            if let fs = a.fontSize {
                lines.append("                    set savedBounds to bounds of w")
                lines.append("                    set font size of t to \(fs)")
                lines.append("                    set bounds of w to savedBounds")
            }
            lines.append("                end if")
        }
        lines.append(contentsOf: [
            "            end try",
            "        end repeat",
            "    end repeat",
            "end tell",
        ])
        let script = lines.joined(separator: "\n")
        ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
    }

    /// Spawn a new Terminal.app window that cd's into `cwd` and resumes the
    /// given session. Optionally sets bounds + font size so the caller can
    /// tile the windows. Single-quoted shell args are escaped so a path with
    /// apostrophes can't break out.
    private static func openTerminalRunningClaude(
        cwd: String,
        sessionId: String,
        bounds: (left: Int, top: Int, right: Int, bottom: Int)? = nil,
        fontSize: Int? = nil
    ) {
        let safeCwd = cwd.replacingOccurrences(of: "'", with: "'\\''")
        let safeSid = sessionId.replacingOccurrences(of: "'", with: "'\\''")
        let shellCmd = "cd '\(safeCwd)' && claude -r '\(safeSid)'"
        let escapedCmd = shellCmd.replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")

        var lines = ["tell application \"Terminal\"", "    activate", "    do script \"\(escapedCmd)\""]
        // Order matters: setting font size makes Terminal snap the window to
        // the nearest whole row/col grid, which can shrink it. So we set
        // bounds, then font, then bounds again — the second `set bounds`
        // forces the pixel size we actually want, even if Terminal would
        // rather round it.
        if let b = bounds {
            lines.append("    set bounds of front window to {\(b.left), \(b.top), \(b.right), \(b.bottom)}")
        }
        if let fs = fontSize {
            lines.append("    set font size of selected tab of front window to \(fs)")
        }
        if let b = bounds {
            lines.append("    set bounds of front window to {\(b.left), \(b.top), \(b.right), \(b.bottom)}")
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

    struct TileLayout {
        let cols: Int
        let rows: Int
        let cellWidth: Int
        let cellHeight: Int
        let originX: Int  // visible-frame left edge
        let originY: Int  // visible-frame top edge (below menu bar), AS coords
        let fontSize: Int

        struct Cell {
            let bounds: (left: Int, top: Int, right: Int, bottom: Int)
        }

        func cellAt(index: Int) -> Cell {
            let row = index / cols
            let col = index % cols
            let left = originX + col * cellWidth + tileGutter
            let top = originY + row * cellHeight + tileGutter
            let right = originX + (col + 1) * cellWidth - tileGutter
            let bottom = originY + (row + 1) * cellHeight - tileGutter
            return Cell(bounds: (left, top, right, bottom))
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

    /// Pack `count` windows into the most-square grid that fits the main
    /// display's visible frame. Font size scales down for denser grids so
    /// even N=10 stays legible.
    private static func computeTileLayout(count: Int, geom: ScreenGeom, near: Bool = false) -> TileLayout? {
        guard count > 0 else { return nil }
        let cols = max(1, Int(ceil(Double(count).squareRoot())))
        let rows = max(1, Int(ceil(Double(count) / Double(cols))))
        let cellW = geom.width / cols
        let cellH = geom.height / rows
        // Inner pixels available per window after the gutter is taken out.
        let innerW = max(1, cellW - 2 * tileGutter)
        let innerH = max(1, cellH - 2 * tileGutter)

        // Pick the largest font that fits ~24 rows × ~80 cols of monospace
        // in the inner area. A char cell is roughly fontSize * 0.6 wide and
        // fontSize * 1.2 tall in Menlo/SF Mono. Enlarging is fine — for low
        // N the cells are huge, so the font scales up rather than us
        // wasting empty pixels at a fixed default.
        let fontByH = Double(innerH) / (24.0 * 1.2)
        let fontByW = Double(innerW) / (80.0 * 0.6)
        let raw = min(fontByH, fontByW)
        // Near = "I'm sitting close, give me density" → 60% of the
        // legible-from-typical-distance size, floored at 6pt.
        let scaled = near ? raw * 0.6 : raw
        let fontSize = max(near ? 6 : 8, Int(scaled.rounded()))

        return TileLayout(
            cols: cols,
            rows: rows,
            cellWidth: cellW,
            cellHeight: cellH,
            originX: geom.originX,
            originY: geom.originY,
            fontSize: fontSize
        )
    }

    /// Tile every currently-open Terminal.app window into the same grid the
    /// auto-tile path uses. Independent of the auto-tile flag — this is the
    /// "I forgot to enable it" / "I want to re-pack what's open" button.
    @objc func tileNow() {
        guard let geom = Self.screenGeom() else { return }
        let near = state.nearText
        DispatchQueue.global(qos: .userInitiated).async {
            // Ask Terminal for its window count first so we can size the
            // grid to what's actually on screen.
            let countOut = ShellRunner.run(
                "/usr/bin/osascript",
                args: ["-e", "tell application \"Terminal\" to count windows"],
                timeout: 5
            ).output.trimmingCharacters(in: .whitespacesAndNewlines)
            guard let n = Int(countOut), n > 0 else { return }
            guard let layout = Self.computeTileLayout(count: n, geom: geom, near: near) else { return }

            // Remember the tile's base font + reset decor memo. The script
            // below resets every window to base; without this, a session
            // already in `.awaiting` would still match its old "we pushed
            // the bumped font" key and skip the re-bump on the next refresh.
            DispatchQueue.main.async { [weak self] in
                self?.lastTiledFontSize = layout.fontSize
                self?.lastTerminalDecor.removeAll()
            }

            var lines: [String] = ["tell application \"Terminal\"", "    activate"]
            for i in 0..<n {
                let cell = layout.cellAt(index: i)
                let asIndex = i + 1  // AppleScript is 1-based
                let boundsLine = "    set bounds of window \(asIndex) to {\(cell.bounds.left), \(cell.bounds.top), \(cell.bounds.right), \(cell.bounds.bottom)}"
                // bounds → font → bounds: same pattern as the restore path
                // so Terminal's row/col snap can't shrink the cell.
                lines.append(boundsLine)
                lines.append("    set font size of selected tab of window \(asIndex) to \(layout.fontSize)")
                lines.append(boundsLine)
            }
            lines.append("end tell")
            let script = lines.joined(separator: "\n")
            ShellRunner.runAsync("/usr/bin/osascript", args: ["-e", script])
        }
    }

    private func syncMail(account: String?) {
        mailSyncing = true
        mailStatus = "syncing…"
        statusItem.menu = MenuBuilder.build(state: state, mailStatus: mailStatus, target: self)

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
