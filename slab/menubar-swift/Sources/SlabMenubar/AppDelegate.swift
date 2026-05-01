import AppKit

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem!
    private var refreshTimer: Timer?
    private var animTimer: Timer?
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
            ShellRunner.runAsync("/usr/bin/pkill", args: ["-TERM", "-f", "lid-reactive.py"])
            ShellRunner.runAsync("/usr/bin/pkill", args: ["-x", "afplay"])
        }
        refresh()
    }

    @objc func syncBoth() { syncMail(account: nil) }
    @objc func syncAcMail() { syncMail(account: "ac-mail") }
    @objc func syncJasMail() { syncMail(account: "jas-mail") }

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
