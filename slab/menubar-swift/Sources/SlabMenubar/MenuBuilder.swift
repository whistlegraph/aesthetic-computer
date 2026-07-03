import AppKit

enum MenuBuilder {
    static func build(
        state: StateSnapshot,
        mailStatus: String,
        imsgStatus: String,
        imsgConfigured: Bool,
        asana: AsanaState,
        deploy: DeployStatusState,
        target: AppDelegate
    ) -> NSMenu {
        let menu = NSMenu()
        populate(
            menu,
            state: state,
            mailStatus: mailStatus,
            imsgStatus: imsgStatus,
            imsgConfigured: imsgConfigured,
            asana: asana,
            deploy: deploy,
            target: target
        )
        return menu
    }

    /// Rebuild `menu`'s contents in place from the latest cached state.
    /// Cheap, main-thread, in-memory work (no shelling out) — called from
    /// `menuNeedsUpdate(_:)` the instant before the menu displays, so the
    /// menu is always fresh on open without ever being swapped while tracked.
    static func populate(
        _ menu: NSMenu,
        state: StateSnapshot,
        mailStatus: String,
        imsgStatus: String,
        imsgConfigured: Bool,
        asana: AsanaState,
        deploy: DeployStatusState,
        target: AppDelegate
    ) {
        menu.removeAllItems()
        menu.autoenablesItems = false

        menu.addItem(info("Status: \(state.statusLine)"))
        menu.addItem(buildSystem(state: state, target: target))
        menu.addItem(.separator())

        appendPopRenders(to: menu, state: state)
        appendClaude(to: menu, state: state, target: target)
        menu.addItem(info("Subagents in flight: \(state.activeSubagents)"))
        menu.addItem(.separator())

        menu.addItem(buildTailnet(state: state, target: target))
        menu.addItem(buildMail(status: mailStatus, target: target))
        menu.addItem(buildImsg(status: imsgStatus, configured: imsgConfigured, target: target))
        menu.addItem(buildAsana(state: asana, target: target))
        menu.addItem(buildDeploy(state: deploy, target: target))
        appendOvertime(to: menu, target: target)
        menu.addItem(buildPdf(target: target))
        menu.addItem(buildVideo(target: target))
        menu.addItem(buildAudio(target: target))
        if state.deskflow.configured {
            menu.addItem(buildDeskflow(state: state, target: target))
        }

        // Request for Audio — sing a /pop melody, one note at a time. The
        // wizard plays the pitch + shows the word per note, records, then
        // recompiles the track and plays it back.
        let rfa = item("🎙 Request for Audio — sing “hum”",
                       selector: #selector(AppDelegate.requestForAudio(_:)), target: target)
        rfa.representedObject = "hum"
        rfa.toolTip = "Open the RFA wizard in iTerm2 — sing the 'hum' melody note by note, then hear it recompiled."
        menu.addItem(rfa)

        // Call recording — captures mic (+ system audio if an aggregate
        // device is wired) to a WAV in ~/Documents/Shelf/meetings/. On stop
        // the WAV is handed off to meetings/cli.mjs which transcribes,
        // detects whistlepops, and builds an arxiv-style PDF.
        menu.addItem(buildCall(state: state, target: target))
        menu.addItem(.separator())

        // Close the frontmost Terminal / iTerm2 window from the menubar.
        // Useful for closing Claude sessions cleanly without reaching for
        // the keyboard, and a faster path than ⌘W when the menu is already
        // open. Auto-re-tiles the remaining windows when auto-tile is on.
        let closeTerm = item("Close front terminal window",
                             selector: #selector(AppDelegate.closeFrontTerminal),
                             target: target)
        closeTerm.toolTip = "Close the frontmost Terminal or iTerm2 window. If auto-tile is on, the remaining windows re-tile."
        menu.addItem(closeTerm)

        // Tidy a scattered desktop of Stickies notes into a tight collapsed
        // column in the top-left via Stickies' own "Arrange By" command.
        let stackSticky = item("Arrange Stickies (collapse + top-left)",
                               selector: #selector(AppDelegate.stackStickies),
                               target: target)
        stackSticky.toolTip = "Run Stickies' built-in Arrange By (Color) with Collapse All — packs all notes into a collapsed column in the top-left corner."
        menu.addItem(stackSticky)
        menu.addItem(.separator())

        let stayAwake = item("Stay awake (lid closed)", selector: #selector(AppDelegate.toggleStayAwake), target: target)
        stayAwake.state = state.sleepDisabled ? .on : .off
        menu.addItem(stayAwake)
        menu.addItem(item("Sleep now", selector: #selector(AppDelegate.sleepNow), target: target))

        let saver = item("Start screensaver", selector: #selector(AppDelegate.startScreensaver), target: target)
        saver.toolTip = "Launch the currently-selected screen saver now (Slab Status if chosen in System Settings)."
        menu.addItem(saver)

        menu.addItem(buildAppearance(target: target))

        let mute = item("Mute ambient sonification", selector: #selector(AppDelegate.toggleMute), target: target)
        mute.state = state.muted ? .on : .off
        menu.addItem(mute)
        menu.addItem(.separator())

        menu.addItem(item("Open daemon log", selector: #selector(AppDelegate.openDaemonLog), target: target))
        menu.addItem(item("Open sounds folder", selector: #selector(AppDelegate.openSoundsFolder), target: target))
        menu.addItem(.separator())

        menu.addItem(item("Reload daemon", selector: #selector(AppDelegate.reloadDaemon), target: target))
        menu.addItem(item("Quit menu bar", selector: #selector(AppDelegate.quitMenubar), target: target))
    }

    private static func info(_ title: String) -> NSMenuItem {
        let it = NSMenuItem(title: title, action: nil, keyEquivalent: "")
        it.isEnabled = false
        return it
    }

    private static func item(_ title: String, selector: Selector, target: AnyObject) -> NSMenuItem {
        let it = NSMenuItem(title: title, action: selector, keyEquivalent: "")
        it.target = target
        it.isEnabled = true
        return it
    }

    /// Extra mail accounts loaded from untracked `~/.config/slab/mail-accounts.json`
    /// so client identities never appear in tracked code. Returns [] if absent
    /// or malformed.
    private static func extraMailAccounts() -> [(account: String, label: String)] {
        guard let data = FileManager.default.contents(atPath: Paths.mailAccountsConfig),
              let arr = try? JSONSerialization.jsonObject(with: data) as? [[String: String]]
        else { return [] }
        return arr.compactMap { dict in
            guard let account = dict["account"], !account.isEmpty else { return nil }
            return (account, dict["label"] ?? account)
        }
    }

    /// Whole-machine RAM/swap/load line with a submenu of the top
    /// memory hogs — the "what's cutting into resources" view for the
    /// 8 GB Neo. The title goes orange once SystemStats.pressure trips
    /// (free < 15% or swap > 90% full) so a squeeze is visible at a
    /// glance before the machine starts thrashing.
    private static func buildSystem(state: StateSnapshot, target: AppDelegate) -> NSMenuItem {
        let s = state.system
        var bits: [String] = []
        if s.memFreePct > 0 { bits.append("mem \(s.memFreePct)% free") }
        if s.swapTotalMB > 0 {
            bits.append(String(format: "swap %.1f/%.0fG",
                               Double(s.swapUsedMB) / 1024, Double(s.swapTotalMB) / 1024))
        }
        if s.loadAvg > 0 { bits.append(String(format: "load %.1f", s.loadAvg)) }
        let title = bits.isEmpty ? "System: —" : bits.joined(separator: " · ")

        let parent = NSMenuItem(title: title, action: nil, keyEquivalent: "")
        parent.isEnabled = true
        var attrs: [NSAttributedString.Key: Any] = [.font: renderFont]
        if s.pressure { attrs[.foregroundColor] = NSColor.systemOrange }
        parent.attributedTitle = NSAttributedString(string: title, attributes: attrs)

        let sub = NSMenu()
        sub.autoenablesItems = false
        for hog in s.hogs {
            let mem = "\(hog.rssMB)M".padding(toLength: 6, withPad: " ", startingAt: 0)
            let cpu = String(format: "%3.0f%%", hog.cpu)
                .padding(toLength: 6, withPad: " ", startingAt: 0)
            let row = NSMenuItem(title: "", action: nil, keyEquivalent: "")
            row.isEnabled = false
            row.attributedTitle = NSAttributedString(
                string: "\(mem)\(cpu)\(hog.name)",
                attributes: [.font: renderFont])
            sub.addItem(row)
        }
        if !s.hogs.isEmpty { sub.addItem(.separator()) }
        let open = item("Open Activity Monitor",
                        selector: #selector(AppDelegate.openActivityMonitor),
                        target: target)
        sub.addItem(open)
        parent.submenu = sub
        return parent
    }

    /// Identifier prefix used to tag each pop-render NSMenuItem so
    /// `updatePopRenders(in:state:)` can find and rewrite it in place
    /// while the menu is open. Full identifier = prefix + heartbeat id.
    static let popRenderItemIDPrefix = "pop-render:"

    /// Monospaced font used for both initial build and live updates so
    /// the bar/cell widths stay aligned tick-to-tick.
    private static let renderFont = NSFont.monospacedSystemFont(ofSize: 11, weight: .regular)

    /// Temporary progress bars — one monospaced row per live /pop render
    /// heartbeat (audio / illy / video). Present only while a render is
    /// running; the rows vanish when the heartbeat files are gone.
    ///
    /// Each row is tagged with `pop-render:<id>` so
    /// `updatePopRenders(in:state:)` can rewrite its title in place while
    /// the menu is on screen, without restructuring the menu mid-tracking.
    private static func appendPopRenders(to menu: NSMenu, state: StateSnapshot) {
        guard !state.popRenders.isEmpty else { return }
        let n = state.popRenders.count
        menu.addItem(info("Renders: \(n) running"))
        for r in state.popRenders {
            let it = NSMenuItem(title: renderLine(r), action: nil, keyEquivalent: "")
            it.isEnabled = false
            it.identifier = NSUserInterfaceItemIdentifier(popRenderItemIDPrefix + r.id)
            it.attributedTitle = NSAttributedString(
                string: renderLine(r),
                attributes: [.font: renderFont])
            menu.addItem(it)
        }
        menu.addItem(.separator())
    }

    /// Rewrite the title of each pop-render row from the latest snapshot.
    /// Called from a fast `.common`-mode timer started in `menuWillOpen`,
    /// so the bars tick live while the dropdown is open. Pure title
    /// mutation — never adds or removes items mid-tracking (that's the
    /// historical sharp edge); rows that disappear from the heartbeat
    /// dir just stop updating until the menu next opens.
    static func updatePopRenders(in menu: NSMenu, state: StateSnapshot) {
        guard !state.popRenders.isEmpty else { return }
        var byID: [String: PopRender] = [:]
        for r in state.popRenders { byID[r.id] = r }
        for item in menu.items {
            guard let raw = item.identifier?.rawValue,
                  raw.hasPrefix(popRenderItemIDPrefix) else { continue }
            let id = String(raw.dropFirst(popRenderItemIDPrefix.count))
            guard let r = byID[id] else { continue }
            let line = renderLine(r)
            if item.attributedTitle?.string == line { continue }
            item.attributedTitle = NSAttributedString(
                string: line,
                attributes: [.font: renderFont])
        }
    }

    /// `illy   ▓▓▓▓▓▓░░░░░░  58%  142/240  helpabeach-p`
    private static func renderLine(_ r: PopRender) -> String {
        let width = 12
        let bar: String
        let pctText: String
        if let p = r.pct {
            let raw = (Double(p) / 100.0) * Double(width)
            let filled = max(0, min(width, Int(raw.rounded())))
            bar = String(repeating: "▓", count: filled)
                + String(repeating: "░", count: width - filled)
            pctText = "\(p)%".padding(toLength: 4, withPad: " ", startingAt: 0)
        } else {
            bar = String(repeating: "·", count: width)
            pctText = " ⋯  "
        }
        let type = r.type.padding(toLength: 6, withPad: " ", startingAt: 0)
        // Pad the count cell to the width of the larger total so adjacent
        // rows align (frame counts stay in a fixed column instead of
        // jitter-marching as digits roll over). Empty cell when the writer
        // didn't supply done/total — keeps spacing consistent.
        let countText: String
        if let d = r.done, let t = r.total, t > 0 {
            let ts = String(t)
            let ds = String(d)
            let padded = ds.count >= ts.count
                ? ds
                : String(repeating: " ", count: ts.count - ds.count) + ds
            countText = "\(padded)/\(ts)"
        } else {
            countText = ""
        }
        let countCell = countText.padding(toLength: 11, withPad: " ", startingAt: 0)
        // Driver-process resident memory, when the heartbeat pid is alive —
        // ties each running job to what it actually costs in RAM.
        let memCell = (r.rssMB.map { "\($0)M" } ?? "")
            .padding(toLength: 7, withPad: " ", startingAt: 0)
        return "\(type)\(bar)  \(pctText)  \(countCell)\(memCell)\(r.label)"
    }

    /// "Appearance" submenu — flip macOS Dark Mode on this host plus the
    /// reachable tailscale macs in one click. Mirrors the global ⌃⌥⌘A toggle.
    private static func buildAppearance(target: AppDelegate) -> NSMenuItem {
        let parent = NSMenuItem(title: "Appearance", action: nil, keyEquivalent: "")
        let sub = NSMenu()

        let dark = NSMenuItem(title: "All Macs → Dark",
                              action: #selector(AppDelegate.setAppearance(_:)), keyEquivalent: "")
        dark.target = target
        dark.representedObject = "dark"
        sub.addItem(dark)

        let light = NSMenuItem(title: "All Macs → Light",
                               action: #selector(AppDelegate.setAppearance(_:)), keyEquivalent: "")
        light.target = target
        light.representedObject = "light"
        sub.addItem(light)

        sub.addItem(.separator())
        let toggle = NSMenuItem(title: "Toggle  (⌃⌥⌘A)",
                                action: #selector(AppDelegate.toggleAppearance), keyEquivalent: "")
        toggle.target = target
        sub.addItem(toggle)

        parent.submenu = sub
        return parent
    }

    private static func buildTailnet(state: StateSnapshot, target: AppDelegate) -> NSMenuItem {
        let online = state.tailnetPeers.filter { $0.online }.count
        let total = state.tailnetPeers.count
        let label: String
        if total == 0 {
            label = "Tailnet: —"
        } else {
            label = "Tailnet: \(online)/\(total) online"
        }
        let parent = NSMenuItem(title: label, action: nil, keyEquivalent: "")
        let sub = NSMenu()

        if state.tailnetPeers.isEmpty {
            sub.addItem(info("(no peers)"))
        } else {
            for peer in state.tailnetPeers {
                let marker = peer.online ? "●" : "○"
                let entry = NSMenuItem(
                    title: "\(marker) \(peer.hostname)",
                    action: #selector(AppDelegate.sshPeer(_:)),
                    keyEquivalent: ""
                )
                entry.target = target
                entry.representedObject = peer.hostname
                entry.isEnabled = true
                sub.addItem(entry)
            }
        }
        parent.submenu = sub
        return parent
    }

    /// Inline the Claude session list straight into the main menu so the
    /// user can see (and click into) every thread without drilling through
    /// a submenu. Each row is colored with the same palette as its icon
    /// edge — cyan for working, red for awaiting, gray for stale — so the
    /// menubar polygon and the dropdown read as the same picture.
    private static func appendClaude(to menu: NSMenu, state: StateSnapshot, target: AppDelegate) {
        let sessions = state.claudeSessions
        let header: String
        if sessions.isEmpty {
            header = "Claude: idle"
        } else if state.anyAwaiting {
            header = "Claude: \(state.awaitingCount) awaiting · \(sessions.count) active"
        } else {
            header = "Claude: \(sessions.count) active"
        }
        menu.addItem(info(header))

        menu.addItem(buildRestoreSubmenu(state: state, target: target))

        if !sessions.isEmpty {
            let restartAll = item(
                "Restart all active (\(sessions.count))",
                selector: #selector(AppDelegate.restartAllActive),
                target: target
            )
            menu.addItem(restartAll)
        }

        if sessions.isEmpty { return }

        for s in sessions {
            let dot: String
            switch s.state {
            case .awaiting:    dot = "◉"
            case .working:     dot = "●"
            case .rendering:   dot = "◐"   // turn done, its render still cooking
            case .complete:    dot = "✓"
            case .interrupted: dot = "✕"   // Esc'd, idle at the prompt
            case .blank:       dot = "·"   // tiny midpoint — fresh, nothing yet
            case .stale:       dot = "○"
            }
            // Remote sessions (mirrored from jasellite et al. by the bridge)
            // wear a 🛰 + host badge so they read distinctly from local ones.
            let remoteBadge = s.isRemote ? "  🛰 \(s.remoteHost)" : ""
            let tail = (s.cwdLabel.isEmpty ? "" : "  ·  \(s.cwdLabel)") + remoteBadge
            // Blank sessions have no subject yet — substitute a placeholder
            // so the menu row reads as "· (new) · aesthetic-computer" instead
            // of the awkward "·   ·  aesthetic-computer".
            let subjectText: String = {
                let s2 = s.shortSubject.trimmingCharacters(in: .whitespaces)
                if !s2.isEmpty { return s2 }
                return s.state == .blank ? "(new)" : s2
            }()
            // The session's sticky emoji rides between dot and subject so a
            // menu row and its window wear the same mark (see TitleEmoji).
            let emojiPrefix = s.emoji.isEmpty ? "" : "\(s.emoji) "
            let entry = NSMenuItem(
                title: "\(dot) \(emojiPrefix)\(subjectText)\(tail)",
                action: #selector(AppDelegate.focusClaudeSession(_:)),
                keyEquivalent: ""
            )
            entry.target = target
            entry.representedObject = s.tty
            entry.toolTip = sessionTooltip(s)
            entry.isEnabled = !s.tty.isEmpty
            entry.attributedTitle = coloredTitle(for: s, dot: dot, subject: "\(emojiPrefix)\(subjectText)", tail: tail)
            menu.addItem(entry)
        }
    }

    /// "Restore threads" submenu — recovery hatch for the case where
    /// Terminal.app dies and takes every Claude tab with it. Each item opens
    /// N fresh Terminal windows running `claude -r <session-id>` for the
    /// most-recently-modified sessions on disk that aren't already live.
    private static func buildRestoreSubmenu(state: StateSnapshot, target: AppDelegate) -> NSMenuItem {
        let parent = NSMenuItem(title: "Restore threads", action: nil, keyEquivalent: "")
        let sub = NSMenu()

        let tile = item("Auto tile windows", selector: #selector(AppDelegate.toggleAutoTile), target: target)
        tile.state = state.autoTile ? .on : .off
        sub.addItem(tile)
        let tileNow = item("Tile now", selector: #selector(AppDelegate.tileNow), target: target)
        tileNow.keyEquivalent = "t"
        tileNow.keyEquivalentModifierMask = [.command, .option]
        sub.addItem(tileNow)

        // Text size — radio trio so the active mode is visible at a glance.
        // Far = auto-fit "suitable", Near = denser, Tiny = edge-of-legibility.
        let textParent = NSMenuItem(title: "Text size", action: nil, keyEquivalent: "")
        let textSub = NSMenu()
        let farItem = NSMenuItem(title: "Far (suitable)", action: #selector(AppDelegate.setTextFar), keyEquivalent: "")
        farItem.target = target
        farItem.state = state.textSize == .far ? .on : .off
        textSub.addItem(farItem)
        let nearItem = NSMenuItem(title: "Near (small)", action: #selector(AppDelegate.setTextNear), keyEquivalent: "")
        nearItem.target = target
        nearItem.state = state.textSize == .near ? .on : .off
        textSub.addItem(nearItem)
        let tinyItem = NSMenuItem(title: "Tiny (tightest)", action: #selector(AppDelegate.setTextTiny), keyEquivalent: "")
        tinyItem.target = target
        tinyItem.state = state.textSize == .tiny ? .on : .off
        textSub.addItem(tinyItem)
        textParent.submenu = textSub
        sub.addItem(textParent)

        let theme = item("Theme by status", selector: #selector(AppDelegate.toggleThemeByStatus), target: target)
        theme.state = state.themeByStatus ? .on : .off
        theme.toolTip = "Re-skin Terminal windows by Claude state — Ocean for working, Red Sands for awaiting, custom title shows the subject"
        sub.addItem(theme)

        let bright = item("Bright (sunlight)", selector: #selector(AppDelegate.toggleForceBright), target: target)
        bright.state = state.forceBright ? .on : .off
        bright.toolTip = "Force the bright, sunlight-readable status palettes regardless of the macOS Auto dark/light schedule — for working outdoors"
        sub.addItem(bright)

        let sigils = item("PromptRocks", selector: #selector(AppDelegate.togglePromptSigils), target: target)
        sigils.state = state.promptSigils ? .on : .off
        sigils.toolTip = "Pin a PromptRock — a named little stone hashed from the prompt text — to each session's terminal top-right, so prompts are distinguishable at a glance (shape + name per prompt; colour still follows status; hover or click a rock for its subject summary)"
        sub.addItem(sigils)

        let preferIterm = item("Spawn in iTerm2", selector: #selector(AppDelegate.togglePreferIterm), target: target)
        preferIterm.state = state.preferIterm ? .on : .off
        preferIterm.toolTip = "Restore-threads and restart-all open sessions in iTerm2 instead of Terminal.app — the only terminal that shows the tiled, per-session topic wallpapers."
        sub.addItem(preferIterm)

        sub.addItem(.separator())

        // Experimental: tint overlay over the focused Terminal window. Toggles
        // on/off for the front window. Mostly here to prove the
        // overlay-window-tracks-Terminal approach works before we sink time
        // into the real plist-rewrite-for-bg-image path.
        let overlay = item("Tint overlay on front window", selector: #selector(AppDelegate.toggleBackgroundOverlay), target: target)
        overlay.toolTip = "Park a translucent NSWindow over the front Terminal — proof of concept for bg image / transparency. Click again on the same window to remove."
        sub.addItem(overlay)

        let clearOverlays = item("Clear all overlays", selector: #selector(AppDelegate.clearBackgroundOverlays), target: target)
        clearOverlays.toolTip = "Remove every active tint overlay across all Terminal windows."
        sub.addItem(clearOverlays)

        sub.addItem(.separator())

        for n in 1...10 {
            let entry = NSMenuItem(
                title: "Restore last \(n)",
                action: #selector(AppDelegate.restoreRecentThreads(_:)),
                keyEquivalent: ""
            )
            entry.target = target
            entry.representedObject = n
            entry.isEnabled = true
            sub.addItem(entry)
        }
        parent.submenu = sub
        return parent
    }

    private static func coloredTitle(for s: ClaudeSession, dot: String, subject: String, tail: String) -> NSAttributedString {
        let full = "\(dot) \(subject)\(tail)"
        let attr = NSMutableAttributedString(string: full)
        let dotColor: NSColor
        switch s.state {
        // Working = green (active/healthy), complete = soft slate (calm),
        // awaiting = warm amber (needs you), blank = quiet gray (fresh, no
        // input yet), stale = gray.
        case .working:  dotColor = NSColor(deviceHue: 0.33, saturation: 0.70, brightness: 0.78, alpha: 1.0)
        case .rendering: dotColor = NSColor(deviceHue: 0.92, saturation: 0.62, brightness: 0.96, alpha: 1.0) // pink — between green and amber
        case .complete: dotColor = NSColor(deviceHue: 0.58, saturation: 0.30, brightness: 0.70, alpha: 1.0)
        case .awaiting: dotColor = NSColor(deviceHue: 0.10, saturation: 0.95, brightness: 0.95, alpha: 1.0)
        case .interrupted: dotColor = NSColor(deviceHue: 0.78, saturation: 0.55, brightness: 0.80, alpha: 1.0) // violet
        case .blank:    dotColor = NSColor.tertiaryLabelColor
        case .stale:    dotColor = NSColor(deviceWhite: 0.55, alpha: 1.0)
        }
        // Color the status dot strong, the rest of the line in the dot's
        // hue at lower intensity so the row is glanceable but the text is
        // still readable in both light and dark menubar themes.
        let dotRange = NSRange(location: 0, length: (dot as NSString).length)
        attr.addAttribute(.foregroundColor, value: dotColor, range: dotRange)
        let textRange = NSRange(location: dotRange.length, length: (full as NSString).length - dotRange.length)
        let textColor = dotColor.blended(withFraction: 0.55, of: NSColor.labelColor) ?? NSColor.labelColor
        attr.addAttribute(.foregroundColor, value: textColor, range: textRange)
        return attr
    }

    private static func sessionTooltip(_ s: ClaudeSession) -> String {
        var parts: [String] = []
        switch s.state {
        case .awaiting:
            parts.append("awaiting: \(s.awaitingMessage ?? "input")")
        case .complete:
            parts.append("turn complete (idle)")
        case .working:
            parts.append("working")
        case .rendering:
            parts.append("rendering (a launched render is still running)")
        case .interrupted:
            parts.append("interrupted (Esc'd, idle at prompt)")
        case .blank:
            parts.append("blank (no prompt yet)")
        case .stale:
            parts.append("stale (claude pid gone)")
        }
        if !s.cwd.isEmpty { parts.append(s.cwd) }
        if !s.tty.isEmpty { parts.append("/dev/\(s.tty)") }
        parts.append("session \(s.sessionId)")
        return parts.joined(separator: "\n")
    }

    private static func buildMail(status: String, target: AppDelegate) -> NSMenuItem {
        let parent = NSMenuItem(title: "Mail: \(status)", action: nil, keyEquivalent: "")
        let sub = NSMenu()
        sub.addItem(item("Sync all", selector: #selector(AppDelegate.syncBoth), target: target))
        sub.addItem(item("Sync ac-mail", selector: #selector(AppDelegate.syncAcMail), target: target))
        sub.addItem(item("Sync jas-mail", selector: #selector(AppDelegate.syncJasMail), target: target))
        sub.addItem(item("Sync sotce-mail", selector: #selector(AppDelegate.syncSotceMail), target: target))
        sub.addItem(item("Sync quiltnet-mail", selector: #selector(AppDelegate.syncQuiltnetMail), target: target))
        for acct in extraMailAccounts() {
            let mi = NSMenuItem(title: "Sync \(acct.label)",
                                action: #selector(AppDelegate.syncMailFromMenuItem(_:)),
                                keyEquivalent: "")
            mi.target = target
            mi.representedObject = acct.account
            mi.isEnabled = true
            sub.addItem(mi)
        }
        sub.addItem(.separator())
        sub.addItem(item("Open sync log", selector: #selector(AppDelegate.openSyncLog), target: target))
        parent.submenu = sub
        return parent
    }

    /// "Start Call" / "◉ Stop Call" item — toggles slab-call-record. While
    /// recording, the title shows a recording dot + elapsed-style label and
    /// the submenu offers Stop + a shortcut to open ~/Documents/Shelf/meetings.
    private static func buildCall(state: StateSnapshot, target: AppDelegate) -> NSMenuItem {
        if state.callRecording {
            let parent = NSMenuItem(title: "◉ Recording call…", action: nil, keyEquivalent: "")
            // Color the title red so it reads as a hot record indicator even
            // at a glance — same vocabulary as the Claude awaiting dot.
            let attr = NSMutableAttributedString(string: "◉ Recording call…")
            attr.addAttribute(.foregroundColor,
                              value: NSColor(deviceHue: 0.99, saturation: 0.90, brightness: 0.95, alpha: 1.0),
                              range: NSRange(location: 0, length: 1))
            parent.attributedTitle = attr
            let sub = NSMenu()
            let stop = item("Stop call", selector: #selector(AppDelegate.stopCall), target: target)
            stop.toolTip = "Stop ffmpeg, finalize the WAV, hand off to meetings/cli.mjs ingest."
            sub.addItem(stop)
            sub.addItem(.separator())
            let openShelf = item("Open meetings shelf",
                                 selector: #selector(AppDelegate.openMeetingsShelf), target: target)
            sub.addItem(openShelf)
            let openMeetings = item("Open meetings dir",
                                    selector: #selector(AppDelegate.openMeetingsDir), target: target)
            sub.addItem(openMeetings)
            parent.submenu = sub
            return parent
        }
        let parent = NSMenuItem(title: "📞 Start call", action: nil, keyEquivalent: "")
        let sub = NSMenu()
        let start = item("Start call recording",
                         selector: #selector(AppDelegate.startCall), target: target)
        start.toolTip = "Begin capturing mic (+ system audio if an aggregate device is wired) to a WAV. Stop from this menu when the call ends."
        sub.addItem(start)
        sub.addItem(.separator())
        let openShelf = item("Open meetings shelf",
                             selector: #selector(AppDelegate.openMeetingsShelf), target: target)
        sub.addItem(openShelf)
        let openMeetings = item("Open meetings dir",
                                selector: #selector(AppDelegate.openMeetingsDir), target: target)
        sub.addItem(openMeetings)
        parent.submenu = sub
        return parent
    }

    /// iMessage submenu — mirrors Mail. The parent title is whatever the
    /// helper reported (the contact's display name lives only in the
    /// untracked config, never here), so nothing personal is in tracked code.
    /// PDF viewer submenu — slab's minimal scroller (PdfViewer.swift). Lists
    /// every open document (click → bring forward), plus open/close-all. The
    /// title carries the count so glancing tells you what slab is holding.
    private static func buildPdf(target: AppDelegate) -> NSMenuItem {
        let open = PdfViewer.shared.openPaths
        let parent = NSMenuItem(
            title: open.isEmpty ? "PDF viewer" : "PDF viewer: \(open.count)",
            action: nil, keyEquivalent: "")
        let sub = NSMenu()
        sub.autoenablesItems = false
        sub.addItem(item("Open PDF…", selector: #selector(AppDelegate.openPdfFromPanel), target: target))
        if !open.isEmpty {
            sub.addItem(.separator())
            for path in open {
                let row = item((path as NSString).lastPathComponent,
                               selector: #selector(AppDelegate.focusPdf(_:)), target: target)
                row.representedObject = path
                row.toolTip = path
                sub.addItem(row)
            }
            sub.addItem(.separator())
            sub.addItem(item("Close all", selector: #selector(AppDelegate.closeAllPdfs), target: target))
        }
        parent.submenu = sub
        return parent
    }

    /// Video viewer submenu — slab's minimal player (VideoViewer.swift), so
    /// rendered videos preview without QuickTime. Same shape as buildPdf.
    private static func buildVideo(target: AppDelegate) -> NSMenuItem {
        let open = VideoViewer.shared.openPaths
        let parent = NSMenuItem(
            title: open.isEmpty ? "Video viewer" : "Video viewer: \(open.count)",
            action: nil, keyEquivalent: "")
        let sub = NSMenu()
        sub.autoenablesItems = false
        sub.addItem(item("Open video…", selector: #selector(AppDelegate.openVideoFromPanel), target: target))
        if !open.isEmpty {
            sub.addItem(.separator())
            for path in open {
                let row = item((path as NSString).lastPathComponent,
                               selector: #selector(AppDelegate.focusVideo(_:)), target: target)
                row.representedObject = path
                row.toolTip = path
                sub.addItem(row)
            }
            sub.addItem(.separator())
            sub.addItem(item("Close all", selector: #selector(AppDelegate.closeAllVideos), target: target))
        }
        parent.submenu = sub
        return parent
    }

    /// Audio wall submenu — slab's tiled jukebox (AudioGroupPreview.swift),
    /// so rendered audio previews without QuickTime/Music. Same shape as
    /// buildVideo; one wall at a time, so rows list the current group.
    private static func buildAudio(target: AppDelegate) -> NSMenuItem {
        let open = AudioGroupPreview.shared.openPaths
        let parent = NSMenuItem(
            title: open.isEmpty ? "Audio wall" : "Audio wall: \(open.count)",
            action: nil, keyEquivalent: "")
        let sub = NSMenu()
        sub.autoenablesItems = false
        sub.addItem(item("Open audio…", selector: #selector(AppDelegate.openAudioFromPanel), target: target))
        if !open.isEmpty {
            sub.addItem(.separator())
            for path in open {
                let row = item((path as NSString).lastPathComponent,
                               selector: #selector(AppDelegate.focusAudio(_:)), target: target)
                row.representedObject = path
                row.toolTip = path
                sub.addItem(row)
            }
            sub.addItem(.separator())
            sub.addItem(item("Close all", selector: #selector(AppDelegate.closeAllAudio), target: target))
        }
        parent.submenu = sub
        return parent
    }

    private static func buildImsg(status: String, configured: Bool, target: AppDelegate) -> NSMenuItem {
        let parent = NSMenuItem(title: status, action: nil, keyEquivalent: "")
        let sub = NSMenu()
        if configured {
            sub.addItem(item("Reply…", selector: #selector(AppDelegate.replyImsg), target: target))
            sub.addItem(item("Live tail in terminal", selector: #selector(AppDelegate.openImsgTail), target: target))
            sub.addItem(item("Open in Messages", selector: #selector(AppDelegate.openImsg), target: target))
            sub.addItem(.separator())
        } else {
            sub.addItem(info("Not set up — fill in the contact config:"))
        }
        sub.addItem(item("Edit contact config", selector: #selector(AppDelegate.openImsgConfig), target: target))
        parent.submenu = sub
        return parent
    }

    /// Asana submenu — assigned, incomplete tasks grouped by project. The
    /// parent title is the helper's "Asana: N" count; the submenu lists each
    /// project as a disabled header followed by its task items (click → open
    /// the task's permalink in the browser). Overdue tasks get a red dot,
    /// today's a yellow one. Unconfigured machines show a one-line setup hint.
    private static func buildAsana(state: AsanaState, target: AppDelegate) -> NSMenuItem {
        let parent = NSMenuItem(title: state.label, action: nil, keyEquivalent: "")
        let sub = NSMenu()
        sub.autoenablesItems = false

        if !state.configured {
            sub.addItem(info("Not set up — paste a Personal Access Token:"))
            sub.addItem(item("Edit Asana config",
                             selector: #selector(AppDelegate.openAsanaConfig), target: target))
            parent.submenu = sub
            return parent
        }

        if state.projects.isEmpty {
            sub.addItem(info("No assigned tasks 🎉"))
        } else {
            for (i, project) in state.projects.enumerated() {
                if i > 0 { sub.addItem(.separator()) }
                sub.addItem(info("\(project.name) (\(project.tasks.count))"))
                for task in project.tasks {
                    sub.addItem(asanaTaskItem(task, target: target))
                }
            }
        }

        sub.addItem(.separator())
        sub.addItem(item("Refresh now", selector: #selector(AppDelegate.refreshAsanaNow), target: target))
        sub.addItem(item("Open My Tasks in browser",
                         selector: #selector(AppDelegate.openAsana), target: target))
        sub.addItem(item("Edit Asana config",
                         selector: #selector(AppDelegate.openAsanaConfig), target: target))
        parent.submenu = sub
        return parent
    }

    /// Deploy submenu — a per-environment rollup of Cloudflare Workers Builds.
    /// The parent title is the helper's glyph summary (e.g. "fuser ✓ staging
    /// ✗ production"); each environment expands to its branch tip + per-worker
    /// build states. Repo identity stays in the untracked config; an
    /// unconfigured machine shows a one-line set-up hint.
    private static func buildDeploy(state: DeployStatusState, target: AppDelegate) -> NSMenuItem {
        let parent = NSMenuItem(title: state.label, action: nil, keyEquivalent: "")
        let sub = NSMenu()
        sub.autoenablesItems = false

        if !state.configured {
            sub.addItem(info("Not set up — add a repo + GitHub token:"))
            sub.addItem(item("Edit deploy-status config",
                             selector: #selector(AppDelegate.openDeployConfig), target: target))
            parent.submenu = sub
            return parent
        }

        if state.envs.isEmpty {
            sub.addItem(info("No environments configured"))
        } else {
            for (i, env) in state.envs.enumerated() {
                if i > 0 { sub.addItem(.separator()) }
                sub.addItem(deployEnvItem(env, target: target))
                for app in env.apps {
                    sub.addItem(deployAppItem(app, target: target))
                }
            }
        }

        sub.addItem(.separator())
        sub.addItem(item("Refresh now", selector: #selector(AppDelegate.refreshDeployNow), target: target))
        sub.addItem(item("Edit deploy-status config",
                         selector: #selector(AppDelegate.openDeployConfig), target: target))
        parent.submenu = sub
        return parent
    }

    /// Environment header: "<glyph> <env> <sha> — <message>", colored by state,
    /// opening the branch-tip commit on click.
    private static func deployEnvItem(_ env: DeployEnv, target: AppDelegate) -> NSMenuItem {
        let sha = env.sha.isEmpty ? "" : " \(env.sha)"
        let msg = env.message.isEmpty ? "" : " — \(env.message)"
        let title = "\(deployGlyph(env.state)) \(env.env)\(sha)\(msg)"
        let mi = NSMenuItem(title: title,
                            action: #selector(AppDelegate.openDeploy(_:)),
                            keyEquivalent: "")
        mi.target = target
        mi.isEnabled = !env.url.isEmpty
        mi.representedObject = env.url
        mi.attributedTitle = deployColored(title, state: env.state)
        return mi
    }

    /// One worker row, indented and colored by state; opens its build log.
    private static func deployAppItem(_ app: DeployApp, target: AppDelegate) -> NSMenuItem {
        let title = "    \(deployGlyph(app.state)) \(app.name)"
        let mi = NSMenuItem(title: title,
                            action: #selector(AppDelegate.openDeploy(_:)),
                            keyEquivalent: "")
        mi.target = target
        mi.isEnabled = !app.url.isEmpty
        mi.representedObject = app.url
        mi.attributedTitle = deployColored(title, state: app.state)
        return mi
    }

    private static func deployGlyph(_ state: String) -> String {
        switch state {
        case "deployed": return "✓"
        case "building": return "⏳"
        case "failing":  return "✗"
        case "error":    return "⚠"
        default:         return "·"
        }
    }

    /// Tint a whole row green/amber/red/orange by deploy state; unknown stays
    /// default so failing workers jump out of the list.
    private static func deployColored(_ title: String, state: String) -> NSAttributedString {
        let attr = NSMutableAttributedString(string: title)
        let color: NSColor?
        switch state {
        case "deployed": color = .systemGreen
        case "building": color = .systemYellow
        case "failing":  color = .systemRed
        case "error":    color = .systemOrange
        default:         color = nil
        }
        if let c = color {
            attr.addAttribute(.foregroundColor, value: c,
                              range: NSRange(location: 0, length: attr.length))
        }
        return attr
    }

    /// OVERTIME toggle — only on machines armed as overtime workers (the
    /// untracked config exists). Mirrors the badge's right-click toggle:
    /// same flag file, same worker kickstart. While on, the worker's current
    /// activity line rides along as the tooltip.
    private static func appendOvertime(to menu: NSMenu, target: AppDelegate) {
        // Armed workers carry a "machine" identity in their config; the
        // control machine's config only lists machines — no item there.
        guard let data = FileManager.default.contents(atPath: Paths.overtimeConfig),
              let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let machine = json["machine"] as? String, !machine.isEmpty else { return }
        let on = FileManager.default.fileExists(atPath: Paths.overtimeFlag)
        let mi = item("⚡ Overtime — work Asana tasks autonomously",
                      selector: #selector(AppDelegate.toggleOvertime), target: target)
        mi.state = on ? .on : .off
        if on, let s = try? String(contentsOfFile: Paths.overtimeStatus, encoding: .utf8),
           let line = s.split(separator: "\n").first, !line.isEmpty {
            mi.toolTip = "On — \(line). Click to stop picking new tasks (a running job finishes)."
        } else {
            mi.toolTip = "Work this machine's tagged Asana tasks one at a time — claude in a Terminal + a test browser, stall-watched, a draft PR per task."
        }
        menu.addItem(mi)
    }

    /// One task row. A leading ● is colored red when overdue, yellow when due
    /// today; undated/future tasks get no dot. The due date is appended dim.
    /// `representedObject` carries the permalink for `openAsanaTask(_:)`.
    private static func asanaTaskItem(_ task: AsanaTask, target: AppDelegate) -> NSMenuItem {
        let dot = (task.overdue || task.today) ? "● " : ""
        let suffix = task.due.isEmpty ? "" : "  · \(task.due)"
        let mi = NSMenuItem(title: "\(dot)\(task.name)\(suffix)",
                            action: #selector(AppDelegate.openAsanaTask(_:)),
                            keyEquivalent: "")
        mi.target = target
        mi.isEnabled = !task.url.isEmpty
        mi.representedObject = task.url
        if task.overdue || task.today {
            let attr = NSMutableAttributedString(string: "\(dot)\(task.name)\(suffix)")
            let hue = task.overdue ? 0.99 : 0.14   // red vs amber
            attr.addAttribute(.foregroundColor,
                              value: NSColor(deviceHue: hue, saturation: 0.85, brightness: 0.95, alpha: 1.0),
                              range: NSRange(location: 0, length: 1))
            mi.attributedTitle = attr
            mi.toolTip = task.overdue ? "Overdue \(task.due)" : "Due today"
        }
        return mi
    }

    /// Deskflow KVM submenu. Parent title shows the configured label + role
    /// + run state (green dot when a deskflow-core is alive); the submenu
    /// drives the LaunchAgent (Start/Stop/Restart) so the KVM can be managed
    /// without the GUI app. Only built when a deskflow.json marks this machine.
    private static func buildDeskflow(state: StateSnapshot, target: AppDelegate) -> NSMenuItem {
        let d = state.deskflow
        let roleTag = d.role.isEmpty ? "" : " (\(d.role))"
        let dot = d.running ? "●" : "○"
        let title = "\(dot) \(d.label)\(roleTag): \(d.running ? "running" : "stopped")"
        let parent = NSMenuItem(title: title, action: nil, keyEquivalent: "")
        // Tint the leading dot green when running, gray when stopped — same
        // glance-vocabulary as the Claude session rows.
        let attr = NSMutableAttributedString(string: title)
        let dotColor = d.running
            ? NSColor(deviceHue: 0.33, saturation: 0.70, brightness: 0.78, alpha: 1.0)
            : NSColor(deviceWhite: 0.55, alpha: 1.0)
        attr.addAttribute(.foregroundColor, value: dotColor,
                          range: NSRange(location: 0, length: 1))
        parent.attributedTitle = attr

        let sub = NSMenu()
        let start = item("Start", selector: #selector(AppDelegate.deskflowStart), target: target)
        start.isEnabled = !d.running
        sub.addItem(start)
        let stop = item("Stop", selector: #selector(AppDelegate.deskflowStop), target: target)
        stop.isEnabled = d.running
        sub.addItem(stop)
        sub.addItem(item("Restart", selector: #selector(AppDelegate.deskflowRestart), target: target))
        sub.addItem(.separator())
        sub.addItem(item("Open Deskflow log", selector: #selector(AppDelegate.openDeskflowLog), target: target))
        sub.addItem(item("Edit Deskflow config", selector: #selector(AppDelegate.openDeskflowConfig), target: target))
        parent.submenu = sub
        return parent
    }

}
