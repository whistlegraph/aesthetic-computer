import AppKit

enum MenuBuilder {
    static func build(
        state: StateSnapshot,
        mailStatus: String,
        imsgStatus: String,
        imsgConfigured: Bool,
        target: AppDelegate
    ) -> NSMenu {
        let menu = NSMenu()
        populate(
            menu,
            state: state,
            mailStatus: mailStatus,
            imsgStatus: imsgStatus,
            imsgConfigured: imsgConfigured,
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
        target: AppDelegate
    ) {
        menu.removeAllItems()
        menu.autoenablesItems = false

        menu.addItem(info("Status: \(state.statusLine)"))
        menu.addItem(.separator())

        appendPopRenders(to: menu, state: state)
        appendClaude(to: menu, state: state, target: target)
        menu.addItem(info("Subagents in flight: \(state.activeSubagents)"))
        menu.addItem(.separator())

        menu.addItem(buildTailnet(state: state, target: target))
        menu.addItem(buildMail(status: mailStatus, target: target))
        menu.addItem(buildImsg(status: imsgStatus, configured: imsgConfigured, target: target))

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
        return "\(type)\(bar)  \(pctText)  \(countCell)\(r.label)"
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
            case .awaiting: dot = "◉"
            case .working:  dot = "●"
            case .complete: dot = "✓"
            case .blank:    dot = "·"   // tiny midpoint — fresh, nothing yet
            case .stale:    dot = "○"
            }
            let tail = s.cwdLabel.isEmpty ? "" : "  ·  \(s.cwdLabel)"
            // Blank sessions have no subject yet — substitute a placeholder
            // so the menu row reads as "· (new) · aesthetic-computer" instead
            // of the awkward "·   ·  aesthetic-computer".
            let subjectText: String = {
                let s2 = s.shortSubject.trimmingCharacters(in: .whitespaces)
                if !s2.isEmpty { return s2 }
                return s.state == .blank ? "(new)" : s2
            }()
            let entry = NSMenuItem(
                title: "\(dot) \(subjectText)\(tail)",
                action: #selector(AppDelegate.focusClaudeSession(_:)),
                keyEquivalent: ""
            )
            entry.target = target
            entry.representedObject = s.tty
            entry.toolTip = sessionTooltip(s)
            entry.isEnabled = !s.tty.isEmpty
            entry.attributedTitle = coloredTitle(for: s, dot: dot, subject: subjectText, tail: tail)
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
        case .complete: dotColor = NSColor(deviceHue: 0.58, saturation: 0.30, brightness: 0.70, alpha: 1.0)
        case .awaiting: dotColor = NSColor(deviceHue: 0.10, saturation: 0.95, brightness: 0.95, alpha: 1.0)
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

}
