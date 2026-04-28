import AppKit

enum MenuBuilder {
    static func build(state: StateSnapshot, mailStatus: String, target: AppDelegate) -> NSMenu {
        let menu = NSMenu()
        menu.autoenablesItems = false

        menu.addItem(info("Status: \(state.statusLine)"))
        menu.addItem(.separator())

        menu.addItem(buildClaude(state: state, target: target))
        menu.addItem(info("Subagents in flight: \(state.activeSubagents)"))
        menu.addItem(.separator())

        menu.addItem(buildTailnet(state: state, target: target))
        menu.addItem(buildMail(status: mailStatus, target: target))
        menu.addItem(.separator())

        let stayAwake = item("Stay awake (lid closed)", selector: #selector(AppDelegate.toggleStayAwake), target: target)
        stayAwake.state = state.sleepDisabled ? .on : .off
        menu.addItem(stayAwake)
        menu.addItem(item("Sleep now", selector: #selector(AppDelegate.sleepNow), target: target))
        menu.addItem(.separator())

        menu.addItem(item("Open daemon log", selector: #selector(AppDelegate.openDaemonLog), target: target))
        menu.addItem(item("Open sounds folder", selector: #selector(AppDelegate.openSoundsFolder), target: target))
        menu.addItem(.separator())

        menu.addItem(item("Reload daemon", selector: #selector(AppDelegate.reloadDaemon), target: target))
        menu.addItem(item("Quit menu bar", selector: #selector(AppDelegate.quitMenubar), target: target))

        return menu
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

    private static func buildClaude(state: StateSnapshot, target: AppDelegate) -> NSMenuItem {
        let sessions = state.claudeSessions
        let label: String
        if sessions.isEmpty {
            label = "Claude: idle"
        } else if state.anyAwaiting {
            label = "Claude: \(state.awaitingCount) awaiting · \(sessions.count) active"
        } else {
            label = "Claude: \(sessions.count) active"
        }
        let parent = NSMenuItem(title: label, action: nil, keyEquivalent: "")
        let sub = NSMenu()

        if sessions.isEmpty {
            sub.addItem(info("(no active prompts)"))
        } else {
            for s in sessions {
                let dot: String
                switch s.state {
                case .awaiting: dot = "◉"
                case .working:  dot = "●"
                case .stale:    dot = "○"
                }
                let tail = s.cwdLabel.isEmpty ? "" : "  ·  \(s.cwdLabel)"
                let title = "\(dot) \(s.shortSubject)\(tail)"
                let entry = NSMenuItem(
                    title: title,
                    action: #selector(AppDelegate.focusClaudeSession(_:)),
                    keyEquivalent: ""
                )
                entry.target = target
                entry.representedObject = s.tty
                entry.toolTip = sessionTooltip(s)
                entry.isEnabled = !s.tty.isEmpty
                sub.addItem(entry)
            }
        }

        parent.submenu = sub
        return parent
    }

    private static func sessionTooltip(_ s: ClaudeSession) -> String {
        var parts: [String] = []
        switch s.state {
        case .awaiting:
            parts.append("awaiting: \(s.awaitingMessage ?? "input")")
        case .working:
            parts.append("working")
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
        sub.addItem(item("Sync both", selector: #selector(AppDelegate.syncBoth), target: target))
        sub.addItem(item("Sync ac-mail", selector: #selector(AppDelegate.syncAcMail), target: target))
        sub.addItem(item("Sync jas-mail", selector: #selector(AppDelegate.syncJasMail), target: target))
        sub.addItem(.separator())
        sub.addItem(item("Open sync log", selector: #selector(AppDelegate.openSyncLog), target: target))
        parent.submenu = sub
        return parent
    }

}
