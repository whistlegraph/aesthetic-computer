// main.swift — NCC Circles: a Signal client for the computer club's circles.
//
// Launch order: the NCC login gate, then the Signal device link, then the
// sheet. Nothing from Signal is read or shown before both have passed.

import AppKit

struct Circle {
    var conf: CircleConf
    var log: CircleLog
    var members = 0
    var canSend = true
}

final class App: NSObject, NSApplicationDelegate, NSWindowDelegate {
    var window: NSWindow!
    let shell = Shell()
    var entered = false
    let gate = GateView()
    let main = MainView()
    let auth = Auth()
    let engine = SignalEngine()
    let people = People()
    let wiki = Wiki()
    var wikiReady = false
    var openDoc: (title: String, text: String, url: URL?)?   // a wiki page shown in place of the ledger
    var startDoc: (title: String, text: String, url: URL?)?  // the wiki's start page, when it has one
    var onStart = false                                      // the START entry is open, not a circle
    var picked = false                                       // the member has chosen something on the rail
    var member: Member?
    var account: SignalAccount?
    var circles: [Circle] = []
    var selected = 0
    var linking = false

    func applicationDidFinishLaunching(_ n: Notification) {
        buildMenu()
        Theme.apply(UserDefaults.standard.string(forKey: "appearance") ?? "system")
        window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 1040, height: 700),
                          styleMask: [.titled, .closable, .miniaturizable, .resizable, .fullSizeContentView],
                          backing: .buffered, defer: false)
        window.title = "NCC Circles"
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        window.backgroundColor = Theme.mint
        window.minSize = NSSize(width: 300, height: 220)   // small enough for a cell of the slab tiler
        window.delegate = self
        window.center()
        window.setFrameAutosaveName("ncc-circles")
        shell.set(gate)
        shell.setTitle("CIRCLES")
        window.contentView = shell
        window.makeKeyAndOrderFront(nil)

        gate.authButton.target = self
        gate.authButton.action = #selector(signIn)
        gate.signalButton.target = self
        gate.signalButton.action = #selector(linkSignal)
        main.onSelect = { [weak self] i in self?.picked = true; i < 0 ? self?.showStart() : self?.select(i) }
        main.onSend = { [weak self] s in self?.send(s) }
        main.onDoc = { [weak self] id in self?.showDoc(id) }
        main.onBack = { [weak self] in self?.openDoc = nil; self?.refresh() }
        engine.onEnvelope = { [weak self] e in self?.handle(e) }
        engine.onExit = { [weak self] why in
            self?.main.setStatus("signal-cli stopped" + (why.isEmpty ? "" : ": " + String(why.suffix(160))))
        }

        account = SignalEngine.account()
        refreshGate()
        scheduleSnapshot()
        if Config.shared.conf.auth.required {
            auth.restore { [weak self] r in
                if case .success(let m) = r { self?.member = m }
                self?.refreshGate()
            }
        }
    }

    // Test aid: NCC_SNAPSHOT=<file.png> draws the window into that file after
    // NCC_SNAPSHOT_AFTER seconds (default 10) and quits. It needs no screen
    // capture and no clicks, so a check can run while someone uses the Mac.
    func scheduleSnapshot() {
        let env = ProcessInfo.processInfo.environment
        guard let path = env["NCC_SNAPSHOT"] else { return }
        DispatchQueue.main.asyncAfter(deadline: .now() + (Double(env["NCC_SNAPSHOT_AFTER"] ?? "") ?? 10)) {
            if let view = self.window.contentView, let rep = view.bitmapImageRepForCachingDisplay(in: view.bounds) {
                view.cacheDisplay(in: view.bounds, to: rep)
                try? rep.representation(using: .png, properties: [:])?.write(to: URL(fileURLWithPath: path))
            }
            NSApp.terminate(nil)
        }
    }

    func applicationWillTerminate(_ n: Notification) {
        engine.cancelLink()
        engine.stop()
    }
    func applicationShouldTerminateAfterLastWindowClosed(_ a: NSApplication) -> Bool { true }
    func windowDidBecomeKey(_ n: Notification) { if !circles.isEmpty { markRead() } }

    // ─── gate ────────────────────────────────────────────────────────────────

    var authPassed: Bool { !Config.shared.conf.auth.required || member != nil }

    func refreshGate() {
        let conf = Config.shared.conf.auth
        if !conf.required {
            gate.authStatus.stringValue = "not required (config)"
            gate.authButton.isHidden = true
        } else if let m = member {
            gate.authStatus.stringValue = "signed in as \(m.name)"
            gate.authButton.isHidden = true
        } else if !auth.configured {
            gate.authStatus.stringValue = "waiting for a client id"
            gate.authButton.isEnabled = false
            gate.note.stringValue = "NCC login needs an OIDC client for this app. Ask the Auth DRI for a "
                + "public client with redirect http://127.0.0.1:\(conf.redirectPort)/callback, "
                + "then put its id in \(Paths.config.path.replacingOccurrences(of: NSHomeDirectory(), with: "~"))."
        } else {
            gate.authStatus.stringValue = "not signed in"
            gate.authButton.isEnabled = true
        }

        if let a = account {
            gate.signalStatus.stringValue = "linked as \(a.number)"
            gate.signalButton.isHidden = true
        } else if Config.shared.signalCli == nil {
            gate.signalStatus.stringValue = "signal-cli is missing"
            gate.signalButton.isEnabled = false
            gate.note.stringValue = "Install the Signal engine first: brew install signal-cli"
        } else {
            gate.signalStatus.stringValue = linking ? "scan the code with your phone" : "not linked"
            // Signal stays shut until the NCC login has passed.
            gate.signalButton.isEnabled = authPassed && !linking
        }
        if authPassed, account != nil { enter() }
    }

    @objc func signIn() {
        gate.authStatus.stringValue = "finish in your browser…"
        gate.authButton.isEnabled = false
        auth.login { [weak self] r in
            guard let self else { return }
            NSApp.activate(ignoringOtherApps: true)
            switch r {
            case .success(let m): self.member = m; self.gate.note.stringValue = ""
            case .failure(let e): self.gate.note.stringValue = "NCC login: \(e.message)"
            }
            self.refreshGate()
        }
    }

    @objc func linkSignal() {
        guard authPassed else { return }
        linking = true
        gate.note.stringValue = "On your phone: Signal → Settings → Linked devices → Link new device."
        refreshGate()
        engine.link(deviceName: "NCC Circles", onURI: { [weak self] uri in self?.gate.showQR(uri) }) { [weak self] r in
            guard let self else { return }
            self.linking = false
            self.gate.showQR(nil)
            switch r {
            case .success: self.account = SignalEngine.account(); self.gate.note.stringValue = ""
            case .failure(let e): self.gate.note.stringValue = "Signal: \(e.message)"
            }
            self.refreshGate()
        }
    }

    @objc func signOut() {
        auth.logout()
        NSApp.terminate(nil)
    }

    // ─── the sheet ───────────────────────────────────────────────────────────

    func enter() {
        guard !entered, let account else { return }
        entered = true
        shell.set(main)
        main.setWho([member.map { "NCC · \($0.name)" }, "Signal · \(account.number)"].compactMap { $0 }.joined(separator: "\n"))
        main.setStatus("starting Signal…")
        loadCircles()
        do { try engine.start(account: account) } catch {
            main.setStatus((error as? SignalEngine.EngineError)?.message ?? error.localizedDescription)
            return
        }
        loadWiki()
        engine.call("listContacts") { [weak self] r in
            if case .success(let v) = r, let list = v as? [[String: Any]] { self?.people.setContacts(list) }
            self?.refresh()
        }
        engine.call("listGroups") { [weak self] r in
            guard let self else { return }
            switch r {
            case .failure(let e): self.main.setStatus("Signal: \(e.message)")
            case .success(let v):
                self.adoptGroups((v as? [[String: Any]]) ?? [])
                // A first run starts with empty sheets; fill them when the bridge is there.
                if !self.circles.isEmpty, self.circles.allSatisfy({ $0.log.msgs.isEmpty }),
                   Config.shared.bridge != nil {
                    return self.importHistory()
                }
                self.main.setStatus(self.circles.isEmpty
                    ? "No club groups on this Signal account yet. A new link can take a minute to sync." : "")
            }
        }
    }

    func loadCircles() {
        circles = Config.shared.conf.circles.map { Circle(conf: $0, log: CircleLog(groupId: $0.groupId)) }
        buildRail()
        if !circles.isEmpty { select(min(selected, circles.count - 1)) }
    }

    // Match the account's groups against the club patterns; remember new ones.
    func adoptGroups(_ groups: [[String: Any]]) {
        var conf = Config.shared.conf
        var changed = false
        for g in groups {
            guard let id = g["id"] as? String, let name = g["name"] as? String,
                  (g["isMember"] as? Bool) ?? true else { continue }
            let lower = name.lowercased()
            if !conf.circles.contains(where: { $0.groupId == id }) {
                guard conf.match.contains(where: { lower.contains($0.lowercased()) }) else { continue }
                conf.circles.append(Self.describe(id: id, name: name))
                changed = true
            }
        }
        if changed {
            conf.circles.sort { ($0.kind, $0.title) < ($1.kind, $1.title) }
            Config.shared.conf = conf
            Config.shared.save()
            loadCircles()
        }
        for i in circles.indices {
            guard let g = groups.first(where: { ($0["id"] as? String) == circles[i].conf.groupId }) else { continue }
            circles[i].members = (g["members"] as? [Any])?.count ?? 0
            let adminsOnly = (g["permissionSendMessage"] as? String) == "ONLY_ADMINS"
            let admins = (g["admins"] as? [[String: Any]]) ?? []
            let iAmAdmin = admins.contains { ($0["uuid"] as? String) == account?.uuid || ($0["number"] as? String) == account?.number }
            circles[i].canSend = !adminsOnly || iAmAdmin
        }
        refresh()
    }

    // "📢 [Announce] Computer Club" → Announce (commons);
    // "NCC Infrastructure Subcommittee" → Infra (circle).
    static func describe(id: String, name: String) -> CircleConf {
        if let a = name.firstIndex(of: "["), let b = name.firstIndex(of: "]"), a < b {
            let t = name[name.index(after: a)..<b].trimmingCharacters(in: .whitespaces).capitalized
            return CircleConf(groupId: id, name: name, title: t, kind: "commons", roleKey: nil)
        }
        let drop: Set<String> = ["ncc", "nela", "computer", "club", "committee", "subcommittee", "circle", "the"]
        let words = name.split(whereSeparator: { !$0.isLetter }).map(String.init).filter { !drop.contains($0.lowercased()) }
        var t = (words.first ?? name).capitalized
        if t.lowercased().hasPrefix("infra") { t = "Infra" }
        return CircleConf(groupId: id, name: name, title: t, kind: "circle", roleKey: t)
    }

    func buildRail() {
        let pick = { (kind: String) -> [(Int, String, NSColor?)] in
            self.circles.enumerated().filter { $0.element.conf.kind == kind }
                .map { ($0.offset, $0.element.conf.title, self.color(of: $0.offset)) }
        }
        let start: [(Int, String, NSColor?)] = startDoc == nil ? [] : [(-1, "Start", nil)]
        main.setRail([("", start), ("CIRCLES", pick("circle")), ("COMMONS", pick("commons"))])
        updateUnread()
    }

    // A circle's colour: its wiki collection's, else one of the club's own.
    func color(of i: Int) -> NSColor {
        let title = circles[i].conf.title
        return Theme.circleColor(title, wiki: wikiReady ? wiki.color(for: title) : nil)
    }

    func diagramItems() -> [DiagramItem] {
        circles.enumerated().map { i, c in
            let since = (UserDefaults.standard.object(forKey: readKey(c)) as? Int64) ?? 0
            return DiagramItem(index: i, title: c.conf.title, commons: c.conf.kind != "circle", color: color(of: i),
                               members: c.members, unread: c.log.unread(since: since, me: account?.uuid ?? ""))
        }
    }

    func select(_ i: Int) {
        guard circles.indices.contains(i) else { return }
        selected = i
        onStart = false
        openDoc = nil
        markRead()
        refresh()
        refreshWiki()
        main.focusField()
    }

    func refresh() {
        guard let account else { return }
        if onStart, let page = openDoc ?? startDoc {
            // On START the sheet is the wiki's start page, or a page reached from it.
            main.show(index: -1, heading: "START · " + wiki.base.replacingOccurrences(of: "https://", with: ""),
                      diagram: openDoc == nil ? diagramItems() : nil,
                      roleLine: [], canSend: false, placeholder: "")
            shell.setTitle("CIRCLES · START")
            main.renderDoc(title: page.title, markdown: page.text, url: page.url,
                           back: openDoc == nil ? nil : "start", wikiBase: wiki.base)
            return updateUnread()
        }
        guard circles.indices.contains(selected) else { return }
        let c = circles[selected]
        let count = c.members > 0 ? " · \(c.members) members" : ""
        shell.setTitle("CIRCLES · " + c.conf.title.uppercased())
        main.show(index: selected, heading: c.conf.title.uppercased() + count + " · " + c.conf.name,
                  color: color(of: selected),
                  roleLine: c.conf.roleKey.map { people.roles(forKey: $0) } ?? [],
                  canSend: c.canSend,
                  placeholder: c.canSend ? "write to \(c.conf.title)" : "only admins write here")
        if let d = openDoc {
            main.renderDoc(title: d.title, markdown: d.text, url: d.url, back: "messages", wikiBase: wiki.base)
        } else {
            main.render(Array(c.log.msgs.suffix(600)), reactions: c.log.reactions, people: people, me: account.uuid)
        }
        updateUnread()
    }

    // ─── wiki ────────────────────────────────────────────────────────────────

    func loadWiki() {
        guard wiki.token != nil else {
            return main.renderWiki(heading: "wiki", rows: [], note: "No wiki key yet.\nMenu → Set Wiki Key…")
        }
        wiki.load { [weak self] ok in
            guard let self else { return }
            self.wikiReady = ok
            self.people.wikiColors = self.wiki.userColors
            guard ok else {
                return self.main.renderWiki(heading: "wiki", rows: [], note: "The wiki did not answer for this key.")
            }
            self.buildRail()        // the wiki's collection colours are in now
            self.refresh()
            self.refreshWiki()
            // The club's start page becomes the app's START entry, and the first thing shown.
            self.wiki.startPage(titled: Config.shared.conf.wikiStart ?? "STARTING PAGE") { id in
                guard let id else { return }
                self.wiki.page(id) { page in
                    guard let page else { return }
                    self.startDoc = (page.title, page.text, URL(string: self.wiki.base + page.path))
                    self.buildRail()
                    if self.picked { self.refresh() } else { self.showStart() }
                }
            }
        }
    }

    func showStart() {
        guard startDoc != nil else { return }
        onStart = true
        openDoc = nil
        refresh()
        refreshWiki()
    }

    // The page tree beside the sheet: the open circle's collection, or on START
    // (and for a circle with no collection) the whole wiki.
    func refreshWiki() {
        guard wikiReady else { return }
        let i = selected, start = onStart
        let own = start || !circles.indices.contains(i) ? [] : wiki.collections(for: circles[i].conf.title)
        let cols = own.isEmpty ? wiki.allCollections : own
        let heading = own.isEmpty ? "wiki" : "wiki · " + own.map { $0.name }.joined(separator: " + ")
        wiki.tree(cols) { [weak self] rows in
            guard let self, self.selected == i, self.onStart == start else { return }
            self.main.renderWiki(heading: heading, rows: rows, note: rows.isEmpty ? "No pages." : nil)
        }
    }

    func showDoc(_ id: String) {
        wiki.page(id) { [weak self] page in
            guard let self else { return }
            self.openDoc = page.map { ($0.title, $0.text, URL(string: self.wiki.base + $0.path)) }
                ?? ("Not found", "(this page could not be read)", nil)
            self.refresh()
        }
    }

    @objc func setWikiKey() {
        let alert = NSAlert()
        alert.messageText = "Wiki API key"
        alert.informativeText = "Make one at \(wiki.base) → Settings → API keys. It is kept in your login Keychain."
        let input = NSSecureTextField(frame: NSRect(x: 0, y: 0, width: 300, height: 22))
        alert.accessoryView = input
        alert.addButton(withTitle: "Save")
        alert.addButton(withTitle: "Cancel")
        guard alert.runModal() == .alertFirstButtonReturn else { return }
        wiki.setToken(input.stringValue.trimmingCharacters(in: .whitespacesAndNewlines))
        loadWiki()
    }

    // ─── unread ──────────────────────────────────────────────────────────────

    func readKey(_ c: Circle) -> String { "read." + c.conf.groupId }

    func markRead() {
        guard circles.indices.contains(selected), let last = circles[selected].log.msgs.last else { return }
        UserDefaults.standard.set(last.ts, forKey: readKey(circles[selected]))
        updateUnread()
    }

    func updateUnread() {
        var total = 0
        for (i, c) in circles.enumerated() {
            let since = (UserDefaults.standard.object(forKey: readKey(c)) as? Int64) ?? 0
            let n = c.log.unread(since: since, me: account?.uuid ?? "")
            main.setUnread(i, n)
            total += n
        }
        NSApp.dockTile.badgeLabel = total > 0 ? "\(total)" : nil
    }

    // ─── Signal in ───────────────────────────────────────────────────────────

    func handle(_ env: [String: Any]) {
        guard let account else { return }
        let src = (env["sourceUuid"] as? String) ?? ""
        if let dm = env["dataMessage"] as? [String: Any] {
            ingest(dm, author: src, name: env["sourceName"] as? String)
        }
        // What this account sent from another device (the phone) comes as a sync.
        if let sent = (env["syncMessage"] as? [String: Any])?["sentMessage"] as? [String: Any] {
            ingest(sent, author: account.uuid, name: nil)
        }
    }

    func ingest(_ dm: [String: Any], author: String, name: String?) {
        guard let gid = (dm["groupInfo"] as? [String: Any])?["groupId"] as? String,
              let i = circles.firstIndex(where: { $0.conf.groupId == gid }),
              let ts = (dm["timestamp"] as? NSNumber)?.int64Value else { return }
        var m = Msg(ts: ts, author: author, name: name)
        if let r = dm["reaction"] as? [String: Any] {
            m.kind = "reaction"
            m.target = (r["targetSentTimestamp"] as? NSNumber)?.int64Value
            m.emoji = r["emoji"] as? String
            m.remove = r["isRemove"] as? Bool
        } else {
            let body = NSMutableString(string: (dm["message"] as? String) ?? "")
            let mentions = ((dm["mentions"] as? [[String: Any]]) ?? [])
                .sorted { (($0["start"] as? Int) ?? 0) > (($1["start"] as? Int) ?? 0) }
            for mention in mentions {
                guard let start = mention["start"] as? Int, let len = mention["length"] as? Int,
                      start + len <= body.length else { continue }
                let who = people.person((mention["uuid"] as? String) ?? "", fallback: mention["name"] as? String).name
                body.replaceCharacters(in: NSRange(location: start, length: len), with: "@" + who)
            }
            m.body = body as String
            let dir = Config.shared.signalConfigDir.appendingPathComponent("attachments")
            let atts = ((dm["attachments"] as? [[String: Any]]) ?? []).filter { $0["id"] is String }
            if !atts.isEmpty {
                m.files = atts.map { dir.appendingPathComponent($0["id"] as! String).path }
                m.fileNames = atts.map { ($0["filename"] as? String) ?? ($0["id"] as! String) }
            }
            let previews = ((dm["previews"] as? [[String: Any]]) ?? []).compactMap { p -> Preview? in
                guard let url = p["url"] as? String else { return nil }
                let image = ((p["image"] as? [String: Any])?["id"] as? String).map { dir.appendingPathComponent($0).path }
                return Preview(url: url, title: (p["title"] as? String) ?? "",
                               description: (p["description"] as? String) ?? "", image: image)
            }
            if !previews.isEmpty { m.previews = previews }
            m.quote = (dm["quote"] as? [String: Any])?["text"] as? String
            if m.body.isEmpty, m.files == nil { return }
        }
        guard circles[i].log.add(m) else { return }
        if i == selected, openDoc == nil {
            refresh()
            if window.isKeyWindow { markRead() }
        }
        updateUnread()
    }

    // ─── Signal out ──────────────────────────────────────────────────────────

    func send(_ text: String) {
        guard circles.indices.contains(selected), let account else { return }
        let i = selected
        let gid = circles[i].conf.groupId
        main.setStatus("sending to \(circles[i].conf.title)…")
        engine.call("send", ["groupId": gid, "message": text]) { [weak self] r in
            guard let self else { return }
            switch r {
            case .failure(let e):
                self.main.setStatus("not sent: \(e.message)")
            case .success(let v):
                let ts = ((v as? [String: Any])?["timestamp"] as? NSNumber)?.int64Value
                    ?? Int64(Date().timeIntervalSince1970 * 1000)
                self.circles[i].log.add(Msg(ts: ts, author: account.uuid, name: nil, body: text))
                self.main.setStatus("")
                if self.selected == i { self.main.clearField(); self.markRead(); self.openDoc = nil; self.refresh() }
            }
        }
    }

    // ─── history ─────────────────────────────────────────────────────────────
    // signal-cli starts with an empty past. On a Mac that also runs Signal
    // Desktop, the slab bridge can read each circle's history out of it.

    // node has no fixed home (Homebrew, fnm, nvm), so ask the login shell last.
    static func findNode() -> String? {
        let fm = FileManager.default
        let home = NSHomeDirectory()
        let fixed = ["/opt/homebrew/bin/node", "/usr/local/bin/node",
                     home + "/.local/share/fnm/aliases/default/bin/node",
                     home + "/Library/Application Support/fnm/aliases/default/bin/node"]
        if let hit = fixed.first(where: { fm.isExecutableFile(atPath: $0) }) { return hit }
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        let p = Process()
        p.executableURL = URL(fileURLWithPath: shell)
        p.arguments = ["-l", "-c", "command -v node"]
        let out = Pipe()
        p.standardOutput = out
        p.standardError = Pipe()
        guard (try? p.run()) != nil else { return nil }
        let d = out.fileHandleForReading.readDataToEndOfFile()
        p.waitUntilExit()
        let path = String(data: d, encoding: .utf8)?.split(separator: "\n").last.map(String.init) ?? ""
        return fm.isExecutableFile(atPath: path) ? path : nil
    }

    @objc func importHistory() {
        guard let account, let bridge = Config.shared.bridge,
              let node = Self.findNode() else {
            return main.setStatus("history import needs node on this Mac")
        }
        main.setStatus("reading history from Signal Desktop…")
        let ids = circles.map { $0.conf.groupId }
        let media = Paths.root.appendingPathComponent("media").path
        DispatchQueue.global().async {
            var found: [String: [[String: Any]]] = [:]
            for id in ids {
                let p = Process()
                p.executableURL = URL(fileURLWithPath: node)
                p.arguments = [bridge, "export", "400", "--to", id, "--media", media]
                let out = Pipe()
                p.standardOutput = out
                p.standardError = Pipe()
                guard (try? p.run()) != nil else { continue }
                let d = out.fileHandleForReading.readDataToEndOfFile()
                p.waitUntilExit()
                if let rows = try? JSONSerialization.jsonObject(with: d) as? [[String: Any]] { found[id] = rows }
            }
            DispatchQueue.main.async {
                var added = 0
                for i in self.circles.indices {
                    for r in found[self.circles[i].conf.groupId] ?? [] {
                        guard let ts = (r["ts"] as? NSNumber)?.int64Value, let body = r["body"] as? String else { continue }
                        let mine = (r["outgoing"] as? Bool) == true
                        let author = mine ? account.uuid : ((r["author"] as? String) ?? "")
                        var m = Msg(ts: ts, author: author, name: r["name"] as? String, body: body)
                        let files = (r["files"] as? [[String: Any]]) ?? []
                        if !files.isEmpty {
                            m.files = files.compactMap { $0["path"] as? String }
                            m.fileNames = files.map { ($0["name"] as? String) ?? "" }
                        }
                        let previews = ((r["previews"] as? [[String: Any]]) ?? []).compactMap { p -> Preview? in
                            guard let url = p["url"] as? String else { return nil }
                            let image = (p["image"] as? String).flatMap { $0.isEmpty ? nil : $0 }
                            return Preview(url: url, title: (p["title"] as? String) ?? "",
                                           description: (p["description"] as? String) ?? "", image: image)
                        }
                        if !previews.isEmpty { m.previews = previews }
                        if self.circles[i].log.add(m) { added += 1 }
                    }
                }
                // Imported history is the past, not news: start each sheet as read.
                for c in self.circles where UserDefaults.standard.object(forKey: self.readKey(c)) == nil {
                    if let last = c.log.msgs.last { UserDefaults.standard.set(last.ts, forKey: self.readKey(c)) }
                }
                self.main.setStatus(found.isEmpty ? "Signal Desktop history could not be read" : "imported \(added) messages")
                self.refresh()
            }
        }
    }

    @objc func setAppearance(_ sender: NSMenuItem) {
        let mode = (sender.representedObject as? String) ?? "system"
        UserDefaults.standard.set(mode, forKey: "appearance")
        Theme.apply(mode)
        refresh()
    }

    // ─── menu ────────────────────────────────────────────────────────────────

    func buildMenu() {
        let bar = NSMenu()
        let app = NSMenu()
        app.addItem(withTitle: "Import History from Signal Desktop", action: #selector(importHistory), keyEquivalent: "i")
        app.addItem(withTitle: "Set Wiki Key…", action: #selector(setWikiKey), keyEquivalent: "k")
        app.addItem(withTitle: "Sign Out of NCC and Quit", action: #selector(signOut), keyEquivalent: "")
        app.addItem(.separator())
        app.addItem(withTitle: "Quit NCC Circles", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        let edit = NSMenu(title: "Edit")
        edit.addItem(withTitle: "Cut", action: #selector(NSText.cut(_:)), keyEquivalent: "x")
        edit.addItem(withTitle: "Copy", action: #selector(NSText.copy(_:)), keyEquivalent: "c")
        edit.addItem(withTitle: "Paste", action: #selector(NSText.paste(_:)), keyEquivalent: "v")
        edit.addItem(withTitle: "Select All", action: #selector(NSText.selectAll(_:)), keyEquivalent: "a")
        let view = NSMenu(title: "View")
        for (title, mode) in [("System Appearance", "system"), ("Light", "light"), ("Dark", "dark")] {
            let item = view.addItem(withTitle: title, action: #selector(setAppearance(_:)), keyEquivalent: "")
            item.representedObject = mode
        }
        for m in [app, edit, view] {
            let item = NSMenuItem()
            item.submenu = m
            bar.addItem(item)
        }
        NSApp.mainMenu = bar
    }
}

let app = NSApplication.shared
let delegate = App()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
