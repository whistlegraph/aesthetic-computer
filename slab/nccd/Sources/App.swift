// App.swift — NCCd: a Signal client for the computer club's circles.
// The d is for donut: the club's circles are peers on one ring, so the home
// screen is a donut with the circles spread out on it.
//
// There are two logins, and neither waits for the other. Signal (a linked
// device) opens the circles. NCC (the club's login page, in the wiki pane)
// opens the wiki. With neither, the app still opens: the donut, a button to
// link Signal, and the club's login in the wiki pane.
// (The app was first "NCC Circles". Its bundle id, data folder and config path
// keep that name, so a Mac that ran the old build carries on with its link,
// its logs and its keys.)

import AppKit
import WebKit

struct Circle {
    var conf: CircleConf
    var log: CircleLog
    var members = 0
    var memberIds: [String] = []     // their Signal ids, for the "@" list
    var canSend = true
}

final class App: NSObject, NSApplicationDelegate, NSWindowDelegate {
    var window: NSWindow!
    let shell = Shell()
    let main = MainView()
    var login: WikiLogin?                                    // the NCC sign-in, while it shows
    var nccName: String?                                     // who the wiki says is signed in
    let engine = SignalEngine()
    let people = People()
    let wiki = Wiki()
    let updater = Updater()
    let menu = MenuBar()
    let radio = Radio()
    // The zoom of what is in the pane (not of the tabs and bars), kept between launches.
    var zoom: CGFloat = 1 {
        didSet {
            zoom = min(2.4, max(0.6, (zoom * 20).rounded() / 20))
            UserDefaults.standard.set(Double(zoom), forKey: "zoom")
            main.zoom = zoom
            login?.zoom = zoom
            codeWeb?.pageZoom = zoom
            satellites.forEach { $0.view.zoom = zoom }
        }
    }
    var wikiReady = false
    typealias Doc = (id: String, title: String, text: String, url: URL?)
    var openDoc: Doc?                                        // the page in the wiki pane (nil: its page tree)
    var startDoc: Doc?                                       // the wiki's start page, when it has one
    var wikiTree: (heading: String, rows: [WikiRow])?        // the page tree of the wiki pane
    var wikiTouched = false                                  // the member has moved in the wiki pane
    var lastDocId: String?                                   // the page last read, marked in the tree
    var satellites: [Satellite] = []                         // circles open in windows of their own
    var pending: [String: [Msg]] = [:]                       // by group: messages sent, not yet landed
    var onStart = true                                       // the donut (home) is open, not a circle
    var onSearch = false                                     // the Search tab is open
    var onLogin = false                                      // the Login tab is open
    var onCode = false                                       // the Code tab is open
    var codeWeb: WKWebView?
    var searchWork: DispatchWorkItem?
    var picked = false                                       // the member has chosen something on the rail
    var account: SignalAccount?
    var circles: [Circle] = []
    var selected = 0
    var linking = false
    var resumeDraft: String?

    // Development (NCC_DEV, set by dev.sh): the app shows what is on this Mac
    // and starts no Signal engine, so it never competes with the installed
    // copy for the account, and nothing can be sent from a build in progress.
    static let dev = ProcessInfo.processInfo.environment["NCC_DEV"] != nil

    // A snapshot run (NCC_SNAPSHOT) is a test: no Dock icon, no focus, no window on any screen.
    static let snapshotRun = ProcessInfo.processInfo.environment["NCC_SNAPSHOT"] != nil

    func applicationDidFinishLaunching(_ n: Notification) {
        buildMenu()
        Theme.apply(UserDefaults.standard.string(forKey: "appearance") ?? "system")
        window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 1040, height: 700),
                          styleMask: [.titled, .closable, .miniaturizable, .resizable],
                          backing: .buffered, defer: false)
        window.title = "NCCd"                              // the standard macOS title bar
        window.isReleasedWhenClosed = false
        window.backgroundColor = Theme.mint
        window.minSize = NSSize(width: 300, height: 220)   // small enough for a cell of the slab tiler
        window.delegate = self
        shell.set(main)
        window.contentView = shell
        if let z = UserDefaults.standard.object(forKey: "zoom") as? Double { zoom = CGFloat(z) }
        // NCCd in the menu bar (MenuBar.swift): the wordmark, a card on hover, and the notifications.
        if !Self.snapshotRun {
            menu.onOpen = { [weak self] in self?.showWindow() }
            menu.onOpenChat = { [weak self] g in
                guard let self, let i = self.circles.firstIndex(where: { $0.conf.groupId == g }) else { return }
                self.go(i)
            }
            menu.stats = { [weak self] in self?.menuStats() ?? MenuStats() }
            menu.install()
        }
        radio.onChange = { [weak self] in
            guard let self else { return }
            self.main.setRadio(playing: self.radio.playing, now: self.radio.title)
        }
        main.onRadio = { [weak self] in self?.radio.toggle() }
        if Self.snapshotRun {
            // A test run stays out of the way: far off every screen, never key.
            window.setFrameOrigin(NSPoint(x: -30_000, y: -30_000))
            window.orderBack(nil)
        } else {
            window.center()
            window.setFrameAutosaveName("ncc-circles")
            window.makeKeyAndOrderFront(nil)
        }

        main.onLoginBack = { [weak self] in self?.stopLogins() }
        main.link.button.target = self
        main.link.button.action = #selector(linkSignal)
        // A login badge at the foot of the rail goes to where that login happens.
        main.onAccount = { [weak self] which in
            guard let self else { return }
            self.go(-4)                    // a greyed-out place was pressed: on to the logins, and point at them
            self.main.link.flash()
        }
        main.onSelect = { [weak self] i in
            guard let self else { return }
            // the wordmark, pressed when its page is the one showing: there is nowhere to go, so it bounces
            if i == -1, !self.main.wikiOpen, (self.account == nil ? self.onLogin : self.onStart) { return self.main.bounceLogo() }
            if i == -2, self.main.wikiOpen, !self.onCode { self.main.wikiOpen = false; return }   // the open Wiki tab, picked again: back to the sheet
            self.go(i)
        }
        main.onSend = { [weak self] s, marks in
            guard let self else { return }
            if !self.onStart { self.send(s, mentions: marks, to: self.selected, from: self.main) }
        }
        main.onDoc = { [weak self] id in self?.showDoc(id) }
        // The search runs a moment after the typing stops.
        main.onSearchText = { [weak self] words in
            guard let self else { return }
            if !self.onSearch, !words.isEmpty { self.go(-3) }       // typing in the task bar's search opens what is found
            self.searchWork?.cancel()
            let work = DispatchWorkItem { [weak self] in self?.search() }
            self.searchWork = work
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.25, execute: work)
        }
        main.onBack = { [weak self] in self?.openDoc = nil; self?.wikiTouched = true; self?.drawWiki() }
        main.menuFor = { [weak self] i in self?.circleMenu(i) }
        engine.onEnvelope = { [weak self] e in self?.handle(e) }
        engine.onExit = { [weak self] why in
            self?.main.setStatus("signal-cli stopped" + (why.isEmpty ? "" : ": " + String(why.suffix(160))))
        }

        // Updates: the status row says what happens; the restart waits for a
        // pause in the typing and comes back to the same circle and draft.
        updater.onStatus = { [weak self] s in self?.main.setStatus(s) }
        updater.readyToRestart = { [weak self] in self?.main.quiet ?? true }
        updater.willRestart = { [weak self] in
            guard let self else { return }
            UserDefaults.standard.set(self.main.draft, forKey: "resume.draft")
            UserDefaults.standard.set(self.onStart ? -1 : self.selected, forKey: "resume.circle")
        }
        updater.start()

        // Signal and NCC start side by side; neither waits for the other.
        if !["1", "signal"].contains(ProcessInfo.processInfo.environment["NCC_LOGGED_OUT"] ?? "") {   // test aid
            account = SignalEngine.account()
        }
        buildRail()
        showStart()
        refreshAccounts()
        if account != nil { startSignal() } else { go(-4) }      // not linked: the Login tab is the first thing shown
        loadWiki()
        scheduleSnapshot()
    }

    // Test aid: NCC_SNAPSHOT=<file.png> draws the window into that file after
    // NCC_SNAPSHOT_AFTER seconds (default 10) and quits. It needs no screen
    // capture and no clicks, so a check can run while someone uses the Mac.
    func scheduleSnapshot() {
        let env = ProcessInfo.processInfo.environment
        guard let path = env["NCC_SNAPSHOT"] else { return }
        // NCC_SNAPSHOT_WINDOW=<n> gives the nth circle a window of its own, and
        // the picture is of that window.
        if let n = env["NCC_SNAPSHOT_WINDOW"].flatMap({ Int($0) }) {
            DispatchQueue.main.asyncAfter(deadline: .now() + 5) { self.openWindow(n) }
        }
        // NCC_SNAPSHOT_PICK=1 picks the first circle on the donut, with its transition.
        if env["NCC_SNAPSHOT_PICK"] != nil {
            DispatchQueue.main.asyncAfter(deadline: .now() + 5) { self.picked = true; self.main.testPick() }
        }
        // NCC_SNAPSHOT_PENDING=<words> shows those words as a message on its way
        // (in the open circle). Nothing is sent.
        if let words = env["NCC_SNAPSHOT_PENDING"] {
            DispatchQueue.main.asyncAfter(deadline: .now() + 6.5) {
                guard !self.onStart, self.circles.indices.contains(self.selected) else { return }
                let i = self.selected
                var m = Msg(ts: Int64(Date().timeIntervalSince1970 * 1000), author: self.account?.uuid ?? "", name: nil, body: words)
                m.kind = "pending"
                self.pending[self.circles[i].conf.groupId, default: []].append(m)
                self.refresh()
            }
        }
        // NCC_SNAPSHOT_TOP=1 or 0 puts the tabs along the top, or down the side, for the picture (not kept).
        if let t = env["NCC_SNAPSHOT_TOP"] { main.tabsOnTop = t == "1" }
        // NCC_SNAPSHOT_ZOOM=<factor> zooms the interface for the picture (not kept).
        if let z = env["NCC_SNAPSHOT_ZOOM"].flatMap({ Double($0) }) { main.zoom = CGFloat(z) }
        // NCC_SNAPSHOT_SEARCH=<words> opens the Search tab and searches for them.
        if let words = env["NCC_SNAPSHOT_SEARCH"] {
            DispatchQueue.main.asyncAfter(deadline: .now() + 5) {
                self.go(-3)
                self.main.searchText = words
                self.search()
            }
        }
        // NCC_SNAPSHOT_DROP=1 opens the unibar's dropdown for the picture.
        if env["NCC_SNAPSHOT_DROP"] != nil {
            DispatchQueue.main.asyncAfter(deadline: .now() + 6) { self.main.testDrop() }
        }
        // NCC_SNAPSHOT_QR=<words> shows a code to scan in Signal's pane (a code of those words; no link is started).
        if let words = env["NCC_SNAPSHOT_QR"] { DispatchQueue.main.asyncAfter(deadline: .now() + 3) { self.go(-4); self.main.link.showQR(words) } }
        // NCC_SNAPSHOT_ABOUT=1 pictures the About window.
        if env["NCC_SNAPSHOT_ABOUT"] != nil { DispatchQueue.main.asyncAfter(deadline: .now() + 2) { AboutWindow.shared.show() } }
        // NCC_SNAPSHOT_WAIT=1 shows Signal's pane as it is between the press and the code.
        if env["NCC_SNAPSHOT_WAIT"] != nil { DispatchQueue.main.asyncAfter(deadline: .now() + 3) { self.go(-4); self.main.link.waitForCode() } }
        // NCC_SNAPSHOT_BACK=1 presses "← back" a few seconds after a login was started.
        if env["NCC_SNAPSHOT_BACK"] != nil { DispatchQueue.main.asyncAfter(deadline: .now() + 7) { self.stopLogins() } }
        // NCC_SNAPSHOT_NCC=1 starts the NCC login in its pane.
        if env["NCC_SNAPSHOT_NCC"] != nil { DispatchQueue.main.asyncAfter(deadline: .now() + 3) { self.openWikiLogin() } }
        // NCC_SNAPSHOT_TAB=<index> opens that place first (-2 wiki, -3 search, -4 login, -5 code).
        if let t = env["NCC_SNAPSHOT_TAB"].flatMap({ Int($0) }) {
            DispatchQueue.main.asyncAfter(deadline: .now() + 4) { self.go(t) }
        }
        // NCC_SNAPSHOT_CIRCLE=<n> opens the nth circle of the rail first.
        if let n = env["NCC_SNAPSHOT_CIRCLE"].flatMap({ Int($0) }) {
            DispatchQueue.main.asyncAfter(deadline: .now() + 5) { self.picked = true; self.select(n) }
        }
        // NCC_SNAPSHOT_SIZE=<width>x<height> sizes the window for the picture.
        if let size = env["NCC_SNAPSHOT_SIZE"]?.split(separator: "x").compactMap({ Double($0) }), size.count == 2 {
            window.setContentSize(NSSize(width: size[0], height: size[1]))
            window.setFrameOrigin(NSPoint(x: -30_000, y: -30_000))
        }
        // NCC_SNAPSHOT_DOC=<page title, id or address slug> opens that wiki page first,
        // and writes its markdown beside the picture (<file>.md).
        if let doc = env["NCC_SNAPSHOT_DOC"] {
            DispatchQueue.main.asyncAfter(deadline: .now() + 5) {
                self.wiki.startPage(titled: doc) { id in self.showDoc(id ?? doc) }
                DispatchQueue.main.asyncAfter(deadline: .now() + 3) {
                    try? self.openDoc?.text.write(toFile: path + ".md", atomically: true, encoding: .utf8)
                }
            }
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + (Double(env["NCC_SNAPSHOT_AFTER"] ?? "") ?? 10)) {
            guard !self.leaving else { return }         // a build that was replaced takes no picture
            guard let view = AboutWindow.shared.view ?? (self.satellites.first?.window ?? self.window).contentView,
                  let rep = view.bitmapImageRepForCachingDisplay(in: view.bounds) else { return NSApp.terminate(nil) }
            view.cacheDisplay(in: view.bounds, to: rep)
            // Two things are drawn outside AppKit, and this picture cannot see
            // them: the 3D donut (Metal) and the login page (WebKit). Lay their
            // own snapshots in, then the names and chips over the donut.
            func write(_ web: NSImage?) {
                let img = NSImage(size: view.bounds.size)
                img.lockFocus()
                rep.draw(in: view.bounds)
                if self.satellites.isEmpty, let (donut, overlays, pane) = self.main.donutShot() {
                    donut.draw(in: pane.convert(pane.bounds, to: view))
                    for v in overlays {
                        guard let r = v.bitmapImageRepForCachingDisplay(in: v.bounds) else { continue }
                        v.cacheDisplay(in: v.bounds, to: r)
                        r.draw(in: v.convert(v.bounds, to: view))
                    }
                }
                // the switcher is a window of its own: lay it in where it floats
                if let (card, rect) = self.main.dropShot(), let r = card.bitmapImageRepForCachingDisplay(in: card.bounds) {
                    card.cacheDisplay(in: card.bounds, to: r)
                    r.draw(in: view.convert(rect, from: nil))
                }
                if let web, let pane = self.main.wikiLoginView, !pane.isHiddenOrHasHiddenAncestor {
                    web.draw(in: pane.convert(pane.bounds, to: view))
                }
                img.unlockFocus()
                if let tiff = img.tiffRepresentation, let out = NSBitmapImageRep(data: tiff) {
                    try? out.representation(using: .png, properties: [:])?.write(to: URL(fileURLWithPath: path))
                }
                NSApp.terminate(nil)
            }
            if self.satellites.isEmpty, let web = self.main.wikiLoginView as? WKWebView {
                web.takeSnapshot(with: nil) { img, _ in write(img) }
            } else {
                write(nil)
            }
        }
    }

    // Development reload: this build's app leaves, and says where it was, so
    // that the next build's app comes back to the same tab and the same draft.
    var leaving = false
    func teardown() {
        leaving = true
        UserDefaults.standard.set(main.draft, forKey: "resume.draft")
        UserDefaults.standard.set(onStart || onSearch || onLogin ? -1 : selected, forKey: "resume.circle")
        searchWork?.cancel()
        updater.stop()
        radio.stop()
        engine.cancelLink()
        engine.stop()
        menu.remove()
        satellites.forEach { $0.window.delegate = nil; $0.window.close() }
        satellites = []
        window.delegate = nil
        window.contentView = nil
        window.close()
    }

    func applicationWillTerminate(_ n: Notification) {
        radio.stop()
        engine.cancelLink()
        engine.stop()
    }
    // The window can be shut and the app stays (the donut in the menu bar, or the Dock, opens it again).
    func applicationShouldTerminateAfterLastWindowClosed(_ a: NSApplication) -> Bool { false }
    func applicationShouldHandleReopen(_ a: NSApplication, hasVisibleWindows: Bool) -> Bool {
        if !hasVisibleWindows { window.makeKeyAndOrderFront(nil) }
        return true
    }
    func windowDidBecomeKey(_ n: Notification) { if !circles.isEmpty { markRead() } }

    // ─── the two logins ──────────────────────────────────────────────────────

    // The logins, with who is signed in: the name as the wiki has it, and the
    // name as Signal has it (or the end of the number, when Signal has none).
    func refreshAccounts() {
        let me = account.map { a -> String in
            let name = people.person(a.uuid, fallback: nil).name
            return name.isEmpty || name == a.uuid || name == "me" ? "…" + a.number.suffix(4) : name
        }
        main.setAccounts(ncc: nccName, signal: me)
        main.setSearchEnabled(account != nil || wikiReady)
    }

    // The link panel under the donut, while no Signal account is linked.
    // The Login tab: the two logins, each with what it opens, and the way out of each.
    func refreshLink() {
        let panel = main.link
        panel.nccButton.target = self
        if let name = nccName {
            panel.nccButton.title = "✓ NCC"
            panel.nccButton.action = #selector(askSignOut)
            panel.nccStatus.stringValue = "\(name) · press to sign out"
        } else {
            panel.nccButton.title = " Sign into NCC"
            panel.nccButton.action = #selector(openWikiLogin)
            panel.nccStatus.stringValue = "for wiki & code"
        }
        if let account {
            panel.button.title = "✓ Signal"
            panel.button.action = #selector(unlinkSignal)
            panel.button.isEnabled = true
            let me = people.person(account.uuid, fallback: nil).name
            panel.status.stringValue = (me.isEmpty || me == account.uuid || me == "me" ? account.number : me) + " · press to unlink"
            return
        }
        panel.button.title = " Link Signal"
        panel.button.action = #selector(linkSignal)
        ensureLink()
        if Config.shared.signalCli == nil {
            // The Signal engine (signal-cli) is not on this Mac. With Homebrew
            // here, the app installs it itself, as soon as it opens.
            panel.button.isEnabled = false
            if installing {
                panel.status.stringValue = "installing the Signal engine…"
            } else if Config.brew != nil, !triedInstall {
                installEngine()
            } else if Config.brew == nil {
                panel.status.stringValue = "the Signal engine is missing"
                panel.note.stringValue = "NCCd installs its Signal engine with Homebrew. Install Homebrew (brew.sh), then open NCCd again."
            }
        } else {
            panel.button.isEnabled = !linking
            panel.status.stringValue = linking ? "scan the code with your phone" : "to chat"
        }
    }

    // ─── the Signal engine ───────────────────────────────────────────────────
    // signal-cli is a Homebrew package. The app installs it when it is missing
    // and updates it when asked (menu), and says what Homebrew is doing.
    var installing = false, triedInstall = false

    func brew(_ args: [String], line: @escaping (String) -> Void, done: @escaping (Bool, String) -> Void) {
        guard let brew = Config.brew else { return done(false, "Homebrew is not installed") }
        let p = Process()
        p.executableURL = URL(fileURLWithPath: brew)
        p.arguments = args
        var env = ProcessInfo.processInfo.environment
        env["HOMEBREW_NO_ENV_HINTS"] = "1"
        env["NONINTERACTIVE"] = "1"
        env["PATH"] = (brew as NSString).deletingLastPathComponent + ":/usr/bin:/bin:/usr/sbin:/sbin"
        p.environment = env
        let out = Pipe()
        p.standardOutput = out
        p.standardError = out
        var tail = ""
        out.fileHandleForReading.readabilityHandler = { h in
            guard let s = String(data: h.availableData, encoding: .utf8), !s.isEmpty else { return }
            tail = String((tail + s).suffix(400))
            if let last = s.split(separator: "\n").last { DispatchQueue.main.async { line(String(last.prefix(90))) } }
        }
        p.terminationHandler = { p in
            out.fileHandleForReading.readabilityHandler = nil
            DispatchQueue.main.async { done(p.terminationStatus == 0, tail) }
        }
        do { try p.run() } catch { done(false, error.localizedDescription) }
    }

    func installEngine() {
        installing = true
        triedInstall = true
        main.link.status.stringValue = "installing the Signal engine…"
        main.link.note.stringValue = "Homebrew is installing signal-cli. This takes a minute or two, the first time."
        brew(["install", "signal-cli"], line: { [weak self] s in self?.main.link.note.stringValue = s }) { [weak self] ok, tail in
            guard let self else { return }
            self.installing = false
            Config.shared.hideEngine = false
            self.main.link.note.stringValue = ok && Config.shared.signalCli != nil ? ""
                : "The Signal engine was not installed: " + String(tail.replacingOccurrences(of: "\n", with: " ").suffix(160))
            self.refreshLink()
        }
    }

    @objc func updateEngine() {
        guard Config.brew != nil else { return main.setStatus("the Signal engine is updated with Homebrew, which is not on this Mac") }
        main.setStatus("updating the Signal engine…")
        brew(["upgrade", "signal-cli"], line: { [weak self] s in self?.main.setStatus("Signal engine: " + s) }) { [weak self] ok, tail in
            guard let self else { return }
            guard ok else { return self.main.setStatus("Signal engine not updated: " + String(tail.replacingOccurrences(of: "\n", with: " ").suffix(120))) }
            self.main.setStatus("the Signal engine is up to date")
            if self.account != nil, !Self.dev {            // start again on the new engine
                self.engine.stop()
                self.startSignal()
            }
        }
    }

    // What the menu bar's card shows: each chat's unread, and the last day by the hour.
    func menuStats() -> MenuStats {
        var s = MenuStats()
        guard account != nil else { return s }
        let now = Int64(Date().timeIntervalSince1970 * 1000), hour: Int64 = 3_600_000
        for (i, c) in circles.enumerated() {
            let since = (UserDefaults.standard.object(forKey: readKey(c)) as? Int64) ?? 0
            s.rows.append((c.conf.title, color(of: i), c.log.unread(since: since, me: account?.uuid ?? "")))
            for m in c.log.msgs.reversed() {
                let ago = (now - m.ts) / hour
                if ago >= 24 { break }
                if ago >= 0, m.kind == nil { s.hours[23 - Int(ago)] += 1 }
            }
        }
        return s
    }

    // "starting Signal…" goes when Signal first answers, or after a few seconds
    // in any case (a fresh link can be slow to list its groups; the app is usable meanwhile).
    func clearStarting() { if main.statusText.hasPrefix("starting Signal") { main.setStatus("") } }

    @objc func showAbout() { AboutWindow.shared.show() }

    @objc func showWindow() {
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    // Unlink: NCCd stops being a Signal device on this Mac. Its own keys are
    // deleted; a link that other tools on this Mac share is left where it is,
    // and NCCd only stops using it.
    @objc func unlinkSignal() {
        let own = Config.shared.conf.signalConfigDir == nil && ProcessInfo.processInfo.environment["NCC_SIGNAL_CONFIG"] == nil
        let alert = NSAlert()
        alert.messageText = "Unlink Signal from NCCd?"
        alert.informativeText = own
            ? "This deletes NCCd's Signal keys from this Mac. Then remove NCCd under Linked devices in Signal on your phone. The messages already kept on this Mac stay."
            : "NCCd stops using the Signal link that other tools on this Mac share. That link stays as it is."
        alert.addButton(withTitle: "Unlink")
        alert.addButton(withTitle: "Cancel")
        guard alert.runModal() == .alertFirstButtonReturn else { return }
        engine.stop()
        if own {
            try? FileManager.default.removeItem(at: Config.shared.signalConfigDir)
        } else {
            Config.shared.conf.signalConfigDir = nil
            Config.shared.save()
        }
        satellites.forEach { $0.window.close() }
        account = nil
        circles = []
        pending = [:]
        main.setStatus("")
        buildRail()
        refreshAccounts()
        go(-4)
    }

    // The Code tab: the club's forge (Forgejo), in the app. It shares the web
    // session of the NCC login, so the forge's own "sign in with NCC" goes
    // through without the password again.
    func showCode() {
        if codeWeb == nil {
            let w = WKWebView(frame: .zero, configuration: WKWebViewConfiguration())
            w.allowsBackForwardNavigationGestures = true
            w.setValue(false, forKey: "drawsBackground")
            // when the forge sends the member to the club's login, that page is dressed as in the wiki's sign-in
            let c = loginColours()
            w.configuration.userContentController.addUserScript(WKUserScript(
                source: WikiLogin.dress(ink: c.ink, field: c.field, accent: c.accent, onAccent: c.onAccent),
                injectionTime: .atDocumentEnd, forMainFrameOnly: true))
            if let url = URL(string: Config.shared.conf.forgeURL ?? "https://git.nela.computer") { w.load(URLRequest(url: url)) }
            codeWeb = w
        }
        codeWeb?.pageZoom = zoom
        onCode = true
        main.paneTab = -5
        main.showWikiLogin(codeWeb)
        main.wikiOpen = true
        window.title = "NCCd: code"
    }

    @objc func askSignOut() {
        let alert = NSAlert()
        alert.messageText = "Sign out of NCC?"
        alert.informativeText = "The wiki and the code close until you sign in again."
        alert.addButton(withTitle: "Sign Out")
        alert.addButton(withTitle: "Cancel")
        if alert.runModal() == .alertFirstButtonReturn { signOut() }
    }

    // "← back" on the Login page: stop the login that is under way.
    func stopLogins() {
        linking = false                      // the request stays open behind: the code is on hand for the next press
        main.link.showQR(nil)
        login = nil
        main.link.showNCC(nil)
        main.link.note.stringValue = ""
        refreshLink()
    }

    @objc func openWikiLogin() { go(-4); if wiki.token == nil, login == nil { showLogin() } }

    // The code to scan is made ahead of time: while Signal is not linked the app
    // keeps a link request open with Signal, so the code is on hand, and a press
    // on Link Signal shows it at once. A request lasts a minute or so; when one
    // ends unused, the next is made. (`linking` = the member is looking at it.)
    var linkURI: String?
    var linkRunning = false
    var linkFails = 0

    func ensureLink() {
        guard account == nil, !linkRunning, Config.shared.signalCli != nil, !installing,
              !Self.dev, !Self.snapshotRun, ProcessInfo.processInfo.environment["NCC_LOGGED_OUT"] == nil else { return }
        linkRunning = true
        let began = Date()
        engine.link(deviceName: "NCCd", onURI: { [weak self] uri in
            guard let self else { return }
            self.linkURI = uri
            if self.linking { self.main.link.showQR(uri) }
        }) { [weak self] r in
            guard let self else { return }
            self.linkRunning = false
            self.linkURI = nil
            switch r {
            case .success:
                self.linking = false
                self.main.link.showQR(nil)
                self.account = SignalEngine.account()
                self.main.link.note.stringValue = ""
                self.refreshAccounts()
                self.buildRail()
                self.refreshLink()
                if self.account != nil { self.startSignal(); self.showStart() }      // linked: on to the donut
            case .failure(let e):
                // A request that ran its time and was not used: make the next. One that
                // fails at once, three times running, is a real fault: say so and stop.
                self.linkFails = Date().timeIntervalSince(began) < 8 ? self.linkFails + 1 : 0
                if self.linkFails >= 3 {
                    self.linking = false
                    self.main.link.showQR(nil)
                    self.main.link.note.stringValue = "Signal: \(e.message)"
                    return self.refreshLink()
                }
                if self.linking { self.main.link.waitForCode() }
                DispatchQueue.main.asyncAfter(deadline: .now() + 2) { self.ensureLink() }
            }
        }
    }

    @objc func linkSignal() {
        linking = true
        linkFails = 0
        main.link.note.stringValue = ""
        if let uri = linkURI { main.link.showQR(uri) }      // on hand already: no wait, nothing jumps
        else { main.link.waitForCode(); ensureLink() }
        refreshLink()
    }

    // This theme's colours as CSS, for the club's login page.
    func loginColours() -> (ink: String, field: String, accent: String, onAccent: String) {
        func css(_ c: NSColor) -> String {
            var out = c
            window.effectiveAppearance.performAsCurrentDrawingAppearance { out = c.usingColorSpace(.sRGB) ?? c }
            return String(format: "rgba(%d,%d,%d,%.2f)", Int(out.redComponent * 255), Int(out.greenComponent * 255), Int(out.blueComponent * 255), out.alphaComponent)
        }
        return (css(Theme.ink), css(Theme.pair("FFFFFF", "061820", alpha: 0.8)), css(Donut.icing), css(Theme.on(Donut.icing)))
    }

    // The club's login page, in the wiki pane. When the wiki has its session, the wiki opens.
    func showLogin() {
        // The club's login form runs inside NCC's own pane of the Login page, in
        // that pane's colours; Signal's link runs in Signal's. Each on its own.
        let l = WikiLogin(base: wiki.base, ink: "rgba(255,255,255,1)", field: "rgba(0,0,0,0.28)",
                          accent: "rgba(0,208,128,1)", onAccent: "rgba(6,34,24,1)")
        l.onToken = { [weak self] token in
            guard let self else { return }
            self.wiki.setToken(token)
            self.buildRail()                 // the wiki has its tab now
            self.login = nil
            self.main.link.showNCC(nil)
            self.loadWiki()
            self.refreshLink()
        }
        login = l
        l.zoom = zoom
        main.link.showNCC(l.web)
        l.start()
    }

    @objc func signOut() {
        wiki.setToken("")
        wikiReady = false
        nccName = nil
        openDoc = nil
        startDoc = nil
        wikiTree = nil
        wikiTouched = false
        refreshAccounts()
        buildRail()
        refresh()
        WikiLogin.forget { [weak self] in self?.refreshLink(); self?.go(-4) }
    }

    // ─── Signal ──────────────────────────────────────────────────────────────

    func startSignal() {
        guard let account else { return }
        loadCircles()
        if Self.dev { return main.setStatus("development: Signal is not started; this is what is on this Mac") }
        if !Self.snapshotRun { menu.askToNotify() }
        main.setStatus("starting Signal…")
        DispatchQueue.main.asyncAfter(deadline: .now() + 6) { [weak self] in self?.clearStarting() }
        do { try engine.start(account: account) } catch {
            main.setStatus((error as? SignalEngine.EngineError)?.message ?? error.localizedDescription)
            return
        }
        engine.call("listContacts") { [weak self] r in
            if case .success(let v) = r, let list = v as? [[String: Any]] { self?.people.setContacts(list) }
            self?.refreshAccounts()                 // Signal's own name for this account is known now
            self?.clearStarting()                   // Signal answered: it has started
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
        // After an update the app comes back where it was: the circle, and the draft.
        let d = UserDefaults.standard
        if let at = d.object(forKey: "resume.circle") as? Int {
            d.removeObject(forKey: "resume.circle")
            if at >= 0 { selected = at; picked = true }
            resumeDraft = d.string(forKey: "resume.draft")
            d.removeObject(forKey: "resume.draft")
        }
        if picked, !circles.isEmpty { select(min(selected, circles.count - 1)) } else { showStart() }
        if let draft = resumeDraft, picked { main.draft = draft; resumeDraft = nil }
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
            circles[i].memberIds = ((g["members"] as? [[String: Any]]) ?? []).compactMap { $0["uuid"] as? String }
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

    // The rail: the commons first, then the circles, then the wiki. No headings,
    // and no row for the donut (the wordmark's tab is the way to it).
    func buildRail() {
        // Every place has its tab from the start. A place whose login is not
        // done is greyed out (Signal: the chats and circles; NCC: the wiki and
        // the code; either: the search), and a press on it goes to the logins.
        var off: Set<Int> = []
        var commons: [(Int, String, NSColor?)] = [], rings: [(Int, String, NSColor?)] = []
        if account != nil {
            for (i, c) in circles.enumerated() {
                c.conf.kind == "circle" ? rings.append((i, c.conf.title, color(of: i))) : commons.append((i, c.conf.title, color(of: i)))
            }
        } else {
            // not linked: the chats this Mac knows of, by name, or just "Chat"
            let known = Config.shared.conf.circles
            for (n, c) in known.enumerated() {
                let row: (Int, String, NSColor?) = (-100 - n, c.title, Theme.circleColor(c.title, wiki: nil))
                c.kind == "circle" ? rings.append(row) : commons.append(row)
                off.insert(-100 - n)
            }
            if known.isEmpty { commons.append((-100, "Chat", Theme.circleColor("Chat", wiki: nil))); off.insert(-100) }
        }
        if wiki.token == nil { off.formUnion([-2, -5]) }
        if account == nil, wiki.token == nil { off.insert(-3) }
        main.disabledTabs = off
        main.setRail([("", commons), ("", rings),
                      ("", [(-2, "Wiki", Theme.wikiBand), (-5, "Code", Theme.code), (-3, "Search", Theme.search), (-4, "Login", Theme.login)])])
        updateUnread()
    }

    // A circle's colour: its wiki collection's, else one of the club's own.
    func color(of i: Int) -> NSColor {
        let title = circles[i].conf.title
        return Theme.circleColor(title, wiki: wikiReady ? wiki.color(for: title) : nil)
    }

    // The donut page: every chat is a token, and its newest message floats off it.
    func diagramItems() -> [DiagramItem] {
        let now = Date().timeIntervalSince1970
        return circles.enumerated().map { i, c in
            let since = (UserDefaults.standard.object(forKey: readKey(c)) as? Int64) ?? 0
            var it = DiagramItem(index: i, title: c.conf.title, commons: c.conf.kind != "circle", color: color(of: i),
                                 members: c.members, unread: c.log.unread(since: since, me: account?.uuid ?? ""),
                                 lastActive: c.log.msgs.last?.ts ?? 0)
            if let m = c.log.msgs.last {
                let age = now - Double(m.ts) / 1000
                it.when = age < 90 ? "now" : age < 3600 ? "\(Int(age / 60))m" : age < 86400 ? "\(Int(age / 3600))h" : "\(Int(age / 86400))d"
                it.who = m.author == account?.uuid ? "me" : String(people.person(m.author, fallback: m.name).name.prefix(16))
                let words = m.body.replacingOccurrences(of: "\u{FFFC}", with: "").trimmingCharacters(in: .whitespacesAndNewlines)
                it.text = words.isEmpty ? ((m.files ?? []).isEmpty ? "" : "[picture or file]") : words
            }
            return it
        }
    }

    func select(_ i: Int) {
        guard circles.indices.contains(i) else { return }
        selected = i
        onStart = false
        onSearch = false
        onLogin = false
        markRead()
        refresh()
        refreshWiki()
        main.focusField()
    }

    // Draw the sheet: the donut, or the open circle's messages. The wiki pane
    // is its own (drawWiki), so a page stays open while the circles change.
    func refresh() {
        defer {
            satellites.forEach(draw)
            // the rail marks the circles that have windows of their own
            main.setWindowed(Set(satellites.compactMap { s in circles.firstIndex { $0.conf.groupId == s.groupId } }))
        }
        main.showLink(onLogin)                        // the logins have a tab of their own
        if onLogin {
            window.title = "NCCd"
            main.showSearch(false)
            main.showRadio(false)
            main.show(index: -4, heading: "login", color: Theme.login, roleLine: [], canSend: false, placeholder: "")
            main.renderBlank()
            main.showLink(true)
            refreshLink()
            return updateUnread()
        }
        if onSearch {
            window.title = "NCCd: search"
            main.showRadio(false)
            main.show(index: -3, heading: "search", color: Theme.search, roleLine: [], canSend: false, placeholder: "")
            main.showSearch(true)
            search()
            return updateUnread()
        }
        main.showSearch(false)
        if onStart {
            main.showRadio(false)
            window.title = "NCCd"                         // the donut is NCCd itself; elsewhere the title adds where
            // Home is one image: the donut, a token on it for every chat, and the newest message of each floating off its token.
            main.show(index: -1, heading: "donut", diagram: diagramItems(), roleLine: [], canSend: false, placeholder: "")
            main.renderBlank()
            return updateUnread()
        }
        guard circles.indices.contains(selected) else { return }
        let c = circles[selected]
        window.title = "NCCd: " + c.conf.title + (c.members > 0 ? " · \(c.members) members" : "")
        main.show(index: selected, heading: heading(c), color: color(of: selected),
                  roleLine: c.conf.roleKey.map { people.roles(forKey: $0) } ?? [],
                  canSend: c.canSend,
                  placeholder: c.canSend ? "write to \(c.conf.title)" : "only admins write here")
        main.mentionable = mentionable(c)
        main.showRadio(c.conf.title.lowercased().contains("label"))       // the station plays from the Netlabel tab
        main.render(ledger(c), reactions: c.log.reactions, people: people, me: account?.uuid ?? "")
        updateUnread()
    }

    // ─── the global search ───────────────────────────────────────────────────
    // One search over what this member can read: the messages of every circle
    // and chat (with Signal linked), and the wiki's pages (with the NCC login).
    // With one of the two, it searches that one.
    func search() {
        guard onSearch else { return }
        let q = main.searchText.trimmingCharacters(in: .whitespaces)
        let sources = [account != nil ? "the circles and chats" : nil, wikiReady ? "the wiki" : nil].compactMap { $0 }
        guard q.count >= 2 else {
            let what = sources.isEmpty ? "Link Signal or sign in to NCC to have something to search." : "Searches " + sources.joined(separator: " and ") + "."
            return main.renderSearch(query: "", found: [], note: what)
        }
        let low = q.lowercased()
        let day = DateFormatter()
        day.dateFormat = "d MMM yyyy HH:mm"
        var hits: [(Int64, MainView.Found)] = []
        for (i, c) in circles.enumerated() where account != nil {
            for m in c.log.msgs.reversed() where m.kind == nil && m.body.lowercased().contains(low) {
                let who = m.author == account?.uuid ? "me" : people.person(m.author, fallback: m.name).name
                hits.append((m.ts, MainView.Found(link: URL(string: "ncc://circle/\(i)")!, place: c.conf.title, color: color(of: i),
                                                  when: day.string(from: Date(timeIntervalSince1970: Double(m.ts) / 1000)), who: who,
                                                  text: Self.around(m.body, low))))
                if hits.count > 400 { break }
            }
        }
        let messages = hits.sorted { $0.0 > $1.0 }.prefix(120).map { $0.1 }
        let show: ([MainView.Found]) -> Void = { [weak self] pages in
            guard let self, self.onSearch, self.main.searchText.trimmingCharacters(in: .whitespaces) == q else { return }
            let note = "\(messages.count) message\(messages.count == 1 ? "" : "s")" + (self.wikiReady ? ", \(pages.count) wiki page\(pages.count == 1 ? "" : "s")" : "")
                + " · " + (sources.isEmpty ? "nothing to search" : sources.joined(separator: " and "))
            self.main.renderSearch(query: q, found: pages + messages, note: note)
        }
        show([])
        guard wikiReady else { return }
        wiki.search(q) { [weak self] pages in
            guard let self else { return }
            show(pages.map { MainView.Found(link: URL(string: "ncc://doc/" + $0.id)!, place: "Wiki", color: nil, when: "", who: $0.title, text: $0.context) })
        }
    }

    // The words round the first place the query is found.
    static func around(_ body: String, _ low: String) -> String {
        let flat = body.replacingOccurrences(of: "\n", with: " ").replacingOccurrences(of: "\u{FFFC}", with: "")
        let ns = flat as NSString
        let r = (flat.lowercased() as NSString).range(of: low)
        guard r.location != NSNotFound, ns.length > 220 else { return flat }
        let from = max(0, r.location - 80), to = min(ns.length, r.location + r.length + 130)
        return (from > 0 ? "…" : "") + ns.substring(with: NSRange(location: from, length: to - from)) + (to < ns.length ? "…" : "")
    }

    // A circle's ledger: its log, then the messages still on their way.
    func ledger(_ c: Circle, last: Int = 600) -> [Msg] {
        Array(c.log.msgs.suffix(last)) + (pending[c.conf.groupId] ?? [])
    }
    // Who "@" can name in a circle: its members, but not the member themself.
    func mentionable(_ c: Circle) -> [(name: String, id: String)] {
        c.memberIds.filter { $0 != account?.uuid }.map { (people.person($0, fallback: nil).name, $0) }
            .filter { !$0.0.isEmpty && $0.0 != $0.1 }.sorted { $0.0.lowercased() < $1.0.lowercased() }
    }

    func heading(_ c: Circle) -> String {
        c.conf.title.uppercased() + (c.members > 0 ? " · \(c.members) members" : "") + " · " + c.conf.name
    }

    // ─── a circle in a window of its own ─────────────────────────────────────

    final class Satellite: NSObject, NSWindowDelegate {
        let groupId: String
        let view = MainView(solo: true)
        let shell = Shell()
        let window: NSWindow
        var onClose: (() -> Void)?
        var onKey: (() -> Void)?
        init(groupId: String) {
            self.groupId = groupId
            window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 560, height: 520),
                              styleMask: [.titled, .closable, .miniaturizable, .resizable], backing: .buffered, defer: false)
            super.init()
            window.isReleasedWhenClosed = false
            window.minSize = NSSize(width: 300, height: 220)
            shell.set(view)
            window.contentView = shell
            window.delegate = self
        }
        func windowWillClose(_ n: Notification) { onClose?() }
        func windowDidBecomeKey(_ n: Notification) { onKey?() }
    }

    func openWindow(_ i: Int) {
        guard circles.indices.contains(i) else { return }
        let gid = circles[i].conf.groupId
        if let s = satellites.first(where: { $0.groupId == gid }) { return s.window.makeKeyAndOrderFront(nil) }
        let s = Satellite(groupId: gid)
        s.view.zoom = zoom
        s.view.onSend = { [weak self, weak s] text, marks in
            guard let self, let s, let at = self.circles.firstIndex(where: { $0.conf.groupId == gid }) else { return }
            self.send(text, mentions: marks, to: at, from: s.view)
        }
        s.onClose = { [weak self, weak s] in self?.satellites.removeAll { $0 === s }; self?.refresh() }
        s.onKey = { [weak self] in
            guard let self, let at = self.circles.firstIndex(where: { $0.conf.groupId == gid }) else { return }
            self.markRead(at)
        }
        satellites.append(s)
        refresh()
        s.window.setFrameAutosaveName("ncc-circle-" + gid)
        if s.window.frame.origin == .zero { s.window.cascadeTopLeft(from: NSPoint(x: window.frame.minX + 40, y: window.frame.maxY - 40)) }
        draw(s)
        if Self.snapshotRun {
            s.window.setFrameOrigin(NSPoint(x: -30_000, y: -28_000))
            s.window.orderBack(nil)
        } else {
            s.window.makeKeyAndOrderFront(nil)
            s.view.focusField()
        }
    }

    func draw(_ s: Satellite) {
        guard let account, let i = circles.firstIndex(where: { $0.conf.groupId == s.groupId }) else { return }
        let c = circles[i]
        s.window.title = "NCCd: " + c.conf.title + (c.members > 0 ? " · \(c.members) members" : "")
        s.window.backgroundColor = Theme.wash(color(of: i))
        s.view.show(index: i, heading: heading(c), color: color(of: i),
                    roleLine: c.conf.roleKey.map { people.roles(forKey: $0) } ?? [],
                    canSend: c.canSend, placeholder: c.canSend ? "write to \(c.conf.title)" : "only admins write here")
        s.view.mentionable = mentionable(c)
        s.view.render(ledger(c), reactions: c.log.reactions, people: people, me: account.uuid)
    }

    // The menu on a circle's row of the rail (right-click).
    func circleMenu(_ i: Int) -> NSMenu? {
        guard circles.indices.contains(i) else { return nil }
        let menu = NSMenu()
        for (title, action) in [("Open", #selector(menuOpen(_:))), ("Open in New Window", #selector(menuOpenWindow(_:))),
                                ("Mark as Read", #selector(menuMarkRead(_:)))] {
            let item = menu.addItem(withTitle: title, action: action, keyEquivalent: "")
            item.target = self
            item.tag = i
        }
        return menu
    }
    @objc func menuOpen(_ sender: NSMenuItem) { picked = true; select(sender.tag) }
    @objc func menuOpenWindow(_ sender: NSMenuItem) { openWindow(sender.tag) }
    @objc func menuMarkRead(_ sender: NSMenuItem) { markRead(sender.tag) }
    @objc func openSelectedInWindow() { if !onStart { openWindow(selected) } }

    // ─── wiki ────────────────────────────────────────────────────────────────

    func loadWiki() {
        // No session yet: the Login page has the way in.
        guard wiki.token != nil else { return }
        wiki.load { [weak self] ok in
            guard let self else { return }
            self.wikiReady = ok
            self.people.wikiColors = self.wiki.userColors
            guard ok else {
                return self.main.renderWiki(heading: "wiki", rows: [], note: "The wiki did not answer.\nMenu → Sign Out of NCC, then sign in again.")
            }
            self.wiki.me { name in
                self.nccName = name ?? "signed in"
                self.refreshAccounts()
                self.refreshLink()
            }
            self.buildRail()        // the wiki's collection colours are in now
            self.refresh()
            self.refreshWiki()
            // The club's start page is the first thing in the wiki pane.
            self.wiki.startPage(titled: Config.shared.conf.wikiStart ?? "STARTING PAGE") { id in
                guard let id else { return }
                self.wiki.page(id) { page in
                    guard let page else { return }
                    self.startDoc = (page.id, page.title, page.text, URL(string: self.wiki.base + page.path))
                    self.fetchPictures(page.text)
                    if !self.wikiTouched, self.openDoc == nil { self.openDoc = self.startDoc; self.drawWiki() }
                }
            }
        }
    }

    func showStart() {
        onStart = true
        onSearch = false
        onLogin = false
        refresh()
        refreshWiki()
    }

    // The page tree of the wiki pane: the open circle's collection, or on the
    // donut (and for a circle with no collection) the whole wiki.
    func refreshWiki() {
        guard wikiReady else { return }
        let i = selected, start = onStart
        let own = start || !circles.indices.contains(i) ? [] : wiki.collections(for: circles[i].conf.title)
        let cols = own.isEmpty ? wiki.allCollections : own
        let heading = own.isEmpty ? "wiki" : "wiki · " + own.map { $0.name }.joined(separator: " + ")
        wiki.tree(cols) { [weak self] rows in
            guard let self, self.selected == i, self.onStart == start else { return }
            self.wikiTree = (heading, rows)
            if self.openDoc == nil { self.drawWiki() }      // a page being read stays where it is
        }
    }

    // Draw the wiki pane: the open page, or the page tree with the page last read marked.
    func drawWiki() {
        if let d = openDoc {
            lastDocId = d.id
            return main.renderWikiDoc(title: d.title, markdown: d.text, url: d.url, wikiBase: wiki.base, image: wiki.picture)
        }
        guard let t = wikiTree else { return }
        main.renderWiki(heading: t.heading, rows: t.rows, note: t.rows.isEmpty ? "No pages." : nil, open: lastDocId)
    }

    func showDoc(_ id: String) {
        wikiTouched = true
        main.wikiOpen = true
        wiki.page(id) { [weak self] page in
            guard let self else { return }
            self.openDoc = page.map { ($0.id, $0.title, $0.text, URL(string: self.wiki.base + $0.path)) }
                ?? ("", "Not found", "This page could not be read. The wiki key may not reach it.", nil)
            if let page { self.fetchPictures(page.text) }
            self.drawWiki()
        }
    }

    // Fetch a page's pictures; each one that arrives draws the page again, in place.
    func fetchPictures(_ markdown: String) {
        wiki.fetchPictures(Markdown.images(in: markdown)) { [weak self] in
            guard let self, self.openDoc != nil else { return }
            self.drawWiki()
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

    func markRead() { if !onStart { markRead(selected) } }
    func markRead(_ i: Int) {
        guard circles.indices.contains(i), let last = circles[i].log.msgs.last else { return }
        UserDefaults.standard.set(last.ts, forKey: readKey(circles[i]))
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
        // A new message from someone else is a notification, unless its chat is the one being read
        // (and not the old messages that a fresh link catches up on).
        let reading = NSApp.isActive && ((i == selected && !onStart && !onSearch && !onLogin && !main.wikiOpen)
            || satellites.contains { $0.groupId == gid && $0.window.isKeyWindow })
        let fresh = abs(Int64(Date().timeIntervalSince1970 * 1000) - ts) < 120_000
        if m.kind == nil, author != account?.uuid, fresh, !reading, !Self.dev, !Self.snapshotRun {
            menu.notify(chat: circles[i].conf.title, groupId: gid, who: people.person(author, fallback: name).name,
                        words: m.body.isEmpty ? "sent a picture or a file" : m.body)
        }
        if i == selected, !onStart {
            refresh()
            if window.isKeyWindow { markRead() }
        } else if onStart {
            refresh()                       // the donut shows who spoke
        }
        for s in satellites where s.groupId == gid {
            draw(s)
            if s.window.isKeyWindow { markRead(i) }
        }
        updateUnread()
    }

    // ─── Signal out ──────────────────────────────────────────────────────────

    // `view` is where the words were typed: the main window, or the circle's own.
    // The message goes into the ledger at once, glimmering, and leaves the
    // composer. When Signal has it, it lands (it is logged, and stops
    // glimmering). If Signal refuses it, or says nothing for 40 seconds, it
    // comes out of the ledger and goes back into the composer as a draft.
    final class Sending { var settled = false, gaveUp = false }

    func send(_ text: String, mentions: [String], to i: Int, from view: MainView) {
        if Self.dev { return view.setStatus("development: nothing is sent from a build in progress") }
        guard circles.indices.contains(i), let account else { return }
        let gid = circles[i].conf.groupId
        let temp = Int64(Date().timeIntervalSince1970 * 1000)
        var waiting = Msg(ts: temp, author: account.uuid, name: nil, body: text)
        waiting.kind = "pending"
        pending[gid, default: []].append(waiting)
        view.clearField()
        view.setStatus("")
        refresh()

        let state = Sending()
        let back: (String) -> Void = { [weak self, weak view] why in
            guard let self else { return }
            self.pending[gid]?.removeAll { $0.ts == temp }
            if let view, view.draft.isEmpty { view.draft = text }       // back to the composer, unless new words are there
            view?.setStatus("not sent (\(why)); your words are back in the composer")
            self.refresh()
        }
        var params: [String: Any] = ["groupId": gid, "message": text]
        if !mentions.isEmpty { params["mention"] = mentions }
        engine.call("send", params) { [weak self, weak view] r in
            guard let self else { return }
            state.settled = true
            switch r {
            case .failure(let e):
                if !state.gaveUp { back(e.message) }
            case .success(let v):
                let ts = ((v as? [String: Any])?["timestamp"] as? NSNumber)?.int64Value ?? temp
                self.pending[gid]?.removeAll { $0.ts == temp }
                guard let at = self.circles.firstIndex(where: { $0.conf.groupId == gid }) else { return }
                self.circles[at].log.add(Msg(ts: ts, author: account.uuid, name: nil, body: text))
                // it landed after all, after it was given up: take the copy out of the composer again
                if state.gaveUp, let view, view.draft == text { view.clearField() }
                view?.setStatus("")
                self.markRead(at)
                self.refresh()
            }
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 40) {
            guard !state.settled else { return }
            state.gaveUp = true
            back("Signal did not answer")
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

    // The app, and the Signal engine under it (when Homebrew keeps it).
    @objc func checkForUpdates() {
        updater.check(asked: true)
        if Config.brew != nil, Config.shared.signalCli != nil, !Self.dev { updateEngine() }
    }
    // ─── tabs, as a browser moves through them ───────────────────────────────

    // Open a tab: the donut (-1), a circle, or the wiki (-2).
    func go(_ i: Int) {
        if i == -5 { return showCode() }
        if i == -2 {                          // the wiki has its plane back from the code
            onCode = false
            main.paneTab = -2
            main.showWikiLogin(login?.web)
            if login == nil { drawWiki() }
            main.wikiOpen = true
            return
        }
        onCode = false
        main.wikiOpen = false                 // a circle, the donut or the search: the wiki steps aside
        // With Signal not linked, NCCd's own page is the logins.
        if main.link.flowing { stopLogins() }          // leaving a login that is under way stops it: it starts afresh next time
        if i == -4 || (i == -1 && account == nil) { onLogin = true; onSearch = false; onStart = false; refresh(); return }
        onLogin = false
        if i == -3 { onSearch = true; onStart = false; refresh(); return }
        onSearch = false
        picked = true
        i < 0 ? showStart() : select(i)
    }
    // The donut first, then the tabs of the rail in their order.
    var tabs: [Int] { [-1] + main.tabOrder.filter { $0 != -1 } }
    var currentTab: Int { main.wikiOpen ? (onCode ? -5 : -2) : onLogin ? -4 : onSearch ? -3 : onStart ? -1 : selected }
    func stepTab(_ by: Int) {
        let all = tabs
        guard !all.isEmpty else { return }
        let at = all.firstIndex(of: currentTab) ?? 0
        go(all[(at + by + all.count) % all.count])
    }
    @objc func nextTab() { stepTab(1) }
    @objc func previousTab() { stepTab(-1) }
    // ⌘1 to ⌘8 open that tab; ⌘9 opens the last one, as in a browser.
    @objc func numberedTab(_ sender: NSMenuItem) {
        let all = tabs
        guard !all.isEmpty else { return }
        go(sender.tag == 9 ? all[all.count - 1] : all[min(sender.tag - 1, all.count - 1)])
    }

    // The tabs down the side, or along the top like a browser's.
    @objc func toggleTabsOnTop(_ sender: NSMenuItem) {
        let on = !UserDefaults.standard.bool(forKey: "tabsOnTop")
        UserDefaults.standard.set(on, forKey: "tabsOnTop")
        main.tabsOnTop = on
        sender.state = on ? .on : .off
    }
    @objc func zoomIn() { zoom += 0.1 }
    @objc func zoomOut() { zoom -= 0.1 }
    @objc func zoomReset() { zoom = 1 }
    @objc func toggleRadio() { radio.toggle() }

    @objc func setAppearance(_ sender: NSMenuItem) {
        let mode = (sender.representedObject as? String) ?? "system"
        UserDefaults.standard.set(mode, forKey: "appearance")
        Theme.apply(mode)
        buildRail()
        refresh()
        drawWiki()
    }

    // ─── menu ────────────────────────────────────────────────────────────────

    func buildMenu() {
        let bar = NSMenu()
        let app = NSMenu()
        // A short menu. The logins and the ways out of them are on the app's first
        // page; the old messages are read in by themselves on a first link.
        app.addItem(withTitle: "About NCCd", action: #selector(showAbout), keyEquivalent: "").target = self
        app.addItem(withTitle: "Check for Updates…", action: #selector(checkForUpdates), keyEquivalent: "")
        app.addItem(.separator())
        app.addItem(withTitle: "Quit NCCd", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
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
        let win = NSMenu(title: "Window")
        // The tab shortcuts of Safari and Chrome.
        func key(_ title: String, _ action: Selector, _ key: String, _ mods: NSEvent.ModifierFlags, hidden: Bool = false, tag: Int = 0) {
            let item = win.addItem(withTitle: title, action: action, keyEquivalent: key)
            item.keyEquivalentModifierMask = mods
            item.tag = tag
            if hidden { item.isHidden = true; item.allowsKeyEquivalentWhenHidden = true }
        }
        key("Show Next Tab", #selector(nextTab), "\t", [.control])
        key("Show Previous Tab", #selector(previousTab), "\t", [.control, .shift])
        key("Show Next Tab", #selector(nextTab), "]", [.command, .shift], hidden: true)
        key("Show Previous Tab", #selector(previousTab), "[", [.command, .shift], hidden: true)
        key("Show Next Tab", #selector(nextTab), "}", [.command], hidden: true)
        key("Show Previous Tab", #selector(previousTab), "{", [.command], hidden: true)
        for n in 1...9 { key("Tab \(n)", #selector(numberedTab(_:)), "\(n)", [.command], hidden: true, tag: n) }
        win.addItem(.separator())
        win.addItem(withTitle: "Open Circle in New Window", action: #selector(openSelectedInWindow), keyEquivalent: "n")
        win.addItem(.separator())
        win.addItem(withTitle: "Minimize", action: #selector(NSWindow.performMiniaturize(_:)), keyEquivalent: "m")
        win.addItem(withTitle: "Zoom", action: #selector(NSWindow.performZoom(_:)), keyEquivalent: "")
        NSApp.windowsMenu = win
        view.addItem(.separator())
        let onTop = view.addItem(withTitle: "Tabs on Top", action: #selector(toggleTabsOnTop(_:)), keyEquivalent: "")
        onTop.state = UserDefaults.standard.bool(forKey: "tabsOnTop") ? .on : .off
        view.addItem(.separator())
        view.addItem(withTitle: "Zoom In", action: #selector(zoomIn), keyEquivalent: "+")
        let also = view.addItem(withTitle: "Zoom In", action: #selector(zoomIn), keyEquivalent: "=")   // ⌘= with no Shift
        also.isAlternate = false
        also.isHidden = true
        also.allowsKeyEquivalentWhenHidden = true
        view.addItem(withTitle: "Zoom Out", action: #selector(zoomOut), keyEquivalent: "-")
        view.addItem(withTitle: "Actual Size", action: #selector(zoomReset), keyEquivalent: "0")
        view.addItem(.separator())
        view.addItem(withTitle: "Play or Stop \(Radio.name)", action: #selector(toggleRadio), keyEquivalent: "")
        for m in [app, edit, view, win] {
            let item = NSMenuItem()
            item.submenu = m
            bar.addItem(item)
        }
        NSApp.mainMenu = bar
    }
}

// ─── entry points ────────────────────────────────────────────────────────────
// The app's code has no top-level statements, so the same sources build two
// ways: into the release executable (Launch/main.swift calls nccdRun), and
// into a dynamic library that the development host loads, and loads again
// each time it is rebuilt (Dev/host.swift calls nccd_boot and nccd_teardown).

var live: App?

// The release build: make the application and run it.
public func nccdRun() {
    // An exception that nothing catches says what it was before the app goes down.
    NSSetUncaughtExceptionHandler { e in
        FileHandle.standardError.write(Data("NCCd: \(e.name.rawValue): \(e.reason ?? "")\n\(e.callStackSymbols.prefix(14).joined(separator: "\n"))\n".utf8))
    }
    let app = NSApplication.shared
    let delegate = App()
    live = delegate
    app.delegate = delegate
    app.setActivationPolicy(App.snapshotRun ? .accessory : .regular)
    app.run()
}

// The development host: the application is already running; put this build's app in it.
@_cdecl("nccd_boot")
public func nccdBoot() {
    let delegate = App()
    live = delegate
    NSApp.delegate = delegate
    delegate.applicationDidFinishLaunching(Notification(name: NSApplication.didFinishLaunchingNotification))
}

// The development host: this build is being replaced; take its app out, remembering where it was.
@_cdecl("nccd_teardown")
public func nccdTeardown() {
    live?.teardown()
    live = nil
}
