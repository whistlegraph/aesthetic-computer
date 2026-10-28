// WizardController.swift — owns the window + all state (the events, the auth
// token). Drives:
//   • the sign-in screen (when there's no shared AC token / on a 401),
//   • the rolling upcoming agenda,
//   • the add/edit/delete editor sheet for aesthetical events.
import AppKit

final class WizardController: NSWindowController, NSWindowDelegate, AgendaViewDelegate {

    // ── state ────────────────────────────────────────────────────────
    private var api: CalAPI
    private let auth = Auth()

    // How far forward the agenda and the menu-bar next-appointment look. A
    // rolling window from the start of today.
    static let upcomingDays = 14
    // Live watch on the shared ~/.ac-token (suite-wide broadcast). Active for
    // the whole app lifetime so a sign-in/out in any AC app updates us live.
    private var sessionWatch: UUID?

    // The rolling upcoming set (both sources) that feeds the agenda list and
    // the menu bar.
    private var upcomingAesthetical: [CalEvent] = []
    private var upcomingFeed: [FeedEvent] = []
    // Periodic reload so the menu bar and agenda stay honest while the window
    // is closed.
    private var upcomingTimer: Timer?
    // The last merged+capped upcoming set, kept so the next appointment can be
    // re-picked against the wall clock between network reloads.
    private var cachedUpcoming: [LaidEvent] = []
    // Fast, network-free tick that re-selects the next appointment so the menu
    // bar rolls off a meeting the moment it starts.
    private var nextTicker: Timer?
    // No timed event is treated as "ongoing" past this many seconds — a cap so a
    // missing/garbled end time can't keep an appointment alive in the agenda (and
    // masking the ones behind it) indefinitely.
    static let maxTimedDuration: TimeInterval = 12 * 3600

    // Broadcast the start of the next upcoming appointment (nil = none ahead).
    // Wired by AppDelegate to the menu bar.
    var onNextEventChanged: ((Date?, String?) -> Void)?

    // ── UI ───────────────────────────────────────────────────────────
    private var agendaScroll: NSScrollView!
    private var agendaView: AgendaView!
    private var titleLabel: NSTextField!
    private var statusLabel: NSTextField!
    private var addButton: NSButton!

    // Auth overlay (shown when unauthenticated).
    private var authOverlay: NSView?
    private var authStatusLabel: NSTextField?

    private var editor: EventEditor?

    // ── lifecycle ────────────────────────────────────────────────────
    init() {
        let auth = Auth()
        self.api = CalAPI(token: auth.currentToken())
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 840, height: 560),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered, defer: false)
        window.title = "DateWizard"
        window.minSize = NSSize(width: 640, height: 420)
        window.center()
        super.init(window: window)
        window.delegate = self
        setupUI()
    }

    required init?(coder: NSCoder) { fatalError() }

    // Called by the AppDelegate after launch. Ensures auth, then loads.
    func start() {
        // Live suite-wide session watch: any sign-in/out (this app, another
        // wizard, or the AC Electron tray) updates us without a restart.
        if sessionWatch == nil {
            sessionWatch = auth.startWatching { [weak self] in self?.handleSessionChange() }
        }
        if let token = auth.currentToken() {
            api.setToken(token)
            hideAuthScreen()
            refresh()
        } else {
            showAuthScreen()
        }
        startUpcomingTimer()
    }

    // Reload the rolling upcoming set every few minutes so the agenda and the
    // menu bar stay current even when the window is closed.
    private func startUpcomingTimer() {
        guard upcomingTimer == nil else { return }
        let timer = Timer(timeInterval: 300, repeats: true) { [weak self] _ in
            self?.loadUpcoming()
        }
        RunLoop.main.add(timer, forMode: .common)
        upcomingTimer = timer

        // Cheap local re-selection every 20s: advances to the next meeting as
        // soon as the current one starts, with no network round-trip.
        guard nextTicker == nil else { return }
        let tick = Timer(timeInterval: 20, repeats: true) { [weak self] _ in
            self?.publishNextEvent()
        }
        RunLoop.main.add(tick, forMode: .common)
        nextTicker = tick
    }

    // Reacts to a change in the shared ~/.ac-token (broadcast).
    private func handleSessionChange() {
        let who = auth.handle ?? "—"
        NSLog("[ac-session] broadcast: token=\(auth.currentToken() != nil ? "present" : "none") handle=\(who)")
        if let token = auth.currentToken() {
            api.setToken(token)
            if authOverlay != nil {
                authStatusLabel?.stringValue = auth.handle.map { "Signed in as \($0)." } ?? "Signed in."
                hideAuthScreen()
            }
            refresh()
        } else {
            // Signed out / expired elsewhere — drop to the sign-in screen.
            api.setToken(nil)
            showAuthScreen()
        }
    }

    // ── layout ───────────────────────────────────────────────────────
    private func setupUI() {
        guard let cv = window?.contentView else { return }

        let pad: CGFloat = 12
        let topH: CGFloat = 40

        titleLabel = NSTextField(labelWithString: "Upcoming")
        titleLabel.font = .systemFont(ofSize: 17, weight: .bold)
        titleLabel.frame = NSRect(x: pad, y: 560 - topH + 4, width: 245, height: 28)
        titleLabel.autoresizingMask = [.maxXMargin, .minYMargin]
        cv.addSubview(titleLabel)

        addButton = NSButton(title: "+ Event", target: self, action: #selector(addEventAction))
        addButton.bezelStyle = .rounded
        addButton.font = .systemFont(ofSize: 12)
        addButton.autoresizingMask = [.minXMargin, .minYMargin]
        addButton.frame = NSRect(x: 840 - pad - 78, y: 560 - topH + 6, width: 78, height: 26)
        cv.addSubview(addButton)

        let bodyRect = NSRect(x: pad, y: pad + 22,
                              width: 840 - pad * 2,
                              height: 560 - topH - pad * 2 - 22)
        agendaScroll = NSScrollView(frame: bodyRect)
        agendaScroll.autoresizingMask = [.width, .height]
        agendaScroll.drawsBackground = false
        agendaScroll.hasVerticalScroller = true
        agendaScroll.hasHorizontalScroller = false
        agendaScroll.autohidesScrollers = true
        agendaScroll.verticalScrollElasticity = .allowed
        agendaView = AgendaView(frame: NSRect(x: 0, y: 0,
                                              width: bodyRect.width, height: bodyRect.height))
        agendaView.delegate = self
        agendaScroll.documentView = agendaView
        cv.addSubview(agendaScroll)

        statusLabel = NSTextField(labelWithString: "")
        statusLabel.font = .systemFont(ofSize: 11)
        statusLabel.textColor = .secondaryLabelColor
        statusLabel.frame = NSRect(x: pad, y: pad - 2, width: 840 - pad * 2, height: 18)
        statusLabel.autoresizingMask = [.width, .maxYMargin]
        statusLabel.lineBreakMode = .byTruncatingTail
        cv.addSubview(statusLabel)
    }

    func revealUpcoming() {
        showWindow(nil)
        NSApp.activate(ignoringOtherApps: true)
        refresh()
    }

    // Menu-bar left-click: toggle. If the window is already up and focused, a
    // second tap hides it; otherwise reveal it (this also covers the
    // buried-window case, where the tap brings it forward instead).
    func toggleUpcoming() {
        if let w = window, w.isVisible, NSApp.isActive, w.isKeyWindow {
            w.orderOut(nil)
        } else {
            revealUpcoming()
        }
    }

    // ── data load ────────────────────────────────────────────────────
    func refresh() {
        guard let token = auth.currentToken() else { showAuthScreen(); return }
        api.setToken(token)
        loadUpcoming()
    }

    // Rolling window [start of today, +upcomingDays). Feeds the agenda list and
    // the menu bar.
    private func loadUpcoming() {
        guard let token = auth.currentToken() else { return }
        api.setToken(token)
        let cal = Calendar.current
        let from = cal.startOfDay(for: Date())
        let to = cal.date(byAdding: .day, value: Self.upcomingDays, to: from) ?? from

        api.fetchEvents(from: from, to: to) { [weak self] result in
            guard let self else { return }
            switch result {
            case .success(let events):
                self.upcomingAesthetical = events
                self.rebuildUpcoming()
            case .failure(let err):
                if case .unauthorized = err { self.handleUnauthorized() }
            }
        }
        api.fetchFeedEvents(from: from, to: to) { [weak self] result in
            guard let self else { return }
            if case .success(let events) = result {
                self.upcomingFeed = events
                self.rebuildUpcoming()
            }
            // Feed errors are non-fatal — keep the aesthetical agenda.
        }
    }

    // Merge + filter + sort the upcoming sources into the agenda list, and
    // publish the next appointment for the menu bar.
    private func rebuildUpcoming() {
        let now = Date()
        var laid: [LaidEvent] = []
        for e in upcomingAesthetical {
            let allDay = e.allDay ?? false
            guard let s = Self.parseBoundary(e.start, allDay: allDay),
                  let en = Self.parseBoundary(e.end, allDay: allDay) else { continue }
            laid.append(LaidEvent(
                kind: .aesthetical, uid: e.uid, title: e.title,
                start: s, end: Self.cappedEnd(start: s, end: en, allDay: allDay), allDay: allDay,
                note: e.note ?? "", visibility: e.visibility ?? "private",
                sourceLabel: nil, color: nil))
        }
        for e in upcomingFeed {
            let allDay = e.allDay ?? false
            guard let s = Self.parseBoundary(e.start, allDay: allDay) else { continue }
            let en = e.end.flatMap { Self.parseBoundary($0, allDay: allDay) } ?? s.addingTimeInterval(1800)
            laid.append(LaidEvent(
                kind: .feed, uid: e.uid, title: e.title,
                start: s, end: Self.cappedEnd(start: s, end: en, allDay: allDay), allDay: allDay,
                note: e.note ?? "", visibility: "private",
                sourceLabel: e.label, color: Self.color(from: e.color)))
        }
        // Collapse duplicates (same event via import + feed) before filtering,
        // then keep still-relevant events (ongoing or ahead), ordered by start.
        let upcoming = Self.deduped(laid).filter { $0.end >= now }.sorted { $0.start < $1.start }
        cachedUpcoming = upcoming
        agendaView?.events = upcoming

        let count = upcoming.count
        statusLabel.stringValue = count == 0
            ? "Nothing coming up in the next \(Self.upcomingDays) days."
            : "\(count) upcoming event\(count == 1 ? "" : "s") · next \(Self.upcomingDays) days"

        publishNextEvent()
    }

    // Clamp an event's end so a missing or garbled end time can't leave it
    // "ongoing" forever. All-day items keep their span; timed items are capped at
    // maxTimedDuration past their start.
    private static func cappedEnd(start: Date, end: Date, allDay: Bool) -> Date {
        if allDay { return max(end, start) }
        let ceiling = start.addingTimeInterval(maxTimedDuration)
        return min(max(end, start), ceiling)
    }

    // Pick the next appointment for the menu bar from the cached set: the
    // soonest timed event that hasn't started yet (all-day items are skipped so
    // there is always a clock time). Runs on the local ticker too, so it
    // advances the instant a meeting starts.
    private func publishNextEvent() {
        let now = Date()
        let next = cachedUpcoming.first { !$0.allDay && $0.start > now }
        onNextEventChanged?(next?.start, next?.title)
    }


    // Collapse events that represent the same underlying item — e.g. a Google
    // event that is both imported into AesthetiCal (uid "ics:<sourceUID>") and
    // shown live via a connected feed (uid "<sourceUID>"), or the same event
    // arriving from two feeds. Callers append editable aesthetical events before
    // feed events, so keeping the first occurrence prefers the editable copy.
    private static func deduped(_ events: [LaidEvent]) -> [LaidEvent] {
        var seen = Set<String>()
        var out: [LaidEvent] = []
        out.reserveCapacity(events.count)
        for e in events {
            if let key = dedupKey(e), !seen.insert(key).inserted { continue }
            out.append(e)
        }
        return out
    }

    // Normalized identity for duplicate detection. Strips the "ics:" import
    // prefix so an imported event and its feed twin share a key. Returns nil
    // when there's no stable id to match on (those events are always kept).
    private static func dedupKey(_ e: LaidEvent) -> String? {
        var uid = e.uid.trimmingCharacters(in: .whitespacesAndNewlines)
        if uid.hasPrefix("ics:") { uid = String(uid.dropFirst(4)) }
        return uid.isEmpty ? nil : uid
    }

    // ── editor ───────────────────────────────────────────────────────
    @objc private func addEventAction() {
        // Default the new event to the next whole hour.
        let cal = Calendar.current
        let base = Date()
        var comps = cal.dateComponents([.year, .month, .day, .hour], from: base)
        comps.hour = (comps.hour ?? 9) + 1
        comps.minute = 0
        let start = cal.date(from: comps) ?? base
        presentEditor(uid: nil, title: "", start: start,
                      end: start.addingTimeInterval(3600), note: "", visibility: "private")
    }

    private func presentEditor(uid: String?, title: String, start: Date, end: Date,
                               note: String, visibility: String) {
        guard let window else { return }
        let ed = EventEditor(uid: uid, title: title, start: start, end: end,
                             note: note, visibility: visibility) { [weak self] result in
            self?.handleEditorResult(result)
        }
        self.editor = ed
        ed.present(on: window)
    }

    private func handleEditorResult(_ result: EventEditorResult) {
        switch result {
        case .cancel:
            break
        case .save(let uid, let title, let start, let end, let note, let visibility):
            let startISO = CalAPI.iso(start)
            let endISO = CalAPI.iso(end)
            statusLabel.stringValue = "Saving…"
            let done: (Result<CalEvent, CalAPIError>) -> Void = { [weak self] r in
                guard let self else { return }
                switch r {
                case .success: self.refresh()
                case .failure(let err):
                    if case .unauthorized = err { self.handleUnauthorized() }
                    else { self.statusLabel.stringValue = err.localizedDescription }
                }
            }
            if let uid {
                api.updateEvent(uid: uid, title: title, start: startISO, end: endISO,
                                allDay: false, note: note, visibility: visibility, completion: done)
            } else {
                api.createEvent(title: title, start: startISO, end: endISO,
                                allDay: false, note: note, visibility: visibility, completion: done)
            }
        case .delete(let uid):
            statusLabel.stringValue = "Deleting…"
            api.deleteEvent(uid: uid) { [weak self] r in
                guard let self else { return }
                switch r {
                case .success: self.refresh()
                case .failure(let err):
                    if case .unauthorized = err { self.handleUnauthorized() }
                    else { self.statusLabel.stringValue = err.localizedDescription }
                }
            }
        }
    }


    private func presentReadOnlyDetails(_ event: LaidEvent) {
        // Read-only details — no edit.
        let alert = NSAlert()
        alert.messageText = event.title
        let fmt = DateFormatter()
        fmt.dateStyle = .medium; fmt.timeStyle = .short
        var info = "\(fmt.string(from: event.start)) – \(fmt.string(from: event.end))"
        if let src = event.sourceLabel { info += "\n\nSource: \(src) (read-only)" }
        if !event.note.isEmpty { info += "\n\n\(event.note)" }
        alert.informativeText = info
        alert.addButton(withTitle: "OK")
        if let window { alert.beginSheetModal(for: window) { _ in } }
        else { alert.runModal() }
    }

    // ── AgendaViewDelegate ───────────────────────────────────────────
    // Aesthetical → editor, feed → read-only details.
    func agendaView(_ view: AgendaView, didSelectEditable event: LaidEvent) {
        presentEditor(uid: event.uid, title: event.title,
                      start: event.start, end: event.end,
                      note: event.note, visibility: event.visibility)
    }

    func agendaView(_ view: AgendaView, didSelectReadOnly event: LaidEvent) {
        presentReadOnlyDetails(event)
    }

    // ── sign-in screen ───────────────────────────────────────────────
    private func handleUnauthorized() {
        // Token missing/expired — drop it and ask the user to sign in.
        api.setToken(nil)
        showAuthScreen()
    }

    private func showAuthScreen() {
        guard let cv = window?.contentView else { return }
        if authOverlay != nil { return }

        let overlay = NSView(frame: cv.bounds)
        overlay.wantsLayer = true
        overlay.layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor
        overlay.autoresizingMask = [.width, .height]

        let title = NSTextField(labelWithString: "Sign in to Aesthetic Computer")
        title.font = .systemFont(ofSize: 18, weight: .semibold)
        title.alignment = .center
        title.frame = NSRect(x: 0, y: cv.bounds.height - 150, width: cv.bounds.width, height: 28)
        title.autoresizingMask = [.width, .minYMargin]
        overlay.addSubview(title)

        let instr = NSTextField(labelWithString: "Sign in with your browser — no terminal needed")
        instr.alignment = .center
        instr.font = .systemFont(ofSize: 13)
        instr.textColor = .secondaryLabelColor
        instr.frame = NSRect(x: 0, y: cv.bounds.height - 190, width: cv.bounds.width, height: 22)
        instr.autoresizingMask = [.width, .minYMargin]
        overlay.addSubview(instr)

        let signInBtn = NSButton(title: "Sign in",
                                 target: self, action: #selector(signInAction))
        signInBtn.bezelStyle = .rounded
        signInBtn.keyEquivalent = "\r"
        signInBtn.frame = NSRect(x: cv.bounds.width / 2 - 110, y: cv.bounds.height - 240,
                                 width: 220, height: 30)
        signInBtn.autoresizingMask = [.minXMargin, .maxXMargin, .minYMargin]
        overlay.addSubview(signInBtn)

        let cliBtn = NSButton(title: "Use ac-login CLI instead",
                              target: self, action: #selector(runAcLoginAction))
        cliBtn.bezelStyle = .inline
        cliBtn.isBordered = false
        cliBtn.contentTintColor = .tertiaryLabelColor
        cliBtn.font = .systemFont(ofSize: 11)
        cliBtn.frame = NSRect(x: cv.bounds.width / 2 - 110, y: cv.bounds.height - 274,
                              width: 220, height: 22)
        cliBtn.autoresizingMask = [.minXMargin, .maxXMargin, .minYMargin]
        overlay.addSubview(cliBtn)

        let status = NSTextField(labelWithString: "")
        status.alignment = .center
        status.font = .systemFont(ofSize: 12)
        status.textColor = .tertiaryLabelColor
        status.frame = NSRect(x: 0, y: cv.bounds.height - 320, width: cv.bounds.width, height: 20)
        status.autoresizingMask = [.width, .minYMargin]
        overlay.addSubview(status)
        self.authStatusLabel = status

        cv.addSubview(overlay)
        self.authOverlay = overlay
        // No polling — the live session watch (installed in start()) proceeds
        // automatically the instant ~/.ac-token appears.
    }

    private func hideAuthScreen() {
        authOverlay?.removeFromSuperview()
        authOverlay = nil
        authStatusLabel = nil
    }

    @objc private func signInAction() {
        authStatusLabel?.stringValue = "Opening browser… finish signing in there."
        auth.signIn { [weak self] result in
            guard let self else { return }
            switch result {
            case .success:
                // The session file-watch (start()) will proceed automatically;
                // call directly too so we don't wait on the debounce.
                self.handleSessionChange()
            case .failure(let error):
                self.authStatusLabel?.stringValue = error.localizedDescription
            }
        }
    }

    @objc private func runAcLoginAction() {
        auth.runAcLogin()
        authStatusLabel?.stringValue = "Finish signing in your terminal — waiting for ~/.ac-token…"
    }

    // ── parsing helpers ──────────────────────────────────────────────
    private static let isoFractional: ISO8601DateFormatter = {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return f
    }()
    private static let isoPlain: ISO8601DateFormatter = {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime]
        return f
    }()
    // Date-only ("2026-07-16"), read in UTC — how some all-day events arrive.
    private static let isoDateOnly: DateFormatter = {
        let f = DateFormatter()
        f.calendar = Calendar(identifier: .gregorian)
        f.locale = Locale(identifier: "en_US_POSIX")
        f.timeZone = TimeZone(identifier: "UTC")
        f.dateFormat = "yyyy-MM-dd"
        return f
    }()
    // A UTC calendar for reading the calendar date out of an all-day instant.
    private static let utcCalendar: Calendar = {
        var c = Calendar(identifier: .gregorian)
        c.timeZone = TimeZone(identifier: "UTC")!
        return c
    }()

    static func parseISO(_ s: String) -> Date? {
        isoFractional.date(from: s) ?? isoPlain.date(from: s) ?? isoDateOnly.date(from: s)
    }

    // Parse an event boundary (start/end). Timed events keep their exact
    // instant; all-day events are *floating dates* — they arrive as UTC midnight
    // (e.g. "2026-07-16T00:00:00Z", or a bare "2026-07-16"), and converting that
    // instant into a zone behind UTC would push it into the previous evening,
    // rendering the event a full day early. So for all-day items we take the
    // calendar Y/M/D (read in UTC) and rebuild it as local start-of-day, keeping
    // July 16 on July 16 in every timezone.
    static func parseBoundary(_ s: String, allDay: Bool) -> Date? {
        guard let d = parseISO(s) else { return nil }
        guard allDay else { return d }
        let ymd = utcCalendar.dateComponents([.year, .month, .day], from: d)
        return Calendar.current.date(from: ymd) ?? Calendar.current.startOfDay(for: d)
    }

    // Parse a CSS-ish color string ("#rrggbb", "#rgb", or a few named
    // colors) into an NSColor. Falls back to a stable gray.
    static func color(from str: String?) -> NSColor {
        guard let raw = str?.trimmingCharacters(in: .whitespaces), !raw.isEmpty else {
            return .systemGray
        }
        let named: [String: NSColor] = [
            "red": .systemRed, "orange": .systemOrange, "yellow": .systemYellow,
            "green": .systemGreen, "blue": .systemBlue, "purple": .systemPurple,
            "pink": .systemPink, "teal": .systemTeal, "gray": .systemGray,
            "grey": .systemGray, "brown": .systemBrown, "indigo": .systemIndigo,
        ]
        if let c = named[raw.lowercased()] { return c }
        var hex = raw
        if hex.hasPrefix("#") { hex.removeFirst() }
        if hex.count == 3 {
            hex = hex.map { "\($0)\($0)" }.joined()
        }
        guard hex.count == 6, let v = UInt32(hex, radix: 16) else { return .systemGray }
        let r = CGFloat((v >> 16) & 0xff) / 255.0
        let g = CGFloat((v >> 8) & 0xff) / 255.0
        let b = CGFloat(v & 0xff) / 255.0
        return NSColor(calibratedRed: r, green: g, blue: b, alpha: 1)
    }

    // ── window lifecycle ─────────────────────────────────────────────
    // Always-on app: hide the window instead of closing it, so the shared
    // session watch (and live event state) survives. The menu bar item
    // keeps running; clicking it re-shows the window. Real teardown happens
    // on app terminate.
    func windowShouldClose(_ sender: NSWindow) -> Bool {
        window?.orderOut(nil)
        return false
    }

    func windowWillClose(_ notification: Notification) {
        if let id = sessionWatch { auth.stopWatching(id); sessionWatch = nil }
    }
}
