// WizardController.swift — owns the window + all state (focused week, the
// events, the auth token). Drives:
//   • the sign-in screen (when there's no shared AC token / on a 401),
//   • the week view (paging, refresh),
//   • the add/edit/delete editor sheet for aesthetical events,
//   • a "Connect a Google calendar" action.
import AppKit

final class WizardController: NSWindowController, NSWindowDelegate, WeekViewDelegate {

    // ── state ────────────────────────────────────────────────────────
    private var api: CalAPI
    private let auth = Auth()
    // Live watch on the shared ~/.ac-token (suite-wide broadcast). Active for
    // the whole app lifetime so a sign-in/out in any AC app updates us live.
    private var sessionWatch: UUID?

    // The Sunday that begins the focused week.
    private var weekStart = WeekView.startOfWeek(for: Date())

    private var aestheticalEvents: [CalEvent] = []
    private var feedEvents: [FeedEvent] = []

    // ── UI ───────────────────────────────────────────────────────────
    private var backdrop: BackdropView!
    private var weekView: WeekView!
    private var titleLabel: NSTextField!
    private var statusLabel: NSTextField!
    private var prevButton: NSButton!
    private var nextButton: NSButton!
    private var todayButton: NSButton!
    private var refreshButton: NSButton!
    private var addButton: NSButton!
    private var connectButton: NSButton!
    private var modeButton: NSButton!

    // Display span: 7 = week (Sun→Sat), 1 = a single day.
    private var dayCount: Int = 7

    // Auth overlay (shown when unauthenticated).
    private var authOverlay: NSView?
    private var authStatusLabel: NSTextField?

    // Calendars panel (subscription link + connectors), live-updated by async loads.
    private var subLinkField: NSTextField?
    private var connectorsBox: NSView?
    private weak var calendarsWindow: NSWindow?

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

    // Called by the AppDelegate after the window shows. Ensures auth,
    // then loads the current week.
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
        cv.wantsLayer = true

        // Living video backdrop (Seedance loop of the mascot illy), drawn
        // behind everything at low opacity. Sits at the back of the view
        // stack; the week grid and toolbar render in front of it.
        backdrop = BackdropView(frame: cv.bounds)
        backdrop.autoresizingMask = [.width, .height]
        cv.addSubview(backdrop)

        let pad: CGFloat = 12
        let topH: CGFloat = 40

        // Toolbar row (top).
        titleLabel = NSTextField(labelWithString: weekRangeTitle())
        titleLabel.font = .systemFont(ofSize: 14, weight: .semibold)
        titleLabel.frame = NSRect(x: pad, y: 560 - topH + 8, width: 300, height: 22)
        titleLabel.autoresizingMask = [.maxXMargin, .minYMargin]
        cv.addSubview(titleLabel)

        prevButton = NSButton(title: "‹", target: self, action: #selector(prevWeek))
        nextButton = NSButton(title: "›", target: self, action: #selector(nextWeek))
        todayButton = NSButton(title: "Today", target: self, action: #selector(goToday))
        refreshButton = NSButton(title: "Refresh", target: self, action: #selector(refreshAction))
        addButton = NSButton(title: "+ Event", target: self, action: #selector(addEventAction))
        connectButton = NSButton(title: "Calendars…", target: self, action: #selector(calendarsPanelAction))
        modeButton = NSButton(title: dayCount == 1 ? "Week" : "Day",
                              target: self, action: #selector(toggleModeAction))

        let buttons = [prevButton!, nextButton!, todayButton!, modeButton!, refreshButton!, addButton!, connectButton!]
        for b in buttons {
            b.bezelStyle = .rounded
            b.font = .systemFont(ofSize: 12)
            b.autoresizingMask = [.minXMargin, .minYMargin]
        }
        // Lay buttons out right-to-left across the top.
        var x: CGFloat = 840 - pad
        let widths: [(NSButton, CGFloat)] = [
            (connectButton, 96), (addButton, 78), (refreshButton, 76),
            (modeButton, 54), (todayButton, 58), (nextButton, 30), (prevButton, 30),
        ]
        for (b, w) in widths {
            x -= w
            b.frame = NSRect(x: x, y: 560 - topH + 6, width: w, height: 26)
            cv.addSubview(b)
            x -= 6
        }

        // Week view (fills the middle).
        weekView = WeekView(frame: NSRect(x: pad, y: pad + 22,
                                          width: 840 - pad * 2,
                                          height: 560 - topH - pad * 2 - 22))
        weekView.autoresizingMask = [.width, .height]
        weekView.weekStart = weekStart
        weekView.dayCount = dayCount
        weekView.delegate = self
        cv.addSubview(weekView)

        // Status line (bottom).
        statusLabel = NSTextField(labelWithString: "")
        statusLabel.font = .systemFont(ofSize: 11)
        statusLabel.textColor = .secondaryLabelColor
        statusLabel.frame = NSRect(x: pad, y: pad - 2, width: 840 - pad * 2, height: 18)
        statusLabel.autoresizingMask = [.width, .maxYMargin]
        statusLabel.lineBreakMode = .byTruncatingTail
        cv.addSubview(statusLabel)
    }

    private func weekRangeTitle() -> String {
        let cal = Calendar.current
        if dayCount == 1 {
            // Day mode: "Today / Tomorrow / Yesterday · Tue, Jun 16 2026".
            let fmt = DateFormatter(); fmt.dateFormat = "EEE, MMM d yyyy"
            let dayDelta = cal.dateComponents([.day],
                from: cal.startOfDay(for: Date()), to: weekStart).day ?? 0
            let word: String
            switch dayDelta {
            case 0: word = "Today · "
            case 1: word = "Tomorrow · "
            case -1: word = "Yesterday · "
            default: word = ""
            }
            return "\(word)\(fmt.string(from: weekStart))"
        }
        let end = cal.date(byAdding: .day, value: 6, to: weekStart) ?? weekStart
        let fmt = DateFormatter()
        fmt.dateFormat = "MMM d"
        let yfmt = DateFormatter(); yfmt.dateFormat = "yyyy"
        return "\(fmt.string(from: weekStart)) – \(fmt.string(from: end)), \(yfmt.string(from: end))"
    }

    // ── paging ─────────────────────────────────────────────────────────
    @objc private func prevWeek() { shiftWeek(by: -dayCount) }
    @objc private func nextWeek() { shiftWeek(by: dayCount) }
    @objc private func goToday() {
        weekStart = (dayCount == 1)
            ? Calendar.current.startOfDay(for: Date())
            : WeekView.startOfWeek(for: Date())
        weekView.weekStart = weekStart
        titleLabel.stringValue = weekRangeTitle()
        refresh()
    }

    // Day ⇄ Week. Keeps the currently-focused day in view across the switch.
    @objc private func toggleModeAction() {
        let cal = Calendar.current
        // The day to keep focused: today if it's in the current span, else the
        // span's first visible day.
        let now = Date()
        let focus = (weekStart <= now && now < weekBounds().to)
            ? now : weekStart
        if dayCount == 1 {
            dayCount = 7
            weekStart = WeekView.startOfWeek(for: focus)
        } else {
            dayCount = 1
            weekStart = cal.startOfDay(for: focus)
        }
        modeButton.title = (dayCount == 1) ? "Week" : "Day"
        weekView.dayCount = dayCount
        weekView.weekStart = weekStart
        titleLabel.stringValue = weekRangeTitle()
        refresh()
    }

    // Pre-set a single-day view BEFORE start() (used by the --tomorrow / --day
    // launch flag). Does NOT refresh — start() does the single initial load, so
    // we don't race a stale week fetch against the day fetch.
    func setDayMode(_ date: Date) {
        dayCount = 1
        weekStart = Calendar.current.startOfDay(for: date)
        modeButton?.title = "Week"
        weekView?.dayCount = dayCount
        weekView?.weekStart = weekStart
        titleLabel?.stringValue = weekRangeTitle()
    }

    private func shiftWeek(by days: Int) {
        weekStart = Calendar.current.date(byAdding: .day, value: days, to: weekStart) ?? weekStart
        weekView.weekStart = weekStart
        titleLabel.stringValue = weekRangeTitle()
        refresh()
    }

    @objc private func refreshAction() { refresh() }

    // The visible window's UTC bounds [weekStart, weekStart+7d).
    private func weekBounds() -> (from: Date, to: Date) {
        let to = Calendar.current.date(byAdding: .day, value: dayCount, to: weekStart) ?? weekStart
        return (weekStart, to)
    }

    // ── data load ────────────────────────────────────────────────────
    func refresh() {
        guard let token = auth.currentToken() else { showAuthScreen(); return }
        api.setToken(token)
        let (from, to) = weekBounds()
        statusLabel.stringValue = "Loading…"

        api.fetchEvents(from: from, to: to) { [weak self] result in
            guard let self else { return }
            switch result {
            case .success(let events):
                self.aestheticalEvents = events
                self.rebuildLaidEvents()
                self.statusLabel.stringValue =
                    "\(events.count) aesthetical event\(events.count == 1 ? "" : "s")"
                // Now layer the read-only feed overlay.
                self.loadFeedEvents()
            case .failure(let err):
                if case .unauthorized = err {
                    self.handleUnauthorized()
                } else {
                    self.statusLabel.stringValue = err.localizedDescription
                }
            }
        }
    }

    private func loadFeedEvents() {
        let (from, to) = weekBounds()
        api.fetchFeedEvents(from: from, to: to) { [weak self] result in
            guard let self else { return }
            switch result {
            case .success(let events):
                self.feedEvents = events
                self.rebuildLaidEvents()
                if !events.isEmpty {
                    let base = self.aestheticalEvents.count
                    self.statusLabel.stringValue =
                        "\(base) aesthetical · \(events.count) from connected calendars"
                }
            case .failure(let err):
                if case .unauthorized = err { self.handleUnauthorized() }
                // Feed errors are non-fatal — keep showing aesthetical events.
            }
        }
    }

    // Merge the two sources into the laid-out events the WeekView draws.
    private func rebuildLaidEvents() {
        var laid: [LaidEvent] = []

        for e in aestheticalEvents {
            guard let s = Self.parseISO(e.start), let en = Self.parseISO(e.end) else { continue }
            laid.append(LaidEvent(
                kind: .aesthetical, uid: e.uid, title: e.title,
                start: s, end: en, allDay: e.allDay ?? false,
                note: e.note ?? "", visibility: e.visibility ?? "private",
                sourceLabel: nil, color: nil))
        }
        for e in feedEvents {
            guard let s = Self.parseISO(e.start) else { continue }
            let en = e.end.flatMap(Self.parseISO) ?? s.addingTimeInterval(1800)
            laid.append(LaidEvent(
                kind: .feed, uid: e.uid, title: e.title,
                start: s, end: en, allDay: e.allDay ?? false,
                note: e.note ?? "", visibility: "private",
                sourceLabel: e.label, color: Self.color(from: e.color)))
        }
        weekView.events = laid
    }

    // ── editor ───────────────────────────────────────────────────────
    @objc private func addEventAction() {
        // Default the new event to the next whole hour today (or in the
        // focused week if it isn't the current week).
        let cal = Calendar.current
        let now = Date()
        let base = (weekStart <= now && now < weekBounds().to) ? now : weekStart
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

    // ── calendars panel: subscription link + connectors ──────────────
    @objc private func calendarsPanelAction() {
        guard let window else { return }
        let W: CGFloat = 470
        let container = NSView(frame: NSRect(x: 0, y: 0, width: W, height: 248))

        let linkTitle = NSTextField(labelWithString: "Your AesthetiCal subscription link")
        linkTitle.font = .systemFont(ofSize: 12, weight: .semibold)
        linkTitle.frame = NSRect(x: 0, y: 226, width: W, height: 18)
        container.addSubview(linkTitle)

        let hint = NSTextField(labelWithString: "Subscribe in Apple/Google Calendar. Full calendar (private incl.) — keep it secret.")
        hint.font = .systemFont(ofSize: 10); hint.textColor = .secondaryLabelColor
        hint.frame = NSRect(x: 0, y: 210, width: W, height: 14)
        container.addSubview(hint)

        let field = NSTextField(frame: NSRect(x: 0, y: 182, width: 352, height: 22))
        field.isEditable = false; field.isSelectable = true
        field.font = .monospacedSystemFont(ofSize: 10, weight: .regular)
        field.stringValue = "Loading…"
        container.addSubview(field)
        self.subLinkField = field

        let copyBtn = NSButton(title: "Copy", target: self, action: #selector(copySubLink))
        copyBtn.bezelStyle = .rounded
        copyBtn.frame = NSRect(x: 356, y: 181, width: 48, height: 24)
        container.addSubview(copyBtn)
        let rotateBtn = NSButton(title: "Rotate", target: self, action: #selector(rotateSubLink))
        rotateBtn.bezelStyle = .rounded
        rotateBtn.frame = NSRect(x: 406, y: 181, width: 60, height: 24)
        container.addSubview(rotateBtn)

        let sep = NSBox(frame: NSRect(x: 0, y: 166, width: W, height: 1))
        sep.boxType = .separator
        container.addSubview(sep)

        let connTitle = NSTextField(labelWithString: "Connected calendars")
        connTitle.font = .systemFont(ofSize: 12, weight: .semibold)
        connTitle.frame = NSRect(x: 0, y: 142, width: 300, height: 18)
        container.addSubview(connTitle)

        let box = NSView(frame: NSRect(x: 0, y: 0, width: W, height: 134))
        container.addSubview(box)
        self.connectorsBox = box

        let alert = NSAlert()
        alert.messageText = "Calendars"
        alert.accessoryView = container
        alert.addButton(withTitle: "Done")
        alert.addButton(withTitle: "Connect a Calendar…")
        alert.beginSheetModal(for: window) { [weak self] resp in
            guard let self else { return }
            self.subLinkField = nil
            self.connectorsBox = nil
            // "Connect a Calendar…" — present its sheet now that this one is gone.
            if resp == .alertSecondButtonReturn { self.connectFeedAction() }
        }

        loadSubLink()
        reloadConnectors()
    }

    private func loadSubLink() {
        api.fetchSubscriptionLink { [weak self] r in
            guard let self else { return }
            switch r {
            case .success(let link):
                self.subLinkField?.stringValue = link.url ?? "(no link — handle not set)"
            case .failure(let e):
                if case .unauthorized = e { self.handleUnauthorized() }
                self.subLinkField?.stringValue = "Error: \(e.localizedDescription)"
            }
        }
    }

    @objc private func copySubLink() {
        guard let s = subLinkField?.stringValue, s.hasPrefix("http") else { return }
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(s, forType: .string)
        statusLabel.stringValue = "Subscription link copied."
    }

    @objc private func rotateSubLink() {
        subLinkField?.stringValue = "Rotating…"
        api.rotateSubscriptionLink { [weak self] r in
            guard let self else { return }
            switch r {
            case .success(let link):
                self.subLinkField?.stringValue = link.url ?? "(no link)"
                self.statusLabel.stringValue = "Subscription link rotated — old links revoked."
            case .failure(let e):
                if case .unauthorized = e { self.handleUnauthorized() }
                self.subLinkField?.stringValue = "Error: \(e.localizedDescription)"
            }
        }
    }

    private func reloadConnectors() {
        api.fetchFeeds { [weak self] r in
            guard let self, let box = self.connectorsBox else { return }
            box.subviews.forEach { $0.removeFromSuperview() }
            switch r {
            case .success(let feeds):
                if feeds.isEmpty {
                    let l = NSTextField(labelWithString: "No calendars connected yet — use “Connect a Calendar…”.")
                    l.font = .systemFont(ofSize: 11); l.textColor = .secondaryLabelColor
                    l.frame = NSRect(x: 0, y: box.bounds.height - 22, width: box.bounds.width, height: 18)
                    box.addSubview(l)
                    return
                }
                var y = box.bounds.height - 26
                for feed in feeds {
                    let name = NSTextField(labelWithString: feed.label ?? "Calendar")
                    name.font = .systemFont(ofSize: 11)
                    name.frame = NSRect(x: 0, y: y + 3, width: 340, height: 18)
                    box.addSubview(name)
                    let rm = NSButton(title: "Remove", target: self,
                                      action: #selector(self.removeConnector(_:)))
                    rm.bezelStyle = .rounded
                    rm.frame = NSRect(x: box.bounds.width - 84, y: y, width: 84, height: 24)
                    rm.identifier = NSUserInterfaceItemIdentifier(feed.id)
                    box.addSubview(rm)
                    y -= 30
                }
            case .failure(let e):
                if case .unauthorized = e { self.handleUnauthorized() }
            }
        }
    }

    @objc private func removeConnector(_ sender: NSButton) {
        guard let id = sender.identifier?.rawValue else { return }
        sender.isEnabled = false
        api.removeFeed(id: id) { [weak self] r in
            guard let self else { return }
            switch r {
            case .success:
                self.reloadConnectors()
                self.refresh()
            case .failure(let e):
                sender.isEnabled = true
                if case .unauthorized = e { self.handleUnauthorized() }
            }
        }
    }

    // ── connect a Google calendar ────────────────────────────────────
    @objc private func connectFeedAction() {
        guard let window else { return }
        let alert = NSAlert()
        alert.messageText = "Connect a Google calendar"
        alert.informativeText = "Paste the Google \"secret address in iCal format\" URL and a label.\nGoogle Calendar → Settings → your calendar → Integrate calendar → Secret address in iCal format."
        alert.addButton(withTitle: "Connect")
        alert.addButton(withTitle: "Cancel")

        let stack = NSView(frame: NSRect(x: 0, y: 0, width: 320, height: 54))
        let urlField = NSTextField(frame: NSRect(x: 0, y: 28, width: 320, height: 22))
        urlField.placeholderString = "https://calendar.google.com/calendar/ical/.../basic.ics"
        let labelField = NSTextField(frame: NSRect(x: 0, y: 0, width: 320, height: 22))
        labelField.placeholderString = "Label (e.g. Work)"
        stack.addSubview(urlField)
        stack.addSubview(labelField)
        alert.accessoryView = stack

        alert.beginSheetModal(for: window) { [weak self] resp in
            guard let self, resp == .alertFirstButtonReturn else { return }
            let url = urlField.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
            let label = labelField.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
            guard !url.isEmpty else { return }
            self.statusLabel.stringValue = "Connecting calendar…"
            self.api.connectFeed(url: url, label: label.isEmpty ? "Calendar" : label) { r in
                switch r {
                case .success(let feed):
                    self.statusLabel.stringValue = "Connected \(feed.label ?? "calendar")."
                    self.refresh()
                case .failure(let err):
                    if case .unauthorized = err { self.handleUnauthorized() }
                    else { self.statusLabel.stringValue = err.localizedDescription }
                }
            }
        }
    }

    // ── WeekViewDelegate ─────────────────────────────────────────────
    func weekView(_ view: WeekView, didSelectEditable event: LaidEvent) {
        presentEditor(uid: event.uid, title: event.title,
                      start: event.start, end: event.end,
                      note: event.note, visibility: event.visibility)
    }

    func weekView(_ view: WeekView, didSelectReadOnly event: LaidEvent) {
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

    func weekView(_ view: WeekView, didRequestNewEventAt day: Date, hour: Int) {
        let cal = Calendar.current
        var comps = cal.dateComponents([.year, .month, .day], from: day)
        comps.hour = max(0, min(23, hour))
        comps.minute = 0
        let start = cal.date(from: comps) ?? day
        presentEditor(uid: nil, title: "", start: start,
                      end: start.addingTimeInterval(3600), note: "", visibility: "private")
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
        // Translucent scrim so the living mascot backdrop shows through
        // behind the sign-in prompt.
        overlay.layer?.backgroundColor = NSColor.windowBackgroundColor
            .withAlphaComponent(0.62).cgColor
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

    static func parseISO(_ s: String) -> Date? {
        isoFractional.date(from: s) ?? isoPlain.date(from: s)
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
    func windowWillClose(_ notification: Notification) {
        if let id = sessionWatch { auth.stopWatching(id); sessionWatch = nil }
    }
}
