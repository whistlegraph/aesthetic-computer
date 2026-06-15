// WizardController.swift — owns the window + all state (focused week, the
// events, the auth token). Drives:
//   • the device-pair auth screen (when there's no token / on a 401),
//   • the week view (paging, refresh),
//   • the add/edit/delete editor sheet for aesthetical events,
//   • a "Connect a Google calendar" action.
import AppKit

final class WizardController: NSWindowController, NSWindowDelegate, WeekViewDelegate {

    // ── state ────────────────────────────────────────────────────────
    private var creds: AuthCreds?
    private var api: CalAPI
    private let auth = Auth()

    // The Sunday that begins the focused week.
    private var weekStart = WeekView.startOfWeek(for: Date())

    private var aestheticalEvents: [CalEvent] = []
    private var feedEvents: [FeedEvent] = []

    // ── UI ───────────────────────────────────────────────────────────
    private var weekView: WeekView!
    private var titleLabel: NSTextField!
    private var statusLabel: NSTextField!
    private var prevButton: NSButton!
    private var nextButton: NSButton!
    private var todayButton: NSButton!
    private var refreshButton: NSButton!
    private var addButton: NSButton!
    private var connectButton: NSButton!

    // Auth overlay (shown when unauthenticated).
    private var authOverlay: NSView?
    private var codeLabel: NSTextField?
    private var authStatusLabel: NSTextField?

    private var editor: EventEditor?

    // ── lifecycle ────────────────────────────────────────────────────
    init() {
        self.creds = Auth.loadCreds()
        self.api = CalAPI(token: creds?.token)
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
        if creds?.token == nil || creds?.token.isEmpty == true {
            showAuthScreen()
        } else {
            hideAuthScreen()
            refresh()
        }
    }

    // ── layout ───────────────────────────────────────────────────────
    private func setupUI() {
        guard let cv = window?.contentView else { return }
        cv.wantsLayer = true

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
        connectButton = NSButton(title: "Connect Google…", target: self, action: #selector(connectFeedAction))

        let buttons = [prevButton!, nextButton!, todayButton!, refreshButton!, addButton!, connectButton!]
        for b in buttons {
            b.bezelStyle = .rounded
            b.font = .systemFont(ofSize: 12)
            b.autoresizingMask = [.minXMargin, .minYMargin]
        }
        // Lay buttons out right-to-left across the top.
        var x: CGFloat = 840 - pad
        let widths: [(NSButton, CGFloat)] = [
            (connectButton, 130), (addButton, 78), (refreshButton, 76),
            (todayButton, 58), (nextButton, 30), (prevButton, 30),
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
        let end = cal.date(byAdding: .day, value: 6, to: weekStart) ?? weekStart
        let fmt = DateFormatter()
        fmt.dateFormat = "MMM d"
        let yfmt = DateFormatter(); yfmt.dateFormat = "yyyy"
        return "\(fmt.string(from: weekStart)) – \(fmt.string(from: end)), \(yfmt.string(from: end))"
    }

    // ── week paging ──────────────────────────────────────────────────
    @objc private func prevWeek() { shiftWeek(by: -7) }
    @objc private func nextWeek() { shiftWeek(by: 7) }
    @objc private func goToday() {
        weekStart = WeekView.startOfWeek(for: Date())
        weekView.weekStart = weekStart
        titleLabel.stringValue = weekRangeTitle()
        refresh()
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
        let to = Calendar.current.date(byAdding: .day, value: 7, to: weekStart) ?? weekStart
        return (weekStart, to)
    }

    // ── data load ────────────────────────────────────────────────────
    func refresh() {
        guard creds?.token != nil else { showAuthScreen(); return }
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

    // ── auth screen ──────────────────────────────────────────────────
    private func handleUnauthorized() {
        // Token expired or invalid — clear it and re-pair.
        Auth.clearCreds()
        creds = nil
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

        let title = NSTextField(labelWithString: "Link DateWizard to your account")
        title.font = .systemFont(ofSize: 18, weight: .semibold)
        title.alignment = .center
        title.frame = NSRect(x: 0, y: cv.bounds.height - 140, width: cv.bounds.width, height: 28)
        title.autoresizingMask = [.width, .minYMargin]
        overlay.addSubview(title)

        let instr = NSTextField(labelWithString:
            "On prompt.ac (logged in), type:")
        instr.alignment = .center
        instr.font = .systemFont(ofSize: 13)
        instr.textColor = .secondaryLabelColor
        instr.frame = NSRect(x: 0, y: cv.bounds.height - 180, width: cv.bounds.width, height: 22)
        instr.autoresizingMask = [.width, .minYMargin]
        overlay.addSubview(instr)

        let code = NSTextField(labelWithString: "…")
        code.font = .monospacedSystemFont(ofSize: 40, weight: .bold)
        code.alignment = .center
        code.textColor = .controlAccentColor
        code.frame = NSRect(x: 0, y: cv.bounds.height - 250, width: cv.bounds.width, height: 56)
        code.autoresizingMask = [.width, .minYMargin]
        overlay.addSubview(code)
        self.codeLabel = code

        let openBtn = NSButton(title: "Open prompt.ac", target: self, action: #selector(openPromptAction))
        openBtn.bezelStyle = .rounded
        openBtn.frame = NSRect(x: cv.bounds.width / 2 - 70, y: cv.bounds.height - 300, width: 140, height: 28)
        openBtn.autoresizingMask = [.minXMargin, .maxXMargin, .minYMargin]
        overlay.addSubview(openBtn)

        let status = NSTextField(labelWithString: "Requesting a link code…")
        status.alignment = .center
        status.font = .systemFont(ofSize: 12)
        status.textColor = .tertiaryLabelColor
        status.frame = NSRect(x: 0, y: cv.bounds.height - 340, width: cv.bounds.width, height: 20)
        status.autoresizingMask = [.width, .minYMargin]
        overlay.addSubview(status)
        self.authStatusLabel = status

        cv.addSubview(overlay)
        self.authOverlay = overlay

        beginPairing()
    }

    private func hideAuthScreen() {
        auth.stopPolling()
        authOverlay?.removeFromSuperview()
        authOverlay = nil
        codeLabel = nil
        authStatusLabel = nil
    }

    private func beginPairing() {
        auth.createCode { [weak self] result in
            guard let self else { return }
            switch result {
            case .success(let code):
                self.codeLabel?.stringValue = "link \(code)"
                self.authStatusLabel?.stringValue = "Waiting for you to claim this code on prompt.ac…"
                self.auth.startPolling(code: code) { [weak self] creds in
                    guard let self else { return }
                    self.creds = creds
                    self.api.setToken(creds.token)
                    let who = creds.handle.map { "@\($0)" } ?? "your account"
                    self.authStatusLabel?.stringValue = "Linked as \(who)."
                    self.hideAuthScreen()
                    self.refresh()
                }
            case .failure(let err):
                self.authStatusLabel?.stringValue = "Couldn't get a code: \(err.localizedDescription)"
            }
        }
    }

    @objc private func openPromptAction() { Auth.openPromptAC() }

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
        auth.stopPolling()
    }
}
