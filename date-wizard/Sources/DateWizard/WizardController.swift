// WizardController.swift — owns the window + all state (focused week, the
// events, the auth token). Drives:
//   • the sign-in screen (when there's no shared AC token / on a 401),
//   • the week view (paging, refresh),
//   • the add/edit/delete editor sheet for aesthetical events,
//   • a "Connect a Google calendar" action.
import AppKit

final class WizardController: NSWindowController, NSWindowDelegate, WeekViewDelegate, AgendaViewDelegate {

    // ── state ────────────────────────────────────────────────────────
    private var api: CalAPI
    private let auth = Auth()

    // How far forward the "no day selected" agenda + the menu-bar countdown
    // badge look. A rolling window from the start of today.
    static let upcomingDays = 14
    // Live watch on the shared ~/.ac-token (suite-wide broadcast). Active for
    // the whole app lifetime so a sign-in/out in any AC app updates us live.
    private var sessionWatch: UUID?

    // The Sunday that begins the focused week.
    private var weekStart = WeekView.startOfWeek(for: Date())

    private var aestheticalEvents: [CalEvent] = []
    private var feedEvents: [FeedEvent] = []

    // The rolling upcoming set (both sources, merged) that feeds the agenda list
    // and the menu-bar countdown badge. Independent of the focused day/week.
    private var upcomingAesthetical: [CalEvent] = []
    private var upcomingFeed: [FeedEvent] = []
    // Periodic reload so the badge + agenda stay honest even when the window is
    // closed / we're parked in day mode.
    private var upcomingTimer: Timer?

    // Broadcast the focused day (nil in week mode) so the menu bar strip can
    // light the same dot. Wired by the AppDelegate.
    var onFocusedDayChanged: ((Date?) -> Void)?

    // Broadcast the start of the next upcoming appointment (nil = none ahead) so
    // the menu bar can paint a countdown badge on the wand. Wired by AppDelegate.
    var onNextEventChanged: ((Date?) -> Void)?

    // ── UI ───────────────────────────────────────────────────────────
    private var backdrop: BackdropView!
    // Inner radial glow framing the window in the focused day's color (only
    // in single-day mode). A subtle edge highlight, not a full wash.
    private var dayGlow: DayGlowView!
    // In-window twin of the menu bar strip: seven ROYGBIV day dots.
    private var dayDots: DayDotsView!
    private var weekView: WeekView!
    // The ordered "no day selected" agenda list (rolling upcoming events),
    // shown in place of the grid whenever we're not focused on a single day.
    private var agendaScroll: NSScrollView!
    private var agendaView: AgendaView!
    private var titleLabel: NSTextField!
    private var statusLabel: NSTextField!
    private var addButton: NSButton!          // "+ Event" — the only toolbar button.
    // "‹ Upcoming" — jumps back from a single day to the agenda list. Hidden
    // while the agenda is already showing.
    private var upcomingButton: NSButton!
    // Vestigial: there's no mode-toggle button in the window anymore (mode is
    // driven by launch flags / CLI). Live code still harmlessly sets its title.
    private var modeButton: NSButton!

    // MARK: - Deprecated toolbar buttons
    // Paging, "today", refresh, and calendars were removed from the window;
    // their equivalents live in the wizard / ac-login CLI. Kept for reference
    // alongside the matching deprecated actions further down.
    @available(*, deprecated, message: "Removed from window UI — paging lives in the CLI.")
    private var prevButton: NSButton!
    @available(*, deprecated, message: "Removed from window UI — paging lives in the CLI.")
    private var nextButton: NSButton!
    @available(*, deprecated, message: "Removed from window UI — use the menu bar 'Go to Today'.")
    private var todayButton: NSButton!
    @available(*, deprecated, message: "Removed from window UI — refresh is automatic / CLI.")
    private var refreshButton: NSButton!
    @available(*, deprecated, message: "Removed from window UI — calendar setup lives in the CLI.")
    private var connectButton: NSButton!

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
        startUpcomingTimer()
    }

    // Reload the rolling upcoming set every few minutes so the agenda and the
    // menu-bar countdown badge stay current even when the window is closed.
    // (MenuBarDays owns the per-minute countdown tick; this refreshes the data
    // behind it — new/changed events, and the roll to the next appointment.)
    private func startUpcomingTimer() {
        guard upcomingTimer == nil else { return }
        let timer = Timer(timeInterval: 300, repeats: true) { [weak self] _ in
            self?.loadUpcoming()
        }
        RunLoop.main.add(timer, forMode: .common)
        upcomingTimer = timer
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

        // Toolbar row (top): title · day-dots · + Event. Week/day paging,
        // refresh, "today", and calendar management all live in the
        // ac-login / wizard CLI — the window stays deliberately minimal.
        titleLabel = NSTextField(labelWithString: weekRangeTitle())
        titleLabel.font = .systemFont(ofSize: 17, weight: .bold)
        titleLabel.frame = NSRect(x: pad, y: 560 - topH + 4, width: 245, height: 28)
        titleLabel.autoresizingMask = [.maxXMargin, .minYMargin]
        cv.addSubview(titleLabel)

        // Day-dots, to the right of the title — the in-window twin of the menu
        // bar strip. Click a dot to jump to that day of the week.
        dayDots = DayDotsView(frame: NSRect(x: pad + 252, y: 560 - topH + 7,
                                            width: 190, height: 26))
        dayDots.autoresizingMask = [.minYMargin]
        dayDots.onSelect = { [weak self] idx in self?.focusDayInCurrentWeek(idx) }
        cv.addSubview(dayDots)

        // + Event, far right.
        addButton = NSButton(title: "+ Event", target: self, action: #selector(addEventAction))
        addButton.bezelStyle = .rounded
        addButton.font = .systemFont(ofSize: 12)
        addButton.autoresizingMask = [.minXMargin, .minYMargin]
        addButton.frame = NSRect(x: 840 - pad - 78, y: 560 - topH + 6, width: 78, height: 26)
        cv.addSubview(addButton)

        // "‹ Upcoming" — return to the agenda list from a single-day view. Sits
        // just left of + Event; hidden while the agenda is already showing.
        upcomingButton = NSButton(title: "‹ Upcoming", target: self, action: #selector(showUpcomingAction))
        upcomingButton.bezelStyle = .rounded
        upcomingButton.font = .systemFont(ofSize: 12)
        upcomingButton.autoresizingMask = [.minXMargin, .minYMargin]
        upcomingButton.frame = NSRect(x: 840 - pad - 78 - 8 - 96, y: 560 - topH + 6, width: 96, height: 26)
        upcomingButton.isHidden = true
        cv.addSubview(upcomingButton)

        let bodyRect = NSRect(x: pad, y: pad + 22,
                              width: 840 - pad * 2,
                              height: 560 - topH - pad * 2 - 22)

        // Week/day grid (used for the single-day view).
        weekView = WeekView(frame: bodyRect)
        weekView.autoresizingMask = [.width, .height]
        weekView.weekStart = weekStart
        weekView.dayCount = dayCount
        weekView.delegate = self
        weekView.isHidden = (dayCount != 1)
        cv.addSubview(weekView)

        // Agenda list (the "no day selected" face), in a scroll view over the
        // same body rect. Transparent so the living backdrop shows through.
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
        agendaScroll.isHidden = (dayCount == 1)
        cv.addSubview(agendaScroll)

        // Status line (bottom).
        statusLabel = NSTextField(labelWithString: "")
        statusLabel.font = .systemFont(ofSize: 11)
        statusLabel.textColor = .secondaryLabelColor
        statusLabel.frame = NSRect(x: pad, y: pad - 2, width: 840 - pad * 2, height: 18)
        statusLabel.autoresizingMask = [.width, .maxYMargin]
        statusLabel.lineBreakMode = .byTruncatingTail
        cv.addSubview(statusLabel)

        // Inner radial glow, added last so it frames everything (click-through).
        dayGlow = DayGlowView(frame: cv.bounds)
        dayGlow.autoresizingMask = [.width, .height]
        cv.addSubview(dayGlow)

        updateDayTheme()
    }

    // Single source of truth for the day-color theme: washes the pane, lights
    // the in-window dots, colors the title, and tells the menu bar which day
    // is focused. In week mode there's no single focus (the week shows all
    // seven), so the wash clears and the title returns to neutral.
    private func updateDayTheme() {
        let dayMode = (dayCount == 1)
        let dayColor = DayPalette.color(for: weekStart)

        // Frame the window in the day color (edge glow), not a full wash.
        dayGlow?.color = dayMode ? dayColor : nil

        dayDots?.todayIndex = DayPalette.index(for: Date())
        dayDots?.selectedIndex = dayMode ? DayPalette.index(for: weekStart) : nil

        titleLabel?.textColor = dayMode ? dayColor : .labelColor

        // Grid for a single day, agenda list for "no day selected".
        weekView?.isHidden = !dayMode
        agendaScroll?.isHidden = dayMode
        upcomingButton?.isHidden = !dayMode

        onFocusedDayChanged?(dayMode ? weekStart : nil)
    }

    // A dot in the in-window strip was clicked: focus that weekday within the
    // currently displayed week, switching to day mode.
    private func focusDayInCurrentWeek(_ idx: Int) {
        let base = WeekView.startOfWeek(for: weekStart)
        let date = Calendar.current.date(byAdding: .day, value: idx, to: base) ?? weekStart
        dayCount = 1
        weekStart = Calendar.current.startOfDay(for: date)
        modeButton?.title = "Week"
        weekView.dayCount = 1
        weekView.weekStart = weekStart
        titleLabel.stringValue = weekRangeTitle()
        updateDayTheme()
        refresh()
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
        // No day selected → the rolling agenda.
        return "Upcoming"
    }

    // Show the window and jump to today (menu bar "Go to Today").
    func revealToday() {
        showWindow(nil)
        NSApp.activate(ignoringOtherApps: true)
        goToday()
    }

    // Show the window focused on a single day (menu bar day-circle tap).
    func revealDay(_ date: Date) {
        showWindow(nil)
        NSApp.activate(ignoringOtherApps: true)
        setDayMode(date)
        refresh()
    }

    // ── paging ─────────────────────────────────────────────────────────
    // DEPRECATED: week paging was removed from the window UI (CLI / launch
    // flags handle it). Kept for reference; shiftWeek below backs these.
    @available(*, deprecated, message: "Removed from window UI — paging lives in the CLI.")
    @objc private func prevWeek() { shiftWeek(by: -dayCount) }
    @available(*, deprecated, message: "Removed from window UI — paging lives in the CLI.")
    @objc private func nextWeek() { shiftWeek(by: dayCount) }
    @objc private func goToday() {
        weekStart = (dayCount == 1)
            ? Calendar.current.startOfDay(for: Date())
            : WeekView.startOfWeek(for: Date())
        weekView.weekStart = weekStart
        titleLabel.stringValue = weekRangeTitle()
        updateDayTheme()
        refresh()
    }

    // Leave a single-day view and return to the rolling agenda ("no day
    // selected"). Backs the "‹ Upcoming" toolbar button.
    @objc private func showUpcomingAction() {
        dayCount = 7
        weekStart = WeekView.startOfWeek(for: Date())
        modeButton?.title = "Day"
        weekView.dayCount = dayCount
        weekView.weekStart = weekStart
        titleLabel.stringValue = weekRangeTitle()
        updateDayTheme()
        refresh()
    }

    // DEPRECATED: the Day⇄Week toggle button was removed from the window;
    // mode is set via launch flags / CLI. Keeps the currently-focused day in
    // view across the switch. Kept for reference.
    @available(*, deprecated, message: "Removed from window UI — mode is set via CLI / launch flags.")
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
        updateDayTheme()
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
        updateDayTheme()
    }

    // DEPRECATED backing helper for prevWeek/nextWeek (both removed from UI).
    private func shiftWeek(by days: Int) {
        weekStart = Calendar.current.date(byAdding: .day, value: days, to: weekStart) ?? weekStart
        weekView.weekStart = weekStart
        titleLabel.stringValue = weekRangeTitle()
        updateDayTheme()
        refresh()
    }

    // DEPRECATED: the Refresh button was removed; refresh is automatic / CLI.
    @available(*, deprecated, message: "Removed from window UI — refresh is automatic / CLI.")
    @objc private func refreshAction() { refresh() }

    // The visible window's UTC bounds [weekStart, weekStart+7d).
    private func weekBounds() -> (from: Date, to: Date) {
        let to = Calendar.current.date(byAdding: .day, value: dayCount, to: weekStart) ?? weekStart
        return (weekStart, to)
    }

    // ── data load ────────────────────────────────────────────────────
    // Two loads, kept separate:
    //   • loadUpcoming() — the rolling agenda + the menu-bar countdown badge.
    //     Runs on every refresh (and a background timer) so the badge stays
    //     honest even when the window is closed or parked in day mode.
    //   • loadDayGrid()  — the hour grid for the focused single day. Only when
    //     a day is actually selected.
    func refresh() {
        guard let token = auth.currentToken() else { showAuthScreen(); return }
        api.setToken(token)
        loadUpcoming()
        if dayCount == 1 { loadDayGrid() }
    }

    // Rolling window [start of today, +upcomingDays). Feeds the agenda list and
    // the next-appointment badge; independent of the focused day/week.
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
    // publish the next appointment's start for the menu-bar countdown badge.
    private func rebuildUpcoming() {
        let now = Date()
        var laid: [LaidEvent] = []
        for e in upcomingAesthetical {
            guard let s = Self.parseISO(e.start), let en = Self.parseISO(e.end) else { continue }
            laid.append(LaidEvent(
                kind: .aesthetical, uid: e.uid, title: e.title,
                start: s, end: en, allDay: e.allDay ?? false,
                note: e.note ?? "", visibility: e.visibility ?? "private",
                sourceLabel: nil, color: nil))
        }
        for e in upcomingFeed {
            guard let s = Self.parseISO(e.start) else { continue }
            let en = e.end.flatMap(Self.parseISO) ?? s.addingTimeInterval(1800)
            laid.append(LaidEvent(
                kind: .feed, uid: e.uid, title: e.title,
                start: s, end: en, allDay: e.allDay ?? false,
                note: e.note ?? "", visibility: "private",
                sourceLabel: e.label, color: Self.color(from: e.color)))
        }
        // Keep still-relevant events (ongoing or ahead), ordered by start.
        let upcoming = laid.filter { $0.end >= now }.sorted { $0.start < $1.start }
        agendaView?.events = upcoming

        if dayCount != 1 {
            let count = upcoming.count
            statusLabel.stringValue = count == 0
                ? "Nothing coming up in the next \(Self.upcomingDays) days."
                : "\(count) upcoming event\(count == 1 ? "" : "s") · next \(Self.upcomingDays) days"
        }

        // Next appointment = soonest timed event that hasn't started yet. Skip
        // all-day items so the badge always counts down to a real clock time.
        let next = upcoming.first { !$0.allDay && $0.start > now }
        onNextEventChanged?(next?.start)
    }

    private func loadDayGrid() {
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

    // MARK: - Deprecated calendars UI
    // ── calendars panel: subscription link + connectors ──────────────
    // DEPRECATED: the "Calendars…" button was removed from the window;
    // subscription links and Google-calendar connectors are managed via the
    // CLI now. This panel and its helpers (loadSubLink/copySubLink/
    // rotateSubLink/reloadConnectors/removeConnector/connectFeedAction below)
    // are kept for reference and reachable only from here.
    @available(*, deprecated, message: "Removed from window UI — manage calendars via the CLI.")
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

    // ── AgendaViewDelegate ───────────────────────────────────────────
    // Same routing as the grid: aesthetical → editor, feed → read-only details.
    func agendaView(_ view: AgendaView, didSelectEditable event: LaidEvent) {
        presentEditor(uid: event.uid, title: event.title,
                      start: event.start, end: event.end,
                      note: event.note, visibility: event.visibility)
    }

    func agendaView(_ view: AgendaView, didSelectReadOnly event: LaidEvent) {
        weekView(weekView, didSelectReadOnly: event)
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
    // Always-on app: hide the window instead of closing it, so the shared
    // session watch (and live event state) survives. The menu bar strip
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
