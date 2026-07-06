// MenuBarDays.swift — DateWizard's always-on menu bar presence.
//
// A strip of seven circles (Sun→Sat: S M T W T F S) painted in the shared
// ROYGBIV DayPalette (see DayStrip for the drawing). The focused day — the
// day showing in the wizard — is lit and ringed; today carries a faint
// marker; the rest dim back. Hovering a dot lights it; left-clicking a dot
// opens that day; right-click drops a menu.
import AppKit

// NSResponder shim that forwards tracking-area mouse events to closures
// (NSTrackingArea.owner has to be an NSResponder). Mirrors Menu Band's
// HoverResponder so the two apps track hover the same way.
private final class HoverResponder: NSResponder {
    var onMove: ((NSEvent) -> Void)?
    var onExit: (() -> Void)?
    override func mouseEntered(with event: NSEvent) { onMove?(event) }
    override func mouseMoved(with event: NSEvent) { onMove?(event) }
    override func mouseExited(with event: NSEvent) { onExit?() }
}

final class MenuBarDays {

    // Callbacks wired by the AppDelegate.
    var onOpen: (() -> Void)?
    var onToday: (() -> Void)?
    var onSelectDay: ((Date) -> Void)?

    private var statusItem: NSStatusItem!
    private weak var button: NSStatusBarButton?
    private let hover = HoverResponder()
    private var hoveredIndex: Int?
    private var focusedIndex: Int?        // the day showing in the wizard
    private var nextEventDate: Date?      // start of the next appointment (badge)
    private var countdownTimer: Timer?    // ticks the badge text down
    private var midnightTimer: Timer?
    // Menu-bar-fit rung: 2 = full countdown pill, 1 = presence dot, 0 = bare
    // wand. Driven by the shared negotiation bus (MenuBarFit) — under menu-bar
    // pressure the wand sheds its badge before other apps lose more.
    private var fitRung = 2
    private var fit: MenuBarFit?
    private var barThickness: CGFloat = 22
    // The daemon's roster of sibling wizards it can summon (Date is built in).
    private let roster = WizardRoster()

    // ── lifecycle ─────────────────────────────────────────────────────
    func install() {
        barThickness = NSStatusBar.system.thickness
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        if let button = statusItem.button {
            self.button = button
            button.imagePosition = .imageOnly
            button.target = self
            button.action = #selector(clicked(_:))
            button.sendAction(on: [.leftMouseUp, .rightMouseUp])
            button.toolTip = "Wizard — calendar + summon (right-click)"

            // Hover tracking — lights the dot under the cursor.
            hover.onMove = { [weak self] ev in self?.handleHover(ev) }
            hover.onExit = { [weak self] in self?.handleHoverExit() }
            let area = NSTrackingArea(
                rect: button.bounds,
                options: [.mouseMoved, .mouseEnteredAndExited, .activeAlways, .inVisibleRect],
                owner: hover, userInfo: nil)
            button.addTrackingArea(area)
        }
        refresh()
        scheduleMidnightRefresh()
        scheduleCountdownTick()
        startFitNegotiation()
        // Redraw when the system flips light/dark so the (non-template) badged
        // wand re-tints correctly.
        DistributedNotificationCenter.default().addObserver(
            self, selector: #selector(appearanceChanged),
            name: NSNotification.Name("AppleInterfaceThemeChangedNotification"), object: nil)
    }

    @objc private func appearanceChanged() { refresh() }

    /// Highlight the day currently focused in the wizard (nil = none/today).
    func setFocusedDay(_ date: Date?) {
        let idx = date.map { DayPalette.index(for: $0) }
        if idx != focusedIndex { focusedIndex = idx; refresh() }
    }

    /// The start of the next appointment (nil = nothing ahead). Drives the
    /// countdown badge on the wand. Called by the wizard whenever it reloads
    /// the upcoming set.
    func setNextEvent(_ date: Date?) {
        let had = (nextEventDate != nil)
        if date != nextEventDate { nextEventDate = date; refresh() }
        // When an event appears/disappears, reshape the fit ladder so the broker
        // knows whether there's a badge to trade for space.
        if (date != nil) != had { fit?.updateRungs(fitRungs(hasNext: date != nil)) }
    }

    /// Compact time-until-next-appointment: "3d", "5h", "12m", or "now".
    private func countdownText() -> String? {
        guard let d = nextEventDate else { return nil }
        let secs = d.timeIntervalSinceNow
        if secs < 60 { return "now" }
        if secs < 3600 { return "\(Int(secs / 60))m" }
        if secs < 86400 { return "\(Int(secs / 3600))h" }
        return "\(Int(secs / 86400))d"
    }

    // Re-render the badge as the countdown ticks. 30s keeps the minute readout
    // within half a minute of accurate without burning cycles.
    private func scheduleCountdownTick() {
        countdownTimer?.invalidate()
        let timer = Timer(timeInterval: 30, repeats: true) { [weak self] _ in self?.refresh() }
        RunLoop.main.add(timer, forMode: .common)
        countdownTimer = timer
    }

    /// Paint the menu-bar face: a plain black magic wand (template image, so it
    /// tints to the bar like Menu Band's note glyph). This daemon is the "wizard
    /// of wizards" now (the date is built in) — the costumed wizardGuy lives on
    /// only inside the menu/About, where color reads fine.
    func refresh() {
        guard let button = statusItem?.button else { return }
        let dark = button.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let hasNext = (nextEventDate != nil)
        switch fitRung {
        case 2 where hasNext: button.image = wandGlyph(badge: countdownText(), dark: dark)
        case 1 where hasNext: button.image = wandGlyph(dot: true, dark: dark)
        default:              button.image = wandGlyph(dark: dark)   // bare
        }
        button.toolTip = nextEventToolTip()
    }

    // ── menu-bar-fit negotiation ──────────────────────────────────────
    // The wand's ladder: bare → presence-dot → full countdown pill. With no
    // upcoming event the ladder collapses to just the bare wand so the broker's
    // width model stays honest (nothing to shed). Low priority (20): the badge
    // is glanceable-but-redundant with the calendar, so DateWizard sheds it
    // before Menu Band gives up piano keys.
    private func startFitNegotiation() {
        guard let statusItem, fit == nil else { return }
        let has = (nextEventDate != nil)
        fit = MenuBarFit(slug: "datewizard", priority: 20,
                         rungs: fitRungs(hasNext: has), statusItem: statusItem,
                         startAt: has ? 2 : 0) { [weak self] _, idx in
            self?.fitRung = idx
            self?.refresh()
        }
        fit?.start()
    }

    private func fitRungs(hasNext: Bool) -> [MenuBarFit.Rung] {
        hasNext
            ? [.init(name: "bare", width: 24), .init(name: "dot", width: 32),
               .init(name: "badge", width: 46)]
            : [.init(name: "bare", width: 24)]
    }

    private func nextEventToolTip() -> String {
        let base = "Wizard — calendar + summon (right-click)"
        guard let d = nextEventDate else { return base }
        let fmt = DateFormatter(); fmt.dateStyle = .none; fmt.timeStyle = .short
        let cal = Calendar.current
        let dayWord: String
        if cal.isDateInToday(d) { dayWord = "" }
        else if cal.isDateInTomorrow(d) { dayWord = "tomorrow " }
        else { let df = DateFormatter(); df.dateFormat = "EEE "; dayWord = df.string(from: d) }
        return "Next: \(dayWord)\(fmt.string(from: d)) · \(base)"
    }

    // ── input ─────────────────────────────────────────────────────────
    @objc private func clicked(_ sender: NSStatusBarButton) {
        // Right-click → the full daemon menu (calendar controls + summon roster).
        // Left-click → straight into the calendar (the thing you reach for most).
        if NSApp.currentEvent?.type == .rightMouseUp {
            showMenu(from: sender)
        } else {
            onOpen?()
        }
    }

    private func handleHover(_ event: NSEvent) {
        guard let button else { return }
        let local = button.convert(event.locationInWindow, from: nil)
        let idx = DayStrip.index(atX: local.x, in: button.bounds)
        if idx != hoveredIndex { hoveredIndex = idx; refresh() }
    }

    private func handleHoverExit() {
        if hoveredIndex != nil { hoveredIndex = nil; refresh() }
    }

    private func showMenu(from button: NSStatusBarButton) {
        let menu = NSMenu()

        // ── The date, built in ──
        let open = NSMenuItem(title: "Open Calendar", action: #selector(menuOpen), keyEquivalent: "")
        open.target = self
        open.image = wizardGuy(scale: 1)
        menu.addItem(open)
        let today = NSMenuItem(title: "Go to Today", action: #selector(menuToday), keyEquivalent: "")
        today.target = self
        menu.addItem(today)

        // ── Summon a sibling wizard ──
        menu.addItem(.separator())
        let heading = NSMenuItem(title: "Summon", action: nil, keyEquivalent: "")
        heading.isEnabled = false
        menu.addItem(heading)
        for (i, w) in siblingWizards.enumerated() {
            let item = NSMenuItem(title: w.exe, action: #selector(WizardRoster.summon(_:)), keyEquivalent: "")
            item.target = roster
            item.tag = i
            item.toolTip = w.blurb
            item.image = roster.mascotImage(w, side: 18)
            menu.addItem(item)
        }

        // ── About / quit ──
        menu.addItem(.separator())
        let about = NSMenuItem(title: "About Our Wizards…", action: #selector(WizardRoster.showAbout), keyEquivalent: "")
        about.target = roster
        menu.addItem(about)
        let quit = NSMenuItem(title: "Quit Wizard", action: #selector(menuQuit), keyEquivalent: "q")
        quit.target = self
        menu.addItem(quit)

        menu.popUp(positioning: nil,
                   at: NSPoint(x: 0, y: button.bounds.height + 4),
                   in: button)
    }

    @objc private func menuOpen() { onOpen?() }
    @objc private func menuToday() { onToday?() }
    @objc private func menuQuit() { NSApp.terminate(nil) }

    // Refresh the strip exactly when the local day rolls over, then again
    // the next midnight, indefinitely.
    private func scheduleMidnightRefresh() {
        let cal = Calendar.current
        let now = Date()
        let nextMidnight = cal.nextDate(after: now,
                                        matching: DateComponents(hour: 0, minute: 0, second: 1),
                                        matchingPolicy: .nextTime) ?? now.addingTimeInterval(86400)
        midnightTimer?.invalidate()
        let timer = Timer(fire: nextMidnight, interval: 0, repeats: false) { [weak self] _ in
            self?.refresh()
            self?.scheduleMidnightRefresh()
        }
        RunLoop.main.add(timer, forMode: .common)
        midnightTimer = timer
    }
}
