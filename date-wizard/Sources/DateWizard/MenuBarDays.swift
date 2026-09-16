// MenuBarDays.swift — DateWizard's always-on menu bar presence.
//
// A strip of seven circles (Sun→Sat: S M T W T F S) painted in the shared
// ROYGBIV DayPalette (see DayStrip for the drawing). The current production
// face is the wand + next-appointment countdown; left-click opens the agenda
// and right-click drops the wizard menu.
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

// The instant hover label — a zero-delay stand-in for the OS tooltip (which
// waits ~1s). A borderless, non-activating panel that floats just under the
// wand and names the next appointment the moment the pointer arrives, gone the
// moment it leaves. Mirrors Slab's SigilBubble: mouse-transparent, statusBar
// level so it clears the menu bar, no window shadow (rapid re-show would
// flicker it). One shared instance per MenuBarDays.
private final class InstantHoverLabel {
    private let panel: NSPanel
    private let nameField: NSTextField
    private let subField: NSTextField
    private let stack: NSStackView

    init() {
        panel = NSPanel(contentRect: NSRect(x: 0, y: 0, width: 10, height: 10),
                        styleMask: [.borderless, .nonactivatingPanel],
                        backing: .buffered, defer: true)
        panel.isFloatingPanel = true
        panel.hidesOnDeactivate = false
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = false
        panel.ignoresMouseEvents = true
        panel.level = .statusBar
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]

        let card = NSView()
        card.wantsLayer = true
        card.layer?.cornerRadius = 6
        card.layer?.backgroundColor = NSColor(white: 0.09, alpha: 0.95).cgColor
        card.layer?.borderWidth = 1
        card.layer?.borderColor = NSColor(white: 1, alpha: 0.10).cgColor

        nameField = NSTextField(labelWithString: "")
        nameField.font = .systemFont(ofSize: 12, weight: .semibold)
        nameField.textColor = .white
        subField = NSTextField(labelWithString: "")
        subField.font = .systemFont(ofSize: 11, weight: .regular)
        subField.textColor = NSColor(white: 1, alpha: 0.62)

        stack = NSStackView(views: [nameField, subField])
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 1
        stack.translatesAutoresizingMaskIntoConstraints = false
        card.addSubview(stack)
        let pad: CGFloat = 7
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: card.leadingAnchor, constant: pad),
            stack.trailingAnchor.constraint(equalTo: card.trailingAnchor, constant: -pad),
            stack.topAnchor.constraint(equalTo: card.topAnchor, constant: pad - 2),
            stack.bottomAnchor.constraint(equalTo: card.bottomAnchor, constant: -(pad - 2)),
        ])
        panel.contentView = card
    }

    /// Show the label centered just under `button`, naming the appointment.
    /// `sub` (short countdown) rides underneath when present. No delay, no
    /// animation — it's on screen this frame.
    func show(name: String, sub: String?, below button: NSStatusBarButton) {
        guard let win = button.window else { return }
        nameField.stringValue = name
        subField.stringValue = sub ?? ""
        subField.isHidden = (sub == nil || sub!.isEmpty)

        panel.layoutIfNeeded()
        let fit = panel.contentView!.fittingSize
        let w = max(fit.width, 44), h = fit.height
        let anchor = win.convertToScreen(button.convert(button.bounds, to: nil))
        var x = anchor.midX - w / 2
        var y = anchor.minY - h - 3            // tucked just below the wand
        if let vis = (button.window?.screen ?? NSScreen.main)?.visibleFrame {
            x = min(max(vis.minX + 4, x), vis.maxX - w - 4)
            if y < vis.minY + 4 { y = anchor.maxY + 3 }   // flip above if no room
        }
        panel.setFrame(NSRect(x: x, y: y, width: w, height: h), display: true)
        panel.orderFrontRegardless()
    }

    func hide() {
        if panel.isVisible { panel.orderOut(nil) }
    }
}

final class MenuBarDays {

    // Callbacks wired by the AppDelegate.
    var onOpen: (() -> Void)?
    var onToggle: (() -> Void)?
    var onToday: (() -> Void)?

    private var statusItem: NSStatusItem!
    private weak var button: NSStatusBarButton?
    private let hover = HoverResponder()
    private let instantLabel = InstantHoverLabel()
    private var hoveredIndex: Int?
    private var focusedIndex: Int?        // the day showing in the wizard
    private var nextEventDate: Date?      // start of the next appointment (badge)
    private var nextEventTitle: String?   // name of that appointment (tooltip)
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
        // Pin the wand LEFT of Menu Band. NSStatusItem has no absolute-ordering
        // API; its autosaveName + "Preferred Position" default is the lever macOS
        // honors (higher = further left). Seed once; a user ⌘-drag persists over it.
        let posKey = "NSStatusItem Preferred Position datewizard"
        if UserDefaults.standard.object(forKey: posKey) == nil {
            UserDefaults.standard.set(24, forKey: posKey)
        }
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        statusItem.autosaveName = "datewizard"
        if let button = statusItem.button {
            self.button = button
            button.imagePosition = .imageOnly
            button.target = self
            button.action = #selector(clicked(_:))
            button.sendAction(on: [.leftMouseUp, .rightMouseUp])
            // No button.toolTip — the OS tooltip's ~1s delay is exactly what the
            // instant hover label replaces (see InstantHoverLabel / handleHover).

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
    func setNextEvent(_ date: Date?, title: String? = nil) {
        let had = (nextEventDate != nil)
        nextEventTitle = title
        if date != nextEventDate { nextEventDate = date; refresh() }
        // When an event appears/disappears, reshape the fit ladder so the broker
        // knows whether there's a badge to trade for space.
        if (date != nil) != had { fit?.updateRungs(fitRungs(hasNext: date != nil)) }
    }

    /// Short, unit-labeled countdown to the next appointment: the two most
    /// significant non-zero units ("2d3h", "20h41m", "5m30s", "45s"), "now" at
    /// zero. Seconds tick live once the event is under an hour away.
    private func countdownText() -> String? {
        guard let d = nextEventDate else { return nil }
        let secs = Int(d.timeIntervalSinceNow)
        if secs <= 0 { return "now" }
        let days = secs / 86400, h = (secs % 86400) / 3600, m = (secs % 3600) / 60, s = secs % 60
        if days > 0 { return "\(days)d\(h)h" }
        if h > 0 { return "\(h)h\(m)m" }
        if m > 0 { return "\(m)m\(s)s" }
        return "\(s)s"
    }

    // Re-render the badge every second so the H:MM:SS readout ticks live. It's a
    // tiny image redraw, and the wand sheds the badge under menu-bar pressure.
    private func scheduleCountdownTick() {
        countdownTimer?.invalidate()
        let timer = Timer(timeInterval: 1, repeats: true) { [weak self] _ in self?.refresh() }
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
    }

    // ── menu-bar-fit negotiation ──────────────────────────────────────
    // The wand's ladder: bare → presence-dot → full countdown pill. With no
    // upcoming event the ladder collapses to just the bare wand so the broker's
    // width model stays honest (nothing to shed). High priority (40): the wand's
    // countdown is the thing to defend, so Menu Band gives up piano keys before
    // DateWizard sheds the badge.
    private func startFitNegotiation() {
        guard let statusItem, fit == nil else { return }
        let has = (nextEventDate != nil)
        fit = MenuBarFit(slug: "datewizard", priority: 40,
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

    // ── input ─────────────────────────────────────────────────────────
    @objc private func clicked(_ sender: NSStatusBarButton) {
        // Right-click → the full daemon menu (calendar controls + summon roster).
        // Left-click → straight into the calendar (the thing you reach for most).
        if NSApp.currentEvent?.type == .rightMouseUp {
            showMenu(from: sender)
        } else {
            onToggle?()
        }
    }

    private func handleHover(_ event: NSEvent) {
        guard let button else { return }
        showInstantLabel(on: button)
        let local = button.convert(event.locationInWindow, from: nil)
        let idx = DayStrip.index(atX: local.x, in: button.bounds)
        if idx != hoveredIndex { hoveredIndex = idx; refresh() }
    }

    private func handleHoverExit() {
        instantLabel.hide()
        if hoveredIndex != nil { hoveredIndex = nil; refresh() }
    }

    // The zero-delay hover label: the moment the pointer lands on the wand,
    // name the next appointment (with its short countdown underneath). Nothing
    // upcoming → nothing shown. Idempotent, so it's safe on every mouse-move.
    private func showInstantLabel(on button: NSStatusBarButton) {
        guard let d = nextEventDate else { instantLabel.hide(); return }
        let name = (nextEventTitle?.isEmpty == false) ? nextEventTitle! : "appointment"
        instantLabel.show(name: name, sub: whenPhrase(d), below: button)
    }

    /// The label's second line: day-word + clock time, then the live countdown —
    /// "tomorrow 1:00 PM · 19h34m".
    private func whenPhrase(_ d: Date) -> String {
        let cal = Calendar.current
        let day: String
        if cal.isDateInToday(d) { day = "today" }
        else if cal.isDateInTomorrow(d) { day = "tomorrow" }
        else { let df = DateFormatter(); df.dateFormat = "EEE"; day = df.string(from: d) }
        let fmt = DateFormatter(); fmt.dateStyle = .none; fmt.timeStyle = .short
        let when = "\(day) \(fmt.string(from: d))"
        return countdownText().map { "\(when) · \($0)" } ?? when
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
