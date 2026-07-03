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
    private var midnightTimer: Timer?
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
    }

    /// Highlight the day currently focused in the wizard (nil = none/today).
    func setFocusedDay(_ date: Date?) {
        let idx = date.map { DayPalette.index(for: $0) }
        if idx != focusedIndex { focusedIndex = idx; refresh() }
    }

    /// Paint the menu-bar face: the hand-pixeled wizard guy. This daemon is the
    /// "wizard of wizards" now (the date is built in), so it wears the wizard,
    /// not the day strip.
    func refresh() {
        guard let button = statusItem?.button else { return }
        let img = wizardGuy(scale: 1)
        img.isTemplate = false            // keep his colors; don't tint as a template
        button.image = img
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
