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
            button.toolTip = "DateWizard"

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

    /// Repaint the strip for the current day / focus / hover.
    func refresh() {
        guard let button = statusItem?.button else { return }
        let w = DayStrip.contentWidth(forHeight: barThickness)
        let img = NSImage(size: NSSize(width: w, height: barThickness))
        img.lockFocus()
        DayStrip.draw(in: NSRect(x: 0, y: 0, width: w, height: barThickness),
                      selected: focusedIndex,
                      today: DayPalette.index(for: Date()),
                      hovered: hoveredIndex)
        img.unlockFocus()
        img.isTemplate = false            // keep our colors; don't tint as a template
        button.image = img
    }

    // ── input ─────────────────────────────────────────────────────────
    @objc private func clicked(_ sender: NSStatusBarButton) {
        if NSApp.currentEvent?.type == .rightMouseUp {
            showMenu(from: sender)
            return
        }
        // Left-click: open the day under the cursor — like tapping a single
        // note key in Menu Band. The dot maps to that weekday in the current
        // (Sun→Sat) week.
        if let event = NSApp.currentEvent {
            let local = sender.convert(event.locationInWindow, from: nil)
            let idx = DayStrip.index(atX: local.x, in: sender.bounds)
            let weekStart = WeekView.startOfWeek(for: Date())
            let date = Calendar.current.date(byAdding: .day, value: idx, to: weekStart) ?? Date()
            onSelectDay?(date)
            return
        }
        onOpen?()
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
        let open = NSMenuItem(title: "Open DateWizard", action: #selector(menuOpen), keyEquivalent: "")
        open.target = self
        menu.addItem(open)
        let today = NSMenuItem(title: "Go to Today", action: #selector(menuToday), keyEquivalent: "")
        today.target = self
        menu.addItem(today)
        menu.addItem(.separator())
        let quit = NSMenuItem(title: "Quit DateWizard", action: #selector(menuQuit), keyEquivalent: "q")
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
