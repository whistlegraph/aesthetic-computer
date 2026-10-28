// MenuBarDate.swift — DateWizard's always-on menu bar presence.
//
// Plain text that tints with the bar: today's date, then the start time of the
// next appointment when it falls today ("Wed 28 · 9:00"). Hovering names the
// appointment; left-click toggles the agenda, right-click drops a short menu.
import AppKit

// NSResponder shim that forwards tracking-area mouse events to closures
// (NSTrackingArea.owner has to be an NSResponder).
private final class HoverResponder: NSResponder {
    var onEnter: (() -> Void)?
    var onExit: (() -> Void)?
    override func mouseEntered(with event: NSEvent) { onEnter?() }
    override func mouseExited(with event: NSEvent) { onExit?() }
}

// A zero-delay stand-in for the OS tooltip (which waits ~1s): a borderless,
// mouse-transparent panel just under the item naming the next appointment.
private final class InstantHoverLabel {
    private let panel: NSPanel
    private let nameField = NSTextField(labelWithString: "")
    private let subField = NSTextField(labelWithString: "")

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

        nameField.font = .systemFont(ofSize: 12, weight: .semibold)
        nameField.textColor = .white
        subField.font = .systemFont(ofSize: 11)
        subField.textColor = NSColor(white: 1, alpha: 0.62)

        let stack = NSStackView(views: [nameField, subField])
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 1
        stack.translatesAutoresizingMaskIntoConstraints = false
        card.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: card.leadingAnchor, constant: 7),
            stack.trailingAnchor.constraint(equalTo: card.trailingAnchor, constant: -7),
            stack.topAnchor.constraint(equalTo: card.topAnchor, constant: 5),
            stack.bottomAnchor.constraint(equalTo: card.bottomAnchor, constant: -5),
        ])
        panel.contentView = card
    }

    func show(name: String, sub: String, below button: NSStatusBarButton) {
        guard let win = button.window else { return }
        nameField.stringValue = name
        subField.stringValue = sub
        panel.layoutIfNeeded()
        let fit = panel.contentView!.fittingSize
        let w = max(fit.width, 44), h = fit.height
        let anchor = win.convertToScreen(button.convert(button.bounds, to: nil))
        var x = anchor.midX - w / 2
        var y = anchor.minY - h - 3
        if let vis = (win.screen ?? NSScreen.main)?.visibleFrame {
            x = min(max(vis.minX + 4, x), vis.maxX - w - 4)
            if y < vis.minY + 4 { y = anchor.maxY + 3 }
        }
        panel.setFrame(NSRect(x: x, y: y, width: w, height: h), display: true)
        panel.orderFrontRegardless()
    }

    func hide() {
        if panel.isVisible { panel.orderOut(nil) }
    }
}

final class MenuBarDate {

    // Callbacks wired by the AppDelegate.
    var onOpen: (() -> Void)?
    var onToggle: (() -> Void)?

    private var statusItem: NSStatusItem!
    private weak var button: NSStatusBarButton?
    private let hover = HoverResponder()
    private let instantLabel = InstantHoverLabel()
    private var nextEventDate: Date?
    private var nextEventTitle: String?
    // Menu-bar-fit rung: 1 = date + next time, 0 = date only. Driven by the
    // shared negotiation bus (MenuBarFit) under menu-bar pressure.
    private var fitRung = 1
    private var fit: MenuBarFit?

    private let font = NSFont.monospacedDigitSystemFont(
        ofSize: NSFont.menuBarFont(ofSize: 0).pointSize, weight: .regular)

    // ── lifecycle ─────────────────────────────────────────────────────
    func install() {
        // Pin left of Menu Band. "Preferred Position" is the lever macOS honors
        // (higher = further left); seed once so a user ⌘-drag persists over it.
        let posKey = "NSStatusItem Preferred Position datewizard"
        if UserDefaults.standard.object(forKey: posKey) == nil {
            UserDefaults.standard.set(24, forKey: posKey)
        }
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        statusItem.autosaveName = "datewizard"
        if let button = statusItem.button {
            self.button = button
            button.font = font
            button.target = self
            button.action = #selector(clicked(_:))
            button.sendAction(on: [.leftMouseUp, .rightMouseUp])
            hover.onEnter = { [weak self] in self?.showInstantLabel() }
            hover.onExit = { [weak self] in self?.instantLabel.hide() }
            button.addTrackingArea(NSTrackingArea(
                rect: .zero,
                options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
                owner: hover, userInfo: nil))
        }
        refresh()
        observeDayChange()
        startFitNegotiation()
    }

    /// The next timed appointment (nil = nothing ahead). Called by the wizard
    /// whenever it reloads or the current appointment starts.
    func setNextEvent(_ date: Date?, title: String? = nil) {
        nextEventTitle = title
        guard date != nextEventDate else { return }
        nextEventDate = date
        refresh()
        fit?.updateRungs(fitRungs())
    }

    // ── face ──────────────────────────────────────────────────────────
    private func dateText() -> String {
        let f = DateFormatter(); f.dateFormat = "EEE d"
        return f.string(from: Date())
    }

    // The next appointment's clock time, only when it falls today; later days
    // are named on hover instead.
    private func nextTimeText() -> String? {
        guard let d = nextEventDate, Calendar.current.isDateInToday(d) else { return nil }
        let f = DateFormatter(); f.dateFormat = "h:mm"
        return f.string(from: d)
    }

    private func title(forRung rung: Int) -> String {
        if rung >= 1, let t = nextTimeText() { return "\(dateText()) · \(t)" }
        return dateText()
    }

    private func width(of text: String) -> CGFloat {
        ceil((text as NSString).size(withAttributes: [.font: font]).width) + 12
    }

    func refresh() {
        guard let button else { return }
        let text = title(forRung: fitRung)
        button.title = text
        statusItem.length = width(of: text)
    }

    // ── menu-bar-fit negotiation ──────────────────────────────────────
    // High priority (40): the date is the thing to defend, so Menu Band gives up
    // piano keys before DateWizard drops the next-appointment time.
    private func startFitNegotiation() {
        guard let statusItem, fit == nil else { return }
        let rungs = fitRungs()
        fit = MenuBarFit(slug: "datewizard", priority: 40, rungs: rungs,
                         statusItem: statusItem, startAt: rungs.count - 1) { [weak self] _, idx in
            self?.fitRung = idx
            self?.refresh()
        }
        fit?.start()
    }

    private func fitRungs() -> [MenuBarFit.Rung] {
        var rungs = [MenuBarFit.Rung(name: "date", width: width(of: title(forRung: 0)))]
        if nextTimeText() != nil {
            rungs.append(.init(name: "date+next", width: width(of: title(forRung: 1))))
        }
        return rungs
    }

    // ── input ─────────────────────────────────────────────────────────
    @objc private func clicked(_ sender: NSStatusBarButton) {
        if NSApp.currentEvent?.type == .rightMouseUp {
            showMenu(from: sender)
        } else {
            onToggle?()
        }
    }

    private func showInstantLabel() {
        guard let button, let d = nextEventDate else { return }
        let name = (nextEventTitle?.isEmpty == false) ? nextEventTitle! : "appointment"
        instantLabel.show(name: name, sub: whenPhrase(d), below: button)
    }

    /// "today 9:00 AM", "tomorrow 1:00 PM", "Fri 10:30 AM".
    private func whenPhrase(_ d: Date) -> String {
        let cal = Calendar.current
        let day: String
        if cal.isDateInToday(d) { day = "today" }
        else if cal.isDateInTomorrow(d) { day = "tomorrow" }
        else { let df = DateFormatter(); df.dateFormat = "EEE"; day = df.string(from: d) }
        let fmt = DateFormatter(); fmt.dateStyle = .none; fmt.timeStyle = .short
        return "\(day) \(fmt.string(from: d))"
    }

    private func showMenu(from button: NSStatusBarButton) {
        let menu = NSMenu()
        let open = NSMenuItem(title: "Open Calendar", action: #selector(menuOpen), keyEquivalent: "")
        open.target = self
        menu.addItem(open)
        menu.addItem(.separator())
        let quit = NSMenuItem(title: "Quit DateWizard", action: #selector(menuQuit), keyEquivalent: "q")
        quit.target = self
        menu.addItem(quit)
        menu.popUp(positioning: nil,
                   at: NSPoint(x: 0, y: button.bounds.height + 4),
                   in: button)
    }

    @objc private func menuOpen() { onOpen?() }
    @objc private func menuQuit() { NSApp.terminate(nil) }

    // Roll the date over when the local day changes.
    private func observeDayChange() {
        NotificationCenter.default.addObserver(
            forName: .NSCalendarDayChanged, object: nil, queue: .main
        ) { [weak self] _ in
            guard let self else { return }
            self.refresh()
            self.fit?.updateRungs(self.fitRungs())
        }
    }
}
