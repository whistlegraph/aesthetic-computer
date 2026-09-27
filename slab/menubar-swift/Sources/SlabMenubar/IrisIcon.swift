import AppKit

/// The optional second status item: the Iris avatar (a PNG the `iris` helper
/// caches from the controller machine) clipped to a circle, with a small
/// status dot in the lower-right corner. Nothing here is a template image —
/// the avatar keeps its colors in both menubar appearances.
enum IrisIcon {
    /// Point size of the rendered menubar image.
    static let size: CGFloat = 18

    /// Rolled-up fleet condition, worst-wins: red beats amber beats green.
    enum Condition { case idle, running, queued, attention }

    static func condition(for s: IrisState) -> Condition {
        if !s.configured { return .attention }
        if s.machines.contains(where: { !$0.online }) { return .attention }
        if s.hasController && !s.windowOpen { return .attention }
        if !s.lastError.isEmpty { return .attention }
        if s.tasks.contains(where: { $0.phase == "blocked" }) { return .attention }
        if s.tasks.contains(where: { $0.phase == "queued" }) { return .queued }
        if s.tasks.contains(where: { $0.phase == "running" }) { return .running }
        return .idle
    }

    /// Load the cached avatar, if any. Decoded once per call — callers cache
    /// the result and only reload when the file's modification date changes.
    static func loadAvatar(at path: String) -> NSImage? {
        guard FileManager.default.fileExists(atPath: path),
              let img = NSImage(contentsOfFile: path), img.isValid else { return nil }
        return img
    }

    /// Compose the menubar image: circular avatar (or a neutral placeholder
    /// disc when no avatar is cached yet) plus the condition dot.
    static func render(avatar: NSImage?, condition: Condition) -> NSImage {
        let dotColor = dotColor(condition)
        // Block-based image: drawn at the display's scale on demand, so no
        // stray empty bitmap rep can win the Retina match (that left the item blank).
        let img = NSImage(size: NSSize(width: size, height: size), flipped: false) { rect in
            NSGraphicsContext.current?.imageInterpolation = .high
            let circle = NSBezierPath(ovalIn: rect.insetBy(dx: 0.5, dy: 0.5))
            NSGraphicsContext.saveGraphicsState()
            circle.addClip()
            if let avatar = avatar {
                // Fill the circle edge-to-edge (aspect fill), centered.
                let a = avatar.size
                let s = max(rect.width / max(a.width, 1), rect.height / max(a.height, 1))
                let w = a.width * s, h = a.height * s
                avatar.draw(in: NSRect(x: (rect.width - w) / 2, y: (rect.height - h) / 2, width: w, height: h),
                            from: .zero, operation: .sourceOver, fraction: 1)
            } else {
                NSColor(white: 0.5, alpha: 0.35).setFill()
                circle.fill()
            }
            NSGraphicsContext.restoreGraphicsState()

            if let color = dotColor {
                let d: CGFloat = 6.5
                let dotRect = NSRect(x: rect.maxX - d - 0.5, y: rect.minY + 0.5, width: d, height: d)
                // Halo so the dot reads against any avatar pixels beneath it.
                NSColor(white: 0, alpha: 0.55).setFill()
                NSBezierPath(ovalIn: dotRect.insetBy(dx: -1, dy: -1)).fill()
                color.setFill()
                NSBezierPath(ovalIn: dotRect).fill()
            }
            return true
        }
        img.isTemplate = false
        return img
    }

    private static func dotColor(_ c: Condition) -> NSColor? {
        switch c {
        case .idle: return nil
        case .running: return NSColor(deviceHue: 0.33, saturation: 0.85, brightness: 0.95, alpha: 1)
        case .queued: return NSColor(deviceHue: 0.14, saturation: 0.85, brightness: 0.95, alpha: 1)
        case .attention: return NSColor(deviceHue: 0.99, saturation: 0.85, brightness: 0.95, alpha: 1)
        }
    }

    /// Hover text: the helper's label plus one line per machine.
    static func toolTip(for s: IrisState) -> String {
        var lines = [s.label]
        for m in s.machines {
            let what = m.online ? (m.headline.isEmpty ? m.role : m.headline) : "unreachable"
            lines.append("\(m.name): \(what)")
        }
        if s.hasController && !s.windowOpen { lines.append("watch window closed") }
        return lines.joined(separator: "\n")
    }
}

// MARK: - Hover card

/// Borderless card under the Iris Icon, in the slab stats bar's visual
/// language (same panel fill/stroke, monospaced type). One row per machine,
/// the watch-window line, then a running / queued / blocked / review strip.
/// Drawn from a cached IrisState only — no I/O here.
final class IrisHoverView: NSView {
    var state = IrisState() { didSet { needsDisplay = true } }

    private var dark: Bool {
        effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
    }

    /// Height the card wants for this state.
    static func height(for s: IrisState) -> CGFloat {
        let rows = CGFloat(max(1, s.machines.count)) + (s.hasController ? 1 : 0)
        return 8 + 18 + rows * 16 + 6 + 20 + 8
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let panel = NSBezierPath(roundedRect: bounds.insetBy(dx: 0.5, dy: 0.5), xRadius: 8, yRadius: 8)
        (dark ? NSColor(srgbRed: 0.025, green: 0.035, blue: 0.050, alpha: 1)
              : NSColor(srgbRed: 0.965, green: 0.975, blue: 0.98, alpha: 1)).setFill()
        panel.fill()
        (dark ? NSColor.white : NSColor.black).withAlphaComponent(0.22).setStroke()
        panel.lineWidth = 1
        panel.stroke()

        let ink = dark ? NSColor.white : NSColor.black
        let title: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedSystemFont(ofSize: 10, weight: .heavy),
            .foregroundColor: ink.withAlphaComponent(0.92)]
        let body: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 10, weight: .semibold),
            .foregroundColor: ink.withAlphaComponent(0.85)]
        let dim: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 10, weight: .semibold),
            .foregroundColor: ink.withAlphaComponent(0.5)]

        let content = bounds.insetBy(dx: 10, dy: 8)
        var y = content.maxY - 14
        NSString(string: state.label).draw(at: NSPoint(x: content.minX, y: y), withAttributes: title)
        y -= 20

        let s = state
        for m in s.machines {
            let fresh = m.online && m.heartbeatAge >= 0 && m.heartbeatAge < 180
            dot(at: NSPoint(x: content.minX + 4, y: y + 5), hue: fresh ? 0.33 : 0.99)
            let age = m.heartbeatAge < 0 ? "" : " " + Self.age(m.heartbeatAge)
            let head = m.online ? (m.headline.isEmpty ? m.role : m.headline)
                                : (m.error.isEmpty ? "unreachable" : m.error)
            let name = NSString(string: "\(m.name)")
            name.draw(at: NSPoint(x: content.minX + 14, y: y), withAttributes: body)
            let nameW = name.size(withAttributes: body).width
            NSString(string: " · \(m.role)\(age)").draw(at: NSPoint(x: content.minX + 14 + nameW, y: y), withAttributes: dim)
            let headStr = NSString(string: Self.trim(head, 34))
            let hw = headStr.size(withAttributes: body).width
            headStr.draw(at: NSPoint(x: content.maxX - hw, y: y), withAttributes: body)
            y -= 16
        }
        if s.hasController {
            let text: String
            if s.windowOpen { text = "window open until \(Self.shortDate(s.expiresAt))" }
            else { text = "window closed \(Self.shortDate(s.expiresAt)) — nothing dispatches" }
            if !s.windowOpen { dot(at: NSPoint(x: content.minX + 4, y: y + 5), hue: 0.99) }
            NSString(string: text).draw(at: NSPoint(x: content.minX + 14, y: y),
                                        withAttributes: s.windowOpen ? dim : body)
            y -= 16
        }

        // Count strip: four cells, each tinted by its phase colour.
        let cells: [(String, Int, CGFloat?)] = [
            ("run", s.tasks.filter { $0.phase == "running" }.count, 0.33),
            ("queued", s.tasks.filter { $0.phase == "queued" }.count, 0.14),
            ("blocked", s.tasks.filter { $0.phase == "blocked" }.count, 0.99),
            ("review", s.tasks.filter { $0.phase == "awaiting_review" }.count, nil),
        ]
        let stripY = content.minY
        let gap: CGFloat = 4
        let cellW = (content.width - gap * 3) / 4
        for (i, cell) in cells.enumerated() {
            let r = NSRect(x: content.minX + CGFloat(i) * (cellW + gap), y: stripY, width: cellW, height: 20)
            let base = cell.2.map { NSColor(deviceHue: $0, saturation: 0.85, brightness: 0.95, alpha: 1) }
                ?? ink
            base.withAlphaComponent(cell.1 > 0 ? (dark ? 0.20 : 0.16) : (dark ? 0.06 : 0.05)).setFill()
            NSBezierPath(roundedRect: r, xRadius: 4, yRadius: 4).fill()
            let n = NSString(string: "\(cell.1)")
            let nAttr: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedDigitSystemFont(ofSize: 10, weight: .heavy),
                .foregroundColor: (cell.1 > 0 ? base : ink.withAlphaComponent(0.45))]
            n.draw(at: NSPoint(x: r.minX + 6, y: r.minY + 4), withAttributes: nAttr)
            let lbl = NSString(string: cell.0)
            let lw = lbl.size(withAttributes: dim).width
            lbl.draw(at: NSPoint(x: r.maxX - lw - 6, y: r.minY + 4), withAttributes: dim)
        }
    }

    private func dot(at p: NSPoint, hue: CGFloat) {
        NSColor(deviceHue: hue, saturation: 0.85, brightness: 0.95, alpha: 1).setFill()
        NSBezierPath(ovalIn: NSRect(x: p.x - 3, y: p.y - 3, width: 6, height: 6)).fill()
    }

    private static func trim(_ s: String, _ n: Int) -> String {
        s.count <= n ? s : String(s.prefix(n - 1)) + "…"
    }
    private static func age(_ sec: Int) -> String {
        sec < 60 ? "\(sec)s" : sec < 3600 ? "\(sec / 60)m" : "\(sec / 3600)h"
    }
    private static func shortDate(_ iso: String) -> String {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        var d = f.date(from: iso)
        if d == nil { f.formatOptions = [.withInternetDateTime]; d = f.date(from: iso) }
        guard let date = d else { return iso }
        let out = DateFormatter(); out.dateFormat = "MMM d HH:mm"
        return out.string(from: date)
    }
}

/// Owns the hover panel for the Iris Icon: shows under the status button while
/// the pointer is over it, hides when it leaves. Mirrors ResourceGraph's
/// pointer-sync approach (status-item buttons swallow tracking areas
/// inconsistently, so a mouse-move monitor against the button's real screen
/// rect is what makes hover deterministic).
final class IrisHoverCard {
    private weak var button: NSStatusBarButton?
    private var panel: NSPanel?
    private let view = IrisHoverView(frame: .zero)
    private var globalMonitor: Any?
    private var localMonitor: Any?

    func attach(to button: NSStatusBarButton) {
        self.button = button
        detachMonitors()
        globalMonitor = NSEvent.addGlobalMonitorForEvents(matching: .mouseMoved) { [weak self] _ in
            DispatchQueue.main.async { self?.sync() }
        }
        localMonitor = NSEvent.addLocalMonitorForEvents(matching: .mouseMoved) { [weak self] event in
            self?.sync(); return event
        }
    }

    func detach() {
        detachMonitors()
        hide()
        button = nil
    }

    func update(_ state: IrisState) {
        view.state = state
        if let panel, panel.isVisible { place(panel) }
    }

    private func detachMonitors() {
        if let m = globalMonitor { NSEvent.removeMonitor(m) }
        if let m = localMonitor { NSEvent.removeMonitor(m) }
        globalMonitor = nil; localMonitor = nil
    }

    private func buttonScreenRect() -> NSRect? {
        guard let button, let window = button.window else { return nil }
        return window.convertToScreen(button.convert(button.bounds, to: nil))
    }

    private func sync() {
        guard let rect = buttonScreenRect() else { return }
        // Keep the card up while the pointer is on the icon or on the card.
        let onIcon = rect.insetBy(dx: -1, dy: -2).contains(NSEvent.mouseLocation)
        let onCard = panel?.isVisible == true && panel!.frame.insetBy(dx: -2, dy: -6).contains(NSEvent.mouseLocation)
        if onIcon || onCard { show() } else if panel?.isVisible == true { hide() }
    }

    private func show() {
        if let panel, panel.isVisible { return }
        guard buttonScreenRect() != nil else { return }
        let p = panel ?? makePanel()
        panel = p
        place(p)
        p.orderFrontRegardless()
    }

    private func hide() {
        panel?.orderOut(nil)
    }

    private func makePanel() -> NSPanel {
        let p = NSPanel(contentRect: NSRect(x: 0, y: 0, width: 320, height: 100),
                        styleMask: [.borderless, .nonactivatingPanel],
                        backing: .buffered, defer: false)
        p.isOpaque = false
        p.backgroundColor = .clear
        p.hasShadow = true
        p.level = .statusBar
        p.isFloatingPanel = true
        p.hidesOnDeactivate = false
        p.becomesKeyOnlyIfNeeded = true
        p.ignoresMouseEvents = true
        p.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]
        view.autoresizingMask = [.width, .height]
        view.wantsLayer = true
        view.layer?.cornerRadius = 8
        view.layer?.masksToBounds = true
        p.contentView = view
        return p
    }

    private func place(_ p: NSPanel) {
        guard let rect = buttonScreenRect(), let button, let window = button.window else { return }
        let size = NSSize(width: 320, height: IrisHoverView.height(for: view.state))
        var origin = NSPoint(x: rect.maxX - size.width, y: rect.minY - size.height - 4)
        if let visible = (window.screen ?? NSScreen.main)?.visibleFrame {
            origin.x = min(max(origin.x, visible.minX + 2), visible.maxX - size.width - 2)
        }
        p.setFrame(NSRect(origin: origin, size: size), display: true)
        view.frame = NSRect(origin: .zero, size: size)
    }
}
