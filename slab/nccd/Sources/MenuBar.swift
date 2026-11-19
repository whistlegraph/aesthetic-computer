// MenuBar.swift — NCCd in the macOS menu bar, and its notifications.
//
// The item is the wordmark, small. A press brings the window forward. With the
// pointer over it, a card drops down: each chat with what is unread, and a
// graph of the club's messages over the last day, an hour a bar.
//
// Notifications: a new message in a club chat is a macOS notification (the
// chat's name, who, and the words), so the club is heard with the window shut
// and with no other Signal app open. A press on one opens that chat. Nothing
// leaves this Mac: the notification is made here, from the message as it
// arrives.

import AppKit
import UserNotifications

struct MenuStats {
    var rows: [(title: String, color: NSColor, unread: Int)] = []
    var hours: [Int] = Array(repeating: 0, count: 24)        // messages in each of the last 24 hours, oldest first
}

final class MenuBar: NSObject, UNUserNotificationCenterDelegate {
    private var item: NSStatusItem?
    private var card: NSPanel?
    var onOpen: (() -> Void)?
    var onOpenChat: ((String) -> Void)?           // by group id
    var stats: (() -> MenuStats)?

    func install() {
        let item = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        item.button?.image = Self.wordmark()
        item.button?.toolTip = nil
        item.button?.target = self
        item.button?.action = #selector(open)
        if let b = item.button {
            b.addTrackingArea(NSTrackingArea(rect: .zero, options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect], owner: self))
        }
        self.item = item
        UNUserNotificationCenter.current().delegate = self
    }
    func remove() {
        hide()
        if let item { NSStatusBar.system.removeStatusItem(item) }
        item = nil
    }
    @objc private func open() { hide(); onOpen?() }

    // The wordmark in pixels, two points a pixel, in its own colours.
    static func wordmark() -> NSImage {
        let c: CGFloat = 2
        let img = NSImage(size: NSSize(width: 24 * c, height: 7 * c + 2), flipped: true) { _ in
            NSGraphicsContext.current?.shouldAntialias = false
            var x0 = 0
            for letter in NCCLogo.letters {
                for (y, row) in letter.rows.enumerated() {
                    for (x, ch) in row.enumerated() where ch == "X" {
                        let stem = letter.pink && (y < 2 || (x == 4 && (y == 2 || y == 6)))
                        (!letter.pink ? NSColor(hex: "00B870")! : stem ? NSColor(hex: "1E9FDB")! : NSColor(hex: "3CCBF0")!).setFill()
                        NSRect(x: CGFloat(x0 + x) * c, y: 1 + CGFloat(y) * c, width: c, height: c).fill()
                    }
                }
                x0 += letter.rows[0].count + 1
            }
            return true
        }
        return img
    }

    // ─── the hover card ──────────────────────────────────────────────────────

    @objc func mouseEntered(with event: NSEvent) { show() }
    @objc func mouseExited(with event: NSEvent) { hide() }

    private func show() {
        guard let button = item?.button, let window = button.window, let s = stats?() else { return }
        let view = Card(s)
        let panel = NSPanel(contentRect: NSRect(origin: .zero, size: view.frame.size), styleMask: [.borderless, .nonactivatingPanel],
                            backing: .buffered, defer: false)
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = true
        panel.level = .statusBar
        panel.ignoresMouseEvents = true                  // it never takes a press or the keyboard
        panel.contentView = view
        let at = window.convertToScreen(button.convert(button.bounds, to: nil))
        panel.setFrameOrigin(NSPoint(x: min(at.minX, (window.screen?.visibleFrame.maxX ?? at.maxX) - view.frame.width - 6), y: at.minY - view.frame.height - 4))
        panel.orderFront(nil)
        card?.orderOut(nil)
        card = panel
    }
    private func hide() { card?.orderOut(nil); card = nil }

    private final class Card: NSView {
        let s: MenuStats
        override var isFlipped: Bool { true }
        init(_ s: MenuStats) {
            self.s = s
            super.init(frame: NSRect(x: 0, y: 0, width: 230, height: 62 + CGFloat(max(1, s.rows.count)) * 17))
        }
        required init?(coder: NSCoder) { fatalError() }
        override func draw(_ dirty: NSRect) {
            NSColor(hex: "0B2A38")!.setFill()
            NSBezierPath(roundedRect: bounds, xRadius: 8, yRadius: 8).fill()
            let small = NSFont.monospacedSystemFont(ofSize: 10, weight: .regular), bold = NSFont.monospacedSystemFont(ofSize: 10, weight: .bold)
            let faint = NSColor(white: 1, alpha: 0.6)
            var y: CGFloat = 9
            if s.rows.isEmpty {
                ("Link Signal to see the club here" as NSString).draw(at: NSPoint(x: 10, y: y), withAttributes: [.font: small, .foregroundColor: faint])
                y += 17
            }
            for r in s.rows {
                r.color.setFill()
                NSRect(x: 10, y: y + 2, width: 8, height: 8).fill()
                (r.title as NSString).draw(at: NSPoint(x: 24, y: y), withAttributes: [.font: bold, .foregroundColor: NSColor.white])
                let n = (r.unread > 0 ? "\(r.unread) new" : "·") as NSString
                let w = n.size(withAttributes: [.font: small]).width
                n.draw(at: NSPoint(x: bounds.width - 10 - w, y: y), withAttributes: [.font: small, .foregroundColor: r.unread > 0 ? NSColor(hex: "F3D34A")! : faint])
                y += 17
            }
            // the last day, an hour a bar
            y += 4
            let top = y, high: CGFloat = 26
            let most = CGFloat(max(1, s.hours.max() ?? 1))
            let step = (bounds.width - 20) / 24
            for (i, n) in s.hours.enumerated() {
                let h = n == 0 ? 1 : max(2, high * CGFloat(n) / most)
                (n == 0 ? NSColor(white: 1, alpha: 0.18) : NSColor(hex: "3CCBF0")!).setFill()
                NSRect(x: 10 + CGFloat(i) * step, y: top + high - h, width: step - 1.5, height: h).fill()
            }
            let total = s.hours.reduce(0, +)
            ("\(total) message\(total == 1 ? "" : "s") in the last 24 hours" as NSString)
                .draw(at: NSPoint(x: 10, y: top + high + 4), withAttributes: [.font: small, .foregroundColor: faint])
        }
    }

    // ─── notifications ───────────────────────────────────────────────────────

    private var asked = false
    func askToNotify() {
        guard !asked else { return }
        asked = true
        UNUserNotificationCenter.current().requestAuthorization(options: [.alert, .sound, .badge]) { _, _ in }
    }

    func notify(chat: String, groupId: String, who: String, words: String) {
        let c = UNMutableNotificationContent()
        c.title = chat
        c.subtitle = who
        c.body = words
        c.sound = .default
        c.threadIdentifier = groupId                    // macOS groups them by chat
        c.userInfo = ["group": groupId]
        UNUserNotificationCenter.current().add(UNNotificationRequest(identifier: UUID().uuidString, content: c, trigger: nil))
    }

    // Shown also when NCCd is in front, unless it is the chat that is open (the app decides that before it calls notify).
    func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification,
                                withCompletionHandler done: @escaping (UNNotificationPresentationOptions) -> Void) {
        done([.banner, .sound, .list])
    }
    func userNotificationCenter(_ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse,
                                withCompletionHandler done: @escaping () -> Void) {
        if let g = response.notification.request.content.userInfo["group"] as? String {
            DispatchQueue.main.async { self.onOpen?(); self.onOpenChat?(g) }
        }
        done()
    }
}
