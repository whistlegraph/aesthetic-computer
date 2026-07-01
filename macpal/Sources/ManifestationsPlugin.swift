// ManifestationsPlugin — a standing list of manifestations the star cycles
// through on its own, one per hour, shown in a soft bubble beside the avatar
// in mini (collapsed) mode. Distinct from AffirmationsPlugin: that's the single
// line @jeffrey pushes live; this is Fía's own list (transcribed from her note),
// served from aesthetic.computer so it can change without a rebuild.
//
//   GET /api/manifestations?to=<recipient>  → { to, items, seq, at }
//
// The list is cached to disk so a manifestation shows instantly on launch and
// survives offline. Which one shows is deterministic from the wall clock —
// items[hoursSinceEpoch % count] — so it advances exactly on the hour and is
// stable across a relaunch within the same hour.
//
// Push the list with:  node macpal/manifest.mjs   (reads macpal/<to>-manifestations.txt)

import AppKit

final class ManifestationsPlugin: NSObject, PalPlugin {
    private let recipient: String
    private let host: String
    private let stateFile: String     // cache: one manifestation per line
    private weak var c: PalController?

    private var items: [String] = []
    private var seq = -1
    private var shownIndex = -1
    private var collapsedNow = false
    private var polling = false
    private var tickCount = 0
    private let pollEvery = 600        // core ticks ~1Hz → re-fetch the list ~every 10 min

    // The bubble is its own borderless window so it can sit OUTSIDE the star's
    // tightly-sized frame without touching PalController's layout. It follows
    // the star as a child window (drag-glued) and is repositioned on every
    // collapse/expand.
    private var bubble: NSWindow?
    private let label = NSTextField(labelWithString: "")
    private let maxBubbleW: CGFloat = 200

    init(recipient: String, host: String, supportDir: String) {
        self.recipient = recipient
        self.host = host.hasSuffix("/") ? String(host.dropLast()) : host
        self.stateFile = supportDir + "/manifestations"
        super.init()
        if let raw = try? String(contentsOfFile: stateFile, encoding: .utf8) {
            items = raw.split(separator: "\n").map(String.init).filter { !$0.isEmpty }
        }
    }

    func attach(to controller: PalController) {
        c = controller
        // Plugins attach BEFORE PalController sets up `window`, so defer any
        // window-dependent setup to the next runloop hop, by which point the
        // star's window exists.
        DispatchQueue.main.async { [weak self] in self?.setup() }
    }

    private func setup() {
        guard let controller = c, let win = controller.window else { return }
        buildBubble(over: win)
        // Re-glue the bubble whenever the star moves or resizes (corner snap,
        // collapse toggle, screen change).
        let nc = NotificationCenter.default
        nc.addObserver(self, selector: #selector(reposition),
                       name: NSWindow.didMoveNotification, object: win)
        nc.addObserver(self, selector: #selector(reposition),
                       name: NSWindow.didResizeNotification, object: win)
        refreshShown()
    }

    func setCollapsed(_ collapsed: Bool) {
        collapsedNow = collapsed
        // The star's frame settles inside this same layout pass — read it next
        // runloop hop so we glue to the final position.
        DispatchQueue.main.async { [weak self] in self?.reposition() }
    }

    func tick() {
        if tickCount % pollEvery == 0 { poll() }
        // Advance on the hour even without a fetch.
        if currentIndex() != shownIndex { refreshShown() }
        tickCount += 1
    }

    // ── rotation ─────────────────────────────────────────────────────────────
    private func currentIndex() -> Int {
        guard !items.isEmpty else { return -1 }
        let hours = Int(Date().timeIntervalSince1970 / 3600)
        return ((hours % items.count) + items.count) % items.count
    }

    private func refreshShown() {
        let i = currentIndex()
        shownIndex = i
        let text = (i >= 0 && i < items.count) ? items[i] : ""
        render(text)
        reposition()
    }

    // ── network ────────────────────────────────────────────────────────────
    private func poll() {
        guard !polling,
              let url = URL(string: "\(host)/api/manifestations?to=\(recipient)") else { return }
        polling = true
        var req = URLRequest(url: url)
        req.timeoutInterval = 12
        req.cachePolicy = .reloadIgnoringLocalCacheData
        URLSession.shared.dataTask(with: req) { [weak self] data, _, _ in
            guard let self = self else { return }
            defer { self.polling = false }
            guard let data = data,
                  let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let newSeq = (obj["seq"] as? Int) ?? (obj["seq"] as? NSNumber)?.intValue,
                  newSeq != self.seq,
                  let arr = obj["items"] as? [Any] else { return }
            let newItems = arr.compactMap { $0 as? String }.filter { !$0.isEmpty }
            DispatchQueue.main.async {
                self.seq = newSeq
                self.items = newItems
                try? newItems.joined(separator: "\n")
                    .write(toFile: self.stateFile, atomically: true, encoding: .utf8)
                self.refreshShown()
            }
        }.resume()
    }

    // ── bubble ───────────────────────────────────────────────────────────────
    private func buildBubble(over parent: NSWindow) {
        let w = NSWindow(contentRect: NSRect(x: 0, y: 0, width: maxBubbleW, height: 40),
                         styleMask: [.borderless], backing: .buffered, defer: false)
        w.isOpaque = false
        w.backgroundColor = .clear
        w.hasShadow = true
        w.level = parent.level
        w.ignoresMouseEvents = true   // pure decoration — never eats a click
        w.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]

        let bg = BubbleView()
        bg.wantsLayer = true
        label.isBordered = false
        label.drawsBackground = false
        label.isEditable = false
        label.lineBreakMode = .byWordWrapping
        label.maximumNumberOfLines = 3
        label.cell?.wraps = true
        label.alignment = .center
        bg.addSubview(label)
        w.contentView = bg
        bubble = w
        parent.addChildWindow(w, ordered: .above)
        w.orderOut(nil)
    }

    private func render(_ text: String) {
        guard !text.isEmpty else { return }
        let para = NSMutableParagraphStyle()
        para.alignment = .center
        para.lineBreakMode = .byWordWrapping
        label.attributedStringValue = NSAttributedString(string: text, attributes: [
            .font: playfulFont(13, bold: true),
            .foregroundColor: NSColor.white,
            .strokeColor: NSColor(white: 0.06, alpha: 1),
            .strokeWidth: -2.5,
            .paragraphStyle: para,
        ])
    }

    @objc private func reposition() {
        guard let bubble = bubble, let parent = c?.window else { return }
        let show = collapsedNow && shownIndex >= 0 && !items.isEmpty
        guard show else { bubble.orderOut(nil); return }

        // Size to the text (wrapped to maxBubbleW), then frame the bubble just
        // outside the star — left of an R-corner star, right of an L-corner one.
        let padX: CGFloat = 12, padY: CGFloat = 8, gap: CGFloat = 6
        let textW = maxBubbleW - padX * 2
        let bound = label.attributedStringValue.boundingRect(
            with: NSSize(width: textW, height: 200),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        let bw = ceil(min(bound.width, textW)) + padX * 2
        let bh = ceil(bound.height) + padY * 2
        label.frame = NSRect(x: padX, y: padY, width: bw - padX * 2, height: bh - padY * 2)
        if let v = bubble.contentView { v.frame = NSRect(x: 0, y: 0, width: bw, height: bh) }

        let pf = parent.frame
        let onRight = (c?.corner.hasSuffix("R")) ?? true
        let x = onRight ? pf.minX - gap - bw : pf.maxX + gap
        let y = pf.midY - bh / 2
        bubble.setFrame(NSRect(x: x, y: y, width: bw, height: bh), display: true)
        bubble.orderFront(nil)
    }
}

// A rounded, accent-bordered translucent bubble — reads as a little speech
// pill floating beside the star.
private final class BubbleView: NSView {
    override var isFlipped: Bool { false }
    override func draw(_ dirty: NSRect) {
        let r = bounds.insetBy(dx: 1, dy: 1)
        let path = NSBezierPath(roundedRect: r, xRadius: 10, yRadius: 10)
        NSColor(white: 0.10, alpha: 0.92).setFill()
        path.fill()
        accent.withAlphaComponent(0.9).setStroke()
        path.lineWidth = 1.5
        path.stroke()
    }
}
