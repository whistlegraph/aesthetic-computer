import AppKit
import SwiftUI

struct StatusBand: NSViewRepresentable {
    let game: GameStore
    let content: Content

    struct Field {
        let text: String
        var size: CGFloat = 12
        var weight: NSFont.Weight = .regular
        var numeric = false

        var attributed: NSAttributedString {
            let font = numeric ? NSFont.monospacedDigitSystemFont(ofSize: size, weight: weight) : NSFont.systemFont(ofSize: size, weight: weight)
            return NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: NSColor.labelColor])
        }
    }
    struct Content {
        let groups: [[Field]]
        var context: String? = nil
        var text: String { (groups.flatMap { $0.map(\.text) } + (context.map { [$0] } ?? [])).joined(separator: " · ") }
    }
    struct Run {
        let text: NSAttributedString
        let frame: NSRect
    }
    struct Layout {
        let runs: [Run]
        let height: CGFloat
    }

    static let lineHeight: CGFloat = 33

    static func groupTitle(_ fields: [Field]) -> NSAttributedString {
        let title = NSMutableAttributedString(string: "")
        for (index, field) in fields.enumerated() {
            if index > 0 {
                title.append(NSAttributedString(string: " ", attributes: [.font: NSFont.systemFont(ofSize: 12), .kern: 5]))
            }
            title.append(field.attributed)
        }
        let paragraph = NSMutableParagraphStyle()
        paragraph.alignment = .left
        paragraph.lineBreakMode = .byTruncatingTail
        paragraph.minimumLineHeight = 17
        paragraph.maximumLineHeight = 17
        title.addAttribute(.paragraphStyle, value: paragraph, range: NSRange(location: 0, length: title.length))
        return title
    }

    static func layout(_ content: Content, width: CGFloat) -> Layout {
        let inset: CGFloat = min(12, width / 8), gap: CGFloat = 16
        let titles = content.groups.filter { !$0.isEmpty }.map { groupTitle($0) }
        guard !titles.isEmpty else { return Layout(runs: [], height: lineHeight) }
        let spacing = min(gap, max(0, width - inset * 2) / CGFloat(titles.count * 4))
        let available = max(0, width - inset * 2 - spacing * CGFloat(titles.count - 1))
        var widths = titles.map { ceil($0.size().width) }
        var excess = max(0, widths.reduce(0, +) - available)
        // The model takes the remaining space; preserve the account and
        // connection status as long as possible. All text shares one baseline.
        for index in widths.indices {
            let reduction = min(excess, max(0, widths[index] - 64))
            widths[index] -= reduction; excess -= reduction
        }
        if excess > 0 {
            let scale = available / max(1, widths.reduce(0, +))
            widths = widths.map { $0 * scale }
        }
        let extra = titles.count > 1 ? max(0, available - widths.reduce(0, +)) / CGFloat(titles.count - 1) : 0
        var runs = [Run](), x = inset
        for index in titles.indices {
            runs.append(Run(text: titles[index], frame: NSRect(x: x, y: 8, width: widths[index], height: 17)))
            x += widths[index] + spacing + extra
        }
        return Layout(runs: runs, height: lineHeight)
    }

    static func height(_ content: Content, width: CGFloat) -> CGFloat {
        lineHeight
    }

    func makeCoordinator() -> Coordinator { Coordinator(game) }
    func makeNSView(context: Context) -> BandButton {
        let button = BandButton()
        button.isBordered = false
        button.setButtonType(.momentaryChange)
        button.target = context.coordinator
        button.action = #selector(Coordinator.open(_:))
        button.setAccessibilityLabel("Model and generation status")
        return button
    }
    func updateNSView(_ button: BandButton, context: Context) {
        button.content = content
        button.title = content.text
        button.toolTip = content.text + ((game.statusIssue?.detail ?? game.state?.account.remote_status).map { "\n" + $0 } ?? "")
        button.setAccessibilityValue(content.text)
        button.needsDisplay = true
    }
    func sizeThatFits(_ proposal: ProposedViewSize, nsView: BandButton, context: Context) -> CGSize? {
        guard let width = proposal.width else { return nil }
        return CGSize(width: width, height: Self.height(content, width: width))
    }

    final class BandButton: NSButton {
        var content = Content(groups: [])
        private var tracking: NSTrackingArea?
        private var hovering = false
        override var isFlipped: Bool { true }
        override var intrinsicContentSize: NSSize { NSSize(width: NSView.noIntrinsicMetric, height: NSView.noIntrinsicMetric) }
        override func viewDidChangeEffectiveAppearance() {
            super.viewDidChangeEffectiveAppearance()
            needsDisplay = true
        }
        override func resetCursorRects() {
            super.resetCursorRects()
            addCursorRect(bounds, cursor: .pointingHand)
        }
        override func updateTrackingAreas() {
            if let tracking { removeTrackingArea(tracking) }
            let next = NSTrackingArea(rect: .zero, options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect], owner: self)
            tracking = next; addTrackingArea(next)
            super.updateTrackingAreas()
        }
        override func mouseEntered(with event: NSEvent) { hovering = true; needsDisplay = true }
        override func mouseExited(with event: NSEvent) { hovering = false; needsDisplay = true }
        override func draw(_ dirtyRect: NSRect) {
            if isHighlighted || hovering { NSColor.labelColor.withAlphaComponent(isHighlighted ? 0.12 : 0.06).setFill(); bounds.fill() }
            for run in StatusBand.layout(content, width: bounds.width).runs {
                run.text.draw(with: run.frame, options: [.usesLineFragmentOrigin, .truncatesLastVisibleLine])
            }
        }
    }

    @MainActor final class Coordinator: NSObject {
        let game: GameStore
        init(_ game: GameStore) { self.game = game }
        @objc func open(_ sender: NSButton) {
            let menu = NSMenu()
            menu.autoenablesItems = false
            if let issue = game.statusIssue {
                let details = NSMenuItem()
                let label = NSTextField(wrappingLabelWithString: issue.detail)
                label.font = NSFont.systemFont(ofSize: 12)
                label.textColor = .secondaryLabelColor
                let height = ceil((issue.detail as NSString).boundingRect(with: NSSize(width: 300, height: 500), options: [.usesLineFragmentOrigin, .usesFontLeading], attributes: [.font: label.font!]).height)
                let block = NSView(frame: NSRect(x: 0, y: 0, width: 324, height: height + 20))
                label.frame = NSRect(x: 12, y: 10, width: 300, height: height)
                block.addSubview(label); details.view = block; menu.addItem(details)
                let title = issue.recovery == "account" ? "Retry connection" : issue.recovery == "done" ? "Retry Done" : issue.recovery == "dismiss" ? "Dismiss" : "Retry"
                let retry = menu.addItem(withTitle: title, action: #selector(recover(_:)), keyEquivalent: "")
                retry.target = self
                retry.isEnabled = issue.recovery == "done" ? game.canDone : issue.recovery == "account" ? game.state?.account.working != true : !game.sending
                menu.addItem(.separator())
            }
            func model(_ title: String, id: String, in parent: NSMenu? = nil) {
                let item = (parent ?? menu).addItem(withTitle: title, action: #selector(choose(_:)), keyEquivalent: "")
                item.target = self; item.representedObject = id
                item.isEnabled = game.canChange
                item.state = (game.state?.selection ?? game.state?.engine) == id ? .on : .off
            }
            model("Random", id: "random")
            // Keep configured choices in reach. The full catalog remains in
            // Browse OpenRouter, including unconfigured models for selection.
            let choices = (game.state?.models ?? []).filter { $0.paintPrice != nil || $0.id == game.state?.engine }
                .sorted { ($0.paintPrice ?? Int.max) < ($1.paintPrice ?? Int.max) }
            func add(_ option: Model, to parent: NSMenu? = nil) {
                let cost = option.paintPrice.map { " · \($0.formatted()) braincells/Paint · \(game.paintsLeft($0)) paints left" } ?? " · not enabled on AC"
                let unavailable = !option.available && option.paintPrice != nil ? " · unavailable" : ""
                model(option.name + (parent == nil ? " · " + option.location : "") + cost + unavailable, id: option.id, in: parent)
            }
            for option in choices where !option.id.hasPrefix("ac-openrouter:") { add(option) }
            let remote = choices.filter { $0.id.hasPrefix("ac-openrouter:") }
            if remote.count > 8 {
                menu.addItem(.separator())
                let names = ["openai":"OpenAI", "google":"Google", "black-forest-labs":"FLUX", "bytedance-seed":"Seedream", "x-ai":"Grok", "microsoft":"Microsoft", "recraft":"Recraft", "sourceful":"Riverflow", "qwen":"Qwen", "krea":"Krea", "tencent":"Tencent", "inclusionai":"Ming"]
                let groups = Dictionary(grouping: remote) { String($0.model.split(separator: "/").first ?? "") }
                for key in groups.keys.sorted(by: { (names[$0] ?? $0).localizedStandardCompare(names[$1] ?? $1) == .orderedAscending }) {
                    let item = menu.addItem(withTitle: names[key] ?? key, action: nil, keyEquivalent: "")
                    let submenu = NSMenu(title: item.title); submenu.autoenablesItems = false; item.submenu = submenu
                    let options = groups[key] ?? []
                    item.state = options.contains(where: { $0.id == game.state?.selection }) ? .on : .off
                    for option in options { add(option, to: submenu) }
                }
            } else { for option in remote { add(option) } }
            menu.addItem(.separator())
            if game.cloudUnavailable {
                let status = menu.addItem(withTitle: "AC cloud unavailable", action: nil, keyEquivalent: "")
                status.isEnabled = false
                status.toolTip = game.state?.account.remote_status
            }
            let browse = menu.addItem(withTitle: "Browse OpenRouter…", action: #selector(browse(_:)), keyEquivalent: "")
            browse.target = self
            if game.state?.account.connected != true {
                let signIn = menu.addItem(withTitle: game.state?.account.working == true ? "Signing in…" : "Sign in", action: #selector(signIn(_:)), keyEquivalent: "")
                signIn.target = self; signIn.isEnabled = game.state != nil && game.state?.account.working != true
            }
            menu.popUp(positioning: nil, at: NSPoint(x: 0, y: sender.bounds.height), in: sender)
        }
        @objc func choose(_ item: NSMenuItem) {
            if game.canChange, let id = item.representedObject as? String { game.act("engine", extra: ["engine": id]) }
        }
        @objc func browse(_ sender: Any?) { game.browse() }
        @objc func signIn(_ sender: Any?) { game.account("connect") }
        @objc func recover(_ sender: Any?) { game.recoverStatus() }
    }
}
