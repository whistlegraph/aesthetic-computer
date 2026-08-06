import AppKit
import Foundation

private struct DirectorWord: Decodable {
    let text: String
    let fromMs: Double
    let toMs: Double
}

private struct DirectorState: Decodable {
    let goal: String
    let phase: String
    let status: String
    let beatIndex: Int?
    let beatCount: Int
    let currentLine: String
    let nextLine: String
    let words: [DirectorWord]
    let beatStartedAt: String?
    let updatedAt: String
}

private final class DirectorView: NSVisualEffectView {
    private let goal = NSTextField(labelWithString: "Waiting for Panda")
    private let count = NSTextField(labelWithString: "")
    private let line = NSTextField(wrappingLabelWithString: "")
    private let next = NSTextField(wrappingLabelWithString: "")
    private let direction = NSTextField(labelWithString: "PANDA LIVE")
    private var directorState: DirectorState?
    private let iso = ISO8601DateFormatter()

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        material = .hudWindow
        blendingMode = .behindWindow
        self.state = .active
        wantsLayer = true
        layer?.cornerRadius = 18
        layer?.masksToBounds = true
        layer?.borderWidth = 1
        layer?.borderColor = NSColor.white.withAlphaComponent(0.12).cgColor

        goal.font = .systemFont(ofSize: 15, weight: .semibold)
        goal.textColor = .white.withAlphaComponent(0.78)
        goal.lineBreakMode = .byTruncatingTail
        count.font = .monospacedDigitSystemFont(ofSize: 13, weight: .medium)
        count.textColor = .white.withAlphaComponent(0.42)
        count.alignment = .right
        line.font = .systemFont(ofSize: 28, weight: .semibold)
        line.textColor = .white
        line.maximumNumberOfLines = 3
        next.font = .systemFont(ofSize: 16, weight: .regular)
        next.textColor = .white.withAlphaComponent(0.48)
        next.maximumNumberOfLines = 2
        direction.font = .systemFont(ofSize: 13, weight: .semibold)
        direction.textColor = NSColor.systemPink.withAlphaComponent(0.72)
        direction.alignment = .right

        for view in [goal, count, line, next, direction] {
            view.translatesAutoresizingMaskIntoConstraints = false
            addSubview(view)
        }
        NSLayoutConstraint.activate([
            goal.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 22),
            goal.topAnchor.constraint(equalTo: topAnchor, constant: 20),
            goal.trailingAnchor.constraint(equalTo: count.leadingAnchor, constant: -14),
            count.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -22),
            count.centerYAnchor.constraint(equalTo: goal.centerYAnchor),
            count.widthAnchor.constraint(greaterThanOrEqualToConstant: 64),
            line.leadingAnchor.constraint(equalTo: goal.leadingAnchor),
            line.trailingAnchor.constraint(equalTo: count.trailingAnchor),
            line.topAnchor.constraint(equalTo: goal.bottomAnchor, constant: 18),
            next.leadingAnchor.constraint(equalTo: goal.leadingAnchor),
            next.trailingAnchor.constraint(equalTo: direction.leadingAnchor, constant: -16),
            next.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -20),
            direction.trailingAnchor.constraint(equalTo: count.trailingAnchor),
            direction.bottomAnchor.constraint(equalTo: next.bottomAnchor),
            direction.widthAnchor.constraint(equalToConstant: 84),
        ])
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

    func receive(_ nextState: DirectorState) {
        directorState = nextState
        goal.stringValue = nextState.goal
        count.stringValue = nextState.beatIndex.map { "\($0 + 1) / \(nextState.beatCount)" } ?? nextState.phase.uppercased()
        next.stringValue = nextState.nextLine.isEmpty ? "" : "↳  \(nextState.nextLine)"
        renderLine()
    }

    func renderLine() {
        guard let state = directorState else { return }
        guard !state.words.isEmpty, let stamp = state.beatStartedAt, let began = iso.date(from: stamp) else {
            line.stringValue = state.currentLine.isEmpty ? "Director channel ready." : state.currentLine
            return
        }
        let elapsed = Date().timeIntervalSince(began) * 1000
        let rendered = NSMutableAttributedString()
        for (index, word) in state.words.enumerated() {
            if index > 0 { rendered.append(NSAttributedString(string: " ")) }
            let color: NSColor
            if elapsed >= word.toMs { color = .white.withAlphaComponent(0.78) }
            else if elapsed >= word.fromMs { color = .white }
            else { color = .white.withAlphaComponent(0.30) }
            rendered.append(NSAttributedString(string: word.text, attributes: [
                .font: NSFont.systemFont(ofSize: 28, weight: .semibold),
                .foregroundColor: color,
            ]))
        }
        line.attributedStringValue = rendered
    }
}

private final class AppDelegate: NSObject, NSApplicationDelegate {
    private var panel: NSPanel!
    private var directorView: DirectorView!
    private var pollTimer: Timer?
    private var wordTimer: Timer?
    private var healthTimer: Timer?
    private var lastUpdatedAt = ""
    private var lastBroadcastAt = Date.distantPast
    private let stateURL: URL

    init(stateURL: URL) { self.stateURL = stateURL }

    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.setActivationPolicy(.accessory)
        let size = NSSize(width: 610, height: 250)
        let screen = NSScreen.main ?? NSScreen.screens[0]
        let visible = screen.visibleFrame
        let origin = NSPoint(x: visible.maxX - size.width - 24, y: visible.minY + 24)
        panel = NSPanel(
            contentRect: NSRect(origin: origin, size: size),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: false
        )
        panel.level = .floating
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = true
        panel.ignoresMouseEvents = true
        panel.hidesOnDeactivate = false
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]
        directorView = DirectorView(frame: NSRect(origin: .zero, size: size))
        panel.contentView = directorView
        panel.orderFrontRegardless()

        panel.alphaValue = 0
        fetchState()
        pollTimer = Timer.scheduledTimer(withTimeInterval: 0.35, repeats: true) { [weak self] _ in self?.fetchState() }
        wordTimer = Timer.scheduledTimer(withTimeInterval: 0.08, repeats: true) { [weak self] _ in self?.directorView.renderLine() }
        healthTimer = Timer.scheduledTimer(withTimeInterval: 0.5, repeats: true) { [weak self] _ in self?.updateVisibility() }
    }

    private func fetchState() {
        URLSession.shared.dataTask(with: stateURL) { [weak self] data, _, _ in
            guard let data, let state = try? JSONDecoder().decode(DirectorState.self, from: data) else { return }
            DispatchQueue.main.async { self?.receive(state) }
        }.resume()
    }

    private func receive(_ state: DirectorState) {
        directorView.receive(state)
        guard state.updatedAt != lastUpdatedAt else { return }
        lastUpdatedAt = state.updatedAt
        lastBroadcastAt = Date()
        panel.alphaValue = 1
        panel.orderFrontRegardless()
    }

    private func updateVisibility() {
        guard Date().timeIntervalSince(lastBroadcastAt) > 8 else { return }
        panel.alphaValue = 0
    }
}

private let url = URL(string: CommandLine.arguments.dropFirst().first ?? "http://127.0.0.1:47831/state")!
private let app = NSApplication.shared
private let delegate = AppDelegate(stateURL: url)
app.delegate = delegate
app.run()
