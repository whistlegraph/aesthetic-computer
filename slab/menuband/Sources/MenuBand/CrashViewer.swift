import AppKit

/// Scrollable viewer for the .ips diagnostic reports waiting in
/// `~/Library/Logs/DiagnosticReports/`. Opened from the About
/// window's "View N crashes" button so users can read what they
/// are about to send before clicking the upload button at the
/// bottom of this same window.
final class CrashViewerWindowController: NSWindowController, NSWindowDelegate {
    private let logs: [URL]
    private weak var sendButton: NSButton?
    private weak var statusLabel: NSTextField?

    init(logs: [URL]) {
        self.logs = logs
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 640, height: 520),
            styleMask: [.titled, .closable, .resizable, .miniaturizable],
            backing: .buffered,
            defer: false
        )
        window.title = L("popover.about.crash.viewerTitle")
        window.isReleasedWhenClosed = false
        // Float over the popover/About chrome so the user can keep
        // scrolling while the rest of the app stays interactive.
        // Ordinary window level. These secondary windows used to sit at
        // popUpMenu + 1 so the status-bar popover couldn't bury them — but that
        // floated them above every other app on the Mac for as long as they
        // stayed open. Each one activates the app and orders front when shown,
        // which lifts it over the popover at the only moment that matters.
        window.level = .normal
        super.init(window: window)
        window.delegate = self
        buildContent()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) is not used")
    }

    func present() {
        guard let window = window else { return }
        window.center()
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    // MARK: - Layout

    private func buildContent() {
        guard let window = window else { return }
        let content = NSView()
        window.contentView = content

        // Bottom row — Send to Aesthetic.Computer. Built first so the
        // scroll view above can pin its bottom anchor to the row's top.
        let send = NSButton(title: L("popover.about.crash.sendToAC"),
                            target: self,
                            action: #selector(sendCrashLogs(_:)))
        send.bezelStyle = .rounded
        send.controlSize = .regular
        send.translatesAutoresizingMaskIntoConstraints = false
        sendButton = send

        let status = NSTextField(labelWithString: countLine())
        status.font = NSFont.systemFont(ofSize: 11)
        status.textColor = .secondaryLabelColor
        status.translatesAutoresizingMaskIntoConstraints = false
        statusLabel = status

        content.addSubview(send)
        content.addSubview(status)

        // Scroll view holding the read-only text view. NSTextView
        // inside an NSScrollView is the standard recipe for a
        // scrollable text panel — the call to make the scroll view
        // sets the document view to a configured NSTextView for us.
        let scroll = NSScrollView()
        scroll.translatesAutoresizingMaskIntoConstraints = false
        scroll.hasVerticalScroller = true
        scroll.hasHorizontalScroller = false
        scroll.autohidesScrollers = false
        scroll.borderType = .bezelBorder
        scroll.drawsBackground = true

        let textView = NSTextView()
        textView.isEditable = false
        textView.isSelectable = true
        textView.isRichText = false
        textView.allowsUndo = false
        textView.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .regular)
        textView.textContainerInset = NSSize(width: 8, height: 8)
        textView.minSize = NSSize(width: 0, height: 0)
        textView.maxSize = NSSize(width: CGFloat.greatestFiniteMagnitude,
                                  height: CGFloat.greatestFiniteMagnitude)
        textView.isVerticallyResizable = true
        textView.isHorizontallyResizable = false
        textView.autoresizingMask = [.width]
        textView.textContainer?.widthTracksTextView = true
        textView.textContainer?.containerSize = NSSize(
            width: 0, height: CGFloat.greatestFiniteMagnitude)
        textView.string = compose(logs: logs)

        scroll.documentView = textView
        content.addSubview(scroll)

        NSLayoutConstraint.activate([
            scroll.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            scroll.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),
            scroll.topAnchor.constraint(equalTo: content.topAnchor, constant: 16),
            scroll.bottomAnchor.constraint(equalTo: send.topAnchor, constant: -12),

            status.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            status.centerYAnchor.constraint(equalTo: send.centerYAnchor),

            send.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),
            send.bottomAnchor.constraint(equalTo: content.bottomAnchor, constant: -16),
        ])
    }

    /// Concatenate every .ips file with a clear separator so the user
    /// can scroll through them as one document. Each section starts
    /// with the filename + a divider so the boundary between reports
    /// is unmistakable.
    private func compose(logs: [URL]) -> String {
        var parts: [String] = []
        for url in logs {
            let header = "═══ \(url.lastPathComponent) ═══"
            let body: String
            if let text = try? String(contentsOf: url, encoding: .utf8) {
                body = text
            } else {
                body = "(unable to read \(url.path))"
            }
            parts.append("\(header)\n\n\(body)")
        }
        return parts.joined(separator: "\n\n\n")
    }

    private func countLine() -> String {
        return logs.count == 1
            ? L("popover.about.crash.viewOne")
            : L("popover.about.crash.viewMany", String(logs.count))
    }

    // MARK: - Send

    /// Same upload flow the About window's standalone Send button
    /// uses — kept locally here so users can review the report text
    /// in the scroll panel above before committing.
    @objc private func sendCrashLogs(_ sender: NSButton) {
        guard !logs.isEmpty else { return }
        sender.isEnabled = false
        sender.title = L("popover.about.crash.sending")

        let version = (Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String) ?? "?"
        var remaining = logs.count
        var ok = 0
        for log in logs {
            CrashLogReader.upload(log, version: version) { [weak self] success, _ in
                guard let self = self,
                      let btn = self.sendButton else { return }
                if success { ok += 1 }
                remaining -= 1
                if remaining == 0 {
                    btn.title = ok == self.logs.count
                        ? L("popover.about.crash.sentAll")
                        : L("popover.about.crash.sentSome",
                            String(ok), String(self.logs.count))
                    btn.isEnabled = ok != self.logs.count
                }
            }
        }
    }
}
