import AppKit

/// A readable selection confirmation without taking the playing keyboard.
final class SoundSelectionHUD {
    private let panel: NSPanel
    private let label = NSTextField(labelWithString: "")
    private var dismiss: DispatchWorkItem?

    init() {
        panel = NSPanel(contentRect: NSRect(x: 0, y: 0, width: 440, height: 62),
                        styleMask: [.borderless, .nonactivatingPanel],
                        backing: .buffered, defer: true)
        panel.level = .floating
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .transient]
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = true
        panel.ignoresMouseEvents = true
        panel.hidesOnDeactivate = false
        panel.isReleasedWhenClosed = false
        let plate = NSView(frame: NSRect(origin: .zero, size: panel.frame.size))
        plate.wantsLayer = true
        plate.layer?.cornerRadius = 12
        plate.layer?.backgroundColor = NSColor(white: 0.08, alpha: 0.96).cgColor
        label.font = .systemFont(ofSize: 24, weight: .semibold)
        label.textColor = .white
        label.alignment = .center
        label.translatesAutoresizingMaskIntoConstraints = false
        plate.addSubview(label)
        NSLayoutConstraint.activate([
            label.centerYAnchor.constraint(equalTo: plate.centerYAnchor),
            label.leadingAnchor.constraint(equalTo: plate.leadingAnchor, constant: 16),
            label.trailingAnchor.constraint(equalTo: plate.trailingAnchor, constant: -16),
        ])
        panel.contentView = plate
    }

    func show(_ text: String, on screen: NSScreen?) {
        dismiss?.cancel()
        label.stringValue = text
        panel.setAccessibilityLabel(text)
        let visible = (screen ?? NSScreen.main)?.visibleFrame ?? .zero
        let width = min(max(360, label.intrinsicContentSize.width + 40), visible.width - 24)
        panel.setFrame(NSRect(x: visible.midX - width / 2, y: visible.maxY - 88,
                              width: width, height: 62), display: true)
        panel.orderFrontRegardless()
        let work = DispatchWorkItem { [weak self] in self?.panel.orderOut(nil) }
        dismiss = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 2, execute: work)
    }
}
