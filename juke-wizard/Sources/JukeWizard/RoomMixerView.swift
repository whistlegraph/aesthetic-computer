import AppKit

final class MachineBadgeView: NSView {
    let machine: String
    var detail: String = "off" { didSet { needsDisplay = true } }
    var active = false { didSet { needsDisplay = true } }

    init(machine: String) { self.machine = machine; super.init(frame: .zero); wantsLayer = true }
    required init?(coder: NSCoder) { fatalError() }

    override func draw(_ dirtyRect: NSRect) {
        let screen = NSRect(x: bounds.midX - 31, y: 32, width: 62, height: 40)
        let outline = NSBezierPath(roundedRect: screen, xRadius: 5, yRadius: 5)
        (active ? Palette.teal : NSColor.secondaryLabelColor).setStroke(); outline.lineWidth = 2; outline.stroke()
        (active ? Palette.teal.withAlphaComponent(0.22) : NSColor.black.withAlphaComponent(0.18)).setFill(); outline.fill()
        let base = NSBezierPath()
        base.move(to: NSPoint(x: screen.minX - 8, y: 28)); base.line(to: NSPoint(x: screen.maxX + 8, y: 28))
        base.line(to: NSPoint(x: screen.maxX + 3, y: 24)); base.line(to: NSPoint(x: screen.minX - 3, y: 24)); base.close()
        (active ? Palette.teal : NSColor.secondaryLabelColor).setFill(); base.fill()
        let nameStyle: [NSAttributedString.Key: Any] = [.font: NSFont.boldSystemFont(ofSize: 13), .foregroundColor: NSColor.labelColor]
        let detailStyle: [NSAttributedString.Key: Any] = [.font: NSFont.systemFont(ofSize: 10), .foregroundColor: active ? Palette.teal : NSColor.secondaryLabelColor]
        let nameSize = machine.size(withAttributes: nameStyle)
        machine.draw(at: NSPoint(x: bounds.midX - nameSize.width / 2, y: 6), withAttributes: nameStyle)
        let detailSize = detail.size(withAttributes: detailStyle)
        detail.draw(at: NSPoint(x: bounds.midX - detailSize.width / 2, y: 77), withAttributes: detailStyle)
    }
}

final class RoomMixerView: NSView {
    var onLayout: ((JukeRoomAudio.Layout) -> Void)?
    var onPan: ((Float) -> Void)?
    private let neo = MachineBadgeView(machine: "Neo")
    private let blueberry = MachineBadgeView(machine: "Blueberry")
    private let mode = NSPopUpButton()
    private let slider = NSSlider(value: 0, minValue: -1, maxValue: 1, target: nil, action: nil)
    private let panLabel = NSTextField(labelWithString: "pan · Neo  ←  center  →  Blueberry")
    private let status = NSTextField(labelWithString: "Choose how the two MacBooks play")

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        JukeRoomAudio.Layout.allCases.forEach { mode.addItem(withTitle: $0.title) }
        mode.target = self; mode.action = #selector(modeChanged)
        slider.target = self; slider.action = #selector(panChanged); slider.isContinuous = false
        panLabel.alignment = .center; panLabel.textColor = .secondaryLabelColor
        panLabel.font = .systemFont(ofSize: 10)
        status.alignment = .center; status.textColor = .secondaryLabelColor
        status.font = .systemFont(ofSize: 11)
        [neo, blueberry, mode, slider, panLabel, status].forEach(addSubview)
    }
    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        neo.frame = NSRect(x: 24, y: 110, width: 150, height: 100)
        blueberry.frame = NSRect(x: bounds.width - 174, y: 110, width: 150, height: 100)
        mode.frame = NSRect(x: 24, y: 76, width: bounds.width - 48, height: 26)
        slider.frame = NSRect(x: 31, y: 43, width: bounds.width - 62, height: 20)
        panLabel.frame = NSRect(x: 24, y: 25, width: bounds.width - 48, height: 15)
        status.frame = NSRect(x: 24, y: 5, width: bounds.width - 48, height: 16)
    }

    func show(_ state: JukeRoomAudio.State, layout: JukeRoomAudio.Layout, pan: Float) {
        mode.selectItem(at: layout.rawValue)
        slider.floatValue = pan
        slider.isEnabled = true
        switch state {
        case .idle:
            neo.active = false; blueberry.active = false
            neo.detail = "off"; blueberry.detail = "off"; status.stringValue = "room output off"
        case .failed(let message):
            status.stringValue = "⚠ \(message)"
            neo.active = false; blueberry.active = false
        case .live(let snapshot):
            neo.detail = snapshot.neo; blueberry.detail = snapshot.blueberry
            neo.active = snapshot.neo != "off"; blueberry.active = snapshot.blueberry != "off"
            status.stringValue = "\(snapshot.source.rawValue) · \(snapshot.layout.title)"
        }
    }

    @objc private func modeChanged() {
        guard let layout = JukeRoomAudio.Layout(rawValue: mode.indexOfSelectedItem) else { return }
        onLayout?(layout)
    }
    @objc private func panChanged() {
        mode.selectItem(at: JukeRoomAudio.Layout.panMono.rawValue)
        onPan?(slider.floatValue)
    }
}
