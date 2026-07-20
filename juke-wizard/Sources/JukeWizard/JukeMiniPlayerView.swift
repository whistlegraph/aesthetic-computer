import AppKit

final class JukeMiniPlayerView: NSView {
    weak var controller: JukeController?
    private let art = NSImageView()
    private let title = NSTextField(labelWithString: "JukeWizard")
    private let subtitle = NSTextField(labelWithString: "")
    private let room = NSTextField(labelWithString: "")
    private let previous = NSButton(title: "⏮", target: nil, action: nil)
    private let play = NSButton(title: "▶", target: nil, action: nil)
    private let next = NSButton(title: "⏭", target: nil, action: nil)
    private let open = NSButton(title: "Full Player", target: nil, action: nil)
    private let volume = NSSlider(value: 0.8, minValue: 0, maxValue: 1, target: nil, action: nil)
    private let volumeIcon = NSTextField(labelWithString: "◖)))")

    init(controller: JukeController) {
        self.controller = controller
        super.init(frame: NSRect(x: 0, y: 0, width: 370, height: 170))
        wantsLayer = true
        title.font = .boldSystemFont(ofSize: 16)
        title.textColor = Palette.gold
        title.lineBreakMode = .byTruncatingTail
        subtitle.font = .systemFont(ofSize: 11)
        subtitle.textColor = .secondaryLabelColor
        subtitle.lineBreakMode = .byTruncatingTail
        room.font = .systemFont(ofSize: 10)
        room.textColor = Palette.teal
        room.lineBreakMode = .byTruncatingTail
        art.imageScaling = .scaleProportionallyUpOrDown
        [previous, play, next, open].forEach { $0.bezelStyle = .rounded }
        previous.target = controller; previous.action = #selector(JukeController.quickPrevious)
        play.target = controller; play.action = #selector(JukeController.quickTogglePlay)
        next.target = controller; next.action = #selector(JukeController.quickNext)
        open.target = controller; open.action = #selector(JukeController.quickOpenFull)
        volume.target = controller; volume.action = #selector(JukeController.quickVolumeChanged(_:))
        volume.isContinuous = false
        volumeIcon.textColor = .secondaryLabelColor
        [art, title, subtitle, room, previous, play, next, open, volume, volumeIcon].forEach(addSubview)
        refresh()
    }
    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        art.frame = NSRect(x: 16, y: 72, width: 82, height: 82)
        title.frame = NSRect(x: 112, y: 127, width: bounds.width - 128, height: 22)
        subtitle.frame = NSRect(x: 112, y: 105, width: bounds.width - 128, height: 18)
        room.frame = NSRect(x: 112, y: 83, width: bounds.width - 128, height: 17)
        previous.frame = NSRect(x: 112, y: 47, width: 42, height: 28)
        play.frame = NSRect(x: 160, y: 47, width: 48, height: 28)
        next.frame = NSRect(x: 214, y: 47, width: 42, height: 28)
        open.frame = NSRect(x: 262, y: 47, width: 94, height: 28)
        volumeIcon.frame = NSRect(x: 18, y: 18, width: 42, height: 18)
        volume.frame = NSRect(x: 58, y: 17, width: bounds.width - 76, height: 20)
    }

    func refresh() {
        guard let controller else { return }
        title.stringValue = controller.quickTitle
        subtitle.stringValue = controller.quickSubtitle
        room.stringValue = controller.quickRoomSummary
        play.title = controller.quickIsPlaying ? "❚❚" : "▶"
        volume.floatValue = controller.quickVolume
        if let cover = controller.currentArt {
            art.image = CDArtworkRenderer.disc(from: cover, side: 82, shadow: true)
        } else {
            art.image = NSImage(systemSymbolName: "music.note", accessibilityDescription: "Music")
        }
    }
}
