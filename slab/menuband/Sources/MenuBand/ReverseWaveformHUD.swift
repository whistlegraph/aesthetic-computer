import AppKit

/// A momentary, non-activating tape monitor. Space owns its visibility: the
/// panel appears for the reverse dive and dissolves after release without
/// moving focus away from the app being played.
///
/// Two stacked displays, one story: the TOP strip is the tape — the exact
/// reversed clip, frozen, with the orange needle diving right→left across
/// it. The BOTTOM strip is the record meter — a live red sweep of what you
/// are playing RIGHT NOW, which `feed` is printing backwards onto the tape
/// under that same needle (the reverse dub). Sound in the bottom bar while
/// the needle moves above it = new material landing on the tape.
final class ReverseWaveformHUD {
    private let panel: NSPanel
    private let waveform: WaveformStripView
    private let dubMeter: WaveformStripView
    private weak var menuBand: MenuBandController?
    private var hideGeneration: UInt64 = 0

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand

        waveform = WaveformStripView(frame: NSRect(x: 7, y: 33, width: 346, height: 44))
        waveform.menuBand = menuBand
        waveform.tintColor = NSColor.systemOrange
        waveform.hoverColorFeedbackEnabled = false
        waveform.usesReverseClipProgress = true
        waveform.usesDisplayLink = true
        waveform.isLive = false

        dubMeter = WaveformStripView(frame: NSRect(x: 7, y: 7, width: 346, height: 22))
        dubMeter.menuBand = menuBand
        dubMeter.tintColor = NSColor.systemRed
        dubMeter.hoverColorFeedbackEnabled = false
        dubMeter.usesDisplayLink = true
        dubMeter.isLive = false
        // The live input alone — the tape tap sits upstream of the reverse
        // player, so the dive's own sound can't paint itself into this meter.
        dubMeter.externalLevelSource = { [weak menuBand] in
            menuBand?.rewindLiveInputPeak() ?? 0
        }

        panel = NSPanel(
            contentRect: NSRect(x: 0, y: 0, width: 360, height: 84),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: true
        )
        panel.level = .floating
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .transient]
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = true
        panel.ignoresMouseEvents = true
        panel.hidesOnDeactivate = false
        panel.isReleasedWhenClosed = false
        panel.alphaValue = 0

        let plate = NSView(frame: NSRect(origin: .zero, size: panel.frame.size))
        plate.wantsLayer = true
        plate.layer?.cornerRadius = 9
        plate.layer?.masksToBounds = true
        plate.layer?.backgroundColor = NSColor(srgbRed: 0.025, green: 0.03, blue: 0.03, alpha: 0.94).cgColor
        plate.addSubview(waveform)
        plate.addSubview(dubMeter)
        panel.contentView = plate
    }

    func show(levels: [Float], above trackDrumFrame: NSRect?, on screen: NSScreen?) {
        hideGeneration &+= 1
        position(above: trackDrumFrame, on: screen)
        waveform.seedWaveform(levels: levels, cursorAt: 1)
        waveform.isLive = true
        // Fresh sweep each dive, and drop any peak that accumulated between
        // dives so old sound can't flash into the first meter column.
        dubMeter.resetRaster()
        _ = menuBand?.rewindLiveInputPeak()
        dubMeter.isLive = true
        panel.orderFrontRegardless()
        NSAnimationContext.runAnimationGroup { context in
            context.duration = 0.07
            context.timingFunction = CAMediaTimingFunction(name: .easeOut)
            panel.animator().alphaValue = 1
        }
    }

    func hide() {
        hideGeneration &+= 1
        let generation = hideGeneration
        NSAnimationContext.runAnimationGroup({ context in
            context.duration = 0.12
            context.timingFunction = CAMediaTimingFunction(name: .easeIn)
            panel.animator().alphaValue = 0
        }, completionHandler: { [weak self] in
            guard let self, self.hideGeneration == generation else { return }
            self.panel.orderOut(nil)
            self.waveform.isLive = false
            self.dubMeter.isLive = false
        })
    }

    private func position(above trackDrumFrame: NSRect?, on screen: NSScreen?) {
        let size = panel.frame.size
        let visible = (screen ?? NSScreen.main)?.visibleFrame ?? .zero
        let x = visible.midX - size.width / 2
        let trackTop = trackDrumFrame?.maxY ?? (visible.minY + 126)
        let y = min(visible.maxY - size.height - 8, trackTop + 12)
        panel.setFrameOrigin(NSPoint(x: x, y: y))
    }
}
