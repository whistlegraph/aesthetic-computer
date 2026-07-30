import AppKit
import AVFoundation

final class DJFocusFlash: NSPanel {
    static let shared = DJFocusFlash()

    private init() {
        super.init(contentRect: .zero, styleMask: [.borderless, .nonactivatingPanel],
                   backing: .buffered, defer: false)
        isOpaque = false
        backgroundColor = .clear
        hasShadow = false
        level = .screenSaver
        ignoresMouseEvents = true
        hidesOnDeactivate = false
        alphaValue = 0
        collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]
        let view = NSView()
        view.wantsLayer = true
        contentView = view
    }

    func flash(rising: Bool) {
        guard let screen = NSScreen.main, let layer = contentView?.layer else { return }
        setFrame(screen.frame, display: true)
        layer.removeAllAnimations()
        layer.backgroundColor = (rising
            ? NSColor(srgbRed: 0.06, green: 0.42, blue: 1, alpha: 1)
            : NSColor(srgbRed: 1, green: 0.12, blue: 0.16, alpha: 1)).cgColor
        alphaValue = 0.42
        orderFrontRegardless()
        NSAnimationContext.runAnimationGroup({ context in
            context.duration = 0.17
            context.timingFunction = CAMediaTimingFunction(name: .easeOut)
            animator().alphaValue = 0
        }, completionHandler: { [weak self] in self?.orderOut(nil) })
    }
}

final class DJFocusDing {
    static let shared = DJFocusDing()
    private let engine = AVAudioEngine()
    private let player = AVAudioPlayerNode()
    private let sampleRate = 44_100.0
    private var started = false

    private init() {
        engine.attach(player)
        engine.connect(player, to: engine.mainMixerNode,
                       format: AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 1))
        engine.prepare()
    }

    func play(rising: Bool) {
        if !started {
            guard (try? engine.start()) != nil else { return }
            started = true
        }
        let frames = AVAudioFrameCount(sampleRate * 0.12)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 1),
              let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
              let data = buffer.floatChannelData?[0] else { return }
        buffer.frameLength = frames
        let frequency = rising ? 1318.5 : 880.0
        for frame in 0..<Int(frames) {
            let time = Double(frame) / sampleRate
            let envelope = exp(-Double(frame) / Double(frames) * 5)
            let phase = Double.pi * 2 * frequency * time
            data[frame] = Float((sin(phase) + 0.18 * sin(phase * 3)) * envelope * 0.22)
        }
        player.scheduleBuffer(buffer)
        if !player.isPlaying { player.play() }
    }
}
