import AppKit
import CoreGraphics

private let cabinetWidth: CGFloat = 920
private let cabinetHeight: CGFloat = 720
private let screenWidth: CGFloat = 768
private let screenHeight: CGFloat = 448
private let screenLeft: CGFloat = 76
private let screenTop: CGFloat = 92

final class CabinetView: NSView {
    override var isFlipped: Bool { true }

    private func rounded(_ rect: NSRect, radius: CGFloat, color: NSColor) {
        color.setFill()
        NSBezierPath(roundedRect: rect, xRadius: radius, yRadius: radius).fill()
    }

    private func text(_ value: String, at point: NSPoint, size: CGFloat,
                      color: NSColor = .white, weight: NSFont.Weight = .bold) {
        let attributes: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: size, weight: weight),
            .foregroundColor: color,
        ]
        value.draw(at: point, withAttributes: attributes)
    }

    private func centered(_ value: String, y: CGFloat, size: CGFloat,
                          color: NSColor = .white, weight: NSFont.Weight = .bold) {
        let attributes: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: size, weight: weight),
            .foregroundColor: color,
        ]
        let width = value.size(withAttributes: attributes).width
        value.draw(at: NSPoint(x: (bounds.width - width) / 2, y: y), withAttributes: attributes)
    }

    private func key(_ label: String, x: CGFloat, y: CGFloat, color: NSColor) {
        rounded(NSRect(x: x, y: y, width: 42, height: 36), radius: 9, color: color)
        let attributes: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedSystemFont(ofSize: 17, weight: .heavy),
            .foregroundColor: NSColor.white,
        ]
        let size = label.size(withAttributes: attributes)
        label.draw(at: NSPoint(x: x + (42 - size.width) / 2, y: y + 7), withAttributes: attributes)
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)

        // The only surround is a compact control clue below MAME. Everything
        // else in this click-through window remains transparent.
        let deck = NSRect(x: screenLeft, y: 566, width: screenWidth, height: 62)
        rounded(deck, radius: 16,
                color: NSColor(calibratedRed: 0.035, green: 0.055, blue: 0.11, alpha: 0.94))

        let muted = NSColor(calibratedWhite: 0.58, alpha: 1)
        text("MOVE", at: NSPoint(x: 96, y: 578), size: 10, color: muted)
        text("W A S D", at: NSPoint(x: 96, y: 597), size: 14, color: .systemBlue, weight: .heavy)

        text("PUNCH", at: NSPoint(x: 224, y: 578), size: 10, color: muted)
        text("J K L", at: NSPoint(x: 224, y: 597), size: 14, color: .systemPink, weight: .heavy)

        text("KICK", at: NSPoint(x: 322, y: 578), size: 10, color: muted)
        text("N M ,", at: NSPoint(x: 322, y: 597), size: 14, color: .systemOrange, weight: .heavy)

        text("COIN", at: NSPoint(x: 420, y: 578), size: 10, color: muted)
        text("RETURN", at: NSPoint(x: 420, y: 597), size: 14, color: .systemGreen, weight: .heavy)

        text("START", at: NSPoint(x: 534, y: 578), size: 10, color: muted)
        text("1", at: NSPoint(x: 534, y: 597), size: 14, color: .systemPurple, weight: .heavy)

        text("SYSTEM", at: NSPoint(x: 618, y: 578), size: 10, color: muted)
        text("TAB  ·  ESC", at: NSPoint(x: 618, y: 597), size: 13,
             color: NSColor(calibratedWhite: 0.82, alpha: 1), weight: .heavy)
    }
}

final class AppDelegate: NSObject, NSApplicationDelegate, NSWindowDelegate {
    private var window: NSWindow!
    private var mame: Process?
    private var isClosing = false

    func applicationDidFinishLaunching(_ notification: Notification) {
        guard let visible = NSScreen.main?.visibleFrame else {
            NSApp.terminate(nil)
            return
        }

        let frame = NSRect(
            x: visible.midX - cabinetWidth / 2,
            y: visible.midY - cabinetHeight / 2,
            width: cabinetWidth,
            height: cabinetHeight
        )

        window = NSWindow(contentRect: frame,
                          styleMask: [.borderless],
                          backing: .buffered,
                          defer: false)
        window.backgroundColor = .clear
        window.isOpaque = false
        window.hasShadow = true
        window.level = .normal
        window.ignoresMouseEvents = true
        window.collectionBehavior = [.fullScreenAuxiliary]
        window.contentView = CabinetView(frame: NSRect(origin: .zero, size: frame.size))
        window.delegate = self
        window.orderFrontRegardless()

        launchMAME(cabinetFrame: frame, screenFrame: NSScreen.main!.frame)
    }

    private func launchMAME(cabinetFrame: NSRect, screenFrame: NSRect) {
        // SDL uses a top-left display origin; AppKit uses bottom-left.
        let gameX = Int(cabinetFrame.minX + screenLeft)
        let gameTopInAppKit = cabinetFrame.maxY - screenTop
        let gameY = Int(screenFrame.maxY - gameTopInAppKit)

        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/opt/homebrew/bin/mame")
        process.environment = ProcessInfo.processInfo.environment.merging([
            "SDL_VIDEO_WINDOW_POS": "\(gameX),\(gameY)",
        ]) { _, new in new }
        let home = FileManager.default.homeDirectoryForCurrentUser.path
        process.arguments = [
            "-rompath", "\(home)/Arcade/roms",
            "-cfg_directory", "\(home)/Arcade/cfg",
            "-nvram_directory", "\(home)/Arcade/nvram",
            "-state_directory", "\(home)/Arcade/states",
            "-snapshot_directory", "\(home)/Arcade/snaps",
            "-ctrlrpath", "\(home)/Arcade/ctrlr",
            "-ctrlr", "blueberry",
            "-pluginspath", "/opt/homebrew/share/mame/plugins",
            "-plugin", "hiscore",
            "-video", "opengl",
            "-lowlatency",
            "-sound", "coreaudio",
            "-samplerate", "44100",
            "-audio_latency", "3",
            "-noui_mouse",
            "-mouseprovider", "none",
            "-lightgunprovider", "none",
            "-joystick", "-skip_gameinfo", "-window", "-nomaximize", "-nofilter",
            "-resolution", "\(Int(screenWidth))x\(Int(screenHeight))",
            "sf2ce",
        ]
        process.terminationHandler = { [weak self] _ in
            DispatchQueue.main.async {
                guard let self, !self.isClosing else { return }
                self.isClosing = true
                NSApp.terminate(nil)
            }
        }
        do {
            try process.run()
            mame = process
        } catch {
            let alert = NSAlert(error: error)
            alert.runModal()
            NSApp.terminate(nil)
        }
    }

    func windowWillClose(_ notification: Notification) {
        isClosing = true
        if let mame, mame.isRunning { mame.terminate() }
        NSApp.terminate(nil)
    }

    func applicationWillTerminate(_ notification: Notification) {
        isClosing = true
        if let mame, mame.isRunning { mame.terminate() }
    }
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
