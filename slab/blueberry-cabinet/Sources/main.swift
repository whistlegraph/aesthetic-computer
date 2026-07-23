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

        let bg = NSGradient(colors: [
            NSColor(calibratedRed: 0.035, green: 0.055, blue: 0.12, alpha: 1),
            NSColor(calibratedRed: 0.02, green: 0.025, blue: 0.055, alpha: 1),
        ])!
        bg.draw(in: bounds, angle: -90)

        // Marquee.
        rounded(NSRect(x: 24, y: 18, width: bounds.width - 48, height: 62), radius: 18,
                color: NSColor(calibratedRed: 0.08, green: 0.20, blue: 0.48, alpha: 1))
        centered("BLUEBERRY FIGHT CLUB", y: 31, size: 28,
                 color: NSColor(calibratedRed: 0.65, green: 0.88, blue: 1, alpha: 1), weight: .black)

        // Bezel—the live MAME window sits over the black aperture.
        rounded(NSRect(x: screenLeft - 18, y: screenTop - 18,
                       width: screenWidth + 36, height: screenHeight + 36), radius: 22,
                color: NSColor(calibratedWhite: 0.015, alpha: 1))
        rounded(NSRect(x: screenLeft, y: screenTop, width: screenWidth, height: screenHeight),
                radius: 4, color: .black)

        // Control deck.
        rounded(NSRect(x: 32, y: 566, width: bounds.width - 64, height: 126), radius: 22,
                color: NSColor(calibratedRed: 0.055, green: 0.085, blue: 0.16, alpha: 1))
        text("MOVE", at: NSPoint(x: 64, y: 582), size: 12,
             color: NSColor(calibratedWhite: 0.62, alpha: 1))
        key("W", x: 117, y: 576, color: .systemBlue)
        key("A", x: 68, y: 618, color: .systemBlue)
        key("S", x: 117, y: 618, color: .systemBlue)
        key("D", x: 166, y: 618, color: .systemBlue)

        text("PUNCH", at: NSPoint(x: 286, y: 582), size: 12,
             color: NSColor(calibratedWhite: 0.62, alpha: 1))
        key("J", x: 286, y: 608, color: .systemPink)
        key("K", x: 336, y: 608, color: .systemPink)
        key("L", x: 386, y: 608, color: .systemPink)

        text("KICK", at: NSPoint(x: 286, y: 654), size: 12,
             color: NSColor(calibratedWhite: 0.62, alpha: 1))
        key("N", x: 336, y: 650, color: .systemOrange)
        key("M", x: 386, y: 650, color: .systemOrange)
        key(",", x: 436, y: 650, color: .systemOrange)

        key("↩", x: 546, y: 608, color: .systemGreen)
        text("COIN", at: NSPoint(x: 547, y: 654), size: 11,
             color: NSColor(calibratedWhite: 0.7, alpha: 1))
        key("1", x: 612, y: 608, color: .systemPurple)
        text("START", at: NSPoint(x: 610, y: 654), size: 11,
             color: NSColor(calibratedWhite: 0.7, alpha: 1))

        rounded(NSRect(x: 708, y: 592, width: 154, height: 66), radius: 14,
                color: NSColor(calibratedRed: 0.03, green: 0.14, blue: 0.10, alpha: 1))
        text("PERFORMANCE", at: NSPoint(x: 724, y: 603), size: 10,
             color: NSColor(calibratedWhite: 0.65, alpha: 1))
        text("18.6× HEADROOM", at: NSPoint(x: 724, y: 625), size: 15,
             color: .systemGreen, weight: .heavy)

        centered("TAB  settings     ESC  quit", y: 698, size: 11,
                 color: NSColor(calibratedWhite: 0.48, alpha: 1), weight: .medium)
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
