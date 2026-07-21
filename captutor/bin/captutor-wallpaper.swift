// CaptutorWallpaper — a quiet animated Fuser stage behind every filmed window.

import AppKit
import CoreGraphics

private struct Particle {
    let x: CGFloat
    let y: CGFloat
    let speed: CGFloat
    let size: CGFloat
    let phase: CGFloat
    let opacity: CGFloat
}

private func particles() -> [Particle] {
    // Fixed seed: two takes with the same clock position get the same visual
    // rhythm, and no random source can make a render irreproducible.
    var state: UInt64 = 0xF053_2026_0720
    func unit() -> CGFloat {
        state = state &* 6364136223846793005 &+ 1442695040888963407
        return CGFloat((state >> 33) & 0xFFFF) / CGFloat(0xFFFF)
    }
    return (0..<12).map { _ in
        Particle(
            x: unit(), y: unit() * 1.28 - 0.14,
            speed: 0.010 + unit() * 0.018,
            size: 32 + unit() * 24,
            phase: unit() * .pi * 2,
            opacity: 0.72 + unit() * 0.18
        )
    }
}

private final class WallpaperView: NSView {
    private let born = ProcessInfo.processInfo.systemUptime
    private let field = particles()
    private let dark: Bool
    private let logo: NSImage
    private var timer: Timer?

    override init(frame frameRect: NSRect) {
        dark = NSApp.effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        logo = WallpaperView.loadLogo(dark: dark)
        super.init(frame: frameRect)
        wantsLayer = true
        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            self?.needsDisplay = true
        }
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

    deinit { timer?.invalidate() }

    override func draw(_ dirtyRect: NSRect) {
        let rect = bounds
        let t = CGFloat(ProcessInfo.processInfo.systemUptime - born)

        (dark ? NSColor.black : NSColor.white).setFill()
        rect.fill()

        for particle in field {
            let progress = (particle.y + t * particle.speed).truncatingRemainder(dividingBy: 1.28)
            let y = (progress - 0.14) * rect.height
            let sway = sin(t * 0.20 + particle.phase) * rect.width * 0.012
            let x = particle.x * rect.width + sway
            let logoWidth = particle.size
            drawLogo(
                in: NSRect(x: x - logoWidth / 2, y: y - particle.size / 2,
                           width: logoWidth, height: particle.size),
                opacity: particle.opacity
            )
        }
    }

    private func drawLogo(in rect: NSRect, opacity: CGFloat) {
        // The icon path is extracted verbatim from Fuser's production SVG. Draw
        // it as a vector-backed image so it remains sharp in the 2× Stage capture.
        logo.draw(in: rect, from: .zero, operation: .sourceOver, fraction: opacity)
    }

    private static func loadLogo(dark: Bool) -> NSImage {
        guard let url = Bundle.main.url(forResource: "fuser-thumbnail-logo", withExtension: "svg"),
              let source = try? String(contentsOf: url, encoding: .utf8) else {
            fatalError("Captutor Wallpaper is missing fuser-thumbnail-logo.svg")
        }
        // The production thumbnail contains a wordmark and glow as well as the
        // actual icon. Extract its first path verbatim so the desktop shows only
        // the real mark—never the product name—and theme that mark by contrast.
        guard let groupStart = source.range(of: "<g filter=\"url(#filter1_ddii_0_1)\">")?.upperBound,
              let groupEnd = source.range(of: "</g>", range: groupStart..<source.endIndex)?.lowerBound,
              let pathStart = source.range(of: "<path", range: groupStart..<groupEnd)?.lowerBound,
              let pathEndToken = source.range(of: "/>", range: pathStart..<groupEnd)?.upperBound else {
            fatalError("Captutor Wallpaper could not isolate the Fuser mark")
        }
        let color = dark ? "#FAFAFA" : "#171717"
        let path = String(source[pathStart..<pathEndToken])
            .replacingOccurrences(of: "fill=\"#FAFAFA\"", with: "fill=\"\(color)\"")
            .replacingOccurrences(of: "fill=\"white\"", with: "fill=\"\(color)\"")
        let iconOnly = """
        <svg width="96" height="96" viewBox="45 63 36 36" fill="none" xmlns="http://www.w3.org/2000/svg">
        \(path)
        </svg>
        """
        guard let image = NSImage(data: Data(iconOnly.utf8)) else {
            fatalError("Captutor Wallpaper could not decode the Fuser SVG")
        }
        return image
    }
}

private final class AppDelegate: NSObject, NSApplicationDelegate {
    private var windows: [NSWindow] = []

    func applicationDidFinishLaunching(_ notification: Notification) {
        for screen in NSScreen.screens {
            let window = NSWindow(
                contentRect: screen.frame,
                styleMask: [.borderless],
                backing: .buffered,
                defer: false,
                screen: screen
            )
            window.level = NSWindow.Level(rawValue: Int(CGWindowLevelForKey(.desktopWindow)) + 1)
            window.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle]
            window.ignoresMouseEvents = true
            window.isOpaque = true
            window.hasShadow = false
            window.contentView = WallpaperView(frame: NSRect(origin: .zero, size: screen.frame.size))
            window.orderFrontRegardless()
            windows.append(window)
        }
    }
}

let app = NSApplication.shared
private let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory)
app.run()
