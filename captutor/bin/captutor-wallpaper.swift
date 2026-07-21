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
    return (0..<22).map { _ in
        Particle(
            x: unit(), y: unit() * 1.28 - 0.14,
            speed: 0.010 + unit() * 0.018,
            size: 54 + unit() * 76,
            phase: unit() * .pi * 2,
            opacity: 0.34 + unit() * 0.18
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

        let background = dark
            ? [NSColor(calibratedWhite: 0.055, alpha: 1), NSColor(calibratedWhite: 0.14, alpha: 1)]
            : [NSColor(calibratedWhite: 0.97, alpha: 1), NSColor(calibratedWhite: 0.82, alpha: 1)]
        NSGradient(colors: background)!.draw(in: rect, angle: -72)

        drawEdge(in: rect, time: t)

        // Two very slow color breaths keep even empty margins alive without
        // competing with the product window or subtitles.
        drawGlow(
            center: NSPoint(x: rect.width * (0.18 + 0.025 * sin(t * 0.09)),
                            y: rect.height * (0.76 + 0.020 * cos(t * 0.08))),
            radius: max(rect.width, rect.height) * 0.34,
            color: NSColor(calibratedRed: 0.49, green: 0.12, blue: 1.0, alpha: 0.10)
        )
        drawGlow(
            center: NSPoint(x: rect.width * (0.82 + 0.022 * cos(t * 0.07)),
                            y: rect.height * (0.24 + 0.026 * sin(t * 0.10))),
            radius: max(rect.width, rect.height) * 0.30,
            color: NSColor(calibratedRed: 0.06, green: 0.90, blue: 0.70, alpha: 0.08)
        )

        for particle in field {
            let progress = (particle.y + t * particle.speed).truncatingRemainder(dividingBy: 1.28)
            let y = (progress - 0.14) * rect.height
            let sway = sin(t * 0.20 + particle.phase) * rect.width * 0.012
            let x = particle.x * rect.width + sway
            let logoWidth = particle.size * 1.255
            drawLogo(
                in: NSRect(x: x - logoWidth / 2, y: y - particle.size / 2,
                           width: logoWidth, height: particle.size),
                opacity: particle.opacity
            )
        }
    }

    private func drawGlow(center: NSPoint, radius: CGFloat, color: NSColor) {
        let colors = [color, color.withAlphaComponent(0)] as CFArray
        let locations: [CGFloat] = [0, 1]
        guard let gradient = CGGradient(
            colorsSpace: CGColorSpaceCreateDeviceRGB(), colors: colors, locations: locations
        ), let context = NSGraphicsContext.current?.cgContext else { return }
        context.drawRadialGradient(
            gradient, startCenter: center, startRadius: 0,
            endCenter: center, endRadius: radius, options: [.drawsAfterEndLocation]
        )
    }

    private func drawEdge(in rect: NSRect, time: CGFloat) {
        // A narrow, feathered brand-spectrum edge makes the recording feel
        // intentionally staged while leaving the usable desktop margin quiet.
        // Drawing three nested rings avoids a hard neon keyline.
        let colors: [NSColor] = [
            NSColor(calibratedRed: 0.49, green: 0.06, blue: 1.00, alpha: 1),
            NSColor(calibratedRed: 0.98, green: 0.22, blue: 0.66, alpha: 1),
            NSColor(calibratedRed: 0.06, green: 0.94, blue: 0.76, alpha: 1),
            NSColor(calibratedRed: 0.49, green: 0.06, blue: 1.00, alpha: 1),
        ]
        let angle = -8 + 7 * sin(time * 0.10)
        for (width, alpha) in [(54.0, 0.08), (28.0, 0.20), (10.0, 0.74)] {
            guard let context = NSGraphicsContext.current?.cgContext else { return }
            context.saveGState()
            context.addRect(rect)
            context.addRect(rect.insetBy(dx: width, dy: width))
            context.clip(using: .evenOdd)
            NSGradient(colors: colors.map { $0.withAlphaComponent(alpha) })?
                .draw(in: rect, angle: angle)
            context.restoreGState()
        }
    }

    private func drawLogo(in rect: NSRect, opacity: CGFloat) {
        // The source SVG is the production Fuser thumbnail mark, including its
        // own purple→teal halo. Draw it as a single vector-backed image so the
        // logo remains sharp in the 2× Stage capture.
        logo.draw(in: rect, from: .zero, operation: .sourceOver, fraction: opacity)
    }

    private static func loadLogo(dark: Bool) -> NSImage {
        guard let url = Bundle.main.url(forResource: "fuser-thumbnail-logo", withExtension: "svg"),
              let source = try? String(contentsOf: url, encoding: .utf8) else {
            fatalError("Captutor Wallpaper is missing fuser-thumbnail-logo.svg")
        }
        let themed: String
        if dark {
            // Keep the production geometry and colorful halo; swap only the
            // neutral logo surfaces so Dark is truly white-on-black.
            themed = source
                .replacingOccurrences(of: "#171717", with: "__FUSER_DARK__")
                .replacingOccurrences(of: "#FAFAFA", with: "#171717")
                .replacingOccurrences(of: "fill=\"white\"", with: "fill=\"#171717\"")
                .replacingOccurrences(of: "__FUSER_DARK__", with: "#FAFAFA")
        } else {
            themed = source
        }
        guard let image = NSImage(data: Data(themed.utf8)) else {
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
