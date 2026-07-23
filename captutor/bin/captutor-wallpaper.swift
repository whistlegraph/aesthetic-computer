// CaptutorWallpaper — a quiet animated Fuser stage behind every filmed window.

import AppKit
import CoreGraphics

private struct Particle {
    let x: CGFloat
    let y: CGFloat
    let size: CGFloat
    let phase: CGFloat
    let sway: CGFloat
    let swayRate: CGFloat
    let opacity: CGFloat
    let isSolid: Bool
}

private struct CardState: Decodable, Equatable {
    let phase: String
    let kicker: String?
    let title: String?
    let subtitle: String?
    let footer: String?

    static let ambient = CardState(
        phase: "ambient", kicker: nil, title: nil, subtitle: nil, footer: nil
    )
}

private func particles(for canvas: CGSize) -> [Particle] {
    // Fixed seed: two takes with the same clock position get the same visual
    // rhythm, and no random source can make a render irreproducible.
    var state: UInt64 = 0xF053_2026_0720
    func unit() -> CGFloat {
        state = state &* 6364136223846793005 &+ 1442695040888963407
        return CGFloat((state >> 33) & 0xFFFF) / CGFloat(0xFFFF)
    }

    let width = max(canvas.width, 1)
    let height = max(canvas.height, 1)
    let verticalPeriod = height * 1.28
    // Roughly 39 marks at the 2560×1440 Stage size. Smaller displays retain a
    // useful floor without becoming crowded; very large displays get a cap.
    let targetCount = max(28, min(46, Int((width * height) / 95_000)))
    var result: [Particle] = []

    for index in 0..<targetCount {
        // Deliberately stepped sizing makes the hierarchy legible: most marks
        // are texture, some are clearly larger, and a few act as anchors.
        let sizeRoll = unit()
        let logoSize: CGFloat
        if sizeRoll < 0.12 {
            logoSize = 76 + unit() * 28
        } else if sizeRoll < 0.42 {
            logoSize = 48 + unit() * 24
        } else {
            logoSize = 26 + unit() * 22
        }
        let sway = 10 + unit() * 28
        let isSolid = index % 8 == 0
        let candidateOpacity: CGFloat = isSolid ? 1 : 0.38 + unit() * 0.34
        let margin = logoSize / 2 + sway + 10

        // Rejection sampling uses each logo's entire horizontal travel envelope,
        // plus a little breathing room. Because every logo rises at one shared
        // rate, a valid arrangement stays valid throughout the vertical loop.
        var accepted: Particle?
        for _ in 0..<240 {
            let availableWidth = max(0, width - margin * 2)
            let x = margin + unit() * availableWidth
            let y = unit() * verticalPeriod
            let candidate = Particle(
                x: x / width,
                y: y / verticalPeriod,
                size: logoSize,
                phase: unit() * .pi * 2,
                sway: sway,
                swayRate: 0.15 + unit() * 0.14,
                opacity: candidateOpacity,
                isSolid: isSolid
            )
            let clearsField = result.allSatisfy { other in
                let otherX = other.x * width
                let otherY = other.y * verticalPeriod
                let dxAfterSway = max(0, abs(x - otherX) - sway - other.sway)
                let rawY = abs(y - otherY)
                let dy = min(rawY, verticalPeriod - rawY)
                let protectedRadius = (logoSize + other.size) / 2 + 18
                return hypot(dxAfterSway, dy) >= protectedRadius
            }
            if clearsField {
                accepted = candidate
                break
            }
        }
        if let accepted { result.append(accepted) }
    }

    return result
}

private final class WallpaperView: NSView {
    private let born = ProcessInfo.processInfo.systemUptime
    private let field: [Particle]
    private let dark: Bool
    private let logo: NSImage
    private let cardURL = FileManager.default.homeDirectoryForCurrentUser
        .appendingPathComponent(".local/share/captutor/wallpaper-card.json")
    private var card = CardState.ambient
    private var cardPayload = Data()
    private var cardChangedAt = ProcessInfo.processInfo.systemUptime
    private var timer: Timer?

    override init(frame frameRect: NSRect) {
        dark = NSApp.effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        logo = WallpaperView.loadLogo(dark: dark)
        field = particles(for: frameRect.size)
        super.init(frame: frameRect)
        wantsLayer = true
        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            self?.reloadCard()
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

        if card.phase == "ambient" {
          for particle in field {
            // One shared rise rate keeps the collision-safe layout rigid in Y,
            // while independent sine waves make the marks sway organically.
            let progress = (particle.y * 1.28 + t * 0.014).truncatingRemainder(dividingBy: 1.28)
            let y = (progress - 0.14) * rect.height
            let x = particle.x * rect.width
                + sin(t * particle.swayRate + particle.phase) * particle.sway
            let logoWidth = particle.size
            // Solid anchors stay genuinely full-opacity. The surrounding marks
            // breathe softly, providing the requested opacity shift without
            // making every logo compete with the filmed Fuser window.
            let breath = particle.isSolid
                ? 1
                : 0.58 + 0.42 * (sin(t * 0.32 + particle.phase) + 1) / 2
            drawLogo(
                in: NSRect(x: x - logoWidth / 2, y: y - particle.size / 2,
                           width: logoWidth, height: particle.size),
                opacity: particle.opacity * breath
            )
          }
        } else {
            drawCard(in: rect, elapsed: ProcessInfo.processInfo.systemUptime - cardChangedAt)
        }
    }

    private func reloadCard() {
        guard let data = try? Data(contentsOf: cardURL), data != cardPayload,
              let decoded = try? JSONDecoder().decode(CardState.self, from: data) else { return }
        cardPayload = data
        card = decoded
        cardChangedAt = ProcessInfo.processInfo.systemUptime
    }

    private func drawCard(in canvas: NSRect, elapsed: TimeInterval) {
        let darkInk = dark ? NSColor.white : NSColor(calibratedWhite: 0.045, alpha: 1)
        func ease(_ delay: TimeInterval, _ duration: TimeInterval) -> CGFloat {
            let raw = CGFloat(min(1, max(0, (elapsed - delay) / duration)))
            return raw * raw * (3 - 2 * raw)
        }
        let logoProgress = ease(0.04, 0.42)
        let titleProgress = ease(0.20, 0.52)

        NSGraphicsContext.saveGraphicsState()
        let portrait = canvas.width < canvas.height
        let logoSize: CGFloat = portrait ? 116 : 132
        let logoRect = NSRect(
            x: canvas.midX - logoSize / 2,
            y: canvas.midY + (portrait ? 38 : 28) - (1 - logoProgress) * 24,
            width: logoSize,
            height: logoSize
        )
        drawLogo(in: logoRect, opacity: logoProgress)
        let titleSize: CGFloat = portrait ? 48 : 62
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current?.cgContext.setAlpha(titleProgress)
        drawCentered(
            card.title ?? "",
            in: NSRect(
                x: canvas.minX + canvas.width * 0.10,
                y: canvas.midY - (portrait ? 190 : 170) - (1 - titleProgress) * 18,
                width: canvas.width * 0.80,
                height: portrait ? 250 : 220
            ),
            font: NSFont.systemFont(ofSize: titleSize, weight: .black), color: darkInk
        )
        NSGraphicsContext.restoreGraphicsState()
        NSGraphicsContext.restoreGraphicsState()
    }

    private func drawCentered(
        _ text: String, in rect: NSRect, font: NSFont, color: NSColor, kern: CGFloat = 0
    ) {
        let paragraph = NSMutableParagraphStyle()
        paragraph.alignment = .center
        paragraph.lineBreakMode = .byWordWrapping
        let attributed = NSAttributedString(string: text, attributes: [
            .font: font, .foregroundColor: color, .paragraphStyle: paragraph, .kern: kern,
        ])
        attributed.draw(with: rect, options: [.usesLineFragmentOrigin, .usesFontLeading])
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
