import AppKit

/// JukeWizard's compact menu-bar presence: current artist and track beside an
/// artwork CD that spins only while this JukeWizard instance is playing.
final class MenuBarCD {
    struct DeckState {
        let id: String
        let artist: String?
        let title: String
        let art: NSImage?
        let accent: NSColor
        let bpm: Double
        let playing: Bool
    }

    private struct VisibleDeck {
        let id: String
        let title: String
        let image: NSImage
        let bpm: Double
        var angle: CGFloat
    }

    private let statusItem: NSStatusItem
    private let fallbackImage: NSImage
    private var baseImage: NSImage
    private var timer: Timer?
    private var decks: [VisibleDeck] = []
    private let side: CGFloat = 20
    private let maximumCreditWidth: CGFloat = 260
    private let beatsPerRevolution: Double = 8

    var onClick: (() -> Void)?

    init() {
        statusItem = NSStatusBar.system.statusItem(withLength: 24)
        statusItem.autosaveName = "jukewizard"
        fallbackImage = Self.marked(Self.loadCD(side: side))
        baseImage = fallbackImage
        if let button = statusItem.button {
            button.image = baseImage
            button.imagePosition = .imageOnly
            button.imageScaling = .scaleProportionallyDown
            button.font = .systemFont(ofSize: 12, weight: .medium)
            button.lineBreakMode = .byTruncatingTail
            button.toolTip = "JukeWizard"
            button.target = self
            button.action = #selector(clicked)
            button.sendAction(on: [.leftMouseUp])
        }
    }

    deinit { timer?.invalidate() }

    private static func loadCD(side: CGFloat) -> NSImage {
        let url = Bundle.module.url(forResource: "jukewizard-cd", withExtension: "png",
                                    subdirectory: "Assets")
            ?? Bundle.module.url(forResource: "jukewizard-cd", withExtension: "png")
        let image = url.flatMap(NSImage.init(contentsOf:))
            ?? NSImage(size: NSSize(width: side, height: side))
        image.size = NSSize(width: side, height: side)
        image.isTemplate = false
        return image
    }

    private static func marked(_ image: NSImage) -> NSImage {
        let output = NSImage(size: image.size)
        output.lockFocus()
        image.draw(at: .zero, from: NSRect(origin: .zero, size: image.size),
                   operation: .sourceOver, fraction: 1)
        let diameter: CGFloat = 2.2
        let mark = NSRect(x: image.size.width * 0.72 - diameter / 2,
                          y: image.size.height * 0.72 - diameter / 2,
                          width: diameter, height: diameter)
        NSColor.black.withAlphaComponent(0.72).setFill()
        NSBezierPath(ovalIn: mark.insetBy(dx: -0.45, dy: -0.45)).fill()
        NSColor.white.withAlphaComponent(0.96).setFill()
        NSBezierPath(ovalIn: mark).fill()
        output.unlockFocus()
        output.isTemplate = false
        return output
    }

    @objc private func clicked() { onClick?() }

    func setSingleDeck(artist: String?, title: String, art: NSImage?, bpm: Double?, playing: Bool) {
        baseImage = art.map { Self.marked(CDArtworkRenderer.disc(from: $0, side: side)) }
            ?? fallbackImage
        setDecks([DeckState(id: "single", artist: artist, title: title, art: art, accent: Palette.gold,
                            bpm: bpm ?? 120, playing: playing)])
        updateCredit(Self.credit(artist: artist, title: title))
    }

    func setDecks(_ states: [DeckState]) {
        let previousAngles = Dictionary(uniqueKeysWithValues: decks.map { ($0.id, $0.angle) })
        decks = states.filter(\.playing).prefix(2).map { state in
            let candidate = state.bpm.isFinite && state.bpm > 0 ? state.bpm : 120
            let bpm = min(240, max(30, candidate))
            let image = state.art.map { Self.marked(CDArtworkRenderer.disc(from: $0, side: side)) }
                ?? Self.record(side: side, accent: state.accent)
            return VisibleDeck(id: state.id, title: Self.credit(artist: state.artist, title: state.title), image: image,
                               bpm: bpm, angle: previousAngles[state.id] ?? 0)
        }
        updateCredit(decks.map(\.title).joined(separator: " + "))
        if decks.isEmpty {
            stopSpin()
        } else {
            renderDecks()
            startSpin()
        }
    }

    static func credit(artist: String?, title: String) -> String {
        let artist = artist?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        let title = title.trimmingCharacters(in: .whitespacesAndNewlines)
        return [artist, title].filter { !$0.isEmpty }.joined(separator: " — ")
    }

    private func updateCredit(_ credit: String) {
        guard let button = statusItem.button else { return }
        button.title = credit
        button.imagePosition = credit.isEmpty ? .imageOnly : .imageLeading
        button.toolTip = credit.isEmpty ? "JukeWizard" : credit
        let iconWidth: CGFloat = decks.count > 1 ? 40 : 24
        let measured = (credit as NSString).size(withAttributes: [.font: button.font!]).width
        let creditWidth = credit.isEmpty ? 0 : min(maximumCreditWidth, ceil(measured) + 10)
        statusItem.length = iconWidth + creditWidth
    }

    private func startSpin() {
        guard timer == nil else { return }
        timer?.invalidate()
        let timer = Timer(timeInterval: 1.0 / 24.0, repeats: true) { [weak self] _ in
            self?.tick()
        }
        timer.tolerance = 1.0 / 240.0
        RunLoop.main.add(timer, forMode: .common)
        self.timer = timer
    }

    private func stopSpin() {
        timer?.invalidate()
        timer = nil
        statusItem.button?.image = baseImage
    }

    private func tick() {
        for index in decks.indices {
            decks[index].angle -= CGFloat(360 * (decks[index].bpm / 60) / beatsPerRevolution / 24)
            if decks[index].angle <= -360 { decks[index].angle += 360 }
        }
        renderDecks()
    }

    private func renderDecks() {
        guard !decks.isEmpty else { statusItem.button?.image = baseImage; return }
        if decks.count == 1 {
            statusItem.button?.image = rotated(decks[0].image, by: decks[0].angle)
            return
        }
        let output = NSImage(size: NSSize(width: 36, height: side))
        output.lockFocus()
        NSGraphicsContext.current?.imageInterpolation = .high
        for (index, deck) in decks.enumerated() {
            let disc = rotated(deck.image, by: deck.angle)
            disc.draw(in: NSRect(x: CGFloat(index) * 17, y: 1, width: 19, height: 19),
                      from: NSRect(origin: .zero, size: disc.size), operation: .sourceOver, fraction: 1)
        }
        output.unlockFocus()
        output.isTemplate = false
        statusItem.button?.image = output
    }

    private static func record(side: CGFloat, accent: NSColor) -> NSImage {
        let image = NSImage(size: NSSize(width: side, height: side))
        image.lockFocus()
        let outer = NSRect(x: 0.7, y: 0.7, width: side - 1.4, height: side - 1.4)
        NSColor(white: 0.04, alpha: 1).setFill()
        NSBezierPath(ovalIn: outer).fill()
        for inset in stride(from: side * 0.13, through: side * 0.34, by: side * 0.07) {
            NSColor.white.withAlphaComponent(0.20).setStroke()
            let groove = NSBezierPath(ovalIn: outer.insetBy(dx: inset, dy: inset))
            groove.lineWidth = 0.45
            groove.stroke()
        }
        let label = outer.insetBy(dx: side * 0.31, dy: side * 0.31)
        accent.setFill()
        NSBezierPath(ovalIn: label).fill()
        NSColor.white.setFill()
        NSBezierPath(ovalIn: NSRect(x: side / 2 - 1, y: side / 2 - 1,
                                   width: 2, height: 2)).fill()
        accent.setStroke()
        let marker = NSBezierPath()
        marker.move(to: NSPoint(x: side / 2, y: side * 0.70))
        marker.line(to: NSPoint(x: side / 2, y: side * 0.91))
        marker.lineWidth = 1.6
        marker.stroke()
        image.unlockFocus()
        image.isTemplate = false
        return image
    }

    private func rotated(_ image: NSImage, by degrees: CGFloat) -> NSImage {
        let size = image.size
        let output = NSImage(size: size)
        output.lockFocus()
        NSGraphicsContext.current?.imageInterpolation = .high
        let transform = NSAffineTransform()
        transform.translateX(by: size.width / 2, yBy: size.height / 2)
        transform.rotate(byDegrees: degrees)
        transform.translateX(by: -size.width / 2, yBy: -size.height / 2)
        transform.concat()
        image.draw(at: .zero, from: NSRect(origin: .zero, size: size),
                   operation: .sourceOver, fraction: 1)
        output.unlockFocus()
        output.isTemplate = false
        return output
    }
}
