#if !MAC_APP_STORE
import AppKit

/// The Juke's menu-bar presence: the track's cover art printed onto a tiny
/// spinning CD with "artist — title" beside it — the status item the old
/// standalone JukeWizard used to own, recreated inside Menu Band. Appears
/// only while the Juke has something loaded; the disc spins while playing
/// and rests while paused. Clicking raises the Juke window.
final class MenuBandJukeStatusItem {
    private let statusItem: NSStatusItem
    private let fallback: NSImage
    private var baseImage: NSImage
    private var timer: Timer?
    private var angle: CGFloat = 0
    private let side: CGFloat = 22
    private static let maxTitleLength = 34
    var onClick: (() -> Void)?
    /// Whether the item is currently on the menu bar — lets the room-guest
    /// path avoid covering a real Juke display.
    var isShowing: Bool { statusItem.isVisible }
    /// Built fresh on each right-click so it can report what is actually on
    /// screen right now.
    var menuProvider: (() -> NSMenu)?

    init() {
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        statusItem.autosaveName = "menuband-juke"
        fallback = MenuBandCDArtworkRenderer.fallback(side: side)
        baseImage = fallback
        if let button = statusItem.button {
            button.image = fallback
            button.imagePosition = .imageLeft
            button.imageScaling = .scaleProportionallyDown
            button.font = NSFont.monospacedDigitSystemFont(ofSize: 11, weight: .medium)
            button.target = self
            button.action = #selector(clicked)
            button.sendAction(on: [.leftMouseUp, .rightMouseUp])
            button.toolTip = "Juke"
        }
        statusItem.isVisible = false
    }

    func update(title: String, artist: String, disc: NSImage?, playing: Bool) {
        baseImage = disc ?? fallback
        if timer == nil { statusItem.button?.image = baseImage }
        let line = [artist, title].filter { !$0.isEmpty }.joined(separator: " — ")
        statusItem.button?.title = line.isEmpty ? "" : " " + Self.trimmed(line)
        statusItem.button?.toolTip = line.isEmpty ? "Juke" : "Juke · \(line)"
        statusItem.isVisible = true
        setSpinning(playing)
    }

    func hide() {
        setSpinning(false)
        statusItem.isVisible = false
    }

    private static func trimmed(_ line: String) -> String {
        guard line.count > maxTitleLength else { return line }
        return String(line.prefix(maxTitleLength - 1)) + "…"
    }

    private func setSpinning(_ spinning: Bool) {
        if spinning {
            guard timer == nil else { return }
            let timer = Timer(timeInterval: 1.0 / 30.0, repeats: true) {
                [weak self] _ in self?.tick()
            }
            RunLoop.main.add(timer, forMode: .common)
            self.timer = timer
        } else {
            timer?.invalidate()
            timer = nil
            angle = 0
            statusItem.button?.image = baseImage
        }
    }

    private func tick() {
        // 30 fps × 1.8° ≈ one revolution every 6.7 seconds — a lazy platter,
        // not a shredding one.
        angle -= 1.8
        angle.formTruncatingRemainder(dividingBy: 360)
        statusItem.button?.image = rotated(baseImage, by: angle)
    }

    @objc private func clicked() {
        let event = NSApp.currentEvent
        let secondary = event?.type == .rightMouseUp ||
            event?.modifierFlags.contains(.control) == true
        guard secondary, let menu = menuProvider?() else { onClick?(); return }
        // The status item only owns a menu for the length of one click, so a
        // plain left click keeps raising the Juke window.
        statusItem.menu = menu
        statusItem.button?.performClick(nil)
        statusItem.menu = nil
    }

    private func rotated(_ image: NSImage, by degrees: CGFloat) -> NSImage {
        let output = NSImage(size: image.size)
        output.lockFocus()
        let transform = NSAffineTransform()
        transform.translateX(by: image.size.width / 2, yBy: image.size.height / 2)
        transform.rotate(byDegrees: degrees)
        transform.translateX(by: -image.size.width / 2,
                             yBy: -image.size.height / 2)
        transform.concat()
        image.draw(at: .zero, from: NSRect(origin: .zero, size: image.size),
                   operation: .sourceOver, fraction: 1)
        output.unlockFocus()
        output.isTemplate = false
        return output
    }

    deinit {
        timer?.invalidate()
    }
}
#endif
