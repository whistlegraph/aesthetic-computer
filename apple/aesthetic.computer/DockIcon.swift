#if os(macOS)
import AppKit

/// The Dock icon, drawn live: the pals in the system accent on a plate that
/// follows light and dark, and takes Slab's desktop tone when Slab publishes one.
/// The app and its Dock tile plug-in both draw through here, so the icon looks
/// the same open or closed.
enum DockIcon {
    /// Slab posts this (with red / green / blue / dark) whenever its desktop tone moves.
    static let slabToneChanged = Notification.Name("computer.aesthetic.slab.desktop-tint.changed")
    static let themeChanged = Notification.Name("AppleInterfaceThemeChangedNotification")
    static let accentChanged = Notification.Name("AppleColorPreferencesChangedNotification")

    struct Look {
        var plate: NSColor
        var line: NSColor
    }

    static var systemIsDark: Bool {
        UserDefaults.standard.string(forKey: "AppleInterfaceStyle") == "Dark"
    }

    /// Slab's tone, only while it was published for the appearance now showing.
    static func slabTone(dark: Bool) -> NSColor? {
        let home = String(cString: getpwuid(getuid()).pointee.pw_dir)
        let file = "\(home)/.local/share/slab/wallpaper/desktop/current-color.json"
        guard let data = FileManager.default.contents(atPath: file),
              let tone = (try? JSONSerialization.jsonObject(with: data)) as? [AnyHashable: Any]
        else { return nil }
        return slabTone(from: tone, dark: dark)
    }

    static func slabTone(from tone: [AnyHashable: Any]?, dark: Bool) -> NSColor? {
        guard let tone, (tone["dark"] as? Bool) == dark,
              let red = tone["red"] as? Int, let green = tone["green"] as? Int, let blue = tone["blue"] as? Int
        else { return nil }
        return NSColor(srgbRed: CGFloat(red) / 65535, green: CGFloat(green) / 65535,
                       blue: CGFloat(blue) / 65535, alpha: 1)
    }

    static func look(dark: Bool, slab: NSColor?) -> Look {
        var accent = NSColor.systemPink
        NSAppearance(named: dark ? .darkAqua : .aqua)?.performAsCurrentDrawingAppearance {
            accent = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? accent
        }
        let night = NSColor(srgbRed: 0.102, green: 0.102, blue: 0.180, alpha: 1)
        let paper = NSColor(srgbRed: 0.957, green: 0.945, blue: 0.969, alpha: 1)
        let base = dark ? night : paper
        let plate = slab ?? base.blended(withFraction: dark ? 0.14 : 0.08, of: accent) ?? base
        let line = dark ? accent.blended(withFraction: 0.18, of: .white) ?? accent : accent
        return Look(plate: plate, line: line)
    }

    static func pals(in bundle: Bundle) -> NSImage? {
        bundle.url(forResource: "pals", withExtension: "svg").flatMap(NSImage.init(contentsOf:))
    }

    /// Apple's icon grid: an 824-point plate on a 1024-point canvas.
    static func draw(in bounds: NSRect, look: Look, pals: NSImage?) {
        guard let context = NSGraphicsContext.current?.cgContext else { return }
        let unit = min(bounds.width, bounds.height) / 1024
        let plate = NSRect(x: bounds.midX - 412 * unit, y: bounds.midY - 412 * unit,
                           width: 824 * unit, height: 824 * unit)
        let shape = NSBezierPath(roundedRect: plate, xRadius: 185 * unit, yRadius: 185 * unit)

        context.saveGState()
        context.setShadow(offset: CGSize(width: 0, height: -10 * unit), blur: 22 * unit,
                          color: NSColor.black.withAlphaComponent(0.3).cgColor)
        look.plate.setFill()
        shape.fill()
        context.restoreGState()

        context.saveGState()
        shape.addClip()
        let lift = look.plate.blended(withFraction: 0.10, of: .white) ?? look.plate
        NSGradient(starting: look.plate, ending: lift)?.draw(in: plate, angle: 90)
        if let pals {
            let art = plate.insetBy(dx: 82 * unit, dy: 82 * unit)
            context.beginTransparencyLayer(auxiliaryInfo: nil)
            pals.draw(in: art, from: .zero, operation: .sourceOver, fraction: 1)
            context.setBlendMode(.sourceIn)
            context.setFillColor(look.line.cgColor)
            context.fill(art)
            context.endTransparencyLayer()
        }
        context.restoreGState()
    }

    static func image(look: Look, pals: NSImage?) -> NSImage {
        NSImage(size: NSSize(width: 1024, height: 1024), flipped: false) { bounds in
            draw(in: bounds, look: look, pals: pals)
            return true
        }
    }
}

/// Keeps a Dock icon current. `show` receives a fresh look whenever the
/// appearance, the accent or Slab's tone changes.
final class DockIconWatcher {
    private var tokens: [(NotificationCenter, NSObjectProtocol)] = []
    private var posted: [AnyHashable: Any]?
    private let show: (DockIcon.Look) -> Void

    init(show: @escaping (DockIcon.Look) -> Void) {
        self.show = show
        let system = DistributedNotificationCenter.default()
        for name in [DockIcon.themeChanged, DockIcon.accentChanged] {
            watch(system, name) { [weak self] _ in self?.settle() }
        }
        watch(system, DockIcon.slabToneChanged) { [weak self] note in
            self?.posted = note.userInfo
            self?.settle()
        }
        watch(.default, NSColor.systemColorsDidChangeNotification) { [weak self] _ in self?.settle() }
        refresh()
    }

    deinit { tokens.forEach { $0.0.removeObserver($0.1) } }

    private func watch(_ center: NotificationCenter, _ name: Notification.Name,
                       _ block: @escaping (Notification) -> Void) {
        tokens.append((center, center.addObserver(forName: name, object: nil, queue: .main, using: block)))
    }

    /// The system posts the change before the new values are readable everywhere, so draw now and once more.
    private func settle() {
        refresh()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak self] in self?.refresh() }
    }

    private func refresh() {
        let dark = DockIcon.systemIsDark
        let slab = DockIcon.slabTone(from: posted, dark: dark) ?? DockIcon.slabTone(dark: dark)
        show(DockIcon.look(dark: dark, slab: slab))
    }
}
#endif
