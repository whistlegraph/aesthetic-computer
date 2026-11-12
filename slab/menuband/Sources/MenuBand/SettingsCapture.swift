import AppKit

// SettingsCapture — headless capture of the real Settings window, so layout
// changes can be checked without opening the app. Mirrors AboutCLI.
//
//   MenuBand --render-settings --out settings.png [--scale 2] [--dark]
enum SettingsCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-settings") else { return false }
        func val(_ f: String) -> String? {
            guard let i = args.firstIndex(of: f), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let out = val("--out") ?? "/tmp/settings.png"
        let scale = max(1.0, Double(val("--scale") ?? "2") ?? 2)
        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        app.appearance = NSAppearance(named: args.contains("--dark") ? .darkAqua : .aqua)
        AppDelegate.registerBundledFonts()

        let ctrl = SettingsWindowController(menuBand: nil)
        guard let win = ctrl.window, let cv = win.contentView else {
            FileHandle.standardError.write(Data("✗ no settings content view\n".utf8)); return true
        }
        cv.layoutSubtreeIfNeeded()
        win.displayIfNeeded()
        let bounds = cv.bounds
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: Int(bounds.width * scale),
            pixelsHigh: Int(bounds.height * scale),
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0) else { return true }
        rep.size = bounds.size
        cv.cacheDisplay(in: bounds, to: rep)
        try? rep.representation(using: .png, properties: [:])?.write(to: URL(fileURLWithPath: out))
        print("settings \(Int(bounds.width))x\(Int(bounds.height)) → \(out)")
        return true
    }
}
