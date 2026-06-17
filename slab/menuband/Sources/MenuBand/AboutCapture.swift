import AppKit

// AboutCapture — headless capture of the REAL About window (the actual
// AboutWindowController view tree: icon cards, "Menu Band", tagline, the
// LanguageMapView, version/copyright) so promos/reels use the shipping
// graphics, not a reproduction. Mirrors MenubarCLI.runIfRequested.
//
//   MenuBand --render-about --out about.png [--lang es] [--scale 3] [--dark]
//
// --lang sets Localization.current before the panel is built, so the
// language map highlights that language and every string re-reads in it.
enum AboutCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-about") else { return false }
        func val(_ f: String) -> String? {
            guard let i = args.firstIndex(of: f), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let out = val("--out") ?? "/tmp/about.png"
        let scale = max(1.0, Double(val("--scale") ?? "3") ?? 3)

        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        // About follows system appearance; default to light (aqua) so it reads
        // as a clean window card in the reel. --dark flips it.
        app.appearance = NSAppearance(named: args.contains("--dark") ? .darkAqua : .aqua)
        if let lang = val("--lang") { Localization.current = lang }

        let ctrl = AboutWindowController(updateInfo: nil)
        guard let win = ctrl.window, let cv = win.contentView else {
            FileHandle.standardError.write(Data("✗ no about content view\n".utf8)); return true
        }
        // Let the panel size to its natural fitting height (the language map
        // grows it), then snapshot the laid-out tree.
        cv.layoutSubtreeIfNeeded()
        let fit = cv.fittingSize
        win.setContentSize(NSSize(width: max(320, fit.width), height: max(340, fit.height)))
        cv.layoutSubtreeIfNeeded()
        win.displayIfNeeded()

        let bounds = cv.bounds
        let pw = Int((bounds.width * scale).rounded())
        let ph = Int((bounds.height * scale).rounded())
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: pw, pixelsHigh: ph,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0) else {
            FileHandle.standardError.write(Data("✗ bitmap alloc failed\n".utf8)); return true
        }
        rep.size = bounds.size                 // points; pixels are scale× → Retina capture
        cv.cacheDisplay(in: bounds, to: rep)
        guard let png = rep.representation(using: .png, properties: [:]) else {
            FileHandle.standardError.write(Data("✗ png encode failed\n".utf8)); return true
        }
        try? png.write(to: URL(fileURLWithPath: out))
        print("about \(pw)x\(ph) (native \(Int(bounds.width))x\(Int(bounds.height))) lang=\(Localization.current) → \(out)")
        return true
    }
}
