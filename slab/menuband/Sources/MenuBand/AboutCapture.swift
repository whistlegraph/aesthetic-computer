import AppKit

// AboutCapture — headless capture of the REAL About window (the actual
// AboutWindowController view tree: icon cards, "Menu Band", tagline, the
// LanguageMapView, version/copyright) so promos/reels use the shipping
// graphics, not a reproduction. Mirrors MenubarCLI.runIfRequested.
//
//   MenuBand --render-about --out about.png [--lang es] [--scale 3] [--dark] \
//            [--card-hover] [--card-flip 0…1] [--card-jitter r] [--lit-keys 0,2,4]
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
        AppDelegate.registerBundledFonts()
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

        // Drive the REAL icon↔QR card stack. `--card-hover` fans the QR out to
        // peek; `--card-flip <0…1>` scrubs the shuffle the app plays when you
        // click the icon; `--lit-keys` lights the icon's own little keyboard,
        // which is drawn into the icon face rather than laid over it.
        if let stack = firstCardStack(in: cv) {
            // Pose the deck before scrubbing — `--card-front qr` renders the
            // shuffle running the other way (QR tucking back behind the icon).
            if let front = val("--card-front") {
                stack.captureSetFront(icon: front != "qr")
            }
            if args.contains("--card-hover") { stack.setHoverPreview(true) }
            if let keys = val("--lit-keys"), !keys.isEmpty {
                stack.setLitKeys(Set(keys.split(separator: ",").compactMap { Int($0) }))
            }
            if let p = val("--card-flip").flatMap(Double.init) {
                let jitter = CGFloat(Double(val("--card-jitter") ?? "0") ?? 0)
                stack.captureScrubShuffle(progress: CGFloat(p), jitter: jitter)
            }
            // `--emit-card <path>`: where the card stack sits, as fractions of
            // the window, so a promo can park a cursor on the icon and burst
            // notes out of it without guessing at the layout.
            if let cardOut = val("--emit-card") {
                let f = stack.convert(stack.bounds, to: cv)
                let json = "{\"cx\":\(f.midX / cv.bounds.width)," +
                    "\"cy\":\(1 - f.midY / cv.bounds.height)," +
                    "\"side\":\(f.width / cv.bounds.width)}\n"
                do { try Data(json.utf8).write(to: URL(fileURLWithPath: cardOut)) } catch {
                    FileHandle.standardError.write(Data("✗ write \(cardOut): \(error)\n".utf8))
                    exit(1)
                }
            }
            cv.layoutSubtreeIfNeeded()
        }
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

    /// Depth-first search for the icon↔QR card stack in the About tree.
    private static func firstCardStack(in view: NSView) -> CardStackView? {
        if let stack = view as? CardStackView { return stack }
        for sub in view.subviews {
            if let found = firstCardStack(in: sub) { return found }
        }
        return nil
    }
}
