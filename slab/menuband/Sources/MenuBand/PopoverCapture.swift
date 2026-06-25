import AppKit

// PopoverCapture — headless capture of the REAL popover interface (the actual
// MenuBandPopoverViewController view tree: instrument cluster, GM grid, QWERTY
// map, mode picker, mix slider, octave stepper) so App Store screenshots use
// the shipping UI, not a mock. Mirrors AboutCLI.runIfRequested.
//
//   MenuBand --render-popover --out popover.png [--scale 3] [--dark] [--program N]
//
// Unlike AboutCapture (a self-contained window controller), the popover VC
// needs a live MenuBandController — it reads instrument/mode/octave state from
// it. We construct one but never call bootstrap(), so the audio engine / MIDI
// never start; the view tree lays out and paints from the controller's default
// state. The Metal mini-waveform strip may come back empty under cacheDisplay
// (Metal layers don't always honor it) — that's an acceptable dark strip; the
// instrument surface is the point.
enum PopoverCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-popover") else { return false }
        func val(_ f: String) -> String? {
            guard let i = args.firstIndex(of: f), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let out = val("--out") ?? "/tmp/popover.png"
        let scale = max(1.0, Double(val("--scale") ?? "3") ?? 3)

        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        app.appearance = NSAppearance(named: args.contains("--dark") ? .darkAqua : .aqua)
        // Register YWFT Processing so the instrument title renders in the real
        // typeface (not the system fallback) — this path skips app launch.
        AppDelegate.registerBundledFonts()
        if let lang = val("--lang") { Localization.current = lang }   // QA: render the popover localized

        // Load with the instrument chart EXPANDED so the shot shows the full
        // GM grid + QWERTY map, not just the collapsed readout. This writes to
        // the CLI binary's own defaults domain (process-name based), not the
        // installed app's (computer.aestheticcomputer.menuband), so it doesn't
        // disturb the user's real chart state.
        if !args.contains("--collapsed") {
            UserDefaults.standard.set(true, forKey: "MBInstrumentChartExpanded")
        }

        let controller = MenuBandController()
        // Pick a melodic GM program so the readout names a recognizable
        // instrument (defaults to 0 = Acoustic Grand Piano) instead of the
        // mic "Sample Voice" the cold controller starts on.
        let prog = UInt8(val("--program") ?? "0") ?? 0
        controller.setMelodicProgram(prog)

        let vc = MenuBandPopoverViewController()
        vc.menuBand = controller
        // Accessing `.view` triggers loadView (the macOS 11-safe way; there is
        // no loadViewIfNeeded before macOS 14). Then pull instrument/mode/
        // octave state across so the readout, grid selection, and mode buttons
        // paint their real values rather than blank defaults.
        let v = vc.view
        vc.syncFromController()
        vc.refreshInstrumentVisuals()
        // Hide the gamepad config row — it's noise in a marketing shot. Tagged
        // with identifier "mb.gamepadRow" in MenuBandPopoverViewController;
        // hiding an NSStackView arranged subview collapses its space.
        if !args.contains("--keep-gamepad") {
            func hideTagged(_ view: NSView) {
                if view.identifier?.rawValue == "mb.gamepadRow" { view.isHidden = true; return }
                view.subviews.forEach(hideTagged)
            }
            hideTagged(v)
        }
        v.layoutSubtreeIfNeeded()

        // Seed the top mini-scope with a believable synthetic waveform — flat
        // silence on the left, noisy played audio filling toward the center
        // playhead. Headless, no audio engine runs, so the live capture loop
        // never accumulates columns and the strip would otherwise paint as an
        // empty dark bar. WaveformStripView is pure Core Graphics, so the
        // seeded bars render fine under cacheDisplay.
        func seedWaveformStrips(_ view: NSView) {
            if let strip = view as? WaveformStripView { strip.seedSyntheticWaveform() }
            view.subviews.forEach(seedWaveformStrips)
        }
        seedWaveformStrips(v)

        var size = v.fittingSize
        if size.width < 100 || size.height < 100 { size = NSSize(width: 360, height: 560) }
        v.frame = NSRect(origin: .zero, size: size)
        v.layoutSubtreeIfNeeded()
        v.displayIfNeeded()

        let bounds = v.bounds
        let pw = Int((bounds.width * scale).rounded())
        let ph = Int((bounds.height * scale).rounded())
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: pw, pixelsHigh: ph,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0) else {
            FileHandle.standardError.write(Data("✗ bitmap alloc failed\n".utf8)); return true
        }
        rep.size = bounds.size                 // points; pixels are scale× → Retina capture
        v.cacheDisplay(in: bounds, to: rep)
        guard let png = rep.representation(using: .png, properties: [:]) else {
            FileHandle.standardError.write(Data("✗ png encode failed\n".utf8)); return true
        }
        try? png.write(to: URL(fileURLWithPath: out))
        print("popover \(pw)x\(ph) (native \(Int(bounds.width))x\(Int(bounds.height))) → \(out)")
        return true
    }
}

// JamCapture — headless capture of the REAL "Looking For Players?" (Jam)
// window: the AC badge + computer-club invite (looking-for-players.png).
// Mirrors AboutCLI. Usage: MenuBand --render-jam --out jam.png [--scale 3] [--dark]
enum JamCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-jam") else { return false }
        func val(_ f: String) -> String? {
            guard let i = args.firstIndex(of: f), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let out = val("--out") ?? "/tmp/jam.png"
        let scale = max(1.0, Double(val("--scale") ?? "3") ?? 3)
        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        app.appearance = NSAppearance(named: args.contains("--dark") ? .darkAqua : .aqua)
        AppDelegate.registerBundledFonts()
        if let lang = val("--lang") { Localization.current = lang }

        let ctrl = JamWindowController()
        guard let win = ctrl.window, let cv = win.contentView else {
            FileHandle.standardError.write(Data("✗ no jam content view\n".utf8)); return true
        }
        cv.layoutSubtreeIfNeeded()
        let fit = cv.fittingSize
        win.setContentSize(NSSize(width: max(240, fit.width), height: max(200, fit.height)))
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
        rep.size = bounds.size
        cv.cacheDisplay(in: bounds, to: rep)
        guard let png = rep.representation(using: .png, properties: [:]) else {
            FileHandle.standardError.write(Data("✗ png encode failed\n".utf8)); return true
        }
        try? png.write(to: URL(fileURLWithPath: out))
        print("jam \(pw)x\(ph) (native \(Int(bounds.width))x\(Int(bounds.height))) → \(out)")
        return true
    }
}
