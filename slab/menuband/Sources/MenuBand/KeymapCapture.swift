import AppKit

// KeymapCapture — headless capture of the REAL full-screen keymap overlay
// (ExpandedPianoWaveformView: the big piano, QWERTY/Notepat map, waveform, and
// the mode row [LLMs · Notepat · Conventional · Gamepad]). Mirrors PopoverCLI:
// build the view tree against a non-bootstrapped controller, lay it out, snapshot
// via cacheDisplay. Used for the App Store keymap screenshot.
//
//   MenuBand --render-keymap --out keymap.png [--scale 3] [--dark] [--lang en]
enum KeymapCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-keymap") else { return false }
        func val(_ f: String) -> String? {
            guard let i = args.firstIndex(of: f), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let out = val("--out") ?? "/tmp/keymap.png"
        let scale = max(1.0, Double(val("--scale") ?? "3") ?? 3)

        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        app.appearance = NSAppearance(named: args.contains("--dark") ? .darkAqua : .aqua)
        AppDelegate.registerBundledFonts()
        if let lang = val("--lang") { Localization.current = lang }

        let controller = MenuBandController()
        let prog = UInt8(val("--program") ?? "0") ?? 0
        controller.setMelodicProgram(prog)

        let view = ExpandedPianoWaveformView(menuBand: controller)
        view.layoutSubtreeIfNeeded()
        var size = view.fittingSize
        if size.width < 100 || size.height < 100 { size = NSSize(width: 560, height: 420) }
        view.frame = NSRect(origin: .zero, size: size)
        view.layoutSubtreeIfNeeded()
        view.displayIfNeeded()

        let bounds = view.bounds
        let pw = Int((bounds.width * scale).rounded())
        let ph = Int((bounds.height * scale).rounded())
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: pw, pixelsHigh: ph,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0) else {
            FileHandle.standardError.write(Data("✗ bitmap alloc failed\n".utf8)); return true
        }
        rep.size = bounds.size
        view.cacheDisplay(in: bounds, to: rep)
        guard let png = rep.representation(using: .png, properties: [:]) else {
            FileHandle.standardError.write(Data("✗ png encode failed\n".utf8)); return true
        }
        try? png.write(to: URL(fileURLWithPath: out))
        print("keymap \(pw)x\(ph) (native \(Int(bounds.width))x\(Int(bounds.height))) → \(out)")
        return true
    }
}
