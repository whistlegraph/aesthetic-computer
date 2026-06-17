import AppKit

// MenubarCapture — headless capture of the REAL menubar piano graphic, so
// promos/reels use the app's exact KeyboardIconRenderer output (proportions,
// short keys, the dynamic icon on the right) instead of a reproduction.
//
// Mirrors KLCLI.runIfRequested: short-circuits app startup when invoked.
//
//   MenuBand --render-menubar --notes 60,64,67 --out frame.png \
//            [--voice "Marimba"] [--program 0] [--scale 3]
//
// Renders KeyboardIconRenderer.image(...) (the full status-item icon: piano
// keys + settings/voice/MIDI chips) to a TRANSPARENT PNG at `scale`× the
// native pixel size (so it stays crisp when composited into a 1080-wide reel).
// --notes is the set of lit MIDI notes (the keys that glow this frame).
enum MenubarCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-menubar") else { return false }
        func val(_ f: String) -> String? {
            guard let i = args.firstIndex(of: f), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let notes: Set<UInt8> = Set(
            (val("--notes") ?? "").split(separator: ",")
                .compactMap { UInt8($0.trimmingCharacters(in: .whitespaces)) })
        let out = val("--out") ?? "/tmp/menubar.png"
        let voice = val("--voice")
        let program = UInt8(val("--program") ?? "0") ?? 0
        let scale = max(1.0, Double(val("--scale") ?? "3") ?? 3)

        // KeyboardIconRenderer reads NSApp.effectiveAppearance, so the shared
        // application must exist. Force dark aqua so the keys render in their
        // slate menubar theme (matches the reel's dark menu bar). --light flips it.
        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        app.appearance = NSAppearance(named: args.contains("--light") ? .aqua : .darkAqua)

        let img = KeyboardIconRenderer.image(
            litNotes: notes, enabled: true, melodicProgram: program,
            voiceLabel: voice, includeSettings: true, layout: .fixedCanvas)

        let pw = Int((img.size.width * scale).rounded())
        let ph = Int((img.size.height * scale).rounded())
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: pw, pixelsHigh: ph,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0) else {
            FileHandle.standardError.write(Data("✗ bitmap alloc failed\n".utf8)); return true
        }
        rep.size = NSSize(width: pw, height: ph)
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
        img.draw(in: NSRect(x: 0, y: 0, width: pw, height: ph),
                 from: NSRect(origin: .zero, size: img.size),
                 operation: .copy, fraction: 1.0)
        NSGraphicsContext.restoreGraphicsState()
        guard let png = rep.representation(using: .png, properties: [:]) else {
            FileHandle.standardError.write(Data("✗ png encode failed\n".utf8)); return true
        }
        try? png.write(to: URL(fileURLWithPath: out))
        print("menubar \(pw)x\(ph) (native \(Int(img.size.width))x\(Int(img.size.height))) notes=\(notes.sorted()) → \(out)")
        return true
    }
}
