import AppKit

// MenubarCapture — headless capture of the REAL menubar piano graphic, so
// promos/reels use the app's exact KeyboardIconRenderer output (proportions,
// short keys, the dynamic icon on the right) instead of a reproduction.
//
// Mirrors KLCLI.runIfRequested: short-circuits app startup when invoked.
//
//   MenuBand --render-menubar --notes 60,64,67 --out frame.png \
//            [--voice "Marimba"] [--program 0] [--scale 3] \
//            [--light] [--accent "#000000"] [--midi] [--no-settings]
//            [--key-accent] [--emit-keys keys.json]
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

        // Pin the accent for promo renders (`--accent "#000000"` → black keys).
        // Absent the flag the render follows whatever accent the rendering
        // machine happens to be set to, which is not something a reel should
        // depend on.
        if let hex = val("--accent") { KeyboardIconRenderer.accentOverride = NSColor(hex: hex) }
        // `--key-accent`: a pressed key lights in its own ROYGBIV color rather
        // than the accent. Under a black accent the accent-lit state can only
        // come out grey, and the rainbow is the strip's whole idea anyway.
        KeyboardIconRenderer.perKeyAccent = args.contains("--key-accent")

        // `--emit-keys <path>`: dump where each key sits (fraction of image
        // width) and what color its stripe is, so a promo can fire particles
        // off the exact key that lit. Written once; the notes don't move.
        if let keysOut = val("--emit-keys") {
            let rows = KeyboardIconRenderer.bareKeyGeometry().map { key in
                "{\"midi\":\(key.midi),\"cx\":\(String(format: "%.6f", key.cx))," +
                "\"hex\":\(key.hex.map { "\"\($0)\"" } ?? "null")}"
            }
            let json = Data(("[" + rows.joined(separator: ",") + "]\n").utf8)
            do { try json.write(to: URL(fileURLWithPath: keysOut)) } catch {
                FileHandle.standardError.write(Data("✗ write \(keysOut): \(error)\n".utf8))
                exit(1)
            }
        }

        // `enabled:` is the renderer's name for MIDI mode (see AppDelegate,
        // which passes `menuBand.midiMode`). Forcing it true made the voice
        // badge read a permanent "M" and threw away `--voice` / `--program`
        // entirely. Off by default so the badge shows the live voice slot the
        // way the shipping menubar does; `--midi` restores the "M".
        //
        // `--no-settings` drops the whole right-hand chip (music-note glyph,
        // voice badge, LED bars) and yields the bare keyboard — a strip that
        // can sit centered in a promo frame without the chip pulling its
        // visual weight to one side.
        let img = KeyboardIconRenderer.image(
            litNotes: notes, enabled: args.contains("--midi"), melodicProgram: program,
            voiceLabel: voice, includeSettings: !args.contains("--no-settings"),
            layout: .fixedCanvas)

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
        // Not `try?`: a failed write (missing directory, most often) would
        // otherwise exit 0 with nothing on stderr, and the caller would render
        // a whole reel around a strip that was never captured.
        do { try png.write(to: URL(fileURLWithPath: out)) } catch {
            FileHandle.standardError.write(Data("✗ write \(out): \(error)\n".utf8))
            exit(1)
        }
        print("menubar \(pw)x\(ph) (native \(Int(img.size.width))x\(Int(img.size.height))) notes=\(notes.sorted()) → \(out)")
        return true
    }
}

private extension NSColor {
    /// `#RRGGBB` (or bare `RRGGBB`) → color. Only the promo capture needs
    /// this; the app never names a color by hex.
    convenience init?(hex: String) {
        var s = hex.trimmingCharacters(in: .whitespaces)
        if s.hasPrefix("#") { s.removeFirst() }
        guard s.count == 6, let v = UInt32(s, radix: 16) else { return nil }
        self.init(srgbRed: CGFloat((v >> 16) & 0xFF) / 255,
                  green: CGFloat((v >> 8) & 0xFF) / 255,
                  blue: CGFloat(v & 0xFF) / 255, alpha: 1)
    }
}
