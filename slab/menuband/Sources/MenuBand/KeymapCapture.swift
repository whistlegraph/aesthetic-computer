import AppKit

// KeymapCapture — headless capture of the REAL full-screen keymap overlay
// (ExpandedPianoWaveformView: the big piano, QWERTY/Notepat map, waveform, and
// the mode row [LLMs · Notepat · Conventional · Gamepad]). Mirrors PopoverCLI:
// build the view tree against a non-bootstrapped controller, lay it out, snapshot
// via cacheDisplay. Used for the App Store keymap screenshot.
//
//   MenuBand --render-keymap --out keymap.png [--scale 3] [--dark] [--lang en]
//            [--notes 60,64,67]
//   MenuBand --render-keymap --seq frames.json --out-dir dir/   (animated)
//
// frames.json: [{"notes":[60,64],"levels":[0.1,…],"cursor":0.72}, …]
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
        // The view never enters a window, so `effectiveAppearance` would fall
        // back to the SYSTEM appearance rather than the one set on NSApp — and
        // `refresh()` restyles the scope from it. Pin it to the app's.
        view.appearance = app.appearance
        view.layoutSubtreeIfNeeded()
        var size = view.fittingSize
        if size.width < 100 || size.height < 100 { size = NSSize(width: 560, height: 420) }
        view.frame = NSRect(origin: .zero, size: size)
        view.layoutSubtreeIfNeeded()
        let bounds = view.bounds

        func snapshot(to path: String) -> Bool {
            let pw = Int((bounds.width * scale).rounded())
            let ph = Int((bounds.height * scale).rounded())
            guard let rep = NSBitmapImageRep(
                bitmapDataPlanes: nil, pixelsWide: pw, pixelsHigh: ph,
                bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0) else {
                FileHandle.standardError.write(Data("✗ bitmap alloc failed\n".utf8)); return false
            }
            rep.size = bounds.size
            view.cacheDisplay(in: bounds, to: rep)
            guard let png = rep.representation(using: .png, properties: [:]) else {
                FileHandle.standardError.write(Data("✗ png encode failed\n".utf8)); return false
            }
            do { try png.write(to: URL(fileURLWithPath: path)) } catch {
                FileHandle.standardError.write(Data("✗ write \(path): \(error)\n".utf8)); return false
            }
            return true
        }

        // `--seq frames.json --out-dir dir`: render a whole animated sequence
        // through ONE process. Each frame names the notes held and the audio
        // levels behind the scope, so the big piano, the QWERTY map and the LED
        // display all move with the music. Spawning a process per frame would
        // pay the view-tree build hundreds of times over.
        if let seqPath = val("--seq"), let dir = val("--out-dir") {
            struct Frame: Decodable {
                let notes: [UInt8]; let levels: [Float]; let cursor: Double
                let program: UInt8?     // the shot cycles instrument families
                let reverse: Bool?      // spacebar reverse-replay demo
                let scrub: Double?      // playhead swept back (0…1) while reversing
            }
            guard let data = FileManager.default.contents(atPath: seqPath),
                  let frames = try? JSONDecoder().decode([Frame].self, from: data) else {
                FileHandle.standardError.write(Data("✗ bad --seq \(seqPath)\n".utf8)); exit(1)
            }
            try? FileManager.default.createDirectory(
                atPath: dir, withIntermediateDirectories: true)
            var lastProgram = prog
            for (i, f) in frames.enumerated() {
                if let p = f.program, p != lastProgram {
                    controller.setMelodicProgram(p)
                    lastProgram = p
                }
                let reversing = f.reverse ?? false
                controller.captureReverse(reversing)
                controller.captureHold(notes: Set(f.notes), spaceHeld: reversing)
                view.refresh()
                view.seedWaveformForCapture(levels: f.levels, cursorAt: f.cursor,
                                            scrubBack: reversing ? (f.scrub ?? 0) : 0)
                view.displayIfNeeded()
                let path = "\(dir)/keymap-\(String(format: "%04d", i)).png"
                if !snapshot(to: path) { exit(1) }
            }
            print("keymap seq \(frames.count) frames → \(dir)")
            return true
        }

        // Single still: no audio engine runs headless, so paint a believable
        // synthetic note into the needle+buffer scope.
        if let notes = val("--notes"), !notes.isEmpty {
            controller.captureHold(notes: Set(notes.split(separator: ",").compactMap { UInt8($0) }))
            view.refresh()
        }
        view.seedWaveformForCapture()
        view.displayIfNeeded()
        _ = snapshot(to: out)
        print("keymap (native \(Int(bounds.width))x\(Int(bounds.height))) → \(out)")
        return true
    }
}
