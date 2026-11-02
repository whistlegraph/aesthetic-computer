import AppKit

// VisualizerCapture — headless capture of the REAL jam wall (the same
// JamWallView the overlay puts on screen, built through
// VisualizerOverlay.makeWall so the shot can't drift from what ships).
//
//   MenuBand --render-visualizer --out wall.png [--scale 2] [--size 1440x900]
//            [--program N]
//
// No audio engine runs headless, so nothing would ever light the raster. We
// pose it with a deterministic jam instead — a left-hand phrase on the roll,
// a kick/snare/hat pattern on the kit, a ring still crossing the half.
enum VisualizerCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-visualizer") else { return false }
        func val(_ f: String) -> String? {
            guard let i = args.firstIndex(of: f), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let out = val("--out") ?? "/tmp/visualizer.png"
        let scale = max(1.0, Double(val("--scale") ?? "2") ?? 2)

        // Default to a laptop-ish screen in points so the LED pitch reads the
        // way it will in the wild.
        var size = NSSize(width: 1440, height: 900)
        if let s = val("--size") {
            let parts = s.lowercased().split(separator: "x")
            if parts.count == 2, let w = Double(parts[0]), let h = Double(parts[1]) {
                size = NSSize(width: w, height: h)
            }
        }

        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)

        let controller = MenuBandController()
        let prog = UInt8(val("--program") ?? "0") ?? 0
        controller.setMelodicProgram(prog)

        // No controller on the wall: it would start the synth's capture tap
        // the moment it lands in a window, and headless there is no engine.
        let strip = VisualizerOverlay.makeWall(menuBand: nil, size: size)
        strip.tintColor = InstrumentListView.colorForProgram(Int(prog))
        strip.seedSyntheticJam()
        strip.layoutSubtreeIfNeeded()
        strip.displayIfNeeded()

        let bounds = strip.bounds
        let pw = Int((bounds.width * scale).rounded())
        let ph = Int((bounds.height * scale).rounded())
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: pw, pixelsHigh: ph,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0) else {
            FileHandle.standardError.write(Data("✗ bitmap alloc failed\n".utf8)); return true
        }
        rep.size = bounds.size                 // points; pixels are scale× → Retina capture
        strip.cacheDisplay(in: bounds, to: rep)
        guard let png = rep.representation(using: .png, properties: [:]) else {
            FileHandle.standardError.write(Data("✗ png encode failed\n".utf8)); return true
        }
        try? png.write(to: URL(fileURLWithPath: out))
        print("visualizer \(pw)x\(ph) (native \(Int(bounds.width))x\(Int(bounds.height))) → \(out)")
        return true
    }
}
