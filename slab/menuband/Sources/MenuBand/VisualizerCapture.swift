import AppKit

// VisualizerCapture — headless capture of the REAL full-screen LED visualizer
// (the same WaveformStripView the overlay puts on screen, built through
// VisualizerOverlay.makeStrip so the shot can't drift from what ships).
//
//   MenuBand --render-visualizer --out wall.png [--scale 2] [--size 1440x900]
//            [--program N] [--cursor 0.72]
//
// No audio engine runs headless, so the live sweep never accumulates columns
// and the wall would paint as an empty off-grid. We seed the raster with the
// same synthetic note the popover capture uses: silence, a strike, a decaying
// tail trailing up to the write cursor.
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

        let strip = VisualizerOverlay.makeStrip(menuBand: controller, size: size)
        strip.tintColor = InstrumentListView.colorForProgram(Int(prog))
        strip.seedSyntheticWaveform()
        if let cursor = Double(val("--cursor") ?? "") {
            // Re-pose the sweep from real-ish levels when a cursor is asked for.
            let levels = (0..<220).map { i -> Float in
                let t = Float(i) / 220
                return abs(sinf(t * 34)) * (0.25 + 0.75 * expf(-t * 1.6))
            }
            strip.seedWaveform(levels: levels, cursorAt: cursor)
        }
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
