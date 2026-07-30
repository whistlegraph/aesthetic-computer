import AppKit

// Headless capture of the shipping QwertyLayoutView, isolated from every
// popover/fullscreen surface. Notes resolve through MenuBandController's real
// keymap inverse; --keys poses live control keycodes such as left Command (55),
// right Command (54), or Escape (53).
//
//   MenuBand --render-qwerty --notes 60 --keys 55 --out qwerty.png --scale 4
enum QwertyCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-qwerty") else { return false }
        func val(_ flag: String) -> String? {
            guard let i = args.firstIndex(of: flag), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let out = val("--out") ?? "/tmp/qwerty.png"
        let renderScale = max(1, Double(val("--scale") ?? "4") ?? 4)
        let notes = Set((val("--notes") ?? "").split(separator: ",").compactMap { UInt8($0) })
        let keys = Set((val("--keys") ?? "").split(separator: ",").compactMap { UInt16($0) })

        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        app.appearance = NSAppearance(named: args.contains("--dark") ? .darkAqua : .aqua)

        let controller = MenuBandController()
        controller.captureHold(notes: notes, extraKeyCodes: keys)
        let view = QwertyLayoutView()
        view.appearance = app.appearance
        view.scale = 4
        view.keymap = controller.keymap
        view.litKeyCodes = controller.heldKeyCodes()
        view.voiceColor = InstrumentListView.colorForProgram(Int(controller.effectiveMelodicProgram))
        view.frame = NSRect(origin: .zero, size: view.intrinsicContentSize)
        view.layoutSubtreeIfNeeded()
        view.displayIfNeeded()

        let bounds = view.bounds
        let pw = Int((bounds.width * renderScale).rounded())
        let ph = Int((bounds.height * renderScale).rounded())
        guard let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: pw,
            pixelsHigh: ph, bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
            isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0,
            bitsPerPixel: 0) else { return true }
        rep.size = bounds.size
        view.cacheDisplay(in: bounds, to: rep)
        guard let png = rep.representation(using: .png, properties: [:]) else { return true }
        do { try png.write(to: URL(fileURLWithPath: out)) }
        catch { FileHandle.standardError.write(Data("qwerty write failed: \(error)\n".utf8)); exit(1) }
        print("qwerty \(pw)x\(ph) held=\(view.litKeyCodes.sorted()) keys=\(keys.sorted()) notes=\(notes.sorted()) → \(out)")
        return true
    }
}
