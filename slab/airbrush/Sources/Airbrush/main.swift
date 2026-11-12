import AppKit
import AirbrushCore

if let index = CommandLine.arguments.firstIndex(of: "--verify-smoothing"), CommandLine.arguments.count > index + 1 {
    do { try SmoothingVerification.run(path: CommandLine.arguments[index + 1]); exit(0) }
    catch { fputs("Smoothing verification failed: \(error)\n", stderr); exit(1) }
}

if CommandLine.arguments.contains("--verify-input") {
    _ = NSApplication.shared
    do { try PaintCanvas.verifyContacts(); exit(0) }
    catch { fputs("Input verification failed: \(error)\n", stderr); exit(1) }
}

if let index = CommandLine.arguments.firstIndex(of: "--verify-cache"), CommandLine.arguments.count > index + 1 {
    do { try CacheVerification.run(directory: CommandLine.arguments[index + 1]); exit(0) }
    catch { fputs("Cache verification failed: \(error)\n", stderr); exit(1) }
}

if let index = CommandLine.arguments.firstIndex(of: "--verify-render"), CommandLine.arguments.count > index + 1 {
    do {
        let painter = try PaintRenderer()
        let demo = AirbrushApp.demo()
        let decoded = try Painting.decode(demo.encoded())
        let original = demo.strokes.flatMap(Spray.replay)
        let replayed = decoded.strokes.flatMap(Spray.replay)
        guard original == replayed else { fatalError("Stroke replay changed geometry") }
        let size = Point(800, 560)
        let a = try painter.snapshot(dabs: original, camera: demo.camera, size: size, width: 800, height: 560)
        let b = try painter.snapshot(dabs: replayed, camera: decoded.camera, size: size, width: 800, height: 560)
        guard a == b else { fatalError("GPU replay changed pixels") }
        // A blank rendering must differ: this catches missing shader draws.
        let blank = try painter.snapshot(dabs: [], camera: demo.camera, size: size, width: 800, height: 560)
        guard a != blank else { fatalError("GPU produced a blank canvas") }
        var softBrush = Brush()
        softBrush.radius = 60
        softBrush.flow = 1
        softBrush.scatter = 0
        softBrush.hardness = 0
        var hardBrush = softBrush
        hardBrush.hardness = 1
        let softDabs = Spray.replay(Stroke(seed: 0, samples: Array(repeating: Sample(point: Point(), brush: softBrush), count: 60)))
        let hardDabs = Spray.replay(Stroke(seed: 0, samples: Array(repeating: Sample(point: Point(), brush: hardBrush), count: 60)))
        let soft = try painter.snapshot(dabs: softDabs, camera: Camera(), size: Point(160, 160), width: 160, height: 160)
        let hard = try painter.snapshot(dabs: hardDabs, camera: Camera(), size: Point(160, 160), width: 160, height: 160)
        // At 75% radius the hard disk must deposit substantially more pigment.
        // Read the red channel of our blue paint in BGRA storage.
        let edge = (80 * 160 + 125) * 4 + 2
        guard Int(soft[edge]) - Int(hard[edge]) > 40 else { fatalError("Hardness does not change the GPU profile") }
        let url = URL(fileURLWithPath: CommandLine.arguments[index + 1])
        try PaintRenderer.png(a, width: 800, height: 560).write(to: url)
        print("PASS Metal rendering and save/reopen pixel replay: \(original.count) dabs; \(url.path)")
        exit(0)
    } catch { fputs("Render verification failed: \(error)\n", stderr); exit(1) }
}

let app = NSApplication.shared
let delegate = AirbrushApp()
app.delegate = delegate
app.setActivationPolicy(.regular)
let menu = NSMenu()
func addMenu(_ title: String, items: [(String, Selector, String)]) {
    let item = NSMenuItem()
    let submenu = NSMenu(title: title)
    for (label, action, key) in items {
        let entry = NSMenuItem(title: label, action: action, keyEquivalent: key)
        entry.target = action == #selector(NSApplication.terminate(_:)) ? app : delegate
        submenu.addItem(entry)
    }
    item.submenu = submenu
    menu.addItem(item)
}
addMenu("Airbrush", items: [("Controls", #selector(AirbrushApp.help), "?"),
    ("Quit Airbrush", #selector(NSApplication.terminate(_:)), "q")])
addMenu("File", items: [("Open…", #selector(AirbrushApp.openDocument), "o"),
    ("Save…", #selector(AirbrushApp.save), "s"),
    ("Export View as PNG…", #selector(AirbrushApp.exportPNG), "E")])
addMenu("Edit", items: [("Undo Stroke", #selector(AirbrushApp.undo), "z"),
    ("Redo Stroke", #selector(AirbrushApp.redo), "Z")])
app.mainMenu = menu
app.run()
