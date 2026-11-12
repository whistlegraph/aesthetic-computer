import AppKit
import UniformTypeIdentifiers
import AirbrushCore

final class AirbrushApp: NSObject, NSApplicationDelegate, NSWindowDelegate {
    private var window: NSWindow!
    private var canvas: PaintCanvas!
    private var choices: [NSPopUpButton] = []
    private var extraSettings: [NSMenuItem] = []
    private let smoothingChoice = NSPopUpButton(frame: .zero, pullsDown: false)
    private let modeButton = NSButton(title: "Mouse ⇥", target: nil, action: nil)
    private var documentURL: URL?
    private let saveQueue = DispatchQueue(label: "computer.aesthetic.airbrush.save", qos: .utility)
    private var canAutosave = true
    private var autosaveURL: URL {
        FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("Aesthetic Airbrush/Autosave.airbrush")
    }

    func applicationDidFinishLaunching(_ notification: Notification) {
        do { canvas = PaintCanvas(painter: try PaintRenderer()) }
        catch { alert(error); NSApp.terminate(nil); return }
        if let saved = UserDefaults.standard.object(forKey: "strokeSmoothing") as? Int,
           let mode = StrokeSmoothing(rawValue: saved) { canvas.setSmoothing(mode) }
        canvas.brush.marker = true
        canvas.brush.radius = 2
        canvas.brush.hardness = 1
        canvas.brush.flow = 1
        canvas.brush.scatter = 0
        canvas.brush.red = 0.06; canvas.brush.green = 0.06; canvas.brush.blue = 0.06
        window = NSWindow(contentRect: NSRect(x: 180, y: 180, width: 840, height: 620),
            styleMask: [.titled, .closable, .miniaturizable, .resizable], backing: .buffered, defer: false)
        window.title = ""
        window.titleVisibility = .hidden
        window.setAccessibilityLabel("Airbrush")
        window.minSize = NSSize(width: 420, height: 260)
        window.isReleasedWhenClosed = false
        window.delegate = self
        window.acceptsMouseMovedEvents = true
        window.tabbingMode = .disallowed
        let root = NSView()
        window.contentView = root
        let bar = NSStackView()
        bar.orientation = .horizontal
        bar.spacing = 3
        modeButton.target = self
        modeButton.action = #selector(toggleMode)
        modeButton.controlSize = .small
        modeButton.toolTip = "Switch Mouse / Trackpad (Tab). Escape returns to Mouse."
        bar.addArrangedSubview(modeButton)
        let color = NSColorWell()
        color.color = NSColor(calibratedRed: canvas.brush.red, green: canvas.brush.green, blue: canvas.brush.blue, alpha: 1)
        color.target = self
        color.action = #selector(colorChanged(_:))
        color.setAccessibilityLabel("Paint color")
        color.widthAnchor.constraint(equalToConstant: 24).isActive = true
        color.heightAnchor.constraint(equalToConstant: 24).isActive = true
        bar.addArrangedSubview(color)
        for parameter in 0..<2 {
            BrushSteps.set(&canvas.brush, parameter, BrushSteps.index(canvas.brush, parameter))
            let choice = NSPopUpButton(frame: .zero, pullsDown: false)
            choice.controlSize = .small
            choice.font = .systemFont(ofSize: 11)
            choice.tag = parameter
            choice.target = self
            choice.action = #selector(settingChanged(_:))
            choice.addItems(withTitles: BrushSteps.values[parameter].indices.map { BrushSteps.title(parameter, $0) })
            choice.setAccessibilityLabel(BrushSteps.names[parameter])
            choice.toolTip = ["Radius in screen pixels (R or [ ])", "Edge: soft / medium / hard (H)", "Flow percentage (F)", "Scatter percentage (T)"][parameter]
            choices.append(choice)
            bar.addArrangedSubview(choice)
        }
        smoothingChoice.controlSize = .small
        smoothingChoice.font = .systemFont(ofSize: 11)
        smoothingChoice.addItems(withTitles: StrokeSmoothing.allCases.map(\.title))
        smoothingChoice.selectItem(at: canvas.smoothing.rawValue)
        smoothingChoice.target = self
        smoothingChoice.action = #selector(smoothingChanged(_:))
        smoothingChoice.setAccessibilityLabel("Stroke smoothing")
        smoothingChoice.toolTip = "String: pull a 6 / 12 / 24-point tether; stops at the brush tip. Smooth 1–3: adaptive jitter filtering with endpoint catch-up."
        bar.addArrangedSubview(smoothingChoice)
        let spacer = NSView()
        spacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        bar.addArrangedSubview(spacer)
        let more = NSPopUpButton(frame: .zero, pullsDown: true)
        more.controlSize = .small
        more.addItem(withTitle: "⋯")
        more.setAccessibilityLabel("More controls")
        for parameter in 2..<4 {
            let parent = NSMenuItem(title: BrushSteps.names[parameter], action: nil, keyEquivalent: "")
            let submenu = NSMenu(title: BrushSteps.names[parameter])
            for index in BrushSteps.values[parameter].indices {
                let item = NSMenuItem(title: "\(Int(BrushSteps.values[parameter][index] * 100))%", action: #selector(extraSettingChanged(_:)), keyEquivalent: "")
                item.tag = parameter * 100 + index; item.target = self
                submenu.addItem(item); extraSettings.append(item)
            }
            parent.submenu = submenu; more.menu?.addItem(parent)
        }
        more.menu?.addItem(.separator())
        for (title, action) in [("Scatter seed…", #selector(editSeed)), ("New seed", #selector(newSeed)), ("Controls…", #selector(help))] {
            let item = NSMenuItem(title: title, action: action, keyEquivalent: "")
            item.target = self
            more.menu?.addItem(item)
        }
        bar.addArrangedSubview(more)
        for view in [canvas!, bar] {
            view.translatesAutoresizingMaskIntoConstraints = false
            root.addSubview(view)
        }
        NSLayoutConstraint.activate([
            canvas.topAnchor.constraint(equalTo: root.topAnchor),
            canvas.leadingAnchor.constraint(equalTo: root.leadingAnchor),
            canvas.trailingAnchor.constraint(equalTo: root.trailingAnchor),
            bar.topAnchor.constraint(equalTo: canvas.bottomAnchor, constant: 4),
            bar.leadingAnchor.constraint(equalTo: root.leadingAnchor, constant: 6),
            bar.trailingAnchor.constraint(equalTo: root.trailingAnchor, constant: -6),
            bar.bottomAnchor.constraint(equalTo: root.bottomAnchor, constant: -4),
            bar.heightAnchor.constraint(equalToConstant: 28),
        ])
        canvas.onChange = { [weak self] in self?.autosave() }
        canvas.onBrushChange = { [weak self] in self?.syncControls() }
        canvas.onModeChange = { [weak self] in
            guard let self else { return }
            self.modeButton.title = self.canvas.inputMode == .mouse ? "Mouse ⇥" : "Trackpad ⇥"
        }
        canvas.onError = { [weak self] error in self?.alert(error) }
        if FileManager.default.fileExists(atPath: autosaveURL.path) {
            do { canvas.load(try Painting.decode(Data(contentsOf: autosaveURL))) }
            catch { canAutosave = false; alert(error) }
        }
        if CommandLine.arguments.contains("--demo") { canvas.load(Self.demo()); canAutosave = false }
        syncControls()
        window.makeKeyAndOrderFront(nil)
        window.makeFirstResponder(canvas)
        NSApp.activate(ignoringOtherApps: true)
    }

    @objc func smoothingChanged(_ sender: NSPopUpButton) {
        guard let mode = StrokeSmoothing(rawValue: sender.indexOfSelectedItem) else { return }
        canvas.setSmoothing(mode)
        UserDefaults.standard.set(mode.rawValue, forKey: "strokeSmoothing")
        window.makeFirstResponder(canvas)
    }
    @objc func extraSettingChanged(_ sender: NSMenuItem) {
        BrushSteps.set(&canvas.brush, sender.tag / 100, sender.tag % 100)
        syncControls(); canvas.redraw(artwork: false); window.makeFirstResponder(canvas)
    }
    @objc func settingChanged(_ sender: NSPopUpButton) {
        BrushSteps.set(&canvas.brush, sender.tag, sender.indexOfSelectedItem)
        canvas.redraw(artwork: false)
        window.makeFirstResponder(canvas)
    }
    @objc func colorChanged(_ sender: NSColorWell) {
        guard let color = sender.color.usingColorSpace(.deviceRGB) else { return }
        canvas.brush.red = color.redComponent
        canvas.brush.green = color.greenComponent
        canvas.brush.blue = color.blueComponent
    }
    @objc func editSeed() {
        canvas.resetInput()
        let alert = NSAlert()
        alert.messageText = "Scatter seed"
        let field = NSTextField(string: String(canvas.seed))
        field.frame = NSRect(x: 0, y: 0, width: 240, height: 24)
        field.setAccessibilityLabel("Scatter seed, unsigned integer")
        alert.accessoryView = field
        alert.addButton(withTitle: "Apply")
        alert.addButton(withTitle: "Cancel")
        alert.beginSheetModal(for: window) { [weak self] response in
            guard let self else { return }
            if response == .alertFirstButtonReturn {
                if let value = UInt64(field.stringValue) { self.canvas.seed = value }
                else { NSSound.beep() }
            }
            self.window.makeFirstResponder(self.canvas)
        }
    }
    @objc func newSeed() { canvas.seed = Spray.bits(seed: canvas.seed, index: 0); syncControls(); window.makeFirstResponder(canvas) }
    private func syncControls() {
        guard canvas != nil else { return }
        for i in choices.indices { choices[i].selectItem(at: BrushSteps.index(canvas.brush, i)) }
        for item in extraSettings {
            item.state = BrushSteps.index(canvas.brush, item.tag / 100) == item.tag % 100 ? .on : .off
        }
    }
    @objc func undo() { canvas.undoStroke() }
    @objc func redo() { canvas.redoStroke() }
    @objc func toggleMode() { window.makeFirstResponder(canvas); canvas.toggleMode() }
    @objc func help() {
        canvas.resetInput()
        let alert = NSAlert()
        alert.messageText = "Airbrush controls"
        alert.informativeText = "Tab switches Mouse / Trackpad.\nMouse: click and drag, or hold Space. Brush follows the OS pointer.\nTrackpad: every finger paints on contact. Lift to stop. One undo removes the simultaneous gesture. Escape returns to Mouse.\n\nWASD moves through the painting. Hold Q / E or ⌘− / ⌘+ to zoom smoothly at the brush. ⌘= also zooms in. Shift slows movement.\n\nIn Mouse mode, hold R / H / F / T and slide horizontally to step radius / edge / flow / scatter. [ ] also changes radius.\n\nString smoothing holds the brush on a short tether; lift stops at the brush tip. Smooth 1–3 adapt to speed and catch up at lift. Off follows raw input. Flow and scatter are in ⋯.\n\nN changes the seed. The same seed and recorded path reproduce the scatter.\n\nZ / Y undo / redo. 0 resets the view.\n⌘S saves strokes. ⌘O opens. ⇧⌘E exports the visible view as PNG."
        alert.beginSheetModal(for: window) { [weak self] _ in self?.window.makeFirstResponder(self?.canvas) }
    }
    @objc func save() {
        canvas.resetInput()
        if let documentURL { write(to: documentURL); return }
        let panel = NSSavePanel()
        panel.nameFieldStringValue = "Painting.airbrush"
        panel.allowedContentTypes = [UTType(filenameExtension: "airbrush") ?? .json]
        if panel.runModal() == .OK, let url = panel.url { documentURL = url; write(to: url) }
        window.makeFirstResponder(canvas)
    }
    private func write(to url: URL) {
        do { try canvas.painting.encoded().write(to: url, options: .atomic) }
        catch { alert(error) }
    }
    @objc func openDocument() {
        canvas.resetInput()
        let panel = NSOpenPanel()
        panel.allowedContentTypes = [UTType(filenameExtension: "airbrush") ?? .json]
        if panel.runModal() == .OK, let url = panel.url {
            do {
                let document = try Painting.decode(Data(contentsOf: url))
                canvas.load(document)
                documentURL = url
                canAutosave = true
                autosave()
            } catch { alert(error) }
        }
        window.makeFirstResponder(canvas)
    }
    @objc func exportPNG() {
        canvas.resetInput()
        let panel = NSSavePanel()
        panel.nameFieldStringValue = "Painting.png"
        panel.allowedContentTypes = [.png]
        if panel.runModal() == .OK, let url = panel.url {
            do {
                let width = max(1, Int(canvas.drawableSize.width))
                let height = max(1, Int(canvas.drawableSize.height))
                let pixels = try canvas.painter.snapshot(dabs: canvas.dabs, camera: canvas.painting.camera,
                    size: canvas.size, width: width, height: height)
                try PaintRenderer.png(pixels, width: width, height: height).write(to: url, options: .atomic)
            } catch { alert(error) }
        }
        window.makeFirstResponder(canvas)
    }
    private func autosave() {
        guard canAutosave else { return }
        let document = canvas.painting
        let url = autosaveURL
        saveQueue.async { [weak self] in
            do {
                try FileManager.default.createDirectory(at: url.deletingLastPathComponent(), withIntermediateDirectories: true)
                try document.encoded().write(to: url, options: .atomic)
            } catch { DispatchQueue.main.async { self?.alert(error) } }
        }
    }
    private func alert(_ error: Error) {
        let alert = NSAlert(error: error)
        alert.runModal()
    }
    func windowDidResignKey(_ notification: Notification) { canvas.resetInput() }
    func applicationDidResignActive(_ notification: Notification) { canvas.resetInput() }
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
    func applicationShouldTerminate(_ sender: NSApplication) -> NSApplication.TerminateReply {
        canvas?.resetInput()
        if canvas != nil { autosave(); saveQueue.sync {} }
        return .terminateNow
    }

    static func demo() -> Painting {
        var p = Painting()
        for row in 0..<3 {
            var brush = Brush()
            brush.radius = 42
            brush.hardness = [0.0, 1.0, 0.35][row]
            brush.scatter = row == 2 ? 0.85 : 0
            brush.flow = 0.6
            brush.red = [0.12, 0.8, 0.16][row]
            brush.green = [0.22, 0.2, 0.45][row]
            brush.blue = [0.8, 0.32, 0.4][row]
            let samples = (0..<360).map { i -> Sample in
                let t = Double(i) / 359
                return Sample(point: Point(-240 + t * 480, Double(row - 1) * 130 + sin(t * .pi * 2) * 24), brush: brush)
            }
            p.strokes.append(Stroke(seed: 42, samples: samples))
        }
        return p
    }
}
