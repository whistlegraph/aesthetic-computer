import AppKit

/// On-demand seat editor. Network probes and configuration transactions stay off AppKit's thread.
final class DisplayLayoutWindow: NSObject, NSWindowDelegate, NSTextFieldDelegate {
    private static var shared: DisplayLayoutWindow?
    private let window: NSWindow
    private let canvas = DisplaySeatCanvas()
    private let status = NSTextField(labelWithString: "Detecting displays…")
    private let selection = NSTextField(labelWithString: "")
    private var fields: [NSTextField] = []
    private var buttons: [NSButton] = []
    private var applyButton: NSButton!
    private var restoreButton: NSButton!
    private var state: [String: Any] = [:]
    private var original: [[String: Any]] = []
    private var busy = false
    private var dirty = false

    static func show() {
        let controller = shared ?? DisplayLayoutWindow()
        shared = controller
        NSApp.activate(ignoringOtherApps: true)
        controller.window.makeKeyAndOrderFront(nil)
    }

    override init() {
        window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 1000, height: 650),
                          styleMask: [.titled, .closable, .miniaturizable, .resizable], backing: .buffered, defer: false)
        super.init()
        window.title = "Desktop Layout"
        window.minSize = NSSize(width: 760, height: 520)
        window.isReleasedWhenClosed = false
        window.delegate = self
        window.setFrameAutosaveName("SlabDesktopLayout")
        build()
        window.center()
        load()
    }

    private func build() {
        let content = NSView()
        window.contentView = content
        let stack = NSStackView()
        stack.orientation = .vertical
        stack.spacing = 14
        stack.edgeInsets = NSEdgeInsets(top: 18, left: 20, bottom: 18, right: 20)
        stack.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: content.leadingAnchor), stack.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            stack.topAnchor.constraint(equalTo: content.topAnchor), stack.bottomAnchor.constraint(equalTo: content.bottomAnchor),
        ])
        func button(_ title: String, _ action: Selector) -> NSButton {
            let b = NSButton(title: title, target: self, action: action)
            b.bezelStyle = .rounded; buttons.append(b); return b
        }
        let bar = NSStackView()
        bar.spacing = 8
        bar.addArrangedSubview(button("Identify", #selector(identify)))
        bar.addArrangedSubview(button("Refresh", #selector(refresh)))
        bar.addArrangedSubview(button("Reset Draft", #selector(reset)))
        let spacer = NSView(); spacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        bar.addArrangedSubview(spacer)
        restoreButton = button("Restore Previous", #selector(restore))
        bar.addArrangedSubview(restoreButton)
        applyButton = button("Apply", #selector(apply))
        bar.addArrangedSubview(applyButton)
        stack.addArrangedSubview(bar)
        canvas.translatesAutoresizingMaskIntoConstraints = false
        canvas.setContentHuggingPriority(.defaultLow, for: .vertical)
        canvas.onChange = { [weak self] in self?.dirty = true; self?.update() }
        canvas.onSelect = { [weak self] in self?.update() }
        stack.addArrangedSubview(canvas)
        let inspector = NSStackView()
        inspector.spacing = 8
        selection.font = .systemFont(ofSize: 14, weight: .semibold)
        selection.setContentHuggingPriority(.defaultLow, for: .horizontal)
        inspector.addArrangedSubview(selection)
        for key in ["X", "Y", "Width", "Height"] {
            inspector.addArrangedSubview(NSTextField(labelWithString: key))
            let field = NSTextField(string: "")
            field.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
            field.alignment = .right; field.delegate = self
            field.widthAnchor.constraint(equalToConstant: 60).isActive = true
            field.setAccessibilityLabel("Monitor \(key)")
            fields.append(field); inspector.addArrangedSubview(field)
        }
        stack.addArrangedSubview(inspector)
        status.font = .systemFont(ofSize: 12)
        status.textColor = .secondaryLabelColor
        status.lineBreakMode = .byTruncatingTail
        status.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        stack.addArrangedSubview(status)
        for view in [bar, canvas, inspector, status] {
            view.widthAnchor.constraint(equalTo: stack.widthAnchor, constant: -40).isActive = true
        }
    }

    private func setBusy(_ value: Bool, _ text: String) {
        busy = value; canvas.isEditingEnabled = !value
        buttons.forEach { $0.isEnabled = !value }
        fields.forEach { $0.isEnabled = !value }
        status.stringValue = text; status.toolTip = text
        if !value { update() }
    }

    private func command(_ arguments: [String], done: @escaping (Result<[String: Any], Error>) -> Void) {
        DispatchQueue.global(qos: .utility).async {
            let process = Process()
            process.executableURL = URL(fileURLWithPath: "\(Paths.slabBin)/displays")
            process.arguments = arguments
            // A file avoids pipe-buffer deadlocks on inventory / remote error output.
            let url = FileManager.default.temporaryDirectory.appendingPathComponent("slab-layout-\(UUID().uuidString).json")
            FileManager.default.createFile(atPath: url.path, contents: nil)
            do {
                let output = try FileHandle(forWritingTo: url)
                process.standardOutput = output; process.standardError = output
                defer { try? output.close(); try? FileManager.default.removeItem(at: url) }
                try process.run()
                let timeout = DispatchWorkItem { if process.isRunning { process.terminate() } }
                DispatchQueue.global(qos: .utility).asyncAfter(deadline: .now() + 180, execute: timeout)
                process.waitUntilExit(); timeout.cancel()
                let data = try Data(contentsOf: url)
                guard process.terminationStatus == 0 else {
                    throw NSError(domain: "DesktopLayout", code: 1, userInfo: [NSLocalizedDescriptionKey:
                        String(data: data, encoding: .utf8) ?? "Display command failed"])
                }
                let object = try JSONSerialization.jsonObject(with: data)
                let result = object as? [String: Any] ?? ["result": object]
                DispatchQueue.main.async { done(.success(result)) }
            } catch { DispatchQueue.main.async { done(.failure(error)) } }
        }
    }

    private func accept(_ object: [String: Any]) {
        state = object
        original = object["screens"] as? [[String: Any]] ?? []
        canvas.screens = original
        canvas.selected = original.isEmpty ? nil : 0
        canvas.fit(); dirty = false; busy = false
        setBusy(false, "")
    }

    private func load() {
        setBusy(true, "Detecting displays…")
        command(["seat-load"]) { [weak self] result in
            guard let self else { return }
            switch result {
            case .success(let object): self.accept(object)
            case .failure(let error): self.fail(error)
            }
        }
    }

    private func fail(_ error: Error) {
        setBusy(false, "")
        status.stringValue = error.localizedDescription.replacingOccurrences(of: "\n", with: " ")
        status.toolTip = error.localizedDescription
        let alert = NSAlert()
        alert.messageText = "Desktop Layout"
        alert.informativeText = error.localizedDescription
        alert.beginSheetModal(for: window)
    }

    private func update() {
        guard !busy else { return }
        if let i = canvas.selected, canvas.screens.indices.contains(i) {
            let s = canvas.screens[i]
            selection.stringValue = (s["address"] as? String) ?? (s["screenName"] as? String ?? "Monitor")
            for (field, key) in zip(fields, ["x", "y", "width", "height"]) {
                field.stringValue = String((s[key] as? NSNumber)?.intValue ?? 0); field.isEnabled = true
            }
        } else { selection.stringValue = "Select a monitor"; fields.forEach { $0.isEnabled = false; $0.stringValue = "" } }
        let issue = canvas.validationError()
        applyButton.isEnabled = !canvas.screens.isEmpty && issue == nil && (dirty || state["applied"] as? Bool != true)
        restoreButton.isEnabled = state["canRestore"] as? Bool == true
        let warning = state["warning"] as? String ?? ""
        status.stringValue = issue ?? (!warning.isEmpty ? warning : dirty ? "Draft · Apply changes pointer crossings. Shared edges connect automatically."
            : state["applied"] as? Bool == true ? "Applied · Drag a monitor to rearrange your seat."
            : "Draft from your seat map · Drag monitors into place, then Apply.")
        status.toolTip = status.stringValue
        canvas.needsDisplay = true
    }

    func controlTextDidEndEditing(_ notification: Notification) {
        guard let i = canvas.selected else { return }
        let values = fields.map { Int($0.stringValue) }
        guard values.allSatisfy({ $0 != nil && abs($0!) <= 100000 }), values[2]! > 0, values[3]! > 0 else { update(); return }
        for (key, value) in zip(["x", "y", "width", "height"], values) { canvas.screens[i][key] = value! }
        dirty = true; canvas.fit(); update()
    }
    @objc private func refresh() { load() }
    @objc private func reset() { canvas.screens = original; canvas.fit(); dirty = false; update() }
    @objc private func identify() {
        let names = Set(canvas.screens.compactMap { $0["machine"] as? String }).sorted()
        guard !names.isEmpty else { return }
        setBusy(true, "Showing monitor addresses for 8 seconds…")
        command(["seat-identify"]) { [weak self] result in
            guard let self else { return }
            switch result { case .success: self.setBusy(false, ""); case .failure(let error): self.fail(error) }
        }
    }
    @objc private func apply() {
        window.makeFirstResponder(canvas)
        guard canvas.validationError() == nil else { return }
        state["screens"] = canvas.screens
        do {
            let url = FileManager.default.temporaryDirectory.appendingPathComponent("slab-seat-\(UUID().uuidString).json")
            try JSONSerialization.data(withJSONObject: state).write(to: url, options: .atomic)
            setBusy(true, "Applying pointer routes…")
            command(["seat-apply", url.path]) { [weak self] result in
                try? FileManager.default.removeItem(at: url)
                guard let self else { return }
                switch result {
                case .success(let object): self.accept(object["seat"] as? [String: Any] ?? [:])
                case .failure(let error): self.fail(error)
                }
            }
        } catch { fail(error) }
    }
    @objc private func restore() {
        setBusy(true, "Restoring previous pointer routes…")
        command(["seat-restore"]) { [weak self] result in
            guard let self else { return }
            switch result {
            case .success(let object): self.accept(object["seat"] as? [String: Any] ?? [:])
            case .failure(let error): self.fail(error)
            }
        }
    }
    func windowShouldClose(_ sender: NSWindow) -> Bool { !busy }
    func windowWillClose(_ notification: Notification) { Self.shared = nil }
}

final class DisplaySeatCanvas: NSView {
    var screens: [[String: Any]] = []
    var selected: Int?
    var onChange: (() -> Void)?
    var onSelect: (() -> Void)?
    var isEditingEnabled = true
    private var scale: CGFloat = 0.5
    private var offset = CGPoint.zero
    private var down = CGPoint.zero
    private var origin = CGPoint.zero
    override var isFlipped: Bool { true }
    override var acceptsFirstResponder: Bool { true }
    private func rect(_ s: [String: Any]) -> CGRect {
        func n(_ key: String) -> CGFloat { CGFloat((s[key] as? NSNumber)?.doubleValue ?? 0) }
        return CGRect(x: n("x"), y: n("y"), width: n("width"), height: n("height"))
    }
    private func rendered(_ s: [String: Any]) -> CGRect {
        let r = rect(s)
        return CGRect(x: offset.x + r.minX * scale, y: offset.y + r.minY * scale, width: r.width * scale, height: r.height * scale)
    }
    func fit() {
        guard !screens.isEmpty, bounds.width > 0, bounds.height > 0 else { return }
        let all = screens.map(rect).reduce(CGRect.null) { $0.union($1) }
        scale = max(0.01, min((bounds.width - 80) / all.width, (bounds.height - 80) / all.height))
        offset = CGPoint(x: (bounds.width - all.width * scale) / 2 - all.minX * scale,
                         y: (bounds.height - all.height * scale) / 2 - all.minY * scale)
        needsDisplay = true
    }
    override func setFrameSize(_ newSize: NSSize) { super.setFrameSize(newSize); fit() }
    func validationError() -> String? {
        guard !screens.isEmpty else { return "No displays detected" }
        let rects = screens.map(rect)
        for i in rects.indices {
            for j in rects.indices where j > i {
                let overlap = rects[i].intersection(rects[j])
                if !overlap.isNull && overlap.width > 0 && overlap.height > 0 { return "Monitors overlap · Drag them apart before applying." }
            }
        }
        var reached: Set<Int> = [0]
        func touches(_ a: CGRect, _ b: CGRect) -> Bool {
            ((a.maxX == b.minX || b.maxX == a.minX) && min(a.maxY, b.maxY) > max(a.minY, b.minY)) ||
            ((a.maxY == b.minY || b.maxY == a.minY) && min(a.maxX, b.maxX) > max(a.minX, b.minX))
        }
        while true {
            let before = reached
            for i in rects.indices where reached.contains(where: { touches(rects[i], rects[$0]) }) { reached.insert(i) }
            if before == reached { break }
        }
        return reached.count == rects.count ? nil : "Close the gaps · Snap each monitor to a shared edge."
    }
    override func draw(_ dirtyRect: NSRect) {
        NSColor(calibratedWhite: 0.09, alpha: 1).setFill()
        NSBezierPath(roundedRect: bounds, xRadius: 12, yRadius: 12).fill()
        for (i, s) in screens.enumerated() {
            let r = rendered(s).insetBy(dx: 3, dy: 3)
            let chosen = selected == i
            let online = s["online"] as? Bool ?? false
            let shape = NSBezierPath(roundedRect: r, xRadius: 10, yRadius: 10)
            (chosen ? NSColor(calibratedRed: 0.10, green: 0.32, blue: 0.34, alpha: 1)
                : NSColor(calibratedWhite: online ? 0.20 : 0.14, alpha: 1)).setFill(); shape.fill()
            (chosen ? NSColor.systemTeal : NSColor(calibratedWhite: 0.4, alpha: 1)).setStroke()
            shape.lineWidth = chosen ? 3 : 1; shape.stroke()
            let name = (s["address"] as? String) ?? (s["screenName"] as? String ?? "Display")
            let number = (s["number"] as? NSNumber)?.stringValue ?? ""
            label(number, at: NSRect(x: r.minX + 12, y: r.midY - 51, width: r.width - 24, height: 49), size: min(42, r.height * 0.30), weight: .bold)
            label(name, at: NSRect(x: r.minX + 8, y: r.midY + 1, width: r.width - 16, height: 23), size: min(17, r.width / 11), weight: .semibold)
            label(s["pixels"] as? String ?? "", at: NSRect(x: r.minX + 8, y: r.midY + 28, width: r.width - 16, height: 18), size: 11, weight: .regular, color: .lightGray)
        }
        // Actual overlap segments illuminate the boundary where the pointer can cross.
        NSColor.systemTeal.withAlphaComponent(0.85).setStroke()
        for i in screens.indices { for j in screens.indices where j > i {
            let a = rect(screens[i]), b = rect(screens[j]); var p: CGPoint?, q: CGPoint?
            if a.maxX == b.minX || b.maxX == a.minX {
                let lo = max(a.minY,b.minY), hi = min(a.maxY,b.maxY)
                if hi > lo { let x = a.maxX == b.minX ? a.maxX : b.maxX; p = CGPoint(x:x,y:lo); q = CGPoint(x:x,y:hi) }
            } else if a.maxY == b.minY || b.maxY == a.minY {
                let lo = max(a.minX,b.minX), hi = min(a.maxX,b.maxX)
                if hi > lo { let y = a.maxY == b.minY ? a.maxY : b.maxY; p = CGPoint(x:lo,y:y); q = CGPoint(x:hi,y:y) }
            }
            if let p, let q { let line = NSBezierPath(); line.move(to: CGPoint(x:offset.x+p.x*scale,y:offset.y+p.y*scale)); line.line(to: CGPoint(x:offset.x+q.x*scale,y:offset.y+q.y*scale)); line.lineWidth = 2; line.stroke() }
        } }
    }
    private func label(_ text: String, at rect: CGRect, size: CGFloat, weight: NSFont.Weight, color: NSColor = .white) {
        let style = NSMutableParagraphStyle(); style.alignment = .center; style.lineBreakMode = .byTruncatingTail
        (text as NSString).draw(in: rect, withAttributes: [.font: NSFont.systemFont(ofSize: size, weight: weight), .foregroundColor: color, .paragraphStyle: style])
    }
    override func mouseDown(with event: NSEvent) {
        guard isEditingEnabled else { return }
        window?.makeFirstResponder(self)
        down = convert(event.locationInWindow, from: nil)
        selected = screens.indices.reversed().first { rendered(screens[$0]).contains(down) }
        if let selected { origin = rect(screens[selected]).origin }
        onSelect?(); needsDisplay = true
    }
    override func mouseDragged(with event: NSEvent) {
        guard isEditingEnabled, let i = selected else { return }
        let point = convert(event.locationInWindow, from: nil)
        var r = rect(screens[i]); r.origin = CGPoint(x: round(origin.x + (point.x - down.x) / scale), y: round(origin.y + (point.y - down.y) / scale))
        let threshold = 12 / scale
        var dx = threshold, dy = threshold
        for j in screens.indices where j != i {
            let other = rect(screens[j])
            for x in [other.minX, other.maxX, other.minX-r.width, other.maxX-r.width] {
                if abs(x-r.minX) < abs(dx) { dx = x-r.minX }
            }
            for y in [other.minY, other.maxY, other.minY-r.height, other.maxY-r.height] {
                if abs(y-r.minY) < abs(dy) { dy = y-r.minY }
            }
        }
        if abs(dx) < threshold { r.origin.x += dx }; if abs(dy) < threshold { r.origin.y += dy }
        screens[i]["x"] = Int(r.minX); screens[i]["y"] = Int(r.minY)
        onChange?(); needsDisplay = true
    }
    override func mouseUp(with event: NSEvent) { fit() }
    override func keyDown(with event: NSEvent) {
        guard isEditingEnabled, let i = selected else { return }
        let delta = event.modifierFlags.contains(.shift) ? 10 : 1
        let key: String, change: Int
        switch event.keyCode { case 123: key="x"; change = -delta; case 124: key="x"; change=delta
        case 125: key="y"; change=delta; case 126: key="y"; change = -delta
        default: super.keyDown(with: event); return }
        screens[i][key] = ((screens[i][key] as? NSNumber)?.intValue ?? 0) + change
        onChange?(); needsDisplay = true
    }
}
