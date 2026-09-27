import AppKit
import BrushCore
import UniformTypeIdentifiers

@MainActor final class PaintingView: NSView {
    var image: CGImage? { didSet { needsDisplay = true } }
    var contextMenu: (() -> NSMenu?)?
    override func menu(for event: NSEvent) -> NSMenu? { contextMenu?() }
    var onTrack: ((NSEvent) -> Void)?
    var onHover: (() -> Void)?
    var hovered = false { didSet { needsDisplay = true } }
    var pressed = false { didSet { needsDisplay = true } }
    var hoverArea: NSTrackingArea?
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let hoverArea { removeTrackingArea(hoverArea) }
        let area = NSTrackingArea(rect: .zero, options: [.mouseEnteredAndExited, .activeInKeyWindow, .inVisibleRect], owner: self)
        addTrackingArea(area); hoverArea = area
    }
    override func mouseEntered(with event: NSEvent) { hovered = true; onHover?() }
    override func mouseExited(with event: NSEvent) { hovered = false }
    override func mouseDown(with event: NSEvent) { onTrack?(event) }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
    override func draw(_ dirtyRect: NSRect) {
        NSColor(calibratedRed: 24/255, green: 20/255, blue: 40/255, alpha: 1).setFill()
        bounds.fill()
        guard let image, let context = NSGraphicsContext.current?.cgContext else { return }
        let fit = min(bounds.width / CGFloat(image.width), bounds.height / CGFloat(image.height))
        let scale = fit
        let size = CGSize(width: CGFloat(image.width) * scale, height: CGFloat(image.height) * scale)
        context.interpolationQuality = .none
        context.draw(image, in: CGRect(x: (bounds.width-size.width)/2, y: (bounds.height-size.height)/2, width: size.width, height: size.height))
        if hovered || pressed {
            NSColor.white.withAlphaComponent(pressed ? 0.3 : 0.14).setStroke()
            let outline = NSBezierPath(rect: bounds.insetBy(dx: 1, dy: 1)); outline.lineWidth = pressed ? 4 : 2; outline.stroke()
            if pressed { NSColor.black.withAlphaComponent(0.08).setFill(); bounds.fill() }
        }
    }
}

@MainActor final class DecisionButton: NSButton {
    var onTrack: ((NSEvent) -> Void)?
    var onHover: (() -> Void)?
    var hovered = false { didSet { needsDisplay = true } }
    var hoverArea: NSTrackingArea?
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let hoverArea { removeTrackingArea(hoverArea) }
        let area = NSTrackingArea(rect: .zero, options: [.mouseEnteredAndExited, .activeInKeyWindow, .inVisibleRect], owner: self)
        addTrackingArea(area); hoverArea = area
    }
    override func mouseEntered(with event: NSEvent) { hovered = true; onHover?() }
    override func mouseExited(with event: NSEvent) { hovered = false }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
    override func draw(_ dirtyRect: NSRect) {
        let color = bezelColor ?? .systemGray
        (isHighlighted ? color.blended(withFraction: 0.22, of: .black)! : hovered ? color.blended(withFraction: 0.15, of: .white)! : color).setFill()
        bounds.fill()
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 22, weight: .semibold), .foregroundColor: NSColor.white,
        ]
        let size = (title as NSString).size(withAttributes: attrs)
        (title as NSString).draw(at: NSPoint(x: (bounds.width-size.width)/2, y: (bounds.height-size.height)/2), withAttributes: attrs)
    }
    override func mouseDown(with event: NSEvent) {
        guard isEnabled else { return }
        if let onTrack { onTrack(event) } else { super.mouseDown(with: event) }
    }
}

@MainActor final class BrushApp: NSObject, NSApplicationDelegate {
    var window: NSWindow!
    var sourceWindow: NSWindow?
    var aboutWindow: NSWindow?
    let source = NSTextView()
    let canvas = PaintingView()
    let picker = NSPopUpButton()
    let time = NSSlider(value: 30, minValue: 0, maxValue: 60, target: nil, action: nil)
    let status = NSTextField(labelWithString: "")
    var acceptButton: NSButton!
    var rejectButton: NSButton!
    var undoButton: NSButton!
    var redoButton: NSButton!
    var finishMode = false
    enum HeldChoice { case no, paint }
    var heldChoice: HeldChoice?
    var holdingDecision = false
    var animationTimer: Timer?
    var nextSeed = 42
    var sounds: [String: NSSound] = [:]
    let account = ACAccount()
    var accountMenu: NSMenu!
    var loginTask: Task<Void, Never>?
    var brush: Brush?
    var gestureMode = -1
    var activeGestureMode = 0
    var preview: Invocation?
    var painting = Painting(width: 256, height: 256,
        base: Array(repeating: [24, 20, 40, 255], count: 256 * 256).flatMap { $0 })

    func applicationDidFinishLaunching(_ notification: Notification) {
        if let folder = Bundle.main.resourceURL?.appendingPathComponent("Sounds"),
            let files = try? FileManager.default.contentsOfDirectory(at: folder, includingPropertiesForKeys: nil) {
            for file in files where file.pathExtension == "wav" {
                if let sound = NSSound(contentsOf: file, byReference: false) {
                    sound.volume = 0.72; sounds[file.deletingPathExtension().lastPathComponent] = sound
                }
            }
        }
        let menu = NSMenu()
        let item = NSMenuItem(); menu.addItem(item)
        let appMenu = NSMenu(); item.submenu = appMenu
        appMenu.addItem(withTitle: "Quit No Paint", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        let editItem = NSMenuItem(); menu.addItem(editItem)
        let edit = NSMenu(title: "Edit"); editItem.submenu = edit
        for (title, selector, key) in [("Cut", "cut:", "x"), ("Copy", "copy:", "c"), ("Paste", "paste:", "v"), ("Select All", "selectAll:", "a")] {
            edit.addItem(withTitle: title, action: Selector(selector), keyEquivalent: key)
        }
        let fileItem = NSMenuItem(); menu.addItem(fileItem)
        let fileMenu = NSMenu(title: "File"); fileItem.submenu = fileMenu
        for (title, action, key) in [("Open Painting…", #selector(openPainting), "o"), ("Save Painting…", #selector(savePainting), "s")] {
            let entry = fileMenu.addItem(withTitle: title, action: action, keyEquivalent: key); entry.target = self
        }
        for (title, action, key) in [("Undo Painting", #selector(undo), "z"), ("Redo Painting", #selector(redo), "Z")] {
            let entry = edit.addItem(withTitle: title, action: action, keyEquivalent: key); entry.target = self
        }
        let brushItem = NSMenuItem(); menu.addItem(brushItem)
        let brushMenu = NSMenu(title: "Brush"); brushItem.submenu = brushMenu
        let inspect = brushMenu.addItem(withTitle: "Brush Source…", action: #selector(editSource), keyEquivalent: "b"); inspect.target = self
        let about = brushMenu.addItem(withTitle: "About Brush…", action: #selector(aboutBrush), keyEquivalent: "i"); about.target = self
        for (index, title) in ["Line", "Invert"].enumerated() {
            let entry = brushMenu.addItem(withTitle: title, action: #selector(chooseFromMenu(_:)), keyEquivalent: String(index+1))
            entry.target = self; entry.tag = index
        }
        let thicknessItem = brushMenu.addItem(withTitle: "Thickness", action: nil, keyEquivalent: "")
        let thicknessMenu = NSMenu(title: "Thickness"); thicknessItem.submenu = thicknessMenu
        for radius in [0, 1, 3, 6, 12] {
            let entry = thicknessMenu.addItem(withTitle: "\(radius * 2 + 1) px", action: #selector(setThickness(_:)), keyEquivalent: "")
            entry.target = self; entry.tag = radius
        }
        let opacityItem = brushMenu.addItem(withTitle: "Opacity", action: nil, keyEquivalent: "")
        let opacityMenu = NSMenu(title: "Opacity"); opacityItem.submenu = opacityMenu
        for percent in [25, 50, 75, 100] {
            let entry = opacityMenu.addItem(withTitle: "\(percent)%", action: #selector(setOpacity(_:)), keyEquivalent: "")
            entry.target = self; entry.tag = percent * 255 / 100
        }
        let gestureItem = NSMenuItem(); menu.addItem(gestureItem)
        let gestureMenu = NSMenu(title: "Gesture"); gestureItem.submenu = gestureMenu
        for (index, title) in ["Automatic", "Wander", "Sweep", "Loop", "Zigzag"].enumerated() {
            let entry = gestureMenu.addItem(withTitle: title, action: #selector(chooseGesture(_:)), keyEquivalent: "")
            entry.target = self; entry.tag = index - 1
        }
        let accountItem = NSMenuItem(); menu.addItem(accountItem)
        accountMenu = NSMenu(title: "Account"); accountMenu.autoenablesItems = false; accountItem.submenu = accountMenu
        updateAccountMenu()
        NSApp.mainMenu = menu
        window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 320, height: 400),
            styleMask: [.titled, .closable, .miniaturizable, .resizable], backing: .buffered, defer: false)
        window.title = "No Paint"
        window.contentMinSize = NSSize(width: 280, height: 350)
        window.contentAspectRatio = NSSize(width: 4, height: 5)
        window.isReleasedWhenClosed = false
        window.backgroundColor = .windowBackgroundColor
        picker.addItems(withTitles: ["Line", "Invert"])
        picker.target = self; picker.action = #selector(selectBrush)
        time.target = self; time.action = #selector(changeTime); time.isContinuous = true
        time.widthAnchor.constraint(greaterThanOrEqualToConstant: 180).isActive = true
        let paint = DecisionButton(title: "Paint", target: self, action: #selector(rightDecision))
        paint.bezelColor = .systemGreen
        let no = DecisionButton(title: "No", target: self, action: #selector(leftDecision))
        no.bezelColor = .systemRed
        for button in [no, paint] {
            button.isBordered = false; button.bezelStyle = .regularSquare; button.controlSize = .large
            button.setContentHuggingPriority(.defaultLow, for: .vertical)
            button.font = .systemFont(ofSize: 19, weight: .semibold)
            button.setButtonType(.momentaryPushIn); button.translatesAutoresizingMaskIntoConstraints = false
            button.onTrack = { [weak self] event in self?.trackDecision(event) }
            button.onHover = { [weak self] in if self?.holdingDecision == false { self?.playCue("rollover") } }
        }
        acceptButton = paint; rejectButton = no
        undoButton = button("Undo", #selector(undo)); redoButton = button("Redo", #selector(redo))
        canvas.contextMenu = { [weak self] in
            guard let self, self.finishMode else { return nil }
            let menu = NSMenu()
            let item = menu.addItem(withTitle: "About Brush…", action: #selector(self.aboutBrush), keyEquivalent: "")
            item.target = self; return menu
        }
        canvas.onTrack = { [weak self] event in self?.trackPainting(event) }
        canvas.onHover = { [weak self] in if self?.holdingDecision == false { self?.playCue("rollover") } }
        canvas.setAccessibilityRole(.button); canvas.setAccessibilityLabel("Painting — finish or return")
        canvas.translatesAutoresizingMaskIntoConstraints = false
        let content = window.contentView!
        let controls = NSView(); controls.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(canvas); content.addSubview(controls)
        controls.addSubview(no); controls.addSubview(paint)
        NSLayoutConstraint.activate([
            canvas.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            canvas.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            canvas.topAnchor.constraint(equalTo: content.topAnchor),
            canvas.heightAnchor.constraint(equalTo: canvas.widthAnchor),
            canvas.bottomAnchor.constraint(equalTo: controls.topAnchor),
            controls.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            controls.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            controls.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            controls.heightAnchor.constraint(equalTo: content.heightAnchor, multiplier: 0.2),
            no.leadingAnchor.constraint(equalTo: controls.leadingAnchor),
            no.centerYAnchor.constraint(equalTo: controls.centerYAnchor),
            no.heightAnchor.constraint(equalTo: controls.heightAnchor),
            no.widthAnchor.constraint(equalTo: controls.widthAnchor, multiplier: 0.5, constant: -1),
            paint.leadingAnchor.constraint(equalTo: no.trailingAnchor, constant: 2),
            paint.trailingAnchor.constraint(equalTo: controls.trailingAnchor),
            paint.centerYAnchor.constraint(equalTo: controls.centerYAnchor),
            paint.heightAnchor.constraint(equalTo: no.heightAnchor),
        ])
        selectBrush()
        animationTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            MainActor.assumeIsolated { self?.advanceFrame() }
        }
        window.center(); window.makeKeyAndOrderFront(nil); NSApp.activate(ignoringOtherApps: true)
    }
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
    func button(_ title: String, _ action: Selector) -> NSButton {
        NSButton(title: title, target: self, action: action)
    }
    func row(_ views: [NSView]) -> NSStackView {
        let stack = NSStackView(views: views); stack.orientation = .horizontal; stack.spacing = 10
        return stack
    }
    func attempt(_ work: () throws -> Void) {
        do { try work(); status.stringValue = "" }
        catch {
            status.stringValue = String(describing: error)
            let alert = NSAlert(); alert.messageText = "Could not update painting"
            alert.informativeText = String(describing: error); alert.beginSheetModal(for: sourceWindow?.isVisible == true ? sourceWindow! : window)
        }
    }
    func encoded<T: Encodable>(_ value: T) throws -> Data {
        let encoder = JSONEncoder(); encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        return try encoder.encode(value)
    }
    func refresh() throws {
        let pixels = try renderDocument(painting, preview: (finishMode || (holdingDecision && heldChoice == .no)) ? nil : preview)
        let provider = CGDataProvider(data: Data(pixels) as CFData)!
        canvas.image = CGImage(width: painting.width, height: painting.height,
            bitsPerComponent: 8, bitsPerPixel: 32, bytesPerRow: painting.width * 4,
            space: CGColorSpace(name: CGColorSpace.sRGB)!,
            bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.last.rawValue),
            provider: provider, decode: nil, shouldInterpolate: false, intent: .defaultIntent)
        acceptButton.title = finishMode ? "Done" : "Paint"
        rejectButton.title = finishMode ? "Back" : "No"
        rejectButton.bezelColor = finishMode ? .systemOrange : .systemRed
        acceptButton.needsDisplay = true; rejectButton.needsDisplay = true
        acceptButton.isEnabled = finishMode || preview != nil; rejectButton.isEnabled = finishMode || preview != nil
        undoButton.isEnabled = painting.cursor > 0; redoButton.isEnabled = painting.cursor < painting.steps.count
    }
    @objc func setThickness(_ sender: NSMenuItem) {
        guard var next = brush, next.operations.contains(where: { $0.op == "stroke" }) else { return }
        for index in next.operations.indices where next.operations[index].op == "stroke" { next.operations[index].radius = sender.tag }
        attempt { try propose(next, preservingGesture: true) }
    }
    @objc func setOpacity(_ sender: NSMenuItem) {
        guard var next = brush, next.operations.contains(where: { $0.op == "stroke" }) else { return }
        for index in next.operations.indices where next.operations[index].op == "stroke" { next.operations[index].color?[3] = sender.tag }
        attempt { try propose(next, preservingGesture: true) }
    }
    @objc func chooseGesture(_ sender: NSMenuItem) {
        gestureMode = sender.tag
        for item in sender.menu?.items ?? [] { item.state = item === sender ? .on : .off }
        if let brush { attempt { try propose(brush) } }
    }
    func propose(_ next: Brush, preservingGesture: Bool = false) throws {
        let mode = gestureMode < 0 ? Int((UInt32(nextSeed) &* 1664525 &+ 1013904223) >> 16) % 4 : gestureMode
        let gesture = preservingGesture ? preview?.gesture : nil
        let invocation = Invocation(brush: next, seed: nextSeed, tick: 0,
            gesture: gesture ?? makeGesture(seed: nextSeed, width: painting.width, height: painting.height, mode: mode))
        _ = try renderDocument(painting, preview: invocation)
        if !preservingGesture { activeGestureMode = mode }
        time.maxValue = Double(max(next.operations.compactMap(\.durationTicks).max() ?? 1,
            next.operations.contains { $0.op == "stroke" } ? invocation.gesture?.last?.tick ?? 1 : 1))
        brush = next; preview = invocation; finishMode = false; time.integerValue = 0; nextSeed = (nextSeed + 1) & 0xffff_ffff; try refresh()
        window.title = "No Paint — \(next.id.capitalized)"
        if let encoded = try? encoded(next) { source.string = String(decoding: encoded, as: UTF8.self) }
    }
    func updateAccountMenu() {
        accountMenu.removeAllItems()
        if loginTask != nil {
            let status = accountMenu.addItem(withTitle: "Waiting for browser sign-in…", action: nil, keyEquivalent: ""); status.isEnabled = false
            let cancel = accountMenu.addItem(withTitle: "Cancel Sign In", action: #selector(cancelLogin), keyEquivalent: ""); cancel.target = self
        } else if let session = account.session {
            let label = accountMenu.addItem(withTitle: session.account.label, action: nil, keyEquivalent: ""); label.isEnabled = false
            let out = accountMenu.addItem(withTitle: "Sign Out", action: #selector(signOut), keyEquivalent: ""); out.target = self
        } else {
            let login = accountMenu.addItem(withTitle: "Sign In to AC…", action: #selector(signIn), keyEquivalent: ""); login.target = self
        }
    }
    @objc func signIn() {
        guard loginTask == nil else { return }
        loginTask = Task { @MainActor [weak self] in
            guard let self else { return }
            defer { self.loginTask = nil; self.updateAccountMenu() }
            do {
                let (pair, url) = try await self.account.begin()
                try Task.checkCancellation()
                guard NSWorkspace.shared.open(url) else { throw ACLoginError.message("Could not open the browser.") }
                try await self.account.complete(pair)
            } catch is CancellationError {} catch {
                if !Task.isCancelled { self.attempt { throw error } }
            }
        }
        updateAccountMenu()
    }
    @objc func cancelLogin() { loginTask?.cancel() }
    @objc func signOut() { attempt { try account.signOut(); updateAccountMenu() } }
    func nextBrush() throws {
        let names: [String] = ["line", "invert"].filter { $0 != (brush?.id ?? "") }
        var state = UInt32(nextSeed)
        func random(_ n: Int) -> Int { state = state &* 1664525 &+ 1013904223; return Int(state >> 8) % n }
        let name = names[random(names.count)]
        guard let url = Bundle.main.url(forResource: name, withExtension: "brush.json", subdirectory: "Brushes") else { throw BrushError.invalid("Missing brush") }
        var next = try JSONDecoder().decode(Brush.self, from: Data(contentsOf: url))
        for index in next.operations.indices {
            if next.operations[index].op == "invert" {
                next.operations[index].amount = 255
                next.operations[index].durationTicks = nil
            } else {
                next.operations[index].color = [64 + random(192), 64 + random(192), 64 + random(192), 80 + random(140)]
                next.operations[index].radius = random(7)
                if next.operations[index].op == "stroke" { continue }
                next.operations[index].durationTicks = 240 + random(660)
                next.operations[index].jitter = random(7)
                if next.operations[index].op == "walk" { next.operations[index].steps = 64 + random(65) }
                else {
                    next.operations[index].points = (0..<(4+random(8))).map { _ in Point(x: random(painting.width), y: random(painting.height)) }
                    next.operations[index].drift = Point(x: random(5)-2, y: random(5)-2)
                }
            }
        }
        picker.selectItem(withTitle: name.capitalized)
        try propose(next)
    }
    @objc func aboutBrush() {
        guard let brush else { return }
        if !finishMode { finishMode = true; playCue("pause-in"); attempt { try refresh() } }
        let descriptions = [
            "stroke": "Line draws along a separate gesture stream. Radius controls thickness; RGBA controls color and opacity. The same gesture can be replayed through other brushes.",
            "walk": "A seeded moving gesture. Its velocity bends over time and turns back at the canvas edges.",
            "path": "A path grows between its points, with seeded jitter and drift across the painting.",
            "invert": "Flips every RGB channel to its opposite immediately, preserving transparency. Paint keeps the inversion; No discards it.",
        ]
        let explanation = brush.operations.map { descriptions[$0.op] ?? $0.op }.joined(separator: "\n\n")
        let program = (try? encoded(brush)).map { String(decoding: $0, as: UTF8.self) } ?? ""
        let view = NSTextView(); view.isEditable = false; view.isRichText = false
        view.font = .monospacedSystemFont(ofSize: 13, weight: .regular)
        view.textContainerInset = NSSize(width: 20, height: 20)
        view.string = "\(brush.id.capitalized)\n\n\(explanation)\n\nSeed: \(preview?.seed ?? 0)\nFrame: \(preview?.tick ?? 0) at 60 ticks/second\n\nNo discards this proposal. Paint accepts its current frame. Both choose a different brush and fresh parameters.\nGesture: \(["Wander", "Sweep", "Loop", "Zigzag"][activeGestureMode]) (\(preview?.gesture?.count ?? 0) recorded samples).\n\nCurrent brush source\n\n\(program)"
        let scroll = NSScrollView(); scroll.hasVerticalScroller = true; scroll.documentView = view
        view.frame = NSRect(x: 0, y: 0, width: 520, height: 580); view.autoresizingMask = [.width]
        view.isVerticallyResizable = true; view.textContainer?.widthTracksTextView = true
        let panel = aboutWindow ?? NSWindow(contentRect: NSRect(x: 0, y: 0, width: 520, height: 580), styleMask: [.titled, .closable, .resizable], backing: .buffered, defer: false)
        panel.isReleasedWhenClosed = false; panel.title = "About \(brush.id.capitalized)"; panel.contentView = scroll
        if aboutWindow == nil { panel.center() }; aboutWindow = panel; panel.makeKeyAndOrderFront(nil)
    }
    func playCue(_ name: String) {
        guard let sound = sounds[name] else { return }
        sound.stop(); sound.play()
    }
    func trace(_ action: String) {
        guard let path = ProcessInfo.processInfo.environment["BRUSH_UI_TRACE"] else { return }
        let state: [String: Any] = ["action": action, "tick": preview?.tick ?? -1,
            "seed": preview?.seed ?? -1, "accepted": painting.cursor,
            "held": holdingDecision, "choice": heldChoice.map { $0 == .no ? "no" : "paint" } ?? "none",
            "sounds": sounds.count]
        guard let data = try? JSONSerialization.data(withJSONObject: state),
            let handle = FileHandle(forWritingAtPath: path) else { return }
        defer { try? handle.close() }
        _ = try? handle.seekToEnd(); try? handle.write(contentsOf: data + Data([10]))
    }
    func trackPainting(_ initial: NSEvent) {
        holdingDecision = true; canvas.pressed = true; playCue("pause-down")
        canvas.displayIfNeeded()
        var releasedInside = false
        while let event = NSApp.nextEvent(matching: [.leftMouseDragged, .leftMouseUp], until: .distantFuture, inMode: .eventTracking, dequeue: true) {
            let inside = canvas.bounds.contains(canvas.convert(event.locationInWindow, from: nil))
            canvas.pressed = inside; canvas.displayIfNeeded()
            if event.type == .leftMouseUp { releasedInside = inside; break }
        }
        holdingDecision = false; canvas.pressed = false
        if releasedInside { toggleFinish() } else { playCue("pause-out") }
    }
    func trackDecision(_ initial: NSEvent) {
        holdingDecision = true
        func update(_ event: NSEvent) {
            let previous = heldChoice
            let location = event.locationInWindow
            if rejectButton.bounds.contains(rejectButton.convert(location, from: nil)) { heldChoice = .no }
            else if acceptButton.bounds.contains(acceptButton.convert(location, from: nil)) { heldChoice = .paint }
            else { heldChoice = nil }
            canvas.hovered = canvas.bounds.contains(canvas.convert(location, from: nil))
            (rejectButton as? DecisionButton)?.hovered = heldChoice == .no
            (acceptButton as? DecisionButton)?.hovered = heldChoice == .paint
            if heldChoice != previous, let choice = heldChoice {
                playCue(finishMode ? (choice == .no ? "button-down" : "done-down") : (choice == .no ? "no-down" : "paint-down"))
            }
            rejectButton.highlight(heldChoice == .no); acceptButton.highlight(heldChoice == .paint)
            trace("held")
            attempt { try refresh() }
            window.displayIfNeeded()
        }
        update(initial)
        var released = false
        while let event = NSApp.nextEvent(matching: [.leftMouseDragged, .leftMouseUp],
            until: .distantFuture, inMode: .eventTracking, dequeue: true) {
            update(event)
            if event.type == .leftMouseUp { released = true; break }
        }
        trace("before-release")
        let choice = heldChoice
        holdingDecision = false; heldChoice = nil
        rejectButton.highlight(false); acceptButton.highlight(false)
        if released, let choice {
            if choice == .no { leftDecision() } else { rightDecision() }
        } else { attempt { try refresh() } }
        trace("after-release")
    }
    @objc func chooseFromMenu(_ sender: NSMenuItem) { picker.selectItem(at: sender.tag); selectBrush() }
    @objc func toggleFinish() { attempt { finishMode.toggle(); playCue(finishMode ? "pause-in" : "pause-out"); try refresh() } }
    @objc func leftDecision() {
        if finishMode { toggleFinish() }
        else if preview != nil { reject() }
    }
    @objc func rightDecision() {
        if finishMode { playCue("done"); savePainting() }
        else if preview != nil { accept() }
    }
    @objc func selectBrush() {
        attempt {
            let name = picker.titleOfSelectedItem!.lowercased()
            guard let url = Bundle.main.url(forResource: name, withExtension: "brush.json", subdirectory: "Brushes") else {
                throw BrushError.invalid("Missing brush resource")
            }
            let next = try JSONDecoder().decode(Brush.self, from: Data(contentsOf: url))
            try propose(next); source.string = String(decoding: try encoded(next), as: UTF8.self)
        }
    }
    func advanceFrame() {
        guard !finishMode, !holdingDecision, var current = preview else { return }
        let moving = current.brush.operations.contains { ($0.drift?.x ?? 0) != 0 || ($0.drift?.y ?? 0) != 0 }
        let duration = moving ? 3600 : max(current.brush.operations.compactMap(\.durationTicks).max() ?? 1, current.brush.operations.contains { $0.op == "stroke" } ? current.gesture?.last?.tick ?? 1 : 1)
        guard current.tick < duration else { return }
        current.tick += 1; preview = current; time.integerValue = min(Int(time.maxValue), current.tick)
        do { try refresh() } catch { animationTimer?.invalidate(); attempt { throw error } }
    }
    @objc func changeTime() {
        attempt { if var current = preview { current.tick = time.integerValue; preview = current; try refresh() } }
    }
    @objc func reject() { attempt { playCue("no"); try nextBrush() } }
    @objc func accept() {
        attempt {
            guard let preview else { return }
            playCue("paint")
            var next = painting
            next.steps = Array(next.steps.prefix(next.cursor)) + [preview]; next.cursor += 1
            _ = try renderDocument(next)
            painting = next; self.preview = nil; try nextBrush()
        }
    }
    @objc func undo() { attempt { if painting.cursor > 0 { painting.cursor -= 1 }; preview = nil; try refresh() } }
    @objc func redo() { attempt { if painting.cursor < painting.steps.count { painting.cursor += 1 }; preview = nil; try refresh() } }
    func save(_ data: Data, name: String) throws {
        let panel = NSSavePanel(); panel.allowedContentTypes = [.json]; panel.nameFieldStringValue = name
        if panel.runModal() == .OK, let url = panel.url { try data.write(to: url, options: .atomic) }
    }
    @objc func savePainting() { attempt { try save(encoded(painting), name: "painting.json") } }
    @objc func openPainting() {
        attempt {
            let panel = NSOpenPanel(); panel.allowedContentTypes = [.json]; panel.allowsMultipleSelection = false
            guard panel.runModal() == .OK, let url = panel.url else { return }
            let handle = try FileHandle(forReadingFrom: url); defer { try? handle.close() }
            let data = try handle.read(upToCount: 8_000_001) ?? Data()
            guard data.count <= 8_000_000 else { throw BrushError.invalid("Painting too large") }
            let next = try JSONDecoder().decode(Painting.self, from: data)
            _ = try renderDocument(next); painting = next; preview = nil; try refresh()
        }
    }
    @objc func applySource() {
        attempt {
            let next = try JSONDecoder().decode(Brush.self, from: Data(source.string.utf8))
            try propose(next, preservingGesture: true)
        }
    }
    @objc func saveBrush() { attempt { if let brush { try save(encoded(brush), name: "brush.json") } } }
    @objc func editSource() {
        if let sourceWindow { sourceWindow.makeKeyAndOrderFront(nil); return }
        let editor = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 600, height: 650),
            styleMask: [.titled, .closable, .resizable], backing: .buffered, defer: false)
        editor.title = "Brush source"; editor.isReleasedWhenClosed = false
        source.isRichText = false; source.font = .monospacedSystemFont(ofSize: 13, weight: .regular)
        source.isAutomaticQuoteSubstitutionEnabled = false; source.isAutomaticDashSubstitutionEnabled = false
        source.isVerticallyResizable = true; source.autoresizingMask = [.width]
        let scroll = NSScrollView(); scroll.hasVerticalScroller = true; scroll.documentView = source
        source.frame = NSRect(x: 0, y: 0, width: 552, height: 560)
        source.textContainer?.widthTracksTextView = true
        let stack = NSStackView(views: [row([picker, NSTextField(labelWithString: "Time"), time]), scroll, row([button("Apply source", #selector(applySource)), button("Save brush…", #selector(saveBrush))])])
        stack.orientation = .vertical; stack.alignment = .leading; stack.spacing = 12
        stack.translatesAutoresizingMaskIntoConstraints = false; editor.contentView!.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: editor.contentView!.leadingAnchor, constant: 20),
            stack.trailingAnchor.constraint(equalTo: editor.contentView!.trailingAnchor, constant: -20),
            stack.topAnchor.constraint(equalTo: editor.contentView!.topAnchor, constant: 20),
            stack.bottomAnchor.constraint(equalTo: editor.contentView!.bottomAnchor, constant: -20),
            scroll.widthAnchor.constraint(equalTo: stack.widthAnchor),
            scroll.heightAnchor.constraint(greaterThanOrEqualToConstant: 300),
        ])
        sourceWindow = editor; editor.center(); editor.makeKeyAndOrderFront(nil)
    }
}

MainActor.assumeIsolated {
    let app = NSApplication.shared
    app.setActivationPolicy(.regular)
    let delegate = BrushApp()
    app.delegate = delegate
    withExtendedLifetime(delegate) { app.run() }
}
