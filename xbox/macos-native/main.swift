import AppKit
import AVFoundation
import CoreVideo
import Darwin
import Foundation
import GameController
import JavaScriptCore

private struct Ink {
    let red: CGFloat
    let green: CGFloat
    let blue: CGFloat

    init(_ red: Double, _ green: Double, _ blue: Double) {
        self.red = CGFloat(max(0, min(255, red)) / 255)
        self.green = CGFloat(max(0, min(255, green)) / 255)
        self.blue = CGFloat(max(0, min(255, blue)) / 255)
    }

    var color: NSColor { NSColor(srgbRed: red, green: green, blue: blue, alpha: 1) }
}

private enum DrawCommand {
    case box(CGRect, Ink)
    case line(CGPoint, CGPoint, CGFloat, Ink)
    case triangle(CGPoint, CGPoint, CGPoint, Ink)
    case text(String, CGPoint, CGFloat, Ink, String)
}

private final class NativeRenderer {
    var logicalSize = CGSize(width: 1920, height: 1080)
    var backdrop = Ink(7, 8, 28)
    var commands: [DrawCommand] = []

    func beginFrame() {
        commands.removeAll(keepingCapacity: true)
    }

    func wipe(_ red: Double, _ green: Double, _ blue: Double) {
        backdrop = Ink(red, green, blue)
        commands.removeAll(keepingCapacity: true)
    }
}

private final class NativeInput {
    private var keyboard = [Set<String>(), Set<String>()]

    func setKey(_ event: NSEvent, down: Bool) {
        guard let mapping = keyMapping(event.keyCode) else { return }
        if down { keyboard[mapping.0].insert(mapping.1) }
        else { keyboard[mapping.0].remove(mapping.1) }
    }

    func pulse(_ button: String, player: Int = 0) {
        keyboard[player].insert(button)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.08) { [weak self] in
            self?.keyboard[player].remove(button)
        }
    }

    func clear() {
        keyboard[0].removeAll()
        keyboard[1].removeAll()
    }

    func pad(_ index: Int) -> NSDictionary {
        var down = index < keyboard.count ? keyboard[index] : []
        let controllers = GCController.controllers()
        let controller = index < controllers.count ? controllers[index] : nil
        var leftX = 0.0
        var leftY = 0.0
        var rightX = 0.0
        var rightY = 0.0
        var leftTrigger = 0.0
        var rightTrigger = 0.0
        if let gamepad = controller?.extendedGamepad {
            leftX = Double(gamepad.leftThumbstick.xAxis.value)
            leftY = Double(gamepad.leftThumbstick.yAxis.value)
            rightX = Double(gamepad.rightThumbstick.xAxis.value)
            rightY = Double(gamepad.rightThumbstick.yAxis.value)
            leftTrigger = Double(gamepad.leftTrigger.value)
            rightTrigger = Double(gamepad.rightTrigger.value)
            if gamepad.buttonA.isPressed { down.insert("A") }
            if gamepad.buttonB.isPressed { down.insert("B") }
            if gamepad.buttonX.isPressed { down.insert("X") }
            if gamepad.buttonY.isPressed { down.insert("Y") }
            if gamepad.leftShoulder.isPressed { down.insert("LeftShoulder") }
            if gamepad.rightShoulder.isPressed { down.insert("RightShoulder") }
            if gamepad.dpad.up.isPressed { down.insert("ArrowUp") }
            if gamepad.dpad.down.isPressed { down.insert("ArrowDown") }
            if gamepad.dpad.left.isPressed { down.insert("ArrowLeft") }
            if gamepad.dpad.right.isPressed { down.insert("ArrowRight") }
            if gamepad.buttonMenu.isPressed { down.insert("Menu") }
            if gamepad.buttonOptions?.isPressed == true { down.insert("View") }
            if gamepad.leftThumbstickButton?.isPressed == true { down.insert("LeftStick") }
            if gamepad.rightThumbstickButton?.isPressed == true { down.insert("RightStick") }
        }
        return [
            "index": index,
            "connected": index == 0 || !down.isEmpty || controller != nil,
            "down": Array(down).sorted(),
            "leftX": leftX, "leftY": leftY,
            "rightX": rightX, "rightY": rightY,
            "leftTrigger": leftTrigger, "rightTrigger": rightTrigger,
        ] as NSDictionary
    }

    func inventory() -> NSArray {
        let controllers = GCController.controllers()
        return [0, 1].map { index in
            let controller = index < controllers.count ? controllers[index] : nil
            return [
                "index": index,
                "connected": controller != nil || index == 0,
                "name": controller?.vendorName ?? (index == 0 ? "MAC KEYBOARD" : "KEYBOARD P2"),
            ] as NSDictionary
        } as NSArray
    }

    private func keyMapping(_ code: UInt16) -> (Int, String)? {
        switch code {
        case 13: return (0, "ArrowUp")       // W
        case 1: return (0, "ArrowDown")      // S
        case 0: return (0, "ArrowLeft")      // A
        case 2: return (0, "ArrowRight")     // D
        case 49, 3: return (0, "A")          // Space / F: kick
        case 11, 4: return (0, "X")          // B / H: punch
        case 5, 53, 51: return (0, "B")      // G / Escape / Backspace: defend/back
        case 48, 12: return (0, "View")      // Tab / Q: debug
        case 36: return (0, "Menu")          // Return
        case 126: return (1, "ArrowUp")
        case 125: return (1, "ArrowDown")
        case 123: return (1, "ArrowLeft")
        case 124: return (1, "ArrowRight")
        case 40: return (1, "A")             // K
        case 37: return (1, "B")             // L
        case 41: return (1, "X")             // ;
        default: return nil
        }
    }
}

private final class NativeAudio {
    private let engine = AVAudioEngine()
    private var players: [AVAudioPlayerNode] = []
    private var cursor = 0
    private let sampleRate = 48_000.0
    private var noiseState: UInt64 = 0x6f736b6965776172

    init() {
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 2) else { return }
        for _ in 0..<24 {
            let player = AVAudioPlayerNode()
            engine.attach(player)
            engine.connect(player, to: engine.mainMixerNode, format: format)
            players.append(player)
        }
        engine.mainMixerNode.outputVolume = 0.72
        try? engine.start()
    }

    func drum(_ name: String, velocity: Double, pan: Double) {
        let duration: Double
        switch name {
        case "kick": duration = 0.22
        case "clap", "punch", "block": duration = 0.14
        case "whoosh": duration = 0.30
        case "bell": duration = 0.38
        default: duration = 0.075
        }
        play(duration: duration, pan: pan, sample: { [weak self] time, progress in
            guard let self else { return 0 }
            let envelope = pow(max(0, 1 - progress), name == "bell" ? 1.7 : 3.1)
            let noise = self.noise()
            switch name {
            case "kick":
                let frequency = 145 - 104 * progress
                return sin(2 * .pi * frequency * time) * envelope * 0.86
            case "hat", "bullet": return noise * envelope * 0.42
            case "clap": return noise * envelope * (sin(progress * 52) > 0 ? 0.58 : 0.22)
            case "bell":
                return (sin(2 * .pi * 880 * time) + sin(2 * .pi * 1320 * time) * 0.35) * envelope * 0.46
            case "whoosh": return noise * sin(.pi * progress) * 0.34
            case "block": return (noise * 0.25 + sin(2 * .pi * 115 * time) * 0.55) * envelope
            default: return (noise * 0.24 + sin(2 * .pi * 170 * time) * 0.48) * envelope
            }
        }, gain: max(0, min(1, velocity)))
    }

    func synth(_ frequency: Double, duration: Double) {
        play(duration: max(0.02, min(2, duration)), pan: 0, sample: { time, progress in
            sin(2 * .pi * max(30, min(10_000, frequency)) * time) * pow(1 - progress, 2)
        }, gain: 0.42)
    }

    private func play(duration: Double, pan: Double,
                      sample: (Double, Double) -> Double, gain: Double) {
        guard !players.isEmpty,
              let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 2),
              let buffer = AVAudioPCMBuffer(pcmFormat: format,
                  frameCapacity: AVAudioFrameCount(sampleRate * duration)),
              let channels = buffer.floatChannelData else { return }
        buffer.frameLength = buffer.frameCapacity
        let leftGain = sqrt((1 - max(-1, min(1, pan))) * 0.5) * gain
        let rightGain = sqrt((1 + max(-1, min(1, pan))) * 0.5) * gain
        for frame in 0..<Int(buffer.frameLength) {
            let time = Double(frame) / sampleRate
            let progress = Double(frame) / Double(max(1, Int(buffer.frameLength) - 1))
            let value = Float(max(-1, min(1, sample(time, progress))))
            channels[0][frame] = value * Float(leftGain)
            channels[1][frame] = value * Float(rightGain)
        }
        let player = players[cursor % players.count]
        cursor += 1
        player.scheduleBuffer(buffer)
        if !player.isPlaying { player.play() }
    }

    private func noise() -> Double {
        noiseState ^= noiseState << 13
        noiseState ^= noiseState >> 7
        noiseState ^= noiseState << 17
        return Double(noiseState & 0xffff) / 32767.5 - 1
    }
}

private final class OscSender {
    private var descriptor: Int32 = -1
    private var sequence: UInt32 = 0

    init() {
        descriptor = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)
        guard descriptor >= 0 else { return }
        var enabled: Int32 = 1
        setsockopt(descriptor, SOL_SOCKET, SO_BROADCAST, &enabled,
                   socklen_t(MemoryLayout<Int32>.size))
    }

    deinit { if descriptor >= 0 { close(descriptor) } }

    func send(event: String, player: Int32, value: Float, value2: Float) {
        guard descriptor >= 0, event.range(of: "^[a-z0-9_-]{1,32}$",
            options: .regularExpression) != nil else { return }
        sequence &+= 1
        var packet: [UInt8] = []
        appendOscString("/oskiewar/" + event, to: &packet)
        appendOscString(",iffi", to: &packet)
        appendInt(player, to: &packet)
        appendInt(Int32(bitPattern: value.bitPattern), to: &packet)
        appendInt(Int32(bitPattern: value2.bitPattern), to: &packet)
        appendInt(Int32(bitPattern: sequence), to: &packet)
        var address = sockaddr_in()
        address.sin_len = UInt8(MemoryLayout<sockaddr_in>.size)
        address.sin_family = sa_family_t(AF_INET)
        address.sin_port = in_port_t(51338).bigEndian
        address.sin_addr = in_addr(s_addr: inet_addr("255.255.255.255"))
        packet.withUnsafeBytes { bytes in
            withUnsafePointer(to: &address) { pointer in
                pointer.withMemoryRebound(to: sockaddr.self, capacity: 1) { socketAddress in
                    for _ in 0..<3 {
                        _ = Darwin.sendto(descriptor, bytes.baseAddress, bytes.count, 0,
                            socketAddress, socklen_t(MemoryLayout<sockaddr_in>.size))
                    }
                }
            }
        }
    }

    private func appendOscString(_ value: String, to packet: inout [UInt8]) {
        packet.append(contentsOf: value.utf8)
        packet.append(0)
        while packet.count % 4 != 0 { packet.append(0) }
    }

    private func appendInt(_ value: Int32, to packet: inout [UInt8]) {
        var big = UInt32(bitPattern: value).bigEndian
        withUnsafeBytes(of: &big) { packet.append(contentsOf: $0) }
    }
}

private final class NetworkBridge {
    private var liveTask: URLSessionWebSocketTask?
    private var liveMatch = ""

    func saveReplay(_ payload: String) {
        guard let data = payload.data(using: .utf8), data.count <= 524_288,
              let url = URL(string: "https://aesthetic.computer/api/oskiewar-replays?surface=macos-native") else { return }
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "content-type")
        URLSession.shared.uploadTask(with: request, from: data).resume()
    }

    func publishLive(match: String, payload: String) {
        guard match.range(of: "^ow-[a-z0-9-]{5,24}$", options: .regularExpression) != nil,
              payload.utf8.count <= 7168 else { return }
        if match != liveMatch {
            liveTask?.cancel(with: .goingAway, reason: nil)
            liveMatch = match
            var components = URLComponents(string: "wss://session-server.aesthetic.computer/oskiewar-live")
            components?.queryItems = [
                URLQueryItem(name: "match", value: match),
                URLQueryItem(name: "role", value: "publisher"),
                URLQueryItem(name: "surface", value: "macos-native"),
            ]
            guard let url = components?.url else { return }
            liveTask = URLSession.shared.webSocketTask(with: url)
            liveTask?.resume()
        }
        guard let liveTask else { return }
        let envelope = "{\"type\":\"oskiewar:state\",\"content\":\(payload)}"
        Task { try? await liveTask.send(.string(envelope)) }
    }
}

private final class GameView: NSView {
    weak var host: NativeGameHost?
    var renderer: NativeRenderer!
    private var tracking: NSTrackingArea?

    override var isFlipped: Bool { true }
    override var acceptsFirstResponder: Bool { true }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        updateBackingScale()
    }

    override func viewDidChangeBackingProperties() {
        super.viewDidChangeBackingProperties()
        updateBackingScale()
    }

    private func updateBackingScale() {
        wantsLayer = true
        layer?.contentsScale = window?.backingScaleFactor ?? NSScreen.main?.backingScaleFactor ?? 1
        needsDisplay = true
    }

    override func updateTrackingAreas() {
        if let tracking { removeTrackingArea(tracking) }
        tracking = NSTrackingArea(rect: bounds,
            options: [.activeAlways, .mouseMoved, .mouseEnteredAndExited, .inVisibleRect],
            owner: self, userInfo: nil)
        addTrackingArea(tracking!)
        super.updateTrackingAreas()
    }

    override func keyDown(with event: NSEvent) {
        host?.setKey(event, down: true)
    }

    override func keyUp(with event: NSEvent) {
        host?.setKey(event, down: false)
    }

    override func mouseMoved(with event: NSEvent) { updatePointer(event, active: true) }
    override func mouseEntered(with event: NSEvent) { updatePointer(event, active: true) }
    override func mouseExited(with event: NSEvent) { host?.clearPointer() }
    override func mouseDown(with event: NSEvent) {
        updatePointer(event, active: true)
        host?.mousePressed(point: logicalPoint(event))
    }

    private func updatePointer(_ event: NSEvent, active: Bool) {
        host?.updatePointer(point: logicalPoint(event), active: active)
    }

    private func logicalPoint(_ event: NSEvent) -> CGPoint {
        let point = convert(event.locationInWindow, from: nil)
        return CGPoint(x: point.x / max(1, bounds.width) * renderer.logicalSize.width,
                       y: point.y / max(1, bounds.height) * renderer.logicalSize.height)
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let context = NSGraphicsContext.current?.cgContext else { return }
        renderer.backdrop.color.setFill()
        context.fill(bounds)
        let sx = bounds.width / max(1, renderer.logicalSize.width)
        let sy = bounds.height / max(1, renderer.logicalSize.height)
        context.saveGState()
        context.scaleBy(x: sx, y: sy)
        for command in renderer.commands {
            switch command {
            case let .box(rect, ink):
                ink.color.setFill()
                context.fill(rect)
            case let .line(first, second, width, ink):
                context.beginPath()
                context.move(to: first)
                context.addLine(to: second)
                context.setLineWidth(width)
                context.setLineCap(.round)
                context.setStrokeColor(ink.color.cgColor)
                context.strokePath()
            case let .triangle(first, second, third, ink):
                context.beginPath()
                context.move(to: first)
                context.addLine(to: second)
                context.addLine(to: third)
                context.closePath()
                context.setFillColor(ink.color.cgColor)
                context.fillPath()
            case let .text(text, point, size, ink, family):
                let font = NSFont(name: family, size: size) ?? NSFont.systemFont(ofSize: size)
                let attributes: [NSAttributedString.Key: Any] = [
                    .font: font, .foregroundColor: ink.color,
                ]
                for (index, line) in text.components(separatedBy: "\n").enumerated() {
                    (line as NSString).draw(at: CGPoint(x: point.x,
                        y: point.y + CGFloat(index) * size * 1.15), withAttributes: attributes)
                }
            }
        }
        context.restoreGState()
    }
}

private func displayLinkCallback(_ link: CVDisplayLink,
    _ now: UnsafePointer<CVTimeStamp>, _ output: UnsafePointer<CVTimeStamp>,
    _ flagsIn: CVOptionFlags, _ flagsOut: UnsafeMutablePointer<CVOptionFlags>,
    _ context: UnsafeMutableRawPointer?) -> CVReturn {
    guard let context else { return kCVReturnError }
    Unmanaged<NativeGameHost>.fromOpaque(context).takeUnretainedValue().scheduleFrame()
    return kCVReturnSuccess
}

private final class NativeGameHost {
    let renderer = NativeRenderer()
    let input = NativeInput()
    let audio = NativeAudio()
    let osc = OscSender()
    let network = NetworkBridge()
    let view: GameView
    private let javascript = JSContext()!
    private var displayLink: CVDisplayLink?
    private let scheduleLock = NSLock()
    private var framePending = false
    private var lastFrame = CACurrentMediaTime()
    private var simulationTime = CACurrentMediaTime()
    private var accumulator = 0.0
    private var simCount = 0
    private var paintCount = 0
    private var runtimeSeconds = CACurrentMediaTime()
    private var javascriptError = ""

    init(view: GameView) {
        self.view = view
        view.renderer = renderer
        view.host = self
        installBindings()
    }

    deinit { if let displayLink { CVDisplayLinkStop(displayLink) } }

    func start() {
        GCController.startWirelessControllerDiscovery(completionHandler: nil)
        guard let resources = Bundle.main.resourceURL,
              let hello = try? String(contentsOf:
                resources.appendingPathComponent("live/hello.js"), encoding: .utf8),
              var qr = try? String(contentsOf:
                resources.appendingPathComponent("live/qr.mjs"), encoding: .utf8) else {
            javascriptError = "missing native game resources"
            return
        }
        if let regex = try? NSRegularExpression(pattern: "\\nexport\\s*\\{[\\s\\S]*?\\};\\s*$") {
            qr = regex.stringByReplacingMatches(in: qr,
                range: NSRange(qr.startIndex..., in: qr), withTemplate: "\n")
        }
        javascript.evaluateScript(qr + "\n" + hello,
            withSourceURL: URL(fileURLWithPath: "oskiewar/hello.js"))
        call("boot")
        var link: CVDisplayLink?
        if CVDisplayLinkCreateWithActiveCGDisplays(&link) == kCVReturnSuccess,
           let link {
            displayLink = link
            CVDisplayLinkSetOutputCallback(link, displayLinkCallback,
                Unmanaged.passUnretained(self).toOpaque())
            CVDisplayLinkStart(link)
        }
    }

    func scheduleFrame() {
        scheduleLock.lock()
        guard !framePending else { scheduleLock.unlock(); return }
        framePending = true
        scheduleLock.unlock()
        DispatchQueue.main.async { [weak self] in
            guard let self else { return }
            self.frame()
            self.scheduleLock.lock()
            self.framePending = false
            self.scheduleLock.unlock()
        }
    }

    func setKey(_ event: NSEvent, down: Bool) { input.setKey(event, down: down) }

    func updatePointer(point: CGPoint, active: Bool) {
        javascript.evaluateScript("globalThis.__oskiewarTouch.pointer.x=\(point.x);globalThis.__oskiewarTouch.pointer.y=\(point.y);globalThis.__oskiewarTouch.pointer.active=\(active ? "true" : "false");")
    }

    func clearPointer() {
        javascript.evaluateScript("globalThis.__oskiewarTouch.pointer.active=false")
        NSCursor.arrow.set()
    }

    func mousePressed(point: CGPoint) {
        let screen = javascript.objectForKeyedSubscript("__oskiewarTouch")?
            .objectForKeyedSubscript("screen")?.toString() ?? ""
        if screen == "select" {
            javascript.evaluateScript("globalThis.__oskiewarTouch.taps.push({x:\(point.x),y:\(point.y)})")
        } else if screen == "title" {
            if titleButtonContains(point) { input.pulse("A") }
            else { audio.drum("block", velocity: 0.32, pan: 0) }
        }
    }

    private func frame() {
        let now = CACurrentMediaTime()
        let delta = min(0.1, max(0, now - lastFrame))
        lastFrame = now
        accumulator += delta
        let fixed = 1.0 / 60.0
        var steps = 0
        while accumulator >= fixed && steps < 6 {
            simulationTime += fixed
            runtimeSeconds = simulationTime
            simCount += 1
            call("sim")
            accumulator -= fixed
            steps += 1
        }
        if steps == 6 { accumulator = 0 }
        updateLogicalSize()
        renderer.beginFrame()
        runtimeSeconds = now
        paintCount += 1
        if javascriptError.isEmpty { call("paint") }
        if !javascriptError.isEmpty {
            renderer.wipe(8, 10, 20)
            renderer.commands.append(.text("aesthetic.computer error", CGPoint(x: 72, y: 72),
                52, Ink(255, 92, 116), "Comic Relief"))
            renderer.commands.append(.text(javascriptError, CGPoint(x: 72, y: 150),
                25, Ink(230, 235, 248), "Comic Relief"))
        }
        syncCursor()
        view.needsDisplay = true
    }

    private func updateLogicalSize() {
        let width = max(1, view.bounds.width)
        let height = max(1, view.bounds.height)
        renderer.logicalSize = CGSize(width: max(480, min(2880, round(1080 * width / height))),
                                      height: 1080)
    }

    private func call(_ name: String) {
        guard javascriptError.isEmpty,
              let function = javascript.objectForKeyedSubscript(name),
              !function.isUndefined else { return }
        function.call(withArguments: [])
    }

    private func titleButtonContains(_ point: CGPoint) -> Bool {
        guard let bridge = javascript.objectForKeyedSubscript("__oskiewarTouch"),
              let button = bridge.objectForKeyedSubscript("titleButton"),
              !button.isUndefined else { return false }
        let x = button.objectForKeyedSubscript("x")?.toDouble() ?? .nan
        let y = button.objectForKeyedSubscript("y")?.toDouble() ?? .nan
        let width = button.objectForKeyedSubscript("width")?.toDouble() ?? 0
        let height = button.objectForKeyedSubscript("height")?.toDouble() ?? 0
        return point.x >= x && point.x <= x + width &&
            point.y >= y && point.y <= y + height
    }

    private func syncCursor() {
        guard let bridge = javascript.objectForKeyedSubscript("__oskiewarTouch") else { return }
        let titleHover = bridge.objectForKeyedSubscript("titleHover")?.toBool() == true
        let selectionHover = bridge.objectForKeyedSubscript("hover")
        if titleHover || (selectionHover != nil && selectionHover?.isNull == false &&
            selectionHover?.isUndefined == false) { NSCursor.pointingHand.set() }
        else { NSCursor.arrow.set() }
    }

    private func installBindings() {
        javascript.exceptionHandler = { [weak self] _, exception in
            self?.javascriptError = exception?.toString() ?? "javascript exception"
        }
        javascript.evaluateScript("globalThis.__oskiewarTouch={pointer:{active:false,x:0,y:0},taps:[]};")

        let wipe: @convention(block) (Double, Double, Double) -> Void = { [weak self] r, g, b in
            self?.renderer.wipe(r, g, b)
        }
        let box: @convention(block) (Double, Double, Double, Double, Double, Double, Double) -> Void =
            { [weak self] x, y, width, height, r, g, b in
                self?.renderer.commands.append(.box(CGRect(x: x, y: y, width: width, height: height), Ink(r, g, b)))
            }
        let line: @convention(block) (Double, Double, Double, Double, Double, Double, Double, Double) -> Void =
            { [weak self] x1, y1, x2, y2, width, r, g, b in
                self?.renderer.commands.append(.line(CGPoint(x: x1, y: y1), CGPoint(x: x2, y: y2),
                    CGFloat(max(0, width)), Ink(r, g, b)))
            }
        let triangle: @convention(block) (Double, Double, Double, Double, Double, Double, Double, Double, Double) -> Void =
            { [weak self] x1, y1, x2, y2, x3, y3, r, g, b in
                self?.renderer.commands.append(.triangle(CGPoint(x: x1, y: y1), CGPoint(x: x2, y: y2),
                    CGPoint(x: x3, y: y3), Ink(r, g, b)))
            }
        let triangle3d: @convention(block) (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double) -> Void =
            { [weak self] x1, y1, _, x2, y2, _, x3, y3, _, r, g, b in
                self?.renderer.commands.append(.triangle(CGPoint(x: x1, y: y1), CGPoint(x: x2, y: y2),
                    CGPoint(x: x3, y: y3), Ink(r, g, b)))
            }
        let text: (String) -> @convention(block) (String, Double, Double, Double, Double, Double, Double) -> Void =
            { family in { [weak self] value, x, y, size, r, g, b in
                self?.renderer.commands.append(.text(value, CGPoint(x: x, y: y), CGFloat(max(1, size)),
                    Ink(r, g, b), family))
            }}
        let runtime: @convention(block) () -> NSDictionary = { [weak self] in
            guard let self else { return [:] }
            return [
                "width": Int(self.renderer.logicalSize.width),
                "height": Int(self.renderer.logicalSize.height),
                "monotonicUs": Int64(self.runtimeSeconds * 1_000_000),
                "simMonotonicUs": Int64(self.simulationTime * 1_000_000),
                "unixMs": Int64(Date().timeIntervalSince1970 * 1000),
                "simCount": self.simCount, "paintCount": self.paintCount,
                "frameRate": 60, "audioLatencyMs": 0,
                "displayScale": self.view.window?.backingScaleFactor ?? 1,
                "clientErrorReportStatus": "native host",
            ] as NSDictionary
        }
        let gameView: @convention(block) () -> NSDictionary = { [weak self] in
            guard let self else { return [:] }
            let size = self.renderer.logicalSize
            return ["width": Int(size.width), "height": Int(size.height),
                "aspect": size.width / size.height,
                "orientation": size.width < size.height ? "portrait" : "landscape"] as NSDictionary
        }
        let gamepad: @convention(block) (Int32) -> NSDictionary = { [weak self] index in
            self?.input.pad(Int(index)) ?? [:]
        }
        let controllers: @convention(block) () -> NSArray = { [weak self] in
            self?.input.inventory() ?? []
        }
        let capabilities: @convention(block) () -> NSDictionary = { [weak self] in
            let hasController = !GCController.controllers().isEmpty
            let light = NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .aqua
            return [
                "platform": "macos-native", "inputFamily": hasController ? "xbox" : "keyboard",
                "colorScheme": light ? "light" : "dark", "productName": "MACOS",
                "version": Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "1.0",
                "graphics": "APPKIT NATIVE", "audio": "AVFOUNDATION",
                "displayScale": self?.view.window?.backingScaleFactor ?? 1,
                "online": true, "liveLocalState": true,
                "width": Int(self?.renderer.logicalSize.width ?? 1920),
                "height": Int(self?.renderer.logicalSize.height ?? 1080),
            ] as NSDictionary
        }
        let telemetry: @convention(block) (String, String) -> Void = { kind, detail in
            NSLog("oskiewar %@ %@", kind, detail)
        }
        let drum: @convention(block) (String, Double, Double) -> Void = { [weak self] name, velocity, pan in
            self?.audio.drum(name, velocity: velocity, pan: pan)
        }
        let synth: @convention(block) (Double, Double) -> Void = { [weak self] frequency, duration in
            self?.audio.synth(frequency, duration: duration)
        }
        let gameSignal: @convention(block) (String, Int32, Double, Double) -> Void =
            { [weak self] event, player, value, value2 in
                self?.osc.send(event: event, player: player, value: Float(value), value2: Float(value2))
            }
        let saveReplay: @convention(block) (String) -> Void = { [weak self] payload in
            self?.network.saveReplay(payload)
        }
        let publishLive: @convention(block) (String, String) -> Void = { [weak self] match, payload in
            self?.network.publishLive(match: match, payload: payload)
        }
        let ac: @convention(block) () -> NSDictionary = {
            ["fighters": [], "moodHandle": "", "mood": "", "source": "macos-native"] as NSDictionary
        }
        javascript.setObject(wipe, forKeyedSubscript: "wipe" as NSString)
        javascript.setObject(box, forKeyedSubscript: "box" as NSString)
        javascript.setObject(line, forKeyedSubscript: "line" as NSString)
        javascript.setObject(triangle, forKeyedSubscript: "triangle" as NSString)
        javascript.setObject(triangle3d, forKeyedSubscript: "triangle3d" as NSString)
        javascript.setObject(text("Comic Relief"), forKeyedSubscript: "comicWrite" as NSString)
        javascript.setObject(text("Comic Relief"), forKeyedSubscript: "ywftWrite" as NSString)
        javascript.setObject(text("Helvetica"), forKeyedSubscript: "systemWrite" as NSString)
        javascript.setObject(text("Menlo"), forKeyedSubscript: "write" as NSString)
        javascript.setObject(runtime, forKeyedSubscript: "runtime" as NSString)
        javascript.setObject(gameView, forKeyedSubscript: "gameView" as NSString)
        javascript.setObject(gamepad, forKeyedSubscript: "gamepad" as NSString)
        javascript.setObject(controllers, forKeyedSubscript: "controllers" as NSString)
        javascript.setObject(capabilities, forKeyedSubscript: "capabilities" as NSString)
        javascript.setObject(telemetry, forKeyedSubscript: "telemetry" as NSString)
        javascript.setObject(drum, forKeyedSubscript: "drum" as NSString)
        javascript.setObject(synth, forKeyedSubscript: "synth" as NSString)
        javascript.setObject(gameSignal, forKeyedSubscript: "gameSignal" as NSString)
        javascript.setObject(saveReplay, forKeyedSubscript: "saveReplay" as NSString)
        javascript.setObject(publishLive, forKeyedSubscript: "publishLive" as NSString)
        javascript.setObject(ac, forKeyedSubscript: "ac" as NSString)
    }
}

private final class AppDelegate: NSObject, NSApplicationDelegate, NSWindowDelegate {
    private var window: NSWindow!
    private var host: NativeGameHost!

    func applicationDidFinishLaunching(_ notification: Notification) {
        registerComicFont()
        if let iconURL = Bundle.main.url(forResource: "Oskiewar", withExtension: "icns"),
           let icon = NSImage(contentsOf: iconURL) { NSApp.applicationIconImage = icon }
        installMenus()
        let screen = NSScreen.main?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1600, height: 900)
        let targetWidth = min(1600, screen.width * 0.92)
        let targetHeight = min(900, screen.height * 0.92)
        let view = GameView(frame: NSRect(x: 0, y: 0, width: targetWidth, height: targetHeight))
        window = NSWindow(contentRect: view.bounds,
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered, defer: false)
        window.title = "oskiewar"
        window.titleVisibility = .visible
        window.titlebarAppearsTransparent = false
        window.backgroundColor = .black
        window.minSize = NSSize(width: 640, height: 480)
        window.contentView = view
        window.delegate = self
        window.center()
        window.makeKeyAndOrderFront(nil)
        window.makeFirstResponder(view)
        host = NativeGameHost(view: view)
        host.start()
        NSApp.activate(ignoringOtherApps: true)
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }

    func applicationDidResignActive(_ notification: Notification) { host?.input.clear() }

    private func registerComicFont() {
        guard let url = Bundle.main.url(forResource: "ComicRelief-Regular", withExtension: "ttf") else { return }
        CTFontManagerRegisterFontsForURL(url as CFURL, .process, nil)
    }

    private func installMenus() {
        let menu = NSMenu()
        let appItem = NSMenuItem()
        menu.addItem(appItem)
        let appMenu = NSMenu()
        appMenu.addItem(withTitle: "About oskiewar", action: #selector(NSApplication.orderFrontStandardAboutPanel(_:)), keyEquivalent: "")
        appMenu.addItem(.separator())
        appMenu.addItem(withTitle: "Hide oskiewar", action: #selector(NSApplication.hide(_:)), keyEquivalent: "h")
        appMenu.addItem(withTitle: "Quit oskiewar", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        appItem.submenu = appMenu
        let viewItem = NSMenuItem()
        menu.addItem(viewItem)
        let viewMenu = NSMenu(title: "View")
        viewMenu.addItem(withTitle: "Toggle Full Screen", action: #selector(NSWindow.toggleFullScreen(_:)), keyEquivalent: "f")
        viewItem.submenu = viewMenu
        NSApp.mainMenu = menu
    }
}

private let application = NSApplication.shared
private let delegate = AppDelegate()
application.setActivationPolicy(.regular)
application.delegate = delegate
application.run()
