import AppKit
import AVFoundation
import CoreVideo
import Darwin
import Foundation
import GameController
import JavaScriptCore

/// Metal draws corners, so every shape the engine names arrives here as
/// triangles: a box is two faces, a line is a quad struck off the segment's
/// perpendicular. Colour rides through as 0–255 because MetalSceneView
/// normalizes at the vertex, and converting on the way in would only do it
/// twice.
private final class NativeRenderer {
    /// Nearer than the nearest face the engine ever projects (-1.445), so
    /// overlay furniture sits on top of the world.
    private static let overlay = -1.47
    /// Behind the far clamp on the engine's projection (1.4), so the sky it
    /// lays down before the world stays under it.
    private static let backdrop = 1.45

    let scene: MetalSceneView
    private var clear = (red: 7.0, green: 8.0, blue: 28.0)
    private var sawFace = false
    /// One face's worth of floats, reused. `triangleBatch` wants a pointer and
    /// a match frame submits a couple of thousand faces; an array per face
    /// would spend the whole saving on the heap.
    private var face = [Float](repeating: 0, count: 12)

    init(scene: MetalSceneView) { self.scene = scene }

    var logicalSize: CGSize {
        get { scene.logicalSize }
        set { scene.logicalSize = newValue }
    }

    func beginFrame() {
        scene.beginFrame(red: clear.red, green: clear.green, blue: clear.blue)
        sawFace = false
    }

    func wipe(_ red: Double, _ green: Double, _ blue: Double) {
        clear = (red, green, blue)
        beginFrame()
    }

    func box(_ x: Double, _ y: Double, _ width: Double, _ height: Double,
             _ red: Double, _ green: Double, _ blue: Double) {
        let z = flat, right = x + width, bottom = y + height
        emit(x, y, z, right, y, z, right, bottom, z, red, green, blue)
        emit(x, y, z, right, bottom, z, x, bottom, z, red, green, blue)
    }

    /// Butt ends, not round. CoreGraphics gave the caps away with
    /// `setLineCap`; here each one would be another fan per segment, and the
    /// engine already moved every line that reads as a limb over to
    /// `filledCapsule`, which draws its own ends.
    func line(_ x1: Double, _ y1: Double, _ x2: Double, _ y2: Double,
              _ width: Double, _ red: Double, _ green: Double, _ blue: Double) {
        let dx = x2 - x1, dy = y2 - y1
        let span = (dx * dx + dy * dy).squareRoot()
        guard span > 0 else { return }
        let reach = max(1, width) / 2 / span
        let nx = -dy * reach, ny = dx * reach, z = flat
        emit(x1 + nx, y1 + ny, z, x2 + nx, y2 + ny, z, x2 - nx, y2 - ny, z,
             red, green, blue)
        emit(x1 + nx, y1 + ny, z, x2 - nx, y2 - ny, z, x1 - nx, y1 - ny, z,
             red, green, blue)
    }

    func triangle(_ x1: Double, _ y1: Double, _ x2: Double, _ y2: Double,
                  _ x3: Double, _ y3: Double,
                  _ red: Double, _ green: Double, _ blue: Double) {
        let z = flat
        emit(x1, y1, z, x2, y2, z, x3, y3, z, red, green, blue)
    }

    func triangle3d(_ x1: Double, _ y1: Double, _ z1: Double,
                    _ x2: Double, _ y2: Double, _ z2: Double,
                    _ x3: Double, _ y3: Double, _ z3: Double,
                    _ red: Double, _ green: Double, _ blue: Double) {
        sawFace = true
        emit(x1, y1, z1, x2, y2, z2, x3, y3, z3, red, green, blue)
    }

    func write(_ value: String, family: String, x: Double, y: Double,
               size: Double, _ red: Double, _ green: Double, _ blue: Double) {
        scene.write(value, family: family, x: x, y: y, size: max(1, size),
                    red: red, green: green, blue: blue)
    }

    /// The flat lane carries two different things under one name: the sky the
    /// engine lays down before the world, and the keycaps and panels it lays
    /// over it. Nothing at the boundary tells them apart except when they
    /// arrive, so the lane splits at the frame's first projected face —
    /// everything up to it is backdrop, everything after it is overlay.
    /// Painter's order handed the AppKit host this for free; a depth buffer
    /// has to be told.
    private var flat: Double { sawFace ? Self.overlay : Self.backdrop }

    private func emit(_ x1: Double, _ y1: Double, _ z1: Double,
                      _ x2: Double, _ y2: Double, _ z2: Double,
                      _ x3: Double, _ y3: Double, _ z3: Double,
                      _ red: Double, _ green: Double, _ blue: Double) {
        face[0] = Float(x1); face[1] = Float(y1); face[2] = Float(z1)
        face[3] = Float(x2); face[4] = Float(y2); face[5] = Float(z2)
        face[6] = Float(x3); face[7] = Float(y3); face[8] = Float(z3)
        face[9] = Float(red); face[10] = Float(green); face[11] = Float(blue)
        face.withUnsafeBufferPointer {
            _ = scene.triangleBatch($0.baseAddress!, count: 1)
        }
    }
}

private final class NativeInput {
    private var keyboard = [Set<String>(), Set<String>()]

    func setKey(_ event: NSEvent, down: Bool) {
        guard let mapping = keyMapping(event.keyCode) else { return }
        if down { keyboard[mapping.0].insert(mapping.1) }
        else { keyboard[mapping.0].remove(mapping.1) }
    }

    /// Shift and Option never arrive as keyDown — AppKit reports a bare
    /// modifier as a change of flags and nothing else — so X and Y, the two
    /// buttons the web shell binds there, were unpressable until this. The
    /// device-dependent bits are what say *which* key moved; the ordinary
    /// `.shift`/`.option` flags cannot tell the two sides apart, and the game
    /// wants them separable. A modifier let go while the app is in the
    /// background reports its release to whoever has focus instead of here,
    /// which is what `clear()` on resign is for.
    func setFlags(_ event: NSEvent) {
        guard let side = Self.modifierSides[event.keyCode] else { return }
        setKey(event, down: event.modifierFlags.rawValue & side != 0)
    }

    private static let modifierSides: [UInt16: UInt] = [
        56: 0x2,   // shift, left
        60: 0x4,   // shift, right
        58: 0x20,  // option, left
        61: 0x40,  // option, right
    ]

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

    // The web shell's keyMap (xbox/live/mac-test.html) is the reference — a
    // player who learns the game in a browser should not have to relearn it
    // here, and the engine's own on-screen legend describes that layout. The
    // extra letters are native-only conveniences that collide with nothing.
    private func keyMapping(_ code: UInt16) -> (Int, String)? {
        switch code {
        case 13: return (0, "ArrowUp")       // W
        case 1: return (0, "ArrowDown")      // S
        case 0: return (0, "ArrowLeft")      // A
        case 2: return (0, "ArrowRight")     // D
        case 49, 3: return (0, "A")          // Space / F: kick
        case 36, 5: return (0, "B")          // Return / G: punch
        case 56, 60, 11, 4: return (0, "X")  // Shift / B / H: shield
        case 58, 61: return (0, "Y")         // Option: use item
        case 53, 51: return (0, "Menu")      // Escape / Backspace
        case 48, 12: return (0, "View")      // Tab / Q: debug
        case 126: return (1, "ArrowUp")
        case 125: return (1, "ArrowDown")
        case 123: return (1, "ArrowLeft")
        case 124: return (1, "ArrowRight")
        case 40: return (1, "A")             // K
        case 37: return (1, "B")             // L
        case 41: return (1, "X")             // ;
        case 39: return (1, "Y")             // '
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

    // Scoring a fight from a DAW is a studio errand, not something every
    // player is doing, and the web says so already — the MIDI bridge there is
    // opt-in behind `?midi` rather than on by default. Broadcasting without
    // being asked has a price on macOS that the web never pays: the first
    // packet to 255.255.255.255 makes the system demand local-network
    // permission, and because the title screen's attract fight emits signals,
    // that demand arrives before anyone has seen the game. A player who opened
    // a stick-figure brawler should not be answering a network prompt, so the
    // socket is never opened unless the errand was asked for.
    init() {
        guard ProcessInfo.processInfo.environment["OSKIEWAR_OSC"] == "1" else { return }
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

/// The window's contentView, and the game's ear. MetalSceneView is final —
/// one renderer for both Apple apps, no per-platform subclasses — so input
/// lives on the container that holds the surface rather than on the surface.
private final class GameView: NSView {
    let scene = MetalSceneView()
    weak var host: NativeGameHost?
    private var tracking: NSTrackingArea?

    override var acceptsFirstResponder: Bool { true }

    override init(frame: NSRect) {
        super.init(frame: frame)
        scene.frame = bounds
        scene.autoresizingMask = [.width, .height]
        addSubview(scene)
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) is unsupported") }

    // The scene is furniture, not a control. Left to itself it would take the
    // hit and the game would never hear the click.
    override func hitTest(_ point: NSPoint) -> NSView? {
        bounds.contains(convert(point, from: superview)) ? self : nil
    }

    override func viewDidChangeBackingProperties() {
        super.viewDidChangeBackingProperties()
        // The scene only draws when asked, and a move between displays of
        // different backingScaleFactor hands it a new drawable size. Without a
        // nudge it sits on the stale one until the game's next frame.
        scene.present()
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

    override func flagsChanged(with event: NSEvent) {
        host?.setFlags(event)
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
        let stage = scene.logicalSize
        // AppKit measures from the bottom left and the engine from the top
        // left. The CoreGraphics view claimed `isFlipped` to paper over that;
        // a Metal layer would rather not be told its geometry is upside down,
        // so the flip happens here where only the pointer can feel it.
        return CGPoint(x: point.x / max(1, bounds.width) * stage.width,
                       y: (1 - point.y / max(1, bounds.height)) * stage.height)
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
    let renderer: NativeRenderer
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
    // Opt-in frame accounting. The number that matters for a tvOS port is what
    // `sim` costs without a JIT, since third-party apps get JavaScriptCore in
    // interpreter mode on tvOS. Run with OSKIEWAR_PERF=1 and again under
    // JSC_useJIT=0 to read the same fight both ways.
    private let perf = ProcessInfo.processInfo.environment["OSKIEWAR_PERF"] == "1"
    private var perfSimSeconds = 0.0
    private var perfPaintSeconds = 0.0
    private var perfFrames = 0

    init(view: GameView) {
        self.view = view
        renderer = NativeRenderer(scene: view.scene)
        view.host = self
        installBindings()
    }

    deinit { if let displayLink { CVDisplayLinkStop(displayLink) } }

    func start() {
        GCController.startWirelessControllerDiscovery(completionHandler: nil)
        guard let resources = Bundle.main.resourceURL,
              let hello = try? String(contentsOf:
                resources.appendingPathComponent("live/oskiewar.js"), encoding: .utf8),
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
            withSourceURL: URL(fileURLWithPath: "oskiewar/oskiewar.js"))
        call("boot")
        // A title screen barely simulates anything, so measuring one tells you
        // nothing. Self-play drops straight into a real fight.
        if ProcessInfo.processInfo.environment["OSKIEWAR_SELFPLAY"] == "1" {
            javascript.evaluateScript("startSelfPlay(0)")
        }
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

    func setFlags(_ event: NSEvent) { input.setFlags(event) }

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
        if perf { perfSimSeconds += CACurrentMediaTime() - now }
        updateLogicalSize()
        renderer.beginFrame()
        runtimeSeconds = now
        paintCount += 1
        let paintStarted = CACurrentMediaTime()
        if javascriptError.isEmpty { call("paint") }
        if perf { reportPerf(paintSeconds: CACurrentMediaTime() - paintStarted) }
        if !javascriptError.isEmpty {
            renderer.wipe(8, 10, 20)
            renderer.write("aesthetic.computer error", family: "Comic Relief",
                x: 72, y: 72, size: 52, 255, 92, 116)
            renderer.write(javascriptError, family: "Comic Relief",
                x: 72, y: 150, size: 25, 230, 235, 248)
        }
        syncCursor()
        view.scene.present()
    }

    // Averaged over a second, because a single frame is noise. `sim+paint` is
    // the JavaScript half of the budget; everything left over is Metal and the
    // display link.
    private func reportPerf(paintSeconds: Double) {
        perfPaintSeconds += paintSeconds
        perfFrames += 1
        guard perfFrames >= 60 else { return }
        let sim = perfSimSeconds / Double(perfFrames) * 1000
        let paint = perfPaintSeconds / Double(perfFrames) * 1000
        let jit = ProcessInfo.processInfo.environment["JSC_useJIT"] ?? "(default)"
        print(String(format:
            "oskiewar perf · sim %.2f ms · paint %.2f ms · js %.2f ms of 16.67 · JSC_useJIT=%@",
            sim, paint, sim + paint, jit))
        fflush(stdout)
        perfSimSeconds = 0
        perfPaintSeconds = 0
        perfFrames = 0
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
                self?.renderer.box(x, y, width, height, r, g, b)
            }
        let line: @convention(block) (Double, Double, Double, Double, Double, Double, Double, Double) -> Void =
            { [weak self] x1, y1, x2, y2, width, r, g, b in
                self?.renderer.line(x1, y1, x2, y2, width, r, g, b)
            }
        let triangle: @convention(block) (Double, Double, Double, Double, Double, Double, Double, Double, Double) -> Void =
            { [weak self] x1, y1, x2, y2, x3, y3, r, g, b in
                self?.renderer.triangle(x1, y1, x2, y2, x3, y3, r, g, b)
            }
        let triangle3d: @convention(block) (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double) -> Void =
            { [weak self] x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b in
                self?.renderer.triangle3d(x1, y1, z1, x2, y2, z2, x3, y3, z3, r, g, b)
            }
        let text: (String) -> @convention(block) (String, Double, Double, Double, Double, Double, Double) -> Void =
            { family in { [weak self] value, x, y, size, r, g, b in
                self?.renderer.write(value, family: family, x: x, y: y, size: size, r, g, b)
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
                "graphics": "METAL NATIVE", "audio": "AVFOUNDATION",
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
