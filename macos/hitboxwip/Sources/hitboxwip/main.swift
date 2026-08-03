import AppKit
import AudioToolbox
import AVFoundation
import CLibUSB
import GameController

private let vendorID: UInt16 = 0x2dc8
private let productID: UInt16 = 0x202c

private enum ControlShape {
    case circle
    case pill(width: CGFloat, height: CGFloat, rotation: CGFloat)
    case kidney(width: CGFloat, height: CGFloat, rotation: CGFloat, p2Contour: Bool)
    case roundedSquare(side: CGFloat)
}

private struct Control {
    let label: String
    let x: CGFloat
    let y: CGFloat
    let radius: CGFloat
    let shape: ControlShape

    init(label: String, x: CGFloat, y: CGFloat, radius: CGFloat,
         shape: ControlShape = .circle) {
        self.label = label; self.x = x; self.y = y
        self.radius = radius; self.shape = shape
    }
}

private final class ControllerView: NSView {
    var status = "Looking for 8BitDo Arcade Controller for Xbox…" { didSet { needsDisplay = true } }
    var report = "" { didSet { needsDisplay = true } }
    var latency = "Live input timing: waiting for 0x20 input reports…" { didSet { needsDisplay = true } }
    var active = Set<String>() { didSet { needsDisplay = true } }

    private lazy var referenceImage: NSImage? = {
        guard let url = Bundle.module.url(forResource: "controller-reference", withExtension: "png") else { return nil }
        return NSImage(contentsOf: url)
    }()

    private let jamLabels: [String: String] = [
        "←": "G3", "↓": "A3", "→": "D4", "↑": "C4",
        "LSB": "E4", "RSB": "G4",
        "X": "LIGHT SNARE", "Y": "MED SNARE", "RB": "HEAVY SNARE",
        "A": "LIGHT KICK", "B": "MED KICK", "RT": "HEAVY KICK",
        "LB": "CLOSED HAT", "LT": "EDGE CLICK",
        "P1": "A4", "P2": "C5",
    ]

    private let controls: [Control] = [
        .init(label: "←", x: 0.228, y: 0.356, radius: 38),
        .init(label: "↓", x: 0.342, y: 0.356, radius: 38),
        .init(label: "→", x: 0.439, y: 0.445, radius: 38),
        .init(label: "↑", x: 0.496, y: 0.723, radius: 43),
        .init(label: "X", x: 0.542, y: 0.368, radius: 38),
        .init(label: "Y", x: 0.639, y: 0.281, radius: 38),
        .init(label: "RB", x: 0.754, y: 0.281, radius: 38),
        .init(label: "LB", x: 0.862, y: 0.281, radius: 38),
        .init(label: "A", x: 0.533, y: 0.537, radius: 38),
        .init(label: "B", x: 0.639, y: 0.455, radius: 38),
        .init(label: "RT", x: 0.753, y: 0.455, radius: 38),
        .init(label: "LT", x: 0.861, y: 0.455, radius: 38),
        .init(label: "P1", x: 0.365, y: 0.230, radius: 24, shape: .kidney(width: 100, height: 60, rotation: 0, p2Contour: false)),
        .init(label: "LSB", x: 0.523, y: 0.237, radius: 24, shape: .kidney(width: 104, height: 62, rotation: -5, p2Contour: false)),
        .init(label: "P2", x: 0.412, y: 0.673, radius: 24, shape: .kidney(width: 65, height: 95, rotation: 0, p2Contour: true)),
        .init(label: "RSB", x: 0.632, y: 0.610, radius: 24, shape: .kidney(width: 104, height: 63, rotation: -23, p2Contour: false)),
        .init(label: "★", x: 0.197, y: 0.096, radius: 20),
        .init(label: "VIEW", x: 0.799, y: 0.096, radius: 15, shape: .pill(width: 50, height: 25, rotation: 0)),
        .init(label: "SHARE", x: 0.864, y: 0.096, radius: 15, shape: .pill(width: 50, height: 25, rotation: 0)),
        .init(label: "MENU", x: 0.928, y: 0.096, radius: 15, shape: .pill(width: 50, height: 25, rotation: 0)),
    ]

    override var isFlipped: Bool { true }

    override func draw(_ dirtyRect: NSRect) {
        // Fully windowless: clear the backing on every redraw and float only
        // vectors, labels, and live highlights over the desktop.
        NSColor.clear.setFill(); dirtyRect.fill(using: .copy)
        // Device bounds measured directly in official 03-l.png:
        // x=410…1518, y=263…1000 (1108×737 px). Every control coordinate
        // above is normalized inside this exact reference rectangle.
        let productAspect: CGFloat = 1108 / 737
        let available = bounds.insetBy(dx: 35, dy: 86)
        let panelWidth = min(available.width, available.height * productAspect)
        let panelHeight = panelWidth / productAspect
        let panel = CGRect(x: bounds.midX - panelWidth / 2, y: available.minY,
                           width: panelWidth, height: panelHeight)
        let controlScale = min(1.65, max(0.8, panel.width / 900))

        for control in controls {
            let radius = control.radius * controlScale
            let center = CGPoint(x: panel.minX + panel.width * control.x,
                                 y: panel.minY + panel.height * control.y)
            let circle: NSBezierPath
            switch control.shape {
            case .circle:
                circle = NSBezierPath(ovalIn: CGRect(x: center.x - radius, y: center.y - radius,
                                                     width: radius * 2, height: radius * 2))
            case let .roundedSquare(side):
                let scaledSide = side * controlScale
                circle = NSBezierPath(roundedRect: CGRect(x: center.x - scaledSide / 2, y: center.y - scaledSide / 2,
                                                          width: scaledSide, height: scaledSide), xRadius: 6 * controlScale, yRadius: 6 * controlScale)
            case let .pill(width, height, rotation):
                let scaledWidth = width * controlScale, scaledHeight = height * controlScale
                circle = NSBezierPath(roundedRect: CGRect(x: -scaledWidth / 2, y: -scaledHeight / 2,
                                                          width: scaledWidth, height: scaledHeight),
                                      xRadius: scaledHeight / 2, yRadius: scaledHeight / 2)
                var transform = AffineTransform(translationByX: center.x, byY: center.y)
                transform.rotate(byDegrees: rotation)
                circle.transform(using: transform)
            case let .kidney(width, height, rotation, p2Contour):
                let w = width * controlScale, h = height * controlScale
                // Pixel-traced from the two green cap silhouettes in the
                // official 1920×1080 product PNG, simplified at 2.2 px.
                let p1: [CGPoint] = [
                    .init(x:-0.500,y:-0.027),.init(x:-0.459,y:-0.297),.init(x:-0.378,y:-0.419),
                    .init(x:-0.224,y:-0.500),.init(x:-0.012,y:-0.500),.init(x:0.134,y:-0.446),
                    .init(x:0.346,y:-0.270),.init(x:0.467,y:-0.054),.init(x:0.500,y:0.162),
                    .init(x:0.484,y:0.284),.init(x:0.435,y:0.405),.init(x:0.289,y:0.500),
                    .init(x:-0.045,y:0.257),.init(x:-0.370,y:0.230),.init(x:-0.467,y:0.108)
                ]
                let p2: [CGPoint] = [
                    .init(x:-0.500,y:0.237),.init(x:-0.451,y:-0.059),.init(x:-0.244,y:-0.339),
                    .init(x:-0.110,y:-0.432),.init(x:0.073,y:-0.500),.init(x:0.256,y:-0.500),
                    .init(x:0.390,y:-0.458),.init(x:0.488,y:-0.364),.init(x:0.488,y:-0.237),
                    .init(x:0.232,y:0.025),.init(x:0.171,y:0.297),.init(x:0.085,y:0.432),
                    .init(x:-0.049,y:0.492),.init(x:-0.244,y:0.492),.init(x:-0.415,y:0.407)
                ]
                let samples = (p2Contour ? p2 : p1).map { CGPoint(x: $0.x * w, y: $0.y * h) }
                circle = NSBezierPath()
                circle.move(to: samples[0])
                for index in samples.indices {
                    let p0 = samples[(index - 1 + samples.count) % samples.count]
                    let p1 = samples[index]
                    let p2 = samples[(index + 1) % samples.count]
                    let p3 = samples[(index + 2) % samples.count]
                    circle.curve(to: p2,
                                 controlPoint1: CGPoint(x: p1.x + (p2.x - p0.x) / 6,
                                                        y: p1.y + (p2.y - p0.y) / 6),
                                 controlPoint2: CGPoint(x: p2.x - (p3.x - p1.x) / 6,
                                                        y: p2.y - (p3.y - p1.y) / 6))
                }
                circle.close()
                var transform = AffineTransform(translationByX: center.x, byY: center.y)
                transform.rotate(byDegrees: rotation)
                circle.transform(using: transform)
            }
            let baseColor = color(for: control.label)
            let isActive = active.contains(control.label)
            let surfaceColor = isActive
                ? baseColor.blended(withFraction: 0.42, of: .white) ?? .white
                : baseColor

            // A restrained cap surface: soft elevation shadow, colored body,
            // top-edge light and lower falloff. The board itself stays clear.
            NSGraphicsContext.saveGraphicsState()
            let shadow = NSShadow()
            shadow.shadowColor = NSColor.black.withAlphaComponent(0.34)
            shadow.shadowBlurRadius = 5 * controlScale
            shadow.shadowOffset = NSSize(width: 0, height: 3 * controlScale)
            shadow.set()
            surfaceColor.setFill()
            circle.fill()
            NSGraphicsContext.restoreGraphicsState()

            NSGraphicsContext.saveGraphicsState()
            circle.addClip()
            NSGradient(colors: [
                NSColor.white.withAlphaComponent(isActive ? 0.34 : 0.22),
                NSColor.clear,
                NSColor.black.withAlphaComponent(isActive ? 0.08 : 0.25),
            ])?.draw(in: circle, angle: 90)
            NSGraphicsContext.restoreGraphicsState()

            (isActive ? NSColor.white : NSColor.white.withAlphaComponent(0.42)).setStroke()
            circle.lineWidth = isActive ? 3 : 1.35
            circle.stroke()
            let size: CGFloat = (control.label.count > 1 ? 14 : 19) * controlScale
            if control.label == "XBOX",
               let logo = NSImage(systemSymbolName: "xbox.logo", accessibilityDescription: "Xbox")?
                    .withSymbolConfiguration(.init(pointSize: 25 * controlScale, weight: .bold)) {
                let side = 31 * controlScale
                logo.draw(in: CGRect(x: center.x - side / 2, y: center.y - side / 2,
                                     width: side, height: side), from: .zero,
                          operation: .sourceOver, fraction: 1,
                          respectFlipped: true, hints: nil)
            } else {
                drawCentered(control.label, center: center, size: size)
            }
            if isActive, let mapping = jamLabels[control.label] {
                drawCentered(mapping,
                             center: CGPoint(x: center.x, y: center.y + radius + 17 * controlScale),
                             size: 14 * controlScale, color: .white)
            }
        }
    }

    private func color(for label: String) -> NSColor {
        switch label {
        case "←", "↓", "→", "↑", "LSB", "RSB":
            return NSColor(calibratedRed: 0.20, green: 0.62, blue: 1.0, alpha: 0.88)
        case "X": return NSColor(calibratedRed: 1.00, green: 0.56, blue: 0.52, alpha: 0.92)
        case "Y": return NSColor(calibratedRed: 0.92, green: 0.34, blue: 0.30, alpha: 0.92)
        case "RB": return NSColor(calibratedRed: 0.66, green: 0.12, blue: 0.14, alpha: 0.94)
        case "A": return NSColor(calibratedRed: 0.46, green: 0.72, blue: 1.00, alpha: 0.92)
        case "B": return NSColor(calibratedRed: 0.20, green: 0.50, blue: 0.92, alpha: 0.92)
        case "RT": return NSColor(calibratedRed: 0.08, green: 0.25, blue: 0.62, alpha: 0.94)
        case "LB": return NSColor(calibratedRed: 1.00, green: 0.80, blue: 0.22, alpha: 0.92)
        case "LT": return NSColor(calibratedRed: 0.96, green: 0.90, blue: 0.68, alpha: 0.96)
        case "P1", "P2":
            return NSColor(calibratedRed: 0.78, green: 0.40, blue: 1.0, alpha: 0.78)
        case "XBOX":
            return NSColor(calibratedWhite: 0.92, alpha: 0.96)
        case "★", "PROF", "RGB":
            return NSColor(calibratedRed: 1.0, green: 0.40, blue: 0.68, alpha: 0.78)
        default:
            return NSColor(calibratedWhite: 0.38, alpha: 0.78)
        }
    }

    private func drawText(_ text: String, at point: CGPoint, size: CGFloat, color: NSColor) {
        text.draw(at: point, withAttributes: [.font: NSFont.monospacedSystemFont(ofSize: size, weight: .medium), .foregroundColor: color])
    }

    private func drawCentered(_ text: String, center: CGPoint, size: CGFloat, color: NSColor = .white) {
        let attrs: [NSAttributedString.Key: Any] = [.font: NSFont.systemFont(ofSize: size, weight: .bold), .foregroundColor: color]
        let box = text.size(withAttributes: attrs)
        text.draw(at: CGPoint(x: center.x - box.width / 2, y: center.y - box.height / 2), withAttributes: attrs)
    }
}

private final class JamAudio {
    private let engine = AVAudioEngine()
    private let instrument: AVAudioUnitMIDIInstrument
    private var heldChords: [String: [UInt8]] = [:]
    private var previous = Set<String>()

    private let chords: [String: [UInt8]] = [
        "←": [55], "↓": [57], "→": [62], "↑": [60],
        "LSB": [64], "RSB": [67],       // C major pentatonic, Up = tonic
        "P1": [69], "P2": [72],
    ]
    private let melodicVelocities: [String: UInt8] = [
        "←": 78, "↓": 86, "→": 94, "↑": 104, "LSB": 108, "RSB": 116,
        "P1": 112, "P2": 120,
    ]
    private let drums: [String: UInt8] = [
        "X": 38, "Y": 38, "RB": 40,
        "A": 35, "B": 36, "RT": 41,
        "LB": 42, "LT": 37,
    ]
    private let drumVelocities: [String: UInt8] = [
        "X": 72, "Y": 98, "RB": 124,
        "A": 76, "B": 102, "RT": 127,
        "LB": 88, "LT": 96,
    ]
    private let liftNotes: [String: (note: UInt8, velocity: UInt8)] = [
        "X": (37, 34), "Y": (37, 40), "RB": (37, 48),
        "A": (37, 28), "B": (37, 34), "RT": (37, 42),
        "LB": (44, 50), "LT": (37, 44),
    ]

    init() {
        let description = AudioComponentDescription(
            componentType: kAudioUnitType_MusicDevice,
            componentSubType: kAudioUnitSubType_DLSSynth,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0, componentFlagsMask: 0)
        instrument = AVAudioUnitMIDIInstrument(audioComponentDescription: description)
        engine.attach(instrument)
        engine.connect(instrument, to: engine.mainMixerNode, format: nil)
        engine.mainMixerNode.outputVolume = 0.72
        do {
            try engine.start()
            // Apple DLS starts GM channels with an audible ambience send.
            // Zero both effects sends on every channel for a dry jamboard.
            for channel in UInt8(0)..<16 {
                instrument.sendMIDIEvent(0xB0 | channel, data1: 91, data2: 0) // reverb
                instrument.sendMIDIEvent(0xB0 | channel, data1: 93, data2: 0) // chorus
            }
            // MenuBand's displayed instrument 79 = GM Whistle. DLS program
            // numbers are zero-based, hence program 78 on melodic channel 1.
            instrument.sendMIDIEvent(0xC0, data1: 78, data2: 0)
        }
        catch { fputs("[audio] \(error)\n", stderr) }
    }

    func update(_ controls: Set<String>) {
        let pressed = controls.subtracting(previous)
        let released = previous.subtracting(controls)
        previous = controls

        for label in pressed {
            if let notes = chords[label] {
                heldChords[label] = notes
                for note in notes {
                    instrument.sendMIDIEvent(0x90, data1: note,
                                             data2: melodicVelocities[label] ?? 92)
                }
            }
            if let note = drums[label] {
                instrument.sendMIDIEvent(0x99, data1: note, data2: drumVelocities[label] ?? 108)
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.12) { [weak instrument] in
                    instrument?.sendMIDIEvent(0x89, data1: note, data2: 0)
                }
            }
        }
        for label in released {
            if let notes = heldChords.removeValue(forKey: label) {
                for note in notes { instrument.sendMIDIEvent(0x80, data1: note, data2: 0) }
            }
            if let lift = liftNotes[label] {
                instrument.sendMIDIEvent(0x99, data1: lift.note, data2: lift.velocity)
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.055) { [weak instrument] in
                    instrument?.sendMIDIEvent(0x89, data1: lift.note, data2: 0)
                }
            }
        }
    }
}

private final class USBReader {
    private weak var view: ControllerView?
    private let audio = JamAudio()
    private var context: OpaquePointer?
    private var handle: OpaquePointer?
    private var interfaceNumber: Int32 = -1
    private var endpoint: UInt8 = 0
    private var outputEndpoint: UInt8 = 0
    private var discoveryDetail = ""
    private var lastInputTime: UInt64?
    private var inputIntervals = [Double]()
    private var currentControls = Set<String>()
    private var timer: DispatchSourceTimer?

    init(view: ControllerView) { self.view = view }

    func start() {
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in self?.connectAndRead() }
    }

    private func connectAndRead() {
        guard libusb_init(&context) == 0 else { update("Could not initialize libusb") ; return }
        guard let context, let found = libusb_open_device_with_vid_pid(context, vendorID, productID) else {
            update("Not connected (USB 2dc8:202c)"); return
        }
        handle = found
        guard discoverInputEndpoint(handle: found) else {
            update("Connected, but no input endpoint was found\(discoveryDetail)"); return
        }
        let claim = libusb_claim_interface(found, interfaceNumber)
        guard claim == 0 else {
            update("Connected, but macOS owns interface \(interfaceNumber) (libusb error \(claim))")
            return
        }
        guard outputEndpoint != 0 else { update("Connected, but no output endpoint was found"); return }
        var startInput: [UInt8] = [0x05, 0x20, 0x00, 0x01, 0x00]
        var sent: Int32 = 0
        let startResult = libusb_interrupt_transfer(found, outputEndpoint, &startInput,
                                                    Int32(startInput.count), &sent, 1_000)
        guard startResult == 0 else {
            update("Connected, but GIP start-input failed (libusb error \(startResult))")
            return
        }
        update(String(format: "Connected • raw USB interface %d • endpoint 0x%02x", interfaceNumber, endpoint))
        readLoop(found)
    }

    private func discoverInputEndpoint(handle: OpaquePointer) -> Bool {
        guard let device = libusb_get_device(handle) else { return false }
        var config: UnsafeMutablePointer<libusb_config_descriptor>?
        var result = libusb_get_active_config_descriptor(device, &config)
        if result != 0 || config == nil {
            result = libusb_get_config_descriptor(device, 0, &config)
        }
        guard result == 0, let config else {
            discoveryDetail = " (descriptor error \(result))"
            return false
        }
        defer { libusb_free_config_descriptor(config) }
        discoveryDetail = " (\(config.pointee.bNumInterfaces) interfaces)"
        for i in 0..<Int(config.pointee.bNumInterfaces) {
            let interface = config.pointee.interface[i]
            for a in 0..<Int(interface.num_altsetting) {
                let alt = interface.altsetting[a]
                var candidateInput: UInt8 = 0
                var candidateOutput: UInt8 = 0
                for e in 0..<Int(alt.bNumEndpoints) {
                    let ep = alt.endpoint[e]
                    fputs(String(format: "[usb] interface %d alt %d endpoint 0x%02x attributes 0x%02x interval %d\n",
                                 alt.bInterfaceNumber, alt.bAlternateSetting, ep.bEndpointAddress,
                                 ep.bmAttributes, ep.bInterval), stderr)
                    guard ep.bmAttributes & 0x03 == UInt8(LIBUSB_TRANSFER_TYPE_INTERRUPT.rawValue) else { continue }
                    if ep.bEndpointAddress & UInt8(LIBUSB_ENDPOINT_IN.rawValue) != 0 {
                        candidateInput = ep.bEndpointAddress
                    } else {
                        candidateOutput = ep.bEndpointAddress
                    }
                }
                if candidateInput != 0, candidateOutput != 0 {
                    interfaceNumber = Int32(alt.bInterfaceNumber)
                    endpoint = candidateInput
                    outputEndpoint = candidateOutput
                    return true
                }
            }
        }
        return false
    }

    private func readLoop(_ handle: OpaquePointer) {
        var bytes = [UInt8](repeating: 0, count: 64)
        while true {
            var transferred: Int32 = 0
            let result = libusb_interrupt_transfer(handle, endpoint, &bytes, Int32(bytes.count), &transferred, 250)
            if result == 0, transferred > 0 {
                let packet = Array(bytes.prefix(Int(transferred)))
                fputs("[gip] \(packet.map { String(format: "%02x", $0) }.joined(separator: " "))\n", stderr)
                let latencyText = packet.first == 0x20 ? recordInputTiming() : nil
                DispatchQueue.main.async { [weak self] in
                    guard let self else { return }
                    self.view?.report = packet.map { String(format: "%02x", $0) }.joined(separator: " ")
                    if packet.first == 0x20 {
                        self.currentControls = Self.decodeXboxReport(packet)
                        self.view?.active = self.currentControls
                        self.audio.update(self.currentControls)
                    } else if packet.count >= 5, packet.first == 0x07 {
                        if packet[4] != 0 { self.currentControls.insert("XBOX") }
                        else { self.currentControls.remove("XBOX") }
                        self.view?.active = self.currentControls
                    }
                    if let latencyText { self.view?.latency = latencyText }
                }
            } else if result != LIBUSB_ERROR_TIMEOUT.rawValue {
                update("USB read stopped (libusb error \(result))")
                return
            }
        }
    }

    private func recordInputTiming() -> String? {
        let now = DispatchTime.now().uptimeNanoseconds
        defer { lastInputTime = now }
        guard let previous = lastInputTime else { return nil }
        let milliseconds = Double(now - previous) / 1_000_000
        // Ignore pauses caused by disconnect/reconnect when calculating the
        // rolling transport statistics.
        guard milliseconds < 1_000 else { inputIntervals.removeAll(); return nil }
        inputIntervals.append(milliseconds)
        if inputIntervals.count > 512 { inputIntervals.removeFirst(inputIntervals.count - 512) }
        let sorted = inputIntervals.sorted()
        let average = inputIntervals.reduce(0, +) / Double(inputIntervals.count)
        let p95 = sorted[min(sorted.count - 1, Int(Double(sorted.count - 1) * 0.95))]
        let worst = sorted.last ?? milliseconds
        let hz = average > 0 ? 1_000 / average : 0
        return String(format: "Live event timing  spacing %.2f ms  avg %.2f ms  p95 %.2f ms  max %.2f ms  (%.0f events/s, n=%d)",
                      milliseconds, average, p95, worst, hz, inputIntervals.count)
    }

    // Xbox GIP report layouts can vary by firmware. This handles the standard
    // 0x20 gamepad payload and leaves every packet visible for easy refinement.
    private static func decodeXboxReport(_ b: [UInt8]) -> Set<String> {
        // GIP packets begin command/options/sequence/payload-length. Command
        // 0x20 is controller input; 0x02, for example, is device announce.
        if b.count >= 5, b[0] == 0x07, b[4] != 0 { return ["XBOX"] }
        guard b.count >= 10, b[0] == 0x20 else { return [] }
        let offset = 4
        let lo = b[offset], hi = b[offset + 1]
        var on = Set<String>()
        let low: [(UInt8, String)] = [(0x04,"MENU"),(0x08,"VIEW"),(0x10,"A"),(0x20,"B"),(0x40,"X"),(0x80,"Y")]
        let high: [(UInt8, String)] = [(0x01,"↑"),(0x02,"↓"),(0x04,"←"),(0x08,"→"),(0x10,"LB"),(0x20,"RB"),(0x40,"LSB"),(0x80,"RSB")]
        for (mask, name) in low where lo & mask != 0 { on.insert(name) }
        for (mask, name) in high where hi & mask != 0 { on.insert(name) }
        if b.count > offset + 5 {
            let leftTrigger = UInt16(b[offset + 2]) | UInt16(b[offset + 3]) << 8
            let rightTrigger = UInt16(b[offset + 4]) | UInt16(b[offset + 5]) << 8
            if leftTrigger > 0 { on.insert("LT") }
            if rightTrigger > 0 { on.insert("RT") }
        }
        // This controller puts Share in the extended GIP payload rather than
        // alongside View/Menu in the standard button bytes.
        if b.count > 18, b[18] & 0x01 != 0 { on.insert("SHARE") }
        // This physical unit is presently programmed P1→View and P2→Menu. Xbox
        // GIP carries only the mirrored target bits, so both caps must light:
        // there is no packet-level information that can distinguish them.
        if on.contains("VIEW") { on.insert("P1") }
        if on.contains("MENU") { on.insert("P2") }
        return on
    }

    private func update(_ status: String) {
        DispatchQueue.main.async { [weak self] in self?.view?.status = status }
    }
}

private final class OverlayWindow: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
}

private final class AppDelegate: NSObject, NSApplicationDelegate {
    private var window: NSWindow!
    private var reader: USBReader!
    private var keyMonitor: Any?

    func applicationDidFinishLaunching(_ notification: Notification) {
        guard let screen = NSScreen.main ?? NSScreen.screens.first else { return }
        let overlaySize = NSSize(width: min(1380, screen.visibleFrame.width * 0.94),
                                 height: min(900, screen.visibleFrame.height * 0.92))
        let overlayFrame = NSRect(
            x: screen.visibleFrame.midX - overlaySize.width / 2,
            y: screen.visibleFrame.midY - overlaySize.height / 2,
            width: overlaySize.width, height: overlaySize.height)
        let view = ControllerView(frame: NSRect(origin: .zero, size: overlaySize))
        view.autoresizingMask = [.width, .height]
        view.wantsLayer = true
        view.layer?.isOpaque = false
        view.layer?.backgroundColor = NSColor.clear.cgColor
        window = OverlayWindow(contentRect: overlayFrame, styleMask: [.borderless], backing: .buffered, defer: false)
        window.level = .floating
        window.backgroundColor = .clear
        window.isOpaque = false
        window.hasShadow = false
        window.animationBehavior = .none
        window.hidesOnDeactivate = false
        window.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary, .ignoresCycle]
        window.contentView = view
        window.makeKeyAndOrderFront(nil)
        keyMonitor = NSEvent.addLocalMonitorForEvents(matching: .keyDown) { event in
            if event.keyCode == 53 || event.charactersIgnoringModifiers == "q" {
                NSApp.terminate(nil); return nil
            }
            return event
        }
        reader = USBReader(view: view); reader.start()
        NSApp.activate(ignoringOtherApps: true)
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

let app = NSApplication.shared
private let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory)
app.run()
