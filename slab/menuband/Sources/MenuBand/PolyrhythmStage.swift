import AppKit

/// The trainer strip, wall-sized. While the circles are out, the whole
/// screen splits into the same vertical lanes as the trackpad — one per
/// rhythm, left to right — and each lane carries a giant twin of its clock.
/// A strike bursts in its lane at a size that reads from across a room. The
/// layer is transparent and click-through, so the desktop keeps working
/// underneath; only the small strip (the helper) stays opaque.
enum PolyrhythmStageLayout {
    static func laneRect(index: Int, count: Int, in bounds: NSRect) -> NSRect {
        let n = CGFloat(max(1, count))
        let width = bounds.width / n
        return NSRect(x: bounds.minX + width * CGFloat(index), y: bounds.minY,
                      width: width, height: bounds.height)
    }

    /// Dial radius: as wide as the lane allows without touching its
    /// neighbor, and never taller than the lane can hold with the tempo
    /// readout under it.
    static func clockRadius(lane: NSRect, bounds: NSRect) -> CGFloat {
        min(lane.width * 0.38, bounds.height * 0.28)
    }

    static func clockCenter(lane: NSRect, bounds: NSRect) -> CGPoint {
        CGPoint(x: lane.midX, y: bounds.midY + bounds.height * 0.06)
    }
}

final class PolyrhythmStageView: NSView {
    var snapshot: PolyrhythmTrainerSnapshot? { didSet { needsDisplay = true } }

    override var isOpaque: Bool { false }

    /// Same rule as the strip: a clean strike rings in the circle's own hue;
    /// only a late one turns orange or red.
    static func tapColor(_ tap: PolyrhythmTapFeedback, lane color: NSColor) -> NSColor {
        tap.accuracy >= 0.7 ? color : (tap.accuracy >= 0.4 ? .systemOrange : .systemRed)
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let snapshot else { return }
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let ink = dark ? NSColor.white : NSColor.black
        let colors = PolyrhythmTrainerView.circleColors(
            accent: KeyboardIconRenderer.accent,
            count: snapshot.rhythms.count, dark: dark
        )
        let count = snapshot.rhythms.count

        for (index, rhythm) in snapshot.rhythms.enumerated() {
            let lane = PolyrhythmStageLayout.laneRect(index: index, count: count, in: bounds)
            let color = colors[index]
            let taps = snapshot.tapFeedback.filter { $0.rhythmIndex == index }
            let freshest = taps.max { $0.opacity < $1.opacity }
            let pulse = CGFloat(freshest?.opacity ?? 0)

            // The lane itself answers the strike: a wash in the tap's color
            // that fades with it, so which band the finger landed in is the
            // first thing anyone sees.
            let washColor = freshest.map { Self.tapColor($0, lane: color) } ?? color
            washColor.withAlphaComponent(0.03 + pulse * pulse * 0.20).setFill()
            lane.fill()
            if index + 1 < count {
                ink.withAlphaComponent(0.10).setStroke()
                let divider = NSBezierPath()
                divider.move(to: CGPoint(x: lane.maxX, y: lane.minY))
                divider.line(to: CGPoint(x: lane.maxX, y: lane.maxY))
                divider.lineWidth = 1
                divider.stroke()
            }

            let radius = PolyrhythmStageLayout.clockRadius(lane: lane, bounds: bounds)
                * (1 + pulse * pulse * 0.06)
            let center = PolyrhythmStageLayout.clockCenter(lane: lane, bounds: bounds)
            drawClock(center: center, radius: radius, rhythm: rhythm,
                      phase: snapshot.phase, needleFlash: snapshot.needleFlash,
                      color: color, ink: ink, dark: dark, pulse: pulse)
            drawBursts(taps, center: center, radius: radius, color: color)
        }

        // The tempo reads from the back of the room and breathes with the
        // primary rhythm, exactly like the strip's small readout.
        let pulse = CGFloat(max(0, min(1, snapshot.bpmPulse)))
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(
                ofSize: max(28, bounds.height * 0.07), weight: .bold),
            .foregroundColor: ink.withAlphaComponent(0.30 + pulse * 0.60)
        ]
        let rate = "\(snapshot.bpm)"
        let size = rate.size(withAttributes: attrs)
        rate.draw(at: CGPoint(x: bounds.midX - size.width / 2,
                              y: bounds.minY + bounds.height * 0.05),
                  withAttributes: attrs)
    }

    private func drawClock(center: CGPoint, radius: CGFloat,
                           rhythm: PolyrhythmRhythmSnapshot, phase: Double,
                           needleFlash: Double, color: NSColor, ink: NSColor,
                           dark: Bool, pulse: CGFloat) {
        // A translucent face keeps the dial legible over whatever desktop
        // is behind it without hiding that desktop.
        let faceRect = NSRect(x: center.x - radius, y: center.y - radius,
                              width: radius * 2, height: radius * 2)
        let face = NSBezierPath(ovalIn: faceRect)
        PolyrhythmTrainerView.faceColor(dark: dark).withAlphaComponent(0.55).setFill()
        face.fill()
        color.withAlphaComponent(0.35 + pulse * 0.55).setStroke()
        face.lineWidth = 3 + pulse * 6
        face.stroke()

        let dotRing = radius * 0.80
        for index in 0..<rhythm.count {
            let angle = CGFloat.pi / 2
                - CGFloat(index) / CGFloat(rhythm.count) * 2 * .pi
            let point = CGPoint(x: center.x + cos(angle) * dotRing,
                                y: center.y + sin(angle) * dotRing)
            let diameter = index == rhythm.step ? radius * 0.17 : radius * 0.09
            (index == rhythm.step ? color
                : color.withAlphaComponent(dark ? 0.30 : 0.24)).setFill()
            NSBezierPath(ovalIn: NSRect(x: point.x - diameter / 2,
                                        y: point.y - diameter / 2,
                                        width: diameter, height: diameter)).fill()
        }

        // The count sits behind the hand, in ink, big enough to be the lane's
        // name from a distance.
        let label = "\(rhythm.count)"
        let labelAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: radius * 0.72, weight: .bold),
            .foregroundColor: ink.withAlphaComponent(0.82)
        ]
        let labelSize = label.size(withAttributes: labelAttrs)
        label.draw(at: CGPoint(x: center.x - labelSize.width / 2,
                               y: center.y - labelSize.height / 2),
                   withAttributes: labelAttrs)

        let angle = CGFloat.pi / 2 - CGFloat(phase) * 2 * .pi
        let tip = CGPoint(x: center.x + cos(angle) * radius * 0.88,
                          y: center.y + sin(angle) * radius * 0.88)
        let hand = NSBezierPath()
        hand.move(to: center); hand.line(to: tip)
        let flash = CGFloat(needleFlash)
        color.withAlphaComponent(0.78 + flash * 0.22).setStroke()
        hand.lineWidth = 3 + flash * 6
        hand.lineCapStyle = .round
        hand.stroke()
        let hub = radius * 0.06
        color.setFill()
        NSBezierPath(ovalIn: NSRect(x: center.x - hub, y: center.y - hub,
                                    width: hub * 2, height: hub * 2)).fill()
    }

    /// Each strike: a ring that leaves the dot ring and grows past the face
    /// while it thins and fades, plus a solid mark where on the wheel the
    /// finger actually landed.
    private func drawBursts(_ taps: [PolyrhythmTapFeedback], center: CGPoint,
                            radius: CGFloat, color laneColor: NSColor) {
        for tap in taps {
            let color = Self.tapColor(tap, lane: laneColor)
            let age = 1 - CGFloat(tap.opacity)
            let ringRadius = radius * (0.80 + age * 0.75)
            let ring = NSBezierPath(ovalIn: NSRect(x: center.x - ringRadius,
                                                   y: center.y - ringRadius,
                                                   width: ringRadius * 2,
                                                   height: ringRadius * 2))
            color.withAlphaComponent(CGFloat(tap.opacity) * 0.9).setStroke()
            ring.lineWidth = max(1.5, 22 * (1 - age))
            ring.stroke()

            let angle = CGFloat.pi / 2 - CGFloat(tap.phase) * 2 * .pi
            let point = CGPoint(x: center.x + cos(angle) * radius * 0.80,
                                y: center.y + sin(angle) * radius * 0.80)
            let diameter = radius * 0.15
            color.withAlphaComponent(CGFloat(tap.opacity)).setFill()
            NSBezierPath(ovalIn: NSRect(x: point.x - diameter / 2,
                                        y: point.y - diameter / 2,
                                        width: diameter, height: diameter)).fill()
        }
    }
}

/// Fullscreen, transparent, click-through, on the display that holds Menu
/// Band's status item. Sits one level under the strip so the helper always
/// stays on top of its own stage.
final class PolyrhythmStageWindow: NSPanel {
    private let stage = PolyrhythmStageView()

    init() {
        super.init(contentRect: NSScreen.main?.frame ?? NSRect(x: 0, y: 0, width: 1440, height: 900),
                   styleMask: [.borderless, .nonactivatingPanel],
                   backing: .buffered, defer: false)
        isOpaque = false
        backgroundColor = .clear
        hasShadow = false
        level = NSWindow.Level(rawValue: NSWindow.Level.screenSaver.rawValue - 1)
        ignoresMouseEvents = true
        hidesOnDeactivate = false
        collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle,
                              .fullScreenAuxiliary]
        stage.autoresizingMask = [.width, .height]
        stage.frame = contentView?.bounds ?? frame
        contentView?.addSubview(stage)
    }

    func show(_ snapshot: PolyrhythmTrainerSnapshot, on screen: NSScreen) {
        if frame != screen.frame { setFrame(screen.frame, display: false) }
        stage.frame = contentView?.bounds ?? NSRect(origin: .zero, size: screen.frame.size)
        stage.snapshot = snapshot
        if !isVisible { orderFrontRegardless() }
    }

    func hide() {
        stage.snapshot = nil
        if isVisible { orderOut(nil) }
    }
}

/// Headless render of the stage over a plain backdrop, for inspection.
///
/// Flags after `--render-polyrhythm-stage`:
///   --pattern 3:2        colon-separated counts (default 3:2)
///   --phase 0.18         cycle phase 0…1
///   --bpm 180
///   --taps 0@0.34,1@0.5  strikes as lane@phase; accuracy is scored on that
///                        lane's grid, opacity from --tap-age (default 0.35)
///   --size 1512x982      logical size (default: a 14" MacBook)
///   --scale 1            pixel scale
///   --light | --dark
///   --out /path.png      (default /tmp/menuband-polyrhythm-stage.png)
enum PolyrhythmStageCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-polyrhythm-stage") else { return false }
        func value(_ flag: String) -> String? {
            guard let index = args.firstIndex(of: flag), index + 1 < args.count else { return nil }
            return args[index + 1]
        }
        let output = value("--out") ?? "/tmp/menuband-polyrhythm-stage.png"
        let scale = max(1, Double(value("--scale") ?? "1") ?? 1)
        let counts = (value("--pattern") ?? "3:2").split(separator: ":")
            .compactMap { Int($0) }.filter { $0 >= 1 }
        guard !counts.isEmpty else {
            FileHandle.standardError.write(Data("stage: --pattern wants counts like 3:2\n".utf8))
            exit(1)
        }
        let rawPhase = Double(value("--phase") ?? "0.18") ?? 0.18
        let phase = rawPhase - floor(rawPhase)
        let bpm = min(300, max(30, Int(value("--bpm") ?? "180") ?? 180))
        let dims = (value("--size") ?? "1512x982").split(separator: "x").compactMap { Double($0) }
        let size = dims.count == 2 ? NSSize(width: dims[0], height: dims[1]) : NSSize(width: 1512, height: 982)
        let tapAge = min(0.59, max(0, Double(value("--tap-age") ?? "0.35") ?? 0.35))
        let taps: [PolyrhythmTapFeedback] = (value("--taps") ?? "").split(separator: ",").compactMap { spec in
            let parts = spec.split(separator: "@")
            guard parts.count == 2, let lane = Int(parts[0]), let tapPhase = Double(parts[1]),
                  counts.indices.contains(lane) else { return nil }
            let scaled = tapPhase * Double(counts[lane])
            let error = abs(scaled - scaled.rounded()) * 2
            return PolyrhythmTapFeedback(phase: tapPhase, rhythmIndex: lane,
                                         accuracy: 1 - min(1, error), opacity: 1 - tapAge / 0.6)
        }

        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        let dark = args.contains("--dark") && !args.contains("--light")
        app.appearance = NSAppearance(named: dark ? .darkAqua : .aqua)

        let cycleDuration = 60.0 / Double(bpm) * Double(counts[0])
        let sinceBeat = counts.map { count -> Double in
            let beats = phase * Double(count)
            return (beats - floor(beats)) / Double(count) * cycleDuration
        }.min() ?? .infinity
        let snapshot = PolyrhythmTrainerSnapshot(
            phase: phase,
            rhythms: counts.map { PolyrhythmRhythmSnapshot(count: $0, step: Int(floor(phase * Double($0))) % $0) },
            label: counts.map(String.init).joined(separator: ":"),
            bpm: bpm,
            bpmPulse: PolyrhythmTrainerClock.bpmPulse(phase: phase, primaryCount: counts[0], bpm: bpm),
            needleFlash: max(0, 1 - sinceBeat / 0.12),
            tapFeedback: taps
        )

        // A flat desktop-like backdrop stands in for whatever the layer
        // floats over; the real window is fully transparent.
        let root = NSView(frame: NSRect(origin: .zero, size: size))
        root.appearance = app.appearance
        root.wantsLayer = true
        root.layer?.backgroundColor = (dark
            ? NSColor(srgbRed: 0.13, green: 0.14, blue: 0.17, alpha: 1)
            : NSColor(srgbRed: 0.86, green: 0.88, blue: 0.91, alpha: 1)).cgColor
        let stage = PolyrhythmStageView(frame: root.bounds)
        stage.appearance = app.appearance
        stage.snapshot = snapshot
        root.addSubview(stage)
        root.displayIfNeeded()

        let pixelWidth = Int((size.width * scale).rounded())
        let pixelHeight = Int((size.height * scale).rounded())
        guard let bitmap = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: pixelWidth, pixelsHigh: pixelHeight,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0
        ) else { return true }
        bitmap.size = size
        root.cacheDisplay(in: root.bounds, to: bitmap)
        guard let png = bitmap.representation(using: .png, properties: [:]) else { return true }
        do { try png.write(to: URL(fileURLWithPath: output)) } catch {
            FileHandle.standardError.write(Data("stage write failed: \(error)\n".utf8))
            exit(1)
        }
        print("polyrhythm stage \(pixelWidth)x\(pixelHeight) → \(output)")
        return true
    }
}
