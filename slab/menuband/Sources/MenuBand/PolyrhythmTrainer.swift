import AppKit

/// One shared cycle rendered as N side-by-side clocks — one circle per
/// rhythm, so each finger can individuate a rhythm on the trackpad (left
/// band strikes the left circle, and so on). Keeping the clock pure makes
/// its boundary behavior testable without audio or AppKit timers.
struct PolyrhythmTrainerClock {
    struct Pattern: Equatable {
        let counts: [Int]
        var label: String { counts.map(String.init).joined(separator: ":") }
        init(_ counts: [Int]) { self.counts = counts }
    }

    static let patterns = [
        Pattern([3, 2]),
        Pattern([4, 3]),
        Pattern([5, 4]),
        Pattern([3, 4, 5]),
    ]

    private(set) var isActive = false
    private(set) var startedAt: CFTimeInterval = 0
    private(set) var pattern: Pattern = PolyrhythmTrainerClock.patterns[0]
    private(set) var bpm = 75
    private var lastOrdinals: [Int] = []
    private var lastPulseAt: CFTimeInterval = -.infinity
    private struct RecordedTap {
        let at: CFTimeInterval
        let phase: Double
        let rhythmIndex: Int
        let accuracy: Double
    }
    private var recordedTaps: [RecordedTap] = []
    var cycleDuration: TimeInterval {
        60.0 / Double(bpm) * Double(pattern.counts[0])
    }

    mutating func start(at now: CFTimeInterval) {
        isActive = true
        startedAt = now
        lastOrdinals = Array(repeating: -1, count: pattern.counts.count)
        lastPulseAt = -.infinity
        recordedTaps.removeAll(keepingCapacity: true)
    }

    mutating func stop() {
        isActive = false
        lastOrdinals = []
        lastPulseAt = -.infinity
        recordedTaps.removeAll(keepingCapacity: true)
    }

    /// `/` walks a finite loop so the feature remains easy to dismiss:
    /// off → 3:2 → 4:3 → 5:4 → 3:4:5 → off. A typed custom pattern sits
    /// outside the walk, so from one the next `/` goes straight to off —
    /// dismissal stays one key away no matter what was entered.
    mutating func cyclePattern(at now: CFTimeInterval) {
        if !isActive {
            pattern = Self.patterns[0]
            start(at: now)
        } else if let index = Self.patterns.firstIndex(of: pattern),
                  index + 1 < Self.patterns.count {
            pattern = Self.patterns[index + 1]
            start(at: now)
        } else {
            stop()
        }
    }

    /// Typed entry — digits with `/` separators while the circles are out —
    /// replaces the preset walk with an exact pattern like 7:4 or 2:3:4.
    /// Divisions clamp to 1…16 and at most five circles (one per finger /
    /// hue). The shared wheel keeps its phase so retyping mid-play refines
    /// the grid without restarting the cycle.
    mutating func setPattern(_ counts: [Int], at now: CFTimeInterval) {
        let cleaned = counts.filter { $0 >= 1 }.prefix(5).map { min(16, $0) }
        guard !cleaned.isEmpty else { return }
        guard isActive else {
            pattern = Pattern(Array(cleaned))
            start(at: now)
            return
        }
        let oldPhase = (max(0, now - startedAt) / cycleDuration)
            .truncatingRemainder(dividingBy: 1)
        pattern = Pattern(Array(cleaned))
        startedAt = now - oldPhase * cycleDuration
        lastOrdinals = Array(repeating: -1, count: pattern.counts.count)
        recordedTaps.removeAll(keepingCapacity: true)
    }

    mutating func changeRate(by delta: Int, at now: CFTimeInterval) {
        guard isActive, delta != 0 else { return }
        let oldDuration = cycleDuration
        let oldPhase = (max(0, now - startedAt) / oldDuration)
            .truncatingRemainder(dividingBy: 1)
        bpm = min(300, max(30, bpm + delta))
        startedAt = now - oldPhase * cycleDuration
        lastOrdinals = Array(repeating: -1, count: pattern.counts.count)
    }

    /// The trackpad splits into equal vertical bands, one per circle,
    /// mirroring the on-screen left-to-right layout: for 3:2 the left half
    /// strikes the 3-circle and the right half the 2-circle.
    static func rhythmIndex(forNormalizedX x: Double, rhythmCount: Int) -> Int {
        guard rhythmCount > 0 else { return 0 }
        let clamped = min(max(x, 0), 1)
        return min(rhythmCount - 1, Int(clamped * Double(rhythmCount)))
    }

    private func phase(at now: CFTimeInterval) -> Double {
        let cycle = max(0, now - startedAt) / cycleDuration
        return cycle - floor(cycle)
    }

    /// 0 on a grid line, 1 exactly between two grid lines of that rhythm.
    private static func gridError(phase: Double, count: Int) -> Double {
        abs(phase * Double(count) - (phase * Double(count)).rounded()) * 2
    }

    /// Route a strike to the circle under the finger's horizontal band and
    /// score it against that circle's own grid ONLY — a tap on the 3-circle
    /// is never excused by landing near a 2-circle beat.
    mutating func registerTap(at now: CFTimeInterval, normalizedX: Double) {
        record(at: now, rhythmIndex: Self.rhythmIndex(
            forNormalizedX: normalizedX, rhythmCount: pattern.counts.count
        ))
    }

    /// Positionless fallback (no touch coordinate at the call site): place
    /// the strike on the rhythm whose grid it most closely fits, ties going
    /// to the leftmost circle.
    mutating func registerTap(at now: CFTimeInterval) {
        guard isActive else { return }
        let phase = phase(at: now)
        let index = pattern.counts.indices.min(by: {
            Self.gridError(phase: phase, count: pattern.counts[$0])
                < Self.gridError(phase: phase, count: pattern.counts[$1])
        }) ?? 0
        record(at: now, rhythmIndex: index)
    }

    /// Accuracy is normalized within half of the chosen rhythm's beat
    /// interval.
    private mutating func record(at now: CFTimeInterval, rhythmIndex: Int) {
        guard isActive, pattern.counts.indices.contains(rhythmIndex) else {
            return
        }
        let phase = phase(at: now)
        let error = Self.gridError(phase: phase, count: pattern.counts[rhythmIndex])
        recordedTaps.append(RecordedTap(
            at: now, phase: phase, rhythmIndex: rhythmIndex,
            accuracy: 1 - min(1, error)
        ))
        recordedTaps = recordedTaps.filter { now - $0.at < 0.65 }
        if recordedTaps.count > 12 { recordedTaps.removeFirst(recordedTaps.count - 12) }
    }

    /// Indices of the rhythms whose beat boundary passed since the last
    /// tick, left to right. Each index maps to its own click voice so the
    /// circles stay audibly distinct.
    mutating func tick(at now: CFTimeInterval) -> [Int] {
        guard isActive else { return [] }
        if lastOrdinals.count != pattern.counts.count {
            lastOrdinals = Array(repeating: -1, count: pattern.counts.count)
        }
        let elapsed = max(0, now - startedAt)
        var fired: [Int] = []
        for (index, count) in pattern.counts.enumerated() {
            let ordinal = Int(floor(elapsed / cycleDuration * Double(count)))
            if ordinal != lastOrdinals[index] {
                fired.append(index)
                lastOrdinals[index] = ordinal
            }
        }
        if !fired.isEmpty { lastPulseAt = now }
        return fired
    }

    /// The bpm readout flashes on the primary rhythm's beats only — the
    /// first circle IS the tempo (cycleDuration divides by its count), so
    /// its grid is the one the number should breathe with. Derived from
    /// phase, not wall-clock state, so the headless renderer agrees.
    static func bpmPulse(phase: Double, primaryCount: Int, bpm: Int) -> Double {
        let beats = phase * Double(max(1, primaryCount))
        let sinceBeat = (beats - floor(beats)) * 60.0 / Double(bpm)
        return max(0, 1 - sinceBeat / 0.16)
    }

    func snapshot(at now: CFTimeInterval) -> PolyrhythmTrainerSnapshot? {
        guard isActive else { return nil }
        let elapsed = max(0, now - startedAt)
        let cycle = elapsed / cycleDuration
        let phase = cycle - floor(cycle)
        return PolyrhythmTrainerSnapshot(
            phase: phase,
            rhythms: pattern.counts.map {
                PolyrhythmRhythmSnapshot(
                    count: $0,
                    step: Int(floor(cycle * Double($0))) % $0
                )
            },
            label: pattern.label,
            bpm: bpm,
            bpmPulse: Self.bpmPulse(phase: phase,
                                    primaryCount: pattern.counts[0],
                                    bpm: bpm),
            needleFlash: max(0, 1 - (now - lastPulseAt) / 0.12),
            tapFeedback: recordedTaps.compactMap { tap in
                let age = now - tap.at
                guard age >= 0, age < 0.6 else { return nil }
                return PolyrhythmTapFeedback(
                    phase: tap.phase,
                    rhythmIndex: tap.rhythmIndex,
                    accuracy: tap.accuracy,
                    opacity: 1 - age / 0.6
                )
            }
        )
    }
}

struct PolyrhythmRhythmSnapshot {
    let count: Int
    let step: Int
}

struct PolyrhythmTrainerSnapshot {
    let phase: Double
    let rhythms: [PolyrhythmRhythmSnapshot]
    let label: String
    let bpm: Int
    let bpmPulse: Double
    let needleFlash: Double
    let tapFeedback: [PolyrhythmTapFeedback]
}

struct PolyrhythmTapFeedback {
    let phase: Double
    let rhythmIndex: Int
    let accuracy: Double
    let opacity: Double
}

/// Compact, distance-readable polyrhythm notation: one clock per rhythm,
/// laid side by side so each circle sits above the trackpad band that
/// strikes it. Dots are beats, the radial hand is shared time, and each
/// circle's center digit is its subdivision count.
final class PolyrhythmTrainerView: NSView {
    static let circleSlotWidth: CGFloat = 100
    static let circleGap: CGFloat = 8
    static let sideMargin: CGFloat = 6
    static let fixedHeight: CGFloat = 116
    private static let faceRadius: CGFloat = 44
    private static let dotRadius: CGFloat = 34
    private static let handLength: CGFloat = 38
    private static let circleCenterY: CGFloat = 64

    static func logicalSize(rhythmCount: Int) -> NSSize {
        let circles = CGFloat(max(1, rhythmCount))
        return NSSize(
            width: sideMargin * 2 + circles * circleSlotWidth
                + (circles - 1) * circleGap,
            height: fixedHeight
        )
    }

    /// Each rhythm owns a hue, so a finger can be told by color alone rather
    /// than by counting dots. The first circle IS the system accent; the rest
    /// step around the wheel from it, which keeps the set flavored by whatever
    /// accent the user chose instead of hard-coding a palette beside it.
    ///
    /// Even spacing (360/count) collapses to red-vs-cyan at two circles, so the
    /// steps are fixed and deliberately uneven — far enough apart to separate,
    /// close enough to read as one family.
    private static let hueSteps: [CGFloat] = [0, 0.38, 0.62, 0.20, 0.80]

    static func circleColors(accent: NSColor, count: Int,
                             dark: Bool) -> [NSColor] {
        guard let base = accent.usingColorSpace(.deviceRGB) else {
            return Array(repeating: accent, count: max(1, count))
        }
        let hue = base.hueComponent
        // A grey or near-grey accent has no hue to rotate; borrow a blue so the
        // circles still separate instead of coming out as five identical greys.
        let saturation = base.saturationComponent < 0.15 ? 0.62 : base.saturationComponent
        let seed: CGFloat = base.saturationComponent < 0.15 ? 0.58 : hue
        return (0..<max(1, count)).map { index in
            let step = hueSteps[index % hueSteps.count]
            let h = (seed + step).truncatingRemainder(dividingBy: 1)
            // Dark mode paints on a near-black face and light mode on white, so
            // the same hue needs opposite treatment to hold equal weight: lift
            // brightness and ease saturation in the dark, deepen both in light.
            return NSColor(
                deviceHue: h,
                saturation: dark ? min(0.82, saturation * 0.92) : min(1, saturation * 1.08),
                brightness: dark ? 1.0 : 0.82,
                alpha: 1
            )
        }
    }

    /// A whisper of the TrackDrum pad under each clock — cream over the light
    /// skin, dark olive over the dark one — so the trainer reads as part of the
    /// same instrument sitting right below it, not a panel borrowed from
    /// somewhere else.
    static func faceColor(dark: Bool) -> NSColor {
        dark
            ? NSColor(srgbRed: 0.10, green: 0.11, blue: 0.09, alpha: 0.90)
            : NSColor(srgbRed: 0.99, green: 0.98, blue: 0.94, alpha: 0.92)
    }

    var snapshot: PolyrhythmTrainerSnapshot? { didSet { needsDisplay = true } }

    override var isOpaque: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        guard let snapshot else { return }
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let accent = KeyboardIconRenderer.accent

        let ink = dark ? NSColor.white : NSColor.black
        let colors = Self.circleColors(accent: accent,
                                       count: snapshot.rhythms.count,
                                       dark: dark)
        let size = Self.logicalSize(rhythmCount: snapshot.rhythms.count)
        let originX = bounds.minX + (bounds.width - size.width) / 2

        for (index, rhythm) in snapshot.rhythms.enumerated() {
            let center = CGPoint(
                x: originX + Self.sideMargin
                    + CGFloat(index) * (Self.circleSlotWidth + Self.circleGap)
                    + Self.circleSlotWidth / 2,
                y: bounds.minY + Self.circleCenterY
            )
            drawCircle(center: center, rhythm: rhythm,
                       phase: snapshot.phase,
                       needleFlash: snapshot.needleFlash,
                       color: colors[index], ink: ink, dark: dark)
            drawTapFeedback(
                snapshot.tapFeedback.filter { $0.rhythmIndex == index },
                center: center, color: colors[index]
            )
        }

        // The tempo is the metronome's face: big enough to read from
        // playing distance, and it blinks with the primary rhythm's beats
        // — bright on the strike, settling between them — so the number
        // itself is the pulse.
        let rate = "\(snapshot.bpm)"
        let pulse = CGFloat(max(0, min(1, snapshot.bpmPulse)))
        let rateAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 15, weight: .bold),
            .foregroundColor: ink.withAlphaComponent(0.38 + pulse * 0.62)
        ]
        let rateSize = rate.size(withAttributes: rateAttrs)
        rate.draw(at: CGPoint(x: bounds.midX - rateSize.width / 2,
                              y: bounds.minY + 1),
                  withAttributes: rateAttrs)
    }

    private func drawCircle(center: CGPoint, rhythm: PolyrhythmRhythmSnapshot,
                            phase: Double, needleFlash: Double,
                            color: NSColor, ink: NSColor,
                            dark: Bool) {
        // The panel floats over arbitrary apps. A near-opaque clock face
        // keeps each voice readable without turning the guide into a card.
        let faceRect = NSRect(x: center.x - Self.faceRadius,
                              y: center.y - Self.faceRadius,
                              width: Self.faceRadius * 2,
                              height: Self.faceRadius * 2)
        let face = NSBezierPath(ovalIn: faceRect)
        Self.faceColor(dark: dark).setFill()
        face.fill()
        // The rim carries the hue too, so a circle is identifiable before any
        // beat lands on it.
        color.withAlphaComponent(0.30).setStroke()
        face.lineWidth = 1.5
        face.stroke()

        for index in 0..<rhythm.count {
            let angle = CGFloat.pi / 2
                - CGFloat(index) / CGFloat(rhythm.count) * 2 * .pi
            let point = CGPoint(x: center.x + cos(angle) * Self.dotRadius,
                                y: center.y + sin(angle) * Self.dotRadius)
            let diameter: CGFloat = index == rhythm.step ? 10 : 6
            let dot = NSBezierPath(ovalIn: NSRect(x: point.x - diameter / 2,
                                                  y: point.y - diameter / 2,
                                                  width: diameter,
                                                  height: diameter))
            // Every dot is the circle's hue — the waiting beats just sit far
            // back. Grey dots would have made all the circles look alike
            // between strikes, which is exactly when you need to tell them
            // apart.
            (index == rhythm.step
                ? color
                : color.withAlphaComponent(dark ? 0.30 : 0.24)).setFill()
            dot.fill()
        }

        let angle = CGFloat.pi / 2 - CGFloat(phase) * 2 * .pi
        let tip = CGPoint(x: center.x + cos(angle) * Self.handLength,
                          y: center.y + sin(angle) * Self.handLength)
        let hand = NSBezierPath()
        hand.move(to: center); hand.line(to: tip)
        let flash = CGFloat(needleFlash)
        color.withAlphaComponent(0.78 + flash * 0.22).setStroke()
        hand.lineWidth = 1.5 + flash * 2.5
        hand.stroke()
        let hub = NSBezierPath(ovalIn: NSRect(x: center.x - 2.5, y: center.y - 2.5,
                                              width: 5, height: 5))
        color.setFill(); hub.fill()

        // The count sits on ink, not the hue — it is the one thing that must
        // stay legible at a glance in both appearances, and a saturated digit
        // this small goes soft.
        let label = "\(rhythm.count)"
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 12, weight: .bold),
            .foregroundColor: ink.withAlphaComponent(0.88)
        ]
        let size = label.size(withAttributes: attrs)
        label.draw(at: CGPoint(x: center.x - size.width / 2,
                               y: center.y - size.height - 3),
                   withAttributes: attrs)
    }

    /// A clean strike rings in the circle's OWN hue — the reward for landing it
    /// is the circle lighting up as itself. Only a late one changes color, so
    /// orange/red always means "off the grid" and never "different rhythm".
    private func drawTapFeedback(_ feedback: [PolyrhythmTapFeedback],
                                 center: CGPoint, color circleColor: NSColor) {
        for tap in feedback {
            let angle = CGFloat.pi / 2 - CGFloat(tap.phase) * 2 * .pi
            let point = CGPoint(x: center.x + cos(angle) * Self.dotRadius,
                                y: center.y + sin(angle) * Self.dotRadius)
            let color: NSColor = tap.accuracy >= 0.7
                ? circleColor : (tap.accuracy >= 0.4 ? .systemOrange : .systemRed)
            let mark = NSBezierPath(ovalIn: NSRect(x: point.x - 6, y: point.y - 6,
                                                   width: 12, height: 12))
            color.withAlphaComponent(CGFloat(tap.opacity) * 0.9).setStroke()
            mark.lineWidth = 2
            mark.stroke()
        }
    }
}

/// Headless render of the real guide stacked over TrackDrum artwork. This is
/// intentionally a QA door, not another product surface — reel tooling
/// composites these PNGs.
///
/// Flags after `--render-polyrhythm`:
///   --pattern 3:2      colon-separated subdivision counts (e.g. 3:4:5)
///   --phase 0.25       cycle phase 0…1 (wraps; default 0.18)
///   --bpm 85           tempo readout + needle-flash timing (default 85)
///   --light | --dark   appearance (light is the default; reels use light)
///   --scale 4          pixel scale (default 4)
///   --out /path.png    output PNG (default /tmp/menuband-polyrhythm.png)
enum PolyrhythmTrainerCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-polyrhythm") else { return false }
        func value(_ flag: String) -> String? {
            guard let index = args.firstIndex(of: flag), index + 1 < args.count else {
                return nil
            }
            return args[index + 1]
        }
        let output = value("--out") ?? "/tmp/menuband-polyrhythm.png"
        let scale = max(1, Double(value("--scale") ?? "4") ?? 4)
        let counts = (value("--pattern") ?? "3:2").split(separator: ":")
            .compactMap { Int($0) }.filter { $0 >= 1 }
        guard !counts.isEmpty else {
            FileHandle.standardError.write(Data(
                "polyrhythm: --pattern wants colon-separated counts like 3:2 or 3:4:5\n"
                    .utf8
            ))
            exit(1)
        }
        let rawPhase = Double(value("--phase") ?? "0.18") ?? 0.18
        let phase = rawPhase - floor(rawPhase)
        let bpm = min(300, max(30, Int(value("--bpm") ?? "85") ?? 85))
        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        let dark = args.contains("--dark") && !args.contains("--light")
        app.appearance = NSAppearance(named: dark ? .darkAqua : .aqua)

        // Deterministic: the same flags always yield the same pixels, so a
        // reel can sweep --phase frame by frame. The needle flash re-derives
        // the clock's 0.12 s decay from phase distance to the nearest beat.
        let cycleDuration = 60.0 / Double(bpm) * Double(counts[0])
        let sinceBeat = counts.map { count -> Double in
            let beats = phase * Double(count)
            return (beats - floor(beats)) / Double(count) * cycleDuration
        }.min() ?? .infinity
        let snapshot = PolyrhythmTrainerSnapshot(
            phase: phase,
            rhythms: counts.map {
                PolyrhythmRhythmSnapshot(
                    count: $0, step: Int(floor(phase * Double($0))) % $0
                )
            },
            label: counts.map(String.init).joined(separator: ":"),
            bpm: bpm,
            bpmPulse: PolyrhythmTrainerClock.bpmPulse(
                phase: phase, primaryCount: counts[0], bpm: bpm
            ),
            needleFlash: max(0, 1 - sinceBeat / 0.12),
            tapFeedback: []
        )

        let size = PitchBendCursorOverlayWindow.tracktrampSurfaceSize(
            rhythmCount: counts.count
        )
        let root = NSView(frame: NSRect(origin: .zero, size: size))
        root.appearance = app.appearance
        let skin = NSImageView(frame: NSRect(
            x: (size.width - TracktrampMetalView.logicalSize.width) / 2,
            y: 0,
            width: TracktrampMetalView.logicalSize.width,
            height: TracktrampMetalView.logicalSize.height
        ))
        skin.image = TrackpadDrumSkinPad.image(touches: [], energy: [], membrane: nil,
                                                appearance: app.appearance)
        root.addSubview(skin)
        let trainerSize = PolyrhythmTrainerView.logicalSize(rhythmCount: counts.count)
        let circles = PolyrhythmTrainerView(frame: NSRect(
            x: (size.width - trainerSize.width) / 2,
            y: TracktrampMetalView.logicalSize.height
                + PitchBendCursorOverlayWindow.polyrhythmGap,
            width: trainerSize.width,
            height: trainerSize.height
        ))
        circles.appearance = app.appearance
        circles.snapshot = snapshot
        root.addSubview(circles)
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
        guard let png = bitmap.representation(using: .png, properties: [:]) else {
            return true
        }
        do { try png.write(to: URL(fileURLWithPath: output)) }
        catch {
            FileHandle.standardError.write(Data("polyrhythm write failed: \(error)\n".utf8))
            exit(1)
        }
        print("polyrhythm \(pixelWidth)x\(pixelHeight) → \(output)")
        return true
    }
}
