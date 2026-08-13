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
    private(set) var patternIndex = 0
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
    var pattern: Pattern { Self.patterns[patternIndex] }
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
    /// off → 3:2 → 4:3 → 5:4 → 3:4:5 → off.
    mutating func cyclePattern(at now: CFTimeInterval) {
        if !isActive {
            patternIndex = 0
            start(at: now)
        } else if patternIndex + 1 < Self.patterns.count {
            patternIndex += 1
            start(at: now)
        } else {
            stop()
        }
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

    func snapshot(at now: CFTimeInterval) -> PolyrhythmTrainerSnapshot? {
        guard isActive else { return nil }
        let elapsed = max(0, now - startedAt)
        let cycle = elapsed / cycleDuration
        return PolyrhythmTrainerSnapshot(
            phase: cycle - floor(cycle),
            rhythms: pattern.counts.map {
                PolyrhythmRhythmSnapshot(
                    count: $0,
                    step: Int(floor(cycle * Double($0))) % $0
                )
            },
            label: pattern.label,
            bpm: bpm,
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

    /// Left to right: accent, ink, then a blend for a third rhythm — three
    /// voices that stay apart without adding loud new hues.
    static func circleColors(accent: NSColor, ink: NSColor,
                             count: Int) -> [NSColor] {
        let blend = accent.blended(withFraction: 0.55, of: ink) ?? accent
        let palette = [accent, ink, blend]
        return (0..<max(1, count)).map { palette[$0 % palette.count] }
    }

    var snapshot: PolyrhythmTrainerSnapshot? { didSet { needsDisplay = true } }

    override var isOpaque: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        guard let snapshot else { return }
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let accent = KeyboardIconRenderer.accent
        let quiet = (dark ? NSColor.white : NSColor.black).withAlphaComponent(0.18)
        let ink = dark ? NSColor.white : NSColor.black
        let colors = Self.circleColors(accent: accent, ink: ink,
                                       count: snapshot.rhythms.count)
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
                       color: colors[index], quiet: quiet, ink: ink, dark: dark)
            drawTapFeedback(
                snapshot.tapFeedback.filter { $0.rhythmIndex == index },
                center: center, accent: accent
            )
        }

        let rate = "\(snapshot.bpm)"
        let rateAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 8, weight: .medium),
            .foregroundColor: ink.withAlphaComponent(0.58)
        ]
        let rateSize = rate.size(withAttributes: rateAttrs)
        rate.draw(at: CGPoint(x: bounds.midX - rateSize.width / 2,
                              y: bounds.minY + 4),
                  withAttributes: rateAttrs)
    }

    private func drawCircle(center: CGPoint, rhythm: PolyrhythmRhythmSnapshot,
                            phase: Double, needleFlash: Double,
                            color: NSColor, quiet: NSColor, ink: NSColor,
                            dark: Bool) {
        // The panel floats over arbitrary apps. A near-opaque clock face
        // keeps each voice readable without turning the guide into a card.
        let faceRect = NSRect(x: center.x - Self.faceRadius,
                              y: center.y - Self.faceRadius,
                              width: Self.faceRadius * 2,
                              height: Self.faceRadius * 2)
        let face = NSBezierPath(ovalIn: faceRect)
        (dark ? NSColor.black : NSColor.white).withAlphaComponent(0.86).setFill()
        face.fill()
        quiet.setStroke(); face.lineWidth = 1; face.stroke()

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
            (index == rhythm.step ? color : quiet).setFill()
            dot.fill()
        }

        let angle = CGFloat.pi / 2 - CGFloat(phase) * 2 * .pi
        let tip = CGPoint(x: center.x + cos(angle) * Self.handLength,
                          y: center.y + sin(angle) * Self.handLength)
        let hand = NSBezierPath()
        hand.move(to: center); hand.line(to: tip)
        let flash = CGFloat(needleFlash)
        (flash > 0 ? color : ink).withAlphaComponent(0.78 + flash * 0.22).setStroke()
        hand.lineWidth = 1.5 + flash * 2.5
        hand.stroke()
        let hub = NSBezierPath(ovalIn: NSRect(x: center.x - 2.5, y: center.y - 2.5,
                                              width: 5, height: 5))
        ink.setFill(); hub.fill()

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

    private func drawTapFeedback(_ feedback: [PolyrhythmTapFeedback],
                                 center: CGPoint, accent: NSColor) {
        for tap in feedback {
            let angle = CGFloat.pi / 2 - CGFloat(tap.phase) * 2 * .pi
            let point = CGPoint(x: center.x + cos(angle) * Self.dotRadius,
                                y: center.y + sin(angle) * Self.dotRadius)
            let color: NSColor = tap.accuracy >= 0.7
                ? accent : (tap.accuracy >= 0.4 ? .systemOrange : .systemRed)
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
