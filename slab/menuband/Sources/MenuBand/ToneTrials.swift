import AppKit

/// Combo trials for scales — the fighting-game practice loop worn by the
/// pitch-slider page. A ladder of numbered trials (each one scale: name,
/// root, ascending intervals); the strip shows the whole sequence as
/// colored pills, a wrong note DROPS the pass back to the start (like a
/// dropped combo), and playing it clean flashes a CLEAR banner, rings the
/// clear chime, and auto-advances to the next trial. `/` toggles the mode,
/// `-`/`=` skip backward/forward through the ladder.
///
/// A class, deliberately: this is the director that owns the trial list,
/// the pass state, and the banner→advance clock, so the app delegate only
/// feeds it notes and repaints.
final class ToneTrials {
    struct Trial {
        let title: String
        let root: Int
        let intervals: [Int]
    }

    /// Twelve rungs, easy keys first, ending on the chromatic "boss".
    static let trials: [Trial] = [
        Trial(title: "C Major", root: 0, intervals: majorScale),
        Trial(title: "G Major", root: 7, intervals: majorScale),
        Trial(title: "F Major", root: 5, intervals: majorScale),
        Trial(title: "A Minor", root: 9, intervals: minorScale),
        Trial(title: "E Minor", root: 4, intervals: minorScale),
        Trial(title: "D Minor", root: 2, intervals: minorScale),
        Trial(title: "C Pentatonic", root: 0, intervals: [0, 2, 4, 7, 9, 12]),
        Trial(title: "A Minor Pentatonic", root: 9,
              intervals: [0, 3, 5, 7, 10, 12]),
        Trial(title: "C Blues", root: 0, intervals: [0, 3, 5, 6, 7, 10, 12]),
        Trial(title: "D Dorian", root: 2,
              intervals: [0, 2, 3, 5, 7, 9, 10, 12]),
        Trial(title: "G Mixolydian", root: 7,
              intervals: [0, 2, 4, 5, 7, 9, 10, 12]),
        Trial(title: "C Chromatic", root: 0,
              intervals: Array(0...12)),
    ]
    private static let majorScale = [0, 2, 4, 5, 7, 9, 11, 12]
    private static let minorScale = [0, 2, 3, 5, 7, 8, 10, 12]

    static let noteNames = ["C", "C♯", "D", "D♯", "E", "F",
                            "F♯", "G", "G♯", "A", "A♯", "B"]
    /// How long the CLEAR banner holds before the next trial loads.
    static let clearBannerDuration: CFTimeInterval = 1.5

    private(set) var isActive = false
    private(set) var index = 0
    private(set) var progress = 0
    /// Trials cleared this session (keeps counting across wraps).
    private(set) var cleared = 0
    /// Fires whenever `index` moves — skip keys or auto-advance — so the
    /// owner can persist ladder position without polling.
    var onIndexChange: ((Int) -> Void)?

    private var lastHitAt: CFTimeInterval = -.infinity
    private var lastDropAt: CFTimeInterval = -.infinity
    private var clearedAt: CFTimeInterval?

    var trial: Trial { Self.trials[index] }

    func start(at index: Int? = nil) {
        isActive = true
        if let index {
            self.index = ((index % Self.trials.count) + Self.trials.count)
                % Self.trials.count
        }
        resetPass()
    }

    func stop() {
        isActive = false
        resetPass()
    }

    /// `-`/`=` move through the ladder by hand; a new trial is a new pass.
    func step(by delta: Int) {
        guard isActive else { return }
        index = ((index + delta) % Self.trials.count + Self.trials.count)
            % Self.trials.count
        resetPass()
        onIndexChange?(index)
    }

    private func resetPass() {
        progress = 0
        clearedAt = nil
        lastHitAt = -.infinity
        lastDropAt = -.infinity
    }

    /// Judge a sounded note against the next degree, by pitch class so any
    /// octave counts. Right note advances; re-striking the degree just hit
    /// is free (repeats while phrasing are not drops); a wrong note DROPS
    /// the whole pass back to the start — trials rules, not a scold: the
    /// restart IS the practice. Notes during the CLEAR banner are ignored.
    func registerNote(_ midi: Int, at now: CFTimeInterval) {
        guard isActive, clearedAt == nil else { return }
        let intervals = trial.intervals
        let pitchClass = ((midi % 12) + 12) % 12
        if pitchClass == (trial.root + intervals[progress]) % 12 {
            progress += 1
            lastHitAt = now
            if progress == intervals.count {
                cleared += 1
                clearedAt = now
                FocusCueBeep.shared.trialClear()
            }
            return
        }
        if progress > 0,
           pitchClass == (trial.root + intervals[progress - 1]) % 12 {
            return
        }
        progress = 0
        lastDropAt = now
    }

    /// The banner→advance clock. Call before each repaint: once the CLEAR
    /// banner has held its beat, the next trial loads automatically.
    func update(at now: CFTimeInterval) {
        guard isActive, let clearedAt,
              now - clearedAt >= Self.clearBannerDuration else { return }
        index = (index + 1) % Self.trials.count
        resetPass()
        onIndexChange?(index)
    }

    func snapshot(at now: CFTimeInterval) -> ToneTrialsSnapshot? {
        guard isActive else { return nil }
        let intervals = trial.intervals
        return ToneTrialsSnapshot(
            trialNumber: index + 1,
            trialCount: Self.trials.count,
            title: trial.title,
            degrees: intervals.enumerated().map { degreeIndex, interval in
                ToneTrialDegreeSnapshot(
                    name: Self.noteNames[(trial.root + interval) % 12],
                    state: clearedAt != nil ? .done
                        : degreeIndex < progress ? .done
                        : (degreeIndex == progress ? .next : .pending)
                )
            },
            cleared: cleared,
            hitFlash: max(0, 1 - (now - lastHitAt) / 0.35),
            dropFlash: max(0, 1 - (now - lastDropAt) / 0.5),
            clearBanner: clearedAt.map {
                max(0, 1 - (now - $0) / Self.clearBannerDuration)
            } ?? 0
        )
    }
}

struct ToneTrialDegreeSnapshot {
    enum State { case done, next, pending }
    let name: String
    let state: State
}

struct ToneTrialsSnapshot {
    let trialNumber: Int
    let trialCount: Int
    let title: String
    let degrees: [ToneTrialDegreeSnapshot]
    let cleared: Int
    let hitFlash: Double
    let dropFlash: Double
    /// 1 → 0 while the CLEAR banner holds; 0 when no banner is up.
    let clearBanner: Double
}

/// Headless render of the trials strip over the real FX chart — a QA door
/// for eyeballing states, and the pixel source if a reel ever wants one.
///
/// Flags after `--render-tonetrials`:
///   --trial 3          1-based ladder position (default 1)
///   --progress 4       degrees already landed (default 0)
///   --clear | --drop   flash state to capture
///   --cleared 5        session tally shown at the right edge
///   --light | --dark   appearance (light default)
///   --scale 3          pixel scale (default 3)
///   --out /path.png    output PNG (default /tmp/menuband-tonetrials.png)
enum ToneTrialsCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-tonetrials") else { return false }
        func value(_ flag: String) -> String? {
            guard let index = args.firstIndex(of: flag),
                  index + 1 < args.count else { return nil }
            return args[index + 1]
        }
        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        let dark = args.contains("--dark") && !args.contains("--light")
        app.appearance = NSAppearance(named: dark ? .darkAqua : .aqua)

        let trials = ToneTrials()
        trials.start(at: (Int(value("--trial") ?? "1") ?? 1) - 1)
        let intervals = trials.trial.intervals
        let progress = min(max(0, Int(value("--progress") ?? "0") ?? 0),
                           intervals.count - 1)
        var strikeAt: CFTimeInterval = 10
        for interval in intervals.prefix(progress) {
            trials.registerNote(trials.trial.root + interval, at: strikeAt)
            strikeAt += 1
        }
        if args.contains("--clear") {
            for interval in intervals.dropFirst(progress) {
                trials.registerNote(trials.trial.root + interval, at: strikeAt)
            }
        } else if args.contains("--drop") {
            // One semitone above the expected degree is always wrong for
            // every scale in the ladder.
            trials.registerNote(trials.trial.root + intervals[progress] + 1,
                                at: strikeAt)
        }
        guard var snapshot = trials.snapshot(at: strikeAt) else { return true }
        if let cleared = Int(value("--cleared") ?? ""), cleared > 0 {
            snapshot = ToneTrialsSnapshot(
                trialNumber: snapshot.trialNumber,
                trialCount: snapshot.trialCount,
                title: snapshot.title,
                degrees: snapshot.degrees,
                cleared: cleared,
                hitFlash: snapshot.hitFlash,
                dropFlash: snapshot.dropFlash,
                clearBanner: snapshot.clearBanner
            )
        }

        let chart = PitchBendCursor.image(forBend: 0, echo: 0, keyDown: false)
        let image = ToneTrialsStrip.composite(chart: chart,
                                              snapshot: snapshot, dark: dark)
        let scale = max(1, Double(value("--scale") ?? "3") ?? 3)
        let output = value("--out") ?? "/tmp/menuband-tonetrials.png"
        let pixelWidth = Int((image.size.width * scale).rounded())
        let pixelHeight = Int((image.size.height * scale).rounded())
        guard let bitmap = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: pixelWidth,
            pixelsHigh: pixelHeight, bitsPerSample: 8, samplesPerPixel: 4,
            hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB,
            bytesPerRow: 0, bitsPerPixel: 0
        ) else { return true }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmap)
        NSGraphicsContext.current?.imageInterpolation = .high
        image.draw(in: NSRect(x: 0, y: 0,
                              width: pixelWidth, height: pixelHeight))
        NSGraphicsContext.restoreGraphicsState()
        guard let png = bitmap.representation(using: .png, properties: [:])
        else { return true }
        do { try png.write(to: URL(fileURLWithPath: output)) }
        catch {
            FileHandle.standardError.write(
                Data("tonetrials write failed: \(error)\n".utf8))
            exit(1)
        }
        print("tonetrials \(pixelWidth)x\(pixelHeight) → \(output)")
        return true
    }
}

/// Draws the trials strip and stacks it above the FX chart, so the
/// pitch-slider page carries its trainer the way TrackDrum carries the
/// circles. Pure image composition — the fx overlay is an image surface,
/// so the strip rides the existing show/update/anchor machinery for free.
enum ToneTrialsStrip {
    static let height: CGFloat = 58
    static let gap: CGFloat = 10
    static let minWidth: CGFloat = 248

    static func stripWidth(degreeCount: Int) -> CGFloat {
        max(minWidth, CGFloat(degreeCount) * 26 + 32)
    }

    static func composite(chart: NSImage,
                          snapshot: ToneTrialsSnapshot,
                          dark: Bool) -> NSImage {
        let width = max(chart.size.width,
                        stripWidth(degreeCount: snapshot.degrees.count))
        let size = NSSize(width: width,
                          height: chart.size.height + gap + height)
        let image = NSImage(size: size)
        image.lockFocus()
        chart.draw(at: NSPoint(x: (width - chart.size.width) / 2, y: 0),
                   from: .zero, operation: .sourceOver, fraction: 1)
        draw(snapshot,
             in: NSRect(x: 0, y: chart.size.height + gap,
                        width: width, height: height),
             dark: dark)
        image.unlockFocus()
        return image
    }

    static func draw(_ snapshot: ToneTrialsSnapshot, in rect: NSRect,
                     dark: Bool) {
        let ink = dark ? NSColor.white : NSColor.black
        let accent = KeyboardIconRenderer.accent
        // One hue per degree — the ladder reads as a rainbow of steps, the
        // same family trick as the polyrhythm circles.
        let hues = PolyrhythmTrainerView.circleColors(
            accent: accent, count: snapshot.degrees.count, dark: dark
        )
        let card = NSBezierPath(
            roundedRect: rect.insetBy(dx: 1, dy: 1), xRadius: 10, yRadius: 10
        )
        PolyrhythmTrainerView.faceColor(dark: dark).setFill()
        card.fill()
        // The rim is the mood ring: family accent at rest, gold while the
        // CLEAR banner holds, warning-red only on a dropped pass.
        let rim: NSColor = snapshot.clearBanner > 0.01
            ? NSColor.systemYellow.withAlphaComponent(
                0.55 + 0.45 * CGFloat(snapshot.clearBanner))
            : snapshot.dropFlash > 0.01
                ? NSColor.systemRed.withAlphaComponent(
                    0.35 + 0.45 * CGFloat(snapshot.dropFlash))
                : accent.withAlphaComponent(0.35)
        rim.setStroke()
        card.lineWidth = snapshot.clearBanner > 0.01 ? 2.5 : 1.5
        card.stroke()

        // Header: "TRIAL 3/12 · C MAJOR" with the clear tally riding the
        // right edge once anything's been landed.
        let header = "TRIAL \(snapshot.trialNumber)/\(snapshot.trialCount)"
        let headerAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 9, weight: .heavy),
            .foregroundColor: accent
        ]
        let title = snapshot.title.uppercased()
        let titleAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 11, weight: .black),
            .foregroundColor: ink.withAlphaComponent(0.92)
        ]
        let headerSize = header.size(withAttributes: headerAttrs)
        let titleSize = title.size(withAttributes: titleAttrs)
        let lineWidth = headerSize.width + 6 + titleSize.width
        let lineX = rect.midX - lineWidth / 2
        let lineY = rect.maxY - titleSize.height - 5
        header.draw(at: NSPoint(x: lineX, y: lineY + 1.5),
                    withAttributes: headerAttrs)
        title.draw(at: NSPoint(x: lineX + headerSize.width + 6, y: lineY),
                   withAttributes: titleAttrs)
        if snapshot.cleared > 0 {
            let tally = "✓\(snapshot.cleared)"
            let tallyAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedDigitSystemFont(ofSize: 9,
                                                        weight: .bold),
                .foregroundColor: NSColor.systemYellow
            ]
            tally.draw(at: NSPoint(
                x: rect.maxX - tally.size(withAttributes: tallyAttrs).width - 10,
                y: lineY + 1.5
            ), withAttributes: tallyAttrs)
        }

        let count = CGFloat(snapshot.degrees.count)
        let pillWidth = min(30, (rect.width - 20 - (count - 1) * 3) / count)
        let pillHeight: CGFloat = 20
        var x = rect.midX - (pillWidth * count + 3 * (count - 1)) / 2
        let y = rect.minY + 7
        for (degreeIndex, degree) in snapshot.degrees.enumerated() {
            let hue = hues[degreeIndex]
            let pill = NSBezierPath(
                roundedRect: NSRect(x: x, y: y,
                                    width: pillWidth, height: pillHeight),
                xRadius: 6, yRadius: 6
            )
            switch degree.state {
            case .done:
                hue.setFill()
                pill.fill()
            case .next:
                hue.withAlphaComponent(
                    0.22 + 0.5 * CGFloat(snapshot.hitFlash)).setFill()
                pill.fill()
                hue.setStroke()
                pill.lineWidth = 1.5
                pill.stroke()
            case .pending:
                hue.withAlphaComponent(dark ? 0.22 : 0.16).setFill()
                pill.fill()
            }
            let nameInk: NSColor = degree.state == .done
                ? (dark ? .black : .white)
                : ink.withAlphaComponent(degree.state == .next ? 0.95 : 0.6)
            let nameAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 9, weight: .bold),
                .foregroundColor: nameInk
            ]
            let nameSize = degree.name.size(withAttributes: nameAttrs)
            degree.name.draw(
                at: NSPoint(x: x + (pillWidth - nameSize.width) / 2,
                            y: y + (pillHeight - nameSize.height) / 2),
                withAttributes: nameAttrs)
            x += pillWidth + 3
        }

        // The payoff: the finished ladder washes back and a big gold
        // CLEAR! owns the whole strip while the next trial loads.
        if snapshot.clearBanner > 0.01 {
            let wash = NSBezierPath(
                roundedRect: rect.insetBy(dx: 2, dy: 2),
                xRadius: 9, yRadius: 9
            )
            PolyrhythmTrainerView.faceColor(dark: dark)
                .withAlphaComponent(0.72).setFill()
            wash.fill()
            let banner = "CLEAR!"
            let bannerAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 24, weight: .black),
                .foregroundColor: NSColor.systemYellow,
                .strokeWidth: -2.5,
                .strokeColor: dark ? NSColor.black : NSColor.white,
            ]
            let bannerSize = banner.size(withAttributes: bannerAttrs)
            banner.draw(at: NSPoint(x: rect.midX - bannerSize.width / 2,
                                    y: rect.midY - bannerSize.height / 2),
                        withAttributes: bannerAttrs)
        }
    }
}
