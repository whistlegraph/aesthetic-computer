import AppKit
import QuartzCore

/// Horizontally-scrolling, all-Swift notation staff. Renders a whole
/// `TapeScore` as one continuous treble staff: time flows left → right
/// at `pixelsPerSecond`, notes draw as glossy chromatic pills at their
/// diatonic Y (same visual language as `StaffView`'s rolling pills),
/// ledger lines mark off-staff pitches. Saves vertical space vs. the
/// old portrait Verovio page and keeps the notes large + legible.
///
/// `TapeStaffScroller` wraps this in an `NSScrollView` so two-finger
/// trackpad scrolling natively scrubs back through the take; while the
/// take is live the scroller auto-follows the playhead unless the user
/// has scrolled back to look at earlier bars.
final class TapeStaffView: NSView {
    weak var score: TapeScore?
    var bpm: Double = 120
    var live: Bool = false

    /// Beats are the horizontal unit now (not wall-clock seconds): the
    /// staff is a true metronome-driven grid. One beat == one quarter
    /// note == `pixelsPerBeat` points; barlines every `beatsPerBar`.
    var pixelsPerBeat: CGFloat = 46
    let beatsPerBar = 4   // 4/4

    /// Playhead position in BEATS from the grid origin. Advanced by the
    /// scroller's live timer; drawn as a thin red line when `live`.
    var playheadBeat: Double = 0

    var staffColor: NSColor = .labelColor
    var noteColor: NSColor = .labelColor

    // Layout constants. y-UP (origin bottom-left) so the diatonic
    // pitch math reads "higher pitch = larger y" directly.
    static let lineSpacing: CGFloat = 13
    static let leftPad: CGFloat = 8
    /// Gap reserved at the left for the clef + time signature before
    /// the first note column.
    static let clefGap: CGFloat = 44
    static let rightPad: CGFloat = 40

    /// Bottom staff line (E4) Y. Placed so the 5 staff lines sit in the
    /// lower-middle with headroom above for high notes + ledger lines
    /// (and a bit below for bass ledgers). The staff spans
    /// `baselineY ... baselineY + 4*lineSpacing`.
    private var baselineY: CGFloat { bounds.height * 0.34 }

    override var isFlipped: Bool { false }

    // MARK: - Pitch → staff geometry
    //
    // Mirror of StaffView's (private) half-line math so the two Swift
    // staves agree on note placement. Half-line steps from E4: C4 = -2,
    // E4 = 0, B4 = 4, F5 = 8; each octave adds 7.
    private static let pitchClassStep: [Int] = [
        -2, -2, -1, -1, 0, 1, 1, 2, 2, 3, 3, 4,
    ]
    private static let pitchIsSharp: [Bool] = [
        false, true, false, true, false,
        false, true, false, true, false, true, false,
    ]
    private static func halfLineSteps(forMidi midi: UInt8) -> Int {
        let pc = Int(midi) % 12
        let octave = Int(midi) / 12 - 1
        return pitchClassStep[pc] + (octave - 4) * 7
    }

    /// Notehead Y center for a pitch. Unlike the old pill view, sharps
    /// sit on the SAME line/space as their natural (a real accidental
    /// glyph in front carries the chromatic info), so no quarter-step
    /// nudge.
    private func y(forMidi midi: UInt8) -> CGFloat {
        let steps = Self.halfLineSteps(forMidi: midi)
        return baselineY + CGFloat(steps) * (Self.lineSpacing / 2)
    }

    /// X (content coords) of a beat position on the grid.
    private func x(forBeat b: Double) -> CGFloat {
        Self.leftPad + Self.clefGap + CGFloat(b) * pixelsPerBeat
    }

    // MARK: - Beat clock
    //
    // The grid is anchored to the metronome's global downbeat clock so
    // a note's x position is its real beat number, not a guess from
    // inter-onset gaps. Absolute media-time → beats:
    //   beat = (t - metronomeStartTime) / secondsPerBeat
    // Notes quantize to the nearest 16th (quarter of a beat) for tidy
    // placement; the metronome's first audible tick is beat 0.

    private var secondsPerBeat: Double { 60.0 / max(20.0, bpm) }

    private func beat(forAbsolute t: TimeInterval) -> Double {
        let start = KeyboardIconRenderer.metronomeStartTime
        guard start > 0 else { return 0 }
        return (t - start) / secondsPerBeat
    }

    /// Snap a beat to the 16th-note grid (0.25-beat resolution).
    private func quantize(_ b: Double) -> Double {
        (b * 4).rounded() / 4
    }

    /// Beat of the most recent note onset (quantized) — drives the
    /// scroller's onset-follow so a fresh attack pulls the viewport.
    var lastOnsetBeat: Double {
        guard let evs = score?.absoluteEvents(), let last = evs.last else { return 0 }
        return quantize(beat(forAbsolute: last.on))
    }

    /// Total beats spanned by the take (for content width), at least a
    /// few empty bars so a fresh staff has room to play into.
    private func totalBeats(now: TimeInterval) -> Double {
        let evs = score?.absoluteEvents(now: now) ?? []
        let lastEnd = evs.map { beat(forAbsolute: $0.off) }.max() ?? 0
        let head = max(playheadBeat, lastEnd)
        // Round up to whole bars + one spare bar.
        let bars = (head / Double(beatsPerBar)).rounded(.up) + 1
        return max(Double(beatsPerBar) * 2, bars * Double(beatsPerBar))
    }

    /// Total content width to show the whole take plus a margin.
    func contentWidth(now: TimeInterval = CACurrentMediaTime()) -> CGFloat {
        let raw = Self.leftPad + Self.clefGap
            + CGFloat(totalBeats(now: now)) * pixelsPerBeat + Self.rightPad
        return max(raw, 320)
    }

    // MARK: - Draw

    override func draw(_ dirtyRect: NSRect) {
        let now = CACurrentMediaTime()
        // Paper background — warm cream in light, cocoa-dark in dark,
        // matching the retired Verovio page's palette family.
        let paper: NSColor = isDarkMode
            ? NSColor(srgbRed: 0.11, green: 0.086, blue: 0.063, alpha: 1)
            : NSColor(srgbRed: 0.972, green: 0.937, blue: 0.851, alpha: 1)
        paper.setFill()
        bounds.fill()

        let base = baselineY
        // Five staff lines (E4 G4 B4 D5 F5), full content width.
        staffColor.withAlphaComponent(0.85).setStroke()
        for i in 0..<5 {
            let ly = base + CGFloat(i) * Self.lineSpacing
            let p = NSBezierPath()
            p.lineWidth = 1
            p.move(to: NSPoint(x: 0, y: ly))
            p.line(to: NSPoint(x: bounds.maxX, y: ly))
            p.stroke()
        }

        // Barlines every bar across the whole content width.
        let bars = Int((totalBeats(now: now) / Double(beatsPerBar)).rounded(.up))
        staffColor.withAlphaComponent(0.55).setStroke()
        for barIdx in 0...max(1, bars) {
            let bx = x(forBeat: Double(barIdx * beatsPerBar))
            let bl = NSBezierPath()
            bl.lineWidth = 1
            bl.move(to: NSPoint(x: bx, y: base))
            bl.line(to: NSPoint(x: bx, y: base + 4 * Self.lineSpacing))
            bl.stroke()
        }

        drawClefAndMeter(staffBottomY: base)

        guard let score = score else { return }
        let evs = score.absoluteEvents(now: now)
        if !evs.isEmpty {
            NSLog("TapeStaff draw: %d events, metStart=%.2f bpm=%.0f bounds.w=%.0f content first onBeat=%.2f x=%.0f",
                  evs.count, KeyboardIconRenderer.metronomeStartTime, bpm,
                  bounds.width,
                  quantize(beat(forAbsolute: evs[0].on)),
                  x(forBeat: quantize(beat(forAbsolute: evs[0].on))))
        }
        for e in evs {
            let onBeat = quantize(beat(forAbsolute: e.on))
            let durBeats = beat(forAbsolute: e.off) - beat(forAbsolute: e.on)
            let value = noteValue(forBeats: durBeats)
            let xOn = x(forBeat: onBeat)
            let yy = y(forMidi: e.midi)
            drawLedgerLinesIfNeeded(forMidi: e.midi, centerX: xOn,
                                    staffBottomY: base)
            drawNote(midi: e.midi, x: xOn, y: yy, value: value)
        }

        // Live playhead — thin red line at the current beat.
        if live {
            let px = x(forBeat: playheadBeat)
            let top = base + 4 * Self.lineSpacing + 6
            let bot = base - 6
            NSColor.systemRed.withAlphaComponent(0.85).setStroke()
            let ph = NSBezierPath()
            ph.lineWidth = 1.4
            ph.move(to: NSPoint(x: px, y: bot))
            ph.line(to: NSPoint(x: px, y: top))
            ph.stroke()
        }
    }

    private var isDarkMode: Bool {
        effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
    }

    /// Standard note values, longest → shortest. `flags` drives how
    /// many flags hang off the stem; `filled`/`stemmed` shape the head.
    enum NoteValue {
        case whole, half, quarter, eighth, sixteenth
        var filled: Bool { self == .quarter || self == .eighth || self == .sixteenth }
        var stemmed: Bool { self != .whole }
        var flags: Int {
            switch self {
            case .eighth: return 1
            case .sixteenth: return 2
            default: return 0
            }
        }
    }

    /// Map a held duration in BEATS to the nearest classical note
    /// value (quarter == 1 beat). Thresholds sit at the geometric
    /// midpoints so a note rounds to the closest value.
    private func noteValue(forBeats beats: Double) -> NoteValue {
        if beats >= 3.0 { return .whole }
        if beats >= 1.5 { return .half }
        if beats >= 0.75 { return .quarter }
        if beats >= 0.375 { return .eighth }
        return .sixteenth
    }

    /// SMuFL notehead/flag glyphs for a value.
    private func headGlyph(_ v: NoteValue) -> Bravura.Glyph {
        switch v {
        case .whole: return .noteheadWhole
        case .half: return .noteheadHalf
        default: return .noteheadBlack
        }
    }

    /// Draw a note from real Bravura glyphs: notehead + (stroked) stem
    /// + SMuFL flag glyph + sharp accidental. `x` is the notehead's
    /// horizontal center, `y` its vertical center on the staff.
    private func drawNote(midi: UInt8, x: CGFloat, y: CGFloat,
                          value: NoteValue) {
        if x < -30 || x > bounds.maxX + 30 { return }
        let sp = Self.lineSpacing            // one staff space
        let ink = noteColor

        // Accidental glyph in front of the head for black keys (we
        // spell everything with sharps).
        if Self.pitchIsSharp[Int(midi) % 12] {
            let sharp = String(Bravura.Glyph.accidentalSharp.rawValue)
            let aw = Bravura.width(sharp, staffSpace: sp)
            Bravura.draw(.accidentalSharp,
                         at: NSPoint(x: x - sp * 0.95 - aw,
                                     y: y - Bravura.noteheadCenterOffset(staffSpace: sp)),
                         staffSpace: sp, color: ink)
        }

        // Notehead. Bravura draws from a left-baseline origin; nudge
        // left by half the glyph width to center it on `x`, and down by
        // the notehead center offset to center it on the staff line.
        let head = headGlyph(value)
        let hw = Bravura.width(String(head.rawValue), staffSpace: sp)
        let headX = x - hw / 2
        let baselineY = y - Bravura.noteheadCenterOffset(staffSpace: sp)
        Bravura.draw(head, at: NSPoint(x: headX, y: baselineY),
                     staffSpace: sp, color: ink)

        guard value.stemmed else { return }

        // Stem — up on the right edge of the head for notes below the
        // middle line, down on the left edge above it. Bravura's stem
        // anchor sits at the head's right (up) / left (down) side.
        let stemUp = Self.halfLineSteps(forMidi: midi) < 4
        let stemLen = sp * 3.3
        let stemX = stemUp ? (x + hw / 2 - 0.6) : (x - hw / 2 + 0.6)
        let stemTop = stemUp ? (y + stemLen) : y
        let stemBot = stemUp ? y : (y - stemLen)
        ink.setStroke()
        let stem = NSBezierPath()
        stem.lineWidth = 1.3
        stem.move(to: NSPoint(x: stemX, y: stemBot))
        stem.line(to: NSPoint(x: stemX, y: stemTop))
        stem.stroke()

        // Flag glyph at the free stem end (eighth = 1, sixteenth = 2).
        let flag: Bravura.Glyph?
        switch value {
        case .eighth: flag = stemUp ? .flag8thUp : .flag8thDown
        case .sixteenth: flag = stemUp ? .flag16thUp : .flag16thDown
        default: flag = nil
        }
        if let flag = flag {
            let tipY = stemUp ? stemTop : stemBot
            Bravura.draw(flag, at: NSPoint(x: stemX, y: tipY),
                         staffSpace: sp, color: ink)
        }
    }

    /// Short ledger strokes through a note head that sits outside the
    /// 5-line staff (above F5 or below E4).
    private func drawLedgerLinesIfNeeded(forMidi midi: UInt8,
                                         centerX: CGFloat,
                                         staffBottomY: CGFloat) {
        let steps = Self.halfLineSteps(forMidi: midi)
        guard steps < 0 || steps > 8 else { return }
        let half = Self.lineSpacing
        let w: CGFloat = Self.lineSpacing * 0.9
        let path = NSBezierPath()
        path.lineWidth = 1
        if steps > 8 {
            var s = 10
            while s <= steps {
                let ly = staffBottomY + CGFloat(s) * (Self.lineSpacing / 2)
                path.move(to: NSPoint(x: centerX - w, y: ly))
                path.line(to: NSPoint(x: centerX + w, y: ly))
                s += 2
            }
        } else {
            var s = -2
            while s >= steps {
                let ly = staffBottomY + CGFloat(s) * (Self.lineSpacing / 2)
                path.move(to: NSPoint(x: centerX - w, y: ly))
                path.line(to: NSPoint(x: centerX + w, y: ly))
                s -= 2
            }
        }
        _ = half
        guard !path.isEmpty else { return }
        staffColor.withAlphaComponent(0.7).setStroke()
        path.stroke()
    }

    /// Treble clef + 4/4 time signature from Bravura, at the staff's
    /// left edge. The clef's curl sits on the G line (2nd from bottom);
    /// the meter digits stack centered on the staff. Drawn at content
    /// origin (scrolls with the music, like a real engraving — the very
    /// first bar carries the clef + meter).
    private func drawClefAndMeter(staffBottomY: CGFloat) {
        let sp = Self.lineSpacing
        let ink = staffColor

        // G clef: SMuFL baseline sits on the G line (staffBottomY + 1
        // space). Bravura positions the spiral correctly when its
        // baseline is on that line.
        let gLineY = staffBottomY + sp   // 2nd line from bottom = G4
        Bravura.draw(.gClef, at: NSPoint(x: Self.leftPad, y: gLineY),
                     staffSpace: sp, color: ink)

        // 4/4 time signature — two digit glyphs stacked, centered
        // between the staff lines. Top "4" straddles the 4th line,
        // bottom "4" the 2nd line (standard placement).
        let four = String(Bravura.timeSigDigit(4))
        let dw = Bravura.width(four, staffSpace: sp)
        let clefW = Bravura.width(String(Bravura.Glyph.gClef.rawValue),
                                  staffSpace: sp)
        let meterX = Self.leftPad + clefW + sp * 0.4
        // Each digit spans ~2 spaces; baseline of the top digit sits
        // on the middle line, the bottom digit one staff-half below.
        let topBaseline = staffBottomY + sp * 2     // top number centered on upper half
        let botBaseline = staffBottomY              // bottom number on lower half
        _ = dw
        Bravura.draw(four, at: NSPoint(x: meterX, y: topBaseline),
                     staffSpace: sp, color: ink)
        Bravura.draw(four, at: NSPoint(x: meterX, y: botBaseline),
                     staffSpace: sp, color: ink)
    }
}

/// Scroll container for `TapeStaffView`. Owns a horizontal-only
/// `NSScrollView`, exposes the staff's knobs, runs the live-follow
/// timer, and detects two-finger scrub-back so the user can review
/// earlier bars without the playhead yanking them back to "now".
final class TapeStaffScroller: NSView {
    private let scrollView = NSScrollView()
    let staff = TapeStaffView()

    var score: TapeScore? {
        get { staff.score }
        set { staff.score = newValue; refresh() }
    }
    var bpm: Double {
        get { staff.bpm }
        set { staff.bpm = newValue }
    }
    var live: Bool {
        get { staff.live }
        set {
            staff.live = newValue
            if newValue { startTimer() } else { stopTimer() }
        }
    }
    var staffColor: NSColor {
        get { staff.staffColor }
        set { staff.staffColor = newValue; staff.needsDisplay = true }
    }
    var noteColor: NSColor {
        get { staff.noteColor }
        set { staff.noteColor = newValue; staff.needsDisplay = true }
    }

    private var followTimer: Timer?
    /// True while the user has scrolled away from the live edge — the
    /// timer keeps advancing the playhead + growing the page but stops
    /// dragging the viewport so the user can read history in peace.
    private var userScrolledBack = false
    /// Set while WE move the clip view so the bounds-changed observer
    /// doesn't misread our own auto-follow as a user scrub.
    private var programmaticScroll = false

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setUp()
    }
    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setUp()
    }

    private func setUp() {
        scrollView.translatesAutoresizingMaskIntoConstraints = false
        scrollView.hasHorizontalScroller = true
        scrollView.hasVerticalScroller = false
        scrollView.horizontalScrollElasticity = .allowed
        scrollView.verticalScrollElasticity = .none
        scrollView.autohidesScrollers = true
        scrollView.drawsBackground = false
        scrollView.borderType = .noBorder
        scrollView.documentView = staff
        addSubview(scrollView)
        NSLayoutConstraint.activate([
            scrollView.leadingAnchor.constraint(equalTo: leadingAnchor),
            scrollView.trailingAnchor.constraint(equalTo: trailingAnchor),
            scrollView.topAnchor.constraint(equalTo: topAnchor),
            scrollView.bottomAnchor.constraint(equalTo: bottomAnchor),
        ])
        let clip = scrollView.contentView
        clip.postsBoundsChangedNotifications = true
        NotificationCenter.default.addObserver(
            self, selector: #selector(clipBoundsChanged),
            name: NSView.boundsDidChangeNotification, object: clip)
    }

    deinit { NotificationCenter.default.removeObserver(self) }

    override func layout() {
        super.layout()
        sizeDocument()
    }

    /// Resize the document view: width = content width, height = clip
    /// height (so the staff fills vertically and never scrolls in Y).
    private func sizeDocument() {
        let h = scrollView.contentView.bounds.height
        let w = staff.contentWidth()
        if staff.frame.size != NSSize(width: w, height: h) {
            staff.frame = NSRect(x: 0, y: 0, width: w, height: max(h, 1))
        }
    }

    func refresh() {
        sizeDocument()
        staff.needsDisplay = true
        // A note edit (onset or release) just landed — scroll to the
        // latest onset so a new attack pulls the staff along, but a
        // sustain (which also fires onChange as its value updates)
        // leaves the viewport where it is.
        followToLastOnset()
    }

    /// Clear back to an empty staff scrolled home.
    func resetView() {
        userScrolledBack = false
        playheadHome()
        refresh()
    }

    private func playheadHome() {
        staff.playheadBeat = 0
        programmaticScroll = true
        scrollView.contentView.scroll(to: NSPoint(x: 0, y: 0))
        scrollView.reflectScrolledClipView(scrollView.contentView)
        programmaticScroll = false
    }

    // MARK: - Live follow

    private func startTimer() {
        if followTimer != nil { return }
        let t = Timer(timeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            self?.tick()
        }
        RunLoop.main.add(t, forMode: .common)
        followTimer = t
    }

    private func stopTimer() {
        followTimer?.invalidate()
        followTimer = nil
    }

    private func tick() {
        // Advance the playhead along the beat grid from the metronome
        // clock so the red line tracks real time even between notes.
        let start = KeyboardIconRenderer.metronomeStartTime
        if start > 0 {
            let spb = 60.0 / max(20.0, staff.bpm)
            staff.playheadBeat = max(0, (CACurrentMediaTime() - start) / spb)
        }
        // Repaint so the playhead moves + a held note's value updates
        // in place, and grow the page — but DON'T scroll while a note
        // is sustaining. Scrolling follows note ONSETS only (see
        // followToLastOnset), so holding a note keeps the staff still.
        sizeDocument()
        staff.needsDisplay = true
    }

    /// Scroll so the most recent note ONSET sits ~70% across the
    /// visible width. Called when a note is added (via refresh), not on
    /// every frame — so a sustained note never drags the viewport.
    private func followToLastOnset() {
        guard live, !userScrolledBack else { return }
        let clip = scrollView.contentView
        let visW = clip.bounds.width
        guard visW > 0 else { return }
        let docW = staff.frame.width
        // Most-recent onset in beats, on the metronome grid.
        let onsetBeat = staff.lastOnsetBeat
        let headX = TapeStaffView.leftPad + TapeStaffView.clefGap
            + CGFloat(onsetBeat) * staff.pixelsPerBeat
        var targetX = headX - visW * 0.70
        targetX = max(0, min(targetX, max(0, docW - visW)))
        programmaticScroll = true
        clip.scroll(to: NSPoint(x: targetX, y: 0))
        scrollView.reflectScrolledClipView(clip)
        programmaticScroll = false
    }

    @objc private func clipBoundsChanged() {
        guard live, !programmaticScroll else { return }
        // A user-driven scroll. If they're within a few px of the live
        // edge, treat it as "back at now" and resume auto-follow;
        // otherwise pause follow so they can review earlier bars.
        let clip = scrollView.contentView
        let visW = clip.bounds.width
        let docW = staff.frame.width
        let rightEdge = clip.bounds.origin.x + visW
        userScrolledBack = rightEdge < docW - 24
    }
}
