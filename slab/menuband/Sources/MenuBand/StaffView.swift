import AppKit

/// Five-line music staff drawn into the music-theory popover. The
/// staff is a treble clef in C — bottom line E4, top line F5 — and
/// renders any currently-held MIDI notes as filled ovals at their
/// correct diatonic positions, with a small sharp glyph in front
/// of any sharped pitch class. Pitches outside the visible
/// 5-line/4-space window get ledger lines drawn through them.
final class StaffView: NSView {
    /// Each entry is one note head on the staff. `ghost == true`
    /// means the user hasn't pressed it yet (typically a missing
    /// chord-candidate note); rendered as a thin outline + faded
    /// label so it reads as a "play me" suggestion. Played notes
    /// fill solid. `color` sets the route hue for ghost notes —
    /// nil falls back to the default staff color.
    struct Note: Equatable {
        let midi: UInt8
        let pitchClass: String
        let keyLabel: String?
        let ghost: Bool
        let color: NSColor?

        init(midi: UInt8, pitchClass: String,
             keyLabel: String?, ghost: Bool, color: NSColor? = nil) {
            self.midi = midi
            self.pitchClass = pitchClass
            self.keyLabel = keyLabel
            self.ghost = ghost
            self.color = color
        }

        static func == (lhs: Note, rhs: Note) -> Bool {
            lhs.midi == rhs.midi
                && lhs.pitchClass == rhs.pitchClass
                && lhs.keyLabel == rhs.keyLabel
                && lhs.ghost == rhs.ghost
                && lhs.color?.cgColor == rhs.color?.cgColor
        }
    }

    /// A colored "route" through a set of notes — drawn as a
    /// translucent path connecting the heads in MIDI order, so
    /// the user reads each chord candidate as a single connected
    /// shape on the staff. Rendered BEHIND the note heads.
    struct Route: Equatable {
        let color: NSColor
        let midis: [UInt8]
        let alpha: CGFloat

        static func == (lhs: Route, rhs: Route) -> Bool {
            lhs.midis == rhs.midis
                && lhs.alpha == rhs.alpha
                && lhs.color.cgColor == rhs.color.cgColor
        }
    }

    /// Replace this whenever held notes or the displayed chord
    /// candidate change. Triggers a redraw on diff. Held-note
    /// add/release transitions are forwarded into `scrollNotes` so
    /// the staff renders a piano-roll-style scroll: held notes
    /// freeze at the right edge while pressed, then slide leftward
    /// the moment they're released.
    var notes: [Note] = [] {
        didSet {
            if notes == oldValue { return }
            mergeHeldChange(oldHeld: oldValue.filter { !$0.ghost },
                            newHeld: notes.filter { !$0.ghost })
            needsDisplay = true
        }
    }

    /// One entry per note that has ever been pressed and not yet
    /// scrolled off the left edge. While `releasedAt == nil` the
    /// note is still held — it pins to the rightmost staff column
    /// and time effectively stops for it. Once released, x slides
    /// leftward at `scrollSpeed` until the entry falls off and gets
    /// pruned.
    private struct ScrollNote {
        let note: Note
        let appearedAt: TimeInterval
        var releasedAt: TimeInterval?
    }
    private var scrollNotes: [ScrollNote] = []
    private var scrollTimer: Timer?

    /// Beat markers dropped by the metronome each audible tick.
    /// Render as faint vertical bars that scroll leftward at
    /// `scrollSpeed`, just like released notes — gives the staff
    /// a visible tempo grid alongside the notes. Each marker
    /// remembers the X (in staff coordinates) where it spawned so
    /// it visually drops straight down from the metronome icon.
    private struct BeatMarker {
        let capturedAt: TimeInterval
        let originX: CGFloat
    }
    private var beatMarkers: [BeatMarker] = []

    /// Wall-clock timestamp of the most recent perfect-on-beat hit
    /// (note pressed within ~50 ms of a beat tick). Drives the
    /// brief white flash across the staff line that the user
    /// landed on — looks like a tiny "bonus" reward for nailing
    /// the timing.
    private struct PerfectHit {
        let halfLineSteps: Int
        let firedAt: TimeInterval
    }
    private var perfectHits: [PerfectHit] = []

    /// Drop a beat marker at a specific x in this view's coordinate
    /// space. Called from the popover whenever the metronome's
    /// `onTick` fires; the popover passes the metronome icon's
    /// projected x so each bar appears vertically aligned with the
    /// metronome itself.
    func dropBeatMarker(atX x: CGFloat) {
        beatMarkers.append(BeatMarker(capturedAt: CACurrentMediaTime(),
                                       originX: x))
        startScrollTimer()
        needsDisplay = true
    }

    /// Live pitch shift in SEMITONES (octave shift + trackpad bend
    /// combined). The whole staff drawing translates vertically by
    /// `shift * (lineSpacing / 2)` so a +1 semitone bend kicks the
    /// staff visibly upward, +12 (one octave) slides it a full
    /// 7-half-line jump. Updated by AppDelegate from the controller's
    /// bend + octave hooks; eased internally so changes feel
    /// continuous instead of snapping.
    var targetPitchShiftSemitones: CGFloat = 0 {
        didSet {
            guard targetPitchShiftSemitones != oldValue else { return }
            startScrollTimer()
        }
    }
    private var displayedPitchShiftSemitones: CGFloat = 0
    /// Points per second a released note rides leftward across the
    /// staff. Tuned so a freshly-released note takes ~1.5 s to
    /// glide from the play column past the clef.
    private static let scrollSpeed: CGFloat = 70

    private func mergeHeldChange(oldHeld: [Note], newHeld: [Note]) {
        let now = CACurrentMediaTime()
        let oldKeys = Set(oldHeld.map { $0.midi })
        let newKeys = Set(newHeld.map { $0.midi })
        // Mark released: anything in scrollNotes that is no longer
        // present in newHeld AND isn't already released.
        for i in scrollNotes.indices {
            if scrollNotes[i].releasedAt == nil
                && !newKeys.contains(scrollNotes[i].note.midi) {
                scrollNotes[i].releasedAt = now
            }
        }
        // Beat snap: if the user hits a key within `beatSnapWindow`
        // of the most recent metronome tick, anchor the new pill's
        // appearedAt to the beat's capturedAt instead of `now`.
        // That way a hit "right on the beat" lines up its leading
        // cap exactly with the metronome bar (compensates for the
        // user's reaction-time latency between hearing the click
        // and pressing the key — typically ~50–120ms).
        let beatSnapWindow: TimeInterval = 0.13
        // Tighter window for the "perfect hit" reward — only the
        // very-on-beat presses get the white-flash bonus.
        let perfectWindow: TimeInterval = 0.06
        let recentBeat = beatMarkers
            .map { $0.capturedAt }
            .filter { now - $0 <= beatSnapWindow && now >= $0 }
            .max()
        let snapTo: TimeInterval = recentBeat ?? now
        // Add freshly-pressed notes — anything in newHeld that
        // isn't already a held entry in scrollNotes.
        let alreadyHeld: Set<UInt8> = Set(scrollNotes
            .filter { $0.releasedAt == nil }
            .map { $0.note.midi })
        for n in newHeld where !alreadyHeld.contains(n.midi) {
            scrollNotes.append(ScrollNote(note: n, appearedAt: snapTo,
                                          releasedAt: nil))
            // Perfect-hit reward: if the press landed within
            // `perfectWindow` of a beat, queue a white-flash on the
            // staff line for this pitch.
            if let beat = recentBeat, now - beat <= perfectWindow {
                perfectHits.append(PerfectHit(
                    halfLineSteps: Self.halfLineSteps(forMidi: n.midi),
                    firedAt: now
                ))
            }
        }
        _ = oldKeys
        startScrollTimer()
    }
    var routes: [Route] = [] {
        didSet {
            if routes != oldValue { needsDisplay = true }
        }
    }

    // Camera scrolling was tried + retired — felt too weird to
    // have the staff slide under held notes. Staff stays put with
    // B4 anchored at the view's vertical midline; ledger lines
    // handle anything outside the 5-line range.

    /// Tint used for the staff lines and clef. Notes always render
    /// in `noteColor` regardless. Default labelColor follows the
    /// system theme automatically.
    var staffColor: NSColor = .labelColor {
        didSet { needsDisplay = true }
    }

    /// Tint used for the note heads + accidental glyphs.
    var noteColor: NSColor = .labelColor {
        didSet { needsDisplay = true }
    }

    override var isFlipped: Bool { false }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        // Clip the scrolling staff to the view's frame so the camera
        // pan can't drag note heads / ledger lines outside the
        // bezel housing it.
        wantsLayer = true
        layer?.masksToBounds = true
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        wantsLayer = true
        layer?.masksToBounds = true
    }

    private static let baseLineSpacing: CGFloat = 24.0    // wide enough that "full-size" natural pills clear the sharp guides
    private static let minLineSpacing: CGFloat = 9.0      // collapses when many notes span multiple octaves
    private static let lineWidth: CGFloat = 1.6   // thicker traditional staff lines so the 5-line skeleton reads clearly above the faint ghost guides
    // Horizontal padding chosen so the visible staff (left clef +
    // labels + 5-line range) sits horizontally centered in the
    // view. leftPad reserves room for the clef + pitch labels;
    // rightPad mirrors that so the staff lines are flanked evenly.
    private static let leftPad: CGFloat = 38.0
    private static let rightPad: CGFloat = 28.0
    private static let topPad: CGFloat = 14.0
    private static let bottomPad: CGFloat = 14.0
    /// Soft clamp on how far the camera can slide off center
    /// (in line-spacings). Stops the staff from sliding fully
    /// off-screen even on very high or low notes; ledger lines
    /// drawn through note heads handle the rest.
    private static let cameraOffsetClamp: CGFloat = 4.0

    /// Per-frame spacing used by `draw(_:)`. Shrinks toward
    /// `minLineSpacing` when multiple notes span a wide range so
    /// every held note + the centerline guide stays visible.
    private var lineSpacing: CGFloat = StaffView.baseLineSpacing
    private var staffHeight: CGFloat { 4 * lineSpacing }

    /// Step value (number of half-line increments above E4) for
    /// each pitch class in C major. Sharps live a step above the
    /// natural to their left and render an accidental glyph.
    /// Index = pitch class (0 = C, 1 = C#, …, 11 = B).
    /// `step` is in half-line units: bottom line E4 = 0, F4 space
    /// = 1, G4 line = 2, etc.
    private static let pitchClassStep: [Int] = [
        -2,  // C  (below bottom line)
        -2,  // C# (sharp on C position)
        -1,  // D
        -1,  // D# (sharp on D)
         0,  // E  (bottom line)
         1,  // F
         1,  // F# (sharp on F)
         2,  // G
         2,  // G#
         3,  // A
         3,  // A#
         4,  // B
    ]
    private static let pitchIsSharp: [Bool] = [
        false, true, false, true, false,
        false, true, false, true, false, true, false,
    ]

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        // Translate the entire drawing by the displayed pitch shift
        // so an octave change or trackpad bend visibly slides the
        // staff up / down — gives the user a feel for the shift in
        // addition to the audible pitch change. +1 semitone = +half
        // a staff-line of vertical translation.
        NSGraphicsContext.current?.saveGraphicsState()
        defer { NSGraphicsContext.current?.restoreGraphicsState() }
        let pitchShiftY = displayedPitchShiftSemitones * (Self.baseLineSpacing / 2)
        let xform = NSAffineTransform()
        xform.translateX(by: 0, yBy: pitchShiftY)
        xform.concat()
        let staffLeftX = bounds.minX + Self.leftPad

        // Half-line steps of each held note (sharp + natural alike).
        // Used to size the camera so a chord that spans an octave
        // shrinks lineSpacing until every head fits inside the bezel.
        let heldHalfLines: [CGFloat] = notes.compactMap { note -> CGFloat? in
            guard !note.ghost else { return nil }
            return CGFloat(Self.halfLineSteps(forMidi: note.midi))
        }
        // Natural-only set lights staff slots; sharps don't get a
        // slot highlight (their accidental glyph + head do).
        let heldNaturalSteps: Set<Int> = Set(notes.compactMap { note -> Int? in
            guard !note.ghost else { return nil }
            let pc = Int(note.midi) % 12
            guard !Self.pitchIsSharp[pc] else { return nil }
            return Self.halfLineSteps(forMidi: note.midi)
        })

        _ = heldHalfLines  // unused in fixed-staff layout, kept for clarity

        // Standard fixed staff. Anchor staffBottomY so the LOWEST
        // slot's pill sits with a small pad above the view's
        // bottom — eliminates the asymmetric whitespace we had
        // when forcing B4 onto bounds.midY (the slot range is
        // bigger below E4 than above F5, so center-anchoring
        // wasted ~12pt of headroom up top).
        lineSpacing = Self.baseLineSpacing
        let lowestSlotStep: CGFloat = -4
        // Match the natural pill diameter the per-note loop uses
        // below so the lowest slot's pill clears the bottom edge
        // exactly. Using a stale ratio here was leaving the dots
        // slightly off from the bar (the bar drew through markerX
        // exactly, but the staff anchor was computed assuming a
        // different pill size).
        let pillRadiusForLayout: CGFloat = lineSpacing * 0.30 / 2
        let staffBottomY = bounds.minY + 4 + pillRadiusForLayout
            - lowestSlotStep * (lineSpacing / 2)

        // Slot table: every diatonic position the user can play
        // across the C4..B5 range, ordered low → high. Each entry
        // carries the pitch class, half-line offset from E4, label,
        // and whether the slot sits ON a line (vs in a space) —
        // the on-line flag controls how lit slots paint (stroke
        // through vs translucent band).
        struct Slot {
            let pc: Int
            let halfLineSteps: Int
            let label: String
            let onLine: Bool
            /// True when this is a ledger position (off-staff) so
            /// we know whether to draw a faint guide line.
            let isLedger: Bool
        }
        // Slot table (pc, halfLineSteps, label, onLine, isLedger).
        // We then derive each slot's MIDI value and filter against
        // the active keymap's labelByMidi so notes the user can't
        // physically reach (e.g. notes outside the Ableton 60–76
        // range, or A3 in Notepat which lacks a key) are hidden
        // from the staff entirely — letters, lines, bands, gaps.
        let allSlots: [Slot] = [
            Slot(pc: 9,  halfLineSteps: -4, label: "A", onLine: true,  isLedger: true),  // A3 ledger
            Slot(pc: 11, halfLineSteps: -3, label: "B", onLine: false, isLedger: true),  // B3 below
            Slot(pc: 0,  halfLineSteps: -2, label: "C", onLine: true,  isLedger: true),  // C4 ledger
            Slot(pc: 2,  halfLineSteps: -1, label: "D", onLine: false, isLedger: true),  // D4 below
            Slot(pc: 4,  halfLineSteps: 0,  label: "E", onLine: true,  isLedger: false),
            Slot(pc: 5,  halfLineSteps: 1,  label: "F", onLine: false, isLedger: false),
            Slot(pc: 7,  halfLineSteps: 2,  label: "G", onLine: true,  isLedger: false),
            Slot(pc: 9,  halfLineSteps: 3,  label: "A", onLine: false, isLedger: false),
            Slot(pc: 11, halfLineSteps: 4,  label: "B", onLine: true,  isLedger: false),
            Slot(pc: 0,  halfLineSteps: 5,  label: "C", onLine: false, isLedger: false),
            Slot(pc: 2,  halfLineSteps: 6,  label: "D", onLine: true,  isLedger: false),
            Slot(pc: 4,  halfLineSteps: 7,  label: "E", onLine: false, isLedger: false),
            Slot(pc: 5,  halfLineSteps: 8,  label: "F", onLine: true,  isLedger: false),
            Slot(pc: 7,  halfLineSteps: 9,  label: "G", onLine: false, isLedger: true),  // G5 above
            Slot(pc: 9,  halfLineSteps: 10, label: "A", onLine: true,  isLedger: true),  // A5 ledger
            Slot(pc: 11, halfLineSteps: 11, label: "B", onLine: false, isLedger: true),  // B5 above
        ]
        let labels = KeyboardIconRenderer.labelByMidi
        let slots: [Slot] = allSlots.filter { slot in
            // MIDI = (octave + 1) * 12 + pc, where octave derives
            // from how far above E4 the slot's halfLineSteps sit.
            let octave = 4 + (slot.halfLineSteps - Self.pitchClassStep[slot.pc]) / 7
            let midi = (octave + 1) * 12 + slot.pc
            return labels[midi] != nil
        }

        // Diatonic octave step in points — distance one octave
        // covers vertically on the staff. Used to tile lines /
        // guides / ladder / highlight bands across all visible
        // octaves so the bezel always reads as the same 5-line
        // skeleton no matter which octave the user shifts to.
        let octaveStep: CGFloat = 7 * (lineSpacing / 2)
        // Visible region in pre-translation coordinates — anything
        // we draw in here will land somewhere in the bezel after
        // the pitch-shift transform. Used to bound every per-octave
        // tiling loop below so we never paint off-screen.
        let visibleMinY = bounds.minY - pitchShiftY
        let visibleMaxY = bounds.maxY - pitchShiftY

        // Pass 1 — paint translucent chromatic bands behind any
        // SPACE that's currently lit. Tiled per octave so the
        // played pitch class lights up at the same staff position
        // regardless of which octave it actually sounds in.
        for slot in slots where !slot.onLine && heldNaturalSteps.contains(slot.halfLineSteps) {
            let baseY = staffBottomY
                + CGFloat(slot.halfLineSteps) * (lineSpacing / 2)
            var k = Int(floor((visibleMinY - baseY) / octaveStep)) - 1
            let kMax = Int(ceil((visibleMaxY - baseY) / octaveStep)) + 1
            while k <= kMax {
                let yMid = baseY + CGFloat(k) * octaveStep
                let band = NSRect(
                    x: bounds.minX,
                    y: yMid - lineSpacing / 2 + 0.5,
                    width: bounds.width,
                    height: lineSpacing - 1
                )
                Self.chromaticColor(forPitchClass: slot.pc).withAlphaComponent(0.18).setFill()
                NSBezierPath(rect: band).fill()
                k += 1
            }
        }

        // Pass 1.5 — very faint "ghost" guides at every slot that
        // ISN'T one of the 5 traditional staff lines: in-staff
        // spaces (F4, A4, C5, E5) and ledger positions (above
        // and below). Lets the user track a rolling note's pitch
        // even when it sits between the proper lines, without
        // letting those guides compete with the staff itself.
        // Pre-compute per-slot activity so ledger / off-staff
        // positions can fade in when notes are on them and fade
        // out when notes leave (full-width fixed lines, no
        // horizontal scrolling). Held = full strength;
        // recently-released = decays over `fadeDuration`.
        let now = CACurrentMediaTime()
        let fadeDuration: CGFloat = 0.6
        var slotActivity: [Int: CGFloat] = [:]
        for snap in scrollNotes {
            let step = Self.halfLineSteps(forMidi: snap.note.midi)
            let active: CGFloat
            if let releasedAt = snap.releasedAt {
                let age = CGFloat(now - releasedAt)
                active = max(0, 1 - age / fadeDuration)
            } else {
                active = 1
            }
            if let cur = slotActivity[step] {
                slotActivity[step] = max(cur, active)
            } else {
                slotActivity[step] = active
            }
        }

        for slot in slots where !(slot.onLine && !slot.isLedger) {
            let baseY = staffBottomY
                + CGFloat(slot.halfLineSteps) * (lineSpacing / 2)
            // Ledger guides ghost-faint at rest; brighten under
            // active notes and fade back out after release. In-
            // staff space guides stay at their fixed faint level
            // (they sit between the 5 lines and don't need to call
            // attention to themselves). Tiled per octave so the
            // visible window always shows the in-octave guides.
            let baseAlpha: CGFloat = slot.isLedger ? 0.06 : 0.10
            let active = slotActivity[slot.halfLineSteps] ?? 0
            let alpha = slot.isLedger
                ? baseAlpha + (0.55 - baseAlpha) * active
                : baseAlpha
            var k = Int(floor((visibleMinY - baseY) / octaveStep)) - 1
            let kMax = Int(ceil((visibleMaxY - baseY) / octaveStep)) + 1
            while k <= kMax {
                let y = baseY + CGFloat(k) * octaveStep
                staffColor.withAlphaComponent(alpha).setStroke()
                let path = NSBezierPath()
                path.lineWidth = slot.isLedger ? Self.lineWidth * 0.7 : 0.5
                path.move(to: NSPoint(x: bounds.minX, y: y))
                path.line(to: NSPoint(x: bounds.maxX, y: y))
                path.stroke()
                k += 1
            }
        }

        // Always-on accent-color hairlines at every SHARP position
        // that's mapped on the active keymap. Tiled per octave so
        // the chromatic ladder is visible at every visible octave —
        // not just the base one.
        let sharpRoots: Set<Int> = [0, 2, 5, 7, 9]   // C, D, F, G, A → sharps above
        for slot in slots where sharpRoots.contains(slot.pc) {
            let sharpMidi = Self.midi(forSlotPc: slot.pc, halfLineSteps: slot.halfLineSteps) + 1
            guard labels[sharpMidi] != nil else { continue }
            let baseY = staffBottomY
                + CGFloat(slot.halfLineSteps) * (lineSpacing / 2)
                + lineSpacing / 4
            var k = Int(floor((visibleMinY - baseY) / octaveStep)) - 1
            let kMax = Int(ceil((visibleMaxY - baseY) / octaveStep)) + 1
            while k <= kMax {
                let y = baseY + CGFloat(k) * octaveStep
                NSColor.controlAccentColor.setStroke()
                let path = NSBezierPath()
                path.lineWidth = 0.7
                path.move(to: NSPoint(x: bounds.minX, y: y))
                path.line(to: NSPoint(x: bounds.maxX, y: y))
                path.stroke()
                k += 1
            }
        }

        // Per-line "perfect hit" flash strength. Lasts ~120ms and
        // fades smoothly — looks like the staff line briefly
        // glows white when a note hits exactly on the beat.
        let perfectFlashDuration: CGFloat = 0.18
        var flashByStep: [Int: CGFloat] = [:]
        for hit in perfectHits {
            let age = CGFloat(now - hit.firedAt)
            if age >= perfectFlashDuration { continue }
            let strength = max(0, 1 - age / perfectFlashDuration)
            flashByStep[hit.halfLineSteps] = max(flashByStep[hit.halfLineSteps] ?? 0,
                                                  strength)
        }

        // Tile the 5 main staff lines at every octave that lands in
        // the visible region. Lit highlight + on-beat flash apply
        // only to the base copy (k=0); other octave copies render
        // in the plain staff tint so the eye still anchors on the
        // actual sounding line. octaveStep + visibleMinY/MaxY were
        // computed earlier in this draw pass.
        for slot in slots where slot.onLine && !slot.isLedger {
            let baseY = staffBottomY
                + CGFloat(slot.halfLineSteps) * (lineSpacing / 2)
            let lit = heldNaturalSteps.contains(slot.halfLineSteps)
            let baseColor: NSColor = lit
                ? Self.chromaticColor(forPitchClass: slot.pc).withAlphaComponent(0.95)
                : staffColor
            let flash = flashByStep[slot.halfLineSteps] ?? 0
            let drawColor: NSColor = flash > 0.01
                ? (baseColor.blended(withFraction: flash, of: .white) ?? baseColor)
                : baseColor
            let baseWidth: CGFloat = lit ? Self.lineWidth + 0.8 : Self.lineWidth

            var k = Int(floor((visibleMinY - baseY) / octaveStep)) - 1
            let kMax = Int(ceil((visibleMaxY - baseY) / octaveStep)) + 1
            while k <= kMax {
                let y = baseY + CGFloat(k) * octaveStep
                let path = NSBezierPath()
                if k == 0 {
                    drawColor.setStroke()
                    path.lineWidth = baseWidth
                } else {
                    staffColor.setStroke()
                    path.lineWidth = Self.lineWidth
                }
                // Full view width — the rolling timeline reads as a
                // continuous strip, no left/right padding.
                path.move(to: NSPoint(x: bounds.minX, y: y))
                path.line(to: NSPoint(x: bounds.maxX, y: y))
                path.stroke()
                k += 1
            }
        }

        // Treble clef glyph fully retired — the staff is just five
        // full-width lines now, with the rolling note stream alone
        // providing pitch context.

        // Center-column letter chart — at the play column X, draw a
        // string of all available pitch-class letters stacked
        // vertically (one per slot). Resting letters are a flat
        // dark gray; when the slot is sounding the letter flips
        // chromatic + shakes. The colored trail dragged behind
        // each held note (drawn in the scroll-notes pass below)
        // emanates from each shaking letter.

        // Predictive beat lines — incoming from the RIGHT edge of
        // the staff, sliding leftward toward the play column at
        // `scrollSpeed`. Each upcoming click is materialized at
        // the moment the bar is dropped (= clickTime − traverseTime)
        // so by the time the audible click fires, the bar has
        // arrived AT the play column. While the bar is approaching
        // (right → center) it's GHOSTED; once it crosses center
        // it pops to opaque (the "slot in" cue) and continues
        // scrolling left as history.
        let dotRadius: CGFloat = 1.6
        let lineHalfLineSteps: [Int] = [0, 2, 4, 6, 8]
        let dotYs: [CGFloat] = lineHalfLineSteps.map {
            staffBottomY + CGFloat($0) * (lineSpacing / 2)
        }
        let centerX = bounds.midX
        let rightEdge = bounds.maxX
        let approachDistance = rightEdge - centerX
        let traverseTime = TimeInterval(approachDistance / Self.scrollSpeed)
        let approachingColor = staffColor.withAlphaComponent(0.18)
        let slotInColor = NSColor.systemYellow
        let restColor = staffColor.withAlphaComponent(0.55)

        if KeyboardIconRenderer.metronomeOn {
            let bpm = max(20, KeyboardIconRenderer.metronomeBPM)
            let period = 60.0 / Double(bpm) * 2.0
            let tickInterval = period / 2.0
            let metronomeStart = KeyboardIconRenderer.metronomeStartTime
            // First audible tick lands at start + period * 0.5 (see
            // MetronomeWidget.restartSwing). Subsequent ticks every
            // tickInterval.
            let firstClickTime = metronomeStart + period * 0.5
            // Render bars whose center-arrival times fall inside
            // [now - 2s, now + traverseTime + 0.5s]. Clamp index to
            // ≥0 so we don't try to render bars before the metronome
            // started.
            let windowStart = now - 2.0
            let windowEnd = now + traverseTime + 0.5
            let i_start = max(0, Int(floor((windowStart - firstClickTime) / tickInterval)))
            let i_end = max(i_start, Int(ceil((windowEnd - firstClickTime) / tickInterval)))
            // Drop animation phase windows for the moment the bar
            // arrives at center. Mirrors the original retired drop
            // sequence: bar holds at center while a vertical line
            // sweeps top → bottom, dots paint as it crosses each
            // staff line, and the color sweeps yellow → orange →
            // red → settle.
            let dropDuration: CGFloat = 0.18
            let yellowEnd: CGFloat = 0.10
            let orangeEnd: CGFloat = 0.20
            let redEnd: CGFloat = 0.25
            let topY = bounds.maxY
            let viewHeight = bounds.maxY - bounds.minY
            for i in i_start...i_end {
                let clickTime = firstClickTime + Double(i) * tickInterval
                let dropTime = clickTime - traverseTime
                let age = now - dropTime
                if age < 0 { continue }
                let centerArrival = CGFloat(now - clickTime)
                let isApproaching = centerArrival < 0
                let inDrop = !isApproaching && centerArrival < dropDuration
                // X position: while approaching, slide from right
                // toward center. While in the drop phase, hold at
                // center. After drop completes, scroll left.
                let x: CGFloat
                if isApproaching {
                    x = rightEdge - CGFloat(age) * Self.scrollSpeed
                } else if inDrop {
                    x = centerX
                } else {
                    let scrollAge = centerArrival - dropDuration
                    x = centerX - scrollAge * Self.scrollSpeed
                }
                if x < bounds.minX - 2 || x > bounds.maxX + 2 { continue }
                // Color band — incandescent strike that cools as it
                // ages out of the drop phase.
                let strokeColor: NSColor
                let lineWidth: CGFloat
                if isApproaching {
                    strokeColor = approachingColor
                    lineWidth = 0.6
                } else if centerArrival < yellowEnd {
                    strokeColor = NSColor.systemYellow
                    lineWidth = 1.2
                } else if centerArrival < orangeEnd {
                    strokeColor = NSColor.systemOrange
                    lineWidth = 1.0
                } else if centerArrival < redEnd {
                    strokeColor = NSColor.systemRed
                    lineWidth = 0.9
                } else {
                    strokeColor = restColor
                    lineWidth = 0.6
                }
                _ = slotInColor
                // Vertical bar — height varies by phase. While in
                // the drop window the leading edge sweeps from top
                // down to the bottom; after the drop the bar is
                // full height and just scrolls.
                let bottomY: CGFloat
                if inDrop {
                    let raw = centerArrival / dropDuration
                    let progress = raw * raw * (3 - 2 * raw)
                    bottomY = topY - viewHeight * progress
                } else {
                    bottomY = bounds.minY
                }
                let connector = NSBezierPath()
                connector.move(to: NSPoint(x: x, y: topY))
                connector.line(to: NSPoint(x: x, y: bottomY))
                connector.lineWidth = lineWidth
                strokeColor.setStroke()
                connector.stroke()
                // Dots paint at each staff line as the descending
                // edge passes through during drop, and stay
                // painted afterwards while the bar scrolls left.
                if !isApproaching {
                    strokeColor.setFill()
                    for dotY in dotYs where bottomY <= dotY {
                        let dot = NSBezierPath(ovalIn: NSRect(
                            x: x - dotRadius,
                            y: dotY - dotRadius,
                            width: dotRadius * 2,
                            height: dotRadius * 2
                        ))
                        dot.fill()
                    }
                }
            }
        }

        // Each held / released note draws as a horizontal colored
        // BAR ("trail") at its slot's y, with a shaking letter at
        // the play column for the held portion. While held the bar
        // grows leftward at `scrollSpeed`; once released the bar
        // keeps sliding leftward until it falls off the staff.
        _ = staffLeftX
        // Play column at the view's centerline so beat markers and
        // notes share the same vertical axis as the metronome icon.
        let playColumnX = bounds.midX

        // Per-note draw pass — each scrollNote becomes one solid
        // colored PILL extending from where the note first sounded
        // (leftX) to where it currently leads (rightX). The leading
        // cap of the pill IS the bubble — same chromatic color, and
        // the keyboard letter sits inside it. While the note is
        // held the leading cap pins to the play column and shakes;
        // once released the whole pill slides off leftward.
        // Geometry: sharp pills sit ±lineSpacing/4 from their
        // natural neighbors, so naturalRadius + sharpRadius MUST
        // be <lineSpacing/4 for the two not to overlap. With these
        // ratios:
        //   natural radius = 0.30 / 2 × lineSpacing = 0.15 × LS
        //   sharp   radius = 0.10 / 2 × lineSpacing = 0.05 × LS
        //   sum            = 0.20 × LS < 0.25 × LS ✓
        // (≈1.2pt clearance at lineSpacing=24, scales with LS.)
        // Sharp pill ends up looking like a small dot riding on
        // the accent guide line — distinct enough that you can
        // see the chromatic step, small enough that it never
        // crowds the natural pill on the line directly above or
        // below it.
        let naturalDiameter: CGFloat = lineSpacing * 0.30
        let sharpDiameter: CGFloat = lineSpacing * 0.10
        for snap in scrollNotes.sorted(by: { $0.note.midi < $1.note.midi }) {
            let note = snap.note
            let pc = Int(note.midi) % 12
            let isSharp = Self.pitchIsSharp[pc]
            // Naturals get their ROYGBIV chromatic hue. SEMITONES
            // (sharps / flats) all paint in the system accent color
            // so they read as "off-staff chromatic" against the
            // diatonic palette of the naturals.
            let chroma: NSColor = isSharp
                ? NSColor.controlAccentColor
                : Self.chromaticColor(forPitchClass: pc)
            // Sharps sit a quarter staff-step ABOVE their natural's
            // y position so they don't overlap the natural pill at
            // the same diatonic slot — reads as "raised half step
            // above the staff line you'd notate it on."
            var y = staffY(for: note.midi, staffBottomY: staffBottomY)
            if isSharp { y += lineSpacing / 4 }
            // Wrap the note's drawn Y into the visible window by
            // an integer number of TWO-octave spans, not one. Single-
            // octave wrap collapsed C5 and C6 onto the same Y, which
            // hid the natural 2-octave layout of the Notepat / Ableton
            // playable range. With a 2-octave wrap, the bezel always
            // shows ~2 octaves of distinct vertical positions while
            // still pulling the played octave into view when octaveShift
            // takes the absolute halfLineStep far above or below the
            // standard treble window.
            let wrapStep = octaveStep * 2
            let yPostRaw = y + pitchShiftY
            let visibleMidY = bounds.midY
            let wrapDelta = ((yPostRaw - visibleMidY) / wrapStep).rounded()
            y -= wrapDelta * wrapStep
            let bornAge = CGFloat(now - snap.appearedAt)
            let leftX = playColumnX - bornAge * Self.scrollSpeed
            let rightX: CGFloat
            if let releasedAt = snap.releasedAt {
                let releasedAge = CGFloat(now - releasedAt)
                rightX = playColumnX - releasedAge * Self.scrollSpeed
            } else {
                rightX = playColumnX
            }
            // Pill body stays solid + pinned. While held, the
            // letter shaking happens on the CENTER-COLUMN slot
            // letter, not on the pill cap — so "the actual letter
            // shakes in the stripe" without the stripe itself
            // jiggling. After release the leading cap inherits a
            // copy of the letter that scrolls leftward with the
            // pill.
            let leadX = rightX
            let bubbleYOffset: CGFloat = 0
            // Naturals draw at the larger "full size" diameter;
            // sharps render as a smaller satellite pill riding on
            // the accent-color guide line that marks their
            // position.
            let pillHeight = isSharp ? sharpDiameter : naturalDiameter
            let pillLeft = min(leftX, leadX - pillHeight)
            let pillRect = NSRect(
                x: pillLeft,
                y: y - pillHeight / 2 + bubbleYOffset,
                width: max(pillHeight, leadX - pillLeft),
                height: pillHeight
            )
            if pillRect.maxX < bounds.minX { continue }
            let pill = NSBezierPath(roundedRect: pillRect,
                                     xRadius: pillHeight / 2,
                                     yRadius: pillHeight / 2)
            // Aqua-style gloss: vertical chromatic gradient (light
            // crown → base → deeper foot) plus a translucent white
            // specular cap in the upper half of the pill, clipped
            // to the pill's rounded rect. Reads like a candy-button
            // marble. Sharps stay flat — the 5pt diameter is too
            // small for the gradient to register.
            if isSharp {
                chroma.setFill()
                pill.fill()
            } else {
                let topColor = chroma.blended(withFraction: 0.45, of: .white) ?? chroma
                let bottomColor = chroma.blended(withFraction: 0.22, of: .black) ?? chroma
                if let body = NSGradient(colors: [topColor, chroma, bottomColor],
                                          atLocations: [0.0, 0.55, 1.0],
                                          colorSpace: .deviceRGB) {
                    body.draw(in: pill, angle: 270)  // top → bottom in non-flipped coords
                } else {
                    chroma.setFill()
                    pill.fill()
                }
                NSGraphicsContext.current?.saveGraphicsState()
                pill.addClip()
                let hlInset = pillHeight * 0.18
                let hlMinY = pillRect.minY + pillHeight * 0.50
                let hlMaxY = pillRect.minY + pillHeight * 0.95
                let hlRect = NSRect(
                    x: pillRect.minX + hlInset,
                    y: hlMinY,
                    width: pillRect.width - hlInset * 2,
                    height: hlMaxY - hlMinY
                )
                let hl = NSBezierPath(roundedRect: hlRect,
                                       xRadius: hlRect.height / 2,
                                       yRadius: hlRect.height / 2)
                let hlGradient = NSGradient(colors: [
                    NSColor.white.withAlphaComponent(0.78),
                    NSColor.white.withAlphaComponent(0.00),
                ])
                hlGradient?.draw(in: hl, angle: 270)
                NSGraphicsContext.current?.restoreGraphicsState()
                // Soft inner rim — very faint dark line along the
                // bottom half so the bubble reads as 3D against
                // the staff lines.
                NSGraphicsContext.current?.saveGraphicsState()
                pill.addClip()
                NSColor.black.withAlphaComponent(0.18).setStroke()
                let rim = NSBezierPath(roundedRect: pillRect.insetBy(dx: 0.5, dy: 0.5),
                                        xRadius: pillHeight / 2,
                                        yRadius: pillHeight / 2)
                rim.lineWidth = 0.6
                rim.stroke()
                NSGraphicsContext.current?.restoreGraphicsState()
            }
            // Short ledger strokes through off-staff note heads
            // were retired — they scrolled horizontally with the
            // pill and read as visual noise on the upper/lower
            // edges. The full-width fading ledger lines in the
            // staff-line pass already mark off-staff pitches
            // without needing a per-note dash through the head.
            // Cap letter retired alongside the chart letters. The
            // pill is just chromatic color now — the position on
            // the staff identifies the pitch.
        }

        // Center-column letter chart retired — the staff reads
        // cleaner as just colored pills + lines + dots without
        // pitch labels at every slot. The QWERTY map below the
        // popover already disambiguates which key plays which
        // pitch, so the staff doesn't need to repeat that.
    }

    /// Half-line offset from E4 (the staff's bottom line). C4 = -2,
    /// E4 = 0, B4 = 4, F5 = 8, etc. Each octave adds 7.
    private static func halfLineSteps(forMidi midi: UInt8) -> Int {
        let pc = Int(midi) % 12
        let octave = Int(midi) / 12 - 1
        return pitchClassStep[pc] + (octave - 4) * 7
    }

    /// Inverse of `halfLineSteps(forMidi:)` for a known pitch class —
    /// recovers the MIDI value of the slot at `(pc, halfLineSteps)`.
    /// Used by the sharp-guide pass to look up whether the natural's
    /// neighboring sharp is mapped on the active keymap.
    private static func midi(forSlotPc pc: Int, halfLineSteps: Int) -> Int {
        let octave = 4 + (halfLineSteps - pitchClassStep[pc]) / 7
        return (octave + 1) * 12 + pc
    }

    /// Drive the per-frame redraw for scrolling released notes.
    /// While anything is still riding leftward (i.e. has a
    /// `releasedAt`), tick at ~60 Hz; once everything has either
    /// settled at the right column (still held) or scrolled off
    /// the left edge (pruned), the timer suspends itself.
    private func startScrollTimer() {
        if scrollTimer != nil { return }
        // Add to the .common run-loop mode so the timer keeps
        // ticking during mouse-tracking sessions (e.g. dragging
        // across the piano keys). Otherwise scheduledTimer runs
        // only in `.default` mode and the staff freezes mid-drag.
        let timer = Timer(timeInterval: 1.0 / 60.0, repeats: true) { [weak self] timer in
            guard let self = self else { timer.invalidate(); return }
            // Drop entries that have scrolled fully past the left
            // edge of the view (not just the staff's left padding)
            // so the heads draw all the way across before being
            // pruned. Beat markers prune on the same age cutoff.
            let now = CACurrentMediaTime()
            let leftCutoffAge = self.bounds.width / Self.scrollSpeed + 2.0
            self.scrollNotes.removeAll { snap in
                guard let releasedAt = snap.releasedAt else { return false }
                return (now - releasedAt) > leftCutoffAge
            }
            self.beatMarkers.removeAll { (now - $0.capturedAt) > leftCutoffAge }
            // Drop perfect-hit flashes once they've fully decayed
            // so the activity tracker can suspend cleanly.
            self.perfectHits.removeAll { now - $0.firedAt > 0.20 }
            // Ease the displayed pitch shift toward target each tick
            // — staff slides smoothly between octaves / under bend
            // rather than snapping.
            let pitchDiff = self.targetPitchShiftSemitones - self.displayedPitchShiftSemitones
            if abs(pitchDiff) > 0.005 {
                self.displayedPitchShiftSemitones += pitchDiff * 0.18
            } else {
                self.displayedPitchShiftSemitones = self.targetPitchShiftSemitones
            }
            let stillEasingPitch = self.displayedPitchShiftSemitones != self.targetPitchShiftSemitones
            // Keep ticking while ANY note is alive OR any metronome
            // beat marker is still on screen OR pitch is still
            // animating.
            if self.scrollNotes.isEmpty
                && self.beatMarkers.isEmpty
                && self.perfectHits.isEmpty
                && !stillEasingPitch {
                timer.invalidate()
                self.scrollTimer = nil
            }
            self.needsDisplay = true
        }
        RunLoop.main.add(timer, forMode: .common)
        scrollTimer = timer
    }

    /// Render a single line / space label — the pitch-class
    /// letter painted in the column to the left of the staff.
    /// When that pitch is currently held, the label flips into
    /// its chromatic color and goes heavy so the slot reads as
    /// "this is the line you're sitting on."
    private func drawSlotLabel(_ text: String,
                                atY y: CGFloat,
                                pitchClass pc: Int,
                                held: Bool,
                                onLine: Bool,
                                leftX: CGFloat) {
        // Each pitch slot gets a small square chip on the left side
        // of the staff — reads like a key column on a piano roll,
        // with a horizontal track running rightward through the
        // staff for that pitch's notes. Lit slots fill with the
        // chromatic hue; resting slots paint a faint outline so
        // the chart reads as a quiet index that highlights to the
        // sounding pitch when hit.
        let chipSize: CGFloat = lineSpacing * 0.78
        let chipRect = NSRect(
            x: 4,
            y: y - chipSize / 2,
            width: chipSize,
            height: chipSize
        )
        let chipPath = NSBezierPath(roundedRect: chipRect,
                                     xRadius: 2, yRadius: 2)
        let chroma = Self.chromaticColor(forPitchClass: pc)
        if held {
            chroma.setFill()
            chipPath.fill()
        } else {
            staffColor.withAlphaComponent(onLine ? 0.18 : 0.10).setFill()
            chipPath.fill()
            staffColor.withAlphaComponent(onLine ? 0.42 : 0.28).setStroke()
            chipPath.lineWidth = 0.6
            chipPath.stroke()
        }
        // Faint horizontal track running rightward from the chip
        // through the staff so each pitch's lane reads as its own
        // row — gives the rolling notes a "they all have a track
        // they're running in" feel.
        if !held {
            let track = NSBezierPath()
            track.move(to: NSPoint(x: chipRect.maxX + 1, y: y))
            track.line(to: NSPoint(x: bounds.maxX, y: y))
            track.lineWidth = 0.3
            staffColor.withAlphaComponent(onLine ? 0.10 : 0.06).setStroke()
            track.stroke()
        }
        // Lit slots reverse-tint the letter (white on chroma);
        // resting slots paint dark gray ink on the faint chip fill.
        let textColor: NSColor = held
            ? NSColor.white.withAlphaComponent(0.95)
            : NSColor(white: 0.32, alpha: 0.95)
        let color = textColor
        let font = NSFont.monospacedSystemFont(
            ofSize: held ? 9 : 8,
            weight: .heavy
        )
        let attr = NSAttributedString(
            string: text,
            attributes: [.font: font, .foregroundColor: color]
        )
        let size = attr.size()
        // Letter centered inside the chip square so the chart
        // reads as a tidy row of labeled cells.
        _ = leftX
        attr.draw(at: NSPoint(
            x: chipRect.midX - size.width / 2,
            y: y - size.height / 2
        ))
    }

    /// ROYGBIV chromatic palette — same hues as the bottoms of
    /// the menubar piano keys (and the chord-route holds), so a
    /// pitch class lights with the same color across every
    /// surface in the app.
    static func chromaticColor(forPitchClass pc: Int) -> NSColor {
        switch ((pc % 12) + 12) % 12 {
        case 0:  return NSColor(srgbRed: 1.00, green: 0.20, blue: 0.20, alpha: 1)  // C
        case 1:  return NSColor(srgbRed: 1.00, green: 0.42, blue: 0.10, alpha: 1)  // C#
        case 2:  return NSColor(srgbRed: 1.00, green: 0.63, blue: 0.00, alpha: 1)  // D
        case 3:  return NSColor(srgbRed: 1.00, green: 0.80, blue: 0.00, alpha: 1)  // D#
        case 4:  return NSColor(srgbRed: 1.00, green: 0.90, blue: 0.00, alpha: 1)  // E
        case 5:  return NSColor(srgbRed: 0.20, green: 0.78, blue: 0.20, alpha: 1)  // F
        case 6:  return NSColor(srgbRed: 0.18, green: 0.65, blue: 0.55, alpha: 1)  // F#
        case 7:  return NSColor(srgbRed: 0.20, green: 0.47, blue: 1.00, alpha: 1)  // G
        case 8:  return NSColor(srgbRed: 0.42, green: 0.30, blue: 0.92, alpha: 1)  // G#
        case 9:  return NSColor(srgbRed: 0.51, green: 0.20, blue: 0.78, alpha: 1)  // A
        case 10: return NSColor(srgbRed: 0.62, green: 0.20, blue: 0.78, alpha: 1)  // A#
        case 11: return NSColor(srgbRed: 0.71, green: 0.31, blue: 1.00, alpha: 1)  // B
        default: return .systemGray
        }
    }

    /// Compute the y-coordinate (in view space) of the center of a
    /// note head for the given MIDI note. Uses the diatonic step
    /// table so chromatic neighbors share a position (the sharp
    /// glyph differentiates them).
    private func staffY(for midi: UInt8, staffBottomY: CGFloat) -> CGFloat {
        let pc = Int(midi) % 12
        let octave = Int(midi) / 12 - 1   // C4 → octave 4
        let baseStep = Self.pitchClassStep[pc]
        // Each octave above 4 adds 7 diatonic steps, expressed as
        // 7 half-line increments. (Diatonic step = half-line.)
        let octaveSteps = (octave - 4) * 7
        let halfLineSteps = baseStep + octaveSteps
        return staffBottomY + CGFloat(halfLineSteps) * (lineSpacing / 2.0)
    }

    /// Stroke ledger lines through a note position when it lies
    /// outside the 5-line staff. Each ledger line is the same
    /// length as the note head plus a small overhang on each side.
    private func drawLedgerLines(at y: CGFloat,
                                  centerX: CGFloat,
                                  staffBottomY: CGFloat) {
        let staffTopY = staffBottomY + staffHeight
        let halfLine = lineSpacing / 2
        let ledgerHalfWidth = lineSpacing * 1.0
        let path = NSBezierPath()
        path.lineWidth = Self.lineWidth
        if y > staffTopY + halfLine {
            // Above the staff — draw a line at every full-line step
            // between staffTopY and y.
            var ly = staffTopY + lineSpacing
            while ly <= y + halfLine * 0.5 {
                path.move(to: NSPoint(x: centerX - ledgerHalfWidth, y: ly))
                path.line(to: NSPoint(x: centerX + ledgerHalfWidth, y: ly))
                ly += lineSpacing
            }
        } else if y < staffBottomY - halfLine {
            // Below the staff.
            var ly = staffBottomY - lineSpacing
            while ly >= y - halfLine * 0.5 {
                path.move(to: NSPoint(x: centerX - ledgerHalfWidth, y: ly))
                path.line(to: NSPoint(x: centerX + ledgerHalfWidth, y: ly))
                ly -= lineSpacing
            }
        }
        guard !path.isEmpty else { return }
        staffColor.setStroke()
        path.stroke()
    }

    override var intrinsicContentSize: NSSize {
        // Larger and shorter (per-the-design): bigger staff for
        // legibility, narrower frame so the staff sheet doesn't
        // hog the popover width.
        NSSize(width: 220, height: 110)
    }
}
