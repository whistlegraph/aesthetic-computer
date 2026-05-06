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
    /// candidate change. Triggers a redraw on diff.
    var notes: [Note] = [] {
        didSet {
            if notes != oldValue {
                retargetCamera()
                needsDisplay = true
            }
        }
    }
    var routes: [Route] = [] {
        didSet {
            if routes != oldValue { needsDisplay = true }
        }
    }

    /// Camera offset from the default centered position, in screen
    /// points. Positive = the staff is shifted DOWN on screen
    /// (showing lower-pitched notes near center); negative = staff
    /// shifted UP (centering higher-pitched notes). Drives the
    /// "scrolling staff" feel: as you play higher / lower the
    /// staff slides to keep the held average roughly centered.
    private var cameraOffsetY: CGFloat = 0
    private var targetCameraOffsetY: CGFloat = 0
    private var cameraTimer: Timer?

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

    private static let lineSpacing: CGFloat = 22.0    // bigger now that the camera keeps notes in frame
    private static let lineWidth: CGFloat = 1.1
    private static let leftPad: CGFloat = 42.0
    private static let rightPad: CGFloat = 8.0
    private static let topPad: CGFloat = 14.0
    private static let bottomPad: CGFloat = 14.0
    /// Height of just the 5-line staff (4 gaps × spacing).
    private static let staffHeight: CGFloat = 4 * lineSpacing
    /// Soft clamp on how far the camera can slide off center
    /// (in line-spacings). Stops the staff from sliding fully
    /// off-screen even on very high or low notes; ledger lines
    /// drawn through note heads handle the rest.
    private static let cameraOffsetClamp: CGFloat = 2.5

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
        let staffLeftX = bounds.minX + Self.leftPad
        let staffRightX = bounds.maxX - Self.rightPad

        // Pitch-class set for the currently-played natural notes —
        // used to light up whichever staff slot the user is sitting
        // on. Sharp/flat presses don't light a slot; their
        // accidental glyph + note head are the cue.
        let heldNaturalPCs = Set(notes.compactMap { note -> Int? in
            guard !note.ghost else { return nil }
            let pc = Int(note.midi) % 12
            return Self.pitchIsSharp[pc] ? nil : pc
        })

        // The staff's "default" center puts B4 (the middle line,
        // halfLineSteps = 4) at the view's vertical center.
        // `cameraOffsetY` shifts the whole staff up/down to keep
        // the held-notes' average near center — produces the
        // scrolling-camera effect across higher/lower play.
        let defaultStaffBottomY = bounds.midY - 2 * Self.lineSpacing
        let staffBottomY = defaultStaffBottomY + cameraOffsetY

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
        let slots: [Slot] = [
            // Below the staff — extends down to A3 so the
            // notepat z (A♯3) and x (B3) keys have visual slots.
            Slot(pc: 9,  halfLineSteps: -4, label: "A", onLine: true,  isLedger: true),  // A3 ledger
            Slot(pc: 11, halfLineSteps: -3, label: "B", onLine: false, isLedger: true),  // B3 below
            Slot(pc: 0,  halfLineSteps: -2, label: "C", onLine: true,  isLedger: true),  // C4 ledger
            Slot(pc: 2,  halfLineSteps: -1, label: "D", onLine: false, isLedger: true),  // D4 below
            // The staff itself — E4 G4 B4 D5 F5 lines, F4 A4 C5 E5 spaces
            Slot(pc: 4,  halfLineSteps: 0,  label: "E", onLine: true,  isLedger: false),
            Slot(pc: 5,  halfLineSteps: 1,  label: "F", onLine: false, isLedger: false),
            Slot(pc: 7,  halfLineSteps: 2,  label: "G", onLine: true,  isLedger: false),
            Slot(pc: 9,  halfLineSteps: 3,  label: "A", onLine: false, isLedger: false),
            Slot(pc: 11, halfLineSteps: 4,  label: "B", onLine: true,  isLedger: false),
            Slot(pc: 0,  halfLineSteps: 5,  label: "C", onLine: false, isLedger: false),
            Slot(pc: 2,  halfLineSteps: 6,  label: "D", onLine: true,  isLedger: false),
            Slot(pc: 4,  halfLineSteps: 7,  label: "E", onLine: false, isLedger: false),
            Slot(pc: 5,  halfLineSteps: 8,  label: "F", onLine: true,  isLedger: false),
            // Above the staff
            Slot(pc: 7,  halfLineSteps: 9,  label: "G", onLine: false, isLedger: true),  // G5 above
            Slot(pc: 9,  halfLineSteps: 10, label: "A", onLine: true,  isLedger: true),  // A5 ledger
            Slot(pc: 11, halfLineSteps: 11, label: "B", onLine: false, isLedger: true),  // B5 above
        ]

        // Pass 1 — paint translucent chromatic bands behind any
        // SPACE that's currently lit (so it sits behind the lines
        // rather than overdrawing them).
        for slot in slots where !slot.onLine && heldNaturalPCs.contains(slot.pc) {
            let yMid = staffBottomY
                + CGFloat(slot.halfLineSteps) * (Self.lineSpacing / 2)
            let band = NSRect(
                x: staffLeftX,
                y: yMid - Self.lineSpacing / 2 + 0.5,
                width: staffRightX - staffLeftX,
                height: Self.lineSpacing - 1
            )
            Self.chromaticColor(forPitchClass: slot.pc).withAlphaComponent(0.18).setFill()
            NSBezierPath(rect: band).fill()
        }

        // Pass 2 — five traditional staff lines only. Off-staff
        // pitches still render with their own short ledger
        // strokes through the note head (drawLedgerLines), but
        // we don't paint always-visible dashed guides for the
        // ledger positions — the staff stays a clean 5-line
        // notation surface.
        for slot in slots where slot.onLine && !slot.isLedger {
            let y = staffBottomY
                + CGFloat(slot.halfLineSteps) * (Self.lineSpacing / 2)
            let lit = heldNaturalPCs.contains(slot.pc)
            let drawColor = lit
                ? Self.chromaticColor(forPitchClass: slot.pc).withAlphaComponent(0.95)
                : staffColor
            drawColor.setStroke()
            let path = NSBezierPath()
            path.lineWidth = lit ? Self.lineWidth + 0.8 : Self.lineWidth
            path.move(to: NSPoint(x: staffLeftX, y: y))
            path.line(to: NSPoint(x: staffRightX, y: y))
            path.stroke()
        }

        // Treble clef glyph at the staff's left margin. Smaller now —
        // the pitch-class labels do most of the "you are here" work,
        // so the clef just needs to mark the staff as treble without
        // dominating the column.
        let clefFontSize: CGFloat = Self.lineSpacing * 2.6
        let clef = NSAttributedString(
            string: "\u{1D11E}",
            attributes: [
                .font: NSFont.systemFont(ofSize: clefFontSize, weight: .regular),
                .foregroundColor: staffColor,
            ]
        )
        clef.draw(at: NSPoint(
            x: bounds.minX + 4,
            y: staffBottomY - Self.lineSpacing * 0.7
        ))

        // Pass 3 — pitch-class labels for every slot (lines AND
        // spaces, including the ledger ones). Label flips to
        // bold + chromatic when the slot is sounding.
        for slot in slots {
            let y = staffBottomY
                + CGFloat(slot.halfLineSteps) * (Self.lineSpacing / 2)
            drawSlotLabel(slot.label,
                           atY: y,
                           pitchClass: slot.pc,
                           held: heldNaturalPCs.contains(slot.pc),
                           leftX: staffLeftX)
        }

        // Note heads — march from staffLeftX + a small inset so the
        // first note doesn't kiss the clef. All entries (held +
        // ghost) pack horizontally; ghosts render outlined so the
        // user reads them as "press these to complete the chord."
        guard !notes.isEmpty else { return }
        let noteAreaLeft = staffLeftX + 12
        let noteAreaRight = staffRightX - 4
        let noteAreaWidth = max(0, noteAreaRight - noteAreaLeft)
        let sortedNotes = notes.sorted { $0.midi < $1.midi }
        let noteSpacing = min(Self.lineSpacing * 2.4,
                               noteAreaWidth / CGFloat(max(1, sortedNotes.count)))
        let noteHeadW: CGFloat = Self.lineSpacing * 1.35
        let noteHeadH: CGFloat = Self.lineSpacing * 1.05

        for (i, note) in sortedNotes.enumerated() {
            let x = noteAreaLeft + CGFloat(i) * noteSpacing
            let y = staffY(for: note.midi, staffBottomY: staffBottomY)

            drawLedgerLines(at: y,
                             centerX: x + noteHeadW / 2,
                             staffBottomY: staffBottomY)

            let pc = Int(note.midi) % 12
            if Self.pitchIsSharp[pc] {
                let sharp = NSAttributedString(
                    string: "♯",
                    attributes: [
                        .font: NSFont.systemFont(ofSize: Self.lineSpacing * 1.7,
                                                  weight: .regular),
                        .foregroundColor: noteColor.withAlphaComponent(note.ghost ? 0.45 : 1.0),
                    ]
                )
                sharp.draw(at: NSPoint(x: x - 6, y: y - Self.lineSpacing))
            }

            // Played: filled oval. Ghost: outlined + faded so it
            // reads as a suggestion rather than a played note.
            // Ghost notes use their per-route color when supplied.
            let head = NSBezierPath(ovalIn: NSRect(
                x: x, y: y - noteHeadH / 2,
                width: noteHeadW, height: noteHeadH
            ))
            if note.ghost {
                let ghostStroke = (note.color ?? noteColor).withAlphaComponent(0.55)
                ghostStroke.setStroke()
                head.lineWidth = 1.4
                head.stroke()
                if let c = note.color {
                    c.withAlphaComponent(0.18).setFill()
                    head.fill()
                }
            } else {
                // Played note — paint in its chromatic color so
                // the head, the line/space it sits on, and the
                // pitch-class label on the left ALL light up in
                // the same hue.
                let chroma = Self.chromaticColor(forPitchClass: pc)
                chroma.setFill()
                head.fill()
                NSColor.black.withAlphaComponent(0.5).setStroke()
                head.lineWidth = 0.8
                head.stroke()
            }

            // Single keyboard letter centered INSIDE the note
            // head — that's the key the user types to play this
            // pitch. Played heads get near-black ink (high
            // contrast on every chromatic hue); ghost outlines
            // get a faded version of the route color.
            if let key = note.keyLabel, !key.isEmpty {
                let inkColor: NSColor = note.ghost
                    ? (note.color ?? noteColor).withAlphaComponent(0.75)
                    : NSColor(white: 0.10, alpha: 1)
                let keyAttr = NSAttributedString(
                    string: key,
                    attributes: [
                        .font: NSFont.monospacedSystemFont(ofSize: 8, weight: .heavy),
                        .foregroundColor: inkColor,
                    ]
                )
                let keySize = keyAttr.size()
                keyAttr.draw(at: NSPoint(
                    x: x + noteHeadW / 2 - keySize.width / 2,
                    y: y - keySize.height / 2 + 0.5
                ))
            }
        }
    }

    /// Recompute the camera target whenever held notes change.
    /// Target = average half-line position of all currently-held
    /// natural+sharp notes, mapped so the staff slides such that
    /// the held average lands at the staff's midline (B4 line).
    /// Clamped so the 5 staff lines stay mostly on-screen even
    /// for extreme notes — ledger strokes handle anything that
    /// goes past the clamp.
    private func retargetCamera() {
        let heldHalfLines: [CGFloat] = notes.compactMap { note in
            guard !note.ghost else { return nil }
            let pc = Int(note.midi) % 12
            let octave = Int(note.midi) / 12 - 1
            let baseStep = Self.pitchClassStep[pc]
            return CGFloat(baseStep + (octave - 4) * 7)
        }
        guard !heldHalfLines.isEmpty else { return }   // keep last position
        let avg = heldHalfLines.reduce(0, +) / CGFloat(heldHalfLines.count)
        let raw = (4 - avg) * (Self.lineSpacing / 2)
        let clamp = Self.cameraOffsetClamp * Self.lineSpacing
        targetCameraOffsetY = max(-clamp, min(clamp, raw))
        startCameraAnim()
    }

    private func startCameraAnim() {
        cameraTimer?.invalidate()
        cameraTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0,
                                             repeats: true) { [weak self] timer in
            guard let self = self else { timer.invalidate(); return }
            let diff = self.targetCameraOffsetY - self.cameraOffsetY
            if abs(diff) < 0.25 {
                self.cameraOffsetY = self.targetCameraOffsetY
                timer.invalidate()
                self.cameraTimer = nil
            } else {
                self.cameraOffsetY += diff * 0.18
            }
            self.needsDisplay = true
        }
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
                                leftX: CGFloat) {
        let color: NSColor = held
            ? Self.chromaticColor(forPitchClass: pc)
            : staffColor.withAlphaComponent(0.55)
        // Larger left-column pitch labels — they're the easiest read
        // when scanning the staff, especially with the smaller clef.
        let font = NSFont.monospacedSystemFont(
            ofSize: held ? 13 : 11,
            weight: .heavy
        )
        let attr = NSAttributedString(
            string: text,
            attributes: [.font: font, .foregroundColor: color]
        )
        let size = attr.size()
        attr.draw(at: NSPoint(
            x: leftX - size.width - 2,
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
        return staffBottomY + CGFloat(halfLineSteps) * (Self.lineSpacing / 2.0)
    }

    /// Stroke ledger lines through a note position when it lies
    /// outside the 5-line staff. Each ledger line is the same
    /// length as the note head plus a small overhang on each side.
    private func drawLedgerLines(at y: CGFloat,
                                  centerX: CGFloat,
                                  staffBottomY: CGFloat) {
        let staffTopY = staffBottomY + Self.staffHeight
        let halfLine = Self.lineSpacing / 2
        let ledgerHalfWidth = Self.lineSpacing * 1.0
        let path = NSBezierPath()
        path.lineWidth = Self.lineWidth
        if y > staffTopY + halfLine {
            // Above the staff — draw a line at every full-line step
            // between staffTopY and y.
            var ly = staffTopY + Self.lineSpacing
            while ly <= y + halfLine * 0.5 {
                path.move(to: NSPoint(x: centerX - ledgerHalfWidth, y: ly))
                path.line(to: NSPoint(x: centerX + ledgerHalfWidth, y: ly))
                ly += Self.lineSpacing
            }
        } else if y < staffBottomY - halfLine {
            // Below the staff.
            var ly = staffBottomY - Self.lineSpacing
            while ly >= y - halfLine * 0.5 {
                path.move(to: NSPoint(x: centerX - ledgerHalfWidth, y: ly))
                path.line(to: NSPoint(x: centerX + ledgerHalfWidth, y: ly))
                ly -= Self.lineSpacing
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
