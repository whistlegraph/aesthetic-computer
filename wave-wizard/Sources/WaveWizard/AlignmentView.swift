import AppKit
import AVFoundation

// Visualises the lyric ↔ note mapping for the current sample. Renders
// the 11-note THEME antecedent as a horizontal track of beat-sized
// boxes labelled with midi notes, and draws the parsed lyric syllables
// inside each box. Click a box to AUDITION that note as a sine tone.
final class AlignmentView: NSView {
    // Audio engine used to audition individual notes when the user
    // clicks a box. Lazily started on first click.
    private static var audioEngine: AVAudioEngine?
    private static var audioMixer: AVAudioMixerNode?
    private static let toneSampleRate: Double = 48000

    private static func ensureAudio() {
        if audioEngine != nil { return }
        let e = AVAudioEngine()
        let m = AVAudioMixerNode()
        e.attach(m)
        e.connect(m, to: e.mainMixerNode, format: nil)
        do {
            try e.start()
            audioEngine = e
            audioMixer = m
        } catch {
            // engine failed to start — silent fail; clicks won't play
        }
    }

    // ── HOLD-TO-SUSTAIN tone (tone mode: press a pad and keep the
    // bell ringing as long as the mouse is down). One held player at
    // a time — pressing a new pad cuts the old one with a quick fade.
    private static var heldPlayer: AVAudioPlayerNode?
    private static var heldMixer:  AVAudioMixerNode?

    static func startHeldTone(midi: Int) {
        ensureAudio()
        stopHeldTone()
        guard let engine = audioEngine, let mainMix = audioMixer else { return }
        let freq = 440.0 * pow(2.0, Double(midi - 69) / 12.0)
        let sr = toneSampleRate
        // Generate a looping 0.5 s buffer that is exactly an integer
        // number of cycles long, so the loop point is click-free.
        let cycles = max(1, Int(freq * 0.5))
        let frames = AVAudioFrameCount(Double(cycles) / freq * sr)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sr, channels: 1),
              let buf = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames)
        else { return }
        buf.frameLength = frames
        let ptr = buf.floatChannelData![0]
        let twoPiF = 2.0 * .pi * freq
        for i in 0..<Int(frames) {
            let t = Double(i) / sr
            let v = sin(twoPiF * t)
                  + 0.40 * sin(twoPiF * 2 * t)
                  + 0.15 * sin(twoPiF * 3 * t)
            ptr[i] = Float(0.25 * v)
        }
        let player = AVAudioPlayerNode()
        let mix = AVAudioMixerNode()
        mix.outputVolume = 0
        engine.attach(player); engine.attach(mix)
        engine.connect(player, to: mix, format: format)
        engine.connect(mix, to: mainMix, format: nil)
        player.scheduleBuffer(buf, at: nil, options: [.loops, .interrupts])
        player.play()
        heldPlayer = player
        heldMixer = mix
        // 5 ms attack ramp
        let steps = 8
        for i in 0..<steps {
            DispatchQueue.main.asyncAfter(deadline: .now() + Double(i) * 0.0006) {
                if heldMixer === mix { mix.outputVolume = Float(i + 1) / Float(steps) }
            }
        }
    }

    static func stopHeldTone() {
        guard let player = heldPlayer, let mix = heldMixer, let engine = audioEngine else {
            heldPlayer = nil; heldMixer = nil; return
        }
        // 30 ms release ramp, then detach.
        let steps = 12
        for i in 0..<steps {
            DispatchQueue.main.asyncAfter(deadline: .now() + Double(i) * 0.0025) {
                mix.outputVolume = Float(steps - i - 1) / Float(steps)
            }
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.035) {
            player.stop()
            engine.disconnectNodeOutput(player)
            engine.disconnectNodeOutput(mix)
            engine.detach(player)
            engine.detach(mix)
        }
        heldPlayer = nil
        heldMixer = nil
    }

    static func playMidi(_ midi: Int, beats: Double, bpm: Double = 182) {
        ensureAudio()
        guard let engine = audioEngine, let mixer = audioMixer else { return }
        let freq = 440.0 * pow(2.0, Double(midi - 69) / 12.0)
        let dur = beats * (60.0 / bpm) * 0.96
        let sr = toneSampleRate
        let frames = AVAudioFrameCount(sr * dur)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sr, channels: 1),
              let buf = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames)
        else { return }
        buf.frameLength = frames
        let ptr = buf.floatChannelData![0]
        let twoPiF = 2.0 * .pi * freq
        let atkFrames = Int(0.005 * sr)
        let relFrames = Int(0.040 * sr)
        for i in 0..<Int(frames) {
            let t = Double(i) / sr
            var env = 1.0
            if i < atkFrames { env = Double(i) / Double(atkFrames) }
            else if i > Int(frames) - relFrames {
                env = Double(Int(frames) - i) / Double(relFrames)
            }
            // 3-partial bell-ish timbre so clicks read tonal.
            let v = sin(twoPiF * t)
                  + 0.40 * sin(twoPiF * 2 * t)
                  + 0.15 * sin(twoPiF * 3 * t)
            ptr[i] = Float(env * 0.25 * v)
        }
        let player = AVAudioPlayerNode()
        engine.attach(player)
        engine.connect(player, to: mixer, format: format)
        player.scheduleBuffer(buf) {
            DispatchQueue.main.async {
                player.stop()
                engine.disconnectNodeOutput(player)
                engine.detach(player)
            }
        }
        player.play()
    }

    // Stored rects so mouseDown can hit-test which box was clicked.
    private var noteRects: [(rect: NSRect, midi: Int, beats: Double)] = []

    // Optional recorded waveform overlaid on the note boxes, time-locked
    // to the same beat grid the notes use. If `waveformRange` is set,
    // the waveform only paints across those note boxes (e.g. when a
    // partial range take is loaded). Nil range = paint full width.
    private var recordedSamples: [Float] = []
    private var recordedSampleRate: Double = 48000
    private var waveformRange: (start: Int, end: Int)? = nil
    // When false, mouseDown / mouseDragged are ignored — used to lock
    // the timeline during recording / trimming / playback so the user
    // can't accidentally start a new range while a take is in flight.
    var isInteractive: Bool = true
    var clickHighlightIdx: Int? = nil
    // ── click MODE — what happens when the singer taps a pad ────────
    //   .tone   → audition the note as a sine bell (default)
    //   .arm    → press-and-hold RECORDS that word; mouseUp commits.
    //             "Nuclear arm" switch — the pad becomes a take pad.
    //   .sample → click plays back the previously-recorded take for
    //             that pad (so the singer can audition each word).
    enum ClickMode { case tone, arm, sample }
    var clickMode: ClickMode = .tone { didSet { needsDisplay = true } }
    var armPressingIdx: Int? = nil
    // Hovered pad index (mouse-over highlight). Drives a subtle pad
    // outline so the user sees what they're about to interact with.
    var hoverIdx: Int? = nil
    // Hooks back to WizardController for arm + sample modes.
    var onArmedPress:   ((Int) -> Void)?
    var onArmedRelease: ((Int) -> Void)?
    var onSamplePlay:   ((Int) -> Void)?

    // ── KEYBOARD NAVIGATION ─────────────────────────────────────────
    //   ← / →   move the focused note left / right (clamped at ends)
    //   space   in .arm mode: hold-to-record on the focused note
    //           in .tone mode: hold-to-sustain the focused note
    //           in .sample mode: play back the saved take
    //
    // `selectedIdx` is the keyboard-focused note. Drawn with a bold
    // accent-colour outline so the user can see what space will act on.
    // Mouse clicks also update it so click-then-arrow works naturally.
    var selectedIdx: Int? = nil
    private var spaceHeld: Bool = false

    // ── PER-NOTE HAMBURGER (3 layers, replaces global tone/arm/sample mode)
    //   Each note pad has three horizontal stripes:
    //     top    → TONE   (blue)   audition the sine bell
    //     middle → ARM    (red)    hold to record
    //     bottom → SAMPLE (green)  play back the saved take
    //   ↑/↓ moves the focused stripe; click goes straight to the stripe
    //   under the cursor; the global mode selector is hidden.
    enum SubZone: Int { case tone = 0, arm = 1, sample = 2 }
    var selectedSubZone: SubZone = .arm
    // Which sub-zone the mouse is currently pressing on (or that space
    // is holding). Drives the active-stripe highlight + which release
    // path to fire on mouseUp / keyUp.
    private var activeZone: SubZone? = nil

    private func stripeRect(in box: NSRect, zone: SubZone) -> NSRect {
        let h = box.height / 3.0
        let y: CGFloat
        switch zone {
        case .tone:   y = box.minY + 2 * h
        case .arm:    y = box.minY + 1 * h
        case .sample: y = box.minY
        }
        return NSRect(x: box.minX, y: y, width: box.width, height: h).insetBy(dx: 1, dy: 0.5)
    }
    private func noteSubZoneAt(_ point: NSPoint) -> (idx: Int, zone: SubZone)? {
        for (i, n) in noteRects.enumerated() {
            if n.rect.contains(point) {
                // Y is bottom-up in AppKit. Top stripe = tone, bottom = sample.
                let h = n.rect.height / 3.0
                let yFromBottom = point.y - n.rect.minY
                let zone: SubZone
                if yFromBottom >= 2 * h      { zone = .tone   }
                else if yFromBottom >= h     { zone = .arm    }
                else                         { zone = .sample }
                return (i, zone)
            }
        }
        return nil
    }
    private func trigger(noteIdx i: Int, zone: SubZone) {
        // Press behavior for any of the three layers. Called from
        // mouseDown and from keyDown(space).
        guard i < theme.count else { return }
        activeZone = zone
        armPressingIdx = i
        needsDisplay = true
        switch zone {
        case .tone:
            Self.startHeldTone(midi: rootMel + theme[i].off)
        case .arm:
            onArmedPress?(i)
        case .sample:
            clickHighlightIdx = i
            onSamplePlay?(i)
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) { [weak self] in
                self?.clickHighlightIdx = nil
                self?.armPressingIdx = nil
                self?.activeZone = nil
                self?.needsDisplay = true
            }
        }
    }
    private func releaseActiveZone() {
        guard let z = activeZone, let i = armPressingIdx else { return }
        switch z {
        case .tone:
            Self.stopHeldTone()
        case .arm:
            onArmedRelease?(i)
        case .sample:
            break   // already cleared by its async deadline
        }
        if z != .sample {
            armPressingIdx = nil
            activeZone = nil
            clickHighlightIdx = nil
            needsDisplay = true
        }
    }

    override var acceptsFirstResponder: Bool { true }
    override func becomeFirstResponder() -> Bool { needsDisplay = true; return true }
    override func resignFirstResponder() -> Bool {
        spaceHeld = false
        needsDisplay = true
        return true
    }

    override func keyDown(with event: NSEvent) {
        let kc = event.keyCode
        switch kc {
        case 123: moveSelection(by: -1); return          // ←
        case 124: moveSelection(by: +1); return          // →
        case 125: moveSubZone(by: +1); return            // ↓ (next layer in hamburger)
        case 126: moveSubZone(by: -1); return            // ↑
        case 36, 76:                                      // return / enter
            // Audition the focused note as a quick bell
            guard let i = selectedIdx, i < theme.count else { return }
            Self.playMidi(rootMel + theme[i].off, beats: min(theme[i].beats, 1.5))
            return
        case 49:                                          // space — trigger focused (note, sub-zone)
            if event.isARepeat { return }
            guard let i = selectedIdx else { return }
            spaceHeld = true
            trigger(noteIdx: i, zone: selectedSubZone)
            return
        default: break
        }
        super.keyDown(with: event)
    }

    override func keyUp(with event: NSEvent) {
        if event.keyCode == 49 && spaceHeld {
            spaceHeld = false
            releaseActiveZone()
            return
        }
        super.keyUp(with: event)
    }

    private func moveSelection(by delta: Int) {
        let n = theme.count
        if n == 0 { selectedIdx = nil; return }
        let cur = selectedIdx ?? 0
        let next = max(0, min(n - 1, cur + delta))
        if next != selectedIdx {
            selectedIdx = next
            needsDisplay = true
        }
    }
    private func moveSubZone(by delta: Int) {
        // ↑ = previous layer (tone above arm above sample), ↓ = next.
        // SubZone.rawValue: tone=0, arm=1, sample=2 (visually top-down).
        let cur = selectedSubZone.rawValue
        let next = max(0, min(2, cur + delta))
        if next != cur {
            selectedSubZone = SubZone(rawValue: next) ?? .arm
            needsDisplay = true
        }
    }

    private var trackingArea: NSTrackingArea?

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let t = trackingArea { removeTrackingArea(t) }
        let opts: NSTrackingArea.Options = [.activeInActiveApp, .mouseEnteredAndExited, .mouseMoved, .inVisibleRect]
        let t = NSTrackingArea(rect: bounds, options: opts, owner: self, userInfo: nil)
        addTrackingArea(t)
        trackingArea = t
    }

    override func mouseMoved(with event: NSEvent) {
        let p = convert(event.locationInWindow, from: nil)
        let i = noteIndexAt(p)
        if i != hoverIdx {
            hoverIdx = i
            needsDisplay = true
        }
    }

    override func mouseExited(with event: NSEvent) {
        if hoverIdx != nil { hoverIdx = nil; needsDisplay = true }
    }
    // While auditioning a note (single click or range), the currently
    // sounding note's index + its elapsed progress (0..1). draw()
    // renders a sweeping progress bar inside the box so the eye has
    // a metronome.
    private(set) var playingIdx: Int? = nil
    private(set) var playingProgress: Double = 0
    private var playTimer: Timer?
    // Range selection — start/end indices into the note array. nil
    // means no range is active. Click+drag to draw a range; click on
    // an existing range to clear it.
    private(set) var rangeStart: Int? = nil
    private(set) var rangeEnd: Int? = nil
    // Called whenever the range changes. WizardController can use this
    // to update its UI ("Record range N-M" / "Audition Range" buttons).
    var onRangeChanged: (() -> Void)?

    // Returns the inclusive (start, end) range, sorted. nil if unset.
    func selectedRange() -> (start: Int, end: Int)? {
        if let a = rangeStart, let b = rangeEnd {
            return (min(a, b), max(a, b))
        }
        return nil
    }

    private var dragStartIdx: Int? = nil

    private func noteIndexAt(_ point: NSPoint) -> Int? {
        for (i, n) in noteRects.enumerated() {
            if n.rect.contains(point) { return i }
        }
        return nil
    }

    override func mouseDown(with event: NSEvent) {
        guard isInteractive else { return }
        let p = convert(event.locationInWindow, from: nil)
        // 3-layer hamburger dispatch: click directly on the stripe under
        // the cursor (tone/arm/sample). The global mode selector is gone.
        if let hit = noteSubZoneAt(p) {
            window?.makeFirstResponder(self)
            selectedIdx = hit.idx
            selectedSubZone = hit.zone
            trigger(noteIdx: hit.idx, zone: hit.zone)
            return
        }
        guard let idx = noteIndexAt(p) else { return }
        window?.makeFirstResponder(self)
        selectedIdx = idx
        // SHIFT-click = extend range from current start. Plain click =
        // start a fresh drag or clear an existing range.
        if event.modifierFlags.contains(.shift), rangeStart != nil {
            rangeEnd = idx
            needsDisplay = true
            onRangeChanged?()
            return
        }
        // If clicking on an existing range, clear it and audition.
        if let r = selectedRange(), idx >= r.start && idx <= r.end {
            rangeStart = nil
            rangeEnd = nil
            needsDisplay = true
            onRangeChanged?()
            Self.playMidi(noteRects[idx].midi, beats: noteRects[idx].beats)
            return
        }
        // Otherwise: start a drag (or single-note audition on mouseUp).
        dragStartIdx = idx
        rangeStart = idx
        rangeEnd = idx
        clickHighlightIdx = idx
        needsDisplay = true
    }

    override func mouseDragged(with event: NSEvent) {
        guard isInteractive else { return }
        let p = convert(event.locationInWindow, from: nil)
        guard let idx = noteIndexAt(p), dragStartIdx != nil else { return }
        rangeEnd = idx
        needsDisplay = true
    }

    override func mouseUp(with event: NSEvent) {
        defer { dragStartIdx = nil }
        // 3-layer hamburger release: hand off to releaseActiveZone which
        // knows whether to stop the held tone, fire onArmedRelease, or
        // no-op (sample plays through to its own deadline).
        if activeZone != nil {
            releaseActiveZone()
            return
        }
        if let r = selectedRange(), r.start == r.end {
            // No actual drag — single-note selection. Audition the
            // note AND keep it selected as a 1-note range so the
            // Preview Melody button only plays this note (rather than
            // the full melody). Re-click the box to clear.
            let i = r.start
            clickHighlightIdx = i
            playNote(at: i)
            onRangeChanged?()
            return
        }
        onRangeChanged?()
    }

    // Play a single note while animating a progress bar inside its box.
    private func playNote(at idx: Int) {
        guard idx >= 0 && idx < noteRects.count else { return }
        let n = noteRects[idx]
        Self.playMidi(n.midi, beats: n.beats)
        startProgress(at: idx, beats: n.beats) { [weak self] in
            self?.clickHighlightIdx = nil
            self?.needsDisplay = true
        }
    }

    // Step through the selected range, sounding each note in turn
    // while sweeping the progress bar across each box.
    func auditionSelectedRange() {
        guard let r = selectedRange() else { return }
        playSequence(from: r.start, to: r.end)
    }

    // Helper: play notes [start...end] in sequence, with the progress
    // bar tracking the currently sounding box.
    private func playSequence(from start: Int, to end: Int) {
        playTimer?.invalidate()
        playTimer = nil
        var queue: [(idx: Int, beats: Double)] = []
        for i in start...end { queue.append((i, noteRects[i].beats)) }
        playNext(queue: queue)
    }

    private func playNext(queue: [(idx: Int, beats: Double)]) {
        guard let head = queue.first else {
            playingIdx = nil; playingProgress = 0; needsDisplay = true; return
        }
        let rest = Array(queue.dropFirst())
        let n = noteRects[head.idx]
        Self.playMidi(n.midi, beats: head.beats)
        startProgress(at: head.idx, beats: head.beats) { [weak self] in
            self?.playNext(queue: rest)
        }
    }

    private func startProgress(at idx: Int, beats: Double, onComplete: @escaping () -> Void) {
        playTimer?.invalidate()
        playingIdx = idx
        playingProgress = 0
        needsDisplay = true
        let dur = beats * (60.0 / 182.0)        // SPB
        let start = CACurrentMediaTime()
        playTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] t in
            guard let self = self else { t.invalidate(); return }
            let elapsed = CACurrentMediaTime() - start
            self.playingProgress = min(1.0, elapsed / dur)
            self.needsDisplay = true
            if elapsed >= dur {
                t.invalidate()
                self.playTimer = nil
                onComplete()
            }
        }
    }

    func clearRange() {
        rangeStart = nil
        rangeEnd = nil
        needsDisplay = true
        onRangeChanged?()
    }

    func setRecordedWaveform(samples: [Float], sampleRate: Double, range: (start: Int, end: Int)? = nil) {
        recordedSamples = samples
        recordedSampleRate = sampleRate
        waveformRange = range
        needsDisplay = true
    }

    func clearRecordedWaveform() {
        recordedSamples = []
        waveformRange = nil
        needsDisplay = true
    }

    // ── per-note waveform thumbnails ────────────────────────────────
    // After a take is saved for a given note, store a downsampled set
    // of peaks so the SAMPLE stripe (bottom green band) can draw a tiny
    // waveform preview of what was recorded.
    private var noteWaveforms: [Int: [Float]] = [:]
    func setNoteWaveform(noteIdx: Int, samples: [Float], downsamplePeaks: Int = 64) {
        // Pre-compute a downsampled peak envelope so draw() doesn't
        // re-traverse the full take every frame.
        guard !samples.isEmpty else { noteWaveforms.removeValue(forKey: noteIdx); needsDisplay = true; return }
        let n = max(1, downsamplePeaks)
        let stride = max(1, samples.count / n)
        var peaks: [Float] = []
        peaks.reserveCapacity(n)
        var i = 0
        while i < samples.count {
            let end = min(samples.count, i + stride)
            var m: Float = 0
            for j in i..<end {
                let a = abs(samples[j])
                if a > m { m = a }
            }
            peaks.append(m)
            i += stride
        }
        noteWaveforms[noteIdx] = peaks
        needsDisplay = true
    }
    func clearNoteWaveforms() {
        noteWaveforms.removeAll()
        needsDisplay = true
    }
    // First 11 notes of the brass theme (offset from defaultRootMel=62, beats).
    // "that" lengthened from 0.5 → 1.0β so the vocal can actually land
    // on the note; v2 ("ney") sustain trimmed 2.0 → 1.5β to compensate.
    //
    // Note (2026-05-29): these were originally `static let theme/rootMel`
    // hardcoded for hellsine. They're now the DEFAULTS — instance
    // properties `self.theme` / `self.rootMel` are used at draw time and
    // can be overridden per-sample via `setScore(rootMel:notes:)`. Specs
    // that don't carry a `score` field still render with the hellsine
    // brass theme as before.
    static let defaultTheme: [(off: Int, beats: Double)] = [
        (-5, 1.0),   // A3
        ( 0, 1.5),   // D4
        ( 3, 1.0),   // F4    (was 0.5 — "that")
        ( 7, 1.0),   // A4
        ( 7, 1.0),   // A4
        ( 8, 1.0),   // Bb4
        ( 7, 1.0),   // A4
        ( 5, 1.0),   // G4
        ( 3, 1.5),   // F4 sustain (was 2.0 — "ney")
        ( 2, 1.0),   // E4
        ( 0, 1.0),   // D4
    ]
    static let defaultRootMel = 62

    // Per-view instance state — overridable via setScore().
    var theme: [(off: Int, beats: Double)] = AlignmentView.defaultTheme
    var rootMel: Int = AlignmentView.defaultRootMel

    /// Swap in a per-sample score (lyric+notes mapping). Pass an empty
    /// `notes` array to revert to the default brass theme. Also resets
    /// the keyboard-focused note + clears any leftover per-note
    /// waveform thumbnails from a previous sample.
    func setScore(rootMel: Int, notes: [(off: Int, beats: Double)]) {
        self.rootMel = rootMel
        self.theme = notes.isEmpty ? AlignmentView.defaultTheme : notes
        self.selectedIdx = self.theme.isEmpty ? nil : 0
        self.noteWaveforms.removeAll()
        needsDisplay = true
    }

    static let noteNames = ["C","C#","D","D#","E","F","F#","G","G#","A","Bb","B"]

    // ── AC note colour palette ──────────────────────────────────────
    // Mirrors system/public/aesthetic.computer/lib/note-colors.mjs.
    // Octave 4 = base saturated ROYGBIV; octave 5+ = dayglo neon;
    // octave 3- = muted. Sharps/flats stay near-black.
    // (We use these to TINT the note boxes so the eye learns which
    // chord-tone landed on which slot.)
    static let noteColorBase: [String: NSColor] = [
        "c": NSColor(red: 255/255, green:  50/255, blue:  50/255, alpha: 1),
        "d": NSColor(red: 255/255, green: 160/255, blue:   0/255, alpha: 1),
        "e": NSColor(red: 255/255, green: 230/255, blue:   0/255, alpha: 1),
        "f": NSColor(red:  50/255, green: 200/255, blue:  50/255, alpha: 1),
        "g": NSColor(red:  50/255, green: 120/255, blue: 255/255, alpha: 1),
        "a": NSColor(red: 130/255, green:  50/255, blue: 200/255, alpha: 1),
        "b": NSColor(red: 180/255, green:  80/255, blue: 255/255, alpha: 1),
    ]
    static let noteColorDayglo: [String: NSColor] = [
        "c": NSColor(red: 255/255, green:  40/255, blue:  80/255, alpha: 1),
        "d": NSColor(red: 255/255, green: 180/255, blue:   0/255, alpha: 1),
        "e": NSColor(red: 255/255, green: 255/255, blue:  50/255, alpha: 1),
        "f": NSColor(red:  50/255, green: 255/255, blue: 100/255, alpha: 1),
        "g": NSColor(red:  50/255, green: 200/255, blue: 255/255, alpha: 1),
        "a": NSColor(red: 180/255, green:  50/255, blue: 255/255, alpha: 1),
        "b": NSColor(red: 255/255, green:  80/255, blue: 255/255, alpha: 1),
    ]
    static let noteColorMuted: [String: NSColor] = [
        "c": NSColor(red: 139/255, green:  26/255, blue:  26/255, alpha: 1),
        "d": NSColor(red: 180/255, green: 100/255, blue:   0/255, alpha: 1),
        "e": NSColor(red: 180/255, green: 150/255, blue:   0/255, alpha: 1),
        "f": NSColor(red:  20/255, green:  90/255, blue:  20/255, alpha: 1),
        "g": NSColor(red:  20/255, green:  60/255, blue: 120/255, alpha: 1),
        "a": NSColor(red:  50/255, green:   0/255, blue:  90/255, alpha: 1),
        "b": NSColor(red:  90/255, green:  30/255, blue: 150/255, alpha: 1),
    ]

    static func noteColor(midi: Int) -> NSColor {
        let octave = midi / 12 - 1
        let pc = ((midi % 12) + 12) % 12
        let letterNames = ["c","c","d","d","e","f","f","g","g","a","b","b"]
        let isSharp = [false,true,false,true,false,false,true,false,true,false,true,false][pc]
        if isSharp { return NSColor.darkGray }
        let key = letterNames[pc]
        if octave >= 5      { return noteColorDayglo[key] ?? .gray }
        else if octave <= 3 { return noteColorMuted[key]  ?? .gray }
        else                { return noteColorBase[key]   ?? .gray }
    }

    struct Syllable {
        let syl: String
        let sustain: Bool
    }

    // The parsed lyric for the current sample. nil = nothing to show.
    var syllables: [Syllable] = []

    func setLyric(_ lyric: String?) {
        guard let l = lyric, !l.isEmpty else { syllables = []; needsDisplay = true; return }
        syllables = Self.parse(l)
        needsDisplay = true
    }

    // Parse "i hope_ you get all the mo-ney you want" into syllables.
    //   space     = word boundary
    //   hyphen    = internal syllable split  ("mo-ney" → mo, ney)
    //   trailing _ = melisma (previous syllable holds an extra note)
    //   bare _    = explicit sustain placeholder
    static func parse(_ line: String) -> [Syllable] {
        var out: [Syllable] = []
        let tokens = line.trimmingCharacters(in: .whitespaces)
                         .split(whereSeparator: { $0.isWhitespace })
        for tok in tokens {
            let parts = tok.split(separator: "-")
            for p in parts {
                let s = String(p)
                if s == "_" || s == "·" {
                    out.append(Syllable(syl: "·", sustain: true))
                } else if s.hasSuffix("_") {
                    let core = String(s.dropLast())
                    out.append(Syllable(syl: core, sustain: false))
                    out.append(Syllable(syl: "·", sustain: true))
                } else {
                    out.append(Syllable(syl: s, sustain: false))
                }
            }
        }
        return out
    }

    static func midiLabel(_ m: Int) -> String {
        let n = noteNames[((m % 12) + 12) % 12]
        let oct = m / 12 - 1
        return "\(n)\(oct)"
    }

    override func draw(_ dirtyRect: NSRect) {
        let theme = self.theme
        let totalBeats = theme.reduce(0) { $0 + $1.beats }
        let pad: CGFloat = 8
        let usable = bounds.width - pad * 2
        // ── geometry ──────────────────────────────────────────────────
        //   header (top): bounds.height − 16
        //   note boxes : trackY 0.18 → top 0.78  (well clear of header)
        //   syllable   : just under the boxes
        //   index "0"  : removed (redundant with the natural left→right order)
        let trackY: CGFloat = bounds.height * 0.32
        let boxH: CGFloat   = bounds.height * 0.46
        let sylY: CGFloat   = trackY - 20
        let okCount = syllables.count == theme.count

        // background — subtle panel
        NSColor.controlBackgroundColor.withAlphaComponent(0.6).setFill()
        let bg = NSBezierPath(roundedRect: bounds.insetBy(dx: 2, dy: 2), xRadius: 6, yRadius: 6)
        bg.fill()

        // header text — "lyric ↔ note mapping (n/m syllables)"
        do {
            let style = NSMutableParagraphStyle(); style.alignment = .left
            let header = "lyric ↔ note alignment · \(syllables.count) syllables / \(theme.count) notes"
            let color: NSColor = okCount ? .systemGreen : .systemOrange
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 11, weight: .semibold),
                .foregroundColor: color,
                .paragraphStyle: style,
            ]
            (header as NSString).draw(at: NSPoint(x: pad + 4, y: bounds.height - 18), withAttributes: attrs)
        }

        noteRects.removeAll(keepingCapacity: true)
        var cursor: Double = 0
        for (i, n) in theme.enumerated() {
            let x = pad + CGFloat(cursor / totalBeats) * usable
            let w = CGFloat(n.beats / totalBeats) * usable - 3
            let boxRect = NSRect(x: x, y: trackY, width: w, height: boxH)
            noteRects.append((boxRect, self.rootMel + n.off, n.beats))
            let box = NSBezierPath(roundedRect: boxRect, xRadius: 4, yRadius: 4)
            // current syllable status: filled = has a non-sustain syl, hatched = sustain, empty = missing
            let s = i < syllables.count ? syllables[i] : nil
            let isMissing = s == nil
            let isSustain = s?.sustain ?? false
            // ── tint each box with the AC note-colour palette so the
            //     eye learns which chord-tone landed on which slot ──
            //   base = note's pitch-class color at a low alpha
            //   range selected → punchier alpha
            //   single-click   → punchier still
            //   missing / sustain markers shade the underlying tint
            let inRange: Bool = {
                guard let r = selectedRange() else { return false }
                return i >= r.start && i <= r.end
            }()
            let baseTint = Self.noteColor(midi: self.rootMel + n.off)
            let alpha: CGFloat
            if clickMode == .arm && armPressingIdx == i {
                // Recording NOW — bright red overlay regardless of pitch-class
                NSColor.systemRed.withAlphaComponent(0.75).setFill()
                box.fill()
                alpha = 0   // skip the pitch tint below
            } else if clickHighlightIdx == i { alpha = 0.72 }
            else if inRange                  { alpha = 0.55 }
            else if isMissing                { alpha = 0.12 }
            else if isSustain                { alpha = 0.22 }
            else                             { alpha = 0.32 }
            if alpha > 0 {
                baseTint.withAlphaComponent(alpha).setFill()
                box.fill()
            }
            NSColor.tertiaryLabelColor.setStroke()
            box.lineWidth = 1
            box.stroke()
            // ── 3-LAYER HAMBURGER STRIPES ───────────────────────────
            // Top → tone (blue), middle → arm (red), bottom → sample (green).
            // Stripes are always visible; the pressed stripe pulses bright.
            let stripes: [(zone: SubZone, color: NSColor)] = [
                (.tone,   .systemBlue),
                (.arm,    .systemRed),
                (.sample, .systemGreen),
            ]
            for (zone, color) in stripes {
                let r = stripeRect(in: boxRect, zone: zone)
                let isActive  = armPressingIdx == i && activeZone == zone
                let isFocused = selectedIdx == i && selectedSubZone == zone
                let alpha: CGFloat
                if isActive       { alpha = 0.85 }
                else if isFocused { alpha = 0.55 }
                else              { alpha = 0.30 }
                color.withAlphaComponent(alpha).setFill()
                NSBezierPath(rect: r).fill()
                // SAMPLE stripe → waveform thumbnail if a take exists.
                if zone == .sample, let peaks = noteWaveforms[i], !peaks.isEmpty {
                    let mid = r.midY
                    let halfH = (r.height - 2) / 2
                    let stride = r.width / CGFloat(peaks.count)
                    NSColor.labelColor.withAlphaComponent(0.65).setStroke()
                    let path = NSBezierPath()
                    path.lineWidth = 1
                    for (k, p) in peaks.enumerated() {
                        let x = r.minX + CGFloat(k) * stride + stride * 0.5
                        let h = halfH * CGFloat(min(1, max(0, p))) * 0.95
                        path.move(to: NSPoint(x: x, y: mid - h))
                        path.line(to: NSPoint(x: x, y: mid + h))
                    }
                    path.stroke()
                }
            }
            // Focus ring around the currently-selected stripe — the one
            // that ←/→/↑/↓ moves and that space will fire.
            if selectedIdx == i {
                let focusRect = stripeRect(in: boxRect, zone: selectedSubZone)
                NSColor.controlAccentColor.withAlphaComponent(0.95).setStroke()
                let sel = NSBezierPath(rect: focusRect.insetBy(dx: -0.5, dy: -0.5))
                sel.lineWidth = 2
                sel.stroke()
            }
            // Hover state — bolder outline so the user can see which
            // pad they're about to press.
            if hoverIdx == i && armPressingIdx != i {
                NSColor.labelColor.withAlphaComponent(0.55).setStroke()
                let hover = NSBezierPath(roundedRect: boxRect.insetBy(dx: 0.5, dy: 0.5),
                                          xRadius: 4, yRadius: 4)
                hover.lineWidth = 2
                hover.stroke()
            }

            // Progress bar — sweeps left→right inside the currently
            // sounding box so the eye has a metronome while the
            // selected range is auditioning.
            if playingIdx == i {
                let pw = boxRect.width * CGFloat(playingProgress)
                let progressRect = NSRect(x: boxRect.minX, y: boxRect.minY,
                                          width: pw, height: 4)
                NSColor.systemYellow.withAlphaComponent(0.95).setFill()
                NSBezierPath(roundedRect: progressRect, xRadius: 2, yRadius: 2).fill()
                // Also outline the playing box in yellow for emphasis.
                NSColor.systemYellow.withAlphaComponent(0.95).setStroke()
                let outline = NSBezierPath(roundedRect: boxRect.insetBy(dx: 0.5, dy: 0.5),
                                            xRadius: 4, yRadius: 4)
                outline.lineWidth = 1.8
                outline.stroke()
            }

            // ── unified typography ladder (all monospaced):
            //     index  · size 11 regular tertiary
            //     midi   · size 11 medium  label
            //     beats  · size 11 regular secondary
            //     syl    · size 11 semibold accent
            let midi = self.rootMel + n.off
            let cstyle = NSMutableParagraphStyle(); cstyle.alignment = .center
            // Use system semantic colors so labels adapt to light/dark
            // appearance automatically. labelColor + the tinted box at
            // 0.32 alpha gives a readable contrast in both themes.
            let lattrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 12, weight: .bold),
                .foregroundColor: NSColor.labelColor,
                .paragraphStyle: cstyle,
            ]
            let battrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 11, weight: .medium),
                .foregroundColor: NSColor.secondaryLabelColor,
                .paragraphStyle: cstyle,
            ]
            let ixattrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 11, weight: .regular),
                .foregroundColor: NSColor.tertiaryLabelColor,
                .paragraphStyle: cstyle,
            ]
            (Self.midiLabel(midi) as NSString).draw(
                in: NSRect(x: x, y: trackY + boxH * 0.55, width: w, height: 14),
                withAttributes: lattrs)
            ("\(n.beats)β" as NSString).draw(
                in: NSRect(x: x, y: trackY + boxH * 0.20, width: w, height: 14),
                withAttributes: battrs)
            _ = ixattrs   // (index labels removed — left-to-right order is already obvious)
            // syllable below box — semantic label colors so contrast
            // works in light AND dark mode. State conveyed by glyph
            // (— or ↳) + opacity, not raw hue.
            let sylText: String
            let sylColor: NSColor
            if let s = s {
                sylText = s.sustain ? "↳" : s.syl
                sylColor = s.sustain
                    ? NSColor.secondaryLabelColor
                    : NSColor.labelColor
            } else {
                sylText = "—"
                sylColor = NSColor.tertiaryLabelColor
            }
            let sylAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 12, weight: .semibold),
                .foregroundColor: sylColor,
                .paragraphStyle: cstyle,
            ]
            (sylText as NSString).draw(
                in: NSRect(x: x, y: sylY, width: w, height: 16),
                withAttributes: sylAttrs)
            cursor += n.beats
        }

        // ── recorded waveform overlay ───────────────────────────────
        if !recordedSamples.isEmpty {
            // If a range was supplied, paint only across those note
            // boxes so a partial take doesn't visually overflow the
            // notes it actually covers.
            let waveLeft: CGFloat
            let waveRight: CGFloat
            if let r = waveformRange,
               r.start < noteRects.count, r.end < noteRects.count {
                waveLeft  = noteRects[r.start].rect.minX
                waveRight = noteRects[r.end].rect.maxX
            } else {
                waveLeft  = pad
                waveRight = pad + usable
            }
            let waveW = waveRight - waveLeft
            let waveCY = trackY + boxH * 0.5
            let waveH = boxH * 0.85
            let n = recordedSamples.count
            // Min/max per pixel.
            NSColor.white.withAlphaComponent(0.78).setStroke()
            let path = NSBezierPath()
            path.lineWidth = 1.0
            let pixels = Int(waveW)
            for px in 0..<pixels {
                let i0 = Int(Double(px) / Double(pixels) * Double(n))
                let i1 = Int(Double(px + 1) / Double(pixels) * Double(n))
                guard i0 < n else { break }
                var mn: Float = 0, mx: Float = 0
                for j in i0..<min(i1, n) {
                    let v = recordedSamples[j]
                    if v < mn { mn = v }
                    if v > mx { mx = v }
                }
                let xp = waveLeft + CGFloat(px)
                let yt = waveCY + CGFloat(mx) * waveH * 0.5
                let yb = waveCY + CGFloat(mn) * waveH * 0.5
                path.move(to: NSPoint(x: xp, y: yt))
                path.line(to: NSPoint(x: xp, y: yb))
            }
            path.stroke()
        }

        // overflow syllables — drawn below as a horizontal list with "+" prefix
        if syllables.count > theme.count {
            let overflow = syllables[theme.count..<syllables.count].map { "+\($0.syl)" }.joined(separator: " ")
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 11, weight: .regular),
                .foregroundColor: NSColor.systemOrange,
            ]
            (overflow as NSString).draw(at: NSPoint(x: pad + 4, y: 2), withAttributes: attrs)
        }
    }
}
