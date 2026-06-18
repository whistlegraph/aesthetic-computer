import Foundation

/// A live, *conductible* musical engine.
///
/// Instead of scheduling a finite sequence and ending, the engine holds a
/// chord as a sustained pad and runs a repeating arpeggio + drum loop off a
/// single timer — indefinitely. The chord, the arpeggio, the drum pattern,
/// and the tempo can all be changed on the fly while the loop keeps running.
/// A chord change is *crossfaded*: the new chord is brought up on an idle
/// voice bank via Expression (CC 11) while the old bank fades out, so the
/// harmony **morphs** rather than cuts. The arpeggio reads the current chord
/// every step, so morphing the harmony evolves the melodic line for free.
///
/// You conduct it by posting `computer.aestheticcomputer.menuband.engine.*`
/// notifications (see AppDelegate) — start / chord / pattern / bpm / stop.
final class MenuBandEngine {
    private weak var mb: MenuBandController?
    init(menuBand: MenuBandController) { self.mb = menuBand }

    // Two pad banks so a chord change can crossfade A↔B. Arp on its own
    // channel. Channel 9 is GM percussion — left clear for the drum loop.
    private let padA: [UInt8] = [0, 1, 2]
    private let padB: [UInt8] = [3, 4, 5]
    private let arpCh: UInt8 = 6
    private var useA = true                     // which bank currently sounds

    private(set) var running = false
    private var bpm = 110.0
    private var stepBeats = 0.5                 // arp/drum grid (0.5 = eighths)
    private var chord: [UInt8] = [60, 64, 67]
    private var arp: [Int] = [0, 1, 2, 1]       // chord-tone indices, -1 = rest
    private var arpOctave = 12                  // arp sits an octave above the pad
    private var drums: [String] = []            // per-step letters: k/s/h/c, "" = rest
    private var program: UInt8 = 89             // warm pad
    private var padVelocity: UInt8 = 70
    private var step = 0
    private var timer: Timer?
    private var heldPad: [(note: UInt8, channel: UInt8)] = []
    private var arpHeld: UInt8?

    // MARK: - Lifecycle

    func start(bpm: Double, chord: [UInt8], program: UInt8,
               arp: [Int], stepBeats: Double, drums: [String]) {
        stop(fade: 0)
        self.bpm = bpm
        self.chord = chord.isEmpty ? [60, 64, 67] : chord
        self.program = program
        self.arp = arp.isEmpty ? [0, 1, 2, 1] : arp
        self.stepBeats = stepBeats > 0 ? stepBeats : 0.5
        self.drums = drums
        self.step = 0
        self.useA = true
        mb?.engineSetProgram(program)
        soundPad(self.chord, bank: padA)
        running = true
        restartTimer()
    }

    func stop(fade: Double = 0.6) {
        running = false
        timer?.invalidate(); timer = nil
        if let prev = arpHeld { mb?.engineVoiceOff(prev, channel: arpCh); arpHeld = nil }
        let held = heldPad
        heldPad = []
        if fade <= 0 {
            for v in held { mb?.engineVoiceOff(v.note, channel: v.channel) }
            return
        }
        let steps = 12
        for s in 0...steps {
            let frac = Double(s) / Double(steps)
            DispatchQueue.main.asyncAfter(deadline: .now() + fade * frac) { [weak self] in
                let e = UInt8((1 - frac) * 127)
                for v in held { self?.mb?.engineExpression(e, channel: v.channel) }
            }
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + fade + 0.05) { [weak self] in
            for v in held { self?.mb?.engineVoiceOff(v.note, channel: v.channel) }
        }
    }

    // MARK: - Live mutation

    /// Morph the held chord to a new voicing, crossfading onto the idle pad
    /// bank over `glide` seconds. The arpeggio picks up the new chord tones
    /// on its next step automatically.
    func morph(toChord newChord: [UInt8], glide: Double) {
        guard !newChord.isEmpty else { return }
        guard running else {
            start(bpm: bpm, chord: newChord, program: program,
                  arp: arp, stepBeats: stepBeats, drums: drums)
            return
        }
        let fromBank = useA ? padA : padB
        let toBank   = useA ? padB : padA

        // Bring the new chord up silently on the idle bank.
        var newHeld: [(note: UInt8, channel: UInt8)] = []
        for (i, n) in newChord.enumerated() where i < toBank.count {
            mb?.engineExpression(0, channel: toBank[i])
            mb?.engineVoiceOn(n, channel: toBank[i], velocity: padVelocity)
            newHeld.append((n, toBank[i]))
        }
        // Equal-ish crossfade via Expression ramps.
        let g = max(0.05, glide)
        let steps = max(2, Int(g / 0.03))
        for s in 0...steps {
            let frac = Double(s) / Double(steps)
            DispatchQueue.main.asyncAfter(deadline: .now() + g * frac) { [weak self] in
                guard let self = self else { return }
                let up = UInt8(frac * 127), down = UInt8((1 - frac) * 127)
                for ch in toBank { self.mb?.engineExpression(up, channel: ch) }
                for ch in fromBank { self.mb?.engineExpression(down, channel: ch) }
            }
        }
        let oldHeld = heldPad
        DispatchQueue.main.asyncAfter(deadline: .now() + g + 0.05) { [weak self] in
            guard let self = self else { return }
            for v in oldHeld where fromBank.contains(v.channel) {
                self.mb?.engineVoiceOff(v.note, channel: v.channel)
            }
        }
        heldPad = newHeld
        chord = newChord
        useA.toggle()
    }

    func setArp(_ a: [Int]) { if !a.isEmpty { arp = a } }
    func setDrums(_ d: [String]) { drums = d }
    func setBPM(_ b: Double) { bpm = max(20, b); if running { restartTimer() } }
    func setStepBeats(_ s: Double) { if s > 0 { stepBeats = s; if running { restartTimer() } } }

    // MARK: - Internals

    private func soundPad(_ notes: [UInt8], bank: [UInt8]) {
        for (i, n) in notes.enumerated() where i < bank.count {
            mb?.engineExpression(127, channel: bank[i])
            mb?.engineVoiceOn(n, channel: bank[i], velocity: padVelocity)
            heldPad.append((n, bank[i]))
        }
    }

    private func restartTimer() {
        timer?.invalidate()
        let interval = stepBeats * 60.0 / max(1, bpm)
        let t = Timer(timeInterval: interval, repeats: true) { [weak self] _ in self?.tick() }
        RunLoop.main.add(t, forMode: .common)
        timer = t
    }

    private func tick() {
        // Arpeggio — reads the *current* chord, so it follows morphs live.
        let idx = arp[step % arp.count]
        if idx >= 0, !chord.isEmpty {
            if let prev = arpHeld { mb?.engineVoiceOff(prev, channel: arpCh) }
            let tone = Int(chord[((idx % chord.count) + chord.count) % chord.count])
            let note = UInt8(max(0, min(127, tone + arpOctave)))
            mb?.engineVoiceOn(note, channel: arpCh, velocity: 96)
            arpHeld = note
        }
        // Drum loop.
        if !drums.isEmpty {
            let token = drums[step % drums.count]
            let drum: MenuBandPercussion.Drum? = {
                switch token {
                case "k": return .kick
                case "s": return .snare
                case "h": return .hatClosed
                case "ho": return .hatOpen
                case "c": return .clap
                default: return nil
                }
            }()
            if let drum = drum {
                mb?.engineDrum(drum, velocity: token == "h" ? 60 : 116)
            }
        }
        step += 1
    }
}
