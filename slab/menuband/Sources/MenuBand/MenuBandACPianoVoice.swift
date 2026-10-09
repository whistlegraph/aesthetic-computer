import AVFoundation
import Foundation

/// The AC grand piano — the first custom instrument (` then 1).
///
/// Plays the same sample bank AC OS plays: the Salamander Grand Piano V3
/// anchors in `fedac/native/samples/piano` (CC0, Alexander Holm), 26 notes
/// every three semitones as header-less float32 mono at 48 kHz. And it plays
/// them the way `audio.c` does — nearest anchor by MIDI distance, playback
/// step `2^(semitones/12)`, linear interpolation, the note running out at
/// the end of its own recorded decay — so a C4 here is the C4 the native
/// `piano` wave gives. Key-up adds the one thing a struck sample needs from
/// the player: a damper, a short exponential release.
///
/// Same chassis as `MenuBandFluoddityVoice`: an `AVAudioSourceNode` whose
/// render callback owns a fixed voice pool; the control thread stages note
/// intent through a tiny critical section; the render thread drains it at
/// the top of each block. The bank is loaded once, on the control thread,
/// before the first note can route here.
final class MenuBandACPianoVoice {
    // MARK: Bank

    /// One anchor: its MIDI pitch and the recorded decay. The buffer lives
    /// for the process — the render thread reads it by pointer.
    private struct Anchor {
        let midi: Int
        let data: UnsafeMutablePointer<Float>
        let count: Int
    }
    /// Written once by `ensureLoaded` under `lock`; the render thread takes
    /// a snapshot of the array under the same lock each block.
    private var anchors: [Anchor] = []
    private var loadAttempted = false
    /// Anchors were recorded at 48 kHz; the engine may run elsewhere.
    private let bankRate: Double = 48_000

    // MARK: Voice state (render thread owns the active pool)

    private struct Voice {
        var midi: UInt8 = 0
        var channel: UInt8 = 0
        var anchor: Int = -1
        var pos: Double = 0
        var step: Double = 1
        var gain: Float = 0
        var panL: Float = 0.7071
        var panR: Float = 0.7071
        /// Damper multiplier: 1 while held, decaying after key-up.
        var env: Float = 1
        var releasing = false
        var active = false
    }

    // MARK: Audio graph

    private var sampleRate: Double = 48_000
    private var format: AVAudioFormat!
    private var sourceNode: AVAudioSourceNode!
    private weak var engine: AVAudioEngine?
    private var attached = false

    /// Salamander's mezzo layer sits several dB under the ±1 oscillators;
    /// AC OS lifts it 3× then soft-clips. Same idea here, a touch gentler
    /// because the GM voices this sits beside are quieter than notepat's
    /// raw waves, with a tanh on the sum catching chord peaks.
    private let voiceAmp: Float = 2.4
    private let masterGain: Float = 0.9
    /// Damper time constant. −40 dB in about a third of a second — a felt
    /// damper landing, not a cut.
    private let releaseSeconds: Double = 0.07
    private var releaseCoeff: Float = 0.999

    private let maxVoices = 24
    private var voices: [Voice]

    /// Trackpad bend target + render-thread glide, same scheme (and time
    /// constant) as the GM node and the Fluoddity voice.
    private var pitchScale: Double = 1.0
    private var glidePitch: Double = 1.0
    private var pitchGlideCoeff: Double = 1.0 - exp(-1.0 / (48_000 * 0.004))

    /// Per-channel CC10 pan (0–127, 64 = center), stamped onto a voice at
    /// note-on so each key keeps the column pan the keyboard gives it.
    private var channelPan: [UInt8] = [UInt8](repeating: 64, count: 16)

    // MARK: Control → render handoff

    private enum Command {
        case noteOn(midi: UInt8, channel: UInt8, velocity: UInt8, pan: UInt8)
        case noteOff(midi: UInt8, channel: UInt8)
        case panic
    }
    private var pending: [Command] = []
    private let lock = NSLock()

    init() {
        voices = [Voice](repeating: Voice(), count: maxVoices)
        pending.reserveCapacity(64)
    }

    deinit {
        for a in anchors { a.data.deallocate() }
    }

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        let outRate = engine.outputNode.outputFormat(forBus: 0).sampleRate
        sampleRate = outRate > 0 ? outRate : 48_000
        pitchGlideCoeff = 1.0 - exp(-1.0 / (sampleRate * 0.004))
        releaseCoeff = Float(exp(-1.0 / (sampleRate * releaseSeconds)))
        format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                               channels: 2)!
        sourceNode = AVAudioSourceNode(format: format) {
            [weak self] _, _, frameCount, ablPointer -> OSStatus in
            self?.render(frameCount: Int(frameCount), abl: ablPointer)
            return noErr
        }
        engine.attach(sourceNode)
        engine.connect(sourceNode, to: output, format: format)
        attached = true
    }

    // MARK: Bank loading (control thread)

    /// True once at least one anchor is in memory.
    var isLoaded: Bool {
        lock.lock(); defer { lock.unlock() }
        return !anchors.isEmpty
    }

    /// Where the bank lives. The installed app carries the samples in
    /// `Contents/Resources/acpiano/` (install.sh copies them from
    /// fedac/native/samples/piano — one source of truth, no second copy in
    /// git); the Xcode store target copies the folder as `piano/`; a flat
    /// Resources root is accepted too. A `swift run` dev build has none of
    /// those, so it reads the repo's bank directly.
    static func bankURLs() -> [URL] {
        let bundle = Bundle.appResources
        for sub in ["acpiano", "piano", nil] {
            if let urls = bundle.urls(forResourcesWithExtension: "raw",
                                      subdirectory: sub),
               urls.contains(where: { Int($0.deletingPathExtension().lastPathComponent) != nil }) {
                return urls
            }
        }
        var dev = URL(fileURLWithPath: #filePath)
        for _ in 0..<5 { dev.deleteLastPathComponent() }   // → repo root
        dev.appendPathComponent("fedac/native/samples/piano")
        let listed = (try? FileManager.default.contentsOfDirectory(
            at: dev, includingPropertiesForKeys: nil)) ?? []
        return listed.filter { $0.pathExtension == "raw" }
    }

    /// Read the anchors into memory. Idempotent; a few milliseconds for the
    /// 14 MB bank. Called before the voice is routed to, never from audio.
    func ensureLoaded() {
        lock.lock()
        let done = loadAttempted
        loadAttempted = true
        lock.unlock()
        if done { return }
        var loaded: [Anchor] = []
        for url in Self.bankURLs() {
            guard let midi = Int(url.deletingPathExtension().lastPathComponent),
                  (0...127).contains(midi),
                  let data = try? Data(contentsOf: url),
                  data.count >= MemoryLayout<Float>.size * 2,
                  data.count % MemoryLayout<Float>.size == 0 else { continue }
            let count = data.count / MemoryLayout<Float>.size
            let buf = UnsafeMutablePointer<Float>.allocate(capacity: count)
            data.withUnsafeBytes { raw in
                buf.initialize(from: raw.bindMemory(to: Float.self).baseAddress!,
                               count: count)
            }
            loaded.append(Anchor(midi: midi, data: buf, count: count))
        }
        loaded.sort { $0.midi < $1.midi }
        lock.lock()
        anchors = loaded
        lock.unlock()
        NSLog("MenuBand ACPiano: loaded %d anchors", loaded.count)
    }

    // MARK: Public API (control thread)

    func setPan(_ pan: UInt8, channel: UInt8) {
        lock.lock()
        channelPan[Int(channel & 0x0F)] = pan & 0x7F
        lock.unlock()
    }

    func setPitchBend(amount: Float) {
        pitchScale = pow(2.0, Double(amount))
    }

    func noteOn(_ midi: UInt8, velocity: UInt8, channel: UInt8) {
        lock.lock()
        let pan = channelPan[Int(channel & 0x0F)]
        pending.append(.noteOn(midi: midi, channel: channel, velocity: velocity, pan: pan))
        lock.unlock()
    }

    func noteOff(_ midi: UInt8, channel: UInt8) {
        lock.lock()
        pending.append(.noteOff(midi: midi, channel: channel))
        lock.unlock()
    }

    func panic() {
        lock.lock()
        pending.append(.panic)
        lock.unlock()
    }

    // MARK: Helpers

    /// Nearest anchor by MIDI distance (audio.c's `pick_piano_anchor`).
    private static func nearestAnchor(to midi: Int, in bank: [Anchor]) -> Int {
        var best = -1
        var bestDist = Int.max
        for (i, a) in bank.enumerated() {
            let d = abs(midi - a.midi)
            if d < bestDist { bestDist = d; best = i }
        }
        return best
    }

    private func allocateVoice() -> Int {
        for i in 0..<maxVoices where !voices[i].active { return i }
        // Steal the quietest: a releasing voice first, then the one
        // furthest into its decay.
        var best = 0
        var bestScore = Float.greatestFiniteMagnitude
        for i in 0..<maxVoices {
            let score = (voices[i].releasing ? 0 : 1_000) + voices[i].env
            if score < bestScore { bestScore = score; best = i }
        }
        return best
    }

    // MARK: Render thread

    private func render(frameCount: Int, abl: UnsafeMutablePointer<AudioBufferList>) {
        let buffers = UnsafeMutableAudioBufferListPointer(abl)
        let left = buffers[0].mData!.assumingMemoryBound(to: Float.self)
        let right = (buffers.count > 1 ? buffers[1].mData! : buffers[0].mData!)
            .assumingMemoryBound(to: Float.self)
        for i in 0..<frameCount { left[i] = 0; right[i] = 0 }

        lock.lock()
        let cmds = pending
        if !pending.isEmpty { pending.removeAll(keepingCapacity: true) }
        let bank = anchors
        lock.unlock()

        for cmd in cmds {
            switch cmd {
            case let .noteOn(midi, channel, velocity, pan):
                guard !bank.isEmpty else { continue }
                let slot = allocateVoice()
                let a = Self.nearestAnchor(to: Int(midi), in: bank)
                guard a >= 0 else { continue }
                let semis = Double(Int(midi) - bank[a].midi)
                voices[slot].midi = midi
                voices[slot].channel = channel
                voices[slot].anchor = a
                voices[slot].pos = 0
                voices[slot].step = pow(2.0, semis / 12.0) * (bankRate / sampleRate)
                voices[slot].gain = voiceAmp * max(0.05, min(1.0, Float(velocity) / 127.0))
                // Constant-power pan from CC10 (64 = center).
                let p = Float(pan) / 127.0 * (.pi / 2)
                voices[slot].panL = cos(p)
                voices[slot].panR = sin(p)
                voices[slot].env = 1
                voices[slot].releasing = false
                voices[slot].active = true
            case let .noteOff(midi, channel):
                for i in 0..<maxVoices where
                    voices[i].active && !voices[i].releasing
                    && voices[i].midi == midi && voices[i].channel == channel {
                    voices[i].releasing = true
                }
            case .panic:
                for i in 0..<maxVoices { voices[i].active = false }
            }
        }

        let pitchStart = glidePitch
        let pitchTarget = pitchScale
        let pitchCoeff = pitchGlideCoeff
        let rel = releaseCoeff

        for idx in 0..<maxVoices where voices[idx].active {
            let a = voices[idx].anchor
            guard a >= 0, a < bank.count else { voices[idx].active = false; continue }
            let data = bank[a].data
            let last = bank[a].count - 1
            let baseStep = voices[idx].step
            let gain = voices[idx].gain
            let pl = voices[idx].panL, pr = voices[idx].panR
            let releasing = voices[idx].releasing
            var pos = voices[idx].pos
            var env = voices[idx].env
            var active = true
            var pitch = pitchStart
            for i in 0..<frameCount {
                pitch += (pitchTarget - pitch) * pitchCoeff
                let ip = Int(pos)
                if ip >= last {
                    // Ran out its own decay — the note is over.
                    active = false
                    break
                }
                let frac = Float(pos - Double(ip))
                let s = data[ip] + (data[ip + 1] - data[ip]) * frac
                if releasing {
                    env *= rel
                    if env < 0.001 { active = false; break }
                }
                let v = s * gain * env
                left[i] += v * pl
                right[i] += v * pr
                pos += baseStep * pitch
            }
            voices[idx].pos = pos
            voices[idx].env = env
            voices[idx].active = active
        }
        glidePitch = pitchStart + (pitchTarget - pitchStart)
            * (1 - pow(1 - pitchCoeff, Double(frameCount)))

        // Soft-clip the sum so a lifted chord can't slam the limiter.
        let g = masterGain
        for i in 0..<frameCount {
            left[i] = tanh(left[i] * g)
            right[i] = tanh(right[i] * g)
        }
    }
}
