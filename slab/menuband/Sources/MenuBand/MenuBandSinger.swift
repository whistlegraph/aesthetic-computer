// MenuBandSinger.swift — the machine speaks, and the speech is sung.
//
// A `.play` payload that carries `lyrics` is a SUNG voice. This is Menu
// Band's host for live/singer.c SCORE mode, ported from live/hosts/macos/
// livesing.swift: speech is rendered to PCM without touching the speaker —
// AVSpeechSynthesizer.write in the named system voice, or a pre-recorded stem
// plus whisper word timings (how neo sings in jeffrey's own cloned voice,
// offline) — WORLD analyzes it, and the core stretches every syllable onto
// the score and replaces the pitch with the notes. The formant envelope is
// untouched, so it is still THAT voice singing.
//
// `MenuBandSinger` is the pipeline: ~0.5–3 s of work on its own serial queue
// (the core's render scratch is static, so one render at a time).
// `MenuBandSingerVoice` is the AVAudioPlayerNode it sounds through — hung on
// the synth's pre-limiter bus so a sung line rides the same bend/space/echo
// as the whistle, and scheduled on the engine's sample clock so it lands on
// the shared downbeat exactly like the notes do.

import AVFoundation
import CSinger

/// One sung line, parsed from a `.play` payload.
/// lyrics: one token per note; syllables of a word joined by "-" (fas-ter).
/// notes:  .mbscore grammar, `midi:beats` and `r:beats`.
struct SungLine {
    var lyrics: String
    var text: String?            // what the voice says (default: lyrics minus "-")
    var notes: String
    var bpm: Double
    var voice = "Fred"
    var lock = 0.875, vibHz = 5.0, vibCents = 18.0, f0Floor = 55.0
    var stemPath: String?, wordsPath: String?

    init?(info: [String: String], bpm: Double) {
        guard let l = info["lyrics"], !l.isEmpty, let n = info["notes"] else { return nil }
        lyrics = l; notes = n; self.bpm = bpm
        text = info["text"]
        voice = info["singVoice"] ?? voice
        lock = Double(info["singLock"] ?? "") ?? lock
        vibHz = Double(info["singVibratoHz"] ?? "") ?? vibHz
        vibCents = Double(info["singVibCents"] ?? "") ?? vibCents
        f0Floor = Double(info["singF0Floor"] ?? "") ?? f0Floor
        stemPath = info["stemPath"]; wordsPath = info["wordsPath"]
    }

    /// Lyric tokens, without the "/" line breaks.
    var tokens: [String] { lyrics.split(separator: " ").map(String.init).filter { $0 != "/" } }
    /// Tokens grouped by line: "/" in the lyrics is a line break — one
    /// caption's worth; the composer puts one at every phrase.
    var lineTokens: [[String]] {
        var out: [[String]] = [[]]
        for t in lyrics.split(separator: " ").map(String.init) {
            if t == "/" { if !out[out.count - 1].isEmpty { out.append([]) } }
            else { out[out.count - 1].append(t) }
        }
        return out.filter { !$0.isEmpty }
    }
    var spoken: String { text ?? tokens.joined(separator: " ").replacingOccurrences(of: "-", with: "") }
    /// Words as TTS says them, with how many sung units each splits into.
    var words: [(text: String, nsyl: Int)] {
        tokens.map { t in
            let parts = t.split(separator: "-").map(String.init)
            return (parts.joined(), parts.count)
        }
    }

    /// One SungLine per "/"-separated line, each carrying only its own notes —
    /// led by a rest so its timing stays absolute — so a long lyric (a ballad)
    /// renders line by line instead of as one utterance: each line is ready
    /// well before its own downbeat, and the core's per-render limits (500
    /// words / 500 notes) never bind. A single-line lyric comes back as is.
    func splitLines() -> [SungLine] {
        let lines = lineTokens
        guard lines.count > 1 else { return [self] }
        var pos = 0.0
        var abs: [(tok: String, at: Double, dur: Double, rest: Bool)] = []
        for tok in notes.split(separator: ",") {
            let kv = tok.split(separator: ":")
            let d = kv.count > 1 ? (Double(kv[1]) ?? 1) : 1
            abs.append((String(tok), pos, d, kv.first == "r"))
            pos += d
        }
        let sounding = abs.filter { !$0.rest }
        var out: [SungLine] = []
        var cursor = 0
        for lt in lines {
            let nsyl = lt.reduce(0) { $0 + $1.split(separator: "-").count }
            let hi = min(cursor + nsyl, sounding.count)
            guard cursor < hi else { break }
            let slice = sounding[cursor..<hi]
            cursor = hi
            let start = slice.first!.at, end = slice.last!.at + slice.last!.dur
            var toks: [String] = []
            if start > 0 { toks.append("r:" + String(format: "%g", start)) }
            for n in abs where n.at >= start && n.at < end { toks.append(n.tok) }
            var l = self
            l.lyrics = lt.joined(separator: " ")
            l.notes = toks.joined(separator: ",")
            l.text = nil
            out.append(l)
        }
        return out
    }
}

/// A rendered line, ready for the player: mono in the voice's graph format.
struct SungRender {
    let buffer: AVAudioPCMBuffer
    let spanOffset: Double       // seconds from the downbeat to buffer[0]
    let notesUsed: Int, noteCount: Int
    let peak: Float
    var duration: Double { Double(buffer.frameLength) / buffer.format.sampleRate }
}

final class MenuBandSinger {
    private let queue = DispatchQueue(label: "menuband.singer", qos: .userInitiated)
    private let synth = AVSpeechSynthesizer()

    /// Render `line` off the main thread; `completion` lands on main with nil
    /// when the speech source or the core gave nothing to sing.
    func render(_ line: SungLine, into format: AVAudioFormat,
                completion: @escaping (SungRender?) -> Void) {
        queue.async {
            let r = self.renderNow(line, format: format)
            DispatchQueue.main.async { completion(r) }
        }
    }

    // MARK: - speech sources

    private struct Speech {
        var pcm: [Double]
        var fs: Int
        var spans: [(a: Int, b: Int)]   // word spans in samples, lyric order
    }

    /// Captures AVSpeechSynthesizer.write: PCM plus each word's onset sample.
    private final class Capture: NSObject, AVSpeechSynthesizerDelegate {
        let lock = NSLock()
        var pcm: [Double] = []
        var fs = 22_050.0
        var onsets: [Int] = []
        let done = DispatchSemaphore(value: 0)

        func speechSynthesizer(_ s: AVSpeechSynthesizer,
                               willSpeakRangeOfSpeechString r: NSRange,
                               utterance: AVSpeechUtterance) {
            lock.lock(); onsets.append(pcm.count); lock.unlock()
        }
        func append(_ b: AVAudioPCMBuffer) {
            lock.lock(); defer { lock.unlock() }
            fs = b.format.sampleRate
            let n = Int(b.frameLength)
            if let ch = b.floatChannelData {
                for i in 0..<n { pcm.append(Double(ch[0][i])) }
            } else if let ch = b.int16ChannelData {
                for i in 0..<n { pcm.append(Double(ch[0][i]) / 32768.0) }
            }
        }
    }

    /// Voice by bare name, preferring the best installed tier (Premium >
    /// Enhanced > compact) — "Ava" picks "Ava (Premium)" once downloaded. An
    /// explicit "Evan (Enhanced)" pins a tier; an identifier matches exactly.
    static func voice(named raw: String) -> AVSpeechSynthesisVoice? {
        let tier = #" \((Premium|Enhanced)\)"#
        let bare = { (s: String) in
            s.replacingOccurrences(of: tier, with: "", options: .regularExpression).lowercased()
        }
        let all = AVSpeechSynthesisVoice.speechVoices()
        if let v = all.first(where: { $0.identifier == raw }) { return v }
        let want = bare(raw)
        let pin: Int? = raw.contains("(Premium)") ? 3 : raw.contains("(Enhanced)") ? 2 : nil
        let rank = { (v: AVSpeechSynthesisVoice) in
            v.quality.rawValue * 2 + (v.language.hasPrefix("en-US") ? 1 : 0)
        }
        return all.filter { bare($0.name) == want && (pin == nil || $0.quality.rawValue == pin) }
            .max { rank($0) < rank($1) }
    }

    private func speak(_ line: SungLine, words: [(text: String, nsyl: Int)]) -> Speech? {
        let cap = Capture()
        synth.delegate = cap
        let utt = AVSpeechUtterance(string: line.spoken)
        let v = MenuBandSinger.voice(named: line.voice)
        utt.voice = v
        utt.rate = 0.42
        synth.write(utt) { buf in
            guard let b = buf as? AVAudioPCMBuffer, b.frameLength > 0 else { cap.done.signal(); return }
            cap.append(b)
        }
        // The synthesizer calls back on its own queue; a wedged voice must
        // not hold the render queue forever.
        guard cap.done.wait(timeout: .now() + 20) == .success else {
            NSLog("🎤 sing: %@ never finished speaking", line.voice); return nil
        }
        cap.lock.lock(); defer { cap.lock.unlock() }
        guard !cap.pcm.isEmpty else { NSLog("🎤 sing: %@ spoke nothing", line.voice); return nil }
        let n = cap.pcm.count, nw = words.count
        var spans: [(a: Int, b: Int)] = []
        if cap.onsets.count >= nw {
            // word onsets from the synthesizer; a word ends where the next begins
            let on = Array(cap.onsets.prefix(nw))
            for i in 0..<nw { spans.append((on[i], i + 1 < nw ? on[i + 1] : n)) }
        } else {
            // no ranges: split evenly — the core finds the nuclei
            for i in 0..<nw { spans.append((n * i / nw, n * (i + 1) / nw)) }
        }
        let tierName = ["", "compact", "Enhanced", "Premium"]
        NSLog("🎤 sing: spoke %d samples @%.0f Hz in %@ [%@], %d/%d word ranges",
              n, cap.fs, v?.name ?? "default",
              tierName[min(3, v?.quality.rawValue ?? 1)], cap.onsets.count, nw)
        return Speech(pcm: cap.pcm, fs: Int(cap.fs), spans: spans)
    }

    /// The offline route: a spoken stem (any AVAudioFile format) and whisper
    /// words `[{text, fromMs, toMs}]` from `pop/bin/align.mjs`. Whisper's
    /// words pair with the lyric words in order; a word ends where the next
    /// begins (like the synthesizer route), capped so a long pause isn't sung.
    private func load(_ line: SungLine, words: [(text: String, nsyl: Int)]) -> Speech? {
        guard let stem = line.stemPath, let wp = line.wordsPath else { return nil }
        guard let file = try? AVAudioFile(forReading: URL(fileURLWithPath: stem)),
              let buf = AVAudioPCMBuffer(pcmFormat: file.processingFormat,
                                         frameCapacity: AVAudioFrameCount(file.length)),
              (try? file.read(into: buf)) != nil, let ch = buf.floatChannelData
        else { NSLog("🎤 sing: can't read stem %@", stem); return nil }
        let n = Int(buf.frameLength), nch = Int(buf.format.channelCount)
        var pcm = [Double](repeating: 0, count: n)
        for c in 0..<nch { for i in 0..<n { pcm[i] += Double(ch[c][i]) / Double(nch) } }
        let fs = Int(file.processingFormat.sampleRate)

        // A bare `[{text,fromMs,toMs}]` (align.mjs) or `{words: [...]}` (an
        // elevenlabs alignment) — the stem may hold several lines, so find
        // THIS lyric's words inside it before pairing.
        guard let data = FileManager.default.contents(atPath: wp),
              let root = try? JSONSerialization.jsonObject(with: data),
              let json = (root as? [[String: Any]])
                ?? ((root as? [String: Any])?["words"] as? [[String: Any]])
        else { NSLog("🎤 sing: can't read words %@", wp); return nil }
        let heard: [(from: Int, to: Int, text: String)] = json.compactMap { w in
            guard let f = w["fromMs"] as? Double, let t = w["toMs"] as? Double else { return nil }
            return (Int(f / 1000 * Double(fs)), Int(t / 1000 * Double(fs)), w["text"] as? String ?? "")
        }
        let norm = { (s: String) in s.lowercased().filter { $0.isLetter || $0.isNumber } }
        let want = words.map { norm($0.text) }
        let nw = words.count
        var start = 0
        if heard.count >= nw,
           let i = (0...(heard.count - nw)).first(where: { i in
               (0..<nw).allSatisfy { norm(heard[i + $0].text) == want[$0] }
           }) {
            start = i
        } else {
            NSLog("🎤 sing: lyric not found verbatim in %d transcript words — pairing from the top", heard.count)
        }
        let window = Array(heard[start..<min(start + nw, heard.count)])
        let after = start + window.count < heard.count ? heard[start + window.count].from : n
        var spans: [(a: Int, b: Int)] = []
        for (i, w) in window.enumerated() {
            let next = i + 1 < window.count ? window[i + 1].from : after
            spans.append((w.from, min(next, w.to + fs * 3 / 10)))
        }
        // lyric words the transcript lacks: split what's left evenly
        let tail0 = spans.last?.b ?? 0, rest = nw - window.count
        for i in 0..<max(0, rest) {
            spans.append((tail0 + (n - tail0) * i / rest, tail0 + (n - tail0) * (i + 1) / rest))
        }
        NSLog("🎤 sing: stem %@ — %d samples @%d Hz; lyric = transcript words %d…%d of %d (%.2f–%.2f s)",
              (stem as NSString).lastPathComponent, n, fs, start, start + window.count - 1, heard.count,
              Double(spans.first?.a ?? 0) / Double(fs), Double(spans.last?.b ?? 0) / Double(fs))
        return Speech(pcm: pcm, fs: fs, spans: spans)
    }

    // MARK: - the core

    private func renderNow(_ line: SungLine, format: AVAudioFormat) -> SungRender? {
        let t0 = Date()
        let words = line.words
        guard !words.isEmpty else { return nil }

        var notes: [singer_note] = []
        var beat = 0.0
        for tok in line.notes.split(separator: ",") {
            let kv = tok.split(separator: ":")
            let d = kv.count > 1 ? (Double(kv[1]) ?? 1) : 1
            if kv[0] != "r", let m = Double(kv[0]) {
                notes.append(singer_note(midi: m, at16: beat * 4, dur16: d * 4))
            }
            beat += d
        }
        guard !notes.isEmpty else { return nil }
        let nsyl = words.reduce(0) { $0 + $1.nsyl }
        if notes.count != nsyl {
            NSLog("🎤 sing: %d notes vs %d syllables — pairing in order", notes.count, nsyl)
        }

        guard let sp = line.stemPath != nil ? load(line, words: words) : speak(line, words: words)
        else { return nil }

        // word spans → WORLD frames
        let frameOf = { (sample: Int) -> Int32 in
            Int32(Double(sample) / Double(sp.fs) * 1000.0 / SINGER_FP_MS)
        }
        var W: [singer_word] = []
        for (i, s) in sp.spans.enumerated() {
            var w = singer_word()
            w.a = frameOf(s.a); w.b = frameOf(s.b); w.vs = w.a; w.ve = w.b
            w.nsyl = Int32(words[i].nsyl)
            W.append(w)
        }

        let pcm = sp.pcm
        guard let S = pcm.withUnsafeBufferPointer({
            singer_create($0.baseAddress, Int32(pcm.count), Int32(sp.fs))
        }) else { return nil }
        defer { singer_destroy(S) }
        let P = singer_params_ptr(S)!
        P.pointee.bpm = line.bpm; P.pointee.morph = 1.0; P.pointee.mode = SINGER_SCORE
        P.pointee.lock = line.lock; P.pointee.vib_hz = line.vibHz
        P.pointee.vib_cents = line.vibCents; P.pointee.f0_floor = line.f0Floor
        W.withUnsafeBufferPointer { singer_set_words(S, $0.baseAddress, Int32(W.count)) }
        notes.withUnsafeBufferPointer { singer_set_score(S, $0.baseAddress, Int32(notes.count)) }
        let ta = Date()
        singer_analyze_chunk(S, 0, singer_total_frames(S))
        let analyzeMs = Date().timeIntervalSince(ta) * 1000

        // Render only the span that sings — from the beat before the first
        // note to the beat after the last — and offset playback by its start.
        let first16 = max(0, floor(notes[0].at16 / 4) * 4 - 4)
        let end16 = notes.map { $0.at16 + $0.dur16 }.max() ?? 0
        let last16 = min(beat * 4, ceil(end16 / 4) * 4 + 4)
        let spanOffset = first16 / 4 * (60.0 / line.bpm)
        var outLen: Int32 = 0, used: Int32 = 0
        let tr = Date()
        guard let y = singer_render_score(S, first16, last16, &outLen, &used), outLen > 0
        else { NSLog("🎤 sing: render gave nothing"); return nil }
        defer { free(y) }
        let n = Int(outLen)
        var peak: Float = 0
        let src = AVAudioFormat(commonFormat: .pcmFormatFloat32, sampleRate: Double(sp.fs),
                                channels: 1, interleaved: false)!
        guard let raw = AVAudioPCMBuffer(pcmFormat: src, frameCapacity: AVAudioFrameCount(n)) else { return nil }
        raw.frameLength = AVAudioFrameCount(n)
        let out = raw.floatChannelData![0]
        for i in 0..<n { let f = Float(y[i]); out[i] = f; peak = max(peak, abs(f)) }
        guard let buf = MenuBandSingerVoice.convert(raw, to: format) else { return nil }
        NSLog("🎤 sing: analyzed %d frames in %.0f ms; sang %d/%d notes over beats %.0f…%.0f of %.0f in %.0f ms; %.2f s @%.0f Hz peak %.3f — ask→ready %.0f ms",
              singer_total_frames(S), analyzeMs, used, notes.count, first16 / 4, last16 / 4, beat,
              Date().timeIntervalSince(tr) * 1000, Double(buf.frameLength) / format.sampleRate,
              format.sampleRate, peak, Date().timeIntervalSince(t0) * 1000)
        return SungRender(buffer: buf, spanOffset: spanOffset, notesUsed: Int(used),
                          noteCount: notes.count, peak: peak)
    }
}

/// The sung voice's place in the engine: one player node on the fx bus.
/// Connected once at attach with a fixed mono format (the engine resamples
/// to the device), so scheduling never touches topology.
final class MenuBandSingerVoice {
    private let player = AVAudioPlayerNode()
    private weak var engine: AVAudioEngine?
    private var attached = false
    /// The face this voice's meter drives (a sim tile's, or the shared one).
    var face: SingerFace = .shared
    /// Stereo place, -1…1 (a sim slot sits its member left or right).
    var pan: Float = 0 { didSet { player.pan = pan } }
    private var generation = 0            // bumped on stop; late timers become no-ops
    let format = AVAudioFormat(commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
                               channels: 1, interleaved: false)!

    private static var lastTapLog = 0.0

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.connect(player, to: output, format: format)
        attached = true
        // Proof-of-life tap: whenever the singer's node actually renders
        // audio, log its peak (rate-limited). Silence here = the buffer never
        // sounded; sound here = a downstream bus (duck/limiter/route) ate it.
        // Lip-sync meter + proof of life: every ~23 ms, the singer node's
        // RMS (jaw) and zero-crossing rate (sibilants) go to the face; a
        // rate-limited peak log says the voice is really sounding.
        player.pan = pan
        player.installTap(onBus: 0, bufferSize: 1024, format: nil) { [weak self] buf, _ in
            guard let ch = buf.floatChannelData, buf.frameLength > 0 else { return }
            let n = Int(buf.frameLength)
            var peak: Float = 0, sum: Float = 0, cross = 0
            var prev = ch[0][0]
            for i in 0..<n {
                let v = ch[0][i]
                peak = max(peak, abs(v)); sum += v * v
                if (v >= 0) != (prev >= 0) { cross += 1 }
                prev = v
            }
            let rms = sqrt(sum / Float(n)), zcr = Float(cross) / Float(n)
            let face = self?.face ?? .shared
            DispatchQueue.main.async { face.meter(rms: CGFloat(rms), zcr: CGFloat(zcr)) }
            guard peak > 0.01 else { return }
            let now = Date().timeIntervalSince1970
            if now - MenuBandSingerVoice.lastTapLog > 2.0 {
                MenuBandSingerVoice.lastTapLog = now
                NSLog("🎤 tap: singer rendering, peak %.3f", peak)
            }
        }
    }

    /// Sound `render` so its first sample lands at `epoch` (Unix seconds).
    /// Uses the player's own sample clock when it has one (the engine is
    /// rendering), else the host clock; a late line is trimmed to start now.
    /// Main thread — node control. Returns the lead in seconds (negative = late).
    @discardableResult
    func schedule(_ render: SungRender, atEpoch epoch: Double) -> Double {
        guard attached, let engine else { return 0 }
        let lead = epoch - Date().timeIntervalSince1970
        let gen = generation
        // Play the line by waiting for its instant on the main thread, then
        // scheduling the buffer for immediate playout — the same clock the
        // drums and key-lights use. Precise sample-time scheduling on a
        // player node proved unreliable across the engine's idle pause and
        // restarts (the buffer was accepted but silently skipped — the
        // silent-voice bug of Sept 20); this always sounds.
        let fire: () -> Void = { [weak self] in
            guard let self, self.generation == gen else { return }
            if !engine.isRunning { try? engine.start() }
            if !self.player.isPlaying { self.player.play() }
            guard self.player.isPlaying else { NSLog("🎤 sing: player would not start"); return }
            self.player.scheduleBuffer(render.buffer, completionHandler: nil)
            NSLog("🎤 sing: playing %.2f s now",
                  Double(render.buffer.frameLength) / render.buffer.format.sampleRate)
        }
        if lead <= 0.01 {
            if Thread.isMainThread { fire() } else { DispatchQueue.main.async(execute: fire) }
        } else {
            DispatchQueue.main.asyncAfter(deadline: .now() + lead, execute: fire)
        }
        return lead
    }

    /// Drop every queued line (stop cancels a pending sung line).
    func stop() {
        guard attached else { return }
        generation += 1
        player.stop()
    }

    static func convert(_ pcm: AVAudioPCMBuffer, to format: AVAudioFormat) -> AVAudioPCMBuffer? {
        if pcm.format == format { return pcm }
        guard let conv = AVAudioConverter(from: pcm.format, to: format) else { return nil }
        let ratio = format.sampleRate / pcm.format.sampleRate
        let cap = AVAudioFrameCount(Double(pcm.frameLength) * ratio) + 1_024
        guard let out = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: cap) else { return nil }
        var fed = false
        var error: NSError?
        conv.convert(to: out, error: &error) { _, status in
            if fed { status.pointee = .noDataNow; return nil }
            fed = true
            status.pointee = .haveData
            return pcm
        }
        return error == nil && out.frameLength > 0 ? out : nil
    }

    private static func dropping(_ skip: AVAudioFrameCount, from buf: AVAudioPCMBuffer) -> AVAudioPCMBuffer? {
        let n = buf.frameLength - skip
        guard let out = AVAudioPCMBuffer(pcmFormat: buf.format, frameCapacity: n),
              let src = buf.floatChannelData, let dst = out.floatChannelData else { return nil }
        out.frameLength = n
        for c in 0..<Int(buf.format.channelCount) {
            dst[c].update(from: src[c] + Int(skip), count: Int(n))
        }
        return out
    }
}
