import AVFoundation

/// Speaks short phrases (a language's own name) through the Menu Band
/// effects chain. Its `AVAudioPlayerNode` feeds the synth's
/// `preLimiterMixer` exactly like the sample + radio voices, so the
/// synthesized speech rides the same bend / space (proximity) / echo /
/// pitch the instruments do — a tiny easter egg: pick a language in the
/// About window and hear its name spoken back, warped by whatever fx the
/// gesture last left engaged.
///
/// The player node is wired into the graph **once** at attach time with a
/// fixed render format, and every `AVSpeechSynthesizer.write` buffer is
/// converted into that format before scheduling. Connecting up-front (vs.
/// lazily on the first buffer) is what keeps `play()` from ever hitting a
/// "started in a disconnected state" — the node is always connected, it
/// just renders silence until buffers arrive.
final class MenuBandSpeechVoice {
    private let synthesizer = AVSpeechSynthesizer()
    private let sampleRenderer = AVSpeechSynthesizer()
    private let player = AVAudioPlayerNode()
    /// Digit feedback is polyphonic: rapid number entry takes a fresh player
    /// instead of cutting off the word already sounding.
    private let digitPlayers = (0..<8).map { _ in AVAudioPlayerNode() }
    private let digitPitch = AVAudioUnitTimePitch()
    private let digitMixer = AVAudioMixerNode()
    private var nextDigitPlayer = 0
    /// Pitch-only shift (rate stays 1.0) so the trackpad bend slides the
    /// spoken voice without speeding it up — like the radio backend. The
    /// reverb/echo inserts already sit downstream on the fx bus; pitch has
    /// to be applied here at the source because it's not a bus insert.
    private let pitch = AVAudioUnitTimePitch()
    private let mixer = AVAudioMixerNode()
    private weak var engine: AVAudioEngine?
    private var attached = false
    private var warmedUp = false

    /// Fixed format the player is connected with; speech buffers are
    /// converted into this before scheduling.
    private let renderFormat = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
        channels: 1, interleaved: false)!
    /// Reused converter; rebuilt if the synthesizer's output format changes.
    private var converter: AVAudioConverter?
    private var converterInputFormat: AVAudioFormat?
    /// Startup-rendered digit clips. Each value is the sequence of PCM chunks
    /// AVSpeech produced for that word; scheduling those chunks directly makes
    /// number-key feedback start on key-down without a live TTS render.
    private var digitSamples: [Int: [AVAudioPCMBuffer]] = [:]
    private var digitSampleStarted: Set<Int> = []

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(pitch)
        engine.attach(mixer)
        engine.attach(digitPitch)
        engine.attach(digitMixer)
        for digitPlayer in digitPlayers {
            engine.attach(digitPlayer)
            engine.connect(digitPlayer, to: digitMixer, format: renderFormat)
        }
        engine.connect(player, to: pitch, format: renderFormat)
        engine.connect(pitch, to: mixer, format: renderFormat)
        engine.connect(mixer, to: output, format: nil)
        engine.connect(digitMixer, to: digitPitch, format: renderFormat)
        engine.connect(digitPitch, to: output, format: nil)
        mixer.outputVolume = 1.0
        digitMixer.outputVolume = 1.0
        attached = true
        prewarm()
        prepareDigitSamples()
    }

    /// Prime AVSpeech's renderer while the rest of Menu Band is starting.
    /// The first real `write` otherwise pays Apple's lazy voice-loading cost
    /// after the number key is already down, which makes a sampled gesture
    /// feel noticeably late. A whitespace utterance produces no scheduled
    /// audio but loads the selected English voice and render machinery ahead
    /// of the player's first digit entry.
    private func prewarm() {
        guard !warmedUp else { return }
        warmedUp = true
        let utterance = AVSpeechUtterance(string: " ")
        utterance.voice = Self.bestVoice(for: "en")
        utterance.rate = AVSpeechUtteranceDefaultSpeechRate * 0.92
        synthesizer.write(utterance) { _ in }
    }

    /// Render the ten spoken digits once, off the interaction path. AVSpeech
    /// may return a phrase in several buffers, so retain every non-empty chunk
    /// and schedule the sequence as one monophonic sample when its key lands.
    private func prepareDigitSamples() {
        let words = ["zero", "one", "two", "three", "four",
                     "five", "six", "seven", "eight", "nine"]
        for (digit, word) in words.enumerated() {
            let utterance = AVSpeechUtterance(string: word)
            utterance.voice = Self.bestVoice(for: "en")
            utterance.rate = AVSpeechUtteranceDefaultSpeechRate * 0.92
            sampleRenderer.write(utterance) { [weak self] buffer in
                guard let self = self,
                      let pcm = buffer as? AVAudioPCMBuffer,
                      pcm.frameLength > 0 else { return }
                DispatchQueue.main.async {
                    guard let converted = self.convertedBuffer(pcm) else { return }
                    if self.digitSampleStarted.contains(digit) {
                        self.digitSamples[digit, default: []].append(converted)
                    } else if let trimmed = self.trimmingLeadingSilence(converted) {
                        self.digitSampleStarted.insert(digit)
                        self.digitSamples[digit, default: []].append(trimmed)
                    }
                }
            }
        }
    }

    /// Remove AVSpeech's leading render pad so the consonant begins almost at
    /// key-down. Keep 64 frames (~1.5 ms) before the first audible sample to
    /// preserve the attack and avoid introducing a hard-zero click.
    private func trimmingLeadingSilence(_ buffer: AVAudioPCMBuffer) -> AVAudioPCMBuffer? {
        guard let source = buffer.floatChannelData?[0] else { return buffer }
        let count = Int(buffer.frameLength)
        let threshold: Float = 0.0015
        guard let audible = (0..<count).first(where: { abs(source[$0]) >= threshold })
        else { return nil }
        let start = max(0, audible - 64)
        let remaining = count - start
        guard let out = AVAudioPCMBuffer(pcmFormat: buffer.format,
                                         frameCapacity: AVAudioFrameCount(remaining)),
              let destination = out.floatChannelData?[0] else { return buffer }
        for frame in 0..<remaining {
            destination[frame] = source[start + frame]
        }
        out.frameLength = AVAudioFrameCount(remaining)
        return out
    }

    /// Fire a pre-rendered digit like a sample pad. During the brief startup
    /// window before its cache is ready, fall back to normal speech so a press
    /// is never silently dropped.
    func playDigit(_ digit: Int) {
        guard attached, (0...9).contains(digit) else { return }
        guard let buffers = digitSamples[digit], !buffers.isEmpty else {
            say(String(digit), languageCode: "en")
            return
        }
        let digitPlayer = digitPlayers[nextDigitPlayer]
        nextDigitPlayer = (nextDigitPlayer + 1) % digitPlayers.count
        // Only steal this one slot when all eight voices have wrapped around;
        // other digit words continue through their natural tails.
        digitPlayer.stop()
        for buffer in buffers {
            digitPlayer.scheduleBuffer(buffer, completionHandler: nil)
        }
        if let engine = engine, !engine.isRunning { try? engine.start() }
        digitPlayer.play()
    }

    /// Trackpad pitch-bend hook. `amount` is the controller's signed bend
    /// (one unit = one octave); slides the spoken voice's pitch live and
    /// stays put so a phrase started mid-bend picks it up. Clamped to
    /// AVAudioUnitTimePitch's ±2400-cent (±2 octave) range.
    func setBend(amount: Float) {
        let cents = max(-2400, min(2400, amount * 1200))
        pitch.pitch = cents
        digitPitch.pitch = cents
    }

    /// Speak `text` in `languageCode` (our short code: en/es/zh/ja/ru),
    /// routed through the engine's effect chain. Cuts off any in-flight
    /// phrase so rapid clicks don't pile up.
    func say(_ text: String, languageCode: String) {
        guard attached else { return }
        player.stop()
        let utterance = AVSpeechUtterance(string: text)
        utterance.voice = Self.bestVoice(for: languageCode)
        // A touch slower than default so the fx have something to chew on.
        utterance.rate = AVSpeechUtteranceDefaultSpeechRate * 0.92
        synthesizer.write(utterance) { [weak self] buffer in
            guard let self = self,
                  let pcm = buffer as? AVAudioPCMBuffer,
                  pcm.frameLength > 0 else { return }
            DispatchQueue.main.async { self.schedule(pcm) }
        }
    }

    /// Convert one synthesizer buffer into `renderFormat` and queue it.
    /// Runs on the main thread (engine topology + node control).
    private func schedule(_ pcm: AVAudioPCMBuffer) {
        guard attached, let engine = engine,
              let out = convertedBuffer(pcm) else { return }
        if !engine.isRunning { try? engine.start() }
        player.scheduleBuffer(out, completionHandler: nil)
        if !player.isPlaying { player.play() }
    }

    /// Convert a speech-renderer chunk into the player's fixed graph format.
    private func convertedBuffer(_ pcm: AVAudioPCMBuffer) -> AVAudioPCMBuffer? {
        if converterInputFormat != pcm.format {
            converter = AVAudioConverter(from: pcm.format, to: renderFormat)
            converterInputFormat = pcm.format
        }
        guard let converter = converter else { return nil }
        let ratio = renderFormat.sampleRate / pcm.format.sampleRate
        let capacity = AVAudioFrameCount(Double(pcm.frameLength) * ratio) + 1_024
        guard let out = AVAudioPCMBuffer(pcmFormat: renderFormat,
                                         frameCapacity: capacity) else { return nil }
        var fed = false
        var error: NSError?
        converter.convert(to: out, error: &error) { _, status in
            if fed { status.pointee = .noDataNow; return nil }
            fed = true
            status.pointee = .haveData
            return pcm
        }
        guard error == nil, out.frameLength > 0 else { return nil }
        return out
    }

    /// BCP-47 locale for each of our short language codes, used to pick a
    /// native voice for the spoken name.
    private static let localeForCode: [String: String] = [
        "en": "en-US", "es": "es-ES", "zh": "zh-CN", "ja": "ja-JP", "ru": "ru-RU",
        "da": "da-DK",
    ]

    /// Prefer the locale's default voice; fall back to any installed voice
    /// whose language shares the prefix.
    private static func bestVoice(for code: String) -> AVSpeechSynthesisVoice? {
        let locale = localeForCode[code] ?? code
        if let exact = AVSpeechSynthesisVoice(language: locale) { return exact }
        let prefix = code.lowercased()
        return AVSpeechSynthesisVoice.speechVoices()
            .first { $0.language.lowercased().hasPrefix(prefix) }
    }
}
