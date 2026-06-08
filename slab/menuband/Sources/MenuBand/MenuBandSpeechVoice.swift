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
    private let player = AVAudioPlayerNode()
    /// Pitch-only shift (rate stays 1.0) so the trackpad bend slides the
    /// spoken voice without speeding it up — like the radio backend. The
    /// reverb/echo inserts already sit downstream on the fx bus; pitch has
    /// to be applied here at the source because it's not a bus insert.
    private let pitch = AVAudioUnitTimePitch()
    private let mixer = AVAudioMixerNode()
    private weak var engine: AVAudioEngine?
    private var attached = false

    /// Fixed format the player is connected with; speech buffers are
    /// converted into this before scheduling.
    private let renderFormat = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
        channels: 1, interleaved: false)!
    /// Reused converter; rebuilt if the synthesizer's output format changes.
    private var converter: AVAudioConverter?
    private var converterInputFormat: AVAudioFormat?

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(pitch)
        engine.attach(mixer)
        engine.connect(player, to: pitch, format: renderFormat)
        engine.connect(pitch, to: mixer, format: renderFormat)
        engine.connect(mixer, to: output, format: nil)
        mixer.outputVolume = 1.0
        attached = true
    }

    /// Trackpad pitch-bend hook. `amount` is the controller's signed bend
    /// (one unit = one octave); slides the spoken voice's pitch live and
    /// stays put so a phrase started mid-bend picks it up. Clamped to
    /// AVAudioUnitTimePitch's ±2400-cent (±2 octave) range.
    func setBend(amount: Float) {
        pitch.pitch = max(-2400, min(2400, amount * 1200))
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
        guard attached, let engine = engine else { return }
        if converterInputFormat != pcm.format {
            converter = AVAudioConverter(from: pcm.format, to: renderFormat)
            converterInputFormat = pcm.format
        }
        guard let converter = converter else { return }
        let ratio = renderFormat.sampleRate / pcm.format.sampleRate
        let capacity = AVAudioFrameCount(Double(pcm.frameLength) * ratio) + 1_024
        guard let out = AVAudioPCMBuffer(pcmFormat: renderFormat,
                                         frameCapacity: capacity) else { return }
        var fed = false
        var error: NSError?
        converter.convert(to: out, error: &error) { _, status in
            if fed { status.pointee = .noDataNow; return nil }
            fed = true
            status.pointee = .haveData
            return pcm
        }
        guard error == nil, out.frameLength > 0 else { return }
        if !engine.isRunning { try? engine.start() }
        player.scheduleBuffer(out, completionHandler: nil)
        if !player.isPlaying { player.play() }
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
