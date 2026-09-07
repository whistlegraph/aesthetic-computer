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
    /// Voice-number feedback is one complete utterance after the controller's
    /// short type-ahead debounce. A single player makes a newer selection
    /// supersede an older announcement cleanly.
    private let numberPlayer = AVAudioPlayerNode()
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
    /// Invalidates late buffers from an utterance superseded by a newer key.
    /// ABC is intentionally monophonic, like the original learning toy.
    private var utteranceGeneration = 0
    /// Bundled, pre-rendered Jeffrey ElevenLabs clips for every selectable
    /// voice number (0...128). Loaded off the interaction path at startup.
    private var numberSamples: [Int: AVAudioPCMBuffer] = [:]
    /// Fred's A-Z bank is decoded synchronously while the audio graph attaches.
    /// `spell` therefore does no synthesis, file I/O, or conversion on key-down:
    /// even the first press only schedules an already-resident PCM buffer.
    private var alphabetSamples: [String: AVAudioPCMBuffer] = [:]

    func attach(to engine: AVAudioEngine, output: AVAudioNode,
                dryOutput: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(pitch)
        engine.attach(mixer)
        engine.attach(numberPlayer)
        engine.connect(player, to: pitch, format: renderFormat)
        engine.connect(pitch, to: mixer, format: renderFormat)
        // Voice-number feedback is UI, not the selected melodic instrument.
        // Keep it on the same dry post-FX bus as percussion so a latched pitch
        // or echo gesture cannot smear the announced number.
        engine.connect(numberPlayer, to: dryOutput, format: renderFormat)
        engine.connect(mixer, to: output, format: nil)
        mixer.outputVolume = 1.0
        numberPlayer.volume = 1.0
        prepareAlphabetSamples()
        attached = true
        prewarm()
        prepareNumberSamples()
    }

    /// Load the tiny A-Z bank before `attach` returns. This intentionally adds
    /// a small amount of launch work so ABC never has a lazy first-key penalty.
    private func prepareAlphabetSamples() {
        var loaded: [String: AVAudioPCMBuffer] = [:]
        for scalar in UnicodeScalar("A").value...UnicodeScalar("Z").value {
            guard let unicode = UnicodeScalar(scalar) else { continue }
            let letter = String(Character(unicode))
            let resource = "abc-\(letter)"
            // SwiftPM flattens resources in the signed app; development
            // bundles may retain the source subdirectory. Accept both.
            let url = Bundle.appResources.url(
                forResource: resource, withExtension: "aiff",
                subdirectory: "voice-letters"
            ) ?? Bundle.appResources.url(
                forResource: resource, withExtension: "aiff"
            )
            guard let url,
                  let file = try? AVAudioFile(forReading: url),
                  let source = AVAudioPCMBuffer(
                    pcmFormat: file.processingFormat,
                    frameCapacity: AVAudioFrameCount(file.length)
                  )
            else { continue }
            do { try file.read(into: source) } catch { continue }
            guard let converted = Self.convertedBuffer(source, to: renderFormat),
                  let trimmed = trimmingLeadingSilence(converted)
            else { continue }
            loaded[letter] = trimmed
        }
        alphabetSamples = loaded
        NSLog("MenuBand: preloaded %d zero-latency Fred alphabet clips",
              loaded.count)
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

    private func prepareNumberSamples() {
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            guard let self else { return }
            var loaded: [Int: AVAudioPCMBuffer] = [:]
            for number in 0...128 {
                // SwiftPM's `.process` flattens this resource directory in a
                // command-line app bundle; Xcode preserves the subdirectory.
                // Accept both so development and signed installs load the
                // identical offline bank. `appResources`, never `Bundle.module`:
                // the installed .app has no nested bundle and the generated
                // accessor fatalErrors once the .build fallback dir is cleaned.
                let url = Bundle.appResources.url(
                    forResource: String(number), withExtension: "mp3",
                    subdirectory: "voice-numbers"
                ) ?? Bundle.appResources.url(
                    forResource: String(number), withExtension: "mp3"
                )
                guard let url,
                   let file = try? AVAudioFile(forReading: url),
                   let source = AVAudioPCMBuffer(
                    pcmFormat: file.processingFormat,
                    frameCapacity: AVAudioFrameCount(file.length)
                   ) else { continue }
                do { try file.read(into: source) } catch { continue }
                guard let converted = Self.convertedBuffer(
                    source, to: self.renderFormat
                ), let trimmed = self.trimmingLeadingSilence(converted)
                else { continue }
                loaded[number] = trimmed
            }
            DispatchQueue.main.async { [weak self] in
                self?.numberSamples = loaded
                NSLog("MenuBand: preloaded %d Jeffrey voice-number clips",
                      loaded.count)
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

    /// Announce one complete selectable voice number. During the brief startup
    /// loading window, fall back to local speech rather than dropping it.
    func playNumber(_ number: Int) {
        guard attached, (0...128).contains(number) else { return }
        guard let buffer = numberSamples[number] else {
            say(String(number), languageCode: "en")
            return
        }
        numberPlayer.stop()
        numberPlayer.scheduleBuffer(buffer, completionHandler: nil)
        if let engine = engine, !engine.isRunning { try? engine.start() }
        numberPlayer.play()
    }

    /// Trackpad pitch-bend hook. `amount` is the controller's signed bend
    /// (one unit = one octave); slides the spoken voice's pitch live and
    /// stays put so a phrase started mid-bend picks it up. Clamped to
    /// AVAudioUnitTimePitch's ±2400-cent (±2 octave) range.
    func setBend(amount: Float) {
        let cents = max(-2400, min(2400, amount * 1200))
        pitch.pitch = cents
    }

    /// Speak `text` in `languageCode` (our short code: en/es/zh/ja/ru),
    /// routed through the engine's effect chain. Cuts off any in-flight
    /// phrase so rapid clicks don't pile up.
    func say(_ text: String, languageCode: String) {
        render(text, languageCode: languageCode, rateMultiplier: 0.92,
               pitchMultiplier: 1)
    }

    /// Short, deliberate alphabet articulation inspired by the classic
    /// Speak & Spell. It uses the existing effects bus as a secondary layer;
    /// the player's selected musical voice continues independently. Every
    /// letter is already decoded, so the first key and every later key take
    /// the same buffer-scheduling path.
    func spell(_ letter: String) {
        let key = letter.uppercased()
        guard attached, let buffer = alphabetSamples[key] else {
            NSLog("MenuBand: missing preloaded ABC clip for %@", key)
            return
        }
        utteranceGeneration += 1
        _ = synthesizer.stopSpeaking(at: .immediate)
        player.stop()
        player.scheduleBuffer(buffer, completionHandler: nil)
        if let engine, !engine.isRunning { try? engine.start() }
        player.play()
    }

    private func render(_ text: String, languageCode: String,
                        rateMultiplier: Float, pitchMultiplier: Float,
                        voice: AVSpeechSynthesisVoice? = nil) {
        guard attached else { return }
        player.stop()
        utteranceGeneration += 1
        let generation = utteranceGeneration
        _ = synthesizer.stopSpeaking(at: .immediate)
        let utterance = AVSpeechUtterance(string: text)
        // Fred is the built-in classic Mac voice closest to the clipped,
        // electronic learning-toy character. Other speech keeps its native
        // language voice, and Macs without Fred fall back to English.
        utterance.voice = voice ?? Self.bestVoice(for: languageCode)
        utterance.rate = AVSpeechUtteranceDefaultSpeechRate * rateMultiplier
        utterance.pitchMultiplier = pitchMultiplier
        synthesizer.write(utterance) { [weak self] buffer in
            guard let self = self,
                  let pcm = buffer as? AVAudioPCMBuffer,
                  pcm.frameLength > 0 else { return }
            DispatchQueue.main.async { [weak self] in
                guard let self = self,
                      self.utteranceGeneration == generation else { return }
                self.schedule(pcm)
            }
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

    private static func convertedBuffer(_ pcm: AVAudioPCMBuffer,
                                        to format: AVAudioFormat)
        -> AVAudioPCMBuffer? {
        guard let converter = AVAudioConverter(from: pcm.format, to: format) else {
            return nil
        }
        let ratio = format.sampleRate / pcm.format.sampleRate
        let capacity = AVAudioFrameCount(Double(pcm.frameLength) * ratio) + 1_024
        guard let output = AVAudioPCMBuffer(
            pcmFormat: format, frameCapacity: capacity
        ) else { return nil }
        var fed = false
        var error: NSError?
        converter.convert(to: output, error: &error) { _, status in
            if fed { status.pointee = .noDataNow; return nil }
            fed = true
            status.pointee = .haveData
            return pcm
        }
        return error == nil && output.frameLength > 0 ? output : nil
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
