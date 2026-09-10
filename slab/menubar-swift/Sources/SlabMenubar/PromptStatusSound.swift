import AVFoundation
import Foundation

/// The ear's version of theme-by-status: a short cue when a tracked prompt
/// finishes its turn, stops to ask for input, or disconnects.
///
/// Synthesized rather than shipped as wavs so the three cues share one voice
/// and differ only in contour — which is what lets them be told apart from
/// across the room without looking up:
///
///   complete      — two notes falling a third (E5 → C5). A resolve: "done".
///   awaiting      — three notes climbing a triad (C5 E5 G5). A question: "you?"
///   disconnected  — one tone sliding down an octave with a beat in it.
///                   A power-down: "gone".
///
/// Independent of the menubar's ambient mute. That toggle silences the hooks'
/// layer (the TTS stingers and the drone), which is exactly what has been
/// muted on this machine — these cues are the quiet replacement, and get
/// their own switch (Paths.statusSoundsDisabledFlag).
enum PromptStatusSound {
    enum Cue: Int, CaseIterable, Comparable {
        case complete
        case awaiting
        case disconnected
        static func < (a: Cue, b: Cue) -> Bool { a.rawValue < b.rawValue }
    }

    private static let sampleRate = 44_100.0
    /// Loud enough to carry past a fan and a typing hand; well under a system
    /// alert. These fire a few times an hour, not on every keystroke.
    private static let gain = 0.22

    private static let queue = DispatchQueue(label: "computer.slab.prompt-status-sound",
                                             qos: .userInitiated)
    private static let engine = AVAudioEngine()
    private static let player = AVAudioPlayerNode()
    private static let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                              channels: 1)
    private static var configured = false
    private static var running = false
    private static var routeObserver: NSObjectProtocol?

    /// Queue the cues back to back. Several prompts changing in the same
    /// refresh play in turn rather than on top of each other, and a cue never
    /// cuts one that is still sounding.
    static func play(_ cues: [Cue]) {
        guard !cues.isEmpty else { return }
        queue.async {
            guard start() else { return }
            for cue in cues {
                guard let buffer = render(cue) else { continue }
                player.scheduleBuffer(buffer, at: nil, options: [])
            }
            player.play()
        }
    }

    /// Lazy, like PopSound: a machine that never hears a cue never spins up
    /// the engine. A route change (headphones in, display speakers gone) marks
    /// the engine for a restart on the next cue instead of failing silently.
    private static func start() -> Bool {
        if !configured {
            guard let format else { return false }
            engine.attach(player)
            engine.connect(player, to: engine.mainMixerNode, format: format)
            routeObserver = NotificationCenter.default.addObserver(
                forName: .AVAudioEngineConfigurationChange,
                object: engine,
                queue: nil
            ) { _ in
                queue.async { running = false }
            }
            configured = true
        }
        if running && engine.isRunning { return true }
        do {
            try engine.start()
        } catch {
            running = false
            NSLog("slab status sounds: audio engine failed to start: \(error)")
            return false
        }
        running = true
        return true
    }

    // MARK: - Rendering

    private static func render(_ cue: Cue) -> AVAudioPCMBuffer? {
        switch cue {
        case .complete:     return renderComplete()
        case .awaiting:     return renderAwaiting()
        case .disconnected: return renderDisconnected()
        }
    }

    /// A zeroed mono buffer `seconds` long, with a little silence at the end
    /// so queued cues breathe instead of running together.
    private static func blank(seconds: Double) -> (AVAudioPCMBuffer, UnsafeMutablePointer<Float>, Int)? {
        guard let format else { return nil }
        let frames = AVAudioFrameCount(sampleRate * (seconds + 0.12))
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
              let samples = buffer.floatChannelData?[0] else { return nil }
        buffer.frameLength = frames
        for i in 0..<Int(frames) { samples[i] = 0 }
        return (buffer, samples, Int(frames))
    }

    /// One struck note: a sine fundamental with a quieter, faster-fading
    /// octave partial, so the cue reads as a small bell and not a test tone.
    private static func strike(_ samples: UnsafeMutablePointer<Float>, count: Int,
                               at start: Double, frequency: Double,
                               duration: Double, decay: Double, level: Double) {
        let startFrame = Int(start * sampleRate)
        let frames = Int(duration * sampleRate)
        var phase = 0.0
        var octave = 0.0
        for i in 0..<frames where startFrame + i < count {
            let t = Double(i) / Double(frames)
            phase += 2 * .pi * frequency / sampleRate
            octave += 2 * .pi * frequency * 2 / sampleRate
            let attack = min(1.0, t / 0.015)             // a tap, not a swell
            let body = exp(-decay * t)
            let shimmer = sin(octave) * 0.28 * exp(-(decay * 2.2) * t)
            samples[startFrame + i] += Float((sin(phase) + shimmer) * attack * body * level)
        }
    }

    /// E5 → C5. The second note lingers so the pair settles rather than stops.
    private static func renderComplete() -> AVAudioPCMBuffer? {
        guard let (buffer, samples, count) = blank(seconds: 0.5) else { return nil }
        strike(samples, count: count, at: 0.00, frequency: 659.25, duration: 0.22, decay: 5.0, level: gain)
        strike(samples, count: count, at: 0.16, frequency: 523.25, duration: 0.34, decay: 3.6, level: gain)
        return buffer
    }

    /// C5 E5 G5, evenly spaced, the top note held. Rising is what makes it a
    /// question; three notes is what makes it more than the done cue.
    private static func renderAwaiting() -> AVAudioPCMBuffer? {
        guard let (buffer, samples, count) = blank(seconds: 0.62) else { return nil }
        strike(samples, count: count, at: 0.00, frequency: 523.25, duration: 0.20, decay: 5.0, level: gain)
        strike(samples, count: count, at: 0.13, frequency: 659.25, duration: 0.20, decay: 5.0, level: gain)
        strike(samples, count: count, at: 0.26, frequency: 783.99, duration: 0.36, decay: 3.8, level: gain * 1.1)
        return buffer
    }

    /// G4 sliding to G3 with a second oscillator a few hertz off, so the tail
    /// wobbles the way a spun-down motor does. Nothing else in slab falls.
    private static func renderDisconnected() -> AVAudioPCMBuffer? {
        guard let (buffer, samples, count) = blank(seconds: 0.5) else { return nil }
        let duration = 0.42
        let frames = Int(duration * sampleRate)
        let high = 392.0
        let low = 196.0
        var phase = 0.0
        var beat = 0.0
        for i in 0..<frames where i < count {
            let t = Double(i) / Double(frames)
            // Equal ratios per unit time: the ear hears a straight slide.
            let f = high * pow(low / high, t)
            phase += 2 * .pi * f / sampleRate
            beat += 2 * .pi * (f + 6.0) / sampleRate
            let attack = min(1.0, t / 0.012)
            let body = exp(-3.0 * t) * (1 - t * 0.35)
            let tone = sin(phase) * 0.7 + sin(beat) * 0.3
            samples[i] = Float(tone * attack * body * gain)
        }
        return buffer
    }
}

/// Decides, once per refresh, which cues the tick has earned. Fed the session
/// list on main from AppDelegate.refresh; keeps only the previous state per
/// session, so the five-second cadence costs nothing.
///
/// The first list after launch is swallowed — slab starting up next to eight
/// finished prompts is not eight completions. Toggling the sounds off still
/// tracks state, so toggling them back on cannot replay a backlog.
final class PromptStatusWatcher {
    static let shared = PromptStatusWatcher()

    /// A known session has to be missing from this many consecutive refreshes
    /// before it counts as disconnected. A marker caught mid-rewrite fails one
    /// parse and is back on the next tick; a closed terminal stays gone.
    static let absentTicksBeforeGone = 2

    private var last: [String: ClaudeSession.State] = [:]
    private var absentTicks: [String: Int] = [:]
    /// Sessions whose disconnect already sounded (a `.stale` sighting), so the
    /// marker's later reaping is silent.
    private var announcedGone = Set<String>()
    private var primed = false

    func observe(sessions: [ClaudeSession], enabled: Bool) {
        var cues = Set<PromptStatusSound.Cue>()
        var seen = Set<String>()

        for session in sessions {
            let sid = session.sessionId
            seen.insert(sid)
            absentTicks[sid] = nil
            let prev = last[sid]
            last[sid] = session.state
            guard primed, let prev else { continue }
            guard let cue = Self.cue(from: prev, to: session) else { continue }
            if cue == .disconnected {
                if announcedGone.insert(sid).inserted { cues.insert(cue) }
            } else {
                cues.insert(cue)
            }
        }

        for sid in Array(last.keys) where !seen.contains(sid) {
            let ticks = (absentTicks[sid] ?? 0) + 1
            if ticks < Self.absentTicksBeforeGone {
                absentTicks[sid] = ticks
                continue
            }
            if primed && !announcedGone.contains(sid) { cues.insert(.disconnected) }
            last[sid] = nil
            absentTicks[sid] = nil
            announcedGone.remove(sid)
        }

        primed = true
        guard enabled, !cues.isEmpty else { return }
        PromptStatusSound.play(cues.sorted())
    }

    /// The transition rules. A session only ever sounds when its state
    /// actually changed; which way it changed decides the cue.
    static func cue(from prev: ClaudeSession.State,
                    to session: ClaudeSession) -> PromptStatusSound.Cue? {
        guard prev != session.state else { return nil }
        switch session.state {
        case .complete:
            // Any route into complete is a turn ending — including a permission
            // that was granted and then finished, and a render that cooked
            // down to idle. A fresh session that appears already complete never
            // gets here (no previous state).
            return .complete
        case .awaiting:
            // Claude Code's idle notification lands about a minute after Stop
            // and says "waiting for your input". That is the same event the
            // done cue already announced, so it stays quiet unless the prompt
            // was still working when it arrived.
            let message = (session.awaitingMessage ?? "").lowercased()
            let idleEcho = message.contains("waiting for your input")
            if idleEcho && (prev == .complete || prev == .interrupted) { return nil }
            return .awaiting
        case .stale:
            return .disconnected
        case .blank, .working, .rendering, .interrupted:
            // Starting, resuming, and the inferred Esc are the user's own
            // doing; nothing to announce.
            return nil
        }
    }
}
