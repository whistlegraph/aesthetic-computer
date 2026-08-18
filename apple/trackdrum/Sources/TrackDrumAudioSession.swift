import AVFoundation

/// The phone's audio contract, tuned for a percussion instrument, plus the
/// three ways iOS takes it back.
///
/// A drum is nothing but attack, so the IO buffer matters more here than any
/// other setting on the phone: iOS hands out about 23 ms by default, which on a
/// drum is not latency, it is a different instrument. We ask for 5 ms and read
/// back what we actually got, because the request is advisory.
///
/// `.playback` so the drum sounds through the silent switch — an instrument
/// that goes quiet because the ringer is off is broken — with mode
/// `.measurement`, which turns off the system's output signal processing and
/// the delay that comes with it. `.playAndRecord` would reach the same IO path
/// but drags in a microphone permission prompt for an app that never records,
/// so it is deliberately not used.
final class TrackDrumAudioSession {
    /// The engine is no longer ours — stop it and expect nothing.
    var onLost: (() -> Void)?
    /// It can come back up.
    var onReturned: (() -> Void)?

    private let session = AVAudioSession.sharedInstance()
    private var listening = false

    /// What the system granted, once it is running. Reported rather than
    /// assumed — nobody can hear a millisecond by looking at the source.
    var report: String {
        String(format: "🥁 buffer %.2f ms (asked 5.00) · rate %.0f Hz · "
                     + "output latency %.2f ms · round trip %.2f ms",
               session.ioBufferDuration * 1000,
               session.sampleRate,
               session.outputLatency * 1000,
               (session.ioBufferDuration + session.outputLatency) * 1000)
    }

    func activate() throws {
        try apply()
        guard !listening else { return }
        listening = true
        let center = NotificationCenter.default
        center.addObserver(self, selector: #selector(interrupted),
                           name: AVAudioSession.interruptionNotification,
                           object: session)
        center.addObserver(self, selector: #selector(rerouted),
                           name: AVAudioSession.routeChangeNotification,
                           object: session)
        center.addObserver(self, selector: #selector(mediaServicesReset),
                           name: AVAudioSession.mediaServicesWereResetNotification,
                           object: nil)
    }

    deinit { NotificationCenter.default.removeObserver(self) }

    private func apply() throws {
        try session.setCategory(.playback, mode: .measurement, options: [])
        // Match the hardware so nothing resamples on the way out.
        try session.setPreferredSampleRate(48_000)
        // 240 frames at 48 kHz. Asking is the only control there is.
        try session.setPreferredIOBufferDuration(0.005)
        try session.setActive(true)
    }

    @objc private func interrupted(_ note: Notification) {
        guard let raw = note.userInfo?[AVAudioSessionInterruptionTypeKey] as? UInt,
              let type = AVAudioSession.InterruptionType(rawValue: raw) else { return }
        switch type {
        case .began:
            onLost?()
        case .ended:
            // Resuming uninvited under an alarm gets us silence anyway, so
            // wait to be told.
            let raw = note.userInfo?[AVAudioSessionInterruptionOptionKey] as? UInt ?? 0
            guard AVAudioSession.InterruptionOptions(rawValue: raw).contains(.shouldResume)
            else { return }
            try? session.setActive(true)
            onReturned?()
        @unknown default: break
        }
    }

    @objc private func rerouted(_ note: Notification) {
        guard let raw = note.userInfo?[AVAudioSessionRouteChangeReasonKey] as? UInt,
              let reason = AVAudioSession.RouteChangeReason(rawValue: raw) else { return }
        // Headphones pulled or a speaker gained: the IO format and the buffer
        // size can both change underneath, and the drum's source node was built
        // at the old rate. Rebuild rather than play into a format that is gone.
        switch reason {
        case .oldDeviceUnavailable, .newDeviceAvailable, .override:
            onLost?()
            onReturned?()
        default: break
        }
    }

    @objc private func mediaServicesReset() {
        // Everything AVFoundation held is gone, category included.
        try? apply()
        onLost?()
        onReturned?()
    }
}
