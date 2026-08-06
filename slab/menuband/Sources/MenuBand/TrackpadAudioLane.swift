import Foundation
import CoreGraphics
import QuartzCore

/// The small, non-visual surface that trackpad contact frames may touch.
/// Keeping this protocol free of AppKit makes the realtime lane independently
/// testable and prevents presentation work from leaking back into hit timing.
protocol TrackpadAudioOutput: AnyObject {
    func markTrackpadInput(at callbackTime: Double)
    func percussionNoteOn(_ drum: MenuBandPercussion.Drum,
                          velocity: UInt8, pan: UInt8, accent: Bool) -> UInt64
    func percussionNoteOff(_ group: UInt64)
    func trackpadReverseKick(velocity: UInt8, pan: UInt8)
    func trackpadDrumSkin(strike: CGPoint, anchors: [CGPoint], velocity: UInt8)
    func trackpadSynthSurface(strike: CGPoint, anchors: [CGPoint], velocity: UInt8)
    func trackpadSurfaceLift(at point: CGPoint, anchors: [CGPoint],
                             velocity: UInt8, synthetic: Bool)
}

extension MenuBandController: TrackpadAudioOutput {}

/// Serial, user-interactive contact-to-voice lane for the direct-download
/// MultitouchSupport path. AppDelegate continues to own visuals and gesture
/// chrome on main; this object owns only the minimum transition state needed
/// to stage a drum before main-thread congestion can delay it.
final class TrackpadAudioLane {
    enum Mode: Equatable { case off, kit, skin, synth }

    private let queue = DispatchQueue(
        label: "computer.aesthetic.menuband.trackpad-audio",
        qos: .userInteractive
    )
    private weak var output: TrackpadAudioOutput?
    private var mode: Mode = .off
    private var contactsByID: [Int32: CGPoint] = [:]
    private var kitState = TrackpadPercussionPad.State()
    private var kitGroups: [TrackpadPercussionPad.Voice: UInt64] = [:]
    private var surfaceEnergy = TrackpadSurfaceEnergy()

    init(output: TrackpadAudioOutput) {
        self.output = output
    }

    func setMode(_ newMode: Mode) {
        queue.async { [weak self] in self?.applyMode(newMode) }
    }

    /// Copies are already detached from MultitouchSupport's callback memory.
    /// Enqueueing here keeps its private callback short while preserving frame
    /// order on a latency-critical queue that never waits for AppKit.
    func process(contacts: [TrackpadContact], timestamp: Double,
                 callbackTime: Double, shiftDown: Bool,
                 suppressed: Bool) {
        queue.async { [weak self] in
            self?.processFrame(
                contacts: contacts,
                timestamp: timestamp,
                callbackTime: callbackTime,
                shiftDown: shiftDown,
                suppressed: suppressed
            )
        }
    }

    func stop() {
        queue.sync {
            applyMode(.off)
            contactsByID.removeAll(keepingCapacity: false)
        }
    }

    /// Test seam: wait until all preceding mode/frame work has completed.
    func flushForTesting() { queue.sync {} }

    private func applyMode(_ newMode: Mode) {
        guard newMode != mode else { return }
        releaseKitGroups()
        kitState = TrackpadPercussionPad.State()
        surfaceEnergy.reset(at: CACurrentMediaTime())
        mode = newMode
    }

    private func processFrame(contacts: [TrackpadContact], timestamp: Double,
                              callbackTime: Double, shiftDown: Bool,
                              suppressed: Bool) {
        let changes = TrackpadContactChanges.resolve(
            previous: contactsByID, contacts: contacts
        )
        // Track contacts even while off/suppressed so enabling the lane beneath
        // resting fingers cannot manufacture a false new strike.
        contactsByID = changes.activeByID
        guard !suppressed else { return }

        switch mode {
        case .off:
            return
        case .kit:
            processKit(changes, callbackTime: callbackTime)
        case .skin, .synth:
            // Shift temporarily lends the continuous surface to melodic FX.
            // Its contacts remain remembered, but no drum event crosses the
            // lane while the modifier is down.
            guard !shiftDown else { return }
            processSurface(
                changes,
                callbackTime: callbackTime,
                synthetic: mode == .synth
            )
        }
        _ = timestamp // retained in the API for future scratch-clock staging
    }

    private func processKit(_ changes: TrackpadContactChanges,
                            callbackTime: Double) {
        guard let output else { return }
        let touches = changes.active.map(\.point)
        let transition = TrackpadPercussionPad.transition(
            from: kitState, touches: touches, began: changes.began
        )
        let changed = !transition.entered.isEmpty || !transition.exited.isEmpty
        if changed { output.markTrackpadInput(at: callbackTime) }

        for voice in transition.exited {
            if let group = kitGroups.removeValue(forKey: voice) {
                output.percussionNoteOff(group)
            }
        }
        for voice in transition.entered {
            switch voice {
            case .kick:
                kitGroups[voice] = output.percussionNoteOn(
                    .kick, velocity: 112, pan: voice.pan, accent: false
                )
            case .reverseKick:
                output.trackpadReverseKick(velocity: 108, pan: voice.pan)
            case .snare:
                kitGroups[voice] = output.percussionNoteOn(
                    .snare, velocity: 104, pan: voice.pan, accent: false
                )
            case .hatClosed:
                kitGroups[voice] = output.percussionNoteOn(
                    .hatClosed, velocity: 92, pan: voice.pan, accent: false
                )
            case .hatOpen:
                kitGroups[voice] = output.percussionNoteOn(
                    .hatOpen, velocity: 96, pan: voice.pan, accent: false
                )
            }
        }
        kitState = transition.state
    }

    private func processSurface(_ changes: TrackpadContactChanges,
                                callbackTime: Double, synthetic: Bool) {
        guard let output else { return }
        let touches = changes.active.map(\.point)
        let now = CACurrentMediaTime()
        surfaceEnergy.decay(to: now)

        if !changes.lifted.isEmpty {
            output.markTrackpadInput(at: callbackTime)
        }
        for lift in changes.lifted {
            let retained = surfaceEnergy.energy(at: lift, now: now)
            let downVelocity = MenuBandPercussion.surfaceVelocity(
                at: lift, anchors: touches, inertia: retained
            )
            let velocity = UInt8(max(36, Int(Double(downVelocity) * 0.62)))
            output.trackpadSurfaceLift(
                at: lift, anchors: touches,
                velocity: velocity, synthetic: synthetic
            )
        }

        for strike in changes.began {
            output.markTrackpadInput(at: callbackTime)
            let anchors = touches.filter { $0 != strike }
            let retained = surfaceEnergy.energy(at: strike, now: now)
            let velocity = MenuBandPercussion.surfaceVelocity(
                at: strike, anchors: anchors, inertia: retained
            )
            if synthetic {
                output.trackpadSynthSurface(
                    strike: strike, anchors: anchors, velocity: velocity
                )
            } else {
                output.trackpadDrumSkin(
                    strike: strike, anchors: anchors, velocity: velocity
                )
            }
            surfaceEnergy.energize(
                at: strike,
                amount: 0.22 + Double(velocity) / 127.0 * 0.34,
                now: now
            )
        }
    }

    private func releaseKitGroups() {
        guard let output else {
            kitGroups.removeAll(keepingCapacity: true)
            return
        }
        for group in kitGroups.values { output.percussionNoteOff(group) }
        kitGroups.removeAll(keepingCapacity: true)
    }
}
