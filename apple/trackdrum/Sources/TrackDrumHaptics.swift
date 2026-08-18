import CoreHaptics
import Foundation

/// The click the glass does not have.
///
/// Half of what makes the Mac TrackDrum feel like an instrument is the
/// trackpad's own haptic click under the strike — the surface pushes back. A
/// phone screen pushes back with nothing, and the drum reads as a picture of a
/// drum until the Taptic Engine is doing the same job.
///
/// One transient per strike: intensity is the velocity the engine already
/// computed, so a soft hit taps softly; sharpness follows the zone, so the deep
/// centre thuds and the chassis rim ticks. Nothing is scheduled, nothing is
/// queued — the event is played from the touch handler on the same turn as the
/// sound, because a haptic that arrives late is worse than none.
final class TrackDrumHaptics {
    private var engine: CHHapticEngine?

    var isAvailable: Bool { engine != nil }

    func start() {
        guard CHHapticEngine.capabilitiesForHardware().supportsHaptics,
              engine == nil else { return }
        do {
            let engine = try CHHapticEngine()
            engine.playsHapticsOnly = true
            // iOS puts the engine to sleep aggressively; without this a drum
            // left alone for a few seconds goes numb on the next strike.
            engine.isAutoShutdownEnabled = false
            engine.resetHandler = { [weak engine] in try? engine?.start() }
            engine.stoppedHandler = { _ in }
            try engine.start()
            self.engine = engine
        } catch {
            // A drum with no haptics is still a drum. Say so and carry on.
            NSLog("🥁 haptics unavailable: %@", String(describing: error))
        }
    }

    func stop() {
        engine?.stop()
        engine = nil
    }

    /// `velocity` is the engine's own 0–127; `distance` its 0–1 depth from the
    /// drum's centre, which is what decides the material you are hitting.
    func strike(velocity: UInt8, distance: Double) {
        guard let engine else { return }
        let intensity = Float(max(0.16, min(1, Double(velocity) / 127)))
        let sharpness = Float(max(0, min(1, 0.15 + distance * 0.85)))
        do {
            let event = CHHapticEvent(eventType: .hapticTransient, parameters: [
                .init(parameterID: .hapticIntensity, value: intensity),
                .init(parameterID: .hapticSharpness, value: sharpness),
            ], relativeTime: 0)
            try engine.makePlayer(with: CHHapticPattern(events: [event],
                                                        parameters: []))
                .start(atTime: CHHapticTimeImmediate)
        } catch {
            // Losing one tap is not worth a log line per strike.
        }
    }

    /// A lighter tick for a finger leaving the surface, matching the engine's
    /// quieter lift articulation.
    func lift(velocity: UInt8) {
        strike(velocity: UInt8(max(1, Int(velocity) / 3)), distance: 0.2)
    }
}
