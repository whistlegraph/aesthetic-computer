import AVFoundation
import CoreGraphics
import QuartzCore

/// Lifted from slab/tracktramp's `TracktrampPerformer` — same clock, same order
/// of operations, same numbers. Nothing in here knows it is on a phone: the
/// surface hands it normalized drum coordinates and the engine behaves exactly
/// as it does on a Mac trackpad. That is the whole point of doing the turn in
/// `TrackDrumFit` instead of down here.
///
/// One thing IS different, and it is the reason a phone drum can feel like an
/// instrument at all: the Mac receives trackpad frames on a hardware callback
/// thread and hops them onto a serial queue. UIKit already delivers touches on
/// the main thread, so this runs there and a strike goes straight from
/// `touchesBegan` into the engine with no dispatch in between. The 60 Hz frame
/// that advances the membrane and repaints is a CADisplayLink on the same
/// thread, so there is no lock and no shared state to race over.
final class TrackDrumPerformer {
    /// Touches, the surface's live charges, and the membrane — the three things
    /// the chart is lit by.
    var onActivity: (([CGPoint], [TrackpadSurfaceEnergy.Charge],
                      TrackpadMembraneSimulation.Snapshot) -> Void)?

    private let engine = AVAudioEngine()
    private let percussion = MenuBandPercussion()
    private let haptics = TrackDrumHaptics()
    private var contactsByID: [Int32: CGPoint] = [:]
    private var energies: [Energy] = []
    private var previousTimestamp: Double = 0
    private var scratchFrames = 0
    private var scratchTravel: CGFloat = 0
    private var scratchSpeed: Double?
    private var membrane = TrackpadMembraneSimulation()
    private var visibleTouches: [CGPoint] = []
    private var frameLink: CADisplayLink?
    private var isRunning = false

    private struct Energy {
        var point: CGPoint
        var level: Double
        var at: Double
    }

    enum Failure: LocalizedError {
        case audio

        var errorDescription: String? { "Audio could not start." }
    }

    init(panAxis: (x: Double, y: Double)) {
        percussion.panAxis = panAxis
    }

    func start() throws {
        guard !isRunning else { return }
        // `attach` no-ops if the graph is already built, so a resume after an
        // interruption reuses the same source node.
        percussion.attach(to: engine, output: engine.mainMixerNode)
        engine.mainMixerNode.outputVolume = 0.92
        engine.prepare()
        do {
            try engine.start()
        } catch {
            throw Failure.audio
        }
        haptics.start()

        membrane.reset(at: CACurrentMediaTime())
        let link = CADisplayLink(target: self, selector: #selector(presentFrame))
        link.preferredFrameRateRange = CAFrameRateRange(minimum: 30, maximum: 60,
                                                        preferred: 60)
        link.add(to: .main, forMode: .common)
        frameLink = link
        isRunning = true
    }

    func stop() {
        guard isRunning else { return }
        isRunning = false
        frameLink?.invalidate()
        frameLink = nil
        contactsByID.removeAll(keepingCapacity: true)
        visibleTouches.removeAll(keepingCapacity: true)
        energies.removeAll(keepingCapacity: true)
        previousTimestamp = 0
        resetScratch()
        percussion.silence()
        haptics.stop()
        engine.stop()
    }

    /// Called straight from the touch handler. Deliberately synchronous: the
    /// strike is built and staged for the render thread right here, so the only
    /// delay left between glass and speaker is the IO buffer.
    func receive(contacts: [TrackpadContact], timestamp: Double, callbackTime: Double) {
        guard isRunning else { return }
        process(contacts: contacts, timestamp: timestamp, callbackTime: callbackTime)
    }

    func releaseAllContacts() {
        receive(contacts: [],
                timestamp: ProcessInfo.processInfo.systemUptime,
                callbackTime: CACurrentMediaTime())
    }

    private func process(contacts: [TrackpadContact], timestamp: Double,
                         callbackTime: Double) {
        let changes = TrackpadContactChanges.resolve(previous: contactsByID,
                                                     contacts: contacts)
        let previous = contactsByID
        contactsByID = changes.activeByID
        let touches = changes.active.map(\.point)
        visibleTouches = touches
        let now = CACurrentMediaTime()
        decayEnergies(now: now)

        if !changes.began.isEmpty || !changes.lifted.isEmpty {
            percussion.markTrackpadInput(at: callbackTime)
        }

        // Strikes first, before the membrane simulation and any bookkeeping —
        // whatever else this turn does, it does after the sound is staged.
        for strike in changes.began {
            let anchors = touches.filter { $0 != strike }
            let velocity = MenuBandPercussion.surfaceVelocity(
                at: strike,
                anchors: anchors,
                inertia: energy(at: strike)
            )
            percussion.playDrumSkin(strike: strike, anchors: anchors, velocity: velocity)
            haptics.strike(velocity: velocity, distance: distance(of: strike))
            membrane.impulse(at: strike,
                             amount: 0.28 + Double(velocity) / 127.0 * 0.54)
            energies.append(Energy(point: strike,
                                   level: 0.48 + Double(velocity) / 127.0 * 0.42,
                                   at: now))
        }

        for lift in changes.lifted {
            let velocity = UInt8(max(36, Int(Double(MenuBandPercussion.surfaceVelocity(
                at: lift,
                anchors: touches,
                inertia: energy(at: lift)
            )) * 0.62)))
            percussion.playSurfaceLift(at: lift, anchors: touches,
                                       velocity: velocity, synthetic: false)
            haptics.lift(velocity: velocity)
        }

        membrane.advance(to: now, touches: touches)

        let contactChanged = !changes.began.isEmpty || !changes.lifted.isEmpty
        if contactChanged || touches.isEmpty {
            resetScratch()
        } else if previousTimestamp > 0,
                  let movement = dominantMovement(previous: previous,
                                                  current: contactsByID) {
            let dt = max(1.0 / 240.0, min(0.050, timestamp - previousTimestamp))
            scratchFrames += 1
            scratchTravel += movement.distance
            if scratchFrames >= 2, scratchTravel >= 0.012 {
                let measured = Double(movement.distance) / dt
                let alpha = 1.0 - exp(-dt / 0.040)
                let speed = scratchSpeed.map { $0 + alpha * (measured - $0) } ?? measured
                scratchSpeed = speed
                let anchors = touches.filter { $0 != movement.point }
                percussion.setDrumSkinScratch(
                    at: movement.point,
                    speed: speed,
                    anchors: anchors,
                    direction: movement.vector,
                    surfaceEnergy: energy(at: movement.point),
                    synthetic: false
                )
                energize(at: movement.point, amount: min(0.035, speed * 0.008), now: now)
            }
        }
        previousTimestamp = touches.isEmpty ? 0 : timestamp
    }

    @objc private func presentFrame() {
        let now = CACurrentMediaTime()
        membrane.advance(to: now, touches: visibleTouches)
        decayEnergies(now: now)
        onActivity?(visibleTouches,
                    energies.map { .init(point: $0.point, level: $0.level) },
                    membrane.snapshot())
    }

    private func resetScratch() {
        scratchFrames = 0
        scratchTravel = 0
        scratchSpeed = nil
        percussion.stopDrumSkinScratch()
    }

    private func distance(of point: CGPoint) -> Double {
        MenuBandPercussion.roundedTrackpadDistance(sx: Double(point.x - 0.5) * 2,
                                                   sy: Double(point.y - 0.5) * 2)
    }

    private func dominantMovement(previous: [Int32: CGPoint], current: [Int32: CGPoint])
        -> (point: CGPoint, distance: CGFloat, vector: CGVector)? {
        current.compactMap { identifier, point -> (CGPoint, CGFloat, CGVector)? in
            guard let old = previous[identifier] else { return nil }
            let vector = CGVector(dx: point.x - old.x, dy: point.y - old.y)
            return (point, hypot(vector.dx * 1.64, vector.dy), vector)
        }.max { $0.1 < $1.1 }
    }

    private func decayEnergies(now: Double) {
        for index in energies.indices {
            let age = max(0, now - energies[index].at)
            energies[index].level *= exp(-age / 0.90)
            energies[index].at = now
        }
        energies.removeAll { $0.level < 0.01 }
    }

    private func energy(at point: CGPoint) -> Double {
        min(1, energies.reduce(0) { total, energy in
            let dx = Double(point.x - energy.point.x) * 1.64
            let dy = Double(point.y - energy.point.y)
            return total + energy.level * exp(-(dx * dx + dy * dy) / (2 * 0.19 * 0.19))
        })
    }

    private func energize(at point: CGPoint, amount: Double, now: Double) {
        if let index = energies.indices.min(by: {
            distanceSquared(energies[$0].point, point)
                < distanceSquared(energies[$1].point, point)
        }), distanceSquared(energies[index].point, point) < 0.05 {
            energies[index].level = min(1, energies[index].level + amount)
            energies[index].at = now
        } else {
            energies.append(Energy(point: point, level: amount, at: now))
        }
    }

    private func distanceSquared(_ a: CGPoint, _ b: CGPoint) -> Double {
        let dx = Double(a.x - b.x) * 1.64
        let dy = Double(a.y - b.y)
        return dx * dx + dy * dy
    }
}
