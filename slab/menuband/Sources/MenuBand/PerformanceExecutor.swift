import Foundation
import QuartzCore

/// Small value snapshots for UI readers. Never used by an audio callback;
/// copying a readout cannot wait behind engine preparation or note processing.
final class PerformanceSnapshot<Value> {
    private let lock = NSLock()
    private var value: Value
    init(_ value: Value) { self.value = value }
    func read() -> Value {
        lock.lock(); defer { lock.unlock() }
        return value
    }
    func update(_ change: (inout Value) -> Void) {
        lock.lock(); defer { lock.unlock() }
        change(&value)
    }
}

/// Ordered musical control work. This is not the CoreAudio render thread.
/// It may stage commands and update control state, but never waits for AppKit.
final class PerformanceExecutor {
    let queue = DispatchQueue(label: "computer.aesthetic.menuband.performance", qos: .userInteractive)
    private let key = DispatchSpecificKey<Bool>()

    init() { queue.setSpecific(key: key, value: true) }
    var isCurrent: Bool { DispatchQueue.getSpecific(key: key) == true }

    func perform(_ work: @escaping () -> Void) {
        if isCurrent { work() } else { enqueue(work) }
    }
    func sync<T>(_ work: () -> T) -> T {
        if isCurrent { return work() }
        return queue.sync(execute: work)
    }
    private var inputTiming: PerformanceInputTiming?
    private var inputSubmitted = false

    func enqueueInput(_ work: @escaping () -> Void) {
        let input = InputLatencyProbe.capturedEvent
        enqueue { [self] in withInput(input, work) }
    }

    func withInput(_ input: PerformanceInputTiming?, _ work: () -> Void) {
        let previous = (inputTiming, inputSubmitted)
        inputTiming = input
        inputTiming?.queued = CACurrentMediaTime()
        inputSubmitted = false
        work()
        if inputSubmitted, let inputTiming { InputLatencyProbe.shared.stage(inputTiming) }
        (inputTiming, inputSubmitted) = previous
    }

    func markInputSubmitted() {
        if isCurrent, inputTiming != nil { inputSubmitted = true }
    }

    func enqueue(_ work: @escaping () -> Void) { queue.async(execute: work) }
    func after(_ delay: TimeInterval, _ work: @escaping () -> Void) {
        queue.asyncAfter(deadline: .now() + delay, execute: work)
    }
    func flush() { if !isCurrent { queue.sync {} } }
}

/// Gate shared by the focused event tap and its main-thread lifecycle owner.
/// Only short admission/enqueue operations hold the lock; no audio or UI work
/// runs under it. Closing orders release after all admitted events.
final class PerformanceInputGate {
    private let lock = NSLock()
    private var open = false
    private var held: Set<UInt16> = []

    func activate() { lock.lock(); open = true; lock.unlock() }

    func close(release: () -> Void) {
        lock.lock()
        guard open else { lock.unlock(); return }
        open = false
        held.removeAll()
        release()
        lock.unlock()
    }

    func submit(key: UInt16, down: Bool, eligible: Bool, enqueue: () -> Void) -> Bool {
        lock.lock(); defer { lock.unlock() }
        guard open else { return false }
        // Once a down is owned, keep its up even if modifiers/layout changed.
        let owned = held.contains(key)
        guard (down && eligible) || owned else { return false }
        if down { held.insert(key) } else { held.remove(key) }
        enqueue()
        return true
    }
}

/// Main-thread refresh scheduler: bounded pending work and at most one draw
/// per frame, even if several independent animation clocks invalidate it.
final class CoalescedUIRefresh {
    private var pending = false
    private var lastDraw = -Double.infinity
    private let interval: TimeInterval

    init(framesPerSecond: Double = 60) { interval = 1 / framesPerSecond }

    func request(_ draw: @escaping () -> Void) {
        dispatchPrecondition(condition: .onQueue(.main))
        guard !pending else { return }
        pending = true
        let now = ProcessInfo.processInfo.systemUptime
        let delay = max(0, interval - (now - lastDraw))
        DispatchQueue.main.asyncAfter(deadline: .now() + delay) { [weak self] in
            guard let self else { return }
            self.pending = false
            self.lastDraw = ProcessInfo.processInfo.systemUptime
            draw()
        }
    }
}
