import Foundation

/// Main-thread ownership of device resets. Repeated requests for a device
/// join its current operation, including the settling interval. Different
/// devices reset in order; the UI stays busy until the last one finishes.
final class InterfaceResetCoordinator {
    typealias Completion = (Bool) -> Void
    private let perform: (UInt32, @escaping Completion) -> Void
    private let busyChanged: (Bool) -> Void
    private var pending: [UInt32] = []
    private var completions: [UInt32: [Completion]] = [:]
    private var active: UInt32?

    init(perform: @escaping (UInt32, @escaping Completion) -> Void,
         busyChanged: @escaping (Bool) -> Void) {
        self.perform = perform
        self.busyChanged = busyChanged
    }

    func request(_ device: UInt32, completion: Completion? = nil) {
        dispatchPrecondition(condition: .onQueue(.main))
        if completions[device] != nil {
            if let completion { completions[device]?.append(completion) }
            return
        }
        completions[device] = completion.map { [$0] } ?? []
        pending.append(device)
        if active == nil {
            busyChanged(true)
            startNext()
        }
    }

    private func startNext() {
        guard !pending.isEmpty else {
            active = nil
            busyChanged(false)
            return
        }
        let device = pending.removeFirst()
        active = device
        perform(device) { [weak self] success in
            dispatchPrecondition(condition: .onQueue(.main))
            guard let self, self.active == device else { return }
            let callbacks = self.completions.removeValue(forKey: device) ?? []
            // Keep active set during callbacks: a callback may request another
            // reset, which must queue behind the remaining work.
            callbacks.forEach { $0(success) }
            self.startNext()
        }
    }
}
