#if MAC_APP_STORE
import AppKit
import Foundation
import Network
import QuartzCore

/// Local, bounded contact bridge from the separately distributed TrackDrum
/// helper. The App Store process remains sandboxed and owns all sound and UI.
final class MenuBandTrackpadPluginClient {
    static let port: NWEndpoint.Port = 51_983
    static let helperBundleIdentifier = "computer.aestheticcomputer.menuband-trackpad"

    var onConnectionChanged: ((Bool) -> Void)?
    var onExitRequested: (() -> Void)?
    var onSummonRequested: (() -> Void)?
    var onFrame: (([TrackpadContact], Double, Double) -> Void)?

    private struct Command: Codable {
        let version: Int
        let command: String
        let enabled: Bool
    }

    private struct WireContact: Codable {
        let identifier: Int32
        let x: Double
        let y: Double
        let state: Int32
    }

    private struct Frame: Codable {
        let version: Int
        let timestamp: Double
        let contacts: [WireContact]
        let event: String?
    }

    private let queue = DispatchQueue(
        label: "computer.aesthetic.menuband.trackpad-plugin",
        qos: .userInteractive
    )
    private var connection: NWConnection?
    private var retryWork: DispatchWorkItem?
    private var receiveBuffer = Data()
    private var running = false
    private var captureEnabled = false
    private var lastHelperLaunchAt: CFTimeInterval = -.infinity

    func start() {
        NSLog("MenuBand TrackDrum bridge: starting")
        queue.async { [weak self] in
            guard let self, !self.running else { return }
            self.running = true
            self.connect()
        }
    }

    func stop() {
        queue.async { [weak self] in
            guard let self else { return }
            self.running = false
            self.retryWork?.cancel()
            self.retryWork = nil
            self.connection?.cancel()
            self.connection = nil
            self.receiveBuffer.removeAll(keepingCapacity: false)
        }
    }

    func setCaptureEnabled(_ enabled: Bool) {
        queue.async { [weak self] in
            guard let self else { return }
            self.captureEnabled = enabled
            self.sendCaptureCommandIfReady()
        }
    }

    private func connect() {
        guard running, connection == nil else { return }
        let connection = NWConnection(
            host: NWEndpoint.Host("127.0.0.1"),
            port: Self.port,
            using: .tcp
        )
        self.connection = connection
        connection.stateUpdateHandler = { [weak self, weak connection] state in
            guard let self, let connection, self.connection === connection else { return }
            switch state {
            case .ready:
                NSLog("MenuBand TrackDrum bridge: connected")
                self.receiveBuffer.removeAll(keepingCapacity: true)
                self.reportConnection(true)
                self.sendCaptureCommandIfReady()
                self.receive(on: connection)
            case .failed(let error):
                NSLog("MenuBand TrackDrum bridge: connection failed: %@",
                      String(describing: error))
                self.connection = nil
                self.reportConnection(false)
                self.scheduleRetry()
            case .waiting(let error):
                NSLog("MenuBand TrackDrum bridge: connection waiting: %@",
                      String(describing: error))
                self.connection = nil
                connection.cancel()
                self.reportConnection(false)
                self.scheduleRetry()
            case .cancelled:
                self.connection = nil
                self.reportConnection(false)
                self.scheduleRetry()
            default:
                break
            }
        }
        connection.start(queue: queue)
        launchInstalledHelperIfNeeded()
    }

    private func launchInstalledHelperIfNeeded() {
        let now = CACurrentMediaTime()
        guard now - lastHelperLaunchAt >= 2.0 else { return }
        lastHelperLaunchAt = now
        DispatchQueue.main.async {
            guard NSRunningApplication.runningApplications(
                withBundleIdentifier: Self.helperBundleIdentifier
            ).isEmpty else { return }
            let installedURL = URL(
                fileURLWithPath: "/Applications/TrackDrum for Menu Band.app",
                isDirectory: true
            )
            let url = NSWorkspace.shared.urlForApplication(
                withBundleIdentifier: Self.helperBundleIdentifier
            ) ?? (FileManager.default.fileExists(atPath: installedURL.path)
                ? installedURL : nil)
            guard let url else {
                NSLog("MenuBand TrackDrum helper is not installed")
                return
            }
            let configuration = NSWorkspace.OpenConfiguration()
            configuration.activates = false
            NSWorkspace.shared.openApplication(
                at: url, configuration: configuration
            ) { _, error in
                if let error {
                    NSLog("MenuBand TrackDrum helper launch failed: %@",
                          error.localizedDescription)
                }
            }
        }
    }

    private func scheduleRetry() {
        guard running, retryWork == nil else { return }
        let work = DispatchWorkItem { [weak self] in
            guard let self else { return }
            self.retryWork = nil
            self.connect()
        }
        retryWork = work
        queue.asyncAfter(deadline: .now() + 1.0, execute: work)
    }

    private func sendCaptureCommandIfReady() {
        guard let connection else { return }
        guard case .ready = connection.state else { return }
        let command = Command(
            version: 1,
            command: "capture",
            enabled: captureEnabled
        )
        guard var payload = try? JSONEncoder().encode(command) else { return }
        payload.append(0x0A)
        connection.send(content: payload, completion: .contentProcessed { error in
            if let error {
                NSLog("MenuBand TrackDrum command failed: %@", String(describing: error))
            }
        })
    }

    private func receive(on connection: NWConnection) {
        connection.receive(
            minimumIncompleteLength: 1,
            maximumLength: 16 * 1024
        ) { [weak self, weak connection] data, _, isComplete, error in
            guard let self, let connection, self.connection === connection else { return }
            if let data, !data.isEmpty { self.consume(data) }
            if isComplete || error != nil {
                connection.cancel()
                return
            }
            self.receive(on: connection)
        }
    }

    private func consume(_ data: Data) {
        receiveBuffer.append(data)
        guard receiveBuffer.count <= 64 * 1024 else {
            receiveBuffer.removeAll(keepingCapacity: false)
            connection?.cancel()
            return
        }
        while let newline = receiveBuffer.firstIndex(of: 0x0A) {
            let line = receiveBuffer[..<newline]
            receiveBuffer.removeSubrange(...newline)
            decodeFrame(Data(line))
        }
    }

    private func decodeFrame(_ data: Data) {
        guard data.count <= 16 * 1024,
              let frame = try? JSONDecoder().decode(Frame.self, from: data),
              frame.version == 1,
              frame.timestamp.isFinite,
              frame.contacts.count <= 16 else { return }
        if frame.event == "exit" {
            guard frame.contacts.isEmpty else { return }
            DispatchQueue.main.async { [weak self] in
                self?.onExitRequested?()
            }
            return
        }
        if frame.event == "summon" {
            guard frame.contacts.isEmpty else { return }
            DispatchQueue.main.async { [weak self] in
                self?.onSummonRequested?()
            }
            return
        }
        guard frame.event == nil else { return }
        var contacts: [TrackpadContact] = []
        contacts.reserveCapacity(frame.contacts.count)
        for contact in frame.contacts {
            guard contact.x.isFinite, contact.y.isFinite,
                  (3...5).contains(contact.state) else { return }
            contacts.append(TrackpadContact(
                identifier: contact.identifier,
                point: CGPoint(
                    x: min(1, max(0, contact.x)),
                    y: min(1, max(0, contact.y))
                ),
                state: contact.state
            ))
        }
        let callbackTime = CACurrentMediaTime()
        DispatchQueue.main.async { [weak self] in
            self?.onFrame?(contacts, frame.timestamp, callbackTime)
        }
    }

    private func reportConnection(_ connected: Bool) {
        DispatchQueue.main.async { [weak self] in
            self?.onConnectionChanged?(connected)
        }
    }
}
#endif
