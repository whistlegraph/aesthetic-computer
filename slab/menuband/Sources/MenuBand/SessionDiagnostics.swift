import Foundation

/// Opt-in, local state transitions only. Never pass keys, notes, device/app
/// names, window titles, error descriptions or arbitrary text to this API.
final class SessionDiagnostics {
    static let preferenceKey = "MBSessionDiagnosticsEnabled"
    static let shared = SessionDiagnostics(enabled: UserDefaults.standard.bool(forKey: preferenceKey))

    enum Event: String, Codable {
        case sessionStarted, previousSessionUnclean, terminationRequested, terminationBegan, sessionEnded
        case appActive, appInactive, windowKey, windowResigned, workspaceActivation
        case systemSleep, systemWake, sessionActive, sessionInactive
        case focusRequested, performanceFocus, focusExitRequested, focusRestoreRequested
        case captureArm, captureArmed, captureDisarmed, captureResigned, capturePreserved
        case tapStarted, tapFailed, tapDisabled, tapReenabled, tapStopped
        case audioConfigurationChanged, audioRecovery, audioRecoveryResult, audioInterfaceReset
    }
    enum Reason: String, Codable {
        case observed, shortcut, commandGesture, escapeAction, cursorWatchdog
        case cancelled, resignedKey, statusItemPress, statusItemOrphaned, trackDrumShield
        case sigterm, appTermination, timeout, userInput
        case trackDrumAction, pianoWindowRelease, recordingAction
        case invalidOutput, stalledOutput, engineStopped
    }
    enum Source: String, Codable {
        case app, localCapture, globalKeyboard, commandSpace, bendTab, quietFocus, audio, gestureTap
    }
    struct Row: Codable {
        let schema: Int
        let session: String
        let timestamp: Double
        let uptime: Double
        let event: Event
        let reason: Reason
        let source: Source
        let state: Bool?
    }
    private struct Marker: Codable {
        let session: String
        let timestamp: Double
        let clean: Bool
    }

    let enabled: Bool
    private let directory: URL
    private let maxBytes: Int
    private let queue = DispatchQueue(label: "menuband.session-diagnostics", qos: .utility)
    // A stalled disk must not allow a focus/tap notification storm to grow
    // memory indefinitely. Producers drop events instead of waiting for IO.
    private let slots = DispatchSemaphore(value: 256)
    private let session = UUID().uuidString
    private let startedAt = Date().timeIntervalSince1970
    private var writeFailureReported = false
    private var began = false
    private var ended = false
    private var size: Int?
    private var prepared = false
    var currentURL: URL { directory.appendingPathComponent("session-diagnostics.jsonl") }
    var previousURL: URL { directory.appendingPathComponent("session-diagnostics.previous.jsonl") }
    var markerURL: URL { directory.appendingPathComponent("session-diagnostics-state.json") }

    init(enabled: Bool, directory: URL = FileManager.default.homeDirectoryForCurrentUser
         .appendingPathComponent("Library/Logs/MenuBand/Session", isDirectory: true),
         maxBytes: Int = 2 * 1024 * 1024) {
        self.enabled = enabled
        self.directory = directory
        self.maxBytes = maxBytes
    }

    func begin() {
        guard enabled else { return }
        queue.sync {
            guard !began else { return }
            began = true
            if let data = try? Data(contentsOf: markerURL),
               let previous = try? JSONDecoder().decode(Marker.self, from: data), !previous.clean {
                // Absence of a completed shutdown is evidence, not a crash
                // diagnosis: SIGKILL, power loss and a failed write also fit.
                append(makeRow(.previousSessionUnclean))
            }
            writeMarker(clean: false)
            append(makeRow(.sessionStarted))
        }
    }

    /// State-transition paths only; never call from an audio render callback.
    func record(_ event: Event, reason: Reason = .observed, source: Source = .app,
                state: Bool? = nil) {
        guard enabled, slots.wait(timeout: .now()) == .success else { return }
        let row = makeRow(event, reason: reason, source: source, state: state)
        queue.async {
            defer { self.slots.signal() }
            guard self.began, !self.ended else { return }
            self.append(row)
        }
    }

    /// Call only after app/audio teardown completed. No signal/crash handler
    /// does Foundation IO; macOS remains responsible for crash reports.
    func end() {
        guard enabled else { return }
        queue.sync {
            guard began, !ended else { return }
            append(makeRow(.sessionEnded, reason: .appTermination))
            writeMarker(clean: true)
            ended = true
        }
    }

    func flush() { queue.sync {} }

    private func makeRow(_ event: Event, reason: Reason = .observed,
                         source: Source = .app, state: Bool? = nil) -> Row {
        Row(schema: 1, session: session, timestamp: Date().timeIntervalSince1970,
            uptime: ProcessInfo.processInfo.systemUptime, event: event,
            reason: reason, source: source, state: state)
    }

    private func prepare() throws {
        guard !prepared else { return }
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true,
                                               attributes: [.posixPermissions: 0o700])
        try FileManager.default.setAttributes([.posixPermissions: 0o700], ofItemAtPath: directory.path)
        prepared = true
    }

    private func append(_ row: Row) {
        do {
            try prepare()
            var data = try JSONEncoder().encode(row)
            data.append(0x0a)
            guard data.count <= maxBytes else { return }
            let fm = FileManager.default
            if size == nil {
                size = (try? fm.attributesOfItem(atPath: currentURL.path)[.size] as? Int) ?? 0
            }
            if (size ?? 0) + data.count > maxBytes {
                if fm.fileExists(atPath: previousURL.path) { try fm.removeItem(at: previousURL) }
                // Don't retain an oversized legacy/corrupt file.
                if (size ?? 0) <= maxBytes { try fm.moveItem(at: currentURL, to: previousURL) }
                else { try fm.removeItem(at: currentURL) }
                size = 0
            }
            if !fm.fileExists(atPath: currentURL.path) {
                guard fm.createFile(atPath: currentURL.path, contents: nil,
                                    attributes: [.posixPermissions: 0o600]) else { return }
            }
            try fm.setAttributes([.posixPermissions: 0o600], ofItemAtPath: currentURL.path)
            let handle = try FileHandle(forWritingTo: currentURL)
            defer { try? handle.close() }
            try handle.seekToEnd()
            try handle.write(contentsOf: data)
            size = (size ?? 0) + data.count
        } catch { reportWriteFailure() }
    }

    private func writeMarker(clean: Bool) {
        do {
            try prepare()
            let data = try JSONEncoder().encode(Marker(session: session, timestamp: startedAt, clean: clean))
            try data.write(to: markerURL, options: .atomic)
            try FileManager.default.setAttributes([.posixPermissions: 0o600], ofItemAtPath: markerURL.path)
        } catch { reportWriteFailure() }
    }

    private func reportWriteFailure() {
        guard !writeFailureReported else { return }
        writeFailureReported = true
        NSLog("MenuBand session diagnostics could not write local evidence")
    }
}
