import Foundation

/// Local diagnostics only. The closed vocabulary deliberately cannot carry
/// text, account identifiers, URLs, credentials, source code, or audio.
final class DeviceActionLog: @unchecked Sendable {
    enum Event: String, Codable {
        case launch, lifecycle, touch, screen, setting, command, commandDelivery
        case typeEdit, typeSend, talkBegin, talkEnd, talkLatch, talkCancel, speech
        case accountToken, accountIdentity, signIn, signOut, consent, consentBridge
        case workspace, snapshot, inference, credits, storeCatalog, storePurchase, storeDelivery
        case drawing, preview, story, share, deletion, logExport, logClear, source, projection, notifications, feed, publish
    }
    enum Outcome: String, Codable {
        case requested, started, ended, ready, succeeded, failed, cancelled, denied
        case notSignedIn, notReady, busy, accountChanged, emptyInput, inputTooLong, captureActive, storageFull
        case unavailable, exhausted, declined, pending, alreadyAdded, ignored, presented, dismissed
        case active, inactive, background, opening, recording, processing, partial, final, sound
        case enabled, disabled, add, undo, clear, committed, painted, invalidated, httpError
        case networkError, invalidResponse, providerCredits, userCredits, permission, authentication
        case microphonePermission, speechPermission, recognizerUnavailable, noMicrophone, next, previous, paused, resumed
    }
    enum Control: String, Codable {
        case type, talk, chalk, brain, account, pieces, story, tv, privacy, deleteAccount, debugLog
        case ask, retry, stop, signIn, newPiece, openPiece, deletePiece, checkout, presentVersion, endPresentation
        case setModel, refreshBraincells, density, format, appearance, sounds, costUnit, creation
        case cloudSpeech, cloudNarration, requestText, send, cancel, allow, decline, drawingPad
    }
    enum Metric: String { case characters, version, busy, engineReady, remaining, purchased, used, limit, status, errorCode, strokes, points, enabled, durationMs, accountGeneration, pixelSize, format, touches, selection }
    enum Stage: String, Codable {
        case requestDispatched, firstModelOutput, firstCheckpoint, generationFinished, generationFailed
        case signedIn, generationEntered, guidesReady, inferenceHeaders, inputSocketReady, inputSocketAck
        case inputHttpFallback, refinementFailed, localEditDispatched, localEditPainted
        case braincellsRequest, braincellsHeaders, braincellsLoaded, braincellsFailed
    }
    struct Entry: Codable {
        let at: Date
        let elapsedMs: Int
        let session: UUID
        let sequence: Int
        let build: String
        let event: Event
        let outcome: Outcome?
        let control: Control?
        let stage: Stage?
        let counts: [String: Int]
    }
    static let shared = DeviceActionLog()
    private let queue = DispatchQueue(label: "computer.aesthetic.whistlegraph.action-log", qos: .utility)
    private let directory: URL
    private let fileLimit: Int
    private let fileCount: Int
    private let session = UUID()
    private let started = ProcessInfo.processInfo.systemUptime
    private let build = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion") as? String ?? "test"
    private var sequence = 0
    private var failure = false
    private var prepared = false

    init(directory: URL? = nil, fileLimit: Int = 256 * 1024, fileCount: Int = 4) {
        self.directory = directory ?? FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0].appendingPathComponent("ActionLog", isDirectory: true)
        self.fileLimit = max(1024, fileLimit); self.fileCount = max(1, fileCount)
    }
    func record(_ event: Event, _ outcome: Outcome? = nil, control: Control? = nil, _ counts: [Metric: Int] = [:], stage: Stage? = nil) {
        let at = Date(), elapsedMs = Int((ProcessInfo.processInfo.systemUptime - started) * 1000)
        queue.async { [self] in
            do {
                try prepare()
                sequence += 1
                let entry = Entry(at: at, elapsedMs: elapsedMs, session: session,
                    sequence: sequence, build: build, event: event, outcome: outcome, control: control, stage: stage,
                    counts: Dictionary(uniqueKeysWithValues: counts.map { ($0.key.rawValue, $0.value) }))
                let encoder = JSONEncoder(); encoder.dateEncodingStrategy = .iso8601; encoder.outputFormatting = [.sortedKeys]
                var data = try encoder.encode(entry); data.append(10)
                let current = file(0)
                let size = (try? current.resourceValues(forKeys: [.fileSizeKey]).fileSize) ?? 0
                if size + data.count > fileLimit { try rotate() }
                if !FileManager.default.fileExists(atPath: current.path) { try protectedWrite(Data(), to: current) }
                let handle = try FileHandle(forWritingTo: current)
                defer { try? handle.close() }
                try handle.seekToEnd(); try handle.write(contentsOf: data)
            } catch { failure = true; prepared = false } // Diagnostics must never stop creation.
        }
    }
    func recordError(_ event: Event, _ error: Error) {
        record(event, error is URLError ? .networkError : .failed, [.errorCode: (error as NSError).code])
    }
    var hasWriteFailure: Bool { queue.sync { failure } }
    func snapshot() throws -> String { try queue.sync { String(decoding: try snapshotData(), as: UTF8.self) } }
    func export() throws -> URL {
        try queue.sync {
            try prepare()
            let url = directory.appendingPathComponent("Whistlegraph-debug.jsonl")
            try protectedWrite(try snapshotData(), to: url)
            return url
        }
    }
    func clear() throws {
        try queue.sync {
            if FileManager.default.fileExists(atPath: directory.path) { try FileManager.default.removeItem(at: directory) }
            failure = false
            prepared = false
        }
    }
    private func file(_ index: Int) -> URL { directory.appendingPathComponent("actions-\(index).jsonl") }
    private func prepare() throws {
        guard !prepared else { return }
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        var url = directory; var values = URLResourceValues(); values.isExcludedFromBackup = true
        try url.setResourceValues(values)
        #if os(iOS)
        try FileManager.default.setAttributes([.protectionKey: FileProtectionType.completeUntilFirstUserAuthentication], ofItemAtPath: directory.path)
        #endif
        prepared = true
    }
    private func protectedWrite(_ data: Data, to url: URL) throws {
        #if os(iOS)
        try data.write(to: url, options: [.atomic, .completeFileProtectionUntilFirstUserAuthentication])
        #else
        try data.write(to: url, options: .atomic)
        #endif
    }
    private func rotate() throws {
        let manager = FileManager.default
        if manager.fileExists(atPath: file(fileCount - 1).path) { try manager.removeItem(at: file(fileCount - 1)) }
        if fileCount > 1 {
            for index in stride(from: fileCount - 2, through: 0, by: -1) where manager.fileExists(atPath: file(index).path) {
                try manager.moveItem(at: file(index), to: file(index + 1))
            }
        }
    }
    private func snapshotData() throws -> Data {
        var result = Data()
        for index in (0..<fileCount).reversed() where FileManager.default.fileExists(atPath: file(index).path) {
            result.append(try Data(contentsOf: file(index)))
        }
        return result
    }
    static func failureKind(_ text: String) -> Outcome {
        let text = text.lowercased()
        if text.contains("microphone permission") { return .microphonePermission }
        if text.contains("speech permission") { return .speechPermission }
        if text.contains("on-device") && text.contains("unavailable") { return .recognizerUnavailable }
        if text.contains("no microphone") { return .noMicrophone }
        if text.contains("provider") && (text.contains("credit") || text.contains("balance")) { return .providerCredits }
        if (text.contains("braincell") || text.contains("allowance")) && (text.contains("exhaust") || text.contains("ran out") || text.contains("not enough") || text.contains("insufficient")) { return .userCredits }
        if text.contains("permission") || text.contains("consent") { return .permission }
        if text.contains("sign in") || text.contains("sign-in") || text.contains("unauthorized") { return .authentication }
        return .failed
    }
}
