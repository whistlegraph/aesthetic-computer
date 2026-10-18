import Foundation

/// Used only on the diagnostics queue. Two bounded local JSONL files; no
/// network transport, audio buffers, MIDI pitches or microphone recordings.
final class AudioDiagnosticLog {
    let url: URL
    private let previous: URL
    private let maximumBytes: UInt64
    private var handle: FileHandle?
    private var bytes: UInt64 = 0

    init(directory: URL, maximumBytes: UInt64 = 4 * 1024 * 1024) {
        url = directory.appendingPathComponent("audio-diagnostics.jsonl")
        previous = directory.appendingPathComponent("audio-diagnostics.previous.jsonl")
        self.maximumBytes = maximumBytes
    }

    func append(_ fields: [String: Any]) throws {
        var row = fields
        row["time"] = ISO8601DateFormatter().string(from: Date())
        row["uptime"] = ProcessInfo.processInfo.systemUptime
        row["pid"] = ProcessInfo.processInfo.processIdentifier
        var data = try JSONSerialization.data(withJSONObject: row, options: [.sortedKeys])
        data.append(0x0a)
        guard UInt64(data.count) <= maximumBytes else { return }
        if handle == nil { try open() }
        if bytes + UInt64(data.count) > maximumBytes {
            try handle?.close(); handle = nil
            if FileManager.default.fileExists(atPath: previous.path) {
                try FileManager.default.removeItem(at: previous)
            }
            try FileManager.default.moveItem(at: url, to: previous)
            try open()
        }
        try handle?.write(contentsOf: data)
        bytes += UInt64(data.count)
    }

    private func open() throws {
        try FileManager.default.createDirectory(at: url.deletingLastPathComponent(),
            withIntermediateDirectories: true, attributes: [.posixPermissions: 0o700])
        if !FileManager.default.fileExists(atPath: url.path) {
            FileManager.default.createFile(atPath: url.path, contents: nil,
                                          attributes: [.posixPermissions: 0o600])
        }
        handle = try FileHandle(forWritingTo: url)
        bytes = try handle!.seekToEnd()
    }

    deinit { try? handle?.close() }
}
