import Foundation
import CryptoKit

// Completed clips survive leaving cards and app launches. Incomplete files never
// become cache hits. The format version also invalidates old rendering/voice rules.
struct StoryCache {
    let directory: URL
    let limit: Int
    init(name: String = "StoryMovies", limit: Int = 256_000_000, root: URL? = nil) {
        directory = (root ?? FileManager.default.urls(for: .cachesDirectory, in: .userDomainMask)[0]).appendingPathComponent(name)
        self.limit = limit
    }
    static func key(_ fields: [String]) -> String {
        let bytes = try! JSONEncoder().encode(fields)
        return SHA256.hash(data: bytes).map { String(format: "%02x", $0) }.joined()
    }
    func url(_ key: String, ext: String = "mp4") -> URL { directory.appendingPathComponent(key).appendingPathExtension(ext) }
    func find(_ key: String, ext: String = "mp4") -> URL? {
        let path = url(key, ext: ext)
        guard let size = try? path.resourceValues(forKeys: [.fileSizeKey]).fileSize, size > 0 else { return nil }
        try? FileManager.default.setAttributes([.modificationDate: Date()], ofItemAtPath: path.path)
        return path
    }
    func store(_ source: URL, key: String, ext: String = "mp4", protecting: Set<String> = []) throws -> URL {
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        let destination = url(key, ext: ext), staging = directory.appendingPathComponent(UUID().uuidString + ".partial")
        defer { try? FileManager.default.removeItem(at: staging) }
        try FileManager.default.copyItem(at: source, to: staging)
        if FileManager.default.fileExists(atPath: destination.path) { try FileManager.default.removeItem(at: destination) }
        try FileManager.default.moveItem(at: staging, to: destination)
        prune(protecting: protecting.union([key]))
        return destination
    }
    func prune(protecting: Set<String> = []) {
        let files = (try? FileManager.default.contentsOfDirectory(at: directory, includingPropertiesForKeys: [.fileSizeKey, .contentModificationDateKey])) ?? []
        let entries = files.compactMap { file -> (URL, Int, Date)? in
            guard file.pathExtension != "partial", let values = try? file.resourceValues(forKeys: [.fileSizeKey, .contentModificationDateKey]) else { return nil }
            return (file, values.fileSize ?? 0, values.contentModificationDate ?? .distantPast)
        }.sorted { $0.2 < $1.2 }
        var size = entries.reduce(0) { $0 + $1.1 }
        for (file, bytes, _) in entries where size > limit && !protecting.contains(file.deletingPathExtension().lastPathComponent) {
            if (try? FileManager.default.removeItem(at: file)) != nil { size -= bytes }
        }
    }
}
