import Foundation

/// One restorable Claude session discovered on disk.
struct ClaudeHistoryEntry {
    let sessionId: String
    let cwd: String
    let mtime: Date
    let subject: String

    var cwdLabel: String {
        let url = URL(fileURLWithPath: cwd)
        return url.lastPathComponent.isEmpty ? cwd : url.lastPathComponent
    }

    var shortSubject: String {
        let trimmed = subject.trimmingCharacters(in: .whitespacesAndNewlines)
        if trimmed.count <= 60 { return trimmed }
        let idx = trimmed.index(trimmed.startIndex, offsetBy: 57)
        return trimmed[..<idx] + "…"
    }
}

/// Walks `~/.claude/projects/<encoded-cwd>/<session-id>.jsonl` and returns the
/// most-recently-modified sessions. Used by the menubar's "Restore threads"
/// submenu so a Terminal.app crash that wipes every Claude tab can be
/// recovered with one click.
enum ClaudeHistoryReader {
    /// Return the `limit` most-recently-modified sessions, excluding any
    /// session id present in `excludeSessionIds` (typically the live ones
    /// listed in `~/.local/share/slab/state/active-prompts/`).
    static func recent(limit: Int, excluding excludeSessionIds: Set<String> = []) -> [ClaudeHistoryEntry] {
        let fm = FileManager.default
        let projectsDir = "\(Paths.home)/.claude/projects"
        guard let projectDirs = try? fm.contentsOfDirectory(atPath: projectsDir) else {
            return []
        }

        // Pass 1: collect (path, mtime, sessionId) for every JSONL across all
        // project dirs, skipping anything already live. We only need the
        // metadata at this stage so directory walks stay cheap.
        struct Candidate {
            let path: String
            let sessionId: String
            let mtime: Date
        }
        var candidates: [Candidate] = []
        for dir in projectDirs where !dir.hasPrefix(".") {
            let dirPath = "\(projectsDir)/\(dir)"
            guard let files = try? fm.contentsOfDirectory(atPath: dirPath) else { continue }
            for file in files where file.hasSuffix(".jsonl") {
                let sessionId = String(file.dropLast(".jsonl".count))
                if excludeSessionIds.contains(sessionId) { continue }
                let path = "\(dirPath)/\(file)"
                let mtime = (try? fm.attributesOfItem(atPath: path)[.modificationDate] as? Date) ?? .distantPast
                candidates.append(Candidate(path: path, sessionId: sessionId, mtime: mtime))
            }
        }

        // Pass 2: sort by mtime desc, then read the first user message of the
        // top `limit` files for subject/cwd. Avoid parsing thousands of files
        // when the user only asked for 10.
        candidates.sort { $0.mtime > $1.mtime }
        var entries: [ClaudeHistoryEntry] = []
        for c in candidates {
            if entries.count >= limit { break }
            guard let (cwd, subject) = firstUserMessage(path: c.path) else { continue }
            // Empty cwd would mean we can't `cd` to relaunch — drop it.
            if cwd.isEmpty { continue }
            entries.append(ClaudeHistoryEntry(
                sessionId: c.sessionId,
                cwd: cwd,
                mtime: c.mtime,
                subject: subject
            ))
        }
        return entries
    }

    /// Stream the first `type:"user"` line of a JSONL and return its
    /// `(cwd, content)` pair. We read line-by-line to avoid pulling
    /// multi-megabyte conversations into memory.
    private static func firstUserMessage(path: String) -> (String, String)? {
        guard let handle = FileHandle(forReadingAtPath: path) else { return nil }
        defer { try? handle.close() }

        var buffer = Data()
        var fallbackCwd = ""
        // Read in 64KB chunks; almost every session has a user message in the
        // first chunk. Cap total read so a pathological file can't hang us.
        let chunkSize = 64 * 1024
        let maxRead = 1 * 1024 * 1024
        var totalRead = 0

        while totalRead < maxRead {
            let chunk = handle.readData(ofLength: chunkSize)
            if chunk.isEmpty { break }
            totalRead += chunk.count
            buffer.append(chunk)

            while let nlIdx = buffer.firstIndex(of: 0x0A) {
                let lineData = buffer[..<nlIdx]
                buffer = buffer[(nlIdx + 1)...]
                if lineData.isEmpty { continue }
                guard let obj = try? JSONSerialization.jsonObject(with: lineData) as? [String: Any] else { continue }

                if fallbackCwd.isEmpty, let c = obj["cwd"] as? String, !c.isEmpty {
                    fallbackCwd = c
                }
                if (obj["type"] as? String) != "user" { continue }
                let cwd = (obj["cwd"] as? String) ?? fallbackCwd
                let subject = extractContent(from: obj["message"]) ?? "(no subject)"
                return (cwd, subject)
            }
        }
        return nil
    }

    /// `message.content` is either a plain string or an array of content
    /// blocks (`{type:"text",text:"…"}`, tool results, etc). We only want
    /// the first chunk of human-readable text for the menu label.
    private static func extractContent(from message: Any?) -> String? {
        guard let msg = message as? [String: Any] else { return nil }
        if let s = msg["content"] as? String { return s }
        if let arr = msg["content"] as? [[String: Any]] {
            for block in arr {
                if (block["type"] as? String) == "text", let t = block["text"] as? String {
                    return t
                }
            }
        }
        return nil
    }
}
