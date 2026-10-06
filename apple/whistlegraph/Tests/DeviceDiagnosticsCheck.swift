import Foundation

@main struct DeviceDiagnosticsCheck {
    @MainActor static func main() async throws {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent("whistlegraph-diagnostics-test-" + UUID().uuidString)
        defer { try? FileManager.default.removeItem(at: root) }
        let log = DeviceActionLog(directory: root, fileLimit: 2048, fileCount: 3)
        DispatchQueue.concurrentPerform(iterations: 200) { n in log.record(.typeSend, .requested, [.characters: n]) }
        let snapshot = try log.snapshot()
        let decoder = JSONDecoder(); decoder.dateDecodingStrategy = .iso8601
        let entries = try snapshot.split(separator: "\n").map { try decoder.decode(DeviceActionLog.Entry.self, from: Data($0.utf8)) }
        precondition(!entries.isEmpty && entries.count < 200, "Old entries rotate out")
        precondition(zip(entries, entries.dropFirst()).allSatisfy { $0.sequence + 1 == $1.sequence }, "Concurrent writes stay complete and ordered")
        let files = try FileManager.default.contentsOfDirectory(at: root, includingPropertiesForKeys: [.fileSizeKey])
        precondition(files.count <= 3)
        for file in files { let size = try file.resourceValues(forKeys: [.fileSizeKey]).fileSize!; precondition(size <= 2048) }
        let excluded = try root.resourceValues(forKeys: [.isExcludedFromBackupKey]).isExcludedFromBackup
        precondition(excluded == true)
        let exported = try log.export()
        let exportedText = try String(contentsOf: exported, encoding: .utf8)
        precondition(exportedText == snapshot)
        let reopened = DeviceActionLog(directory: root, fileLimit: 2048, fileCount: 3)
        let restored = try reopened.snapshot()
        precondition(restored == snapshot, "Survives a process restart")
        let secret = "PRIVATE_TOKEN_AND_PROMPT"
        log.recordError(.accountIdentity, NSError(domain: secret, code: 401, userInfo: [NSLocalizedDescriptionKey: secret]))
        let redacted = try log.snapshot()
        precondition(!redacted.contains(secret), "Errors cannot leak their domain or description")
        try log.clear()
        let cleared = try log.snapshot()
        precondition(!FileManager.default.fileExists(atPath: exported.path) && cleared.isEmpty)
        let blocker = root.appendingPathExtension("file")
        try Data().write(to: blocker); defer { try? FileManager.default.removeItem(at: blocker) }
        let broken = DeviceActionLog(directory: blocker.appendingPathComponent("cannot-create"))
        broken.record(.launch)
        precondition(broken.hasWriteFailure, "A full/unwritable store reports failure without crashing")

        var calls = 0
        let identity = VerifiedAccountIdentity { request in
            calls += 1
            precondition(request.url?.absoluteString == "https://hi.aesthetic.computer/userinfo")
            precondition(request.value(forHTTPHeaderField: "Authorization") == "Bearer opaque-token")
            try await Task.sleep(for: .milliseconds(20))
            return (Data(#"{"sub":"account-a"}"#.utf8), HTTPURLResponse(url: request.url!, statusCode: 200, httpVersion: nil, headerFields: nil)!)
        }
        async let a = identity.subject(token: "opaque-token", generation: 1)
        async let b = identity.subject(token: "opaque-token", generation: 1)
        let both = try await [a, b]
        precondition(both == ["account-a", "account-a"] && calls == 1, "Opaque tokens work and concurrent requests share one lookup")
        let cached = try await identity.subject(token: "opaque-token", generation: 1)
        precondition(cached == "account-a" && calls == 1)
        identity.invalidate()
        _ = try await identity.subject(token: "opaque-token", generation: 2)
        precondition(calls == 2, "Account changes invalidate identity")
        var latest = "account-b"
        let switched = VerifiedAccountIdentity { request in
            (Data("{\"sub\":\"\(latest)\"}".utf8), HTTPURLResponse(url: request.url!, statusCode: 200, httpVersion: nil, headerFields: nil)!)
        }
        let first = try await switched.subject(token: "looks.like.jwt", generation: 1)
        latest = "account-c"
        let second = try await switched.subject(token: "fresh-token", generation: 1)
        precondition(first == "account-b" && second == "account-c", "JWT-shaped tokens are verified too; refreshed credentials invalidate the cache")
        for (status, body) in [(401, #"{"sub":"wrong"}"#), (503, "unavailable"), (200, "{}"), (200, #"{"sub":" "}"#), (200, "not json")] {
            let failure = VerifiedAccountIdentity { request in (Data(body.utf8), HTTPURLResponse(url: request.url!, statusCode: status, httpVersion: nil, headerFields: nil)!) }
            do { _ = try await failure.subject(token: "opaque", generation: 1); preconditionFailure("Invalid identity must fail") }
            catch { }
        }
        let timeout = VerifiedAccountIdentity { _ in throw URLError(.timedOut) }
        do { _ = try await timeout.subject(token: "opaque", generation: 1); preconditionFailure("Timeout must fail") }
        catch { precondition(error is URLError) }
        var started = false
        let stale = VerifiedAccountIdentity { request in
            started = true
            // Deliberately ignores cancellation like a late network response.
            try? await Task.sleep(for: .milliseconds(40))
            return (Data(#"{"sub":"old-account"}"#.utf8), HTTPURLResponse(url: request.url!, statusCode: 200, httpVersion: nil, headerFields: nil)!)
        }
        let old = Task { try await stale.subject(token: "old", generation: 1) }
        while !started { await Task.yield() }
        stale.invalidate()
        do { _ = try await old.value; preconditionFailure("Stale account lookup must not succeed") }
        catch { }
        print("DeviceDiagnosticsCheck passed: rotation, concurrency, persistence, export, clear, redaction, write failure; opaque identity, cache, refresh, HTTP/malformed/timeout failures and account-switch fencing.")
    }
}
