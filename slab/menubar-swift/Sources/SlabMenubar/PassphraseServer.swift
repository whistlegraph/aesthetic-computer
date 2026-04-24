import Foundation
import Darwin

final class PassphraseServer {
    private var listenFd: Int32 = -1
    private var cache: [String: CacheEntry] = [:]
    private let cacheQueue = DispatchQueue(label: "slab.passphrase.cache")

    private struct CacheEntry {
        let secret: String
        let expiry: Date
    }

    func start() throws {
        let sockPath = Paths.passphraseSocket
        unlink(sockPath)

        listenFd = socket(AF_UNIX, SOCK_STREAM, 0)
        if listenFd < 0 { throw NSError(domain: "slab.passphrase", code: Int(errno)) }

        var addr = sockaddr_un()
        addr.sun_family = sa_family_t(AF_UNIX)
        let pathBytes = Array(sockPath.utf8)
        let maxPath = MemoryLayout.size(ofValue: addr.sun_path) - 1
        precondition(pathBytes.count <= maxPath, "socket path too long")

        withUnsafeMutableBytes(of: &addr.sun_path) { raw in
            let ptr = raw.baseAddress!.assumingMemoryBound(to: UInt8.self)
            for i in 0..<pathBytes.count { ptr[i] = pathBytes[i] }
            ptr[pathBytes.count] = 0
        }

        let addrLen = socklen_t(MemoryLayout<sockaddr_un>.size)
        let bindResult = withUnsafePointer(to: &addr) { ap -> Int32 in
            ap.withMemoryRebound(to: sockaddr.self, capacity: 1) { sa in
                Darwin.bind(listenFd, sa, addrLen)
            }
        }
        if bindResult < 0 {
            let e = errno
            close(listenFd); listenFd = -1
            throw NSError(domain: "slab.passphrase.bind", code: Int(e))
        }
        chmod(sockPath, 0o600)

        if listen(listenFd, 8) < 0 {
            let e = errno
            close(listenFd); listenFd = -1
            throw NSError(domain: "slab.passphrase.listen", code: Int(e))
        }

        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            self?.acceptLoop()
        }
    }

    func stop() {
        if listenFd >= 0 {
            close(listenFd)
            listenFd = -1
        }
        unlink(Paths.passphraseSocket)
    }

    private func acceptLoop() {
        while listenFd >= 0 {
            let fd = accept(listenFd, nil, nil)
            if fd < 0 { continue }
            DispatchQueue.global(qos: .userInitiated).async { [weak self] in
                self?.handleClient(fd)
            }
        }
    }

    private func handleClient(_ fd: Int32) {
        defer { close(fd) }

        var buf = [UInt8](repeating: 0, count: 8192)
        let n = read(fd, &buf, buf.count)
        guard n > 0 else { return }
        let data = Data(bytes: buf, count: n)

        guard let obj = try? JSONSerialization.jsonObject(with: data),
              let json = obj as? [String: Any] else {
            writeJson(fd: fd, ["ok": false, "err": "bad_json"])
            return
        }
        let op = (json["op"] as? String) ?? ""

        switch op {
        case "passphrase":
            let label = (json["label"] as? String) ?? "vault"
            let timeout = (json["timeout"] as? Int) ?? 600
            handlePassphrase(fd: fd, label: label, ttl: TimeInterval(timeout))
        case "forget":
            let label = json["label"] as? String
            cacheQueue.sync {
                if let label = label {
                    cache.removeValue(forKey: label)
                } else {
                    cache.removeAll()
                }
            }
            writeJson(fd: fd, ["ok": true])
        case "ping":
            writeJson(fd: fd, ["ok": true, "pong": true])
        default:
            writeJson(fd: fd, ["ok": false, "err": "unknown_op"])
        }
    }

    private func handlePassphrase(fd: Int32, label: String, ttl: TimeInterval) {
        let now = Date()
        let cached: String? = cacheQueue.sync {
            if let entry = cache[label], entry.expiry > now {
                return entry.secret
            }
            cache.removeValue(forKey: label)
            return nil
        }
        if let secret = cached {
            writeJson(fd: fd, ["ok": true, "secret": secret, "cached": true])
            return
        }

        let sem = DispatchSemaphore(value: 0)
        var typed: String?
        DispatchQueue.main.async {
            typed = PassphraseModal.prompt(label: label)
            sem.signal()
        }
        sem.wait()

        guard let secret = typed else {
            writeJson(fd: fd, ["ok": false, "cancelled": true])
            return
        }

        let expiry = Date().addingTimeInterval(ttl)
        cacheQueue.sync { cache[label] = CacheEntry(secret: secret, expiry: expiry) }
        writeJson(fd: fd, ["ok": true, "secret": secret, "cached": false])
    }

    private func writeJson(fd: Int32, _ dict: [String: Any]) {
        guard let data = try? JSONSerialization.data(withJSONObject: dict, options: []) else { return }
        var remaining = data
        while !remaining.isEmpty {
            let wrote = remaining.withUnsafeBytes { raw -> Int in
                guard let base = raw.baseAddress else { return 0 }
                return Darwin.write(fd, base, remaining.count)
            }
            if wrote <= 0 { break }
            remaining = remaining.advanced(by: wrote)
        }
    }
}
