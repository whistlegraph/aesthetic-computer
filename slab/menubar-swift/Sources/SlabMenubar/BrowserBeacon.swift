import Foundation
import Darwin

// ── browser beacon: which machine is this Chrome on? ─────────────────────
// The Claude Chrome extension names itself "Browser 1" and its only machine
// hint is the OS, which every Mac in the fleet shares. So a session navigates
// the extension's tab to http://127.0.0.1:5254/whoami: loopback resolves on
// whatever machine that Chrome runs on, and the page names that machine.
//
// A check-in is recorded only for a top-level navigation with no Referer and
// no Sec-Fetch-User: typed URLs and the extension's tabs.update (which Chrome
// labels Sec-Fetch-Site: cross-site) qualify; an embed, fetch or link click
// from a web page does not. The latest
// check-in rides along in this machine's ledger, which lets every menubar
// show where the extension was last seen.
struct BrowserCheckin: Codable {
    var host: String
    var at: Double            // ms since epoch
    var session: String?
}

final class BrowserBeacon {
    static let shared = BrowserBeacon()
    static let port: UInt16 = 5254
    static var file: String { "\(LedgerStore.dir)/browser.json" }

    private var fd: Int32 = -1
    private let queue = DispatchQueue(label: "slab.browser.beacon")
    private var host = ""

    func start() {
        guard fd < 0 else { return }
        fd = socket(AF_INET, SOCK_STREAM, 0)
        guard fd >= 0 else { return }
        var yes: Int32 = 1
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &yes, socklen_t(MemoryLayout<Int32>.size))
        setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &yes, socklen_t(MemoryLayout<Int32>.size))
        var addr = sockaddr_in()
        addr.sin_family = sa_family_t(AF_INET)
        addr.sin_port = Self.port.bigEndian
        addr.sin_addr.s_addr = inet_addr("127.0.0.1")   // loopback only
        let bound = withUnsafePointer(to: &addr) {
            $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
                bind(fd, $0, socklen_t(MemoryLayout<sockaddr_in>.size))
            }
        }
        guard bound == 0, listen(fd, 8) == 0 else {
            NSLog("🌐 [browser] beacon bind failed on :\(Self.port) errno=\(errno)")
            close(fd); fd = -1
            return
        }
        queue.async { [weak self] in self?.acceptLoop() }
    }

    func stop() { if fd >= 0 { close(fd); fd = -1 } }

    private func acceptLoop() {
        while fd >= 0 {
            let client = accept(fd, nil, nil)
            if client < 0 { break }
            handle(client)
        }
    }

    private func handle(_ client: Int32) {
        defer { close(client) }
        var tv = timeval(tv_sec: 2, tv_usec: 0)
        setsockopt(client, SOL_SOCKET, SO_RCVTIMEO, &tv, socklen_t(MemoryLayout<timeval>.size))
        var data = Data()
        var buf = [UInt8](repeating: 0, count: 8192)
        while data.range(of: Data("\r\n\r\n".utf8)) == nil, data.count < 32_768 {
            let n = read(client, &buf, buf.count)
            if n <= 0 { break }
            data.append(contentsOf: buf[0..<n])
        }
        let lines = String(decoding: data, as: UTF8.self).components(separatedBy: "\r\n")
        let request = lines.first?.split(separator: " ") ?? []
        guard request.count >= 2, request[0] == "GET",
              let url = URLComponents(string: "http://127.0.0.1" + request[1]),
              url.path == "/whoami" || url.path == "/whoami.json"
        else { return respond(client, status: "404 Not Found", type: "text/plain", body: "not found\n") }

        // The tailnet name, so the answer matches fleet naming (poorslice's
        // local hostname is "macbookpro"); the OS hostname covers no tailnet.
        if host.isEmpty {
            host = LedgerStore.selfIdentity().host.lowercased()
            if host.isEmpty {
                host = ProcessInfo.processInfo.hostName
                    .lowercased().split(separator: ".").first.map(String.init) ?? ""
            }
        }
        let headers = Set(lines.dropFirst().map { $0.lowercased() })
        let named = Set(headers.compactMap { $0.split(separator: ":").first.map(String.init) })
        let direct = headers.contains("sec-fetch-mode: navigate")
            && headers.contains("sec-fetch-dest: document")
            && !named.contains("referer") && !named.contains("sec-fetch-user")
        let session = url.queryItems?.first { $0.name == "session" }?.value
            .map { String($0.filter { $0.isLetter || $0.isNumber || "-_.:".contains($0) }.prefix(80)) }
        let checkin = BrowserCheckin(host: host, at: Date().timeIntervalSince1970 * 1000,
                                     session: session?.isEmpty == false ? session : nil)
        if direct, let out = try? JSONEncoder().encode(checkin) {
            try? out.write(to: URL(fileURLWithPath: Self.file), options: [.atomic])
        }

        let json = "{\"slab_whoami\":\"\(host)\",\"recorded\":\(direct)}"
        if url.path == "/whoami.json" {
            return respond(client, status: "200 OK", type: "application/json", body: json + "\n")
        }
        let html = """
        <!doctype html><meta charset="utf-8"><title>slab whoami: \(host)</title>
        <style>body{font:600 15vmin ui-monospace,monospace;display:grid;place-items:center;height:100vh;margin:0;background:#111;color:#7f7}</style>
        <body>\(host)<pre hidden id="slab-whoami">\(json)</pre>
        """
        respond(client, status: "200 OK", type: "text/html; charset=utf-8", body: html)
    }

    private func respond(_ client: Int32, status: String, type: String, body: String) {
        let bytes = Data(body.utf8)
        var out = Data(("HTTP/1.0 \(status)\r\nContent-Type: \(type)\r\n"
            + "Cache-Control: no-store\r\nContent-Length: \(bytes.count)\r\n"
            + "Connection: close\r\n\r\n").utf8)
        out.append(bytes)
        _ = out.withUnsafeBytes { write(client, $0.baseAddress, $0.count) }
    }

    // ── reading ──────────────────────────────────────────────────────────
    static func local() -> BrowserCheckin? {
        (try? Data(contentsOf: URL(fileURLWithPath: file)))
            .flatMap { try? JSONDecoder().decode(BrowserCheckin.self, from: $0) }
    }

    /// The most recent check-in across this machine and every cached peer ledger.
    static func latestInFleet() -> BrowserCheckin? {
        let fm = FileManager.default
        let peers = ((try? fm.contentsOfDirectory(atPath: LedgerStore.peersDir)) ?? [])
            .filter { $0.hasSuffix(".json") }
            .compactMap { name -> BrowserCheckin? in
                (try? Data(contentsOf: URL(fileURLWithPath: LedgerStore.peersDir + "/" + name)))
                    .flatMap { try? JSONDecoder().decode(Ledger.self, from: $0) }?.browser
            }
        return (peers + [local()].compactMap { $0 }).max { $0.at < $1.at }
    }
}
