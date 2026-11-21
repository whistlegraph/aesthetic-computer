// SignalEngine.swift — the Signal side of NCC Circles.
//
// The app is a Signal *linked device*, like Signal Desktop. It keeps its own
// signal-cli data directory, so it has its own device link and never contends
// with another signal-cli user on the same Mac. One long-lived
// `signal-cli jsonRpc` child carries everything: requests go down its stdin as
// JSON-RPC lines, replies and incoming envelopes come back on stdout.

import Foundation

struct SignalAccount {
    let number: String
    let uuid: String
}

#if ENGINE_LIB
// What the library's thread hands back: one JSON line. It goes to the main queue,
// and to an engine that is still there.
private final class EngineBox {
    weak var engine: SignalEngine?
}

private let engineSaid: @convention(c) (UnsafeMutableRawPointer?, UnsafePointer<CChar>?) -> Void = { ctx, json in
    guard let ctx, let json else { return }
    let line = Data(bytes: json, count: strlen(json))
    let box = Unmanaged<EngineBox>.fromOpaque(ctx).takeUnretainedValue()
    DispatchQueue.main.async { box.engine?.said(line) }
}

// The key of the library's store: 32 random bytes, in the login Keychain, as the log's key is.
enum EngineKey {
    private static let service = Paths.id + ".engine"
    private static var query: [CFString: Any] { [kSecClass: kSecClassGenericPassword, kSecAttrService: service, kSecAttrAccount: "key"] }

    static func get() -> String? {
        // Test aid: NCC_ENGINE_KEY names the key (64 hex characters), and the Keychain is not asked.
        if let k = ProcessInfo.processInfo.environment["NCC_ENGINE_KEY"], k.count == 64, k.allSatisfy({ $0.isHexDigit }) { return k.lowercased() }
        var read = query
        read[kSecReturnData] = true
        var out: CFTypeRef?
        let status = SecItemCopyMatching(read as CFDictionary, &out)
        if status == errSecSuccess, let d = out as? Data, d.count == 32 { return d.map { String(format: "%02x", $0) }.joined() }
        guard status == errSecItemNotFound else { return nil }      // refused, or locked: never make a second key
        var bytes = [UInt8](repeating: 0, count: 32)
        guard SecRandomCopyBytes(kSecRandomDefault, 32, &bytes) == errSecSuccess else { return nil }
        var add = query
        add[kSecValueData] = Data(bytes)
        add[kSecAttrLabel] = "NCCd Signal store key"
        return SecItemAdd(add as CFDictionary, nil) == errSecSuccess ? bytes.map { String(format: "%02x", $0) }.joined() : nil
    }

    static func forget() { SecItemDelete(query as CFDictionary) }
}
#endif

final class SignalEngine {
    private var proc: Process?
    private var stdin: FileHandle?
    private var buffer = Data()
    private var pending: [String: (Result<Any, EngineError>) -> Void] = [:]
    private var nextId = 1

    var onEnvelope: (([String: Any]) -> Void)?
    var onExit: ((String) -> Void)?
    private var stderrTail = ""

    struct EngineError: Error { let message: String }

    // ─── the engine inside the app ───────────────────────────────────────────
    // engine/ is a library on libsignal. It takes the same requests and gives the same
    // answers as signal-cli's jsonRpc, so the rest of this file's callers see no difference.
    #if ENGINE_LIB
    private var lib: OpaquePointer?
    private var box: Unmanaged<EngineBox>?
    private var linkURI: ((String) -> Void)?
    private var linkDone: ((Result<Void, EngineError>) -> Void)?
    var onIdentityChanged: ((String) -> Void)?       // a member's safety number is not the one seen before

    private static var accountFile: URL { Config.shared.signalConfigDir.appendingPathComponent("account.json") }

    private func startLibrary(link name: String?) throws {
        guard lib == nil else { return }
        guard let key = EngineKey.get() else { throw EngineError(message: "the Keychain did not give the Signal store's key") }
        let dir = Config.shared.signalConfigDir
        try? FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        Vault.lock(dir, dir: true)
        var conf: [String: Any] = ["db": dir.appendingPathComponent("store.db").path, "key": key,
                                   "attachments": dir.appendingPathComponent("attachments").path]
        if let name { conf["link"] = name }
        guard let d = try? JSONSerialization.data(withJSONObject: conf), let text = String(data: d, encoding: .utf8) else { return }
        let holder = EngineBox()
        holder.engine = self
        let kept = Unmanaged.passRetained(holder)
        guard let made = nccd_engine_start(text, engineSaid, kept.toOpaque()) else {
            kept.release()
            throw EngineError(message: "the Signal engine did not start")
        }
        lib = made
        box = kept
    }

    private func stopLibrary() {
        guard let lib else { return }
        self.lib = nil
        box?.takeUnretainedValue().engine = nil      // lines on their way to the main queue reach no one
        nccd_engine_stop(lib)                        // no line is made after this returns
        box?.release()
        box = nil
        let waiting = pending
        pending = [:]
        for (_, cb) in waiting { cb(.failure(EngineError(message: "Signal stopped"))) }
    }

    fileprivate func said(_ line: Data) {
        guard let j = try? JSONSerialization.jsonObject(with: line) as? [String: Any] else { return }
        let params = (j["params"] as? [String: Any]) ?? [:]
        switch j["method"] as? String {
        case "linkUri":
            if let uri = params["uri"] as? String { linkURI?(uri) }
        case "account":
            // The account is linked. Its id and number are kept beside the store: they say "linked" at the next launch.
            if let d = try? JSONSerialization.data(withJSONObject: ["uuid": params["uuid"] ?? "", "number": params["number"] ?? ""]) {
                try? d.write(to: Self.accountFile, options: .atomic)
                Vault.lock(Self.accountFile)
            }
            let done = linkDone
            linkDone = nil; linkURI = nil
            done?(.success(()))
        case "exit":
            let why = (params["reason"] as? String) ?? "stopped"
            if why == "not linked" { try? FileManager.default.removeItem(at: Self.accountFile) }
            let done = linkDone
            linkDone = nil; linkURI = nil
            stopLibrary()
            if let done { done(.failure(EngineError(message: why == "stopped" ? "linking was cancelled" : why))) }
            else if why != "stopped" { onExit?(why) }
        case "identityChanged":
            if let who = params["uuid"] as? String { onIdentityChanged?(who) }
        default:
            feed(line + Data([0x0A]))                // a reply, or a message: the same lines signal-cli gives
        }
    }
    #endif

    // The linked account, read from signal-cli's own accounts file.
    static func account() -> SignalAccount? {
        #if ENGINE_LIB
        if Config.shared.usesLibrary {
            guard let d = try? Data(contentsOf: accountFile),
                  let j = try? JSONSerialization.jsonObject(with: d) as? [String: String],
                  let uuid = j["uuid"], !uuid.isEmpty else { return nil }
            return SignalAccount(number: j["number"] ?? "", uuid: uuid)
        }
        #endif
        let f = Config.shared.signalConfigDir.appendingPathComponent("data/accounts.json")
        guard let d = try? Data(contentsOf: f),
              let j = try? JSONSerialization.jsonObject(with: d) as? [String: Any],
              let a = (j["accounts"] as? [[String: Any]])?.first,
              let n = a["number"] as? String else { return nil }
        return SignalAccount(number: n, uuid: (a["uuid"] as? String) ?? "")
    }

    func start(account: SignalAccount) throws {
        #if ENGINE_LIB
        if Config.shared.usesLibrary { return try startLibrary(link: nil) }
        #endif
        guard let cli = Config.shared.signalCli else {
            throw EngineError(message: "signal-cli is not installed (brew install signal-cli)")
        }
        let p = Process()
        p.executableURL = URL(fileURLWithPath: cli)
        p.arguments = ["--config", Config.shared.signalConfigDir.path, "-a", account.number, "jsonRpc", "--ignore-stories"]
        let inPipe = Pipe(), outPipe = Pipe(), errPipe = Pipe()
        p.standardInput = inPipe
        p.standardOutput = outPipe
        p.standardError = errPipe
        outPipe.fileHandleForReading.readabilityHandler = { [weak self] h in
            let d = h.availableData
            guard !d.isEmpty else { return }
            DispatchQueue.main.async { self?.feed(d) }
        }
        errPipe.fileHandleForReading.readabilityHandler = { [weak self] h in
            let d = h.availableData
            guard !d.isEmpty, let s = String(data: d, encoding: .utf8) else { return }
            DispatchQueue.main.async {
                guard let self else { return }
                self.stderrTail = String((self.stderrTail + s).suffix(600))
            }
        }
        p.terminationHandler = { [weak self] _ in
            DispatchQueue.main.async {
                guard let self else { return }
                let waiting = self.pending
                self.pending = [:]
                for (_, cb) in waiting { cb(.failure(EngineError(message: "signal-cli stopped"))) }
                self.onExit?(self.stderrTail.trimmingCharacters(in: .whitespacesAndNewlines))
            }
        }
        try p.run()
        proc = p
        stdin = inPipe.fileHandleForWriting
    }

    func stop() {
        #if ENGINE_LIB
        stopLibrary()
        #endif
        proc?.terminationHandler = nil
        proc?.terminate()
        proc = nil
    }

    // One JSON-RPC request. The completion runs on the main queue.
    func call(_ method: String, _ params: [String: Any] = [:],
              done: @escaping (Result<Any, EngineError>) -> Void) {
        #if ENGINE_LIB
        if Config.shared.usesLibrary {
            guard let lib else { return done(.failure(EngineError(message: "Signal is not running"))) }
            let id = String(nextId)
            nextId += 1
            let req: [String: Any] = ["jsonrpc": "2.0", "id": id, "method": method, "params": params]
            guard let d = try? JSONSerialization.data(withJSONObject: req), let text = String(data: d, encoding: .utf8),
                  nccd_engine_request(lib, text) == 0 else {
                return done(.failure(EngineError(message: "Signal did not take the request")))
            }
            pending[id] = done
            return
        }
        #endif
        guard let stdin, proc?.isRunning == true else {
            return done(.failure(EngineError(message: "signal-cli is not running")))
        }
        let id = String(nextId)
        nextId += 1
        pending[id] = done
        let req: [String: Any] = ["jsonrpc": "2.0", "id": id, "method": method, "params": params]
        guard var d = try? JSONSerialization.data(withJSONObject: req) else { return }
        d.append(0x0A)
        try? stdin.write(contentsOf: d)
    }

    private func feed(_ d: Data) {
        buffer.append(d)
        while let nl = buffer.firstIndex(of: 0x0A) {
            let line = buffer.subdata(in: buffer.startIndex..<nl)
            buffer.removeSubrange(buffer.startIndex...nl)
            guard !line.isEmpty,
                  let j = try? JSONSerialization.jsonObject(with: line) as? [String: Any] else { continue }
            if let id = j["id"] as? String, let cb = pending.removeValue(forKey: id) {
                if let e = j["error"] as? [String: Any] {
                    cb(.failure(EngineError(message: (e["message"] as? String) ?? "signal-cli error")))
                } else {
                    cb(.success(j["result"] ?? NSNull()))
                }
            } else if (j["method"] as? String) == "receive",
                      let params = j["params"] as? [String: Any],
                      let env = params["envelope"] as? [String: Any] {
                onEnvelope?(env)
            }
        }
    }

    // ─── Signal login ────────────────────────────────────────────────────────
    // `signal-cli link` prints a sgnl://linkdevice URI, then waits for the
    // phone to scan it. The app shows that URI as a QR code.
    private var linkProc: Process?

    func link(deviceName: String, onURI: @escaping (String) -> Void,
              done: @escaping (Result<Void, EngineError>) -> Void) {
        #if ENGINE_LIB
        if Config.shared.usesLibrary {
            // The engine that links is the engine that then runs: it is not started a second time.
            stopLibrary()
            linkURI = onURI
            linkDone = done
            do { try startLibrary(link: deviceName) } catch {
                linkURI = nil; linkDone = nil
                done(.failure((error as? EngineError) ?? EngineError(message: error.localizedDescription)))
            }
            return
        }
        #endif
        guard let cli = Config.shared.signalCli else {
            return done(.failure(EngineError(message: "signal-cli is not installed (brew install signal-cli)")))
        }
        let dir = Config.shared.signalConfigDir
        try? FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        let p = Process()
        p.executableURL = URL(fileURLWithPath: cli)
        p.arguments = ["--config", dir.path, "link", "-n", deviceName]
        let out = Pipe(), err = Pipe()
        p.standardOutput = out
        p.standardError = err
        var sawURI = false
        var errText = ""
        out.fileHandleForReading.readabilityHandler = { h in
            guard let s = String(data: h.availableData, encoding: .utf8) else { return }
            for line in s.split(separator: "\n") where line.hasPrefix("sgnl://") && !sawURI {
                sawURI = true
                let uri = String(line)
                DispatchQueue.main.async { onURI(uri) }
            }
        }
        err.fileHandleForReading.readabilityHandler = { h in
            if let s = String(data: h.availableData, encoding: .utf8) { errText += s }
        }
        p.terminationHandler = { p in
            DispatchQueue.main.async {
                if p.terminationStatus == 0, SignalEngine.account() != nil {
                    done(.success(()))
                } else {
                    let tail = errText.trimmingCharacters(in: .whitespacesAndNewlines).suffix(300)
                    done(.failure(EngineError(message: tail.isEmpty ? "linking was cancelled" : String(tail))))
                }
            }
        }
        do { try p.run(); linkProc = p } catch {
            done(.failure(EngineError(message: error.localizedDescription)))
        }
    }

    func cancelLink() {
        #if ENGINE_LIB
        if linkDone != nil { linkDone = nil; linkURI = nil; stopLibrary() }
        #endif
        linkProc?.terminate(); linkProc = nil
    }

    // The library's own unlink: Signal takes the device off the account, and its keys go from this Mac.
    func unlinkLibrary(done: @escaping () -> Void) {
        #if ENGINE_LIB
        call("unlink") { [weak self] _ in
            self?.stopLibrary()
            try? FileManager.default.removeItem(at: Config.shared.signalConfigDir)
            EngineKey.forget()
            done()
        }
        #else
        done()
        #endif
    }
}
