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

    // The linked account, read from signal-cli's own accounts file.
    static func account() -> SignalAccount? {
        let f = Config.shared.signalConfigDir.appendingPathComponent("data/accounts.json")
        guard let d = try? Data(contentsOf: f),
              let j = try? JSONSerialization.jsonObject(with: d) as? [String: Any],
              let a = (j["accounts"] as? [[String: Any]])?.first,
              let n = a["number"] as? String else { return nil }
        return SignalAccount(number: n, uuid: (a["uuid"] as? String) ?? "")
    }

    func start(account: SignalAccount) throws {
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
        proc?.terminationHandler = nil
        proc?.terminate()
        proc = nil
    }

    // One JSON-RPC request. The completion runs on the main queue.
    func call(_ method: String, _ params: [String: Any] = [:],
              done: @escaping (Result<Any, EngineError>) -> Void) {
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

    func cancelLink() { linkProc?.terminate(); linkProc = nil }
}
