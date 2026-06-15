// Auth.swift — device-pair flow to obtain the user's bearer token.
//
// Persisted to ~/Library/Application Support/DateWizard/auth.json as
//   { "token": "...", "handle": "...", "sub": "..." }
//
// Flow:
//   1. POST  /.netlify/functions/device-pair  {action:"create"}  → { code }
//   2. Show the code; instruct the user to open prompt.ac (logged in)
//      and type:  link <CODE>
//   3. Poll GET /.netlify/functions/device-pair?code=<CODE> every ~2s.
//      When it returns { status:"claimed", token, handle, sub }, save and proceed.
//
// The `token` is the user's Auth0 bearer — used as the Authorization Bearer
// for /api/cal.
import AppKit

// Persisted credentials.
struct AuthCreds: Codable {
    var token: String
    var handle: String?
    var sub: String?
}

// device-pair wire shapes.
private struct CreateResponse: Codable { var code: String }
private struct PollResponse: Codable {
    var status: String?           // "pending" | "claimed"
    var token: String?
    var handle: String?
    var sub: String?
}

final class Auth {
    static let base = "https://aesthetic.computer"
    static let pairPath = "/.netlify/functions/device-pair"

    private let session: URLSession
    private var pollTimer: Timer?
    private(set) var pairingCode: String?

    init() {
        let cfg = URLSessionConfiguration.default
        cfg.timeoutIntervalForRequest = 20
        cfg.requestCachePolicy = .reloadIgnoringLocalCacheData
        self.session = URLSession(configuration: cfg)
    }

    // ── persistence ──────────────────────────────────────────────────

    static var supportDir: URL {
        let base = FileManager.default.urls(for: .applicationSupportDirectory,
                                            in: .userDomainMask).first!
        return base.appendingPathComponent("DateWizard", isDirectory: true)
    }
    static var authFile: URL { supportDir.appendingPathComponent("auth.json") }

    static func loadCreds() -> AuthCreds? {
        guard let data = try? Data(contentsOf: authFile) else { return nil }
        return try? JSONDecoder().decode(AuthCreds.self, from: data)
    }

    static func saveCreds(_ creds: AuthCreds) {
        try? FileManager.default.createDirectory(at: supportDir,
                                                 withIntermediateDirectories: true)
        if let data = try? JSONEncoder().encode(creds) {
            try? data.write(to: authFile, options: .atomic)
        }
    }

    static func clearCreds() {
        try? FileManager.default.removeItem(at: authFile)
    }

    // ── pairing ──────────────────────────────────────────────────────

    // Request a fresh pairing code. Calls back on the main queue.
    func createCode(completion: @escaping (Result<String, Error>) -> Void) {
        var req = URLRequest(url: URL(string: "\(Self.base)\(Self.pairPath)")!)
        req.httpMethod = "POST"
        req.setValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpBody = try? JSONSerialization.data(withJSONObject: ["action": "create"])
        let task = session.dataTask(with: req) { [weak self] data, _, error in
            DispatchQueue.main.async {
                if let error { completion(.failure(error)); return }
                guard let data,
                      let resp = try? JSONDecoder().decode(CreateResponse.self, from: data) else {
                    completion(.failure(AuthError.badResponse)); return
                }
                self?.pairingCode = resp.code
                completion(.success(resp.code))
            }
        }
        task.resume()
    }

    // Poll the given code every ~2s until it is claimed; on success persist
    // the creds and invoke onClaimed on the main queue. Stops on success.
    func startPolling(code: String, onClaimed: @escaping (AuthCreds) -> Void) {
        stopPolling()
        let timer = Timer(timeInterval: 2.0, repeats: true) { [weak self] _ in
            self?.pollOnce(code: code) { creds in
                guard let creds else { return }
                self?.stopPolling()
                Auth.saveCreds(creds)
                onClaimed(creds)
            }
        }
        RunLoop.main.add(timer, forMode: .common)
        pollTimer = timer
        // Fire one immediately so we don't wait a full interval.
        timer.fire()
    }

    func stopPolling() {
        pollTimer?.invalidate()
        pollTimer = nil
    }

    private func pollOnce(code: String, completion: @escaping (AuthCreds?) -> Void) {
        var comps = URLComponents(string: "\(Self.base)\(Self.pairPath)")!
        comps.queryItems = [URLQueryItem(name: "code", value: code)]
        let task = session.dataTask(with: comps.url!) { data, _, _ in
            DispatchQueue.main.async {
                guard let data,
                      let resp = try? JSONDecoder().decode(PollResponse.self, from: data),
                      resp.status == "claimed",
                      let token = resp.token, !token.isEmpty else {
                    completion(nil); return
                }
                completion(AuthCreds(token: token, handle: resp.handle, sub: resp.sub))
            }
        }
        task.resume()
    }

    // Open prompt.ac so the user can claim the code there.
    static func openPromptAC() {
        if let url = URL(string: "https://prompt.ac") {
            NSWorkspace.shared.open(url)
        }
    }
}

enum AuthError: Error, LocalizedError {
    case badResponse
    var errorDescription: String? { "Pairing service returned an unexpected response." }
}
