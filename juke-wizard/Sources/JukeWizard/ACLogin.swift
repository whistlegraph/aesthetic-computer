// ACLogin.swift — native, in-app AC sign-in for the macOS app suite.
//
// SOURCE OF TRUTH: shared/swift/ACLogin.swift. Like ACSession.swift, the
// standalone SwiftPM apps (date-wizard, wave-wizard, clip-wizard, juke-wizard,
// slab menubar, menuband) each keep a COPY in their own Sources/. When you
// change this file, re-copy it into each consumer:
//   cp shared/swift/ACLogin.swift <app>/Sources/<App>/
//
// This is a native port of `tezos/ac-login.mjs` — the same OAuth 2.0
// Authorization-Code + PKCE flow against Auth0 (`hi.aesthetic.computer`) with a
// localhost loopback callback on port 44233. It writes the SAME ~/.ac-token
// JSON shape that ACSession.swift reads, so a sign-in here is indistinguishable
// from one done by the `ac-login` CLI — the whole suite picks it up live via
// the shared file-watch. No Terminal, no Node, no CLI dependency.
//
//   ACLogin.shared.signIn { result in
//       switch result {
//       case .success(let handle): …   // "@handle" (or email/name fallback)
//       case .failure(let error):  …
//       }
//   }
//
// The callback fires on the main queue. ACSession's file-watch will ALSO fire
// independently the instant the token lands, so UIs that already observe the
// session refresh on their own — the completion handler is for surfacing
// progress/errors on the sign-in screen itself.
import Foundation
import AppKit
import CryptoKit

final class ACLogin {
    static let shared = ACLogin()

    // ── config (must match ac-login.mjs / Auth0 allowed callback) ────────
    private let authDomain = "hi.aesthetic.computer"
    private let clientID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt"
    private let callbackPort: UInt16 = 44233
    private var redirectURI: String { "http://localhost:\(callbackPort)/callback" }

    enum LoginError: LocalizedError {
        case portInUse
        case serverFailed(String)
        case stateMismatch
        case tokenExchange(String)
        case cancelled
        case timeout

        var errorDescription: String? {
            switch self {
            case .portInUse:           return "Port \(44233) is in use — close any running ac-login and retry."
            case .serverFailed(let m): return "Sign-in server failed: \(m)"
            case .stateMismatch:       return "Sign-in state mismatch — please retry."
            case .tokenExchange(let m): return "Token exchange failed: \(m)"
            case .cancelled:           return "Sign-in cancelled."
            case .timeout:             return "Sign-in timed out."
            }
        }
    }

    private var listener: SocketListener?
    private var inFlight = false

    /// True while a sign-in is waiting on the browser/callback.
    var isSigningIn: Bool { inFlight }

    // ── PKCE ─────────────────────────────────────────────────────────────
    private func base64URL(_ data: Data) -> String {
        data.base64EncodedString()
            .replacingOccurrences(of: "+", with: "-")
            .replacingOccurrences(of: "/", with: "_")
            .replacingOccurrences(of: "=", with: "")
    }
    private func randomURLToken(byteCount: Int) -> String {
        var bytes = [UInt8](repeating: 0, count: byteCount)
        _ = SecRandomCopyBytes(kSecRandomDefault, byteCount, &bytes)
        return base64URL(Data(bytes))
    }

    // ── public entry ───────────────────────────────────────────────────
    /// Start the browser sign-in flow. `forcePrompt` adds `prompt=login` to
    /// force the Auth0 account chooser (the CLI's `ac-login fresh`).
    func signIn(forcePrompt: Bool = false,
                completion: @escaping (Result<String, Error>) -> Void) {
        if inFlight { listener?.stop(); inFlight = false }

        let verifier = randomURLToken(byteCount: 32)
        let challenge = base64URL(Data(SHA256.hash(data: Data(verifier.utf8))))
        let state = randomURLToken(byteCount: 16)

        let finish: (Result<String, Error>) -> Void = { [weak self] result in
            guard let self else { return }
            self.inFlight = false
            self.listener?.stop()
            self.listener = nil
            DispatchQueue.main.async { completion(result) }
        }

        let server: SocketListener
        do {
            server = try SocketListener(port: callbackPort)
        } catch {
            finish(.failure(LoginError.portInUse))
            return
        }
        listener = server
        inFlight = true

        // 5-minute safety timeout, matching the CLI.
        DispatchQueue.global().asyncAfter(deadline: .now() + 300) { [weak self] in
            guard let self, self.inFlight else { return }
            finish(.failure(LoginError.timeout))
        }

        server.onRequest = { [weak self] req, respond in
            guard let self else { return }
            guard req.path.contains("callback") else {
                respond(404, "text/plain", "Not found"); return
            }
            if let err = req.query["error"] {
                let desc = req.query["error_description"] ?? ""
                respond(200, "text/html; charset=utf-8", Self.failureHTML(err, desc))
                finish(.failure(LoginError.tokenExchange(desc.isEmpty ? err : desc)))
                return
            }
            guard let code = req.query["code"] else {
                respond(400, "text/plain", "Missing authorization code"); return
            }
            guard req.query["state"] == state else {
                respond(400, "text/plain", "State mismatch")
                finish(.failure(LoginError.stateMismatch))
                return
            }
            // Exchange code → tokens, fetch userinfo + handle, write ~/.ac-token.
            self.exchangeAndStore(code: code, verifier: verifier) { result in
                switch result {
                case .success(let display):
                    respond(200, "text/html; charset=utf-8", Self.successHTML(display))
                    finish(.success(display))
                case .failure(let error):
                    respond(500, "text/plain", "Error: \(error.localizedDescription)")
                    finish(.failure(error))
                }
            }
        }

        do {
            try server.start()
        } catch {
            finish(.failure(LoginError.serverFailed("\(error)")))
            return
        }

        openBrowser(authURL(state: state, challenge: challenge, forcePrompt: forcePrompt))
    }

    /// Stop a pending sign-in (e.g. user navigated away).
    func cancel() {
        guard inFlight else { return }
        inFlight = false
        listener?.stop()
        listener = nil
    }

    // ── token exchange + persistence ─────────────────────────────────────
    private func exchangeAndStore(code: String, verifier: String,
                                  completion: @escaping (Result<String, Error>) -> Void) {
        var req = URLRequest(url: URL(string: "https://\(authDomain)/oauth/token")!)
        req.httpMethod = "POST"
        req.setValue("application/json", forHTTPHeaderField: "Content-Type")
        let body: [String: String] = [
            "grant_type": "authorization_code",
            "client_id": clientID,
            "code_verifier": verifier,
            "code": code,
            "redirect_uri": redirectURI,
        ]
        req.httpBody = try? JSONSerialization.data(withJSONObject: body)

        URLSession.shared.dataTask(with: req) { [weak self] data, resp, err in
            guard let self else { return }
            if let err { completion(.failure(err)); return }
            guard let data,
                  let http = resp as? HTTPURLResponse, http.statusCode == 200,
                  let tokens = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let access = tokens["access_token"] as? String else {
                let txt = data.flatMap { String(data: $0, encoding: .utf8) } ?? "unknown"
                completion(.failure(LoginError.tokenExchange(txt)))
                return
            }
            self.fetchUserAndWrite(tokens: tokens, access: access, completion: completion)
        }.resume()
    }

    private func fetchUserAndWrite(tokens: [String: Any], access: String,
                                   completion: @escaping (Result<String, Error>) -> Void) {
        var req = URLRequest(url: URL(string: "https://\(authDomain)/userinfo")!)
        req.setValue("Bearer \(access)", forHTTPHeaderField: "Authorization")
        URLSession.shared.dataTask(with: req) { [weak self] data, _, _ in
            guard let self else { return }
            let user = (data.flatMap {
                (try? JSONSerialization.jsonObject(with: $0)) as? [String: Any]
            }) ?? [:]
            let sub = user["sub"] as? String
            self.fetchHandle(sub: sub) { handle in
                self.writeToken(tokens: tokens, user: user, handle: handle)
                ACSession.shared.broadcastChanged()
                let display = handle.map { "@\($0)" }
                    ?? (user["email"] as? String)
                    ?? (user["name"] as? String)
                    ?? "signed in"
                completion(.success(display))
            }
        }.resume()
    }

    private func fetchHandle(sub: String?, completion: @escaping (String?) -> Void) {
        guard let sub, let encoded = sub.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed),
              let url = URL(string: "https://aesthetic.computer/handle?for=\(encoded)") else {
            completion(nil); return
        }
        URLSession.shared.dataTask(with: url) { data, _, _ in
            let handle = data.flatMap {
                (try? JSONSerialization.jsonObject(with: $0) as? [String: Any])?["handle"] as? String
            }
            completion(handle)
        }.resume()
    }

    private func writeToken(tokens: [String: Any], user: [String: Any], handle: String?) {
        let expiresIn = (tokens["expires_in"] as? Double)
            ?? (tokens["expires_in"] as? Int).map(Double.init) ?? 0
        var payload: [String: Any] = [
            "access_token": tokens["access_token"] as? String ?? "",
            "expires_at": Date().timeIntervalSince1970 * 1000 + expiresIn * 1000,
        ]
        if let r = tokens["refresh_token"] as? String { payload["refresh_token"] = r }
        if let i = tokens["id_token"] as? String { payload["id_token"] = i }
        var u: [String: Any] = [:]
        if let v = user["email"] as? String { u["email"] = v }
        if let v = user["name"] as? String { u["name"] = v }
        if let v = user["sub"] as? String { u["sub"] = v }
        if let v = user["picture"] as? String { u["picture"] = v }
        if let handle { u["handle"] = handle }
        payload["user"] = u

        guard let data = try? JSONSerialization.data(
            withJSONObject: payload, options: [.prettyPrinted]) else { return }
        // Atomic write so ACSession's file-watch sees a single clean change.
        try? data.write(to: ACSession.tokenURL, options: [.atomic])
    }

    // ── URLs / browser ────────────────────────────────────────────────
    private func authURL(state: String, challenge: String, forcePrompt: Bool) -> URL {
        var c = URLComponents(string: "https://\(authDomain)/authorize")!
        var items = [
            URLQueryItem(name: "response_type", value: "code"),
            URLQueryItem(name: "client_id", value: clientID),
            URLQueryItem(name: "redirect_uri", value: redirectURI),
            URLQueryItem(name: "scope", value: "openid profile email offline_access"),
            URLQueryItem(name: "state", value: state),
            URLQueryItem(name: "code_challenge", value: challenge),
            URLQueryItem(name: "code_challenge_method", value: "S256"),
        ]
        if forcePrompt { items.append(URLQueryItem(name: "prompt", value: "login")) }
        c.queryItems = items
        return c.url!
    }

    private func openBrowser(_ url: URL) {
        DispatchQueue.main.async { NSWorkspace.shared.open(url) }
    }

    // ── browser-facing HTML (mirrors ac-login.mjs styling) ───────────────
    private static func successHTML(_ display: String) -> String {
        """
        <!DOCTYPE html><html lang="en"><head><meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Logged In · Aesthetic Computer</title><style>
        @media (prefers-color-scheme: dark){body{background:rgb(50,30,60);color:rgb(220,30,100)}strong{color:white}code{color:pink}}
        @media (prefers-color-scheme: light){body{background:rgb(255,240,245);color:rgb(180,20,80)}strong{color:rgb(100,20,60)}code{color:rgb(150,50,100)}}
        body{font-family:monospace;display:flex;width:100vw;height:100vh;margin:0}
        #wrapper{margin:auto;text-align:center;padding:2em}h1{font-family:sans-serif;font-weight:normal}code{font-size:120%;opacity:.75}
        </style></head><body><div id="wrapper"><h1>✅ Login Successful</h1>
        <p>Welcome, <strong>\(display)</strong>!</p><p><code>You may close this page.</code></p></div>
        <script>setTimeout(()=>window.close(),3000)</script></body></html>
        """
    }
    private static func failureHTML(_ error: String, _ desc: String) -> String {
        """
        <!DOCTYPE html><html lang="en"><head><meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Login Failed · Aesthetic Computer</title><style>
        @media (prefers-color-scheme: dark){body{background:rgb(50,30,60);color:rgb(220,30,100)}mark{background:maroon;color:pink}code{color:pink}}
        @media (prefers-color-scheme: light){body{background:rgb(255,240,245);color:rgb(180,20,80)}mark{background:rgb(255,200,220);color:rgb(150,20,60)}code{color:rgb(150,50,100)}}
        body{font-family:monospace;display:flex;width:100vw;height:100vh;margin:0}
        #wrapper{margin:auto;text-align:center;padding:2em}h1{font-family:sans-serif;font-weight:normal}code{font-size:120%;opacity:.75}
        </style></head><body><div id="wrapper"><h1>❌ Authentication Failed</h1>
        <p><mark>\(error)</mark></p><p>\(desc)</p><p><code>You may close this page.</code></p></div></body></html>
        """
    }
}

// ── minimal loopback HTTP server (BSD sockets) ───────────────────────────
// A single-connection-at-a-time HTTP/1.0 responder. Enough to receive Auth0's
// redirect GET on 127.0.0.1 and reply with a close-this-page page. No external
// deps; avoids pulling in Network.framework just for one request.
private final class SocketListener {
    struct Request { let path: String; let query: [String: String] }

    var onRequest: ((Request, _ respond: @escaping (Int, String, String) -> Void) -> Void)?

    private let port: UInt16
    private var fd: Int32 = -1
    private let queue = DispatchQueue(label: "ac.login.socket")
    private var running = false

    init(port: UInt16) throws {
        self.port = port
        fd = socket(AF_INET, SOCK_STREAM, 0)
        guard fd >= 0 else { throw ACLogin.LoginError.serverFailed("socket()") }
        var yes: Int32 = 1
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &yes, socklen_t(MemoryLayout<Int32>.size))

        var addr = sockaddr_in()
        addr.sin_family = sa_family_t(AF_INET)
        addr.sin_port = port.bigEndian
        addr.sin_addr.s_addr = inet_addr("127.0.0.1")
        let bound = withUnsafePointer(to: &addr) {
            $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
                bind(fd, $0, socklen_t(MemoryLayout<sockaddr_in>.size))
            }
        }
        guard bound == 0 else {
            close(fd); fd = -1
            throw ACLogin.LoginError.portInUse
        }
        guard listen(fd, 4) == 0 else {
            close(fd); fd = -1
            throw ACLogin.LoginError.serverFailed("listen()")
        }
    }

    func start() throws {
        running = true
        queue.async { [weak self] in self?.acceptLoop() }
    }

    func stop() {
        running = false
        if fd >= 0 { close(fd); fd = -1 }
    }

    private func acceptLoop() {
        while running {
            let client = accept(fd, nil, nil)
            if client < 0 { break }
            handle(client)
            // One callback redirect is all we need; keep looping in case the
            // browser retries (favicon, double-fetch) until stop() closes fd.
        }
    }

    private func handle(_ client: Int32) {
        defer { close(client) }
        var buf = [UInt8](repeating: 0, count: 8192)
        let n = read(client, &buf, buf.count)
        guard n > 0 else { return }
        let raw = String(decoding: buf[0..<n], as: UTF8.self)
        guard let line = raw.split(separator: "\r\n").first else { return }
        let parts = line.split(separator: " ")
        guard parts.count >= 2 else { return }
        let target = String(parts[1])

        let comps = URLComponents(string: "http://localhost\(target)")
        let path = comps?.path ?? target
        var query: [String: String] = [:]
        for item in comps?.queryItems ?? [] { query[item.name] = item.value }

        let request = Request(path: path, query: query)
        var responded = false
        let respond: (Int, String, String) -> Void = { status, contentType, body in
            guard !responded else { return }
            responded = true
            let bytes = Array(body.utf8)
            let header = "HTTP/1.0 \(status) \(status == 200 ? "OK" : "Error")\r\n"
                + "Content-Type: \(contentType)\r\n"
                + "Content-Length: \(bytes.count)\r\n"
                + "Connection: close\r\n\r\n"
            let out = Array(header.utf8) + bytes
            _ = out.withUnsafeBytes { write(client, $0.baseAddress, $0.count) }
        }

        if let onRequest {
            onRequest(request, respond)
        } else {
            respond(404, "text/plain", "Not found")
        }
        // Give async respond() (token exchange) time to flush before close.
        if !responded {
            let deadline = Date().addingTimeInterval(310)
            while !responded && Date() < deadline && running {
                usleep(50_000)
            }
        }
    }
}
