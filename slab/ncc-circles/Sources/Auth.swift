// Auth.swift — the NCC login gate.
//
// OpenID Connect against the club's identity provider: authorization code +
// PKCE as a public client, with a loopback redirect the app listens on. The
// ID token's signature, issuer, audience, expiry and nonce are checked before
// anyone gets in. The refresh token lives in the login Keychain, so the next
// launch signs in without a browser.

import AppKit
import CryptoKit
import Foundation
import Network
import Security

struct Member {
    let sub: String
    let name: String
}

final class Auth {
    struct AuthError: Error { let message: String }
    typealias Done = (Result<Member, AuthError>) -> Void

    private var conf: AuthConf { Config.shared.conf.auth }
    private var listener: NWListener?
    private var discovery: [String: Any]?
    private let keychainService = "computer.aesthetic.ncc-circles"

    var configured: Bool { !conf.clientId.isEmpty }
    private var redirectURI: String { "http://127.0.0.1:\(conf.redirectPort)/callback" }

    // ─── entry points ────────────────────────────────────────────────────────

    // Silent sign-in from the stored refresh token.
    func restore(done: @escaping Done) {
        guard configured, let refresh = keychainRead() else {
            return done(.failure(AuthError(message: "not signed in")))
        }
        discover { r in
            switch r {
            case .failure(let e): done(.failure(e))
            case .success(let disco):
                self.token(disco, ["grant_type": "refresh_token", "refresh_token": refresh], nonce: nil, done: done)
            }
        }
    }

    // Interactive sign-in: opens the browser, waits for the loopback redirect.
    func login(done: @escaping Done) {
        guard configured else {
            return done(.failure(AuthError(message: "no OIDC client id in the config yet")))
        }
        discover { r in
            switch r {
            case .failure(let e): done(.failure(e))
            case .success(let disco): self.authorize(disco, done: done)
            }
        }
    }

    func logout() { keychainDelete() }

    // ─── flow ────────────────────────────────────────────────────────────────

    private func discover(done: @escaping (Result<[String: Any], AuthError>) -> Void) {
        if let d = discovery { return done(.success(d)) }
        let base = conf.issuer.hasSuffix("/") ? conf.issuer : conf.issuer + "/"
        guard let url = URL(string: base + ".well-known/openid-configuration") else {
            return done(.failure(AuthError(message: "bad issuer URL")))
        }
        getJSON(url) { j in
            guard let j, j["authorization_endpoint"] is String, j["token_endpoint"] is String else {
                return done(.failure(AuthError(message: "the NCC login server did not answer for this app")))
            }
            self.discovery = j
            done(.success(j))
        }
    }

    private func authorize(_ disco: [String: Any], done: @escaping Done) {
        let verifier = Self.random(48)
        let challenge = Self.b64url(Data(SHA256.hash(data: Data(verifier.utf8))))
        let state = Self.random(24), nonce = Self.random(24)

        listener?.cancel()
        let params = NWParameters.tcp
        params.requiredLocalEndpoint = .hostPort(host: "127.0.0.1",
                                                 port: NWEndpoint.Port(rawValue: UInt16(conf.redirectPort))!)
        params.allowLocalEndpointReuse = true
        guard let l = try? NWListener(using: params) else {
            return done(.failure(AuthError(message: "could not listen on port \(conf.redirectPort)")))
        }
        listener = l
        var finished = false
        let finish: Done = { r in
            guard !finished else { return }
            finished = true
            self.listener?.cancel()
            self.listener = nil
            done(r)
        }
        l.newConnectionHandler = { conn in
            conn.start(queue: .main)
            conn.receive(minimumIncompleteLength: 1, maximumLength: 16384) { data, _, _, _ in
                let head = data.flatMap { String(data: $0, encoding: .utf8) } ?? ""
                let path = head.split(separator: " ").dropFirst().first.map(String.init) ?? ""
                let q = URLComponents(string: "http://127.0.0.1" + path)?.queryItems ?? []
                let get = { (k: String) in q.first { $0.name == k }?.value }
                guard path.hasPrefix("/callback") else {
                    Self.reply(conn, "Not here.")
                    return
                }
                if let code = get("code"), get("state") == state {
                    Self.reply(conn, "You are signed in. Go back to NCC Circles.")
                    self.token(disco, ["grant_type": "authorization_code", "code": code,
                                       "redirect_uri": self.redirectURI, "code_verifier": verifier],
                               nonce: nonce, done: finish)
                } else {
                    Self.reply(conn, "Sign-in did not finish. Go back to NCC Circles and try again.")
                    finish(.failure(AuthError(message: get("error_description") ?? get("error") ?? "sign-in was refused")))
                }
            }
        }
        l.start(queue: .main)

        var c = URLComponents(string: disco["authorization_endpoint"] as! String)!
        c.queryItems = [
            .init(name: "response_type", value: "code"),
            .init(name: "client_id", value: conf.clientId),
            .init(name: "redirect_uri", value: redirectURI),
            .init(name: "scope", value: conf.scopes),
            .init(name: "state", value: state),
            .init(name: "nonce", value: nonce),
            .init(name: "code_challenge", value: challenge),
            .init(name: "code_challenge_method", value: "S256"),
        ]
        NSWorkspace.shared.open(c.url!)
        DispatchQueue.main.asyncAfter(deadline: .now() + 300) {
            finish(.failure(AuthError(message: "sign-in timed out")))
        }
    }

    private func token(_ disco: [String: Any], _ form: [String: String], nonce: String?, done: @escaping Done) {
        var form = form
        form["client_id"] = conf.clientId
        var req = URLRequest(url: URL(string: disco["token_endpoint"] as! String)!)
        req.httpMethod = "POST"
        req.setValue("application/x-www-form-urlencoded", forHTTPHeaderField: "Content-Type")
        var allowed = CharacterSet.alphanumerics
        allowed.insert(charactersIn: "-._~")
        req.httpBody = form.map { "\($0.key)=\($0.value.addingPercentEncoding(withAllowedCharacters: allowed) ?? "")" }
            .joined(separator: "&").data(using: .utf8)
        URLSession.shared.dataTask(with: req) { data, _, _ in
            let j = data.flatMap { try? JSONSerialization.jsonObject(with: $0) as? [String: Any] }
            DispatchQueue.main.async {
                guard let j, let idToken = j["id_token"] as? String else {
                    if form["grant_type"] == "refresh_token" { self.keychainDelete() }
                    let why = (j?["error_description"] as? String) ?? (j?["error"] as? String) ?? "no token came back"
                    return done(.failure(AuthError(message: why)))
                }
                self.verify(idToken, disco, nonce: nonce) { r in
                    if case .success = r, let refresh = j["refresh_token"] as? String {
                        self.keychainWrite(refresh)
                    }
                    done(r)
                }
            }
        }.resume()
    }

    // ─── ID token checks ─────────────────────────────────────────────────────

    private func verify(_ jwt: String, _ disco: [String: Any], nonce: String?, done: @escaping Done) {
        let parts = jwt.split(separator: ".").map(String.init)
        guard parts.count == 3,
              let header = Self.unb64url(parts[0]).flatMap({ try? JSONSerialization.jsonObject(with: $0) as? [String: Any] }),
              let claims = Self.unb64url(parts[1]).flatMap({ try? JSONSerialization.jsonObject(with: $0) as? [String: Any] }),
              let sig = Self.unb64url(parts[2]),
              header["alg"] as? String == "RS256",
              let jwks = (disco["jwks_uri"] as? String).flatMap(URL.init(string:)) else {
            return done(.failure(AuthError(message: "the ID token is malformed")))
        }
        let fail = { (m: String) in done(.failure(AuthError(message: m))) }
        let aud = (claims["aud"] as? [String]) ?? [(claims["aud"] as? String) ?? ""]
        guard claims["iss"] as? String == disco["issuer"] as? String else { return fail("the ID token has the wrong issuer") }
        guard aud.contains(conf.clientId) else { return fail("the ID token is for another app") }
        guard let exp = claims["exp"] as? Double, exp > Date().timeIntervalSince1970 else { return fail("the ID token has expired") }
        if let nonce, claims["nonce"] as? String != nonce { return fail("the ID token nonce does not match") }
        guard let sub = claims["sub"] as? String else { return fail("the ID token has no subject") }

        getJSON(jwks) { j in
            let keys = (j?["keys"] as? [[String: Any]]) ?? []
            let kid = header["kid"] as? String
            let signed = Data((parts[0] + "." + parts[1]).utf8)
            let ok = keys.contains { k in
                guard kid == nil || k["kid"] as? String == kid,
                      let n = (k["n"] as? String).flatMap(Self.unb64url),
                      let e = (k["e"] as? String).flatMap(Self.unb64url),
                      let key = Self.rsaKey(n: n, e: e) else { return false }
                return SecKeyVerifySignature(key, .rsaSignatureMessagePKCS1v15SHA256,
                                             signed as CFData, sig as CFData, nil)
            }
            guard ok else { return fail("the ID token signature does not verify") }
            let name = (claims["name"] as? String) ?? (claims["preferred_username"] as? String) ?? "member"
            done(.success(Member(sub: sub, name: name)))
        }
    }

    // RSAPublicKey DER (PKCS#1) from a JWK's modulus and exponent.
    private static func rsaKey(n: Data, e: Data) -> SecKey? {
        func len(_ n: Int) -> [UInt8] {
            if n < 128 { return [UInt8(n)] }
            var bytes: [UInt8] = [], v = n
            while v > 0 { bytes.insert(UInt8(v & 0xFF), at: 0); v >>= 8 }
            return [0x80 | UInt8(bytes.count)] + bytes
        }
        func int(_ d: Data) -> [UInt8] {
            var b = Array(d.drop { $0 == 0 })
            if b.isEmpty { b = [0] }
            if b[0] & 0x80 != 0 { b.insert(0, at: 0) }
            return [0x02] + len(b.count) + b
        }
        let body = int(n) + int(e)
        let der = Data([0x30] + len(body.count) + body)
        let attrs: [CFString: Any] = [kSecAttrKeyType: kSecAttrKeyTypeRSA, kSecAttrKeyClass: kSecAttrKeyClassPublic]
        return SecKeyCreateWithData(der as CFData, attrs as CFDictionary, nil)
    }

    // ─── helpers ─────────────────────────────────────────────────────────────

    private func getJSON(_ url: URL, done: @escaping ([String: Any]?) -> Void) {
        var req = URLRequest(url: url)
        req.timeoutInterval = 20
        URLSession.shared.dataTask(with: req) { data, resp, _ in
            let ok = (resp as? HTTPURLResponse)?.statusCode == 200
            let j = ok ? data.flatMap { try? JSONSerialization.jsonObject(with: $0) as? [String: Any] } : nil
            DispatchQueue.main.async { done(j) }
        }.resume()
    }

    private static func reply(_ conn: NWConnection, _ text: String) {
        let html = "<!doctype html><meta charset=utf-8><title>NCC Circles</title>"
            + "<body style='background:#CFEBD9;color:#1B2B22;font:18px ui-monospace,monospace;padding:3em'>\(text)</body>"
        let resp = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\n"
            + "Content-Length: \(html.utf8.count)\r\nConnection: close\r\n\r\n" + html
        conn.send(content: resp.data(using: .utf8), completion: .contentProcessed { _ in conn.cancel() })
    }

    private static func random(_ bytes: Int) -> String {
        var b = [UInt8](repeating: 0, count: bytes)
        _ = SecRandomCopyBytes(kSecRandomDefault, bytes, &b)
        return b64url(Data(b))
    }

    static func b64url(_ d: Data) -> String {
        d.base64EncodedString().replacingOccurrences(of: "+", with: "-")
            .replacingOccurrences(of: "/", with: "_").replacingOccurrences(of: "=", with: "")
    }

    static func unb64url(_ s: String) -> Data? {
        var t = s.replacingOccurrences(of: "-", with: "+").replacingOccurrences(of: "_", with: "/")
        while t.count % 4 != 0 { t += "=" }
        return Data(base64Encoded: t)
    }

    // ─── Keychain (refresh token) ────────────────────────────────────────────

    private var keychainQuery: [CFString: Any] {
        [kSecClass: kSecClassGenericPassword, kSecAttrService: keychainService, kSecAttrAccount: conf.issuer]
    }

    private func keychainRead() -> String? {
        var q = keychainQuery
        q[kSecReturnData] = true
        var out: CFTypeRef?
        guard SecItemCopyMatching(q as CFDictionary, &out) == errSecSuccess, let d = out as? Data else { return nil }
        return String(data: d, encoding: .utf8)
    }

    private func keychainWrite(_ s: String) {
        keychainDelete()
        var q = keychainQuery
        q[kSecValueData] = Data(s.utf8)
        SecItemAdd(q as CFDictionary, nil)
    }

    private func keychainDelete() { SecItemDelete(keychainQuery as CFDictionary) }
}
