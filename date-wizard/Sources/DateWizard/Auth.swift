// Auth.swift — reads the shared AC session token written by `ac-login`.
//
// The AC stack (ac-login CLI, ac-os, …) stores a single session token at
// ~/.ac-token as JSON:
//
//   { "access_token": "<jwt>", "refresh_token": "...", "expires_at": <ms-epoch> }
//
// DateWizard reuses this exact token as the Authorization Bearer for /api/cal.
// There is no device-pair / link-code flow anymore — signing in is just
// running `ac-login` in a terminal, which (re)writes ~/.ac-token.
import AppKit

final class Auth {
    // The on-disk shared token shape.
    private struct TokenFile: Codable {
        var access_token: String?
        var refresh_token: String?
        var expires_at: Double?     // ms-epoch
        // Optional identity hints — present in some writers, ignored if absent.
        var email: String?
        var sub: String?
        var handle: String?
    }

    // ~/.ac-token
    static var tokenURL: URL {
        FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".ac-token")
    }

    private func load() -> TokenFile? {
        guard let data = try? Data(contentsOf: Self.tokenURL) else { return nil }
        return try? JSONDecoder().decode(TokenFile.self, from: data)
    }

    // The current valid access token, or nil if the file is missing,
    // unparseable, has no access_token, or has expired.
    func currentToken() -> String? {
        guard let tf = load(),
              let token = tf.access_token, !token.isEmpty else { return nil }
        if let exp = tf.expires_at, exp < Date().timeIntervalSince1970 * 1000 {
            return nil   // expired
        }
        return token
    }

    // Best-effort identity for display. Often nil — never block on it.
    var handle: String? {
        guard let tf = load() else { return nil }
        return tf.handle ?? tf.email ?? tf.sub
    }

    // Launch `ac-login` in Terminal so the user can sign in without leaving
    // the app. (Re)writes ~/.ac-token on success.
    func runAcLogin() {
        let script = "tell application \"Terminal\"\nactivate\ndo script \"ac-login\"\nend tell"
        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/usr/bin/osascript")
        task.arguments = ["-e", script]
        try? task.run()
    }
}
