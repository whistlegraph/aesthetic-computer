// Auth.swift — thin wrapper over the canonical shared ACSession.
//
// The whole AC macOS suite shares ONE sign-in via ~/.ac-token (written by
// `ac-login` / the AC Electron tray). ACSession.swift (copied from
// shared/swift/) does the real work — parsing, expiry, live file-watch
// broadcast, and the sign-in helper. This wrapper keeps DateWizard's existing
// call sites (currentToken / handle / runAcLogin) stable.
import AppKit

final class Auth {
    private let session = ACSession.shared

    static var tokenURL: URL { ACSession.tokenURL }

    /// The current valid access token, or nil if missing/expired.
    func currentToken() -> String? { session.token() }

    /// "@handle" for display, or nil — never email/PII.
    var handle: String? { session.displayName }

    /// Fire `onChange` (main queue) whenever the shared session changes —
    /// sign-in, sign-out, refresh — across any app in the suite. Replaces the
    /// old 2-second polling loop.
    @discardableResult
    func startWatching(_ onChange: @escaping () -> Void) -> UUID {
        session.startWatching(onChange)
    }
    func stopWatching(_ id: UUID) { session.stopWatching(id) }

    /// Native in-app sign-in — runs AC's OAuth2 + PKCE browser flow and writes
    /// ~/.ac-token directly, no CLI/Terminal. `completion` gets "@handle" (or an
    /// email/name fallback) on success. ACSession's file-watch ALSO fires the
    /// instant the token lands, so observers refresh on their own.
    func signIn(forcePrompt: Bool = false,
                completion: @escaping (Result<String, Error>) -> Void) {
        ACLogin.shared.signIn(forcePrompt: forcePrompt, completion: completion)
    }

    var isSigningIn: Bool { ACLogin.shared.isSigningIn }

    /// Fallback: launch the `ac-login` CLI in Terminal (used only if the native
    /// flow can't bind its loopback port).
    func runAcLogin() { session.runAcLogin() }
}
