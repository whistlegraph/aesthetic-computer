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

    /// Launch `ac-login` in Terminal to (re)write ~/.ac-token.
    func runAcLogin() { session.runAcLogin() }
}
