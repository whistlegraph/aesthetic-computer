import Foundation

@main @MainActor struct NativeWebAuthenticationChecks {
    static func main() throws {
        let callback = NativeWebAuthentication.redirect(for: "/callback?state=fixture-state&code=fixture-code", expectedState: "fixture-state")!
        precondition(callback.scheme == "aesel-auth" && callback.host == "callback")
        for target in ["/other?state=fixture-state&code=x", "/callback?state=wrong&code=x", "/callback?code=x", "/callback?state=fixture-state", "/callback?state=fixture-state&state=fixture-state&code=x", "/callback?state=fixture-state&code=x#fragment", "http://evil.test/callback?state=fixture-state&code=x"] {
            precondition(NativeWebAuthentication.redirect(for: target, expectedState: "fixture-state") == nil, "Unsafe callback accepted")
        }
        precondition(NativeWebAuthentication.redirect(for: "/callback?state=fixture-state&error=access_denied", expectedState: "fixture-state") != nil)
        print("System sign-in callback accepts only the fixed route and matching, unique OAuth state.")
    }
}
