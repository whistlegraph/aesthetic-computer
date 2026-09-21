import Foundation

@main struct NativeIntegrationChecks {
    static func main() throws {
        let viewport = CGSize(width: 440, height: 500)
        let original = PreviewBounds()
        let larger = original.resized(edge: "sw", translation: CGSize(width: -60, height: 40), in: viewport)
        precondition(larger.width == 238 && larger.height == original.height + 40)
        precondition(larger.right == original.right && larger.top == original.top)
        for edge in ["n","s","w","e","nw","ne","sw","se"] {
            let result = original.resized(edge: edge, translation: CGSize(width: -5000, height: 5000), in: viewport)
            precondition(result.width >= 96 && result.height >= 72 && result.right >= 8 && result.top >= 8)
            precondition(result.width + result.right <= viewport.width - 16)
            precondition(result.height + result.top <= viewport.height - 48)
        }
        for delta in [-5000.0, -180, -30, 0, 30, 180, 5000] {
            let east = original.resized(edge: "e", translation: CGSize(width: delta, height: 0), in: viewport)
            precondition(abs(east.width + east.right - original.width - original.right) < 0.001)
            let west = original.resized(edge: "w", translation: CGSize(width: delta, height: 0), in: viewport)
            precondition(west.right == original.right)
            let north = original.resized(edge: "n", translation: CGSize(width: 0, height: delta), in: viewport)
            precondition(abs(north.top + north.height - original.top - original.height) < 0.001)
        }
        var auth = try NativeSignIn()
        precondition(auth.url.host == "hi.aesthetic.computer" && auth.url.path == "/authorize")
        let query = URLComponents(url: auth.url, resolvingAgainstBaseURL: false)!.queryItems!
        precondition(query.contains { $0.name == "code_challenge_method" && $0.value == "S256" })
        precondition(!auth.url.absoluteString.contains(auth.verifier))
        let wrong = URL(string: NativeSignIn.callback + "?code=test&state=wrong")!
        do { _ = try auth.exchangeBody(for: wrong); preconditionFailure("Wrong state accepted") } catch {}
        let callback = URL(string: NativeSignIn.callback + "?code=test&state=" + auth.state)!
        let body = try auth.exchangeBody(for: callback)
        let payload = try JSONSerialization.jsonObject(with: body) as! [String:String]
        precondition(payload["code_verifier"] == auth.verifier && payload["code"] == "test")
        do { _ = try auth.exchangeBody(for: callback); preconditionFailure("Callback replay accepted") } catch {}
        precondition(!NativeSignIn.isCallback(URL(string: "https://example.com/callback")!))
        let interrupted = NSError(domain: "WebKitErrorDomain", code: 102)
        precondition(NativeSignIn.ignoresNavigationFailure(interrupted, callbackAccepted: true, presented: true))
        precondition(!NativeSignIn.ignoresNavigationFailure(interrupted, callbackAccepted: false, presented: true))
        let offline = NSError(domain: NSURLErrorDomain, code: NSURLErrorNotConnectedToInternet)
        precondition(!NativeSignIn.ignoresNavigationFailure(offline, callbackAccepted: false, presented: true))
        precondition(NativeSignIn.ignoresNavigationFailure(offline, callbackAccepted: false, presented: false))
        precondition(NativeSignIn.ignoresNavigationFailure(NSError(domain: NSURLErrorDomain, code: NSURLErrorCancelled), callbackAccepted: false, presented: true))
        print("Resize bounds and direct PKCE sign-in checks passed")
    }
}
