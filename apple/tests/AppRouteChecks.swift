import Foundation

@main struct AppRouteChecks {
    static func main() {
        precondition(AppRoute.windowTitle(for: nil) == "aesthetic.computer")
        precondition(AppRoute.windowTitle(for: URL(string: AppRoute.prompt)) == "prompt")
        precondition(AppRoute.windowTitle(for: URL(string: "https://aesthetic.computer/notepat?nogap=true")) == "notepat")
        precondition(AppRoute.windowTitle(for: URL(string: "https://aesthetic.computer/@Bash/hub")) == "@Bash/hub")
        func route(_ input: String) -> String? {
            AppRoute.destination(for: URL(string: input)!)
        }
        precondition(route("aesthetic://prompt") == AppRoute.prompt)
        precondition(route("aesthetic://") == AppRoute.prompt)
        precondition(route("aesthetic://@Bash/hub") == "https://aesthetic.computer/@Bash/hub")
        precondition(route("aesthetic://$cow") == "https://aesthetic.computer/$cow")
        precondition(route("aesthetic://merryo~%5Epads") == "https://aesthetic.computer/merryo~%5Epads")
        precondition(route("https://aesthetic.computer/notepat?x=1") == "https://aesthetic.computer/notepat?x=1")
        for link in ["mime://create", "mime://create?type=image%2Fpng",
                     "mime://remix?post=ac-123&version=0"] {
            precondition(AppRoute.mimeFallback(for: URL(string: link)!)?.host == "mime.ac")
        }
        for link in ["mime://other", "mime://create?type=bad", "mime://create?type=image/png&type=text/javascript",
                     "mime://user@create", "mime://create/path", "mime://remix?post=x&version=-1",
                     "mime://remix?post=../x&version=0", "mime://remix?post=x", "https://mime.ac/create"] {
            precondition(AppRoute.mimeFallback(for: URL(string: link)!) == nil, "Accepted MIME link: \(link)")
        }
        let letter = String(repeating: "a", count: 24)
        precondition(AppRoute.mimeFallback(for: URL(string: "mime://remix?post=work-123&version=4")!)?.absoluteString == "https://mime.ac/work-123")
        precondition(AppRoute.notificationDestination(["kind": "tell", "tellId": letter, "piece": "mail"])?.absoluteString == "https://aesthetic.computer/mail?letter=" + letter)
        precondition(AppRoute.notificationDestination(["kind": "tell", "tellId": "../other"])?.absoluteString == "https://aesthetic.computer/mail")
        precondition(AppRoute.notificationDestination(["piece": "mood"])?.absoluteString == "https://aesthetic.computer/mood")
        precondition(AppRoute.notificationDestination([:]) == nil)
        precondition(AppRoute.notificationDestination(["piece": "/evil.test"]) == nil)
        for input in ["file:///etc/passwd", "https://example.com/prompt",
                      "https://aesthetic.computer.evil.test/prompt",
                      "https://user@aesthetic.computer/prompt",
                      "http://aesthetic.computer/prompt", "aesthetic:///example.com"] {
            precondition(route(input) == nil, "Accepted an unsupported route: \(input)")
        }
        print("App routes passed")
    }
}
