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
        for input in ["file:///etc/passwd", "https://example.com/prompt",
                      "https://aesthetic.computer.evil.test/prompt",
                      "https://user@aesthetic.computer/prompt",
                      "http://aesthetic.computer/prompt", "aesthetic:///example.com"] {
            precondition(route(input) == nil, "Accepted an unsupported route: \(input)")
        }
        print("App routes passed")
    }
}
