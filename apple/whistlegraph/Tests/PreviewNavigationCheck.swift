import Foundation

@main struct PreviewNavigationCheck {
    static func main() {
        let runtime = "https://aesthetic.computer/wipe?noauth=true&noplot=true&nogap=true&nolabel=true&preview=walkieware"
        func allowed(_ value: String, main: Bool? = false, document: PreviewNavigation.Document = .workspace) -> Bool {
            PreviewNavigation.allows(URL(string: value), mainFrame: main, document: document)
        }
        precondition(allowed("walkieware://app/index.html?whistlegraph=1", main: true))
        precondition(!allowed(runtime.replacingOccurrences(of: "preview=walkieware", with: "preview=whistlegraph")), "Use the deployed runtime's private preview marker")
        precondition(!allowed("walkieware://app/index.html?whistlegraph=1&walkie=1", main: true))
        precondition(!allowed(runtime + "&preview=whistlegraph"))
        precondition(!allowed(runtime.replacingOccurrences(of: "preview=walkieware", with: "preview=unknown")))
        precondition(PreviewNavigation.bridge(URL(string: "walkieware://app/index.html?whistlegraph=1"), mainFrame: true, document: .workspace) == .workspace)
        precondition(allowed("walkieware://app/index.html?walkie=1", main: true))
        precondition(allowed("walkieware://app/index.html?walkie=1#local", main: true))
        precondition(allowed("walkieware://app/story.html", main: true, document: .story))
        precondition(allowed(runtime) && allowed(runtime, document: .story))
        let rewritten = "https://aesthetic.computer/wipe?nogap=true&nolabel=true&noauth=true&preview=walkieware"
        precondition(allowed(rewritten), "The runtime rewrites its URL after boot; the frame stays the artwork frame")
        precondition(allowed("https://aesthetic.computer/whistlegraph-preview?nogap=true&nolabel=true&noauth=true&preview=walkieware"))
        precondition(!allowed("https://aesthetic.computer/wipe?nogap=true&nolabel=true&preview=walkieware"), "noauth is required")
        precondition(!allowed(runtime + "&extra=1"), "Unknown flags are not artwork")
        precondition(!allowed("https://aesthetic.computer/wipe/extra?nogap=true&nolabel=true&noauth=true&preview=walkieware"))
        precondition(PreviewNavigation.bridge(URL(string: rewritten), mainFrame: false, document: .workspace) == .artwork)
        precondition(allowed("about:blank"))
        for value in ["walkieware://app/easel/phone/host.html", "https://aesthetic.computer/braincells/",
                      "https://aesthetic.computer/mint/#secret", "https://checkout.stripe.com/c/pay",
                      "https://pay.aesthetic.computer/", "temple://connect", "javascript:alert(1)",
                      "https://aesthetic.computer.evil.invalid/wipe", "https://evil.invalid/?next=wipe",
                      "data:text/html,checkout", "walkieware://app/index.html?walkie=1&checkout=1"] {
            precondition(!allowed(value) && !allowed(value, main: true) && !allowed(value, main: nil), value)
        }
        precondition(!allowed(runtime, main: true), "The runtime cannot replace native app's root document")
        precondition(!allowed(runtime, main: nil), "New windows are not artwork frames")
        precondition(!allowed(runtime + "&noauth=false"), "Duplicate flags cannot bypass preview constraints")
        precondition(!allowed(runtime.replacingOccurrences(of: "noauth=true", with: "noauth=false")))
        precondition(!allowed(runtime.replacingOccurrences(of: "https://", with: "http://")))
        precondition(!allowed(runtime.replacingOccurrences(of: "aesthetic.computer/", with: "user@aesthetic.computer/")))
        precondition(!allowed(runtime.replacingOccurrences(of: "aesthetic.computer/", with: "aesthetic.computer:443/")))
        precondition(!allowed("walkieware://app/story.html", main: true))
        precondition(!allowed("walkieware://app/index.html?walkie=1", main: true, document: .story))
        precondition(PreviewNavigation.bridge(URL(string: runtime), mainFrame: false, document: .workspace) == .artwork)
        precondition(PreviewNavigation.bridge(URL(string: runtime), mainFrame: true, document: .workspace) == .none)
        precondition(PreviewNavigation.bridge(URL(string: "walkieware://app/index.html?walkie=1"), mainFrame: true, document: .workspace) == .workspace)
        precondition(PreviewNavigation.bridge(URL(string: "walkieware://app/index.html?walkie=1"), mainFrame: false, document: .workspace) == .none)
        precondition(PreviewNavigation.bridge(URL(string: "about:blank"), mainFrame: false, document: .workspace) == .none)
        precondition(PreviewNavigation.bridge(URL(string: "https://aesthetic.computer/mint/"), mainFrame: false, document: .workspace) == .none)
        print("PASS: preview documents render; checkout, bundled Aesel, redirects, and new-window destinations are denied")
    }
}
