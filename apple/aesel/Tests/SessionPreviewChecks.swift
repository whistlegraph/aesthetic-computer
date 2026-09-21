// Compile with Sources/Session.swift; no device, login or network required.
import Foundation

@main
struct SessionPreviewChecks {
    static func main() {
        let session = Session()
        precondition(session.previewURL == Session.draftPreviewURL)
        session.receive(["type": "piece", "source": "starter", "route": "new-piece"])
        precondition(session.previewURL == Session.draftPreviewURL)
        precondition(session.source == "starter" && session.shareURL == nil)

        let published = "https://aesthetic.computer/@test/painting"
        session.receive(["type": "preview", "url": published])
        let query = URLComponents(url: session.previewURL!, resolvingAgainstBaseURL: false)!.queryItems!
        for flag in ["nogap", "nolabel", "autoreload"] {
            precondition(query.contains(URLQueryItem(name: flag, value: "true")))
        }
        precondition(session.shareURL?.absoluteString == published)
        session.receive(["type": "source", "source": "edited draft", "version": 1])
        precondition(session.pieceVersion == 1)
        precondition(session.previewURL == Session.draftPreviewURL)
        precondition(session.source == "edited draft")

        session.receive(["type": "piece", "source": "next piece", "route": "next-piece"])
        precondition(session.previewURL == Session.draftPreviewURL)
        precondition(session.shareURL == nil)
        precondition(session.pieceVersion == 0)
        let embedded = Session.embeddedPreviewURL(URL(string: "https://prompt.ac/@test/name?zoom=2&nolabel=false#frame")!)
        let parts = URLComponents(url: embedded, resolvingAgainstBaseURL: false)!
        precondition(parts.host == "aesthetic.computer" && parts.fragment == "frame")
        precondition(parts.queryItems!.filter { $0.name == "nolabel" } == [URLQueryItem(name: "nolabel", value: "true")])
        precondition(parts.queryItems!.contains(URLQueryItem(name: "zoom", value: "2")))
        print("Preview remains visible for launch, drafts, published pieces and thread changes.")
    }
}
