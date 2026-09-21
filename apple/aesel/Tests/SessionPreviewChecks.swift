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
        precondition(session.previewURL?.absoluteString == published)
        precondition(session.shareURL?.absoluteString == published)
        session.receive(["type": "source", "source": "edited draft"])
        precondition(session.previewURL == Session.draftPreviewURL)
        precondition(session.source == "edited draft")

        session.receive(["type": "piece", "source": "next piece", "route": "next-piece"])
        precondition(session.previewURL == Session.draftPreviewURL)
        precondition(session.shareURL == nil)
        print("Preview remains visible for launch, drafts, published pieces and thread changes.")
    }
}
