// Compile with Sources/Session.swift; no device, login or network required.
import Foundation

@main
struct SessionPreviewChecks {
    static func main() {
        let session = Session()
        session.receive(["type":"approval", "approval":["id":"1", "title":"Allow this action?", "detail":"ac: Run this tool?", "canAccept":true, "alwaysLabel":"Always allow", "alwaysScope":"All connected MCP servers"]])
        precondition(session.approval?.detail == "ac: Run this tool?" && session.approval?.alwaysLabel == "Always allow")
        session.receive(["type":"approval", "approval":["id":"2", "title":"Input needed", "detail":"Sign in", "canAccept":false]])
        precondition(session.approval?.canAccept == false && session.approval?.alwaysLabel == nil)
        session.receive(["type":"approval", "approval":NSNull()])
        precondition(session.approval == nil)
        session.receive(["type":"providers", "mcpAutoAllow":false, "supportsApprovalPolicy":true])
        precondition(!session.mcpAutoAllow && session.supportsApprovalPolicy)
        precondition(session.previewURL == Session.draftPreviewURL)
        session.receive(["type": "piece", "source": "starter", "route": "new-piece"])
        precondition(session.previewURL == Session.draftPreviewURL)
        precondition(session.source == "starter" && session.shareURL == nil)

        let published = "https://aesthetic.computer/@test/painting"
        session.receive(["type": "preview", "url": published])
        precondition(session.previewURL == Session.draftPreviewURL)
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
        session.receive(["type":"thread", "id":"test", "composer":"unsent", "events":[["type":"you", "text":"latest"]]])
        session.receive(["type":"source", "source":"new", "version":2])
        session.signedIn = true
        session.handle = "test"
        session.receive(["type":"revisionSelection", "threadID":"test", "version":1, "current":2, "source":"old", "transcriptAvailable":true, "events":[["type":"you", "text":"earlier"]]])
        precondition(session.viewingHistory && !session.canStartTurn)
        precondition(session.displayedSource == "old" && session.source == "new")
        precondition(session.displayedEntries.first?.text == "earlier" && session.entries.first?.text == "latest")
        precondition(session.composer == "unsent")
        session.receive(["type":"revisionSelection", "threadID":"test", "version":2, "current":2, "source":"new", "events":[]])
        precondition(!session.viewingHistory && session.canStartTurn && session.displayedRevision == 2)
        precondition(session.displayedEntries.first?.text == "latest")
        print("Preview remains visible for launch, drafts, published pieces and thread changes.")
    }
}
