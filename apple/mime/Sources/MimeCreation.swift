import SwiftUI

/// MIME hosts Whistlegraph's native editor, account, engine and version library.
/// The feed's untrusted runtimes keep their separate, anonymous WebKit stores.
struct MimeCreation: View {
    @Binding var requested: Ware?
    @Binding var active: Bool
    @StateObject private var session = WhistlegraphSession()
    @Environment(\.scenePhase) private var phase

    var body: some View {
        VStack(spacing: 0) {
            HStack {
                Spacer()
                Button(action: close) {
                    Image(systemName: "xmark").font(.title3)
                        .frame(width: 44, height: 44)
                        .background(.ultraThinMaterial, in: Circle())
                }.buttonStyle(.plain).foregroundStyle(.white)
                    .disabled(session.snapshot.busy || session.capturePhase != .idle)
                    .accessibilityLabel("Explore mimes")
                    .accessibilityIdentifier("mime-creation-back")
            }.padding(.horizontal, 18).padding(.vertical, 4)
            ZStack {
                WhistlegraphScreen(session: session, overlaysFeed: true)
                    .opacity(session.accountReady && requested == nil ? 1 : 0)
                    .allowsHitTesting(session.accountReady && requested == nil)
                    .accessibilityElement(children: session.accountReady && requested == nil ? .contain : .ignore)
                    .accessibilityHidden(!session.accountReady || requested != nil)
                if !session.accountReady {
                    WhistlegraphAccountEntry(session: session, onExplore: close, overlaysFeed: true)
                } else if requested != nil {
                    ProgressView("Opening new work…")
                }
            }
        }
        .background(Color.black.opacity(0.58).ignoresSafeArea())
        .environment(\.exploreMimes, close)
        .accessibilityElement(children: .contain)
        .accessibilityIdentifier("mime-creation")
        .task { await session.braincells.start(session: session); startRequestedWork() }
        .onChange(of: requested) { _, _ in startRequestedWork() }
        .onChange(of: session.engineReady) { _, _ in startRequestedWork() }
        .onChange(of: session.accountReady) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.busy) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.handle) { _, _ in Task { await session.syncAIAccount(); await session.braincells.accountChanged() } }
        .onChange(of: active) { _, visible in
            session.webView?.setAllMediaPlaybackSuspended(!visible, completionHandler: nil)
            if visible { session.resumePieceAudio(); startRequestedWork() }
        }
        .onChange(of: phase) { _, phase in
            if phase == .background { session.cancelHold() }
            if phase == .active && active { session.resumePieceAudio() }
        }
    }

    private func close() {
        guard !session.snapshot.busy, session.capturePhase == .idle else { return }
        session.cancelHold()
        session.webView?.setAllMediaPlaybackSuspended(true, completionHandler: nil)
        active = false
    }

    private func startRequestedWork() {
        guard active, let ware = requested, session.accountReady, session.engineReady,
              !session.snapshot.busy, session.capturePhase == .idle else { return }
        if session.snapshot.wareID != ware.rawValue {
            session.command("setWare", ware: ware.rawValue)
        } else {
            requested = nil
            session.command("newPiece")
        }
    }
}

extension Ware {
    /// Only types that the shared editor actually authors get a Create action.
    static func creationType(_ mime: String, kind: String? = nil) -> Ware? {
        let type = mime.split(separator: ";", maxSplits: 1).first?.trimmingCharacters(in: .whitespaces).lowercased() ?? ""
        switch type {
        case "image/png": return .painting
        case "application/x-doom-wad", "application/x-doom": return .doom
        case "application/x-roblox", "application/x-roblox-place": return .roblox
        case "text/javascript", "application/javascript", "application/x-aesthetic-computer-piece": return .piece
        default: return ["piece", "wgpiece"].contains(kind ?? "") ? .piece : nil
        }
    }
}
