import SwiftUI

/// MIME hosts Whistlegraph's native editor, account, engine and version library.
/// The feed's untrusted runtimes keep their separate, anonymous WebKit stores.
struct MimeCreation: View {
    @Binding var requested: Ware?
    @Binding var active: Bool
    let size: CGSize
    @Binding var drag: CGFloat
    @State private var switching: Ware?
    @State private var previewRevealed = false
    @StateObject private var session = WhistlegraphSession()
    @Environment(\.scenePhase) private var phase

    private var editorReady: Bool {
        session.accountReady && session.engineReady && session.workspaceReady && requested == nil
            && (previewRevealed || session.snapshot.wareID != "piece" || !session.snapshot.hasPreview || session.previewPainted)
    }
    private var needsAccount: Bool {
        session.workspaceReady && session.accountStatus != .checking && !session.accountReady
    }
    var body: some View {
        VStack(spacing: 0) {
            Capsule().fill(.white.opacity(0.4)).frame(width: 40, height: 5)
                .frame(maxWidth: .infinity).frame(height: 28).contentShape(Rectangle())
                .onTapGesture(perform: close)
                .gesture(DragGesture(minimumDistance: 8).onChanged { value in
                    if !session.snapshot.busy && session.capturePhase == .idle { drag = max(0, value.translation.height) }
                }.onEnded { value in
                    if value.translation.height > 64 || value.predictedEndTranslation.height > 120 { close() }
                    withAnimation(.spring(response: 0.3, dampingFraction: 0.85)) { drag = 0 }
                })
                .accessibilityLabel("Close editor").accessibilityAddTraits(.isButton)
                .accessibilityIdentifier("mime-creation-back")
            ZStack {
                WhistlegraphScreen(session: session, availableSize: CGSize(width: size.width, height: size.height - 40))
                    .allowsHitTesting(editorReady).accessibilityHidden(!editorReady)
                // Keep WebKit rendering under the cover; opacity zero can suspend its first frame.
                if !editorReady { WhistlegraphTheme(phase: .ready, dark: true).background }
                if let failure = session.startupFailure {
                    VStack(spacing: 16) { Text(failure); Button("Reload") { session.reloadWorkspace() } }.padding()
                } else if needsAccount {
                    WhistlegraphAccountEntry(session: session, onExplore: close)
                } else if !editorReady {
                    ProgressView().tint(.white).accessibilityLabel("Opening editor")
                        .accessibilityIdentifier("mime-creation-loading")
                }
            }.padding(.bottom, 12)
        }
        .background(WhistlegraphTheme(phase: .ready, dark: true).background)
        .accessibilityAction(.escape, close)
        .environment(\.exploreMimes, close)
        .accessibilityElement(children: .contain)
        .accessibilityIdentifier("mime-creation")
        .task {
            session.webView?.setAllMediaPlaybackSuspended(!active, completionHandler: nil)
            await session.braincells.start(session: session); startRequestedWork()
        }
        .onChange(of: requested) { _, ware in
            if ware != nil { previewRevealed = false }
            startRequestedWork()
        }
        .onChange(of: editorReady) { _, ready in if ready { previewRevealed = true } }
        .onChange(of: session.engineReady) { _, _ in
            session.webView?.setAllMediaPlaybackSuspended(!active, completionHandler: nil)
            startRequestedWork()
        }
        .onChange(of: session.accountReady) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.busy) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.handle) { _, _ in Task { await session.syncAIAccount(); await session.braincells.accountChanged() } }
        .onChange(of: active) { _, visible in
            session.webView?.setAllMediaPlaybackSuspended(!visible, completionHandler: nil)
            if visible { previewRevealed = false; switching = nil; session.resumePieceAudio(); startRequestedWork() }
            else { drag = 0 }
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
        requested = nil
        active = false
    }

    private func startRequestedWork() {
        guard active, let ware = requested, session.accountReady, session.engineReady,
              !session.snapshot.busy, session.capturePhase == .idle else { return }
        if session.snapshot.wareID != ware.rawValue {
            guard switching != ware else { return }
            switching = ware
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
