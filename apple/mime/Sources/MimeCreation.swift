import SwiftUI

/// MIME hosts Whistlegraph's native editor, account, engine and version library.
/// The feed's untrusted runtimes keep their separate, anonymous WebKit stores.
struct MimeCreation: View {
    @ObservedObject var session: WhistlegraphSession
    @Binding var requested: Ware?
    @Binding var active: Bool
    let size: CGSize
    @Binding var drag: CGFloat
    @State private var switching: Ware?
    @State private var surface = WhistlegraphTheme(phase: .ready, dark: true)
    @Environment(\.scenePhase) private var phase

    private var editorReady: Bool {
        session.accountReady && session.engineReady && session.workspaceReady && requested == nil
    }
    private var needsAccount: Bool {
        session.workspaceReady && session.accountStatus != .checking && !session.accountReady
    }
    var body: some View {
        VStack(spacing: 0) {
            HStack {
                Button(action: close) { Label("Feed", systemImage: "chevron.left").frame(minWidth: 72, minHeight: 44) }
                    .buttonStyle(.plain).accessibilityIdentifier("mime-creation-back")
                Spacer()
            }
                .font(.custom("ComicRelief-Bold", size: 17)).foregroundStyle(surface.foreground)
                .padding(.horizontal, 16).frame(height: 44).contentShape(Rectangle())
                .gesture(DragGesture(minimumDistance: 12, coordinateSpace: .global).onChanged { value in
                    if abs(value.translation.width) > abs(value.translation.height) { drag = min(0, value.translation.width) }
                }.onEnded { value in finishSwipe(value.translation.width, predicted: value.predictedEndTranslation.width) })
            ZStack {
                WhistlegraphScreen(session: session, availableSize: CGSize(width: size.width, height: size.height - 44))
                    .allowsHitTesting(editorReady).accessibilityHidden(!editorReady)
                // Keep WebKit rendering under the cover; opacity zero can suspend its first frame.
                if !editorReady { surface.background }
                if let failure = session.startupFailure {
                    VStack(spacing: 16) { Text(failure); Button("Reload") { session.reloadWorkspace() } }.padding()
                } else if needsAccount {
                    WhistlegraphAccountEntry(session: session, onExplore: close)
                } else if !editorReady {
                    ProgressView().tint(.white).accessibilityLabel("Opening editor")
                        .accessibilityIdentifier("mime-creation-loading")
                }
            }
        }
        .background(surface.background.ignoresSafeArea())
        .onPreferenceChange(WhistlegraphThemePreference.self) { if let theme = $0 { surface = theme } }
        .background(MimeReturnSwipe(enabled: active, moved: { drag = min(0, $0) }, ended: finishSwipe))
        .accessibilityAction(.escape, close)
        .environment(\.exploreMimes, close)
        .accessibilityElement(children: .contain)
        .accessibilityIdentifier("mime-creation")
        .task {
            session.webView?.setAllMediaPlaybackSuspended(!active, completionHandler: nil)
            await session.braincells.start(session: session); startRequestedWork()
        }
        .onChange(of: requested) { _, _ in startRequestedWork() }
        .onChange(of: session.engineReady) { _, _ in
            session.webView?.setAllMediaPlaybackSuspended(!active, completionHandler: nil)
            startRequestedWork()
        }
        .onChange(of: session.accountReady) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.busy) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.wareID) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.handle) { _, _ in Task { await session.syncAIAccount(); await session.braincells.accountChanged() } }
        .onChange(of: active) { _, visible in
            session.webView?.setAllMediaPlaybackSuspended(!visible, completionHandler: nil)
            if visible { switching = nil; session.resumePieceAudio(); startRequestedWork() }
            else { drag = 0 }
        }
        .onChange(of: phase) { _, phase in
            if phase == .background { session.cancelHold() }
            if phase == .active && active { session.resumePieceAudio() }
        }
    }

    private func close() {
        // Navigation never waits for inference or a preview. A dispatched turn
        // keeps its session; only an unfinished microphone hold is cancelled.
        if session.capturePhase == .opening || session.capturePhase == .recording { session.cancelHold() }
        session.webView?.endEditing(true)
        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
        session.webView?.setAllMediaPlaybackSuspended(true, completionHandler: nil)
        requested = nil
        switching = nil
        active = false
    }

    private func finishSwipe(_ translation: CGFloat, predicted: CGFloat) {
        if translation < -70 || predicted < -150 { close() }
        withAnimation(.spring(response: 0.3, dampingFraction: 0.85)) { drag = 0 }
    }

    private func startRequestedWork() {
        guard active, let ware = requested, session.accountReady, session.engineReady else { return }
        if session.snapshot.busy || session.capturePhase != .idle {
            requested = nil; switching = nil
            return
        }
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

/// A native right-edge pan leaves canvas drags, Doom gestures and vertical
/// history scrolling alone. It is disabled whenever MIME shows the feed.
private struct MimeReturnSwipe: UIViewRepresentable {
    var enabled: Bool
    let moved: (CGFloat) -> Void
    let ended: (CGFloat, CGFloat) -> Void
    func makeUIView(context: Context) -> Host { Host() }
    func updateUIView(_ view: Host, context: Context) {
        view.moved = moved; view.ended = ended; view.pan.isEnabled = enabled
    }
    static func dismantleUIView(_ view: Host, coordinator: ()) { view.detach() }

    final class Host: UIView, UIGestureRecognizerDelegate {
        var moved: ((CGFloat) -> Void)?
        var ended: ((CGFloat, CGFloat) -> Void)?
        private weak var attachedWindow: UIWindow?
        lazy var pan: UIScreenEdgePanGestureRecognizer = {
            let value = UIScreenEdgePanGestureRecognizer(target: self, action: #selector(changed))
            value.edges = .right; value.delegate = self
            return value
        }()
        init() { super.init(frame: .zero); isUserInteractionEnabled = false }
        required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }
        override func didMoveToWindow() {
            super.didMoveToWindow()
            detach()
            attachedWindow = window
            window?.addGestureRecognizer(pan)
        }
        func detach() { attachedWindow?.removeGestureRecognizer(pan); attachedWindow = nil }
        override func gestureRecognizerShouldBegin(_ gestureRecognizer: UIGestureRecognizer) -> Bool {
            guard let window = attachedWindow, window.rootViewController?.presentedViewController == nil else { return false }
            let velocity = pan.velocity(in: window)
            return velocity.x < 0 && abs(velocity.x) > abs(velocity.y)
        }
        @objc private func changed() {
            let x = pan.translation(in: attachedWindow).x
            switch pan.state {
            case .changed: moved?(x)
            case .ended: ended?(x, x + pan.velocity(in: attachedWindow).x * 0.18)
            case .cancelled, .failed: ended?(0, 0)
            default: break
            }
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
