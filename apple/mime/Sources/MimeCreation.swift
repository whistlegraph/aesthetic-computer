import SwiftUI
import UniformTypeIdentifiers

/// MIME hosts Whistlegraph's native editor, account, engine and version library.
/// The feed's untrusted runtimes keep their separate, anonymous WebKit stores.
struct MimeCreation: View {
    @ObservedObject var session: WhistlegraphSession
    @Binding var requested: Ware?
    @Binding var remix: Post?
    @Binding var active: Bool
    let size: CGSize
    @Binding var drag: CGFloat
    let opened: () -> Void
    let failed: (String) -> Void
    @State private var switching: Ware?
    @State private var remixTask: Task<Void, Never>?
    @State private var surface = WhistlegraphTheme(phase: .ready, dark: true)
    @Environment(\.scenePhase) private var phase

    private var editorReady: Bool {
        session.accountReady && session.engineReady && session.workspaceReady && requested == nil && remix == nil
    }
    private var needsAccount: Bool {
        session.workspaceReady && session.accountStatus != .checking && !session.accountReady
    }
    var body: some View {
        ZStack {
            WhistlegraphScreen(session: session, availableSize: size)
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
        .background(surface.background.ignoresSafeArea())
        .overlayPreferenceValue(WhistlegraphPreviewBounds.self) { anchor in
            GeometryReader { geometry in
                let picture = anchor.map { geometry[$0] } ?? CGRect(x: 44, y: 80, width: size.width - 88, height: 280)
                let gutter = max(44, picture.minX)
                Button(action: close) {
                    Image(systemName: "chevron.left").font(.system(size: 17, weight: .medium))
                        .foregroundStyle(surface.foreground.opacity(0.65))
                        .frame(width: 44, height: 44).contentShape(Rectangle())
                }
                .buttonStyle(.plain).accessibilityLabel("Return to feed")
                .accessibilityIdentifier("mime-creation-back")
                .frame(width: gutter, height: picture.height).contentShape(Rectangle())
                .simultaneousGesture(DragGesture(minimumDistance: 12, coordinateSpace: .global).onChanged { value in
                    if abs(value.translation.width) > abs(value.translation.height) { drag = min(0, value.translation.width) }
                }.onEnded { value in
                    if abs(value.translation.width) > abs(value.translation.height) {
                        // The gutter is narrow; a short leftward flick is enough.
                        if value.translation.width < -24 || value.predictedEndTranslation.width < -70 { close() }
                    }
                    finishSwipe(0, predicted: 0)
                })
                .position(x: gutter / 2, y: picture.midY)
            }
        }
        .onPreferenceChange(WhistlegraphThemePreference.self) { if let theme = $0 { surface = theme } }
        .background(MimeReturnSwipe(enabled: active, moved: { drag = min(0, $0) }, ended: finishSwipe))
        .accessibilityAction(.escape, close)
        .environment(\.exploreMimes, close)
        .accessibilityElement(children: .contain)
        .accessibilityIdentifier("mime-creation")
        .task {
            session.setPreviewPlaybackActive(active && phase == .active)
            await session.braincells.start(session: session); startRequestedWork()
        }
        .onChange(of: requested) { _, _ in startRequestedWork() }
        .onChange(of: remix?.id) { _, _ in startRequestedWork() }
        .onChange(of: session.engineReady) { _, _ in
            session.setPreviewPlaybackActive(active && phase == .active)
            startRequestedWork()
        }
        .onChange(of: session.accountReady) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.busy) { _, _ in startRequestedWork() }
        .onChange(of: session.canLeaveWork) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.wareID) { _, _ in startRequestedWork() }
        .onChange(of: session.snapshot.handle) { _, _ in Task { await session.syncAIAccount(); await session.braincells.accountChanged() } }
        .onChange(of: active) { _, visible in
            session.setPreviewPlaybackActive(visible && phase == .active)
            if visible { switching = nil; startRequestedWork() }
            else { drag = 0 }
        }
        .onChange(of: phase) { _, phase in
            if phase == .background { session.cancelHold() }
            session.setPreviewPlaybackActive(active && phase == .active)
        }
    }

    private func close() {
        // Navigation never waits for inference or a preview. A dispatched turn
        // keeps its session; only an unfinished microphone hold is cancelled.
        if session.capturePhase == .opening || session.capturePhase == .recording { session.cancelHold() }
        session.webView?.endEditing(true)
        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
        session.setPreviewPlaybackActive(false)
        remixTask?.cancel(); remixTask = nil; remix = nil
        requested = nil
        switching = nil
        active = false
    }

    private func finishSwipe(_ translation: CGFloat, predicted: CGFloat) {
        if translation < -70 || predicted < -150 { close() }
        withAnimation(.spring(response: 0.3, dampingFraction: 0.85)) { drag = 0 }
    }

    private func startRequestedWork() {
        if let remix {
            guard active, session.accountReady, session.engineReady, remixTask == nil, let ware = remix.remixWare else { return }
            guard session.canLeaveWork else { return }
            if session.snapshot.wareID != ware.rawValue {
                guard switching != ware else { return }
                switching = ware; session.command("setWare", ware: ware.rawValue); return
            }
            remixTask = Task { @MainActor in
                defer { remixTask = nil }
                do {
                    let payload = try await remix.remixPayload()
                    try Task.checkCancellation()
                    guard active, self.remix?.id == remix.id else { return }
                    try await session.remixWork(payload)
                    self.remix = nil; switching = nil; opened()
                } catch {
                    if !Task.isCancelled { self.remix = nil; switching = nil; failed(error.localizedDescription) }
                }
            }
            return
        }
        guard active, let ware = requested, session.accountReady, session.engineReady else { return }
        guard session.canLeaveWork else { return }
        if session.snapshot.wareID != ware.rawValue {
            guard switching != ware else { return }
            switching = ware
            session.command("setWare", ware: ware.rawValue)
        } else {
            requested = nil
            session.command("newPiece", onAccepted: opened)
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

/// A mood uses the existing AC publishing endpoint; it never enters inference.
struct MimeMoodComposer: View {
    @ObservedObject var session: WhistlegraphSession
    @Binding var draft: String
    let close: () -> Void
    let published: (Post) -> Void
    @State private var posting = false
    @State private var notice: String?
    @FocusState private var editing: Bool
    @Environment(\.openURL) private var openURL

    private var ready: Bool {
        #if DEBUG
        if ProcessInfo.processInfo.environment["MIME_MOOD_FIXTURE"] == "signed-in" { return true }
        #endif
        return session.accountReady
    }
    private var words: String { draft.trimmingCharacters(in: .whitespacesAndNewlines) }
    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            HStack {
                Button(action: close) { Text("Back").frame(minHeight: 44) }
                    .disabled(posting).accessibilityIdentifier("mime-mood-back")
                Spacer()
                if ready {
                    Button { Task { await post() } } label: { Text(posting ? "Posting…" : "Post mood").frame(minHeight: 44) }
                        .disabled(posting || words.isEmpty || draft.utf16.count > 4000)
                        .accessibilityIdentifier("mime-mood-submit")
                }
            }
            ZStack(alignment: .topLeading) {
                if draft.isEmpty { Text("How are you feeling?").foregroundStyle(.white.opacity(0.5)).padding(.top, 8).padding(.leading, 5).allowsHitTesting(false) }
                TextEditor(text: $draft).scrollContentBackground(.hidden)
                    .focused($editing).disabled(posting)
                    .accessibilityLabel("Mood").accessibilityIdentifier("mime-mood-text")
            }.font(.custom("ComicRelief-Bold", size: 26))
            if ready, !session.snapshot.handle.isEmpty { Text(session.snapshot.handle).font(.custom("ComicRelief-Bold", size: 18)) }
            if draft.utf16.count > 4000 { Text("Keep your mood under 4,000 characters.").foregroundStyle(.pink) }
            if !ready {
                if session.accountStatus == .needsHandle {
                    Button("Choose your @handle") { openURL(URL(string: "https://aesthetic.computer/handle")!) }
                    Button("I’ve chosen a handle") { session.restoreAccount() }
                } else if session.accountStatus == .checking || !session.workspaceReady {
                    ProgressView("Checking your account…")
                } else {
                    HStack(spacing: 24) {
                        Button("Log in to post") { editing = false; session.signIn() }.accessibilityIdentifier("mime-mood-login")
                        Button("I'm new") { editing = false; session.signIn(signUp: true) }
                    }
                    if session.accountStatus == .failed { Button("Retry account verification") { session.restoreAccount() } }
                }
                if !session.accountNotice.isEmpty { Text(session.accountNotice).font(.body) }
            }
            if let notice { Text(notice).foregroundStyle(.pink).accessibilityIdentifier("mime-mood-notice") }
        }
        .font(.custom("ComicRelief-Bold", size: 18)).foregroundStyle(.white).tint(Color(hex: 0x55CDD9))
        .padding(24).background(Color.paper.ignoresSafeArea())
        .accessibilityElement(children: .contain)
        .accessibilityIdentifier("mime-mood-composer")
        .accessibilityAction(.escape) { if !posting { close() } }
    }

    @MainActor private func post() async {
        guard ready, !posting, !words.isEmpty, draft.utf16.count <= 4000 else { return }
        posting = true; notice = nil; editing = false
        defer { posting = false }
        do {
            #if DEBUG
            if ProcessInfo.processInfo.environment["MIME_MOOD_FIXTURE"] == "signed-in" {
                let json: [String: Any] = ["code": "mood_created", "name": "@fixture", "board": "text/plain", "text": words,
                    "media": ["kind": "mood", "code": "new"], "when": ISO8601DateFormatter().string(from: Date())]
                published(try JSONDecoder().decode(Post.self, from: JSONSerialization.data(withJSONObject: json)))
                return
            }
            #endif
            let generation = session.account.generation
            guard let token = try await session.account.token(), generation == session.account.generation else {
                session.restoreAccount(); throw NativeSignIn.failure("Log in again to post your mood.")
            }
            var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/mood")!, timeoutInterval: 60)
            request.httpMethod = "POST"
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.setValue("Bearer " + token, forHTTPHeaderField: "Authorization")
            request.httpBody = try JSONSerialization.data(withJSONObject: ["mood": words])
            let (data, response) = try await URLSession.shared.data(for: request)
            guard let http = response as? HTTPURLResponse, http.statusCode == 200 else {
                if (response as? HTTPURLResponse)?.statusCode == 401 { session.restoreAccount() }
                throw NativeSignIn.failure("Could not post your mood. Your draft is still here; try again.")
            }
            struct Publication: Decodable { let post: Post }
            let result = try JSONDecoder().decode(Publication.self, from: data)
            guard generation == session.account.generation else { throw NativeSignIn.failure("Your account changed. Reopen the feed to see your mood.") }
            published(result.post)
        } catch { notice = error.localizedDescription }
    }
}


/// Publish ROM bytes through the same file endpoint used by the website.
struct MimeROMImporter: View {
    @ObservedObject var session: WhistlegraphSession
    let close: () -> Void
    let published: (Post) -> Void
    @State private var choosing = false
    @State private var bytes: Data?
    @State private var name = ""
    @State private var posting = false
    @State private var postedCode: String?
    @State private var notice: String?
    @Environment(\.openURL) private var openURL

    var body: some View {
        VStack(alignment: .leading, spacing: 20) {
            HStack {
                Button("Back", action: close).disabled(posting)
                Spacer()
                Button(posting ? "Posting…" : postedCode == nil ? "Post ROM" : "Open posted ROM") { Task { await publish() } }
                    .disabled(posting || bytes == nil || !session.accountReady)
            }
            Text("NES ROM · up to 8 MB").font(.title3)
            Button(name.isEmpty ? "Choose .nes file" : name) { choosing = true }
                .disabled(posting || postedCode != nil).accessibilityIdentifier("mime-rom-choose")
            if !session.accountReady {
                if session.accountStatus == .needsHandle {
                    Button("Choose your @handle") { openURL(URL(string: "https://aesthetic.computer/handle")!) }
                    Button("I’ve chosen a handle") { session.restoreAccount() }
                } else if session.accountStatus == .checking || !session.workspaceReady { ProgressView("Checking account…") }
                else { Button("Log in to post") { session.signIn() } }
            }
            if let notice { Text(notice).foregroundStyle(.pink).accessibilityIdentifier("mime-rom-notice") }
            Spacer()
        }
        .padding(24).font(.custom("ComicRelief-Bold", size: 18))
        .tint(Color(hex: 0x7AE7CE)).background(Color.paper.ignoresSafeArea())
        .interactiveDismissDisabled(posting)
        .accessibilityIdentifier("mime-rom-importer")
        .fileImporter(isPresented: $choosing, allowedContentTypes: [.data]) { result in
            do {
                let url = try result.get()
                bytes = nil; name = ""
                let access = url.startAccessingSecurityScopedResource()
                defer { if access { url.stopAccessingSecurityScopedResource() } }
                guard url.pathExtension.lowercased() == "nes" else { throw NativeSignIn.failure("Choose a .nes ROM.") }
                guard let size = try url.resourceValues(forKeys: [.fileSizeKey]).fileSize, size >= 16, size <= 8 * 1024 * 1024 else { throw NativeSignIn.failure("Choose a NES ROM under 8 MB.") }
                let data = try Data(contentsOf: url)
                guard data.count <= 8 * 1024 * 1024, data.prefix(4) == Data([0x4e, 0x45, 0x53, 0x1a]) else { throw NativeSignIn.failure("Choose an iNES or NES 2.0 ROM.") }
                bytes = data; name = url.lastPathComponent; notice = nil
            } catch { notice = error.localizedDescription }
        }
    }
    @MainActor private func publish() async {
        guard let bytes, session.accountReady, !posting else { return }
        posting = true; notice = nil
        defer { posting = false }
        do {
            let generation = session.account.generation
            guard let token = try await session.account.token(), generation == session.account.generation else { throw NativeSignIn.failure("Log in again to post.") }
            if postedCode == nil {
                var request = URLRequest(url: URL(string: "https://mime.ac/api/mime")!, timeoutInterval: 60)
                request.httpMethod = "POST"
                request.setValue("application/json", forHTTPHeaderField: "Content-Type")
                request.setValue("Bearer " + token, forHTTPHeaderField: "Authorization")
                request.httpBody = try JSONSerialization.data(withJSONObject: ["file": ["name": name, "type": "application/x-nes-rom", "data": bytes.base64EncodedString()]])
                let (data, response) = try await URLSession.shared.data(for: request)
                guard (response as? HTTPURLResponse)?.statusCode == 200 else {
                    if (response as? HTTPURLResponse)?.statusCode == 401 { session.restoreAccount() }
                    let error = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
                    throw NativeSignIn.failure(error?["error"] as? String ?? "Could not post your ROM. Try again.")
                }
                struct Receipt: Decodable { let code: String }
                postedCode = try JSONDecoder().decode(Receipt.self, from: data).code
            }
            var url = URLComponents(string: "https://mime.ac/api/mime")!
            url.queryItems = [.init(name: "thread", value: postedCode)]
            let (data, response) = try await URLSession.shared.data(from: url.url!)
            guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw NativeSignIn.failure("Posted. Try again to open your ROM.") }
            struct Thread: Decodable { let op: Post }
            let post = try JSONDecoder().decode(Thread.self, from: data).op
            guard generation == session.account.generation else { throw NativeSignIn.failure("Your account changed. Reopen the feed to see your ROM.") }
            published(post)
        } catch { notice = error.localizedDescription }
    }
}
