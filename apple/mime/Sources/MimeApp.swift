import SwiftUI
import AVFoundation
import CoreText

/// mime.ac watches the network and shares Whistlegraph's native creation engine.
@main
struct MimeApp: App {
    init() {
        if let root = Bundle.main.url(forResource: "Web", withExtension: nil) {
            for name in ["ComicRelief-Regular.ttf", "ComicRelief-Bold.ttf"] { CTFontManagerRegisterFontsForURL(root.appendingPathComponent(name) as CFURL, .process, nil) }
        }
        // Pieces, KidLisp and videos sound like a reel: on, and mixed with nothing else.
        try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .default)
        try? AVAudioSession.sharedInstance().setActive(true)
    }
    var body: some Scene {
        WindowGroup { MimeRoot() }
    }
}

struct MimeRoot: View {
    @StateObject private var workspace = WhistlegraphSession()
    @StateObject private var queue = MimeWorkQueue()
    @StateObject private var interface = MimeInterface()
    @State private var openingWork: UUID?
    @State private var showingQueue = false
    @State private var creating = false
    @State private var editorDrag: CGFloat = 0
    @State private var requested: Ware?
    @State private var remix: Post?
    @State private var remixError: String?
    /// Making work waits once for the rules (mime.ac/terms), App Store guideline 1.2.
    @AppStorage("mime-rules-agreed") private var agreedRules = false
    @State private var afterRules: (() -> Void)?
    @Environment(\.openURL) private var openURL
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    var body: some View {
        GeometryReader { geometry in
            ZStack {
                Reel(active: !creating && interface.presentation.media.active, viewerHandle: interface.presentation.media.viewerHandle ?? "", accountActionLabel: interface.presentation.logo.label, openAccount: {
                    intent(interface.presentation.logo.id)
                }, remixWork: { post in
                    if let version = post.edition { intent(interface.presentation.creation.versionAction, values: ["post": post.code, "version": version], post: post) }
                }) { mimeType in
                    intent(interface.presentation.creation.typeAction, values: ["mimeType": mimeType])
                }
                    .offset(x: creating && !reduceMotion ? geometry.size.width + editorDrag : 0)
                    .allowsHitTesting(!creating && interface.presentation.media.active)
                    .accessibilityHidden(creating || !interface.presentation.media.active)
                // Stay mounted so account, fonts and engine warm up while the feed is open.
                MimeCreation(session: workspace, requested: $requested, remix: $remix, active: $creating, size: geometry.size, drag: $editorDrag, opened: workOpened, failed: workFailed)
                    .environment(\.mimeLettersActive, creating)
                    .frame(width: geometry.size.width, height: geometry.size.height)
                    .offset(x: reduceMotion ? 0 : creating ? editorDrag : -geometry.size.width)
                    .opacity(creating ? 1 : 0)
                    .allowsHitTesting(creating).accessibilityHidden(!creating)
                if !creating, !queued.isEmpty {
                    Button { showingQueue = true } label: {
                        Label("\(queued.count) queued", systemImage: "square.stack.3d.up")
                            .font(.custom("ComicRelief-Bold", size: 16)).padding(12)
                            .background(.ultraThinMaterial, in: Capsule())
                    }.buttonStyle(.plain).accessibilityIdentifier("mime-work-queue")
                        .frame(maxHeight: .infinity, alignment: .bottom).padding(.bottom, 78)
                }
                if interface.presentation.screen == "account" {
                    MimeAccountPanel(model: interface.presentation) { intent($0) }
                        .transition(.opacity)
                }
                #if DEBUG
                if creating && ["remote-voice", "progress"].contains(ProcessInfo.processInfo.environment["MIME_CREATION_FIXTURE"] ?? "") {
                    Button("Fixture voice request") { workspace.webView?.evaluateJavaScript("voiceStart()") }
                        .accessibilityIdentifier("mime-fixture-voice")
                        .frame(maxHeight: .infinity, alignment: .bottom)
                }
                if ProcessInfo.processInfo.environment["MIME_CREATION_FIXTURE"] == "audio" {
                    Text(workspace.playbackTestResult).font(.caption).allowsHitTesting(false)
                        .accessibilityIdentifier("mime-preview-audio-state")
                }
                #endif
            }
            .animation(reduceMotion ? .easeOut(duration: 0.15) : .spring(response: 0.42, dampingFraction: 0.88), value: creating)
        }.preferredColorScheme(.dark)
            .onChange(of: accountSnapshot, initial: true) { _, snapshot in
                interface.send("account.changed", values: ["account": ["status": snapshot[0], "handle": snapshot[1], "notice": snapshot[2]]])
            }
            .sheet(isPresented: $showingQueue) { MimeWorkQueueSheet(items: queued, open: openQueued, remove: queue.remove) }
            .onChange(of: workspace.canLeaveWork) { _, ready in
                if ready, !creating, interface.presentation.media.active, openingWork == nil, let next = queued.first(where: { $0.issue == nil }) { openQueued(next) }
            }
            .onChange(of: creating) { _, visible in if !visible { openingWork = nil } }
            .onOpenURL { url in
                if url.scheme == "mime", url.host == "create" {
                    let type = URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems?.first(where: { $0.name == "type" })?.value
                    if let type { intent(interface.presentation.creation.typeAction, values: ["mimeType": type]) }
                    else { intent("editor.open") }
                    return
                }
                guard url.scheme == "mime", url.host == "remix", let query = URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems,
                      let code = query.first(where: { $0.name == "post" })?.value, code.range(of: "^[A-Za-z0-9_-]{1,100}$", options: .regularExpression) != nil,
                      let rawVersion = query.first(where: { $0.name == "version" })?.value, let version = Int(rawVersion), version >= 0 else { return }
                Task { @MainActor in
                    do {
                        var endpoint = URLComponents(string: "https://mime.ac/api/mime")!
                        endpoint.queryItems = [.init(name: "thread", value: code)]
                        struct Thread: Decodable { let op: Post }
                        let thread = try JSONDecoder().decode(Thread.self, from: await MimeRemix.fetch(endpoint.url!, limit: 500_000))
                        guard thread.op.edition == version else { throw MimeRemix.failure("Refresh the feed to load this work’s current version.") }
                        intent(interface.presentation.creation.versionAction, values: ["post": thread.op.code, "version": version], post: thread.op)
                    } catch { remixError = error.localizedDescription }
                }
            }
            .alert("Remix unavailable", isPresented: Binding(get: { remixError != nil }, set: { if !$0 { remixError = nil } })) {
                Button("OK") { remixError = nil }
            } message: { Text(remixError ?? "") }
            .alert("Before you make something", isPresented: Binding(get: { afterRules != nil }, set: { if !$0 { afterRules = nil } })) {
                Button("Sounds good") { agreedRules = true; let next = afterRules; afterRules = nil; next?() }
                Button("Read the rules") { openURL(URL(string: "https://mime.ac/terms/")!); afterRules = nil }
                Button("Not now", role: .cancel) { afterRules = nil }
            } message: {
                Text("Everything you post shows in everyone’s feed. No harassment, hate, sexual or violent work, or spam. Reports are reviewed within 24 hours, and people who break the rules are removed.")
            }
    }
    private var accountSnapshot: [String] {
        [workspace.startupFailure != nil ? "failed" : !workspace.workspaceReady ? "checking" : workspace.accountStatus.rawValue,
         workspace.snapshot.handle, workspace.startupFailure ?? workspace.accountNotice]
    }
    private func intent(_ action: String, values: [String: Any] = [:], post: Post? = nil) {
        for effect in interface.send(action, values: values) {
            switch effect.type {
            case "authenticate": workspace.signIn(signUp: effect.signUp ?? false)
            case "signOut": workspace.signOut()
            case "refreshAccount":
                if workspace.startupFailure != nil { workspace.reloadWorkspace() } else { workspace.restoreAccount() }
            case "chooseHandle": openURL(URL(string: "https://aesthetic.computer/handle")!)
            case "openEditor": withRules { requested = nil; creating = true }
            case "createWork":
                withRules {
                    if let mime = effect.mimeType, let ware = Ware.creationType(mime) { openQueued(queue.enqueue(ware: ware, owner: workspace.snapshot.handle)) }
                    else { requested = nil; creating = true }
                }
            case "remixWork":
                if let post, post.code == effect.post, post.edition == effect.version { withRules { openRemix(post) } }
            default: break
            }
        }
    }
    private func withRules(_ action: @escaping () -> Void) {
        if agreedRules { action() } else { afterRules = action }
    }
    private func openRemix(_ post: Post) {
        guard let ware = post.remixWare else { return }
        openQueued(queue.enqueue(post: post, ware: ware, owner: workspace.snapshot.handle))
    }
    private var queued: [MimeWorkQueue.Item] { queue.available(to: workspace.snapshot.handle) }
    private func openQueued(_ item: MimeWorkQueue.Item) {
        queue.setIssue(nil, for: item.id)
        guard openingWork == nil, workspace.canLeaveWork || (!workspace.engineReady && !workspace.snapshot.busy) else { return }
        openingWork = item.id; requested = item.post == nil ? Ware(rawValue: item.ware) : nil
        remix = item.post; creating = true
    }
    private func workOpened() {
        if let id = openingWork { queue.remove(id) }
        openingWork = nil
    }
    private func workFailed(_ issue: String) {
        if let id = openingWork { queue.setIssue(issue, for: id) }
        openingWork = nil; creating = false
    }
}

private struct MimeAccountPanel: View {
    let model: MimeInterface.Projection
    let action: (String) -> Void
    var body: some View {
        ZStack {
            Color.black.opacity(0.72).ignoresSafeArea().onTapGesture { action("feed.open") }
            ScrollView {
                VStack(spacing: 16) {
                    Text(model.account.title).font(.custom("ComicRelief-Bold", size: 30))
                    if model.account.busy { ProgressView().tint(.white) }
                    if !model.account.notice.isEmpty {
                        Text(model.account.notice).font(.custom("ComicRelief-Regular", size: 17))
                    }
                    ForEach(model.account.actions) { item in
                        Button { action(item.id) } label: {
                            Text(item.label).frame(maxWidth: .infinity, minHeight: 48)
                                .background(item.id == "account.login" ? Color(hex: 0xB7A4FF) : item.id == "account.signup" ? Color(hex: 0x7AE7CE) : Color.white.opacity(0.1), in: RoundedRectangle(cornerRadius: 10))
                                .foregroundStyle(["account.login", "account.signup"].contains(item.id) ? Color.black : Color.white)
                        }.buttonStyle(.plain).accessibilityIdentifier("mime-" + item.id)
                    }
                }.font(.custom("ComicRelief-Bold", size: 22)).multilineTextAlignment(.center)
                    .padding(28).frame(maxWidth: 340)
                    .background(Color(hex: 0x211D30), in: RoundedRectangle(cornerRadius: 18))
                    .padding(20).frame(maxWidth: .infinity)
            }.fixedSize(horizontal: false, vertical: true)
        }
        .accessibilityAddTraits(.isModal)
        .accessibilityIdentifier("mime-account-panel")
        .accessibilityAction(.escape) { action("feed.open") }
    }
}
