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
                Reel(active: !creating, viewerHandle: workspace.accountReady ? workspace.snapshot.handle : "", resumeEditor: {
                    withRules { requested = nil; creating = true }
                }, remixWork: { post in withRules { openRemix(post) } }) { ware in
                    withRules { openQueued(queue.enqueue(ware: ware, owner: workspace.snapshot.handle)) }
                }
                    .offset(x: creating && !reduceMotion ? geometry.size.width + editorDrag : 0)
                    .allowsHitTesting(!creating).accessibilityHidden(creating)
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
            .sheet(isPresented: $showingQueue) { MimeWorkQueueSheet(items: queued, open: openQueued, remove: queue.remove) }
            .onChange(of: workspace.canLeaveWork) { _, ready in
                if ready, !creating, openingWork == nil, let next = queued.first(where: { $0.issue == nil }) { openQueued(next) }
            }
            .onChange(of: creating) { _, visible in if !visible { openingWork = nil } }
            .onOpenURL { url in
                if url.scheme == "mime", url.host == "create" {
                    let type = URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems?.first(where: { $0.name == "type" })?.value
                    withRules {
                        if let type, let ware = Ware.creationType(type) { openQueued(queue.enqueue(ware: ware, owner: workspace.snapshot.handle)) }
                        else { requested = nil; creating = true }
                    }
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
                        withRules { openRemix(thread.op) }
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
