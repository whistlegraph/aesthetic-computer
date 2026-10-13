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
    @State private var creating = false
    @State private var editorDrag: CGFloat = 0
    @State private var requested: Ware?
    @State private var remix: Post?
    @State private var remixError: String?
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    var body: some View {
        GeometryReader { geometry in
            ZStack {
                Reel(active: !creating, resumeEditor: {
                    requested = nil
                    creating = true
                }, remixWork: openRemix) { ware in
                    // A running turn owns the workspace. Reopen it rather than
                    // hiding it behind a pending New command until it finishes.
                    requested = workspace.snapshot.busy || workspace.capturePhase != .idle ? nil : ware
                    creating = true
                }
                    .offset(x: creating && !reduceMotion ? geometry.size.width + editorDrag : 0)
                    .allowsHitTesting(!creating).accessibilityHidden(creating)
                // Stay mounted so account, fonts and engine warm up while the feed is open.
                MimeCreation(session: workspace, requested: $requested, remix: $remix, active: $creating, size: geometry.size, drag: $editorDrag)
                    .environment(\.mimeLettersActive, creating)
                    .frame(width: geometry.size.width, height: geometry.size.height)
                    .offset(x: reduceMotion ? 0 : creating ? editorDrag : -geometry.size.width)
                    .opacity(creating ? 1 : 0)
                    .allowsHitTesting(creating).accessibilityHidden(!creating)
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
            .onOpenURL { url in
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
                        openRemix(thread.op)
                    } catch { remixError = error.localizedDescription }
                }
            }
            .alert("Could not remix", isPresented: Binding(get: { remixError != nil }, set: { if !$0 { remixError = nil } })) {
                Button("OK") { remixError = nil }
            } message: { Text(remixError ?? "") }
    }
    private func openRemix(_ post: Post) {
        guard !workspace.snapshot.busy, workspace.capturePhase == .idle else {
            remixError = "Finish or stop your current work before remixing another."; return
        }
        requested = nil; remix = post; creating = true
    }
}
