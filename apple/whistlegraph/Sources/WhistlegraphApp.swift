import SwiftUI
import WebKit
import Speech
import AVFoundation
import CoreText

@main
struct WhistlegraphApp: App {
    @StateObject private var voice = WhistlegraphSession()
    @Environment(\.scenePhase) private var phase
    @AppStorage("walkieware-appearance") private var appearance = "system"
    init() {
        if let root = Bundle.main.url(forResource: "Web", withExtension: nil) {
            for name in ["ComicRelief-Regular.ttf", "ComicRelief-Bold.ttf"] { CTFontManagerRegisterFontsForURL(root.appendingPathComponent(name) as CFURL, .process, nil) }
        }
    }
    var body: some Scene {
        WindowGroup {
            ZStack {
                Color(uiColor: .systemBackground).ignoresSafeArea()
                WhistlegraphScreen(session: voice)
                #if DEBUG
                if NativeScreenFixture.mode == "audio" {
                    Text(voice.audioTestResult).accessibilityIdentifier("audio-autoplay-result")
                }
                if voice.isConsentFixture {
                    Text("Requests: \(voice.consentFixtureRequests)")
                        .accessibilityIdentifier("consent-fixture-requests")
                }
                #endif
                if !voice.workspaceReady {
                    VStack(spacing: 20) {
                        Text("whistlegraph").font(.largeTitle.bold())
                        if let failure = voice.startupFailure {
                            Text(failure).multilineTextAlignment(.center)
                            Button("Reload") { voice.reloadWorkspace() }.buttonStyle(.borderedProminent)
                        } else { ProgressView("Opening…") }
                    }.padding(30).foregroundStyle(.primary)
                }
            }
            .task { if !voice.isConsentFixture { await voice.braincells.start(session: voice) } }
            .onChange(of: voice.snapshot.handle) { _, _ in Task {
                await voice.syncAIAccount()
                if !voice.isConsentFixture { await voice.braincells.accountChanged() }
            } }
            .sheet(isPresented: $voice.showingAIConsent, onDismiss: voice.finishAIConsentPrompt) {
                WhistlegraphAIConsentSheet(canAllow: voice.canAllowAIConsent,
                    allow: voice.allowAIConsent, decline: voice.declineAIConsent)
            }
            .preferredColorScheme(appearance == "light" ? .light : appearance == "dark" ? .dark : nil)
            .onChange(of: phase) { _, value in
                if value == .background { voice.cancelHold() }
                if value == .active && voice.capturePhase == .idle { voice.resumePieceAudio() }
                if value == .active && !voice.isConsentFixture {
                    Task {
                        await TezDisplayRate.shared.refresh()
                        await voice.braincells.recover()
                        #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
                        await voice.tezosBraincells.refresh(session: voice)
                        #endif
                    }
                }
            }
            #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
            .onOpenURL { url in
                if url.scheme == "whistlegraph" && url.host == "braincells" {
                    Task { await voice.tezosBraincells.refresh(session: voice) }
                }
            }
            #endif
        }
    }
}

struct Workspace: UIViewRepresentable {
    let voice: WhistlegraphSession
    func makeCoordinator() -> WorkspaceCoordinator { WorkspaceCoordinator(session: voice) }
    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareNativeShell = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        if let data = try? JSONSerialization.data(withJSONObject: voice.aiConsent.bridge), let json = String(data: data, encoding: .utf8) {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphAIConsent = \(json);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        config.userContentController.add(context.coordinator, name: "walkie")
        config.setURLSchemeHandler(WhistlegraphBundle(), forURLScheme: "walkieware")
        // Custom-scheme fetch responses have status 0 on device. Seed the
        // shared VFS exactly as Aesel does, before its modules are evaluated.
        var guides: [String: String] = [:]
        for name in ["pieces.md", "screen.md", "hand.md", "kidlisp.md", "api.json"] {
            if let root = Bundle.main.url(forResource: "Web", withExtension: nil),
               let text = try? String(contentsOf: root.appendingPathComponent("easel/context/" + name), encoding: .utf8) {
                guides["/easel/context/" + name] = text
            }
        }
        if let data = try? JSONSerialization.data(withJSONObject: guides), let json = String(data: data, encoding: .utf8) {
            config.userContentController.addUserScript(WKUserScript(source: "window.__aeselGuides = \(json);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        #if DEBUG
        if NativeScreenFixture.mode == "audio" { config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphAudioTest = true;", injectionTime: .atDocumentStart, forMainFrameOnly: false)) }
        #endif
        config.userContentController.addUserScript(WKUserScript(source: StoryTape.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
        config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewarePixelSize = \(voice.pixelSize);", injectionTime: .atDocumentStart, forMainFrameOnly: false))
        config.userContentController.addUserScript(WKUserScript(source: WhistlegraphPreview.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
        #if DEBUG
        if NativeScreenFixture.enabled { config.userContentController.addUserScript(WKUserScript(source: NativeScreenFixture.script, injectionTime: .atDocumentStart, forMainFrameOnly: true)) }
        if let model = ProcessInfo.processInfo.environment["WALKIE_MODEL"],
           let data = try? JSONSerialization.data(withJSONObject: [model]),
           let json = String(data: data, encoding: .utf8) {
            config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareModel = \(json)[0];", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if let revision = Int(ProcessInfo.processInfo.environment["WALKIE_RESTORE_VERSION"] ?? ""), revision >= 0 {
            config.userContentController.addUserScript(WKUserScript(source: """
            try {
              const key = 'walkieware-sequence-source-versions';
              const ledger = JSON.parse(localStorage.getItem(key));
              const selected = ledger?.versions?.find(v => v.id === \(revision));
              if (selected) {
                ledger.head = selected.id;
                localStorage.setItem(key, JSON.stringify(ledger));
                localStorage.setItem('walkieware-sequence-source', selected.source);
              }
            } catch {}
            """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if let keepTest = ProcessInfo.processInfo.environment["WALKIE_KEEP_TEST_PIECE"], ["1", "audio", "local", "space"].contains(keepTest) {
            let testSourceKey = keepTest == "space" ? "walkieware-space-source" : keepTest == "local" ? "walkieware-local-source" : keepTest == "audio" ? "walkieware-benchmark-source" : "walkieware-sequence-source"
            config.userContentController.addUserScript(WKUserScript(source: """
            try {
              const result = localStorage.getItem('\(testSourceKey)');
              if (result) {
                const previous = localStorage.getItem('walkieware-source');
                if (previous && previous !== result) localStorage.setItem('walkieware-before-sequence', previous);
                localStorage.setItem('walkieware-source', result);
                const versions = localStorage.getItem('\(testSourceKey)-versions');
                if (versions) {
                  const old = localStorage.getItem('walkieware-source-versions');
                  if (old) localStorage.setItem('walkieware-before-sequence-versions', old);
                  localStorage.setItem('walkieware-source-versions', versions);
                }
              }
            } catch {}
            """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WALKIE_SCENE_TEST"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareScene = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_VISUAL_CAPTURE_TEST"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphVisualCaptureTest = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if let revision = Int(ProcessInfo.processInfo.environment["WALKIE_REVIEW_VERSION"] ?? ""), revision >= 0 {
            config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareReviewVersion = \(revision);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WALKIE_LOCAL_SEQUENCE"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareLocalSequence = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WALKIE_SPACE_TEST"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareSpace = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if SequenceBenchmark.enabled {
            let start = max(1, min(32, Int(ProcessInfo.processInfo.environment["WALKIE_SEQUENCE_START"] ?? "1") ?? 1))
            config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareSequence = true; window.__walkiewareSequenceStart = \(start);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if AudioBenchmark.enabled {
            config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareBenchmark = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
            config.userContentController.addUserScript(WKUserScript(source: """
            const audioCheck = setInterval(() => {
              if (!window.walkiewareAccountReady || !window.walkiewareAsk) return;
              clearInterval(audioCheck);
              window.walkiewareStartFixture();
            }, 100);
            """, injectionTime: .atDocumentEnd, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WALKIE_PREVIEW_CHECK"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: """
            const check = setInterval(() => {
              if (!window.walkiewareAsk) return;
              clearInterval(check);
              document.body.classList.add('live-mode','live-preview');
              document.getElementById('live-work').hidden = false;
              document.getElementById('live-phase').textContent = 'Preview smoke test';
              window.webkit.messageHandlers.walkie.postMessage({action:'render',id:'smoke',source:'export function paint({wipe,ink,screen}) { wipe("navy"); ink("pink").circle(screen.width/2,screen.height/2,50); }'});
            }, 100);
            """, injectionTime: .atDocumentEnd, forMainFrameOnly: true))
        }
        #endif
        config.preferences.inactiveSchedulingPolicy = .none
        config.allowsInlineMediaPlayback = true
        config.mediaTypesRequiringUserActionForPlayback = []
        PieceAudio.activate()
        let view = WKWebView(frame: .zero, configuration: config)
        view.allowsLinkPreview = false
        view.isOpaque = false
        view.backgroundColor = .clear
        view.scrollView.backgroundColor = .clear
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        voice.webView = view
        view.navigationDelegate = context.coordinator
        voice.reloadWorkspace()
        return view
    }
    func updateUIView(_ view: WKWebView, context: Context) {}
    static func dismantleUIView(_ view: WKWebView, coordinator: WorkspaceCoordinator) {
        coordinator.session?.cancelHold()
        view.navigationDelegate = nil
        view.stopLoading()
        view.configuration.userContentController.removeScriptMessageHandler(forName: "walkie")
    }
}

@MainActor
final class WhistlegraphSession: NSObject, ObservableObject, WKScriptMessageHandler, WKNavigationDelegate {
    @Published var layout = NativeLayout()
    @Published var snapshot = PieceSnapshot()
    @Published var showingAIConsent = false
    private var consentRequest: (subject: String, handle: String, generation: Int, code: String, head: Int)?
    private var afterAIConsent: (() -> Void)?
    private var acceptedAIConsent = false
    @Published private(set) var localDataRevision = 0
    let aiConsent = AIConsent.shared
    @Published var engineReady = false
    private var performanceTurn = false
    @Published var performanceCapture = false
    @Published var capturePhase: CapturePhase = .idle
    @Published var captureStarted: Date?
    @Published var microphoneLevels = Array(repeating: 0.0, count: 28)
    @Published var transcript = ""
    @Published var captureError: String?
    @Published var narratedFrame = 0
    var narratedVersion: Int?
    @Published var workspaceReady = false
    #if DEBUG
    @Published var audioTestResult = "Waiting for piece audio"
    @Published var consentFixtureRequests = 0
    #endif
    var isConsentFixture: Bool {
        #if DEBUG
        return NativeScreenFixture.mode == "consent"
        #else
        return false
        #endif
    }
    @Published var pieces: [PieceSummary] = []
    @Published private(set) var pixelSize = WhistlegraphPreview.savedPixelSize
    @Published private(set) var previewFormat = PreviewFormat.saved
    let drawing = DrawingDraft()
    let tv = WhistlegraphTV()
    private var speechStartedAt: TimeInterval?

    func resumePieceAudio() {
        PieceAudio.activate()
        guard let frame = previewFrame else { return }
        Task { _ = try? await webView?.callAsyncJavaScript("window.AC?.startAudio?.();", arguments: [:], in: frame, contentWorld: .page) }
    }

    func setPixelSize(_ size: Int) {
        guard (1...4).contains(size), !snapshot.busy, capturePhase == .idle else { return }
        pixelSize = size
        UserDefaults.standard.set(size, forKey: WhistlegraphPreview.pixelSizeKey)
        guard let frame = previewFrame else { return }
        Task { _ = try? await webView?.callAsyncJavaScript("window.walkiewareSetPixelSize?.(size);", arguments: ["size": size], in: frame, contentWorld: .page) }
    }

    func setPreviewFormat(_ format: PreviewFormat) {
        guard !snapshot.busy, capturePhase == .idle else { return }
        previewFormat = format
        UserDefaults.standard.set(format.rawValue, forKey: PreviewFormat.preference)
        // WKWebView resizes the live framebuffer. Keep the piece, its state,
        // and the normalized chalk strokes instead of reloading the runtime.
    }

    func command(_ action: String, version: Int? = nil, text: String? = nil, piece: String? = nil) {
        guard ["checkout", "newPiece", "openPiece", "stop", "signIn", "ask", "retry", "presentVersion", "endPresentation", "setModel", "refreshBraincells"].contains(action) else { return }
        if ["ask", "retry"].contains(action), !aiConsent.creation {
            requestAIConsent { [weak self] in self?.command(action, version: version, text: text, piece: piece) }
            return
        }
        if action == "newPiece" || action == "openPiece" {
            guard engineReady, !snapshot.busy, capturePhase == .idle else { return }
            if action == "openPiece" { guard let piece, pieces.contains(where: { $0.id == piece && !$0.current }) else { return } }
            drawing.clear(); drawing.enabled = false
            previewSource = ""; engineReady = false
        }
        if action == "presentVersion" || action == "endPresentation" {
            presentingStory = action == "presentVersion"
            if !presentingStory { storyPreview.stop() }
            if let frame = previewFrame {
                let script = presentingStory ? "window.__storyPreviousVolume ??= window.AC?.getMasterVolume(); window.AC?.setMasterVolume(0);" : "window.AC?.setMasterVolume(window.__storyPreviousVolume ?? 1); delete window.__storyPreviousVolume;"
                Task { _ = try? await webView?.callAsyncJavaScript(script, arguments: [:], in: frame, contentWorld: .page) }
            }
        }
        var value: [String: Any] = ["action": action]
        if action == "setModel" {
            guard !snapshot.busy, capturePhase == .idle, let text else { return }
            value["text"] = text
        }
        if let version { value["version"] = version }
        if action == "openPiece", let piece { value["piece"] = piece }
        if action == "ask" {
            guard engineReady, !snapshot.busy, capturePhase == .idle, let text, (!text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty || drawing.hasInk), text.count <= 96 else { return }
            value["text"] = text
            if let sketch = drawing.payload() { value["drawing"] = sketch }
        }
        #if DEBUG
        if isConsentFixture && ["ask", "retry"].contains(action) {
            consentFixtureRequests += 1; return // Exercise the native gate without paid inference.
        }
        #endif
        Task { _ = try? await webView?.callAsyncJavaScript("window.walkiewareNativeCommand?.(command)", arguments: ["command": value], in: nil, contentWorld: .page) }
    }
    func beginHold() {
        guard engineReady, !snapshot.busy, capturePhase == .idle else { return }
        guard !snapshot.handle.isEmpty else { command("signIn"); return }
        // Permission does not start a microphone after the finger has lifted.
        guard aiConsent.creation else { requestAIConsent(); return }
        if isConsentFixture { return }
        performanceTurn = false
        captureError = nil; transcript = ""; speechStartedAt = nil; capturePhase = .opening
        webView?.evaluateJavaScript("voiceStart()")
    }
    func latchPerformance() {
        guard capturePhase == .opening || capturePhase == .recording else { return }
        performanceTurn = true; performanceCapture = true; drawing.enabled = true; capture.latchPerformance()
        webView?.evaluateJavaScript("window.walkiewareLatchPerformance?.()")
    }
    func endHold() { webView?.evaluateJavaScript("voiceEnd()") }
    func syncAIAccount() async {
        if isConsentFixture { aiConsent.bind(subject: "fixture:ai-consent", handle: "preview"); return }
        let expected = account.generation
        let subject = try? await account.subject()
        guard expected == account.generation else { return }
        aiConsent.bind(subject: subject ?? nil, handle: snapshot.handle)
    }
    var canAllowAIConsent: Bool {
        guard let request = consentRequest else { return false }
        return aiConsent.signedIn && request.subject == aiConsent.subject &&
            request.handle == snapshot.handle && request.generation == account.generation
    }
    func requestAIConsent(resume: (() -> Void)? = nil) {
        guard !showingAIConsent else { return }
        if aiConsent.creation { resume?(); return }
        guard !snapshot.handle.isEmpty else { command("signIn"); return }
        let generation = account.generation, handle = snapshot.handle
        let code = snapshot.code, head = snapshot.head
        Task {
            await syncAIAccount()
            guard generation == account.generation, handle == snapshot.handle,
                  code == snapshot.code, head == snapshot.head, !showingAIConsent,
                  let subject = aiConsent.subject, aiConsent.signedIn else { return }
            if aiConsent.creation { resume?(); return }
            consentRequest = (subject, handle, generation, code, head)
            afterAIConsent = resume; acceptedAIConsent = false
            showingAIConsent = true
        }
    }
    func allowAIConsent() {
        guard canAllowAIConsent else { declineAIConsent(); return }
        aiConsent.set(\.creation, true)
        acceptedAIConsent = true; showingAIConsent = false
    }
    func declineAIConsent() {
        acceptedAIConsent = false; showingAIConsent = false
    }
    func finishAIConsentPrompt() {
        let request = consentRequest, resume = afterAIConsent
        let accepted = acceptedAIConsent && canAllowAIConsent
        consentRequest = nil; afterAIConsent = nil; acceptedAIConsent = false
        guard accepted, let request, let resume else { return }
        Task {
            // Await the WebView gate before continuing the original action.
            guard let webView else { return }
            do {
                _ = try await webView.callAsyncJavaScript("window.__whistlegraphAIConsent = value; window.walkiewareSetAIConsent?.(value);",
                    arguments: ["value": aiConsent.bridge], in: nil, contentWorld: .page)
            } catch { return }
            guard aiConsent.creation, request.subject == aiConsent.subject,
                  request.handle == snapshot.handle, request.generation == account.generation,
                  request.code == snapshot.code, request.head == snapshot.head else { return }
            resume()
        }
    }
    private func applyAIConsent() {
        if showingAIConsent && !canAllowAIConsent { declineAIConsent() }
        if !aiConsent.creation { command("stop"); cancelHold() }
        if !aiConsent.cloudSpeech { cancelHold() }
        if !aiConsent.cloudNarration { StoryVoice.cancelCloudRequests() }
        let value = aiConsent.bridge
        Task { _ = try? await webView?.callAsyncJavaScript("window.__whistlegraphAIConsent = value; window.walkiewareSetAIConsent?.(value);", arguments: ["value": value], in: nil, contentWorld: .page) }
    }
    func eraseDeletedAccountLocally() async throws {
        command("stop"); cancelHold(); storyPreview.stop(); tv.disconnect()
        localDataRevision += 1
        aiConsent.forget(); StoryVoice.erasePendingWork()
        account.signOut()
        _ = try? await webView?.callAsyncJavaScript("window.walkiewareForgetLocalData?.();", arguments: [:], in: nil, contentWorld: .page)
        webView?.stopLoading()
        await WKWebsiteDataStore.default().removeData(ofTypes: WKWebsiteDataStore.allWebsiteDataTypes(), modifiedSince: .distantPast)
        let manager = FileManager.default
        let support = manager.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
        let caches = manager.urls(for: .cachesDirectory, in: .userDomainMask)[0]
        let documents = manager.urls(for: .documentDirectory, in: .userDomainMask)[0]
        var cleanupError: Error?
        for url in [support.appendingPathComponent("Utterances"), support.appendingPathComponent("whistlegraph-drawing-draft.json"),
                    caches.appendingPathComponent("StoryVoice"), caches.appendingPathComponent("StoryMovies")] {
            if manager.fileExists(atPath: url.path) { do { try manager.removeItem(at: url) } catch { cleanupError = error } }
        }
        // Documents contains app-created local exports and test artifacts only.
        do {
            for url in try manager.contentsOfDirectory(at: documents, includingPropertiesForKeys: nil) {
                do { try manager.removeItem(at: url) } catch { cleanupError = error }
            }
        } catch { cleanupError = error }
        do {
            for url in try manager.contentsOfDirectory(at: manager.temporaryDirectory, includingPropertiesForKeys: nil)
                where ["story-voice-", "story-canvas-", "Whistlegraph-"].contains(where: { url.lastPathComponent.hasPrefix($0) }) {
                do { try manager.removeItem(at: url) } catch { cleanupError = error }
            }
        } catch { cleanupError = error }
        if let name = Bundle.main.bundleIdentifier { UserDefaults.standard.removePersistentDomain(forName: name) }
        drawing.clear(); drawing.enabled = false
        snapshot = PieceSnapshot(); pieces = []; previewSource = ""; engineReady = false
        reloadWorkspace()
        if let cleanupError { throw cleanupError }
    }
    /// Drops the Keychain sign-in and tells the engine, which parks its sockets.
    func signOut() {
        guard capturePhase == .idle else { return }
        account.signOut()
        aiConsent.bind(subject: nil, handle: "")
        emitEngine(["kind": "account", "token": ""])
    }
    func cancelHold() { performanceCapture = false; webView?.evaluateJavaScript("voiceEnd(true)"); cancel() }

    @Published var startupFailure: String?
    private var startupTimeout: Task<Void, Never>?
    private var recoveryCount = 0
    weak var webView: WKWebView?

    func reloadWorkspace() {
        workspaceReady = false; engineReady = false; startupFailure = nil
        startupTimeout?.cancel()
        webView?.load(URLRequest(url: URL(string: "walkieware://app/index.html?walkie=1")!))
        startupTimeout = Task { [weak self] in
            try? await Task.sleep(for: .seconds(12))
            guard !Task.isCancelled, let self, !self.workspaceReady else { return }
            self.startupFailure = "The workspace did not finish opening. Tap Reload to try again."
        }
    }
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        print("[walkieware] main page loaded")
        webView.evaluateJavaScript("({controls:!!document.getElementById('speak'),engine:typeof window.walkiewareAsk,ready:document.readyState})") { value, error in
            print("[walkieware] startup check: \(value ?? error?.localizedDescription ?? "no result")")
        }
    }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        startupFailure = "Could not open the workspace: " + error.localizedDescription
        print("[walkieware] navigation failed: \((error as NSError).code)")
    }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        didFailProvisionalNavigation(webView, navigation: navigation, error: error)
    }
    private func didFailProvisionalNavigation(_ view: WKWebView, navigation: WKNavigation!, error: Error) {
        startupFailure = "Could not open the workspace: " + error.localizedDescription
    }
    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
        cancel(); workspaceReady = false
        print("[walkieware] web content process terminated")
        if recoveryCount < 1 { recoveryCount += 1; reloadWorkspace() }
        else { startupFailure = "The workspace stopped. Tap Reload to reopen it." }
    }
    let account = WhistlegraphAccount()
    let braincells = WhistlegraphBraincells()
    #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
    let tezosBraincells = TezosBraincells()
    #endif
    // The microphone, recognizer and release timing live in SpeechCapture; this
    // class only turns its events into screen state and bridge messages.
    private let capture = SpeechCapture()
    override init() {
        super.init()
        #if DEBUG
        if isConsentFixture, ProcessInfo.processInfo.environment["WALKIE_RESET_AI_CONSENT"] == "1" {
            UserDefaults.standard.removeObject(forKey: AIConsentRecord.key(subject: "fixture:ai-consent"))
        }
        #endif
        aiConsent.changed = { [weak self] in self?.applyAIConsent() }
        capture.hasVisualInput = { [weak self] in self?.drawing.hasInk == true }
        capture.speechToken = { [weak self] in
            guard let self, self.snapshot.handle == "jeffrey", self.aiConsent.cloudSpeech else { return nil }
            let generation = self.account.generation
            let token = try await self.account.token()
            guard !Task.isCancelled, generation == self.account.generation, self.aiConsent.cloudSpeech else { return nil }
            return token
        }
        capture.onEvent = { [weak self] kind, text, id in self?.emit(kind, text: text, id: id) }
        capture.onLevel = { [weak self] rms in guard let self else { return }; self.microphoneLevels = Array(self.microphoneLevels.dropFirst()) + [rms] }
        capture.onReplayRelease = { [weak self] in self?.webView?.evaluateJavaScript("voiceEnd()", completionHandler: nil) }
    }
    @Published var storyStatus = ""
    lazy var storyPreview = StoryPreview(session: self)
    private var presentingStory = false
    var storyTapeEvent: (([String: Any]) -> Void)?
    func storyTape(_ action: String, arguments: [String: Any] = [:]) async throws {
        if presentingStory { try await storyPreview.tape(action, arguments: arguments); return }
        guard let frame = previewFrame, let webView else { throw NSError(domain: "StoryTape", code: 1, userInfo: [NSLocalizedDescriptionKey: "The piece is not ready to export."]) }
        _ = try await webView.callAsyncJavaScript("if (!window.whistlegraphStoryTape) throw Error('Canvas tape is unavailable'); await window.whistlegraphStoryTape[action](value ?? id);", arguments: ["action": action, "value": arguments["value"] ?? NSNull(), "id": arguments["id"] ?? NSNull()], in: frame, contentWorld: .page)
    }
    private var previewFrame: WKFrameInfo?
    private var previewSource = ""
    private var previewRequestID = 0
    private var paintedPreviewHash: String?
    private var visualCaptureTask: Task<Void, Never>?
    private var previewThreadID = UUID().uuidString

    #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
    func mintCapture() async throws -> (hash: String, png: String) {
        guard let webView, !snapshot.busy, !presentingStory, !previewSource.isEmpty,
              paintedPreviewHash == VisualCapture.hash(previewSource) else {
            throw NativeSignIn.failure("Wait for this version to finish painting.")
        }
        let hash = VisualCapture.hash(previewSource), request = previewRequestID
        let geometry = try await webView.callAsyncJavaScript("const r = document.getElementById('live-piece').getBoundingClientRect(); return {rect:{x:r.x,y:r.y,width:r.width,height:r.height},viewport:{width:innerWidth,height:innerHeight}};", arguments: [:], in: nil, contentWorld: .page)
        guard let geometry = geometry as? [String: Any], let rect = geometry["rect"] as? [String: Double],
              let viewport = geometry["viewport"] as? [String: Double] else { throw NativeSignIn.failure("Could not locate the artwork.") }
        let frames = try await VisualCapture.frames(view: webView, rect: rect, viewport: viewport, count: 1) {
            self.previewRequestID == request && self.paintedPreviewHash == hash
        }
        guard let png = frames.first?["png"] as? String else { throw NativeSignIn.failure("Could not capture the artwork cover.") }
        return (hash, png)
    }
    #endif

    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if PreviewNavigation.bridge(message.frameInfo.request.url, mainFrame: message.frameInfo.isMainFrame, document: .workspace) == .artwork,
           let body = message.body as? [String: Any] {
            #if DEBUG
            if NativeScreenFixture.mode == "audio", body["action"] as? String == "audioProbe" {
                let peak = body["peak"] as? Double ?? 0
                let gestures = body["gestures"] as? Int ?? -1
                if peak > 0.001 && gestures == 0 { audioTestResult = "Piece audio without a tap" }
                else { audioTestResult = "Audio: \(body["state"] ?? "unknown"), worklet \(body["ready"] ?? false), peak \(peak), gestures \(gestures)" }
                return
            }
            #endif
            if body["action"] as? String == "storyTape" { storyTapeEvent?(body); return }
            if body["action"] as? String == "previewReady" {
                #if DEBUG
                print("[walkieware] preview runtime ready")
                #endif
                previewFrame = message.frameInfo
                renderPreview()
                emitEngine(["kind": "previewReady"])
            } else if body["action"] as? String == "previewEvent" {
                if let event = body["event"] as? [String: Any],
                   event["requestID"] as? Int == previewRequestID,
                   event["sourceHash"] as? String == VisualCapture.hash(previewSource) {
                    if event["kind"] as? String == "painted" { paintedPreviewHash = event["sourceHash"] as? String; tv.update(previewSource) }
                    if event["kind"] as? String == "invalidated" { paintedPreviewHash = nil }
                }
                #if DEBUG
                if let event = body["event"] as? [String: Any], event["kind"] as? String == "painted" { print("[walkieware] checkpoint painted"); AudioBenchmark.mark("firstPainted")
                    if AudioBenchmark.enabled {
                        webView?.takeSnapshot(with: nil) { image, _ in
                            guard let data = image?.pngData(), let directory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first else { return }
                            try? data.write(to: directory.appendingPathComponent("walkieware-benchmark.png"), options: .atomic)
                        }
                    } }
                #endif
                emitEngine(["kind": "previewEvent", "event": body["event"] ?? [:]])
            }
            return
        }
        guard PreviewNavigation.bridge(message.frameInfo.request.url, mainFrame: message.frameInfo.isMainFrame, document: .workspace) == .workspace,
              let body = message.body as? [String: Any],
              let action = body["action"] as? String,
              let id = body["id"] as? String, id.count <= 100 else { return }
        switch action {
        case "cancelVisualCapture":
            visualCaptureTask?.cancel()
        case "visualCapture":
            guard let captureID = body["captureID"] as? String, UUID(uuidString: captureID) != nil,
                  let hash = body["sourceHash"] as? String,
                  let renderID = body["renderID"] as? Int,
                  let viewport = body["viewport"] as? [String: Double],
                  let rect = body["rect"] as? [String: Double], let webView else { return }
            visualCaptureTask?.cancel()
            visualCaptureTask = Task { [weak self] in
                guard let self else { return }
                var result: [String: Any] = ["kind": "visualCapture", "captureID": captureID, "sourceHash": hash, "renderID": renderID]
                do {
                    result["frames"] = try await VisualCapture.frames(view: webView, rect: rect, viewport: viewport) {
                        self.previewRequestID == renderID && self.paintedPreviewHash == hash
                    }
                } catch { result["error"] = error.localizedDescription }
                #if DEBUG
                result["geometry"] = ["viewport": viewport, "rect": rect, "view": ["width": webView.bounds.width, "height": webView.bounds.height]]
                if ProcessInfo.processInfo.environment["WHISTLEGRAPH_VISUAL_CAPTURE_TEST"] == "1",
                   let directory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first,
                   let data = try? JSONSerialization.data(withJSONObject: result) {
                    try? data.write(to: directory.appendingPathComponent("whistlegraph-visual-capture.json"), options: .atomic)
                }
                #endif
                self.emitEngine(result)
            }
        case "presentation":
            guard presentingStory, let version = body["version"] as? Int, let source = body["source"] as? String, source.utf8.count <= 512_000 else { return }
            storyPreview.present(version: version, source: source)
        case "narrationReady":
            narratedVersion = body["version"] as? Int; narratedFrame += 1
        case "layout":
            guard let values = body["layout"] as? [String: Double], values.count <= 5 else { return }
            layout = NativeLayout(tokens: values)
            print("[walkieware] native layout spacing=\(layout.spacing) title=\(layout.titleSize)")
        case "snapshot":
            guard let value = body["snapshot"] as? [String: Any],
                  let data = try? JSONSerialization.data(withJSONObject: value), data.count < 2_000_000,
                  var next = try? JSONDecoder().decode(PieceSnapshot.self, from: data),
                  next.versions.count <= 256, next.code.count <= 32, next.handle.count <= 100 else { return }
            if next.revisions == nil { next.versions = snapshot.versions }
            #if DEBUG
            if NativeScreenFixture.enabled {
                next.handle = "preview"; next.code = "wwDemo"
                next.colors = [[220,180,255],[150,150,255],[255,170,80],[90,220,140],[90,215,210],[200,140,255],[255,140,210],[150,225,90]]
                if !engineReady && NativeScreenFixture.mode == "recording" { capturePhase = .recording; captureStarted = Date(); transcript = "Make the moon bounce like this…" }
                // The fixture runs without a cloud thread, so the engine posts no
                // piece list; seed one so the pieces sheet can be exercised.
                if pieces.isEmpty {
                    let formatter = ISO8601DateFormatter()
                    pieces = [PieceSummary(id: "fixture", code: "wwDemo", utterance: "Add a little moon", versions: 3, updatedAt: formatter.string(from: Date().addingTimeInterval(-30)), current: true),
                              PieceSummary(id: "fixture-pond", code: "wwPond", utterance: "A frog on a lily pad", versions: 5, updatedAt: formatter.string(from: Date().addingTimeInterval(-5400)), current: false)]
                }
            }
            #endif
            snapshot = next; tv.updateProgress(next); engineReady = true
        case "voiceIdle":
            performanceCapture = false
            capturePhase = .idle; captureStarted = nil
        case "pieces":
            guard let value = body["pieces"] as? [[String: Any]], value.count <= 256,
                  let data = try? JSONSerialization.data(withJSONObject: value),
                  let next = try? JSONDecoder().decode([PieceSummary].self, from: data) else { return }
            pieces = next

        case "threadStatus":
            if let code = body["code"] as? String, let status = body["status"] as? String {
                print("[walkieware] \(code) · \(status)")
            }
        case "sequenceCapture":
            #if DEBUG
            if SequenceBenchmark.enabled, let report = body["report"] as? [String: Any], let rect = body["rect"] as? [String: Double], let webView {
                Task { await SequenceBenchmark.capture(report, rect: rect, view: webView) }
            }
            #endif
        case "benchmark":
            if let name = body["event"] as? String, ["requestDispatched", "firstModelOutput", "firstCheckpoint", "generationFinished", "generationFailed", "transcriptPainted", "signedIn", "generationEntered", "guidesReady", "firstIncrementalCompile", "inferenceHeaders", "jevDecision", "jevFallback", "jevCacheHit", "jevApplied", "inputSocketReady", "inputSocketAck", "inputHttpFallback", "starterDispatched", "starterPainted", "refinementFailed", "localEditDispatched", "localEditPainted"].contains(name) { AudioBenchmark.mark(name, fields: body["fields"] as? [String: Any] ?? [:]) }
        case "ready":
            workspaceReady = true; startupFailure = nil; startupTimeout?.cancel()
            print("[walkieware] controls ready")
            if AudioBenchmark.enabled || SequenceBenchmark.enabled { UIApplication.shared.isIdleTimerDisabled = true }
            Task { await NetworkBenchmark.run() }
        case "account":
            if isConsentFixture { emitEngine(["kind": "account", "token": ""]); return }
            Task { do { emitEngine(["kind": "account", "token": try await account.token() ?? ""]) }
                catch { emitEngine(["kind": "account", "token": ""]) } }
        case "aiConsent":
            // The network guard can reject background recovery or stale account
            // work. Only native Send/retry/Talk gestures may present permission.
            break
        case "signIn":
            if isConsentFixture { return }
            account.signIn(from: webView) { [weak self] result in
                switch result {
                case .success(let token): self?.emitEngine(["kind": "account", "token": token])
                case .failure(let error): self?.emitEngine(["kind": "error", "text": error.localizedDescription])
                }
            }
        case "drawingCommitted":
            if let id = body["drawingID"] as? String, let revision = body["revision"] as? Int { drawing.consume(id: id, revision: revision) }
        case "render":
            guard let source = body["source"] as? String, source.utf8.count < 500_000 else { return }
            visualCaptureTask?.cancel(); paintedPreviewHash = nil
            if let id = body["threadID"] as? String, UUID(uuidString: id) != nil { previewThreadID = id }
            previewRequestID = body["renderID"] as? Int ?? 0
            previewSource = source; renderPreview()
        case "start":
            capturePhase = .opening; captureError = nil; transcript = ""
            microphoneLevels = Array(repeating: 0, count: 28)
            capture.start(id)
            if performanceCapture { capture.latchPerformance() }
        case "stop": capture.stop(matching: id)
        case "cancel": if id == capture.turn { cancel() }
        case "share":
            guard let value = body["data"] as? String, value.count < 10_000_000,
                  value.hasPrefix("data:image/png;base64,"),
                  let data = Data(base64Encoded: String(value.dropFirst(22))),
                  let image = UIImage(data: data),
                  let host = webView?.window?.rootViewController else { return }
            let share = UIActivityViewController(activityItems: [image], applicationActivities: nil)
            share.popoverPresentationController?.sourceView = webView
            host.present(share, animated: true)
        default: break
        }
    }

    private func renderPreview() {
        guard let frame = previewFrame, !previewSource.isEmpty else { return }
        let source = previewSource
        let threadID = previewThreadID
        let renderID = previewRequestID
        let size = pixelSize
        Task { _ = try? await webView?.callAsyncJavaScript("window.walkiewareSetPixelSize?.(size); window.walkiewareRender?.(source, threadID, renderID);", arguments: ["source": source, "threadID": threadID, "renderID": renderID, "size": size], in: frame, contentWorld: .page) }
    }


    func emitEngine(_ event: [String: Any]) {
        guard let data = try? JSONSerialization.data(withJSONObject: event),
              let json = String(data: data, encoding: .utf8) else { return }
        webView?.evaluateJavaScript("window.walkiewareEngineEvent?.(\(json))", completionHandler: nil)
    }

    private func emit(_ kind: String, text: String = "", id: String? = nil) {
        if ["processing", "mixedFinal", "final", "error"].contains(kind) { resumePieceAudio() }
        if ["processing", "mixedFinal", "final", "error"].contains(kind) { performanceCapture = false }
        switch kind {
        case "listening": capturePhase = .recording; captureStarted = Date(); speechStartedAt = ProcessInfo.processInfo.systemUptime
        case "partial": transcript = text
        case "processing", "mixedFinal", "final": capturePhase = .processing; captureStarted = nil; if kind != "processing" { ButtonSounds.play(.sent) }
        case "error": capturePhase = .idle; captureStarted = nil; captureError = text; ButtonSounds.play(.error)
        default: break
        }
        if kind == "partial", !text.isEmpty { AudioBenchmark.mark("firstRecognizedWords") }
        if kind == "final" { AudioBenchmark.checkTranscript(text) }
        var event: [String: Any] = ["kind": kind, "text": text, "id": id ?? capture.turn]
        if performanceTurn { event["performance"] = true }
        if ["mixedFinal", "final"].contains(kind), let sketch = drawing.payload(speechStart: speechStartedAt) { event["drawing"] = sketch }
        guard let data = try? JSONSerialization.data(withJSONObject: event),
              let json = String(data: data, encoding: .utf8) else { return }
        webView?.evaluateJavaScript("window.walkieNativeEvent?.(\(json))") { _, error in
            if kind == "final", AudioBenchmark.enabled {
                AudioBenchmark.mark("finalDelivered", fields: ["javascriptError": error?.localizedDescription ?? ""])
            }
        }
    }

    func cancel() {
        capturePhase = .idle; captureStarted = nil
        capture.cancel()
    }
}
