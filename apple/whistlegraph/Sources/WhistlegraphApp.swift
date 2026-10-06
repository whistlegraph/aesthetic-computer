import SwiftUI
import WebKit
import Speech
import AVFoundation
import CoreText

@main
struct WhistlegraphApp: App {
    @StateObject private var voice = WhistlegraphSession()
    @Environment(\.scenePhase) private var phase
    @AppStorage("whistlegraph-appearance") private var appearance = "system"
    init() {
        let defaults = UserDefaults.standard
        if defaults.object(forKey: "whistlegraph-appearance") == nil,
           let appearance = defaults.string(forKey: "walkieware-appearance") {
            defaults.set(appearance, forKey: "whistlegraph-appearance")
        }
        if let root = Bundle.main.url(forResource: "Web", withExtension: nil) {
            for name in ["ComicRelief-Regular.ttf", "ComicRelief-Bold.ttf"] { CTFontManagerRegisterFontsForURL(root.appendingPathComponent(name) as CFURL, .process, nil) }
        }
    }
    var body: some Scene {
        WindowGroup {
            ZStack {
                Color(uiColor: .systemBackground).ignoresSafeArea()
                WhistlegraphScreen(session: voice)
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
            .preferredColorScheme(appearance == "light" ? .light : appearance == "dark" ? .dark : nil)
            .onChange(of: phase) { _, value in
                if value == .background { voice.cancelHold() }
            }
        }
    }
}

struct Workspace: UIViewRepresentable {
    let voice: WhistlegraphSession
    func makeCoordinator() -> WorkspaceCoordinator { WorkspaceCoordinator(session: voice) }
    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphNativeShell = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        config.userContentController.add(context.coordinator, name: "whistlegraph")
        config.setURLSchemeHandler(WhistlegraphBundle(), forURLScheme: WhistlegraphBundle.storageScheme)
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
        config.userContentController.addUserScript(WKUserScript(source: WhistlegraphPreview.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
        #if DEBUG
        if NativeScreenFixture.enabled { config.userContentController.addUserScript(WKUserScript(source: NativeScreenFixture.script, injectionTime: .atDocumentStart, forMainFrameOnly: true)) }
        if let model = ProcessInfo.processInfo.environment["WHISTLEGRAPH_MODEL"],
           let data = try? JSONSerialization.data(withJSONObject: [model]),
           let json = String(data: data, encoding: .utf8) {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphModel = \(json)[0];", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if let revision = Int(ProcessInfo.processInfo.environment["WHISTLEGRAPH_RESTORE_VERSION"] ?? ""), revision >= 0 {
            config.userContentController.addUserScript(WKUserScript(source: """
            try {
              const key = 'whistlegraph-sequence-source-versions';
              const ledger = JSON.parse(localStorage.getItem(key));
              const selected = ledger?.versions?.find(v => v.id === \(revision));
              if (selected) {
                ledger.head = selected.id;
                localStorage.setItem(key, JSON.stringify(ledger));
                localStorage.setItem('whistlegraph-sequence-source', selected.source);
              }
            } catch {}
            """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if let keepTest = ProcessInfo.processInfo.environment["WHISTLEGRAPH_KEEP_TEST_PIECE"], ["1", "audio", "local", "space"].contains(keepTest) {
            let testSourceKey = keepTest == "space" ? "whistlegraph-space-source" : keepTest == "local" ? "whistlegraph-local-source" : keepTest == "audio" ? "whistlegraph-benchmark-source" : "whistlegraph-sequence-source"
            config.userContentController.addUserScript(WKUserScript(source: """
            try {
              const result = localStorage.getItem('\(testSourceKey)');
              if (result) {
                const previous = localStorage.getItem('whistlegraph-source');
                if (previous && previous !== result) localStorage.setItem('whistlegraph-before-sequence', previous);
                localStorage.setItem('whistlegraph-source', result);
                const versions = localStorage.getItem('\(testSourceKey)-versions');
                if (versions) {
                  const old = localStorage.getItem('whistlegraph-source-versions');
                  if (old) localStorage.setItem('whistlegraph-before-sequence-versions', old);
                  localStorage.setItem('whistlegraph-source-versions', versions);
                }
              }
            } catch {}
            """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_SCENE_TEST"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphScene = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if let revision = Int(ProcessInfo.processInfo.environment["WHISTLEGRAPH_REVIEW_VERSION"] ?? ""), revision >= 0 {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphReviewVersion = \(revision);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_LOCAL_SEQUENCE"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphLocalSequence = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_SPACE_TEST"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphSpace = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if SequenceBenchmark.enabled {
            let start = max(1, min(32, Int(ProcessInfo.processInfo.environment["WHISTLEGRAPH_SEQUENCE_START"] ?? "1") ?? 1))
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphSequence = true; window.__whistlegraphSequenceStart = \(start);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if AudioBenchmark.enabled {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphBenchmark = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
            config.userContentController.addUserScript(WKUserScript(source: """
            const audioCheck = setInterval(() => {
              if (!window.whistlegraphAccountReady || !window.whistlegraphAsk) return;
              clearInterval(audioCheck);
              window.whistlegraphStartFixture();
            }, 100);
            """, injectionTime: .atDocumentEnd, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_PREVIEW_CHECK"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: """
            const check = setInterval(() => {
              if (!window.whistlegraphAsk) return;
              clearInterval(check);
              document.body.classList.add('live-mode','live-preview');
              document.getElementById('live-work').hidden = false;
              document.getElementById('live-phase').textContent = 'Preview smoke test';
              window.webkit.messageHandlers.whistlegraph.postMessage({action:'render',id:'smoke',source:'export function paint({wipe,ink,screen}) { wipe("navy"); ink("pink").circle(screen.width/2,screen.height/2,50); }'});
            }, 100);
            """, injectionTime: .atDocumentEnd, forMainFrameOnly: true))
        }
        #endif
        config.preferences.inactiveSchedulingPolicy = .none
        config.allowsInlineMediaPlayback = true
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
        view.configuration.userContentController.removeScriptMessageHandler(forName: "whistlegraph")
    }
}

@MainActor
final class WhistlegraphSession: NSObject, ObservableObject, WKScriptMessageHandler, WKNavigationDelegate {
    @Published var layout = NativeLayout()
    @Published var snapshot = PieceSnapshot()
    @Published var engineReady = false
    @Published var capturePhase: CapturePhase = .idle
    @Published var captureStarted: Date?
    @Published var microphoneLevels = Array(repeating: 0.0, count: 28)
    @Published var transcript = ""
    @Published var captureError: String?
    @Published var narratedFrame = 0
    var narratedVersion: Int?
    @Published var workspaceReady = false
    @Published var pieces: [PieceSummary] = []

    func command(_ action: String, version: Int? = nil, text: String? = nil, piece: String? = nil, ware: String? = nil) {
        guard ["checkout", "newPiece", "openPiece", "stop", "signIn", "ask", "retry", "presentVersion", "endPresentation", "setWare", "playRoblox", "exportRoom", "undoRoom"].contains(action) else { return }
        if ["setWare", "playRoblox", "exportRoom", "undoRoom"].contains(action) {
            guard engineReady, !snapshot.busy, capturePhase == .idle else { return }
        }
        if action == "newPiece" || action == "openPiece" {
            guard engineReady, !snapshot.busy, capturePhase == .idle else { return }
            if action == "openPiece" { guard let piece, pieces.contains(where: { $0.id == piece && !$0.current }) else { return } }
            previewSource = ""; engineReady = false
        }
        var value: [String: Any] = ["action": action]
        if action == "setWare" {
            guard let ware, Ware(rawValue: ware) != nil else { return }
            value["ware"] = ware
        }
        if let version { value["version"] = version }
        if action == "openPiece", let piece { value["piece"] = piece }
        if action == "ask" {
            guard engineReady, !snapshot.busy, capturePhase == .idle, let text, !text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty, text.count <= 96 else { return }
            value["text"] = text
        }
        Task { _ = try? await webView?.callAsyncJavaScript("window.whistlegraphNativeCommand?.(command)", arguments: ["command": value], in: nil, contentWorld: .page) }
    }
    func beginHold() {
        guard engineReady, !snapshot.busy, capturePhase == .idle else { return }
        captureError = nil; transcript = ""; capturePhase = .opening
        webView?.evaluateJavaScript("voiceStart()")
    }
    func endHold() { webView?.evaluateJavaScript("voiceEnd()") }
    /// Drops the Keychain sign-in and tells the engine, which parks its sockets.
    func signOut() {
        guard capturePhase == .idle else { return }
        account.signOut()
        emitEngine(["kind": "account", "token": ""])
    }
    func cancelHold() { webView?.evaluateJavaScript("voiceEnd(true)"); cancel() }

    @Published var startupFailure: String?
    private var startupTimeout: Task<Void, Never>?
    private var recoveryCount = 0
    weak var webView: WKWebView?

    func reloadWorkspace() {
        workspaceReady = false; engineReady = false; startupFailure = nil
        startupTimeout?.cancel()
        webView?.load(URLRequest(url: URL(string: "\(WhistlegraphBundle.storageScheme)://app/index.html?whistlegraph=1")!))
        startupTimeout = Task { [weak self] in
            try? await Task.sleep(for: .seconds(12))
            guard !Task.isCancelled, let self, !self.workspaceReady else { return }
            self.startupFailure = "The workspace did not finish opening. Tap Reload to try again."
        }
    }
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        print("[whistlegraph] main page loaded")
        webView.evaluateJavaScript("({controls:!!document.getElementById('speak'),engine:typeof window.whistlegraphAsk,ready:document.readyState})") { value, error in
            print("[whistlegraph] startup check: \(value ?? error?.localizedDescription ?? "no result")")
        }
    }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        startupFailure = "Could not open the workspace: " + error.localizedDescription
        print("[whistlegraph] navigation failed: \((error as NSError).code)")
    }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        didFailProvisionalNavigation(webView, navigation: navigation, error: error)
    }
    private func didFailProvisionalNavigation(_ view: WKWebView, navigation: WKNavigation!, error: Error) {
        startupFailure = "Could not open the workspace: " + error.localizedDescription
    }
    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
        cancel(); workspaceReady = false
        print("[whistlegraph] web content process terminated")
        if recoveryCount < 1 { recoveryCount += 1; reloadWorkspace() }
        else { startupFailure = "The workspace stopped. Tap Reload to reopen it." }
    }
    let account = WhistlegraphAccount()
    // The microphone, recognizer and release timing live in SpeechCapture; this
    // class only turns its events into screen state and bridge messages.
    private let capture = SpeechCapture()
    override init() {
        super.init()
        capture.onEvent = { [weak self] kind, text, id in self?.emit(kind, text: text, id: id) }
        capture.onLevel = { [weak self] rms in guard let self else { return }; self.microphoneLevels = Array(self.microphoneLevels.dropFirst()) + [rms] }
        capture.onReplayRelease = { [weak self] in self?.webView?.evaluateJavaScript("voiceEnd()", completionHandler: nil) }
    }
    private var previewFrame: WKFrameInfo?
    private var previewSource = ""
    private var previewThreadID = UUID().uuidString

    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if !message.frameInfo.isMainFrame,
           message.frameInfo.request.url?.host == "aesthetic.computer",
           message.frameInfo.request.url?.scheme == "https",
           let body = message.body as? [String: Any] {
            if body["action"] as? String == "previewReady" {
                #if DEBUG
                print("[whistlegraph] preview runtime ready")
                #endif
                previewFrame = message.frameInfo
                renderPreview()
                emitEngine(["kind": "previewReady"])
            } else if body["action"] as? String == "previewEvent" {
                #if DEBUG
                if let event = body["event"] as? [String: Any], event["kind"] as? String == "painted" { print("[whistlegraph] checkpoint painted"); AudioBenchmark.mark("firstPainted")
                    if AudioBenchmark.enabled {
                        webView?.takeSnapshot(with: nil) { image, _ in
                            guard let data = image?.pngData(), let directory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first else { return }
                            try? data.write(to: directory.appendingPathComponent("whistlegraph-benchmark.png"), options: .atomic)
                        }
                    } }
                #endif
                emitEngine(["kind": "previewEvent", "event": body["event"] ?? [:]])
            }
            return
        }
        guard message.frameInfo.isMainFrame,
              message.frameInfo.request.url?.scheme == WhistlegraphBundle.storageScheme,
              message.frameInfo.request.url?.host == "app",
              let body = message.body as? [String: Any],
              let action = body["action"] as? String,
              let id = body["id"] as? String, id.count <= 100 else { return }
        switch action {
        case "wareSwitching":
            previewSource = ""; previewFrame = nil; engineReady = false
            snapshot = PieceSnapshot(); pieces = []
        case "openRoblox":
            guard let raw = body["url"] as? String, let url = URL(string: raw),
                  url.scheme == "https", url.host == "www.roblox.com", url.path == "/share",
                  let parts = URLComponents(url: url, resolvingAgainstBaseURL: false),
                  parts.user == nil, parts.password == nil, parts.port == nil,
                  parts.queryItems?.contains(where: { $0.name == "code" && !($0.value ?? "").isEmpty }) == true else { return }
            UIApplication.shared.open(url, options: [:]) { [weak self] opened in
                self?.emitEngine(["kind": "robloxLaunch", "opened": opened])
            }
        case "exportRoom":
            guard let source = body["source"] as? String, source.utf8.count <= 300_000,
                  let host = webView?.window?.rootViewController else { return }
            let directory = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
            do {
                try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
                let file = directory.appendingPathComponent("Whistlegraph-Room.rbxlx")
                try source.write(to: file, atomically: true, encoding: .utf8)
                let share = UIActivityViewController(activityItems: [file], applicationActivities: nil)
                share.popoverPresentationController?.sourceView = webView
                share.completionWithItemsHandler = { _, _, _, _ in try? FileManager.default.removeItem(at: directory) }
                host.present(share, animated: true)
            } catch { emitEngine(["kind": "error", "text": "Could not export the room."]) }
        case "narrationReady":
            narratedVersion = body["version"] as? Int; narratedFrame += 1
        case "layout":
            guard let values = body["layout"] as? [String: Double], values.count <= 5 else { return }
            layout = NativeLayout(tokens: values)
            print("[whistlegraph] native layout spacing=\(layout.spacing) title=\(layout.titleSize)")
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
            snapshot = next; engineReady = true
        case "voiceIdle":
            capturePhase = .idle; captureStarted = nil
        case "pieces":
            guard let value = body["pieces"] as? [[String: Any]], value.count <= 256,
                  let data = try? JSONSerialization.data(withJSONObject: value),
                  let next = try? JSONDecoder().decode([PieceSummary].self, from: data) else { return }
            pieces = next

        case "threadStatus":
            if let code = body["code"] as? String, let status = body["status"] as? String {
                print("[whistlegraph] \(code) · \(status)")
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
            print("[whistlegraph] controls ready")
            if AudioBenchmark.enabled || SequenceBenchmark.enabled { UIApplication.shared.isIdleTimerDisabled = true }
            Task { await NetworkBenchmark.run() }
        case "account":
            Task { do { emitEngine(["kind": "account", "token": try await account.token() ?? ""]) }
                catch { emitEngine(["kind": "account", "token": ""]) } }
        case "signIn":
            account.signIn(from: webView) { [weak self] result in
                switch result {
                case .success(let token): self?.emitEngine(["kind": "account", "token": token])
                case .failure(let error): self?.emitEngine(["kind": "error", "text": error.localizedDescription])
                }
            }
        case "render":
            guard let source = body["source"] as? String, source.utf8.count < 500_000 else { return }
            if let id = body["threadID"] as? String, UUID(uuidString: id) != nil { previewThreadID = id }
            previewSource = source; renderPreview()
        case "start":
            capturePhase = .opening; captureError = nil; transcript = ""
            microphoneLevels = Array(repeating: 0, count: 28)
            capture.start(id)
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
        Task { _ = try? await webView?.callAsyncJavaScript("window.whistlegraphRender?.(source, threadID);", arguments: ["source": source, "threadID": threadID], in: frame, contentWorld: .page) }
    }

    func emitEngine(_ event: [String: Any]) {
        guard let data = try? JSONSerialization.data(withJSONObject: event),
              let json = String(data: data, encoding: .utf8) else { return }
        webView?.evaluateJavaScript("window.whistlegraphEngineEvent?.(\(json))", completionHandler: nil)
    }

    private func emit(_ kind: String, text: String = "", id: String? = nil) {
        switch kind {
        case "listening": capturePhase = .recording; captureStarted = Date()
        case "partial": transcript = text
        case "processing", "mixedFinal", "final": capturePhase = .processing; captureStarted = nil
        case "error": capturePhase = .idle; captureStarted = nil; captureError = text
        default: break
        }
        if kind == "partial", !text.isEmpty { AudioBenchmark.mark("firstRecognizedWords") }
        if kind == "final" { AudioBenchmark.checkTranscript(text) }
        let event = ["kind": kind, "text": text, "id": id ?? capture.turn]
        guard let data = try? JSONSerialization.data(withJSONObject: event),
              let json = String(data: data, encoding: .utf8) else { return }
        webView?.evaluateJavaScript("window.whistlegraphNativeEvent?.(\(json))") { _, error in
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
