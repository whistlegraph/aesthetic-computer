import SwiftUI
import WebKit
import Speech
import AVFoundation
import CoreText

@main
struct WalkiewareApp: App {
    @StateObject private var voice = WalkiewareSession()
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
                WalkiewareScreen(session: voice)
                if !voice.workspaceReady {
                    VStack(spacing: 20) {
                        Text("walkieware").font(.largeTitle.bold())
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
    let voice: WalkiewareSession
    func makeCoordinator() -> WorkspaceCoordinator { WorkspaceCoordinator(session: voice) }
    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.userContentController.addUserScript(WKUserScript(source: "window.__walkiewareNativeShell = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        config.userContentController.add(context.coordinator, name: "walkie")
        config.setURLSchemeHandler(WalkiewareBundle(), forURLScheme: "walkieware")
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
        config.userContentController.addUserScript(WKUserScript(source: WalkiewarePreview.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
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
final class WalkiewareSession: NSObject, ObservableObject, WKScriptMessageHandler, WKNavigationDelegate {
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

    func command(_ action: String, version: Int? = nil, text: String? = nil) {
        guard ["checkout", "newPiece", "stop", "signIn", "ask", "retry", "presentVersion", "endPresentation"].contains(action) else { return }
        if action == "newPiece" {
            guard engineReady, !snapshot.busy, capturePhase == .idle else { return }
            previewSource = ""; engineReady = false
        }
        var value: [String: Any] = ["action": action]
        if let version { value["version"] = version }
        if action == "ask" {
            guard engineReady, !snapshot.busy, capturePhase == .idle, let text, !text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty, text.count <= 96 else { return }
            value["text"] = text
        }
        Task { _ = try? await webView?.callAsyncJavaScript("window.walkiewareNativeCommand?.(command)", arguments: ["command": value], in: nil, contentWorld: .page) }
    }
    func beginHold() {
        guard engineReady, !snapshot.busy, capturePhase == .idle else { return }
        captureError = nil; transcript = ""; capturePhase = .opening
        webView?.evaluateJavaScript("voiceStart()")
    }
    func endHold() { webView?.evaluateJavaScript("voiceEnd()") }
    func cancelHold() { webView?.evaluateJavaScript("voiceEnd(true)"); cancel() }

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
    private let engine = AVAudioEngine()
    private let recognizer = SFSpeechRecognizer(locale: Locale(identifier: "en-US"))
    let account = WalkiewareAccount()
    private var previewFrame: WKFrameInfo?
    private var previewSource = ""
    private var previewThreadID = UUID().uuidString
    private var request: SFSpeechAudioBufferRecognitionRequest?
    private var task: SFSpeechRecognitionTask?
    private var turn = ""
    private var held = false
    private var tapped = false
    private var latest = ""
    private var musicalInput = MusicalInput()
    private var wordSegments: [[String: Any]] = []
    private var delivering = false
    private var recognitionFinal = false
    private var audioEnded = false
    private var replayTask: Task<Void, Never>?
    private var finishing: Task<Void, Never>?
    private var deadline: Task<Void, Never>?

    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if !message.frameInfo.isMainFrame,
           message.frameInfo.request.url?.host == "aesthetic.computer",
           message.frameInfo.request.url?.scheme == "https",
           let body = message.body as? [String: Any] {
            if body["action"] as? String == "previewReady" {
                #if DEBUG
                print("[walkieware] preview runtime ready")
                #endif
                previewFrame = message.frameInfo
                renderPreview()
                emitEngine(["kind": "previewReady"])
            } else if body["action"] as? String == "previewEvent" {
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
        guard message.frameInfo.isMainFrame,
              message.frameInfo.request.url?.scheme == "walkieware",
              message.frameInfo.request.url?.host == "app",
              let body = message.body as? [String: Any],
              let action = body["action"] as? String,
              let id = body["id"] as? String, id.count <= 100 else { return }
        switch action {
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
            }
            #endif
            snapshot = next; engineReady = true
        case "voiceIdle":
            capturePhase = .idle; captureStarted = nil

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
        case "start": start(id)
        case "stop": if id == turn { stop() }
        case "cancel": if id == turn { cancel() }
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
        Task { _ = try? await webView?.callAsyncJavaScript("window.walkiewareRender?.(source, threadID);", arguments: ["source": source, "threadID": threadID], in: frame, contentWorld: .page) }
    }

    func emitEngine(_ event: [String: Any]) {
        guard let data = try? JSONSerialization.data(withJSONObject: event),
              let json = String(data: data, encoding: .utf8) else { return }
        webView?.evaluateJavaScript("window.walkiewareEngineEvent?.(\(json))", completionHandler: nil)
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
        let event = ["kind": kind, "text": text, "id": id ?? turn]
        guard let data = try? JSONSerialization.data(withJSONObject: event),
              let json = String(data: data, encoding: .utf8) else { return }
        webView?.evaluateJavaScript("window.walkieNativeEvent?.(\(json))") { _, error in
            if kind == "final", AudioBenchmark.enabled {
                AudioBenchmark.mark("finalDelivered", fields: ["javascriptError": error?.localizedDescription ?? ""])
            }
        }
    }

    private func start(_ id: String) {
        cancel()
        capturePhase = .opening; captureError = nil; transcript = ""
        microphoneLevels = Array(repeating: 0, count: 28)
        turn = id; held = true; latest = ""; wordSegments = []; delivering = false; recognitionFinal = false; audioEnded = false
        #if DEBUG
        if NativeScreenFixture.enabled && NativeScreenFixture.mode == "gestures" { emit("listening"); return }
        #endif
        musicalInput = MusicalInput()
        musicalInput.onUpdate = { [weak self] pitch, rms in
            Task { @MainActor in
                guard let self, self.turn == id, self.held else { return }
                self.microphoneLevels = Array(self.microphoneLevels.dropFirst()) + [rms.isFinite ? max(0, rms) : 0]
                if let pitch, rms > 0.012 { self.emit("sound", text: "\(Int(pitch)) Hz") }
            }
        }
        musicalInput.onObservation = { [weak self] sound in
            Task { @MainActor in
                guard let self, self.turn == id, self.held else { return }
                let value: [String: Any] = ["transcript":self.latest,"words":self.wordSegments,"sound":sound]
                guard let data = try? JSONSerialization.data(withJSONObject:value), let json = String(data:data,encoding:.utf8) else { return }
                self.emit("musicalObservation", text:json)
            }
        }
        if AudioBenchmark.enabled { AudioBenchmark.reset(); AudioBenchmark.mark("holdStarted") }
        Task { [weak self] in
            guard let self else { return }
            let speech = await withCheckedContinuation { continuation in
                SFSpeechRecognizer.requestAuthorization { continuation.resume(returning: $0 == .authorized) }
            }
            guard self.turn == id, self.held else { return }
            guard speech else { self.fail("Speech permission is off. Enable it in Settings."); return }
            let microphone = await AVCaptureDevice.requestAccess(for: .audio)
            guard self.turn == id, self.held else { return }
            guard microphone else { self.fail("Microphone permission is off."); return }
            guard let recognizer = self.recognizer, recognizer.isAvailable,
                  recognizer.supportsOnDeviceRecognition else {
                self.fail("On-device English speech is unavailable."); return
            }
            do {
                let session = AVAudioSession.sharedInstance()
                try session.setCategory(.playAndRecord, mode: .measurement, options: [.defaultToSpeaker, .allowBluetooth])
                try session.setActive(true)
                let request = SFSpeechAudioBufferRecognitionRequest()
                request.shouldReportPartialResults = true
                request.taskHint = .dictation
                request.requiresOnDeviceRecognition = true
                request.contextualStrings = ["Walkieware", "Aesthetic Computer", "garden", "flowers"]
                self.request = request
                if !AudioBenchmark.enabled {
                let input = self.engine.inputNode
                let format = input.outputFormat(forBus: 0)
                guard format.sampleRate > 0, format.channelCount > 0 else {
                    self.fail("No microphone is available."); return
                }
                let sound = self.musicalInput
                input.installTap(onBus: 0, bufferSize: 1024, format: format) { buffer, _ in request.append(buffer); sound.feed(buffer) }
                self.tapped = true
                self.engine.prepare()
                try self.engine.start()
                }
                self.emit("listening")
                self.task = recognizer.recognitionTask(with: request) { [weak self] result, error in
                    let text = result?.bestTranscription.formattedString
                    let segments = result?.bestTranscription.segments.map { ["text": $0.substring, "atMs": $0.timestamp * 1000, "durationMs": $0.duration * 1000, "confidence": Double($0.confidence)] as [String: Any] }
                    let isFinal = result?.isFinal ?? false
                    let message = error?.localizedDescription
                    Task { @MainActor in
                        guard let self, self.turn == id else { return }
                        if let segments { self.wordSegments = Array(segments.prefix(256)) }
                        if let text { self.latest = text; self.emit("partial", text: text) }
                        if isFinal { self.recognitionFinal = true; AudioBenchmark.mark("recognitionFinal") }
                        if self.audioEnded && isFinal { self.deliver() }
                        else if self.audioEnded && message != nil && self.latest.isEmpty {
                            self.deliver() // Sound remains useful when speech finds no words.
                        }
                    }
                }
                if AudioBenchmark.enabled {
                    let sound = self.musicalInput
                    self.replayTask = Task { [weak self] in
                        do {
                            try await AudioBenchmark.replay(into: request, sound: sound) {
                                self?.webView?.evaluateJavaScript("voiceEnd()", completionHandler: nil)
                            }
                        } catch { if !Task.isCancelled { self?.fail("Audio fixture replay failed.") } }
                    }
                }
                self.deadline = Task { [weak self] in
                    try? await Task.sleep(for: .seconds(8))
                    guard !Task.isCancelled, let self, self.turn == id else { return }
                    self.stop()
                }
            } catch { self.fail("Could not start the microphone. Please try again.") }
        }
    }

    private func stopAudio() {
        engine.stop()
        if tapped { engine.inputNode.removeTap(onBus: 0); tapped = false }
        request?.endAudio()
        try? AVAudioSession.sharedInstance().setActive(false, options: .notifyOthersOnDeactivation)
    }

    private func stop() {
        guard !turn.isEmpty, held else { return }
        held = false
        AudioBenchmark.mark("releaseReceived")
        guard request != nil else { cancel(); return }
        // Capture a short release tail, within the eight-second recording budget.
        let elapsed = captureStarted.map { Date().timeIntervalSince($0) } ?? 8
        let tail = min(0.25, max(0, 8 - elapsed))
        emit("processing")
        finishing?.cancel()
        let id = turn
        finishing = Task { [weak self] in
            try? await Task.sleep(for: .seconds(tail))
            guard !Task.isCancelled, let self, self.turn == id else { return }
            self.stopAudio()
            self.audioEnded = true
            AudioBenchmark.mark("audioDrained")
            if self.recognitionFinal { self.deliver(); return }
            // endAudio lets recognition consume every queued buffer and finalize.
            try? await Task.sleep(for: .seconds(3))
            guard !Task.isCancelled, self.turn == id, !self.delivering else { return }
            if self.latest.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
                self.deliver() // Preserve nonverbal musical input.
            } else {
                self.fail("Speech didn’t finish transcribing. Please try again.")
            }
        }
    }

    private func deliver() {
        guard !delivering, !turn.isEmpty else { return }
        delivering = true
        let text = latest.trimmingCharacters(in: .whitespacesAndNewlines)
        let id = turn, words = wordSegments
        stopAudio()
        musicalInput.finish { [weak self] sound in
            Task { @MainActor in
                guard let self, self.turn == id else { return }
                guard !text.isEmpty || (sound["audibleMs"] as? Double ?? 0) >= 150 else {
                    self.fail("I didn’t hear words or a sound. Hold to try again."); return
                }
                let value: [String: Any] = ["transcript":text,"words":words,"sound":sound]
                guard let data = try? JSONSerialization.data(withJSONObject:value), let json = String(data:data,encoding:.utf8) else { self.fail("Could not read sound input."); return }
                AudioBenchmark.checkTranscript(text)
                AudioBenchmark.mark("soundSubmitted", fields: value)
                self.cancel()
                self.emit("mixedFinal", text:json, id:id)
            }
        }
    }

    private func fail(_ message: String) {
        AudioBenchmark.mark("recognitionFailed", fields: ["message": message, "recognizedCharacters": latest.count])
        let id = turn
        cancel()
        emit("error", text: message, id: id)
    }

    func cancel() {
        capturePhase = .idle; captureStarted = nil
        turn = ""; held = false
        replayTask?.cancel(); replayTask = nil
        finishing?.cancel(); finishing = nil
        deadline?.cancel(); deadline = nil
        stopAudio()
        task?.cancel(); task = nil; request = nil
    }
}
