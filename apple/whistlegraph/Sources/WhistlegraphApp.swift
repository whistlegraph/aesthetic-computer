import SwiftUI
import WebKit
import Speech
import AVFoundation
import CoreText

@main
struct WhistlegraphApp: App {
    @StateObject private var voice = WhistlegraphSession()
    @UIApplicationDelegateAdaptor(WhistlegraphAppDelegate.self) private var delegate
    @Environment(\.scenePhase) private var phase
    @AppStorage("whistlegraph-appearance") private var appearance = "system"
    init() {
        let defaults = UserDefaults.standard
        if defaults.object(forKey: "whistlegraph-appearance") == nil,
           let appearance = defaults.string(forKey: "walkieware-appearance") {
            defaults.set(appearance, forKey: "whistlegraph-appearance")
        }
        DeviceActionLog.shared.record(.launch)
        if let root = Bundle.main.url(forResource: "Web", withExtension: nil) {
            for name in ["ComicRelief-Regular.ttf", "ComicRelief-Bold.ttf"] { CTFontManagerRegisterFontsForURL(root.appendingPathComponent(name) as CFURL, .process, nil) }
        }
    }
    var body: some Scene {
        WindowGroup {
            ZStack {
                Color(uiColor: .systemBackground).ignoresSafeArea()
                WhistlegraphScreen(session: voice)
                    .opacity(voice.accountReady ? 1 : 0)
                    .allowsHitTesting(voice.accountReady)
                    .accessibilityElement(children: voice.accountReady ? .contain : .ignore)
                    .accessibilityHidden(!voice.accountReady)
                if !voice.accountReady { WhistlegraphAccountEntry(session: voice) }
                #if DEBUG
                if NativeScreenFixture.mode == "audio" {
                    Text(voice.audioTestResult).accessibilityIdentifier("audio-autoplay-result")
                }
                if voice.isConsentFixture {
                    Text("Requests: \(voice.consentFixtureRequests)")
                        .accessibilityIdentifier("consent-fixture-requests")
                }
                #endif
                if voice.accountReady && !voice.workspaceReady {
                    VStack(spacing: 20) {
                        Text("whistlegraph").font(.largeTitle.bold())
                        if let failure = voice.startupFailure {
                            Text(failure).multilineTextAlignment(.center)
                            Button("Reload") { voice.reloadWorkspace() }.buttonStyle(.borderedProminent)
                        } else { ProgressView("Opening…") }
                    }.padding(30).foregroundStyle(.primary)
                }
            }
            .background(ActionTouchProbe().frame(width: 0, height: 0).allowsHitTesting(false))
            .task { if !voice.isConsentFixture && !voice.accountEntryTest { await voice.braincells.start(session: voice) } }
            .onChange(of: voice.snapshot.handle) { _, _ in Task {
                await voice.syncAIAccount()
                if !voice.isConsentFixture && !voice.accountEntryTest { await voice.braincells.accountChanged() }
            } }
            .preferredColorScheme(appearance == "light" ? .light : appearance == "dark" ? .dark : nil)
            .onChange(of: voice.snapshot.versions.count) { old, new in
                // Version 0 is the starting piece; the first saved one is a good moment to ask.
                if new > old && new > 1 && !voice.isConsentFixture && !voice.accountEntryTest {
                    Task { await WhistlegraphNotifications.askAfterFirstPiece(account: voice.account) }
                }
            }
            .onChange(of: phase) { _, value in
                DeviceActionLog.shared.record(.lifecycle, value == .active ? .active : value == .background ? .background : .inactive)
                if value == .background { voice.cancelHold() }
                if value == .active && voice.capturePhase == .idle { voice.resumePieceAudio() }
                if value == .active && !voice.isConsentFixture && !voice.accountEntryTest {
                    delegate.session = voice
                    DeviceRegistry.report(.open, account: voice.account)
                    Task { await WhistlegraphNotifications.refresh(account: voice.account) }
                }
                if value == .active && !voice.isConsentFixture && !voice.accountEntryTest {
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
        #if DEBUG
        if voice.accountEntryTest { config.websiteDataStore = .nonPersistent() }
        #endif
        config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphNativeShell = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        if let data = try? JSONSerialization.data(withJSONObject: voice.aiConsent.bridge), let json = String(data: data, encoding: .utf8) {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphAIConsent = \(json);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
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
        #if DEBUG
        if NativeScreenFixture.mode == "audio" { config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphAudioTest = true;", injectionTime: .atDocumentStart, forMainFrameOnly: false)) }
        #endif
        config.userContentController.addUserScript(WKUserScript(source: StoryTape.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
        config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphPixelSize = \(voice.pixelSize);", injectionTime: .atDocumentStart, forMainFrameOnly: false))
        config.userContentController.addUserScript(WKUserScript(source: WhistlegraphPreview.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
        #if DEBUG
        if NativeScreenFixture.enabled { config.userContentController.addUserScript(WKUserScript(source: NativeScreenFixture.script, injectionTime: .atDocumentStart, forMainFrameOnly: true)) }
        if let words = ProcessInfo.processInfo.environment["WHISTLEGRAPH_AUTO_ASK"], !words.isEmpty,
           let data = try? JSONSerialization.data(withJSONObject: [words]), let json = String(data: data, encoding: .utf8) {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphAutoAsk = \(json)[0];", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_RETRY_ON_LAUNCH"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphRetryOnLaunch = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
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
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_VISUAL_CAPTURE_TEST"] == "1" {
            config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphVisualCaptureTest = true;", injectionTime: .atDocumentStart, forMainFrameOnly: true))
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
        config.mediaTypesRequiringUserActionForPlayback = []
        PieceAudio.activate()
        let view = WKWebView(frame: .zero, configuration: config)
        #if DEBUG
        view.isInspectable = true // Safari › Develop › <phone> shows the engine console on debug installs.
        #endif
        view.allowsLinkPreview = false
        view.isOpaque = false
        view.backgroundColor = .clear
        view.scrollView.backgroundColor = .clear
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        voice.webView = view
        view.navigationDelegate = context.coordinator
        DispatchQueue.main.async { [weak view] in
            guard let view, voice.webView === view else { return }
            voice.reloadWorkspace()
        }
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
    @Published var speechNotice: String?
    @Published private(set) var verifyingAIAccount = false
    @Published var actionError: String?
    @Published var typedDraft = ""
    @Published private(set) var localDataRevision = 0
    let aiConsent = AIConsent.shared
    @Published var accountStatus: AccountEntryStatus = .checking {
        didSet { if accountStatus == .ready && oldValue != .ready { DeviceRegistry.report(.login, account: account) } }
    }
    @Published var accountNotice = ""
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
    private let consentFixtureIdentity = VerifiedAccountIdentity { request in
        let fails = ProcessInfo.processInfo.environment["WALKIE_IDENTITY_FAILURE"] == "1"
        return (Data(#"{"sub":"fixture:ai-consent"}"#.utf8),
            HTTPURLResponse(url: request.url!, statusCode: fails ? 503 : 200, httpVersion: nil, headerFields: nil)!)
    }
    #endif
    var accountEntryTest: Bool {
        #if DEBUG
        return ProcessInfo.processInfo.environment["WHISTLEGRAPH_ACCOUNT_ENTRY_TEST"] == "1"
        #else
        return false
        #endif
    }
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
        DeviceActionLog.shared.record(.setting, .requested, control: .density, [.pixelSize: size])
        guard (1...4).contains(size), !snapshot.busy, capturePhase == .idle else { return }
        pixelSize = size
        UserDefaults.standard.set(size, forKey: WhistlegraphPreview.pixelSizeKey)
        guard let frame = previewFrame else { return }
        Task { _ = try? await webView?.callAsyncJavaScript("window.whistlegraphSetPixelSize?.(size);", arguments: ["size": size], in: frame, contentWorld: .page) }
    }

    func setPreviewFormat(_ format: PreviewFormat) {
        DeviceActionLog.shared.record(.setting, .requested, control: .format, [.format: PreviewFormat.allCases.firstIndex(of: format) ?? -1])
        guard !snapshot.busy, capturePhase == .idle else { return }
        previewFormat = format
        UserDefaults.standard.set(format.rawValue, forKey: PreviewFormat.preference)
        // WKWebView resizes the live framebuffer. Keep the piece, its state,
        // and the normalized chalk strokes instead of reloading the runtime.
    }

    func command(_ action: String, version: Int? = nil, text: String? = nil, piece: String? = nil, ware: String? = nil, onAccepted: (() -> Void)? = nil) {
        guard ["checkout", "newPiece", "openPiece", "deletePiece", "keepDraft", "discardDraft", "discardAttempt", "stop", "signIn", "ask", "retry", "presentVersion", "endPresentation", "setModel", "refreshBraincells", "setWare", "playRoblox", "exportRoom", "undoRoom"].contains(action) else { return }
        if ["setWare", "playRoblox", "exportRoom", "undoRoom"].contains(action) {
            guard engineReady, !snapshot.busy, capturePhase == .idle else { return }
        }
        let control = DeviceActionLog.Control(rawValue: action)
        DeviceActionLog.shared.record(.command, .requested, control: control,
            [.characters: text?.count ?? 0, .version: version ?? snapshot.head, .busy: snapshot.busy ? 1 : 0, .engineReady: engineReady ? 1 : 0])
        if ["ask", "retry"].contains(action), !aiConsent.creation {
            requestAIConsent { [weak self] in self?.command(action, version: version, text: text, piece: piece, onAccepted: onAccepted) }
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
        if action == "setWare" {
            guard let ware, Ware(rawValue: ware) != nil else { return }
            value["ware"] = ware
        }
        if action == "setModel" {
            guard !snapshot.busy, capturePhase == .idle, let text else { return }
            DeviceActionLog.shared.record(.setting, .requested, control: .setModel, [.selection: snapshot.inference?.models.firstIndex(where: { $0.id == text }) ?? -1])
            value["text"] = text
        }
        if let version { value["version"] = version }
        if ["openPiece", "deletePiece"].contains(action), let piece { value["piece"] = piece }
        if action == "ask" {
            guard canStartAIAction() else { return }
            guard let text, (!text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty || drawing.hasInk) else {
                reportActionFailure("Type a few words or add chalk before sending.", reason: .emptyInput); return
            }
            guard text.count <= 96 else { reportActionFailure("Keep your request to 96 characters or fewer.", reason: .inputTooLong); return }
            value["text"] = text
            if let sketch = drawing.payload() { value["drawing"] = sketch }
        }
        #if DEBUG
        if isConsentFixture && ["ask", "retry"].contains(action) {
            consentFixtureRequests += 1; onAccepted?(); return // Exercise the native gate without paid inference.
        }
        #endif
        let commandGeneration = account.generation
        Task {
            do {
                if ["ask", "retry"].contains(action), commandGeneration != account.generation {
                    reportActionFailure("Your account changed. Try that action again.", reason: .accountChanged); return
                }
                guard let webView else { throw NativeSignIn.failure("The workspace is still opening. Try again in a moment.") }
                let result = try await webView.callAsyncJavaScript("if (!window.whistlegraphNativeCommand) throw Error('Workspace not ready'); return window.whistlegraphNativeCommand(command);", arguments: ["command": value], in: nil, contentWorld: .page)
                if let result = result as? [String: Any], result["accepted"] as? Bool == false {
                    let reason = (result["reason"] as? String).flatMap(DeviceActionLog.Outcome.init(rawValue:)) ?? .failed
                    let message: String
                    switch reason {
                    case .busy: message = "A piece is still being made. Wait or tap Stop, then send again."
                    case .authentication: message = "Your account is still connecting. Your draft is here; try sending again in a moment."
                    case .permission: message = "AI permission has not reached the workspace. Your draft is here; try sending again."
                    case .storageFull: message = "This phone's piece storage is full, so the open piece could not be put away. Delete an old piece in Your Pieces, then try again."
                    default: message = "The workspace is not ready for that request. Your draft is still here. Try again."
                    }
                    reportActionFailure(message, reason: reason); return
                }
                DeviceActionLog.shared.record(.commandDelivery, .succeeded, control: control)
                // The chalk went with the request: it dissipates the moment the send is taken.
                if action == "ask", value["drawing"] != nil { drawing.dissipate() }
                onAccepted?()
            } catch {
                DeviceActionLog.shared.recordError(.commandDelivery, error)
                if ["ask", "retry", "signIn", "newPiece", "openPiece", "deletePiece"].contains(action) {
                    reportActionFailure("The workspace could not receive that action. Your draft is still here. Try again.", reason: .notReady)
                }
            }
        }
    }
    private func canStartAIAction() -> Bool {
        if !engineReady { reportActionFailure("The workspace is still opening. Try again in a moment.", reason: .notReady); return false }
        if snapshot.busy { reportActionFailure("A piece is still being made. Wait for it to finish or tap Stop.", reason: .busy); return false }
        if capturePhase != .idle { reportActionFailure("Finish or cancel the current recording first.", reason: .captureActive); return false }
        return true
    }
    func reportActionFailure(_ message: String, reason: DeviceActionLog.Outcome) {
        DeviceActionLog.shared.record(.command, reason)
        actionError = message
    }
    func beginHold() {
        DeviceActionLog.shared.record(.talkBegin, .requested)
        guard canStartAIAction() else { return }
        guard !snapshot.handle.isEmpty else { command("signIn"); return }
        // Permission does not start a microphone after the finger has lifted.
        guard aiConsent.creation else { requestAIConsent(); return }
        if isConsentFixture { return }
        speechNotice = nil
        performanceTurn = false
        captureError = nil; transcript = ""; speechStartedAt = nil; capturePhase = .opening
        webView?.evaluateJavaScript("voiceStart()") { [weak self] _, error in
            guard let error else { return }
            DeviceActionLog.shared.recordError(.talkBegin, error)
            self?.cancel()
            self?.reportActionFailure("Recording could not start. Try holding Talk again.", reason: .failed)
        }
    }
    func latchPerformance() {
        DeviceActionLog.shared.record(.talkLatch, .requested)
        guard capturePhase == .opening || capturePhase == .recording else { return }
        performanceTurn = true; performanceCapture = true; drawing.enabled = true; capture.latchPerformance()
        webView?.evaluateJavaScript("window.whistlegraphLatchPerformance?.()")
    }
    func endHold() { DeviceActionLog.shared.record(.talkEnd, .requested); webView?.evaluateJavaScript("voiceEnd()") }
    @discardableResult func syncAIAccount(reportFailure: Bool = false) async -> Bool {
        let expected = account.generation
        do {
            let subject: String?
            #if DEBUG
            if isConsentFixture { subject = try await consentFixtureIdentity.subject(token: "fixture-opaque-token", generation: expected) }
            else { subject = try await account.subject() }
            #else
            subject = try await account.subject()
            #endif
            guard expected == account.generation else { return false }
            aiConsent.bind(subject: subject, handle: snapshot.handle)
            // Logging in is the permission; only the switch in AI & privacy turns it off.
            if aiConsent.signedIn && !aiConsent.decided && !accountEntryTest { aiConsent.set(allowed: true) }
            if subject == nil && reportFailure { reportActionFailure("Your sign-in has expired. Sign out from Account, then log in again. Your draft is still here.", reason: .notSignedIn) }
            return aiConsent.signedIn
        } catch {
            guard expected == account.generation else { return false }
            DeviceActionLog.shared.recordError(.accountIdentity, error)
            aiConsent.bind(subject: nil, handle: snapshot.handle)
            if reportFailure {
                let expired: Bool
                if case VerifiedAccountIdentity.Failure.http(401) = error { expired = true } else { expired = false }
                reportActionFailure(expired ? "Your sign-in has expired. Sign out from Account, then log in again. Your draft is still here." : "Could not verify your account. Check your connection and try again. Your draft is still here.", reason: expired ? .authentication : .networkError)
            }
            return false
        }
    }
    /// Logging in is the permission. Off here means the switch in AI & privacy.
    func requestAIConsent(resume: (() -> Void)? = nil) {
        DeviceActionLog.shared.record(.consent, .requested)
        if aiConsent.creation { resume?(); return }
        guard !snapshot.handle.isEmpty else { command("signIn"); return }
        guard !verifyingAIAccount else { return }
        verifyingAIAccount = true
        let generation = account.generation, handle = snapshot.handle
        Task {
            defer { verifyingAIAccount = false }
            guard await syncAIAccount(reportFailure: true) else { return }
            guard generation == account.generation, handle == snapshot.handle else {
                reportActionFailure("Your account or piece changed. Try that action again.", reason: .accountChanged); return
            }
            guard aiConsent.creation else {
                DeviceActionLog.shared.record(.consent, .declined)
                reportActionFailure("Create with AI is switched off for this account. Turn it on in Brain → AI & privacy.", reason: .permission); return
            }
            // Await the WebView gate before continuing the original action.
            guard let webView else { reportActionFailure("The workspace is not ready. Try again.", reason: .notReady); return }
            do {
                _ = try await webView.callAsyncJavaScript("window.__whistlegraphAIConsent = value; window.whistlegraphSetAIConsent?.(value);",
                    arguments: ["value": aiConsent.bridge], in: nil, contentWorld: .page)
            } catch {
                DeviceActionLog.shared.recordError(.consentBridge, error)
                reportActionFailure("Could not apply AI permission. Your draft is still here. Try again.", reason: .failed); return
            }
            guard generation == account.generation, handle == snapshot.handle, aiConsent.creation else { return }
            resume?()
        }
    }
    private func applyAIConsent() {
        if !aiConsent.allowed { command("stop"); cancelHold(); StoryVoice.cancelCloudRequests() }
        let value = aiConsent.bridge
        Task { _ = try? await webView?.callAsyncJavaScript("window.__whistlegraphAIConsent = value; window.whistlegraphSetAIConsent?.(value);", arguments: ["value": value], in: nil, contentWorld: .page) }
    }
    func eraseDeletedAccountLocally() async throws {
        command("stop"); cancelHold(); storyPreview.stop(); tv.disconnect()
        localDataRevision += 1
        aiConsent.forget(); StoryVoice.erasePendingWork()
        account.signOut()
        _ = try? await webView?.callAsyncJavaScript("window.whistlegraphForgetLocalData?.();", arguments: [:], in: nil, contentWorld: .page)
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
        typedDraft = ""
        snapshot = PieceSnapshot(); pieces = []; previewSource = ""; engineReady = false
        reloadWorkspace()
        try DeviceActionLog.shared.clear()
        if let cleanupError { throw cleanupError }
    }
    /// A notification's `url` is "/<piece>"; open it if it is one of this account's pieces.
    func openFromNotification(_ url: String) {
        let piece = String(url.drop(while: { $0 == "/" }).prefix(64))
        DeviceActionLog.shared.record(.notifications, .presented)
        // The push names the piece by code. One not on this phone is fetched from the knot.
        guard !piece.isEmpty else { return }
        if pieces.contains(where: { ($0.id == piece || $0.code == piece) && $0.current }) { return }
        command("openPiece", piece: piece)
    }
    /// Drops the Keychain sign-in and tells the engine, which parks its sockets.
    func signOut() {
        guard capturePhase == .idle else { return }
        account.signOut()
        DeviceRegistry.report(.logout, account: nil)
        accountStatus = .signedOut; accountNotice = ""
        aiConsent.bind(subject: nil, handle: "")
        emitEngine(["kind": "account", "token": ""])
    }
    func cancelHold() { DeviceActionLog.shared.record(.talkCancel, .requested); performanceCapture = false; webView?.evaluateJavaScript("voiceEnd(true)"); cancel() }

    @Published var startupFailure: String?
    private var startupTimeout: Task<Void, Never>?
    private var recoveryCount = 0
    weak var webView: WKWebView?

    func reloadWorkspace() {
        DeviceActionLog.shared.record(.workspace, .opening)
        workspaceReady = false; engineReady = false; startupFailure = nil
        startupTimeout?.cancel()
        webView?.load(URLRequest(url: URL(string: "\(WhistlegraphBundle.storageScheme)://app/index.html?whistlegraph=1")!))
        startupTimeout = Task { [weak self] in
            try? await Task.sleep(for: .seconds(12))
            guard !Task.isCancelled, let self, !(self.workspaceReady && self.engineReady) else { return }
            self.startupFailure = "The workspace did not finish opening. Tap Reload to try again."
            DeviceActionLog.shared.record(.workspace, .failed)
        }
    }
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        print("[whistlegraph] main page loaded")
        webView.evaluateJavaScript("({controls:!!document.getElementById('speak'),engine:typeof window.whistlegraphAsk,ready:document.readyState})") { value, error in
            print("[whistlegraph] startup check: \(value ?? error?.localizedDescription ?? "no result")")
        }
    }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        DeviceActionLog.shared.recordError(.workspace, error)
        startupFailure = "Could not open the workspace: " + error.localizedDescription
        print("[whistlegraph] navigation failed: \((error as NSError).code)")
    }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        didFailProvisionalNavigation(webView, navigation: navigation, error: error)
    }
    private func didFailProvisionalNavigation(_ view: WKWebView, navigation: WKNavigation!, error: Error) {
        DeviceActionLog.shared.recordError(.workspace, error)
        startupFailure = "Could not open the workspace: " + error.localizedDescription
    }
    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
        DeviceActionLog.shared.record(.workspace, .failed)
        cancel(); workspaceReady = false
        print("[whistlegraph] web content process terminated")
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
        capture.cloudSpeechEnabled = { [weak self] in self?.aiConsent.cloudSpeech == true }
        capture.onSpeechNotice = { [weak self] text in self?.speechNotice = text }
        capture.onSpeechCharge = { [weak self] in self?.command("refreshBraincells") }
        capture.speechToken = { [weak self] in
            guard let self, self.aiConsent.cloudSpeech else { return nil }
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
            if body["action"] as? String == "frameHello" {
                DeviceActionLog.shared.record(.preview, .started, [.enabled: (body["top"] as? Bool == true) ? 1 : 0, .status: (body["ac"] as? Bool == true) ? 1 : 0]); return
            }
            if body["action"] as? String == "previewScriptError" {
                DeviceActionLog.shared.record(.preview, .failed, [.characters: (body["message"] as? String ?? "").count])
                #if DEBUG
                print("[whistlegraph] preview script error:", body["message"] ?? "")
                #endif
                return
            }
            if body["action"] as? String == "previewWaiting" {
                DeviceActionLog.shared.record(.preview, .pending, [.enabled: (body["preloaded"] as? Bool == true) ? 1 : 0, .status: (body["acSEND"] as? Bool == true) ? 1 : 0, .durationMs: (body["ticks"] as? Int ?? 0) * 100]); return
            }
            if body["action"] as? String == "previewReady" {
                DeviceActionLog.shared.record(.preview, .ready)
                #if DEBUG
                print("[whistlegraph] preview runtime ready")
                #endif
                previewFrame = message.frameInfo
                renderPreview()
                emitEngine(["kind": "previewReady"])
            } else if body["action"] as? String == "previewEvent" {
                if let event = body["event"] as? [String: Any],
                   event["requestID"] as? Int == previewRequestID,
                   event["sourceHash"] as? String == VisualCapture.hash(previewSource) {
                    if event["kind"] as? String == "painted" { DeviceActionLog.shared.record(.preview, .painted); paintedPreviewHash = event["sourceHash"] as? String; tv.update(previewSource) }
                    if event["kind"] as? String == "invalidated" { DeviceActionLog.shared.record(.preview, .invalidated); paintedPreviewHash = nil }
                }
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
        guard PreviewNavigation.bridge(message.frameInfo.request.url, mainFrame: message.frameInfo.isMainFrame, document: .workspace) == .workspace,
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
            print("[whistlegraph] native layout spacing=\(layout.spacing) title=\(layout.titleSize)")
        case "snapshot":
            guard var value = body["snapshot"] as? [String: Any] else { return }
            #if DEBUG
            if NativeScreenFixture.enabled,
               let state = ProcessInfo.processInfo.environment["WHISTLEGRAPH_BRAINCELLS_FIXTURE"],
               var inference = value["inference"] as? [String: Any] {
                inference["braincells"] = ["remaining": state == "empty" ? 0 : 75000,
                    "used": state == "empty" ? 100000 : 25000, "limit": 100000,
                    "purchased": state == "empty" ? 0 : 1000000, "unlimited": state == "unlimited",
                    "resetsAt": ISO8601DateFormatter().string(from: Date().addingTimeInterval(3600))] as [String: Any]
                inference["braincellsError"] = ""
                value["inference"] = inference
            }
            #endif
            guard let data = try? JSONSerialization.data(withJSONObject: value), data.count < 2_000_000,
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
            traceSnapshot(next)
            snapshot = next; tv.updateProgress(next); engineReady = true
            if workspaceReady { startupTimeout?.cancel() }
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
                print("[whistlegraph] \(code) · \(status)")
            }
        case "sequenceCapture":
            #if DEBUG
            if SequenceBenchmark.enabled, let report = body["report"] as? [String: Any], let rect = body["rect"] as? [String: Double], let webView {
                Task { await SequenceBenchmark.capture(report, rect: rect, view: webView) }
            }
            #endif
        case "benchmark":
            if let name = body["event"] as? String, let stage = DeviceActionLog.Stage(rawValue: name) {
                let status = (body["fields"] as? [String: Any])?["status"] as? Int
                DeviceActionLog.shared.record(.inference, nil, status.map { [.status: $0] } ?? [:], stage: stage)
            }
            if let name = body["event"] as? String, ["requestDispatched", "firstModelOutput", "firstCheckpoint", "generationFinished", "generationFailed", "transcriptPainted", "signedIn", "generationEntered", "guidesReady", "firstIncrementalCompile", "inferenceHeaders", "jevDecision", "jevFallback", "jevCacheHit", "jevApplied", "inputSocketReady", "inputSocketAck", "inputHttpFallback", "starterDispatched", "starterPainted", "refinementFailed", "localEditDispatched", "localEditPainted"].contains(name) { AudioBenchmark.mark(name, fields: body["fields"] as? [String: Any] ?? [:]) }
        case "startupError":
            startupTimeout?.cancel()
            startupFailure = "Could not load the workspace: " + String((body["text"] as? String ?? "Unknown error").prefix(300))
            DeviceActionLog.shared.record(.workspace, .failed)
            #if DEBUG
            print("[whistlegraph] startup failure: \(startupFailure ?? "")")
            if let data = startupFailure?.data(using: .utf8) {
                try? data.write(to: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent("whistlegraph-startup-error.txt"), options: .atomic)
            }
            #endif
        case "ready":
            DeviceActionLog.shared.record(.workspace, .ready)
            workspaceReady = true; startupFailure = nil
            if engineReady { startupTimeout?.cancel() }
            print("[whistlegraph] controls ready")
            if AudioBenchmark.enabled || SequenceBenchmark.enabled { UIApplication.shared.isIdleTimerDisabled = true }
            Task { await NetworkBenchmark.run() }
        case "accountState":
            #if DEBUG
            if NativeScreenFixture.enabled { accountStatus = .ready; return }
            #endif
            guard let name = body["status"] as? String, let status = AccountEntryStatus(rawValue: name) else { return }
            accountStatus = status
            accountNotice = String((body["notice"] as? String ?? "").prefix(500))
        case "account":
            if isConsentFixture { emitEngine(["kind": "account", "token": ""]); return }
            #if DEBUG
            if accountEntryTest { emitEngine(["kind": "account", "token": ""]); return }
            #endif
            restoreAccount(force: false)
        case "aiConsent":
            // The network guard can reject background recovery or stale account
            // work. Only native Send/retry/Talk gestures may present permission.
            DeviceActionLog.shared.record(.consentBridge, .denied)
        case "signIn":
            if isConsentFixture { return }
            signIn()

        case "drawingCommitted":
            if let id = body["drawingID"] as? String, let revision = body["revision"] as? Int { drawing.consume(id: id, revision: revision) }
        case "render":
            guard let source = body["source"] as? String, source.utf8.count < 500_000 else { return }
            visualCaptureTask?.cancel(); paintedPreviewHash = nil
            if let id = body["threadID"] as? String, UUID(uuidString: id) != nil { previewThreadID = id }
            previewRequestID = body["renderID"] as? Int ?? 0
            previewSource = source; renderPreview()
        case "start":
            DeviceActionLog.shared.record(.speech, .opening)
            capturePhase = .opening; captureError = nil; transcript = ""
            microphoneLevels = Array(repeating: 0, count: 28)
            capture.start(id)
            if performanceCapture { capture.latchPerformance() }
        case "stop": capture.stop(matching: id)
        case "cancel": if id == capture.turn { cancel() }
        case "share":
            DeviceActionLog.shared.record(.share, .requested)
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

    private func traceSnapshot(_ next: PieceSnapshot) {
        if !engineReady || next.busy != snapshot.busy || next.head != snapshot.head || next.handle != snapshot.handle {
            DeviceActionLog.shared.record(.snapshot, next.busy ? .processing : .ready,
                [.version: next.head, .busy: next.busy ? 1 : 0, .enabled: next.handle.isEmpty ? 0 : 1])
        }
        let failure = next.attempt?.status == "failed" ? next.attempt?.error ?? next.error : next.error
        let oldFailure = snapshot.attempt?.status == "failed" ? snapshot.attempt?.error ?? snapshot.error : snapshot.error
        if !failure.isEmpty && failure != oldFailure {
            DeviceActionLog.shared.record(.inference, DeviceActionLog.failureKind(failure))
        }
        let current = next.inference?.braincells, previous = snapshot.inference?.braincells
        if let current, current.remaining != previous?.remaining || current.purchased != previous?.purchased || current.used != previous?.used {
            func count(_ value: Double) -> Int { value.isFinite ? Int(max(-1e12, min(1e12, value))) : 0 }
            DeviceActionLog.shared.record(.credits, current.remaining + current.purchased <= 0 && current.unlimited != true ? .exhausted : .ready,
                [.remaining: count(current.remaining), .purchased: count(current.purchased), .used: count(current.used), .limit: count(current.limit)])
        }
        if let error = next.inference?.braincellsError, !error.isEmpty, error != snapshot.inference?.braincellsError {
            DeviceActionLog.shared.record(.credits, DeviceActionLog.failureKind(error))
        }
    }

    private func renderPreview() {
        guard let frame = previewFrame, !previewSource.isEmpty else { return }
        let source = previewSource
        let threadID = previewThreadID
        let renderID = previewRequestID
        let size = pixelSize
        Task { _ = try? await webView?.callAsyncJavaScript("window.whistlegraphSetPixelSize?.(size); window.whistlegraphRender?.(source, threadID, renderID);", arguments: ["source": source, "threadID": threadID, "renderID": renderID, "size": size], in: frame, contentWorld: .page) }
    }


    /// A saved version's source, for recording a story card off-screen.
    func versionSource(_ id: Int) async throws -> String {
        guard let webView else { throw URLError(.resourceUnavailable) }
        let value = try await webView.callAsyncJavaScript("return window.whistlegraphVersionSource?.(id) ?? null;", arguments: ["id": id], in: nil, contentWorld: .page)
        guard let source = value as? String, !source.isEmpty else { throw URLError(.resourceUnavailable) }
        return source
    }
    func emitEngine(_ event: [String: Any]) {
        guard let data = try? JSONSerialization.data(withJSONObject: event),
              let json = String(data: data, encoding: .utf8) else { return }
        webView?.evaluateJavaScript("window.whistlegraphEngineEvent?.(\(json))", completionHandler: nil)
    }

    private func emit(_ kind: String, text: String = "", id: String? = nil) {
        let phase: [String: DeviceActionLog.Outcome] = ["listening": .recording, "partial": .partial,
            "processing": .processing, "mixedFinal": .final, "final": .final, "error": .failed, "sound": .sound, "musicalObservation": .sound]
        if let outcome = phase[kind] { DeviceActionLog.shared.record(.speech, kind == "error" ? DeviceActionLog.failureKind(text) : outcome, [.characters: text.count]) }
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
        if ["mixedFinal", "final"].contains(kind), let sketch = drawing.payload(speechStart: speechStartedAt) { event["drawing"] = sketch; drawing.dissipate() }
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
