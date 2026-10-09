import SwiftUI

struct PieceSnapshot: Decodable {
    var ware: String?
    var roblox: RobloxRoomSnapshot?
    var wareID: String { ware ?? "piece" }
    var code = ""
    var caption: String?
    var output: String?
    var inference: InferenceSnapshot?
    var handle = ""
    var colors: [[Double]] = []
    var head = 0
    var hasPiece = false
    var hasPreview = false
    var error = ""
    var busy = false
    var phase = ""
    var attempt: PieceAttempt?
    var revisions: [PieceRevision]? = []
    var versions: [PieceRevision] { get { revisions ?? [] } set { revisions = newValue } }
    enum CodingKeys: String, CodingKey { case ware, roblox, code, caption, output, inference, handle, colors, head, hasPiece, hasPreview, error, busy, phase, attempt; case revisions = "versions" }
    var hasHistory: Bool { versions.count > 1 }
}
struct PieceAttempt: Decodable { let request: String; let status: String; let error: String }
struct PieceRevision: Decodable, Identifiable {
    let id: Int
    let parent: Int?
    let words: [PlaybackWord]?
    let recordingID: String?
    let utterance: String
    let createdAt: String
    let sound: PieceSound?
    var hasDrawing: Bool? = nil
    var date: Date? {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter.date(from: createdAt)
    }
}
struct PieceSound: Decodable {
    let durationMs: Double
    let frames: [SoundFrame]
}
struct SoundFrame: Decodable { let atMs: Double; let rms: Double; let pitchHz: Double? }
enum CapturePhase { case idle, opening, recording, processing }

struct NativeLayout {
    var spacing: CGFloat = 12
    var pageInset: CGFloat = 20
    var historySize: CGFloat = 22
    var talkHeight: CGFloat = 98
    var titleSize: CGFloat = 30
    init(tokens: [String: Double] = [:]) {
        func bounded(_ key: String, _ fallback: Double, _ range: ClosedRange<Double>) -> CGFloat {
            guard let value = tokens[key], value.isFinite else { return CGFloat(fallback) }
            return CGFloat(min(range.upperBound, max(range.lowerBound, value)))
        }
        spacing = bounded("spacing", 12, 4...28); pageInset = bounded("page-inset", 20, 8...32)
        historySize = bounded("history-size", 22, 18...32); talkHeight = bounded("talk-height", 98, 80...160)
        titleSize = bounded("title-size", 30, 22...40)
    }
}

struct WhistlegraphScreen: View {
    @ObservedObject var session: WhistlegraphSession
    @ObservedObject private var drawing: DrawingDraft
    init(session: WhistlegraphSession) { self.session = session; self.drawing = session.drawing }
    @StateObject private var narrator = VersionNarrator()
    @StateObject private var exporter = StoryExport()
    @Environment(\.scenePhase) private var scenePhase
    @State private var held = false
    @State private var chalkDrag: CGFloat = 0
    private var chalkReveal: CGFloat { session.performanceCapture ? 1 : min(1, chalkDrag / 80) }
    @State private var showComposer = false
    @State private var showTV = false
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @Environment(\.colorScheme) private var colorScheme
    @AppStorage("whistlegraph-appearance") private var appearance = "system"
    private var themePhase: WhistlegraphTheme.Phase {
        if session.captureError != nil || !session.snapshot.error.isEmpty { return .error }
        if session.capturePhase == .recording || session.capturePhase == .opening || showComposer { return .listening }
        if session.snapshot.busy && session.snapshot.phase.localizedCaseInsensitiveContains("evaluat") { return .rendering }
        if session.snapshot.busy || session.capturePhase == .processing { return .working }
        return .ready
    }
    private var theme: WhistlegraphTheme { WhistlegraphTheme(phase: themePhase, dark: colorScheme == .dark) }
    // iOS offers the keyboard in light or dark only; match whichever the piece is wearing.
    private func openComposer() {
        UITextField.appearance().keyboardAppearance = colorScheme == .dark ? .dark : .light
        showComposer = true
    }
    private var paper: Color { theme.foreground }
    private var accent: Color { theme.accent }
    private var storyBackground: Color {
        let rgb = StoryCardStyle.background(code: session.snapshot.code, version: session.snapshot.head)
        return Color(red: Double(rgb[0]) / 255, green: Double(rgb[1]) / 255, blue: Double(rgb[2]) / 255)
    }
    private var chalkActive: Bool { drawing.enabled || held || session.capturePhase == .opening || session.capturePhase == .recording }
    private var previewSize: CGSize {
        session.previewFormat.fit(width: UIScreen.main.bounds.width - session.layout.pageInset * 2 - 12,
                                  height: min(380, UIScreen.main.bounds.height * 0.4))
    }
    // With the keyboard and composer up, the full-size preview pushes the text
    // field under the keyboard. Scale the framed preview (not the runtime, which
    // would resize the piece) so the sketch stays visible above the composer.
    private var composeScale: CGFloat {
        guard showComposer, !narrator.isPlaying else { return 1 }
        return min(1, UIScreen.main.bounds.height * 0.2 / (previewSize.height + 12))
    }
    var body: some View {
        VStack(spacing: narrator.isPlaying ? 0 : session.layout.spacing) {
            HStack {
                IdentityHeader(session: session, appearance: $appearance, size: session.layout.historySize) { narrator.stop(); showComposer = false }
                Spacer()
                if session.snapshot.wareID == "piece" {
                Button { showTV = true } label: {
                    SendToTVIcon().frame(width: 44, height: 44)
                }.accessibilityLabel("Send to TV").accessibilityIdentifier("project-tv")
                Button {
                    showComposer = false
                    ButtonSounds.play(.play); openStory()
                } label: {
                    CardFanIcon().frame(width: 44, height: 44)
                }
                .disabled(session.capturePhase != .idle || !session.engineReady || !session.snapshot.versions.contains(where: { $0.id > 0 }))
                .accessibilityLabel("Open story cards").accessibilityIdentifier("play-versions")
                }
            }.frame(height: narrator.isPlaying ? 0 : nil).clipped().accessibilityHidden(narrator.isPlaying)
            VStack(spacing: narrator.isPlaying ? 12 : 4) {
            // This representable never changes identity when cards or versions change.
            GeometryReader { geometry in
            ZStack {
                Workspace(voice: session)
                    .opacity(session.snapshot.hasPreview ? 1 : 0)
                    .allowsHitTesting(session.snapshot.hasPreview && session.capturePhase == .idle)
                    .accessibilityHidden(!session.snapshot.hasPreview || narrator.isPlaying)
                if narrator.isPlaying { StoryWorkspace(player: session.storyPreview) }
                if !session.snapshot.hasPreview && !session.engineReady { ProgressView() }
                if !narrator.isPlaying && (chalkActive || drawing.hasInk) {
                    DrawingPad(draft: drawing, interactive: chalkActive && canTalk)
                        .accessibilityIdentifier("drawing-pad")
                        .background(chalkActive ? Color.black.opacity(0.16) : Color.clear)
                        .allowsHitTesting(chalkActive && canTalk)
                }
                if let failure = session.startupFailure {
                    VStack(spacing: 12) { Text(failure); Button("Reload") { session.reloadWorkspace() } }.padding().background(.black.opacity(0.85))
                }
            }
            .frame(width: geometry.size.width, height: geometry.size.height)
            }
            .frame(width: narrator.isPlaying ? nil : previewSize.width,
                   height: narrator.isPlaying ? max(1, (UIScreen.main.bounds.width - 12) * 3 / 4) : previewSize.height)
            .overlay { WhistlegraphPreviewInset() }
            .padding(6)
            .background(WhistlegraphWoodFrame())
            .scaleEffect(composeScale, anchor: .top)
            .frame(height: composeScale < 1 ? (previewSize.height + 12) * composeScale : nil, alignment: .top)
            .animation(reduceMotion ? nil : .easeInOut(duration: 0.25), value: showComposer)
            .accessibilityIdentifier("story-picture")
            .overlay {
                if narrator.isPlaying && !exporter.requested {
                    HStack {
                        Color.clear.contentShape(Rectangle()).frame(width: 28).onTapGesture { narrator.previous() }
                        Spacer().allowsHitTesting(false)
                        Color.clear.contentShape(Rectangle()).frame(width: 28).onTapGesture { narrator.next() }
                    }.accessibilityHidden(true)
                }
            }
            .frame(maxWidth: .infinity)
            if !narrator.isPlaying {
                HStack(spacing: 18) {
                    if session.performanceCapture {
                        Button { session.cancelHold() } label: { Image(systemName: "xmark") }
                            .accessibilityLabel("Cancel recording, keep drawing")
                    }
                    Button { ButtonSounds.play(.tick); drawing.enabled.toggle() } label: {
                        Label("Chalk", systemImage: drawing.enabled ? "pencil.tip.crop.circle.fill" : "pencil.tip.crop.circle")
                    }.accessibilityIdentifier("draw-control").accessibilityValue(drawing.enabled ? "On" : "Off")
                        .disabled(!canTalk)
                    if drawing.hasInk {
                        Button { drawing.undo() } label: { Image(systemName: "arrow.uturn.backward") }
                            .accessibilityLabel("Undo stroke").accessibilityIdentifier("drawing-undo").disabled(!canTalk)
                        Button { drawing.clear() } label: { Image(systemName: "trash") }
                            .accessibilityLabel("Clear chalk").accessibilityIdentifier("drawing-clear").disabled(!canTalk)
                    }
                    Spacer(minLength: 0)
                    if drawing.hasInk && !showComposer {
                        Button("Send") { ButtonSounds.play(.sent); session.command("ask", text: "") }
                            .accessibilityLabel("Send chalk").accessibilityIdentifier("drawing-send")
                            .disabled(!canTalk || session.capturePhase != .idle)
                    }
                    BrainButton(session: session) { showComposer = false }
                }.font(.title3).buttonStyle(.plain).frame(minHeight: 44)
                if drawing.full { Text("Chalk full · send or undo a stroke").font(.caption) }
                if session.capturePhase == .recording || session.capturePhase == .opening {
                    Text(session.transcript.isEmpty ? "Listening…" : session.transcript)
                        .font(.custom("ComicRelief-Regular", size: 20)).lineLimit(2)
                        .frame(maxWidth: .infinity, alignment: .leading).allowsHitTesting(false)
                }
            }
            if narrator.isPlaying {
                VStack(alignment: .leading, spacing: 6) {
                    ComicTitle(text: "v\(narrator.currentVersion ?? session.snapshot.head)", size: 22)
                        .accessibilityLabel("Running version \(narrator.currentVersion ?? session.snapshot.head)")
                        .accessibilityIdentifier("story-version")
                    PlaybackCaption(text: narrator.utterance, spokenRange: narrator.spokenRange, accent: .yellow)
                        .font(.custom("ComicRelief-Regular", size: 22, relativeTo: .title3))
                        .lineLimit(3).multilineTextAlignment(.leading)
                        .accessibilityIdentifier("spoken-word")
                }.frame(maxWidth: .infinity, minHeight: 82, alignment: .topLeading).foregroundStyle(.white)
                Spacer(minLength: 0)
            }
            }
            .padding(.horizontal, narrator.isPlaying ? 28 : 0)
            .padding(.top, narrator.isPlaying ? 100 : 0)
            .padding(.bottom, narrator.isPlaying ? 150 : 0)
            .frame(maxWidth: .infinity)
            .frame(height: narrator.isPlaying ? UIScreen.main.bounds.width * 16 / 9 : nil)
            .background(narrator.isPlaying ? storyBackground : Color.clear)
            if !narrator.error.isEmpty && !narrator.isPlaying { Text(narrator.error).foregroundStyle(.orange) }
            if let notice = session.speechNotice { Text(notice).font(.body).foregroundStyle(.orange).frame(maxWidth: .infinity, alignment: .leading).accessibilityIdentifier("speech-fallback-notice") }
            if let failure = session.captureError, !narrator.isPlaying { Text(failure).font(.body).foregroundStyle(.orange).frame(maxWidth: .infinity, alignment: .leading) }
            if !session.snapshot.error.isEmpty && !narrator.isPlaying { Text(session.snapshot.error).font(.body).foregroundStyle(.orange).accessibilityIdentifier("workspace-error") }
            if session.verifyingAIAccount { ProgressView("Checking your account…").accessibilityIdentifier("account-verifying") }
            if !narrator.isPlaying && session.snapshot.wareID == "roblox" { RobloxRoomControls(session: session) }
            if !narrator.isPlaying && (session.snapshot.hasPiece || session.snapshot.hasHistory || session.snapshot.busy || session.snapshot.attempt?.status == "failed") {
                VersionFeed(snapshot: session.snapshot, foreground: paper, selectionColor: paper, textSize: session.layout.historySize, disabled: session.snapshot.busy || session.capturePhase != .idle, holdSelection: drawing.hasInk, stop: { session.command("stop") }, retry: { session.command("retry") }) { narrator.select($0, session: session) }
            } else if !narrator.isPlaying { Spacer(minLength: 0) }
        }
        .padding(.horizontal, narrator.isPlaying ? 0 : session.layout.pageInset)
        .foregroundStyle(paper)
        // Tapping anywhere outside the composer closes it and the keyboard; the draft stays.
        .simultaneousGesture(TapGesture().onEnded { if showComposer { showComposer = false } })
        .safeAreaInset(edge: .bottom, spacing: 8) {
            Group {
                if narrator.isPlaying { EmptyView() } else if showComposer {
                    InlineRequestComposer(theme: theme, disabled: session.snapshot.busy || session.verifyingAIAccount, hasDrawing: drawing.hasInk, text: $session.typedDraft, cancel: { ButtonSounds.play(.pop); session.typedDraft = ""; showComposer = false }) { text in
                        session.requestAIConsent {
                            session.command("ask", text: text) { ButtonSounds.play(.sent); session.typedDraft = ""; showComposer = false }
                        }
                    }
                } else {
                    HStack(spacing: 12 * (1 - chalkReveal)) {
                        Button { ButtonSounds.play(.key); openComposer() } label: {
                            TypingButtonLabel()
                                .frame(maxWidth: .infinity, minHeight: session.layout.talkHeight)
                                .foregroundStyle(theme.buttonInk)
                                .background(Color(red: 0.40, green: 0.83, blue: 0.95), in: RoundedRectangle(cornerRadius: 34, style: .continuous))
                                .overlay(RoundedRectangle(cornerRadius: 34, style: .continuous).strokeBorder(paper, lineWidth: 3))
                        }.buttonStyle(.plain).disabled(!canTalk || session.capturePhase != .idle).accessibilityLabel("Type").accessibilityIdentifier("type-control")
                        .frame(width: max(0, (UIScreen.main.bounds.width - session.layout.pageInset * 2 - 12) / 2 * (1 - chalkReveal)))
                        .clipped().opacity(1 - chalkReveal).allowsHitTesting(chalkReveal == 0).accessibilityHidden(chalkReveal > 0.9)
                        talkControl
                    }
                }
            }.padding(.horizontal, session.layout.pageInset).foregroundStyle(paper)
                .background(narrator.isPlaying ? storyBackground : theme.background)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .overlay {
            if narrator.isPlaying {
                StoryControls(narrator: narrator, exporter: exporter, export: exportStory, close: { exporter.cancel(); narrator.stop() })
                    .opacity(exporter.requested ? 0 : 1).allowsHitTesting(!exporter.requested)
                if exporter.requested {
                    VStack { Spacer(); StoryExportProgress(narrator: narrator, exporter: exporter) { exporter.cancel(); narrator.setPaused(true) } }
                }
            }
        }
        .alert("Could not export", isPresented: Binding(get: { !exporter.error.isEmpty }, set: { if !$0 { exporter.error = "" } })) {
            Button("OK") { exporter.error = "" }
        } message: { Text(exporter.error) }
        .statusBarHidden(narrator.isPlaying)
        .alert("Could not continue", isPresented: Binding(get: { session.actionError != nil }, set: { if !$0 { session.actionError = nil } })) {
            Button("OK") { session.actionError = nil }
        } message: { Text(session.actionError ?? "") }
        .sheet(isPresented: $showTV) { WhistlegraphTVSheet(tv: session.tv) }
        .onChange(of: showComposer) { _, open in DeviceActionLog.shared.record(.screen, open ? .presented : .dismissed, control: .type) }
        .onChange(of: showTV) { _, open in DeviceActionLog.shared.record(.screen, open ? .presented : .dismissed, control: .tv) }
        .onChange(of: narrator.isPlaying) { _, playing in if !playing { exporter.cancel() } }
        .onChange(of: session.localDataRevision) { _, _ in exporter.cancel(); narrator.stop(); showComposer = false }
        .onChange(of: session.narratedFrame) { _, _ in narrator.painted(session.narratedVersion) }
        .onChange(of: exporter.movie?.id) { _, value in if value != nil { narrator.setPaused(true) } }
        .onChange(of: scenePhase) { _, value in if value == .background { exporter.cancel(); narrator.stop() } else if value == .inactive { narrator.setPaused(true) } }
        .onChange(of: session.capturePhase) { _, value in if value != .idle { narrator.stop() }; if value == .idle || value == .processing { chalkDrag = 0; held = false } }
        .onDisappear { exporter.cancel(); narrator.stop() }
        .background((narrator.isPlaying ? storyBackground : theme.background).ignoresSafeArea())
        .tint(paper)
        .animation(reduceMotion ? nil : .easeInOut(duration: 0.3), value: themePhase)
    }
    private func openStory() {
        DeviceActionLog.shared.record(.story, .presented)
        narrator.onNarration = { row, audio in await exporter.startCard(row, audio: audio) }
        narrator.onCardComplete = { await exporter.finishCard() }
        narrator.onSkip = { exporter.skipCard() }
        narrator.onPause = { exporter.pause($0) }
        narrator.shouldPlay = { !exporter.requested || exporter.needsCard(at: $0) }
        narrator.play(session)
        exporter.prepare(session: session, rows: narrator.branch)
    }
    private func exportStory() {
        DeviceActionLog.shared.record(.share, .requested, control: .story)
        if exporter.readyURL != nil { narrator.setPaused(true); exporter.request(); return }
        let restart = exporter.needsRestart(before: narrator.index)
        exporter.request()
        if restart, let missing = exporter.firstMissingIndex { narrator.jump(to: missing) } else { narrator.setPaused(false) }
    }
    private var canTalk: Bool { session.workspaceReady && session.engineReady && !session.snapshot.busy && session.capturePhase != .processing }
    private var talkControl: some View {
        TimelineView(.animation(minimumInterval: 1 / 30, paused: session.capturePhase != .recording)) { context in
            let duration = session.performanceCapture ? 45.0 : 8.0
            let progress = session.captureStarted.map { min(1, max(0, context.date.timeIntervalSince($0) / duration)) } ?? 0
            VStack(spacing: 10) {
                if session.capturePhase == .recording || chalkReveal > 0.5 {
                    Text(session.performanceCapture ? "Send" : chalkReveal > 0.5 ? "Chalk" : "Talk").font(.custom("ComicRelief-Bold", size: 30, relativeTo: .title2))
                    MicrophoneWaveform(levels: session.microphoneLevels).frame(height: 22)
                } else { ShoutButtonLabel() }
                if session.capturePhase == .recording {
                    ProgressView(value: progress).tint(theme.buttonInk)
                        .accessibilityLabel("Recording time").accessibilityValue("\(Int(progress * duration)) of \(Int(duration)) seconds")
                }
            }
            .padding(.horizontal, 14)
            .padding(.vertical, 10)
            .frame(maxWidth: .infinity, minHeight: session.layout.talkHeight)
            .foregroundStyle(theme.buttonInk)
            .background((chalkReveal > 0.5 ? Color(red: 0.94, green: 0.88, blue: 0.79) : Color(red: 1, green: 0.64, blue: 0.43)).opacity(canTalk ? 1 : 0.55))
            .clipShape(RoundedRectangle(cornerRadius: 34, style: .continuous))
            .overlay(RoundedRectangle(cornerRadius: 34, style: .continuous).strokeBorder(paper, lineWidth: 3))
            .contentShape(RoundedRectangle(cornerRadius: 34, style: .continuous))
            .overlay {
                TalkHoldInput(enabled: canTalk || held, began: {
                    if session.performanceCapture { held = true; return }
                    guard canTalk, !held, session.capturePhase == .idle else { return }
                    chalkDrag = 0; held = true; ButtonSounds.play(.press); session.beginHold()
                }, moved: { delta in
                    guard held, !session.performanceCapture else { return }
                    chalkDrag = max(0, -delta)
                }, ended: {
                    guard held else { return }
                    held = false
                    if chalkDrag >= 48 && !session.performanceCapture {
                        session.latchPerformance(); chalkDrag = 0; ButtonSounds.play(.tick); return
                    }
                    chalkDrag = 0
                    if session.capturePhase == .opening || session.capturePhase == .recording {
                        ButtonSounds.play(.release); session.endHold()
                    }
                }, cancelled: {
                    guard held else { return }
                    held = false; chalkDrag = 0
                    if session.capturePhase == .opening || session.capturePhase == .recording { session.cancelHold() }
                }).accessibilityHidden(true)
            }
            .accessibilityElement(children: .ignore)
            .accessibilityLabel(session.performanceCapture ? "Send performance" : "Talk")
            .accessibilityHint(session.performanceCapture ? "Drawing and microphone are recording together. Tap to send. Up to 45 seconds." : "Hold to record. Swipe left and release to draw and record together. Release without swiping to send.")
            .accessibilityIdentifier("talk-control")
            .accessibilityAddTraits(.isButton)
            .accessibilityAction(named: Text("Draw and record")) { if canTalk { session.beginHold(); session.latchPerformance() } }
            .accessibilityAction(named: Text("Type a request")) { if canTalk { session.cancelHold(); ButtonSounds.play(.key); openComposer() } }
            .accessibilityAction { if session.capturePhase == .recording { session.endHold() } else if canTalk { session.beginHold() } }
        }
    }
}

struct VersionFeed: View {
    let snapshot: PieceSnapshot
    var foreground: Color
    var selectionColor: Color
    var textSize: CGFloat = 22
    let disabled: Bool
    var holdSelection = false
    let stop: () -> Void
    let retry: () -> Void
    let select: (Int) -> Void
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var focusedVersion: Int?
    private var rowHeight: CGFloat { max(56, textSize + 28) }
    var body: some View {
        GeometryReader { geometry in
        ScrollView {

            LazyVStack(alignment: .leading, spacing: 0) {
                ForEach(snapshot.versions.filter { $0.id > 0 }.reversed()) { version in
                    Button {
                        ButtonSounds.play(.tick); withAnimation(reduceMotion ? nil : .easeInOut(duration: 0.28)) { focusedVersion = version.id }
                    } label: {
                        VersionRow(version: version, foreground: foreground, selected: version.id == snapshot.head, textSize: textSize, rowHeight: rowHeight)
                    }.buttonStyle(.plain).disabled(disabled || holdSelection)
                        .id(version.id)
                        .visualEffect { content, geometry in
                            let distance = max(0, geometry.frame(in: .scrollView(axis: .vertical)).minY)
                            return content.opacity(Double(max(0.22, 1 - distance / 280)))
                        }
                        .accessibilityAddTraits(version.id == snapshot.head ? [.isSelected] : [])
                        .accessibilityIdentifier("version-\(version.id)")
                        .accessibilityValue(version.id == snapshot.head ? "Current version" : "")
                }
            }.scrollTargetLayout().font(.custom("ComicRelief-Regular", size: textSize, relativeTo: .title3))
        }
        .safeAreaPadding(.bottom, max(0, geometry.size.height - rowHeight))
        .scrollTargetBehavior(.viewAligned)
        .scrollPosition(id: $focusedVersion, anchor: .top)
        .overlay(alignment: .top) {
            if snapshot.versions.contains(where: { $0.id > 0 }) {
            Rectangle().fill(foreground.opacity(0.055))
                .overlay(alignment: .bottom) { Rectangle().fill(foreground.opacity(0.35)).frame(height: 1) }
                .overlay(alignment: .top) { Rectangle().fill(foreground.opacity(0.35)).frame(height: 1) }
                .frame(height: rowHeight).allowsHitTesting(false).accessibilityHidden(true)
            }
        }
        .safeAreaInset(edge: .top, spacing: 0) {
            if let attempt = snapshot.attempt, ["working", "failed", "unchanged", "interrupted"].contains(attempt.status) {
                HStack(spacing: 12) {
                    if snapshot.busy { ProgressView().frame(width: 48) }
                    if snapshot.busy && snapshot.phase.hasPrefix("Checking picture") {
                        Text("Checking picture…")
                            .font(.custom("ComicRelief-Regular", size: textSize, relativeTo: .title3))
                            .frame(maxWidth: .infinity, alignment: .trailing)
                            .accessibilityIdentifier("generation-phase")
                    } else if snapshot.busy, let output = snapshot.output, !output.isEmpty {
                        CodeTicker(output: output, thinking: snapshot.phase.hasPrefix("Thinking"))
                    } else {
                    Text(attempt.request.replacingOccurrences(of: #" · [0-9.]+ seconds$"#, with: "", options: .regularExpression))
                        .font(.custom("ComicRelief-Regular", size: textSize, relativeTo: .title3))
                        .lineLimit(1).truncationMode(.tail).frame(maxWidth: .infinity, alignment: .trailing)
                    }
                    if snapshot.busy {
                        Button { ButtonSounds.play(.stop); stop() } label: { KidlispStopMark() }.buttonStyle(KidlispStopStyle()).accessibilityLabel("Stop generation")
                    } else if attempt.status == "interrupted" || attempt.status == "failed" {
                        Button("Try again") { ButtonSounds.play(.press); retry() }.disabled(disabled)
                    }
                }.frame(height: rowHeight).padding(.horizontal, 10)
            }
        }
        .scrollDisabled(disabled || holdSelection)
        .onAppear { focusedVersion = snapshot.head > 0 ? snapshot.head : nil }
        .onChange(of: snapshot.versions.count) { _, _ in focusedVersion = snapshot.head }
        .task(id: focusedVersion) {
            try? await Task.sleep(for: .milliseconds(320))
            guard !Task.isCancelled, !disabled, !holdSelection, let focusedVersion, focusedVersion != snapshot.head else { return }
            select(focusedVersion)
        }
        .accessibilityLabel("Version history").accessibilityIdentifier("version-rolodex")
        }
    }
}

private struct VersionRow: View {
    let version: PieceRevision
    let foreground: Color
    let selected: Bool
    let textSize: CGFloat
    let rowHeight: CGFloat
    var body: some View {
                        HStack(spacing: 12) {
                            ComicTitle(text: "v\(version.id)", size: textSize).frame(width: 48, alignment: .leading)
                            if version.hasDrawing == true { Image(systemName: "pencil.tip").accessibilityLabel("With chalk") }
                            Text(version.utterance).lineLimit(1).truncationMode(.tail)
                                .frame(maxWidth: .infinity, alignment: .trailing)
                        }
                        .frame(height: rowHeight)
                        .padding(.horizontal, 10)
                        .frame(maxWidth: .infinity, alignment: .leading)
                        .background(foreground.opacity(version.id.isMultiple(of: 2) ? 0.045 : 0.10))
                        .background {
                            if let sound = version.sound { VersionWaveform(sound: sound).foregroundStyle(foreground.opacity(0.12)).allowsHitTesting(false).accessibilityHidden(true) }
                        }
                        .foregroundStyle(selected ? foreground : foreground.opacity(0.8))
                        .contentShape(Rectangle())
    }
}

struct VersionWaveform: View {
    let sound: PieceSound
    var body: some View {
        Canvas { context, size in
            let frames = sound.frames.filter { $0.atMs.isFinite && $0.rms.isFinite && $0.rms >= 0 }
            let peak = max(0.001, frames.map(\.rms).max() ?? 0)
            let duration = max(1, sound.durationMs)
            var bars = Path()
            for frame in frames {
                let x = min(size.width, max(0, frame.atMs / duration * size.width))
                let height = frame.rms / peak * size.height * 0.46
                bars.move(to: CGPoint(x: x, y: size.height / 2 - height))
                bars.addLine(to: CGPoint(x: x, y: size.height / 2 + height))
            }
            context.stroke(bars, with: .foreground, lineWidth: max(2, size.width / CGFloat(max(1, frames.count)) * 0.6))
        }.clipped()
    }
}

// kidlisp.com's stop button: a red disc with a white rounded square, lifted by
// a soft drop shadow and a hairline highlight along its top edge.
struct KidlispStopMark: View {
    @Environment(\.colorScheme) private var scheme
    var diameter: CGFloat = 44
    private var red: Color { scheme == .dark ? Color(red: 239 / 255, green: 83 / 255, blue: 80 / 255) : Color(red: 244 / 255, green: 67 / 255, blue: 54 / 255) }
    var body: some View {
        ZStack {
            Circle().fill(red)
                .overlay(Circle().strokeBorder(.white.opacity(scheme == .dark ? 0.3 : 0.2), lineWidth: 1).mask(LinearGradient(colors: [.white, .clear], startPoint: .top, endPoint: .center)))
                .shadow(color: .black.opacity(scheme == .dark ? 0.6 : 0.3), radius: 3, y: 3)
            RoundedRectangle(cornerRadius: 2, style: .continuous).fill(.white).frame(width: diameter * 14 / 48, height: diameter * 14 / 48)
        }
        .frame(width: diameter, height: diameter)
        .contentShape(Circle())
    }
}

struct KidlispStopStyle: ButtonStyle {
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .scaleEffect(configuration.isPressed ? 0.95 : 1)
            .animation(.easeOut(duration: 0.2), value: configuration.isPressed)
    }
}

struct ComicTitle: View {
    let text: String
    var colors: [[Double]] = []
    var size: CGFloat
    private var colored: AttributedString {
        var result = AttributedString()
        for (index, character) in text.enumerated() {
            var part = AttributedString(String(character))
            if colors.indices.contains(index), colors[index].count == 3 {
                part.foregroundColor = Color(red: colors[index][0] / 255, green: colors[index][1] / 255, blue: colors[index][2] / 255)
            } else { part.foregroundColor = Color(red: 1, green: 0.96, blue: 0.91) }
            result += part
        }
        return result
    }
    var body: some View {
        ZStack {
            Text(text).foregroundStyle(.cyan).offset(x: 2, y: 2)
            Text(text).foregroundStyle(.pink).offset(x: 1, y: 1)
            ForEach(0..<8, id: \.self) { i in Text(text).foregroundStyle(Color(white: 0.08)).offset(x: cos(Double(i) * .pi / 4), y: sin(Double(i) * .pi / 4)) }
            Text(colored)
        }.font(.custom("ComicRelief-Bold", size: size, relativeTo: .title3)).lineLimit(1).minimumScaleFactor(0.65)
            .accessibilityElement(children: .ignore).accessibilityLabel(text)
    }
}


struct InlineRequestComposer: View {
    let theme: WhistlegraphTheme
    var disabled: Bool
    var hasDrawing = false
    @Binding var text: String
    let cancel: () -> Void
    let send: (String) -> Void
    @FocusState private var focused: Bool
    @StateObject private var keySounds = PromptKeySounds()
    private var prompt: String { text.trimmingCharacters(in: .whitespacesAndNewlines) }
    private func submit() {
        DeviceActionLog.shared.record(.typeSend, .requested, [.characters: prompt.count])
        guard !disabled, !prompt.isEmpty || hasDrawing else { return }
        send(prompt)
    }
    private func singleLine(_ value: String) -> String {
        String(value.components(separatedBy: .newlines).joined(separator: " ").prefix(96))
    }
    private func edited(from old: String, to value: String) {
        let previous = singleLine(old), next = singleLine(value)
        if next != value { text = next }
        guard next != previous else { return }
        DeviceActionLog.shared.record(.typeEdit, nil, [.characters: next.count])
        // One click per edit, including delete/paste; never a burst for pasted text
        // or a second click when the length/newline correction updates the field.
        let inserted = next.difference(from: previous).compactMap { change -> Character? in
            if case let .insert(_, character, _) = change { return character }
            return nil
        }
        keySounds.play(key: inserted.count == 1 ? inserted.first : nil)
    }
    var body: some View {
        VStack(spacing: 8) {
            TextField("What happens next?", text: $text)
                .font(.custom("ComicRelief-Regular", size: 26, relativeTo: .title2))
                .lineLimit(1).focused($focused)
                .submitLabel(.send).onSubmit(submit)
                .onChange(of: text) { old, value in edited(from: old, to: value) }
                .accessibilityLabel("Request, up to 96 characters").accessibilityIdentifier("typed-request")
            HStack {
                Button("Cancel") { DeviceActionLog.shared.record(.typeSend, .cancelled); cancel() }.accessibilityIdentifier("request-cancel")
                Spacer()
                Text("\(text.count) / 96").accessibilityIdentifier("request-count").monospacedDigit().foregroundStyle(.secondary)
                Spacer()
                Button("Send", action: submit)
                    .accessibilityIdentifier("request-send")
                    .disabled(disabled || (prompt.isEmpty && !hasDrawing))
            }.font(.title3)
        }.padding(14)
            .background(theme.surface, in: RoundedRectangle(cornerRadius: 22, style: .continuous))
            .task { keySounds.prepare(); focused = true }
            .onDisappear { keySounds.stop() }
    }
}

/// A TV with a small arrow rising out of it: the piece goes up to the screen.
struct SendToTVIcon: View {
    var body: some View {
        VStack(spacing: -1) {
            Image(systemName: "arrow.up").font(.system(size: 12, weight: .heavy))
            Image(systemName: "tv").font(.system(size: 23, weight: .bold))
        }
    }
}

/// Story cards held like a hand of cards: four cards fanned from a pivot
/// below the frame, each outlined so the overlaps read as separate cards.
struct CardFanIcon: View {
    var body: some View {
        ZStack {
            ForEach([-27.0, -9.0, 9.0, 27.0], id: \.self) { angle in
                RoundedRectangle(cornerRadius: 2.5, style: .continuous)
                    .fill(.tint)
                    .overlay(RoundedRectangle(cornerRadius: 2.5, style: .continuous)
                        .stroke(Color(uiColor: .systemBackground), lineWidth: 1.5))
                    .frame(width: 12, height: 19)
                    .rotationEffect(.degrees(angle), anchor: UnitPoint(x: 0.5, y: 1.55))
            }
        }.offset(y: -2)
    }
}
