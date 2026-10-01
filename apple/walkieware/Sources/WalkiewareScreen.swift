import SwiftUI

struct PieceSnapshot: Decodable {
    var code = ""
    var caption: String?
    var output: String?
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
    enum CodingKeys: String, CodingKey { case code, caption, output, handle, colors, head, hasPiece, hasPreview, error, busy, phase, attempt; case revisions = "versions" }
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

struct WalkiewareScreen: View {
    @ObservedObject var session: WalkiewareSession
    @StateObject private var narrator = VersionNarrator()
    @Environment(\.scenePhase) private var scenePhase
    @State private var held = false
    @GestureState private var touching = false
    @State private var showComposer = false
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @Environment(\.colorScheme) private var colorScheme
    @AppStorage("walkieware-appearance") private var appearance = "system"
    private var themePhase: WalkiewareTheme.Phase {
        if session.captureError != nil || !session.snapshot.error.isEmpty { return .error }
        if session.capturePhase == .recording || session.capturePhase == .opening || showComposer { return .listening }
        if session.snapshot.busy && session.snapshot.phase.localizedCaseInsensitiveContains("evaluat") { return .rendering }
        if session.snapshot.busy || session.capturePhase == .processing { return .working }
        return .ready
    }
    private var theme: WalkiewareTheme { WalkiewareTheme(phase: themePhase, dark: colorScheme == .dark) }
    private var paper: Color { theme.foreground }
    private var accent: Color { theme.accent }
    var body: some View {
        VStack(spacing: session.layout.spacing) {
            HStack {
                IdentityHeader(session: session, appearance: $appearance, size: session.layout.historySize) { narrator.stop(); showComposer = false }
                Spacer()
                Button {
                    showComposer = false
                    narrator.play(session)
                } label: {
                    Image(systemName: "play.fill").font(.system(size: 28, weight: .bold)).frame(width: 44, height: 44)
                }
                .disabled(session.snapshot.busy || session.capturePhase != .idle || !session.engineReady || !session.snapshot.versions.contains(where: { $0.id > 0 }))
                .accessibilityLabel("Play version story").accessibilityIdentifier("play-versions")
            }.frame(height: narrator.isPlaying ? 0 : nil).clipped().accessibilityHidden(narrator.isPlaying)
            VStack(spacing: 4) {
            // This representable never changes identity when cards or versions change.
            ZStack {
                Workspace(voice: session)
                    .opacity(session.snapshot.hasPreview ? 1 : 0)
                    .allowsHitTesting(session.snapshot.hasPreview && session.capturePhase == .idle)
                    .accessibilityHidden(!session.snapshot.hasPreview)
                if !session.snapshot.hasPreview && !session.engineReady { ProgressView() }
                if session.capturePhase == .recording || session.capturePhase == .opening {
                    ScrollView {
                        Text(session.transcript.isEmpty ? (session.capturePhase == .opening ? "Opening microphone…" : "Listening…") : session.transcript)
                            .font(.custom("ComicRelief-Regular", size: 30, relativeTo: .title)).frame(maxWidth: .infinity, alignment: .leading).padding(18)
                    }.background(theme.background.opacity(0.95))
                }
                if let failure = session.startupFailure {
                    VStack(spacing: 12) { Text(failure); Button("Reload") { session.reloadWorkspace() } }.padding().background(.black.opacity(0.85))
                }
            }
            .aspectRatio(narrator.isPlaying ? nil : 4 / 3, contentMode: .fit)
            .frame(maxHeight: narrator.isPlaying ? .infinity : nil)
            .overlay {
                if narrator.isPlaying { Rectangle().strokeBorder(paper.opacity(0.8), lineWidth: 2).allowsHitTesting(false) }
            }
            .overlay(alignment: .topTrailing) {
                if narrator.isPlaying { Button { narrator.stop() } label: { Image(systemName: "xmark.circle.fill").font(.largeTitle).padding(16).background(.black.opacity(0.5), in: Circle()) }.accessibilityLabel("Close version story") }
            }
            if narrator.isPlaying && (session.snapshot.hasPreview || session.snapshot.hasHistory) {
                HStack(alignment: .bottom) {
                    ComicTitle(text: "v\(session.snapshot.head)", size: session.layout.historySize)
                        .accessibilityLabel("Running version \(session.snapshot.head)")
                    Spacer(minLength: 0)
                    PlaybackCaption(text: narrator.utterance, spokenRange: narrator.spokenRange, accent: accent)
                        .font(.custom("ComicRelief-Regular", size: session.layout.historySize, relativeTo: .title3))
                        .multilineTextAlignment(.trailing)
                        .accessibilityIdentifier("spoken-word")

                }
            }
            }
            if !narrator.error.isEmpty { Text(narrator.error).foregroundStyle(.orange) }
            if let failure = session.captureError { Text(failure).font(.body).foregroundStyle(.orange).frame(maxWidth: .infinity, alignment: .leading) }
            if !narrator.isPlaying && (session.snapshot.hasPiece || session.snapshot.hasHistory || session.snapshot.busy || session.snapshot.attempt?.status == "failed") {
                VersionFeed(snapshot: session.snapshot, foreground: paper, selectionColor: paper, textSize: session.layout.historySize, disabled: session.snapshot.busy || session.capturePhase != .idle, stop: { session.command("stop") }, retry: { session.command("retry") }) { narrator.select($0, session: session) }
            } else if !narrator.isPlaying { Spacer(minLength: 0) }
        }
        .padding(.horizontal, narrator.isPlaying ? 0 : session.layout.pageInset)
        .foregroundStyle(paper)
        .safeAreaInset(edge: .bottom, spacing: 8) {
            Group {
                if narrator.isPlaying { EmptyView() } else if showComposer {
                    InlineRequestComposer(theme: theme, disabled: session.snapshot.busy, cancel: { showComposer = false }) { text in
                        session.command("ask", text: text); showComposer = false
                    }
                } else {
                    HStack(spacing: 12) {
                        Button { showComposer = true } label: {
                            TypingButtonLabel()
                                .frame(maxWidth: .infinity, minHeight: session.layout.talkHeight)
                                .foregroundStyle(theme.buttonInk)
                                .background(Color(red: 0.40, green: 0.83, blue: 0.95), in: RoundedRectangle(cornerRadius: 34, style: .continuous))
                                .overlay(RoundedRectangle(cornerRadius: 34, style: .continuous).strokeBorder(paper, lineWidth: 3))
                        }.buttonStyle(.plain).disabled(!canTalk || session.capturePhase != .idle).accessibilityLabel("Type").accessibilityIdentifier("type-control")
                        talkControl
                    }
                }
            }.padding(.horizontal, session.layout.pageInset).foregroundStyle(paper)
                .background(theme.background)
        }
        .statusBarHidden(narrator.isPlaying)
        .onChange(of: session.narratedFrame) { _, _ in narrator.painted(session.narratedVersion) }
        .onChange(of: scenePhase) { _, value in if value != .active { narrator.stop() } }
        .onChange(of: session.capturePhase) { _, value in if value != .idle { narrator.stop() } }
        .onDisappear { narrator.stop() }
        .background(theme.background.ignoresSafeArea())
        .tint(paper)
        .animation(reduceMotion ? nil : .easeInOut(duration: 0.3), value: themePhase)
    }
    private var identity: String { (session.snapshot.handle.isEmpty ? "Sign in" : "@" + session.snapshot.handle) + (session.snapshot.code.isEmpty ? "" : "/" + session.snapshot.code) }
    private var canTalk: Bool { session.workspaceReady && session.engineReady && !session.snapshot.busy && session.capturePhase != .processing }
    private var talkControl: some View {
        TimelineView(.animation(minimumInterval: 1 / 30, paused: session.capturePhase != .recording)) { context in
            let progress = session.captureStarted.map { min(1, max(0, context.date.timeIntervalSince($0) / 8)) } ?? 0
            VStack(spacing: 10) {
                if session.capturePhase == .recording {
                    Text("Talk").font(.custom("ComicRelief-Bold", size: 30, relativeTo: .title2))
                    MicrophoneWaveform(levels: session.microphoneLevels).frame(height: 22)
                } else { ShoutButtonLabel() }
                if session.capturePhase == .recording {
                    ProgressView(value: progress).tint(theme.buttonInk)
                        .accessibilityLabel("Recording time").accessibilityValue("\(Int(progress * 8)) of 8 seconds")
                }
            }
            .padding(.horizontal, 14)
            .padding(.vertical, 10)
            .frame(maxWidth: .infinity, minHeight: session.layout.talkHeight)
            .foregroundStyle(theme.buttonInk)
            .background(Color(red: 1, green: 0.64, blue: 0.43).opacity(canTalk ? 1 : 0.55))
            .clipShape(RoundedRectangle(cornerRadius: 34, style: .continuous))
            .overlay(RoundedRectangle(cornerRadius: 34, style: .continuous).strokeBorder(paper, lineWidth: 3))
            .contentShape(RoundedRectangle(cornerRadius: 34, style: .continuous))
            .gesture(DragGesture(minimumDistance: 0).updating($touching) { _, state, _ in state = true }.onChanged { value in
                guard canTalk || held else { return }
                guard !held, session.capturePhase == .idle else { return }
                held = true; session.beginHold()
            }.onEnded { value in
                if held { session.endHold() }
                held = false
            })
            .onChange(of: touching) { _, active in
                if !active && held { held = false; session.cancelHold() }
            }
            .accessibilityElement(children: .ignore)
            .accessibilityLabel("Talk")
            .accessibilityHint("Hold to record, release to send. Up to eight seconds.")
            .accessibilityIdentifier("talk-control")
            .accessibilityAddTraits(.isButton)
            .accessibilityAction(named: Text("Type a request")) { if canTalk { session.cancelHold(); showComposer = true } }
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
                        withAnimation(reduceMotion ? nil : .easeInOut(duration: 0.28)) { focusedVersion = version.id }
                    } label: {
                        VersionRow(version: version, foreground: foreground, selected: version.id == snapshot.head, textSize: textSize, rowHeight: rowHeight)
                    }.buttonStyle(.plain).disabled(disabled)
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
                    if snapshot.busy, let output = snapshot.output, !output.isEmpty {
                        CodeTicker(output: output, thinking: snapshot.phase.hasPrefix("Thinking"))
                    } else {
                    Text(attempt.request.replacingOccurrences(of: #" · [0-9.]+ seconds$"#, with: "", options: .regularExpression))
                        .font(.custom("ComicRelief-Regular", size: textSize, relativeTo: .title3))
                        .lineLimit(1).truncationMode(.tail).frame(maxWidth: .infinity, alignment: .trailing)
                    }
                    if snapshot.busy {
                        Button(action: stop) { KidlispStopMark() }.buttonStyle(KidlispStopStyle()).accessibilityLabel("Stop generation")
                    } else if attempt.status == "interrupted" || attempt.status == "failed" {
                        Button("Try again", action: retry).disabled(disabled)
                    }
                }.frame(height: rowHeight).padding(.horizontal, 10)
            }
        }
        .scrollDisabled(disabled)
        .onAppear { focusedVersion = snapshot.head > 0 ? snapshot.head : nil }
        .onChange(of: snapshot.versions.count) { _, _ in focusedVersion = snapshot.head }
        .task(id: focusedVersion) {
            try? await Task.sleep(for: .milliseconds(320))
            guard !Task.isCancelled, !disabled, let focusedVersion, focusedVersion != snapshot.head else { return }
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
    let theme: WalkiewareTheme
    var disabled: Bool
    let cancel: () -> Void
    let send: (String) -> Void
    @FocusState private var focused: Bool
    @State private var text = ""
    var body: some View {
        VStack(spacing: 8) {
            TextField("What happens next?", text: $text, axis: .vertical)
                .font(.custom("ComicRelief-Regular", size: 26, relativeTo: .title2))
                .lineLimit(1...3).focused($focused)
                .accessibilityLabel("Request, up to 96 characters").accessibilityIdentifier("typed-request")
                .onChange(of: text) { _, value in if value.count > 96 { text = String(value.prefix(96)) } }
            HStack {
                Button("Cancel") { cancel() }
                Spacer()
                Text("\(text.count) / 96").accessibilityIdentifier("request-count").monospacedDigit().foregroundStyle(.secondary)
                Spacer()
                Button("Send") { send(text.trimmingCharacters(in: .whitespacesAndNewlines)) }
                    .disabled(disabled || text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            }.font(.title3)
        }.padding(14)
            .background(theme.surface, in: RoundedRectangle(cornerRadius: 22, style: .continuous))
            .task { focused = true }
    }
}
