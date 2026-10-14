import SwiftUI
import AVKit

/// MIME loads the first post behind its wordmark, then breaks the mark apart
/// to reveal the native reel. No introductory swipe is required.
struct Reel: View {
    var active = true
    var viewerHandle = ""
    let resumeEditor: () -> Void
    let remixWork: (Post) -> Void
    let makeWork: (Ware) -> Void
    @StateObject private var feed = Feed()
    @StateObject private var pool = RuntimePool()
    @State private var current: String?
    @State private var interacting: String?
    @State private var edgesReady = true
    @State private var cooldown: Task<Void, Never>?
    @State private var revealed = false
    @State private var breakupAt: Date?
    @State private var scrolling = false
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @Environment(\.scenePhase) private var scenePhase

    var body: some View {
        GeometryReader { screen in
            ZStack(alignment: .top) {
                RuntimeParking(pool: pool).ignoresSafeArea()
                Color.paper.ignoresSafeArea()
                ScrollView(.vertical) {
                    LazyVStack(spacing: 0) {
                        ForEach(feed.posts) { post in
                            Card(post: post, layout: feed.layout, pool: pool, focused: active && current == post.id, interacting: interacting == post.id, edgesReady: edgesReady, topInset: screen.safeAreaInsets.top, bottomInset: screen.safeAreaInsets.bottom, escape: resetInteraction, step: step, ready: { firstContentReady(post.id) }, makeWork: makeWork, resumeEditor: resumeEditor, remixWork: remixWork, hide: hide, activity: { holding in contentActivity(post.id, holding: holding) })
                            .containerRelativeFrame([.horizontal, .vertical])
                            .id(post.id)
                            .environment(\.mimeLettersActive, active && current == post.id)
                            .onAppear { if post.id == feed.posts.dropLast(3).last?.id || feed.posts.count < 4 { Task { await feed.loadMore() } } }
                        }
                        if feed.failed {
                            Text("Could not reach mime.ac.\nPull back up to try again.")
                                .font(.custom("ComicRelief-Bold", size: 18)).multilineTextAlignment(.center)
                                .foregroundStyle(.white.opacity(0.7))
                                .containerRelativeFrame([.horizontal, .vertical])
                        }
                    }
                    .scrollTargetLayout()
                }
                .coordinateSpace(name: "reel")
                .scrollTargetBehavior(.paging)
                .scrollPosition(id: $current)
                .scrollIndicators(.hidden)
                .modifier(ScrollMotion(moving: $scrolling))
                .scrollDisabled(!revealed || currentPost?.playable == true)
                .accessibilityHidden(!revealed)
                .ignoresSafeArea()
                .onChange(of: active) { _, visible in
                    if visible { prepare() } else { resetInteraction(); pool.focus(nil) }
                }
                .onChange(of: current) { _, id in
                    resetInteraction()
                    prepare()
                }
                .onChange(of: viewerHandle) { _, handle in pool.setViewer(handle) }
                .onChange(of: feed.posts) { _, posts in
                    if current == nil { current = posts.first?.id }
                    prepare()
                }
                .onChange(of: pool.ready) { _, ready in
                    if let first = feed.posts.first, ready.contains(first.id) { firstContentReady(first.id) }
                }
                .task {
                    pool.setViewer(viewerHandle)
                    pool.onInteraction = { id, holding in contentActivity(id, holding: holding) }
                    async let posts: Void = feed.loadMore()
                    async let layout: Void = feed.refreshLayout()
                    _ = await (posts, layout)
                    if current == nil { current = feed.posts.first?.id }
                    prepare()
                }
                .onChange(of: scenePhase) { _, phase in
                    if phase == .active { prepare(); Task { await feed.refreshLayout() } }
                    else { resetInteraction(); pool.focus(nil) }
                }

                // MIME.ac slides off to the right while the feed moves or a piece is being played.
                Banner(corner: 1, width: screen.size.width)
                    .offset(x: scrolling || interacting != nil ? 180 : 0, y: interacting != nil ? -90 : 0)
                    .animation(reduceMotion ? .linear(duration: 0.1) : .easeInOut(duration: 0.22), value: scrolling || interacting != nil)
                    .overlay(alignment: .topTrailing) {
                        Button(action: resumeEditor) { Color.clear.frame(width: 115, height: 44).contentShape(Rectangle()) }
                            .buttonStyle(.plain).accessibilityLabel("Open editor")
                            .accessibilityHint("Resume your current work")
                            .accessibilityIdentifier("mime-resume-editor")
                            .padding(.trailing, 10)
                    }
                    .allowsHitTesting(interacting == nil)
                    .accessibilityHidden(interacting != nil)
                if revealed && active && interacting == nil {
                    Button(action: resumeEditor) { Image(systemName: "plus").font(.system(size: 23)).frame(width: 44, height: 44) }
                        .buttonStyle(.plain).foregroundStyle(Color(hex: 0x7AE7CE))
                        .accessibilityLabel("Open editor").accessibilityIdentifier("mime-create-work")
                        .padding(.top, 4)
                }
                if revealed && active && currentPost?.playable == true {
                    ReelEdges(ready: edgesReady, topInset: 0, bottomInset: 0, escapeOnly: true, escape: resetInteraction, step: step)
                        .ignoresSafeArea()
                }
                if !revealed {
                    ZStack {
                        Color.paper.opacity(breakupAt == nil ? 1 : 0)
                            .animation(.easeOut(duration: breakupDuration), value: breakupAt)
                        Wordmark(height: min(110, screen.size.width * 0.8 * 52 / 151), breakupAt: reduceMotion ? nil : breakupAt, breakupDuration: breakupDuration)
                            .opacity(reduceMotion && breakupAt != nil ? 0 : 1)
                            .animation(.easeOut(duration: breakupDuration), value: breakupAt)
                        if feed.failed || feed.loaded && feed.posts.isEmpty {
                            VStack(spacing: 12) {
                                Text(feed.failed ? "Could not reach mime.ac." : "No pieces yet.")
                                Button("Try again") { Task { await feed.retry() } }
                            }.font(.custom("ComicRelief-Bold", size: 18))
                                .padding(.top, 180)
                        }
                    }.frame(maxWidth: .infinity, maxHeight: .infinity).ignoresSafeArea()
                        .accessibilityIdentifier("mime-loading")
                        .accessibilityLabel("Loading MIME")
                }
            }
        }
        .preferredColorScheme(.dark)
        .environment(\.mimeLettersActive, active)
    }
}

extension Reel {
    private var currentPost: Post? { feed.posts.first { $0.id == current } }

    private func resetInteraction() {
        cooldown?.cancel(); cooldown = nil
        interacting = nil; edgesReady = true
    }

    private func contentActivity(_ id: String, holding: Bool) {
        guard active, id == current, currentPost?.playable == true else { return }
        cooldown?.cancel()
        interacting = id; edgesReady = false
        guard !holding else { return }
        cooldown = Task { @MainActor in
            do {
                try await Task.sleep(for: .seconds(1))
                interacting = nil
                try await Task.sleep(for: .seconds(reduceMotion ? 0.1 : 0.22))
                edgesReady = true
            } catch { /* New input or a new card owns the timer now. */ }
        }
    }

    private func step(_ direction: Int) {
        guard let index = feed.posts.firstIndex(where: { $0.id == current }) else { return }
        let next = index + direction
        guard feed.posts.indices.contains(next) else { return }
        resetInteraction()
        withAnimation(reduceMotion ? nil : .easeOut(duration: 0.28)) { current = feed.posts[next].id }
    }

    private var breakupDuration: Double { reduceMotion ? 0.2 : feed.layout.loadingBreakupSeconds ?? 0.8 }

    private func firstContentReady(_ id: String) {
        guard !revealed, breakupAt == nil, id == feed.posts.first?.id else { return }
        breakupAt = Date()
        Task { @MainActor in
            try? await Task.sleep(for: .seconds(breakupDuration))
            revealed = true
        }
    }

    /// Hide everything by this card's author; the card after it takes its place.
    private func hide(_ post: Post) {
        guard let name = post.name else { return }
        let index = feed.posts.firstIndex { $0.id == current } ?? 0
        if interacting == post.id { interacting = nil }
        feed.hide(name)
        current = feed.posts.dropFirst(min(index, feed.posts.count)).first?.id ?? feed.posts.last?.id
        prepare()
    }

    /// Load runtimes around the card in view, including the first under the loading mark.
    private func prepare() {
        guard !feed.posts.isEmpty else { return }
        pool.prepare(feed.posts, around: feed.posts.firstIndex { $0.id == current } ?? 0)
        pool.focus(active ? current : nil)
    }
}

/// The 48-point strips wait for the corners. The outer 8 points never wait.
private struct ReelEdges: View {
    let ready: Bool
    let topInset: CGFloat
    let bottomInset: CGFloat
    var escapeOnly = false
    let escape: () -> Void
    let step: (Int) -> Void
    @State private var glowing: String?
    @State private var admitted: String?

    var body: some View {
        GeometryReader { area in
            ZStack {
                if escapeOnly {
                    strip("top", emergency: true).frame(height: 8).frame(maxHeight: .infinity, alignment: .top)
                    strip("bottom", emergency: true).frame(height: 8).frame(maxHeight: .infinity, alignment: .bottom)
                    strip("left", emergency: true).frame(width: 8).frame(maxWidth: .infinity, alignment: .leading)
                    strip("right", emergency: true).frame(width: 8).frame(maxWidth: .infinity, alignment: .trailing)
                } else {
                    strip("top", emergency: false).frame(height: 48 + topInset).frame(maxHeight: .infinity, alignment: .top)
                    strip("bottom", emergency: false).frame(height: 48 + bottomInset).frame(maxHeight: .infinity, alignment: .bottom)
                }
            }.frame(width: area.size.width, height: area.size.height)
        }
    }

    private func strip(_ side: String, emergency: Bool) -> some View {
        Rectangle().fill(glowing == side ? Color.cyan.opacity(0.12) : .clear)
            .overlay { Rectangle().stroke(Color.cyan.opacity(glowing == side ? 0.65 : 0), lineWidth: 3).blur(radius: 5) }
            .contentShape(Rectangle())
            .gesture(DragGesture(minimumDistance: 0)
                .onChanged { _ in
                    if admitted == nil { admitted = emergency || ready ? side : "blocked" }
                    guard admitted == side else { return }
                    if emergency { escape() }
                    glowing = side
                }
                .onEnded { value in
                    defer { admitted = nil; glowing = nil }
                    guard admitted == side, abs(value.translation.height) >= 32 else { return }
                    step(value.translation.height < 0 ? 1 : -1)
                })
            .accessibilityElement()
            .accessibilityLabel("Swipe feed from " + side + " edge")
            .accessibilityIdentifier("mime-edge-" + side + (emergency ? "-escape" : ""))
            .accessibilityAction(named: "Next post") { if emergency || ready { escape(); step(1) } }
            .accessibilityAction(named: "Previous post") { if emergency || ready { escape(); step(-1) } }
    }
}

private struct CornerRetreat: ViewModifier {
    let hidden: Bool
    let x: CGFloat
    let y: CGFloat
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    func body(content: Content) -> some View {
        content
            .offset(x: hidden && !reduceMotion ? x * 500 : 0, y: hidden && !reduceMotion ? y * 160 : 0)
            .opacity(hidden ? 0 : 1)
            .allowsHitTesting(!hidden)
            .accessibilityHidden(hidden)
            .animation(reduceMotion ? .linear(duration: 0.1) : .easeInOut(duration: 0.22), value: hidden)
    }
}

/// Whether the feed is being dragged or is still settling (iOS 18 and later).
private struct ScrollMotion: ViewModifier {
    @Binding var moving: Bool
    func body(content: Content) -> some View {
        if #available(iOS 18.0, *) {
            content.onScrollPhaseChange { _, phase in moving = phase.isScrolling }
        } else { content }
    }
}

/// The wordmark on its banner; `corner` slides it into a top-right watermark.
private struct Banner: View {
    let corner: CGFloat
    let width: CGFloat
    var body: some View {
        let mark: CGFloat = 37 * 151 / 52 + 36   // the wordmark and its .ac
        HStack(alignment: .lastTextBaseline, spacing: -6) {
            Wordmark(height: 37)
            PalText(text: ".ac", size: 22, accent: Color(white: 0.08))
                .overlay(alignment: .leading) { Circle().fill(Color.pink).frame(width: 6, height: 6).offset(x: 2, y: 6) }
        }
        .scaleEffect(1 - 0.25 * corner, anchor: .trailing)
        .opacity(Double(1 - 0.45 * corner))
        .frame(width: mark, alignment: .leading)
        .offset(x: 14 + (width - mark - 28) * corner)
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(.top, 4)
        .frame(height: 52, alignment: .top)
        .background(Color(hex: 0xEEE9F1).opacity(Double(1 - corner)).ignoresSafeArea(edges: .top))
        .allowsHitTesting(false)
    }
}

/// One post: live media fills the center; its identity retreats to the corners
/// while playing and returns before the wider navigation strips unlock.
private struct Card: View {
    let post: Post
    let layout: MimeLayout
    @ObservedObject var pool: RuntimePool
    let focused: Bool
    let interacting: Bool
    let edgesReady: Bool
    let topInset: CGFloat
    let bottomInset: CGFloat
    let escape: () -> Void
    let step: (Int) -> Void
    let ready: () -> Void
    let makeWork: (Ware) -> Void
    let resumeEditor: () -> Void
    let remixWork: (Post) -> Void
    let hide: (Post) -> Void
    let activity: (Bool) -> Void
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var showingQR = false
    @State private var showingProfile = false

    var body: some View {
        ZStack {
            Checker(tint: Color.author(post.name))
            media
            if focused && post.playable {
                ReelEdges(ready: edgesReady, topInset: topInset, bottomInset: bottomInset, escape: escape, step: step)
            }
        }
        .overlay(alignment: .topLeading) {
            HStack(alignment: .lastTextBaseline, spacing: 12) {
            Button(action: { withAnimation(.spring(duration: 0.4, bounce: 0.3)) { showingQR.toggle() } }) {
                PalText(text: post.command, size: CGFloat(layout.identifierSize), accent: Color(hex: 0xED629D), swing: 2.2, entrance: true)
            }
            .buttonStyle(.plain)
            .accessibilityIdentifier("mime-global-identifier")
            .accessibilityHint(showingQR ? "Hide the QR code" : "Show a QR code for this card")
            if let version = post.edition, post.remixWare != nil {
                Button { remixWork(post) } label: {
                    PalText(text: "v\(version)", size: CGFloat(layout.identifierSize), accent: Color(hex: 0x55CDD9))
                        .frame(minWidth: 44, minHeight: 44).contentShape(Rectangle())
                }.buttonStyle(.plain).accessibilityIdentifier("mime-remix-version")
                    .accessibilityLabel("v\(version)").accessibilityHint("Remix this version")
            }
            }.padding(.leading, layout.padding).padding(.top, 52)
                .modifier(CornerRetreat(hidden: interacting, x: -1, y: -1))
        }
        .overlay(alignment: .topLeading) {
            if showingQR && !interacting {
                CornerQR(post: post) { withAnimation(.spring(duration: 0.35, bounce: 0.2)) { showingQR = false } }
                    .padding(.leading, layout.padding).padding(.top, 52 + CGFloat(layout.identifierSize) + 14)
                    .transition(.scale(scale: 0.05, anchor: .topLeading).combined(with: .opacity))
            }
        }
        .overlay(alignment: .bottom) {
            VStack(alignment: .leading, spacing: 8) {
                if !interacting, layout.showTitle, let title = post.media?.title, !title.isEmpty, title != post.command {
                    PalText(text: title, size: CGFloat(layout.titleSize))
                }
                if !interacting, (layout.showCaption || post.embed != nil), !post.isMood, let text = post.embed?.caption ?? (post.embed == nil ? post.text : nil), !text.isEmpty {
                    Text(text).font(.custom("ComicRelief-Bold", size: layout.captionSize, relativeTo: .body)).foregroundStyle(.white)
                        .lineLimit(3)
                }
                HStack(alignment: .lastTextBaseline) {
                    if let name = post.name {
                        Button { withAnimation(.spring(duration: 0.45, bounce: 0.3)) { showingProfile = true } } label: {
                            PalText(text: name, size: CGFloat(layout.handleSize), accent: Color.author(name), colors: (post.nameColors ?? []).map(Color.savedLetter), swing: 2.2, entrance: true)
                                .contentShape(Rectangle())
                        }.buttonStyle(.plain).accessibilityHint("Open their profile").accessibilityIdentifier("mime-author")
                            .modifier(CornerRetreat(hidden: interacting, x: -1, y: 1))
                    }
                    Spacer(minLength: 12)
                    if let ware = Ware.creationType(post.mimeType, kind: post.media?.kind) {
                        Button { makeWork(ware) } label: {
                            TypeLabel(type: post.typeLabel, size: CGFloat(layout.mimeTypeSize)).contentShape(Rectangle())
                        }.buttonStyle(.plain).accessibilityIdentifier("mime-type")
                            .accessibilityHint("Make a new " + ware.title)
                            .modifier(CornerRetreat(hidden: interacting, x: 1, y: 1))
                    } else {
                        Button(action: resumeEditor) {
                            TypeLabel(type: post.typeLabel, size: CGFloat(layout.mimeTypeSize)).contentShape(Rectangle())
                        }.buttonStyle(.plain).accessibilityIdentifier("mime-type")
                            .accessibilityHint("Open editor")
                            .modifier(CornerRetreat(hidden: interacting, x: 1, y: 1))
                    }
                }
            }
            .padding(.horizontal, layout.padding).padding(.bottom, 34)
        }
        .clipped()
        .overlay {
            if showingProfile {
                ProfilePopover(post: post, hide: hide) { withAnimation(.spring(duration: 0.35, bounce: 0.15)) { showingProfile = false } }
                    .transition(.scale(scale: 0.1, anchor: .bottomLeading).combined(with: .opacity))
            }
        }
        .onChange(of: focused) { _, now in if !now { showingQR = false; showingProfile = false } }
    }

    @ViewBuilder private var media: some View {
        switch post.body {
        case .image(let url):
            ZoomableImage(url: url, focused: focused, ready: ready)
                .accessibilityLabel(post.file?.alt ?? post.command)
        case .video(let url):
            LoopingVideo(url: url, playing: focused, ready: ready)
                .simultaneousGesture(DragGesture(minimumDistance: 0).onChanged { _ in activity(true) }.onEnded { _ in activity(false) })
        case .runtime, .packed:
            PieceRuntime(pool: pool, id: post.id, interactive: focused && post.playable)
        case .words(let words):
            Words(text: words, post: post, padding: CGFloat(layout.padding), ready: ready)
        case .document(let url):
            DocumentText(url: url, ready: ready)
        }
    }
}

/// A card's MIME type, bottom-right. A long one (application/x-oskiewar-demo)
/// breaks after its slash: the family rides small above the subtype, so the
/// type keeps its size and never crowds the handle.
private struct TypeLabel: View {
    let type: String
    let size: CGFloat

    var body: some View {
        let colors = Color.letters(forType: type), accent = Color.tint(forType: type)
        if type.count > 18, let slash = type.firstIndex(of: "/") {
            let cut = type.distance(from: type.startIndex, to: slash) + 1
            VStack(alignment: .trailing, spacing: 0) {
                PalText(text: String(type[..<type.index(after: slash)]), size: size * 0.62, accent: accent, colors: Array(colors.prefix(cut)), swing: 2.2, entrance: true)
                PalText(text: String(type[type.index(after: slash)...]), size: size, accent: accent, colors: Array(colors.dropFirst(cut)), swing: 2.2, entrance: true)
            }.accessibilityElement(children: .ignore).accessibilityLabel(type)
        } else {
            PalText(text: type, size: size, accent: accent, colors: colors, swing: 2.2, entrance: true)
        }
    }
}

/// mime.ac's drifting checkerboard behind every card, tinted by the author.
private struct Checker: View {
    let tint: Color
    var body: some View {
        Canvas { context, size in
            let cell: CGFloat = 32
            context.fill(Path(CGRect(origin: .zero, size: size)), with: .color(Color(hex: 0x1B1729)))
            for row in 0...Int(size.height / cell) {
                for col in 0...Int(size.width / cell) where (row + col) % 2 == 0 {
                    context.fill(Path(CGRect(x: CGFloat(col) * cell, y: CGFloat(row) * cell, width: cell, height: cell)), with: .color(tint.opacity(0.14)))
                }
            }
        }
        .ignoresSafeArea()
    }
}

private struct WavyUnderline: View {
    var body: some View {
        Path { p in
            p.move(to: .zero)
            for x in stride(from: 0, through: 60, by: 6) { p.addQuadCurve(to: CGPoint(x: CGFloat(x) + 6, y: 0), control: CGPoint(x: CGFloat(x) + 3, y: x % 12 == 0 ? -3 : 3)) }
        }
        .stroke(Color(hex: 0x55CDD9), lineWidth: 2)
        .frame(width: 66, height: 6)
    }
}

/// A feed video plays muted-free and looping while its card is in view.
private struct LoopingVideo: View {
    let url: URL
    let playing: Bool
    let ready: () -> Void
    @State private var observation: NSKeyValueObservation?
    @State private var player: AVQueuePlayer?
    @State private var looper: AVPlayerLooper?
    var body: some View {
        VideoPlayer(player: player)
            .onAppear {
                guard player == nil else { return }
                let queue = AVQueuePlayer()
                looper = AVPlayerLooper(player: queue, templateItem: AVPlayerItem(url: url))
                player = queue
                observation = queue.currentItem?.observe(\.status, options: [.initial, .new]) { item, _ in
                    if item.status != .unknown { Task { @MainActor in ready() } }
                }
                if playing { queue.play() }
            }
            .onChange(of: playing) { _, now in now ? player?.play() : player?.pause() }
            .onDisappear { player?.pause(); observation?.invalidate(); observation = nil }
    }
}

/// Text and code posts read in place, scrolling inside the card.
private struct DocumentText: View {
    let url: URL
    let ready: () -> Void
    @State private var text = "…"
    var body: some View {
        ScrollView {
            Text(text).font(.system(size: 14, design: .monospaced)).foregroundStyle(.white.opacity(0.9))
                .frame(maxWidth: .infinity, alignment: .leading).padding(.horizontal, 18).padding(.top, 110).padding(.bottom, 90)
        }
        .scrollDisabled(true)
        .task {
            guard let (data, _) = try? await URLSession.shared.data(from: url), let body = String(data: data, encoding: .utf8) else { text = "file unavailable"; ready(); return }
            text = body.count > 8000 ? String(body.prefix(8000)) + "\n…" : body
            ready()
        }
    }
}

/// A post that is only words (a mood, a chat line) is drawn the way mime.ac draws
/// it: big comic lettering that breathes letter by letter, outlined over a
/// pink-purple-cyan offset, on the author's colour drifting under it. The words
/// shrink until they fit one page; where and when it was said sits underneath.
private struct Words: View {
    let text: String
    let post: Post
    let padding: CGFloat
    let ready: () -> Void
    @Environment(\.accessibilityReduceMotion) private var reduced
    @Environment(\.scenePhase) private var scenePhase
    @Environment(\.mimeLettersActive) private var onstage
    @State private var started = Date()

    var body: some View {
        let moving = !reduced && onstage && scenePhase == .active
        TimelineView(.animation(minimumInterval: 1 / 30, paused: !moving)) { timeline in
            let time = moving ? timeline.date.timeIntervalSince(started) : nil
            ZStack {
                MoodSky(tint: Color.author(post.name), time: time)
                VStack(spacing: 18) {
                    lettering(time: time)
                    if let footer = Self.footer(post, now: timeline.date) {
                        PalText(text: footer, size: 15, accent: Color.author(post.name), fill: .white.opacity(0.85), animated: false)
                            .accessibilityIdentifier("mime-words-when")
                    }
                }
                .padding(.horizontal, padding + 8)
                .padding(.top, 104).padding(.bottom, 92)
            }
        }
        .onAppear(perform: ready)
    }

    @ViewBuilder private func lettering(time: Double?) -> some View {
        let words = Text(text).font(.custom("ComicRelief-Bold", size: 46))
        Group {
            if #available(iOS 18, *) { words.textRenderer(BreathingLetters(time: time)) }
            else { words.shadow(color: Color(hex: 0x55CDD9), radius: 0, x: 3, y: 3) }
        }
        .foregroundStyle(.white)
        .multilineTextAlignment(.center).lineSpacing(4)
        .minimumScaleFactor(0.3)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    /// "laer-klokken · 5m ago", in mime.ac's own units.
    static func footer(_ post: Post, now: Date) -> String? {
        let when = post.when.flatMap(Self.date).map { Self.ago($0, now: now) }
        let parts = [post.origin?.name, when].compactMap { $0 }.filter { !$0.isEmpty }
        return parts.isEmpty ? nil : parts.joined(separator: " · ")
    }

    static func date(_ iso: String) -> Date? {
        let fractional = ISO8601DateFormatter()
        fractional.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return fractional.date(from: iso) ?? ISO8601DateFormatter().date(from: iso)
    }

    static func ago(_ date: Date, now: Date) -> String {
        let s = now.timeIntervalSince(date)
        if s < 60 { return "just now" }
        if s < 3600 { return "\(Int(s / 60))m ago" }
        if s < 86400 { return "\(Int(s / 3600))h ago" }
        return "\(Int(s / 86400))d ago"
    }
}

/// mime.ac's mood-colors: the author's colour, a cyan and a pink glow, drifting
/// on 19 s and 27 s cycles over the card's paper.
private struct MoodSky: View {
    let tint: Color
    let time: Double?
    var body: some View {
        let t = time ?? 0
        let a = sin(t * .pi / 19), b = sin((t + 11) * .pi / 27)
        GeometryReader { geo in
            let reach = max(geo.size.width, geo.size.height)
            ZStack {
                Color.paper
                tint.opacity(0.3)
                RadialGradient(colors: [tint.opacity(0.6), .clear], center: UnitPoint(x: 0.25 + 0.06 * a, y: 0.35 + 0.05 * a), startRadius: 0, endRadius: reach * 0.6 * (1 + 0.1 * a))
                RadialGradient(colors: [Color(hex: 0x55CDD9, opacity: 0.26), .clear], center: UnitPoint(x: 0.75 - 0.06 * a, y: 0.7 - 0.05 * a), startRadius: 0, endRadius: reach * 0.55)
                RadialGradient(colors: [Color(hex: 0xED629D, opacity: 0.22), .clear], center: UnitPoint(x: 0.65 + 0.07 * b, y: 0.25 - 0.05 * b), startRadius: 0, endRadius: reach * 0.5)
            }
        }
        .ignoresSafeArea()
    }
}

/// Each glyph bobs and tilts on PalText's breath (±0.055 em, ±2°), and wears a
/// near-black outline over a pink-purple-cyan offset. All shadows paint first,
/// then all outlines, then all fills, so nothing cuts into a neighbour.
@available(iOS 18, *)
private struct BreathingLetters: TextRenderer {
    var time: Double?

    func draw(layout: Text.Layout, in ctx: inout GraphicsContext) {
        var glyphs: [(slice: Text.Layout.RunSlice, place: GraphicsContext, em: CGFloat)] = []
        var index = 0
        for line in layout {
            for run in line {
                let em = (run.typographicBounds.ascent + run.typographicBounds.descent) / 1.25
                for slice in run {
                    let i = index; index += 1
                    let motion = time.map { PalText.breathe(i, time: $0) } ?? 0
                    let jitter = PalText.jitter(i)
                    let r = slice.typographicBounds.rect
                    var place = ctx
                    place.translateBy(x: r.midX, y: r.midY + em * (motion * 0.055 - jitter.dy * 0.012))
                    place.rotate(by: .degrees(motion * 2 - jitter.rot * 0.6))
                    place.translateBy(x: -r.midX, y: -r.midY)
                    glyphs.append((slice, place, em))
                }
            }
        }
        let shadows: [(CGFloat, UInt32)] = [(0.075, 0x55CDD9), (0.05, 0xA478DF), (0.025, 0xED629D)]
        for glyph in glyphs {
            for (step, hex) in shadows {
                var g = glyph.place
                g.translateBy(x: glyph.em * step, y: glyph.em * step)
                g.addFilter(.colorMultiply(Color(hex: hex)))
                g.draw(glyph.slice)
            }
        }
        for glyph in glyphs {
            let outline = max(1.2, glyph.em * 0.045)
            for i in 0..<8 {
                let angle = Double(i) * .pi / 4
                var g = glyph.place
                g.translateBy(x: cos(angle) * outline, y: sin(angle) * outline)
                g.addFilter(.colorMultiply(Color(white: 0.08)))
                g.draw(glyph.slice)
            }
        }
        for glyph in glyphs { glyph.place.draw(glyph.slice) }
    }
}

/// Paintings and images pinch to zoom, pixels kept sharp, and stay zoomed for
/// a closer look; a double tap toggles 3x, and leaving the card resets it. At
/// 1x the inner scroll view has nowhere to go, so a swipe still pages the reel.
private struct ZoomableImage: UIViewRepresentable {
    let url: URL
    let focused: Bool
    let ready: () -> Void

    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeUIView(context: Context) -> UIScrollView {
        let scroll = UIScrollView()
        scroll.minimumZoomScale = 1
        scroll.maximumZoomScale = 12
        scroll.showsVerticalScrollIndicator = false
        scroll.showsHorizontalScrollIndicator = false
        scroll.contentInsetAdjustmentBehavior = .never
        scroll.bouncesZoom = true
        scroll.delegate = context.coordinator
        let image = context.coordinator.image
        image.contentMode = .scaleAspectFit
        image.layer.magnificationFilter = .nearest
        image.frame = scroll.bounds
        image.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        scroll.addSubview(image)
        let spinner = context.coordinator.spinner
        spinner.color = .white
        spinner.startAnimating()
        spinner.translatesAutoresizingMaskIntoConstraints = false
        scroll.addSubview(spinner)
        NSLayoutConstraint.activate([spinner.centerXAnchor.constraint(equalTo: scroll.frameLayoutGuide.centerXAnchor),
                                     spinner.centerYAnchor.constraint(equalTo: scroll.frameLayoutGuide.centerYAnchor)])
        let double = UITapGestureRecognizer(target: context.coordinator, action: #selector(Coordinator.toggle(_:)))
        double.numberOfTapsRequired = 2
        scroll.addGestureRecognizer(double)
        context.coordinator.load(url, ready: ready)
        return scroll
    }

    func updateUIView(_ scroll: UIScrollView, context: Context) {
        if !focused, scroll.zoomScale != 1 { scroll.setZoomScale(1, animated: false) }
    }

    final class Coordinator: NSObject, UIScrollViewDelegate {
        let image = UIImageView()
        let spinner = UIActivityIndicatorView(style: .medium)
        private var task: URLSessionDataTask?

        func load(_ url: URL, ready: @escaping () -> Void) {
            task = URLSession.shared.dataTask(with: url) { [weak self] data, _, _ in
                let picture = data.flatMap(UIImage.init(data:))
                DispatchQueue.main.async {
                    guard let self else { return }
                    self.spinner.stopAnimating()
                    self.image.image = picture
                    if picture == nil {
                        let label = UILabel()
                        label.text = "Preview unavailable"
                        label.font = UIFont(name: "ComicRelief-Bold", size: 16)
                        label.textColor = .white.withAlphaComponent(0.6)
                        label.textAlignment = .center
                        label.frame = self.image.bounds
                        label.autoresizingMask = [.flexibleWidth, .flexibleHeight]
                        self.image.addSubview(label)
                    }
                    ready()
                }
            }
            task?.resume()
        }

        deinit { task?.cancel() }

        func viewForZooming(in scrollView: UIScrollView) -> UIView? { image }

        @objc func toggle(_ tap: UITapGestureRecognizer) {
            guard let scroll = tap.view as? UIScrollView else { return }
            if scroll.zoomScale > 1 { scroll.setZoomScale(1, animated: true); return }
            let at = tap.location(in: image), scale: CGFloat = 3
            let size = CGSize(width: scroll.bounds.width / scale, height: scroll.bounds.height / scale)
            scroll.zoom(to: CGRect(x: at.x - size.width / 2, y: at.y - size.height / 2, width: size.width, height: size.height), animated: true)
        }
    }
}
