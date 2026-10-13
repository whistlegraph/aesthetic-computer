import SwiftUI
import AVKit

/// MIME loads the first post behind its wordmark, then breaks the mark apart
/// to reveal the native reel. No introductory swipe is required.
struct Reel: View {
    var active = true
    let makeWork: (Ware) -> Void
    @StateObject private var feed = Feed()
    @StateObject private var pool = RuntimePool()
    @State private var current: String?
    @State private var interacting: String?
    @State private var revealed = false
    @State private var breakupAt: Date?
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
                            Card(post: post, layout: feed.layout, pool: pool, focused: active && current == post.id, interacting: interacting == post.id, ready: { firstContentReady(post.id) }, makeWork: makeWork) {
                                interacting = interacting == post.id ? nil : post.id
                            }
                            .containerRelativeFrame([.horizontal, .vertical])
                            .id(post.id)
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
                .scrollDisabled(!revealed || interacting != nil && interacting == current)
                .accessibilityHidden(!revealed)
                .ignoresSafeArea()
                .onChange(of: active) { _, visible in
                    if visible { prepare() } else { pool.focus(nil) }
                }
                .onChange(of: current) { _, id in
                    interacting = nil
                    prepare()
                }
                .onChange(of: feed.posts) { _, posts in
                    if current == nil { current = posts.first?.id }
                    prepare()
                }
                .onChange(of: pool.ready) { _, ready in
                    if let first = feed.posts.first, ready.contains(first.id) { firstContentReady(first.id) }
                }
                .task {
                    async let posts: Void = feed.loadMore()
                    async let layout: Void = feed.refreshLayout()
                    _ = await (posts, layout)
                    if current == nil { current = feed.posts.first?.id }
                    prepare()
                }
                .onChange(of: scenePhase) { _, phase in
                    if phase == .active { Task { await feed.refreshLayout() } }
                }

                Banner(corner: 1, width: screen.size.width)
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
    }
}

extension Reel {
    private var breakupDuration: Double { reduceMotion ? 0.2 : feed.layout.loadingBreakupSeconds ?? 0.8 }

    private func firstContentReady(_ id: String) {
        guard !revealed, breakupAt == nil, id == feed.posts.first?.id else { return }
        breakupAt = Date()
        Task { @MainActor in
            try? await Task.sleep(for: .seconds(breakupDuration))
            revealed = true
        }
    }

    /// Load runtimes around the card in view, including the first under the loading mark.
    private func prepare() {
        guard !feed.posts.isEmpty else { return }
        pool.prepare(feed.posts, around: feed.posts.firstIndex { $0.id == current } ?? 0)
        pool.focus(active ? current : nil)
    }
}

/// The wordmark on its banner; `corner` slides it into a top-right watermark.
private struct Banner: View {
    let corner: CGFloat
    let width: CGFloat
    var body: some View {
        let mark: CGFloat = 37 * 151 / 52
        HStack(alignment: .lastTextBaseline, spacing: 2) {
            Wordmark(height: 37)
            PalText(text: ".ac", size: 19, accent: Color(white: 0.08))
                .overlay(alignment: .leading) { Circle().fill(Color.pink).frame(width: 6, height: 6).offset(x: 2, y: 6) }
                .opacity(Double(max(0, 1 - 2 * corner)))
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

/// One post: its media fills the screen; the code sits top-left (and is the
/// Interact control where there is something to play with), the handle
/// bottom-left and the type bottom-right.
private struct Card: View {
    let post: Post
    let layout: MimeLayout
    @ObservedObject var pool: RuntimePool
    let focused: Bool
    let interacting: Bool
    let ready: () -> Void
    let makeWork: (Ware) -> Void
    let toggleInteract: () -> Void
    @Environment(\.openURL) private var openURL

    private var playable: Bool {
        switch post.body { case .runtime, .packed, .video: return true; default: return false }
    }

    var body: some View {
        ZStack {
            Checker(tint: Color.author(post.name))
            media
        }
        .overlay(alignment: .topLeading) {
            Button(action: { playable ? toggleInteract() : open() }) {
                PalText(text: post.command, size: CGFloat(layout.identifierSize), accent: Color(hex: 0xED629D))
                    .overlay(alignment: .bottom) {
                        if interacting { WavyUnderline().offset(y: 6) }
                    }
            }
            .buttonStyle(.plain)
            .padding(.leading, layout.padding).padding(.top, 62)
            .accessibilityIdentifier("mime-global-identifier")
            .accessibilityHint(playable ? (interacting ? "Back to scrolling" : "Play with this piece") : "Open in Aesthetic Computer")
        }
        .overlay(alignment: .bottom) {
            VStack(alignment: .leading, spacing: 8) {
                if layout.showTitle, let title = post.media?.title, !title.isEmpty, title != post.command {
                    PalText(text: title, size: CGFloat(layout.titleSize))
                }
                if layout.showCaption, !post.isMood, let text = post.text, !text.isEmpty {
                    Text(text).font(.custom("ComicRelief-Bold", size: layout.captionSize, relativeTo: .body)).foregroundStyle(.white)
                        .lineLimit(3)
                }
                HStack(alignment: .lastTextBaseline) {
                    if let name = post.name { PalText(text: name, size: CGFloat(layout.handleSize), accent: Color.author(name)).accessibilityIdentifier("mime-author") }
                    Spacer(minLength: 12)
                    if let ware = Ware.creationType(post.mimeType, kind: post.media?.kind) {
                        Button { makeWork(ware) } label: {
                            PalText(text: post.mimeType, size: CGFloat(layout.mimeTypeSize), accent: Color.tint(forType: post.mimeType))
                                .contentShape(Rectangle())
                        }.buttonStyle(.plain).accessibilityIdentifier("mime-type")
                            .accessibilityHint("Make a new " + ware.title)
                    } else {
                        PalText(text: post.mimeType, size: CGFloat(layout.mimeTypeSize), accent: Color.tint(forType: post.mimeType)).accessibilityIdentifier("mime-type")
                    }
                }
            }
            .padding(.horizontal, layout.padding).padding(.bottom, 34)
        }
        .clipped()
    }

    @ViewBuilder private var media: some View {
        switch post.body {
        case .image(let url):
            AsyncImage(url: url, transaction: Transaction(animation: .easeOut(duration: 0.2))) { phase in
                if let image = phase.image { image.resizable().interpolation(.none).scaledToFit().onAppear(perform: ready) }
                else if phase.error != nil { Text("Preview unavailable").font(.custom("ComicRelief-Bold", size: 16)).foregroundStyle(.white.opacity(0.6)).onAppear(perform: ready) }
                else { ProgressView() }
            }
            .accessibilityLabel(post.file?.alt ?? post.command)
        case .video(let url):
            LoopingVideo(url: url, playing: focused, ready: ready)
        case .runtime, .packed:
            PieceRuntime(pool: pool, id: post.id, interactive: interacting)
        case .words(let words):
            Text(words)
                .font(.custom("ComicRelief-Bold", size: 40)).multilineTextAlignment(.center)
                .foregroundStyle(.white).shadow(color: Color(white: 0.08), radius: 0, x: 3, y: 3)
                .padding(28).frame(maxWidth: .infinity, maxHeight: .infinity)
                .background(Color.author(post.name).opacity(0.3))
                .onAppear(perform: ready)
        case .document(let url):
            DocumentText(url: url, ready: ready)
        }
    }

    private func open() { if let link = post.media?.url, let url = URL(string: link) { openURL(url) } }
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
            .disabled(true)
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
