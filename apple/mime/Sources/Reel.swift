import SwiftUI
import AVKit

/// mime.ac as a native reel: a "swipe up" splash, then one post per screen.
/// The wordmark rides the splash's banner top-left and follows the first swipe
/// to the top-right corner, shrinking and fading into a watermark.
struct Reel: View {
    @StateObject private var feed = Feed()
    @State private var current: String?
    @State private var interacting: String?
    @State private var corner: CGFloat = 0 // 0 on the splash, 1 from the first post on

    var body: some View {
        GeometryReader { screen in
            ZStack(alignment: .top) {
                Color.paper.ignoresSafeArea()
                ScrollView(.vertical) {
                    LazyVStack(spacing: 0) {
                        Splash()
                            .containerRelativeFrame([.horizontal, .vertical])
                            .id("splash")
                            .background(GeometryReader { proxy in
                                Color.clear.preference(key: SplashOffset.self, value: proxy.frame(in: .named("reel")).minY)
                            })
                        ForEach(feed.posts) { post in
                            Card(post: post, focused: current == post.id, interacting: interacting == post.id) {
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
                .scrollDisabled(interacting != nil && interacting == current)
                .ignoresSafeArea()
                .onPreferenceChange(SplashOffset.self) { y in
                    // The lazy stack lets go of the splash once it scrolls away;
                    // then there is no offset, and the mark stays where it went.
                    guard let y else { return }
                    corner = min(1, max(0, -y / max(1, screen.size.height + screen.safeAreaInsets.top + screen.safeAreaInsets.bottom)))
                }
                .onChange(of: current) { _, id in
                    interacting = nil
                    if let id, id != "splash" { corner = 1 }
                }
                .task { await feed.loadMore() }

                Banner(corner: corner, width: screen.size.width)
            }
        }
        .preferredColorScheme(.dark)
    }
}

private struct SplashOffset: PreferenceKey {
    static var defaultValue: CGFloat? = nil
    static func reduce(value: inout CGFloat?, nextValue: () -> CGFloat?) { value = nextValue() ?? value }
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

private struct Splash: View {
    @State private var nudge = false
    var body: some View {
        VStack(spacing: 6) {
            PalText(text: "↑", size: 44, accent: Color(white: 0.08), fill: .pink)
            PalText(text: "swipe up", size: 46, accent: .pink)
        }
        .offset(y: nudge ? -5 : 0)
        .animation(.easeInOut(duration: 0.8).repeatForever(autoreverses: true), value: nudge)
        .onAppear { nudge = true }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color(hex: 0xE7E1ED))
        .accessibilityElement(children: .ignore)
        .accessibilityLabel("Swipe up for the feed")
    }
}

/// One post: its media fills the screen; the code sits top-left (and is the
/// Interact control where there is something to play with), the handle
/// bottom-left and the type bottom-right.
private struct Card: View {
    let post: Post
    let focused: Bool
    let interacting: Bool
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
                PalText(text: post.command, size: 21, accent: Color(hex: 0xED629D))
                    .overlay(alignment: .bottom) {
                        if interacting { WavyUnderline().offset(y: 6) }
                    }
            }
            .buttonStyle(.plain)
            .padding(.leading, 14).padding(.top, 62)
            .accessibilityHint(playable ? (interacting ? "Back to scrolling" : "Play with this piece") : "Open in Aesthetic Computer")
        }
        .overlay(alignment: .bottom) {
            HStack(alignment: .bottom) {
                if let name = post.name { PalText(text: name, size: 21, accent: Color.author(name)) }
                Spacer(minLength: 12)
                PalText(text: post.mimeType, size: 21, accent: Color.tint(forType: post.mimeType))
            }
            .padding(.horizontal, 14).padding(.bottom, 34)
        }
        .clipped()
    }

    @ViewBuilder private var media: some View {
        switch post.body {
        case .image(let url):
            AsyncImage(url: url, transaction: Transaction(animation: .easeOut(duration: 0.2))) { phase in
                if let image = phase.image { image.resizable().interpolation(.none).scaledToFit() }
                else if phase.error != nil { Text("Preview unavailable").font(.custom("ComicRelief-Bold", size: 16)).foregroundStyle(.white.opacity(0.6)) }
                else { ProgressView() }
            }
            .accessibilityLabel(post.file?.alt ?? post.command)
        case .video(let url):
            LoopingVideo(url: url, playing: focused)
        case .runtime(let url, _), .packed(let url):
            if focused { PieceRuntime(url: url, interactive: interacting).id(url) }
            else { ProgressView().tint(.white) }
        case .words(let words):
            Text(words)
                .font(.custom("ComicRelief-Bold", size: 40)).multilineTextAlignment(.center)
                .foregroundStyle(.white).shadow(color: Color(white: 0.08), radius: 0, x: 3, y: 3)
                .padding(28).frame(maxWidth: .infinity, maxHeight: .infinity)
                .background(Color.author(post.name).opacity(0.3))
        case .document(let url):
            DocumentText(url: url)
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
                if playing { queue.play() }
            }
            .onChange(of: playing) { _, now in now ? player?.play() : player?.pause() }
            .onDisappear { player?.pause() }
    }
}

/// Text and code posts read in place, scrolling inside the card.
private struct DocumentText: View {
    let url: URL
    @State private var text = "…"
    var body: some View {
        ScrollView {
            Text(text).font(.system(size: 14, design: .monospaced)).foregroundStyle(.white.opacity(0.9))
                .frame(maxWidth: .infinity, alignment: .leading).padding(.horizontal, 18).padding(.top, 110).padding(.bottom, 90)
        }
        .scrollDisabled(true)
        .task {
            guard let (data, _) = try? await URLSession.shared.data(from: url), let body = String(data: data, encoding: .utf8) else { text = "file unavailable"; return }
            text = body.count > 8000 ? String(body.prefix(8000)) + "\n…" : body
        }
    }
}
