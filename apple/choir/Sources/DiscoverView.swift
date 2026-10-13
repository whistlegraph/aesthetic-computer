import SwiftUI

/// Fía's "doom scroll on news": one headline per screen, swipe up for the next.
/// Each card opens its comments in the same sheet as the chat.
struct DiscoverView: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    @Environment(\.dismiss) private var dismiss
    @Environment(\.scenePhase) private var phase
    @State private var news: [Post] = []
    @State private var fetched = Date.distantPast
    @State private var error: String?
    @State private var open: Post?
    @State private var detent: PresentationDetent = .medium

    var body: some View {
        NavigationStack {
            GeometryReader { geo in
                ScrollView(.vertical) {
                    LazyVStack(spacing: 0) {
                        if news.isEmpty {
                            VStack(spacing: 12) {
                                if error == nil { FlowerSpinner(size: 40) }
                                Text(error ?? "Gathering news…").font(Theme.type(15)).foregroundStyle(Theme.ink.opacity(0.6))
                            }
                            .frame(width: geo.size.width, height: geo.size.height)
                        }
                        ForEach(news) { post in
                            VStack(alignment: .leading, spacing: 16) {
                                Spacer(minLength: 0)
                                if let link = post.links.first {
                                    HeadlineCard(link: link, words: PostRow(post: post).shownText, large: true, stored: post.preview, opportunity: post.kind == "opportunity", opening: post.kind == "opening")
                                }
                                // Fía (22:12): comments go to the main chat, linking the article,
                                // so everyone sees what you're referring to.
                                Button { room.about = post; Trail.send("discover:talk"); dismiss() } label: {
                                    HStack(spacing: 6) { Image(systemName: "bubble.left"); Text("Talk about it in chat") }
                                        .font(Theme.type(14, bold: true))
                                }
                                .buttonStyle(GlassChipStyle())
                                Spacer(minLength: 0)
                            }
                            .padding(22)
                            .frame(width: geo.size.width, height: geo.size.height)
                        }
                    }
                    .scrollTargetLayout()
                }
                .scrollTargetBehavior(.paging)
                .scrollIndicators(.hidden)
            }
            .background(RuledPaper())
            .navigationTitle("Discover")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) {
                Button { dismiss() } label: { Hand("HandChat", height: 18).foregroundStyle(Theme.ink) }.accessibilityLabel("Back to chat")
            } }
            .sheet(item: $open) { post in
                NavigationStack { ThreadView(rootID: post.id, expand: { detent = .large }) }
                    .environmentObject(account).environmentObject(room)
                    .presentationDetents([.medium, .large], selection: $detent).presentationDragIndicator(.visible)
                    .onAppear { detent = .medium }
                    .presentationBackground(.regularMaterial)
            }
            .task { Trail.send("discover:open"); await load() }
            // Morning coffee: left open overnight, the slate reloads when the
            // phone wakes up to it (Fía, 10-13). Pull down to refresh any time.
            .onChange(of: phase) { _, p in
                if p == .active, fetched.timeIntervalSinceNow < -300 { Task { await load() } }
            }
            .refreshable { await load() }
        }
    }

    func load() async {
        do { news = try await API.news(); error = nil; fetched = Date() }
        catch { if news.isEmpty { self.error = error.localizedDescription } }
    }
}
