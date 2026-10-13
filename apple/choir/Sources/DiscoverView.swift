import SwiftUI

/// Fía's "doom scroll on news": one headline per screen, swipe up for the next.
/// Each card opens its comments in the same sheet as the chat.
struct DiscoverView: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    @Environment(\.dismiss) private var dismiss
    @State private var news: [Post] = []
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
                                Button { open = post; Trail.send("discover:comments") } label: {
                                    HStack(spacing: 6) {
                                        Image(systemName: "bubble.left")
                                        Text(post.replies == 0 ? "Comment" : "\(post.replies) \(post.replies == 1 ? "comment" : "comments")")
                                    }
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
            .task {
                Trail.send("discover:open")
                do { news = try await API.news() } catch { self.error = error.localizedDescription }
            }
        }
    }
}
