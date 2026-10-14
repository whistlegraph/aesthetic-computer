import SwiftUI

/// A person's AC profile, from GET aesthetic.computer/api/profile-card — the
/// same data as mime.ac's <ac-profile> (aesthetic.computer/lib/profile-card.mjs):
/// their lettered handle over their latest painting, mood and counts. Report
/// and Hide sit at the bottom for the card that was tapped.
struct ProfileCard: View {
    let post: Post
    let hide: () -> Void
    @State private var profile: Profile?
    @State private var failed = false
    @State private var done: String?
    @State private var confirmingHide = false
    @Environment(\.openURL) private var openURL

    struct Profile: Decodable {
        struct Mood: Decodable { let text: String? }
        struct Painting: Decodable { struct File: Decodable { let url: String }; let file: File? }
        let handle: String
        let colors: [Post.LetterRGB?]?
        let mood: Mood?
        let counts: [String: Int]?
        let latestPainting: Painting?
    }

    private let kinds = [("painting", "Paintings"), ("piece", "Pieces"), ("kidlisp", "KidLisp"),
                         ("tape", "Tapes"), ("whistlegraph", "Whistlegraphs"), ("mood", "Moods")]
    private var name: String { post.name ?? "" }

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 18) {
                Button { openURL(URL(string: "https://aesthetic.computer/" + name)!) } label: {
                    PalText(text: name, size: 34, accent: Color.author(name),
                            colors: (profile?.colors ?? post.nameColors ?? []).map(Color.savedLetter), swing: 2.2, entrance: true)
                }.buttonStyle(.plain).accessibilityHint("Open on Aesthetic Computer")

                if let mood = profile?.mood?.text, !mood.isEmpty {
                    Text(mood).font(.custom("ComicRelief-Bold", size: 20)).foregroundStyle(.white)
                }

                if let counts = profile?.counts {
                    LazyVGrid(columns: [GridItem(.flexible()), GridItem(.flexible()), GridItem(.flexible())], alignment: .leading, spacing: 14) {
                        ForEach(kinds, id: \.0) { kind, label in
                            VStack(alignment: .leading, spacing: 2) {
                                Text((counts[kind] ?? 0).formatted()).font(.custom("ComicRelief-Bold", size: 22)).foregroundStyle(.white)
                                Text(label).font(.custom("ComicRelief-Bold", size: 13)).foregroundStyle(.white.opacity(0.65))
                            }
                        }
                    }
                } else if failed {
                    Text("Couldn’t load this profile.").font(.custom("ComicRelief-Bold", size: 16)).foregroundStyle(.white.opacity(0.7))
                } else {
                    ProgressView().tint(.white)
                }

                Divider().overlay(.white.opacity(0.2)).padding(.vertical, 4)

                Menu {
                    ForEach(MimeModeration.reasons, id: \.self) { reason in
                        Button(reason) { Task { await send(reason) } }
                    }
                } label: {
                    Label("Report this card", systemImage: "flag").font(.custom("ComicRelief-Bold", size: 17))
                }.tint(Color(hex: 0xFF91C3)).accessibilityIdentifier("mime-report")

                Button(role: .destructive) { confirmingHide = true } label: {
                    Label("Hide \(name)", systemImage: "eye.slash").font(.custom("ComicRelief-Bold", size: 17))
                }.accessibilityIdentifier("mime-hide")

                Text("Every report is reviewed within 24 hours. Hiding someone removes their work from your feed.")
                    .font(.custom("ComicRelief-Bold", size: 13)).foregroundStyle(.white.opacity(0.55))
                Link("Rules", destination: URL(string: "https://mime.ac/terms/")!)
                    .font(.custom("ComicRelief-Bold", size: 13)).tint(Color(hex: 0x7DE3D7))
            }
            .padding(24).padding(.top, 8)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .background(alignment: .top) { wallpaper }
        .task { await load() }
        .confirmationDialog("Hide \(name)?", isPresented: $confirmingHide, titleVisibility: .visible) {
            Button("Hide \(name)", role: .destructive) {
                Task { try? await MimeModeration.report(post, reason: "hidden") }
                hide()
            }
        } message: { Text("Their work leaves your feed on this phone.") }
        .alert(done ?? "", isPresented: Binding(get: { done != nil }, set: { if !$0 { done = nil } })) {
            Button("OK", role: .cancel) {}
        }
    }

    /// Their latest painting, dimmed behind the card, as on the web.
    @ViewBuilder private var wallpaper: some View {
        if let path = profile?.latestPainting?.file?.url, let url = URL(string: path, relativeTo: URL(string: "https://aesthetic.computer")!) {
            AsyncImage(url: url.absoluteURL) { image in
                image.interpolation(.none).resizable().scaledToFill()
            } placeholder: { Color.clear }
            .frame(height: 320).clipped()
            .overlay(LinearGradient(colors: [Color.paper.opacity(0.35), Color.paper], startPoint: .top, endPoint: .bottom))
            .ignoresSafeArea()
            .accessibilityHidden(true)
        }
    }

    private func load() async {
        var components = URLComponents(string: "https://aesthetic.computer/api/profile-card")!
        components.queryItems = [.init(name: "handle", value: name)]
        do {
            let (data, response) = try await URLSession.shared.data(for: URLRequest(url: components.url!, timeoutInterval: 12))
            guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
            profile = try JSONDecoder().decode(Profile.self, from: data)
        } catch { failed = true }
    }

    private func send(_ reason: String) async {
        do {
            try await MimeModeration.report(post, reason: reason)
            done = "Thanks. We look at every report within 24 hours."
        } catch { done = "That report didn't go through. Try again in a moment." }
    }
}
