import SwiftUI
import CoreMotion

/// A person's AC profile as a card you could hold: the license-like
/// <ac-profile> from aesthetic.computer/lib/profile-card.(mjs|css) — mint to
/// cream, security stripes, their paintings tiled faintly behind, a stacked
/// edge — tilting in 3D with the phone or a drag, a sheen following the tilt.
/// Report and Hide sit under it for the card that was tapped.
struct ProfileCard: View {
    let post: Post
    let hide: () -> Void
    @State private var profile: Profile?
    @State private var failed = false
    @State private var done: String?
    @State private var confirmingHide = false
    @State private var drag: CGSize = .zero
    @StateObject private var motion = Tilt()
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
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
    private static let ink = Color(hex: 0x182E2A), label = Color(hex: 0x385348)

    /// Degrees of tilt about x and y, from the drag when held, else the phone.
    private var tilt: (x: Double, y: Double) {
        if reduceMotion { return (0, 0) }
        if drag != .zero { return (Double(-drag.height / 9).clamped(18), Double(drag.width / 9).clamped(18)) }
        return (motion.pitch * 22, motion.roll * 22)
    }

    var body: some View {
        ScrollView {
            VStack(spacing: 22) {
                license
                    .rotation3DEffect(.degrees(tilt.x), axis: (1, 0, 0), perspective: 0.6)
                    .rotation3DEffect(.degrees(tilt.y), axis: (0, 1, 0), perspective: 0.6)
                    .gesture(DragGesture(minimumDistance: 4)
                        .onChanged { drag = $0.translation }
                        .onEnded { _ in withAnimation(.spring(duration: 0.6, bounce: 0.45)) { drag = .zero } })
                    .animation(.interactiveSpring(response: 0.25), value: motion.roll)
                    .padding(.top, 18)
                moderation
            }
            .padding(.horizontal, 20).padding(.bottom, 28)
        }
        .task { await load() }
        .onAppear { if !reduceMotion { motion.start() } }
        .onDisappear { motion.stop() }
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

    // MARK: The card

    private var license: some View {
        let shape = RoundedRectangle(cornerRadius: 17, style: .continuous)
        return VStack(alignment: .leading, spacing: 10) {
            field("Handle")
            Button { openURL(URL(string: "https://aesthetic.computer/" + name)!) } label: {
                PalText(text: name, size: 30, accent: Color.author(name),
                        colors: (profile?.colors ?? post.nameColors ?? []).map(Color.savedLetter), swing: 2.2, entrance: true)
            }.buttonStyle(.plain).accessibilityHint("Open on Aesthetic Computer")
            if let mood = profile?.mood?.text, !mood.isEmpty {
                field("Mood")
                Text(mood).font(.system(size: 15)).foregroundStyle(Self.ink).lineLimit(3)
            }
            Rectangle().fill(Self.ink.opacity(0.3)).frame(height: 1).padding(.top, 4)
            if let counts = profile?.counts {
                Grid(alignment: .leading, horizontalSpacing: 22, verticalSpacing: 6) {
                    ForEach(0..<3, id: \.self) { row in
                        GridRow {
                            ForEach(kinds[(row * 2)..<(row * 2 + 2)], id: \.0) { kind, title in
                                HStack(alignment: .firstTextBaseline) {
                                    Text(title).font(.system(size: 11, design: .monospaced)).foregroundStyle(Self.label)
                                    Spacer(minLength: 6)
                                    Text((counts[kind] ?? 0).formatted()).font(.system(size: 15, weight: .bold, design: .monospaced)).foregroundStyle(Self.ink)
                                }
                            }
                        }
                    }
                }
            } else {
                Text(failed ? "Couldn’t load this profile." : "Loading…")
                    .font(.system(size: 13, design: .monospaced)).foregroundStyle(Self.label)
            }
        }
        .padding(.horizontal, 22).padding(.top, 20).padding(.bottom, 16)
        .frame(maxWidth: 420, alignment: .leading)
        .background {
            ZStack {
                LinearGradient(stops: [.init(color: Color(hex: 0xC6E3CC), location: 0), .init(color: Color(hex: 0xF2EDCD), location: 0.37),
                                       .init(color: Color(hex: 0xF5EBD7), location: 0.67), .init(color: Color(hex: 0xD1E6E9), location: 1)],
                               startPoint: UnitPoint(x: 0.1, y: 0), endPoint: UnitPoint(x: 0.9, y: 1))
                paintings.padding(5)
                SecurityStripes().opacity(0.28).padding(5)
                sheen
            }
            .clipShape(shape)
        }
        .overlay { shape.inset(by: 1.5).stroke(Color(hex: 0xFFFFED, opacity: 0.55), lineWidth: 3) }
        .overlay { shape.inset(by: 4).stroke(Color(hex: 0x2B5B49, opacity: 0.2), lineWidth: 2) }
        .overlay { shape.stroke(Color(hex: 0xE9E5C8), lineWidth: 1) }
        // The card's edge: two stacked lips, then its shadow on the table.
        .background { shape.fill(Color(hex: 0x243C34)).offset(y: 6) }
        .background { shape.fill(Color(hex: 0x6C8375)).offset(y: 3) }
        .shadow(color: .black.opacity(0.5), radius: 22, x: -tilt.y * 0.6, y: 22 + tilt.x * 0.6)
        .environment(\.colorScheme, .light)
    }

    private func field(_ title: String) -> some View {
        Text(title).font(.system(size: 10, design: .monospaced)).foregroundStyle(Self.label)
    }

    /// Their latest painting, tiled gray and faint, like the web card's wallpaper.
    @ViewBuilder private var paintings: some View {
        if let path = profile?.latestPainting?.file?.url, let url = URL(string: path, relativeTo: URL(string: "https://aesthetic.computer")!) {
            AsyncImage(url: url.absoluteURL) { image in
                GeometryReader { box in
                    let tile: CGFloat = 115
                    VStack(spacing: 0) {
                        ForEach(0..<Int(box.size.height / tile) + 1, id: \.self) { _ in
                            HStack(spacing: 0) {
                                ForEach(0..<Int(box.size.width / tile) + 1, id: \.self) { _ in
                                    image.interpolation(.none).resizable().scaledToFill().frame(width: tile, height: tile).clipped()
                                }
                            }
                        }
                    }
                }
                .grayscale(1).contrast(1.25).opacity(0.13).blendMode(.multiply)
            } placeholder: { Color.clear }
            .accessibilityHidden(true)
        }
    }

    /// A holographic band that slides across as the card tilts.
    private var sheen: some View {
        let shift = (tilt.y / 18 + tilt.x / 36).clamped(1)
        return LinearGradient(colors: [.clear, Color(hex: 0x67CFB9, opacity: 0.25), .white.opacity(0.55), Color(hex: 0xEEA4DC, opacity: 0.3), .clear],
                              startPoint: UnitPoint(x: -0.6 + shift, y: 0), endPoint: UnitPoint(x: 0.4 + shift, y: 1))
            .blendMode(.overlay)
            .allowsHitTesting(false)
    }

    // MARK: Under the card

    private var moderation: some View {
        VStack(alignment: .leading, spacing: 14) {
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
        .frame(maxWidth: 420, alignment: .leading)
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

/// The web card's ::before: thin white lines every 13 px at 115°.
private struct SecurityStripes: View {
    var body: some View {
        Canvas { context, size in
            let reach = size.width + size.height
            context.rotate(by: .degrees(25))
            var x = -reach
            while x < reach {
                context.fill(Path(CGRect(x: x, y: -reach, width: 1, height: reach * 2)), with: .color(.white.opacity(0.6)))
                x += 13
            }
        }
        .allowsHitTesting(false)
    }
}

/// The phone's lean, eased toward level, while the card is open.
@MainActor private final class Tilt: ObservableObject {
    @Published var roll = 0.0
    @Published var pitch = 0.0
    private let manager = CMMotionManager()
    private var rest: (roll: Double, pitch: Double)?

    func start() {
        guard manager.isDeviceMotionAvailable, !manager.isDeviceMotionActive else { return }
        manager.deviceMotionUpdateInterval = 1 / 30
        manager.startDeviceMotionUpdates(to: .main) { [weak self] data, _ in
            guard let self, let attitude = data?.attitude else { return }
            // Lean is measured from how the phone was held when the card opened.
            let base = self.rest ?? (attitude.roll, attitude.pitch)
            self.rest = base
            self.roll = (attitude.roll - base.roll).clamped(0.8)
            self.pitch = (attitude.pitch - base.pitch).clamped(0.8)
        }
    }

    func stop() { manager.stopDeviceMotionUpdates(); rest = nil }
}

private extension Double {
    func clamped(_ limit: Double) -> Double { Swift.min(limit, Swift.max(-limit, self)) }
}
