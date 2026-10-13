import SwiftUI

/// One of Fía's hand drawings from Art/fia-2026-10-12 — her words (send,
/// reply, chat, discover), doodads and daisies — tinted like text.
struct Drawn: View {
    let name: String
    var height: CGFloat = 18
    var label: String? = nil
    var body: some View {
        Image(name).renderingMode(.template).resizable().scaledToFit().frame(height: height)
            .accessibilityLabel(label ?? name)
    }
}

/// Which mark sits at the top of the room. The four singing Cs are choir's
/// mark today; the daisies are Fía's new ones, here to be tried in place.
enum Logo: String, CaseIterable, Identifiable {
    case cs, loop, petal
    var id: String { rawValue }
    var title: String {
        switch self {
        case .cs: "Four Cs"
        case .loop: "Loop daisy"
        case .petal: "Petal daisy"
        }
    }
}

/// The mark pinned to the top centre of the room, the way Instagram's sits.
struct ChoirLogo: View {
    @AppStorage("logo") private var logo = Logo.cs.rawValue
    var height: CGFloat = 30
    var body: some View {
        switch Logo(rawValue: logo) ?? .cs {
        case .cs: SingingMark(height: height, energy: 0.5)
        case .loop: Drawn(name: "DaisyLoop", height: height, label: "choir")
        case .petal: Drawn(name: "DaisyPetal", height: height, label: "choir")
        }
    }
}

/// A few of Fía's doodads lying faintly on the desk, behind the cards.
struct DeskDoodads: View {
    private let marks: [(name: String, x: CGFloat, y: CGFloat, size: CGFloat, turn: Double)] = [
        ("DaisyPetal", 0.08, 0.16, 54, -12), ("GlyphAsterisk", 0.9, 0.1, 26, 8),
        ("DaisyLoop", 0.88, 0.46, 60, 14), ("GlyphAsterisk", 0.12, 0.62, 20, -20),
        ("DaisyPetal", 0.84, 0.86, 44, 22), ("DaisyLoop", 0.14, 0.92, 40, -6),
    ]
    var body: some View {
        GeometryReader { box in
            ForEach(marks.indices, id: \.self) { i in
                let m = marks[i]
                Drawn(name: m.name, height: m.size).rotationEffect(.degrees(m.turn))
                    .position(x: box.size.width * m.x, y: box.size.height * m.y)
            }
        }
        .foregroundStyle(Theme.ink.opacity(0.07))
        .allowsHitTesting(false).accessibilityHidden(true)
    }
}

/// The news, to scroll: every post that carries a link, newest first.
struct DiscoverView: View {
    @EnvironmentObject var room: Room
    @Environment(\.dismiss) private var dismiss
    var body: some View {
        NavigationStack {
            ScrollView {
                LazyVStack(spacing: 14) {
                    ForEach(room.visible.filter { !$0.links.isEmpty }) { post in
                        if let link = post.links.first { HeadlineCard(link: link, words: post.text.replacingOccurrences(of: link, with: "").trimmingCharacters(in: .whitespacesAndNewlines), large: true) }
                    }
                }
                .padding(14)
            }
            .background(RuledPaper())
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .principal) { Drawn(name: "WordDiscover", height: 22, label: "discover") }
                ToolbarItem(placement: .topBarLeading) {
                    Button { dismiss() } label: { Drawn(name: "WordChat", height: 20, label: "back to chat") }
                }
            }
            .task { Trail.send("discover:open") }
        }
        .tint(Theme.ink)
    }
}
