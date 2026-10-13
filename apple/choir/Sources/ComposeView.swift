import SwiftUI

struct ComposeView: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    @Environment(\.dismiss) private var dismiss
    var onPosted: () -> Void
    @State private var text = ""
    @State private var picked: [String] = []
    @State private var newTag = ""
    @State private var error: String?
    @State private var sending = false
    @FocusState private var focused: Bool
    static let minTags = 2

    var all: [String] { (room.tags.map(\.tag) + picked).reduce(into: []) { if !$0.contains($1) { $0.append($1) } } }
    var ready: Bool { !text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty && picked.count >= Self.minTags && !sending }

    var body: some View {
        NavigationStack {
            ScrollView {
                VStack(alignment: .leading, spacing: 18) {
                    PadPage {
                        TextField("Make notes on culture", text: $text, axis: .vertical)
                            .font(Theme.type(18)).lineSpacing(9).lineLimit(6...14)
                            .foregroundStyle(Theme.ink).focused($focused)
                    }
                    VStack(alignment: .leading, spacing: 10) {
                        Text(picked.count >= Self.minTags ? "\(picked.count) tags" : "Tag it at least \(Self.minTags) ways")
                            .font(Theme.type(13)).foregroundStyle(Theme.ink.opacity(0.7))
                        FlowTags(tags: all, picked: picked) { t in
                            if let i = picked.firstIndex(of: t) { picked.remove(at: i) } else { picked.append(t) }
                        }
                        HStack {
                            TextField("new tag", text: $newTag).font(Theme.type(15))
                                .textInputAutocapitalization(.never).autocorrectionDisabled().onSubmit(addTag)
                                .padding(.horizontal, 12).padding(.vertical, 8).glass(Capsule())
                            Button("Add", action: addTag).buttonStyle(GlassChipStyle()).disabled(Tag.normalize(newTag).isEmpty)
                        }
                        if let error { Text(error).font(Theme.type(14)).foregroundStyle(Theme.head) }
                    }
                }
                .padding(16)
            }
            .onPad()
            .navigationTitle("New post")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) { Button("Cancel") { dismiss() } }
                ToolbarItem(placement: .confirmationAction) { Button("Post") { Task { await send() } }.disabled(!ready) }
            }
            .onAppear { focused = true; if let t = room.tag, !picked.contains(t) { picked.append(t) } }
        }
    }

    func addTag() {
        let t = Tag.normalize(newTag)
        guard !t.isEmpty else { return }
        if !picked.contains(t) { picked.append(t) }
        newTag = ""
    }

    func send() async {
        sending = true; defer { sending = false }
        do {
            _ = try await API.post(.init(text: text, tags: picked, replyTo: nil), token: await account.token())
            onPosted(); dismiss()
        } catch {
            self.error = error.localizedDescription
            Trail.send("post:failed", ["why": error.localizedDescription])
        }
    }
}

/// Tags that wrap onto as many lines as they need.
struct FlowTags: View {
    let tags: [String]
    let picked: [String]
    let toggle: (String) -> Void
    var body: some View {
        WrapLayout(spacing: 6) {
            ForEach(tags, id: \.self) { t in Chip(label: "#\(t)", on: picked.contains(t)) { toggle(t) } }
        }
        .padding(.vertical, 4)
    }
}

struct WrapLayout: Layout {
    var spacing: CGFloat = 6
    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) -> CGSize {
        rows(proposal.width ?? .infinity, subviews).size
    }
    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) {
        for (i, p) in rows(bounds.width, subviews).points.enumerated() {
            subviews[i].place(at: CGPoint(x: bounds.minX + p.x, y: bounds.minY + p.y), proposal: .unspecified)
        }
    }
    private func rows(_ width: CGFloat, _ subviews: Subviews) -> (points: [CGPoint], size: CGSize) {
        var x: CGFloat = 0, y: CGFloat = 0, line: CGFloat = 0, widest: CGFloat = 0
        var points: [CGPoint] = []
        for v in subviews {
            let s = v.sizeThatFits(.unspecified)
            if x > 0 && x + s.width > width { x = 0; y += line + spacing; line = 0 }
            points.append(CGPoint(x: x, y: y))
            x += s.width + spacing; line = max(line, s.height); widest = max(widest, x - spacing)
        }
        return (points, CGSize(width: widest, height: y + line))
    }
}
