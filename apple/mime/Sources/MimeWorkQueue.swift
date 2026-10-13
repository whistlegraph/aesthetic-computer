import SwiftUI

/// Public remix references and new-work intents survive leaving the feed or
/// relaunching. Server generation itself stays in the existing turn queue.
@MainActor final class MimeWorkQueue: ObservableObject {
    struct Item: Codable, Identifiable {
        var id = UUID()
        let post: Post?
        let ware: String
        let owner: String
        var issue: String?
        var title: String { post.map { "\($0.command) · v\($0.edition ?? 0)" } ?? "New \(Ware(rawValue: ware)?.fileExtension ?? ware)" }
    }
    @Published private(set) var items: [Item] = []
    private let key = "mime-work-queue-v1"
    private let persistent = ProcessInfo.processInfo.environment["MIME_CREATION_FIXTURE"] == nil
    init() {
        if persistent, let data = UserDefaults.standard.data(forKey: key), let saved = try? JSONDecoder().decode([Item].self, from: data) { items = saved }
    }
    func enqueue(post: Post? = nil, ware: Ware, owner: String) -> Item {
        if let post, let existing = items.first(where: { $0.owner == owner && $0.post?.id == post.id && $0.post?.edition == post.edition }) { return existing }
        let item = Item(post: post, ware: ware.rawValue, owner: owner)
        items.append(item); save(); return item
    }
    func available(to owner: String) -> [Item] { items.filter { $0.owner.isEmpty || $0.owner == owner } }
    func remove(_ id: UUID) { items.removeAll { $0.id == id }; save() }
    func setIssue(_ issue: String?, for id: UUID) {
        guard let index = items.firstIndex(where: { $0.id == id }) else { return }
        items[index].issue = issue; save()
    }
    private func save() {
        if persistent, let data = try? JSONEncoder().encode(items) { UserDefaults.standard.set(data, forKey: key) }
    }
}

struct MimeWorkQueueSheet: View {
    let items: [MimeWorkQueue.Item]
    let open: (MimeWorkQueue.Item) -> Void
    let remove: (UUID) -> Void
    @Environment(\.dismiss) private var dismiss
    var body: some View {
        NavigationStack {
            List(items) { item in
                Button { dismiss(); open(item) } label: {
                    VStack(alignment: .leading, spacing: 4) {
                        Text(item.title).font(.custom("ComicRelief-Bold", size: 20))
                        if let issue = item.issue { Text(issue).font(.footnote).foregroundStyle(.secondary) }
                    }
                }.accessibilityIdentifier("queued-work-\(item.post?.id ?? item.ware)")
                    .swipeActions { Button("Remove", role: .destructive) { remove(item.id) } }
            }
            .navigationTitle("Queued").navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
        }.presentationDetents([.medium, .large])
    }
}
