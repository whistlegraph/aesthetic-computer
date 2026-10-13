import SwiftUI

struct RoomView: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    @State private var sheet: Sheet?
    enum Sheet: String, Identifiable { case login, new, compose, account; var id: String { rawValue } }

    var body: some View {
        NavigationStack {
            ScrollView {
                LazyVStack(spacing: 16) {
                    if !account.signedIn {
                        Entry { door in sheet = door == .login ? .login : .new }
                    }
                    if let error = room.error {
                        Text(error).font(Theme.chrome(12, .regular)).foregroundStyle(Theme.ink)
                    }
                    ForEach(room.visible) { post in
                        NavigationLink(value: post) { PostRow(post: post, onTag: pick) }.buttonStyle(.plain)
                    }
                }
                .padding(.leading, 34).padding(.trailing, 14).padding(.vertical, 14)
            }
            .background(RuledPaper())
            .navigationBarTitleDisplayMode(.inline)
            .toolbarBackground(Theme.panel, for: .navigationBar).toolbarBackground(.visible, for: .navigationBar)
            .navigationDestination(for: Post.self) { ThreadView(rootID: $0.id) }
            .safeAreaInset(edge: .top, spacing: 0) { TagBar(selected: room.tag, tags: room.tags, onPick: pick) }
            .refreshable { await room.load() }
            .toolbar {
                ToolbarItem(placement: .principal) {
                    Image("Mark").renderingMode(.template).resizable().scaledToFit()
                        .frame(height: 30).foregroundStyle(Theme.ink).accessibilityLabel("choir")
                }
                if account.signedIn {
                    ToolbarItem(placement: .topBarLeading) {
                        Button(account.handle ?? "you") { sheet = .account }.font(Theme.chrome(13)).tint(Theme.ink)
                    }
                    ToolbarItem(placement: .topBarTrailing) {
                        Button { sheet = account.handle == nil ? .account : .compose } label: { Image(systemName: "square.and.pencil") }.tint(Theme.ink)
                            .accessibilityLabel("New post")
                    }
                }
            }
            .sheet(item: $sheet) { s in
                switch s {
                case .login: SignInView(door: .login)
                case .new: SignInView(door: .new)
                case .compose: ComposeView(onPosted: { Task { await room.load() } })
                case .account: AccountView()
                }
            }
            .task {
                await room.load()
                // Live sockets come later; a gentle poll while open.
                while !Task.isCancelled { try? await Task.sleep(for: .seconds(15)); await room.load() }
            }
        }
    }

    func pick(_ tag: String?) {
        room.tag = tag
        Trail.send("tag", ["tag": tag ?? "all"])
        Task { await room.load() }
    }
}

struct TagBar: View {
    let selected: String?
    let tags: [TagCount]
    let onPick: (String?) -> Void
    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 6) {
                Chip(label: "all", on: selected == nil) { onPick(nil) }
                ForEach(tags, id: \.tag) { t in
                    Chip(label: "#\(t.tag)", count: t.count, on: selected == t.tag) { onPick(t.tag) }
                }
            }
            .padding(.horizontal, 16).padding(.vertical, 8)
        }
        .background(Theme.panel)
        .overlay(alignment: .bottom) { Rectangle().fill(Theme.ink).frame(height: Theme.line) }
    }
}

struct Chip: View {
    let label: String
    var count = 0
    let on: Bool
    let action: () -> Void
    var body: some View {
        Button(action: action) {
            HStack(spacing: 4) {
                Text(label)
                if count > 0 { Text("\(count)").opacity(0.6) }
            }
        }
        .buttonStyle(DeskButtonStyle(on: on))
    }
}
