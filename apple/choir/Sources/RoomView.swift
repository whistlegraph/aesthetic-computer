import SwiftUI

/// The main thing is a chat: oldest at the top, newest at the bottom, the
/// input always there. Tapping a message opens its thread.
struct RoomView: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    @State private var sheet: Sheet?
    enum Sheet: String, Identifiable { case login, new, account; var id: String { rawValue } }

    var body: some View {
        NavigationStack {
            ScrollViewReader { scroller in
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 10) {
                        SingingMark(height: account.signedIn ? 56 : 84, energy: account.signedIn ? 0.5 : 1)
                            .frame(maxWidth: .infinity).onTapGesture { pick(nil) }
                        if !account.signedIn {
                            Text("Comments on culture").font(Theme.type(15)).foregroundStyle(Theme.ink.opacity(0.75))
                                .frame(maxWidth: .infinity)
                        }
                        if let tag = room.tag {
                            Button { pick(nil) } label: {
                                HStack(spacing: 6) { Text("#\(tag)"); Image(systemName: "xmark").font(.caption2.bold()) }
                            }
                            .buttonStyle(GlassChipStyle(on: true)).frame(maxWidth: .infinity)
                        }
                        if let error = room.error {
                            Text(error).font(Theme.type(12)).foregroundStyle(Theme.ink)
                        }
                        ForEach(room.chat) { post in
                            if Samples.isSample(post) {
                                ChatRow(post: post, mine: false, onTag: pick).opacity(0.85)
                            } else {
                                NavigationLink(value: post) {
                                    ChatRow(post: post, mine: post.handle == account.handle, onTag: pick)
                                }
                                .buttonStyle(.plain)
                            }
                        }
                        Color.clear.frame(height: 1).id("bottom")
                    }
                    .padding(.horizontal, 14).padding(.vertical, 12)
                }
                .defaultScrollAnchor(.bottom)
                .background(RuledPaper())
                .navigationBarTitleDisplayMode(.inline)
                .navigationDestination(for: Post.self) { ThreadView(rootID: $0.id) }
                .refreshable { await room.load() }
                .onChange(of: room.chat.last?.id) { _, _ in withAnimation { scroller.scrollTo("bottom", anchor: .bottom) } }
                .safeAreaInset(edge: .bottom, spacing: 0) {
                    if account.handle != nil {
                        ChatInput(onSent: { Task { await room.load(); withAnimation { scroller.scrollTo("bottom", anchor: .bottom) } } })
                    } else {
                        HStack(spacing: 10) {
                            if account.signedIn {
                                Button("Choose your @name") { sheet = .account }.buttonStyle(CuteDoorStyle(small: true))
                            } else {
                                Button("Log in") { sheet = .login }.buttonStyle(CuteDoorStyle(small: true))
                                Button("I'm new") { sheet = .new }.buttonStyle(CuteDoorStyle(new: true, small: true))
                            }
                        }
                        .frame(maxWidth: .infinity).padding(.vertical, 12).padding(.bottom, 4)
                        .glass(Rectangle())
                    }
                }
            }
            .toolbar {
                if account.signedIn {
                    ToolbarItem(placement: .topBarLeading) {
                        Button(account.handle ?? "you") { sheet = .account }.font(Theme.type(14)).tint(Theme.ink)
                    }
                }
            }
            .sheet(item: $sheet) { s in
                switch s {
                case .login: SignInView(door: .login)
                case .new: SignInView(door: .new)
                case .account: AccountView()
                }
            }
            .task {
                await room.load()
                // Live sockets come later; a quick poll while the chat is open.
                while !Task.isCancelled { try? await Task.sleep(for: .seconds(5)); await room.load() }
            }
        }
    }

    func pick(_ tag: String?) {
        room.tag = tag
        Trail.send("tag", ["tag": tag ?? "all"])
        Task { await room.load() }
    }
}

/// One chat message: name and time, the words, a link card, its tags and replies.
struct ChatRow: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    let post: Post
    let mine: Bool
    var onTag: (String?) -> Void
    @State private var confirmDelete = false

    var body: some View {
        VStack(alignment: mine ? .trailing : .leading, spacing: 3) {
            HStack(spacing: 6) {
                if post.handle == "@choir" {
                    Image("Mark").renderingMode(.template).resizable().scaledToFit().frame(height: 14).accessibilityLabel("choir")
                } else {
                    Text(post.handle).font(.system(size: 12, weight: .semibold))
                }
                Text(post.date, format: .relative(presentation: .numeric, unitsStyle: .narrow)).font(Theme.type(10)).opacity(0.45)
            }
            .foregroundStyle(Theme.ink)
            VStack(alignment: .leading, spacing: 8) {
                if post.deleted {
                    Text("deleted").italic().foregroundStyle(.secondary)
                } else {
                    let words = shownText
                    if !words.isEmpty {
                        Text(linked(words)).font(.system(size: 16, design: .serif)).lineSpacing(2)
                            .foregroundStyle(Theme.ink).textSelection(.enabled)
                    }
                    if let link = post.links.first, let url = URL(string: link) { LinkPreview(url: url).frame(minHeight: 56) }
                }
                HStack(spacing: 8) {
                    ForEach(post.tags, id: \.self) { t in
                        Button("#\(t)") { onTag(t) }.buttonStyle(.plain).font(Theme.type(11)).foregroundStyle(Theme.ink.opacity(0.5))
                    }
                    if post.replies > 0 {
                        Text("\(post.replies) \(post.replies == 1 ? "reply" : "replies")").font(Theme.type(11).bold()).foregroundStyle(Theme.ink.opacity(0.7))
                    }
                }
            }
            .padding(.horizontal, 12).padding(.vertical, 9)
            .background(mine ? Color(red: 0.86, green: 0.91, blue: 1.0) : Theme.paper, in: RoundedRectangle(cornerRadius: 14))
            .shadow(color: .black.opacity(0.06), radius: 4, y: 2)
            .frame(maxWidth: 320, alignment: mine ? .trailing : .leading)
        }
        .frame(maxWidth: .infinity, alignment: mine ? .trailing : .leading)
        .contextMenu {
            if !post.deleted, let url = post.links.first.flatMap(URL.init) { ShareLink(item: url) }
            if mine && !post.deleted {
                Button("Delete", systemImage: "trash", role: .destructive) { confirmDelete = true }
            } else if !Samples.isSample(post) {
                Button("Hide \(post.handle)", systemImage: "eye.slash") { room.hide(post.handle) }
                Link(destination: PostRow(post: post).report) { Label("Report", systemImage: "flag") }
            }
        }
        .confirmationDialog("Delete this message?", isPresented: $confirmDelete) {
            Button("Delete", role: .destructive) {
                Task { try? await API.delete(post.id, token: await account.token()); Trail.send("post:delete"); await room.load() }
            }
        }
    }

    var shownText: String {
        guard let first = post.links.first else { return post.text }
        return post.text.replacingOccurrences(of: first, with: "").trimmingCharacters(in: .whitespacesAndNewlines)
    }
    func linked(_ text: String) -> AttributedString { PostRow(post: post).linked(text) }
}

/// The always-there input. Tags are optional and stick between messages.
struct ChatInput: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    var onSent: () -> Void
    @AppStorage("chatTags") private var stored = ""
    @State private var text = ""
    @State private var newTag = ""
    @State private var adding = false
    @State private var error: String?
    @State private var sending = false
    static let minTags = 0

    var picked: [String] { stored.split(separator: ",").map(String.init) }
    func toggle(_ t: String) {
        var p = picked
        if let i = p.firstIndex(of: t) { p.remove(at: i) } else { p.append(t) }
        stored = p.joined(separator: ",")
    }
    var all: [String] { (room.tags.map(\.tag) + picked).reduce(into: []) { if !$0.contains($1) { $0.append($1) } } }
    var ready: Bool { !text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty && picked.count >= Self.minTags && !sending }

    var body: some View {
        VStack(spacing: 8) {
            ScrollView(.horizontal, showsIndicators: false) {
                HStack(spacing: 6) {
                    ForEach(all, id: \.self) { t in
                        Button("#\(t)") { toggle(t) }.buttonStyle(GlassChipStyle(on: picked.contains(t)))
                    }
                    if adding {
                        TextField("tag", text: $newTag).font(Theme.type(13)).frame(width: 90)
                            .textInputAutocapitalization(.never).autocorrectionDisabled()
                            .onSubmit { let t = Tag.normalize(newTag); if !t.isEmpty && !picked.contains(t) { toggle(t) }; newTag = ""; adding = false }
                            .padding(.horizontal, 10).padding(.vertical, 6).glass(Capsule())
                    } else {
                        Button { adding = true } label: { Image(systemName: "plus") }.buttonStyle(GlassChipStyle())
                    }
                }
                .padding(.horizontal, 14)
            }
            if let error { Text(error).font(Theme.type(12)).foregroundStyle(Theme.head).padding(.horizontal, 14) }
            HStack(alignment: .bottom, spacing: 8) {
                TextField("Comments on culture", text: $text, axis: .vertical)
                    .font(.system(size: 16, design: .serif)).lineLimit(1...5)
                    .padding(.horizontal, 14).padding(.vertical, 10).glass(RoundedRectangle(cornerRadius: 20))
                Button { Task { await send() } } label: { Image(systemName: "arrow.up").font(.body.bold()) }
                    .buttonStyle(GlassChipStyle(on: true)).disabled(!ready).opacity(ready ? 1 : 0.4)
            }
            .padding(.horizontal, 12)
        }
        .padding(.top, 8).padding(.bottom, 6)
    }

    func send() async {
        sending = true; defer { sending = false }
        do {
            _ = try await API.post(.init(text: text, tags: picked, replyTo: nil), token: await account.token())
            text = ""; error = nil; Trail.send("chat:send"); onSent()
        } catch { self.error = error.localizedDescription; Trail.send("post:failed", ["why": error.localizedDescription]) }
    }
}
