import SwiftUI

struct ThreadView: View {
    @EnvironmentObject var account: Account
    let rootID: String
    var expand: () -> Void = {}   // grow the sheet to full height while typing
    @FocusState private var typing: Bool
    @State private var thread: API.Thread?
    @State private var error: String?
    @State private var reply = ""
    @State private var sending = false
    @State private var signIn = false

    var body: some View {
      VStack(spacing: 0) {
        ScrollView {
            LazyVStack(spacing: 16) {
                if let thread {
                    if let link = thread.root.links.first, InstagramCard.isInstagram(link) {
                        InstagramCard(link: link, large: true)
                            .padding(18).background(Theme.paper, in: RoundedRectangle(cornerRadius: 10))
                    } else if let link = thread.root.links.first {
                        HeadlineCard(link: link, words: PostRow(post: thread.root).shownText, large: true, stored: thread.root.preview, opportunity: thread.root.kind == "opportunity", opening: thread.root.kind == "opening")
                            .padding(18).background(Theme.paper, in: RoundedRectangle(cornerRadius: 10))
                        Text("\(thread.replies.count) \(thread.replies.count == 1 ? "comment" : "comments")")
                            .font(Theme.type(13)).foregroundStyle(Theme.ink.opacity(0.6)).frame(maxWidth: .infinity, alignment: .leading)
                    } else {
                        PostRow(post: thread.root, inThread: true, onChange: load)
                        if let files = thread.root.attachments, !files.isEmpty { AttachmentsView(items: files) }
                    }
                    ForEach(thread.replies) { ChatRow(post: $0, mine: $0.handle == account.handle, onTag: { _ in }) }
                } else if let error {
                    Text(error).font(Theme.chrome(12, .regular))
                } else {
                    ProgressView().frame(maxWidth: .infinity)
                }
            }
            .padding(.horizontal, 16).padding(.vertical, 16)
        }
        .scrollDismissesKeyboard(.interactively)
        .refreshable { await fetch() }

        // A real bottom bar, so the keyboard pushes it up.
            HStack(alignment: .bottom) {
                if account.handle != nil {
                    TextField("Add a comment", text: $reply, axis: .vertical).lineLimit(1...6).font(Theme.type(16))
                        .focused($typing)
                        .onChange(of: typing) { _, on in if on { expand() } }
                        .padding(.horizontal, 12).padding(.vertical, 8).glass(RoundedRectangle(cornerRadius: 18))
                    let ready = !reply.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty && !sending
                    Button { Task { await send() } } label: { Hand("HandReply", height: 22) }.buttonStyle(SendStyle(ready: ready))
                        .accessibilityLabel("Reply").disabled(!ready)
                } else {
                    Button(account.signedIn ? "Choose your @name to reply" : "Log in to reply") { signIn = true }
                        .frame(maxWidth: .infinity)
                }
            }
            .padding(12)
      }
        .navigationTitle("Comments")
        .navigationBarTitleDisplayMode(.inline)
        .task { Trail.send("thread:open"); await fetch() }
        .sheet(isPresented: $signIn) { if account.signedIn { AccountView() } else { SignInView(door: .login) } }
    }

    func load() { Task { await fetch() } }
    func fetch() async {
        do { thread = try await API.thread(rootID); error = nil } catch { self.error = error.localizedDescription }
    }
    func send() async {
        sending = true; defer { sending = false }
        do {
            _ = try await API.post(.init(text: reply, tags: [], replyTo: rootID), token: await account.token())
            reply = ""; typing = true; await fetch()
        } catch { self.error = error.localizedDescription; Trail.send("post:failed", ["why": error.localizedDescription]) }
    }
}
