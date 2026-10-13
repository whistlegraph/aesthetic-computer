import SwiftUI

struct ThreadView: View {
    @EnvironmentObject var account: Account
    let rootID: String
    @State private var thread: API.Thread?
    @State private var error: String?
    @State private var reply = ""
    @State private var sending = false
    @State private var signIn = false

    var body: some View {
        ScrollView {
            LazyVStack(spacing: 16) {
                if let thread {
                    PostRow(post: thread.root, inThread: true, onChange: load)
                    ForEach(thread.replies) { PostRow(post: $0, inThread: true, onChange: load).padding(.leading, 18) }
                } else if let error {
                    Text(error).font(Theme.chrome(12, .regular))
                } else {
                    ProgressView().frame(maxWidth: .infinity)
                }
            }
            .padding(.leading, 34).padding(.trailing, 14).padding(.vertical, 14)
        }
        .background(RuledPaper())
        .toolbarBackground(Theme.panel, for: .navigationBar).toolbarBackground(.visible, for: .navigationBar)
        .navigationTitle("Thread")
        .navigationBarTitleDisplayMode(.inline)
        .refreshable { await fetch() }
        .task { Trail.send("thread:open"); await fetch() }
        .safeAreaInset(edge: .bottom) {
            HStack(alignment: .bottom) {
                if account.handle != nil {
                    TextField("Reply", text: $reply, axis: .vertical).lineLimit(1...5).textFieldStyle(.roundedBorder)
                    Button("Send") { Task { await send() } }.buttonStyle(DeskButtonStyle())
                        .disabled(reply.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty || sending)
                } else {
                    Button(account.signedIn ? "Choose your @name to reply" : "Log in to reply") { signIn = true }
                        .frame(maxWidth: .infinity)
                }
            }
            .padding(12).background(Theme.panel)
            .overlay(alignment: .top) { Rectangle().fill(Theme.ink).frame(height: Theme.line) }
        }
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
            reply = ""; await fetch()
        } catch { self.error = error.localizedDescription; Trail.send("post:failed", ["why": error.localizedDescription]) }
    }
}
