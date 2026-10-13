import SwiftUI

/// Invite friends: a name (so you remember who it's for), a code and a choir.ac
/// link to send however you like. Every member gets a few; Fía has as many as she wants.
struct InviteSection: View {
    @EnvironmentObject var account: Account
    @State private var list: API.Invites?
    @State private var name = ""
    @State private var made: API.Made?
    @State private var busy = false
    @State private var note: String?
    @State private var code = ""

    var body: some View {
        Section {
            TextField("Who's it for? (optional)", text: $name).textInputAutocapitalization(.words)
            Button(busy ? "Making…" : "Make an invite") { Task { await make() } }
                .disabled(busy || list?.left == 0)
            if let made, let url = URL(string: made.url) {
                ShareLink(item: url, message: Text("Come join me on choir 🌼 Your invite code is \(made._id)")) {
                    Label("Send invite \(made._id)", systemImage: "square.and.arrow.up")
                }
            }
            ForEach(list?.invites ?? []) { i in
                HStack {
                    Text(i.code).font(.system(.body, design: .monospaced))
                    if let n = i.name { Text(n).foregroundStyle(.secondary) }
                    Spacer()
                    if let j = i.joined { Text("joined as \(j)").font(.caption).foregroundStyle(.green) }
                    else if let url = URL(string: i.url) { ShareLink(item: url) { Image(systemName: "square.and.arrow.up") }.buttonStyle(.borderless) }
                }
            }
            if let note { Text(note).font(.footnote).foregroundStyle(.secondary) }
        } header: {
            Text("Invite")
        } footer: {
            if let left = list?.left { Text(left == 0 ? "You've sent all your invites." : "\(left) \(left == 1 ? "invite" : "invites") left.") }
        }
        Section {
            HStack {
                TextField("Got an invite code?", text: $code).textInputAutocapitalization(.characters).autocorrectionDisabled()
                Button("Use") { Task { await claim() } }.disabled(code.count < 6)
            }
        }
        .task { await load() }
    }

    func load() async { list = try? await API.invites(token: await account.token()) }
    func make() async {
        busy = true; defer { busy = false }
        do {
            made = try await API.invite(name: name, token: await account.token())
            name = ""; note = nil; Trail.send("invite:made")
            await load()
        } catch { note = error.localizedDescription }
    }
    func claim() async {
        do {
            let c = try await API.claim(code, token: await account.token())
            note = "Invited by \(c.from) 🌼"; code = ""
        } catch { note = error.localizedDescription }
    }
}
