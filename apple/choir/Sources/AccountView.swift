import SwiftUI

struct AccountView: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    @Environment(\.dismiss) private var dismiss
    @State private var confirmDelete = false
    @State private var deleting = false
    @State private var note: String?

    var body: some View {
        NavigationStack {
            Form {
                Section {
                    if let handle = account.handle {
                        LabeledContent("Posting as", value: handle)
                    } else {
                        Text("You're signed in but don't have a name yet.")
                        Link("Choose your @name", destination: URL(string: "https://aesthetic.computer/handle")!)
                        Button("I chose one") { Task { await account.refreshMe() } }
                    }
                }
                if account.handle != nil { InviteSection() }
                Section("Notifications") {
                    Toggle("New messages in the chat", isOn: Binding(get: { account.notifyChat }, set: { on in Task { await account.setNotifications(.chat, on) } }))
                    Toggle("New in Discover", isOn: Binding(get: { account.notifyDiscover }, set: { on in Task { await account.setNotifications(.discover, on) } }))
                }
                if !room.hidden.isEmpty {
                    Section { Button("Show \(room.hidden.count) hidden \(room.hidden.count == 1 ? "person" : "people") again") { room.unhideAll() } }
                }
                Section {
                    Button("Sign out") { account.signOut(); dismiss() }
                    Button("Delete account", role: .destructive) { confirmDelete = true }.disabled(deleting)
                }
                if let note { Section { Text(note).foregroundStyle(.secondary) } }
            }
            .onPad()
            .navigationTitle("You")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
            .confirmationDialog("Delete your account?", isPresented: $confirmDelete, titleVisibility: .visible) {
                Button("Delete", role: .destructive) { Task { await delete() } }
            } message: {
                Text("This locks the account you signed in with and deletes it after a grace period.")
            }
        }
    }

    func delete() async {
        deleting = true; defer { deleting = false }
        do {
            let s = try await AccountDeletion.delete(token: await account.token() ?? "")
            Trail.send("account:delete")
            note = s.date.map { "Your account will be deleted on \($0.formatted(date: .long, time: .omitted))." } ?? "Your account is scheduled for deletion."
            account.signOut()
        } catch { note = error.localizedDescription }
    }
}
