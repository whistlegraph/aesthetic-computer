import SwiftUI

struct SignInView: View {
    @EnvironmentObject var account: Account
    @Environment(\.dismiss) private var dismiss
    @State private var email = ""
    @State private var code = ""
    @State private var sent = false
    @State private var busy = false
    @State private var error: String?
    @FocusState private var field: Field?
    enum Field { case email, code }

    var body: some View {
        NavigationStack {
            Form {
                Section {
                    if !sent {
                        TextField("you@gallery.org", text: $email)
                            .keyboardType(.emailAddress).textContentType(.emailAddress)
                            .textInputAutocapitalization(.never).autocorrectionDisabled()
                            .focused($field, equals: .email).onSubmit { Task { await send() } }
                    } else {
                        TextField("123456", text: $code)
                            .keyboardType(.numberPad).textContentType(.oneTimeCode)
                            .focused($field, equals: .code)
                            .onChange(of: code) { _, c in if c.filter(\.isNumber).count == 6 { Task { await verify() } } }
                    }
                } footer: {
                    Text(sent ? "We sent six digits to \(email)." : "No password. We email you a six digit code.")
                }
                if let error { Section { Text(error).foregroundStyle(.red) } }
                Section {
                    Button(sent ? "Enter" : "Send code") { Task { sent ? await verify() : await send() } }
                        .disabled(busy || (sent ? code.filter(\.isNumber).count < 6 : !email.contains("@")))
                    if sent { Button("Use another email") { sent = false; code = ""; error = nil; field = .email } }
                }
                if busy { ProgressView().frame(maxWidth: .infinity) }
            }
            .navigationTitle("Sign in")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .cancellationAction) { Button("Cancel") { dismiss() } } }
            .onAppear { field = .email }
        }
    }

    func send() async {
        busy = true; error = nil; defer { busy = false }
        do { try await account.sendCode(to: email.trimmingCharacters(in: .whitespaces)); sent = true; field = .code }
        catch { self.error = error.localizedDescription }
    }
    func verify() async {
        guard !busy else { return }
        busy = true; error = nil; defer { busy = false }
        do { try await account.verify(email: email.trimmingCharacters(in: .whitespaces), code: code.filter(\.isNumber)); dismiss() }
        catch { self.error = error.localizedDescription; code = "" }
    }
}
