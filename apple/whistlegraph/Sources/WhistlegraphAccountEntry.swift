import SwiftUI

enum AccountEntryStatus: String { case checking, signedOut, needsHandle, ready, failed }

struct WhistlegraphAccountEntry: View {
    @ObservedObject var session: WhistlegraphSession
    @Environment(\.colorScheme) private var scheme
    @Environment(\.openURL) private var openURL

    private func action(_ title: String, signUp: Bool = false, perform: @escaping () -> Void) -> some View {
        Button(action: perform) {
            Text(title).font(.system(size: 20, design: .monospaced))
                .multilineTextAlignment(.center).fixedSize(horizontal: false, vertical: true)
        }.buttonStyle(CurtainAccountButtonStyle(signUp: signUp))
    }

    var body: some View {
        let theme = WhistlegraphTheme(phase: .ready, dark: scheme == .dark)
        GeometryReader { geometry in
            ScrollView {
                VStack(spacing: 30) {
                    ComicTitle(text: "whistlegraph", size: 36)
                    VStack(spacing: 18) {
                        if let failure = session.startupFailure {
                            Text(failure).multilineTextAlignment(.center)
                            action("Reload") { session.reloadWorkspace() }
                        } else if !session.workspaceReady || session.accountStatus == .checking {
                            ProgressView(session.workspaceReady ? "Checking your account…" : "Opening…")
                                .accessibilityIdentifier("account-entry-progress")
                        } else if session.accountStatus == .needsHandle {
                            action("Choose your @handle") { openURL(URL(string: "https://aesthetic.computer/handle")!) }
                            Button("I’ve chosen a handle") { session.restoreAccount() }
                            Button("Use another account") { session.signOut() }
                        } else {
                            HStack(spacing: 12) {
                                action("Log in") { session.signIn() }.accessibilityIdentifier("account-entry-login")
                                action("I'm new", signUp: true) { session.signIn(signUp: true) }.accessibilityIdentifier("account-entry-signup")
                            }
                            // Logging in is the AI permission (App Review 5.1.2(i)); the policy names who sees what.
                            Text("By continuing, what you say, type and draw goes to AI helpers. [Privacy](https://aesthetic.computer/privacy-policy.html)")
                                .font(.custom("ComicRelief-Regular", size: 14, relativeTo: .footnote))
                                .multilineTextAlignment(.center).tint(theme.foreground)
                                .accessibilityIdentifier("account-entry-ai-line")
                            if session.accountStatus == .failed {
                                Button("Retry account verification") { session.restoreAccount() }
                                    .accessibilityIdentifier("account-entry-retry")
                            }
                        }
                        if !session.accountNotice.isEmpty {
                            Text(session.accountNotice).font(.body).multilineTextAlignment(.center)
                                .accessibilityIdentifier("account-entry-notice")
                        }
                    }
                }
                .frame(maxWidth: 330).padding(24)
                .frame(maxWidth: .infinity, minHeight: geometry.size.height)
            }.scrollIndicators(.hidden)
        }
        .foregroundStyle(theme.foreground).background(theme.background.ignoresSafeArea())
        .accessibilityIdentifier("account-entry")
    }
}

extension WhistlegraphSession {
    var accountReady: Bool { accountStatus == .ready }
    func signIn(signUp: Bool = false) {
        accountNotice = ""
        account.signIn(from: webView, signUp: signUp) { [weak self] result in
            guard let self else { return }
            switch result {
            case .success(let token):
                self.aiConsent.bind(subject: nil, handle: "")
                self.accountStatus = .checking
                self.emitEngine(["kind": "account", "token": token])
            case .failure(let error): self.accountNotice = error.localizedDescription
            }
        }
    }
    func restoreAccount(force: Bool = true) {
        if force || !accountReady { accountStatus = .checking; accountNotice = "" }
        let generation = account.generation
        Task {
            do {
                let token = try await account.token() ?? ""
                guard generation == account.generation else { return }
                emitEngine(["kind": "account", "token": token, "retry": force])
            } catch {
                guard generation == account.generation else { return }
                DeviceActionLog.shared.recordError(.accountToken, error)
                emitEngine(["kind": "account", "token": "", "notice": error.localizedDescription])
            }
        }
    }
}
