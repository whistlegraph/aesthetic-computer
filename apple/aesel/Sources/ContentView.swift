import SwiftUI
import WebKit

struct ContentView: View {
    let session: Session
    let host: SessionHost
    @State private var draft = ""
    @State private var showHome = true
    @State private var showHelp = false
    @State private var showSettings = false
    private enum SettingsDestination { case signIn, help }
    @State private var settingsDestination: SettingsDestination?
    @State private var expandedPreview = false
    @State private var previewHidden = false
    @FocusState private var writing: Bool
    @Environment(\.openURL) private var openURL

    var body: some View {
        VStack(spacing: 0) {
            if !expandedPreview { title }
            if session.previewURL != nil && !previewHidden { preview }
            if !expandedPreview {
                transcript
                shelf
            }
        }
        .overlay {
            if showHome {
                AeselHomeView(session: session, host: host) { showHome = false }
            }
        }
        .font(Paint.font())
        .foregroundStyle(Paint.ink)
        .background { AeselCloth().ignoresSafeArea() }
        .background(HostCarrier(host: host).frame(width: 0, height: 0))
        .onChange(of: session.currentSessionID) { draft = ""; previewHidden = false; expandedPreview = false }
        .onAppear {
            #if DEBUG
            if ProcessInfo.processInfo.environment["AESEL_NOTEBOOK_PREVIEW"] == "1" {
                showHome = false
                showSettings = ProcessInfo.processInfo.environment["AESEL_SETTINGS_PREVIEW"] == "1"
            }
            #endif
        }
        .sheet(isPresented: $showHelp) { help }
        .sheet(isPresented: $showSettings, onDismiss: {
            let destination = settingsDestination
            settingsDestination = nil
            switch destination {
            case .signIn: host.signIn()
            case .help: showHelp = true
            case nil: break
            }
        }) { settings }
        .sheet(isPresented: Binding(get: { session.showSignIn }, set: { session.showSignIn = $0 })) {
            VStack(spacing: 0) {
                HStack {
                    AeselWordmark(text: "Aesel")
                    Spacer()
                    Button("/close") { session.showSignIn = false }
                        .font(Paint.font()).foregroundStyle(Paint.dim)
                }.padding(16).background(Paint.bg)
                ZStack {
                    SignInCarrier(host: host)
                    if session.signInLoading {
                        ProgressView("Opening AC sign-in…")
                            .font(Paint.font()).tint(Paint.ac)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                            .background(Paint.bg)
                    }
                    if let error = session.signInError {
                        VStack(spacing: 18) {
                            Text(error).multilineTextAlignment(.center)
                            Button("/retry") { host.signIn() }.foregroundStyle(Paint.you)
                        }
                        .font(Paint.font()).padding(24)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                        .background(Paint.bg)
                    }
                }
            }
        }

    }

    private var title: some View {
        HStack(spacing: 12) {
            Button {
                if let url = session.shareURL { openURL(url) }
            } label: {
                AeselTitle(text: session.route.isEmpty ? "new piece" : session.route,
                           colors: session.handleColors, status: session.status)
                    .lineLimit(1).minimumScaleFactor(0.6)
            }
            .buttonStyle(.plain)
            .disabled(session.shareURL == nil)
            .accessibilityLabel("Open piece in browser")
            Spacer(minLength: 0)
        }
        .padding(.horizontal, 16).padding(.vertical, 8)
    }

    private var preview: some View {
        ZStack(alignment: .bottomTrailing) {
            if let url = session.previewURL {
                PieceView(url: url, source: session.source)
                    .frame(maxWidth: .infinity, maxHeight: expandedPreview ? .infinity : writing ? 140 : 240)
                    .clipped()
            }
            Button { expandedPreview.toggle(); writing = false } label: {
                Image(systemName: expandedPreview ? "arrow.down.right.and.arrow.up.left" : "arrow.up.left.and.arrow.down.right")
                    .font(.system(size: 14)).padding(10).background(Paint.deep.opacity(0.85))
            }
            .accessibilityLabel(expandedPreview ? "Return to aesel" : "Expand piece")
        }
        .padding(expandedPreview ? 0 : 5)
        .background(Paint.frame)
        .overlay { Rectangle().stroke(Paint.frameEdge, lineWidth: 1) }
        .shadow(color: Paint.accent, radius: 0, x: 3, y: 3)
        .shadow(color: Paint.accent.opacity(0.45), radius: 0, x: 5, y: 5)
        .padding(.horizontal, expandedPreview ? 0 : 16).padding(.top, expandedPreview ? 0 : 12)
    }

    private var transcript: some View {
        VStack(spacing: 0) {
            AeselNotebook(session: session) { openURL($0) }
            if session.fatal != nil {
                Button("/retry") { host.restore() }.padding(8)
            }
        }
        .frame(maxHeight: .infinity)
    }

    private var appVersion: String {
        Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "—"
    }

    private var shelf: some View {
        HStack(alignment: .bottom, spacing: 8) {
            TextField("make something…", text: $draft, axis: .vertical)
                .font(Paint.font()).lineLimit(1...4).textFieldStyle(.plain)
                .tint(Paint.you).focused($writing).submitLabel(.send)
                .onSubmit { send() }.autocorrectionDisabled()
                .padding(.vertical, 10)
            Button { session.busy ? host.stop() : send() } label: {
                Text(session.busy ? "■" : "↵")
                    .font(Paint.font(28)).frame(width: 44, height: 44)
                    .foregroundStyle(Paint.you)
            }
            .disabled(!session.busy && draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
            .accessibilityLabel(session.busy ? "Stop" : "Send")
            Button { writing = false; showSettings = true; host.refreshCredits() } label: {
                AeselTitle(text: "v\(appVersion)", shadow: Paint.accent, size: 16)
                    .frame(minHeight: 44)
            }
            .accessibilityLabel("Version \(appVersion). Settings")
        }
        .buttonStyle(.plain).padding(.leading, 16).padding(.trailing, 8)
    }

    private var settings: some View {
        NavigationStack {
            List {
                Section {
                    if session.signedIn {
                        AeselHandle(handle: session.handle, colors: session.handleColors)
                        Button { host.refreshCredits() } label: {
                            VStack(alignment: .leading, spacing: 4) {
                                if let dollars = session.braincellDollars {
                                    Text(dollars, format: .currency(code: "USD"))
                                        .font(.system(size: 28, weight: .medium, design: .rounded))
                                }
                                if let balance = session.braincells {
                                    Text("\(balance.formatted(.number.precision(.fractionLength(0)))) braincells")
                                } else { Text(session.creditsStatus) }
                                if let free = session.freeDollars, let paid = session.purchasedDollars {
                                    Text("\(free.formatted(.currency(code: "USD"))) daily · \(paid.formatted(.currency(code: "USD"))) purchased")
                                        .font(.footnote).foregroundStyle(Paint.dim)
                                }
                            }
                        }
                        .buttonStyle(.plain).accessibilityHint("Refresh balance")
                    } else {
                        Button("Sign in") { settingsDestination = .signIn; showSettings = false }
                    }
                    Text("Braincells · automatic model")
                    Text(session.status).foregroundStyle(Paint.dim)
                } footer: {
                    Text("Dollar equivalent at $5 per million braincells. Daily allowance resets at midnight UTC.")
                }
                Section {
                    Button("New piece / threads") { showSettings = false; showHome = true }
                    Button("Publish") { host.publish() }
                        .disabled(!session.signedIn || session.busy)
                    Button("Open in browser") { if let url = session.shareURL { openURL(url) } }
                        .disabled(session.shareURL == nil)
                    if session.previewURL != nil {
                        Toggle("Show preview", isOn: Binding(get: { !previewHidden }, set: { previewHidden = !$0 }))
                    }
                    Button("Help") { settingsDestination = .help; showSettings = false }
                    if session.signedIn {
                        Button("Sign out") { showSettings = false; host.signOut() }
                    }
                }
            }
            .scrollContentBackground(.hidden)
            .background { AeselCloth().ignoresSafeArea() }
            .navigationTitle("Settings").navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { showSettings = false } } }
        }
        .tint(Paint.you).presentationDetents([.medium, .large])
    }

    private var help: some View {
        VStack(alignment: .leading, spacing: 24) {
            AeselWordmark()
            Text("describe a picture, a sound, or a little world. aesel writes it while you watch.")
            Text("/new     start a piece\n/publish share your piece\n/open    open in your browser\n/login   your AC account")
                .foregroundStyle(Paint.dim)
            Button("/close") { showHelp = false }.foregroundStyle(Paint.you)
            Spacer()
        }
        .font(Paint.font(22)).padding(24)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background { AeselCloth().ignoresSafeArea() }
        .presentationDetents([.medium])
    }

    private func send() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        draft = ""
        switch text {
        case "/new", "/home": writing = false; showHome = true
        case "/publish": host.publish()
        case "/login": host.signIn()
        case "/logout": host.signOut()
        case "/open": if let url = session.shareURL { openURL(url) }
        case "/settings": writing = false; showSettings = true; host.refreshCredits()
        case "/help": showHelp = true
        default: host.ask(text)
        }
    }


}

/// Puts the hidden session webview in the hierarchy without drawing it.
private struct HostCarrier: UIViewRepresentable {
    let host: SessionHost

    func makeUIView(context: Context) -> WKWebView { host.attachable }
    func updateUIView(_ view: WKWebView, context: Context) {}
}


private struct SignInCarrier: UIViewRepresentable {
    let host: SessionHost
    func makeUIView(context: Context) -> WKWebView { host.signInView }
    func updateUIView(_ view: WKWebView, context: Context) {}
}
