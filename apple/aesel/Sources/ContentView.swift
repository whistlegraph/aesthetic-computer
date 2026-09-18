import SwiftUI
import WebKit

struct ContentView: View {
    let session: Session
    let host: SessionHost
    @State private var draft = ""
    @State private var showHome = true
    @State private var showHelp = false
    @State private var expandedPreview = false
    @FocusState private var writing: Bool
    @Environment(\.openURL) private var openURL

    var body: some View {
        VStack(spacing: 0) {
            if !expandedPreview { title }
            if session.previewURL != nil { preview }
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
        .onChange(of: session.currentSessionID) { draft = "" }
        .sheet(isPresented: $showHelp) { help }
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
        HStack(alignment: .firstTextBaseline, spacing: 12) {
            Button {
                writing = false
                expandedPreview = false
                showHome = true
            } label: { AeselWordmark(text: "Aesel") }
                .buttonStyle(.plain)
                .accessibilityLabel("aesel title screen")
            Spacer(minLength: 0)
            if session.signedIn {
                AeselHandle(handle: session.handle, colors: session.handleColors)
            } else {
                Button("/login") { host.signIn() }.foregroundStyle(Paint.dim)
                    .accessibilityLabel("Sign in to Aesthetic Computer")
            }
        }
        .padding(.horizontal, 16).padding(.vertical, 8)
        .overlay(alignment: .bottom) { Rectangle().fill(Paint.rule).frame(height: 1).padding(.horizontal, 16) }
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
        .background { AeselWood() }
        .overlay { Rectangle().stroke(Color(rgb: 0x9e7548), lineWidth: 1) }
        .shadow(color: .black.opacity(0.3), radius: 0, x: 3, y: 3)
        .padding(.horizontal, expandedPreview ? 0 : 16).padding(.top, expandedPreview ? 0 : 12)
    }

    private var transcript: some View {
        ScrollViewReader { scroller in
            ScrollView {
                LazyVStack(alignment: .leading, spacing: 14) {
                    if session.entries.isEmpty {
                        Text(session.signedIn ? "what shall we make?" : "sign in to make something.")
                            .foregroundStyle(Paint.dim)
                    }
                    if let fatal = session.fatal {
                        Text(fatal).foregroundStyle(Paint.bad)
                        Button("/retry") { host.restore() }
                    }
                    ForEach(session.entries) { entry in
                        VStack(alignment: .leading, spacing: 3) {
                            if entry.kind == .you { Text("› " + entry.text).foregroundStyle(Paint.you) }
                            else {
                                Text(entry.text)
                                    .foregroundStyle(colour(entry.kind))
                                    .textSelection(.enabled)
                            }
                        }
                        .id(entry.id)
                        .fixedSize(horizontal: false, vertical: true)
                    }
                }
                .frame(maxWidth: .infinity, alignment: .leading)
                .padding(16)
            }
            .scrollDismissesKeyboard(.interactively)
            .onChange(of: session.entries.last?.text) {
                guard let last = session.entries.last else { return }
                scroller.scrollTo(last.id, anchor: .bottom)
            }
        }
        .frame(maxHeight: .infinity)
    }

    private var shelf: some View {
        VStack(spacing: 0) {
            HStack(alignment: .bottom, spacing: 0) {
                VStack(alignment: .leading, spacing: 5) {
                    HStack(spacing: 8) {
                        Text("REMOTE").foregroundStyle(.orange)
                        Text("·")
                        Text(session.status.uppercased()).foregroundStyle(session.busy ? Paint.ac : session.health == .failed ? Paint.bad : Paint.edit)
                    }
                    .font(Paint.font(18))
                    if !session.model.isEmpty {
                        AeselModelPicker(session: session, host: host)
                    }
                    Text(session.route.isEmpty ? "aesel" : session.route)
                        .foregroundStyle(Paint.dim).lineLimit(1).truncationMode(.middle)
                }
                .padding(.bottom, 10)
                Spacer(minLength: 0)
                AeselDonkey(busy: session.busy, failed: session.health == .failed)
                    .frame(height: 86, alignment: .bottom)
            }
            .padding(.horizontal, 16)

            VStack(spacing: 0) {
                HStack(alignment: .center, spacing: 9) {
                    Text("›").foregroundStyle(Paint.you)
                    TextField("make something…", text: $draft, axis: .vertical)
                        .font(Paint.font(22))
                        .lineLimit(1...4)
                        .textFieldStyle(.plain)
                        .tint(Paint.you)
                        .focused($writing)
                        .submitLabel(.send)
                        .onSubmit { send() }
                        .autocorrectionDisabled()
                    Button { session.busy ? host.stop() : send() } label: {
                        Text(session.busy ? "■" : "↵")
                            .font(Paint.font(28))
                            .frame(width: 44, height: 44)
                            .foregroundStyle(Paint.you)
                    }
                    .disabled(!session.busy && draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
                    .accessibilityLabel(session.busy ? "Stop" : "Send")
                }
                .padding(.leading, 12)
                .background(Paint.deep.opacity(0.8))
                .overlay { Rectangle().stroke(Paint.rule, lineWidth: 1) }
                ScrollView(.horizontal, showsIndicators: false) {
                    HStack(spacing: 20) {
                        Button("/new") { writing = false; showHome = true }
                        Button("/publish") { host.publish() }.disabled(!session.signedIn || session.busy)
                        Button("/open") { if let url = session.shareURL { openURL(url) } }
                            .disabled(session.shareURL == nil)
                        Button(session.signedIn ? "/logout" : "/login") { session.signedIn ? host.signOut() : host.signIn() }
                        Button("/help") { showHelp = true }
                    }
                    .font(Paint.font(19)).foregroundStyle(Paint.dim)
                    .buttonStyle(.plain)
                    .frame(height: 44)
                }
            }
            .padding(.horizontal, 12).padding(.top, 9)
            .background { AeselWood().ignoresSafeArea(edges: .bottom) }
            .overlay(alignment: .top) { Rectangle().fill(Color(rgb: 0x9e7548)).frame(height: 1) }
        }
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
        case "/help": showHelp = true
        default: host.ask(text)
        }
    }

    private func colour(_ kind: Entry.Kind) -> Color {
        switch kind {
        case .you: return Paint.you
        case .ac: return Paint.ink
        case .edit: return Paint.edit
        case .bad: return Paint.bad
        case .note: return Paint.dim
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
