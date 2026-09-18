import SwiftUI
import WebKit

struct ContentView: View {
    let session: Session
    let host: SessionHost
    @State private var draft = ""
    // Straight onto the notebook; /new or Settings opens the chooser.
    @State private var showHome = false
    @State private var showHelp = false
    @State private var showSettings = false
    @State private var braincells = Braincells()
    @State private var expandedPreview = false
    @State private var previewHidden = false
    @State private var notebookHeight: CGFloat = 24
    /// Slab's "complete" is an attention colour that clears once you return to
    /// the window; here, once you touch the draft again.
    @State private var attended = true
    @FocusState private var writing: Bool
    @Environment(\.openURL) private var openURL

    /// One ruled row of the sheet, as the desktop's --notebook-line-height.
    private let row: CGFloat = 24
    /// The desktop's #artifact-shell: a 178×119.33 piece in a 4pt frame, 14pt
    /// in from the top-right corner. The prose starts on the first ruled row
    /// clear of it (the desktop reserves the full width when the page is this
    /// narrow).
    private let previewSize = CGSize(width: 178, height: 119.33)
    private let previewInset: CGFloat = 14
    private var previewVisible: Bool { session.previewURL != nil && !previewHidden }
    private var previewBlockHeight: CGFloat { previewVisible ? 168 : row }

    /// Slab's palette for what the session is doing right now.
    private var paint: Paint {
        let status = attended && session.status == "ready" ? "blank" : session.status
        return Paint.slab(status: status, busy: session.busy, failed: session.health == .failed || session.fatal != nil)
    }

    var body: some View {
        Group {
            if expandedPreview { expandedPiece } else { sheet }
        }
        .blur(radius: showSettings ? 4 : 0)
        .overlay {
            if showHome {
                AeselHomeView(session: session, host: host) { showHome = false }
            }
        }
        .overlay(alignment: .bottomTrailing) {
            if !expandedPreview && !showHome && !showSettings { versionLabel }
        }
        .overlay { if showSettings { settingsPane } }
        .font(Paint.font())
        .foregroundStyle(paint.ink)
        .background { AeselCloth().ignoresSafeArea() }
        .background(HostCarrier(host: host).frame(width: 0, height: 0))
        .environment(\.paint, paint)
        .animation(.easeInOut(duration: 0.5), value: paint)
        .onChange(of: session.currentSessionID) { draft = ""; previewHidden = false; expandedPreview = false; attended = true }
        .onChange(of: session.busy) { if session.busy { attended = false } }
        .onChange(of: writing) { if writing { attended = true } }
        .onChange(of: draft) { attended = true }
        .onChange(of: session.signedIn) { if session.signedIn { Task { await braincells.load() } } }
        .task { braincells.start(token: { host.accessToken() }, credited: { host.refreshCredits() }) }
        .onAppear {
            #if DEBUG
            if ProcessInfo.processInfo.environment["AESEL_NOTEBOOK_PREVIEW"] == "1" {
                showHome = false
                showSettings = ProcessInfo.processInfo.environment["AESEL_SETTINGS_PREVIEW"] == "1"
            }
            #endif
        }
        .sheet(isPresented: $showHelp) { help }
        .sheet(isPresented: Binding(get: { session.showSignIn }, set: { session.showSignIn = $0 })) {
            VStack(spacing: 0) {
                HStack {
                    AeselWordmark(text: "Aesel")
                    Spacer()
                    Button("/close") { session.showSignIn = false }
                        .font(Paint.font()).foregroundStyle(paint.dim)
                }.padding(16).background(paint.bg)
                ZStack {
                    SignInCarrier(host: host)
                    if session.signInLoading {
                        ProgressView("Opening AC sign-in…")
                            .font(Paint.font()).tint(paint.ac)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                            .background(paint.bg)
                    }
                    if let error = session.signInError {
                        VStack(spacing: 18) {
                            Text(error).multilineTextAlignment(.center)
                            Button("/retry") { host.signIn() }.foregroundStyle(paint.you)
                        }
                        .font(Paint.font()).padding(24)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                        .background(paint.bg)
                    }
                }
            }
        }
    }

    // MARK: - The sheet

    /// The desktop's notebook: one scrolling ruled sheet holding the preview,
    /// the prose and the draft, with the title fixed on the first row and the
    /// version label parked at the bottom-right corner.
    private var sheet: some View {
        GeometryReader { geometry in
            ScrollViewReader { proxy in
                ScrollView {
                    VStack(alignment: .leading, spacing: 0) {
                        ZStack(alignment: .topTrailing) {
                            Color.clear.frame(height: previewBlockHeight)
                            if previewVisible { previewBox }
                        }
                        AeselNotebook(session: session, paint: paint, height: $notebookHeight) { openURL($0) }
                            .frame(height: max(row, notebookHeight))
                        if session.fatal != nil {
                            Button("/retry") { host.restore() }
                                .foregroundStyle(paint.you).frame(height: row).padding(.horizontal, 10)
                        }
                        Spacer(minLength: 0)
                        prompt.id("prompt")
                    }
                    .frame(minHeight: geometry.size.height, alignment: .top)
                    .background(alignment: .top) { AeselRuling(spacing: row) }
                }
                .scrollDismissesKeyboard(.interactively)
                .overlay(alignment: .topLeading) {
                    let clear = previewVisible ? previewSize.width + 8 + 2 + previewInset + 16 : 0
                    title.frame(maxWidth: max(60, geometry.size.width - 10 - clear), alignment: .leading).clipped()
                }
                .onChange(of: notebookHeight) { if session.busy || writing { proxy.scrollTo("prompt", anchor: .bottom) } }
                .onChange(of: writing) { if writing { withAnimation { proxy.scrollTo("prompt", anchor: .bottom) } } }
                .onChange(of: session.busy) { if session.busy { withAnimation { proxy.scrollTo("prompt", anchor: .bottom) } } }
            }
        }
    }

    private var title: some View {
        Button {
            if let url = session.shareURL { openURL(url) }
        } label: {
            AeselTitle(text: session.route.isEmpty ? "new piece" : session.route, colors: session.handleColors)
        }
        .buttonStyle(.plain)
        .disabled(session.shareURL == nil)
        .accessibilityLabel("Open piece in browser")
        .frame(height: row)
        .padding(.leading, 10)
    }

    /// The little piece at the top-right, in the desktop's tinted frame with
    /// its accent shadows and the 24pt fullscreen control in its corner.
    private var previewBox: some View {
        ZStack(alignment: .topTrailing) {
            if let url = session.previewURL {
                PieceView(url: url, source: session.source)
                    .frame(width: previewSize.width, height: previewSize.height)
                    .clipped()
            }
            Button { expandedPreview = true; writing = false } label: {
                Image(systemName: "arrow.up.left.and.arrow.down.right")
                    .font(.system(size: 12)).frame(width: 24, height: 24)
                    .background(Color(rgb: 0x1c1728).opacity(0.9))
                    .overlay { RoundedRectangle(cornerRadius: 4).stroke(Color.white.opacity(0.4), lineWidth: 1) }
                    .clipShape(RoundedRectangle(cornerRadius: 4))
            }
            .buttonStyle(.plain).foregroundStyle(.white).padding(5)
            .accessibilityLabel("Expand piece")
        }
        .padding(4)
        .background(paint.frame)
        .overlay { Rectangle().stroke(paint.frameEdge, lineWidth: 1) }
        .shadow(color: Color(rgb: 0x282130), radius: 0, x: 1, y: 1)
        .shadow(color: paint.accent, radius: 0, x: 3, y: 3)
        .shadow(color: paint.accent.opacity(0.45), radius: 0, x: 5, y: 5)
        .padding(.top, previewInset).padding(.trailing, previewInset)
    }

    private var expandedPiece: some View {
        ZStack(alignment: .topTrailing) {
            if let url = session.previewURL {
                PieceView(url: url, source: session.source)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
            Button { expandedPreview = false } label: {
                Image(systemName: "arrow.down.right.and.arrow.up.left")
                    .font(.system(size: 14)).padding(10).background(paint.deep.opacity(0.85))
            }
            .buttonStyle(.plain).padding(5)
            .accessibilityLabel("Return to aesel")
        }
    }

    private var appVersion: String {
        Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "—"
    }

    /// The draft, written on the sheet's last rows in the user's ink like the
    /// desktop's prose prompt; the trailing 100pt stays clear for the version.
    private var prompt: some View {
        HStack(alignment: .top, spacing: 8) {
            TextField("make something…", text: $draft, axis: .vertical)
                .font(Paint.font(16)).foregroundStyle(paint.userInk).tint(paint.userInk)
                .lineLimit(1...6).textFieldStyle(.plain)
                .focused($writing).submitLabel(.send)
                .onSubmit { send() }.autocorrectionDisabled()
                .frame(minHeight: row)
            if session.busy {
                Button { host.stop() } label: {
                    Text("■").font(Paint.font(16)).foregroundStyle(paint.you).frame(width: row, height: row)
                }
                .accessibilityLabel("Stop")
            } else if !draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
                Button { send() } label: {
                    Text("↵").font(Paint.font(20)).foregroundStyle(paint.you).frame(width: row, height: row)
                }
                .accessibilityLabel("Send")
            }
        }
        .buttonStyle(.plain)
        .padding(.leading, 10).padding(.trailing, 100).padding(.bottom, 8)
    }

    /// The desktop's #credit-label: the version in Prox lettering, 10pt from
    /// the right and 8pt up, and the way into settings.
    private var versionLabel: some View {
        Button { openSettings() } label: {
            AeselTitle(text: "v\(appVersion)", size: 16).padding(9)
        }
        .buttonStyle(.plain)
        .padding(.trailing, 1)
        .accessibilityLabel("Version \(appVersion). Settings")
    }

    // MARK: - Settings

    private func openSettings() {
        writing = false
        braincells.notice = ""
        host.refreshCredits()
        withAnimation(.easeOut(duration: 0.18)) { showSettings = true }
    }

    private func closeSettings(then action: (() -> Void)? = nil) {
        withAnimation(.easeIn(duration: 0.15)) { showSettings = false }
        if let action { DispatchQueue.main.asyncAfter(deadline: .now() + 0.2, execute: action) }
    }

    private var modelName: String {
        let served = session.reportedModel.isEmpty ? session.model : session.reportedModel
        return served.isEmpty ? "choosing…" : served
    }

    private func usd(_ value: Double) -> String { value.formatted(.currency(code: "USD")) }

    /// The desktop's #provider-menu: a centred card over the blurred sheet,
    /// 15pt Helvetica, hairline sections, with the account, the automatic
    /// model, braincells and the way to buy more.
    private var settingsPane: some View {
        ZStack {
            Color.black.opacity(0.4).ignoresSafeArea()
                .onTapGesture { closeSettings() }
            ScrollView {
                VStack(alignment: .leading, spacing: 0) {
                    HStack(alignment: .center) {
                        if session.signedIn {
                            AeselHandle(handle: session.handle, colors: session.handleColors).font(Paint.title(20))
                        } else {
                            AeselWordmark(text: "aesel")
                        }
                        Spacer()
                        Button { closeSettings() } label: {
                            Text("×").font(Paint.font(24)).frame(width: 30, height: 30)
                        }
                        .accessibilityLabel("Close settings")
                    }
                    .padding(.bottom, 6)

                    settingsField("Provider") {
                        HStack(spacing: 9) {
                            Image("braincell").resizable().scaledToFit().frame(width: 25, height: 25)
                            Text("Braincells")
                        }
                    }
                    settingsField("Automatic model") {
                        Text(modelName).lineLimit(1).truncationMode(.middle)
                    }
                    .accessibilityLabel("Automatic model, \(modelName)")

                    Text(["Remote inference", session.status].joined(separator: " · "))
                        .padding(.top, 14)
                    if let url = session.shareURL {
                        Text("Auto-publish on · every save goes to \(url.absoluteString)")
                            .padding(.top, 10)
                    }

                    if session.signedIn {
                        HStack(spacing: 9) {
                            Image("braincell").resizable().scaledToFit().frame(width: 25, height: 25)
                            if let dollars = session.braincellDollars, let balance = session.braincells {
                                Text("\(usd(dollars)) · \(balance.formatted(.number.precision(.fractionLength(0)))) braincells")
                            } else if let balance = session.braincells {
                                Text("\(balance.formatted(.number.precision(.fractionLength(0)))) braincells")
                            } else {
                                Text(session.creditsStatus)
                            }
                        }
                        .padding(.top, 14)
                        .onTapGesture { host.refreshCredits() }
                        .accessibilityHint("Refresh balance")
                        if let free = session.freeDollars, let paid = session.purchasedDollars {
                            Text("\(usd(free)) daily · \(usd(paid)) purchased")
                                .font(Paint.font(13)).foregroundStyle(paint.dim).padding(.top, 4)
                        }
                        buyButton.padding(.top, 14)
                        if !braincells.notice.isEmpty {
                            Text(braincells.notice).font(Paint.font(13)).foregroundStyle(paint.dim).padding(.top, 8)
                        } else if !braincells.storeStatus.isEmpty {
                            Text(braincells.storeStatus).font(Paint.font(13)).foregroundStyle(paint.dim).padding(.top, 8)
                        }
                    } else {
                        settingsItem("Sign in to AC") { closeSettings { host.signIn() } }
                            .padding(.top, 14)
                    }

                    settingsDivider
                    HStack {
                        Text("Aesel").fontWeight(.semibold)
                        Spacer()
                        Text("v\(appVersion)").foregroundStyle(paint.dim)
                    }
                    .font(Paint.font(13))
                }
                .padding(EdgeInsets(top: 20, leading: 24, bottom: 20, trailing: 24))
            }
            .frame(maxWidth: 560)
            .frame(maxHeight: UIScreen.main.bounds.height * 0.85)
            .fixedSize(horizontal: false, vertical: true)
            .background(paint.bg)
            .clipShape(RoundedRectangle(cornerRadius: 16))
            .overlay { RoundedRectangle(cornerRadius: 16).stroke(paint.ink.opacity(0.18), lineWidth: 1) }
            .shadow(color: .black.opacity(0.2), radius: 30, y: 20)
            .padding(24)
        }
        .font(Paint.font(15))
        .buttonStyle(.plain)
        .transition(.opacity)
    }

    /// The App Store purchase: one big green button with Apple's price.
    private var buyButton: some View {
        Button {
            Task { await braincells.buy() }
        } label: {
            HStack(spacing: 10) {
                Image("braincell").resizable().scaledToFit().frame(width: 28, height: 28)
                VStack(alignment: .leading, spacing: 2) {
                    Text(braincells.busy ? "Opening the App Store…" : "Add 1,000,000 braincells")
                        .font(Paint.font(18)).fontWeight(.semibold)
                    Text(braincells.busy ? "" : "\(braincells.price) · App Store").font(Paint.font(13)).opacity(0.85)
                }
                Spacer()
            }
            .padding(EdgeInsets(top: 14, leading: 16, bottom: 14, trailing: 16))
            .frame(maxWidth: .infinity, minHeight: 64)
            .foregroundStyle(.white)
            .background(Color(rgb: 0x1f9d55))
            .clipShape(RoundedRectangle(cornerRadius: 10))
            .overlay { RoundedRectangle(cornerRadius: 10).stroke(Color.white.opacity(0.25), lineWidth: 1) }
        }
        .disabled(braincells.busy || braincells.product == nil)
        .opacity(braincells.product == nil ? 0.55 : 1)
        .accessibilityLabel("Add one million braincells for \(braincells.price) through the App Store")
    }

    private func settingsField<Content: View>(_ label: String, @ViewBuilder content: () -> Content) -> some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(label).font(Paint.font(13))
            content()
                .frame(maxWidth: .infinity, minHeight: 36, alignment: .leading)
                .padding(.horizontal, 8)
                .overlay { RoundedRectangle(cornerRadius: 6).stroke(paint.ink.opacity(0.25), lineWidth: 1) }
        }
        .padding(.top, 14)
    }

    private func settingsItem(_ label: String, enabled: Bool = true, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Text(label).frame(maxWidth: .infinity, alignment: .leading)
                .padding(EdgeInsets(top: 9, leading: 10, bottom: 9, trailing: 10))
                .contentShape(Rectangle())
        }
        .disabled(!enabled)
        .opacity(enabled ? 1 : 0.5)
    }

    private var settingsDivider: some View {
        Rectangle().fill(paint.ink.opacity(0.18)).frame(height: 1)
            .padding(.top, 16).padding(.bottom, 12)
    }

    private var help: some View {
        VStack(alignment: .leading, spacing: 24) {
            AeselWordmark()
            Text("describe a picture, a sound, or a little world. aesel writes it while you watch.")
            Text("/new     start a piece\n/publish share your piece\n/open    open in your browser\n/login   your AC account")
                .foregroundStyle(paint.dim)
            Button("/close") { showHelp = false }.foregroundStyle(paint.you)
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
        case "/settings": openSettings()
        case "/buy": openSettings(); Task { await braincells.buy() }
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
