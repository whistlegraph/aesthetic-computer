import SwiftUI
import WebKit
import UniformTypeIdentifiers

struct ContentView: View {
    let session: Session
    let host: SessionHost
    private var draft: String {
        get { session.composer }
        nonmutating set {
            session.composer = newValue
            host.setDraft(newValue, threadID: session.currentSessionID)
        }
    }
    // Straight onto the notebook; /new or Settings opens the chooser.
    @State private var showHome = false
    @State private var showHelp = false
    @State private var showSource = false
    @State private var showImport = false
    @State private var showExport = false
    @State private var fileNotice: String?
    @State private var showSettings = false
    @State private var braincells = Braincells()
    @StateObject private var preview = PiecePreview()
    @State private var expandedPreview = false
    @State private var previewHidden = false
    @State private var notebookHeight: CGFloat = 24
    @State private var notebookTop: CGFloat = 12
    /// Slab's "complete" is an attention colour that clears once you return to
    /// the window; here, once you touch the draft again.
    @State private var attended = true
    @State private var writing = true
    @State private var composerHeight: CGFloat = 48
    @State private var sheetSize = CGSize(width: 840, height: 680)
    @AppStorage("aesel.uiScale") private var uiScale = 1.0
    @Environment(\.openURL) private var openURL
    @Environment(\.colorScheme) private var colorScheme

    /// One ruled row of the sheet, as the desktop's --notebook-line-height.
    private let row: CGFloat = 24
    private let paperTop: CGFloat = 12
    private let edgeInset = PreviewBounds.edgeInset
    /// The corner stays fixed; notebook prose flows around its frame.
    @State private var previewBounds = PreviewBounds()
    private var previewSize: CGSize { CGSize(width: previewBounds.width, height: previewBounds.height) }
    private var previewInset: CGFloat { previewBounds.right }
    private var previewVisible: Bool { session.previewURL != nil && !previewHidden && sheetSize.width >= 420 && sheetSize.height >= 300 }
    private var previewBlockHeight: CGFloat { paperTop }
    private var hasNotebook: Bool { session.entries.contains { $0.kind != .edit } || session.fatal != nil }
    // WebKit reserves a paint row for descenders; the next editor row shares it.
    private var transcriptHeight: CGFloat { hasNotebook ? max(row, notebookHeight) - row : 0 }
    private var notebookExclusion: [String: CGFloat] {
        guard previewVisible else { return [:] }
        let width = previewBounds.width + 8 + previewBounds.right + 16 - edgeInset
        let available = sheetSize.width - edgeInset * 2
        return ["width": available - width < 220 ? available : width,
                "height": max(0, previewBounds.top + previewBounds.height + 8 + 16 - notebookTop),
                "top": max(0, previewBounds.top - 16 - notebookTop)]
    }
    private var compact: Bool { sheetSize.width < 340 || sheetSize.height < 240 }

    /// Slab's palette for what the session is doing right now.
    private var paint: Paint {
        let status = attended && session.status == "ready" ? "blank" : session.status
        return Paint.slab(status: status, busy: session.busy, failed: session.health == .failed || session.fatal != nil, colorScheme: colorScheme)
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
        .overlay { if showSettings { settingsPane } }
        .font(Paint.font())
        .buttonStyle(AeselButtonStyle())
        .foregroundStyle(paint.ink)
        .background { AeselCloth().ignoresSafeArea() }
        .background(HostCarrier(host: host).frame(width: 0, height: 0))
        .aeselWindowTitle(session.route, paper: paint.bg)
        .environment(\.paint, paint)
        .coordinateSpace(name: "aesel-ui")
        .animation(.easeInOut(duration: 0.5), value: paint)
        .onChange(of: session.currentSessionID) { previewHidden = false; expandedPreview = false; attended = true }
        .onChange(of: preview.failure) { host.automation.previewFailure = preview.failure }
        .onChange(of: session.busy) { if session.busy { attended = false } }
        .onChange(of: writing) { if writing { attended = true } }
        .onChange(of: draft) { attended = true }
        .onChange(of: session.showSignIn) { if !session.showSignIn { host.cancelSignIn() } }
        .onChange(of: session.signedIn) { if session.signedIn { Task { await braincells.load() } } }
        .task { braincells.start(token: { host.accessToken() }, credited: { host.refreshCredits() }) }
        .onAppear {
            host.automation.inspect = { automationState }
            host.automation.perform = { action, params in try await automationAction(action, params) }
            host.automation.preview = preview.view
            host.automation.retryPreview = { preview.reload() }
            host.automation.start()
            #if DEBUG
            if ProcessInfo.processInfo.environment["AESEL_NOTEBOOK_PREVIEW"] == "1" {
                showHome = false
                showSettings = ProcessInfo.processInfo.environment["AESEL_SETTINGS_PREVIEW"] == "1"
            }
            #endif
        }
        .sheet(item: Binding(get: { session.approval }, set: { value in
            if value == nil, let pending = session.approval { host.respondToApproval(id: pending.id, decision: "decline") }
        })) { request in
            VStack(alignment: .leading, spacing: 16) {
                Text("Allow this action?").font(Paint.title(20))
                ScrollView { Text(request.detail).font(Paint.font(15)).textSelection(.enabled) }
                HStack {
                    Button("Deny") { host.respondToApproval(id: request.id, decision: "decline") }
                    Spacer()
                    Button("Allow once") { host.respondToApproval(id: request.id, decision: "accept") }
                }
            }.padding(24).frame(minWidth: 300, minHeight: 200)
                .background(paint.bg).foregroundStyle(paint.ink).buttonStyle(AeselButtonStyle())
        }
        .onChange(of: session.exportJSON) { if session.exportJSON != nil { showExport = true } }
        .fileExporter(isPresented: $showExport, document: NotebookDocument(text: session.exportJSON ?? ""), contentType: .json,
                      defaultFilename: "Aesel Notebook") { result in
            session.exportJSON = nil
            if case .failure(let error) = result { fileNotice = error.localizedDescription }
        }
        .fileImporter(isPresented: $showImport, allowedContentTypes: [.json]) { result in
            do {
                let url = try result.get()
                let scoped = url.startAccessingSecurityScopedResource()
                defer { if scoped { url.stopAccessingSecurityScopedResource() } }
                let size = try url.resourceValues(forKeys: [.fileSizeKey]).fileSize ?? 0
                guard size <= 8 * 1024 * 1024 else { throw CocoaError(.fileReadTooLarge) }
                host.importNotebook(try String(contentsOf: url, encoding: .utf8))
            } catch { fileNotice = error.localizedDescription }
        }
        .alert("Notebook file", isPresented: Binding(get: { fileNotice != nil }, set: { if !$0 { fileNotice = nil } })) {
            Button("OK") { fileNotice = nil }
        } message: { Text(fileNotice ?? "") }
        .sheet(isPresented: $showSource) {
            AeselSourceView(session: session, host: host).environment(\.paint, paint)
        }
        .sheet(isPresented: $showHelp) { help }
        .sheet(isPresented: Binding(get: { session.showSignIn }, set: { session.showSignIn = $0 })) {
            ZStack(alignment: .topTrailing) {
                SignInCarrier(host: host)
                if let error = session.signInError {
                    VStack(spacing: 18) {
                        Text(error).multilineTextAlignment(.center)
                        Button("Retry") { host.signIn() }.foregroundStyle(paint.you)
                    }.padding(24).frame(maxWidth: .infinity, maxHeight: .infinity).background(paint.bg)
                }
                Button { session.showSignIn = false } label: {
                    Image(systemName: "xmark").font(.system(size: 13, weight: .semibold))
                        .frame(width: 32, height: 32).background(paint.bg, in: Circle())
                }.accessibilityLabel("Close sign-in").padding(10)
            }
            .buttonStyle(AeselButtonStyle())
            .environment(\.paint, paint)
            .aeselSignInSheet()
        }
    }

    // MARK: - The sheet

    /// A fixed title strip above one scrolling document, starting on its first row.
    private var sheet: some View {
        VStack(spacing: 0) {
            notebookHeader
            GeometryReader { geometry in
                ScrollViewReader { proxy in
                    ScrollView {
                        VStack(alignment: .leading, spacing: 0) {
                            Color.clear.frame(height: previewBlockHeight)
                            AeselNotebook(session: session, automation: host.automation, paint: paint, exclusion: notebookExclusion, height: $notebookHeight) { openURL($0) }
                                .frame(height: hasNotebook ? max(row, notebookHeight) : 0)
                                .clipped()
                                .padding(.bottom, hasNotebook ? -row : 0)
                                .allowsHitTesting(hasNotebook)
                                .onGeometryChange(for: CGFloat.self) { $0.frame(in: .named("notebook-scroll")).minY } action: { notebookTop = $0 }
                            if session.fatal != nil {
                                Button("/retry") { host.restore() }
                                    .foregroundStyle(paint.you).frame(height: row).padding(.horizontal, edgeInset)
                            }
                            prompt(minHeight: max(48, geometry.size.height - paperTop - transcriptHeight - row)).id("prompt")
                        }
                        .frame(maxWidth: .infinity, minHeight: geometry.size.height, alignment: .topLeading)
                        .background(alignment: .top) { AeselRuling(spacing: row, topInset: paperTop) }
                    }
                    .coordinateSpace(name: "notebook-scroll")
                    .aeselKeyboardScrolling()
                    .overlay(alignment: .topTrailing) {
                        if previewVisible { previewBox(container: geometry.size) }
                    }
                    .onGeometryChange(for: CGSize.self) { $0.size } action: { sheetSize = $0 }
                    .onChange(of: notebookHeight) { if session.busy || writing { proxy.scrollTo("prompt", anchor: .bottom) } }
                    .onChange(of: writing) { if writing { withAnimation { proxy.scrollTo("prompt", anchor: .bottom) } } }
                    .onChange(of: session.busy) { if session.busy { withAnimation { proxy.scrollTo("prompt", anchor: .bottom) } } }
                }
            }
        }
    }

    private var headerReservedWidth: CGFloat {
        #if os(macOS)
        52 / uiScale + edgeInset * 2 + 36 + 48
        #else
        edgeInset * 2 + 24 + 48
        #endif
    }

    private var notebookHeader: some View {
        GeometryReader { geometry in
            HStack(spacing: 12) {
                #if os(macOS)
                Color.clear.frame(width: 52 / uiScale)
                #endif
                title(availableWidth: max(0, geometry.size.width - headerReservedWidth))
                Spacer(minLength: 8)
                versionLabel
            }
            .padding(.horizontal, edgeInset)
            .frame(height: 32)
            .background { AeselWindowDragArea() }
        }
        .frame(height: 32)
    }

    private var titleURL: URL? {
        session.shareURL ?? (session.route.isEmpty ? nil : URL(string: "https://aesthetic.computer/")?.appendingPathComponent(session.route))
    }

    private func title(availableWidth: CGFloat) -> some View {
        Button { if let url = titleURL { openURL(url) } } label: {
            AeselTitle(text: session.route.isEmpty ? "aesel" : session.route, colors: session.handleColors, size: compact ? 12 : 16,
                       maximumWidth: availableWidth, horizontalInset: 0,
                       hoverSound: { AeselHoverSound.play(project: session.route, revision: session.currentRevision, control: "title") })
        }
        .buttonStyle(AeselButtonStyle())
        .accessibilityLabel("Open piece in browser")
        .frame(height: row)
    }

    /// The corner piece keeps the original desktop wood grain and resize edges.
    private func previewBox(container: CGSize) -> some View {
        ZStack(alignment: .topTrailing) {
            if let url = session.previewURL {
                PieceView(url: url, source: session.source, preview: preview)
                    .frame(width: previewSize.width, height: previewSize.height, alignment: .topTrailing)
                    .clipped()
                    .overlay { AeselPreviewInset() }
            }

        }
        .padding(4)
        .background {
            AeselWoodFrame()
                .shadow(color: Color(red: 1, green: 0.176, blue: 0.333).opacity(0.5), radius: 0, x: 1.5, y: 1.5)
        }
        .overlay { PreviewResizeOverlay(bounds: $previewBounds, container: container) }
        .padding(.top, previewBounds.top).padding(.trailing, previewBounds.right)
        .animation(nil, value: previewBounds)
        .onChange(of: container) {
            if container.width >= 420 && container.height >= 300 { previewBounds = previewBounds.fitted(in: container) }
        }
    }

    private var expandedPiece: some View {
        ZStack(alignment: .topTrailing) {
            if let url = session.previewURL {
                PieceView(url: url, source: session.source, preview: preview)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
            Button { expandedPreview = false } label: {
                Image(systemName: "arrow.down.right.and.arrow.up.left")
                    .font(.system(size: 14)).padding(10).background(paint.deep.opacity(0.85))
            }
            .buttonStyle(AeselButtonStyle()).padding(5)
            .accessibilityLabel("Return to aesel")
        }
    }

    /// The empty paper remains editable below the conversation.
    private func prompt(minHeight: CGFloat) -> some View {
        HStack(alignment: .top, spacing: 8) {
            AeselComposer(text: Binding(get: { draft }, set: { draft = $0 }), height: $composerHeight,
                          focused: $writing, color: paint.userInk, submit: send)
                .frame(height: max(composerHeight, minHeight))
            if session.busy {
                Button { host.stop() } label: {
                    Text("■").font(Paint.font(16)).foregroundStyle(paint.you).frame(width: row, height: row)
                }
                .accessibilityLabel("Stop")
            }
        }
        .buttonStyle(AeselButtonStyle())
        .overlay(alignment: .bottomTrailing) {
            if !session.busy && !draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
                Button(action: send) { Color.clear.frame(width: row, height: row).contentShape(Rectangle()) }
                    .accessibilityLabel("Send")
            }
        }
        .padding(.leading, edgeInset)
        .padding(.trailing, previewVisible && notebookTop + transcriptHeight < previewBounds.top + previewBounds.height + 8
                 ? previewSize.width + previewInset + 24 : edgeInset)
        .padding(.bottom, row)
    }

    /// The piece version opens settings from the fixed title strip.
    private var versionLabel: some View {
        Button { openSettings() } label: {
            AeselTitle(text: "v\(session.currentRevision)", size: compact ? 11 : 13, horizontalInset: 0,
                       hoverSound: { AeselHoverSound.play(project: session.route, revision: session.currentRevision, control: "version") })
        }
        .buttonStyle(AeselButtonStyle())
        .frame(height: 32)
        .accessibilityLabel("Piece version \(session.currentRevision). Settings")
    }

    // MARK: - Settings

    private func openSettings() {
        writing = false
        braincells.notice = ""
        host.refreshCredits()
        host.refreshProviders()
        withAnimation(.easeOut(duration: 0.18)) { showSettings = true }
    }

    private func closeSettings(then action: (() -> Void)? = nil) {
        withAnimation(.easeIn(duration: 0.15)) { showSettings = false }
        if let action { DispatchQueue.main.asyncAfter(deadline: .now() + 0.2, execute: action) }
    }

    private func usd(_ value: Double) -> String { value.formatted(.currency(code: "USD")) }

    /// The desktop's #provider-menu: a centred card over the blurred sheet,
    /// 15pt Helvetica, hairline sections, with the account, the automatic
    /// model, braincells and the way to buy more.
    private var settingsPane: some View {
        GeometryReader { geometry in
            ZStack {
                Color.black.opacity(0.4).ignoresSafeArea().onTapGesture { closeSettings() }
                VStack(spacing: 0) {
                    HStack {
                        Spacer()
                        Button { closeSettings() } label: {
                            Image(systemName: "xmark").font(.system(size: 13, weight: .semibold))
                                .frame(width: 30, height: 30).background(paint.ink.opacity(0.08), in: RoundedRectangle(cornerRadius: 6))
                        }.accessibilityLabel("Close settings")
                    }.padding(.horizontal, 18).padding(.top, 12)
                    ScrollView {
                        VStack(alignment: .leading, spacing: 16) {
                            let fields = geometry.size.width < 420 ? AnyLayout(VStackLayout(alignment: .leading, spacing: 12)) : AnyLayout(HStackLayout(alignment: .top, spacing: 12))
                            fields {
                                settingsField("Provider") { AeselProviderPicker(session: session, host: host) }
                                if session.provider != "ac" {
                                    settingsField("Model") { AeselModelPicker(session: session, host: host) }
                                }
                            }
                            if !session.providerNotice.isEmpty {
                                Text(session.providerNotice).font(Paint.font(13)).foregroundStyle(paint.dim)
                            }
                            if session.hostOperationID != nil && !session.busy {
                                settingsItem("Reconnect to current turn") { host.resumeHostTurn() }
                            }
                            VStack(alignment: .leading, spacing: 8) {
                                Text("Preview volume").font(Paint.font(13)).foregroundStyle(paint.dim)
                                HStack(spacing: 10) {
                                    Slider(value: $preview.volume, in: 0...1).tint(paint.accent).accessibilityLabel("Preview volume")
                                    Button { preview.volume = preview.volume == 0 ? 1 : 0 } label: {
                                        Image(systemName: preview.volume == 0 ? "speaker.slash" : "speaker.wave.2")
                                            .frame(width: 36, height: 32).background(paint.accent.opacity(0.18), in: RoundedRectangle(cornerRadius: 6))
                                    }.accessibilityLabel(preview.volume == 0 ? "Unmute preview" : "Mute preview")
                                }
                            }
                            Rectangle().fill(paint.ink.opacity(0.16)).frame(height: 1)
                            AeselVersionList(session: session, host: host)
                        }.padding(.horizontal, 20).padding(.bottom, 20)
                    }
                }
                .frame(maxWidth: 560).frame(maxHeight: min(560, geometry.size.height * 0.85))
                .background(paint.bg)
                .clipShape(RoundedRectangle(cornerRadius: 12))
                .overlay { RoundedRectangle(cornerRadius: 12).stroke(paint.ink.opacity(0.3), lineWidth: 1) }
                .shadow(color: .black.opacity(0.25), radius: 20, y: 10).padding(20)
            }.font(Paint.font(15)).buttonStyle(AeselButtonStyle()).transition(.opacity)
        }
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

    private func settingsField<Content: View>(_ label: String, bordered: Bool = true, @ViewBuilder content: () -> Content) -> some View {
        VStack(alignment: .leading, spacing: 5) {
            Text(label).font(Paint.font(13))
            content()
                .frame(maxWidth: .infinity, minHeight: 36, alignment: .leading)
                .padding(.horizontal, bordered ? 8 : 0)
                .overlay { if bordered { RoundedRectangle(cornerRadius: 6).stroke(paint.ink.opacity(0.45), lineWidth: 1) } }
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
        .aeselHelpSheet()
    }

    private func send() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        switch text {
        case "/new", "/home": writing = false; showHome = true
        case "/publish": host.publish()
        case "/login": host.signIn()
        case "/logout": host.signOut()
        case "/open": if let url = session.shareURL { openURL(url) }
        case "/settings": openSettings()
        case "/buy": openSettings(); Task { await braincells.buy() }
        case "/help": showHelp = true
        default:
            if session.provider == "ac" && !session.signedIn { host.signIn(); return }
            guard session.canStartTurn else { openSettings(); return }
            host.ask(text)
        }
        draft = ""
    }


    private var automationState: [String: Any] {
        #if os(macOS)
        let canResize = true
        #else
        let canResize = false
        #endif
        let controls: [(String, String, Bool)] = [
            ("settings.open", "Open settings", !showSettings), ("settings.close", "Close settings", showSettings),
            ("home.open", "Show saved threads", !showHome), ("home.close", "Return to notebook", showHome),
            ("help.open", "Open help", !showHelp), ("help.close", "Close help", showHelp),
            ("composer.set", "Set message draft", true), ("composer.clear", "Clear message draft", true),
            ("composer.send", "Send message; may run inference and publish", session.canStartTurn && !draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty),
            ("turn.stop", "Stop current turn", session.busy),
            ("turn.reconnect", "Check current host turn without resending it", session.hostOperationID != nil && !session.busy),
            ("session.retry", "Restore failed session", session.fatal != nil),
            ("session.new", "Start a blank piece", !session.busy), ("session.resume", "Resume a saved thread", !session.busy),
            ("preview.expand", "Expand preview", previewVisible && !expandedPreview),
            ("preview.collapse", "Return to notebook", expandedPreview),
            ("preview.hide", "Hide preview", previewVisible), ("preview.show", "Show preview", previewHidden),
            ("preview.reload", "Reload embedded preview", previewVisible),
            ("preview.resize", "Resize corner preview", canResize && previewVisible && !expandedPreview),
            ("preview.retry", "Retry failed preview", host.automation.previewFailure != nil),
            ("window.resize", "Resize app window without focusing it", canResize),
            ("ui.scale", "Set UI size from 0.7 to 1.75; does not resize the window", canResize),
            ("piece.open", "Open public piece in browser", titleURL != nil),
            ("piece.publish", "Publish current source", session.signedIn && !session.busy),
            ("account.signin", "Open AC sign-in", !session.signedIn),
            ("account.signin.close", "Close sign-in", session.showSignIn),
            ("account.signin.retry", "Retry sign-in", session.showSignIn && session.signInError != nil),
            ("account.signout", "Sign out of AC", session.signedIn),
            ("balance.refresh", "Refresh balance", session.signedIn),
            ("credits.buy", "Open App Store purchase confirmation", session.signedIn && !braincells.busy && braincells.product != nil),
            ("source.open", "Open source and revisions", !session.busy),
            ("provider.select", "Choose connected provider", !session.busy && session.hostOperationID == nil),
            ("model.select", "Choose provider model", session.provider != "ac" && !session.busy && session.hostOperationID == nil)
        ]
        return ["schema": 1, "surface": expandedPreview ? "preview" : showSettings ? "settings" : showHome ? "home" : "notebook",
                "overlays": ["settings": showSettings, "home": showHome, "help": showHelp, "signin": session.showSignIn],
                "piece": ["route": session.route, "version": session.currentRevision, "sourceBytes": session.source.utf8.count,
                          "previewURL": session.previewURL?.absoluteString ?? "", "shareURL": session.shareURL?.absoluteString ?? ""],
                "composer": ["characters": draft.count, "placeholder": "", "focused": writing],
                "title": ["opacity": 1, "linked": titleURL != nil], "footer": "v\(session.currentRevision)",
                "session": ["id": session.currentSessionID, "busy": session.busy, "status": session.status, "signedIn": session.signedIn,
                            "entryCount": session.entries.count, "history": session.history.map { ["id": $0.id, "title": $0.title, "route": $0.route] }],
                "provider": ["selected": session.provider, "ready": session.providerReady,
                             "model": session.model, "reportedModel": session.reportedModel,
                             "operationID": session.hostOperationID ?? "",
                             "choices": session.providers.map { ["id": $0.id, "available": $0.available, "notice": $0.notice] },
                             "models": session.modelChoices.map { ["id": $0.id, "title": $0.title] }],
                "preview": ["visible": previewVisible, "expanded": expandedPreview, "width": previewBounds.width, "height": previewBounds.height, "right": previewBounds.right, "top": previewBounds.top],
                "layout": ["uiScale": uiScale, "compact": compact, "width": sheetSize.width, "height": sheetSize.height, "row": row, "paperTop": paperTop, "previewBlockHeight": previewBlockHeight, "composerHeight": composerHeight],
                "notebook": ["visible": !expandedPreview && !showHome && !showSettings && !showHelp && !session.showSignIn],
                "controls": controls.map { ["id": $0.0, "label": $0.1, "enabled": $0.2] }]
    }

    private func automationAction(_ action: String, _ params: [String: Any]) async throws {
        guard params["expectedSessionID"] as? String == session.currentSessionID else { throw AeselAutomation.error("Stale or missing expectedSessionID; inspect state before acting") }
        let controls = automationState["controls"] as? [[String: Any]] ?? []
        guard let control = controls.first(where: { $0["id"] as? String == action }) else { throw AeselAutomation.error("Unknown control: \(action)") }
        guard control["enabled"] as? Bool == true else { throw AeselAutomation.error("Control is disabled: \(action)") }
        switch action {
        case "settings.open": openSettings()
        case "settings.close": closeSettings()
        case "home.open": showHome = true
        case "home.close": showHome = false
        case "help.open": showHelp = true
        case "help.close": showHelp = false
        case "composer.set":
            guard let text = params["text"] as? String, text.utf8.count <= 32768 else { throw AeselAutomation.error("text is required, at most 32768 bytes") }
            draft = text
        case "composer.clear": draft = ""
        case "composer.send": send()
        case "turn.stop": host.stop()
        case "turn.reconnect": host.resumeHostTurn()
        case "session.retry": host.restore()
        case "session.new": host.newSession(medium: "piece"); showHome = false
        case "session.resume":
            guard let id = params["sessionId"] as? String, session.history.contains(where: { $0.id == id }) else { throw AeselAutomation.error("Known sessionId is required") }
            host.resumeSession(id: id); showHome = false
        case "preview.expand": expandedPreview = true; writing = false
        case "preview.collapse": expandedPreview = false
        case "preview.hide": previewHidden = true; expandedPreview = false
        case "preview.show": previewHidden = false
        case "preview.resize":
            #if os(macOS)
            guard let width = params["width"] as? Double, let height = params["height"] as? Double,
                  (96...2400).contains(width), (72...1800).contains(height),
                  let window = NSApp.windows.first(where: { !($0 is NSPanel) && $0.contentView != nil }) else { throw AeselAutomation.error("Preview dimensions are out of range") }
            var bounds = previewBounds
            bounds.width = width; bounds.height = height
            previewBounds = bounds.fitted(in: sheetSize)
            #else
            throw AeselAutomation.error("Automation resizing is macOS-only")
            #endif
        case "preview.reload": host.automation.preview?.reload()
        case "preview.retry": host.automation.retryPreview?()
        case "ui.scale":
            guard let scale = params["scale"] as? Double, scale.isFinite, (0.7...1.75).contains(scale) else { throw AeselAutomation.error("UI scale must be 0.7–1.75") }
            uiScale = scale
        case "window.resize":
            #if os(macOS)
            guard let width = params["width"] as? Double, let height = params["height"] as? Double,
                  (220...2400).contains(width), (160...1800).contains(height),
                  let window = NSApp.windows.first(where: { !($0 is NSPanel) && $0.contentView != nil }) else { throw AeselAutomation.error("Window width 220–2400 and height 160–1800 are required") }
            window.setContentSize(NSSize(width: width, height: height))
            #else
            throw AeselAutomation.error("Window resizing is macOS-only")
            #endif
        case "piece.open": if let url = titleURL { openURL(url) }
        case "piece.publish": host.publish()
        case "account.signin", "account.signin.retry": host.signIn()
        case "account.signin.close": session.showSignIn = false
        case "account.signout": host.signOut()
        case "balance.refresh": host.refreshCredits()
        case "credits.buy": await braincells.buy()
        case "source.open": showSource = true
        case "provider.select":
            guard let id = params["provider"] as? String, session.providers.contains(where: { $0.id == id && $0.available }) else { throw AeselAutomation.error("Connected provider required") }
            host.setProvider(id)
        case "model.select":
            guard let id = params["model"] as? String, session.modelChoices.contains(where: { $0.id == id }) else { throw AeselAutomation.error("Known model required") }
            host.setModel(id: id)
        default: throw AeselAutomation.error("Unsupported control: \(action)")
        }
    }
}

/// Puts the hidden session webview in the hierarchy without drawing it.
private struct HostCarrier: AeselWebViewRepresentable {
    let host: SessionHost

    func makeWebView(context: Context) -> WKWebView { host.attachable }
    func updateWebView(_ view: WKWebView, context: Context) {}
}


private struct SignInCarrier: AeselWebViewRepresentable {
    let host: SessionHost
    @Environment(\.colorScheme) private var colorScheme
    func makeWebView(context: Context) -> WKWebView { host.signInView }
    func updateWebView(_ view: WKWebView, context: Context) { ApplePlatform.setAppearance(view, colorScheme: colorScheme) }
}
