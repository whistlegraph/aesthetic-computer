import SwiftUI

private struct ExploreMimesActionKey: EnvironmentKey {
    static let defaultValue: (() -> Void)? = nil
}
extension EnvironmentValues {
    var exploreMimes: (() -> Void)? {
        get { self[ExploreMimesActionKey.self] }
        set { self[ExploreMimesActionKey.self] = newValue }
    }
}

// What the engine knows about every piece on this phone: the one that is open
// and the ones archived by "New piece". The engine posts the list on load and
// whenever it changes; opening one swaps the archives and reloads.
struct PieceSummary: Decodable, Identifiable {
    let id: String
    let code: String
    var name: String? = nil
    let utterance: String
    let versions: Int
    let updatedAt: String
    let current: Bool
    /// Made on the knot and not yet on this phone; opening fetches it.
    var remote: Bool? = nil
    /// A turn for this piece is queued or running on the knot.
    var working: Bool? = nil
    var date: Date? {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter.date(from: updatedAt) ?? ISO8601DateFormatter().date(from: updatedAt)
    }
    var title: String { code.isEmpty ? (name ?? "untitled") : code }
}

// Tapping the piece name: every piece, newest first, and a way to start one.
struct PiecesSheet: View {
    let pieces: [PieceSummary]
    let colors: [[Double]]
    let inference: InferenceSnapshot?
    let disabled: Bool
    let open: (String) -> Void
    let newPiece: () -> Void
    var delete: (String) -> Void = { _ in }
    var mintSession: WhistlegraphSession? = nil
    var session: WhistlegraphSession? = nil
    @Environment(\.dismiss) private var dismiss
    @StateObject private var publishing = MimePublishModel()
    @Environment(\.exploreMimes) private var exploreMimes
    @State private var showingFeed = false
    var body: some View {
        NavigationStack {
            List {
                Section {
                    Button { ButtonSounds.play(.pop); newPiece(); dismiss() } label: {
                        Label(session?.snapshot.wareID == "painting" ? "New painting" : session?.snapshot.wareID == "doom" ? "New empty WAD" : session?.snapshot.wareID == "roblox" ? "New Roblox room" : "New piece", systemImage: "plus")
                            .font(.custom("ComicRelief-Bold", size: 20, relativeTo: .title3))
                    }.disabled(disabled).accessibilityIdentifier("pieces-new")
                }
                #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
                if let session = mintSession, session.snapshot.wareID == "piece", session.snapshot.handle == "jeffrey", session.snapshot.hasPiece {
                    Section {
                        NavigationLink { WhistlegraphMintSheet(session: session) } label: {
                            Label("Mint on HEN", systemImage: "seal")
                        }.disabled(disabled).accessibilityIdentifier("pieces-mint")
                    }
                }
                #endif
                Section("mime.ac") {
                    Button {
                        ButtonSounds.play(.play)
                        if let exploreMimes { dismiss(); exploreMimes() } else { showingFeed = true }
                    } label: {
                        Label("Explore mimes", systemImage: "play.rectangle.on.rectangle")
                    }.accessibilityIdentifier("pieces-explore")
                    if let session, session.snapshot.wareID == "piece", let current = pieces.first(where: { $0.current }), !current.code.isEmpty {
                        Toggle(isOn: Binding(get: { publishing.published == true },
                                             set: { value in Task { await publishing.set(value, account: session.account) } })) {
                            Label("Publish /" + current.code, systemImage: "antenna.radiowaves.left.and.right")
                        }
                        .disabled(publishing.published == nil || publishing.busy || current.versions == 0)
                        .accessibilityIdentifier("pieces-publish")
                        if current.versions == 0 { Text("Make a first version to publish.").font(.footnote).foregroundStyle(.secondary) }
                        else if !publishing.notice.isEmpty { Text(publishing.notice).font(.footnote).foregroundStyle(.secondary) }
                    }
                }
                Section("Your projects") {
                    ForEach(pieces) { piece in
                        Button {
                            ButtonSounds.play(piece.current ? .tick : .pop)
                            if !piece.current { open(piece.id) }
                            dismiss()
                        } label: {
                            HStack(alignment: .firstTextBaseline, spacing: 12) {
                                ComicTitle(text: piece.title, size: 22)
                                VStack(alignment: .trailing, spacing: 2) {
                                    Text(piece.utterance.isEmpty ? "No requests yet" : piece.utterance)
                                        .lineLimit(1).truncationMode(.tail)
                                        .font(.custom("ComicRelief-Regular", size: 17, relativeTo: .body))
                                    Text(detail(piece)).font(.footnote).foregroundStyle(.secondary)
                                }.frame(maxWidth: .infinity, alignment: .trailing)
                                if piece.working == true { ProgressView().controlSize(.small).accessibilityLabel("Working on the knot") }
                                if piece.current { Image(systemName: "checkmark").foregroundStyle(.secondary).accessibilityLabel("Open now") }
                                else if piece.remote == true { Image(systemName: "icloud.and.arrow.down").foregroundStyle(.secondary).accessibilityLabel("On the knot") }
                            }
                        }
                        .disabled(disabled && !piece.current)
                        .accessibilityIdentifier("piece-" + (session?.snapshot.wareID == "piece" ? piece.title : piece.id))
                        // Deleting frees this phone's 5 MB piece store; the open piece stays.
                        .swipeActions(edge: .trailing, allowsFullSwipe: false) {
                            // A knot piece is not in this phone's store; nothing here to delete.
                            if !piece.current && piece.remote != true {
                                Button(role: .destructive) { ButtonSounds.play(.pop); delete(piece.id) } label: { Label("Delete", systemImage: "trash") }
                                    .disabled(disabled).accessibilityIdentifier("piece-delete-" + piece.title)
                            }
                        }
                    }
                }
            }
            .navigationTitle("Projects")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
            .task { if let session, session.snapshot.wareID == "piece", let current = pieces.first(where: { $0.current }) { await publishing.load(code: current.code, account: session.account) } }
            .fullScreenCover(isPresented: $showingFeed) { MimeFeedView { showingFeed = false; dismiss() } }
        }
        .presentationDetents([.medium, .large])
    }
    private func detail(_ piece: PieceSummary) -> String {
        let versions = piece.versions == 1 ? "1 version" : "\(piece.versions) versions"
        let where_ = piece.working == true ? "working · " : piece.remote == true ? "on the knot · " : ""
        guard let date = piece.date else { return where_ + versions }
        return where_ + versions + " · " + date.formatted(.relative(presentation: .named))
    }
}

// Tapping the handle: who is signed in, and the door out. Settings grow here.
struct AccountSheet: View {
    let handle: String
    let colors: [[Double]]
    @Binding var appearance: String
    let signIn: () -> Void
    let signOut: () -> Void
    var session: WhistlegraphSession? = nil
    @Environment(\.dismiss) private var dismiss
    @State private var confirmingSignOut = false
    @AppStorage(ButtonSounds.settingKey) private var sounds = true
    var body: some View {
        NavigationStack {
            List {
                Section {
                    if handle.isEmpty {
                        Button { ButtonSounds.play(.press); signIn(); dismiss() } label: { Label("Log in to Aesthetic Computer", systemImage: "person") }
                    } else {
                        HStack {
                            ComicTitle(text: "@" + handle, colors: colors, size: 26)
                            Spacer()
                            Text("signed in").font(.footnote).foregroundStyle(.secondary)
                        }
                    }
                }
                Section("Appearance") {
                    Picker("Appearance", selection: $appearance) {
                        Text("System").tag("system")
                        Text("Light").tag("light")
                        Text("Dark").tag("dark")
                    }.pickerStyle(.segmented)
                }
                Section {
                    Toggle("Interface sounds", isOn: $sounds).accessibilityIdentifier("account-sounds")
                        .onChange(of: sounds) { _, on in DeviceActionLog.shared.record(.setting, on ? .enabled : .disabled, control: .sounds); if on { ButtonSounds.play(.tick) } }
                } footer: { Text("Keys and buttons respect silent mode. Haptics stay on.") }
                if let session {
                    Section {
                        NavigationLink { WhistlegraphDebugLog() } label: { Label("Debug log", systemImage: "list.bullet.rectangle") }.accessibilityIdentifier("account-debug-log")
                        NavigationLink { WhistlegraphPrivacySheet(session: session) } label: { Label("AI & privacy", systemImage: "hand.raised") }
                        NavigationLink { WhistlegraphDeleteAccountSheet(session: session) } label: { Label("Delete AC account", systemImage: "trash") }
                            .disabled(handle.isEmpty).accessibilityIdentifier("account-delete")
                    }
                }
                if !handle.isEmpty {
                    Section {
                        Button(role: .destructive) { confirmingSignOut = true } label: { Label("Sign out", systemImage: "rectangle.portrait.and.arrow.right") }
                            .accessibilityIdentifier("account-sign-out")
                    } footer: { Text("Your pieces stay on this phone. AI-generated versions need a signed-in handle; local source edits do not.") }
                }
            }
            .navigationTitle(handle.isEmpty ? "Account" : "@" + handle)
            .onChange(of: appearance) { _, value in DeviceActionLog.shared.record(.setting, .succeeded, control: .appearance, [.selection: ["system", "light", "dark"].firstIndex(of: value) ?? -1]) }
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
            .confirmationDialog("Sign out of @" + handle + "?", isPresented: $confirmingSignOut, titleVisibility: .visible) {
                Button("Sign out", role: .destructive) { ButtonSounds.play(.stop); signOut(); dismiss() }
            }
        }
        .presentationDetents([.medium, .large])
    }
}

// The top-left identity, split in two: the handle opens the account sheet and
// the /code opens the pieces sheet.
struct IdentityHeader: View {
    @ObservedObject var session: WhistlegraphSession
    @Binding var appearance: String
    let size: CGFloat
    let beforeOpening: () -> Void
    @State private var showingPieces = false
    @State private var showingAccount = false
    private var signedIn: Bool { !session.snapshot.handle.isEmpty }
    private var handleText: String { "@" + session.snapshot.handle }
    private var mediaName: String {
        if !session.snapshot.code.isEmpty { return session.snapshot.code }
        guard session.snapshot.wareID != "piece", let title = session.snapshot.caption else { return "untitled" }
        let name = title.folding(options: .diacriticInsensitive, locale: Locale(identifier: "en_US_POSIX"))
            .lowercased().replacingOccurrences(of: "[^a-z0-9]+", with: "-", options: .regularExpression)
            .trimmingCharacters(in: CharacterSet(charactersIn: "-"))
        return name.isEmpty ? "untitled" : String(name.prefix(80))
    }
    private var nameLabel: some View { ComicTitle(text: "/" + mediaName, size: size) }
    var body: some View {
        HStack(spacing: 0) {
            if signedIn {
                Button {
                    beforeOpening(); ButtonSounds.play(.pop); showingAccount = true
                } label: {
                    ComicTitle(text: handleText, colors: session.snapshot.colors, size: size)
                }.buttonStyle(.plain).accessibilityLabel(handleText + ", account").accessibilityIdentifier("workspace-account")
                Button { beforeOpening(); ButtonSounds.play(.pop); showingPieces = true } label: { nameLabel }
                    .buttonStyle(.plain).accessibilityLabel(mediaName + ", projects").accessibilityIdentifier("workspace-settings")
                WarePicker(session: session, size: size, beforeSwitch: beforeOpening)
            }
        }
        .onChange(of: showingAccount) { _, open in DeviceActionLog.shared.record(.screen, open ? .presented : .dismissed, control: .account) }
        .onChange(of: showingPieces) { _, open in DeviceActionLog.shared.record(.screen, open ? .presented : .dismissed, control: .pieces) }
        .sheet(isPresented: $showingPieces) {
            PiecesSheet(pieces: session.pieces, colors: session.snapshot.colors,
                        inference: session.snapshot.inference,
                        // A turn on the knot does not pin the phone to its piece; a local one does.
                        disabled: !session.canLeaveWork,
                        open: { session.command("openPiece", piece: $0) },
                        newPiece: { session.command("newPiece") },
                        delete: { session.command("deletePiece", piece: $0) }, mintSession: session, session: session)
        }
        .sheet(isPresented: $showingAccount) {
            AccountSheet(handle: session.snapshot.handle, colors: session.snapshot.colors, appearance: $appearance,
                         signIn: { session.command("signIn") }, signOut: { session.signOut() }, session: session)
        }
    }
}
