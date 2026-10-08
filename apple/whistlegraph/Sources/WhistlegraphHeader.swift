import SwiftUI

// What the engine knows about every piece on this phone: the one that is open
// and the ones archived by "New piece". The engine posts the list on load and
// whenever it changes; opening one swaps the archives and reloads.
struct PieceSummary: Decodable, Identifiable {
    let id: String
    let code: String
    let utterance: String
    let versions: Int
    let updatedAt: String
    let current: Bool
    var date: Date? {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter.date(from: updatedAt) ?? ISO8601DateFormatter().date(from: updatedAt)
    }
    var title: String { code.isEmpty ? "untitled" : code }
}

// Tapping the piece name: every piece, newest first, and a way to start one.
struct PiecesSheet: View {
    let pieces: [PieceSummary]
    let colors: [[Double]]
    let inference: InferenceSnapshot?
    let disabled: Bool
    let open: (String) -> Void
    let newPiece: () -> Void
    var mintSession: WhistlegraphSession? = nil
    @Environment(\.dismiss) private var dismiss
    var body: some View {
        NavigationStack {
            List {
                Section {
                    Button { ButtonSounds.play(.pop); newPiece(); dismiss() } label: {
                        Label("New piece", systemImage: "plus")
                            .font(.custom("ComicRelief-Bold", size: 20, relativeTo: .title3))
                    }.disabled(disabled).accessibilityIdentifier("pieces-new")
                }
                #if WHISTLEGRAPH_INTERNAL_PAYMENTS && DEBUG
                if let session = mintSession, session.snapshot.handle == "jeffrey", session.snapshot.hasPiece {
                    Section {
                        NavigationLink { WhistlegraphMintSheet(session: session) } label: {
                            Label("Mint on HEN", systemImage: "seal")
                        }.disabled(disabled).accessibilityIdentifier("pieces-mint")
                    }
                }
                #endif
                Section(pieces.count == 1 ? "Your piece" : "Your pieces") {
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
                                if piece.current { Image(systemName: "checkmark").foregroundStyle(.secondary).accessibilityLabel("Open now") }
                            }
                        }
                        .disabled(disabled && !piece.current)
                        .accessibilityIdentifier("piece-" + piece.title)
                    }
                }
            }
            .navigationTitle("Pieces")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
        }
        .presentationDetents([.medium, .large])
    }
    private func detail(_ piece: PieceSummary) -> String {
        let versions = piece.versions == 1 ? "1 version" : "\(piece.versions) versions"
        guard let date = piece.date else { return versions }
        return versions + " · " + date.formatted(.relative(presentation: .named))
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
    private var handleText: String { signedIn ? "@" + session.snapshot.handle : "Log in" }
    var body: some View {
        HStack(spacing: 0) {
            // Signed out, the whole corner is one plain "Log in" that starts the
            // login; the piece code and the account sheet wait for a handle.
            Button {
                beforeOpening(); ButtonSounds.play(signedIn ? .pop : .press)
                if signedIn { showingAccount = true } else { session.command("signIn") }
            } label: {
                ComicTitle(text: handleText, colors: session.snapshot.colors, size: size)
            }.buttonStyle(.plain).accessibilityLabel(signedIn ? handleText + ", account" : "Log in").accessibilityIdentifier("workspace-account")
            if session.snapshot.wareID == "piece" && signedIn && !session.snapshot.code.isEmpty {
                Button { beforeOpening(); ButtonSounds.play(.pop); showingPieces = true } label: {
                    ComicTitle(text: "/" + session.snapshot.code, size: size)
                }.buttonStyle(.plain).accessibilityLabel(session.snapshot.code + ", pieces").accessibilityIdentifier("workspace-settings")
            }
        }
        .onChange(of: showingAccount) { _, open in DeviceActionLog.shared.record(.screen, open ? .presented : .dismissed, control: .account) }
        .onChange(of: showingPieces) { _, open in DeviceActionLog.shared.record(.screen, open ? .presented : .dismissed, control: .pieces) }
        .sheet(isPresented: $showingPieces) {
            PiecesSheet(pieces: session.pieces, colors: session.snapshot.colors,
                        inference: session.snapshot.inference,
                        disabled: session.snapshot.busy || session.capturePhase != .idle,
                        open: { session.command("openPiece", piece: $0) },
                        newPiece: { session.command("newPiece") }, mintSession: session)
        }
        .sheet(isPresented: $showingAccount) {
            AccountSheet(handle: session.snapshot.handle, colors: session.snapshot.colors, appearance: $appearance,
                         signIn: { session.command("signIn") }, signOut: { session.signOut() }, session: session)
        }
    }
}
