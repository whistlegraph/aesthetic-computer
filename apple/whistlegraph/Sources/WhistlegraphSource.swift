import SwiftUI
import WebKit

struct WhistlegraphSourceDocument: Decodable {
    let piece: String
    let code: String
    let version: Int
    let source: String
    let sourceHash: String
    // Include the source hash so a draft can never replace a different revision.
    var draftKey: String { "whistlegraph-source-draft:\(piece):\(version):\(sourceHash)" }
}

extension WhistlegraphSession {
    func sourceDocument() async throws -> WhistlegraphSourceDocument {
        DeviceActionLog.shared.record(.source, .requested)
        return try await sourceOperation("read", arguments: [:])
    }

    func applySource(_ source: String, to document: WhistlegraphSourceDocument) async throws -> WhistlegraphSourceDocument {
        DeviceActionLog.shared.record(.source, .requested, [.characters: source.count, .version: document.version])
        guard !snapshot.busy, capturePhase == .idle else {
            throw sourceFailure("Finish the current request or recording before editing source.")
        }
        return try await sourceOperation("apply", arguments: ["piece": document.piece, "version": document.version,
            "sourceHash": document.sourceHash, "source": source])
    }

    private func sourceFailure(_ message: String) -> NSError {
        NSError(domain: "WhistlegraphSource", code: 1, userInfo: [NSLocalizedDescriptionKey: message])
    }

    private func sourceOperation(_ action: String, arguments: [String: Any]) async throws -> WhistlegraphSourceDocument {
        guard engineReady, let webView else { throw sourceFailure("The piece is still loading.") }
        // Source travels as a structured argument, never interpolated JavaScript.
        let value = try await webView.callAsyncJavaScript("""
            try {
              const editor = window.whistlegraphSourceEditor;
              if (!editor) throw Error('Source is not ready.');
              return {document: await (action === 'read' ? editor.read() : editor.apply(request))};
            } catch (error) { return {error: error.message || 'Could not edit source.'}; }
            """, arguments: ["action": action, "request": arguments], in: nil, contentWorld: .page)
        guard let response = value as? [String: Any] else { throw sourceFailure("Could not read source.") }
        if let error = response["error"] as? String { throw sourceFailure(error) }
        guard let document = response["document"] as? [String: Any] else { throw sourceFailure("Could not read source.") }
        return try JSONDecoder().decode(WhistlegraphSourceDocument.self, from: JSONSerialization.data(withJSONObject: document))
    }
}

// Present inside a NavigationStack. The complete source stays selectable and
// editable, independently of the abbreviated streaming ticker on the workspace.
struct WhistlegraphSourceSheet: View {
    @ObservedObject var session: WhistlegraphSession
    @State private var document: WhistlegraphSourceDocument?
    @State private var draft = ""
    @State private var working = false
    @State private var notice = ""
    @State private var showingReset = false
    @FocusState private var editing: Bool
    private var changed: Bool { document.map { draft != $0.source } ?? false }

    var body: some View {
        VStack(spacing: 12) {
            if let document {
                HStack {
                    Text("Version \(document.version)").font(.headline)
                    Spacer()
                    Text(changed ? "Draft saved on this phone" : "Saved source").font(.footnote).foregroundStyle(.secondary)
                }
                TextEditor(text: $draft)
                    .font(.system(.body, design: .monospaced))
                    .textInputAutocapitalization(.never).autocorrectionDisabled(true)
                    .focused($editing).disabled(working)
                    .accessibilityLabel("Complete piece source").accessibilityIdentifier("source-editor")
                    .overlay(RoundedRectangle(cornerRadius: 8).stroke(.secondary.opacity(0.3)))
                if !notice.isEmpty {
                    Text(notice).font(.callout).frame(maxWidth: .infinity, alignment: .leading)
                        .accessibilityIdentifier("source-notice")
                }
                Button {
                    editing = false
                    Task { await apply() }
                } label: {
                    HStack {
                        if working { ProgressView() }
                        Text(working ? "Checking preview…" : "Apply local edit")
                    }.frame(maxWidth: .infinity)
                }
                .buttonStyle(.borderedProminent)
                .disabled(working || !changed || session.snapshot.busy || session.capturePhase != .idle)
                .accessibilityIdentifier("source-apply")
                Text("Checks the code and preview, then saves a new version. Uses no AI or braincells.")
                    .font(.footnote).foregroundStyle(.secondary)
            } else if working {
                ProgressView("Loading source…").frame(maxWidth: .infinity, maxHeight: .infinity)
            } else {
                ContentUnavailableView {
                    Label("Source unavailable", systemImage: "curlybraces")
                } description: { Text(notice) } actions: { Button("Retry") { Task { await load() } } }
            }
        }
        .padding()
        .navigationTitle("Source").navigationBarTitleDisplayMode(.inline)
        .toolbar {
            ToolbarItemGroup(placement: .topBarTrailing) {
                if document != nil {
                    Button { UIPasteboard.general.string = draft; notice = "Source copied." } label: {
                        Image(systemName: "doc.on.doc")
                    }.accessibilityLabel("Copy complete source").accessibilityIdentifier("source-copy")
                    ShareLink(item: draft) { Image(systemName: "square.and.arrow.up") }
                        .accessibilityLabel("Share complete source").accessibilityIdentifier("source-share")
                    Button { showingReset = true } label: { Image(systemName: "arrow.counterclockwise") }
                        .disabled(!changed || working).accessibilityLabel("Discard source draft")
                }
            }
            ToolbarItemGroup(placement: .keyboard) {
                Spacer()
                Button("Done") { editing = false }
            }
        }
        .confirmationDialog("Discard this source draft?", isPresented: $showingReset, titleVisibility: .visible) {
            Button("Discard draft", role: .destructive) {
                if let document { draft = document.source; UserDefaults.standard.removeObject(forKey: document.draftKey); notice = "" }
            }
        }
        .onChange(of: draft) { _, source in
            guard let document else { return }
            if source == document.source { UserDefaults.standard.removeObject(forKey: document.draftKey) }
            else { UserDefaults.standard.set(source, forKey: document.draftKey) }
        }
        .task { if document == nil { await load() } }
    }

    @MainActor private func load() async {
        working = true
        defer { working = false }
        do {
            let value = try await session.sourceDocument()
            document = value
            draft = UserDefaults.standard.string(forKey: value.draftKey) ?? value.source
            notice = ""
        } catch { notice = error.localizedDescription }
    }

    @MainActor private func apply() async {
        guard let original = document else { return }
        working = true; notice = ""
        defer { working = false }
        do {
            let saved = try await session.applySource(draft, to: original)
            UserDefaults.standard.removeObject(forKey: original.draftKey)
            document = saved; draft = saved.source
            notice = "Saved version \(saved.version)."
        } catch { notice = error.localizedDescription }
    }
}
