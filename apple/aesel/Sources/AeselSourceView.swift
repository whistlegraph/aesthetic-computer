import SwiftUI

struct AeselSourceView: View {
    let session: Session
    let host: SessionHost
    @State private var text = ""
    @State private var threadID = ""
    @State private var revision = 0
    @Environment(\.dismiss) private var dismiss
    @Environment(\.paint) private var paint
    private var locked: Bool { session.viewingHistory || session.busy || session.hostOperationID != nil || threadID != session.currentSessionID }

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            HStack {
                Text("Source · v\(session.currentRevision)").font(Paint.title(20))
                Spacer()
                Button("Close") { dismiss() }
            }
            TextEditor(text: $text)
                .font(.system(size: 14, design: .monospaced)).scrollContentBackground(.hidden)
                .background(paint.deep).disabled(locked)
                .accessibilityLabel("Piece JavaScript source")
            HStack {
                Picker("Revision", selection: $revision) {
                    ForEach(session.revisions.reversed()) { item in
                        Text("v\(item.id) · \(item.reason)").tag(item.id)
                    }
                }
                Button("Restore") { host.restoreRevision(revision, threadID: threadID) }
                    .disabled(locked || revision == session.currentRevision)
            }
            HStack {
                Text(session.publishedRevision.map { "Public: v\($0)" } ?? "Unpublished")
                    .foregroundStyle(paint.dim)
                Spacer()
                Button("Save & preview") { host.editSource(text, threadID: threadID) }
                    .disabled(locked || text == session.source)
            }
            if let error = session.entries.last, error.kind == .bad { Text(error.text).font(Paint.font(13)) }
        }
        .padding(20).frame(minWidth: 300, minHeight: 400)
        .background(paint.bg).foregroundStyle(paint.ink).font(Paint.font(15)).buttonStyle(AeselButtonStyle())
        .onAppear { text = session.source; threadID = session.currentSessionID; revision = session.currentRevision }
        .onChange(of: session.source) { text = session.source; revision = session.currentRevision }
    }
}
