import SwiftUI

struct AeselVersionList: View {
    let session: Session
    let host: SessionHost
    @State private var selected: Int?
    @StateObject private var preview = PiecePreview()
    @Environment(\.paint) private var paint

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("Versions").font(Paint.font(15)).fontWeight(.semibold)
            ForEach(session.revisions.reversed()) { revision in
                VStack(alignment: .leading, spacing: 10) {
                    Button {
                        if selected == revision.id { selected = nil }
                        else { selected = revision.id; host.previewRevision(revision.id, threadID: session.currentSessionID) }
                    } label: {
                        VStack(alignment: .leading, spacing: 5) {
                            HStack {
                                Text("v\(revision.id)").fontWeight(.bold)
                                if revision.id == session.currentRevision {
                                    Image(systemName: "checkmark").font(.system(size: 11, weight: .semibold)).accessibilityLabel("Current version")
                                }
                                Spacer()
                                if let date = ISO8601DateFormatter().date(from: revision.at) {
                                    Text(date, format: .dateTime.month(.abbreviated).day().hour().minute()).foregroundStyle(paint.dim)
                                }
                            }
                            Text(revision.reason.isEmpty ? (revision.id == 0 ? "First version." : "Saved changes.") : revision.reason)
                                .multilineTextAlignment(.leading)
                        }.font(Paint.font(13)).frame(maxWidth: .infinity, alignment: .leading).padding(10)
                            .background(paint.accent.opacity(selected == revision.id ? 0.22 : 0.08), in: RoundedRectangle(cornerRadius: 7))
                            .overlay { RoundedRectangle(cornerRadius: 7).stroke(paint.accent.opacity(selected == revision.id ? 0.7 : 0.18), lineWidth: 1) }
                    }.accessibilityLabel("Preview version \(revision.id). \(revision.reason)")
                    if selected == revision.id, let value = session.inspectedRevision, value.version == revision.id {
                        PieceView(url: Session.draftPreviewURL, source: value.source, preview: preview).frame(height: 150).clipped()
                        if revision.id != session.currentRevision {
                            Button("Restore v\(revision.id)") { host.restoreRevision(revision.id, threadID: session.currentSessionID); selected = nil }
                                .padding(.horizontal, 12).padding(.vertical, 8)
                                .background(paint.accent.opacity(0.25), in: RoundedRectangle(cornerRadius: 6))
                                .disabled(session.busy || session.hostOperationID != nil)
                        }
                    }
                }
            }
        }.onAppear { preview.volume = 0 }
            .onChange(of: session.currentSessionID) { selected = nil }
    }
}
