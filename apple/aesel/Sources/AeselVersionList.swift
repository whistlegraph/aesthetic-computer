import SwiftUI

struct AeselVersionList: View {
    let session: Session
    let host: SessionHost
    var onSelect: () -> Void = {}
    @Environment(\.paint) private var paint

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            Text("Versions").font(Paint.font(15)).fontWeight(.semibold)
            ForEach(session.revisions.reversed()) { revision in
                Button {
                    host.selectRevision(revision.id)
                    onSelect()
                } label: {
                    VStack(alignment: .leading, spacing: 5) {
                        HStack {
                            Text("v\(revision.id)").fontWeight(.bold)
                            if revision.id == session.displayedRevision {
                                Image(systemName: "checkmark").font(.system(size: 11, weight: .semibold)).accessibilityLabel("Selected version")
                            }
                            if revision.id == session.currentRevision { Text("Current").foregroundStyle(paint.dim) }
                            Spacer()
                        }
                        Text(revision.reason.isEmpty ? (revision.id == 0 ? "First version." : "Saved changes.") : revision.reason)
                            .multilineTextAlignment(.leading)
                    }.font(Paint.font(13)).frame(maxWidth: .infinity, alignment: .leading).padding(10)
                        .background(paint.accent.opacity(revision.id == session.displayedRevision ? 0.22 : 0.08), in: RoundedRectangle(cornerRadius: 7))
                        .overlay { RoundedRectangle(cornerRadius: 7).stroke(paint.accent.opacity(0.18), lineWidth: 1) }
                }
                .disabled(session.busy || session.hostOperationID != nil)
                .accessibilityLabel("View version \(revision.id)\(revision.id == session.currentRevision ? ", current" : ", read only"). \(revision.reason)")
            }
        }
    }
}
