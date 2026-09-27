import SwiftUI

struct AeselModelPicker: View {
    let session: Session
    let host: SessionHost
    @Environment(\.paint) private var paint
    @State private var expanded = false

    var body: some View {
        Button { expanded.toggle() } label: {
            HStack {
                Text(session.provider == "ac" ? "Automatic" : session.modelChoices.first { $0.id == session.model }?.title ?? (session.model.isEmpty ? "CLI default" : session.model))
                    .lineLimit(1).truncationMode(.middle)
                Spacer()
                Image(systemName: "chevron.down").font(.system(size: 11))
            }.frame(maxWidth: .infinity).frame(height: 36)
        }
        .disabled(session.provider == "ac" || session.busy || session.hostOperationID != nil || session.modelChoices.isEmpty)
        .accessibilityLabel("Model")
        .popover(isPresented: $expanded, arrowEdge: .bottom) {
            ScrollView {
                VStack(alignment: .leading, spacing: 4) {
                    ForEach(session.modelChoices) { model in
                        Button { host.setModel(id: model.id); expanded = false } label: {
                            HStack {
                                Text(model.title)
                                Spacer()
                                if session.model == model.id { Image(systemName: "checkmark") }
                            }.padding(8).frame(maxWidth: .infinity, minHeight: 36)
                        }
                    }
                }.padding(8)
            }.frame(width: 300).frame(maxHeight: 360)
                .font(Paint.font(15)).foregroundStyle(paint.ink).background(paint.bg)
                .buttonStyle(AeselButtonStyle()).presentationCompactAdaptation(.popover)
        }
    }
}
