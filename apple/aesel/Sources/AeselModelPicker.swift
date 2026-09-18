import SwiftUI

/// Select the saved thread's model; the session owns validation and persistence.
struct AeselModelPicker: View {
    let session: Session
    let host: SessionHost

    var body: some View {
        Menu {
            ForEach(session.modelChoices) { choice in
                Button {
                    host.setModel(model: choice.id)
                } label: {
                    Label(choice.title + (choice.premium ? " · premium" : "") + "\n" + choice.id,
                          systemImage: choice.id == session.model ? "checkmark" : choice.premium ? "sparkles" : "circle")
                }
            }
        } label: {
            HStack(alignment: .firstTextBaseline, spacing: 5) {
                Text(session.reportedModel.isEmpty ? session.model + " · requested" : session.reportedModel)
                    .lineLimit(2)
                Text("▾")
            }
            .font(Paint.font(16))
            .foregroundStyle(Paint.dim)
        }
        .disabled(session.busy || session.modelChoices.isEmpty)
        .accessibilityLabel("Choose model")
        .accessibilityValue(session.model)
    }
}
