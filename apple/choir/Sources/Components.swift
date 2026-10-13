import SwiftUI

// choir's component library: the shared pieces every screen uses, so they look
// and behave the same everywhere. See apple/choir/COMPONENTS.md. The button
// styles (SendStyle, CuteDoorStyle, CircleChipStyle, GlassChipStyle,
// DeskButtonStyle) live in Theme.swift.

/// A confirmation for anything that can't be undone, like deleting a message.
/// A short paper card slides up from the bottom: a title, one line of what
/// happens, then Cancel and the action. Use it instead of a confirmationDialog,
/// which on a long-press menu just reopens as a second menu (jeffrey, 10-12:
/// "it should be more like a confirmation, and more elegant").
///
///     .confirm($asking, "Delete this message?", detail: "It's removed for everyone.",
///              action: "Delete") { … }
struct ConfirmCard: View {
    let title: String
    var detail: String?
    let action: String
    var destructive = true
    var perform: () -> Void
    @Environment(\.dismiss) private var dismiss

    var body: some View {
        VStack(spacing: 14) {
            Text(title).font(.system(size: 19, weight: .semibold, design: .serif)).foregroundStyle(Theme.ink)
            if let detail { Text(detail).font(Theme.type(14)).foregroundStyle(Theme.ink.opacity(0.65)) }
            HStack(spacing: 12) {
                Button("Cancel") { dismiss() }
                    .buttonStyle(ConfirmButtonStyle(kind: .quiet))
                Button(action) { dismiss(); perform() }
                    .buttonStyle(ConfirmButtonStyle(kind: destructive ? .destructive : .primary))
            }
            .padding(.top, 4)
        }
        .multilineTextAlignment(.center)
        .padding(.horizontal, 22).padding(.top, 26).padding(.bottom, 12)
        .frame(maxWidth: .infinity)
        .presentationDetents([.height(detail == nil ? 170 : 200)])
        .presentationCornerRadius(28)
        .presentationBackground(Theme.paper)
        .presentationDragIndicator(.hidden)
    }
}

struct ConfirmButtonStyle: ButtonStyle {
    enum Kind { case quiet, primary, destructive }
    var kind: Kind
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(.system(size: 16, weight: .semibold, design: .rounded))
            .foregroundStyle(kind == .quiet ? Theme.ink : .white)
            .frame(maxWidth: .infinity).frame(height: 48)
            .background(Capsule().fill(kind == .quiet ? Theme.desk : kind == .destructive ? Theme.head : Theme.ink))
            .scaleEffect(configuration.isPressed ? 0.97 : 1)
            .animation(.easeOut(duration: 0.12), value: configuration.isPressed)
    }
}

extension View {
    /// Ask before doing something that can't be undone. See ConfirmCard.
    func confirm(_ isPresented: Binding<Bool>, _ title: String, detail: String? = nil, action: String,
                 destructive: Bool = true, perform: @escaping () -> Void) -> some View {
        sheet(isPresented: isPresented) {
            ConfirmCard(title: title, detail: detail, action: action, destructive: destructive, perform: perform)
        }
    }
}
