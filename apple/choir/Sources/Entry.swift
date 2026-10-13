import SwiftUI

/// The signed-out front: one line and AC's two doors, centered.
struct Entry: View {
    let onDoor: (SignInView.Door) -> Void
    var body: some View {
        VStack(spacing: 22) {
            Text("Make notes on culture").font(Theme.type(22)).foregroundStyle(Theme.ink)
            HStack(spacing: 14) {
                Button("Log in") { onDoor(.login) }.buttonStyle(CuteDoorStyle())
                Button("I'm new") { onDoor(.new) }.buttonStyle(CuteDoorStyle(new: true))
            }
        }
        .frame(maxWidth: .infinity, minHeight: 240)
    }
}
