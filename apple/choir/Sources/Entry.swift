import SwiftUI

/// The signed-out front: Fía's mark and AC's two doors.
struct Entry: View {
    let onDoor: (SignInView.Door) -> Void
    var body: some View {
        VStack(spacing: 14) {
            Image("Mark").renderingMode(.template).resizable().scaledToFit()
                .frame(height: 64).foregroundStyle(.primary).accessibilityHidden(true)
            Text("A room for people who work in art.").font(.subheadline).foregroundStyle(.secondary)
            HStack(spacing: 12) {
                Button { onDoor(.login) } label: { Text("Log in").font(.title3.bold()) }
                    .buttonStyle(CurtainAccountButtonStyle())
                Button { onDoor(.new) } label: { Text("I'm new").font(.title3.bold()) }
                    .buttonStyle(CurtainAccountButtonStyle(signUp: true))
            }
        }
        .frame(maxWidth: .infinity).padding(.vertical, 18)
    }
}
