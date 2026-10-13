import SwiftUI

/// The signed-out front: AC's two doors.
struct Entry: View {
    let onDoor: (SignInView.Door) -> Void
    var body: some View {
        HStack(spacing: 12) {
            Button { onDoor(.login) } label: { Text("Log in").font(Theme.chrome(18)) }
                .buttonStyle(CurtainAccountButtonStyle())
            Button { onDoor(.new) } label: { Text("I'm new").font(Theme.chrome(18)) }
                .buttonStyle(CurtainAccountButtonStyle(signUp: true))
        }
        .frame(maxWidth: .infinity).padding(.vertical, 14)
    }
}
