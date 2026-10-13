import SwiftUI

/// The signed-out front: the singing mark, one line, and AC's two doors.
struct Entry: View {
    let onDoor: (SignInView.Door) -> Void
    @State private var shown = false
    var body: some View {
        VStack(spacing: 12) {
            SingingMark(height: 84)
                .scaleEffect(shown ? 1 : 0.85).opacity(shown ? 1 : 0)
            Text("Comments on culture").font(Theme.type(16)).foregroundStyle(Theme.ink.opacity(0.8))
                .opacity(shown ? 1 : 0)
            HStack(spacing: 10) {
                Button("Log in") { onDoor(.login) }.buttonStyle(CuteDoorStyle(small: true))
                Button("I'm new") { onDoor(.new) }.buttonStyle(CuteDoorStyle(new: true, small: true))
            }
            .opacity(shown ? 1 : 0).offset(y: shown ? 0 : 8)
            .padding(.top, 2)
        }
        .frame(maxWidth: .infinity).padding(.vertical, 6)
        .onAppear { withAnimation(.spring(response: 0.7, dampingFraction: 0.7).delay(0.1)) { shown = true } }
    }
}
