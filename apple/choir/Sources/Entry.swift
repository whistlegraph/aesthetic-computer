import SwiftUI

/// The signed-out front: the singing mark, one line, and AC's two doors.
struct Entry: View {
    let onDoor: (SignInView.Door) -> Void
    @State private var shown = false
    var body: some View {
        VStack(spacing: 24) {
            SingingMark(height: 170)
                .scaleEffect(shown ? 1 : 0.85).opacity(shown ? 1 : 0)
            Text("Comments on culture").font(Theme.type(24)).foregroundStyle(Theme.ink)
                .opacity(shown ? 1 : 0).offset(y: shown ? 0 : 8)
            HStack(spacing: 14) {
                Button("Log in") { onDoor(.login) }.buttonStyle(CuteDoorStyle())
                Button("I'm new") { onDoor(.new) }.buttonStyle(CuteDoorStyle(new: true))
            }
            .opacity(shown ? 1 : 0).offset(y: shown ? 0 : 14)
        }
        .frame(maxWidth: .infinity, minHeight: 440)
        .onAppear { withAnimation(.spring(response: 0.7, dampingFraction: 0.7).delay(0.1)) { shown = true } }
    }
}
