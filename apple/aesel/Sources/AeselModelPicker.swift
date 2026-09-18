import SwiftUI

/// Braincell routing is managed by AC; old callers also get a read-only label.
struct AeselModelPicker: View {
    let session: Session
    let host: SessionHost
    @Environment(\.paint) private var paint

    var body: some View {
        Text("Braincells · automatic model")
            .font(Paint.font(16)).foregroundStyle(paint.dim)
    }
}
