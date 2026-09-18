import SwiftUI

/// Braincell routing is managed by AC; old callers also get a read-only label.
struct AeselModelPicker: View {
    let session: Session
    let host: SessionHost

    var body: some View {
        Text("Braincells · automatic model")
            .font(Paint.font(16)).foregroundStyle(Paint.dim)
    }
}
