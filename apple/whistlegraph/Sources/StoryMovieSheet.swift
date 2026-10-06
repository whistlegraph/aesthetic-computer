import SwiftUI
import AVKit
import Photos

struct StoryMovieSheet: View {
    let url: URL
    @State private var sharing = false
    @State private var saving = false
    @State private var saved = false
    @State private var error = ""
    var body: some View {
        VStack(spacing: 12) {
            Button { save() } label: {
                HStack {
                    if saving { ProgressView() }
                    Label(saved ? "Saved to Photos" : saving ? "Saving…" : "Save video", systemImage: saved ? "checkmark" : "square.and.arrow.down")
                }.frame(maxWidth: .infinity, minHeight: 44)
            }.buttonStyle(.borderedProminent).disabled(saved || saving).accessibilityIdentifier("story-save-video")
            Button { sharing = true } label: {
                Label("Share MP4", systemImage: "square.and.arrow.up").frame(maxWidth: .infinity, minHeight: 44)
            }.buttonStyle(.bordered).disabled(saving).accessibilityIdentifier("story-share-video")
        }
        .padding(16).frame(width: 280).accessibilityIdentifier("story-movie-popover")
        .interactiveDismissDisabled(saving)
        .sheet(isPresented: $sharing) { StoryShareSheet(url: url) }
        .alert("Could not save video", isPresented: Binding(get: { !error.isEmpty }, set: { if !$0 { error = "" } })) {
            Button("OK") { error = "" }
        } message: { Text(error) }
    }
    private func save() {
        saving = true
        Task { @MainActor in
            defer { saving = false }
            let status = await PHPhotoLibrary.requestAuthorization(for: .addOnly)
            guard status == .authorized || status == .limited else { error = "Allow Whistlegraph to add videos in Settings → Privacy & Security → Photos."; return }
            do {
                try await PHPhotoLibrary.shared().performChanges { PHAssetChangeRequest.creationRequestForAssetFromVideo(atFileURL: url) }
                saved = true
            } catch { self.error = error.localizedDescription }
        }
    }
}
