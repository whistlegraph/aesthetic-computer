import SwiftUI
import AVKit
import Photos

struct StoryMovieSheet: View {
    let url: URL
    @Environment(\.dismiss) private var dismiss
    @State private var player: AVPlayer?
    @State private var sharing = false
    @State private var saving = false
    @State private var saved = false
    @State private var error = ""
    var body: some View {
        NavigationStack {
            VStack(spacing: 18) {
                VideoPlayer(player: player).aspectRatio(9.0 / 16.0, contentMode: .fit)
                HStack(spacing: 16) {
                    Button { save() } label: {
                        HStack {
                            if saving { ProgressView() }
                            Label(saved ? "Saved to Photos" : saving ? "Saving…" : "Save video", systemImage: saved ? "checkmark" : "square.and.arrow.down")
                        }
                            .frame(maxWidth: .infinity)
                    }.buttonStyle(.borderedProminent).disabled(saved || saving).accessibilityIdentifier("story-save-video")
                    Button { sharing = true } label: { Label("Share", systemImage: "square.and.arrow.up") }.buttonStyle(.bordered)
                }
            }.padding().navigationTitle("Your video").navigationBarTitleDisplayMode(.inline)
                .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() }.disabled(saving) } }
        }
        .interactiveDismissDisabled(saving)
        .onAppear { player = AVPlayer(url: url) }
        .onDisappear { player?.pause() }
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
