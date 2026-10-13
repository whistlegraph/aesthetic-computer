import SwiftUI
import AVFoundation
import AVKit
import CoreTransferable
import UniformTypeIdentifiers

/// Videos (Fía, 10-12): "Video support for ppl to record or upload a video file?"
/// Picked or recorded clips are cut to a minute, re-encoded to MP4 small enough
/// for the 25 MB upload limit, and sent with a poster frame, their size and length.
enum Videos {
    static let limit: Double = 60
    /// Leaves room under the server's 25 MB for the request to be honest about size.
    static let budget = 24 * 1024 * 1024

    struct Made { var pending: Pending; var trimmed: Bool }
    enum Problem: LocalizedError {
        case unreadable, tooBig, failed
        var errorDescription: String? {
            switch self {
            case .unreadable: "That video couldn't be opened."
            case .tooBig: "That video is too big to send, even squeezed. Try a shorter clip."
            case .failed: "That video couldn't be prepared."
            }
        }
    }

    /// A poster for the pending strip, quickly, before the clip is re-encoded.
    static func poster(_ url: URL) async -> UIImage? {
        let g = AVAssetImageGenerator(asset: AVURLAsset(url: url))
        g.appliesPreferredTrackTransform = true
        g.maximumSize = CGSize(width: 1280, height: 1280)
        g.requestedTimeToleranceBefore = .zero
        g.requestedTimeToleranceAfter = CMTime(seconds: 0.5, preferredTimescale: 600)
        guard let (cg, _) = try? await g.image(at: CMTime(seconds: 0.1, preferredTimescale: 600)) else { return nil }
        return UIImage(cgImage: cg)
    }

    /// Re-encode `url` into an MP4 that fits the budget: the first 60 s,
    /// at the best of a ladder of presets that comes in under 24 MB.
    static func prepare(_ url: URL) async throws -> Made {
        let asset = AVURLAsset(url: url)
        guard let seconds = try? await asset.load(.duration).seconds, seconds.isFinite, seconds > 0 else { throw Problem.unreadable }
        let length = min(seconds, limit)
        let trimmed = seconds > limit + 0.5
        // Rough output rates: start where a clip this long probably fits, step down if it didn't.
        let ladder = [AVAssetExportPresetHEVC1920x1080, AVAssetExportPreset1280x720, AVAssetExportPreset960x540,
                      AVAssetExportPreset640x480, AVAssetExportPresetMediumQuality]
        let start = length <= 20 ? 0 : length <= 30 ? 1 : 2
        let out = FileManager.default.temporaryDirectory.appendingPathComponent("choir-video-\(UUID().uuidString).mp4")
        defer { try? FileManager.default.removeItem(at: out) }
        for preset in ladder[start...] {
            try? FileManager.default.removeItem(at: out)
            guard let s = AVAssetExportSession(asset: asset, presetName: preset) else { continue }
            s.shouldOptimizeForNetworkUse = true
            s.timeRange = CMTimeRange(start: .zero, duration: CMTime(seconds: length, preferredTimescale: 600))
            guard s.supportedFileTypes.contains(.mp4) else { continue }
            do { try await export(s, to: out) } catch { continue }
            guard let data = try? Data(contentsOf: out) else { continue }
            if data.count > budget { continue }
            let result = AVURLAsset(url: out)
            var size = CGSize.zero
            if let track = try? await result.loadTracks(withMediaType: .video).first,
               let (natural, transform) = try? await track.load(.naturalSize, .preferredTransform) {
                let r = CGRect(origin: .zero, size: natural).applying(transform)
                size = CGSize(width: abs(r.width), height: abs(r.height))
            }
            let poster = await poster(out)
            let kept = (try? await result.load(.duration).seconds).flatMap { $0.isFinite ? $0 : nil } ?? length
            var p = Pending(data: data, name: "video-\(Int(Date().timeIntervalSince1970)).mp4", type: "video/mp4",
                            width: size.width > 0 ? Int(size.width) : poster.map { Int($0.size.width) },
                            height: size.height > 0 ? Int(size.height) : poster.map { Int($0.size.height) },
                            thumbnail: poster, duration: min(limit, kept))
            p.posterData = poster?.jpegData(compressionQuality: 0.8)
            Trail.send("video:prepared", ["seconds": Int(kept), "bytes": data.count, "preset": preset, "trimmed": trimmed])
            return Made(pending: p, trimmed: trimmed)
        }
        throw Problem.tooBig
    }

    private static func export(_ s: AVAssetExportSession, to out: URL) async throws {
        if #available(iOS 18, *) { try await s.export(to: out, as: .mp4); return }
        s.outputURL = out; s.outputFileType = .mp4
        await withCheckedContinuation { (c: CheckedContinuation<Void, Never>) in s.exportAsynchronously { c.resume() } }
        if s.status != .completed { throw s.error ?? Problem.failed }
    }
}

/// A movie from the Photos picker, copied out to a file we own.
struct PickedMovie: Transferable {
    let url: URL
    static var transferRepresentation: some TransferRepresentation {
        FileRepresentation(contentType: .movie) { SentTransferredFile($0.url) } importing: { received in
            let ext = received.file.pathExtension.isEmpty ? "mov" : received.file.pathExtension
            let copy = FileManager.default.temporaryDirectory.appendingPathComponent("choir-pick-\(UUID().uuidString).\(ext)")
            try FileManager.default.copyItem(at: received.file, to: copy)
            return Self(url: copy)
        }
    }
}

/// The system camera, for recording a clip of up to a minute.
struct VideoRecorderView: UIViewControllerRepresentable {
    var onRecorded: (URL) -> Void
    @Environment(\.dismiss) private var dismiss

    func makeUIViewController(context: Context) -> UIImagePickerController {
        let c = UIImagePickerController()
        c.sourceType = UIImagePickerController.isSourceTypeAvailable(.camera) ? .camera : .photoLibrary
        c.mediaTypes = [UTType.movie.identifier]
        if c.sourceType == .camera { c.cameraCaptureMode = .video }
        c.videoMaximumDuration = Videos.limit
        c.videoQuality = .typeMedium
        c.delegate = context.coordinator
        return c
    }
    func updateUIViewController(_ c: UIImagePickerController, context: Context) {}
    func makeCoordinator() -> Coordinator { Coordinator(self) }

    final class Coordinator: NSObject, UIImagePickerControllerDelegate, UINavigationControllerDelegate {
        let parent: VideoRecorderView
        init(_ parent: VideoRecorderView) { self.parent = parent }
        func imagePickerController(_ picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [UIImagePickerController.InfoKey: Any]) {
            if let url = info[.mediaURL] as? URL {
                // The picker's file goes away with it; keep a copy.
                let copy = FileManager.default.temporaryDirectory.appendingPathComponent("choir-shot-\(UUID().uuidString).\(url.pathExtension.isEmpty ? "mov" : url.pathExtension)")
                if (try? FileManager.default.copyItem(at: url, to: copy)) != nil { parent.onRecorded(copy) }
            }
            parent.dismiss()
        }
        func imagePickerControllerDidCancel(_ picker: UIImagePickerController) { parent.dismiss() }
    }
}

/// A video in a message: its poster with a play button and its length. Tap to play.
struct VideoTile: View {
    let video: Attachment
    var body: some View {
        ZStack {
            if let p = video.poster, let u = URL(string: p) {
                AsyncImage(url: u, transaction: Transaction(animation: .easeOut(duration: 0.2))) { phase in
                    if let image = phase.image { image.resizable().scaledToFill() }
                    else { Theme.desk.opacity(0.6).overlay(FlowerSpinner(size: 26).opacity(phase.error == nil ? 1 : 0)) }
                }
            } else {
                Theme.ink.opacity(0.85).overlay(Image(systemName: "film").font(.title).foregroundStyle(.white.opacity(0.5)))
            }
            Image(systemName: "play.fill").font(.system(size: 20, weight: .bold)).foregroundStyle(.white)
                .frame(width: 48, height: 48).background(Theme.ink.opacity(0.75), in: Circle())
        }
        .overlay(alignment: .bottomTrailing) {
            if let d = video.duration {
                Text(RecordingBar.clock(d)).font(.system(size: 11, weight: .semibold)).monospacedDigit().foregroundStyle(.white)
                    .padding(.horizontal, 6).padding(.vertical, 2).background(.black.opacity(0.55), in: Capsule()).padding(6)
            }
        }
        .accessibilityElement().accessibilityLabel("Video").accessibilityAddTraits(.isButton)
    }
}

/// Full screen: the system player, playing from the tap that opened it.
struct VideoViewer: View {
    let attachment: Attachment
    @Environment(\.dismiss) private var dismiss
    @State private var player: AVPlayer?

    var body: some View {
        VStack(spacing: 0) {
            HStack(spacing: 10) {
                Spacer()
                if let u = URL(string: attachment.url) {
                    ShareLink(item: u) { Hand("HandArrow", height: 16) }.buttonStyle(GlassChipStyle()).accessibilityLabel("Share")
                }
                Button { dismiss() } label: { Hand("HandX", height: 16) }.buttonStyle(GlassChipStyle()).accessibilityLabel("Close")
            }
            .padding()
            if let player { PlayerController(player: player) } else { Spacer() }
        }
        .background(Color.black.ignoresSafeArea())
        .onAppear {
            guard player == nil, let u = URL(string: attachment.url) else { return }
            try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .moviePlayback)
            let p = AVPlayer(url: u); player = p; p.play()
            Trail.send("video:play")
        }
        .onDisappear { player?.pause() }
    }
}

private struct PlayerController: UIViewControllerRepresentable {
    let player: AVPlayer
    func makeUIViewController(context: Context) -> AVPlayerViewController {
        let c = AVPlayerViewController(); c.player = player; c.view.backgroundColor = .black
        return c
    }
    func updateUIViewController(_ c: AVPlayerViewController, context: Context) { c.player = player }
}
