import AppKit
import Foundation
import MusicKit

struct AppleMusicTrackResult {
    let title: String
    let artist: String
    let album: String
    let duration: Double
    let artworkURL: URL?
    fileprivate let song: Song
}

@available(macOS 12.0, *)
final class JukeAppleMusic {
    enum AppleMusicError: LocalizedError {
        case denied

        var errorDescription: String? {
            switch self {
            case .denied: return "Apple Music access was not granted."
            }
        }
    }

    var authorizationStatus: MusicAuthorization.Status { MusicAuthorization.currentStatus }

    func authorize() async throws {
        guard await MusicAuthorization.request() == .authorized else {
            throw AppleMusicError.denied
        }
    }

    func search(_ query: String) async throws -> [AppleMusicTrackResult] {
        if MusicAuthorization.currentStatus != .authorized { try await authorize() }
        var request = MusicCatalogSearchRequest(term: query, types: [Song.self])
        request.limit = 30
        let response = try await request.response()
        return response.songs.map { song in
            AppleMusicTrackResult(
                title: song.title,
                artist: song.artistName,
                album: song.albumTitle ?? "",
                duration: song.duration ?? 0,
                artworkURL: song.artwork?.url(width: 512, height: 512),
                song: song)
        }
    }

    @available(macOS 14.0, *)
    func play(_ result: AppleMusicTrackResult) async throws {
        let player = ApplicationMusicPlayer.shared
        player.queue = ApplicationMusicPlayer.Queue(for: [result.song])
        try await player.play()
    }

    @available(macOS 14.0, *)
    func pause() { ApplicationMusicPlayer.shared.pause() }

    @available(macOS 14.0, *)
    func toggle() async throws {
        let player = ApplicationMusicPlayer.shared
        if player.state.playbackStatus == .playing { player.pause() }
        else { try await player.play() }
    }
}

final class AppleMusicTrackRowView: NSTableCellView {
    static let id = NSUserInterfaceItemIdentifier("apple-music-track-row")
    private let titleField = NSTextField(labelWithString: "")
    private let detailField = NSTextField(labelWithString: "")
    private let durationField = NSTextField(labelWithString: "")
    var selected = false { didSet { needsDisplay = true } }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        titleField.font = .systemFont(ofSize: 13, weight: .semibold)
        titleField.lineBreakMode = .byTruncatingTail
        detailField.font = .systemFont(ofSize: 11)
        detailField.textColor = .secondaryLabelColor
        detailField.lineBreakMode = .byTruncatingTail
        durationField.font = .monospacedDigitSystemFont(ofSize: 11, weight: .regular)
        durationField.textColor = .secondaryLabelColor
        durationField.alignment = .right
        [titleField, detailField, durationField].forEach(addSubview)
    }
    required init?(coder: NSCoder) { fatalError() }

    func configure(_ track: AppleMusicTrackResult) {
        titleField.stringValue = track.title
        detailField.stringValue = [track.artist, track.album].filter { !$0.isEmpty }.joined(separator: " · ")
        durationField.stringValue = JukeController.mmss(track.duration)
    }

    override func layout() {
        titleField.frame = NSRect(x: 12, y: bounds.height - 23, width: bounds.width - 80, height: 18)
        detailField.frame = NSRect(x: 12, y: 5, width: bounds.width - 80, height: 15)
        durationField.frame = NSRect(x: bounds.width - 63, y: 13, width: 51, height: 16)
    }

    override func draw(_ dirtyRect: NSRect) {
        if selected {
            NSColor.white.withAlphaComponent(0.16).setFill()
            NSBezierPath(roundedRect: bounds.insetBy(dx: 4, dy: 2), xRadius: 6, yRadius: 6).fill()
        }
        super.draw(dirtyRect)
    }
}

final class AestheticCloudRowView: NSTableCellView {
    static let id = NSUserInterfaceItemIdentifier("aesthetic-cloud-track-row")
    private let titleField = NSTextField(labelWithString: "")
    private let detailField = NSTextField(labelWithString: "")
    var selected = false { didSet { needsDisplay = true } }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        titleField.font = .systemFont(ofSize: 13, weight: .semibold)
        titleField.lineBreakMode = .byTruncatingTail
        detailField.font = .systemFont(ofSize: 11)
        detailField.textColor = .secondaryLabelColor
        addSubview(titleField); addSubview(detailField)
    }
    required init?(coder: NSCoder) { fatalError() }

    func configure(_ track: JukeCloudTrack) {
        titleField.stringValue = track.name
        detailField.stringValue = ByteCountFormatter.string(fromByteCount: track.bytes, countStyle: .file)
    }

    override func layout() {
        titleField.frame = NSRect(x: 12, y: bounds.height - 23, width: bounds.width - 24, height: 18)
        detailField.frame = NSRect(x: 12, y: 5, width: bounds.width - 24, height: 15)
    }

    override func draw(_ dirtyRect: NSRect) {
        if selected {
            Palette.coral.withAlphaComponent(0.20).setFill()
            NSBezierPath(roundedRect: bounds.insetBy(dx: 4, dy: 2), xRadius: 6, yRadius: 6).fill()
        }
        super.draw(dirtyRect)
    }
}
