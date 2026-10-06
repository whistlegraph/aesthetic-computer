import SwiftUI
import AVFoundation
import AVKit
import MediaPlayer

// MARK: - /pop player
//
// Plays every released /pop track natively, so music keeps going on the lock
// screen, in Control Center and over AirPlay after the web runtime sleeps.
// The catalog is the one pop.aesthetic.computer renders
// (pop/bin/publish-release-records.mjs); the last good copy is kept for
// launches without a network. Opened from the prompt with `pop` / `pop <slug>`
// through the iOSApp bridge ("pop:open").

struct PopTrack: Decodable, Identifiable, Equatable {
    let slug: String
    let title: String
    let artist: String
    let album: String?
    let duration: Double?
    let cover: URL?
    let audio: URL
    var id: String { slug }
}

private struct PopCatalog: Decodable {
    let tracks: [PopTrack]
}

@MainActor
final class PopPlayer: ObservableObject {
    static let shared = PopPlayer()

    @Published var isPresented = false
    @Published private(set) var tracks: [PopTrack] = []
    @Published private(set) var index: Int? = nil
    @Published private(set) var isPlaying = false
    @Published private(set) var elapsed: Double = 0
    @Published private(set) var duration: Double = 0
    @Published private(set) var loadError: String? = nil
    @Published private(set) var artwork: [String: UIImage] = [:]

    var current: PopTrack? { index.map { tracks[$0] } }

    private let catalogURL = URL(string: "https://pop.aesthetic.computer/releases/catalog.json")!
    private let catalogKey = "acPopCatalog"
    private let player = AVPlayer()
    private var timeObserver: Any?
    private var endObserver: NSObjectProtocol?
    private var pendingSlug: String?
    private var loading = false

    private init() {
        if let data = UserDefaults.standard.data(forKey: catalogKey),
           let catalog = try? JSONDecoder().decode(PopCatalog.self, from: data) {
            tracks = catalog.tracks
        }
        timeObserver = player.addPeriodicTimeObserver(
            forInterval: CMTime(seconds: 0.5, preferredTimescale: 600), queue: .main
        ) { [weak self] time in
            MainActor.assumeIsolated { self?.tick(time) }
        }
        NotificationCenter.default.addObserver(
            forName: AVAudioSession.interruptionNotification, object: nil, queue: .main
        ) { [weak self] note in
            let info = note.userInfo
            MainActor.assumeIsolated { self?.interrupted(info) }
        }
        registerRemoteCommands()
    }

    // MARK: opening

    func open(slug: String?) {
        isPresented = true
        pendingSlug = slug
        if !tracks.isEmpty { playPending() }
        refresh()
    }

    func refresh() {
        guard !loading else { return }
        loading = true
        Task {
            defer { loading = false }
            do {
                let request = URLRequest(url: catalogURL, cachePolicy: .reloadIgnoringLocalCacheData, timeoutInterval: 20)
                let (data, _) = try await URLSession.shared.data(for: request)
                let catalog = try JSONDecoder().decode(PopCatalog.self, from: data)
                UserDefaults.standard.set(data, forKey: catalogKey)
                let playing = current?.slug
                tracks = catalog.tracks
                index = playing.flatMap { slug in tracks.firstIndex { $0.slug == slug } }
                loadError = nil
            } catch {
                if tracks.isEmpty { loadError = "Couldn’t load /pop. Check your connection." }
            }
            playPending()
        }
    }

    private func playPending() {
        guard let slug = pendingSlug else { return }
        pendingSlug = nil
        if let i = tracks.firstIndex(where: { $0.slug == slug }) { play(at: i) }
    }

    // MARK: transport

    func play(at i: Int) {
        guard tracks.indices.contains(i) else { return }
        let session = AVAudioSession.sharedInstance()
        try? session.setCategory(.playback, mode: .default)
        try? session.setActive(true)

        index = i
        elapsed = 0
        duration = tracks[i].duration ?? 0
        let item = AVPlayerItem(url: tracks[i].audio)
        if let endObserver { NotificationCenter.default.removeObserver(endObserver) }
        endObserver = NotificationCenter.default.addObserver(
            forName: .AVPlayerItemDidPlayToEndTime, object: item, queue: .main
        ) { [weak self] _ in
            MainActor.assumeIsolated { self?.next() }
        }
        player.replaceCurrentItem(with: item)
        player.play()
        isPlaying = true
        updateNowPlaying()
        loadArtwork(for: tracks[i])
    }

    func toggle() {
        guard index != nil else { if !tracks.isEmpty { play(at: 0) }; return }
        isPlaying ? pause() : resume()
    }

    func resume() {
        guard index != nil else { return }
        try? AVAudioSession.sharedInstance().setActive(true)
        player.play()
        isPlaying = true
        updateNowPlaying()
    }

    func pause() {
        player.pause()
        isPlaying = false
        updateNowPlaying()
    }

    // The whole catalog loops: after the last track comes the first.
    func next() {
        guard !tracks.isEmpty else { return }
        play(at: ((index ?? -1) + 1) % tracks.count)
    }

    // Like a CD player: past three seconds, previous restarts the track.
    func previous() {
        guard !tracks.isEmpty else { return }
        if elapsed > 3 { seek(to: 0); return }
        play(at: ((index ?? 0) - 1 + tracks.count) % tracks.count)
    }

    func seek(to seconds: Double) {
        elapsed = seconds
        player.seek(to: CMTime(seconds: seconds, preferredTimescale: 600)) { [weak self] _ in
            Task { @MainActor in self?.updateNowPlaying() }
        }
    }

    private func tick(_ time: CMTime) {
        elapsed = time.seconds.isFinite ? time.seconds : 0
        if let seconds = player.currentItem?.duration.seconds, seconds.isFinite, seconds > 0,
           abs(seconds - duration) > 0.5 {
            duration = seconds
            updateNowPlaying()
        }
    }

    private func interrupted(_ info: [AnyHashable: Any]?) {
        guard let raw = info?[AVAudioSessionInterruptionTypeKey] as? UInt,
              let type = AVAudioSession.InterruptionType(rawValue: raw) else { return }
        switch type {
        case .began:
            isPlaying = false
            updateNowPlaying()
        case .ended:
            let options = (info?[AVAudioSessionInterruptionOptionKey] as? UInt)
                .map(AVAudioSession.InterruptionOptions.init) ?? []
            if options.contains(.shouldResume) { resume() }
        @unknown default:
            break
        }
    }

    // MARK: lock screen + Control Center

    private func registerRemoteCommands() {
        let center = MPRemoteCommandCenter.shared()
        center.playCommand.addTarget { [weak self] _ in
            Task { @MainActor in self?.resume() }
            return .success
        }
        center.pauseCommand.addTarget { [weak self] _ in
            Task { @MainActor in self?.pause() }
            return .success
        }
        center.togglePlayPauseCommand.addTarget { [weak self] _ in
            Task { @MainActor in self?.toggle() }
            return .success
        }
        center.nextTrackCommand.addTarget { [weak self] _ in
            Task { @MainActor in self?.next() }
            return .success
        }
        center.previousTrackCommand.addTarget { [weak self] _ in
            Task { @MainActor in self?.previous() }
            return .success
        }
        center.changePlaybackPositionCommand.addTarget { [weak self] event in
            guard let event = event as? MPChangePlaybackPositionCommandEvent else { return .commandFailed }
            let target = event.positionTime
            Task { @MainActor in self?.seek(to: target) }
            return .success
        }
    }

    private func updateNowPlaying() {
        guard let track = current else {
            MPNowPlayingInfoCenter.default().nowPlayingInfo = nil
            return
        }
        var info: [String: Any] = [
            MPMediaItemPropertyTitle: track.title,
            MPMediaItemPropertyArtist: track.artist,
            MPMediaItemPropertyPlaybackDuration: duration,
            MPNowPlayingInfoPropertyElapsedPlaybackTime: elapsed,
            MPNowPlayingInfoPropertyPlaybackRate: isPlaying ? 1.0 : 0.0,
            MPNowPlayingInfoPropertyMediaType: MPNowPlayingInfoMediaType.audio.rawValue,
        ]
        if let album = track.album { info[MPMediaItemPropertyAlbumTitle] = album }
        if let image = artwork[track.slug] {
            info[MPMediaItemPropertyArtwork] = MPMediaItemArtwork(boundsSize: image.size) { _ in image }
        }
        MPNowPlayingInfoCenter.default().nowPlayingInfo = info
    }

    func loadArtwork(for track: PopTrack) {
        guard artwork[track.slug] == nil, let url = track.cover else { return }
        Task {
            guard let (data, _) = try? await URLSession.shared.data(from: url),
                  let image = UIImage(data: data) else { return }
            artwork[track.slug] = image
            if current == track { updateNowPlaying() }
        }
    }
}

// MARK: - Views

private let popGrey = Color(red: grey, green: grey, blue: grey)
private let popInk = Color(red: 1, green: 240/255, blue: 200/255)

private func timecode(_ seconds: Double) -> String {
    let s = max(0, Int(seconds.rounded(.down)))
    return String(format: "%d:%02d", s / 60, s % 60)
}

struct AirPlayButton: UIViewRepresentable {
    func makeUIView(context: Context) -> AVRoutePickerView {
        let view = AVRoutePickerView()
        view.tintColor = .lightGray
        view.activeTintColor = .systemYellow
        return view
    }
    func updateUIView(_ view: AVRoutePickerView, context: Context) {}
}

struct PopCover: View {
    @ObservedObject var player: PopPlayer
    let track: PopTrack
    let size: CGFloat

    var body: some View {
        ZStack {
            Color.black.opacity(0.4)
            if let image = player.artwork[track.slug] {
                Image(uiImage: image).resizable().scaledToFill()
            }
        }
        .frame(width: size, height: size)
        .clipped()
        .onAppear { player.loadArtwork(for: track) }
    }
}

struct PopPlayerView: View {
    @ObservedObject var player = PopPlayer.shared
    @State private var scrub: Double? = nil

    var body: some View {
        VStack(spacing: 0) {
            HStack {
                Text("pop")
                    .font(.system(size: 20, weight: .bold, design: .monospaced))
                    .foregroundColor(popInk)
                Spacer()
                AirPlayButton().frame(width: 36, height: 36)
                Button { player.isPresented = false } label: {
                    Image(systemName: "chevron.down")
                        .font(.system(size: 18, weight: .semibold))
                        .foregroundColor(.white)
                        .frame(width: 44, height: 44)
                }
                .accessibilityLabel("Close")
            }
            .padding(.horizontal, 16)
            .padding(.top, 8)

            if let error = player.loadError, player.tracks.isEmpty {
                Spacer()
                Text(error).foregroundColor(.white).multilineTextAlignment(.center).padding()
                Button("Retry") { player.refresh() }.foregroundColor(.yellow)
                Spacer()
            } else {
                ScrollView {
                    LazyVStack(spacing: 0) {
                        ForEach(Array(player.tracks.enumerated()), id: \.element.id) { i, track in
                            Button { player.play(at: i) } label: { row(track, playing: player.index == i) }
                                .buttonStyle(.plain)
                        }
                    }
                }
            }

            if let track = player.current { nowPlaying(track) }
        }
        .background(popGrey.ignoresSafeArea())
        .onAppear { if player.tracks.isEmpty { player.refresh() } }
    }

    private func row(_ track: PopTrack, playing: Bool) -> some View {
        HStack(spacing: 12) {
            PopCover(player: player, track: track, size: 52)
            VStack(alignment: .leading, spacing: 3) {
                Text(track.title)
                    .font(.system(size: 17, weight: .semibold, design: .monospaced))
                    .foregroundColor(playing ? .yellow : .white)
                Text(track.artist)
                    .font(.system(size: 14, design: .monospaced))
                    .foregroundColor(.gray)
            }
            .lineLimit(1)
            Spacer()
            if let duration = track.duration {
                Text(timecode(duration))
                    .font(.system(size: 14, design: .monospaced))
                    .foregroundColor(.gray)
            }
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 8)
        .background(playing ? Color.white.opacity(0.06) : Color.clear)
        .contentShape(Rectangle())
    }

    private func nowPlaying(_ track: PopTrack) -> some View {
        VStack(spacing: 10) {
            HStack(spacing: 12) {
                PopCover(player: player, track: track, size: 64)
                VStack(alignment: .leading, spacing: 3) {
                    Text(track.title)
                        .font(.system(size: 18, weight: .bold, design: .monospaced))
                        .foregroundColor(popInk)
                    Text(track.artist)
                        .font(.system(size: 14, design: .monospaced))
                        .foregroundColor(.gray)
                }
                .lineLimit(1)
                Spacer()
            }
            Slider(
                value: Binding(get: { scrub ?? player.elapsed }, set: { scrub = $0 }),
                in: 0...max(player.duration, 1),
                onEditingChanged: { editing in
                    if !editing, let target = scrub { player.seek(to: target); scrub = nil }
                }
            )
            .accentColor(.yellow)
            HStack {
                Text(timecode(scrub ?? player.elapsed))
                Spacer()
                Text("-" + timecode(player.duration - (scrub ?? player.elapsed)))
            }
            .font(.system(size: 12, design: .monospaced))
            .foregroundColor(.gray)
            HStack(spacing: 48) {
                transport("backward.fill", "Previous") { player.previous() }
                transport(player.isPlaying ? "pause.fill" : "play.fill", player.isPlaying ? "Pause" : "Play", size: 34) {
                    player.toggle()
                }
                transport("forward.fill", "Next") { player.next() }
            }
        }
        .padding(16)
        .background(Color.black.opacity(0.35).ignoresSafeArea(edges: .bottom))
    }

    private func transport(_ symbol: String, _ label: String, size: CGFloat = 24, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Image(systemName: symbol)
                .font(.system(size: size))
                .foregroundColor(.white)
                .frame(width: 56, height: 56)
        }
        .accessibilityLabel(label)
    }
}
