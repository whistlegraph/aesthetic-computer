// PlayerView.swift
// Fullscreen AVPlayer view for tape playback
// 2026.01.01

import SwiftUI
import AVKit

struct PlayerView: UIViewControllerRepresentable {
    let player: AVPlayer
    
    func makeUIViewController(context: Context) -> AVPlayerViewController {
        let controller = AVPlayerViewController()
        controller.player = player
        controller.showsPlaybackControls = false // We handle controls ourselves
        controller.videoGravity = .resizeAspect
        return controller
    }
    
    func updateUIViewController(_ uiViewController: AVPlayerViewController, context: Context) {
        // Player updates handled by Coordinator
    }
}

// Player coordinator that manages playback state and transitions
@MainActor
class PlayerCoordinator: ObservableObject {
    @Published var isPlaying: Bool = true
    @Published var progress: Double = 0
    @Published var duration: Double = 0
    
    nonisolated(unsafe) var player: AVPlayer
    nonisolated(unsafe) private var timeObserver: Any?
    private var itemObserver: NSKeyValueObservation?
    
    var onPlaybackEnded: (() -> Void)?
    
    init() {
        self.player = AVPlayer()
        setupObservers()
    }
    
    deinit {
        if let observer = timeObserver {
            player.removeTimeObserver(observer)
        }
        NotificationCenter.default.removeObserver(self)
    }
    
    func loadTape(_ tape: Tape) {
        guard let url = tape.videoURL else {
            print("❌ Invalid video URL for tape: \(tape.id)")
            return
        }
        
        print("📺 Loading tape: \(tape.id) from \(url)")
        
        let item = AVPlayerItem(url: url)
        player.replaceCurrentItem(with: item)
        
        // Observe when item finishes
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(playerDidFinishPlaying),
            name: .AVPlayerItemDidPlayToEndTime,
            object: item
        )
        
        if isPlaying {
            player.play()
        }
    }
    
    func play() {
        isPlaying = true
        player.play()
    }
    
    func pause() {
        isPlaying = false
        player.pause()
    }
    
    func togglePlayback() {
        if isPlaying {
            pause()
        } else {
            play()
        }
    }
    
    @objc private func playerDidFinishPlaying() {
        onPlaybackEnded?()
    }
    
    private func setupObservers() {
        // Periodic time observer for progress
        let interval = CMTime(seconds: 0.5, preferredTimescale: CMTimeScale(NSEC_PER_SEC))
        timeObserver = player.addPeriodicTimeObserver(forInterval: interval, queue: .main) { [weak self] time in
            guard let self = self,
                  let item = self.player.currentItem,
                  item.duration.isNumeric else { return }
            
            self.progress = time.seconds
            self.duration = item.duration.seconds
        }
    }
    
    private func removeObservers() {
        if let observer = timeObserver {
            player.removeTimeObserver(observer)
        }
        NotificationCenter.default.removeObserver(self)
    }
}
