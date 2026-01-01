// AppState.swift
// Global app state for Aesthetic TV
// 2026.01.01

import SwiftUI
import Combine
import AVFoundation

@MainActor
class AppState: ObservableObject {
    @Published var tapes: [Tape] = []
    @Published var currentIndex: Int = 0
    @Published var isLoading: Bool = true
    @Published var error: String? = nil
    @Published var showOverlay: Bool = true
    
    // Playback settings
    @Published var isPlaying: Bool = true
    @Published var loopPlaylist: Bool = true
    @Published var shuffle: Bool = false
    
    var currentTape: Tape? {
        guard !tapes.isEmpty, currentIndex >= 0, currentIndex < tapes.count else {
            return nil
        }
        return tapes[currentIndex]
    }
    
    /// Returns the next 8 tapes after current (for "Up Next" display)
    var upcomingTapes: [Tape] {
        guard !tapes.isEmpty else { return [] }
        var upcoming: [Tape] = []
        for i in 1...8 {
            let index = (currentIndex + i) % tapes.count
            if index != currentIndex {
                upcoming.append(tapes[index])
            }
        }
        return upcoming
    }
    
    private let feedService = FeedService()
    
    init() {
        configureAudioSession()
        
        Task {
            await loadTapes()
        }
    }
    
    private func configureAudioSession() {
        do {
            let audioSession = AVAudioSession.sharedInstance()
            // Use .playback for video content that should play even in silent mode
            try audioSession.setCategory(.playback, mode: .moviePlayback, options: [])
            try audioSession.setActive(true, options: .notifyOthersOnDeactivation)
            print("🔊 Audio session configured: category=\(audioSession.category.rawValue), mode=\(audioSession.mode.rawValue)")
        } catch {
            print("❌ Failed to configure audio session: \(error)")
        }
    }
    
    func loadTapes() async {
        isLoading = true
        error = nil
        
        do {
            let fetchedTapes = try await feedService.fetchTapes()
            tapes = fetchedTapes
            currentIndex = 0
            isLoading = false
            print("📼 Loaded \(tapes.count) tapes")
        } catch {
            self.error = error.localizedDescription
            isLoading = false
            print("❌ Failed to load tapes: \(error)")
        }
    }
    
    func nextTape() {
        guard !tapes.isEmpty else { return }
        
        if shuffle {
            currentIndex = Int.random(in: 0..<tapes.count)
        } else {
            currentIndex = (currentIndex + 1) % tapes.count
        }
        print("⏭️ Next tape: \(currentIndex) - \(currentTape?.id ?? "none")")
    }
    
    func previousTape() {
        guard !tapes.isEmpty else { return }
        
        if shuffle {
            currentIndex = Int.random(in: 0..<tapes.count)
        } else {
            currentIndex = (currentIndex - 1 + tapes.count) % tapes.count
        }
        print("⏮️ Previous tape: \(currentIndex) - \(currentTape?.id ?? "none")")
    }
    
    func toggleOverlay() {
        showOverlay.toggle()
    }
    
    func togglePlayback() {
        isPlaying.toggle()
        print("⏯️ Playback: \(isPlaying ? "playing" : "paused")")
    }
}
