// AppState.swift
// Global app state for Aesthetic TV
// 2026.01.01

import SwiftUI
import Combine

@MainActor
class AppState: ObservableObject {
    @Published var tapes: [Tape] = []
    @Published var currentIndex: Int = 0
    @Published var isLoading: Bool = true
    @Published var error: String? = nil
    @Published var showOverlay: Bool = false
    
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
    
    private let feedService = FeedService()
    
    init() {
        Task {
            await loadTapes()
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
        } catch {
            self.error = error.localizedDescription
            isLoading = false
        }
    }
    
    func nextTape() {
        guard !tapes.isEmpty else { return }
        
        if shuffle {
            currentIndex = Int.random(in: 0..<tapes.count)
        } else {
            currentIndex = (currentIndex + 1) % tapes.count
        }
    }
    
    func previousTape() {
        guard !tapes.isEmpty else { return }
        
        if shuffle {
            currentIndex = Int.random(in: 0..<tapes.count)
        } else {
            currentIndex = (currentIndex - 1 + tapes.count) % tapes.count
        }
    }
    
    func toggleOverlay() {
        showOverlay.toggle()
    }
    
    func togglePlayback() {
        isPlaying.toggle()
    }
}
