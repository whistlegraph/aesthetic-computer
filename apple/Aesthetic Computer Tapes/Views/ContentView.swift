// ContentView.swift
// Main view for Aesthetic TV - tape playback with Siri Remote controls
// 2026.01.01

import SwiftUI
import AVKit

struct ContentView: View {
    @EnvironmentObject var appState: AppState
    @StateObject private var playerCoordinator = PlayerCoordinator()
    
    // Auto-hide overlay timer
    @State private var overlayTimer: Timer?
    
    var body: some View {
        ZStack {
            // Black background
            Color.black.ignoresSafeArea()
            
            if appState.isLoading {
                loadingView
            } else if let error = appState.error {
                errorView(error)
            } else if let tape = appState.currentTape {
                // Player
                PlayerView(player: playerCoordinator.player)
                    .ignoresSafeArea()
                
                // Overlay
                OverlayView(
                    tape: tape,
                    progress: playerCoordinator.progress,
                    duration: playerCoordinator.duration,
                    isVisible: appState.showOverlay
                )
            } else {
                emptyView
            }
        }
        .focusable()
        .onAppear {
            setupPlayer()
        }
        .onChange(of: appState.currentTape) { _, newTape in
            if let tape = newTape {
                playerCoordinator.loadTape(tape)
            }
        }
        .onChange(of: appState.isPlaying) { _, isPlaying in
            if isPlaying {
                playerCoordinator.play()
            } else {
                playerCoordinator.pause()
            }
        }
        // Siri Remote gestures
        .onPlayPauseCommand {
            appState.togglePlayback()
        }
        .onMoveCommand { direction in
            handleMoveCommand(direction)
        }
    }
    
    // MARK: - Subviews
    
    private var loadingView: some View {
        VStack(spacing: 16) {
            ProgressView()
                .scaleEffect(1.5)
            Text("Loading tapes...")
                .font(.headline)
                .foregroundColor(.white.opacity(0.7))
        }
    }
    
    private func errorView(_ error: String) -> some View {
        VStack(spacing: 16) {
            Image(systemName: "exclamationmark.triangle")
                .font(.system(size: 48))
                .foregroundColor(.yellow)
            
            Text("Unable to load tapes")
                .font(.headline)
                .foregroundColor(.white)
            
            Text(error)
                .font(.subheadline)
                .foregroundColor(.white.opacity(0.7))
                .multilineTextAlignment(.center)
            
            Button("Retry") {
                Task {
                    await appState.loadTapes()
                }
            }
            .padding(.top, 8)
        }
        .padding()
    }
    
    private var emptyView: some View {
        VStack(spacing: 16) {
            Image(systemName: "film")
                .font(.system(size: 48))
                .foregroundColor(.white.opacity(0.5))
            
            Text("No tapes available")
                .font(.headline)
                .foregroundColor(.white.opacity(0.7))
        }
    }
    
    // MARK: - Setup
    
    private func setupPlayer() {
        // Set up playback ended callback
        playerCoordinator.onPlaybackEnded = { [weak appState] in
            guard let appState = appState else { return }
            if appState.loopPlaylist {
                appState.nextTape()
            }
        }
        
        // Load first tape if available
        if let tape = appState.currentTape {
            playerCoordinator.loadTape(tape)
        }
    }
    
    // MARK: - Remote Handling
    
    private func handleMoveCommand(_ direction: MoveCommandDirection) {
        switch direction {
        case .left:
            appState.previousTape()
        case .right:
            appState.nextTape()
        case .up:
            showOverlayTemporarily()
        case .down:
            appState.showOverlay = false
        @unknown default:
            break
        }
    }
    
    private func showOverlayTemporarily() {
        appState.showOverlay = true
        
        // Cancel existing timer
        overlayTimer?.invalidate()
        
        // Auto-hide after 5 seconds
        overlayTimer = Timer.scheduledTimer(withTimeInterval: 5.0, repeats: false) { _ in
            Task { @MainActor in
                appState.showOverlay = false
            }
        }
    }
}

#Preview {
    ContentView()
        .environmentObject(AppState())
}
