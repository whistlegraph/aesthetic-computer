// ContentView.swift
// Main view combining player, overlay, and waveform
// 2026.01.01

import SwiftUI

struct ContentView: View {
    @EnvironmentObject var appState: AppState
    @StateObject private var coordinator = PlayerCoordinator()
    
    var body: some View {
        ZStack {
            // Video player
            PlayerView(player: coordinator.player)
                .ignoresSafeArea()
            
            // Overlay with tape info, QR code, and waveform
            if let tape = appState.currentTape {
                OverlayView(
                    tape: tape,
                    progress: coordinator.progress,
                    duration: coordinator.duration,
                    isPlaying: coordinator.isPlaying,
                    upcomingTapes: appState.upcomingTapes,
                    audioLevels: coordinator.audioLevels
                )
            }
        }
        .onAppear {
            coordinator.onPlaybackEnded = {
                appState.nextTape()
            }
        }
        .onChange(of: appState.currentTape) { _, newTape in
            if let tape = newTape {
                coordinator.loadTape(tape)
            }
        }
        .onExitCommand {
            coordinator.togglePlayback()
        }
    }
}
