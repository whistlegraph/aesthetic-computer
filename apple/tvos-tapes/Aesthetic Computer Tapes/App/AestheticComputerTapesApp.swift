// AestheticComputerTapesApp.swift
// Main entry point for Aesthetic Computer Tapes (Apple TV)
// 2026.01.01

import SwiftUI
import AVKit

@main
struct AestheticComputerTapesApp: App {
    @StateObject private var appState = AppState()
    
    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(appState)
        }
    }
}
