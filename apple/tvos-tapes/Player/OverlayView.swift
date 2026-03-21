// OverlayView.swift
// Minimal metadata overlay for tape playback
// 2026.01.01

import SwiftUI

struct OverlayView: View {
    let tape: Tape
    let progress: Double
    let duration: Double
    let isVisible: Bool
    
    var body: some View {
        VStack {
            Spacer()
            
            if isVisible {
                overlayContent
                    .transition(.opacity.combined(with: .move(edge: .bottom)))
            }
        }
        .animation(.easeInOut(duration: 0.3), value: isVisible)
    }
    
    private var overlayContent: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack {
                Text("Aesthetic Computer")
                    .font(.headline)
                    .foregroundColor(.white)
                
                Spacer()
                
                if let owner = tape.owner {
                    Text(owner)
                        .font(.subheadline)
                        .foregroundColor(.white.opacity(0.7))
                }
            }
            
            HStack(spacing: 16) {
                Text(tape.title)
                    .font(.title3)
                    .fontWeight(.medium)
                    .foregroundColor(.white)
                
                Text("·")
                    .foregroundColor(.white.opacity(0.5))
                
                Text(formatTime(progress))
                    .font(.subheadline)
                    .foregroundColor(.white.opacity(0.7))
                    .monospacedDigit()
                
                if duration > 0 {
                    Text("/ \(formatTime(duration))")
                        .font(.subheadline)
                        .foregroundColor(.white.opacity(0.5))
                        .monospacedDigit()
                }
                
                Spacer()
                
                Text(tape.resolutionDisplay)
                    .font(.caption)
                    .foregroundColor(.white.opacity(0.5))
                
                Text("@")
                    .foregroundColor(.white.opacity(0.3))
                
                Text(tape.fpsDisplay)
                    .font(.caption)
                    .foregroundColor(.white.opacity(0.5))
            }
            
            // Progress bar
            GeometryReader { geometry in
                ZStack(alignment: .leading) {
                    Rectangle()
                        .fill(Color.white.opacity(0.2))
                        .frame(height: 4)
                    
                    Rectangle()
                        .fill(Color.white)
                        .frame(width: progressWidth(in: geometry.size.width), height: 4)
                }
                .cornerRadius(2)
            }
            .frame(height: 4)
        }
        .padding(24)
        .background(
            LinearGradient(
                colors: [.clear, .black.opacity(0.8)],
                startPoint: .top,
                endPoint: .bottom
            )
        )
    }
    
    private func formatTime(_ seconds: Double) -> String {
        guard seconds.isFinite && seconds >= 0 else { return "0:00" }
        let mins = Int(seconds) / 60
        let secs = Int(seconds) % 60
        return String(format: "%d:%02d", mins, secs)
    }
    
    private func progressWidth(in totalWidth: CGFloat) -> CGFloat {
        guard duration > 0 else { return 0 }
        return CGFloat(progress / duration) * totalWidth
    }
}

#Preview {
    ZStack {
        Color.black
        OverlayView(
            tape: Tape(
                id: "abc",
                title: "Tape ABC",
                mp4: "https://example.com/tape.mp4",
                duration: 120,
                resolution: "1280x720",
                fps: 30,
                seed: 12345,
                generator: "kidlisp-oven",
                created_at: Date(),
                owner: "@jeffrey",
                tags: ["tape"]
            ),
            progress: 45,
            duration: 120,
            isVisible: true
        )
    }
}
