// OverlayView.swift
// Tape info overlay with QR code, progress bar, and waveform
// 2026.01.01

import SwiftUI
import CoreImage.CIFilterBuiltins

struct OverlayView: View {
    let tape: Tape
    let progress: Double
    let duration: Double
    let isPlaying: Bool
    let upcomingTapes: [Tape]
    @ObservedObject var audioLevels: AudioLevels
    
    var body: some View {
        ZStack {
            topLeftSection
            topRightSection
            bottomSection
        }
    }
    
    // MARK: - Top Left: UP NEXT list
    private var topLeftSection: some View {
        VStack(alignment: .leading, spacing: 12) {
            if !upcomingTapes.isEmpty {
                Text("UP NEXT")
                    .font(.system(size: 18, weight: .bold))
                    .foregroundColor(.white.opacity(0.6))
                
                ForEach(Array(upcomingTapes.prefix(8).enumerated()), id: \.element.id) { index, nextTape in
                    upNextRow(index: index, tapeId: nextTape.id)
                }
            }
            Spacer()
        }
        .padding(.top, 60)
        .padding(.leading, 60)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }
    
    private func upNextRow(index: Int, tapeId: String) -> some View {
        HStack(spacing: 8) {
            Text("\(index + 1).")
                .font(.system(size: 20, weight: .medium, design: .monospaced))
                .foregroundColor(.white.opacity(0.4))
            Text("!\(tapeId)")
                .font(.system(size: 20, weight: .medium, design: .monospaced))
                .foregroundColor(.white.opacity(0.7))
        }
    }
    
    // MARK: - Top Right: QR Code
    private var topRightSection: some View {
        VStack(alignment: .trailing, spacing: 8) {
            QRCodeView(url: "https://prompt.ac/!\(tape.id)")
                .frame(width: 140, height: 140)
            
            Text("prompt.ac/!\(tape.id)")
                .font(.system(size: 16, weight: .medium, design: .monospaced))
                .foregroundColor(.white.opacity(0.7))
            
            Spacer()
        }
        .padding(.top, 60)
        .padding(.trailing, 60)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topTrailing)
    }
    
    // MARK: - Bottom: Tape info, waveform, and progress bar
    private var bottomSection: some View {
        VStack(spacing: 20) {
            Spacer()
            
            // Waveform visualizer
            WaveformView(levels: audioLevels)
                .frame(height: 60)
                .padding(.horizontal, 60)
            
            // Tape code
            tapeCodeRow
            
            // Progress bar
            progressBar
            
            // Time
            timeRow
        }
        .padding(.bottom, 60)
    }
    
    private var tapeCodeRow: some View {
        HStack {
            Text("!\(tape.id)")
                .font(.system(size: 32, weight: .bold, design: .monospaced))
                .foregroundColor(.white)
            
            if !isPlaying {
                Image(systemName: "pause.fill")
                    .font(.system(size: 24))
                    .foregroundColor(.white.opacity(0.8))
            }
        }
    }
    
    private var progressBar: some View {
        GeometryReader { geo in
            ZStack(alignment: .leading) {
                // Background
                RoundedRectangle(cornerRadius: 4)
                    .fill(Color.white.opacity(0.3))
                    .frame(height: 8)
                
                // Progress
                RoundedRectangle(cornerRadius: 4)
                    .fill(Color.white)
                    .frame(width: progressWidth(geo.size.width), height: 8)
            }
        }
        .frame(height: 8)
        .padding(.horizontal, 60)
    }
    
    private func progressWidth(_ totalWidth: CGFloat) -> CGFloat {
        guard duration > 0 else { return 0 }
        return totalWidth * CGFloat(progress / duration)
    }
    
    private var timeRow: some View {
        HStack {
            Text(formatTime(progress))
            Spacer()
            Text(formatTime(duration))
        }
        .font(.system(size: 18, weight: .medium, design: .monospaced))
        .foregroundColor(.white.opacity(0.7))
        .padding(.horizontal, 60)
    }
    
    private func formatTime(_ seconds: Double) -> String {
        guard seconds.isFinite && seconds >= 0 else { return "0:00" }
        let mins = Int(seconds) / 60
        let secs = Int(seconds) % 60
        return String(format: "%d:%02d", mins, secs)
    }
}

struct QRCodeView: View {
    let url: String
    
    var body: some View {
        if let image = generateQRCode(from: url) {
            Image(uiImage: image)
                .interpolation(.none)
                .resizable()
                .scaledToFit()
                .background(Color.white)
                .cornerRadius(8)
        }
    }
    
    func generateQRCode(from string: String) -> UIImage? {
        let context = CIContext()
        let filter = CIFilter.qrCodeGenerator()
        
        filter.message = Data(string.utf8)
        filter.correctionLevel = "M"
        
        guard let outputImage = filter.outputImage else { return nil }
        
        // Scale up the QR code
        let scale = CGAffineTransform(scaleX: 10, y: 10)
        let scaledImage = outputImage.transformed(by: scale)
        
        guard let cgImage = context.createCGImage(scaledImage, from: scaledImage.extent) else { return nil }
        
        return UIImage(cgImage: cgImage)
    }
}
