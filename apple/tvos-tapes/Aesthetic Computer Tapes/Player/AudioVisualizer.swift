// AudioVisualizer.swift
// Realtime audio waveform visualization using MTAudioProcessingTap
// 2026.01.01

import SwiftUI
import AVFoundation
import Accelerate
import MediaToolbox

// MARK: - Observable audio levels storage
@MainActor
class AudioLevels: ObservableObject {
    @Published var samples: [Float] = Array(repeating: 0, count: 64)
    
    nonisolated func update(_ newSamples: [Float]) {
        Task { @MainActor in
            withAnimation(.linear(duration: 0.05)) {
                self.samples = newSamples
            }
        }
    }
}

// MARK: - Waveform view
struct WaveformView: View {
    @ObservedObject var levels: AudioLevels
    let barCount: Int = 32
    
    var body: some View {
        GeometryReader { geo in
            HStack(spacing: 4) {
                ForEach(0..<barCount, id: \.self) { index in
                    let sampleIndex = index * levels.samples.count / barCount
                    let level = CGFloat(levels.samples[sampleIndex])
                    
                    RoundedRectangle(cornerRadius: 2)
                        .fill(Color.white.opacity(0.8))
                        .frame(width: (geo.size.width - CGFloat(barCount - 1) * 4) / CGFloat(barCount))
                        .frame(height: max(4, geo.size.height * level))
                        .frame(maxHeight: .infinity, alignment: .center)
                }
            }
        }
    }
}

// MARK: - Audio tap processor
class AudioTapProcessor {
    private let audioLevels: AudioLevels
    private var tap: MTAudioProcessingTap?
    
    init(levels: AudioLevels) {
        self.audioLevels = levels
    }
    
    func installTap(on item: AVPlayerItem) {
        Task {
            guard let track = try? await item.asset.loadTracks(withMediaType: .audio).first else {
                print("⚠️ No audio track found")
                return
            }
            
            await MainActor.run {
                setupTap(with: track, on: item)
            }
        }
    }
    
    private func setupTap(with track: AVAssetTrack, on item: AVPlayerItem) {
        // Create audio mix with our tap
        let inputParams = AVMutableAudioMixInputParameters(track: track)
        
        var callbacks = MTAudioProcessingTapCallbacks(
            version: kMTAudioProcessingTapCallbacksVersion_0,
            clientInfo: UnsafeMutableRawPointer(Unmanaged.passUnretained(self).toOpaque()),
            init: tapInit,
            finalize: tapFinalize,
            prepare: tapPrepare,
            unprepare: tapUnprepare,
            process: tapProcess
        )
        
        var newTap: MTAudioProcessingTap?
        let status = MTAudioProcessingTapCreate(
            kCFAllocatorDefault,
            &callbacks,
            kMTAudioProcessingTapCreationFlag_PostEffects,
            &newTap
        )
        
        guard status == noErr, let tapInstance = newTap else {
            print("❌ Failed to create audio tap: \(status)")
            return
        }
        
        self.tap = tapInstance
        inputParams.audioTapProcessor = tapInstance
        
        let audioMix = AVMutableAudioMix()
        audioMix.inputParameters = [inputParams]
        item.audioMix = audioMix
        
        print("✅ Audio tap installed")
    }
    
    func removeTap() {
        tap = nil
    }
    
    func updateLevels(_ newLevels: [Float]) {
        audioLevels.update(newLevels)
    }
}

// MARK: - MTAudioProcessingTap C callbacks
private func tapInit(
    tap: MTAudioProcessingTap,
    clientInfo: UnsafeMutableRawPointer?,
    tapStorageOut: UnsafeMutablePointer<UnsafeMutableRawPointer?>
) {
    tapStorageOut.pointee = clientInfo
}

private func tapFinalize(tap: MTAudioProcessingTap) {
    // Cleanup if needed
}

private func tapPrepare(
    tap: MTAudioProcessingTap,
    maxFrames: CMItemCount,
    processingFormat: UnsafePointer<AudioStreamBasicDescription>
) {
    // Prepare for processing
}

private func tapUnprepare(tap: MTAudioProcessingTap) {
    // Unprepare
}

private func tapProcess(
    tap: MTAudioProcessingTap,
    numberFrames: CMItemCount,
    flags: MTAudioProcessingTapFlags,
    bufferListInOut: UnsafeMutablePointer<AudioBufferList>,
    numberFramesOut: UnsafeMutablePointer<CMItemCount>,
    flagsOut: UnsafeMutablePointer<MTAudioProcessingTapFlags>
) {
    // Get audio data
    let status = MTAudioProcessingTapGetSourceAudio(
        tap,
        numberFrames,
        bufferListInOut,
        flagsOut,
        nil,
        numberFramesOut
    )
    
    guard status == noErr else { return }
    
    // Get processor from storage
    let storage = MTAudioProcessingTapGetStorage(tap)
    let processor = Unmanaged<AudioTapProcessor>.fromOpaque(storage).takeUnretainedValue()
    
    // Extract audio levels
    let bufferList = UnsafeMutableAudioBufferListPointer(bufferListInOut)
    guard let buffer = bufferList.first, let data = buffer.mData else { return }
    
    let frameCount = Int(numberFramesOut.pointee)
    let samples = data.assumingMemoryBound(to: Float.self)
    
    // Calculate RMS for 64 segments
    let segmentSize = max(1, frameCount / 64)
    var levels = [Float](repeating: 0, count: 64)
    
    for i in 0..<64 {
        let start = i * segmentSize
        let end = min(start + segmentSize, frameCount)
        
        if start < frameCount {
            var sum: Float = 0
            for j in start..<end {
                let sample = samples[j]
                sum += sample * sample
            }
            let rms = sqrt(sum / Float(end - start))
            levels[i] = min(1.0, rms * 3.0) // Scale up for visibility
        }
    }
    
    processor.updateLevels(levels)
}
