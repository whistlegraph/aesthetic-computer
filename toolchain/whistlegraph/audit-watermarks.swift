#!/usr/bin/env swift
// Sample video frames with AVFoundation and use macOS Vision OCR to detect
// cross-platform watermarks before a Whistlegraph clip enters a Reels queue.

import AVFoundation
import Foundation
import Vision

struct Audit: Codable {
    let file: String
    let duration: Double?
    let sampledSeconds: [Double]
    let recognizedText: [String]
    let matchedTerms: [String]
    let status: String
    let errors: [String]
}

let suspiciousTerms = ["tiktok", "@whistlegraph", "whistlegraph"]
let fractions = [0.04, 0.18, 0.35, 0.52, 0.69, 0.86, 0.96]
let encoder = JSONEncoder()
encoder.outputFormatting = [.sortedKeys]

func recognize(_ image: CGImage) throws -> [String] {
    let request = VNRecognizeTextRequest()
    request.recognitionLevel = .accurate
    request.usesLanguageCorrection = false
    request.minimumTextHeight = 0.008
    let handler = VNImageRequestHandler(cgImage: image, options: [:])
    try handler.perform([request])
    return (request.results ?? []).compactMap { $0.topCandidates(1).first?.string }
}

for argument in CommandLine.arguments.dropFirst() {
    let url = URL(fileURLWithPath: argument)
    let asset = AVURLAsset(url: url)
    var errors: [String] = []
    let seconds: Double?
    do {
        let duration = try await asset.load(.duration)
        let value = CMTimeGetSeconds(duration)
        seconds = value.isFinite && value > 0 ? value : nil
    } catch {
        seconds = nil
        errors.append("duration: \(error.localizedDescription)")
    }

    let generator = AVAssetImageGenerator(asset: asset)
    generator.appliesPreferredTrackTransform = true
    generator.requestedTimeToleranceBefore = CMTime(seconds: 0.08, preferredTimescale: 600)
    generator.requestedTimeToleranceAfter = CMTime(seconds: 0.08, preferredTimescale: 600)

    var sampled: [Double] = []
    var text: [String] = []
    if let duration = seconds {
        for fraction in fractions {
            let second = max(0, min(duration - 0.02, duration * fraction))
            do {
                let image = try await generator.image(
                    at: CMTime(seconds: second, preferredTimescale: 600)
                ).image
                sampled.append(second)
                text.append(contentsOf: try recognize(image))
            } catch {
                errors.append(String(format: "frame %.2fs: %@", second, error.localizedDescription))
            }
        }
    }

    let normalized = text.joined(separator: " ").lowercased()
    let matches = suspiciousTerms.filter { normalized.contains($0) }
    let status: String
    if !matches.isEmpty {
        status = "blocked-watermark-text"
    } else if sampled.count < fractions.count {
        status = "review-audit-incomplete"
    } else {
        status = "ocr-clear"
    }

    let audit = Audit(
        file: url.path,
        duration: seconds,
        sampledSeconds: sampled,
        recognizedText: Array(Set(text)).sorted(),
        matchedTerms: matches,
        status: status,
        errors: errors
    )
    if let data = try? encoder.encode(audit), let line = String(data: data, encoding: .utf8) {
        print(line)
    }
}
