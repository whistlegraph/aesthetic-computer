// Tape.swift
// Tape data model for Apple TV
// 2026.01.01

import Foundation

struct Tape: Codable, Identifiable, Equatable {
    let id: String
    let title: String
    let mp4: String
    let thumbnail: String?
    let duration: Int?
    let resolution: String?
    let fps: Int?
    let seed: Int?
    let generator: String?
    let created_at: Date?
    let owner: String?
    let tags: [String]?
    
    var videoURL: URL? {
        URL(string: mp4)
    }
    
    var thumbnailURL: URL? {
        guard let thumbnail = thumbnail else { return nil }
        return URL(string: thumbnail)
    }
    
    /// URL to view this tape on Aesthetic Computer
    var webURL: URL? {
        URL(string: "https://prompt.ac/!\(id)")
    }
    
    var formattedDuration: String {
        guard let duration = duration else { return "--:--" }
        let minutes = duration / 60
        let seconds = duration % 60
        return String(format: "%d:%02d", minutes, seconds)
    }
    
    var resolutionDisplay: String {
        resolution ?? "1280x720"
    }
    
    var fpsDisplay: String {
        "\(fps ?? 30)fps"
    }
}

struct TapeFeedResponse: Codable {
    let tapes: [Tape]
    let total: Int
    let lastUpdated: String
}
