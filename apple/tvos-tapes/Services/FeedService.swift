// FeedService.swift
// Fetches tape feed from Aesthetic Computer API
// 2026.01.01

import Foundation

actor FeedService {
    private let feedURL = URL(string: "https://aesthetic.computer/api/tv-tapes")!
    private let cacheKey = "cachedTapes"
    private var cachedFeed: TapeFeedResponse?
    
    func fetchTapes(limit: Int = 50) async throws -> [Tape] {
        var components = URLComponents(url: feedURL, resolvingAgainstBaseURL: false)!
        components.queryItems = [URLQueryItem(name: "limit", value: String(limit))]
        
        guard let url = components.url else {
            throw FeedError.invalidURL
        }
        
        let (data, response) = try await URLSession.shared.data(from: url)
        
        guard let httpResponse = response as? HTTPURLResponse else {
            throw FeedError.invalidResponse
        }
        
        guard httpResponse.statusCode == 200 else {
            throw FeedError.httpError(statusCode: httpResponse.statusCode)
        }
        
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        
        let feed = try decoder.decode(TapeFeedResponse.self, from: data)
        cachedFeed = feed
        
        // Cache to disk for offline playback
        try? saveToDisk(feed)
        
        return feed.tapes
    }
    
    func getCachedTapes() -> [Tape] {
        if let cached = cachedFeed {
            return cached.tapes
        }
        
        // Try loading from disk
        if let diskCached = loadFromDisk() {
            cachedFeed = diskCached
            return diskCached.tapes
        }
        
        return []
    }
    
    private func saveToDisk(_ feed: TapeFeedResponse) throws {
        let encoder = JSONEncoder()
        let data = try encoder.encode(feed)
        
        let cacheURL = FileManager.default
            .urls(for: .cachesDirectory, in: .userDomainMask)
            .first!
            .appendingPathComponent("tapeFeed.json")
        
        try data.write(to: cacheURL)
    }
    
    private func loadFromDisk() -> TapeFeedResponse? {
        let cacheURL = FileManager.default
            .urls(for: .cachesDirectory, in: .userDomainMask)
            .first!
            .appendingPathComponent("tapeFeed.json")
        
        guard let data = try? Data(contentsOf: cacheURL) else {
            return nil
        }
        
        let decoder = JSONDecoder()
        decoder.dateDecodingStrategy = .iso8601
        
        return try? decoder.decode(TapeFeedResponse.self, from: data)
    }
}

enum FeedError: LocalizedError {
    case invalidURL
    case invalidResponse
    case httpError(statusCode: Int)
    
    var errorDescription: String? {
        switch self {
        case .invalidURL:
            return "Invalid feed URL"
        case .invalidResponse:
            return "Invalid server response"
        case .httpError(let code):
            return "Server error: \(code)"
        }
    }
}
