import Foundation
import CryptoKit

extension Post {
    var remixWare: Ware? {
        guard file != nil else { return nil }
        switch mimeType {
        case "image/png": return .painting
        case "application/x-doom-wad", "application/x-doom": return .doom
        case "text/javascript", "application/javascript", "application/x-aesthetic-computer-piece": return .piece
        default: return nil
        }
    }
    var edition: Int? {
        if let value = media?.version ?? version, value >= 0 { return value }
        // Standalone uploads have one starting revision. Live WG threads
        // must supply their actual version; an old response cannot guess it.
        return media?.kind == "wgpiece" ? nil : remixWare == nil ? nil : 0
    }
    var permalink: URL {
        URL(string: "https://mime.ac/p/" + code.addingPercentEncoding(withAllowedCharacters: .urlPathAllowed)!)!
    }
    func remixPayload() async throws -> [String: Any] {
        guard let ware = remixWare, let edition else { throw MimeRemix.failure("This work has no editable version available.") }
        var source: String?, bytes: Data, author = name ?? ""
        #if DEBUG
        if ["remix", "progress"].contains(ProcessInfo.processInfo.environment["MIME_CREATION_FIXTURE"] ?? ""), code.hasPrefix("fixture-remix-") {
            if ware == .piece {
                source = "export function paint({wipe,ink,screen}){wipe('#76d17b');ink('purple').circle(screen.width/2,screen.height/2,30);}"
                bytes = Data(source!.utf8)
            } else if ware == .painting {
                bytes = try Data(contentsOf: Bundle.main.url(forResource: "remix", withExtension: "png", subdirectory: "Fixtures")!)
            } else { bytes = try await MimeRemix.fetch(URL(string: file!.url)!, limit: 8 * 1024 * 1024) }
            var fixture: [String: Any] = ["ware": ware.rawValue, "title": "Remix fixture", "reference": ["code": command, "handle": author, "version": edition, "url": permalink.absoluteString, "hash": MimeRemix.hash(bytes)]]
            if let source { fixture["source"] = source } else { fixture["data"] = bytes.base64EncodedString() }
            return fixture
        }
        #endif
        if media?.kind == "wgpiece", let code = media?.code {
            var endpoint = URLComponents(string: "https://aesthetic.computer/api/mime-feed")!
            endpoint.queryItems = [.init(name: "code", value: code), .init(name: "version", value: String(edition))]
            let data = try await MimeRemix.fetch(endpoint.url!, limit: 700_000)
            struct Piece: Decodable { let code: String; let source: String; let version: Int; let handle: String; let sourceHash: String }
            let piece = try JSONDecoder().decode(Piece.self, from: data)
            guard piece.code == code, piece.version == edition, !piece.source.isEmpty else { throw MimeRemix.failure("Refresh the feed to load this work’s current version.") }
            bytes = Data(piece.source.utf8); source = piece.source; author = piece.handle
            guard MimeRemix.hash(bytes) == piece.sourceHash else { throw MimeRemix.failure("The original source could not be verified.") }
        } else {
            guard let file, let url = URL(string: file.url, relativeTo: URL(string: "https://mime.ac"))?.absoluteURL, url.scheme == "https" else { throw MimeRemix.failure("The original file is unavailable.") }
            bytes = try await MimeRemix.fetch(url, limit: ware == .piece ? 500_000 : 8 * 1024 * 1024)
            if ware == .piece { source = String(data: bytes, encoding: .utf8); if source?.isEmpty != false { throw MimeRemix.failure("This file is not an AC source piece.") } }
        }
        try Task.checkCancellation()
        let reference: [String: Any] = ["code": command, "handle": author, "version": edition, "url": permalink.absoluteString, "hash": MimeRemix.hash(bytes)]
        var payload: [String: Any] = ["ware": ware.rawValue, "reference": reference, "title": media?.title ?? file?.name?.replacingOccurrences(of: ".png", with: "").replacingOccurrences(of: ".wad", with: "") ?? command]
        if let source { payload["source"] = source } else { payload["data"] = bytes.base64EncodedString() }
        return payload
    }
}

enum MimeRemix {
    static func failure(_ text: String) -> NSError { NSError(domain: "MimeRemix", code: 1, userInfo: [NSLocalizedDescriptionKey: text]) }
    static func hash(_ data: Data) -> String { SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined() }
    static func fetch(_ url: URL, limit: Int) async throws -> Data {
        let (data, response) = try await URLSession.shared.data(for: URLRequest(url: url, cachePolicy: .reloadIgnoringLocalCacheData, timeoutInterval: 20))
        guard let response = response as? HTTPURLResponse, response.statusCode == 200 else {
            let message = (try? JSONSerialization.jsonObject(with: data) as? [String: Any])?["error"] as? String
            throw failure(message ?? "The original work could not be loaded.")
        }
        guard !data.isEmpty, data.count <= limit else { throw failure("This work is too large to remix on this phone.") }
        return data
    }
}
