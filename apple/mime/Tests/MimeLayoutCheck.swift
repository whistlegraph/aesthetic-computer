import Foundation

@main struct MimeLayoutCheck {
    static func main() throws {
        let data = try Data(contentsOf: URL(fileURLWithPath: CommandLine.arguments[1]))
        guard let layout = MimeLayout.decode(data), layout == .standard else { fatalError("Shared layout must match the emergency fallback") }
        var json = try JSONSerialization.jsonObject(with: data) as! [String: Any]
        json["revision"] = "remote-update"
        json["identifierSize"] = 28
        json["showCaption"] = false
        let update = MimeLayout.decode(try JSONSerialization.data(withJSONObject: json))
        precondition(update?.identifierSize == 28 && update?.showCaption == false)
        json["schemaVersion"] = 2
        let unsupported = try JSONSerialization.data(withJSONObject: json)
        precondition(MimeLayout.decode(unsupported) == nil)
        json["schemaVersion"] = 1
        for key in ["identifierSize", "handleSize", "mimeTypeSize", "captionSize", "titleSize", "padding"] {
            var invalid = json
            invalid[key] = 1000
            let invalidData = try JSONSerialization.data(withJSONObject: invalid)
            precondition(MimeLayout.decode(invalidData) == nil)
        }
        precondition(MimeLayout.decode(Data("not JSON".utf8)) == nil)
        precondition(MimeLayout.decode(Data(repeating: 32, count: 4097)) == nil)
        var older = try JSONSerialization.jsonObject(with: data) as! [String: Any]
        older.removeValue(forKey: "loadingBreakupSeconds")
        let compatible = try JSONSerialization.data(withJSONObject: older)
        precondition(MimeLayout.decode(compatible) != nil, "Existing schema-1 layouts remain supported")
        older["loadingBreakupSeconds"] = 100
        let invalidDuration = try JSONSerialization.data(withJSONObject: older)
        precondition(MimeLayout.decode(invalidDuration) == nil)
        let wad = try JSONDecoder().decode(Post.self, from: Data("{\"code\":\"abc\",\"file\":{\"name\":\"arena.wad\",\"type\":\"application/x-doom-wad\",\"url\":\"/api/mime?file=abc\"}}".utf8))
        for mime in ["application/x-nes-rom", "application/x-nes", "application/x-nintendo-nes-rom", "application/octet-stream"] {
            let data = try JSONSerialization.data(withJSONObject: ["code":"nes", "file":["name":"game.NES", "type":mime, "url":"/api/mime?file=nes"]])
            let post = try JSONDecoder().decode(Post.self, from: data)
            precondition(post.playable)
            precondition(post.command == "nes~nes")
            guard case let .runtime(url, kind) = post.body else { fatalError("NES must render as a runtime") }
            precondition(kind == "nes" && url.path == "/nes-player")
            let query = URLComponents(url: url, resolvingAgainstBaseURL: false)!.queryItems!
            precondition(query.first(where: { $0.name == "file" })!.value == "https://mime.ac/api/mime?file=nes")
            precondition(query.first(where: { $0.name == "warm" })!.value == "1")
        }
        let colored = try JSONDecoder().decode(Post.self, from: Data("{\"code\":\"colors\",\"name\":\"@artist\",\"nameColors\":[{\"r\":24,\"g\":128,\"b\":240},null]}".utf8))
        precondition(colored.nameColors?.count == 2 && colored.nameColors?[0]?.g == 128 && colored.nameColors?[1] == nil)
        precondition(wad.command == "doom~abc", "WAD identifiers must resolve in the AC prompt namespace")
        if case let .runtime(url, kind) = wad.body {
            precondition(kind == "doom" && url.path == "/doom-player")
            let flags = URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems ?? []
            precondition(flags.contains { $0.name == "preview" && $0.value == "1" })
            precondition(flags.contains { $0.name == "mime" && $0.value == "1" })
            precondition(URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems?.first(where: { $0.name == "file" })?.value == "https://mime.ac/api/mime?file=abc")
        } else { fatalError("WADs must use the shared playable runtime") }
        for (kind, code, expected) in [("piece", "q7f", "q7f"), ("kidlisp", "abc", "$abc"), ("painting", "abc", "#abc"), ("tape", "abc", "!abc"), ("wgpiece", "wgDuck", "wgDuck")] {
            let postData = try JSONSerialization.data(withJSONObject: ["code": "thread_123", "name": "@artist", "media": ["kind": kind, "code": code, "title": "@artist/ducks"]])
            let post = try JSONDecoder().decode(Post.self, from: postData)
            precondition(post.command == expected, "A title or handle must never replace the global identifier")
        }
        print("MIME native layout: shared fallback, updates, bounds and unknown-schema rejection passed")
    }
}
