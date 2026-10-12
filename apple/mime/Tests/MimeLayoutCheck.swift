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
        for (kind, code, expected) in [("piece", "q7f", "q7f"), ("kidlisp", "abc", "$abc"), ("painting", "abc", "#abc"), ("tape", "abc", "!abc"), ("wgpiece", "wgDuck", "wgDuck")] {
            let postData = try JSONSerialization.data(withJSONObject: ["code": "thread_123", "name": "@artist", "media": ["kind": kind, "code": code, "title": "@artist/ducks"]])
            let post = try JSONDecoder().decode(Post.self, from: postData)
            precondition(post.command == expected, "A title or handle must never replace the global identifier")
        }
        print("MIME native layout: shared fallback, updates, bounds and unknown-schema rejection passed")
    }
}
