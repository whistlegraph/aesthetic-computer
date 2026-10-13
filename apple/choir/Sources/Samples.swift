import Foundation

/// Faux cards shown only while the room is empty, so the interface has
/// something to hold. Never sent to the server; the handles say what they are.
enum Samples {
    static func posts() -> [Post] {
        let now = Date()
        func at(_ minutes: Double) -> String {
            let f = ISO8601DateFormatter(); f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
            return f.string(from: now.addingTimeInterval(-minutes * 60))
        }
        func card(_ i: Int, _ handle: String, _ text: String, _ tags: [String], _ links: [String] = [], replies: Int = 0, ago: Double) -> Post {
            Post(id: String(format: "sample%018d", i), handle: handle, text: text, tags: tags, links: links, replyTo: nil,
                 replies: replies, lastReplyAt: nil, when: at(ago), deleted: false)
        }
        return [
            card(1, "@lorem", "Lorem ipsum dolor sit amet, the group show opens Friday at seven. Consectetur adipiscing, bring a friend.", ["news", "los-angeles"], replies: 3, ago: 12),
            card(2, "@ipsum", "Sed do eiusmod tempor: anyone selling a flat file? Incididunt ut labore, will pick up.", ["free-and-for-sale", "new-york"], replies: 1, ago: 48),
            card(3, "@dolor", "Ut enim ad minim veniam. Is the residency deadline the 1st or the 15th?", ["discussion", "news"], ago: 95),
            card(4, "@amet", "Quis nostrud exercitation ullamco laboris. Great review this week.", ["news", "discussion"], ["https://www.artforum.com"], replies: 5, ago: 230),
            card(5, "@consectetur", "Duis aute irure dolor in reprehenderit. Free pedestals on the curb, first come.", ["free-and-for-sale", "los-angeles"], ago: 410),
        ]
    }
    static func isSample(_ p: Post) -> Bool { p.id.hasPrefix("sample") }
}
