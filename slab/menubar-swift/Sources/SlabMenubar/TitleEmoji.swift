import Foundation

/// Sticky per-session title emoji — the visual anchor that lets the eye
/// re-find a window after the tiler shuffles the grid. Inferred once from
/// the session's first real prompt (topic keyword → meaningful emoji),
/// falling back to a hash-picked entry from a visually-distinct palette,
/// and deduped across the live wall so no two sessions wear the same mark.
/// Once assigned it never changes for the session's lifetime: stability is
/// the whole point.
///
/// The cache is only ever touched from `refresh()`'s gather pass, which is
/// serialized by AppDelegate's `gathering` guard — no locking needed.
enum TitleEmoji {
    /// sessionId → assigned emoji. Reaped as sessions disappear.
    private static var cache: [String: String] = [:]

    /// Topic rules, first match wins. Substring match on the lowercased
    /// subject (the first prompt's opening 140 chars). Every emoji here has
    /// default emoji presentation (no U+FE0F variation selector) so it
    /// renders as a color glyph in window title bars and the menubar alike.
    private static let rules: [(keys: [String], emoji: String)] = [
        (["slab", "menubar", "tile", "iterm", "terminal"], "🧱"),
        (["mail", "email", "inbox", "draft"], "📨"),
        (["paper", "latex", "arxiv", "dossier"], "📚"),
        (["song", "music", "track", "melody", "vocal", "synth", "mix", "audio", "mp3"], "🎶"),
        (["video", "mp4", "ffmpeg", "motion", "film", "seedance"], "🎬"),
        (["oven", "ota", "deploy", "release", "ship", "publish"], "🔥"),
        (["bug", "fix", "debug", "crash", "broken", "error"], "🐛"),
        (["test", "spec", "verify"], "🧪"),
        (["kidlisp", "lisp"], "🐢"),
        (["piece", "disk", "prompt"], "🧩"),
        (["shop", "stripe", "invoice", "grant", "tax", "money"], "💸"),
        (["server", "lith", "droplet", "ssh", "dns", "cdn", "domain"], "🌐"),
        (["wallpaper", "image", "photo", "cover", "illy"], "📷"),
        (["fedac", "native", "kernel", "boot"], "💾"),
        (["git", "commit", "push", "branch", "merge", "rebase"], "🌿"),
        (["readme", "notes", "memo", "write", "doc"], "📝"),
        (["chat", "imsg", "signal", "message"], "💬"),
        (["notepat", "menuband", "piano", "instrument"], "🎹"),
    ]

    /// Fallback marks: fruits, creatures, and objects chosen to stay
    /// distinguishable at title-bar size and from across the room.
    private static let palette = [
        "🍉", "🫐", "🍋", "🍇", "🥝", "🍑", "🍒", "🌵", "🍄", "🌊",
        "🌙", "🪐", "🔮", "🧊", "🐠", "🦜", "🐸", "🦊", "🐙", "🐝",
        "🌻", "🍩", "🎈", "🚀", "🛸", "🪩", "🎲", "🌈",
    ]

    /// Stamp every live session with its sticky emoji. Blank sessions stay
    /// unmarked (their first real prompt does the inferring — assigning off
    /// an empty subject would lock in a meaningless fallback). Assignment
    /// iterates in sessionId order so a menubar restart converges on the
    /// same wall.
    static func assign(_ sessions: [ClaudeSession]) -> [ClaudeSession] {
        let liveIds = Set(sessions.map { $0.sessionId })
        cache = cache.filter { liveIds.contains($0.key) }
        for s in sessions.sorted(by: { $0.sessionId < $1.sessionId }) {
            guard s.state != .blank, cache[s.sessionId] == nil else { continue }
            cache[s.sessionId] = infer(
                subject: s.subject,
                sessionId: s.sessionId,
                taken: Set(cache.values)
            )
        }
        return sessions.map { s in
            var out = s
            out.emoji = cache[s.sessionId] ?? ""
            return out
        }
    }

    private static func infer(subject: String, sessionId: String, taken: Set<String>) -> String {
        let lower = subject.lowercased()
        for rule in rules where rule.keys.contains(where: { lower.contains($0) }) {
            if !taken.contains(rule.emoji) { return rule.emoji }
            break  // topic mark already on the wall — distinctness beats meaning
        }
        // FNV-1a over the sessionId: Swift's hashValue is seed-randomized
        // per launch, and the pick must survive a menubar restart.
        var h: UInt64 = 0xcbf2_9ce4_8422_2325
        for b in sessionId.utf8 {
            h ^= UInt64(b)
            h = h &* 0x1_0000_0001_b3
        }
        var i = Int(h % UInt64(palette.count))
        for _ in 0..<palette.count {
            if !taken.contains(palette[i]) { return palette[i] }
            i = (i + 1) % palette.count
        }
        return palette[i]  // >28 live sessions: collisions are forgivable
    }
}
