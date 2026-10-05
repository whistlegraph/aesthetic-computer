import Foundation

/// Chrome's AX resize clamps to its UI minimum; its AppleScript bounds setter
/// accepts smaller windows. Chrome's script IDs are NOT CGWindowIDs, so resolve
/// each target by its measured frame, refusing ambiguous or missing matches.
enum ChromeTileScript {
    struct Placement {
        let current: (left: Int, top: Int, right: Int, bottom: Int)
        let target: (left: Int, top: Int, right: Int, bottom: Int)
    }

    static func make(placements: [Placement]) -> String {
        guard !placements.isEmpty else { return "" }
        var lines = [
            "set applied to 0",
            "if application id \"com.google.Chrome\" is running then",
            "  tell application id \"com.google.Chrome\"",
        ]
        for p in placements {
            let c = p.current
            let t = p.target
            lines += [
                "    try",
                "      set matches to every window whose bounds is {\(c.left), \(c.top), \(c.right), \(c.bottom)} and minimized is false",
                "      if (count matches) is 1 then",
                "        set w to item 1 of matches",
                "        set bounds of w to {\(t.left), \(t.top), \(t.right), \(t.bottom)}",
                "        set applied to applied + 1",
                "      end if",
                "    end try",
            ]
        }
        lines += ["  end tell", "end if", "return applied"]
        return lines.joined(separator: "\n")
    }
}
