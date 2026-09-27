import Foundation

/// Terminal keeps scriptable windows after their last tab closes. Every
/// deferred write must revalidate its original target before touching it.
enum TerminalTileScript {
    struct Placement {
        let id: UInt32
        let bounds: (left: Int, top: Int, right: Int, bottom: Int)
    }

    static let liveWindowHandler = """
    on slabTileWindow(wid)
      if application "Terminal" is not running then error "Terminal stopped"
      tell application "Terminal"
        set w to first window whose id is wid
        if miniaturized of w then error "Window minimized"
        if (count tabs of w) is 0 then error "Window closed"
        set liveTab to selected tab of w
        return w
      end tell
    end slabTileWindow
    """

    static let liveWindowIDs = liveWindowHandler + "\n" + """
    set liveIds to {}
    if application "Terminal" is running then
      tell application "Terminal" to set candidates to id of every window
      repeat with wid in candidates
        try
          my slabTileWindow(contents of wid)
          set end of liveIds to contents of wid
        end try
      end repeat
    end if
    return liveIds
    """

    static func make(placements: [Placement], fontSize: Int, resetZoom: Bool) -> String {
        guard !placements.isEmpty else { return "" }
        var lines = [liveWindowHandler]
        for placement in placements {
            lines += [
                "try",
                "  set w to my slabTileWindow(\(placement.id))",
                "  tell application \"Terminal\" to set font size of current settings of w to \(fontSize)",
                "end try",
            ]
        }
        if resetZoom {
            // Never enumerate again or send `activate` (which can open a
            // default window). Focus only the still-live transaction targets.
            for placement in placements {
                lines += [
                    "try",
                    "  set w to my slabTileWindow(\(placement.id))",
                    "  tell application \"Terminal\" to set index of w to 1",
                    "  tell application \"System Events\" to tell process \"Terminal\" to set frontmost to true",
                    "  delay 0.04",
                    "  my slabTileWindow(\(placement.id))",
                    "  tell application \"Terminal\" to set frontID to id of front window",
                    "  if frontID is \(placement.id) then",
                    "    tell application \"System Events\" to tell process \"Terminal\" to click menu item \"Default Font Size\" of menu 1 of menu bar item \"View\" of menu bar 1",
                    "  end if",
                    "end try",
                ]
            }
        }
        // Revalidate after each reflow delay: a target can close while this
        // script is queued or while another window's font is being reset.
        for delay in [0.0, 0.06, 0.16] {
            if delay > 0 { lines.append("delay \(delay)") }
            for placement in placements {
                let b = placement.bounds
                lines += [
                    "try",
                    "  set w to my slabTileWindow(\(placement.id))",
                    "  tell application \"Terminal\" to set bounds of w to {\(b.left), \(b.top), \(b.right), \(b.bottom)}",
                    "end try",
                ]
            }
        }
        return lines.joined(separator: "\n")
    }
}
