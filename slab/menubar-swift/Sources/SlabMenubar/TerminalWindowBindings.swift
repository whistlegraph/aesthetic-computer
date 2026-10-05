import Foundation

/// Bind visible terminal tabs to their native window IDs. Geometry cannot
/// identify a window: minimized windows retain the same tile as a live one.
enum TerminalWindowBindings {
    static func reconcile(previous: [String: Int], probeOutput: String?,
                          liveKeys: Set<String>, visibleWindowIDs: Set<Int>) -> [String: Int] {
        let retained = previous.filter {
            liveKeys.contains($0.key) && visibleWindowIDs.contains($0.value)
        }
        // A failed Apple Events round trip keeps only still-visible bindings.
        guard let output = probeOutput else { return retained }

        // A successful probe is authoritative, including an empty response.
        // Desktop Aesel bindings are refreshed separately by snapshotWindows.
        var bindings = retained.filter { $0.key.hasPrefix("easel-") }
        for line in output.split(separator: "\n") {
            let fields = line.split(separator: "|")
            guard fields.count == 2,
                  let windowID = Int(fields[1].trimmingCharacters(in: .whitespaces)),
                  visibleWindowIDs.contains(windowID) else { continue }
            let tty = (fields[0].trimmingCharacters(in: .whitespaces) as NSString).lastPathComponent
            guard liveKeys.contains(tty) else { continue }
            bindings[tty] = windowID
        }
        return bindings
    }

    /// Both apps expose their native window ID. Only selected tabs belong on
    /// the visible window; background tabs and minimized windows have no rock.
    static func script(terminal: Bool, iterm: Bool) -> String {
        var script = "set out to \"\"\n"
        if terminal {
            script += """
            tell application "Terminal"
                repeat with w in windows
                    try
                        if not miniaturized of w then
                            set t to selected tab of w
                            set out to out & (tty of t) & "|" & (id of w as text) & linefeed
                        end if
                    end try
                end repeat
            end tell

            """
        }
        if iterm {
            script += """
            tell application "iTerm2"
                repeat with w in windows
                    try
                        if not miniaturized of w then
                            repeat with ss in sessions of current tab of w
                                set out to out & (tty of ss) & "|" & (id of w as text) & linefeed
                            end repeat
                        end if
                    end try
                end repeat
            end tell

            """
        }
        return script + "return out"
    }
}
