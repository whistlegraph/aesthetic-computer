// swiftc slab/menubar-swift/Sources/SlabMenubar/TerminalWindowBindings.swift \
//   slab/test/TerminalWindowBindingsTests.swift -o /tmp/terminal-window-bindings-test
import Foundation

@main struct TerminalWindowBindingsTests {
    static func check(_ actual: [String: Int], _ expected: [String: Int], _ message: String) {
        precondition(actual == expected, "\(message): \(actual)")
    }

    static func main() {
        // Print the production probe for a read-only live AppleScript check.
        if CommandLine.arguments.contains("--terminal-script") {
            print(TerminalWindowBindings.script(terminal: true, iterm: false))
            return
        }

        let keys: Set<String> = ["ttys003", "ttys004", "easel-desktop"]
        let visible: Set<Int> = [2734, 9000]
        let wrong = ["ttys003": 2734, "ttys004": 2734, "easel-desktop": 9000]
        let correct = ["ttys004": 2734, "easel-desktop": 9000]

        check(TerminalWindowBindings.reconcile(previous: wrong,
            probeOutput: "/dev/ttys004|2734\n", liveKeys: keys, visibleWindowIDs: visible),
            correct, "minimized lopap must lose its stale binding to tiba's visible window")

        check(TerminalWindowBindings.reconcile(previous: wrong,
            probeOutput: "/dev/ttys003|2727\n/dev/ttys004|2734\n",
            liveKeys: keys, visibleWindowIDs: visible), correct,
            "a window minimized during the probe must not bind to another window")

        check(TerminalWindowBindings.reconcile(previous: correct,
            probeOutput: "/dev/ttys003|2727\n/dev/ttys004|2734\n",
            liveKeys: keys, visibleWindowIDs: visible.union([2727])),
            ["ttys003": 2727, "ttys004": 2734, "easel-desktop": 9000],
            "restored windows must regain their own IDs even with identical rectangles")

        check(TerminalWindowBindings.reconcile(previous: correct,
            probeOutput: "/dev/ttys003|2734\n", liveKeys: keys, visibleWindowIDs: visible),
            ["ttys003": 2734, "easel-desktop": 9000],
            "switching selected tabs must remove the previous tab's rock")

        check(TerminalWindowBindings.reconcile(previous: correct,
            probeOutput: "", liveKeys: keys, visibleWindowIDs: visible),
            ["easel-desktop": 9000], "successful empty probes must clear terminal bindings")

        check(TerminalWindowBindings.reconcile(previous: correct,
            probeOutput: nil, liveKeys: keys, visibleWindowIDs: visible), correct,
            "failed probes must preserve still-live bindings")

        check(TerminalWindowBindings.reconcile(previous: correct,
            probeOutput: nil, liveKeys: keys, visibleWindowIDs: [9000]),
            ["easel-desktop": 9000], "failed probes must not retain off-screen windows")

        check(TerminalWindowBindings.reconcile(previous: wrong,
            probeOutput: "/dev/ttys004|2734\n/dev/ttys999|2734\nbad|NaN\n",
            liveKeys: ["ttys004"], visibleWindowIDs: visible), ["ttys004": 2734],
            "ended sessions and invalid probe rows must not produce bindings")
        print("TerminalWindowBindings: 8 regression checks passed")
    }
}
