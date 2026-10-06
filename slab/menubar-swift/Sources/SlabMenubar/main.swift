import AppKit

// Hook fast-path: `slab-menubar hook <event>` is how a downloaded Slab tracks
// sessions without `jq` on the machine. It runs on every prompt and every tool
// call, so it comes first and exits before anything else is touched.
if HookCLI.handleIfPresent(CommandLine.arguments) { exit(0) }

// CLI fast-path: `slab-menubar ledger …` resolves handles from the kept
// on-disk cache and exits before any menubar bootstrap.
if LedgerCLI.handleIfPresent(CommandLine.arguments) { exit(0) }

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
// Isolated editor mode is also useful for UI verification without a second menubar daemon.
if CommandLine.arguments.contains("--display-layout") {
    DisplayLayoutWindow.show()
    app.run()
    exit(0)
}
let delegate = AppDelegate()
app.delegate = delegate
app.run()
