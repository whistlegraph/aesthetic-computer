import AppKit

// CLI fast-path: `slab-menubar ledger …` resolves handles from the kept
// on-disk cache and exits before any menubar bootstrap.
if LedgerCLI.handleIfPresent(CommandLine.arguments) { exit(0) }

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let delegate = AppDelegate()
app.delegate = delegate
app.run()
