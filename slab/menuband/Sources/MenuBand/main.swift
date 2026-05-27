import AppKit

// Headless KidLisp renderer mode: `--kidlisp-render <src> <w> <h> <out.ppm>`.
// Short-circuits app startup so conformance harnesses can call the same
// binary without launching a status item.
if KLCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory)  // menubar-only, no Dock icon
app.run()
