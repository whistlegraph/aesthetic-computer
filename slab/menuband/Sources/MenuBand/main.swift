import AppKit

// Headless KidLisp renderer mode: `--kidlisp-render <src> <w> <h> <out.ppm>`.
// Short-circuits app startup so conformance harnesses can call the same
// binary without launching a status item.
if KLCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of the real menubar piano graphic for promos/reels.
if MenubarCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of the real About window for promos/reels.
if AboutCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of the real popover interface for App Store screenshots.
if PopoverCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of the real "Looking For Players?" Jam window.
if JamCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of the real full-screen keymap overlay for App Store shots.
if KeymapCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Singleton guard: when MenuBand is spawned by both launchd's
// KeepAlive (after crash / sleep wake) AND MenuBandLauncher's
// double-tap path at the same time, we get two instances fighting
// for the same NSStatusItem slot. The later instance quits silently
// so the first keeps its menubar slot and run-loop intact.
do {
    let myPid = ProcessInfo.processInfo.processIdentifier
    let duplicate = NSWorkspace.shared.runningApplications.contains { app in
        guard app.processIdentifier != myPid,
              let url = app.executableURL else { return false }
        return url.lastPathComponent == "MenuBand"
    }
    if duplicate {
        NSLog("MenuBand: duplicate instance detected — exiting")
        exit(0)
    }
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory)  // menubar-only, no Dock icon
app.run()
