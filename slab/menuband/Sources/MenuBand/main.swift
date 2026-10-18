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

// Headless capture of the real live QWERTY keymap for promos/reels.
if QwertyCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of the live scale-teaching staff used in promos/reels.
if ScaleStaffCLI.runIfRequested(CommandLine.arguments) {
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

// Headless capture of the real full-screen LED visualizer.
if VisualizerCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of TrackDrum's circle-per-rhythm polyrhythm trainer.
if PolyrhythmTrainerCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of the ⇧Tab-bisected pad (pitch left, drum right).
if TrackpadSplitCLI.runIfRequested(CommandLine.arguments) {
    exit(0)
}

// Headless capture of the pitch-slider page's ToneTrials strip.
if ToneTrialsCLI.runIfRequested(CommandLine.arguments) {
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
              let url = app.executableURL,
              url.lastPathComponent == Bundle.main.executableURL?.lastPathComponent else { return false }
        // The embedded MenuBandLauncher shares our bundle identifier but is
        // a different executable, so it must not count as another Menu Band.
        if let bundleID = Bundle.main.bundleIdentifier {
            return app.bundleIdentifier == bundleID
        }
        return url == Bundle.main.executableURL
    }
    if duplicate {
        NSLog("MenuBand: duplicate instance detected — exiting")
        exit(0)
    }
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
#if MAC_APP_STORE
// The store app has a persistent Dock/menu entry so closing its panel never
// strands the user. The directly installed menu-bar utility stays accessory.
app.setActivationPolicy(.regular)
#else
app.setActivationPolicy(.accessory)
#endif
// A plain SIGTERM (pkill, launchd stop) would kill the process without
// `applicationWillTerminate`, skipping the audio teardown — and a duplex
// session on the Scarlett that dies mid-render leaves the interface wedged
// for every client until a replug. Route the signal through the normal quit.
signal(SIGTERM, SIG_IGN)
let sigtermSource = DispatchSource.makeSignalSource(signal: SIGTERM, queue: .main)
sigtermSource.setEventHandler { NSApp.terminate(nil) }
sigtermSource.resume()
app.run()
