// ClipWizard — native macOS wizard for auditioning, picking, and
// stitching the motion takes of a /pop track into its final cut.
// Sibling of WaveWizard; consumes the pop/lib/motion-pipeline.mjs
// conventions:
//
//   shots   pop/<lane>/out/motion/<slug>-shot-<i>-<name>.mp4
//   archive pop/<lane>/out/motion/archive/*.vN.mp4
//   picks   pop/<lane>/out/motion/takes.json
//   struct  pop/<lane>/out/<slug>.struct.json
//   audio   pop/<lane>/out/<slug>.mp3
//   driver  pop/<lane>/bin/gen-motion-<slug>.mjs   (re-roll / assemble)
//
// Usage:  ClipWizard <lane> <slug> [--pop /path/to/pop]
//   e.g.  clip-wizard/bin/clipwizard marimba marimbaba
//
// Picking is free; only ↻ re-roll spends money (and the old take is
// archived by the driver, never deleted).
import AppKit

final class ClipWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: ClipWizardController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        DockIcon.install(prefix: "clipwizard")
        var args = Array(CommandLine.arguments.dropFirst())
        var popDir = FileManager.default.currentDirectoryPath + "/pop"
        if let i = args.firstIndex(of: "--pop"), i + 1 < args.count {
            popDir = args[i + 1]
            args.removeSubrange(i...(i + 1))
        }
        guard args.count >= 2 else {
            print("usage: ClipWizard <lane> <slug> [--pop /path/to/pop]")
            NSApp.terminate(nil); return
        }
        do {
            let project = try Project(popDir: popDir, lane: args[0], slug: args[1])
            wizard = ClipWizardController(project: project)
            wizard?.showWindow(nil)
            NSApp.activate(ignoringOtherApps: true)
        } catch {
            print("failed to load project: \(error.localizedDescription)")
            NSApp.terminate(nil)
        }
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

let app = NSApplication.shared
let delegate = ClipWizardAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
