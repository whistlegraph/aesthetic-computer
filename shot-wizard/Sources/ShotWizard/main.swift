// ShotWizard — native macOS wizard for storyboarding: pick shots, line
// up the sequence, and watch the storyboard play through with its real
// generated outputs (Seedance clips, captures, cards) before committing
// to the assembled cut. Sibling of ClipWizard / WaveWizard / JukeWizard.
//
// A board is a single board.json describing an ordered list of shots:
//
//   { "title": "...", "width": 1920, "height": 1080, "fps": 30,
//     "voAudio": "vo.mp3", "driver": "build.mjs",
//     "shots": [ { "id","t0","t1","lane","vo","source","prompt",
//                  "clip","card","status" }, ... ] }
//
// Paths inside it resolve relative to the board.json's own directory.
// Reordering writes the new order straight back to the file — that IS
// the edit. Generation/assembly shell out to the board's node driver.
//
// Usage:  ShotWizard <path/to/board.json | project-dir>
//   e.g.  shot-wizard/bin/shotwizard grants/restless-egg-2026/video
import AppKit

final class ShotWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: ShotWizardController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        DockIcon.install(prefix: "shotwizard")
        let args = Array(CommandLine.arguments.dropFirst())
        guard let first = args.first else {
            print("usage: ShotWizard <board.json | project-dir>")
            NSApp.terminate(nil); return
        }
        var path = (first as NSString).expandingTildeInPath
        var isDir: ObjCBool = false
        if FileManager.default.fileExists(atPath: path, isDirectory: &isDir), isDir.boolValue {
            path = (path as NSString).appendingPathComponent("board.json")
        }
        do {
            let board = try Board(boardPath: path)
            wizard = ShotWizardController(board: board)
            wizard?.showWindow(nil)
            NSApp.activate(ignoringOtherApps: true)
        } catch {
            print("ShotWizard: \(error.localizedDescription)")
            NSApp.terminate(nil)
        }
    }
}

let app = NSApplication.shared
app.setActivationPolicy(.regular)
let delegate = ShotWizardAppDelegate()
app.delegate = delegate
app.run()
