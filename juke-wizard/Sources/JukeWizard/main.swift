// JukeWizard — a native macOS popup for selecting, rating, and annotating
// tracks. Sibling of wave-wizard + clip-wizard. Play a queue of audio
// (a folder, an .m3u/.m3u8 playlist, or loose files), leave 1–5 stars,
// freeform notes, and comments pinned to timestamps — all saved beside
// each track as <track>.juke.json so the notes survive re-renders.
//
// Usage:
//   jukewizard [<playlist.m3u8 | folder | file.mp3> ...] [--watch <dir>]
//   (no args → opens ~/Desktop/MASTER-playlist.m3u8 if present)
//
//   --watch <dir>   auto-pop: when a fresh audio file lands here, add it
//                   to the queue and start playing (repeatable).
import AppKit

final class JukeAppDelegate: NSObject, NSApplicationDelegate {
    var controller: JukeController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        DockIcon.install(prefix: "jukewizard")
        let args = Array(CommandLine.arguments.dropFirst())
        var watch: [String] = []
        var paths: [String] = []
        var i = 0
        while i < args.count {
            if args[i] == "--watch", i + 1 < args.count { watch.append(args[i + 1]); i += 2; continue }
            paths.append(args[i]); i += 1
        }
        if paths.isEmpty {
            let master = NSHomeDirectory() + "/Desktop/MASTER-playlist.m3u8"
            if FileManager.default.fileExists(atPath: master) { paths = [master] }
        }
        let library = Library(inputs: paths)
        guard !library.tracks.isEmpty else {
            print("JukeWizard: no tracks found.")
            print("usage: jukewizard [<playlist.m3u8 | folder | file.mp3> ...] [--watch <dir>]")
            NSApp.terminate(nil); return
        }
        controller = JukeController(library: library, watch: watch)
        controller?.showWindow(nil)
        NSApp.activate(ignoringOtherApps: true)
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

let app = NSApplication.shared
let delegate = JukeAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
