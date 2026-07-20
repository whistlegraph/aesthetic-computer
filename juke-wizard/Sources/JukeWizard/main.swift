// JukeWizard — a native macOS popup for selecting, rating, and annotating
// tracks. Sibling of wave-wizard + clip-wizard. Play a queue of audio
// (a folder, an .m3u/.m3u8 playlist, or loose files), leave 1–5 stars,
// freeform notes, and comments pinned to timestamps — all saved beside
// each track as <track>.juke.json so the notes survive re-renders.
//
// Usage:
//   jukewizard [<playlist.m3u8 | folder | file.mp3> ...] [--watch <dir>]
//   bin/jukewizard --queue <file.mp3> <file.mp3>  focused ordered queue
//   jukewizard --spotify-search "artist or track"  headless Spotify search
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
        var selectPath: String? = nil
        var spotifySearch: String? = nil
        var i = 0
        while i < args.count {
            if args[i] == "--watch", i + 1 < args.count { watch.append(args[i + 1]); i += 2; continue }
            if args[i] == "--select", i + 1 < args.count { selectPath = args[i + 1]; i += 2; continue }
            if args[i] == "--spotify-search", i + 1 < args.count { spotifySearch = args[i + 1]; i += 2; continue }
            paths.append(args[i]); i += 1
        }
        if paths.isEmpty {
            var root = URL(fileURLWithPath: #filePath)
            for _ in 0..<4 { root.deleteLastPathComponent() }
            let aesthetic = root.appendingPathComponent("pop/out/pop-library.json").path
            let master = NSHomeDirectory() + "/Desktop/MASTER-playlist.m3u8"
            if FileManager.default.fileExists(atPath: aesthetic) { paths = [aesthetic] }
            else if FileManager.default.fileExists(atPath: master) { paths = [master] }
        }
        let library = Library(inputs: paths)
        // Make sure the requested track is in the queue even if it's a draft
        // not yet in the library index — so we can always select + play it.
        if let sp = selectPath {
            let u = URL(fileURLWithPath: (sp as NSString).expandingTildeInPath)
            if !library.tracks.contains(where: { $0.url.standardizedFileURL.path == u.standardizedFileURL.path }) {
                let lane = u.deletingLastPathComponent().deletingLastPathComponent().lastPathComponent
                library.addFile(u, lane: lane)
            }
        }
        controller = JukeController(library: library, watch: watch, select: selectPath,
                                    spotifySearch: spotifySearch)
        controller?.showWindow(nil)
        if controller?.window?.isMiniaturized == true { controller?.window?.deminiaturize(nil) }
        controller?.window?.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
        // Launch Services can re-apply the previous process's minimized Dock
        // state just after didFinishLaunching. Restore once more on the next
        // run-loop turn so a relaunch can never masquerade as a crash.
        DispatchQueue.main.async { [weak self] in self?.controller?.quickOpenFull() }
    }

    // Stay resident when the window closes — the spinning-CD menu-bar item is
    // JukeWizard's persistent face; click it to bring the window back.
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { false }

    // A hidden resident window should always come back from a Dock click.
    // Without this, AppKit can activate the process while leaving its only
    // window closed, which looks indistinguishable from a crash.
    func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool {
        if !flag { controller?.showWindow(nil) }
        if controller?.window?.isMiniaturized == true { controller?.window?.deminiaturize(nil) }
        controller?.window?.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
        return true
    }

    func applicationDockMenu(_ sender: NSApplication) -> NSMenu? {
        controller?.makeDockMenu()
    }
}

let app = NSApplication.shared
let delegate = JukeAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
