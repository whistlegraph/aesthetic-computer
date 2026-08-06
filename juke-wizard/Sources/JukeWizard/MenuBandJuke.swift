// MenuBandJuke.swift — Menu Band's complete Juke feature. Menu Band owns the
// process, application menu, status item, and Dock identity; this module owns
// only the Juke window, playback, library, DJ, room, cloud, annotations, and
// local control socket.
import AppKit

public final class MenuBandJuke {
    private var controller: JukeController?
    private var controlServer: JukeControlServer?

    public init() {}

    public var isVisible: Bool {
        controller?.window?.isVisible == true && controller?.window?.isMiniaturized == false
    }

    /// Start the Juke once. Later calls reuse the same controller and simply
    /// raise its window, so Menu Band never creates competing players.
    public func start(
        arguments: [String] = [],
        showsWindow: Bool = true
    ) {
        if controller != nil {
            if showsWindow { open() }
            return
        }

        var watch: [String] = []
        var paths: [String] = []
        var selectPath: String?
        var spotifySearch: String?
        var startPrimpats = false
        var startBeats = false
        var launchInBackground = !showsWindow
        var index = 0
        while index < arguments.count {
            let argument = arguments[index]
            if argument == "--watch", index + 1 < arguments.count {
                watch.append(arguments[index + 1]); index += 2; continue
            }
            if argument == "--select", index + 1 < arguments.count {
                selectPath = arguments[index + 1]; index += 2; continue
            }
            if argument == "--spotify-search", index + 1 < arguments.count {
                spotifySearch = arguments[index + 1]; index += 2; continue
            }
            if argument == "--primpats" { startPrimpats = true; index += 1; continue }
            if argument == "--beats" { startBeats = true; index += 1; continue }
            if argument == "--background" { launchInBackground = true; index += 1; continue }
            paths.append(argument); index += 1
        }

        let aesthetic = Self.defaultLibraryPath()
        if paths.isEmpty {
            let master = NSHomeDirectory() + "/Desktop/MASTER-playlist.m3u8"
            if FileManager.default.fileExists(atPath: aesthetic) { paths = [aesthetic] }
            else if FileManager.default.fileExists(atPath: master) { paths = [master] }
        }

        let masterPath = URL(fileURLWithPath: aesthetic).standardizedFileURL.path
        let includesMasterLibrary = paths.contains {
            URL(fileURLWithPath: ($0 as NSString).expandingTildeInPath)
                .standardizedFileURL.path == masterPath
        }
        let scopedInputs = includesMasterLibrary ? [] : paths
        let playlistName: String? = scopedInputs.isEmpty ? nil : {
            if scopedInputs.count > 1 { return "\(scopedInputs.count)-track playlist" }
            let url = URL(fileURLWithPath: (scopedInputs[0] as NSString).expandingTildeInPath)
            if ["m3u", "m3u8"].contains(url.pathExtension.lowercased()) {
                return url.deletingPathExtension().lastPathComponent
            }
            return url.hasDirectoryPath ? url.lastPathComponent : "Playlist"
        }()

        let library = Library(inputs: paths)
        if let selectPath {
            let url = URL(fileURLWithPath: (selectPath as NSString).expandingTildeInPath)
            if !library.tracks.contains(where: {
                $0.url.standardizedFileURL.path == url.standardizedFileURL.path
            }) {
                let lane = url.deletingLastPathComponent().deletingLastPathComponent().lastPathComponent
                library.addFile(url, lane: lane)
            }
        }

        let controller = JukeController(
            library: library,
            watch: watch,
            select: selectPath,
            spotifySearch: spotifySearch,
            startPrimpats: startPrimpats,
            startBeats: startBeats,
            playlistName: playlistName,
            fullLibraryPath: aesthetic
        )
        self.controller = controller
        let server = JukeControlServer(controller: controller)
        self.controlServer = server
        server.start()

        if launchInBackground {
            controller.window?.orderOut(nil)
        } else {
            show(controller)
        }

        guard !launchInBackground else { return }
        if startBeats {
            DispatchQueue.main.async { [weak controller] in controller?.showDetachedBeats() }
        } else if startPrimpats {
            DispatchQueue.main.async { [weak controller] in controller?.showDetachedPrimpats() }
        } else {
            DispatchQueue.main.async { [weak controller] in controller?.quickOpenFull() }
        }
    }

    public func open() {
        guard let controller else {
            start(showsWindow: true)
            return
        }
        show(controller)
        controller.quickOpenFull()
    }

    public func toggle() {
        guard let controller else {
            start(showsWindow: true)
            return
        }
        controller.quickToggleFull()
    }

    public func stop() {
        controlServer?.stop()
        controlServer = nil
    }

    private func show(_ controller: JukeController) {
        controller.showWindow(nil)
        if controller.window?.isMiniaturized == true { controller.window?.deminiaturize(nil) }
        controller.window?.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
    }

    private static func defaultLibraryPath() -> String {
        if let configured = ProcessInfo.processInfo.environment["MENU_BAND_JUKE_LIBRARY"],
           !configured.isEmpty {
            return (configured as NSString).expandingTildeInPath
        }
        let home = FileManager.default.homeDirectoryForCurrentUser
        let candidates = [
            home.appendingPathComponent("aesthetic-computer/pop/out/pop-library.json"),
            home.appendingPathComponent("Developer/aesthetic-computer/pop/out/pop-library.json"),
            URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
                .appendingPathComponent("pop/out/pop-library.json"),
        ]
        return candidates.first(where: { FileManager.default.fileExists(atPath: $0.path) })?.path
            ?? candidates[0].path
    }
}
