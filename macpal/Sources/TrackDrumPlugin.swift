import AppKit

/// MacPal carries TrackDrum as signed nested code. The menu item exists only
/// when the helper is actually present, so dev bundles never expose a dead
/// action.
final class TrackDrumPlugin: NSObject, PalPlugin {
    static let menuTitle = "TrackDrum"

    var appURL: URL? {
        let url = Bundle.main.bundleURL
            .appendingPathComponent("Contents/Helpers/TrackDrum.app")
        let executable = url.appendingPathComponent("Contents/MacOS/TrackDrum")
        return FileManager.default.isExecutableFile(atPath: executable.path)
            ? url : nil
    }

    func attach(to c: PalController) {}

    func menuItems(for c: PalController) -> [NSMenuItem] {
        guard appURL != nil else { return [] }
        let item = NSMenuItem(
            title: Self.menuTitle,
            action: #selector(openTrackDrum),
            keyEquivalent: ""
        )
        item.target = self
        return [item]
    }

    @objc private func openTrackDrum() {
        guard let appURL else { return }
        let configuration = NSWorkspace.OpenConfiguration()
        configuration.activates = true
        NSWorkspace.shared.openApplication(
            at: appURL,
            configuration: configuration
        ) { _, error in
            if let error { NSLog("MacPal: TrackDrum launch failed: \(error)") }
        }
    }
}
