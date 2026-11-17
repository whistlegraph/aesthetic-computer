import AppKit

/// The Dock loads this while Aesthetic Computer sits in the Dock closed, so the
/// icon keeps following light, dark, the accent and Slab's tone without the app.
@objc(ACDockTilePlugin)
final class ACDockTilePlugin: NSObject, NSDockTilePlugIn {
    private var watcher: DockIconWatcher?

    func setDockTile(_ dockTile: NSDockTile?) {
        watcher = nil
        guard let dockTile else { return }
        let view = DockTileView(frame: NSRect(origin: .zero, size: dockTile.size))
        view.pals = DockIcon.pals(in: Bundle(for: ACDockTilePlugin.self))
        dockTile.contentView = view
        watcher = DockIconWatcher { [weak dockTile] look in
            view.look = look
            dockTile?.display()
        }
    }
}

private final class DockTileView: NSView {
    var look: DockIcon.Look?
    var pals: NSImage?

    override func draw(_ dirtyRect: NSRect) {
        guard let look else { return }
        DockIcon.draw(in: bounds, look: look, pals: pals)
    }
}
