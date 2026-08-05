import AppKit

let app = NSApplication.shared
let delegate = TracktrampAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)

let mainMenu = NSMenu()
let appMenuItem = NSMenuItem()
mainMenu.addItem(appMenuItem)
let appMenu = NSMenu(title: "TrackDrum")
appMenu.addItem(
    withTitle: "Quit TrackDrum",
    action: #selector(NSApplication.terminate(_:)),
    keyEquivalent: "q"
)
appMenuItem.submenu = appMenu
app.mainMenu = mainMenu
app.run()
