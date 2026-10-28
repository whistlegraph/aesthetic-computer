import AppKit

final class DateWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: WizardController?
    var menuBar: MenuBarDate?

    func applicationDidFinishLaunching(_ notification: Notification) {
        DockIcon.install()
        let controller = WizardController()
        wizard = controller

        // --background (startup/LaunchAgent): come up menu-bar-only — no window,
        // no dock, no focus steal. Clicking the item promotes us to .regular so
        // the window focuses properly. Without the flag, open straight in.
        if CommandLine.arguments.contains("--background") {
            NSApp.setActivationPolicy(.accessory)
        } else {
            controller.showWindow(nil)
            NSApp.activate(ignoringOtherApps: true)
        }

        let menu = MenuBarDate()
        menu.onOpen = { [weak controller] in
            NSApp.setActivationPolicy(.regular)
            controller?.revealUpcoming()
        }
        menu.onToggle = { [weak controller] in
            NSApp.setActivationPolicy(.regular)
            controller?.toggleUpcoming()
        }
        menu.install()
        menuBar = menu

        controller.onNextEventChanged = { [weak menu] date, title in menu?.setNextEvent(date, title: title) }

        // Ensure auth (sign-in screen if no shared ~/.ac-token), then load.
        controller.start()
    }

    // Always-on: closing the window leaves the menu bar item running.
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { false }

    // Clicking the dock icon (or reopening) re-shows the window.
    func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool {
        if !flag { wizard?.showWindow(nil); NSApp.activate(ignoringOtherApps: true) }
        return true
    }
}
