import AppKit

final class DateWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: WizardController?
    var menuBar: MenuBarDays?

    func applicationDidFinishLaunching(_ notification: Notification) {
        DockIcon.install(prefix: "datewizard")
        let controller = WizardController()
        wizard = controller

        // --background (startup/LaunchAgent): come up as a menu-bar-only daemon —
        // just the wand icon, no window, no dock, no focus steal. The calendar
        // opens on demand when the icon is clicked (which promotes us to .regular
        // so the window focuses properly). Without the flag, open straight in.
        let background = CommandLine.arguments.contains("--background")
        if background {
            NSApp.setActivationPolicy(.accessory)
        } else {
            controller.showWindow(nil)
            NSApp.activate(ignoringOtherApps: true)
        }

        // Always-on menu bar: a black magic-wand template icon. Click opens the
        // wizard; the app stays resident so the icon is always there even when
        // the window is closed.
        let menu = MenuBarDays()
        menu.onOpen = { [weak controller] in
            NSApp.setActivationPolicy(.regular)
            controller?.revealUpcoming()
        }
        // Left-clicking the wand toggles: open if hidden, dismiss if already up.
        menu.onToggle = { [weak controller] in
            NSApp.setActivationPolicy(.regular)
            controller?.toggleUpcoming()
        }
        menu.onToday = { [weak controller] in controller?.revealToday() }
        menu.install()
        menuBar = menu

        // Keep the menu bar strip's lit dot in sync with the wizard's focus.
        controller.onFocusedDayChanged = { [weak menu] date in menu?.setFocusedDay(date) }
        // Feed the next appointment to the wand's countdown badge.
        controller.onNextEventChanged = { [weak menu] date, title in menu?.setNextEvent(date, title: title) }

        // Ensure auth (sign-in screen if no shared ~/.ac-token), then load.
        controller.start()
    }

    // Always-on: closing the window leaves the menu bar strip running.
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { false }

    // Clicking the dock icon (or reopening) re-shows the wizard window.
    func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool {
        if !flag { wizard?.showWindow(nil); NSApp.activate(ignoringOtherApps: true) }
        return true
    }
}
