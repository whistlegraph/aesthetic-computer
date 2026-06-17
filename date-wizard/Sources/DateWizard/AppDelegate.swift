import AppKit

final class DateWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: WizardController?
    var menuBar: MenuBarDays?

    func applicationDidFinishLaunching(_ notification: Notification) {
        DockIcon.install(prefix: "datewizard")
        let controller = WizardController()
        wizard = controller
        controller.showWindow(nil)
        NSApp.activate(ignoringOtherApps: true)

        // Always-on menu bar strip: seven ROYGBIV day circles (S M T W T F S),
        // today's lit. Click opens the wizard; the app stays resident so the
        // strip is always there even when the window is closed.
        let menu = MenuBarDays()
        menu.onOpen = { [weak controller] in
            controller?.showWindow(nil)
            NSApp.activate(ignoringOtherApps: true)
        }
        menu.onToday = { [weak controller] in controller?.revealToday() }
        menu.onSelectDay = { [weak controller] date in controller?.revealDay(date) }
        menu.install()
        menuBar = menu

        // Keep the menu bar strip's lit dot in sync with the wizard's focus.
        controller.onFocusedDayChanged = { [weak menu] date in menu?.setFocusedDay(date) }

        // Launch flags: open straight into a single-day view. Applied BEFORE
        // start() so the initial load fetches the day (not the week first).
        //   --tomorrow            tomorrow
        //   --today               today
        //   --day=<+/-N>          N days from today (e.g. --day=2)
        let args = CommandLine.arguments
        let cal = Calendar.current
        if args.contains("--tomorrow") {
            controller.setDayMode(cal.date(byAdding: .day, value: 1, to: Date()) ?? Date())
        } else if args.contains("--today") {
            controller.setDayMode(Date())
        } else if let dayArg = args.first(where: { $0.hasPrefix("--day=") }),
                  let n = Int(dayArg.dropFirst("--day=".count)) {
            controller.setDayMode(cal.date(byAdding: .day, value: n, to: Date()) ?? Date())
        }

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
