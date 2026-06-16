import AppKit

final class DateWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: WizardController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        DockIcon.install(prefix: "datewizard")
        let controller = WizardController()
        wizard = controller
        controller.showWindow(nil)
        NSApp.activate(ignoringOtherApps: true)

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

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}
