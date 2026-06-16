import AppKit

final class DateWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: WizardController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        DockIcon.install(prefix: "datewizard")
        let controller = WizardController()
        wizard = controller
        controller.showWindow(nil)
        NSApp.activate(ignoringOtherApps: true)
        // Ensure auth (sign-in screen if no shared ~/.ac-token), then load the week.
        controller.start()
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}
