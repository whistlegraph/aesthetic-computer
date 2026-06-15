import AppKit

final class DateWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: WizardController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        let controller = WizardController()
        wizard = controller
        controller.showWindow(nil)
        NSApp.activate(ignoringOtherApps: true)
        // Ensure auth (device-pair screen if needed), then load the week.
        controller.start()
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}
