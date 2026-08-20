import AppKit

final class ChartWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: WizardController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        let args = CommandLine.arguments
        let path = args.count >= 2 ? args[1] : defaultLane()
        do {
            let model = try ChartModel(wizardJSON: URL(fileURLWithPath: path))
            let w = WizardController(model: model)
            wizard = w
            w.showWindow(nil)
            NSApp.activate(ignoringOtherApps: true)
        } catch {
            let a = NSAlert()
            a.messageText = "ChartWizard could not open that chart"
            a.informativeText = """
            \(error.localizedDescription)

            Expecting a lane's vox4/.wizard.json — build one with
            pop/.venv/bin/python pop/<lane>/bin/wizard.py
            """
            a.runModal()
            NSApp.terminate(nil)
        }
    }

    /// With no argument, open whichever lane most recently emitted one.
    private func defaultLane() -> String {
        let fm = FileManager.default
        let pop = URL(fileURLWithPath: fm.currentDirectoryPath).appendingPathComponent("pop")
        let lanes = (try? fm.contentsOfDirectory(at: pop, includingPropertiesForKeys: nil)) ?? []
        let charts = lanes.map { $0.appendingPathComponent("vox4/.wizard.json") }
            .filter { fm.fileExists(atPath: $0.path) }
        let newest = charts.max {
            let a = (try? $0.resourceValues(forKeys: [.contentModificationDateKey]).contentModificationDate) ?? .distantPast
            let b = (try? $1.resourceValues(forKeys: [.contentModificationDateKey]).contentModificationDate) ?? .distantPast
            return a < b
        }
        return newest?.path ?? pop.appendingPathComponent("loner/vox4/.wizard.json").path
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}
