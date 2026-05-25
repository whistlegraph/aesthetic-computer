import AppKit
import AVFoundation

struct Spec: Codable {
    var title: String
    var outDir: String
    var samples: [SampleSpec]
    var trim: TrimSpec?
    var autoStop: AutoStopSpec?
}

struct SampleSpec: Codable {
    var name: String
    var desc: String
}

struct TrimSpec: Codable {
    var thresholdPctOfPeak: Double?
    var padHeadMs: Double?
    var padTailMs: Double?
    var fadeMs: Double?
    var normalizeDb: Double?
}

struct AutoStopSpec: Codable {
    var onsetDb: Double?
    var silenceDb: Double?
    var silenceDurationMs: Double?
    var maxRecordSec: Double?
}

final class WaveWizardAppDelegate: NSObject, NSApplicationDelegate {
    var wizard: WizardController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        let args = CommandLine.arguments
        guard args.count >= 2 else {
            print("usage: WaveWizard <spec.json>")
            NSApp.terminate(nil); return
        }
        let specURL = URL(fileURLWithPath: args[1])
        do {
            let data = try Data(contentsOf: specURL)
            let spec = try JSONDecoder().decode(Spec.self, from: data)
            wizard = WizardController(spec: spec)
            wizard?.showWindow(nil)
            NSApp.activate(ignoringOtherApps: true)
        } catch {
            print("failed to load spec: \(error.localizedDescription)")
            NSApp.terminate(nil)
        }
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}
