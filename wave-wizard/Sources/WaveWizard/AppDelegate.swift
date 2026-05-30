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
    // Optional audio file played BEFORE the desc speech — used to cue
    // the singer with the melody / a reference take of the line.
    var melodyFile: String?
    // Optional lyric string showing how the words map onto the 11-note
    // brass theme — rendered by AlignmentView as a visual timeline.
    // Conventions:
    //   space      = word boundary
    //   hyphen     = internal syllable split  (mo-ney = 2 syllables)
    //   trailing _ = melisma (sustains an extra note)
    //   bare _     = explicit sustain placeholder
    var lyric: String?
    // Optional per-sample score. When present, AlignmentView swaps from
    // the default hellsine brass theme to these notes. Specs that omit
    // `score` keep the original hellsine alignment.
    var score: ScoreSpec?
}

struct ScoreSpec: Codable {
    /// MIDI number that note offsets are measured from. Notes are
    /// rendered as `rootMel + note.off`.
    var rootMel: Int
    var notes: [ScoreNote]
}

struct ScoreNote: Codable {
    var off: Int
    var beats: Double
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
