// slab-aesthetic-profiles — provision the Aesthetic Computer terminal family.
//
// The menubar already switches a tab's `current settings` per session status,
// picking a settings set by name (see AppDelegate.profileName). Claude sessions
// get the plain `Slab-<status>-<dark|light>` family and Codex ones a cooler
// `-codex` twin. Aesthetic Code sessions had no family of their own and were
// borrowing Codex's, so the one interface that IS Aesthetic Computer was the
// one wearing somebody else's colours.
//
// This writes the missing family. The palette is not invented here: it is the
// prompt's own dark scheme, the same values `src/render.mjs` paints the
// interface with, so a terminal running the harness and the piece it is writing
// finally agree about what purple is.
//
// The status hues stay inside that scheme rather than reaching for the usual
// green/amber/red. A wall of terminals still has to be readable at a glance —
// which is the whole reason the families exist — so the ground shifts enough to
// separate the states while every one of them remains recognisably AC.
//
// Colours in Terminal.app are archived NSColor, not hex, which is why this is
// Swift and not a shell script. Idempotent: run it again and it rewrites the
// same values.
//
// It writes the SEED, not the live preferences. Terminal.app serves its
// profiles from memory and rewrites the whole domain when it saves, so anything
// written underneath a running Terminal is clobbered the moment it next
// flushes — which is why `patchTerminalTitleComponents` waits for Terminal to
// quit, and why the first version of this tool appeared to work and then
// silently lost all sixteen profiles. The seed is the durable copy and the one
// other machines actually receive; `slab-seed-terminal` installs it.
//
//   swift slab/bin/slab-aesthetic-profiles.swift [--dry-run] [--seed <plist>]

import AppKit
import Foundation

let dryRun = CommandLine.arguments.contains("--dry-run")
let domain = "com.apple.Terminal" as CFString
let suffix = "-aesthetic"

/// Aesthetic Computer's prompt scheme, verbatim from `render.mjs`.
enum AC {
    static let text      = rgb(255, 255, 255)
    static let promptPink = rgb(200, 30, 100)
    static let soft      = rgb(220, 180, 255)
    static let handle    = rgb(255, 100, 255)
}

func rgb(_ r: Int, _ g: Int, _ b: Int) -> NSColor {
    NSColor(srgbRed: CGFloat(r) / 255, green: CGFloat(g) / 255,
            blue: CGFloat(b) / 255, alpha: 1)
}

/// One ground per status. All of them are the prompt's purple moved rather than
/// replaced: lifted while the machine works, pulled toward the prompt's own
/// pink when it wants you, settled deeper when it is done, and drained when the
/// session has gone stale.
let grounds: [String: NSColor] = [
    "blank":     rgb(70, 50, 100),    // #463264 — the scheme's own ground
    "working":   rgb(86, 58, 122),    // lifted: something is happening
    "rendering": rgb(86, 58, 122),
    "awaiting":  rgb(122, 43, 82),    // pulled toward the prompt pink: your turn
    "complete":  rgb(51, 40, 90),     // settled deeper: nothing is owed
    "stale":     rgb(42, 32, 56),     // drained
]

/// The pulse twin of an attention state — the same ground, brighter, so the two
/// provisioned sets can be flipped between on a tick.
func pulsed(_ c: NSColor) -> NSColor {
    NSColor(srgbRed: min(1, c.redComponent * 1.45),
            green: min(1, c.greenComponent * 1.45),
            blue: min(1, c.blueComponent * 1.45), alpha: 1)
}

/// Light-mode grounds are the same hues at the other end of the scale: the
/// interface stays legible dark-on-light without leaving the palette.
func lightened(_ c: NSColor) -> NSColor {
    NSColor(srgbRed: 1 - (1 - c.redComponent) * 0.16,
            green: 1 - (1 - c.greenComponent) * 0.16,
            blue: 1 - (1 - c.blueComponent) * 0.16, alpha: 1)
}

func archived(_ color: NSColor) -> Data {
    (try? NSKeyedArchiver.archivedData(withRootObject: color,
                                       requiringSecureCoding: false)) ?? Data()
}

/// The seed the repo carries and other machines install from.
func seedPath() -> String {
    if let i = CommandLine.arguments.firstIndex(of: "--seed"),
       i + 1 < CommandLine.arguments.count { return CommandLine.arguments[i + 1] }
    let here = URL(fileURLWithPath: CommandLine.arguments[0]).deletingLastPathComponent()
    return here.deletingLastPathComponent()
        .appendingPathComponent("seed/terminal-profiles.plist").path
}

let seed = seedPath()
// The seed keeps its profiles under `Profiles`, beside the `DefaultProfile`
// name — its own shape, not Terminal's `Window Settings` dictionary.
guard let seedData = FileManager.default.contents(atPath: seed),
      var seedPlist = (try? PropertyListSerialization.propertyList(
        from: seedData, options: [], format: nil)) as? [String: Any],
      var windowSettings = seedPlist["Profiles"] as? [String: Any] else {
    FileHandle.standardError.write(Data("cannot read seed at \(seed)\n".utf8))
    exit(1)
}

/// Clone a real profile so font, title flags and every key this tool does not
/// know about carry over untouched. Inventing a profile from nothing means
/// guessing at Terminal's own defaults; borrowing one means only the colours
/// are ours.
func template(for name: String) -> [String: Any]? {
    if let exact = windowSettings[name] as? [String: Any] { return exact }
    for candidate in ["Slab-blank-dark", "Slab-working-dark", "Grass"] {
        if let found = windowSettings[candidate] as? [String: Any] { return found }
    }
    return nil
}

var written: [String] = []

for (status, darkGround) in grounds.sorted(by: { $0.key < $1.key }) {
    for dark in [true, false] {
        // Attention states carry a second, brighter set the menubar alternates
        // with; the calm ones never pulse, so they never get the twin.
        let pulses = (status == "awaiting" || status == "complete")
        for pulse in pulses ? [false, true] : [false] {
            let base = "Slab-\(status)-\(dark ? "dark" : "light")"
            let name = "\(base)\(suffix)\(pulse ? "-pulse" : "")"
            guard var profile = template(for: base) else { continue }

            var ground = dark ? darkGround : lightened(darkGround)
            if pulse { ground = pulsed(ground) }
            let ink = dark ? AC.text : rgb(36, 28, 48)

            profile["name"] = name
            profile["BackgroundColor"] = archived(ground)
            profile["TextColor"] = archived(ink)
            // Bold is the scheme's soft lavender in the dark and its magenta in
            // the light, where lavender would disappear.
            profile["TextBoldColor"] = archived(dark ? AC.soft : AC.handle)
            // The cursor is the prompt's own block colour — the one mark that
            // says which family this window belongs to even mid-scroll.
            profile["CursorColor"] = archived(AC.promptPink)
            // Titles carry the custom title only, matching the other families.
            for key in ["ShowRepresentedURLInTitle", "ShowActiveProcessInTitle",
                        "ShowDimensionsInTitle", "ShowTTYNameInTitle"] {
                profile[key] = false
            }

            windowSettings[name] = profile
            written.append(name)
        }
    }
}

if dryRun {
    print("would write \(written.count) profiles:")
    for n in written { print("  \(n)") }
    exit(0)
}

seedPlist["Profiles"] = windowSettings
guard let out = try? PropertyListSerialization.data(
        fromPropertyList: seedPlist, format: .xml, options: 0) else {
    FileHandle.standardError.write(Data("cannot serialise the seed\n".utf8))
    exit(1)
}
try? out.write(to: URL(fileURLWithPath: seed))

// The companion list is what `slab-seed-terminal` reads to know which names to
// install, so a profile missing from it is a profile nobody receives.
let listPath = (seed as NSString).deletingPathExtension + ".list"
var names = Set((try? String(contentsOfFile: listPath, encoding: .utf8))?
    .split(separator: "\n").map(String.init) ?? [])
names.formUnion(written)
try? names.sorted().joined(separator: "\n")
    .appending("\n").write(toFile: listPath, atomically: true, encoding: .utf8)

print("wrote \(written.count) profiles into \(seed):")
for n in written { print("  \(n)") }
