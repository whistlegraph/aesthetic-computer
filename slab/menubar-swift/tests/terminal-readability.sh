#!/bin/bash
# Exercise the production status palettes, contrast correction, and imported profile.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
probe="$(mktemp -d /tmp/slab-readability.XXXXXX)"
trap 'rm -rf "$probe"' EXIT
cp "$root/Sources/SlabMenubar/TerminalReadability.swift" "$probe/check.swift"
python3 - "$root" "$probe/check.swift" <<'PY'
import sys
source = open(sys.argv[1] + '/Sources/SlabMenubar/AppDelegate.swift').read()
palettes = source[source.index('    typealias RGB ='):source.index('    /// Stable palette identity')]
with open(sys.argv[2], 'a') as f:
    f.write('\nenum ClaudeSession { enum State: CaseIterable { case blank, working, rendering, complete, awaiting, interrupted, stale } }\n')
    f.write('enum AppDelegate {\n' + palettes + '\n}\n')
PY
cat >> "$probe/check.swift" <<'SWIFT'
var checks = 0
func check(_ p: AppDelegate.Palette) {
    let p = p.readable, bg = p.bg!
    for ink in [p.text!, p.bold!] {
        precondition(TerminalReadability.contrast(ink, bg) >= 6.999, "Text below 7:1")
    }
    precondition(TerminalReadability.contrast(p.cursor!, bg) >= 2.999, "Invisible cursor")
    checks += 1
}
for state in ClaudeSession.State.allCases {
    for dark in [false, true] {
        for blink in [false, true] {
            for agent in ["claude", "codex", "easel"] {
                let p = AppDelegate.statusDecor(for: state, dark: dark, blink: blink, agentType: agent).palette
                for palette in [p, AppDelegate.loopboyTint(p, dark: dark, active: false),
                                AppDelegate.loopboyTint(p, dark: dark, active: true)] {
                    check(palette)
                    func blend(_ c: AppDelegate.RGB?, _ m: AppDelegate.RGB, _ f: Double) -> AppDelegate.RGB? {
                        guard let c else { return nil }
                        func v(_ a: Int, _ b: Int) -> Int { Int((Double(a) * (1-f) + Double(b) * f).rounded()) }
                        return (v(c.0,m.0), v(c.1,m.1), v(c.2,m.2))
                    }
                    check(AppDelegate.Palette(bg: blend(palette.bg, (62000,9000,38000), 0.1),
                        text: palette.text, bold: blend(palette.bold, (65535,40000,55000), 0.35),
                        cursor: (62000,9000,38000)))
                }
            }
        }
    }
}
check(AppDelegate.Palette(bg: (65535,39000,53500), text: (26000,800,13000), bold: (16000,0,8000), cursor: (65535,5000,34000)))
check(AppDelegate.Palette(bg: (65535,61000,30000), text: (23000,10500,0), bold: (12000,4500,0), cursor: (65535,12000,33000)))
let directory = URL(fileURLWithPath: CommandLine.arguments[1])
let url = try TerminalReadability.writeProfile(in: directory)
let data = try Data(contentsOf: url)
let profile = try PropertyListSerialization.propertyList(from: data, format: nil) as! [String: Any]
precondition(profile["DisableANSIColor"] as? Bool == true)
precondition(profile["CommandString"] == nil, "New tabs must still open a shell")
precondition(profile["CursorType"] as? Int == 2)
_ = try TerminalReadability.writeProfile(in: directory)
let repeatedData = try Data(contentsOf: url)
precondition(repeatedData == data)
let script = "tell application \"Terminal\"\n" + TerminalReadability.bootstrapScript(profileURL: url) + "\nend tell"
try script.write(to: directory.appendingPathComponent("bootstrap.applescript"), atomically: true, encoding: .utf8)
print("PASS \(checks) palettes: text/bold ≥7:1, cursor ≥3:1; RGB-safe profile")
let seedData = try Data(contentsOf: URL(fileURLWithPath: CommandLine.arguments[2]))
let seed = try PropertyListSerialization.propertyList(from: seedData, format: nil) as! [String: Any]
let profiles = seed["Profiles"] as! [String: [String: Any]]
func rgb(_ data: Data) throws -> TerminalReadability.RGB {
    let c = try NSKeyedUnarchiver.unarchivedObject(ofClass: NSColor.self, from: data)!.usingColorSpace(.deviceRGB)!
    return (Int((c.redComponent*65535).rounded()), Int((c.greenComponent*65535).rounded()), Int((c.blueComponent*65535).rounded()))
}
for (name, p) in profiles {
    precondition(p["DisableANSIColor"] as? Bool == true, name)
    let bg = try rgb(p["BackgroundColor"] as! Data)
    let maximum = max(TerminalReadability.contrast((0,0,0), bg), TerminalReadability.contrast((65535,65535,65535), bg))
    for key in ["TextColor", "TextBoldColor", "CursorColor"] {
        let ink = try rgb(p[key] as! Data)
        precondition(TerminalReadability.contrast(ink, bg) >= min(key == "CursorColor" ? 3 : 7, maximum) - 0.001, "\(name) \(key)")
    }
}
print("PASS \(profiles.count) installed seed palettes")
SWIFT
swift "$probe/check.swift" "$probe" "$root/../seed/terminal-profiles.plist"
osacompile -o "$probe/bootstrap.scpt" "$probe/bootstrap.applescript"
