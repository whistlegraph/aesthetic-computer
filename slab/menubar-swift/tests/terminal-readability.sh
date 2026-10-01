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
    precondition(TerminalReadability.contrast(p.text!, bg) >= 6.999, "Text below 7:1")
    precondition(TerminalReadability.contrast(p.bold!, bg) >= 4.499, "Bold below 4.5:1")
    for color in TerminalReadability.ansi(on: bg) {
        precondition(TerminalReadability.contrast(color, bg) >= 4.499, "ANSI below 4.5:1")
    }
    // The seven rainbow hues must remain distinct after correction.
    let colors = TerminalReadability.ansi(on: bg)
    let hues = [1, 11, 3, 2, 4, 12, 5].map { i -> CGFloat in
        let c = colors[i]
        let n = NSColor(deviceRed: CGFloat(c.0)/65535, green: CGFloat(c.1)/65535,
                        blue: CGFloat(c.2)/65535, alpha: 1)
        precondition(n.saturationComponent > 0.20, "Rainbow lost saturation: bg=\(bg), index=\(i), saturation=\(n.saturationComponent)")
        return n.hueComponent
    }
    precondition(Set(hues.map { Int($0 * 100) }).count == 7, "Rainbow collapsed")
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
check(AppDelegate.Palette(bg: (16000,2000,9500), text: (65535,45000,55000), bold: (65535,54000,62000), cursor: (65535,5000,34000)))
check(AppDelegate.Palette(bg: (17000,12000,1000), text: (65535,61000,30000), bold: (65535,64000,51000), cursor: (65535,12000,33000)))
let directory = URL(fileURLWithPath: CommandLine.arguments[1])
let url = try TerminalReadability.writeProfile(in: directory)
let data = try Data(contentsOf: url)
let profile = try PropertyListSerialization.propertyList(from: data, format: nil) as! [String: Any]
precondition(profile["DisableANSIColor"] as? Bool == false)
precondition(profile["CommandString"] == nil, "New tabs must still open a shell")
precondition(profile["CursorType"] as? Int == 2)
_ = try TerminalReadability.writeProfile(in: directory)
let repeatedData = try Data(contentsOf: url)
precondition(repeatedData == data)
let script = "tell application \"Terminal\"\n" + TerminalReadability.bootstrapScript(profileURL: url) + "\nend tell"
try script.write(to: directory.appendingPathComponent("bootstrap.applescript"), atomically: true, encoding: .utf8)
print("PASS \(checks) palettes: text ≥7:1, bold/16 ANSI ≥4.5:1, cursor ≥3:1; seven distinct rainbow hues")
let seedData = try Data(contentsOf: URL(fileURLWithPath: CommandLine.arguments[2]))
let seed = try PropertyListSerialization.propertyList(from: seedData, format: nil) as! [String: Any]
let profiles = seed["Profiles"] as! [String: [String: Any]]
func rgb(_ data: Data) throws -> TerminalReadability.RGB {
    let c = try NSKeyedUnarchiver.unarchivedObject(ofClass: NSColor.self, from: data)!.usingColorSpace(.genericRGB)!
    return (Int((c.redComponent*65535).rounded()), Int((c.greenComponent*65535).rounded()), Int((c.blueComponent*65535).rounded()))
}
for (name, p) in profiles {
    precondition(p["DisableANSIColor"] as? Bool == false, name)
    let bg = try rgb(p["BackgroundColor"] as! Data)
    let maximum = max(TerminalReadability.contrast((0,0,0), bg), TerminalReadability.contrast((65535,65535,65535), bg))
    for key in ["TextColor", "TextBoldColor", "CursorColor"] + TerminalReadability.ansiNames.map({ "ANSI\($0)Color" }) {
        let ink = try rgb(p[key] as! Data)
        let target: Double = key == "CursorColor" ? 3 : key == "TextColor" ? 7 : 4.5
        precondition(TerminalReadability.contrast(ink, bg) >= min(target, maximum) - 0.001, "\(name) \(key)")
    }
}
print("PASS \(profiles.count) installed seed palettes")
SWIFT
swift "$probe/check.swift" "$probe" "$root/../seed/terminal-profiles.plist"
osacompile -o "$probe/bootstrap.scpt" "$probe/bootstrap.applescript"
