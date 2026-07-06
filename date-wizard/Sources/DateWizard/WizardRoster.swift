// WizardRoster.swift — the "wizard of wizards" roster, folded into DateWizard so
// the calendar daemon IS the one menu-bar wizard. Ported from the standalone
// wizard-wizard app: a hand-pixeled wizard guy for the menu-bar face, plus the
// roster of sibling wizards it can summon. DateWizard is NOT in the list — the
// date is built into this daemon now.
import AppKit
import UniformTypeIdentifiers

// Our own little wizard guy — hand-placed pixels, no emoji. Purple hat, gold
// star, gray beard. Drawn at integer scales so he stays crisp.
let wizardGrid = [
    "......X......",
    ".....XX......",
    ".....XXX.....",
    "....XXXX.....",
    "....XXXXX....",
    "...XXXXXX....",
    "...XX*XXXX...",
    "..XXXXXXXX...",
    ".XXXXXXXXXXX.",
    "...FFFFFF....",
    "...FEFFEF....",
    "..WFFFFFFW...",
    "..WWFFFFWW...",
    "...WWWWWW....",
    "....WWWW.....",
    ".....WW......",
]

let wizardPalette: [Character: NSColor] = [
    "X": NSColor(calibratedRed: 0.42, green: 0.32, blue: 0.85, alpha: 1), // hat
    "*": NSColor(calibratedRed: 1.00, green: 0.80, blue: 0.20, alpha: 1), // star
    "F": NSColor(calibratedRed: 0.95, green: 0.78, blue: 0.60, alpha: 1), // face
    "E": NSColor(calibratedRed: 0.15, green: 0.12, blue: 0.10, alpha: 1), // eyes
    "W": NSColor(calibratedRed: 0.72, green: 0.72, blue: 0.75, alpha: 1), // beard
]

func wizardGuy(scale s: CGFloat) -> NSImage {
    let w = CGFloat(wizardGrid[0].count) * s
    let h = CGFloat(wizardGrid.count) * s
    return NSImage(size: NSSize(width: w, height: h), flipped: true) { _ in
        for (y, row) in wizardGrid.enumerated() {
            for (x, ch) in row.enumerated() {
                guard let c = wizardPalette[ch] else { continue }
                c.setFill()
                NSRect(x: CGFloat(x) * s, y: CGFloat(y) * s, width: s, height: s).fill()
            }
        }
        return true
    }
}

// The menu-bar face: a plain black magic wand. It's a *template* image, so macOS
// tints it to match the bar (black on light, white on dark) exactly like Menu
// Band's note glyph — unlike the multicolored wizardGuy, which stays a costume
// for the About window and the summon roster. SF Symbol so it stays crisp at any
// bar thickness; wizardGuy is the fallback if the symbol is ever missing.
//
// With a `badge` string (e.g. "2h", "45m", "3d") the wand carries the countdown
// to the next appointment as small H:MM:SS text sitting to the LEFT of the glyph,
// raised toward the wand's tip — no pill, no outline, and it never overlaps the
// wand. The text is tinted to THIS MACHINE's system accent color
// (NSColor.controlAccentColor) so each box's wand reads as its own. A colored
// badge can't ride a template image (macOS recolors those
// wholesale), so in that case we return a *non-template* image and tint the wand
// (dark ? white : black) ourselves.
//
// `dot` is the middle rung of the menu-bar-fit ladder: when the bar is a little
// crowded we drop the countdown text but keep a small day-colored presence dot
// (left of the wand) so you still see "there's something coming up." `badge`
// wins over `dot` when both are supplied; neither → the plain template wand.
func wandGlyph(pointSize: CGFloat = 15, badge: String? = nil, dot: Bool = false,
               dark: Bool = false) -> NSImage {
    let cfg = NSImage.SymbolConfiguration(pointSize: pointSize, weight: .regular)
    let symbol = NSImage(systemSymbolName: "wand.and.stars", accessibilityDescription: "Wizard")?
        .withSymbolConfiguration(cfg) ?? wizardGuy(scale: 1)

    let hasBadge = (badge?.isEmpty == false)
    guard hasBadge || dot else {
        symbol.isTemplate = true
        return symbol
    }

    let tinted = tintedImage(symbol, color: dark ? .white : .black)
    let accent = NSColor.controlAccentColor       // this machine's system accent
    let gap: CGFloat = 2                          // clear space between badge and wand

    // Presence-dot rung: a small day-colored dot, left of the wand (no overlap).
    if !hasBadge {
        let dotD: CGFloat = 6
        let size = NSSize(width: dotD + gap + tinted.size.width,
                          height: max(tinted.size.height, dotD))
        let out = NSImage(size: size)
        out.lockFocus()
        accent.setFill()
        NSBezierPath(ovalIn: NSRect(x: 0, y: (size.height - dotD) / 2, width: dotD, height: dotD)).fill()
        tinted.draw(at: NSPoint(x: dotD + gap, y: (size.height - tinted.size.height) / 2),
                    from: .zero, operation: .sourceOver, fraction: 1)
        out.unlockFocus()
        out.isTemplate = false
        return out
    }
    let badge = badge!

    // Countdown rung: small accent-colored text, left of the wand and raised to
    // its tip (top-aligned), no pill/outline.
    let font = NSFont.systemFont(ofSize: 7.5, weight: .bold)
    let textAttrs: [NSAttributedString.Key: Any] = [.font: font, .foregroundColor: accent]
    let textSize = (badge as NSString).size(withAttributes: textAttrs)
    let size = NSSize(width: textSize.width + gap + tinted.size.width,
                      height: max(tinted.size.height, textSize.height))
    let out = NSImage(size: size)
    out.lockFocus()
    (badge as NSString).draw(at: NSPoint(x: 0, y: size.height - textSize.height),
                             withAttributes: textAttrs)
    tinted.draw(at: NSPoint(x: textSize.width + gap, y: (size.height - tinted.size.height) / 2),
                from: .zero, operation: .sourceOver, fraction: 1)
    out.unlockFocus()
    out.isTemplate = false
    return out
}

// Recolor a (usually template) symbol image by drawing it and filling
// source-atop — keeps the glyph's alpha, swaps its color.
private func tintedImage(_ image: NSImage, color: NSColor) -> NSImage {
    let out = NSImage(size: image.size)
    out.lockFocus()
    let rect = NSRect(origin: .zero, size: image.size)
    image.draw(in: rect)
    color.set()
    rect.fill(using: .sourceAtop)
    out.unlockFocus()
    return out
}

struct WizardEntry {
    let dir: String     // package dir under the repo root
    let exe: String     // executable / target name
    let blurb: String
    let needsSpec: Bool  // WaveWizard quits without a spec.json — ask for one

    var slug: String { exe.lowercased() }
    func mascot(in repo: URL) -> URL {
        repo.appendingPathComponent("\(dir)/Sources/\(exe)/Assets/\(slug)-mascot.png")
    }
}

// The sibling wizards this daemon can summon. DateWizard is omitted — it lives
// here now (the date is built in).
let siblingWizards = [
    WizardEntry(dir: "clip-wizard", exe: "ClipWizard",
                blurb: "audition, pick, and stitch motion takes into a /pop cut", needsSpec: false),
    WizardEntry(dir: "glyph-wizard", exe: "GlyphWizard",
                blurb: "parametric type authoring from glyph skeletons", needsSpec: false),
    WizardEntry(dir: "juke-wizard", exe: "JukeWizard",
                blurb: "play, rate, and annotate the whole /pop library", needsSpec: false),
    WizardEntry(dir: "shot-wizard", exe: "ShotWizard",
                blurb: "storyboard shots and watch the sequence play through", needsSpec: false),
    WizardEntry(dir: "wave-wizard", exe: "WaveWizard",
                blurb: "record labelled audio samples from a spec", needsSpec: true),
]

// Owns repo discovery + summon/launch + the "Our Wizards" about window, so
// MenuBarDays can stay focused on the calendar strip.
final class WizardRoster: NSObject {
    // Binary lives at <repo>/date-wizard/.build/<config>/DateWizard — walk up to
    // the repo, falling back to ~/aesthetic-computer.
    let repo: URL = {
        var u = URL(fileURLWithPath: CommandLine.arguments[0]).resolvingSymlinksInPath()
        for _ in 0..<4 { u.deleteLastPathComponent() }
        if FileManager.default.fileExists(atPath: u.appendingPathComponent("juke-wizard").path) { return u }
        return URL(fileURLWithPath: NSHomeDirectory() + "/aesthetic-computer")
    }()
    private var aboutWindow: NSWindow?

    func mascotImage(_ w: WizardEntry, side: CGFloat) -> NSImage {
        if let img = NSImage(contentsOf: w.mascot(in: repo)) {
            img.size = NSSize(width: side, height: side)
            return img
        }
        // no mascot yet — our own guy stands in
        let img = wizardGuy(scale: max(1, (side / CGFloat(wizardGrid.count)).rounded()))
        img.size = NSSize(width: side * 13 / 16, height: side)
        return img
    }

    @objc func summon(_ sender: NSMenuItem) {
        guard sender.tag >= 0, sender.tag < siblingWizards.count else { return }
        let w = siblingWizards[sender.tag]
        var arg: String? = nil
        if w.needsSpec {
            let panel = NSOpenPanel()
            panel.title = "Pick a \(w.exe) spec"
            panel.directoryURL = repo.appendingPathComponent("\(w.dir)/samples")
            panel.allowedContentTypes = [.json]
            NSApp.activate(ignoringOtherApps: true)
            guard panel.runModal() == .OK, let url = panel.url else { return }
            arg = url.path
        }
        launch(w, arg: arg)
    }

    func launch(_ w: WizardEntry, arg: String?) {
        let pkg = repo.appendingPathComponent(w.dir).path
        let launcher = "\(pkg)/bin/\(w.slug)"
        let quotedArg = arg.map { " \"\($0)\"" } ?? ""
        // Prefer the wizard's own bin launcher; fall back to a cached build + run.
        let cmd: String
        if FileManager.default.fileExists(atPath: launcher) {
            cmd = "\"\(launcher)\"\(quotedArg)"
        } else {
            cmd = "swift build -c release --package-path \"\(pkg)\" && exec \"\(pkg)/.build/release/\(w.exe)\"\(quotedArg)"
        }
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/bin/sh")
        p.arguments = ["-c", "( \(cmd) ) >/tmp/wizard-\(w.slug).log 2>&1 &"]
        try? p.run()
    }

    // "Our Wizards" — the roster, mascots and all.
    @objc func showAbout() {
        if let win = aboutWindow { win.makeKeyAndOrderFront(nil); NSApp.activate(ignoringOtherApps: true); return }

        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 10
        stack.edgeInsets = NSEdgeInsets(top: 20, left: 24, bottom: 20, right: 24)

        let titleRow = NSStackView()
        titleRow.orientation = .horizontal
        titleRow.alignment = .centerY
        titleRow.spacing = 10
        titleRow.addArrangedSubview(NSImageView(image: wizardGuy(scale: 3)))
        let title = NSTextField(labelWithString: "Our Wizards")
        title.font = .systemFont(ofSize: 20, weight: .bold)
        titleRow.addArrangedSubview(title)
        stack.addArrangedSubview(titleRow)

        let sub = NSTextField(labelWithString: "Small native tools from Aesthetic Inc — one craft each. The date lives here.")
        sub.font = .systemFont(ofSize: 12)
        sub.textColor = .secondaryLabelColor
        stack.addArrangedSubview(sub)

        for w in siblingWizards {
            let row = NSStackView()
            row.orientation = .horizontal
            row.alignment = .centerY
            row.spacing = 12
            let face = NSImageView(image: mascotImage(w, side: 44))
            face.widthAnchor.constraint(equalToConstant: 44).isActive = true
            face.heightAnchor.constraint(equalToConstant: 44).isActive = true
            row.addArrangedSubview(face)
            let col = NSStackView()
            col.orientation = .vertical
            col.alignment = .leading
            col.spacing = 1
            let name = NSTextField(labelWithString: w.exe)
            name.font = .systemFont(ofSize: 14, weight: .semibold)
            let blurb = NSTextField(labelWithString: w.blurb)
            blurb.font = .systemFont(ofSize: 11)
            blurb.textColor = .secondaryLabelColor
            col.addArrangedSubview(name)
            col.addArrangedSubview(blurb)
            row.addArrangedSubview(col)
            stack.addArrangedSubview(row)
        }

        let foot = NSTextField(labelWithString: "aesthetic.computer")
        foot.font = .systemFont(ofSize: 10)
        foot.textColor = .tertiaryLabelColor
        stack.addArrangedSubview(foot)

        let win = NSWindow(contentRect: .zero,
                           styleMask: [.titled, .closable],
                           backing: .buffered, defer: false)
        win.title = "Our Wizards"
        win.contentView = stack
        win.setContentSize(stack.fittingSize)
        win.isReleasedWhenClosed = false
        win.center()
        aboutWindow = win
        win.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
    }
}
