// WizardWizard — the wizard of wizards. A little wizard guy in the menu
// bar (next to the pals — ⌘-drag to reorder) that lists every Aesthetic
// Inc wizard, launches them on click, and keeps a small "Our Wizards"
// about screen with the mascot roster. Logs land in /tmp/wizardwizard-*.log.
import AppKit
import UniformTypeIdentifiers

// Our own little wizard guy — hand-placed pixels, no emoji. Purple hat,
// gold star, gray beard. Drawn at integer scales so he stays crisp.
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

struct Wizard {
    let dir: String    // package dir under the repo root
    let exe: String    // executable / target name
    let blurb: String
    let needsSpec: Bool // WaveWizard quits without a spec.json — ask for one

    var slug: String { exe.lowercased() }
    func mascot(in repo: URL) -> URL {
        repo.appendingPathComponent("\(dir)/Sources/\(exe)/Assets/\(slug)-mascot.png")
    }
}

let wizards = [
    Wizard(dir: "clip-wizard", exe: "ClipWizard",
           blurb: "audition, pick, and stitch motion takes into a /pop cut", needsSpec: false),
    Wizard(dir: "date-wizard", exe: "DateWizard",
           blurb: "your week, scheduled against AesthetiCal", needsSpec: false),
    Wizard(dir: "glyph-wizard", exe: "GlyphWizard",
           blurb: "parametric type authoring from glyph skeletons", needsSpec: false),
    Wizard(dir: "juke-wizard", exe: "JukeWizard",
           blurb: "play, rate, and annotate the whole /pop library", needsSpec: false),
    Wizard(dir: "shot-wizard", exe: "ShotWizard",
           blurb: "storyboard shots and watch the sequence play through", needsSpec: false),
    Wizard(dir: "wave-wizard", exe: "WaveWizard",
           blurb: "record labelled audio samples from a spec", needsSpec: true),
]

// The binary lives at <repo>/wizard-wizard/.build/release/WizardWizard —
// walk up to the repo, falling back to ~/aesthetic-computer.
func findRepo() -> URL {
    var u = URL(fileURLWithPath: CommandLine.arguments[0]).resolvingSymlinksInPath()
    for _ in 0..<4 { u.deleteLastPathComponent() }
    if FileManager.default.fileExists(atPath: u.appendingPathComponent("juke-wizard").path) { return u }
    return URL(fileURLWithPath: NSHomeDirectory() + "/aesthetic-computer")
}

final class WizardWizardDelegate: NSObject, NSApplicationDelegate {
    let repo = findRepo()
    var statusItem: NSStatusItem!
    var aboutWindow: NSWindow?

    func applicationDidFinishLaunching(_ notification: Notification) {
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        statusItem.button?.image = wizardGuy(scale: 1)
        statusItem.menu = buildMenu()
    }

    func buildMenu() -> NSMenu {
        let menu = NSMenu()
        for (i, w) in wizards.enumerated() {
            let item = NSMenuItem(title: w.exe, action: #selector(summon(_:)), keyEquivalent: "")
            item.target = self
            item.tag = i
            item.toolTip = w.blurb
            item.image = mascotImage(w, side: 18)
            menu.addItem(item)
        }
        menu.addItem(.separator())
        let about = NSMenuItem(title: "About Our Wizards…", action: #selector(showAbout), keyEquivalent: "")
        about.target = self
        menu.addItem(about)
        menu.addItem(.separator())
        let quit = NSMenuItem(title: "Quit WizardWizard", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        menu.addItem(quit)
        return menu
    }

    func mascotImage(_ w: Wizard, side: CGFloat) -> NSImage {
        if let img = NSImage(contentsOf: w.mascot(in: repo)) {
            img.size = NSSize(width: side, height: side)
            return img
        }
        // no mascot yet (GlyphWizard) — our own guy stands in
        let img = wizardGuy(scale: max(1, (side / CGFloat(wizardGrid.count)).rounded()))
        img.size = NSSize(width: side * 13 / 16, height: side)
        return img
    }

    @objc func summon(_ sender: NSMenuItem) {
        let w = wizards[sender.tag]
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

    func launch(_ w: Wizard, arg: String?) {
        let pkg = repo.appendingPathComponent(w.dir).path
        let launcher = "\(pkg)/bin/\(w.slug)"
        let quotedArg = arg.map { " \"\($0)\"" } ?? ""
        // Prefer the wizard's own bin launcher (it knows its flags); fall back
        // to a cached swift build + run for the ones without one.
        let cmd: String
        if FileManager.default.fileExists(atPath: launcher) {
            cmd = "\"\(launcher)\"\(quotedArg)"
        } else {
            cmd = "swift build -c release --package-path \"\(pkg)\" && exec \"\(pkg)/.build/release/\(w.exe)\"\(quotedArg)"
        }
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/bin/sh")
        p.arguments = ["-c", "( \(cmd) ) >/tmp/wizardwizard-\(w.slug).log 2>&1 &"]
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

        let sub = NSTextField(labelWithString: "Small native tools from Aesthetic Inc — one craft each.")
        sub.font = .systemFont(ofSize: 12)
        sub.textColor = .secondaryLabelColor
        stack.addArrangedSubview(sub)

        for w in wizards {
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

let app = NSApplication.shared
let delegate = WizardWizardDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory) // menu bar only, no Dock icon
app.run()
