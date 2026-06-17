// MacPal — entry point. Parses the launch profile, builds a PalConfig, attaches
// that profile's plugins, and runs.
//
//   (no args)                              → the star, for Fía (+ affirmations)
//   --profile fuser --name panda \
//       --corner TL [--repo ~/Developer/fuser]
//                                          → the fuser machine badge
//
// Optional: --to <recipient> (affirmations key, default "fia"),
//           --host <base url> (affirmations endpoint, default aesthetic.computer),
//           --about (open the About panel on launch, for QA).
//
// The avatar/plugin machinery lives in PalCore.swift; the two feature sets live
// in AffirmationsPlugin.swift and FuserPlugin.swift. All compile together via
// `swiftc Sources/*.swift` (see build.sh).

import AppKit
import ServiceManagement

setbuf(stdout, nil)

// ── arg parsing ─────────────────────────────────────────────────────────────
let argv = CommandLine.arguments
func argValue(_ flag: String) -> String? {
    guard let i = argv.firstIndex(of: flag), i + 1 < argv.count else { return nil }
    return argv[i + 1]
}
let profile = argValue("--profile") ?? "star"
let recipient = argValue("--to") ?? "fia"
let host = argValue("--host") ?? "https://aesthetic.computer"
// 3D avatar: `--model3d` turns the pal into a SceneKit model (procedural berry
// placeholder when no path/file is given); `--model3d <path/to.obj>` loads a
// specific mesh. The blueberry machine gets it automatically.
let want3D = argv.contains("--model3d")
let model3DArg = argValue("--model3d")

let starSupport = NSString(string: "~/Library/Application Support/MacPal").expandingTildeInPath
let fuserHome = NSString(string: "~/.local/share/desktop-badge").expandingTildeInPath
// Menu Band's "now playing" signal dir — the existing cross-app protocol, the
// same path for both profiles so the two ship independently and just work.
let noteDir = fuserHome

// ── build the config + plugins for the chosen profile ────────────────────────
var config: PalConfig
var plugins: [PalPlugin] = []

if profile == "fuser" {
    let name = argValue("--name") ?? "fuser"
    let corner = (argValue("--corner") ?? "TL").uppercased()
    let repo = argValue("--repo") ?? NSString(string: "~/Developer/fuser").expandingTildeInPath
    let nameToEmoji = ["panda": "🐼", "chicken": "🐔", "neo": "🦋", "blueberry": "🫐"]
    config = PalConfig(
        profile: "fuser",
        palName: name,
        palEmoji: nameToEmoji[name.lowercased()] ?? "🖥️",
        supportDir: fuserHome,
        noteSignalDir: noteDir,
        factoryCorner: corner,
        fixedCorner: corner,
        marginX: 16, marginY: 16,    // the badge's historical resting gap
        draggable: false,
        colorTogglable: false,
        registersLoginItem: false,
        showsAbout: false,
        aboutDedication: nil,
        posePaths: { _ in
            [fuserHome + "/glyph.svg", fuserHome + "/glyph-2.svg", fuserHome + "/glyph-3.svg"]
        },
        singPath: { _ in fuserHome + "/glyph-sing.svg" }
    )
    plugins = [FuserPlugin(home: fuserHome, repo: repo)]
} else {
    // The star, for Fía. Glyph art ships in the .app's Resources (gold/silver).
    func base(_ color: String) -> String { color == "silver" ? "star-silver" : "star-glyph" }
    config = PalConfig(
        profile: "star",
        palName: "star",
        palEmoji: "⭐",
        supportDir: starSupport,
        noteSignalDir: noteDir,
        factoryCorner: "BR",
        fixedCorner: nil,
        marginX: 4, marginY: 14,
        draggable: true,
        colorTogglable: true,
        registersLoginItem: true,
        showsAbout: true,
        aboutDedication: "This MacPal was made for Fía and is maintained by @jeffrey",
        posePaths: { color in
            [base(color), base(color) + "-2", base(color) + "-3"].compactMap { resource($0, "svg") }
        },
        singPath: { color in resource(base(color) + "-sing", "svg") }
    )
    plugins = [AffirmationsPlugin(recipient: recipient, host: host, supportDir: starSupport)]
}

// 3D avatar opt-in, per machine: a fuser machine named <m> uses a SceneKit
// avatar when <m>-3d/<m>.usdc has been staged (installer packs it). Explicit
// --model3d forces it. So blueberry + neo (+ any future rigged machine) light
// up automatically once their model lands; others stay on the flat glyph.
if profile == "fuser" {
    let m = (argValue("--name") ?? "").lowercased()
    let modelPath = model3DArg ?? (fuserHome + "/\(m)-3d/\(m).usdc")
    if want3D || FileManager.default.fileExists(atPath: modelPath) {
        config.model3D = true
        config.model3DPath = modelPath
        config.avatarTogglable = true   // right-click → 3D ⇄ 2D
        // 2D mode shows the model's flat thumbnail (no SVG glyph for these).
        config.posePaths = { _ in [fuserHome + "/\(m)-3d/\(m)-thumb.png"] }
        config.singPath = { _ in nil }
    }
}

// Dev: `MacPal --profile fuser --name blueberry --snapshot out.png` renders the
// 3D model offscreen to a PNG (mid-animation) and exits — for QA without the
// tiny corner widget.
if let snapPath = argValue("--snapshot") {
    let t = Double(argValue("--snap-time") ?? "1.5") ?? 1.5
    if let img = Pal3DView.snapshot(objPath: config.model3DPath,
                                    size: CGSize(width: 480, height: 480), time: t),
       let tiff = img.tiffRepresentation, let bm = NSBitmapImageRep(data: tiff),
       let png = bm.representation(using: .png, properties: [:]) {
        try? png.write(to: URL(fileURLWithPath: snapPath))
        print("✓ snapshot → \(snapPath)")
    } else {
        print("✗ snapshot failed")
    }
    exit(0)
}

// ── app bootstrap ─────────────────────────────────────────────────────────
let app = NSApplication.shared
app.setActivationPolicy(.accessory)   // no Dock icon, no menu bar item

// First-run (star only): add MacPal to Login Items so the pal is always there.
// SMAppService reflects real system state, so we only ask once — if she later
// removes it in System Settings, we don't fight her.
func registerLoginItemOnce() {
    let key = "MacPal.didRegisterLoginItem"
    guard !UserDefaults.standard.bool(forKey: key) else { return }
    if #available(macOS 13.0, *) {
        do { try SMAppService.mainApp.register() } catch {
            print("MacPal: login-item register skipped — \(error)")
        }
    }
    UserDefaults.standard.set(true, forKey: key)
}
if config.registersLoginItem { registerLoginItemOnce() }

let pal = PalController(config: config)
pal.plugins = plugins
pal.build()

if config.showsAbout, CommandLine.arguments.contains("--about") {
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) { AboutWindow.show(config: config) }
}
app.run()
