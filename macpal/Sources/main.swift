// MacPal — entry point. Parses the launch profile, builds a PalConfig, attaches
// that profile's plugins, and runs.
//
//   (no args)                              → the star, for Fía (+ affirmations)
//   --profile fuser --name panda \
//       --corner TL [--repo ~/Developer/fuser]
//                                          → the fuser machine badge
//
// Optional: --to <recipient> (status/affirmations channel — default "fia" for
//           the star, the machine's --name for fuser badges),
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
// Machine-addressed Loopboy observer mode. The value resolves through the
// canonical fleet registry; it is never interpreted as a hard-coded host list.
let loopboyMachine = argValue("--loopboy")
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
    // --draggable frees the fuser badge to drag-and-snap between corners (the
    // star already does). Without it the badge stays pinned to --corner. Opt-in
    // per machine so the build minis can keep a fixed corner if they want one.
    let draggable = argv.contains("--draggable")
    config = PalConfig(
        profile: "fuser",
        palName: name,
        palEmoji: nameToEmoji[name.lowercased()] ?? "🖥️",
        supportDir: fuserHome,
        noteSignalDir: noteDir,
        factoryCorner: corner,
        fixedCorner: draggable ? nil : corner,  // nil → reads/persists the saved corner
        marginX: 4, marginY: 4,      // hug the screen corner
        draggable: draggable,
        colorTogglable: false,
        registersLoginItem: false,
        showsAbout: true,
        aboutDedication: "This MacPal looks after \(name) and is maintained by @jeffrey",
        // Poses pushed over the wire (ArtPlugin → <home>/art) win when present,
        // so a machine badge can be restyled live just like Fía's star:
        //   node macpal/art.mjs face.svg --to neo
        posePaths: { _ in
            let artDir = fuserHome + "/art"
            if let files = try? FileManager.default.contentsOfDirectory(atPath: artDir) {
                let idle = files.filter { $0.hasSuffix(".svg") && $0 != "sing.svg" }.sorted()
                if !idle.isEmpty { return idle.map { artDir + "/" + $0 } }
            }
            return [fuserHome + "/glyph.svg", fuserHome + "/glyph-2.svg", fuserHome + "/glyph-3.svg"]
        },
        singPath: { _ in
            let wired = fuserHome + "/art/sing.svg"
            if FileManager.default.fileExists(atPath: wired) { return wired }
            return fuserHome + "/glyph-sing.svg"
        }
    )
    plugins = [
        FuserPlugin(home: fuserHome, repo: repo, minimal: argv.contains("--minimal")),
        // Remote status line — the same wire as the star's affirmations, but
        // each machine polls its own channel (default: its name), so
        //   node macpal/affirm.mjs "baking the kernel" --to neo
        // captions the neo badge without touching blueberry's.
        AffirmationsPlugin(recipient: argValue("--to") ?? name.lowercased(),
                           host: host, supportDir: fuserHome),
        // Glyph art over the wire + device round-trip, same channel:
        //   node macpal/art.mjs --get --from device --to neo
        ArtPlugin(recipient: argValue("--to") ?? name.lowercased(),
                  host: host, supportDir: fuserHome),
    ]
} else {
    // The star, for Fía. Glyph art ships in the .app's Resources (gold/silver),
    // but poses pushed over the wire (ArtPlugin → ~/…/MacPal/art) win when
    // present, so @jeffrey can restyle the star live. Clearing the wire art
    // (art.mjs --clear) empties that dir and the bundle poses resume.
    func base(_ color: String) -> String { color == "silver" ? "star-silver" : "star-glyph" }
    let artDir = starSupport + "/art"
    func wireIdlePoses() -> [String]? {
        guard let files = try? FileManager.default.contentsOfDirectory(atPath: artDir) else { return nil }
        let idle = files.filter { $0.hasSuffix(".svg") && $0 != "sing.svg" }.sorted()
        return idle.isEmpty ? nil : idle.map { artDir + "/" + $0 }
    }
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
            if let wire = wireIdlePoses() { return wire }
            return [base(color), base(color) + "-2", base(color) + "-3"].compactMap { resource($0, "svg") }
        },
        singPath: { color in
            let p = artDir + "/sing.svg"
            if FileManager.default.fileExists(atPath: p) { return p }
            return resource(base(color) + "-sing", "svg")
        }
    )
    plugins = [
        AffirmationsPlugin(recipient: recipient, host: host, supportDir: starSupport),
        // Live glyph art over the wire — downloads pushed poses, hot-swaps them,
        // and reports what it renders back for the device round-trip.
        ArtPlugin(recipient: recipient, host: host, supportDir: starSupport),
        // Fía's own manifestations, fetched live and cycled one per hour in a
        // bubble beside the star in mini mode.
        ManifestationsPlugin(recipient: recipient, host: host, supportDir: starSupport),
    ]
}

// Every release profile carries the same signed TrackDrum helper. Keep its
// action first in the right-click menu, and omit it automatically from dev
// bundles where the nested app is absent.
plugins.insert(TrackDrumPlugin(), at: 0)

// The machine heartbeat is an explicit mode, not an ordinary Macpal feature.
// Its plugin is absent unless --loopboy <registered-machine> was requested.
if let machine = loopboyMachine {
    plugins.append(LoopboyHeartbeatPlugin(target: machine))
}

// Every pal — star and machine badge alike — carries the OTA self-updater,
// so new builds ship from `node macpal/release.mjs` instead of over iMessage.
plugins.append(UpdatePlugin())

// 3D avatars are deprecated for now — the per-machine auto-detect
// (<m>-3d/<m>.usdc) and the right-click 3D ⇄ 2D toggle are parked; every pal
// renders its flat SVG glyph. An explicit --model3d still works for dev QA.
if profile == "fuser", want3D {
    let m = (argValue("--name") ?? "").lowercased()
    config.model3D = true
    config.model3DPath = model3DArg ?? (fuserHome + "/\(m)-3d/\(m).usdc")
    if !FileManager.default.fileExists(atPath: fuserHome + "/glyph.svg") {
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

// Headless contract smoke test used by CI/local verification. It exercises the
// Swift renderer model without opening a Macpal window.
if argv.contains("--test-loopboy-heartbeat") {
    let now = Date().timeIntervalSince1970
    let active = LoopboyHeartbeatContract.aggregate(known: true, online: true, now: now,
        machineUpdated: now, rocks: [["status": "working", "updated": now * 1000]], mission: nil)
    let unknown = LoopboyHeartbeatContract.aggregate(known: false, online: nil, now: now,
        machineUpdated: 0, rocks: [], mission: nil)
    let bounded = LoopboyHeartbeatContract.aggregate(known: true, online: true, now: now,
        machineUpdated: now, rocks: [], mission: ["bounded": true, "progress": 0.4, "updatedAt": now])
    precondition(active.state == .active)
    precondition(unknown.state == .unknown)
    precondition(bounded.boundedProgress == 0.4)
    precondition(loopboyMachine == nil, "ordinary Macpal test must not enable Loopboy mode")
    print("Loopboy heartbeat contract OK")
    exit(0)
}

// ── app bootstrap ─────────────────────────────────────────────────────────
// Release smoke test: the exact menu label and its nested executable must both
// be present in the assembled MacPal.app.
if argv.contains("--test-trackdrum") {
    let trackDrum = TrackDrumPlugin()
    precondition(TrackDrumPlugin.menuTitle == "TrackDrum")
    precondition(trackDrum.appURL != nil)
    precondition(plugins.first is TrackDrumPlugin,
                 "TrackDrum must lead every MacPal profile menu")
    print("TrackDrum menu contract OK")
    exit(0)
}

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

// Display focus flash: fuser machines pulse a soft accent bloom around the
// screen edge when the Deskflow cursor crosses onto them (and on same-machine
// display hops). The minis' desktop-badge already does this; this brings the
// same glow to the MacPal machines (neo, blueberry). Held alive for the app's
// lifetime. Touch <supportDir>/noglow to disable per machine.
var glow: GlowController?
if config.profile == "fuser" {
    glow = GlowController(home: config.supportDir)
}

// Neo and Blueberry can trade the Deskflow server role by touching their
// physical trackpads. Machines without the installed claim helper simply skip
// this feature. Keep the controller alive beside the glow for the app lifetime.
#if !MAC_APP_STORE
var deskflowHandoff: DeskflowHandoff?
if config.profile == "fuser" {
    deskflowHandoff = DeskflowHandoff()
    deskflowHandoff?.onControlAcquired = { glow?.controlAcquired() }
}
#endif

if config.showsAbout, CommandLine.arguments.contains("--about") {
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) { AboutWindow.show(config: config) }
}
app.run()
