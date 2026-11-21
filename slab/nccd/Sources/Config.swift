// Config.swift — where NCCd keeps its settings and its data.
//
// This file ships in a PUBLIC repo, so it carries no club data: group ids and
// member names live in the untracked config at ~/.config/ncc-circles/config.json,
// written on first run. The folder and config names keep the app's first name,
// "NCC Circles".

import Foundation

struct CircleConf: Codable {
    var groupId: String       // signal-cli group id (base64)
    var name: String          // the Signal group's own name
    var title: String         // the short name the rail shows
    var kind: String          // "circle" (a sociocracy circle) or "commons"
    var roleKey: String?      // matches "<roleKey> Circle: Role" in the people ledger
}

struct AppConf: Codable {
    var signalCli: String?        // default: Homebrew's signal-cli
    var engine: String?           // "lib": the Signal engine inside the app (engine/), not signal-cli
    var signalConfigDir: String?  // default: this app's own linked device
    var peopleLedger: String?     // optional people.json (names, colours, roles, badges)
    var assetsDir: String?        // optional club artwork
    var wikiURL: String?          // default: https://wiki.nela.computer
    var forgeURL: String?         // the club's code (Forgejo), default: https://git.nela.computer
    var wikiStart: String?        // title of the wiki's start page, default "STARTING PAGE"
    var desktopBridge: String?    // optional slab/bin/signal.mjs, for history import
    var match = ["nela computer club", "computer club", "ncc"]
    var circles: [CircleConf] = []
}

enum Paths {
    static let home = FileManager.default.homeDirectoryForCurrentUser
    // The name of this copy in the Keychain. A trial copy (trial.sh) has its own
    // bundle identifier, so its keys and its wiki session are apart from NCCd's.
    static let id = Bundle.main.bundleIdentifier ?? "computer.aesthetic.ncc-circles"
    static let appName = Bundle.main.infoDictionary?["CFBundleDisplayName"] as? String ?? "NCCd"
    static var root: URL {
        if let h = ProcessInfo.processInfo.environment["NCC_CIRCLES_HOME"] { return URL(fileURLWithPath: h) }
        return home.appendingPathComponent("Library/Application Support/NCC Circles")
    }
    static var config: URL {
        if let h = ProcessInfo.processInfo.environment["NCC_CIRCLES_CONFIG"] { return URL(fileURLWithPath: h) }
        return home.appendingPathComponent(".config/ncc-circles/config.json")
    }
    static var log: URL { root.appendingPathComponent("log") }
    static func expand(_ p: String) -> URL { URL(fileURLWithPath: (p as NSString).expandingTildeInPath) }
}

final class Config {
    static var shared = Config()
    var conf = AppConf()

    init() {
        let fm = FileManager.default
        try? fm.createDirectory(at: Paths.log, withIntermediateDirectories: true)
        // The chats and their pictures are for this account only.
        for dir in [Paths.root, Paths.log, Paths.root.appendingPathComponent("media")] {
            try? fm.setAttributes([.posixPermissions: 0o700], ofItemAtPath: dir.path)
        }
        if let d = try? Data(contentsOf: Paths.config),
           let c = try? JSONDecoder().decode(AppConf.self, from: d) {
            conf = c
        } else {
            let vault = Paths.home.appendingPathComponent("aesthetic-computer-vault/nela-computer-club")
            if fm.fileExists(atPath: vault.path) {
                conf.peopleLedger = "~/aesthetic-computer-vault/nela-computer-club/people/people.json"
                conf.assetsDir = "~/aesthetic-computer-vault/nela-computer-club/style/assets"
            }
            let bridge = Paths.home.appendingPathComponent("aesthetic-computer/slab/bin/signal.mjs")
            if fm.fileExists(atPath: bridge.path) { conf.desktopBridge = "~/aesthetic-computer/slab/bin/signal.mjs" }
            save()
        }
    }

    func save() {
        let enc = JSONEncoder()
        enc.outputFormatting = [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]
        try? FileManager.default.createDirectory(at: Paths.config.deletingLastPathComponent(),
                                                 withIntermediateDirectories: true)
        if let d = try? enc.encode(conf) {
            try? d.write(to: Paths.config, options: .atomic)
            try? FileManager.default.setAttributes([.posixPermissions: 0o600], ofItemAtPath: Paths.config.path)
        }
    }

    // The Signal engine inside the app (engine/, on libsignal), in place of signal-cli.
    // It is in a build only when the library was there to link (build.sh).
    var usesLibrary: Bool {
        #if ENGINE_LIB
        return conf.engine == "lib" || ProcessInfo.processInfo.environment["NCC_ENGINE"] == "lib"
        #else
        return false
        #endif
    }

    var signalConfigDir: URL {
        if usesLibrary { return Paths.root.appendingPathComponent("engine") }
        if let e = ProcessInfo.processInfo.environment["NCC_SIGNAL_CONFIG"] { return Paths.expand(e) }
        if let p = conf.signalConfigDir { return Paths.expand(p) }
        return Paths.root.appendingPathComponent("signal")
    }

    // The Signal Desktop bridge: the config's own, else the copy in the bundle.
    var bridge: String? {
        if let p = conf.desktopBridge {
            let path = Paths.expand(p).path
            if FileManager.default.fileExists(atPath: path) { return path }
        }
        return Bundle.main.path(forResource: "signal", ofType: "mjs")
    }

    // Homebrew, when this Mac has it: the app uses it to install and update its Signal engine.
    static var brew: String? {
        ["/opt/homebrew/bin/brew", "/usr/local/bin/brew"].first { FileManager.default.isExecutableFile(atPath: $0) }
    }
    var hideEngine = ProcessInfo.processInfo.environment["NCC_NO_SIGNAL_CLI"] != nil     // test aid: as if signal-cli were not installed

    var signalCli: String? {
        if usesLibrary { return "library" }      // nothing to install: the engine is in the app
        if hideEngine { return nil }
        let fm = FileManager.default
        for p in [conf.signalCli, "/opt/homebrew/bin/signal-cli", "/usr/local/bin/signal-cli"] {
            if let p, fm.isExecutableFile(atPath: p) { return p }
        }
        return nil
    }
}
