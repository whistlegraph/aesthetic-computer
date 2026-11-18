// Config.swift — where NCC Circles keeps its settings and its data.
//
// This file ships in a PUBLIC repo, so it carries no club data: group ids,
// member names and the OIDC client id all live in the untracked config at
// ~/.config/ncc-circles/config.json, written on first run.

import Foundation

struct CircleConf: Codable {
    var groupId: String       // signal-cli group id (base64)
    var name: String          // the Signal group's own name
    var title: String         // the short name the rail shows
    var kind: String          // "circle" (a sociocracy circle) or "commons"
    var roleKey: String?      // matches "<roleKey> Circle: Role" in the people ledger
}

struct AuthConf: Codable {
    var required = true
    var issuer = "https://auth.nela.computer/application/o/circles/"
    var clientId = ""         // the public OIDC client the Auth DRI registers
    var redirectPort = 47823  // redirect URI: http://127.0.0.1:47823/callback
    var scopes = "openid profile email offline_access"
}

struct AppConf: Codable {
    var auth = AuthConf()
    var signalCli: String?        // default: Homebrew's signal-cli
    var signalConfigDir: String?  // default: this app's own linked device
    var peopleLedger: String?     // optional people.json (names, colours, roles, badges)
    var assetsDir: String?        // optional club artwork
    var wikiURL: String?          // default: https://wiki.nela.computer
    var wikiStart: String?        // title of the wiki's start page, default "STARTING PAGE"
    var desktopBridge: String?    // optional slab/bin/signal.mjs, for history import
    var match = ["nela computer club", "computer club", "ncc"]
    var circles: [CircleConf] = []
}

enum Paths {
    static let home = FileManager.default.homeDirectoryForCurrentUser
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

    var signalConfigDir: URL {
        if let e = ProcessInfo.processInfo.environment["NCC_SIGNAL_CONFIG"] { return Paths.expand(e) }
        if let p = conf.signalConfigDir { return Paths.expand(p) }
        return Paths.root.appendingPathComponent("signal")
    }

    var signalCli: String? {
        let fm = FileManager.default
        for p in [conf.signalCli, "/opt/homebrew/bin/signal-cli", "/usr/local/bin/signal-cli"] {
            if let p, fm.isExecutableFile(atPath: p) { return p }
        }
        return nil
    }
}
