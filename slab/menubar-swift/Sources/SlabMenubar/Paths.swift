import Foundation

enum Paths {
    static let home = ProcessInfo.processInfo.environment["HOME"] ?? NSHomeDirectory()

    static var slabHome: String {
        ProcessInfo.processInfo.environment["SLAB_HOME"] ?? "\(home)/.local/share/slab"
    }

    static var slabBin: String {
        ProcessInfo.processInfo.environment["SLAB_BIN"] ?? "\(home)/.local/bin"
    }

    static var activePromptsDir: String { "\(slabHome)/state/active-prompts" }
    static var awaitingPromptsDir: String { "\(slabHome)/state/awaiting-prompts" }
    static var activeSubagentsDir: String { "\(slabHome)/state/active-subagents" }
    static var soundsDir: String { "\(slabHome)/sounds" }
    static var lidLog: String { "\(slabHome)/logs/lidalive.log" }

    static var mailDir: String {
        ProcessInfo.processInfo.environment["AC_MAIL_MAILDIR"] ?? "\(home)/.mail-all"
    }
    static var mailSyncLog: String { "\(mailDir)/sync.log" }

    static var daemonPlist: String { "\(home)/Library/LaunchAgents/computer.slab.daemon.plist" }
    static var menubarPlist: String { "\(home)/Library/LaunchAgents/computer.slab.menubar.plist" }
    static var claudeSleep: String { "\(slabBin)/claude-sleep" }

    static var passphraseSocket: String { "\(home)/.ac-daemon.sock" }

    static var ambientFlag: String { "/tmp/slab-ambient-active" }
    /// Persistent mute flag. When this file exists, claude-stop.sh skips
    /// chimes and stops ambient instead of starting it. Toggled from the
    /// menubar's "Mute ambient sonification" item.
    static var muteFlag: String { "\(slabHome)/state/muted" }
    /// When this file exists, restored / restarted Claude windows are
    /// auto-tiled across the main display in a grid sized by the window
    /// count, with the Terminal font scaled so no cell is too cramped.
    static var autoTileFlag: String { "\(slabHome)/state/auto-tile" }
    /// When this file exists, the tile font is shrunk well below the
    /// "comfortable from typical distance" size — for sitting close to the
    /// screen and wanting more content per pane.
    static var nearTextFlag: String { "\(slabHome)/state/tile-near" }
    /// When this file exists, each Terminal window matching a live Claude
    /// session is re-themed by status (working/awaiting), so a wall of
    /// terminals reads as a status display at a glance.
    static var themeByStatusFlag: String { "\(slabHome)/state/theme-by-status" }
}

enum Tools {
    static let candidates: [String: [String]] = [
        "tailscale": [
            "/opt/homebrew/bin/tailscale",
            "/usr/local/bin/tailscale",
        ],
        "mbsync":    ["/opt/homebrew/bin/mbsync", "/usr/local/bin/mbsync"],
        "mu":        ["/opt/homebrew/bin/mu", "/usr/local/bin/mu"],
    ]

    static func resolve(_ name: String) -> String? {
        for path in candidates[name] ?? [] {
            if FileManager.default.isExecutableFile(atPath: path) { return path }
        }
        let pathEnv = ProcessInfo.processInfo.environment["PATH"] ?? ""
        for dir in pathEnv.split(separator: ":") {
            let candidate = "\(dir)/\(name)"
            if FileManager.default.isExecutableFile(atPath: candidate) { return candidate }
        }
        return nil
    }
}
