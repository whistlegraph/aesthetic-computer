import Foundation

enum Paths {
    static let home = ProcessInfo.processInfo.environment["HOME"] ?? NSHomeDirectory()

    static var slabHome: String {
        ProcessInfo.processInfo.environment["SLAB_HOME"] ?? "\(home)/.local/share/slab"
    }

    static var slabBin: String {
        ProcessInfo.processInfo.environment["SLAB_BIN"] ?? "\(home)/.local/bin"
    }

    /// The aesthetic-computer checkout — used to launch repo tooling
    /// (e.g. the RFA voice-recording wizard) from the menubar.
    static var acRepo: String {
        ProcessInfo.processInfo.environment["AC_REPO"] ?? "\(home)/aesthetic-computer"
    }

    static var slabWallpaper: String { "\(slabBin)/slab-wallpaper" }
    static var wallpaperStatusDir: String { "\(slabHome)/wallpaper/status" }
    /// Cached near-black + status-glow PNGs set as the macOS desktop
    /// picture (aggregate Claude status, matching the menubar icon).
    static var desktopWallpaperDir: String { "\(slabHome)/wallpaper/desktop" }
    /// The user's pre-slab desktop picture path, captured once before slab
    /// ever overwrites it, so it can be restored when theme-by-status is
    /// off or no sessions are live.
    static var desktopOriginalFile: String { "\(desktopWallpaperDir)/.original" }

    /// Generic iMessage bridge (contact lives in the untracked config below,
    /// never in tracked code). Mirrors the slab-wallpaper wrapper convention.
    static var imsgHelper: String { "\(slabBin)/imsg" }
    static var imsgConfig: String { "\(home)/.config/slab/imsg.json" }

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

    /// "Start Call" recorder + state file. The shell wrapper at
    /// slab/bin/slab-call-record owns the ffmpeg subprocess and writes the
    /// state file; the menubar only reads it. meetingsCli is the node
    /// pipeline that ingests the WAV into meetings/<slug>/.
    static var slabCallRecord: String { "\(slabBin)/slab-call-record" }
    static var meetingRecordingState: String { "\(home)/.ac-meeting-recording.json" }
    static var meetingsCli: String { "\(acRepo)/meetings/cli.mjs" }
    static var meetingsDir: String { "\(acRepo)/meetings" }

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

    /// When this file exists, status themes use the *light* (bright,
    /// sunlight-readable) palettes regardless of the macOS Auto-appearance
    /// schedule — so the wall stays readable outdoors even after the system
    /// has flipped to Dark for the evening.
    static var forceBrightFlag: String { "\(slabHome)/state/force-bright" }
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
