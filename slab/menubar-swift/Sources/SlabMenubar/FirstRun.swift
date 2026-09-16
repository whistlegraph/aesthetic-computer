// THE FIRST LAUNCH OF A DOWNLOADED SLAB.
//
// Dragging an app to Applications installs the app. It does not install Slab.
// The menubar on its own can draw nothing: the rocks, the window colours and
// the window titles are all derived from marker files, and the markers only
// exist because Claude Code was told to write them. That telling lives in
// `~/.claude/settings.json`, which the repo's `install.sh` merges with `jq`.
//
// A downloaded Slab has no repo and probably no `jq`, so this does the merge
// in Swift and points every hook at THIS bundle's own binary (see HookCLI).
// It runs once, behind a dialog, and never touches anything it did not put
// there.
//
// It refuses to act at all when the repo install is present — a `~/.local/bin`
// full of symlinks back into a checkout is somebody's live working setup, and
// silently rewriting their hooks to point at a downloaded copy would be a
// small disaster the next time they edited a script and nothing happened.

import AppKit
import Foundation

enum FirstRun {
    private static let hookEvents: [(event: String, key: String, matcher: String?)] = [
        ("prompt", "UserPromptSubmit", nil),
        ("pre", "PreToolUse", nil),
        ("post", "PostToolUse", nil),
        ("stop", "Stop", nil),
        ("subagent-stop", "SubagentStop", nil),
        ("notify", "Notification", nil),
    ]

    private static var claudeSettings: String { "\(Paths.home)/.claude/settings.json" }
    private static var receipt: String { "\(Paths.home)/.config/slab/solo-install.json" }

    /// Called once from `applicationDidFinishLaunching`. Cheap and silent in
    /// every case except the one it exists for.
    static func offerIfNeeded() {
        guard needsSetup() else { return }
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.5) { prompt() }
    }

    // MARK: - Deciding

    private static func needsSetup() -> Bool {
        // Already ran, or the user declined and we wrote that down.
        if FileManager.default.fileExists(atPath: receipt) { return false }
        // A repo install owns this machine — leave it completely alone.
        if repoInstallPresent() { return false }
        // Hooks already point somewhere sensible.
        if hooksInstalled() { return false }
        return true
    }

    /// The repo install symlinks its scripts out of a checkout. A symlink at
    /// the canonical hook path is the clearest signal that this Mac is a
    /// development machine rather than somebody's download.
    private static func repoInstallPresent() -> Bool {
        let fm = FileManager.default
        let marker = "\(Paths.home)/.local/bin/claude-prompt-log.sh"
        guard let attrs = try? fm.attributesOfItem(atPath: marker) else { return false }
        return (attrs[.type] as? FileAttributeType) == .typeSymbolicLink
    }

    private static func hooksInstalled() -> Bool {
        guard let settings = readSettings(),
              let hooks = settings["hooks"] as? [String: Any]
        else { return false }
        return hooks["UserPromptSubmit"] != nil
    }

    // MARK: - Asking

    private static func prompt() {
        let alert = NSAlert()
        alert.messageText = "Finish setting up Slab?"
        alert.informativeText = """
            Slab watches your Claude Code sessions so it can give each one a \
            colour, a name and a stone. To do that it needs to add its hooks to \
            your Claude Code settings.

            This edits ~/.claude/settings.json and adds nothing else. Your \
            existing settings are kept, and a backup is written beside the file. \
            You can undo it later from the Slab menu.
            """
        alert.alertStyle = .informational
        alert.addButton(withTitle: "Set Up Slab")
        alert.addButton(withTitle: "Not Now")

        // Terminal profiles are a separate, more invasive ask: they write into
        // Terminal.app's own preferences, so they are opt-in rather than
        // bundled into the yes.
        let seedToggle = NSButton(checkboxWithTitle: "Also install Slab's Terminal themes", target: nil, action: nil)
        seedToggle.state = .on
        alert.accessoryView = seedToggle

        NSApp.activate(ignoringOtherApps: true)
        let response = alert.runModal()
        guard response == .alertFirstButtonReturn else {
            writeReceipt(installed: false, seeded: false)
            return
        }

        let installed = installHooks()
        var seeded = false
        if seedToggle.state == .on { seeded = seedTerminalProfiles() }
        writeReceipt(installed: installed, seeded: seeded)
        report(installed: installed, seeded: seeded)
    }

    private static func report(installed: Bool, seeded: Bool) {
        let done = NSAlert()
        if installed {
            done.messageText = "Slab is set up."
            done.informativeText = seeded
                ? "Start a Claude Code session in Terminal and its window will take on a colour and grow a stone. Press ⌘⌥T to tile your sessions."
                : "Start a Claude Code session in Terminal and a stone will appear in its corner. Press ⌘⌥T to tile your sessions. Terminal themes were skipped."
            done.alertStyle = .informational
        } else {
            done.messageText = "Slab could not finish setting up."
            done.informativeText = "Its hooks could not be written to ~/.claude/settings.json. Slab will keep running, but sessions will not be tracked."
            done.alertStyle = .warning
        }
        done.addButton(withTitle: "OK")
        done.runModal()
    }

    // MARK: - Doing

    /// Merge our hook entries into the user's settings, preserving everything
    /// already there. A backup is written first, because this is somebody
    /// else's configuration file and we are a guest in it.
    @discardableResult
    static func installHooks() -> Bool {
        let binary = Bundle.main.executablePath ?? ""
        guard !binary.isEmpty else { return false }

        var settings = readSettings() ?? [:]
        if FileManager.default.fileExists(atPath: claudeSettings) {
            let backup = "\(claudeSettings).before-slab"
            if !FileManager.default.fileExists(atPath: backup) {
                try? FileManager.default.copyItem(atPath: claudeSettings, toPath: backup)
            }
        }

        var hooks = (settings["hooks"] as? [String: Any]) ?? [:]
        for spec in hookEvents {
            var entry: [String: Any] = [
                "hooks": [[
                    "type": "command",
                    "command": "\"\(binary)\" hook \(spec.event)",
                    "async": true,
                ]]
            ]
            if let matcher = spec.matcher { entry["matcher"] = matcher }

            // Append beside whatever the user already has for this event
            // rather than replacing it, but never add ourselves twice.
            var existing = (hooks[spec.key] as? [[String: Any]]) ?? []
            existing.removeAll { group in
                guard let inner = group["hooks"] as? [[String: Any]] else { return false }
                return inner.contains { ($0["command"] as? String)?.contains("hook \(spec.event)") == true }
            }
            existing.append(entry)
            hooks[spec.key] = existing
        }
        settings["hooks"] = hooks

        guard let data = try? JSONSerialization.data(
            withJSONObject: settings, options: [.prettyPrinted, .sortedKeys])
        else { return false }
        try? FileManager.default.createDirectory(
            atPath: "\(Paths.home)/.claude", withIntermediateDirectories: true)
        do {
            try data.write(to: URL(fileURLWithPath: claudeSettings), options: .atomic)
            return true
        } catch {
            return false
        }
    }

    /// Remove only the hook entries whose command points at this binary,
    /// leaving every other hook the user has exactly as it was.
    @discardableResult
    static func removeHooks() -> Bool {
        guard var settings = readSettings(),
              var hooks = settings["hooks"] as? [String: Any]
        else { return false }

        for spec in hookEvents {
            guard var existing = hooks[spec.key] as? [[String: Any]] else { continue }
            existing.removeAll { group in
                guard let inner = group["hooks"] as? [[String: Any]] else { return false }
                return inner.contains { ($0["command"] as? String)?.contains("hook \(spec.event)") == true }
            }
            if existing.isEmpty { hooks.removeValue(forKey: spec.key) }
            else { hooks[spec.key] = existing }
        }
        if hooks.isEmpty { settings.removeValue(forKey: "hooks") }
        else { settings["hooks"] = hooks }

        guard let data = try? JSONSerialization.data(
            withJSONObject: settings, options: [.prettyPrinted, .sortedKeys])
        else { return false }
        try? data.write(to: URL(fileURLWithPath: claudeSettings), options: .atomic)
        try? FileManager.default.removeItem(atPath: receipt)
        return true
    }

    /// Run the bundled `slab-seed-terminal` against the bundled seed. The
    /// script resolves its seed relative to its own parent directory, which is
    /// why the payload keeps the repo's `bin/` + `seed/` shape inside
    /// Resources rather than being flattened.
    private static func seedTerminalProfiles() -> Bool {
        guard let solo = Bundle.main.resourceURL?.appendingPathComponent("solo") else { return false }
        let script = solo.appendingPathComponent("bin/slab-seed-terminal").path
        guard FileManager.default.fileExists(atPath: script) else { return false }

        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/bin/bash")
        task.arguments = [script]
        task.standardOutput = FileHandle.nullDevice
        task.standardError = FileHandle.nullDevice
        do {
            try task.run()
            task.waitUntilExit()
            return task.terminationStatus == 0
        } catch {
            return false
        }
    }

    // MARK: - Receipt

    private static func writeReceipt(installed: Bool, seeded: Bool) {
        try? FileManager.default.createDirectory(
            atPath: "\(Paths.home)/.config/slab", withIntermediateDirectories: true)
        let body: [String: Any] = [
            "installed_hooks": installed,
            "seeded_terminal": seeded,
            "bundle": Bundle.main.bundlePath,
            "at": ISO8601DateFormatter().string(from: Date()),
        ]
        guard let data = try? JSONSerialization.data(withJSONObject: body, options: .prettyPrinted)
        else { return }
        try? data.write(to: URL(fileURLWithPath: receipt), options: .atomic)
    }

    private static func readSettings() -> [String: Any]? {
        guard let data = FileManager.default.contents(atPath: claudeSettings),
              let parsed = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else { return nil }
        return parsed
    }
}
