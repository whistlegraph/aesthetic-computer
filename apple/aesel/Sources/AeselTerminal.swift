#if os(macOS)
import Foundation

/// The direct (Developer ID) build carries the terminal half of Aesel:
/// `Contents/Resources/aesel` plus Node in `Contents/Helpers`. On launch it
/// links `aesel` (this app) and `aes` (the TUI) into ~/.local/bin and installs
/// the Claude/Codex host, once per app version. The sandboxed build has
/// neither folder and does nothing here.
enum AeselTerminal {
    static func install() {
        let bundle = Bundle.main.bundleURL
        let easel = bundle.appendingPathComponent("Contents/Resources/aesel")
        let node = bundle.appendingPathComponent("Contents/Helpers/node")
        let files = FileManager.default
        guard ProcessInfo.processInfo.environment["APP_SANDBOX_CONTAINER_ID"] == nil,
              files.isExecutableFile(atPath: node.path),
              files.fileExists(atPath: easel.appendingPathComponent("bin/aes").path) else { return }
        DispatchQueue.global(qos: .utility).async {
            link(easel: easel)
            installHost(easel: easel, node: node)
        }
    }

    /// Claim a command only when the name is free or already points into an
    /// app bundle. A working checkout's link or anyone's real file stays put.
    private static func link(easel: URL) {
        let files = FileManager.default
        let bin = files.homeDirectoryForCurrentUser.appendingPathComponent(".local/bin")
        try? files.createDirectory(at: bin, withIntermediateDirectories: true)
        for name in ["aesel", "aes", "a"] {
            let path = bin.appendingPathComponent(name).path
            let target = easel.appendingPathComponent("bin/\(name)").path
            if let existing = try? files.destinationOfSymbolicLink(atPath: path) {
                // An app-bundle link from before the rename (Resources/easel) is ours too.
                guard existing != target, existing.contains(".app/Contents/Resources/aesel/")
                        || existing.contains(".app/Contents/Resources/easel/") else { continue }
                try? files.removeItem(atPath: path)
            } else if files.fileExists(atPath: path) {
                continue
            }
            try? files.createSymbolicLink(atPath: path, withDestinationPath: target)
        }
    }

    /// Reinstall the launchd host when this app's version or location changes.
    private static func installHost(easel: URL, node: URL) {
        let files = FileManager.default
        let root = files.homeDirectoryForCurrentUser.appendingPathComponent("Library/Application Support/Aesel Host")
        let stamp = root.appendingPathComponent("installed-by.json")
        let version = Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") as? String ?? "?"
        let mark = ["version": version, "app": Bundle.main.bundleURL.path]
        if let data = try? Data(contentsOf: stamp),
           let previous = try? JSONDecoder().decode([String: String].self, from: data), previous == mark { return }
        let process = Process()
        process.executableURL = node
        process.arguments = [easel.appendingPathComponent("native/install.mjs").path]
        var environment = ProcessInfo.processInfo.environment
        environment["PATH"] = loginPath()
        process.environment = environment
        process.standardOutput = FileHandle.nullDevice
        process.standardError = FileHandle.nullDevice
        guard (try? process.run()) != nil else { return }
        process.waitUntilExit()
        guard process.terminationStatus == 0 else { return }
        try? files.createDirectory(at: root, withIntermediateDirectories: true)
        try? JSONEncoder().encode(mark).write(to: stamp, options: .atomic)
    }

    /// Apps opened from Finder get a bare PATH; the host needs the one the
    /// user's shell sees to find `claude` and `codex`. `printf "%s:" $PATH`
    /// reads the same in fish (a list) and in zsh/bash (one string).
    private static func loginPath() -> String {
        let home = FileManager.default.homeDirectoryForCurrentUser.path
        var directories = ["\(home)/.local/bin", "\(home)/.claude/local", "/opt/homebrew/bin", "/usr/local/bin", "/usr/bin", "/bin"]
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        let process = Process(), pipe = Pipe()
        process.executableURL = URL(fileURLWithPath: shell)
        process.arguments = ["-l", "-c", "printf '%s:' $PATH"]
        process.standardOutput = pipe
        process.standardError = FileHandle.nullDevice
        if (try? process.run()) != nil {
            let data = pipe.fileHandleForReading.readDataToEndOfFile()
            process.waitUntilExit()
            let found = String(decoding: data, as: UTF8.self).split(separator: ":").map(String.init)
            directories = found + directories.filter { !found.contains($0) }
        }
        return directories.joined(separator: ":")
    }
}
#endif
