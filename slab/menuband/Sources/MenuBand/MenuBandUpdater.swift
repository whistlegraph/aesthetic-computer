import AppKit

/// "There's a newer Menu Band on main — update?" for fleet builds, asked once
/// per launch. install.sh stamps the commit (`MBGitCommit`) and the checkout's
/// git dir (`MBSourceGitDir`) into Info.plist. A little after launch this
/// fetches origin/main into that repo and lists the commits since the build
/// that touch Menu Band's sources. When there are any, a modal lists them
/// with Update / Later.
///
/// Update never touches the working tree the build came from (it may hold
/// uncommitted work). It checks origin/main out into a sparse worktree under
/// ~/Library/Caches/MenuBand/update-src, which persists so later builds are
/// incremental, and runs that tree's install.sh in its own session. install.sh
/// swaps the bundle atomically and kickstarts the launch agent, which restarts
/// this app on the new build. If the build fails the app is still running, so
/// it says so and offers the log.
///
/// Not in the App Store build, silent for unstamped `swift run` binaries, and
/// silent when the repo is missing (a bundle built on another Mac).
enum MenuBandUpdater {
    /// Everything install.sh and Package.swift read. A commit outside these
    /// cannot change the built app, so it doesn't count as an update.
    /// `live` and `fedac/native/src` are where CSinger's and CGMSynth's
    /// symlinks land; prepareWorktree also follows any it doesn't know yet.
    static let sourcePaths = [
        "slab/menuband", "slab/bin", "slab/macos-audio",
        "juke-wizard", "fedac/native/samples/piano",
        "live", "fedac/native/src",
    ]
    static let launchDelay: TimeInterval = 12
    /// Shown in the modal; the rest are summarised as "and N more".
    static let listedChanges = 8

    static var onChange: (() -> Void)?
    /// Tooltip line while an update builds, or after one fails.
    private(set) static var statusLine: String?

    private static var checked = false
    private static var buildPID: pid_t = 0
    private static var pollTimer: Timer?

    static var sourceGitDir: String? {
        Bundle.main.infoDictionary?["MBSourceGitDir"] as? String
    }
    static var worktree: URL {
        FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent("Library/Caches/MenuBand/update-src")
    }
    static var logURL: URL {
        FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent("Library/Logs/MenuBand/update.log")
    }

    struct Change: Equatable {
        let hash: String
        let subject: String
    }

    /// `git log --format=%h%x09%s` → changes, with the "Menu Band: " prefix
    /// most of these subjects carry dropped (the modal already says it).
    static func parseChanges(_ log: String) -> [Change] {
        log.split(separator: "\n").compactMap { line in
            let parts = line.split(separator: "\t", maxSplits: 1)
            guard parts.count == 2 else { return nil }
            var subject = String(parts[1]).trimmingCharacters(in: .whitespaces)
            for prefix in ["Menu Band: ", "menuband: ", "MenuBand: "] where subject.hasPrefix(prefix) {
                subject = String(subject.dropFirst(prefix.count))
            }
            return Change(hash: String(parts[0]), subject: subject)
        }
    }

    static func checkOnLaunch() {
        #if MAC_APP_STORE
        return
        #else
        guard !checked, let built = RevisionChecker.buildCommit, let gitDir = sourceGitDir,
              FileManager.default.fileExists(atPath: gitDir) else { return }
        checked = true
        DispatchQueue.global(qos: .utility).asyncAfter(deadline: .now() + launchDelay) {
            let fetch = git(["--git-dir", gitDir, "fetch", "--quiet", "origin", "main"], timeout: 90)
            if fetch.status != 0 {
                NSLog("MenuBand update: fetch failed (\(fetch.status)): \(fetch.output.prefix(200))")
            }
            // A bundle built from a commit this repo has never seen (built
            // elsewhere, or from since-discarded work) has no range to diff.
            guard git(["--git-dir", gitDir, "cat-file", "-e", "\(built)^{commit}"]).status == 0 else {
                NSLog("MenuBand update: build \(built.prefix(10)) not in \(gitDir); skipping")
                return
            }
            let target = git(["--git-dir", gitDir, "rev-parse", "origin/main"]).output
                .trimmingCharacters(in: .whitespacesAndNewlines)
            let log = git(["--git-dir", gitDir, "log", "--no-merges", "--format=%h%x09%s",
                           "\(built)..\(target)", "--"] + sourcePaths)
            let changes = parseChanges(log.output)
            NSLog("MenuBand update: build \(built.prefix(10)) vs origin/main \(target.prefix(10)) → \(changes.count) Menu Band change(s)")
            guard log.status == 0, !target.isEmpty, !changes.isEmpty else { return }
            DispatchQueue.main.async { prompt(changes: changes, target: target, gitDir: gitDir) }
        }
        #endif
    }

    private static func prompt(changes: [Change], target: String, gitDir: String) {
        let alert = NSAlert()
        alert.messageText = "Menu Band update"
        var lines = changes.prefix(listedChanges).map { "• \($0.subject)" }
        if changes.count > listedChanges {
            lines.append("…and \(changes.count - listedChanges) more")
        }
        let count = "\(changes.count) change\(changes.count == 1 ? "" : "s")"
        alert.informativeText = "\(count) on main since this build:\n\n"
            + lines.joined(separator: "\n")
            + "\n\nUpdating builds it here (a few minutes the first time), then Menu Band restarts."
        alert.alertStyle = .informational
        alert.addButton(withTitle: "Update")
        alert.addButton(withTitle: "Later")
        NSApp.activate(ignoringOtherApps: true)
        guard alert.runModal() == .alertFirstButtonReturn else { return }
        start(target: target, gitDir: gitDir)
    }

    private static func start(target: String, gitDir: String) {
        guard buildPID == 0 else { return }
        setStatus("updating to \(target.prefix(10)) — building…")
        DispatchQueue.global(qos: .utility).async {
            if let problem = prepareWorktree(target: target, gitDir: gitDir) {
                DispatchQueue.main.async { fail(problem) }
                return
            }
            let pid = spawnInstall()
            DispatchQueue.main.async {
                guard pid > 0 else { fail("couldn't start install.sh"); return }
                buildPID = pid
                NSLog("MenuBand update: install.sh running as pid \(pid), log \(logURL.path)")
                pollTimer = Timer.scheduledTimer(withTimeInterval: 2, repeats: true) { _ in poll() }
            }
        }
    }

    /// Check origin/main out into the update worktree, creating it (sparse,
    /// detached) on first use. Returns a problem description, or nil.
    private static func prepareWorktree(target: String, gitDir: String) -> String? {
        let wt = worktree.path
        if !FileManager.default.fileExists(atPath: worktree.appendingPathComponent(".git").path) {
            try? FileManager.default.removeItem(at: worktree)
            try? FileManager.default.createDirectory(at: worktree.deletingLastPathComponent(),
                                                     withIntermediateDirectories: true)
            _ = git(["--git-dir", gitDir, "worktree", "prune"])
            let add = git(["--git-dir", gitDir, "worktree", "add", "--no-checkout", "--detach", wt, target])
            if add.status != 0 { return "worktree add failed: \(add.output.prefix(300))" }
        }
        var paths = sourcePaths
        // A symlink that leaves the sparse set checks out dangling, and the
        // build then misses its sources. Widen the set until none do.
        for _ in 0..<4 {
            let sparse = git(["-C", wt, "sparse-checkout", "set", "--cone"] + paths)
            if sparse.status != 0 { return "sparse-checkout failed: \(sparse.output.prefix(300))" }
            let checkout = git(["-C", wt, "checkout", "--detach", "--force", target], timeout: 300)
            if checkout.status != 0 { return "checkout failed: \(checkout.output.prefix(300))" }
            let missing = danglingTargetDirs(root: worktree, under: paths).subtracting(paths)
            if missing.isEmpty { return nil }
            NSLog("MenuBand update: widening checkout for symlinks into \(missing.sorted())")
            paths += missing.sorted()
        }
        return "symlinks still point outside the checkout"
    }

    /// Repo-relative directories that dangling symlinks under `paths` point
    /// into (skipping .build).
    static func danglingTargetDirs(root: URL, under paths: [String]) -> Set<String> {
        let fm = FileManager.default
        let rootPath = root.resolvingSymlinksInPath().path
        var dirs = Set<String>()
        for path in paths {
            let base = root.appendingPathComponent(path)
            guard let walk = fm.enumerator(at: base, includingPropertiesForKeys: [.isSymbolicLinkKey]) else { continue }
            for case let url as URL in walk {
                if url.lastPathComponent == ".build" { walk.skipDescendants(); continue }
                guard (try? url.resourceValues(forKeys: [.isSymbolicLinkKey]))?.isSymbolicLink == true,
                      !fm.fileExists(atPath: url.path),
                      let dest = try? fm.destinationOfSymbolicLink(atPath: url.path) else { continue }
                let resolved = URL(fileURLWithPath: dest, relativeTo: url.deletingLastPathComponent().resolvingSymlinksInPath())
                    .standardizedFileURL.deletingLastPathComponent().path
                guard resolved.hasPrefix(rootPath + "/") else { continue }
                dirs.insert(String(resolved.dropFirst(rootPath.count + 1)))
            }
        }
        return dirs
    }

    /// install.sh in its own session, so launchd's kill of this app's process
    /// group during the kickstart doesn't take the installer down with it.
    private static func spawnInstall() -> pid_t {
        try? FileManager.default.createDirectory(at: logURL.deletingLastPathComponent(),
                                                 withIntermediateDirectories: true)
        let dir = worktree.appendingPathComponent("slab/menuband").path
        let script = "cd \"$1\" && exec ./install.sh"

        var env = ProcessInfo.processInfo.environment
        // The real toolchain first: ~/.local/bin wrappers can defer builds.
        env["PATH"] = "/usr/bin:/bin:/usr/sbin:/sbin:/opt/homebrew/bin"
        // No identity in the keychain → still install, ad-hoc signed.
        env["MENUBAND_ADHOC_FALLBACK"] = "1"
        env["MENUBAND_UPDATE"] = "1"

        var attr: posix_spawnattr_t?
        posix_spawnattr_init(&attr)
        posix_spawnattr_setflags(&attr, Int16(POSIX_SPAWN_SETSID))
        var actions: posix_spawn_file_actions_t?
        posix_spawn_file_actions_init(&actions)
        posix_spawn_file_actions_addopen(&actions, 0, "/dev/null", O_RDONLY, 0)
        posix_spawn_file_actions_addopen(&actions, 1, logURL.path, O_WRONLY | O_CREAT | O_TRUNC, 0o644)
        posix_spawn_file_actions_adddup2(&actions, 1, 2)
        defer {
            posix_spawnattr_destroy(&attr)
            posix_spawn_file_actions_destroy(&actions)
        }

        let args = ["/bin/bash", "-c", script, "menuband-update", dir]
        let argv = args.map { strdup($0) } + [nil]
        let envp = env.map { strdup("\($0.key)=\($0.value)") } + [nil]
        defer {
            argv.forEach { free($0) }
            envp.forEach { free($0) }
        }
        var pid: pid_t = 0
        guard posix_spawn(&pid, "/bin/bash", &actions, &attr, argv, envp) == 0 else { return 0 }
        return pid
    }

    /// Still here once install.sh exits → it failed (success restarts us).
    private static func poll() {
        var status: Int32 = 0
        let r = waitpid(buildPID, &status, WNOHANG)
        guard r != 0 else { return }
        pollTimer?.invalidate()
        pollTimer = nil
        buildPID = 0
        let code = (status & 0x7f) == 0 ? (status >> 8) & 0xff : 128 + (status & 0x7f)
        if r < 0 || code == 0 {
            // Exit 0 without a restart: the launch agent isn't loaded.
            setStatus(nil)
            NSLog("MenuBand update: install.sh finished (\(code)); relaunch to run the new build")
            return
        }
        fail("install.sh exited \(code)")
    }

    private static func fail(_ problem: String) {
        NSLog("MenuBand update: failed — \(problem)")
        setStatus("update failed — see \(logURL.path)")
        let alert = NSAlert()
        alert.messageText = "Menu Band update failed"
        alert.informativeText = "\(problem). This build keeps running."
        alert.alertStyle = .warning
        alert.addButton(withTitle: "Show Log")
        alert.addButton(withTitle: "OK")
        NSApp.activate(ignoringOtherApps: true)
        if alert.runModal() == .alertFirstButtonReturn {
            NSWorkspace.shared.open(logURL)
        }
    }

    private static func setStatus(_ line: String?) {
        statusLine = line
        onChange?()
    }

    @discardableResult
    private static func git(_ args: [String], timeout: TimeInterval = 30) -> (status: Int32, output: String) {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/usr/bin/git")
        p.arguments = args
        var env = ProcessInfo.processInfo.environment
        env["GIT_TERMINAL_PROMPT"] = "0"
        env["GIT_SSH_COMMAND"] = "ssh -o BatchMode=yes -o ConnectTimeout=10"
        p.environment = env
        let pipe = Pipe()
        p.standardOutput = pipe
        p.standardError = pipe
        p.standardInput = FileHandle.nullDevice
        do { try p.run() } catch { return (-1, "\(error)") }
        let killer = DispatchWorkItem { if p.isRunning { p.terminate() } }
        DispatchQueue.global().asyncAfter(deadline: .now() + timeout, execute: killer)
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        p.waitUntilExit()
        killer.cancel()
        return (p.terminationStatus, String(data: data, encoding: .utf8) ?? "")
    }
}
