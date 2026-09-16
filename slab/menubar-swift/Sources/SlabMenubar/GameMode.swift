import AppKit

/// Game Mode — one switch that turns this Mac into a clean GeForce NOW client.
///
/// GFN refuses to stream when it decides you're on a VPN, and it does not
/// inspect routing: a live `utun` carrying a 100.64/10 address, plus a DNS
/// resolver that isn't the gateway, is enough. Tailscale trips both tells
/// even with no exit node set, where not one streamed packet enters the
/// tunnel. Separately, AirDrop's `awdl0` radio hops the Wi-Fi card
/// off-channel every few seconds; measured on blueberry 2026-09-02 that is
/// the difference between avg 18 ms / max 80 ms and avg 3.5 / max 18 on the
/// gateway hop.
///
/// Both faults are network-adjacent and neither is the other's cause, so the
/// mode does both at once and undoes both on the way out:
///
///   on   →  `tailscale down`  +  awdl0 pinned down  +  AirDrop undiscoverable
///   off  →  `tailscale up`    +  awdl0 released     +  AirDrop restored
///
/// Nothing else about the machine changes, and the tailnet comes back with
/// its peers intact — `down`/`up` is Tailscale's own pause, not a logout.
///
/// Privilege: `ifconfig awdl0 down` is root-only, and macOS re-raises awdl0
/// whenever Continuity is touched, so the holding is done by a small root
/// LaunchDaemon that watches a flag file this app owns. That costs exactly
/// one admin prompt, ever; every toggle afterwards is a file write. The
/// tailnet half needs no privilege at all, which is what lets auto-detect
/// engage unattended without a dialog ever appearing behind a running game.
final class GameMode {
    static let shared = GameMode()

    static let helperLabel = "computer.aesthetic.slab.gamemode"
    static var helperPlist: String { "/Library/LaunchDaemons/\(helperLabel).plist" }
    /// `toolchain/macos/gfn.sh on` installs its own unconditional awdl-down
    /// daemon. Two holders would fight over the same interface, so installing
    /// ours evicts that one.
    static let legacyLabel = "computer.aesthetic.gfn-awdl"

    static let gfnBundleID = "com.nvidia.gfnpc.mall"

    /// True when this mode engaged itself because GeForce NOW launched. Only
    /// an automatic engage may auto-disengage: a hold you switched on by hand
    /// outlives the app that happened to be open.
    private var engagedAutomatically = false
    private var watching = false

    // ── state ────────────────────────────────────────────────────────────

    /// Read entirely from flag files and `getifaddrs` — no forks, so
    /// `StateSnapshot.gather()` can call it every tick for free.
    static func state() -> GameModeState {
        var s = GameModeState()
        let fm = FileManager.default
        s.on = fm.fileExists(atPath: Paths.gameModeFlag)
        s.autoDetect = fm.fileExists(atPath: Paths.gameModeAutoFlag)
        s.helperInstalled = fm.fileExists(atPath: helperPlist)
        let (awdlUp, tunnelVisible) = interfaces()
        s.awdlUp = awdlUp
        s.tailnetVisible = tunnelVisible
        s.gfnRunning = !NSRunningApplication
            .runningApplications(withBundleIdentifier: gfnBundleID).isEmpty
        return s
    }

    /// (awdl0 is up, a utun carries a 100.64/10 address). The second value is
    /// precisely what GFN's VPN heuristic keys on, so the menu reports the
    /// same fact the streamer is reading rather than a proxy for it.
    private static func interfaces() -> (awdlUp: Bool, tunnelVisible: Bool) {
        var head: UnsafeMutablePointer<ifaddrs>?
        guard getifaddrs(&head) == 0, let first = head else { return (false, false) }
        defer { freeifaddrs(head) }

        var awdlUp = false
        var tunnel = false
        for ptr in sequence(first: first, next: { $0.pointee.ifa_next }) {
            let name = String(cString: ptr.pointee.ifa_name)
            if name == "awdl0", ptr.pointee.ifa_flags & UInt32(IFF_UP) != 0 { awdlUp = true }
            guard name.hasPrefix("utun"),
                  let sa = ptr.pointee.ifa_addr,
                  sa.pointee.sa_family == UInt8(AF_INET) else { continue }
            var addr = sockaddr_in()
            memcpy(&addr, sa, MemoryLayout<sockaddr_in>.size)
            let ip = UInt32(bigEndian: addr.sin_addr.s_addr)
            if ip & 0xFFC0_0000 == 0x6440_0000 { tunnel = true }   // 100.64.0.0/10
        }
        return (awdlUp, tunnel)
    }

    // ── the switch ───────────────────────────────────────────────────────

    func toggle() {
        if FileManager.default.fileExists(atPath: Paths.gameModeFlag) {
            disengage()
        } else {
            engage(automatic: false)
        }
    }

    /// Engage. `automatic` engages only what can be done silently, so a game
    /// launching never raises an auth dialog over the stream; the awdl half
    /// simply stays off until the helper is installed from the menu.
    func engage(automatic: Bool) {
        engagedAutomatically = automatic
        try? FileManager.default.createDirectory(
            atPath: (Paths.gameModeFlag as NSString).deletingLastPathComponent,
            withIntermediateDirectories: true)
        FileManager.default.createFile(atPath: Paths.gameModeFlag, contents: nil)

        stashAirDropMode()
        setAirDrop("Off")
        tailscale("down")
        // The helper polls, but a direct nudge drops the radio now rather
        // than up to one poll interval into the first match.
        if FileManager.default.fileExists(atPath: GameMode.helperPlist) { pokeHelper() }
    }

    func disengage() {
        engagedAutomatically = false
        try? FileManager.default.removeItem(atPath: Paths.gameModeFlag)
        tailscale("up")
        setAirDrop(stashedAirDropMode() ?? "Contacts Only")
        try? FileManager.default.removeItem(atPath: Paths.airDropStash)
        if FileManager.default.fileExists(atPath: GameMode.helperPlist) { pokeHelper() }
    }

    private func tailscale(_ verb: String) {
        guard let ts = Tools.resolve("tailscale") else { return }
        DispatchQueue.global(qos: .userInitiated).async {
            ShellRunner.run(ts, args: [verb], timeout: 20)
        }
    }

    /// The helper wakes on its own poll; this only shortens the wait by
    /// touching the flag's directory so a watching `stat` sees it sooner.
    private func pokeHelper() {
        try? FileManager.default.setAttributes(
            [.modificationDate: Date()],
            ofItemAtPath: (Paths.gameModeFlag as NSString).deletingLastPathComponent)
    }

    // AirDrop discoverability is saved and put back rather than forced to
    // "Everyone", because most Macs sit on "Contacts Only" and silently
    // widening that is not this feature's business.
    private func stashAirDropMode() {
        guard !FileManager.default.fileExists(atPath: Paths.airDropStash) else { return }
        let current = ShellRunner.output(
            "/usr/bin/defaults",
            args: ["read", "com.apple.sharingd", "DiscoverableMode"], timeout: 5
        )?.trimmingCharacters(in: .whitespacesAndNewlines)
        guard let current, !current.isEmpty, current != "Off" else { return }
        try? current.write(toFile: Paths.airDropStash, atomically: true, encoding: .utf8)
    }

    private func stashedAirDropMode() -> String? {
        guard let raw = try? String(contentsOfFile: Paths.airDropStash, encoding: .utf8) else { return nil }
        let value = raw.trimmingCharacters(in: .whitespacesAndNewlines)
        return value.isEmpty ? nil : value
    }

    private func setAirDrop(_ mode: String) {
        DispatchQueue.global(qos: .utility).async {
            ShellRunner.run("/usr/bin/defaults",
                            args: ["write", "com.apple.sharingd", "DiscoverableMode", "-string", mode],
                            timeout: 5)
            ShellRunner.run("/usr/bin/killall", args: ["sharingd"], timeout: 5)
        }
    }

    // ── auto-detect ──────────────────────────────────────────────────────

    /// Watch for GeForce NOW coming and going. Opt-in: with the preference
    /// off this observes and does nothing, so the notifications stay cheap
    /// and the wiring is identical either way.
    func startWatchingGFN() {
        guard !watching else { return }
        watching = true
        let center = NSWorkspace.shared.notificationCenter
        center.addObserver(self, selector: #selector(appLaunched(_:)),
                           name: NSWorkspace.didLaunchApplicationNotification, object: nil)
        center.addObserver(self, selector: #selector(appTerminated(_:)),
                           name: NSWorkspace.didTerminateApplicationNotification, object: nil)

        // Slab restarts far more often than a game session lasts; adopt a GFN
        // that is already up rather than waiting for the next launch.
        let s = GameMode.state()
        if s.autoDetect, s.gfnRunning, !s.on { engage(automatic: true) }
    }

    @objc private func appLaunched(_ note: Notification) {
        guard bundleID(note) == GameMode.gfnBundleID else { return }
        let s = GameMode.state()
        guard s.autoDetect, !s.on else { return }
        engage(automatic: true)
    }

    @objc private func appTerminated(_ note: Notification) {
        guard bundleID(note) == GameMode.gfnBundleID else { return }
        guard engagedAutomatically else { return }
        disengage()
    }

    private func bundleID(_ note: Notification) -> String? {
        (note.userInfo?[NSWorkspace.applicationUserInfoKey] as? NSRunningApplication)?
            .bundleIdentifier
    }

    func setAutoDetect(_ enabled: Bool) {
        if enabled {
            try? FileManager.default.createDirectory(
                atPath: (Paths.gameModeAutoFlag as NSString).deletingLastPathComponent,
                withIntermediateDirectories: true)
            FileManager.default.createFile(atPath: Paths.gameModeAutoFlag, contents: nil)
            // Turning it on while the game is already open should act now,
            // not at the next launch.
            let s = GameMode.state()
            if s.gfnRunning, !s.on { engage(automatic: true) }
        } else {
            try? FileManager.default.removeItem(atPath: Paths.gameModeAutoFlag)
        }
    }

    // ── the root helper ──────────────────────────────────────────────────

    /// The daemon body. Holding awdl0 down has to be a loop rather than a
    /// one-shot because macOS re-raises the interface whenever anything
    /// touches Continuity; releasing is edge-triggered so the interface is
    /// brought back exactly once, not re-upped every poll against a user who
    /// turned AirDrop off for their own reasons.
    private static func helperScript(flag: String) -> String {
        """
        held=0
        while true; do
          if [ -f '\(flag)' ]; then
            /sbin/ifconfig awdl0 down 2>/dev/null
            held=1
          elif [ "$held" = 1 ]; then
            /sbin/ifconfig awdl0 up 2>/dev/null
            held=0
          fi
          sleep 5
        done
        """
    }

    private static func helperPlistXML(flag: String) -> String {
        """
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0"><dict>
          <key>Label</key><string>\(helperLabel)</string>
          <key>ProgramArguments</key><array>
            <string>/bin/bash</string><string>-c</string>
            <string>\(helperScript(flag: flag).replacingOccurrences(of: "&", with: "&amp;")
                      .replacingOccurrences(of: "<", with: "&lt;")
                      .replacingOccurrences(of: ">", with: "&gt;"))</string>
          </array>
          <key>RunAtLoad</key><true/>
          <key>KeepAlive</key><true/>
        </dict></plist>
        """
    }

    /// One admin prompt, once. Returns an error string on failure, nil on
    /// success. Must run on the main thread — NSAppleScript puts up the
    /// authorization dialog itself.
    @discardableResult
    func installHelper() -> String? {
        let staged = NSTemporaryDirectory() + "\(GameMode.helperLabel).plist"
        do {
            try GameMode.helperPlistXML(flag: Paths.gameModeFlag)
                .write(toFile: staged, atomically: true, encoding: .utf8)
        } catch {
            return "could not stage the helper: \(error.localizedDescription)"
        }
        // Deliberately quote-free so it survives the AppleScript string layer
        // intact; every path here is a fixed location with no spaces.
        let cmd = [
            "/bin/cp \(staged) \(GameMode.helperPlist)",
            "/usr/sbin/chown root:wheel \(GameMode.helperPlist)",
            "/bin/chmod 644 \(GameMode.helperPlist)",
            "/bin/launchctl bootout system/\(GameMode.legacyLabel) 2>/dev/null",
            "/bin/rm -f /Library/LaunchDaemons/\(GameMode.legacyLabel).plist",
            "/bin/launchctl bootout system/\(GameMode.helperLabel) 2>/dev/null",
            "/bin/launchctl bootstrap system \(GameMode.helperPlist)",
        ].joined(separator: "; ")
        return GameMode.runAsAdmin(
            cmd, prompt: "Slab Game Mode needs to install a small helper so it can hold the AirDrop radio down while you stream.")
    }

    @discardableResult
    func removeHelper() -> String? {
        let cmd = [
            "/bin/launchctl bootout system/\(GameMode.helperLabel) 2>/dev/null",
            "/bin/rm -f \(GameMode.helperPlist)",
            "/sbin/ifconfig awdl0 up 2>/dev/null",
        ].joined(separator: "; ")
        return GameMode.runAsAdmin(cmd, prompt: "Slab Game Mode is removing its radio helper.")
    }

    private static func runAsAdmin(_ command: String, prompt: String) -> String? {
        let escaped = command
            .replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "\"", with: "\\\"")
        let source = "do shell script \"\(escaped)\" with administrator privileges with prompt \"\(prompt)\""
        var err: NSDictionary?
        NSAppleScript(source: source)?.executeAndReturnError(&err)
        guard let err else { return nil }
        // -128 is the user clicking Cancel on the auth dialog: a decision,
        // not a failure worth reporting back as one.
        if (err[NSAppleScript.errorNumber] as? Int) == -128 { return nil }
        return (err[NSAppleScript.errorMessage] as? String) ?? "authorization failed"
    }
}
