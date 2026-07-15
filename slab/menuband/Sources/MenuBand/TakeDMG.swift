import AppKit

/// Wraps a finished take into its own little **DMG "record release"**: a
/// compressed disk image whose VOLUME icon is our Menu Band logo, containing
/// the take's WAV (which already wears its generative album-art icon). The
/// .dmg FILE on the Desktop wears the same colorful album art so a folder of
/// takes reads as a shelf of records; mounting one shows the branded logo.
///
/// All via `hdiutil` (always present) + `NSWorkspace.setIcon` for the custom
/// icon bit — no Xcode command-line tools, so it works on any tester's Mac.
enum TakeDMG {
    /// Build `<name>.dmg` on the Desktop containing the WAV plus any `extras`
    /// (e.g. the sidecar `.mid`). The source files are copied INTO the image.
    /// Returns the .dmg URL, or nil on failure (caller can fall back).
    static func build(wav: URL, extras: [URL] = [], name: String, coverIcon: NSImage?) -> URL? {
        let fm = FileManager.default
        let desktop = fm.urls(for: .desktopDirectory, in: .userDomainMask).first
            ?? fm.homeDirectoryForCurrentUser.appendingPathComponent("Desktop")
        let out = uniqueURL(desktop.appendingPathComponent("\(name).dmg"))

        let stage = fm.temporaryDirectory.appendingPathComponent("mbtake-\(UUID().uuidString)")
        defer { try? fm.removeItem(at: stage) }

        do {
            try fm.createDirectory(at: stage, withIntermediateDirectories: true)
            let stagedWav = stage.appendingPathComponent(wav.lastPathComponent)
            try fm.copyItem(at: wav, to: stagedWav)
            var staged = [stagedWav]
            for extra in extras {
                let dst = stage.appendingPathComponent(extra.lastPathComponent)
                if (try? fm.copyItem(at: extra, to: dst)) != nil { staged.append(dst) }
            }
            // Stamp the album art onto the STAGED files (not the /tmp source).
            // eject() stamps the source, but build() copies it immediately after
            // — racing the async icon write, so the copy catches a truncated
            // resource fork and the inner file shows the generic icon. Stamping
            // here (no concurrent copy) writes the full fork, which hdiutil then
            // preserves into the image.
            if let cover = coverIcon {
                for f in staged { NSWorkspace.shared.setIcon(cover, forFile: f.path, options: []) }
            }
            // Bundle our logo as the volume-root icon. (Shows as the volume icon
            // on systems that honor .VolumeIcon.icns; harmless otherwise.)
            if let icns = Bundle.appResources.url(forResource: "AppIcon", withExtension: "icns") {
                try? fm.copyItem(at: icns, to: stage.appendingPathComponent(".VolumeIcon.icns"))
            }

            // ONE step: compressed read-only image straight from the folder.
            // No mount → no stray Desktop volume, and ~1 hdiutil call instead of
            // four (create RW → attach → detach → convert), so it's quick.
            try? fm.removeItem(at: out)
            guard run("/usr/bin/hdiutil",
                      ["create", "-srcfolder", stage.path, "-volname", name,
                       "-fs", "HFS+", "-format", "UDZO", "-ov", "-quiet", out.path])
            else { return nil }

            // The album-art cover on the .dmg FILE itself (what shows on the
            // Desktop / in Messages).
            if let cover = coverIcon {
                NSWorkspace.shared.setIcon(cover, forFile: out.path, options: [])
            }
            NSLog("MenuBand TakeDMG: \(out.path)")
            return out
        } catch {
            NSLog("MenuBand TakeDMG failed: \(error)")
            return nil
        }
    }

    /// Instant alternative to the DMG: a `.mbtape` — a stored (uncompressed)
    /// zip of the take's files, written in milliseconds (no filesystem image,
    /// no mount). Drags into Messages the same way; just not mountable. The
    /// album-art cover goes on the .mbtape file.
    @discardableResult
    static func buildMbtape(files: [URL], name: String, coverIcon: NSImage?) -> URL? {
        let fm = FileManager.default
        let desktop = fm.urls(for: .desktopDirectory, in: .userDomainMask).first
            ?? fm.homeDirectoryForCurrentUser.appendingPathComponent("Desktop")
        let out = uniqueURL(desktop.appendingPathComponent("\(name).mbtape"))
        try? fm.removeItem(at: out)
        // zip -0 store (audio won't compress anyway), -j flat, -q quiet.
        guard run("/usr/bin/zip", ["-0", "-j", "-q", out.path] + files.map { $0.path }) else { return nil }
        if let cover = coverIcon {
            NSWorkspace.shared.setIcon(cover, forFile: out.path, options: [])
        }
        NSLog("MenuBand mbtape: \(out.path)")
        return out
    }

    @discardableResult
    private static func run(_ tool: String, _ args: [String]) -> Bool {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: tool)
        p.arguments = args
        p.standardOutput = FileHandle.nullDevice
        p.standardError = FileHandle.nullDevice
        do { try p.run(); p.waitUntilExit() } catch { return false }
        return p.terminationStatus == 0
    }

    private static func uniqueURL(_ url: URL) -> URL {
        let fm = FileManager.default
        guard fm.fileExists(atPath: url.path) else { return url }
        let ext = url.pathExtension
        let base = url.deletingPathExtension().lastPathComponent
        let dir = url.deletingLastPathComponent()
        var i = 2
        while true {
            let candidate = dir.appendingPathComponent("\(base)-\(i).\(ext)")
            if !fm.fileExists(atPath: candidate.path) { return candidate }
            i += 1
        }
    }
}
