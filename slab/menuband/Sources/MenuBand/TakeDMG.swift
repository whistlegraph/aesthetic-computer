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
    /// Build `<name>.dmg` next to where `wav` should live (the Desktop). The
    /// WAV is consumed INTO the image (not left loose). Returns the .dmg URL,
    /// or nil on failure (caller can fall back to the bare WAV).
    static func build(wav: URL, name: String, coverIcon: NSImage?) -> URL? {
        let fm = FileManager.default
        let desktop = fm.urls(for: .desktopDirectory, in: .userDomainMask).first
            ?? fm.homeDirectoryForCurrentUser.appendingPathComponent("Desktop")
        let out = uniqueURL(desktop.appendingPathComponent("\(name).dmg"))

        let stage = fm.temporaryDirectory.appendingPathComponent("mbtake-\(UUID().uuidString)")
        let rw = fm.temporaryDirectory.appendingPathComponent("mbtake-rw-\(UUID().uuidString).dmg")
        defer { try? fm.removeItem(at: stage); try? fm.removeItem(at: rw) }

        do {
            try fm.createDirectory(at: stage, withIntermediateDirectories: true)
            try fm.copyItem(at: wav, to: stage.appendingPathComponent(wav.lastPathComponent))
            // .VolumeIcon.icns at the volume root is the fallback the OS reads
            // when the custom-icon bit is set (we set it via NSWorkspace below).
            if let icns = Bundle.appResources.url(forResource: "AppIcon", withExtension: "icns") {
                try? fm.copyItem(at: icns, to: stage.appendingPathComponent(".VolumeIcon.icns"))
            }

            // 1) Read-write image from the staging folder.
            guard run("/usr/bin/hdiutil",
                      ["create", "-srcfolder", stage.path,
                       "-volname", name, "-fs", "HFS+",
                       "-format", "UDRW", "-ov", rw.path]) else { return nil }

            // 2) Mount it, grab the mount point, brand the VOLUME with our logo.
            guard let mount = attach(rw) else { return nil }
            if let logo = appLogo() {
                NSWorkspace.shared.setIcon(logo, forFile: mount, options: [])
            }
            _ = run("/usr/bin/hdiutil", ["detach", mount, "-quiet"])

            // 3) Compress to the final read-only .dmg on the Desktop.
            try? fm.removeItem(at: out)
            guard run("/usr/bin/hdiutil",
                      ["convert", rw.path, "-format", "UDZO", "-o", out.path]) else { return nil }

            // 4) The colorful album art on the .dmg FILE itself (Desktop icon).
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

    private static func appLogo() -> NSImage? {
        if let icns = Bundle.appResources.url(forResource: "AppIcon", withExtension: "icns") {
            return NSImage(contentsOf: icns)
        }
        return NSImage(named: NSImage.applicationIconName)
    }

    /// Attach an image and return its mount point (parses hdiutil's plist-free
    /// tabular output — the last field of the line naming a /Volumes path).
    private static func attach(_ image: URL) -> String? {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/usr/bin/hdiutil")
        p.arguments = ["attach", image.path, "-nobrowse", "-noautoopen"]
        let pipe = Pipe()
        p.standardOutput = pipe
        do { try p.run(); p.waitUntilExit() } catch { return nil }
        guard p.terminationStatus == 0 else { return nil }
        let out = String(data: pipe.fileHandleForReading.readDataToEndOfFile(), encoding: .utf8) ?? ""
        for line in out.split(separator: "\n") {
            if let r = line.range(of: "/Volumes/") {
                return String(line[r.lowerBound...]).trimmingCharacters(in: .whitespaces)
            }
        }
        return nil
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
        let base = url.deletingPathExtension().lastPathComponent
        let dir = url.deletingLastPathComponent()
        var i = 2
        while true {
            let candidate = dir.appendingPathComponent("\(base)-\(i).dmg")
            if !fm.fileExists(atPath: candidate.path) { return candidate }
            i += 1
        }
    }
}
