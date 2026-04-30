import Foundation
import AVFoundation

/// Scans the GarageBand sample library on disk and returns the subset of
/// `.exs` instruments that `AVAudioUnitSampler` can actually load. Many
/// of GB's bundled stubs reference samples from packs that haven't been
/// downloaded — those error out at load time with `-43` (file not found)
/// or `-10868` (format unsupported). We pre-flight every patch through a
/// throwaway sampler and only surface the ones that pass.
///
/// Library is *empty* when GarageBand isn't installed or its Sound
/// Library hasn't been downloaded; callers should treat the GarageBand
/// backend as unavailable in that case (hide the toggle, fall back to GM).
enum GarageBandLibrary {
    struct Patch: Hashable {
        let family: String         // "Church Organ", "iOS Instruments", etc.
        let displayName: String    // file basename minus .exs
        let url: URL
    }

    /// Top-level directories where GarageBand drops sampler patches. The
    /// first path is the system one populated by the Sound Library
    /// downloader; the second is the per-user one (rare for GB but Logic
    /// Pro shares this convention).
    private static let roots: [String] = [
        "/Library/Application Support/GarageBand/Instrument Library/Sampler/Sampler Instruments",
        "\(NSHomeDirectory())/Music/Audio Music Apps/Sampler Instruments",
    ]

    /// Cached scan result. The scan is *expensive* (~4 s for 50 files
    /// across 3 families because each pre-flight load touches disk-backed
    /// sample data) so we do it once at app startup and reuse the result
    /// until the next launch. Users who download additional packs while
    /// the app is running won't see them until restart — acceptable
    /// tradeoff vs. either caching nothing or scanning live every time
    /// the popover opens.
    private(set) static var cache: [Patch] = []

    /// True iff at least one loadable patch was found. Drives whether the
    /// popover offers the GM/GarageBand toggle at all.
    static var isAvailable: Bool { !cache.isEmpty }

    /// Patches grouped by family, families sorted alphabetically, patches
    /// inside each family sorted by display name. Stable order so the
    /// popover list doesn't shuffle between launches.
    static var groupedByFamily: [(family: String, patches: [Patch])] {
        let groups = Dictionary(grouping: cache, by: { $0.family })
        return groups.keys.sorted().map { family in
            let sorted = groups[family]!.sorted { $0.displayName < $1.displayName }
            return (family, sorted)
        }
    }

    /// Scan + pre-flight every `.exs` under `roots`. Fills `cache`.
    /// Called once during `MenuBandController.bootstrap` on a background
    /// queue so we don't block app launch on the load-test pass.
    static func scan() {
        // Disposable engine + sampler: we never play through it, just
        // probe whether each EXS loads. Released as soon as the scan is
        // done so we don't keep a second audio graph alive forever.
        let engine = AVAudioEngine()
        let sampler = AVAudioUnitSampler()
        engine.attach(sampler)
        engine.connect(sampler, to: engine.mainMixerNode, format: nil)
        do { try engine.start() } catch {
            NSLog("MenuBand: GB library scan engine start failed: \(error)")
            return
        }
        defer { engine.stop() }

        var found: [Patch] = []
        for root in roots {
            let rootURL = URL(fileURLWithPath: root)
            guard FileManager.default.fileExists(atPath: rootURL.path) else { continue }
            guard let enumerator = FileManager.default.enumerator(
                at: rootURL,
                includingPropertiesForKeys: [.isRegularFileKey],
                options: [.skipsHiddenFiles]
            ) else { continue }
            for case let url as URL in enumerator {
                guard url.pathExtension.lowercased() == "exs" else { continue }
                do {
                    try sampler.loadInstrument(at: url)
                } catch {
                    continue  // patch isn't loadable; skip silently
                }
                let parent = url.deletingLastPathComponent().lastPathComponent
                // Patches living directly under "Sampler Instruments"
                // get a friendlier family label. Nested folders (Church
                // Organ, iOS Instruments) keep their actual folder name.
                let family = (parent == "Sampler Instruments") ? "Sampler Instruments" : parent
                let name = url.deletingPathExtension().lastPathComponent
                found.append(Patch(family: family, displayName: name, url: url))
            }
        }
        cache = found
        NSLog("MenuBand: GB library scan — \(found.count) loadable patches across \(Set(found.map(\.family)).count) families")
    }
}
