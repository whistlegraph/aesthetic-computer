import Foundation

/// The development Electron bundle is shared by many unrelated apps. Only
/// Easel's own window-title contract opts one of those windows into Slab.
enum EaselWindowIdentity {
    static let bundleIDs = ["computer.aesthetic.easel", "computer.aesthetic.aesel", "com.github.Electron"]

    static func accepts(bundleID: String, title: String) -> Bool {
        if bundleID == "computer.aesthetic.easel" || bundleID == "computer.aesthetic.aesel" { return true }
        guard bundleID == "com.github.Electron" else { return false }
        let normalized = title.lowercased()
        return normalized == "easel" || normalized == "aesel" || normalized.hasSuffix(" · easel") || normalized.hasSuffix(" · aesel")
    }
}
