import Foundation

/// The development Electron bundle is shared by many unrelated apps. Only
/// Easel's own window-title contract opts one of those windows into Slab.
enum EaselWindowIdentity {
    static let bundleIDs = ["computer.aesthetic.easel", "com.github.Electron"]

    static func accepts(bundleID: String, title: String) -> Bool {
        if bundleID == "computer.aesthetic.easel" { return true }
        guard bundleID == "com.github.Electron" else { return false }
        return title == "Easel" || title.hasSuffix(" · Easel")
    }
}
