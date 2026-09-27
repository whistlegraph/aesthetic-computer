import Foundation

/// The development Electron bundle is shared by many unrelated apps. Only
/// aesel's own window-title contract opts one of those windows into Slab.
enum AeselWindowIdentity {
    static let bundleIDs = ["computer.aesthetic.easel", "computer.aesthetic.aesel", "computer.aesthetic.aesel.native", "com.github.Electron"]

    static func accepts(bundleID: String, title: String) -> Bool {
        if bundleIDs.contains(bundleID), bundleID != "com.github.Electron" { return true }
        guard bundleID == "com.github.Electron" else { return false }
        return title == "aesel" || title.hasSuffix(" · aesel")
    }
}
