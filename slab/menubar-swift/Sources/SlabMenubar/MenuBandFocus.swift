import AppKit

/// Menu Band plays its instrument on the modifier keys: ⌘⌥ held bare is a
/// chord hand, not a request for Slab's pane chooser. While Menu Band is the
/// frontmost app (direct or App Store build, or its launcher) Slab's ⌘⌥ hold
/// hint stands down for that hold. Performance focus is signalled separately
/// through the distributed notification the AppDelegate already observes.
enum MenuBandFocus {
    static let bundlePrefixes = [
        "computer.aestheticcomputer.menuband",
        "computer.aesthetic.menuband",
    ]

    static func owns(bundleIdentifier: String?) -> Bool {
        guard let id = bundleIdentifier else { return false }
        return bundlePrefixes.contains { id.hasPrefix($0) }
    }

    static var isFrontmost: Bool {
        owns(bundleIdentifier: NSWorkspace.shared.frontmostApplication?.bundleIdentifier)
    }
}
