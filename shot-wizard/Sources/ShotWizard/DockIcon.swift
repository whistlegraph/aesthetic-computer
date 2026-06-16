// DockIcon.swift — give the bare SPM executable a real dock identity:
// the wizard mascot in a squircle, with light/dark renders swapped live
// when the system theme changes. (Copied from ClipWizard.)
import AppKit

enum DockIcon {
    static func install(prefix: String) {
        apply(prefix: prefix)
        DistributedNotificationCenter.default().addObserver(
            forName: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil, queue: .main
        ) { _ in apply(prefix: prefix) }
    }

    private static func apply(prefix: String) {
        let dark = NSApp.effectiveAppearance
            .bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let variant = dark ? "dark" : "light"
        let bundle = Bundle.module
        let img = bundle.url(forResource: "\(prefix)-icon-\(variant)",
                             withExtension: "png", subdirectory: "Assets")
            .flatMap { NSImage(contentsOf: $0) }
            ?? bundle.url(forResource: "\(prefix)-icon-\(variant)", withExtension: "png")
                .flatMap { NSImage(contentsOf: $0) }
        if let img { NSApp.applicationIconImage = img }
    }
}
