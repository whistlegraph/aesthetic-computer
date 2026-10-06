import AppKit

enum NoPaintPalette {
    // Dynamic AppKit colors follow the window's effective appearance in both
    // SwiftUI and the native views. Painting pixels are never color-adjusted.
    static let background = adaptive(light: 0xEEE9DF, dark: 0x20201D)
    static let canvas = adaptive(light: 0xDDD8CE, dark: 0x171715)
    static let ink = adaptive(light: 0x202019, dark: 0xEEE9DF)

    private static func adaptive(light: UInt32, dark: UInt32) -> NSColor {
        NSColor(name: nil) { appearance in
            let rgb = appearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua ? dark : light
            return NSColor(srgbRed: CGFloat((rgb >> 16) & 255) / 255,
                           green: CGFloat((rgb >> 8) & 255) / 255,
                           blue: CGFloat(rgb & 255) / 255, alpha: 1)
        }
    }
}
