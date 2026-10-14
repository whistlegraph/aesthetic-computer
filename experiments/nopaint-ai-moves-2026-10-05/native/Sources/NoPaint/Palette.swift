import AppKit
import ACUI

enum NoPaintPalette {
    // Dynamic AppKit colors follow the window's effective appearance in both
    // SwiftUI and the native views. Painting pixels are never color-adjusted.
    static let background = adaptive("background")
    static let canvas = adaptive("canvas")
    static let ink = adaptive("ink")

    private static func adaptive(_ name: String) -> NSColor {
        let colors = InterfaceSpec.noPaint.colors[name]!
        return adaptive(light: UInt32(colors[0].dropFirst(), radix: 16)!, dark: UInt32(colors[1].dropFirst(), radix: 16)!)
    }

    private static func adaptive(light: UInt32, dark: UInt32) -> NSColor {
        NSColor(name: nil) { appearance in
            let rgb = appearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua ? dark : light
            return NSColor(srgbRed: CGFloat((rgb >> 16) & 255) / 255,
                           green: CGFloat((rgb >> 8) & 255) / 255,
                           blue: CGFloat(rgb & 255) / 255, alpha: 1)
        }
    }
}
