import SwiftUI

@main
struct AppearanceChecks {
    static func luminance(_ hex: String) -> Double {
        let rgb = UInt32(hex.dropFirst(), radix: 16)!
        let linear = [16, 8, 0].map { shift -> Double in
            let c = Double((rgb >> shift) & 255) / 255
            return c <= 0.04045 ? c / 12.92 : pow((c + 0.055) / 1.055, 2.4)
        }
        return linear[0] * 0.2126 + linear[1] * 0.7152 + linear[2] * 0.0722
    }

    static func main() {
        for scheme in [ColorScheme.light, .dark] {
            for status in ["", "ready", "writing", "publishing", "awaiting", "stopped", "failed"] {
                for busy in [false, true] {
                    let paint = Paint.slab(status: status, busy: busy, failed: status == "failed", colorScheme: scheme)
                    precondition(paint.css["colorScheme"] == (scheme == .light ? "light" : "dark"))
                    let background = luminance(paint.css["background"]!)
                    for role in ["foreground", "userInk", "error", "number"] {
                        let ink = luminance(paint.css[role]!)
                        let contrast = (max(ink, background) + 0.05) / (min(ink, background) + 0.05)
                        precondition(contrast >= 4.5, "\(scheme) \(status) \(role): \(contrast)")
                    }
                }
            }
        }
        print("Text contrast passes in light/dark across all activity states.")
    }
}
