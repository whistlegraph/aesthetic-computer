// AESEL ROCK LETTERING.
//
// The Aesel desktop titles a piece in its own lettering: each letter a small
// cached image with cyan and purple echoes, a pink hard shadow, a dark edge and
// a light face, leaning by an FNV hash and swaying on its own beat. Slab labels
// the same pieces — under the code on the prompt rock, on the preview card — so
// it borrows the same lettering rather than a cousin of it. This is a port of
// `Rock` and `AeselTitle` from apple/aesel/Sources/AeselScene.swift; keep the
// two in step, the point is that the rock and the window agree.

import AppKit
import SwiftUI

enum AeselRock {
    /// How far a letter's image overhangs its advance box on every side, so the
    /// echoes and shadow are never clipped.
    static let inset: CGFloat = 4
    private static var cache: [String: (NSImage, CGSize)] = [:]

    static func font(_ size: CGFloat) -> NSFont {
        for name in ["ComicRelief-Bold", "ComicSansMS-Bold", "ChalkboardSE-Bold"] {
            if let font = NSFont(name: name, size: size) { return font }
        }
        return .systemFont(ofSize: size, weight: .heavy)
    }

    /// One letter's image plus its advance box. The image is resolution
    /// independent (a drawing handler), so it stays crisp in a layer or a view.
    static func glyph(_ text: String, size: CGFloat, face: NSColor = .white) -> (image: NSImage, advance: CGSize) {
        let key = "\(text)|\(size)|\(face)"
        if let hit = cache[key] { return hit }
        let font = font(size)
        let advance = (text as NSString).size(withAttributes: [.font: font])
        let bounds = CGSize(width: ceil(advance.width + inset * 2), height: ceil(advance.height + inset * 2))
        let image = NSImage(size: bounds, flipped: true) { _ in
            for (color, x, y) in [(NSColor.systemCyan, CGFloat(1.75), CGFloat(1)),
                                  (NSColor.systemPurple, CGFloat(1), CGFloat(0.75))] {
                NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: color,
                    .strokeColor: color, .strokeWidth: -3])
                    .draw(at: CGPoint(x: inset + x, y: inset + y))
            }
            let shadow = NSShadow()
            shadow.shadowColor = NSColor.systemPink.withAlphaComponent(0.7)
            shadow.shadowBlurRadius = 0
            shadow.shadowOffset = CGSize(width: 0.75, height: 0.75)
            NSAttributedString(string: text, attributes: [
                .font: font,
                .foregroundColor: face,
                .strokeColor: NSColor(white: 0.08, alpha: 1),
                .strokeWidth: -3,
                .shadow: shadow,
            ]).draw(at: CGPoint(x: inset, y: inset))
            return true
        }
        if cache.count > 256 { cache.removeAll() }
        cache[key] = (image, advance)
        return (image, advance)
    }

    /// The desktop's per-letter hash, so a letter leans the same way here as
    /// it does in the Aesel window.
    static func fnv(_ text: String) -> UInt32 {
        var hash: UInt32 = 2166136261
        for byte in text.utf8 { hash = (hash ^ UInt32(byte)) &* 16777619 }
        return hash
    }

    /// Letters of a leading `@handle` (up to the first `/`) wear the account
    /// palette; everything else is the white face.
    static func faces(_ text: String, colors: [NSColor]) -> [NSColor] {
        let handle = text.hasPrefix("@") ? text.prefix(while: { $0 != "/" }).count : 0
        return (0..<text.count).map { $0 < handle && $0 < colors.count ? colors[$0] : .white }
    }

    /// Width of a one-line title at `size`, advances only.
    static func width(_ text: String, size: CGFloat) -> CGFloat {
        let font = font(size)
        return text.reduce(0) { $0 + (String($1) as NSString).size(withAttributes: [.font: font]).width }
    }

    /// Resting lean and lift for letter `index` of `text`, in Core Animation's
    /// y-up space (the desktop's SwiftUI values, flipped).
    static func rest(_ index: Int, of text: String) -> (radians: CGFloat, lift: CGFloat) {
        let hash = fnv("rock\(index)\(text)")
        let degrees = CGFloat(Int((hash >> 8) % 9) - 4) * 0.2
        return (degrees * .pi / 180, (CGFloat(hash % 5) / 2 - 1) * 0.25)
    }

    /// The desktop's sway for a layer: a 1.8s sine, ±0.35° and ±0.35pt,
    /// each letter a beat of 0.12s behind the one before it.
    static func addSway(to layer: CALayer, index: Int, from start: CFTimeInterval) {
        guard !NSWorkspace.shared.accessibilityDisplayShouldReduceMotion else { return }
        let ease = Array(repeating: CAMediaTimingFunction(name: .easeInEaseOut), count: 4)
        for (path, amount) in [("transform.rotation.z", -0.35 * Double.pi / 180), ("transform.translation.y", -0.35)] {
            let sway = CAKeyframeAnimation(keyPath: path)
            sway.values = [0, amount, 0, -amount, 0]
            sway.keyTimes = [0, 0.25, 0.5, 0.75, 1]
            sway.timingFunctions = ease
            sway.duration = 1.8
            sway.repeatCount = .infinity
            sway.beginTime = start + Double(index) * 0.12
            sway.isAdditive = true
            layer.add(sway, forKey: "aeselSway" + path)
        }
    }
}

/// The desktop's `AeselTitle`, sized for Slab's small surfaces: one swaying
/// letter per character, handle letters in the account palette.
struct AeselRockTitle: View {
    let text: String
    var colors: [NSColor] = []
    var size: CGFloat = 11

    var body: some View {
        let faces = AeselRock.faces(text, colors: colors)
        HStack(alignment: .top, spacing: 0) {
            ForEach(Array(text.enumerated()), id: \.offset) { index, letter in
                let rest = AeselRock.rest(index, of: text)
                AeselRockLetter(text: String(letter), size: size, face: faces[index],
                                beat: Double(index) * 0.12)
                    .id("\(index)|\(letter)|\(faces[index])")
                    .rotationEffect(.radians(-Double(rest.radians)))
                    .offset(y: -rest.lift)
            }
        }
        .fixedSize()
        .accessibilityElement(children: .ignore)
        .accessibilityLabel(text)
    }
}

/// One letter, its image overhanging the advance box by `AeselRock.inset`.
/// Before macOS 12 there is no TimelineView, so the letter rests still.
private struct AeselRockLetter: View {
    let text: String
    let size: CGFloat
    let face: NSColor
    let beat: Double

    var body: some View {
        let glyph = AeselRock.glyph(text, size: size, face: face)
        if #available(macOS 12.0, *) {
            let still = NSWorkspace.shared.accessibilityDisplayShouldReduceMotion
            TimelineView(.animation(minimumInterval: 1 / 30, paused: still)) { timeline in
                let wave = still ? 0 : sin((timeline.date.timeIntervalSinceReferenceDate - beat) * .pi / 0.9)
                letter(glyph)
                    .rotationEffect(.degrees(wave * 0.35))
                    .offset(y: wave * 0.35)
            }
        } else {
            letter(glyph)
        }
    }

    private func letter(_ glyph: (image: NSImage, advance: CGSize)) -> some View {
        Color.clear
            .frame(width: glyph.advance.width, height: glyph.advance.height)
            .overlay(Image(nsImage: glyph.image).offset(x: -AeselRock.inset, y: -AeselRock.inset),
                     alignment: .topLeading)
    }
}
