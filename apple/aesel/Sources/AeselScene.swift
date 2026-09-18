import SwiftUI
import UIKit

// The desktop's palette, typefaces and mascot, shared without redraws.
// Title lettering is the desktop's Comic Sans MS Bold; Chalkboard SE Bold is
// the fallback its own CSS names on Apple platforms, so nothing ships.
enum Paint {
    static let bg = Color(rgb: 0x463264)
    static let deep = Color(rgb: 0x241d35)
    static let ink = Color.white
    static let dim = Color(rgb: 0xb394ce)
    static let rule = Color(rgb: 0x9b72b8)
    static let you = Color(rgb: 0xff0088)
    static let ac = Color(rgb: 0xff64ff)
    static let edit = Color(rgb: 0x77ff55)
    static let bad = Color(rgb: 0xff7777)
    static let accent = Color(rgb: 0xff78b2)
    // The preview frame: --aesel-background 75% + accent, edged 65% toward white.
    static let frame = Color(rgb: 0x744478)
    static let frameEdge = Color(rgb: 0xa485a7)
    static func font(_ size: CGFloat = 17) -> Font {
        .custom("Helvetica", size: size, relativeTo: .body)
    }
    static func title(_ size: CGFloat = 28) -> Font {
        .custom("ChalkboardSE-Bold", size: size, relativeTo: .title)
    }
}

extension Color {
    init(rgb: UInt32) {
        self.init(red: Double((rgb >> 16) & 255) / 255,
                  green: Double((rgb >> 8) & 255) / 255,
                  blue: Double(rgb & 255) / 255)
    }
    // "#rrggbb" from the account palette, or nil.
    init?(hex: String) {
        guard hex.count == 7, let rgb = UInt32(hex.dropFirst(), radix: 16) else { return nil }
        self.init(rgb: rgb)
    }
}

struct AeselCloth: View {
    var body: some View { Paint.bg.accessibilityHidden(true) }
}

struct AeselWordmark: View {
    var text = "aesel"
    private let colors: [Color] = [.orange, Paint.ac, Paint.edit, Paint.dim, Paint.you]
    var body: some View {
        HStack(spacing: 0) {
            ForEach(Array(text.enumerated()), id: \.offset) { index, letter in
                Text(String(letter)).foregroundStyle(colors[index % colors.count])
            }
        }
        .font(Paint.title())
        .accessibilityElement(children: .ignore)
        .accessibilityLabel("aesel")
    }
}

/// The desktop's piece title: handle letters in the account palette, a
/// status-coloured shadow, and every letter leaning by the FNV hash Slab uses,
/// so the phone and the rock agree on each letter's tilt.
struct AeselTitle: View {
    let text: String
    var colors: [String] = []
    var status = ""
    var shadow: Color?
    var size: CGFloat = 28
    private static let shadows: [String: UInt32] = [
        "working": 0x36f175, "thinking": 0x36f175, "rendering": 0xe1469c, "awaiting": 0xffb327,
        "complete": 0x5d9cf9, "ready": 0x5d9cf9, "interrupted": 0x9c56e9, "stopped": 0x9c56e9,
        "stale": 0xff4e4e, "failed": 0xff4e4e,
    ]
    private static func fnv(_ text: String) -> UInt32 {
        var hash: UInt32 = 2166136261
        for byte in text.utf8 { hash = (hash ^ UInt32(byte)) &* 16777619 }
        return hash
    }

    // "@handle" letters wear the account palette; the piece name stays white.
    private func ink(_ index: Int, handle: Int) -> Color {
        guard index < handle, colors.indices.contains(index), let color = Color(hex: colors[index]) else { return Paint.ink }
        return color
    }

    var body: some View {
        let handle = text.hasPrefix("@") ? text.prefix(while: { $0 != "/" }).count : 0
        HStack(spacing: 0) {
            ForEach(Array(text.enumerated()), id: \.offset) { index, letter in
                let hash = Self.fnv("rock\(index)\(text)")
                Text(String(letter))
                    .foregroundStyle(ink(index, handle: handle))
                    .rotationEffect(.degrees(-Double(Int((hash >> 8) % 9) - 4) * 0.9))
                    .offset(y: -(CGFloat(hash % 5) / 2 - 1))
            }
        }
        .font(Paint.title(size))
        .shadow(color: shadow ?? Color(rgb: Self.shadows[status] ?? 0x808080), radius: 0, x: size * 0.07, y: size * 0.07)
        .accessibilityElement(children: .ignore)
        .accessibilityLabel(text)
    }
}

/// The desktop's pencil companion: eight 512px cells on a 4×2 sheet, copied in
/// by bundle-session.sh, drawn smooth and running only while a turn is live.
struct AeselDonkey: View {
    var busy: Bool
    var failed: Bool
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    private static let frames: [UIImage] = {
        guard let url = Bundle.main.url(forResource: "donkey-pencil-run-v2", withExtension: "png", subdirectory: "Session/easel/desktop/assets"),
              let sheet = UIImage(contentsOfFile: url.path)?.cgImage else { return [] }
        let cell = sheet.width / 4
        return (0..<8).compactMap { index in
            sheet.cropping(to: CGRect(x: (index % 4) * cell, y: (index / 4) * sheet.height / 2 + 64, width: cell, height: 416)).map { UIImage(cgImage: $0) }
        }
    }()

    var body: some View {
        TimelineView(.animation(minimumInterval: 0.09, paused: reduceMotion || !busy)) { timeline in
            let index = busy && !reduceMotion ? Int(timeline.date.timeIntervalSinceReferenceDate / 0.09) % 8 : 0
            if Self.frames.indices.contains(index) {
                Image(uiImage: Self.frames[index])
                    .resizable()
                    .scaledToFit()
                    .opacity(failed ? 0.5 : 1)
            }
        }
        .frame(width: 112, height: 112)
        .accessibilityLabel(busy ? "aesel is working" : "aesel the donkey")
    }
}

struct AeselHandle: View {
    let handle: String
    let colors: [String]

    private var label: AttributedString {
        var result = AttributedString()
        for (index, character) in ("@" + handle).enumerated() {
            var letter = AttributedString(String(character))
            letter.foregroundColor = colors.indices.contains(index) ? Color(hex: colors[index]) ?? Paint.ac : Paint.ac
            result += letter
        }
        return result
    }

    var body: some View {
        Text(label).lineLimit(1).truncationMode(.tail)
            .accessibilityLabel("@" + handle)
    }
}
