import SwiftUI
import UIKit

// The desktop's bitmap font, sprite sheet and palette, shared without redraws.
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
    static func font(_ size: CGFloat = 20) -> Font {
        .custom("ACEaselUnifont-Regular", size: size, relativeTo: .body)
    }
}

extension Color {
    init(rgb: UInt32) {
        self.init(red: Double((rgb >> 16) & 255) / 255,
                  green: Double((rgb >> 8) & 255) / 255,
                  blue: Double(rgb & 255) / 255)
    }
}

struct AeselCloth: View {
    var body: some View {
        Canvas { context, size in
            context.fill(Path(CGRect(origin: .zero, size: size)), with: .color(Paint.bg))
            for y in stride(from: CGFloat(0), to: size.height, by: 8) {
                context.fill(Path(CGRect(x: 0, y: y, width: size.width, height: 2)), with: .color(.white.opacity(0.016)))
            }
            for x in stride(from: CGFloat(0), to: size.width, by: 8) {
                context.fill(Path(CGRect(x: x, y: 0, width: 2, height: size.height)), with: .color(.black.opacity(0.043)))
            }
        }
        .accessibilityHidden(true)
    }
}

struct AeselWood: View {
    var body: some View {
        Canvas { context, size in
            let boards: [UInt32] = [0x453126, 0x50382a, 0x59402e]
            for y in stride(from: CGFloat(0), to: size.height, by: 32) {
                context.fill(Path(CGRect(x: 0, y: y, width: size.width, height: 32)), with: .color(Color(rgb: boards[Int(y / 32) % 3])))
                for row in 0..<7 {
                    for x in stride(from: CGFloat(0), to: size.width, by: 4) {
                        let wave = (sin((Double(x) + Double(row * 19)) / 42) * 2).rounded() * 2
                        let rect = CGRect(x: x, y: y + CGFloat(3 + row * 4) + wave, width: 4, height: row % 3 == 0 ? 2 : 1)
                        context.fill(Path(rect), with: .color(row % 2 == 0 ? .black.opacity(0.15) : Color(rgb: 0xac7e45).opacity(0.15)))
                    }
                }
                context.fill(Path(CGRect(x: 0, y: y + 31, width: size.width, height: 1)), with: .color(Color(rgb: 0x241e18)))
            }
        }
        .accessibilityHidden(true)
    }
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
        .font(Paint.font(28))
        .accessibilityElement(children: .ignore)
        .accessibilityLabel("aesel")
    }
}

struct AeselDonkey: View {
    var busy: Bool
    var failed: Bool
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    private static let frames: [UIImage] = {
        guard let sheet = UIImage(named: "aesel")?.cgImage else { return [] }
        return (0..<16).compactMap { index in
            sheet.cropping(to: CGRect(x: (index % 4) * 64, y: (index / 4) * 64, width: 64, height: 64)).map { UIImage(cgImage: $0) }
        }
    }()

    var body: some View {
        TimelineView(.animation(minimumInterval: busy ? 0.18 : 0.18, paused: reduceMotion)) { timeline in
            let elapsed = timeline.date.timeIntervalSinceReferenceDate
            let index = reduceMotion ? 0 : busy ? 8 + Int(elapsed / 0.18) % 4 : failed ? 2 + Int(elapsed / 0.5) % 2 : (elapsed.truncatingRemainder(dividingBy: 1.58) < 1.4 ? 0 : 1)
            if Self.frames.indices.contains(index) {
                Image(uiImage: Self.frames[index])
                    .resizable()
                    .interpolation(.none)
                    .scaledToFit()
                    .scaleEffect(x: -1, y: 1)
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
            if colors.indices.contains(index), colors[index].count == 7,
               let rgb = UInt32(colors[index].dropFirst(), radix: 16) {
                letter.foregroundColor = Color(rgb: rgb)
            } else {
                letter.foregroundColor = Paint.ac
            }
            result += letter
        }
        return result
    }

    var body: some View {
        Text(label).lineLimit(1).truncationMode(.tail)
            .accessibilityLabel("@" + handle)
    }
}
