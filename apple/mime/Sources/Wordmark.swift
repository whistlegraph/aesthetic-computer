import SwiftUI

/// The same Comic Relief Bold glyphs, palette and proportions as mime.ac.
struct Wordmark: View {
    var height: CGFloat = 37
    var breakupAt: Date?
    var breakupDuration: Double = 0.8
    @Environment(\.accessibilityReduceMotion) private var reduced
    private let palette: [UInt32] = [0xFF91C3, 0xFFC585, 0xB7A4FF, 0x7AE7CE]
    private let periods: [Double] = [4.7, 3.9, 5.3, 4.3]
    private let phases: [Double] = [-0.6, -2.1, -1.3, -3.4]
    private let departures: [CGSize] = [CGSize(width: -98, height: -72), CGSize(width: -24, height: 92), CGSize(width: 38, height: -95), CGSize(width: 112, height: 66)]

    var body: some View {
        TimelineView(.animation) { timeline in
            let time = timeline.date.timeIntervalSinceReferenceDate
            let progress = breakupAt.map { min(1, max(0, timeline.date.timeIntervalSince($0) / breakupDuration)) } ?? 0
            HStack(spacing: 0) {
                ForEach(Array("MIME".enumerated()), id: \.offset) { i, letter in
                    let phase = reduced ? 0 : sin((time + phases[i]) * 2 * .pi / periods[i])
                    PalText(text: String(letter), size: height * 0.84, accent: Color(white: 0.08), fill: Color(hex: palette[i]))
                        .rotationEffect(.degrees(phase * 2 + (i.isMultiple(of: 2) ? -35 : 35) * progress))
                        .offset(x: departures[i].width * progress * height / 52,
                                y: phase * height * 0.03 + departures[i].height * progress * height / 52)
                        .opacity(1 - progress)
                }
            }.frame(width: height * 151 / 52, height: height)
        }.accessibilityLabel("MIME").accessibilityIdentifier("mime-comic-logo")
    }
}
