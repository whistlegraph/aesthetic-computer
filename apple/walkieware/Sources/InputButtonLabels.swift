import SwiftUI

struct TypingButtonLabel: View {
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    var body: some View {
        TimelineView(.animation(minimumInterval: 0.18, paused: reduceMotion)) { context in
            let beat = context.date.timeIntervalSinceReferenceDate.truncatingRemainder(dividingBy: 4)
            let count = reduceMotion ? 4 : min(4, 1 + Int(beat / 0.22))
            Text("Type▏").hidden().overlay(alignment: .leading) {
                Text(String("Type".prefix(count)) + "▏")
            }
        }.font(.custom("Courier-Bold", size: 38, relativeTo: .title))
            .tracking(-0.6)
            .frame(height: 48)
            .accessibilityHidden(true)
    }
}

struct ShoutButtonLabel: View {
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    var body: some View {
        TimelineView(.animation(minimumInterval: 1 / 20, paused: reduceMotion)) { context in
            ZStack {
                Canvas { canvas, size in
                    let phase = reduceMotion ? 0.4 : context.date.timeIntervalSinceReferenceDate.truncatingRemainder(dividingBy: 1.4) / 1.4
                    for side in [-1.0, 1.0] {
                        for row in [-1.0, 0.0, 1.0] {
                            let x = size.width / 2 + side * (43 + phase * 12)
                            let y = size.height / 2 + row * (12 + phase * 8)
                            var line = Path()
                            line.move(to: CGPoint(x: x, y: y))
                            line.addLine(to: CGPoint(x: x + side * 9, y: y + row * 5))
                            canvas.stroke(line, with: .color(.primary.opacity(0.65 * (1 - phase))), style: StrokeStyle(lineWidth: 2.5, lineCap: .round))
                        }
                    }
                }
                Text("Talk").font(.custom("ComicRelief-Bold", size: 34, relativeTo: .title))
            }
        }.frame(height: 48).accessibilityHidden(true)
    }
}

struct MicrophoneWaveform: View {
    let levels: [Double]
    var body: some View {
        Canvas { context, size in
            guard !levels.isEmpty else { return }
            let step = size.width / Double(levels.count)
            for (index, rms) in levels.enumerated() {
                // Fixed logarithmic gain keeps quiet speech visible without
                // normalizing ambient noise into a full-height waveform.
                let level = min(1, max(0, (20 * log10(max(0.00001, rms)) + 65) / 55))
                let height = max(2, level * size.height)
                let x = (Double(index) + 0.5) * step
                var bar = Path()
                bar.move(to: CGPoint(x: x, y: (size.height - height) / 2))
                bar.addLine(to: CGPoint(x: x, y: (size.height + height) / 2))
                context.stroke(bar, with: .foreground, style: StrokeStyle(lineWidth: max(1, step * 0.55), lineCap: .round))
            }
        }.accessibilityHidden(true)
    }
}
