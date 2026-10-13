import SwiftUI

/// Fía's four hand-drawn Cs, each bobbing and swaying out of step like
/// singers in a choir. Still when Reduce Motion is on.
struct SingingMark: View {
    var height: CGFloat = 120
    var energy: Double = 1
    @Environment(\.accessibilityReduceMotion) private var still
    private let voices: [(speed: Double, phase: Double)] = [(1.9, 0), (2.3, 1.7), (2.1, 3.1), (1.7, 4.4)]

    var body: some View {
        TimelineView(.animation(paused: still)) { clock in
            let t = still ? 0 : clock.date.timeIntervalSinceReferenceDate
            ZStack {
                ForEach(0..<4, id: \.self) { i in
                    let v = voices[i]
                    let wave = sin(t * v.speed + v.phase)
                    Image("Mark\(i + 1)").renderingMode(.template).resizable().scaledToFit()
                        .offset(y: -wave * height * 0.045 * energy)
                        .rotationEffect(.degrees(wave * 4 * energy), anchor: .bottom)
                        .scaleEffect(1 + max(0, wave) * 0.04 * energy, anchor: .bottom)
                }
            }
        }
        .frame(height: height)
        .foregroundStyle(Theme.ink)
        .accessibilityElement().accessibilityLabel("choir")
    }
}
