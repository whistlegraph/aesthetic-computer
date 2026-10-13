import SwiftUI

/// choir's logo, Fía's flower (Assets "Logo", made by marketing/choir/logo/brand.py),
/// breathing a little: a slow bob, a slight sway and a swell on the upswing,
/// like a singer between phrases. Still when Reduce Motion is on.
struct SingingMark: View {
    var height: CGFloat = 120
    var energy: Double = 1
    @Environment(\.accessibilityReduceMotion) private var still

    var body: some View {
        TimelineView(.animation(paused: still)) { clock in
            let t = still ? 0 : clock.date.timeIntervalSinceReferenceDate
            let wave = sin(t * 1.6)
            Image("Logo").resizable().scaledToFit()
                .offset(y: -wave * height * 0.03 * energy)
                .rotationEffect(.degrees(sin(t * 1.1) * 3 * energy))
                .scaleEffect(1 + max(0, wave) * 0.03 * energy)
        }
        .frame(height: height)
        .accessibilityElement().accessibilityLabel("choir")
    }
}
