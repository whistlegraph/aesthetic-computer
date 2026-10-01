import SwiftUI

// Slab's status palette, expressed as native colors across the whole workspace.
struct WalkiewareTheme {
    enum Phase { case ready, listening, working, rendering, error }
    let background: Color
    let foreground: Color
    let accent: Color
    let buttonInk: Color
    var surface: Color { foreground.opacity(0.08) }
    init(phase: Phase, dark: Bool) {
        func rgb(_ r: Double, _ g: Double, _ b: Double) -> Color {
            Color(red: r / 65535, green: g / 65535, blue: b / 65535)
        }
        buttonInk = rgb(5000, 7000, 9000)
        switch phase {
        case .ready:
            background = dark ? rgb(2200,4200,14000) : rgb(42000,50000,65535)
            foreground = dark ? rgb(46000,51000,64000) : rgb(5000,12000,35000)
            accent = dark ? rgb(40000,51000,65535) : rgb(26000,39000,60000)
        case .listening:
            background = dark ? rgb(19000,10500,900) : rgb(65535,59000,45000)
            foreground = dark ? rgb(65535,58000,38000) : rgb(26000,13000,0)
            accent = rgb(65535,46000,10000)
        case .working:
            background = dark ? rgb(600,12000,3200) : rgb(39000,64000,45000)
            foreground = dark ? rgb(40000,64000,47000) : rgb(1200,22000,6000)
            accent = rgb(20000,54000,32000)
        case .rendering, .error:
            background = dark ? rgb(9000,1800,5800) : rgb(65535,54500,60000)
            foreground = dark ? rgb(62000,44000,54000) : rgb(26000,2500,15000)
            accent = rgb(60000,33000,46000)
        }
    }
}
