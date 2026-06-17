// DayPalette.swift — the one ROYGBIV week palette shared by the menu bar
// day-strip (MenuBarDays) and the wizard pane tint (WizardController).
//
// Sun→Sat maps onto the same RGB Menu Band uses for its notepat naturals
// (NoteColors c,d,e,f,g,a,b), so DateWizard and Menu Band read as one
// instrument: the week is a little rainbow.
import AppKit

enum DayPalette {
    static let colors: [NSColor] = [
        rgb(255, 50,  50),   // Sun — red
        rgb(255, 160, 0),    // Mon — orange
        rgb(255, 230, 0),    // Tue — yellow
        rgb(50,  200, 50),   // Wed — green
        rgb(50,  120, 255),  // Thu — blue
        rgb(130, 50,  200),  // Fri — indigo
        rgb(180, 80,  255),  // Sat — violet
    ]
    static let letters = ["S", "M", "T", "W", "T", "F", "S"]

    // Sunday = 0 … Saturday = 6 (Calendar's .weekday is 1…7 from Sunday).
    static func index(for date: Date) -> Int {
        (Calendar.current.component(.weekday, from: date) - 1) % 7
    }

    static func color(for date: Date) -> NSColor { colors[index(for: date)] }

    // Black on bright circles, white on dark ones — same luminance test
    // the Menu Band uses for its chord readout.
    static func contrastColor(on bg: NSColor) -> NSColor {
        let c = bg.usingColorSpace(.deviceRGB) ?? bg
        let lum = 0.299 * c.redComponent + 0.587 * c.greenComponent + 0.114 * c.blueComponent
        return lum > 0.55 ? .black : .white
    }

    private static func rgb(_ r: Int, _ g: Int, _ b: Int) -> NSColor {
        NSColor(deviceRed: CGFloat(r) / 255, green: CGFloat(g) / 255,
                blue: CGFloat(b) / 255, alpha: 1)
    }
}
