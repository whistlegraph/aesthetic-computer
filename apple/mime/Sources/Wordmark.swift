import SwiftUI

/// The MIME wordmark, drawn from the same letter paths as mime.ac's SVG: each
/// letter a coloured back edge, six extruded depth layers, a gradient face
/// and a shine, floating on its own slow period.
struct Wordmark: View {
    /// Letter, x offset in the wordmark's 151-wide space, face colours, float period and phase.
    private struct Letter { let path: String; let x: CGFloat; let back: UInt32; let face: [UInt32]; let shine: String; let period: Double; let phase: Double }
    private static let M = "M0 0H10L18 19L26 0H36V4H33V31H36V35H25V31H28V12L20 31H16L8 12V31H11V35H0V31H3V4H0Z"
    private static let mShine = "M1 0.7H9M27 0.7H35M1 31.7H10M26 31.7H35"
    private static let letters: [Letter] = [
        Letter(path: M, x: 0, back: 0xFF91C3, face: [0xFFE7F4, 0xFF91C3, 0xE93587], shine: mShine, period: 4.7, phase: -0.6),
        Letter(path: "M0 0H15V4H10V31H15V35H0V31H5V4H0Z", x: 43, back: 0xFFC585, face: [0xFFF4D9, 0xFFC585, 0xED765D], shine: "M1 0.7H14M1 31.7H14", period: 3.9, phase: -2.1),
        Letter(path: M, x: 65, back: 0xB7A4FF, face: [0xEEE8FF, 0xB7A4FF, 0x8460D7], shine: mShine, period: 5.3, phase: -3.4),
        Letter(path: "M0 0H26V10H22V4H9V15H19V19H9V31H22V25H26V35H0V31H4V4H0Z", x: 108, back: 0x7AE7CE, face: [0xE5FFF6, 0x7AE7CE, 0x259CAA], shine: "M1 0.7H25M1 31.7H25", period: 4.3, phase: -1.3),
    ]
    var height: CGFloat = 37

    var body: some View {
        TimelineView(.animation) { timeline in
            let t = timeline.date.timeIntervalSinceReferenceDate
            Canvas { context, size in
                let scale = size.height / 52
                context.scaleBy(x: scale, y: scale)
                context.translateBy(x: 4, y: 5) // the SVG viewBox starts at (-4, -5)
                for letter in Self.letters { draw(letter, in: &context, t: t) }
            }
        }
        .frame(width: height * 151 / 52, height: height)
        .accessibilityLabel("MIME.ac")
    }

    private func draw(_ letter: Letter, in context: inout GraphicsContext, t: Double) {
        let glyph = SVGPath(letter.path)
        let box = glyph.boundingRect
        // mime-float: a slow bob, tilt and squash around the letter's lower middle.
        let wave = (1 - cos(2 * .pi * (t - letter.phase) / letter.period)) / 2 // 0 → 1 → 0
        var c = context
        c.translateBy(x: letter.x + box.midX, y: box.minY + box.height * 0.65)
        c.translateBy(x: 0, y: 1 - 3.5 * wave)
        c.rotate(by: .degrees(-5 + 9 * wave))
        c.scaleBy(x: 0.97 + 0.06 * wave, y: 1.02 - 0.04 * wave)
        c.translateBy(x: -box.midX, y: -(box.minY + box.height * 0.65))
        let line = StrokeStyle(lineWidth: 1, lineJoin: .round)
        let back = glyph.offsetBy(dx: 7, dy: 7)
        c.fill(back, with: .color(Color(hex: letter.back)))
        c.stroke(back, with: .color(Color(hex: letter.back)), style: line)
        let depth = GraphicsContext.Shading.linearGradient(Gradient(colors: [Color(hex: 0x8B388A), Color(hex: 0x40317B)]),
                                                          startPoint: .init(x: box.minX, y: box.minY), endPoint: .init(x: box.minX + box.width * 0.6, y: box.maxY + 6))
        for step in stride(from: 6, through: 1, by: -1) {
            let layer = glyph.offsetBy(dx: CGFloat(step), dy: CGFloat(step))
            c.fill(layer, with: depth)
            c.stroke(layer, with: .color(Color(hex: 0x40317B)), style: StrokeStyle(lineWidth: 0.5, lineJoin: .round))
        }
        let light = 0.94 + 0.22 * wave // mime-light
        let face = GraphicsContext.Shading.linearGradient(
            Gradient(stops: [.init(color: Color(hex: letter.face[0]), location: 0), .init(color: Color(hex: letter.face[1]), location: 0.35), .init(color: Color(hex: letter.face[2]), location: 1)]),
            startPoint: .init(x: box.minX, y: box.minY), endPoint: .init(x: box.minX + box.width * 0.3, y: box.maxY))
        var faceContext = c
        faceContext.addFilter(.brightness(light - 1))
        faceContext.fill(glyph, with: face)
        faceContext.stroke(glyph, with: .color(Color(hex: 0x633268)), style: StrokeStyle(lineWidth: 0.65, lineJoin: .round))
        c.stroke(SVGPath(letter.shine), with: .color(Color(hex: 0xFFF5FC).opacity(0.45 + 0.55 * wave)), lineWidth: 0.9)
    }
}

/// The few absolute SVG path commands the letters use: M, L, H, V and Z.
func SVGPath(_ data: String) -> Path {
    var path = Path(), point = CGPoint.zero, numbers: [CGFloat] = [], command: Character = "M"
    func flush() {
        switch command {
        case "M": if numbers.count >= 2 { point = CGPoint(x: numbers[0], y: numbers[1]); path.move(to: point)
                  for i in stride(from: 2, to: numbers.count - 1, by: 2) { point = CGPoint(x: numbers[i], y: numbers[i + 1]); path.addLine(to: point) } }
        case "L": for i in stride(from: 0, to: numbers.count - 1, by: 2) { point = CGPoint(x: numbers[i], y: numbers[i + 1]); path.addLine(to: point) }
        case "H": for x in numbers { point.x = x; path.addLine(to: point) }
        case "V": for y in numbers { point.y = y; path.addLine(to: point) }
        case "Z": path.closeSubpath()
        default: break
        }
        numbers = []
    }
    var token = ""
    for char in data {
        if char.isLetter { if !token.isEmpty { numbers.append(CGFloat(Double(token) ?? 0)); token = "" }; flush(); command = char }
        else if char == " " || char == "," { if !token.isEmpty { numbers.append(CGFloat(Double(token) ?? 0)); token = "" } }
        else if char == "-" && !token.isEmpty { numbers.append(CGFloat(Double(token) ?? 0)); token = "-" }
        else { token.append(char) }
    }
    if !token.isEmpty { numbers.append(CGFloat(Double(token) ?? 0)) }
    flush()
    return path
}

extension Path {
    func offsetBy(dx: CGFloat, dy: CGFloat) -> Path { applying(CGAffineTransform(translationX: dx, y: dy)) }
}
