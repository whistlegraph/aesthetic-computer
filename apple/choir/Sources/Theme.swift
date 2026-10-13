import SwiftUI

/// A card catalog on a desk: white index cards with a red head rule and blue
/// lines, typewritten, each with a call number. Light only, like paper.
enum Theme {
    static let desk = Color(red: 0.886, green: 0.859, blue: 0.80)
    static let paper = Color.white
    static let rule = Color(red: 0.59, green: 0.73, blue: 0.88).opacity(0.6)
    static let head = Color(red: 0.81, green: 0.25, blue: 0.25)
    static let ink = Color(red: 0.11, green: 0.094, blue: 0.086)
    static let panel = Color.white
    static let line: CGFloat = 1.5
    static func chrome(_ size: CGFloat = 13, _ weight: Font.Weight = .bold) -> Font { .system(size: size, weight: weight, design: .monospaced) }
    static func type(_ size: CGFloat = 16, bold: Bool = false) -> Font {
        .custom(bold ? "AmericanTypewriter-Semibold" : "AmericanTypewriter", size: size, relativeTo: .body)
    }
}

/// The desk under the cards.
struct RuledPaper: View {
    var body: some View { Theme.desk.ignoresSafeArea() }
}

/// A library catalog card, quietly: name and call number over a red hairline.
struct DeskWindow<Content: View>: View {
    let title: String
    var detail: String = ""
    var call: String = ""
    @ViewBuilder var content: Content
    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack(alignment: .center) {
                if title == "@choir" {
                    Image("Mark").renderingMode(.template).resizable().scaledToFit().frame(height: 18)
                        .accessibilityLabel("choir")
                } else {
                    Text(title).font(.system(size: 13, weight: .semibold)).tracking(0.3)
                }
                Spacer()
                Text([call, detail].filter { !$0.isEmpty }.joined(separator: "  ·  "))
                    .font(Theme.type(11)).opacity(0.45)
            }
            .foregroundStyle(Theme.ink)
            .padding(.horizontal, 18).padding(.top, 14).padding(.bottom, 10)
            Rectangle().fill(Theme.head.opacity(0.55)).frame(height: 0.75).padding(.horizontal, 18)
            content.padding(.horizontal, 18).padding(.top, 12).padding(.bottom, 16)
                .frame(maxWidth: .infinity, alignment: .leading)
        }
        .background(Theme.paper, in: RoundedRectangle(cornerRadius: 3))
        .shadow(color: .black.opacity(0.08), radius: 10, x: 0, y: 4)
    }
}

/// Dewey-style call number for a post: 700s are the arts, the decimal and the
/// Cutter letter come from its tags, id and author. Decorative, but stable.
enum Dewey {
    static func call(_ post: Post) -> String {
        func hash(_ s: String) -> Int { s.unicodeScalars.reduce(5381) { ($0 &* 33 &+ Int($1.value)) & 0x7fffffff } }
        let klass = 700 + hash(post.tags.joined(separator: ",")) % 100
        let decimal = hash(post.id) % 100
        let cutter = post.handle.drop(while: { $0 == "@" }).prefix(1).uppercased()
        return String(format: "%03d.%02d %@%d", klass, decimal, cutter, hash(post.handle) % 90 + 10)
    }
}

/// A square desktop button: white face, black frame, inverted when on.
struct DeskButtonStyle: ButtonStyle {
    var on = false
    func makeBody(configuration: Configuration) -> some View {
        let inverted = on != configuration.isPressed
        configuration.label
            .font(Theme.chrome(12))
            .padding(.horizontal, 9).padding(.vertical, 5)
            .foregroundStyle(inverted ? Theme.panel : Theme.ink)
            .background(inverted ? Theme.ink : Color(red: 0.98, green: 0.96, blue: 0.9))
            .overlay(RoundedRectangle(cornerRadius: 3).strokeBorder(Theme.ink.opacity(0.75), lineWidth: 1))
            .clipShape(RoundedRectangle(cornerRadius: 3))
    }
}

extension View {
    /// Forms and sheets sit on the same pad.
    func onPad() -> some View {
        scrollContentBackground(.hidden).background(RuledPaper()).tint(Theme.ink)
    }
}

/// A rounder, friendlier take on AC's two doors: same blue and green.
struct CuteDoorStyle: ButtonStyle {
    var new = false
    var small = false
    func makeBody(configuration: Configuration) -> some View {
        let fill = new ? Color(red: 0.2, green: 0.63, blue: 0.2) : Color(red: 0.24, green: 0.47, blue: 0.86)
        configuration.label
            .font(Theme.type(small ? 15 : 19, bold: true))
            .foregroundStyle(.white)
            .padding(.horizontal, small ? 16 : 22).padding(.vertical, small ? 8 : 12)
            .background(fill, in: Capsule())
            .overlay(Capsule().strokeBorder(Theme.ink, lineWidth: 2))
            .background(Capsule().fill(Theme.ink).offset(y: configuration.isPressed ? 1 : (small ? 3 : 4)))
            .offset(y: configuration.isPressed ? (small ? 2 : 3) : 0)
            .animation(.spring(response: 0.2, dampingFraction: 0.5), value: configuration.isPressed)
    }
}

/// The top of a spiral-bound pad: punched holes with wire coils through them.
struct Spiral: View {
    var body: some View {
        GeometryReader { geo in
            let n = max(6, Int(geo.size.width / 26))
            let step = geo.size.width / CGFloat(n)
            ZStack(alignment: .topLeading) {
                ForEach(0..<n, id: \.self) { i in
                    let x = step * (CGFloat(i) + 0.5)
                    Circle().fill(Theme.desk.opacity(0.9))
                        .overlay(Circle().strokeBorder(.black.opacity(0.25), lineWidth: 1))
                        .frame(width: 11, height: 11).position(x: x, y: 26)
                    Capsule()
                        .fill(LinearGradient(colors: [Color(white: 0.95), Color(white: 0.55), Color(white: 0.85), Color(white: 0.4)],
                                             startPoint: .leading, endPoint: .trailing))
                        .overlay(Capsule().strokeBorder(.black.opacity(0.35), lineWidth: 0.6))
                        .frame(width: 6, height: 30).position(x: x, y: 15)
                        .shadow(color: .black.opacity(0.25), radius: 1, x: 1, y: 1)
                }
            }
        }
        .frame(height: 36)
    }
}

/// A notepad page: white, blue lines, red margin, spiral across the top.
struct PadPage<Content: View>: View {
    @ViewBuilder var content: Content
    var body: some View {
        VStack(spacing: 0) {
            Spiral()
            content
                .padding(.leading, 40).padding(.trailing, 16).padding(.vertical, 12)
                .frame(maxWidth: .infinity, alignment: .topLeading)
                .background {
                    Canvas { ctx, size in
                        var y: CGFloat = 34
                        while y < size.height { ctx.fill(Path(CGRect(x: 0, y: y, width: size.width, height: 1)), with: .color(Theme.rule)); y += 28 }
                        ctx.fill(Path(CGRect(x: 30, y: 0, width: 1.2, height: size.height)), with: .color(Theme.head.opacity(0.7)))
                    }
                }
        }
        .background(Theme.paper)
        .clipShape(UnevenRoundedRectangle(bottomLeadingRadius: 4, bottomTrailingRadius: 4))
        .shadow(color: .black.opacity(0.2), radius: 8, y: 4)
    }
}

/// Liquid Glass where the system has it (iOS 26+), frosted material before.
extension View {
    @ViewBuilder func glass<S: Shape>(_ shape: S) -> some View {
        if #available(iOS 26.0, *) { glassEffect(.regular, in: shape) }
        else { background(.ultraThinMaterial, in: shape) }
    }
}

/// Send, louder than the other chips: Fía found the dimmed one hard to see
/// (10-12). Ink at rest, the red head rule once there is something to send.
struct SendStyle: ButtonStyle {
    var ready: Bool
    // Fía (22:05): "higher contrast … it's still hard to read"; (22:28) "the send
    // needs to be legible!" → plain bold type, never faded: white on ink when
    // ready, ink on pale paper when there's nothing to send yet.
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(.system(size: 16, weight: .bold, design: .rounded))
            .foregroundStyle(ready ? Color.white : Theme.ink)
            .frame(height: 40).padding(.horizontal, 18)
            .background(Capsule().fill(ready ? Theme.ink : Color(white: 0.88)))
            .shadow(color: .black.opacity(ready ? 0.2 : 0), radius: 4, y: 2)
            .scaleEffect(configuration.isPressed ? 0.94 : 1)
            .animation(.easeOut(duration: 0.15), value: ready)
    }
}

/// Round glass buttons beside the text box, all the same size so they line up.
struct CircleChipStyle: ButtonStyle {
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .foregroundStyle(Theme.ink)
            .frame(width: 40, height: 40)
            .glass(Circle())
            .scaleEffect(configuration.isPressed ? 0.92 : 1)
    }
}

struct GlassChipStyle: ButtonStyle {
    var on = false
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(Theme.type(13, bold: on))
            .padding(.horizontal, 11).padding(.vertical, 6)
            .foregroundStyle(on ? Color.white : Theme.ink)
            .background { if on { Capsule().fill(Theme.ink) } }
            .glass(Capsule())
            .scaleEffect(configuration.isPressed ? 0.94 : 1)
    }
}
