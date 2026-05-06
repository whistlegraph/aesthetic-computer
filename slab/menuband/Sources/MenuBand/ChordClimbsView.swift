import AppKit

/// Vertical stack of chord candidates rendered as colored
/// climbing routes — each chord is a "wall" with circular holds
/// for every note in the chord, connected by a colored path.
/// Color encodes chord family (green = major, blue = minor, etc.).
/// Each hold is filled with the user's pressed keyboard letter so
/// the user reads off "press a, then s, then d to climb the
/// G-major route." Held notes show as filled holds; missing
/// notes (still to grab) render as outlined holds.
final class ChordClimbsView: NSView {
    struct Hold: Equatable {
        let pitchClass: Int          // 0..11
        let keyLabel: String?        // letter on the user's keyboard
        let held: Bool               // is the user already pressing this?
    }

    struct Route: Equatable {
        let name: String             // e.g. "Cmaj7", "Em", "F#dim"
        let kind: Kind
        let holds: [Hold]            // ordered by pitch (low → high)
        let isComplete: Bool

        enum Kind: Equatable {
            case major, minor, dim, aug, dom7, maj7, min7, sus, other
        }
    }

    var routes: [Route] = [] {
        didSet {
            if routes != oldValue { rebuild() }
        }
    }

    /// Tap callback when the user clicks a route — convenience
    /// hook for future "audition this chord" interaction. Hold
    /// for now, the popover doesn't wire it.
    var onRouteSelect: ((Int) -> Void)?

    private let stack = NSStackView()

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 3
        stack.translatesAutoresizingMaskIntoConstraints = false
        addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 2),
            stack.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -2),
            stack.topAnchor.constraint(equalTo: topAnchor, constant: 2),
            stack.bottomAnchor.constraint(lessThanOrEqualTo: bottomAnchor, constant: -2),
        ])
    }

    required init?(coder: NSCoder) { fatalError() }

    private func rebuild() {
        for v in stack.arrangedSubviews {
            stack.removeArrangedSubview(v)
            v.removeFromSuperview()
        }
        for route in routes {
            let row = ChordClimbRow(route: route)
            row.translatesAutoresizingMaskIntoConstraints = false
            stack.addArrangedSubview(row)
        }
        needsLayout = true
    }
}

/// One climbing-route row — a colored bar with note holds along it.
final class ChordClimbRow: NSView {
    private let route: ChordClimbsView.Route

    private static let holdSize: CGFloat = 18
    private static let holdSpacing: CGFloat = 4
    private static let labelWidth: CGFloat = 40
    private static let rowHeight: CGFloat = 22

    init(route: ChordClimbsView.Route) {
        self.route = route
        super.init(frame: .zero)
        wantsLayer = true
    }
    required init?(coder: NSCoder) { fatalError() }

    override var intrinsicContentSize: NSSize {
        let holdsW = CGFloat(route.holds.count) * Self.holdSize
            + CGFloat(max(0, route.holds.count - 1)) * Self.holdSpacing
        let totalW = Self.labelWidth + 6 + holdsW + 6
        return NSSize(width: totalW, height: Self.rowHeight)
    }

    override var isFlipped: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let routeColor = Self.color(for: route.kind)
        let labelInk = NSColor.labelColor

        // Chord name on the left — colored to match the route.
        let nameAttr = NSAttributedString(
            string: route.name,
            attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 11, weight: .heavy),
                .foregroundColor: route.isComplete ? routeColor : labelInk.withAlphaComponent(0.85),
            ]
        )
        let nameSize = nameAttr.size()
        nameAttr.draw(at: NSPoint(
            x: 4,
            y: bounds.midY - nameSize.height / 2
        ))

        // Connecting "rope" — translucent colored bar running
        // through the centers of the holds.
        let holdsStartX = Self.labelWidth + 6
        let totalHoldsW = CGFloat(route.holds.count) * Self.holdSize
            + CGFloat(max(0, route.holds.count - 1)) * Self.holdSpacing
        let ropeRect = NSRect(
            x: holdsStartX,
            y: bounds.midY - 1.5,
            width: totalHoldsW,
            height: 3
        )
        routeColor.withAlphaComponent(route.isComplete ? 0.85 : 0.45).setFill()
        NSBezierPath(roundedRect: ropeRect, xRadius: 1.5, yRadius: 1.5).fill()

        // Holds — circles along the rope, filled if the user is
        // currently pressing that pitch, outlined if missing.
        // Each hold is colored by the chromatic ROYGBIV palette
        // (same as the bottoms of the piano keys), so the user
        // can see the C=red / D=orange / E=yellow / … pattern in
        // the chord too — chord routes share visual language with
        // the keyboard.
        for (i, hold) in route.holds.enumerated() {
            let x = holdsStartX + CGFloat(i) * (Self.holdSize + Self.holdSpacing)
            let rect = NSRect(
                x: x,
                y: bounds.midY - Self.holdSize / 2,
                width: Self.holdSize,
                height: Self.holdSize
            )
            let circle = NSBezierPath(ovalIn: rect.insetBy(dx: 0.5, dy: 0.5))
            let chroma = Self.chromaticColor(for: hold.pitchClass)
            if hold.held {
                chroma.setFill()
                circle.fill()
                NSColor.black.withAlphaComponent(0.5).setStroke()
                circle.lineWidth = 0.9
                circle.stroke()
            } else {
                chroma.withAlphaComponent(0.18).setFill()
                circle.fill()
                chroma.withAlphaComponent(0.85).setStroke()
                circle.lineWidth = 1.2
                circle.stroke()
            }
            // Letter to type, centered in the hold. Near-black
            // text reads on top of every chromatic hue; ghost
            // holds get a faded version.
            let letter = hold.keyLabel ?? Self.pitchClassName(hold.pitchClass)
            let inkAlpha: CGFloat = hold.held ? 0.95 : 0.65
            let letterColor = NSColor(white: 0.10, alpha: inkAlpha)
            let attr = NSAttributedString(
                string: letter,
                attributes: [
                    .font: NSFont.monospacedSystemFont(ofSize: 9, weight: .heavy),
                    .foregroundColor: letterColor,
                ]
            )
            let size = attr.size()
            attr.draw(at: NSPoint(
                x: rect.midX - size.width / 2,
                y: rect.midY - size.height / 2 + 0.5
            ))
        }
    }

    /// ROYGBIV chromatic palette — C=red, D=orange, E=yellow,
    /// F=green, G=blue, A=purple, B=violet. Sharps/flats slot
    /// between their natural neighbors with a slight desaturate
    /// so the route still has 12 distinct hues without forcing a
    /// new color on every black key.
    static func chromaticColor(for pc: Int) -> NSColor {
        let pcMod = ((pc % 12) + 12) % 12
        switch pcMod {
        case 0:  return NSColor(srgbRed: 1.00, green: 0.20, blue: 0.20, alpha: 1)  // C
        case 1:  return NSColor(srgbRed: 1.00, green: 0.42, blue: 0.10, alpha: 1)  // C#
        case 2:  return NSColor(srgbRed: 1.00, green: 0.63, blue: 0.00, alpha: 1)  // D
        case 3:  return NSColor(srgbRed: 1.00, green: 0.80, blue: 0.00, alpha: 1)  // D#
        case 4:  return NSColor(srgbRed: 1.00, green: 0.90, blue: 0.00, alpha: 1)  // E
        case 5:  return NSColor(srgbRed: 0.20, green: 0.78, blue: 0.20, alpha: 1)  // F
        case 6:  return NSColor(srgbRed: 0.18, green: 0.65, blue: 0.55, alpha: 1)  // F#
        case 7:  return NSColor(srgbRed: 0.20, green: 0.47, blue: 1.00, alpha: 1)  // G
        case 8:  return NSColor(srgbRed: 0.42, green: 0.30, blue: 0.92, alpha: 1)  // G#
        case 9:  return NSColor(srgbRed: 0.51, green: 0.20, blue: 0.78, alpha: 1)  // A
        case 10: return NSColor(srgbRed: 0.62, green: 0.20, blue: 0.78, alpha: 1)  // A#
        case 11: return NSColor(srgbRed: 0.71, green: 0.31, blue: 1.00, alpha: 1)  // B
        default: return .systemGray
        }
    }

    /// Per-family color palette — major = green, minor = blue,
    /// dim = purple, aug = magenta, dominant 7 = orange, maj7 =
    /// teal-green, min7 = cobalt, sus = pink, anything else =
    /// neutral gray.
    static func color(for kind: ChordClimbsView.Route.Kind) -> NSColor {
        switch kind {
        case .major: return NSColor(srgbRed: 0.30, green: 0.78, blue: 0.36, alpha: 1)
        case .minor: return NSColor(srgbRed: 0.22, green: 0.52, blue: 0.95, alpha: 1)
        case .dim:   return NSColor(srgbRed: 0.62, green: 0.30, blue: 0.85, alpha: 1)
        case .aug:   return NSColor(srgbRed: 0.92, green: 0.32, blue: 0.78, alpha: 1)
        case .dom7:  return NSColor(srgbRed: 0.96, green: 0.55, blue: 0.18, alpha: 1)
        case .maj7:  return NSColor(srgbRed: 0.18, green: 0.72, blue: 0.62, alpha: 1)
        case .min7:  return NSColor(srgbRed: 0.18, green: 0.40, blue: 0.85, alpha: 1)
        case .sus:   return NSColor(srgbRed: 0.95, green: 0.40, blue: 0.60, alpha: 1)
        case .other: return NSColor(srgbRed: 0.55, green: 0.55, blue: 0.60, alpha: 1)
        }
    }

    static func pitchClassName(_ pc: Int) -> String {
        let names = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
        return names[((pc % 12) + 12) % 12]
    }
}
