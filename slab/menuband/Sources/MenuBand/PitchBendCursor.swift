import AppKit
import Metal
import MetalKit


// TrackpadSurfaceEnergy and TrackpadMembraneSimulation moved to
// TrackpadSurfacePhysics.swift — they carry no AppKit, and the iOS TrackDrum
// compiles them there. Everything below draws those two on this platform.

private enum TrackpadEnergyVisual {
    static func draw(_ charges: [TrackpadSurfaceEnergy.Charge],
                     in chart: NSRect, accent: NSColor) {
        for charge in charges where charge.level > 0.01 {
            let center = NSPoint(
                x: chart.minX + max(0, min(1, charge.point.x)) * chart.width,
                y: chart.minY + max(0, min(1, charge.point.y)) * chart.height
            )
            let radius = CGFloat(10 + charge.level * 23)
            for layer in stride(from: 6, through: 1, by: -1) {
                let fraction = CGFloat(layer) / 6
                let oval = NSBezierPath(ovalIn: NSRect(
                    x: center.x - radius * 1.64 * fraction,
                    y: center.y - radius * fraction,
                    width: radius * 3.28 * fraction,
                    height: radius * 2 * fraction
                ))
                let alpha = charge.level * Double(7 - layer) * 0.018
                accent.withAlphaComponent(alpha).setFill()
                oval.fill()
            }
        }
    }
}

/// Lights the original drum chart as a height field. Geometry never slides:
/// local slope produces a moving highlight and shadow over the existing ink.
private enum TrackpadMembraneLighting {
    static func draw(_ membrane: TrackpadMembraneSimulation.Snapshot?,
                     in chart: NSRect) {
        guard let membrane, !membrane.isFlat else { return }
        // Render in the simulation's normalized coordinate grid and let
        // AppKit scale the tiny texture. The old code rasterized every logical
        // chart point on every frame (and Retina scaled it once more), doing
        // ~10,000 bilinear samples and allocating ~42 KB at 60 Hz. A 2× grid
        // retains smooth slopes while keeping this decoration out of the
        // notation surface's frame budget.
        let width = max(2, membrane.columns * 2 - 1)
        let height = max(2, membrane.rows * 2 - 1)
        guard let bitmap = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: width, pixelsHigh: height,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
            isPlanar: false, colorSpaceName: .deviceRGB,
            bitmapFormat: [], bytesPerRow: 0, bitsPerPixel: 0
        ), let data = bitmap.bitmapData else { return }
        let dx = 1 / CGFloat(width - 1)
        let dy = 1 / CGFloat(height - 1)
        for row in 0..<height {
            for column in 0..<width {
                let point = CGPoint(
                    x: CGFloat(column) / CGFloat(max(1, width - 1)),
                    y: CGFloat(row) / CGFloat(max(1, height - 1))
                )
                let left = membrane.height(at: CGPoint(x: point.x - dx, y: point.y))
                let right = membrane.height(at: CGPoint(x: point.x + dx, y: point.y))
                let below = membrane.height(at: CGPoint(x: point.x, y: point.y - dy))
                let above = membrane.height(at: CGPoint(x: point.x, y: point.y + dy))
                let center = membrane.height(at: point)
                // A high, upper-left light reveals slope; a small ambient
                // term makes a held depression retain weight at its center.
                let light = Double((left - right) * 1.9 + (above - below) * 2.7
                                   - center * 0.10)
                let alpha = UInt8(min(46, abs(light) * 210))
                // NSBitmapImageRep stores its first scanline at the top while
                // trackpad/AppKit normalized coordinates originate at bottom-
                // left. Flip only the storage row so tap and highlight share
                // the exact same X/Y transform on both 1× and Retina screens.
                let storageRow = height - 1 - row
                let offset = storageRow * bitmap.bytesPerRow + column * 4
                // NSBitmapImageRep defaults to premultiplied alpha: a white
                // overlay stores alpha in RGB as well, never an unbounded 255.
                let channel: UInt8 = light >= 0 ? alpha : 0
                data[offset] = channel
                data[offset + 1] = channel
                data[offset + 2] = channel
                data[offset + 3] = alpha
            }
        }
        let lighting = NSImage(size: NSSize(width: width, height: height))
        lighting.addRepresentation(bitmap)
        let context = NSGraphicsContext.current
        let priorInterpolation = context?.imageInterpolation
        context?.imageInterpolation = .high
        lighting.draw(in: chart,
                      from: NSRect(x: 0, y: 0, width: width, height: height),
                      operation: .sourceOver, fraction: 1)
        if let priorInterpolation { context?.imageInterpolation = priorInterpolation }
    }
}

/// Finger count chooses articulation; vertical position chooses the kit half.
/// Compound two-finger gestures enter a cooldown until that half is clear, so
/// lifting one finger never fires an unintended single-finger hit.
enum TrackpadPercussionPad {
    enum Voice: Hashable {
        case kick, reverseKick, snare, hatClosed, hatOpen

        var pan: UInt8 {
            switch self {
            case .kick, .reverseKick: return 64
            case .snare: return 40
            case .hatClosed, .hatOpen: return 88
            }
        }
    }

    enum TopState: Equatable { case none, kick, reverseKick, cooldown }
    enum BottomState: Equatable { case none, snare, hatClosed, hatOpen, cooldown }

    struct State: Equatable {
        var top: TopState = .none
        var bottom: BottomState = .none

        var voices: Set<Voice> {
            var result: Set<Voice> = []
            if top == .kick { result.insert(.kick) }
            if top == .reverseKick { result.insert(.reverseKick) }
            if bottom == .snare { result.insert(.snare) }
            if bottom == .hatClosed { result.insert(.hatClosed) }
            if bottom == .hatOpen { result.insert(.hatOpen) }
            return result
        }
    }

    struct Transition: Equatable {
        let state: State
        let entered: Set<Voice>
        let exited: Set<Voice>
    }

    static func transition(from previous: State, touches: [CGPoint],
                           began: [CGPoint] = []) -> Transition {
        let top = touches.filter { $0.y >= 0.5 }
        let bottom = touches.filter { $0.y < 0.5 }
        let next = State(top: nextTop(from: previous.top, count: top.count),
                         bottom: nextBottom(from: previous.bottom, touches: bottom))
        let oldVoices = previous.voices
        let newVoices = next.voices
        var entered = newVoices.subtracting(oldVoices)
        var exited = oldVoices.subtracting(newVoices)
        // A hardware contact can end and a replacement begin inside one 8 ms
        // frame, leaving both finger count and articulation unchanged. Native
        // begin state/identity makes that a real retrigger, not a held pad.
        for point in began {
            let voice: Voice?
            if point.y >= 0.5 {
                switch next.top {
                case .kick: voice = .kick
                case .reverseKick: voice = .reverseKick
                default: voice = nil
                }
            } else {
                switch next.bottom {
                case .snare: voice = .snare
                case .hatClosed: voice = .hatClosed
                case .hatOpen: voice = .hatOpen
                default: voice = nil
                }
            }
            if let voice, oldVoices.contains(voice), newVoices.contains(voice) {
                exited.insert(voice)
                entered.insert(voice)
            }
        }
        return Transition(state: next,
                          entered: entered,
                          exited: exited)
    }

    private static func nextTop(from previous: TopState, count: Int) -> TopState {
        if count == 0 { return .none }
        if count >= 2 { return .reverseKick }
        if previous == .reverseKick || previous == .cooldown { return .cooldown }
        return .kick
    }

    private static func nextBottom(from previous: BottomState,
                                   touches: [CGPoint]) -> BottomState {
        if touches.isEmpty { return .none }
        if touches.count >= 2 { return .hatOpen }
        if previous == .hatOpen || previous == .cooldown { return .cooldown }
        return touches[0].x < 0.5 ? .snare : .hatClosed
    }

    static func image(touches: [CGPoint], state: State) -> NSImage {
        let size = PitchBendCursor.cursorSize
        let appearance = NSApp.effectiveAppearance
        let isDark = appearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        return NSImage(size: size, flipped: false) { rect in
            if #available(macOS 11.0, *) {
                appearance.performAsCurrentDrawingAppearance {
                    draw(in: rect, touches: touches, state: state, isDark: isDark)
                }
            } else {
                draw(in: rect, touches: touches, state: state, isDark: isDark)
            }
            return true
        }
    }

    private static func draw(in rect: NSRect, touches: [CGPoint],
                             state: State, isDark: Bool) {
        let chart = rect.insetBy(dx: 4, dy: 4)
        let body = NSBezierPath(roundedRect: chart, xRadius: 7, yRadius: 7)
        let top = NSRect(x: chart.minX, y: chart.midY,
                         width: chart.width, height: chart.height / 2)
        let snare = NSRect(x: chart.minX, y: chart.minY,
                           width: chart.width / 2, height: chart.height / 2)
        let hat = NSRect(x: chart.midX, y: chart.minY,
                         width: chart.width / 2, height: chart.height / 2)
        let open = state.bottom == .hatOpen

        NSGraphicsContext.saveGraphicsState()
        body.addClip()
        drawPad(top, label: state.top == .reverseKick ? "REV" : "KIK",
                active: state.top == .kick || state.top == .reverseKick, isDark: isDark)
        if open {
            drawPad(NSRect(x: chart.minX, y: chart.minY,
                           width: chart.width, height: chart.height / 2),
                    label: "OPEN", active: true, isDark: isDark)
        } else {
            drawPad(snare, label: "SNR", active: state.bottom == .snare, isDark: isDark)
            drawPad(hat, label: "HAT", active: state.bottom == .hatClosed, isDark: isDark)
        }

        let groove = (isDark ? NSColor.black : NSColor.brown)
            .withAlphaComponent(isDark ? 0.55 : 0.40)
        groove.setStroke()
        let horizontal = NSBezierPath()
        horizontal.move(to: NSPoint(x: chart.minX, y: chart.midY))
        horizontal.line(to: NSPoint(x: chart.maxX, y: chart.midY))
        horizontal.lineWidth = 1
        horizontal.stroke()
        if !open {
            let vertical = NSBezierPath()
            vertical.move(to: NSPoint(x: chart.midX, y: chart.minY))
            vertical.line(to: NSPoint(x: chart.midX, y: chart.midY))
            vertical.lineWidth = 1
            vertical.stroke()
        }
        NSGraphicsContext.restoreGraphicsState()

        for touch in touches {
            let px = chart.minX + max(0, min(1, touch.x)) * chart.width
            let py = chart.minY + max(0, min(1, touch.y)) * chart.height
            let dot = NSBezierPath(ovalIn: NSRect(x: px - 4, y: py - 4,
                                                  width: 8, height: 8))
            NSColor.white.withAlphaComponent(0.82).setFill()
            dot.fill()
        }
        NSColor.controlAccentColor.withAlphaComponent(0.95).setStroke()
        body.lineWidth = 2
        body.stroke()
    }

    private static func drawPad(_ rect: NSRect, label: String,
                                active: Bool, isDark: Bool) {
        let accent = NSColor.controlAccentColor
        let top: NSColor
        let bottom: NSColor
        if active {
            top = accent.blended(withFraction: 0.38, of: .white) ?? accent
            bottom = accent
        } else if isDark {
            top = NSColor(white: 0.26, alpha: 1)
            bottom = NSColor(white: 0.13, alpha: 1)
        } else {
            top = NSColor(srgbRed: 0.99, green: 0.97, blue: 0.91, alpha: 1)
            bottom = NSColor(srgbRed: 0.90, green: 0.86, blue: 0.76, alpha: 1)
        }
        NSGradient(starting: top, ending: bottom)?.draw(in: rect, angle: -90)
        let paragraph = NSMutableParagraphStyle()
        paragraph.alignment = .center
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedSystemFont(ofSize: 9, weight: .bold),
            .foregroundColor: active ? NSColor.white
                : (isDark ? NSColor(white: 0.92, alpha: 1)
                          : NSColor(srgbRed: 0.24, green: 0.19, blue: 0.12, alpha: 1)),
            .paragraphStyle: paragraph,
        ]
        label.draw(in: NSRect(x: rect.minX, y: rect.midY - 5,
                              width: rect.width, height: 12), withAttributes: attrs)
    }
}

enum TrackpadDrumSkinPad {
    struct ScratchMotion {
        let point: CGPoint
        let movement: CGFloat
        let delta: CGVector
        let anchors: [CGPoint]
    }

    static func newStrikes(previous: [CGPoint], current: [CGPoint]) -> [CGPoint] {
        guard current.count > previous.count else { return [] }
        var unmatched = current
        for old in previous {
            guard let nearest = unmatched.indices.min(by: {
                distanceSquared(unmatched[$0], old) < distanceSquared(unmatched[$1], old)
            }) else { break }
            unmatched.remove(at: nearest)
        }
        return unmatched
    }

    static func liftedTouches(previous: [CGPoint], current: [CGPoint]) -> [CGPoint] {
        guard previous.count > current.count else { return [] }
        var unmatched = previous
        for held in current {
            guard let nearest = unmatched.indices.min(by: {
                distanceSquared(unmatched[$0], held)
                    < distanceSquared(unmatched[$1], held)
            }) else { break }
            unmatched.remove(at: nearest)
        }
        return unmatched
    }

    /// Mean per-finger travel between hardware frames. Nearest-neighbor
    /// matching survives finger-array reordering; returning the exact measured
    /// distance (including zero) leaves scratch gain continuous and dead-zone
    /// free.
    static func averageMovement(previous: [CGPoint], current: [CGPoint]) -> CGFloat {
        guard !previous.isEmpty, !current.isEmpty else { return 0 }
        var unmatched = current
        var distance: CGFloat = 0
        var matches = 0
        for old in previous {
            guard let nearest = unmatched.indices.min(by: {
                distanceSquared(unmatched[$0], old) < distanceSquared(unmatched[$1], old)
            }) else { break }
            distance += sqrt(distanceSquared(unmatched[nearest], old))
            unmatched.remove(at: nearest)
            matches += 1
        }
        return matches > 0 ? distance / CGFloat(matches) : 0
    }

    /// Locate the contact that moved most after nearest-neighbor matching.
    /// Every other current contact becomes a tension anchor.
    static func dominantScratch(previous: [CGPoint], current: [CGPoint]) -> ScratchMotion? {
        guard !previous.isEmpty, !current.isEmpty else { return nil }
        var available = Array(current.indices)
        var dominantIndex: Int?
        var dominantDistance: CGFloat = -1
        var dominantDelta = CGVector.zero
        for old in previous {
            guard let availablePosition = available.indices.min(by: {
                distanceSquared(current[available[$0]], old)
                    < distanceSquared(current[available[$1]], old)
            }) else { break }
            let currentIndex = available.remove(at: availablePosition)
            let movement = sqrt(distanceSquared(current[currentIndex], old))
            if movement > dominantDistance {
                dominantDistance = movement
                dominantIndex = currentIndex
                dominantDelta = CGVector(dx: current[currentIndex].x - old.x,
                                         dy: current[currentIndex].y - old.y)
            }
        }
        guard let dominantIndex else { return nil }
        return ScratchMotion(
            point: current[dominantIndex],
            movement: max(0, dominantDistance),
            delta: dominantDelta,
            anchors: current.enumerated().compactMap {
                $0.offset == dominantIndex ? nil : $0.element
            }
        )
    }

    private static func distanceSquared(_ a: CGPoint, _ b: CGPoint) -> CGFloat {
        let dx = a.x - b.x, dy = a.y - b.y
        return dx * dx + dy * dy
    }

    static func image(touches: [CGPoint],
                      energy: [TrackpadSurfaceEnergy.Charge] = [],
                      membrane: TrackpadMembraneSimulation.Snapshot? = nil,
                      appearance: NSAppearance? = nil) -> NSImage {
        // 1.64:1, matching the physical trackpad rather than the square FX
        // cursor. Large enough to read the four inset material contours.
        let size = NSSize(width: 140, height: 88)
        // Overlay panels can live on a screen whose appearance differs from
        // NSApp's cached effective appearance. Resolve from the presenting
        // view when supplied so light mode never receives a dark skin texture.
        let appearance = appearance ?? NSApp.effectiveAppearance
        let isDark = appearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        return NSImage(size: size, flipped: false) { rect in
            if #available(macOS 11.0, *) {
                appearance.performAsCurrentDrawingAppearance {
                    draw(in: rect, touches: touches, energy: energy,
                         membrane: membrane, isDark: isDark)
                }
            } else {
                draw(in: rect, touches: touches, energy: energy,
                     membrane: membrane, isDark: isDark)
            }
            return true
        }
    }

    private static func draw(in rect: NSRect, touches: [CGPoint],
                             energy: [TrackpadSurfaceEnergy.Charge],
                             membrane: TrackpadMembraneSimulation.Snapshot?,
                             isDark: Bool) {
        // Match the physical trackpad's wide rounded rectangle instead of
        // depicting the synthesis surface as a circular drum.
        let chart = rect.insetBy(dx: 4, dy: 4)
        let body = NSBezierPath(roundedRect: chart, xRadius: 8, yRadius: 8)
        let accent = NSColor.controlAccentColor
        // These insets mirror the synthesis thresholds in
        // MenuBandPercussion.drumSkinZone: click → hat → snare → tom → kick.
        // Equal pixel insets are geometrically correct because the chart and
        // physical trackpad share the same 1.64:1 aspect ratio.
        func insetZone(_ inset: CGFloat) -> NSBezierPath {
            NSBezierPath(roundedRect: chart.insetBy(dx: inset, dy: inset),
                         xRadius: max(2, 8 - inset * 0.16),
                         yRadius: max(2, 8 - inset * 0.16))
        }
        // 12% click rail, 24% hat, 18% rim, 16% snare, 30% kick core.
        // Insets use half the chart height because the synthesis coordinate
        // reaches 1.0 from the centroid to any physical edge.
        let hatZone = insetZone(4.8)
        let rimZone = insetZone(14.4)
        let snareZone = insetZone(21.6)
        let kickZone = insetZone(28)
        // Preserve the original natural material palette. System accent is a
        // performance signal on the rim and contacts, not the skin's pigment.
        let clickColor = isDark
            ? NSColor(srgbRed: 0.48, green: 0.43, blue: 0.34, alpha: 1)
            : NSColor(srgbRed: 0.97, green: 0.91, blue: 0.77, alpha: 1) // cream
        let hatColor = isDark
            ? NSColor(srgbRed: 0.34, green: 0.39, blue: 0.29, alpha: 1)
            : NSColor(srgbRed: 0.72, green: 0.77, blue: 0.62, alpha: 1) // sage
        let rimColor = isDark
            ? NSColor(srgbRed: 0.43, green: 0.34, blue: 0.20, alpha: 1)
            : NSColor(srgbRed: 0.84, green: 0.68, blue: 0.40, alpha: 1) // ochre
        let snareColor = isDark
            ? NSColor(srgbRed: 0.43, green: 0.25, blue: 0.17, alpha: 1)
            : NSColor(srgbRed: 0.76, green: 0.45, blue: 0.30, alpha: 1) // terracotta
        let kickColor = isDark
            ? NSColor(srgbRed: 0.24, green: 0.15, blue: 0.10, alpha: 1)
            : NSColor(srgbRed: 0.48, green: 0.29, blue: 0.19, alpha: 1) // umber
        NSGraphicsContext.saveGraphicsState()
        body.addClip()
        clickColor.setFill(); body.fill()
        hatColor.setFill(); hatZone.fill()
        snareColor.setFill(); rimZone.fill()

        // Dense parallel wires immediately read as the snare material. The
        // inner tom and kick fills below mask them out of the center.
        NSGraphicsContext.saveGraphicsState()
        rimZone.addClip()
        let wires = NSBezierPath()
        stride(from: chart.minX - chart.height,
               through: chart.maxX, by: 6).forEach { x in
            wires.move(to: NSPoint(x: x, y: chart.minY))
            wires.line(to: NSPoint(x: x + chart.height, y: chart.maxY))
        }
        (isDark ? NSColor.white : NSColor.black)
            .withAlphaComponent(0.16).setStroke()
        wires.lineWidth = 0.55
        wires.stroke()
        NSGraphicsContext.restoreGraphicsState()

        rimColor.setFill(); snareZone.fill()
        kickColor.setFill(); kickZone.fill()

        // Hat teeth span the widened playable metal band; the final bright rail is
        // the hard chassis click. This remains legible at the overlay's actual
        // size and makes the two edge instruments visually different.
        let teeth = NSBezierPath()
        stride(from: chart.minX + 9, through: chart.maxX - 9, by: 9).forEach { x in
            let slant: CGFloat = Int((x - chart.minX) / 9).isMultiple(of: 2) ? 2 : -2
            teeth.move(to: NSPoint(x: x, y: chart.maxY - 5.2))
            teeth.line(to: NSPoint(x: x + slant, y: chart.maxY - 13.6))
            teeth.move(to: NSPoint(x: x, y: chart.minY + 5.2))
            teeth.line(to: NSPoint(x: x - slant, y: chart.minY + 13.6))
        }
        stride(from: chart.minY + 10, through: chart.maxY - 10, by: 9).forEach { y in
            let slant: CGFloat = Int((y - chart.minY) / 9).isMultiple(of: 2) ? 2 : -2
            teeth.move(to: NSPoint(x: chart.minX + 5.2, y: y))
            teeth.line(to: NSPoint(x: chart.minX + 13.6, y: y + slant))
            teeth.move(to: NSPoint(x: chart.maxX - 5.2, y: y))
            teeth.line(to: NSPoint(x: chart.maxX - 13.6, y: y - slant))
        }
        clickColor.withAlphaComponent(0.72).setStroke()
        teeth.lineWidth = 1.25
        teeth.stroke()

        let clickRail = NSBezierPath(roundedRect: chart.insetBy(dx: 1.5, dy: 1.5),
                                     xRadius: 6.5, yRadius: 6.5)
        (isDark ? NSColor.white : NSColor.black)
            .withAlphaComponent(0.62).setStroke()
        clickRail.lineWidth = 2.2
        clickRail.stroke()

        TrackpadEnergyVisual.draw(energy, in: chart, accent: accent)
        TrackpadMembraneLighting.draw(membrane, in: chart)
        let boundaryColor = (isDark ? NSColor.white : NSColor.black)
            .withAlphaComponent(isDark ? 0.32 : 0.50)
        boundaryColor.setStroke()
        for (zone, width) in [(hatZone, 0.9), (rimZone, 2.0),
                              (snareZone, 0.9), (kickZone, 1.3)] {
            zone.lineWidth = width
            zone.stroke()
        }
        NSGraphicsContext.restoreGraphicsState()

        let mapped = touches.map {
            NSPoint(x: chart.minX + max(0, min(1, $0.x)) * chart.width,
                    y: chart.minY + max(0, min(1, $0.y)) * chart.height)
        }
        if touches.count > 1 {
            for i in 0..<(touches.count - 1) {
                for j in (i + 1)..<touches.count {
                    let dx = Double(touches[i].x - touches[j].x) * 1.64
                    let dy = Double(touches[i].y - touches[j].y)
                    let proximity = 1.0 - min(1.0, hypot(dx, dy))
                    let tether = NSBezierPath()
                    tether.move(to: mapped[i])
                    tether.line(to: mapped[j])
                    accent.withAlphaComponent(0.22 + proximity * 0.50).setStroke()
                    tether.lineWidth = 0.7 + proximity * 1.8
                    tether.stroke()

                    let midpoint = NSPoint(x: (mapped[i].x + mapped[j].x) / 2,
                                           y: (mapped[i].y + mapped[j].y) / 2)
                    let knotRadius = 1.2 + proximity * 1.8
                    let knot = NSBezierPath(ovalIn: NSRect(
                        x: midpoint.x - knotRadius, y: midpoint.y - knotRadius,
                        width: knotRadius * 2, height: knotRadius * 2
                    ))
                    accent.withAlphaComponent(0.30 + proximity * 0.42).setFill()
                    knot.fill()
                }
            }
        }
        for (index, point) in mapped.enumerated() {
            let anchors = touches.enumerated().compactMap {
                $0.offset == index ? nil : $0.element
            }
            let retained = TrackpadSurfaceEnergy.energy(at: touches[index],
                                                        charges: energy)
            let velocity = MenuBandPercussion.surfaceVelocityEnergy(
                at: touches[index], anchors: anchors, inertia: retained
            )
            let radius = CGFloat(2.5 + velocity * 3.2)
            let dot = NSBezierPath(ovalIn: NSRect(x: point.x - radius,
                                                  y: point.y - radius,
                                                  width: radius * 2,
                                                  height: radius * 2))
            accent.withAlphaComponent(0.55 + velocity * 0.42).setFill()
            dot.fill()
            NSColor.white.withAlphaComponent(0.85).setStroke()
            dot.lineWidth = 0.8
            dot.stroke()
        }
        accent.withAlphaComponent(0.95).setStroke()
        body.lineWidth = 2
        body.stroke()
    }
}

/// Electronic surface display: the same physical footprint and inset spatial
/// grammar as the skin, but a phase grid and inharmonic nodes distinguish it
/// without borrowing acoustic drum labels.
enum TrackpadSynthPad {
    static func image(touches: [CGPoint],
                      energy: [TrackpadSurfaceEnergy.Charge] = []) -> NSImage {
        let size = NSSize(width: 140, height: 88)
        let appearance = NSApp.effectiveAppearance
        let isDark = appearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        return NSImage(size: size, flipped: false) { rect in
            if #available(macOS 11.0, *) {
                appearance.performAsCurrentDrawingAppearance {
                    draw(in: rect, touches: touches, energy: energy, isDark: isDark)
                }
            } else {
                draw(in: rect, touches: touches, energy: energy, isDark: isDark)
            }
            return true
        }
    }

    private static func draw(in rect: NSRect, touches: [CGPoint],
                             energy: [TrackpadSurfaceEnergy.Charge], isDark: Bool) {
        let chart = rect.insetBy(dx: 4, dy: 4)
        let body = NSBezierPath(roundedRect: chart, xRadius: 8, yRadius: 8)
        let accent = NSColor.controlAccentColor
        let cyan = NSColor(srgbRed: 0.08, green: 0.86, blue: 0.96, alpha: 1)
        let magenta = NSColor(srgbRed: 0.96, green: 0.16, blue: 0.72, alpha: 1)
        let base = isDark
            ? NSColor(srgbRed: 0.035, green: 0.045, blue: 0.080, alpha: 1)
            : NSColor(srgbRed: 0.91, green: 0.95, blue: 1.0, alpha: 1)

        NSGraphicsContext.saveGraphicsState()
        body.addClip()
        base.setFill()
        body.fill()

        for (index, inset) in ([3, 8, 14, 22] as [CGFloat]).enumerated() {
            let zoneRect = chart.insetBy(dx: inset, dy: inset)
            let zone = NSBezierPath(roundedRect: zoneRect,
                                    xRadius: max(2, 8 - inset * 0.18),
                                    yRadius: max(2, 8 - inset * 0.18))
            let hue = index.isMultiple(of: 2)
                ? cyan : magenta
            hue.withAlphaComponent(isDark ? 0.12 : 0.10).setFill()
            zone.fill()
            hue.withAlphaComponent(isDark ? 0.55 : 0.42).setStroke()
            zone.lineWidth = 0.8
            zone.stroke()
        }
        TrackpadEnergyVisual.draw(energy, in: chart, accent: cyan)

        let grid = NSBezierPath()
        for fraction in [0.2, 0.4, 0.6, 0.8] as [CGFloat] {
            let x = chart.minX + chart.width * fraction
            grid.move(to: NSPoint(x: x, y: chart.minY))
            grid.line(to: NSPoint(x: chart.maxX - chart.width * fraction,
                                  y: chart.maxY))
        }
        cyan.withAlphaComponent(isDark ? 0.24 : 0.17).setStroke()
        grid.lineWidth = 0.55
        grid.stroke()
        NSGraphicsContext.restoreGraphicsState()

        let mapped = touches.map {
            NSPoint(x: chart.minX + max(0, min(1, $0.x)) * chart.width,
                    y: chart.minY + max(0, min(1, $0.y)) * chart.height)
        }
        if mapped.count > 1 {
            let links = NSBezierPath()
            links.move(to: mapped[0])
            for point in mapped.dropFirst() { links.line(to: point) }
            magenta.withAlphaComponent(0.58).setStroke()
            links.lineWidth = 1
            links.stroke()
        }
        for (index, point) in mapped.enumerated() {
            let anchors = touches.enumerated().compactMap {
                $0.offset == index ? nil : $0.element
            }
            let retained = TrackpadSurfaceEnergy.energy(at: touches[index],
                                                        charges: energy)
            let velocity = MenuBandPercussion.surfaceVelocityEnergy(
                at: touches[index], anchors: anchors, inertia: retained
            )
            let radius = CGFloat(2.5 + velocity * 3.2)
            let halo = NSBezierPath(ovalIn: NSRect(x: point.x - radius - 3,
                                                   y: point.y - radius - 3,
                                                   width: (radius + 3) * 2,
                                                   height: (radius + 3) * 2))
            accent.withAlphaComponent(0.12 + velocity * 0.20).setFill()
            halo.fill()
            let core = NSBezierPath(ovalIn: NSRect(x: point.x - radius,
                                                   y: point.y - radius,
                                                   width: radius * 2,
                                                   height: radius * 2))
            NSColor.white.withAlphaComponent(0.90).setFill()
            core.fill()
        }
        accent.withAlphaComponent(0.95).setStroke()
        body.lineWidth = 2
        body.stroke()
    }
}

/// Custom cursor replacement used while the trackpad bend gesture
/// is engaged. It renders as a small XY modulation pad — frozen at
/// the lock point — with a puck that slides up/down to show the
/// current pitch-bend and right to show echo amount. The puck IS
/// the live state visualisation; the chart itself never moves, so
/// the user has a stable reference frame to read both axes
/// against as the audio rubber-bands back to neutral on release.
enum PitchBendCursor {
    /// Centered, no-bend, no-echo cursor — used as the push baseline
    /// fallback for in-app cursorUpdate handlers (the actual
    /// visual live one is the floating overlay window).
    static let neutral: NSCursor = cursor(forBend: 0, echo: 0)

    /// Hot-spot at the chart's center so the overlay window anchors
    /// the chart directly over the user's frozen cursor position.
    static let hotSpot = NSPoint(x: 40, y: 40)
    static let cursorSize = NSSize(width: 80, height: 80)

    static func image(forBend amount: Float) -> NSImage {
        buildImage(bend: CGFloat(amount), echo: 0, keyDown: false)
    }

    static func image(forBend bend: Float, echo: Float,
                      keyDown: Bool = false) -> NSImage {
        buildImage(bend: CGFloat(bend), echo: CGFloat(echo), keyDown: keyDown)
    }

    static func cursor(forBend amount: Float) -> NSCursor {
        cursor(forBend: amount, echo: 0)
    }

    static func cursor(forBend bend: Float, echo: Float) -> NSCursor {
        NSCursor(image: image(forBend: bend, echo: echo), hotSpot: hotSpot)
    }

    private static func buildImage(bend: CGFloat, echo: CGFloat,
                                   keyDown: Bool) -> NSImage {
        let bendC = max(-1, min(1, bend))
        // `echo` is the bipolar fx-X driver in [-1, +1]: positive
        // (right) is echo, negative (left) is space/reverb. We keep
        // the parameter name `echo` for source-call stability; the
        // chart treats it as a signed X value.
        let xC = max(-1, min(1, echo))
        let size = cursorSize
        // Theme off the live system appearance. The overlay image is
        // rebuilt on every move, so this re-reads each frame and even
        // tracks a light/dark flip mid-gesture. Drawing inside the
        // appearance also resolves `controlAccentColor` correctly.
        let appearance = NSApp.effectiveAppearance
        let isDark = appearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        return NSImage(size: size, flipped: false) { rect in
            if #available(macOS 11.0, *) {
                appearance.performAsCurrentDrawingAppearance {
                    drawChart(in: rect, bend: bendC, echo: xC, isDark: isDark,
                              keyDown: keyDown)
                }
            } else {
                drawChart(in: rect, bend: bendC, echo: xC, isDark: isDark,
                          keyDown: keyDown)
            }
            return true
        }
    }

    private static func drawChart(in rect: NSRect, bend: CGFloat, echo: CGFloat,
                                  isDark: Bool, keyDown: Bool) {
        // A Menu Band keycap plate, cleanly divided into four quadrants by a
        // single thin cross (no axis labels, no arrowheads, no end-caps). The
        // puck carries the live bend (Y) / echo (X) and lights up with the
        // accent color while a note key is held.
        let chart = rect.insetBy(dx: 4, dy: 4)
        let radius: CGFloat = 7
        let body = NSBezierPath(roundedRect: chart, xRadius: radius, yRadius: radius)

        // Plate gradient (lighter at top) — cream "white key" in light
        // mode, charcoal key in dark mode.
        let plateTop = isDark
            ? NSColor(white: 0.26, alpha: 1)
            : NSColor(srgbRed: 0.99, green: 0.97, blue: 0.91, alpha: 1)
        let plateBot = isDark
            ? NSColor(white: 0.13, alpha: 1)
            : NSColor(srgbRed: 0.90, green: 0.86, blue: 0.76, alpha: 1)
        let cx = chart.midX, cy = chart.midY

        NSGraphicsContext.saveGraphicsState()
        body.addClip()
        NSGradient(starting: plateTop, ending: plateBot)?.draw(in: chart, angle: -90)

        // Clean dividing cross — one thin groove per axis, spanning the full
        // plate (clipped to the rounded body), nothing on the ends.
        let groove = (isDark ? NSColor.black
                             : NSColor(srgbRed: 0.42, green: 0.36, blue: 0.26, alpha: 1))
            .withAlphaComponent(isDark ? 0.55 : 0.40)
        groove.setStroke()
        let hLine = NSBezierPath()
        hLine.move(to: NSPoint(x: chart.minX, y: cy))
        hLine.line(to: NSPoint(x: chart.maxX, y: cy))
        hLine.lineWidth = 1
        hLine.stroke()
        let vLine = NSBezierPath()
        vLine.move(to: NSPoint(x: cx, y: chart.minY))
        vLine.line(to: NSPoint(x: cx, y: chart.maxY))
        vLine.lineWidth = 1
        vLine.stroke()
        NSGraphicsContext.restoreGraphicsState()

        // Puck. Position carries the live bend/echo. Default = a glossy
        // "black key" puck; while a key is held it glows in the accent color.
        let accent = NSColor.controlAccentColor
        let puckR: CGFloat = 6
        let halfW = chart.width / 2 - puckR - 3
        let halfH = chart.height / 2 - puckR - 3
        let puckRect = NSRect(x: cx + echo * halfW - puckR,
                              y: cy + bend * halfH - puckR,
                              width: puckR * 2, height: puckR * 2)
        let knob = NSBezierPath(roundedRect: puckRect, xRadius: 3, yRadius: 3)
        NSGraphicsContext.saveGraphicsState()
        let shadow = NSShadow()
        if keyDown {
            shadow.shadowColor = accent.withAlphaComponent(0.9)
            shadow.shadowOffset = .zero
            shadow.shadowBlurRadius = 5
        } else {
            shadow.shadowColor = NSColor.black.withAlphaComponent(0.5)
            shadow.shadowOffset = NSSize(width: 0, height: -1)
            shadow.shadowBlurRadius = 2
        }
        shadow.set()
        knob.addClip()
        if keyDown {
            let top = accent.blended(withFraction: 0.4, of: .white) ?? accent
            NSGradient(starting: top, ending: accent)?.draw(in: puckRect, angle: -90)
        } else {
            NSGradient(starting: NSColor(white: 0.28, alpha: 1),
                       ending: NSColor(white: 0.07, alpha: 1))?.draw(in: puckRect, angle: -90)
        }
        NSGraphicsContext.restoreGraphicsState()
        (keyDown ? (accent.blended(withFraction: 0.4, of: .black) ?? accent)
                 : NSColor.black.withAlphaComponent(0.9)).setStroke()
        knob.lineWidth = keyDown ? 1.0 : 0.8
        knob.stroke()

        // Keycap outline last so the whole pad is framed like a key.
        let edge = isDark
            ? NSColor.black.withAlphaComponent(0.85)
            : NSColor(srgbRed: 0.34, green: 0.28, blue: 0.18, alpha: 0.85)
        edge.setStroke(); body.lineWidth = 1.3; body.stroke()
    }
}

/// The upgrade card is one large web link, including its button. Its cursor
/// makes that affordance explicit without changing cursor behavior for any
/// playable surface.
private final class TrackDrumInstallCard: NSVisualEffectView {
    var onActivate: (() -> Void)?

    override func resetCursorRects() {
        super.resetCursorRects()
        addCursorRect(bounds, cursor: .pointingHand)
    }

    override func mouseDown(with event: NSEvent) {
        onActivate?()
    }
}

/// Borderless transparent panel that draws the trackpad surface beneath Menu
/// Band. Floats above every app so the
/// chart stays visible regardless of which window the mouse is
/// over — pair with `CGDisplayHideCursor` to hide the real system
/// cursor so the chart visibly replaces it.
final class PitchBendCursorOverlayWindow: NSPanel {
    static let trackDrumInstallSize = NSSize(width: 260, height: 126)
    static let polyrhythmGap: CGFloat = 12

    private let imageView = NSImageView()
    private let tracktrampView = TracktrampMetalView()
    private let polyrhythmView = PolyrhythmTrainerView()
    private let installCard = TrackDrumInstallCard()
    private let installIcon = NSImageView()
    private let installTagline = NSTextField(
        labelWithString: "Play percussion with your trackpad!"
    )
    private let installButton = NSButton()
    private var installAction: (() -> Void)?
    private var anchorScreenPoint = NSPoint.zero
    private var fadeTimer: Timer?

    init() {
        let frame = NSRect(origin: .zero, size: PitchBendCursor.cursorSize)
        super.init(contentRect: frame,
                   styleMask: [.borderless, .nonactivatingPanel],
                   backing: .buffered,
                   defer: false)
        isOpaque = false
        backgroundColor = .clear
        hasShadow = false
        // Float above every other window in every space, never
        // steal focus, never accept mouse clicks (so the trackpad
        // gesture still routes to whatever app is below).
        level = .screenSaver
        ignoresMouseEvents = true
        hidesOnDeactivate = false
        collectionBehavior = [.canJoinAllSpaces,
                              .stationary,
                              .ignoresCycle,
                              .fullScreenAuxiliary]
        imageView.frame = frame
        imageView.imageScaling = .scaleNone
        contentView?.addSubview(imageView)
        tracktrampView.frame = frame
        tracktrampView.isHidden = true
        contentView?.addSubview(tracktrampView)
        polyrhythmView.isHidden = true
        contentView?.addSubview(polyrhythmView)

        installCard.material = .popover
        installCard.blendingMode = .behindWindow
        installCard.state = .active
        installCard.wantsLayer = true
        installCard.layer?.cornerRadius = 10
        installCard.layer?.masksToBounds = true
        installCard.isHidden = true
        installCard.toolTip = "Open the TrackDrum page"
        installCard.onActivate = { [weak self] in self?.installTrackDrum() }
        contentView?.addSubview(installCard)

        installIcon.image = NSImage(
            systemSymbolName: "hand.tap.fill",
            accessibilityDescription: "Trackpad percussion"
        )
        installIcon.contentTintColor = .controlAccentColor
        installIcon.imageScaling = .scaleProportionallyUpOrDown
        installCard.addSubview(installIcon)

        installTagline.font = .systemFont(ofSize: 14, weight: .semibold)
        installTagline.textColor = .labelColor
        installTagline.maximumNumberOfLines = 2
        installTagline.lineBreakMode = .byWordWrapping
        installTagline.setAccessibilityLabel("Play percussion with your trackpad!")
        installCard.addSubview(installTagline)

        installButton.title = "Install TrackDrum"
        installButton.bezelStyle = .rounded
        installButton.controlSize = .large
        installButton.font = .systemFont(ofSize: 13, weight: .semibold)
        installButton.target = self
        installButton.action = #selector(installTrackDrum)
        installButton.setAccessibilityLabel("Install TrackDrum for Menu Band")
        installCard.addSubview(installButton)
    }

    @objc private func installTrackDrum() {
        installAction?()
    }

    private func showImageSurface() {
        ignoresMouseEvents = true
        installAction = nil
        installCard.isHidden = true
        tracktrampView.isHidden = true
        polyrhythmView.isHidden = true
        imageView.isHidden = false
    }

    /// `screenPoint` is the absolute center chosen by AppDelegate, normally
    /// directly beneath the Menu Band status item.
    func show(image: NSImage, atScreenPoint screenPoint: NSPoint) {
        fadeTimer?.invalidate()
        fadeTimer = nil
        anchorScreenPoint = screenPoint
        showImageSurface()
        apply(image: image)
        alphaValue = 1
        if !isVisible { orderFrontRegardless() }
    }

    /// Update only the chart image (puck position changes); window
    /// position stays put unless the caller supplies a refreshed anchor.
    func update(image: NSImage) {
        showImageSurface()
        apply(image: image)
    }

    func update(image: NSImage, atScreenPoint screenPoint: NSPoint) {
        anchorScreenPoint = screenPoint
        showImageSurface()
        apply(image: image)
    }

    /// Present the physical skin through the cached Metal texture. Only the
    /// small height field changes between frames; no NSImage is constructed.
    func showTracktramp(_ membrane: TrackpadMembraneSimulation.Snapshot,
                        touches: [CGPoint],
                        polyrhythm: PolyrhythmTrainerSnapshot? = nil,
                        atScreenPoint screenPoint: NSPoint) {
        fadeTimer?.invalidate()
        fadeTimer = nil
        anchorScreenPoint = screenPoint
        ignoresMouseEvents = true
        installAction = nil
        installCard.isHidden = true
        applyTracktrampFrame(polyrhythm: polyrhythm)
        imageView.isHidden = true
        tracktrampView.isHidden = false
        tracktrampView.update(membrane, touches: touches)
        polyrhythmView.snapshot = polyrhythm
        alphaValue = 1
        if !isVisible { orderFrontRegardless() }
    }

    func updateTracktramp(_ membrane: TrackpadMembraneSimulation.Snapshot,
                          touches: [CGPoint],
                          polyrhythm: PolyrhythmTrainerSnapshot? = nil,
                          atScreenPoint screenPoint: NSPoint) {
        anchorScreenPoint = screenPoint
        ignoresMouseEvents = true
        installAction = nil
        installCard.isHidden = true
        applyTracktrampFrame(polyrhythm: polyrhythm)
        imageView.isHidden = true
        tracktrampView.isHidden = false
        tracktrampView.update(membrane, touches: touches)
        polyrhythmView.snapshot = polyrhythm
    }

    /// Missing-helper state for the Tab-selected percussion surface. This is
    /// the one overlay state that accepts a click; every playable surface
    /// remains click-through.
    func showTrackDrumInstall(atScreenPoint screenPoint: NSPoint,
                              onInstall: @escaping () -> Void) {
        fadeTimer?.invalidate()
        fadeTimer = nil
        anchorScreenPoint = screenPoint
        installAction = onInstall
        imageView.isHidden = true
        tracktrampView.isHidden = true
        polyrhythmView.isHidden = true
        installCard.isHidden = false
        ignoresMouseEvents = false

        let size = Self.trackDrumInstallSize
        setFrame(NSRect(x: screenPoint.x - size.width / 2,
                        y: screenPoint.y - size.height / 2,
                        width: size.width, height: size.height), display: false)
        installCard.frame = NSRect(origin: .zero, size: size)
        installCard.discardCursorRects()
        installCard.resetCursorRects()
        installIcon.frame = NSRect(x: 18, y: 71, width: 38, height: 38)
        installTagline.frame = NSRect(x: 68, y: 70,
                                      width: size.width - 84, height: 40)
        installButton.sizeToFit()
        let buttonSize = NSSize(width: size.width - 36, height: 36)
        installButton.frame = NSRect(
            x: (size.width - buttonSize.width) / 2,
            y: 18,
            width: buttonSize.width,
            height: buttonSize.height
        )
        alphaValue = 1
        if !isVisible { orderFrontRegardless() }
    }

    private func applyTracktrampFrame(polyrhythm: PolyrhythmTrainerSnapshot?) {
        let size = Self.tracktrampSurfaceSize(polyrhythm: polyrhythm)
        let panelFrame = NSRect(x: anchorScreenPoint.x - size.width / 2,
                                y: anchorScreenPoint.y - size.height / 2,
                                width: size.width, height: size.height)
        if frame != panelFrame { setFrame(panelFrame, display: false) }
        let tracktrampFrame = NSRect(
            x: (size.width - TracktrampMetalView.logicalSize.width) / 2,
            y: 0,
            width: TracktrampMetalView.logicalSize.width,
            height: TracktrampMetalView.logicalSize.height
        )
        if tracktrampView.frame != tracktrampFrame { tracktrampView.frame = tracktrampFrame }
        let trainerSize = PolyrhythmTrainerView.logicalSize(
            rhythmCount: polyrhythm?.rhythms.count ?? 1
        )
        let polyrhythmFrame = NSRect(
            x: (size.width - trainerSize.width) / 2,
            y: TracktrampMetalView.logicalSize.height + Self.polyrhythmGap,
            width: trainerSize.width,
            height: trainerSize.height
        )
        if polyrhythmView.frame != polyrhythmFrame { polyrhythmView.frame = polyrhythmFrame }
        polyrhythmView.isHidden = polyrhythm == nil
    }

    static func tracktrampSurfaceSize(polyrhythm: PolyrhythmTrainerSnapshot?) -> NSSize {
        guard let polyrhythm else { return TracktrampMetalView.logicalSize }
        return tracktrampSurfaceSize(rhythmCount: polyrhythm.rhythms.count)
    }

    /// The surface grows sideways with the circle count so the trainer never
    /// clips, while TrackDrum's own artwork stays centered at its fixed size.
    static func tracktrampSurfaceSize(rhythmCount: Int) -> NSSize {
        let trainer = PolyrhythmTrainerView.logicalSize(rhythmCount: rhythmCount)
        return NSSize(width: max(TracktrampMetalView.logicalSize.width,
                                 trainer.width),
                      height: TracktrampMetalView.logicalSize.height
                        + Self.polyrhythmGap
                        + trainer.height)
    }

    /// A new hardware contact reclaims a surface that is still visible but
    /// partway through its idle fade. Image updates alone must not inherit the
    /// old alpha or allow the old timer to keep dissolving the new gesture.
    func resumeFromFade() {
        fadeTimer?.invalidate()
        fadeTimer = nil
        alphaValue = 1
    }

    var isFadeScheduled: Bool { fadeTimer != nil }

    private func apply(image: NSImage) {
        let size = image.size
        let frame = NSRect(x: anchorScreenPoint.x - size.width / 2,
                           y: anchorScreenPoint.y - size.height / 2,
                           width: size.width, height: size.height)
        setFrame(frame, display: false)
        imageView.frame = NSRect(origin: .zero, size: size)
        imageView.image = image
    }

    /// Keep the just-played surface visible long enough to read its residual
    /// energy, then dissolve it. A new touch calls `show`, cancelling this
    /// timer and restoring full opacity without waiting for the old fade.
    func fadeOut(after delay: TimeInterval, duration: TimeInterval) {
        fadeTimer?.invalidate()
        guard isVisible else { return }
        let started = CACurrentMediaTime()
        let safeDelay = max(0, delay)
        let safeDuration = max(0.05, duration)
        let timer = Timer(timeInterval: 1.0 / 60.0, repeats: true) {
            [weak self] timer in
            guard let self else { timer.invalidate(); return }
            let elapsed = CACurrentMediaTime() - started
            guard elapsed >= safeDelay else { return }
            let progress = min(1, (elapsed - safeDelay) / safeDuration)
            self.alphaValue = CGFloat(1 - progress * progress)
            if progress >= 1 {
                timer.invalidate()
                self.fadeTimer = nil
                self.orderOut(nil)
                self.alphaValue = 1
            }
        }
        timer.tolerance = 1.0 / 240.0
        RunLoop.main.add(timer, forMode: .common)
        fadeTimer = timer
    }

    func dismiss() {
        fadeTimer?.invalidate()
        fadeTimer = nil
        alphaValue = 1
        ignoresMouseEvents = true
        installAction = nil
        installCard.isHidden = true
        polyrhythmView.isHidden = true
        tracktrampView.pause()
        orderOut(nil)
    }
}

/// GPU presentation for the physical drum skin. The AppKit artwork is cached
/// as a texture and a single fragment pass backward-warps its pixels from the
/// membrane slope. This view has no clock of its own: the existing 60 Hz
/// presentation timer supplies snapshots, while touch/audio callbacks remain
/// entirely outside the renderer.
final class TracktrampMetalView: MTKView, MTKViewDelegate {
    static let logicalSize = NSSize(width: 140, height: 88)

    private struct Uniforms {
        var columns: UInt32
        var rows: UInt32
        // A restrained fabric pull, not a lens/refraction effect.
        var warp: Float = 0.046
        var lighting: Float = 0.14
        var touchCount: UInt32 = 0
        var _padding: UInt32 = 0
        var accent = SIMD4<Float>(0.2, 0.48, 1, 1)
        // kick, tom, snare, hat; click follows separately for 16-byte layout.
        var zoneLevels = SIMD4<Float>(repeating: 0)
        var clickLevel: Float = 0
        var _zonePadding = SIMD3<Float>(repeating: 0)
    }

    private var queue: MTLCommandQueue?
    private var pipeline: MTLRenderPipelineState?
    private var baseTexture: MTLTexture?
    private var heightBuffer: MTLBuffer?
    private var touchBuffer: MTLBuffer?
    private var uniforms = Uniforms(columns: 1, rows: 1)
    private var cachedScale: CGFloat = 0
    private var drewFlatFrame = false
    private var zoneLevels = [Float](repeating: 0, count: 5)
    private var lastZoneUpdate = CACurrentMediaTime()

    init() {
        let device = MTLCreateSystemDefaultDevice()
        super.init(frame: NSRect(origin: .zero, size: Self.logicalSize),
                   device: device)
        wantsLayer = true
        // MTKView's CAMetalLayer defaults to opaque even when the render-pass
        // clear color has zero alpha. That exposes the whole drawable as a
        // dark square around our rounded shader mask.
        layer?.isOpaque = false
        layer?.backgroundColor = NSColor.clear.cgColor
        clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 0)
        framebufferOnly = true
        enableSetNeedsDisplay = true
        isPaused = true
        preferredFramesPerSecond = 60
        delegate = self
        queue = device?.makeCommandQueue()
        if let device { buildPipeline(device) }
    }

    required init(coder: NSCoder) {
        fatalError("TracktrampMetalView is code-only")
    }

    override var isOpaque: Bool { false }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        rebuildTextureIfNeeded(force: true)
    }

    override func viewDidChangeBackingProperties() {
        super.viewDidChangeBackingProperties()
        rebuildTextureIfNeeded(force: true)
    }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        rebuildTextureIfNeeded(force: true)
    }

    func update(_ snapshot: TrackpadMembraneSimulation.Snapshot,
                touches: [CGPoint]) {
        guard !isHidden, window?.isVisible == true, let device else { return }
        rebuildTextureIfNeeded(force: false)
        let byteCount = snapshot.heights.count * MemoryLayout<Float>.stride
        if heightBuffer == nil || heightBuffer!.length < byteCount {
            heightBuffer = device.makeBuffer(length: byteCount, options: .storageModeShared)
        }
        guard let heightBuffer else { return }
        let destination = heightBuffer.contents().bindMemory(to: Float.self,
                                                             capacity: snapshot.heights.count)
        for index in snapshot.heights.indices {
            destination[index] = Float(snapshot.heights[index])
        }
        uniforms.columns = UInt32(snapshot.columns)
        uniforms.rows = UInt32(snapshot.rows)
        let visibleTouches = touches.prefix(10)
        let touchBytes = max(1, visibleTouches.count) * MemoryLayout<SIMD2<Float>>.stride
        if touchBuffer == nil || touchBuffer!.length < touchBytes {
            touchBuffer = device.makeBuffer(length: touchBytes, options: .storageModeShared)
        }
        if let touchBuffer {
            let destination = touchBuffer.contents().bindMemory(
                to: SIMD2<Float>.self, capacity: max(1, visibleTouches.count))
            for (index, touch) in visibleTouches.enumerated() {
                destination[index] = SIMD2(Float(touch.x), Float(touch.y))
            }
        }
        uniforms.touchCount = UInt32(visibleTouches.count)
        let now = CACurrentMediaTime()
        let elapsed = min(0.1, max(0, now - lastZoneUpdate))
        lastZoneUpdate = now
        let decay = Float(exp(-elapsed / 0.24))
        for index in zoneLevels.indices { zoneLevels[index] *= decay }
        for touch in visibleTouches {
            let index: Int
            switch MenuBandPercussion.drumSkinZone(at: touch) {
            case .kick: index = 0
            case .tom: index = 1
            case .snare: index = 2
            case .hat: index = 3
            case .click: index = 4
            }
            zoneLevels[index] = 1
        }
        uniforms.zoneLevels = SIMD4(zoneLevels[0], zoneLevels[1],
                                    zoneLevels[2], zoneLevels[3])
        uniforms.clickLevel = zoneLevels[4]
        let accent = KeyboardIconRenderer.accent.usingColorSpace(.sRGB)
            ?? NSColor.controlAccentColor
        uniforms.accent = SIMD4(Float(accent.redComponent), Float(accent.greenComponent),
                                Float(accent.blueComponent), Float(accent.alphaComponent))
        // Paint one final undistorted frame, then stop submitting GPU work.
        if snapshot.isFlat && visibleTouches.isEmpty
            && (zoneLevels.max() ?? 0) < 0.01 {
            guard !drewFlatFrame else { return }
            drewFlatFrame = true
        } else {
            drewFlatFrame = false
        }
        // MTKView is paused and dirty-driven; draw exactly when the existing
        // capped presentation path hands us a fresh visual snapshot.
        draw()
    }

    func pause() { drewFlatFrame = false }

    private func buildPipeline(_ device: MTLDevice) {
        do {
            let library: MTLLibrary
            if let url = Bundle.appResources.url(forResource: "TracktrampShaders",
                                                  withExtension: "metalsource"),
               let source = try? String(contentsOf: url, encoding: .utf8) {
                library = try device.makeLibrary(source: source, options: nil)
            } else {
                library = try device.makeDefaultLibrary(bundle: .module)
            }
            let descriptor = MTLRenderPipelineDescriptor()
            descriptor.vertexFunction = library.makeFunction(name: "tracktramp_vertex")
            descriptor.fragmentFunction = library.makeFunction(name: "tracktramp_fragment")
            descriptor.colorAttachments[0].pixelFormat = colorPixelFormat
            descriptor.colorAttachments[0].isBlendingEnabled = true
            descriptor.colorAttachments[0].sourceRGBBlendFactor = .sourceAlpha
            descriptor.colorAttachments[0].destinationRGBBlendFactor = .oneMinusSourceAlpha
            descriptor.colorAttachments[0].sourceAlphaBlendFactor = .one
            descriptor.colorAttachments[0].destinationAlphaBlendFactor = .oneMinusSourceAlpha
            pipeline = try device.makeRenderPipelineState(descriptor: descriptor)
        } catch {
            NSLog("MenuBand: tracktramp Metal pipeline failed: \(error)")
        }
    }

    private func rebuildTextureIfNeeded(force: Bool) {
        guard let device else { return }
        let scale = window?.backingScaleFactor ?? NSScreen.main?.backingScaleFactor ?? 2
        guard force || baseTexture == nil || scale != cachedScale else { return }
        cachedScale = scale
        let image = TrackpadDrumSkinPad.image(touches: [], energy: [], membrane: nil,
                                              appearance: effectiveAppearance)
        let pixelWidth = max(1, Int(ceil(Self.logicalSize.width * scale)))
        let pixelHeight = max(1, Int(ceil(Self.logicalSize.height * scale)))
        guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil,
                                            pixelsWide: pixelWidth,
                                            pixelsHigh: pixelHeight,
                                            bitsPerSample: 8,
                                            samplesPerPixel: 4,
                                            hasAlpha: true,
                                            isPlanar: false,
                                            colorSpaceName: .deviceRGB,
                                            // Keep the cached artwork in the
                                            // standard RGBA layout expected by
                                            // MTKTextureLoader. `.alphaFirst`
                                            // was being sampled as color data,
                                            // turning the tan skin dark blue.
                                            bitmapFormat: [],
                                            bytesPerRow: 0,
                                            bitsPerPixel: 0),
              let context = NSGraphicsContext(bitmapImageRep: bitmap) else { return }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = context
        context.cgContext.scaleBy(x: scale, y: scale)
        image.draw(in: NSRect(origin: .zero, size: Self.logicalSize),
                   from: .zero, operation: .copy, fraction: 1)
        NSGraphicsContext.restoreGraphicsState()
        guard let cgImage = bitmap.cgImage else { return }
        baseTexture = try? MTKTextureLoader(device: device).newTexture(
            cgImage: cgImage,
            options: [.SRGB: true, .origin: MTKTextureLoader.Origin.topLeft]
        )
        drewFlatFrame = false
    }

    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {}

    func draw(in view: MTKView) {
        guard let drawable = currentDrawable,
              let pass = currentRenderPassDescriptor,
              let queue, let pipeline, let baseTexture, let heightBuffer,
              let touchBuffer,
              let command = queue.makeCommandBuffer(),
              let encoder = command.makeRenderCommandEncoder(descriptor: pass) else { return }
        encoder.setRenderPipelineState(pipeline)
        encoder.setFragmentTexture(baseTexture, index: 0)
        encoder.setFragmentBuffer(heightBuffer, offset: 0, index: 0)
        encoder.setFragmentBytes(&uniforms, length: MemoryLayout<Uniforms>.stride,
                                 index: 1)
        encoder.setFragmentBuffer(touchBuffer, offset: 0, index: 2)
        encoder.drawPrimitives(type: .triangleStrip, vertexStart: 0, vertexCount: 4)
        encoder.endEncoding()
        command.present(drawable)
        command.commit()
    }
}

extension NSCursor {
    /// Convenience to push the neutral pitch-bend cursor onto the
    /// stack. Mirrors the original `PitchBendCursor.shared.push()`
    /// callsite.
    static func pushPitchBend() {
        PitchBendCursor.neutral.push()
    }
}
