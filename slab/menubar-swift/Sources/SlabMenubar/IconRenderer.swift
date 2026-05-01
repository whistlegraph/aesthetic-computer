import AppKit

enum IconRenderer {
    /// Render the menubar icon. When there are any active Claude sessions we
    /// draw a regular polygon — one edge per session, colored by THAT
    /// session's state: steady cyan for working, pulsing red for awaiting,
    /// slow-blinking gray for stale. N=1 is a single line, N=2 is two
    /// parallel bars, N≥3 is the matching n-gon (triangle, square,
    /// pentagon, …). The whole shape rotates slowly while any session is
    /// non-stale, faster with more awaiting work. Falls back to SF Symbols
    /// for idle / ambient / lid-closed states.
    static func image(for state: StateSnapshot, phase: CGFloat = 0, rotation: CGFloat = 0) -> NSImage {
        if !state.claudeSessions.isEmpty {
            return polygonImage(state: state, phase: phase, rotation: rotation)
        }

        let name: String
        let weight: NSFont.Weight

        if state.ambientActive {
            name = "waveform.path.ecg"
            weight = .medium
        } else if state.hasWork && state.lidClosed && !state.sleepDisabled {
            name = "exclamationmark.triangle.fill"
            weight = .semibold
        } else if state.hasWork && state.lidClosed {
            name = "moon.zzz.fill"
            weight = .semibold
        } else if state.hasWork {
            name = "square.stack.3d.up.fill"
            weight = .semibold
        } else {
            name = "square.stack.3d.up"
            weight = .regular
        }

        let config = NSImage.SymbolConfiguration(pointSize: 14, weight: weight, scale: .medium)
        let base = NSImage(systemSymbolName: name, accessibilityDescription: "slab: \(state.statusLine)")
            ?? fallbackImage()
        let configured = base.withSymbolConfiguration(config) ?? base
        configured.isTemplate = true
        return configured
    }

    /// Cap the number of distinct polygon edges. Beyond this we still draw
    /// the cap polygon and add a small "+" tick in the center to indicate
    /// overflow.
    private static let maxSides = 8

    private static func polygonImage(state: StateSnapshot, phase: CGFloat, rotation: CGFloat) -> NSImage {
        // Square canvas sized to fill the menubar slot. macOS menubar
        // thickness is 22–24pt depending on version; 22×22 fits the slot
        // edge-to-edge without clipping.
        let pointsW: CGFloat = 22
        let pointsH: CGFloat = 22
        // Render at 2x so diagonal polygon edges stay crisp on retina.
        let scale: CGFloat = 2
        let pixelsW = Int(pointsW * scale)
        let pixelsH = Int(pointsH * scale)

        // Explicit deviceRGB bitmap rep — without this, NSImage(size:) +
        // lockFocus produces a rep that the menubar interprets as a template
        // (monochrome) so all our colored edges render as white.
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil,
            pixelsWide: pixelsW,
            pixelsHigh: pixelsH,
            bitsPerSample: 8,
            samplesPerPixel: 4,
            hasAlpha: true,
            isPlanar: false,
            colorSpaceName: .deviceRGB,
            bytesPerRow: 0,
            bitsPerPixel: 0
        ) else {
            return fallbackImage()
        }
        rep.size = NSSize(width: pointsW, height: pointsH)

        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
        defer {
            NSGraphicsContext.restoreGraphicsState()
        }

        let sessions = state.claudeSessions
        let visible = Array(sessions.prefix(maxSides))
        let n = visible.count
        let overflow = sessions.count - n

        let cx = pointsW / 2.0
        let cy = pointsH / 2.0
        // Bounding circle radius. Leaves ~1.5pt for line width + round caps
        // so the shape kisses the canvas edges but never clips.
        let radius: CGFloat = 9.5
        let lineWidth: CGFloat = 1.6

        // Per-edge color: each edge is one session, colored by that
        // session's own state. Geometry shows count; color shows which
        // threads are working vs paused vs stale at a glance.
        if n == 1 {
            // Single horizontal line, rotated.
            drawSegment(
                center: NSPoint(x: cx, y: cy),
                length: 2 * radius,
                angle: rotation,
                color: sessionColor(visible[0].state, phase: phase),
                lineWidth: lineWidth
            )
        } else if n == 2 {
            // Two parallel bars, pivoting together. The pair tilts as one
            // rigid body — bars stay parallel and equidistant from center.
            // Bar corners stay inside the bounding circle: with length L
            // and half-offset h, sqrt((L/2)² + h²) ≤ radius.
            let half: CGFloat = 3.5
            let length: CGFloat = 17
            let nx = -sin(rotation)
            let ny = cos(rotation)
            for i in 0..<n {
                let sign: CGFloat = (i == 0) ? 1 : -1
                let center = NSPoint(x: cx + nx * half * sign, y: cy + ny * half * sign)
                drawSegment(
                    center: center,
                    length: length,
                    angle: rotation,
                    color: sessionColor(visible[i].state, phase: phase),
                    lineWidth: lineWidth
                )
            }
        } else {
            // Regular n-gon. Vertices offset by π/n so the midpoint of edge
            // 0 sits at the top — ensures triangles point up, squares are
            // squares (not diamonds), etc.
            var vertices: [NSPoint] = []
            vertices.reserveCapacity(n)
            for k in 0..<n {
                let theta = -CGFloat.pi / 2 - CGFloat.pi / CGFloat(n)
                    + 2 * CGFloat.pi * CGFloat(k) / CGFloat(n)
                    + rotation
                vertices.append(NSPoint(x: cx + radius * cos(theta), y: cy + radius * sin(theta)))
            }
            // Each edge stroked separately so its color reflects its own
            // session. Round caps make adjacent edges meet without visible
            // seams at the vertices.
            for k in 0..<n {
                let a = vertices[k]
                let b = vertices[(k + 1) % n]
                let path = NSBezierPath()
                path.move(to: a)
                path.line(to: b)
                path.lineWidth = lineWidth
                path.lineCapStyle = .round
                sessionColor(visible[k].state, phase: phase).setStroke()
                path.stroke()
            }
        }

        if overflow > 0 {
            // Tiny "+" in the center indicates sessions beyond the cap.
            NSColor.white.setFill()
            NSBezierPath(rect: NSRect(x: cx - 1.5, y: cy - 0.5, width: 3, height: 1)).fill()
            NSBezierPath(rect: NSRect(x: cx - 0.5, y: cy - 1.5, width: 1, height: 3)).fill()
        }

        let img = NSImage(size: NSSize(width: pointsW, height: pointsH))
        img.addRepresentation(rep)
        img.isTemplate = false
        return img
    }

    private static func drawSegment(center: NSPoint, length: CGFloat, angle: CGFloat, color: NSColor, lineWidth: CGFloat) {
        let dx = cos(angle) * length / 2
        let dy = sin(angle) * length / 2
        let path = NSBezierPath()
        path.move(to: NSPoint(x: center.x - dx, y: center.y - dy))
        path.line(to: NSPoint(x: center.x + dx, y: center.y + dy))
        path.lineWidth = lineWidth
        path.lineCapStyle = .round
        color.setStroke()
        path.stroke()
    }

    /// Per-session edge color. Working = steady cyan-teal. Awaiting =
    /// pulsing warm red, the loud "look at me" state. Stale = slow gray
    /// blink, "thread is dead but the marker's still on disk."
    private static func sessionColor(_ state: ClaudeSession.State, phase: CGFloat) -> NSColor {
        switch state {
        case .working:
            return NSColor(deviceHue: 0.50, saturation: 0.60, brightness: 0.82, alpha: 1.0)
        case .awaiting:
            // Pulse brightness at 2 Hz so paused threads visibly throb
            // among the steady cyan working ones.
            let pulse = 0.5 + 0.5 * cos(phase * .pi * 4)
            let brightness = 0.70 + 0.30 * pulse
            return NSColor(deviceHue: 0.0, saturation: 0.92, brightness: brightness, alpha: 1.0)
        case .stale:
            let blink = 0.5 + 0.5 * cos(phase * .pi * 2)
            return NSColor(deviceWhite: 0.28 + 0.32 * blink, alpha: 1.0)
        }
    }

    private static func fallbackImage() -> NSImage {
        let size = NSSize(width: 16, height: 16)
        let img = NSImage(size: size)
        img.lockFocus()
        NSColor.labelColor.setStroke()
        let path = NSBezierPath(ovalIn: NSRect(x: 3, y: 3, width: 10, height: 10))
        path.lineWidth = 1.5
        path.stroke()
        img.unlockFocus()
        return img
    }
}
