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
            var poly = polygonImage(state: state, phase: phase, rotation: rotation)
            if state.messageWaiting { poly = withMessageDot(poly, phase: phase) }
            return poly
        }

        let name: String
        let weight: NSFont.Weight

        if state.messageWaiting {
            // She texted (and theme-by-status is on) — this outranks the
            // ambient / idle glyphs. Template so the menubar still tints it.
            name = "message.fill"
            weight = .semibold
        } else if state.ambientActive {
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
            // Pure idle — nothing open. Instead of a static glyph, draw a
            // slowly-spinning line with a rainbow flowing along its length:
            // an enticing "resting" state that rhymes with the single-session
            // line (n==1) but in colour. Themed for the current menubar.
            let dark = state.forceBright ? false : AppDelegate.isDarkAppearance()
            return idleLineImage(phase: phase, rotation: rotation, dark: dark)
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

        // Match the themed terminals: light palettes under the force-bright
        // override or in Light mode, dark palettes otherwise — same decision
        // the terminal/desktop theming makes via `effectiveDark()`.
        let dark = state.forceBright ? false : AppDelegate.isDarkAppearance()

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
            let col = sessionColor(visible[0].state, phase: phase, dark: dark)
            drawSegment(
                center: NSPoint(x: cx, y: cy),
                length: 2 * radius,
                angle: rotation,
                color: col,
                lineWidth: lineWidth
            )
            let h = NSPoint(x: cos(rotation) * radius, y: sin(rotation) * radius)
            drawSubagentDots(from: NSPoint(x: cx - h.x, y: cy - h.y),
                             to: NSPoint(x: cx + h.x, y: cy + h.y),
                             count: visible[0].subagentCount, color: col, dark: dark)
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
                let col = sessionColor(visible[i].state, phase: phase, dark: dark)
                drawSegment(
                    center: center,
                    length: length,
                    angle: rotation,
                    color: col,
                    lineWidth: lineWidth
                )
                let h = NSPoint(x: cos(rotation) * length / 2, y: sin(rotation) * length / 2)
                drawSubagentDots(from: NSPoint(x: center.x - h.x, y: center.y - h.y),
                                 to: NSPoint(x: center.x + h.x, y: center.y + h.y),
                                 count: visible[i].subagentCount, color: col, dark: dark)
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
                let col = sessionColor(visible[k].state, phase: phase, dark: dark)
                col.setStroke()
                path.stroke()
                drawSubagentDots(from: a, to: b, count: visible[k].subagentCount,
                                 color: col, dark: dark)
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

    /// The idle "resting" icon — a single line through the center that spins
    /// slowly while a rainbow flows along its length. Non-template (coloured),
    /// 22×22 @2x like the polygon. `rotation` is the spin angle; `phase`
    /// scrolls the hue. Saturation/brightness adapt to light vs dark so it
    /// stays vivid on either menubar; a soft mid-line brightness taper makes
    /// it glow in the middle and feather at the round-capped ends.
    private static func idleLineImage(phase: CGFloat, rotation: CGFloat, dark: Bool) -> NSImage {
        let pointsW: CGFloat = 22, pointsH: CGFloat = 22
        let scale: CGFloat = 2
        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil,
            pixelsWide: Int(pointsW * scale),
            pixelsHigh: Int(pointsH * scale),
            bitsPerSample: 8,
            samplesPerPixel: 4,
            hasAlpha: true,
            isPlanar: false,
            colorSpaceName: .deviceRGB,
            bytesPerRow: 0,
            bitsPerPixel: 0
        ) else { return fallbackImage() }
        rep.size = NSSize(width: pointsW, height: pointsH)

        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
        defer { NSGraphicsContext.restoreGraphicsState() }

        let cx = pointsW / 2.0, cy = pointsH / 2.0
        let radius: CGFloat = 9.5
        let lineWidth: CGFloat = 1.8
        let sat: CGFloat = dark ? 0.85 : 0.95
        let bri: CGFloat = dark ? 1.0 : 0.82

        let dirX = cos(rotation), dirY = sin(rotation)
        // Stroke the line as short round-capped sub-segments, each one step
        // further along the spectrum, so the line itself is a small moving
        // rainbow rather than a flat colour.
        let steps = 20
        let hueSpread: CGFloat = 0.6   // fraction of the hue wheel spanned end-to-end
        for i in 0..<steps {
            let t0 = CGFloat(i) / CGFloat(steps)
            let t1 = CGFloat(i + 1) / CGFloat(steps)
            let o0 = (t0 * 2 - 1) * radius
            let o1 = (t1 * 2 - 1) * radius
            let p0 = NSPoint(x: cx + dirX * o0, y: cy + dirY * o0)
            let p1 = NSPoint(x: cx + dirX * o1, y: cy + dirY * o1)
            let hue = (phase + t0 * hueSpread).truncatingRemainder(dividingBy: 1.0)
            let taper = 0.55 + 0.45 * sin(t0 * .pi)   // dim ends, bright middle
            let seg = NSBezierPath()
            seg.move(to: p0)
            seg.line(to: p1)
            seg.lineWidth = lineWidth
            seg.lineCapStyle = .round
            NSColor(deviceHue: hue, saturation: sat,
                    brightness: bri * taper, alpha: 1.0).setStroke()
            seg.stroke()
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

    /// One small filled dot per in-flight subagent, spaced along a session's
    /// edge (a→b). Spots are the edge colour brightened toward white so they
    /// read as bright beads riding that segment. Capped so they stay legible
    /// at menubar size; beyond the cap the last bead just doubles up.
    private static func drawSubagentDots(from a: NSPoint, to b: NSPoint, count: Int, color: NSColor, dark: Bool) {
        guard count > 0 else { return }
        let shown = min(count, 7)
        let r: CGFloat = 1.05
        let dot = color.blended(withFraction: dark ? 0.74 : 0.58, of: .white) ?? color
        dot.setFill()
        for i in 0..<shown {
            let t: CGFloat = shown == 1 ? 0.5 : 0.20 + 0.60 * CGFloat(i) / CGFloat(shown - 1)
            let p = NSPoint(x: a.x + (b.x - a.x) * t, y: a.y + (b.y - a.y) * t)
            NSBezierPath(ovalIn: NSRect(x: p.x - r, y: p.y - r, width: 2 * r, height: 2 * r)).fill()
        }
    }

    /// Per-session edge color, sourced from the SAME per-status palette that
    /// theme-by-status pushes to the live terminals + desktop tint — so the
    /// menubar polygon, the themed windows, and the wallpaper all read as one
    /// status system, light or dark. We take the palette's cursor channel (its
    /// most saturated accent) and animate brightness on the attention states:
    /// awaiting throbs brighter, stale fades toward the page.
    private static func sessionColor(_ state: ClaudeSession.State, phase: CGFloat, dark: Bool) -> NSColor {
        let cur = AppDelegate.statusDecor(for: state, dark: dark).palette.cursor
            ?? (32768, 32768, 32768)
        let base = NSColor(deviceRed: CGFloat(cur.0) / 65535,
                           green: CGFloat(cur.1) / 65535,
                           blue: CGFloat(cur.2) / 65535, alpha: 1.0)
        switch state {
        case .awaiting:
            // Throb toward white at 2 Hz so paused threads pulse among the
            // steady working edges.
            let pulse = 0.5 + 0.5 * cos(phase * .pi * 4)
            return base.blended(withFraction: 0.30 * pulse, of: .white) ?? base
        case .stale:
            // Slow fade toward the page so a dead marker blinks out.
            let blink = 0.5 + 0.5 * cos(phase * .pi * 2)
            let page = dark ? NSColor(deviceWhite: 0.10, alpha: 1.0)
                            : NSColor(deviceWhite: 0.92, alpha: 1.0)
            return base.blended(withFraction: (1 - blink) * 0.5, of: page) ?? base
        default:
            return base
        }
    }

    /// Composite a pulsing magenta dot bottom-left so the colored polygon
    /// also carries the "she texted"
    /// accent — the menubar and the themed wall then read one picture. The
    /// hue is deliberately off the working-green / awaiting-amber /
    /// complete-slate / stale-gray axis so it never reads as a session state.
    private static func withMessageDot(_ base: NSImage, phase: CGFloat) -> NSImage {
        let sz = base.size
        let out = NSImage(size: sz)
        out.lockFocus()
        base.draw(in: NSRect(origin: .zero, size: sz))
        let pulse = 0.5 + 0.5 * cos(phase * .pi * 2)
        let d: CGFloat = sz.width * 0.34
        let path = NSBezierPath(ovalIn: NSRect(x: 1, y: 1, width: d, height: d))
        NSColor(deviceHue: 0.92, saturation: 0.85, brightness: 0.98,
                alpha: 0.55 + 0.45 * pulse).setFill()
        path.fill()
        NSColor(deviceWhite: 1.0, alpha: 0.65).setStroke()
        path.lineWidth = 0.75
        path.stroke()
        out.unlockFocus()
        out.isTemplate = false
        return out
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
