import AppKit

enum IconRenderer {
    /// Render the menubar icon. When there are any active Claude sessions we
    /// draw a custom stack — one colored horizontal bar per session, colored
    /// by its state (rainbow-pulsing for awaiting, steady cyan for working,
    /// dim gray for stale). Falls back to SF Symbols for idle / ambient /
    /// lid-closed states.
    static func image(for state: StateSnapshot, phase: CGFloat = 0) -> NSImage {
        if !state.claudeSessions.isEmpty {
            return stackImage(state: state, phase: phase)
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

    /// Draw a horizontal stack: one bar per active session. Bars stack from
    /// bottom up (most-urgent at the top, since the reader sorts awaiting
    /// first). If we have more sessions than fit, the topmost bar gets a
    /// little "+" tick to indicate overflow.
    private static func stackImage(state: StateSnapshot, phase: CGFloat) -> NSImage {
        let pixelsW = 18
        let pixelsH = 16
        let size = NSSize(width: pixelsW, height: pixelsH)

        // Explicit deviceRGB bitmap rep — without this, NSImage(size:) +
        // lockFocus produces a rep that the menubar interprets as a template
        // (monochrome) so all our colored bars render as white.
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

        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
        defer {
            NSGraphicsContext.restoreGraphicsState()
        }

        let maxVisible = 5
        let barH: CGFloat = 2
        let gap: CGFloat = 1
        let barW: CGFloat = 14

        let sessions = state.claudeSessions
        let visible = Array(sessions.prefix(maxVisible))
        let overflow = sessions.count - visible.count

        let stackH = CGFloat(visible.count) * barH + CGFloat(max(0, visible.count - 1)) * gap
        let originY = (size.height - stackH) / 2
        let originX = (size.width - barW) / 2

        for (i, session) in visible.enumerated() {
            let fromTop = CGFloat(i)
            let y = originY + stackH - barH - fromTop * (barH + gap)
            let rect = NSRect(x: originX, y: y, width: barW, height: barH)
            colorFor(session: session, indexFromTop: Int(fromTop), phase: phase).setFill()
            NSBezierPath(rect: rect).fill()
        }

        if overflow > 0, let top = visible.first {
            let y = originY + stackH - barH
            colorFor(session: top, indexFromTop: 0, phase: phase).setFill()
            NSBezierPath(rect: NSRect(x: originX + barW + 1, y: y - 1, width: 1, height: 4)).fill()
            NSBezierPath(rect: NSRect(x: originX + barW, y: y, width: 3, height: 1)).fill()
        }

        let img = NSImage(size: size)
        img.addRepresentation(rep)
        img.isTemplate = false
        return img
    }

    private static func colorFor(session: ClaudeSession, indexFromTop: Int, phase: CGFloat) -> NSColor {
        switch session.state {
        case .awaiting:
            // Rainbow hue rotation, pulsing brightness. Each awaiting bar is
            // offset in hue by its position so multiple awaiting sessions
            // are visually distinct.
            let pulse = 0.55 + 0.45 * (0.5 + 0.5 * cos(phase * .pi * 4))
            var hue = (phase + CGFloat(indexFromTop) * 0.15).truncatingRemainder(dividingBy: 1.0)
            if hue < 0 { hue += 1 }
            return NSColor(deviceHue: hue, saturation: 0.95, brightness: pulse, alpha: 1.0)
        case .working:
            // Calm cyan — steady, clearly distinct from rainbow.
            return NSColor(deviceHue: 0.52, saturation: 0.55, brightness: 0.85, alpha: 1.0)
        case .stale:
            return NSColor(deviceWhite: 0.45, alpha: 1.0)
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
