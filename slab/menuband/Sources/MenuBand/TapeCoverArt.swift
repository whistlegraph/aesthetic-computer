import AppKit

/// Per-tape Finder cover art. Renders a 512×512 cassette icon with the
/// recording's downsampled waveform plotted across the label card and a
/// date/duration stamp on the J-card spine — so when the user drops a
/// tape onto the Desktop, the file's icon shows what's on it rather
/// than the generic audio file glyph.
///
/// Attached via `NSWorkspace.shared.setIcon(_:forFile:options:)` after
/// `AVAudioFile.write` completes. The icon is stored on the file's
/// resource fork (extended attribute on APFS); it travels with the
/// file across Finder copies and survives a re-import into a DAW.
enum TapeCoverArt {
    static let canvasSize = CGSize(width: 512, height: 512)

    /// Build a custom Finder icon for a freshly-rendered tape.
    ///
    /// - Parameters:
    ///   - date: time of recording — colors the body palette (a
    ///     time-of-day tint that distinguishes morning vs evening
    ///     tapes at a glance in a folder full of them).
    ///   - duration: recorded length in seconds. Drawn on the label
    ///     spine; also caps the waveform render to its actual span.
    ///   - waveform: downsampled RMS buckets, one per horizontal pixel
    ///     of the label width. Empty → label stays plain. Values are
    ///     in 0…1 normalized to the recording's peak.
    static func makeIcon(date: Date,
                          duration: TimeInterval,
                          waveform: [Float]) -> NSImage {
        let img = NSImage(size: canvasSize)
        img.lockFocus()
        defer { img.unlockFocus() }
        draw(date: date, duration: duration, waveform: waveform,
             rect: NSRect(origin: .zero, size: canvasSize))
        return img
    }

    private static func draw(date: Date,
                              duration: TimeInterval,
                              waveform: [Float],
                              rect: NSRect) {
        // Clear background so the icon has transparent corners
        // (rounded square + free space around the cassette).
        NSColor.clear.setFill()
        rect.fill()

        // Body color shifts with time of day — same palette logic the
        // recap photo pipeline uses, narrowed to four bands so the
        // tape labels read as "morning tape" / "afternoon tape" /
        // "evening tape" / "night tape" without us having to think
        // about a per-minute gradient. Inspired by the 70s AGFA bold-
        // color blank cassettes in the Core77 archive.
        let palette = palette(for: date)

        // Cassette body — landscape-oriented inside the 512×512
        // canvas. Generous margin so the corners can host the date
        // stamp without crowding the cassette.
        let bodyMargin: CGFloat = 56
        let bodyAspect: CGFloat = 11.0 / 7.0  // matches a real cassette
        let maxW = rect.width - bodyMargin * 2
        var bodyW = maxW
        var bodyH = bodyW / bodyAspect
        if bodyH > rect.height - bodyMargin * 2 {
            bodyH = rect.height - bodyMargin * 2
            bodyW = bodyH * bodyAspect
        }
        let bodyRect = NSRect(x: rect.midX - bodyW / 2,
                               y: rect.midY - bodyH / 2,
                               width: bodyW,
                               height: bodyH)

        // Drop shadow — sits the cassette above the canvas. Stops the
        // icon from looking like a flat sticker against a Finder
        // window background.
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(0.35)
        shadow.shadowOffset = NSSize(width: 0, height: -8)
        shadow.shadowBlurRadius = 22
        NSGraphicsContext.saveGraphicsState()
        shadow.set()

        let bodyPath = NSBezierPath(roundedRect: bodyRect,
                                     xRadius: 14, yRadius: 14)
        NSGradient(colors: [palette.bodyHi, palette.bodyLo],
                    atLocations: [0.0, 1.0],
                    colorSpace: .sRGB)?.draw(in: bodyPath, angle: -90)
        NSGraphicsContext.restoreGraphicsState()

        NSColor.white.withAlphaComponent(0.25).setStroke()
        bodyPath.lineWidth = 1.5
        bodyPath.stroke()

        // Label card — across the top half of the cassette body. This
        // is where the waveform goes. Real cassette J-cards used a
        // cream + thin colored stripe; we match the idiom but trade
        // the typography for an audio-visualization payload.
        let labelInset: CGFloat = 22
        let labelH = bodyRect.height * 0.40
        let labelRect = NSRect(x: bodyRect.minX + labelInset,
                                y: bodyRect.maxY - labelH - 16,
                                width: bodyRect.width - labelInset * 2,
                                height: labelH)
        let labelPath = NSBezierPath(roundedRect: labelRect,
                                     xRadius: 5, yRadius: 5)
        NSGradient(colors: [palette.labelHi, palette.labelLo],
                    atLocations: [0.0, 1.0],
                    colorSpace: .sRGB)?.draw(in: labelPath, angle: -90)
        NSColor(srgbRed: 30/255, green: 30/255, blue: 40/255,
                alpha: 0.20).setStroke()
        labelPath.lineWidth = 0.8
        labelPath.stroke()

        // TDK-gold stripe — bottom edge of the label card.
        let stripeColor = palette.stripe
        let stripeRect = NSRect(x: labelRect.minX,
                                 y: labelRect.minY,
                                 width: labelRect.width,
                                 height: 5)
        stripeColor.setFill()
        NSBezierPath(rect: stripeRect).fill()

        // Waveform — the actual "cover art" of this tape. RMS buckets
        // plotted as vertical bars across the label card, centered on
        // the label's mid-Y so it reads symmetrically (above and
        // below the waveform axis), mimicking a stereo audio editor
        // VU meter.
        drawWaveform(in: labelRect.insetBy(dx: 12, dy: 18),
                     buckets: waveform,
                     color: palette.waveform)

        // Two big hexagonal-hub reels below the label. Same geometry
        // as the inline + popover cassettes; scale gives plenty of
        // detail room here.
        let reelAreaY = bodyRect.minY + 20
        let reelAreaH = labelRect.minY - reelAreaY - 12
        let reelR = min(reelAreaH * 0.46, bodyRect.width * 0.13)
        let reelCY = reelAreaY + reelAreaH / 2
        let leftCX = bodyRect.midX - reelR * 2.2
        let rightCX = bodyRect.midX + reelR * 2.2
        drawReel(at: NSPoint(x: leftCX, y: reelCY),
                  radius: reelR,
                  rotation: 0,
                  fillFraction: 0.85,
                  spoolColor: palette.spool)
        drawReel(at: NSPoint(x: rightCX, y: reelCY),
                  radius: reelR,
                  rotation: .pi / 6,
                  fillFraction: 0.15,
                  spoolColor: palette.spool)

        // Drive holes between the reels — small white circles that
        // every cassette has for the deck's pinch wheels.
        let driveR: CGFloat = 5
        let driveColor = NSColor.black.withAlphaComponent(0.55)
        for i in 0..<3 {
            let x = bodyRect.midX + CGFloat(i - 1) * 22
            driveColor.setFill()
            NSBezierPath(ovalIn: NSRect(x: x - driveR,
                                         y: reelCY - driveR,
                                         width: driveR * 2,
                                         height: driveR * 2)).fill()
        }

        // Date stamp on the label — small monospaced caption inside
        // the label card, lower left.
        drawDateAndDuration(date: date,
                             duration: duration,
                             rect: labelRect.insetBy(dx: 14, dy: 6))

        // Wordmark — "MENU BAND" along the bottom of the cassette
        // body, in tiny letters like a real cassette manufacturer
        // signature.
        let wordmark = "MENU BAND"
        let wordmarkAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 13, weight: .heavy),
            .foregroundColor: NSColor.white.withAlphaComponent(0.7),
            .kern: 3.0,
        ]
        let wmSize = (wordmark as NSString).size(withAttributes: wordmarkAttrs)
        let wmRect = NSRect(x: bodyRect.midX - wmSize.width / 2,
                             y: bodyRect.minY + 8,
                             width: wmSize.width,
                             height: wmSize.height)
        (wordmark as NSString).draw(in: wmRect, withAttributes: wordmarkAttrs)
    }

    // MARK: - Waveform plot

    private static func drawWaveform(in rect: NSRect,
                                      buckets: [Float],
                                      color: NSColor) {
        guard !buckets.isEmpty, rect.width > 1 else { return }

        // Resample to the rect's pixel width so we get one bar per
        // horizontal pixel, regardless of how many buckets came in.
        let columns = Int(rect.width)
        let resampled = resample(buckets, to: columns)

        // Auto-gain so quiet recordings still produce visible bars.
        var peak: Float = 0.0001
        for v in resampled { peak = max(peak, abs(v)) }
        let gain = min(8.0, 0.95 / peak)

        let midY = rect.midY
        let halfH = rect.height / 2
        let barW: CGFloat = 1
        color.setFill()
        for (i, value) in resampled.enumerated() {
            let amp = CGFloat(min(1.0, value * gain))
            let h = amp * halfH
            let x = rect.minX + CGFloat(i) * barW
            // Symmetric VU rendering — looks like a stereo audio
            // editor display, reads as "this is sound."
            let bar = NSRect(x: x, y: midY - h, width: barW, height: h * 2)
            NSBezierPath(rect: bar).fill()
        }

        // Center line — thin axis so the waveform reads as silence
        // (no bars) vs sound (bars rising symmetrically).
        color.withAlphaComponent(0.35).setStroke()
        let axis = NSBezierPath()
        axis.move(to: NSPoint(x: rect.minX, y: midY))
        axis.line(to: NSPoint(x: rect.maxX, y: midY))
        axis.lineWidth = 0.5
        axis.stroke()
    }

    private static func resample(_ buckets: [Float], to count: Int) -> [Float] {
        guard count > 0 else { return [] }
        if buckets.count == count { return buckets }
        var out = [Float](repeating: 0, count: count)
        let ratio = Double(buckets.count) / Double(count)
        for i in 0..<count {
            let start = Int(Double(i) * ratio)
            let end = min(buckets.count, max(start + 1,
                                              Int(Double(i + 1) * ratio)))
            var sumSq: Float = 0
            for j in start..<end { sumSq += buckets[j] * buckets[j] }
            out[i] = sqrt(sumSq / Float(end - start))
        }
        return out
    }

    // MARK: - Date stamp

    private static let labelDateFormatter: DateFormatter = {
        let f = DateFormatter()
        f.dateFormat = "yyyy.MM.dd  HH:mm"
        return f
    }()

    private static func drawDateAndDuration(date: Date,
                                             duration: TimeInterval,
                                             rect: NSRect) {
        let mins = Int(duration) / 60
        let secs = Int(duration) % 60
        let dateStr = labelDateFormatter.string(from: date)
        let durStr = String(format: "%02d:%02d", mins, secs)

        let dateAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedSystemFont(ofSize: 14, weight: .medium),
            .foregroundColor: NSColor(srgbRed: 35/255, green: 30/255,
                                       blue: 40/255, alpha: 0.78),
        ]
        let durAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 14, weight: .bold),
            .foregroundColor: NSColor(srgbRed: 35/255, green: 30/255,
                                       blue: 40/255, alpha: 0.92),
        ]
        let dateSize = (dateStr as NSString).size(withAttributes: dateAttrs)
        let durSize  = (durStr as NSString).size(withAttributes: durAttrs)
        (dateStr as NSString).draw(
            at: NSPoint(x: rect.minX,
                        y: rect.minY + 4),
            withAttributes: dateAttrs)
        (durStr as NSString).draw(
            at: NSPoint(x: rect.maxX - durSize.width,
                        y: rect.minY + 4),
            withAttributes: durAttrs)
        _ = dateSize
    }

    // MARK: - Reel drawing

    private static func drawReel(at center: NSPoint,
                                  radius: CGFloat,
                                  rotation: CGFloat,
                                  fillFraction: CGFloat,
                                  spoolColor: NSColor) {
        // Window — dark recess in the cassette body.
        let windowR = radius + 3
        let windowRect = NSRect(x: center.x - windowR,
                                 y: center.y - windowR,
                                 width: windowR * 2,
                                 height: windowR * 2)
        NSColor.black.withAlphaComponent(0.55).setFill()
        NSBezierPath(ovalIn: windowRect).fill()

        // Outer spool — visible wound tape. Brown so the audience
        // reads "this is magnetic tape, not a generic reel-shaped
        // dial."
        let spoolR = radius * (0.65 + 0.35 * fillFraction)
        let spoolRect = NSRect(x: center.x - spoolR,
                                y: center.y - spoolR,
                                width: spoolR * 2,
                                height: spoolR * 2)
        spoolColor.setFill()
        NSBezierPath(ovalIn: spoolRect).fill()

        // Hexagonal hub.
        let hubR = radius * 0.42
        let hub = NSBezierPath()
        for i in 0..<6 {
            let theta = rotation + CGFloat(i) * (.pi / 3)
            let x = center.x + cos(theta) * hubR
            let y = center.y + sin(theta) * hubR
            if i == 0 { hub.move(to: NSPoint(x: x, y: y)) }
            else      { hub.line(to: NSPoint(x: x, y: y)) }
        }
        hub.close()
        NSColor(srgbRed: 215/255, green: 210/255, blue: 200/255,
                alpha: 1.0).setFill()
        hub.fill()

        // Six radial spokes inside the hub.
        let inner = hubR * 0.15
        NSColor(srgbRed: 80/255, green: 70/255, blue: 60/255,
                alpha: 0.4).setStroke()
        let spokes = NSBezierPath()
        spokes.lineWidth = 1.5
        for i in 0..<6 {
            let theta = rotation + CGFloat(i) * (.pi / 3)
            spokes.move(to: NSPoint(x: center.x + cos(theta) * inner,
                                     y: center.y + sin(theta) * inner))
            spokes.line(to: NSPoint(x: center.x + cos(theta) * hubR * 0.9,
                                     y: center.y + sin(theta) * hubR * 0.9))
        }
        spokes.stroke()

        // Drive pin.
        let pinR: CGFloat = 3
        let pinRect = NSRect(x: center.x - pinR, y: center.y - pinR,
                              width: pinR * 2, height: pinR * 2)
        NSColor.black.withAlphaComponent(0.85).setFill()
        NSBezierPath(ovalIn: pinRect).fill()
    }

    // MARK: - Palette (time-of-day color tint)

    private struct Palette {
        let bodyHi: NSColor
        let bodyLo: NSColor
        let labelHi: NSColor
        let labelLo: NSColor
        let stripe: NSColor
        let waveform: NSColor
        let spool: NSColor
    }

    /// Body color shifts with the hour the tape was recorded. Four
    /// distinct moods so a folder of tapes self-organizes visually:
    ///
    ///   • morning (06–12) — warm pink/peach
    ///   • afternoon (12–18) — classic Walkman steel-blue
    ///   • evening (18–22) — sunset orange/red
    ///   • night (22–06) — deep navy/violet
    private static func palette(for date: Date) -> Palette {
        let cal = Calendar.current
        let hour = cal.component(.hour, from: date)
        switch hour {
        case 6..<12:
            return Palette(
                bodyHi: NSColor(srgbRed: 255/255, green: 175/255, blue: 145/255, alpha: 1),
                bodyLo: NSColor(srgbRed: 210/255, green: 110/255, blue:  90/255, alpha: 1),
                labelHi: NSColor(srgbRed: 252/255, green: 240/255, blue: 215/255, alpha: 1),
                labelLo: NSColor(srgbRed: 232/255, green: 215/255, blue: 180/255, alpha: 1),
                stripe: NSColor(srgbRed: 235/255, green: 100/255, blue:  70/255, alpha: 1),
                waveform: NSColor(srgbRed: 160/255, green: 60/255, blue: 40/255, alpha: 0.92),
                spool: NSColor(srgbRed:  85/255, green:  50/255, blue:  40/255, alpha: 1))
        case 12..<18:
            return Palette(
                bodyHi: NSColor(srgbRed: 110/255, green: 135/255, blue: 175/255, alpha: 1),
                bodyLo: NSColor(srgbRed:  52/255, green:  68/255, blue: 105/255, alpha: 1),
                labelHi: NSColor(srgbRed: 252/255, green: 246/255, blue: 224/255, alpha: 1),
                labelLo: NSColor(srgbRed: 230/255, green: 220/255, blue: 192/255, alpha: 1),
                stripe: NSColor(srgbRed: 215/255, green: 175/255, blue:  70/255, alpha: 1),
                waveform: NSColor(srgbRed:  40/255, green:  55/255, blue:  90/255, alpha: 0.92),
                spool: NSColor(srgbRed:  70/255, green:  55/255, blue:  45/255, alpha: 1))
        case 18..<22:
            return Palette(
                bodyHi: NSColor(srgbRed: 235/255, green: 115/255, blue:  75/255, alpha: 1),
                bodyLo: NSColor(srgbRed: 160/255, green:  50/255, blue:  60/255, alpha: 1),
                labelHi: NSColor(srgbRed: 250/255, green: 235/255, blue: 200/255, alpha: 1),
                labelLo: NSColor(srgbRed: 225/255, green: 200/255, blue: 165/255, alpha: 1),
                stripe: NSColor(srgbRed: 240/255, green: 90/255, blue: 60/255, alpha: 1),
                waveform: NSColor(srgbRed: 130/255, green: 30/255, blue: 40/255, alpha: 0.92),
                spool: NSColor(srgbRed:  90/255, green:  45/255, blue:  35/255, alpha: 1))
        default: // 22..6
            return Palette(
                bodyHi: NSColor(srgbRed:  78/255, green:  70/255, blue: 120/255, alpha: 1),
                bodyLo: NSColor(srgbRed:  32/255, green:  28/255, blue:  60/255, alpha: 1),
                labelHi: NSColor(srgbRed: 230/255, green: 220/255, blue: 235/255, alpha: 1),
                labelLo: NSColor(srgbRed: 200/255, green: 190/255, blue: 215/255, alpha: 1),
                stripe: NSColor(srgbRed: 175/255, green: 100/255, blue: 220/255, alpha: 1),
                waveform: NSColor(srgbRed:  50/255, green:  40/255, blue:  85/255, alpha: 0.92),
                spool: NSColor(srgbRed:  60/255, green:  50/255, blue:  75/255, alpha: 1))
        }
    }
}
