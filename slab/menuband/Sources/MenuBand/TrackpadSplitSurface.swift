import AppKit

/// ⇧Tab bisects the trackpad down the center: the left half is the pitch /
/// echo slider, the right half is TrackDrum. The cut is literal — the drum
/// keeps its whole zone map and the finger simply only reaches the right
/// half of it — and a finger belongs to whichever half it landed on until
/// it lifts, so a strike that drifts left never turns into a bend and a
/// slide that drifts right never strikes.
enum TrackpadSplitSurface {
    /// Normalized x where the pad divides. Everything left of it is pitch.
    static let divide: CGFloat = 0.5

    static func isPitchSide(_ point: CGPoint) -> Bool {
        point.x < divide
    }

    /// The slider is a relative control, so its half is stretched back out
    /// to full travel: x 0…divide → 0…1. Unclamped on purpose — a finger
    /// that started here keeps sliding wherever it goes.
    static func pitchPoint(_ point: CGPoint) -> CGPoint {
        CGPoint(x: point.x / divide, y: point.y)
    }

    /// One frame of the bisected pad, each half fed as if it owned the
    /// fingers it was given.
    struct Frame: Equatable {
        var pitch: [TrackpadContact] = []
        var drumTouches: [CGPoint] = []
        var drumBegan: [CGPoint] = []
        var drumLifted: [CGPoint] = []
    }

    /// Remembers which half each contact landed on. Ownership is decided
    /// once, at the contact's first frame, and forgotten when it lifts.
    struct Ownership: Equatable {
        private(set) var pitchIDs: Set<Int32> = []
        private(set) var drumIDs: Set<Int32> = []

        var drumIsHeld: Bool { !drumIDs.isEmpty }

        /// `previous` is last frame's point per contact id, used to place
        /// a lifted drum finger's release.
        mutating func resolve(previous: [Int32: CGPoint],
                              active: [TrackpadContact]) -> Frame {
            var frame = Frame()
            let liveIDs = Set(active.map(\.identifier))
            for id in drumIDs.subtracting(liveIDs) {
                if let point = previous[id] { frame.drumLifted.append(point) }
            }
            drumIDs.formIntersection(liveIDs)
            pitchIDs.formIntersection(liveIDs)
            for contact in active {
                if pitchIDs.contains(contact.identifier) {
                    frame.pitch.append(remapped(contact))
                } else if drumIDs.contains(contact.identifier) {
                    frame.drumTouches.append(contact.point)
                } else if isPitchSide(contact.point) {
                    pitchIDs.insert(contact.identifier)
                    frame.pitch.append(remapped(contact))
                } else {
                    drumIDs.insert(contact.identifier)
                    frame.drumTouches.append(contact.point)
                    frame.drumBegan.append(contact.point)
                }
            }
            return frame
        }

        /// The drum-owned fingers as they stand, for frames between
        /// hardware callbacks (the membrane keeps moving under a held hand).
        func drumTouches(in contacts: [Int32: CGPoint]) -> [CGPoint] {
            drumIDs.compactMap { contacts[$0] }
        }

        /// The slider-owned fingers, in raw pad coordinates, so the chart
        /// can show the hand that is bending.
        func pitchTouches(in contacts: [Int32: CGPoint]) -> [CGPoint] {
            pitchIDs.compactMap { contacts[$0] }
        }

        mutating func reset() {
            pitchIDs.removeAll()
            drumIDs.removeAll()
        }

        private func remapped(_ contact: TrackpadContact) -> TrackpadContact {
            TrackpadContact(identifier: contact.identifier,
                            point: pitchPoint(contact.point),
                            state: contact.state)
        }
    }

    /// The picture stays one trackpad: the whole TrackDrum skin at its own
    /// size, with the pitch chart laid over its left half and a seam down
    /// the middle. The drum's left zones vanish under the chart exactly as
    /// they do under the finger.
    static let imageSize = NSSize(width: 140, height: 88)

    static func image(chart: NSImage, skin: NSImage,
                      pitchTouches: [CGPoint] = [],
                      appearance: NSAppearance? = nil) -> NSImage {
        let appearance = appearance ?? NSApp.effectiveAppearance
        let isDark = appearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        return NSImage(size: imageSize, flipped: false) { rect in
            skin.draw(in: rect, from: .zero, operation: .sourceOver, fraction: 1)
            let left = NSRect(x: rect.minX, y: rect.minY,
                              width: rect.width * divide, height: rect.height)
            // The chart sits just inside the skin's outer rim so the rim
            // wraps the whole pad; its right edge is pushed past the seam
            // and clipped off, so its rounded corners never show drum
            // through the cut.
            let rim: CGFloat = 2
            NSGraphicsContext.saveGraphicsState()
            NSBezierPath(rect: left).addClip()
            chart.draw(in: NSRect(x: left.minX + rim, y: left.minY + rim,
                                  width: left.width + 8 - rim,
                                  height: left.height - rim * 2),
                       from: .zero, operation: .sourceOver, fraction: 1)
            // The bending fingers show on the chart the way strikes show
            // on the skin, so the slider half answers touch just as fast.
            for touch in pitchTouches {
                let px = rect.minX + max(0, min(1, touch.x)) * rect.width
                let py = rect.minY + max(0, min(1, touch.y)) * rect.height
                let dot = NSBezierPath(ovalIn: NSRect(x: px - 4, y: py - 4,
                                                      width: 8, height: 8))
                NSColor.controlAccentColor.withAlphaComponent(0.85).setFill()
                dot.fill()
                NSColor.white.withAlphaComponent(0.9).setStroke()
                dot.lineWidth = 1
                dot.stroke()
            }
            NSGraphicsContext.restoreGraphicsState()
            (isDark ? NSColor.white.withAlphaComponent(0.22)
                    : NSColor.black.withAlphaComponent(0.32)).setStroke()
            let seam = NSBezierPath()
            seam.move(to: NSPoint(x: left.maxX, y: rect.minY + 4))
            seam.line(to: NSPoint(x: left.maxX, y: rect.maxY - 4))
            seam.lineWidth = 1
            seam.stroke()
            return true
        }
    }
}

/// Headless capture of the bisected pad. Flags after `--render-split`:
/// `--bend -1…1`, `--echo -1…1`, `--touch x,y` (repeatable; landed side
/// decides the job), `--dark`/`--light`, `--scale`, `--out`.
enum TrackpadSplitCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-split") else { return false }
        func value(_ flag: String) -> String? {
            guard let index = args.firstIndex(of: flag), index + 1 < args.count else {
                return nil
            }
            return args[index + 1]
        }
        let output = value("--out") ?? "/tmp/menuband-split.png"
        let scale = max(1, Double(value("--scale") ?? "4") ?? 4)
        let bend = Float(value("--bend") ?? "0") ?? 0
        let echo = Float(value("--echo") ?? "0") ?? 0
        let touches: [CGPoint] = zip(args, args.dropFirst()).compactMap { flag, raw in
            guard flag == "--touch" else { return nil }
            let parts = raw.split(separator: ",").compactMap { Double($0) }
            guard parts.count == 2 else { return nil }
            return CGPoint(x: parts[0], y: parts[1])
        }
        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        let dark = args.contains("--dark") && !args.contains("--light")
        app.appearance = NSAppearance(named: dark ? .darkAqua : .aqua)

        var ownership = TrackpadSplitSurface.Ownership()
        let frame = ownership.resolve(
            previous: [:],
            active: touches.enumerated().map {
                TrackpadContact(identifier: Int32($0.offset), point: $0.element, state: 3)
            }
        )
        let image = TrackpadSplitSurface.image(
            chart: PitchBendCursor.image(forBend: bend, echo: echo,
                                         keyDown: !frame.pitch.isEmpty),
            skin: TrackpadDrumSkinPad.image(touches: frame.drumTouches,
                                            appearance: app.appearance),
            pitchTouches: frame.pitch.map { CGPoint(x: $0.point.x / 2, y: $0.point.y) },
            appearance: app.appearance
        )
        let size = image.size
        let pixelWidth = Int((size.width * scale).rounded())
        let pixelHeight = Int((size.height * scale).rounded())
        guard let bitmap = NSBitmapImageRep(
            bitmapDataPlanes: nil, pixelsWide: pixelWidth, pixelsHigh: pixelHeight,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0
        ) else { return true }
        bitmap.size = size
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmap)
        image.draw(in: NSRect(origin: .zero, size: size))
        NSGraphicsContext.restoreGraphicsState()
        guard let png = bitmap.representation(using: .png, properties: [:]) else {
            return true
        }
        do { try png.write(to: URL(fileURLWithPath: output)) }
        catch {
            FileHandle.standardError.write(Data("split write failed: \(error)\n".utf8))
            exit(1)
        }
        print("split \(pixelWidth)x\(pixelHeight) → \(output)")
        return true
    }
}
