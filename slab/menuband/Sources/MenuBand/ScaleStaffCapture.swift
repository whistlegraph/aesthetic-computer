import AppKit

/// A compact, code-native teaching staff used by Menu Band's reel renderer.
/// The accumulated scale remains visible while the newest note is accented.
final class ScaleStaffView: NSView {
    var notes: [UInt8] = [] { didSet { needsDisplay = true } }

    override var intrinsicContentSize: NSSize { NSSize(width: 520, height: 128) }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let ink = NSColor(calibratedRed: 0.12, green: 0.09, blue: 0.17, alpha: 0.92)
        let accent = NSColor(calibratedRed: 0.71, green: 0.25, blue: 0.77, alpha: 1)
        let left: CGFloat = 66, right = bounds.width - 22
        let space: CGFloat = 13, bottom: CGFloat = 35

        ink.setStroke()
        for line in 0..<5 {
            let y = bottom + CGFloat(line) * space
            let p = NSBezierPath()
            p.move(to: NSPoint(x: left, y: y))
            p.line(to: NSPoint(x: right, y: y))
            p.lineWidth = 1.35
            p.stroke()
        }
        Bravura.draw(.gClef, at: NSPoint(x: 17, y: -7), staffSpace: space, color: ink)

        // C4..C5 are diatonic steps 0...7; E4 (step 2) lies on the bottom
        // staff line. In this flipped view, higher pitches move upward.
        let steps: [UInt8: CGFloat] = [60: 0, 62: 1, 64: 2, 65: 3,
                                      67: 4, 69: 5, 71: 6, 72: 7]
        let count = notes.count
        for (index, midi) in notes.enumerated() {
            guard let step = steps[midi] else { continue }
            let x = left + 43 + CGFloat(index) * 47
            let centerY = bottom - space + step * (space / 2)
            if midi == 60 {
                let ledger = NSBezierPath()
                ledger.move(to: NSPoint(x: x - 9, y: centerY))
                ledger.line(to: NSPoint(x: x + 22, y: centerY))
                ledger.lineWidth = 1.35
                ledger.stroke()
            }
            let color = index == count - 1 ? accent : ink
            color.setFill()
            let head = NSBezierPath(ovalIn: NSRect(x: x, y: centerY - 4.7,
                                                   width: 18, height: 9.4))
            var transform = AffineTransform()
            transform.translate(x: x + 9, y: centerY)
            transform.rotate(byDegrees: -14)
            transform.translate(x: -(x + 9), y: -centerY)
            head.transform(using: transform)
            head.fill()
            color.setStroke()
            let stem = NSBezierPath()
            stem.move(to: NSPoint(x: x + 16, y: centerY))
            stem.line(to: NSPoint(x: x + 16, y: centerY + 39))
            stem.lineWidth = 1.7
            stem.stroke()
        }
    }
}

enum ScaleStaffCLI {
    static func runIfRequested(_ args: [String]) -> Bool {
        guard args.contains("--render-scale-staff") else { return false }
        func val(_ flag: String) -> String? {
            guard let i = args.firstIndex(of: flag), i + 1 < args.count else { return nil }
            return args[i + 1]
        }
        let out = val("--out") ?? "/tmp/menu-band-staff.png"
        let scale = max(1, Double(val("--scale") ?? "2") ?? 2)
        let notes = (val("--notes") ?? "").split(separator: ",").compactMap { UInt8($0) }
        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        app.appearance = NSAppearance(named: .aqua)
        let view = ScaleStaffView()
        view.notes = notes
        view.frame = NSRect(origin: .zero, size: view.intrinsicContentSize)
        view.displayIfNeeded()
        let pw = Int((view.bounds.width * scale).rounded())
        let ph = Int((view.bounds.height * scale).rounded())
        guard let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: pw,
            pixelsHigh: ph, bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
            isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0,
            bitsPerPixel: 0) else { return true }
        rep.size = view.bounds.size
        view.cacheDisplay(in: view.bounds, to: rep)
        guard let png = rep.representation(using: .png, properties: [:]) else { return true }
        do { try png.write(to: URL(fileURLWithPath: out)) }
        catch { FileHandle.standardError.write(Data("staff write failed: \(error)\n".utf8)); exit(1) }
        print("staff \(pw)x\(ph) notes=\(notes) → \(out)")
        return true
    }
}
