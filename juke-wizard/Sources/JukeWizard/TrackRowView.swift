// TrackRowView.swift — one dressed-up row in the Winamp track list: a colored
// chip tinted by the track's lane, a rounded album-art thumbnail, the title +
// artist, and little platform badges (Spotify / Apple / YouTube / DistroKid)
// for released tracks. No star ratings. Reused by the table via makeView.
import AppKit

final class TrackRowView: NSView {
    static let id = NSUserInterfaceItemIdentifier("TrackRow")
    static let height: CGFloat = 46

    private let thumb = NSImageView()
    private let titleField = NSTextField(labelWithString: "")
    private let artistField = NSTextField(labelWithString: "")
    private let badges = NSTextField(labelWithString: "")
    private var tint = NSColor.clear
    var selected = false { didSet { needsDisplay = true } }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        thumb.imageScaling = .scaleProportionallyUpOrDown
        thumb.wantsLayer = true
        thumb.layer?.cornerRadius = 5
        thumb.layer?.masksToBounds = true
        thumb.layer?.borderWidth = 1
        thumb.layer?.borderColor = NSColor.white.withAlphaComponent(0.15).cgColor
        titleField.font = .systemFont(ofSize: 13, weight: .semibold)
        titleField.lineBreakMode = .byTruncatingTail
        artistField.font = .systemFont(ofSize: 10.5)
        artistField.textColor = .secondaryLabelColor
        artistField.lineBreakMode = .byTruncatingTail
        badges.font = .systemFont(ofSize: 12, weight: .bold)
        badges.alignment = .right
        for v in [thumb, titleField, artistField, badges] { addSubview(v) }
    }
    required init?(coder: NSCoder) { fatalError() }
    override var isFlipped: Bool { true }

    func configure(_ t: Track) {
        // fallback cover: a colored disc placeholder if no art
        if let p = t.meta?.art, let img = NSImage(contentsOf: URL(fileURLWithPath: (p as NSString).expandingTildeInPath)) {
            thumb.image = img
        } else {
            thumb.image = TrackRowView.placeholder(for: t.lane)
        }
        let cc = t.data.comments.count
        titleField.stringValue = t.title
        titleField.textColor = TrackRowView.titleColor(t)
        artistField.stringValue = (t.meta?.artist ?? "Aesthetic Dot Computer")
            + (cc > 0 ? "   💬\(cc)" : "")
        badges.attributedStringValue = TrackRowView.badgeString(t)
        tint = TrackRowView.laneTint(t.lane)
        needsDisplay = true
        needsLayout = true
    }

    override func layout() {
        super.layout()
        let h = bounds.height, pad: CGFloat = 8
        let side = h - 10
        thumb.frame = NSRect(x: pad, y: 5, width: side, height: side)
        let tx = pad + side + 9
        let bw: CGFloat = 96
        let textW = bounds.width - tx - bw - pad
        titleField.frame = NSRect(x: tx, y: 6, width: textW, height: 17)
        artistField.frame = NSRect(x: tx, y: 24, width: textW, height: 14)
        badges.frame = NSRect(x: bounds.width - bw - pad, y: 14, width: bw, height: 18)
    }

    override func draw(_ dirtyRect: NSRect) {
        // colored chip tinted by lane; brighter when selected
        let r = bounds.insetBy(dx: 3, dy: 2)
        let path = NSBezierPath(roundedRect: r, xRadius: 7, yRadius: 7)
        (selected ? tint.blended(withFraction: 0.45, of: .white) ?? tint
                  : tint).withAlphaComponent(selected ? 0.55 : 0.22).setFill()
        path.fill()
        if selected {
            tint.withAlphaComponent(0.9).setStroke()
            path.lineWidth = 1.5
            path.stroke()
        }
    }

    // ── styling helpers ──────────────────────────────────────────────────────
    static func laneTint(_ lane: String) -> NSColor {
        // stable hue from the lane name → each lane reads as its own color
        var h: UInt64 = 1469598103934665603
        for b in lane.utf8 { h = (h ^ UInt64(b)) &* 1099511628211 }
        let hue = CGFloat(h % 360) / 360.0
        return NSColor(calibratedHue: hue, saturation: 0.62, brightness: 0.95, alpha: 1)
    }
    static func titleColor(_ t: Track) -> NSColor {
        JukeController.statusColor(t.meta?.status)
    }
    static func badgeString(_ t: Track) -> NSAttributedString {
        let out = NSMutableAttributedString()
        func add(_ s: String, _ c: NSColor, on: Bool) {
            guard on else { return }
            out.append(NSAttributedString(string: s + " ", attributes: [.foregroundColor: c]))
        }
        let l = t.meta?.links
        add("♫", NSColor(srgbRed: 0.11, green: 0.73, blue: 0.33, alpha: 1), on: l?.spotify != nil)
        add("", NSColor(srgbRed: 0.98, green: 0.30, blue: 0.40, alpha: 1), on: l?.apple != nil)
        add("▶", NSColor(srgbRed: 0.95, green: 0.20, blue: 0.20, alpha: 1), on: l?.youtube != nil)
        add("◆", NSColor(srgbRed: 0.55, green: 0.75, blue: 0.95, alpha: 1), on: l?.distrokid != nil)
        return out
    }
    static func placeholder(for lane: String) -> NSImage {
        let side: CGFloat = 40
        let img = NSImage(size: NSSize(width: side, height: side))
        img.lockFocus()
        laneTint(lane).withAlphaComponent(0.9).setFill()
        NSBezierPath(ovalIn: NSRect(x: 0, y: 0, width: side, height: side)).fill()
        NSColor.black.withAlphaComponent(0.55).setFill()
        NSBezierPath(ovalIn: NSRect(x: side/2-5, y: side/2-5, width: 10, height: 10)).fill()
        img.unlockFocus()
        return img
    }
}
