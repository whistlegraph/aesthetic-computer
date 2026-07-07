// AffirmationsPlugin — a little caption beneath Fía's star that @jeffrey can
// change remotely. The star polls an aesthetic.computer endpoint
//   GET /api/macpal-status?to=<recipient>  →  { to, text, seq, sound?, volume? }
// every ~45s; when the `seq` bumps, the new affirmation slides in, the name
// hops, and a chime plays — Glass by default, or whichever named system sound
// (and volume) the payload carries. The last affirmation is cached to disk so
// she still sees it offline / on the next launch.
//
// @jeffrey pushes one with: node macpal/affirm.mjs "proud of you 💛" --to fia
//
// Uses the shared MarqueeField (from FuserPlugin.swift) so a long affirmation
// scrolls gracefully instead of truncating.

import AppKit

final class AffirmationsPlugin: NSObject, PalPlugin, WidthHinting {
    private let recipient: String
    private let host: String          // e.g. "https://aesthetic.computer"
    private let stateFile: String     // cache: "<seq>\t<text>"
    private weak var c: PalController?

    private let caption = MarqueeField()
    private var text = ""
    private var seq = -1
    private var polling = false
    private var tickCount = 0
    private let pollEvery = 45         // core ticks at ~1Hz → ~45s

    private let captionH: CGFloat = 20
    private let gap: CGFloat = 4
    private let wideW: CGFloat = 224   // room for a sentence; star's default is narrower

    var preferredWidth: CGFloat { text.isEmpty ? 160 : wideW }

    init(recipient: String, host: String, supportDir: String) {
        self.recipient = recipient
        self.host = host.hasSuffix("/") ? String(host.dropLast()) : host
        self.stateFile = supportDir + "/affirmation"
        super.init()
        // Seed from the on-disk cache so something shows before the first poll.
        if let raw = try? String(contentsOfFile: stateFile, encoding: .utf8) {
            let parts = raw.split(separator: "\t", maxSplits: 1)
            if let first = parts.first, let s = Int(first) {
                seq = s
                text = parts.count > 1 ? String(parts[1]) : ""
            }
        }
    }

    func attach(to controller: PalController) {
        c = controller
        controller.content.addSubview(caption)
        if !text.isEmpty { renderCaption() }
    }

    func stackHeight(in controller: PalController) -> CGFloat {
        text.isEmpty ? 0 : captionH + gap
    }

    func layoutRows(in controller: PalController, originY: CGFloat) {
        caption.isHidden = text.isEmpty
        let w = controller.fullWidth
        caption.frame = NSRect(x: 0, y: originY, width: w, height: captionH)
    }

    func setCollapsed(_ collapsed: Bool) {
        caption.isHidden = collapsed || text.isEmpty
    }

    func tick() {
        if tickCount % pollEvery == 0 { poll() }
        tickCount += 1
    }

    // ── network ────────────────────────────────────────────────────────────
    private func poll() {
        guard !polling, let url = URL(string: "\(host)/api/macpal-status?to=\(recipient)") else { return }
        polling = true
        var req = URLRequest(url: url)
        req.timeoutInterval = 12
        req.cachePolicy = .reloadIgnoringLocalCacheData
        URLSession.shared.dataTask(with: req) { [weak self] data, _, _ in
            guard let self = self else { return }
            defer { self.polling = false }
            guard let data = data,
                  let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let newSeq = (obj["seq"] as? Int) ?? (obj["seq"] as? NSNumber)?.intValue
            else { return }
            let newText = (obj["text"] as? String) ?? ""
            let newSound = obj["sound"] as? String
            let newVolume = (obj["volume"] as? Double) ?? (obj["volume"] as? NSNumber)?.doubleValue
            guard newSeq != self.seq else { return }
            DispatchQueue.main.async {
                self.apply(seq: newSeq, text: newText, sound: newSound, volume: newVolume)
            }
        }.resume()
    }

    private func apply(seq newSeq: Int, text newText: String, sound: String? = nil, volume: Double? = nil) {
        let firstFill = seq < 0
        seq = newSeq
        text = newText
        try? "\(seq)\t\(text)".write(toFile: stateFile, atomically: true, encoding: .utf8)
        renderCaption()
        c?.layout()
        // Celebrate a genuinely new message (not the silent seed on launch).
        // The chime is server-programmable; an unknown name falls back to Glass.
        if !firstFill, !text.isEmpty {
            c?.nameLabel.bounce()
            let named = sound.flatMap { $0.isEmpty ? nil : $0 } ?? "Glass"
            let snd = NSSound(named: named) ?? NSSound(named: "Glass")
            snd?.volume = Float(min(1, max(0, volume ?? 0.35)))
            snd?.play()
        }
    }

    private func renderCaption() {
        let para = NSMutableParagraphStyle()
        para.alignment = .center; para.lineBreakMode = .byClipping
        let attr = NSAttributedString(string: text, attributes: [
            .font: playfulFont(15, bold: true),
            .foregroundColor: NSColor.white,
            .strokeColor: NSColor(white: 0.08, alpha: 1),
            .strokeWidth: -3.0,
            .paragraphStyle: para,
        ])
        caption.setText(attr)
    }
}
