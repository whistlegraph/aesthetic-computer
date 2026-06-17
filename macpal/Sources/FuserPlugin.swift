// FuserPlugin — the machine-badge features that stack beneath the pal on the
// fuser fleet (neo / panda / chicken / blueberry): a color-coded git status
// line (marquee when it overflows), the machine's current Asana tasks, an
// ⚡OVERTIME alarm + work queue, and a live ANSI "terminal" pane tailing a log.
//
// Every input is a plain file polled at runtime, so content updates never need
// a recompile:
//   <home>/gitstatus        "<ahead> <behind>" (badge-git-sync.sh) or "local"
//   <home>/tasks            Asana task lines    (badge-asana-sync.sh on neo)
//   <home>/overtime         flag file; presence = OVERTIME on
//   <home>/overtime-status  work-queue lines while in OVERTIME
//   <home>/pane.log         the log the terminal pane tails (optional)
// where <home> is the fuser state dir (~/.local/share/desktop-badge), which is
// also PalConfig.supportDir for the fuser profile.
//
// Carved out of the original desktop-badge.swift; the avatar/sing/collapse/drag
// machinery now lives in PalCore.swift and is shared with the star.

import AppKit

func monoFont(_ pt: CGFloat) -> NSFont {
    for n in ["MonaspaceArgon-Bold", "SFMono-Semibold", "Menlo-Bold", "MonaspaceArgon-Regular", "Menlo"] {
        if let f = NSFont(name: n, size: pt) { return f }
    }
    return NSFont.monospacedSystemFont(ofSize: pt, weight: .semibold)
}

// Visual-only overlay — never intercepts the pane's clicks/scroll/selection.
final class GhostView: NSView {
    override func hitTest(_ p: NSPoint) -> NSView? { nil }
}

// ── marquee status field ──────────────────────────────────────────────────
// No truncation: the styled status line is baked (accent shadow and all) into
// an image; when it fits it centers like a label, and when it overflows two
// copies scroll in an endless leftward loop.
final class MarqueeField: NSView {
    private let lead = CALayer()
    private let trail = CALayer()
    private var textW: CGFloat = 0
    private var textH: CGFloat = 0
    private let gap: CGFloat = 28
    private let speed: CGFloat = 26   // px/s

    init() {
        super.init(frame: .zero)
        layer = CALayer()
        wantsLayer = true
        layer?.masksToBounds = true
        for l in [lead, trail] { l.anchorPoint = .zero; layer?.addSublayer(l) }
    }
    required init?(coder: NSCoder) { fatalError("no nib") }

    func setText(_ attr: NSAttributedString) {
        let m = NSMutableAttributedString(attributedString: attr)
        let sh = NSShadow()
        sh.shadowColor = accent; sh.shadowBlurRadius = 0
        sh.shadowOffset = NSSize(width: 2, height: -2)
        m.addAttribute(.shadow, value: sh, range: NSRange(location: 0, length: m.length))
        let size = m.size()
        textW = ceil(size.width) + 2; textH = ceil(size.height) + 2
        let img = NSImage(size: NSSize(width: textW, height: textH), flipped: false) { _ in
            m.draw(at: NSPoint(x: 0, y: 2))
            return true
        }
        let scale = NSScreen.main?.backingScaleFactor ?? 2
        for l in [lead, trail] {
            l.contentsScale = scale
            l.contents = img
            l.bounds = CGRect(x: 0, y: 0, width: textW, height: textH)
        }
        relayout()
    }

    override func setFrameSize(_ s: NSSize) { super.setFrameSize(s); relayout() }

    private func relayout() {
        CATransaction.begin(); CATransaction.setDisableActions(true)
        lead.removeAnimation(forKey: "marquee"); trail.removeAnimation(forKey: "marquee")
        let y = (bounds.height - textH) / 2
        if textW <= bounds.width {
            trail.isHidden = true
            lead.position = CGPoint(x: (bounds.width - textW) / 2, y: y)
        } else {
            trail.isHidden = false
            let span = textW + gap
            lead.position = CGPoint(x: 0, y: y)
            trail.position = CGPoint(x: span, y: y)
            for l in [lead, trail] {
                let a = CABasicAnimation(keyPath: "transform.translation.x")
                a.fromValue = 0; a.toValue = -span
                a.duration = CFTimeInterval(span / speed)
                a.repeatCount = .infinity
                l.add(a, forKey: "marquee")
            }
        }
        CATransaction.commit()
    }
}

// ── ANSI + heuristic pane colorizer ──────────────────────────────────────
struct PaneTheme {
    let dark: Bool
    var defaultText: NSColor {
        dark ? NSColor(calibratedRed: 0.80, green: 0.95, blue: 0.84, alpha: 1)
             : NSColor(calibratedRed: 0.08, green: 0.16, blue: 0.10, alpha: 1)
    }
    private func hex(_ v: UInt32) -> NSColor {
        NSColor(calibratedRed: CGFloat((v >> 16) & 0xff) / 255,
                green: CGFloat((v >> 8) & 0xff) / 255,
                blue: CGFloat(v & 0xff) / 255, alpha: 1)
    }
    func ansi(_ i: Int) -> NSColor {
        let darkP: [UInt32] = [0x8b949e, 0xff6b6b, 0x7ee787, 0xffd66b,
                               0x79b8ff, 0xd2a8ff, 0x76e3ea, 0xe6edf3,
                               0xa5b1bd, 0xff8f8f, 0xa5f3b4, 0xffe28f,
                               0xa3cfff, 0xe2c5ff, 0xa6edf2, 0xffffff]
        let lightP: [UInt32] = [0x57606a, 0xc0342b, 0x1a7f37, 0x9a6700,
                                0x0969da, 0x8250df, 0x1b7c83, 0x1f2328,
                                0x6e7781, 0xa40e26, 0x116329, 0x7d4e00,
                                0x0550ae, 0x6639ba, 0x155e63, 0x24292f]
        return hex((dark ? darkP : lightP)[max(0, min(15, i))])
    }
    func rgb(_ r: Int, _ g: Int, _ b: Int) -> NSColor {
        let (cr, cg, cb) = (CGFloat(r) / 255, CGFloat(g) / 255, CGFloat(b) / 255)
        let luma = 0.299 * cr + 0.587 * cg + 0.114 * cb
        if dark && luma < 0.22 { return ansi(0) }
        if !dark && luma > 0.78 { return ansi(0) }
        return NSColor(calibratedRed: cr, green: cg, blue: cb, alpha: 1)
    }
    func ansi256(_ n: Int) -> NSColor {
        if n < 16 { return ansi(n) }
        if n < 232 {
            let v = n - 16
            let steps = [0, 95, 135, 175, 215, 255]
            return rgb(steps[(v / 36) % 6], steps[(v / 6) % 6], steps[v % 6])
        }
        let g = 8 + (n - 232) * 10
        return rgb(g, g, g)
    }
}

final class PaneRenderer {
    static let urlRx = try! NSRegularExpression(pattern: "https?://[^\\s'\"]+|localhost:\\d+")
    static let strRx = try! NSRegularExpression(pattern: "'[^']*'|\"[^\"]*\"")
    static let numRx = try! NSRegularExpression(pattern: "(?<=[\\s:,\\[(=])-?\\d+(?:\\.\\d+)?(?:ms|s|%)?(?=[\\s,}\\])]|$)")
    static let boolRx = try! NSRegularExpression(pattern: "\\b(true|false|null|undefined)\\b")

    let theme: PaneTheme
    let font = monoFont(8.5)
    private var fg: NSColor?
    private var isDim = false

    init(dark: Bool) { theme = PaneTheme(dark: dark) }

    private func attrs(_ color: NSColor?) -> [NSAttributedString.Key: Any] {
        var c = color ?? theme.defaultText
        if isDim { c = c.withAlphaComponent(0.62) }
        return [.font: font, .foregroundColor: c]
    }

    private func applySGR(_ params: [Int]) {
        if params.isEmpty { fg = nil; isDim = false; return }
        var i = 0
        while i < params.count {
            switch params[i] {
            case 0: fg = nil; isDim = false
            case 2: isDim = true
            case 22: isDim = false
            case 39: fg = nil
            case 30...37: fg = theme.ansi(params[i] - 30)
            case 90...97: fg = theme.ansi(params[i] - 90 + 8)
            case 38 where i + 2 < params.count && params[i + 1] == 5:
                fg = theme.ansi256(params[i + 2]); i += 2
            case 38 where i + 4 < params.count && params[i + 1] == 2:
                fg = theme.rgb(params[i + 2], params[i + 3], params[i + 4]); i += 4
            case 48 where i + 2 < params.count && params[i + 1] == 5: i += 2
            case 48 where i + 4 < params.count && params[i + 1] == 2: i += 4
            default: break
            }
            i += 1
        }
    }

    func render(_ raw: String) -> NSAttributedString {
        let out = NSMutableAttributedString()
        let lines = raw.split(separator: "\n", omittingEmptySubsequences: false)
        for (li, sub) in lines.enumerated() {
            var line = String(sub)
            if let r = line.lastIndex(of: "\r") { line = String(line[line.index(after: r)...]) }
            out.append(renderLine(line))
            if li < lines.count - 1 { out.append(NSAttributedString(string: "\n", attributes: attrs(nil))) }
        }
        return out
    }

    private func renderLine(_ line: String) -> NSAttributedString {
        let out = NSMutableAttributedString()
        var sawColor = fg != nil
        var seg = ""
        var i = line.startIndex
        func flush() {
            if !seg.isEmpty { out.append(NSAttributedString(string: seg, attributes: attrs(fg))); seg = "" }
        }
        while i < line.endIndex {
            guard line[i] == "\u{1B}" else { seg.append(line[i]); i = line.index(after: i); continue }
            var j = line.index(after: i)
            if j < line.endIndex, line[j] == "[" {
                j = line.index(after: j)
                var num = "", params: [Int] = [], fin: Character? = nil
                while j < line.endIndex {
                    let c = line[j]
                    if c.isNumber { num.append(c) }
                    else if c == ";" { params.append(Int(num) ?? 0); num = "" }
                    else if c == "?" || c == " " || c == "!" {}
                    else { fin = c; break }
                    j = line.index(after: j)
                }
                if !num.isEmpty { params.append(Int(num) ?? 0) }
                if fin == "m" { flush(); applySGR(params); if fg != nil { sawColor = true } }
                i = j < line.endIndex ? line.index(after: j) : line.endIndex
            } else if j < line.endIndex, line[j] == "]" {
                var k = line.index(after: j)
                while k < line.endIndex, line[k] != "\u{07}" { k = line.index(after: k) }
                i = k < line.endIndex ? line.index(after: k) : line.endIndex
            } else {
                i = j < line.endIndex ? line.index(after: j) : line.endIndex
            }
        }
        flush()
        return sawColor ? out : highlight(out.string)
    }

    private func serviceColor(_ name: String) -> NSColor {
        var h: UInt32 = 2166136261
        for b in name.utf8 { h = (h ^ UInt32(b)) &* 16777619 }
        let picks = [2, 3, 4, 5, 6, 10, 11, 12, 13, 14]
        return theme.ansi(picks[Int(h % UInt32(picks.count))])
    }

    private func highlight(_ line: String) -> NSAttributedString {
        let out = NSMutableAttributedString(string: line, attributes: attrs(nil))
        let ns = line as NSString
        var rest = NSRange(location: 0, length: ns.length)

        if let colon = line.range(of: ": "),
           line.distance(from: line.startIndex, to: colon.lowerBound) <= 40 {
            let prefix = String(line[..<colon.lowerBound])
            if prefix.contains(":"),
               prefix.allSatisfy({ $0.isLetter || $0.isNumber || ":@/_-.#".contains($0) }) {
                let plen = ns.range(of: prefix + ":").length
                out.addAttribute(.foregroundColor, value: serviceColor(prefix), range: NSRange(location: 0, length: plen))
                rest = NSRange(location: plen, length: ns.length - plen)
            }
        }

        let body = ns.substring(with: rest)
        let lower = body.lowercased()
        if lower.contains("error") || lower.contains("✖") || lower.contains("exception")
            || lower.contains("fail") || lower.contains("fatal") {
            out.addAttribute(.foregroundColor, value: theme.ansi(1), range: rest)
        } else if lower.contains("warn") || lower.contains("deprecated") {
            out.addAttribute(.foregroundColor, value: theme.ansi(3), range: rest)
        } else if lower.contains("ready in") || lower.contains("listening") || lower.contains("compiled")
            || lower.contains("✓") || lower.contains("success") || lower.contains("started") {
            out.addAttribute(.foregroundColor, value: theme.ansi(2), range: rest)
        } else if body.trimmingCharacters(in: .whitespaces).hasPrefix("at ") {
            out.addAttribute(.foregroundColor, value: theme.defaultText.withAlphaComponent(0.55), range: rest)
        } else {
            if body.contains("'") || body.contains("\"") {
                for m in Self.strRx.matches(in: line, range: rest) {
                    out.addAttribute(.foregroundColor, value: theme.ansi(10), range: m.range)
                }
            }
            if body.rangeOfCharacter(from: .decimalDigits) != nil {
                for m in Self.numRx.matches(in: line, range: rest) {
                    out.addAttribute(.foregroundColor, value: theme.ansi(11), range: m.range)
                }
            }
            for m in Self.boolRx.matches(in: line, range: rest) {
                out.addAttribute(.foregroundColor, value: theme.ansi(5), range: m.range)
            }
        }
        if body.contains("http") || body.contains("localhost") {
            for m in Self.urlRx.matches(in: line, range: rest) {
                out.addAttribute(.foregroundColor, value: theme.ansi(6), range: m.range)
            }
        }
        return out
    }
}

// ── the plugin ───────────────────────────────────────────────────────────
final class FuserPlugin: NSObject, PalPlugin, WidthHinting {
    private let home: String       // fuser state dir (== config.supportDir)
    private let repo: String       // repo to read git status from
    private weak var c: PalController?

    private var statusFile: String { home + "/gitstatus" }
    private var paneLog: String { home + "/pane.log" }
    private var tasksFile: String { home + "/tasks" }
    private var overtimeFlag: String { home + "/overtime" }
    private var overtimeStatusFile: String { home + "/overtime-status" }

    let statusField = MarqueeField()
    let tasksField = NSTextField(labelWithString: "")
    let overtimeChip = NSTextField(labelWithString: "")
    let overtimeField = NSTextField(labelWithString: "")
    var taskLines: [String] = []
    var overtimeOn = false
    var overtimeLines: [String] = []

    var paneContainer: NSView?
    var paneScroll: NSScrollView?
    var paneView: NSTextView?
    var paneFlash: NSView?
    var lastPaneRaw = ""
    var lastPaneDark: Bool?
    var refreshing = false
    var curPaneH: CGFloat = 0

    let hasPane: Bool
    let maxPaneH: CGFloat = 470
    let badgeW: CGFloat = 236
    let pad: CGFloat = 7
    var paneW: CGFloat { badgeW - pad * 2 }
    var preferredWidth: CGFloat { badgeW }

    private var tick4 = 0   // git refresh every 4th tick (~4s)

    init(home: String, repo: String) {
        self.home = home
        self.repo = repo
        self.hasPane = FileManager.default.fileExists(atPath: home + "/pane.log")
        super.init()
    }

    // ── git off-main ─────────────────────────────────────────────────────
    private func git(_ a: [String]) -> String {
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/usr/bin/git")
        // fsmonitor=false: the repo's daemon belongs to the login session; git
        // spawned from a LaunchAgent hangs for minutes handshaking with it.
        p.arguments = ["-C", repo, "-c", "core.fsmonitor=false"] + a
        let out = Pipe(); p.standardOutput = out; p.standardError = Pipe()
        do { try p.run() } catch { return "" }
        p.waitUntilExit()
        let d = out.fileHandleForReading.readDataToEndOfFile()
        return (String(data: d, encoding: .utf8) ?? "").trimmingCharacters(in: .whitespacesAndNewlines)
    }

    private func tailLog(_ path: String, maxBytes: UInt64 = 48000) -> String {
        guard let fh = FileHandle(forReadingAtPath: path) else { return "" }
        defer { try? fh.close() }
        let end = (try? fh.seekToEnd()) ?? 0
        let start = end > maxBytes ? end - maxBytes : 0
        try? fh.seek(toOffset: start)
        let data = (try? fh.readToEnd()) ?? Data()
        var s = String(data: data, encoding: .utf8) ?? String(decoding: data, as: UTF8.self)
        if start > 0, let nl = s.firstIndex(of: "\n") { s = String(s[s.index(after: nl)...]) }
        return s.trimmingCharacters(in: CharacterSet.newlines)
    }

    // ── PalPlugin ──────────────────────────────────────────────────────────
    func attach(to controller: PalController) {
        c = controller
        controller.styleField(tasksField); controller.styleField(overtimeField)
        tasksField.maximumNumberOfLines = 3
        overtimeField.maximumNumberOfLines = 4
        overtimeChip.isBordered = false
        overtimeChip.drawsBackground = false
        overtimeChip.alignment = .center
        overtimeChip.wantsLayer = true
        overtimeChip.layer?.masksToBounds = false
        let chipShadow = NSShadow()
        chipShadow.shadowColor = hexColor(0x0a3cff)   // alarm blue
        chipShadow.shadowBlurRadius = 2
        chipShadow.shadowOffset = NSSize(width: 2, height: -2)
        overtimeChip.shadow = chipShadow

        controller.content.addSubview(statusField)
        controller.content.addSubview(tasksField)
        controller.content.addSubview(overtimeChip)
        controller.content.addSubview(overtimeField)

        if hasPane {
            let container = NSView(frame: NSRect(x: pad, y: 8, width: paneW, height: 10))
            container.wantsLayer = true
            container.layer?.shadowColor = accent.cgColor
            container.layer?.shadowOffset = CGSize(width: 2, height: -2)
            container.layer?.shadowRadius = 0
            container.layer?.shadowOpacity = 1
            container.layer?.masksToBounds = false
            let scroll = NSScrollView(frame: container.bounds)
            scroll.autoresizingMask = [.width, .height]
            scroll.drawsBackground = true
            scroll.hasVerticalScroller = true
            scroll.scrollerStyle = .overlay
            scroll.autohidesScrollers = true
            scroll.wantsLayer = true
            scroll.layer?.cornerRadius = 7
            scroll.layer?.borderColor = accent.withAlphaComponent(0.9).cgColor
            scroll.layer?.borderWidth = 1.5
            scroll.layer?.masksToBounds = true
            let tv = NSTextView(frame: scroll.bounds)
            tv.isEditable = false; tv.isSelectable = true
            tv.drawsBackground = false
            tv.font = monoFont(8.5)
            tv.textContainerInset = NSSize(width: 5, height: 5)
            tv.isVerticallyResizable = true; tv.isHorizontallyResizable = false
            tv.textContainer?.widthTracksTextView = true
            tv.autoresizingMask = [.width]
            scroll.documentView = tv
            container.addSubview(scroll)
            let flash = GhostView(frame: container.bounds)
            flash.autoresizingMask = [.width, .height]
            flash.wantsLayer = true
            flash.layer?.backgroundColor = accent.cgColor
            flash.layer?.cornerRadius = 7
            flash.layer?.opacity = 0
            container.addSubview(flash)
            controller.content.addSubview(container)
            paneContainer = container; paneScroll = scroll; paneView = tv
            paneFlash = flash
            applyPaneTheme()
        }
    }

    // Reserved height from the y=12 baseline up to where the name sits — the
    // exact bottom-up stack the original badge computed.
    func stackHeight(in controller: PalController) -> CGFloat {
        let off = (hasPane && curPaneH > 0) ? curPaneH + 8 : 0
        let tasksH: CGFloat = taskLines.isEmpty ? 0 : CGFloat(taskLines.count) * 13 + 3
        let chipH: CGFloat = overtimeOn ? 36 : 0
        let queueH: CGFloat = (overtimeOn && !overtimeLines.isEmpty)
            ? CGFloat(overtimeLines.count) * 16 + 3 : 0
        let overtimeH: CGFloat = overtimeOn ? chipH + (queueH > 0 ? queueH + 4 : 0) : 0
        return off + (tasksH > 0 ? tasksH + 5 : 0) + 18 + (overtimeH > 0 ? 4 : 0) + overtimeH + 14
    }

    func layoutRows(in controller: PalController, originY: CGFloat) {
        let base = originY                     // 12 in practice (single plugin)
        let W = controller.fullWidth
        let off = (hasPane && curPaneH > 0) ? curPaneH + 8 : 0
        let tasksH: CGFloat = taskLines.isEmpty ? 0 : CGFloat(taskLines.count) * 13 + 3
        let chipH: CGFloat = overtimeOn ? 36 : 0
        let queueH: CGFloat = (overtimeOn && !overtimeLines.isEmpty)
            ? CGFloat(overtimeLines.count) * 16 + 3 : 0
        let overtimeH: CGFloat = overtimeOn ? chipH + (queueH > 0 ? queueH + 4 : 0) : 0
        let statusY = base + off + (tasksH > 0 ? tasksH + 5 : 0)
        let overtimeY = statusY + 18 + (overtimeH > 0 ? 4 : 0)

        statusField.isHidden = false
        statusField.frame = NSRect(x: 0, y: statusY, width: W, height: 18)
        overtimeField.isHidden = queueH <= 0
        overtimeField.frame = NSRect(x: 6, y: overtimeY, width: W - 12, height: queueH)
        overtimeChip.isHidden = chipH <= 0
        let chipW = min(overtimeChip.attributedStringValue.size().width + 16, W - 4)
        overtimeChip.frame = NSRect(x: (W - chipW) / 2,
                                    y: overtimeY + queueH + (queueH > 0 ? 4 : 0),
                                    width: chipW, height: chipH)
        tasksField.isHidden = tasksH <= 0
        tasksField.frame = NSRect(x: 6, y: base + off, width: W - 12, height: tasksH)

        if let c = paneContainer {
            c.isHidden = curPaneH <= 0
            c.frame = NSRect(x: pad, y: base - 4, width: paneW, height: curPaneH)
            if !c.isHidden { controller.content.liveRects.append(c.frame) }
        }
    }

    func setCollapsed(_ collapsed: Bool) {
        let hide = collapsed
        statusField.isHidden = hide
        tasksField.isHidden = hide || taskLines.isEmpty
        overtimeField.isHidden = hide || !overtimeOn || overtimeLines.isEmpty
        overtimeChip.isHidden = hide || !overtimeOn
        paneContainer?.isHidden = hide || curPaneH <= 0
    }

    func tick() {
        refresh()
        if hasPane { refreshPane() }
    }

    func menuItems(for controller: PalController) -> [NSMenuItem] {
        let item = NSMenuItem(title: "Overtime", action: #selector(toggleOvertime), keyEquivalent: "")
        item.target = self
        item.state = FileManager.default.fileExists(atPath: overtimeFlag) ? .on : .off
        return [item]
    }

    // ── overtime ───────────────────────────────────────────────────────────
    @objc func toggleOvertime() {
        let fm = FileManager.default
        if fm.fileExists(atPath: overtimeFlag) {
            try? fm.removeItem(atPath: overtimeFlag)
            try? fm.removeItem(atPath: overtimeStatusFile)
        } else {
            fm.createFile(atPath: overtimeFlag, contents: nil)
            try? "idle — overtime on".write(toFile: overtimeStatusFile,
                                            atomically: true, encoding: .utf8)
            let p = Process()
            p.executableURL = URL(fileURLWithPath: "/bin/bash")
            p.arguments = ["-c",
                "launchctl kickstart gui/$(id -u)/computer.aesthetic.overtimeworker 2>/dev/null"]
            try? p.run()
        }
        c?.nameLabel.bounce()
        refresh()
    }

    // ── pane ─────────────────────────────────────────────────────────────
    func paneIsDark() -> Bool {
        NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
    }

    func applyPaneTheme() {
        guard let scroll = paneScroll else { return }
        scroll.backgroundColor = paneIsDark()
            ? NSColor.black.withAlphaComponent(0.58)
            : NSColor.white.withAlphaComponent(0.90)
    }

    func measurePaneH() -> CGFloat {
        guard let tv = paneView, let tc = tv.textContainer, let lm = tv.layoutManager else { return 0 }
        if (tv.string).isEmpty { return 0 }
        lm.ensureLayout(for: tc)
        let used = lm.usedRect(for: tc).height
        return min(max(used + tv.textContainerInset.height * 2 + 3, 22), maxPaneH)
    }

    func flashPane() {
        guard let l = paneFlash?.layer else { return }
        let a = CAKeyframeAnimation(keyPath: "opacity")
        a.values = [0, 0.5, 0]
        a.keyTimes = [0, 0.15, 1]
        a.duration = 0.45
        l.add(a, forKey: "flash")
    }

    func refreshPane() {
        guard let tv = paneView, let c = c else { return }
        applyPaneTheme()
        let dark = paneIsDark()
        let text = tailLog(paneLog)
        guard text != lastPaneRaw || dark != lastPaneDark else { return }
        let newOutput = lastPaneDark != nil && text != lastPaneRaw
        lastPaneRaw = text; lastPaneDark = dark
        tv.textStorage?.setAttributedString(PaneRenderer(dark: dark).render(text))
        let newH = measurePaneH()
        if abs(newH - curPaneH) > 0.5 { curPaneH = newH; if !c.collapsed { c.layout() } }
        tv.scrollRangeToVisible(NSRange(location: (tv.string as NSString).length, length: 0))
        if newOutput && !c.collapsed { c.nameLabel.bounce(); flashPane() }
    }

    // git runs off-main: a slow/hung git must never stall the window.
    func refresh() {
        // git only every ~4th tick; status files are read every tick (cheap).
        let doGit = tick4 % 4 == 0
        tick4 += 1
        guard !refreshing else { return }
        refreshing = true
        DispatchQueue.global(qos: .utility).async { [weak self] in
            guard let self = self else { return }
            let branch = doGit ? self.git(["branch", "--show-current"]) : self.lastBranch
            let dirty = doGit ? !self.git(["status", "--porcelain"]).isEmpty : self.lastDirty
            let syncRaw = (try? String(contentsOfFile: self.statusFile, encoding: .utf8)) ?? ""
            let rawTasks = (try? String(contentsOfFile: self.tasksFile, encoding: .utf8)) ?? ""
            let otOn = FileManager.default.fileExists(atPath: self.overtimeFlag)
            let otRaw = otOn
                ? ((try? String(contentsOfFile: self.overtimeStatusFile, encoding: .utf8)) ?? "")
                : ""
            DispatchQueue.main.async {
                self.refreshing = false
                self.lastBranch = branch; self.lastDirty = dirty
                self.applyStatus(branch: branch, dirty: dirty,
                                 syncRaw: syncRaw, rawTasks: rawTasks,
                                 otOn: otOn, otRaw: otRaw)
            }
        }
    }
    private var lastBranch = ""
    private var lastDirty = false

    func applyStatus(branch: String, dirty: Bool, syncRaw: String, rawTasks: String,
                     otOn: Bool, otRaw: String) {
        guard let c = c else { return }
        var sync: String
        let t = syncRaw.trimmingCharacters(in: .whitespacesAndNewlines)
        let parts = t.split(separator: " ")
        if t.isEmpty { sync = "checking…" }
        else if t == "local" { sync = "local only" }
        else if parts.count >= 2, let a = Int(parts[0]), let b = Int(parts[1]) {
            if a == 0 && b == 0 { sync = "synced" }
            else if a > 0 && b > 0 { sync = "\(a) ahead, \(b) behind" }
            else if a > 0 { sync = "\(a) ahead" }
            else { sync = "\(b) behind" }
        } else { sync = "checking…" }
        let syncColor: NSColor
        if sync == "synced" { syncColor = hexColor(0x7ee787) }
        else if sync.contains("ahead") && sync.contains("behind") { syncColor = hexColor(0xffb14d) }
        else if sync.contains("ahead") { syncColor = hexColor(0xffd66b) }
        else if sync.contains("behind") { syncColor = hexColor(0xff6b6b) }
        else { syncColor = hexColor(0xa5b1bd) }
        let f10 = monoFont(10)
        func seg(_ s: String, _ col: NSColor) -> NSAttributedString {
            NSAttributedString(string: s, attributes: [.font: f10, .foregroundColor: col])
        }
        let dim = NSColor.white.withAlphaComponent(0.55)
        let line = NSMutableAttributedString()
        line.append(seg(branch.isEmpty ? "—" : branch, .white))
        line.append(seg(" · ", dim))
        line.append(seg(sync, syncColor))
        if dirty {
            line.append(seg(" · ", dim))
            line.append(seg("uncommitted", hexColor(0xffaa33)))
        }
        statusField.setText(line)

        let lines = Array(rawTasks.split(separator: "\n")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }
            .prefix(3))
        if lines != taskLines {
            taskLines = lines
            let para = NSMutableParagraphStyle()
            para.alignment = .center; para.lineBreakMode = .byTruncatingTail
            tasksField.attributedStringValue = NSAttributedString(
                string: lines.joined(separator: "\n"),
                attributes: [.font: monoFont(9), .foregroundColor: NSColor.white,
                             .paragraphStyle: para])
            if !c.collapsed { c.layout() }
        }

        let otLines = Array(otRaw.split(separator: "\n")
            .map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty }
            .prefix(4))
        if otOn != overtimeOn || otLines != overtimeLines {
            overtimeOn = otOn; overtimeLines = otLines
            let para = NSMutableParagraphStyle()
            para.alignment = .center; para.lineBreakMode = .byTruncatingTail
            overtimeChip.attributedStringValue = otOn
                ? NSAttributedString(string: "⚡ OVERTIME", attributes: [
                    .font: playfulFont(24, bold: true),
                    .foregroundColor: hexColor(0xff0000),
                    .strokeColor: hexColor(0xffe000),
                    .strokeWidth: -6.0,
                    .paragraphStyle: para])
                : NSAttributedString()
            overtimeField.attributedStringValue = otOn && !otLines.isEmpty
                ? NSAttributedString(string: otLines.joined(separator: "\n"), attributes: [
                    .font: monoFont(11), .foregroundColor: NSColor.white,
                    .paragraphStyle: para])
                : NSAttributedString()
            if !c.collapsed { c.layout() }
        }
    }
}
