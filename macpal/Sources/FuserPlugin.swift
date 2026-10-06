// FuserPlugin — the machine-badge features that stack beneath the pal on the
// fuser fleet (neo / panda / chicken / blueberry): a color-coded git status
// line (marquee when it overflows), the machine's current mission, an
// ⚡OVERTIME alarm + work queue, and a live ANSI "terminal" pane tailing a log.
//
// Every input is a plain file polled at runtime, so content updates never need
// a recompile:
//   <home>/gitstatus        "<ahead> <behind>" (badge-git-sync.sh) or "local"
//   <home>/tasks            retired; mission.json owns the task list
//   <home>/mission.json     the machine's mission — emoji + title, agent
//                           attribution, ✓/▸/○ todo items; each item may add
//                           detail (sub-line), progress (0…1 bar), flag
//                           (green/red/yellow glyph) — 0.2.10; and at (ISO
//                           time of its latest activity, shown as "8m ago",
//                           falling back to updatedAt). Items are shown
//                           active → pending → done, done newest first; the
//                           avatar carries the provider's mark (staged
//                           <home>/agent-<name>.png, else the installed
//                           Claude/Codex app icon, else a built-in vector
//                           mark) — 0.2.11. Written live by
//                           whichever agent is working the machine, or
//                           seeded from the Asana task tagged "mission"
//                           (badge-asana-sync.sh, which never overwrites a
//                           fresh agent-authored mission). Stale >24h hides.
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

// AppKit's label cell pins attachments to the top of a taller field. Draw the
// measured icon + text line in the center of its padded badge instead.
final class MissionHeadingField: NSTextField {
    override func draw(_ dirtyRect: NSRect) {
        let area = bounds.insetBy(dx: 6, dy: 0)
        let height = ceil(attributedStringValue.boundingRect(
            with: NSSize(width: area.width, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading]).height)
        attributedStringValue.draw(with: NSRect(x: area.minX,
            y: (bounds.height - height) / 2, width: area.width, height: height),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
    }
    override func hitTest(_ p: NSPoint) -> NSView? { nil }
}

// ── mission todo list ─────────────────────────────────────────────────────
// Same file-driven contract as everything else on the badge: a plain JSON
// file an agent rewrites whole, polled on the existing refresh cadence.
// Tolerant read — a missing, malformed, or stale file simply means "no
// mission" and the block hides. (Ported from the panda desktop-badge
// implementation so both badge generations speak the same mission.json.)
struct MissionItem: Equatable {
    let text: String
    let status: String   // "done" | "in_progress" | "pending"
    // Optional per-item telemetry (0.2.10): a one-line inferred subtask shown
    // under the text, a 0…1 progress fraction drawn as a bar, and a flag that
    // overrides the checkbox glyph — "green"/"ok", "red"/"fail", "yellow"/"warn".
    var detail: String? = nil
    var progress: Double? = nil
    var flag: String? = nil
    // When this item last moved (0.2.11): its own `at`, else the mission's
    // updatedAt. Drives the "8m ago" age and the done-newest-first order.
    var at: Date? = nil
}

// Sort rank: active rows first, then pending, then done.
func missionStatusRank(_ status: String) -> Int {
    switch status {
    case "in_progress": return 0
    case "done": return 2
    default: return 1
    }
}

// "just now" / "8m ago" / "2h ago" / "3d ago" — minute granularity so the
// badge only re-bakes its rows when the label actually changes.
func relativeAge(_ d: Date, now: Date = Date()) -> String {
    let s = max(0, now.timeIntervalSince(d))
    if s < 45 { return "just now" }
    let m = Int((s / 60).rounded())
    if m < 90 { return "\(max(1, m))m ago" }
    let h = Int((s / 3600).rounded())
    if h < 36 { return "\(h)h ago" }
    return "\(Int((s / 86400).rounded()))d ago"
}
struct Mission: Equatable {
    let title: String
    let agent: String
    let emoji: String
    let items: [MissionItem]
}

func loadMission(_ path: String) -> Mission? {
    guard let data = FileManager.default.contents(atPath: path),
          let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any],
          let title = obj["mission"] as? String, !title.isEmpty
    else { return nil }
    // updatedAt is the heartbeat: absent, unparsable, or older than 24h means
    // the mission is over (or its author crashed) — either way, hide it.
    guard let ts = obj["updatedAt"] as? String else { return nil }
    let iso = ISO8601DateFormatter()
    let isoFrac = ISO8601DateFormatter()
    isoFrac.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
    func parseDate(_ s: String) -> Date? { iso.date(from: s) ?? isoFrac.date(from: s) }
    guard let d = parseDate(ts), Date().timeIntervalSince(d) < 24 * 3600 else { return nil }
    let parsed = ((obj["items"] as? [[String: Any]]) ?? []).compactMap { it -> MissionItem? in
        guard let t = it["text"] as? String, !t.isEmpty else { return nil }
        var item = MissionItem(text: t, status: (it["status"] as? String) ?? "pending")
        item.at = ((it["at"] as? String).flatMap(parseDate)) ?? d
        if let d = it["detail"] as? String, !d.isEmpty { item.detail = d }
        if let p = it["progress"] as? Double {          // accepts 0…1 or 0…100
            item.progress = min(1, max(0, p > 1 ? p / 100 : p))
        } else if let p = it["progress"] as? Int {
            item.progress = min(1, max(0, Double(p) / 100))
        }
        if let f = it["flag"] as? String, !f.isEmpty { item.flag = f.lowercased() }
        return item
    }
    // Active on top, pending next, done underneath with the most recently
    // completed first (directly under the active rows). Index breaks ties so
    // the feeder's order survives within a group.
    let items = parsed.enumerated().sorted { a, b in
        let ra = missionStatusRank(a.element.status), rb = missionStatusRank(b.element.status)
        if ra != rb { return ra < rb }
        if ra == 2, let ta = a.element.at, let tb = b.element.at, ta != tb { return ta > tb }
        return a.offset < b.offset
    }.map { $0.element }
    return Mission(title: title,
                   agent: (obj["agent"] as? String) ?? "",
                   emoji: (obj["emoji"] as? String) ?? "",
                   items: items)
}

// ── marquee status field ──────────────────────────────────────────────────
// No truncation: the styled status line is baked (accent shadow and all) into
// an image; when it fits it centers like a label, and when it overflows two
// copies scroll in an endless leftward loop.
final class MarqueeField: NSView {
    private let lead = CALayer()
    private let trail = CALayer()
    private let fadeMask = CAGradientLayer()
    private var textW: CGFloat = 0
    private var textH: CGFloat = 0
    private let gap: CGFloat = 28
    private let speed: CGFloat = 26   // px/s
    private let fadeW: CGFloat = 18   // edge fade margin when scrolling

    init() {
        super.init(frame: .zero)
        layer = CALayer()
        wantsLayer = true
        layer?.masksToBounds = true
        for l in [lead, trail] { l.anchorPoint = .zero; layer?.addSublayer(l) }
        fadeMask.startPoint = CGPoint(x: 0, y: 0.5)
        fadeMask.endPoint = CGPoint(x: 1, y: 0.5)
        fadeMask.colors = [
            CGColor(gray: 0, alpha: 0), CGColor(gray: 0, alpha: 1),
            CGColor(gray: 0, alpha: 1), CGColor(gray: 0, alpha: 0),
        ]
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
            layer?.mask = nil
        } else {
            trail.isHidden = false
            let f = bounds.width > 0 ? min(0.5, fadeW / bounds.width) : 0
            fadeMask.frame = bounds
            fadeMask.locations = [0, f as NSNumber, (1 - f) as NSNumber, 1]
            layer?.mask = fadeMask
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
    private var repo: String       // repo to read git status from (live-switchable)
    private var repoLabel: String  // the chosen repo's menu label
    private weak var c: PalController?

    // Which repository the pal reads git status from is a right-click choice,
    // persisted in UserDefaults. The launch `--repo` arg seeds the default;
    // picking a repo in the menu overrides it (per machine, so neo/blueberry
    // can read aesthetic-computer while panda/chicken stay on fuser).
    static let repoKey = "MacPal.repo"
    static let knownRepos: [(label: String, path: String)] = [
        ("aesthetic-computer", NSString(string: "~/aesthetic-computer").expandingTildeInPath),
        ("fuser", NSString(string: "~/Developer/fuser").expandingTildeInPath),
    ]
    /// The selectable repos: the known set, plus the launch `--repo` path if
    /// it isn't one of them — so the active repo always has a checkbox.
    private var repoChoices: [(label: String, path: String)] {
        var choices = Self.knownRepos
        if !choices.contains(where: { $0.path == initRepo }) {
            let name = (initRepo as NSString).lastPathComponent
            choices.append((label: name.isEmpty ? "repo" : name, path: initRepo))
        }
        return choices
    }
    private let initRepo: String   // the --repo launch arg (default fallback)

    private var statusFile: String { home + "/gitstatus" }
    private var paneLog: String { home + "/pane.log" }
    private var missionFile: String { home + "/mission.json" }
    private var overtimeFlag: String { home + "/overtime" }
    private var overtimeStatusFile: String { home + "/overtime-status" }

    let statusField = MarqueeField()
    let overtimeChip = NSTextField(labelWithString: "")
    let overtimeField = NSTextField(labelWithString: "")
    // Mission block: title + one field per todo item. The provider mark lives
    // beside the avatar, opposite the resident agent's contact disc.
    // Fields are (re)built on data change; layout measures + places them.
    var mission: Mission?
    let missionTitleField = MissionHeadingField(labelWithString: "")
    let providerChip = GhostImageView()
    var lastMissionDark: Bool?
    var missionItemFields: [NSTextField] = []
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

    // minimal: hide every info row (status line, tasks, overtime, terminal pane)
    // so the badge is just the avatar graphic + the name title, except a live
    // mission, so a machine watching the lanes still shows the lane board.
    let minimal: Bool

    init(home: String, repo: String, minimal: Bool = false) {
        self.minimal = minimal
        self.home = home
        self.initRepo = repo
        // A persisted menu choice wins over the launch arg; otherwise track the
        // --repo path (labeled from the known set when it matches).
        if let lbl = UserDefaults.standard.string(forKey: Self.repoKey),
           let choice = Self.knownRepos.first(where: { $0.label == lbl }) {
            self.repo = choice.path
            self.repoLabel = lbl
        } else {
            self.repo = repo
            self.repoLabel = Self.knownRepos.first(where: { $0.path == repo })?.label
                ?? (repo as NSString).lastPathComponent
        }
        self.hasPane = !minimal && FileManager.default.fileExists(atPath: home + "/pane.log")
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
        controller.styleField(overtimeField)
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

        // Keep the mission as lettering on the desktop, without a backplate.
        providerChip.imageScaling = .scaleProportionallyUpOrDown
        providerChip.isHidden = true
        controller.content.addSubview(providerChip)
        for f in [missionTitleField] {
            f.isBordered = false; f.drawsBackground = false
            f.alignment = .left
            f.maximumNumberOfLines = 0
            f.cell?.wraps = true
            f.cell?.lineBreakMode = .byWordWrapping
            f.shadow = nil
            f.isHidden = true
            controller.content.addSubview(f)
        }

        controller.content.addSubview(statusField)
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
            scroll.drawsBackground = false
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

    // ── mission block ─────────────────────────────────────────────────────
    // Bake the attributed strings for the current mission — one field per
    // item so the active row can breathe on its own. Layout measures and
    // places everything, so a width change just re-wraps.

    // Transparent lettering needs a small contrasting halo over other windows.
    static func missionShadow(dark: Bool) -> NSShadow {
        let sh = NSShadow()
        sh.shadowColor = dark ? NSColor.black : NSColor.white.withAlphaComponent(0.7)
        sh.shadowBlurRadius = dark ? 2 : 1
        sh.shadowOffset = .zero
        return sh
    }

    // ── agent mark ────────────────────────────────────────────────────────
    // The inferrer's logo beside its name. Staged <home>/agent-<name>.png
    // wins (updatable over the wire), then the installed app's icon
    // (Claude.app / Codex.app), then a small built-in vector mark so the
    // minis — which have neither app — still show who is working.
    private var agentIconCache: [String: NSImage] = [:]
    func agentIcon(for agent: String) -> NSImage? {
        let key = agent.trimmingCharacters(in: .whitespaces).lowercased()
        guard !key.isEmpty else { return nil }
        if let hit = agentIconCache[key] { return hit }
        var img: NSImage?
        if let path = resolveAgentAvatar(name: key, supportDir: home) {
            img = NSImage(contentsOfFile: path)
        }
        if img == nil {
            let apps: [String]
            switch key {
            case "claude": apps = ["/Applications/Claude.app", NSHomeDirectory() + "/Applications/Claude.app"]
            case "codex":  apps = ["/Applications/Codex.app", NSHomeDirectory() + "/Applications/Codex.app"]
            default: apps = []
            }
            if let app = apps.first(where: { FileManager.default.fileExists(atPath: $0) }) {
                img = NSWorkspace.shared.icon(forFile: app)
            }
        }
        if img == nil { img = Self.builtInAgentMark(key) }
        if let i = img { agentIconCache[key] = i }
        return img
    }

    // Vector fallbacks drawn at any size: Claude = terracotta tile with the
    // white starburst; Codex = black tile with the white hexagonal knot.
    static func builtInAgentMark(_ key: String) -> NSImage? {
        let tile: NSColor
        switch key {
        case "claude": tile = hexColor(0xD97757)
        case "codex", "openai", "chatgpt": tile = hexColor(0x111111)
        default: return nil
        }
        let S: CGFloat = 64
        return NSImage(size: NSSize(width: S, height: S), flipped: false) { r in
            tile.setFill()
            NSBezierPath(roundedRect: r, xRadius: S * 0.22, yRadius: S * 0.22).fill()
            NSColor.white.setStroke()
            let c = NSPoint(x: S / 2, y: S / 2)
            if key == "claude" {
                // Eight tapered spokes, the longer four on the diagonals.
                for i in 0..<8 {
                    let ang = CGFloat(i) * .pi / 4 + .pi / 8
                    let len: CGFloat = (i % 2 == 0) ? S * 0.30 : S * 0.24
                    let p = NSBezierPath()
                    p.lineWidth = S * 0.085; p.lineCapStyle = .round
                    p.move(to: NSPoint(x: c.x + cos(ang) * S * 0.06, y: c.y + sin(ang) * S * 0.06))
                    p.line(to: NSPoint(x: c.x + cos(ang) * len, y: c.y + sin(ang) * len))
                    p.stroke()
                }
            } else {
                // Hexagonal knot: an outer hexagon ring and an inner rotated one.
                func hex(_ rad: CGFloat, _ rot: CGFloat) -> NSBezierPath {
                    let p = NSBezierPath()
                    for i in 0..<6 {
                        let a = CGFloat(i) * .pi / 3 + rot
                        let pt = NSPoint(x: c.x + cos(a) * rad, y: c.y + sin(a) * rad)
                        if i == 0 { p.move(to: pt) } else { p.line(to: pt) }
                    }
                    p.close(); p.lineWidth = S * 0.075; p.lineJoinStyle = .round
                    return p
                }
                hex(S * 0.34, .pi / 6).stroke()
                hex(S * 0.18, 0).stroke()
            }
            return true
        }
    }

    func rebuildMissionFields() {
        missionItemFields.forEach { $0.removeFromSuperview() }
        missionItemFields = []
        guard let m = mission, let c = c else {
            missionTitleField.isHidden = true
            providerChip.isHidden = true
            return
        }
        let para = NSMutableParagraphStyle()
        para.alignment = .center; para.lineBreakMode = .byWordWrapping
        let dark = paneIsDark()
        let primary = NSColor.labelColor
        let secondary = NSColor.secondaryLabelColor
        let textShadow = Self.missionShadow(dark: dark)
        missionTitleField.shadow = textShadow
        let green = dark ? hexColor(0x7EE787) : hexColor(0x237A37)
        let amber = dark ? hexColor(0xFFD66B) : hexColor(0x865800)
        let red = dark ? hexColor(0xFF8585) : hexColor(0xB32626)
        let titleFont = playfulFont(15, bold: true)
        let irisHeading = m.title.lowercased().contains("iris lanes")
        let titleLine = NSMutableAttributedString()
        if irisHeading, let icon = agentIcon(for: "iris") {
            let art = NSTextAttachment()
            art.image = NSImage(size: NSSize(width: 64, height: 64), flipped: false) { rect in
                NSBezierPath(ovalIn: rect).addClip()
                icon.draw(in: rect)
                return true
            }
            art.bounds = CGRect(x: 0, y: (titleFont.capHeight - 32) / 2, width: 32, height: 32)
            titleLine.append(NSAttributedString(attachment: art))
            titleLine.append(NSAttributedString(string: "  "))
        }
        let titleText = (!irisHeading && !m.emoji.isEmpty ? m.emoji + " " : "") + m.title
        titleLine.append(NSAttributedString(string: titleText, attributes: [
                .font: titleFont,
                .foregroundColor: primary,
                .paragraphStyle: para,
            ]))
        titleLine.addAttributes([.font: titleFont, .paragraphStyle: para],
                               range: NSRange(location: 0, length: titleLine.length))
        missionTitleField.attributedStringValue = titleLine
        // Match the contact disc on the other side of the avatar. The mark
        // identifies the provider without repeating its name in the todo list.
        let provider = m.agent.trimmingCharacters(in: .whitespacesAndNewlines)
        let repeatsResidentAgent = provider.caseInsensitiveCompare(c.agentName) == .orderedSame
        providerChip.image = provider.isEmpty || repeatsResidentAgent ? nil : agentIcon(for: provider)
        providerChip.toolTip = provider.isEmpty ? nil : provider
        providerChip.setAccessibilityLabel(provider.isEmpty ? nil : provider + " provider")
        let now = Date()
        for item in m.items {
            let f = NSTextField(labelWithString: "")
            f.alignment = .left
            f.maximumNumberOfLines = 0
            f.cell?.wraps = true
            f.cell?.lineBreakMode = .byWordWrapping
            f.wantsLayer = true
            f.shadow = textShadow
            let ip = NSMutableParagraphStyle()
            ip.alignment = .left; ip.lineBreakMode = .byWordWrapping
            ip.headIndent = 19   // wrapped lines tuck under the text, past the square
            // Square checkboxes on the left: filled = done, half = active,
            // empty = pending.
            var mark: String, markColor: NSColor, textColor: NSColor
            switch item.status {
            case "done":
                mark = "■"; markColor = green
                textColor = secondary
            case "in_progress":
                mark = "▣"; markColor = amber
                textColor = primary
            default:
                mark = "□"; markColor = secondary
                textColor = primary
            }
            // A flag wins over the status glyph: green ✔, red ✖, yellow ⚠.
            switch item.flag {
            case "green", "ok", "pass":   mark = "✔"; markColor = green
            case "red", "fail", "error":  mark = "✖"; markColor = red; textColor = primary
            case "yellow", "warn":        mark = "⚠"; markColor = amber
            default: break
            }
            let a = NSMutableAttributedString()
            a.append(NSAttributedString(string: mark + " ", attributes: [
                .font: NSFont.systemFont(ofSize: 13, weight: .bold),
                .foregroundColor: markColor, .paragraphStyle: ip]))
            a.append(NSAttributedString(string: item.text, attributes: [
                .font: monoFont(13), .foregroundColor: textColor,
                .paragraphStyle: ip]))
            // Age: when this row last moved, dim and small, trailing the text
            // (wraps under the indent with it rather than truncating anything).
            if let at = item.at {
                a.append(NSAttributedString(string: " · " + relativeAge(at, now: now), attributes: [
                    .font: monoFont(11),
                    .foregroundColor: secondary,
                    .paragraphStyle: ip]))
            }
            // Sub-line: the inferred subtask, dimmer and smaller, tucked under the text.
            if let d = item.detail {
                a.append(NSAttributedString(string: "\n↳ " + d, attributes: [
                    .font: monoFont(11),
                    .foregroundColor: secondary,
                    .paragraphStyle: ip]))
            }
            // Progress bar: ten cells of ▰/▱ plus the percentage, coloured by the
            // flag (red/green) or the status (yellow while in progress).
            if let p = item.progress {
                let cells = 10, filled = Int((p * Double(cells)).rounded())
                let bar = String(repeating: "▰", count: filled)
                    + String(repeating: "▱", count: max(0, cells - filled))
                let barColor: NSColor
                switch item.flag {
                case "red", "fail", "error": barColor = red
                case "green", "ok", "pass":  barColor = green
                default: barColor = p >= 1 ? green : amber
                }
                a.append(NSAttributedString(string: "\n" + bar + " " + String(Int((p * 100).rounded())) + "%", attributes: [
                    .font: monoFont(11), .foregroundColor: barColor,
                    .paragraphStyle: ip]))
            }
            f.attributedStringValue = a
            if item.status == "in_progress" {
                // Subtle breathing on the active row — opacity only, so it's
                // a single composited property, dirt cheap.
                let pulse = CAKeyframeAnimation(keyPath: "opacity")
                pulse.values = [1, 0.55, 1]; pulse.keyTimes = [0, 0.5, 1]
                pulse.duration = 1.6; pulse.repeatCount = .infinity
                f.layer?.add(pulse, forKey: "missionPulse")
            }
            c.content.addSubview(f)
            missionItemFields.append(f)
        }
    }

    // Wrapped height of a field's attributed string at a given width.
    func fieldHeight(_ f: NSTextField, width: CGFloat) -> CGFloat {
        guard f.attributedStringValue.length > 0 else { return 0 }
        let r = f.attributedStringValue.boundingRect(
            with: NSSize(width: width, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        return ceil(r.height) + 2
    }

    // Measured height of the whole mission block at the badge width.
    private func missionMetrics(width: CGFloat)
        -> (title: CGFloat, items: [CGFloat], total: CGFloat) {
        guard mission != nil else { return (0, [], 0) }
        let t = max(44, fieldHeight(missionTitleField, width: width - 12) + 10)
        let its = missionItemFields.map { fieldHeight($0, width: width) }
        let total = t + its.reduce(0, +) + CGFloat(max(0, its.count - 1)) * 8
            + (its.isEmpty ? 0 : 12)
        return (t, its, total)
    }

    // Reserved height from the y=12 baseline up to where the name sits — the
    // exact bottom-up stack the original badge computed.
    func stackHeight(in controller: PalController) -> CGFloat {
        // minimal keeps only a live mission: the lane board a machine is watching.
        if minimal {
            let h = missionMetrics(width: controller.fullWidth - 14).total
            return h > 0 ? h + 8 : 0
        }
        // Mission mode stands the terminal pane down: its height leaves the
        // stack, so the todo list owns the badge's lower half until the
        // mission goes stale or is cleared.
        let off = (hasPane && curPaneH > 0 && mission == nil) ? curPaneH + 8 : 0
        let missionH = missionMetrics(width: controller.fullWidth - 14).total
        let chipH: CGFloat = overtimeOn ? 36 : 0
        let queueH: CGFloat = (overtimeOn && !overtimeLines.isEmpty)
            ? CGFloat(overtimeLines.count) * 16 + 3 : 0
        let overtimeH: CGFloat = overtimeOn ? chipH + (queueH > 0 ? queueH + 4 : 0) : 0
        return off + (missionH > 0 ? missionH + 6 : 0)
            + 18 + (overtimeH > 0 ? 4 : 0) + overtimeH + 14
    }

    func layoutRows(in controller: PalController, originY: CGFloat) {
        if minimal {
            statusField.isHidden = true
            overtimeField.isHidden = true; overtimeChip.isHidden = true
            paneContainer?.isHidden = true
            let mm = missionMetrics(width: controller.fullWidth - 14)
            layoutMission(y: originY, width: controller.fullWidth - 14, mm: mm)
            return
        }
        let base = originY                     // 12 in practice (single plugin)
        let W = controller.fullWidth
        let off = (hasPane && curPaneH > 0 && mission == nil) ? curPaneH + 8 : 0
        let missionW = W - 14
        let mm = missionMetrics(width: missionW)
        let missionH = mm.total
        let chipH: CGFloat = overtimeOn ? 36 : 0
        let queueH: CGFloat = (overtimeOn && !overtimeLines.isEmpty)
            ? CGFloat(overtimeLines.count) * 16 + 3 : 0
        let overtimeH: CGFloat = overtimeOn ? chipH + (queueH > 0 ? queueH + 4 : 0) : 0
        // Stack, bottom-up: mission (in the pane's slot) · git
        // status · overtime queue · sticker. The mission block rides at the
        // bottom so the badge just grows down its screen edge.
        let missionY = base + off
        let statusY = missionY + (missionH > 0 ? missionH + 6 : 0)
        let overtimeY = statusY + 22 + (overtimeH > 0 ? 4 : 0)

        statusField.isHidden = false
        statusField.frame = NSRect(x: 0, y: statusY, width: W, height: 22)

        layoutMission(y: missionY, width: missionW, mm: mm)
        overtimeField.isHidden = queueH <= 0
        overtimeField.frame = NSRect(x: 6, y: overtimeY, width: W - 12, height: queueH)
        overtimeChip.isHidden = chipH <= 0
        let chipW = min(overtimeChip.attributedStringValue.size().width + 16, W - 4)
        overtimeChip.frame = NSRect(x: (W - chipW) / 2,
                                    y: overtimeY + queueH + (queueH > 0 ? 4 : 0),
                                    width: chipW, height: chipH)

        if let c = paneContainer {
            c.isHidden = curPaneH <= 0 || mission != nil
            c.frame = NSRect(x: pad, y: base - 4, width: paneW, height: curPaneH)
            if !c.isHidden { controller.content.liveRects.append(c.frame) }
        }
    }

    // Mission block: title, then items, top-down within its
    // slot. Display-only — clicks pass through.
    private func layoutMission(y missionY: CGFloat, width missionW: CGFloat,
                               mm: (title: CGFloat, items: [CGFloat], total: CGFloat)) {
        let missionVisible = mission != nil && mm.total > 0
        missionTitleField.isHidden = !missionVisible
        providerChip.isHidden = !missionVisible || providerChip.image == nil
        if let c = c, !providerChip.isHidden {
            let side: CGFloat = 32
            providerChip.frame = NSRect(x: c.glyphView.frame.minX - 8,
                                       y: c.glyphView.frame.minY + 2,
                                       width: side, height: side)
            c.content.addSubview(providerChip, positioned: .above, relativeTo: c.glyphView)
        }
        missionItemFields.forEach { $0.isHidden = !missionVisible }
        guard missionVisible else { return }
        var my = missionY + mm.total - mm.title
        missionTitleField.frame = NSRect(x: 7, y: my, width: missionW, height: mm.title)
        my -= 12
        for (i, f) in missionItemFields.enumerated() {
            my -= mm.items[i]
            f.frame = NSRect(x: 7, y: my, width: missionW, height: mm.items[i])
            my -= 8
        }
    }

    func setCollapsed(_ collapsed: Bool) {
        if minimal {
            statusField.isHidden = true
            overtimeField.isHidden = true; overtimeChip.isHidden = true
            paneContainer?.isHidden = true
            missionTitleField.isHidden = collapsed || mission == nil
            providerChip.isHidden = collapsed || mission == nil || providerChip.image == nil
            missionItemFields.forEach { $0.isHidden = collapsed || mission == nil }
            return
        }
        let hide = collapsed
        statusField.isHidden = hide
        missionTitleField.isHidden = hide || mission == nil
        providerChip.isHidden = hide || mission == nil || providerChip.image == nil
        missionItemFields.forEach { $0.isHidden = hide || mission == nil }
        overtimeField.isHidden = hide || !overtimeOn || overtimeLines.isEmpty
        overtimeChip.isHidden = hide || !overtimeOn
        paneContainer?.isHidden = hide || curPaneH <= 0
    }

    func tick() {
        refresh()
        if hasPane { refreshPane() }
    }

    // Deprecated for now: the Overtime toggle and Repo ▸ selector are parked so
    // the context menu stays just "About MacPal" + "Quit". The machinery below
    // (toggleOvertime, pickRepo, the flag files) still works if re-added here.
    func menuItems(for controller: PalController) -> [NSMenuItem] { [] }

    /// Switch the tracked repo from the right-click menu: persist the choice,
    /// repoint git, and force a fresh read so the badge updates immediately.
    @objc func pickRepo(_ sender: NSMenuItem) {
        guard let lbl = sender.representedObject as? String,
              let choice = repoChoices.first(where: { $0.label == lbl }) else { return }
        UserDefaults.standard.set(lbl, forKey: Self.repoKey)
        repo = choice.path
        repoLabel = lbl
        lastBranch = ""; lastDirty = false   // invalidate cache → next tick re-reads
        tick4 = 0
        c?.nameLabel.bounce()
        refresh()
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
        scroll.backgroundColor = .clear
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
        // No bounce/flash while a mission owns the badge — the pane is hidden
        // then, and the name jumping for invisible output reads as a glitch.
        if newOutput && !c.collapsed && mission == nil { c.nameLabel.bounce(); flashPane() }
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
            let missionNow = loadMission(self.missionFile)   // tolerant: nil hides the block
            let otOn = FileManager.default.fileExists(atPath: self.overtimeFlag)
            let otRaw = otOn
                ? ((try? String(contentsOfFile: self.overtimeStatusFile, encoding: .utf8)) ?? "")
                : ""
            DispatchQueue.main.async {
                self.refreshing = false
                self.lastBranch = branch; self.lastDirty = dirty
                self.applyStatus(branch: branch, dirty: dirty,
                                 syncRaw: syncRaw,
                                 mission: missionNow,
                                 otOn: otOn, otRaw: otRaw)
            }
        }
    }
    private var lastBranch = ""
    private var lastDirty = false
    private var lastAgeKey = ""

    func applyStatus(branch: String, dirty: Bool, syncRaw: String,
                     mission missionNow: Mission?, otOn: Bool, otRaw: String) {
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
        let f13 = monoFont(13)
        func seg(_ s: String, _ col: NSColor) -> NSAttributedString {
            NSAttributedString(string: s, attributes: [.font: f13, .foregroundColor: col])
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

        // Re-bake on a data change, or when any row's "Nm ago" label ticked
        // over — the ages ride the existing poll, at minute granularity.
        let now = Date()
        let ageKey = (missionNow?.items ?? []).map { $0.at.map { relativeAge($0, now: now) } ?? "" }.joined(separator: "|")
        let missionDark = paneIsDark()
        if missionNow != mission || ageKey != lastAgeKey || missionDark != lastMissionDark {
            mission = missionNow
            lastAgeKey = ageKey
            lastMissionDark = missionDark
            rebuildMissionFields()
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
