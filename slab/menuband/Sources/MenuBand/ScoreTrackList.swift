import AppKit

/// The track list a collection `.mbscore` opens to.
///
/// A collection is a score whose `tracks` name sibling `.mbscore` files
/// instead of carrying `voices` (scores/README.md → "Collections"). Opening
/// one in Finder shows every track in its sections; double-click, Return or
/// Play performs the chosen track exactly as opening that file would, and
/// Stop halts whatever is sounding. A track may itself be a collection — it
/// opens to its own list.
///
/// Each entry may carry `title` / `machines` / `bpm` / `seconds` as hints so
/// the list draws without reading thirty files; a missing hint is read from
/// the sibling itself.
final class ScoreTrackListController: NSWindowController, NSWindowDelegate,
                                      NSTableViewDataSource, NSTableViewDelegate {
    struct Track {
        let url: URL
        let title: String
        let machines: Int
        let bpm: Int
        let seconds: Double
        let fleet: Bool
        let isCollection: Bool
    }

    private enum Row {
        case section(String)
        case track(Track, number: Int)
    }

    /// One window per collection file; opening the same file again just
    /// brings its list forward. Cleared in `windowWillClose`.
    private static var active: [URL: ScoreTrackListController] = [:]

    private let rows: [Row]
    private let trackCount: Int
    private let play: (URL) -> Void
    private let stop: () -> Void
    private var tableView: NSTableView!
    private var playButton: NSButton!

    // MARK: - Opening

    /// Show (or raise) the list for a collection already parsed by the caller.
    static func present(collection url: URL, obj: [String: Any],
                        play: @escaping (URL) -> Void, stop: @escaping () -> Void) {
        let key = url.standardizedFileURL
        if let open = active[key] { open.raise(); return }
        let controller = ScoreTrackListController(url: key, obj: obj, play: play, stop: stop)
        active[key] = controller
        controller.raise()
    }

    private init(url: URL, obj: [String: Any],
                 play: @escaping (URL) -> Void, stop: @escaping () -> Void) {
        self.play = play
        self.stop = stop
        let tracks = Self.tracks(of: obj, beside: url)
        var rows: [Row] = []
        var section: String?
        for (i, (track, sec)) in tracks.enumerated() {
            if let sec, sec != section { rows.append(.section(sec)); section = sec }
            rows.append(.track(track, number: i + 1))
        }
        self.rows = rows
        self.trackCount = tracks.count

        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 560, height: 520),
            styleMask: [.titled, .closable, .resizable, .miniaturizable],
            backing: .buffered,
            defer: false
        )
        window.title = (obj["title"] as? String) ?? url.deletingPathExtension().lastPathComponent
        window.isReleasedWhenClosed = false
        window.level = .normal
        window.minSize = NSSize(width: 420, height: 260)
        window.setFrameAutosaveName("ScoreTrackList:\(url.lastPathComponent)")
        super.init(window: window)
        window.delegate = self
        buildContent(obj: obj, total: tracks.reduce(0) { $0 + $1.0.seconds })
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) is not used") }

    private func raise() {
        guard let window else { return }
        if !window.isVisible { window.center() }
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
        if tableView.selectedRow < 0, let first = rows.firstIndex(where: { if case .track = $0 { return true } else { return false } }) {
            tableView.selectRowIndexes(IndexSet(integer: first), byExtendingSelection: false)
        }
        window.makeFirstResponder(tableView)
    }

    func windowWillClose(_ notification: Notification) {
        Self.active = Self.active.filter { $0.value !== self }
    }

    // MARK: - Reading the entries

    /// Every track with its section label, in file order. Entries without a
    /// `file` are skipped; hints missing from an entry are read from the sibling.
    private static func tracks(of obj: [String: Any], beside url: URL) -> [(Track, String?)] {
        let dir = url.deletingLastPathComponent()
        var out: [(Track, String?)] = []
        for entry in (obj["tracks"] as? [[String: Any]]) ?? [] {
            guard let file = entry["file"] as? String, !file.isEmpty else { continue }
            let trackURL = file.hasPrefix("/") ? URL(fileURLWithPath: file)
                                              : dir.appendingPathComponent(file)
            let hinted = entry["title"] != nil && entry["machines"] != nil
                      && entry["bpm"] != nil && entry["seconds"] != nil
            let sibling: [String: Any] = hinted ? [:] : (read(trackURL) ?? [:])
            let voices = (sibling["voices"] as? [[String: Any]]) ?? []
            let bpm = intValue(entry["bpm"]) ?? intValue(sibling["bpm"]) ?? 120
            let title = (entry["title"] as? String) ?? (sibling["title"] as? String)
                     ?? trackURL.deletingPathExtension().lastPathComponent
            let machines = intValue(entry["machines"]) ?? intValue(sibling["machines"]) ?? max(1, voices.count)
            let seconds = doubleValue(entry["seconds"]) ?? (longestBeats(voices) * 60.0 / Double(max(1, bpm)))
            let fleet = (entry["requiresFleet"] as? Bool) ?? (sibling["requiresFleet"] as? Bool) ?? false
            let isCollection = sibling["tracks"] != nil
            out.append((Track(url: trackURL, title: title, machines: machines, bpm: bpm,
                              seconds: seconds, fleet: fleet, isCollection: isCollection),
                        entry["section"] as? String))
        }
        return out
    }

    private static func read(_ url: URL) -> [String: Any]? {
        guard let data = try? Data(contentsOf: url) else { return nil }
        return (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
    }

    private static func intValue(_ v: Any?) -> Int? {
        if let i = v as? Int { return i }
        if let d = v as? Double { return Int(d) }
        return nil
    }

    private static func doubleValue(_ v: Any?) -> Double? {
        if let d = v as? Double { return d }
        if let i = v as? Int { return Double(i) }
        return nil
    }

    /// Beats of the longest `notes`/`notes2`/… track across the voices — the
    /// same measure the host-play recorder and the QuickLook preview use.
    private static func longestBeats(_ voices: [[String: Any]]) -> Double {
        voices.flatMap { voice in
            ["notes", "notes2", "notes3", "notes4"].compactMap { voice[$0] as? String }
        }.map { spec in
            spec.split(separator: ",").reduce(0.0) { total, token in
                let parts = token.split(separator: ":")
                return total + (parts.count == 2 ? (Double(parts[1]) ?? 0) : 0)
            }
        }.max() ?? 0
    }

    private static func clock(_ seconds: Double) -> String {
        let s = Int(seconds.rounded())
        return String(format: "%d:%02d", s / 60, s % 60)
    }

    // MARK: - Layout

    private func buildContent(obj: [String: Any], total: Double) {
        guard let window else { return }
        let content = NSView()
        window.contentView = content

        let header = NSTextField(wrappingLabelWithString: Self.headerText(obj, count: trackCount, total: total))
        header.translatesAutoresizingMaskIntoConstraints = false
        header.font = NSFont.systemFont(ofSize: 12)
        header.textColor = .secondaryLabelColor
        header.maximumNumberOfLines = 3
        header.lineBreakMode = .byTruncatingTail
        content.addSubview(header)

        let scroll = NSScrollView()
        scroll.translatesAutoresizingMaskIntoConstraints = false
        scroll.hasVerticalScroller = true
        scroll.borderType = .bezelBorder
        content.addSubview(scroll)

        let table = NSTableView()
        table.usesAlternatingRowBackgroundColors = true
        table.headerView = NSTableHeaderView()
        table.allowsMultipleSelection = false
        table.style = .inset
        table.rowHeight = 22
        table.floatsGroupRows = true
        table.dataSource = self
        table.delegate = self
        table.target = self
        table.doubleAction = #selector(playSelected)

        let columns: [(String, String, CGFloat, NSTextAlignment)] = [
            ("number", "#", 32, .right),
            ("title", "Track", 280, .left),
            ("members", "Members", 70, .left),
            ("bpm", "bpm", 44, .right),
            ("length", "Length", 56, .right),
        ]
        for (id, title, width, _) in columns {
            let col = NSTableColumn(identifier: NSUserInterfaceItemIdentifier(id))
            col.title = title
            col.width = width
            col.minWidth = id == "title" ? 140 : width
            col.resizingMask = id == "title" ? .autoresizingMask : []
            table.addTableColumn(col)
        }
        table.columnAutoresizingStyle = .lastColumnOnlyAutoresizingStyle
        scroll.documentView = table
        tableView = table

        let playButton = NSButton(title: "Play", target: self, action: #selector(playSelected))
        playButton.bezelStyle = .rounded
        playButton.keyEquivalent = "\r"
        playButton.translatesAutoresizingMaskIntoConstraints = false
        self.playButton = playButton

        let stopButton = NSButton(title: "Stop", target: self, action: #selector(stopPlaying))
        stopButton.bezelStyle = .rounded
        stopButton.keyEquivalent = "."
        stopButton.keyEquivalentModifierMask = [.command]
        stopButton.translatesAutoresizingMaskIntoConstraints = false

        let footer = NSTextField(labelWithString: "\(trackCount) tracks · \(Self.clock(total))")
        footer.translatesAutoresizingMaskIntoConstraints = false
        footer.font = NSFont.monospacedDigitSystemFont(ofSize: 11, weight: .regular)
        footer.textColor = .secondaryLabelColor

        content.addSubview(playButton)
        content.addSubview(stopButton)
        content.addSubview(footer)

        NSLayoutConstraint.activate([
            header.topAnchor.constraint(equalTo: content.topAnchor, constant: 12),
            header.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            header.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),

            scroll.topAnchor.constraint(equalTo: header.bottomAnchor, constant: 10),
            scroll.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 12),
            scroll.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -12),
            scroll.bottomAnchor.constraint(equalTo: playButton.topAnchor, constant: -12),

            footer.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            footer.centerYAnchor.constraint(equalTo: playButton.centerYAnchor),

            playButton.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -12),
            playButton.bottomAnchor.constraint(equalTo: content.bottomAnchor, constant: -12),
            playButton.widthAnchor.constraint(greaterThanOrEqualToConstant: 72),
            stopButton.trailingAnchor.constraint(equalTo: playButton.leadingAnchor, constant: -8),
            stopButton.centerYAnchor.constraint(equalTo: playButton.centerYAnchor),
            stopButton.widthAnchor.constraint(greaterThanOrEqualToConstant: 72),
        ])
        updatePlayButton()
    }

    private static func headerText(_ obj: [String: Any], count: Int, total: Double) -> String {
        var parts: [String] = []
        if let composer = obj["composer"] as? String, !composer.isEmpty { parts.append(composer) }
        if let description = obj["description"] as? String, !description.isEmpty { parts.append(description) }
        if parts.isEmpty { parts.append("\(count) tracks · \(clock(total))") }
        return parts.joined(separator: "\n")
    }

    // MARK: - Actions

    private var selectedTrack: Track? {
        let row = tableView.selectedRow
        guard row >= 0, row < rows.count, case .track(let t, _) = rows[row] else { return nil }
        return t
    }

    @objc private func playSelected() {
        guard let track = selectedTrack else { return }
        guard FileManager.default.fileExists(atPath: track.url.path) else {
            NSLog("🎼 mbscore collection: missing track \(track.url.lastPathComponent)")
            NSSound.beep()
            return
        }
        NSLog("🎼 mbscore collection: ▶ \(track.title)")
        play(track.url)
    }

    @objc private func stopPlaying() { stop() }

    private func updatePlayButton() {
        let t = selectedTrack
        playButton.isEnabled = t != nil
        playButton.title = (t?.isCollection ?? false) ? "Open" : "Play"
    }

    // MARK: - NSTableViewDataSource / NSTableViewDelegate

    func numberOfRows(in tableView: NSTableView) -> Int { rows.count }

    func tableView(_ tableView: NSTableView, isGroupRow row: Int) -> Bool {
        if case .section = rows[row] { return true }
        return false
    }

    func tableView(_ tableView: NSTableView, shouldSelectRow row: Int) -> Bool {
        if case .track = rows[row] { return true }
        return false
    }

    func tableView(_ tableView: NSTableView, heightOfRow row: Int) -> CGFloat {
        if case .section = rows[row] { return 24 }
        return 22
    }

    func tableViewSelectionDidChange(_ notification: Notification) { updatePlayButton() }

    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        switch rows[row] {
        case .section(let name):
            let id = NSUserInterfaceItemIdentifier("section")
            let cell = tableView.makeView(withIdentifier: id, owner: self) as? NSTableCellView ?? makeCell(id, alignment: .left)
            cell.textField?.stringValue = name
            cell.textField?.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
            cell.textField?.textColor = .secondaryLabelColor
            return cell
        case .track(let t, let number):
            guard let column = tableColumn else { return nil }
            let key = column.identifier.rawValue
            let id = NSUserInterfaceItemIdentifier("cell-\(key)")
            let alignment: NSTextAlignment = (key == "number" || key == "bpm" || key == "length") ? .right : .left
            let cell = tableView.makeView(withIdentifier: id, owner: self) as? NSTableCellView ?? makeCell(id, alignment: alignment)
            cell.textField?.textColor = .labelColor
            switch key {
            case "number":
                cell.textField?.stringValue = "\(number)"
                cell.textField?.font = NSFont.monospacedDigitSystemFont(ofSize: 11, weight: .regular)
                cell.textField?.textColor = .secondaryLabelColor
            case "title":
                cell.textField?.stringValue = t.isCollection ? "\(t.title)  ▸" : t.title
                cell.textField?.font = NSFont.systemFont(ofSize: 12)
            case "members":
                cell.textField?.stringValue = String(repeating: "●", count: max(1, t.machines)) + (t.fleet ? "  fleet" : "")
                cell.textField?.font = NSFont.systemFont(ofSize: 10)
                cell.textField?.textColor = .secondaryLabelColor
            case "bpm":
                cell.textField?.stringValue = "\(t.bpm)"
                cell.textField?.font = NSFont.monospacedDigitSystemFont(ofSize: 11, weight: .regular)
            case "length":
                cell.textField?.stringValue = Self.clock(t.seconds)
                cell.textField?.font = NSFont.monospacedDigitSystemFont(ofSize: 11, weight: .regular)
            default:
                cell.textField?.stringValue = ""
            }
            return cell
        }
    }

    private func makeCell(_ id: NSUserInterfaceItemIdentifier, alignment: NSTextAlignment) -> NSTableCellView {
        let cell = NSTableCellView()
        cell.identifier = id
        let tf = NSTextField(labelWithString: "")
        tf.translatesAutoresizingMaskIntoConstraints = false
        tf.lineBreakMode = .byTruncatingTail
        tf.alignment = alignment
        cell.addSubview(tf)
        cell.textField = tf
        NSLayoutConstraint.activate([
            tf.leadingAnchor.constraint(equalTo: cell.leadingAnchor, constant: 4),
            tf.trailingAnchor.constraint(equalTo: cell.trailingAnchor, constant: -4),
            tf.centerYAnchor.constraint(equalTo: cell.centerYAnchor),
        ])
        return cell
    }
}
