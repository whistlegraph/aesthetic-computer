import AppKit

// Loopboy-mode machine observer. This is intentionally read-only: it resolves
// a stable fleet identity, reads that host's cached Slab ledger plus an optional
// mission snapshot, and paints aggregate state. It exposes no fleet-control API.

enum LoopboyHeartbeatState: String {
    case active, healthy, quiet, stalled, offline, unknown
}

struct LoopboyHeartbeatSnapshot {
    let state: LoopboyHeartbeatState
    let level: CGFloat
    let boundedProgress: CGFloat?
    let activeRocks: Int
}

enum LoopboyHeartbeatContract {
    static let activeStatuses = Set(["working", "rendering", "awaiting"])

    static func aggregate(known: Bool, online: Bool?, now: TimeInterval,
                          machineUpdated: TimeInterval, rocks: [[String: Any]],
                          mission: [String: Any]?) -> LoopboyHeartbeatSnapshot {
        guard known else { return .init(state: .unknown, level: 0, boundedProgress: nil, activeRocks: 0) }
        if online == false { return .init(state: .offline, level: 0, boundedProgress: nil, activeRocks: 0) }
        let rockTimes = rocks.compactMap { rock -> Double? in
            guard var value = (rock["updated"] as? NSNumber)?.doubleValue else { return nil }
            if value > 100_000_000_000 { value /= 1000 } // Slab ledger uses epoch ms.
            return value
        }
        let missionTime = (mission?["updatedAt"] as? NSNumber)?.doubleValue ?? 0
        let freshest = max(machineUpdated, rockTimes.max() ?? 0, missionTime)
        let age = freshest > 0 ? max(0, now - freshest) : .infinity
        let active = rocks.filter { activeStatuses.contains(($0["status"] as? String ?? "").lowercased()) }
        let blocked = mission?["blocked"] as? Bool == true
        let state: LoopboyHeartbeatState
        if blocked || (age > 15 * 60 && !active.isEmpty) { state = .stalled }
        else if !active.isEmpty && age <= 3 * 60 { state = .active }
        else if online == true && age <= 15 * 60 { state = .healthy }
        else if online == true || age <= 60 * 60 { state = .quiet }
        else { state = .offline }
        let decay = age.isFinite ? max(0.08, 1 - age / (60 * 60)) : 0
        let level: CGFloat = state == .active ? 1 : state == .healthy ? max(0.55, decay)
            : state == .quiet ? min(0.45, decay) : state == .stalled ? 0.22 : 0
        var progress: CGFloat?
        if mission?["bounded"] as? Bool == true,
           let raw = (mission?["progress"] as? NSNumber)?.doubleValue {
            progress = min(1, max(0, raw))
        }
        return .init(state: state, level: level, boundedProgress: progress, activeRocks: active.count)
    }
}

private final class LoopboyHeartbeatView: NSView {
    var snapshot = LoopboyHeartbeatSnapshot(state: .unknown, level: 0, boundedProgress: nil, activeRocks: 0) {
        didSet { needsDisplay = true }
    }
    private var phase: CGFloat = 0
    func advance() { phase += 0.12; needsDisplay = true }

    override func draw(_ dirtyRect: NSRect) {
        let track = bounds.insetBy(dx: 1, dy: 1)
        NSColor.black.withAlphaComponent(0.22).setFill()
        NSBezierPath(roundedRect: track, xRadius: 1.5, yRadius: 1.5).fill()
        let color: NSColor
        switch snapshot.state {
        case .active, .healthy: color = .systemGreen
        case .quiet: color = .systemTeal
        case .stalled: color = .systemOrange
        case .offline: color = .systemRed
        case .unknown: color = .systemGray
        }
        let width: CGFloat
        if let real = snapshot.boundedProgress { width = track.width * real }
        else if snapshot.state == .active {
            // A traveling pulse conveys activity without pretending completion.
            width = max(10, track.width * 0.22)
        } else { width = track.width * snapshot.level }
        var x = track.minX
        if snapshot.boundedProgress == nil && snapshot.state == .active {
            x = track.minX + (track.width + width) * ((sin(phase) + 1) / 2) - width
        }
        NSGraphicsContext.current?.saveGraphicsState()
        NSBezierPath(roundedRect: track, xRadius: 1.5, yRadius: 1.5).addClip()
        color.withAlphaComponent(snapshot.state == .offline ? 0.38 : 0.9).setFill()
        NSBezierPath(roundedRect: NSRect(x: x, y: track.minY, width: width, height: track.height),
                     xRadius: 1.5, yRadius: 1.5).fill()
        NSGraphicsContext.current?.restoreGraphicsState()
    }
}

final class LoopboyHeartbeatPlugin: NSObject, PalPlugin {
    private weak var controller: PalController?
    private let view = LoopboyHeartbeatView()
    private let registryPath: String
    private let ledgerDir: String
    private let stateDir: String
    private var target: String
    private var resolvedID: String?
    private var machines: [String: [String: Any]] = [:]

    init(target: String, registryPath: String? = nil, ledgerDir: String? = nil) {
        self.target = target
        let home = FileManager.default.homeDirectoryForCurrentUser.path
        self.registryPath = registryPath ?? ProcessInfo.processInfo.environment["FLEET_MACHINES"]
            ?? home + "/aesthetic-computer-vault/machines.json"
        self.ledgerDir = ledgerDir ?? home + "/.config/slab/ledger"
        self.stateDir = home + "/.config/slab/loopboy-machines"
        super.init()
    }

    func attach(to c: PalController) {
        controller = c
        view.wantsLayer = true
        c.content.addSubview(view)
        loadRegistry()
        resolveTarget(target)
        refresh()
    }

    func layoutRows(in c: PalController, originY: CGFloat) {
        // Explicit Loopboy-mode gate: this plugin is constructed only when
        // --loopboy is present. Ordinary Macpals never create or lay out this bar.
        view.frame = NSRect(x: 2, y: max(0, c.content.bounds.height - 5),
                            width: max(1, c.content.bounds.width - 4), height: 4)
    }

    func setCollapsed(_ collapsed: Bool) { view.isHidden = false }
    func tick() { refresh(); view.advance() }

    func menuItems(for c: PalController) -> [NSMenuItem] {
        let root = NSMenuItem(title: "Loopboy machine: \(resolvedID ?? target)", action: nil, keyEquivalent: "")
        let menu = NSMenu()
        for id in machines.keys.sorted() {
            let item = NSMenuItem(title: id, action: #selector(selectMachine(_:)), keyEquivalent: "")
            item.target = self; item.representedObject = id
            item.state = id == resolvedID ? .on : .off
            menu.addItem(item)
        }
        root.submenu = menu
        return [root]
    }

    @objc private func selectMachine(_ sender: NSMenuItem) {
        guard let id = sender.representedObject as? String else { return }
        target = id
        resolveTarget(id)
        try? FileManager.default.createDirectory(atPath: stateDir, withIntermediateDirectories: true)
        try? id.write(toFile: stateDir + "/target", atomically: true, encoding: .utf8)
        refresh()
    }

    private func loadRegistry() {
        guard let data = FileManager.default.contents(atPath: registryPath),
              let root = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let raw = root["machines"] as? [String: Any] else { machines = [:]; return }
        machines = raw.compactMapValues { $0 as? [String: Any] }
    }

    private func resolveTarget(_ requested: String) {
        let needle = requested.lowercased().split(separator: ".").first.map(String.init) ?? requested.lowercased()
        resolvedID = machines.first { id, machine in
            let aliases: [String] = [id, machine["hostname"] as? String,
              (machine["tailscale"] as? [String: Any])?["name"] as? String,
              (machine["status"] as? [String: Any])?["key"] as? String].compactMap { $0 }
            return aliases.map { $0.lowercased().split(separator: ".").first.map(String.init)! }.contains(needle)
        }?.key
    }

    private func refresh() {
        guard let id = resolvedID else {
            view.snapshot = LoopboyHeartbeatContract.aggregate(known: false, online: nil,
                now: Date().timeIntervalSince1970, machineUpdated: 0, rocks: [], mission: nil)
            view.toolTip = "Unknown Loopboy machine: \(target)"
            return
        }
        let fm = FileManager.default
        let candidates = [ledgerDir + "/local.json", ledgerDir + "/peers/\(id).json"]
        var ledger: [String: Any]?
        var updated: TimeInterval = 0
        for path in candidates {
            guard let data = fm.contents(atPath: path),
                  let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  (json["host"] as? String)?.lowercased() == id.lowercased() else { continue }
            ledger = json
            updated = ((json["updatedAt"] as? NSNumber)?.doubleValue ?? 0) / 1000
            break
        }
        let rocks = (ledger?["entries"] as? [[String: Any]]) ?? []
        let online: Bool? = ledger == nil ? false : Date().timeIntervalSince1970 - updated < 90
        var mission: [String: Any]?
        if let data = fm.contents(atPath: stateDir + "/\(id).json") {
            mission = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        }
        view.snapshot = LoopboyHeartbeatContract.aggregate(known: true, online: online,
            now: Date().timeIntervalSince1970, machineUpdated: updated, rocks: rocks, mission: mission)
        let progress = view.snapshot.boundedProgress.map { " · \(Int($0 * 100))% bounded" } ?? ""
        view.toolTip = "Loopboy observes \(id): \(view.snapshot.state.rawValue) · \(view.snapshot.activeRocks) active rocks\(progress)"
    }
}
