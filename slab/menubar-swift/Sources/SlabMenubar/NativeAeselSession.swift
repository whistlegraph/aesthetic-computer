import Foundation
import Darwin

/// Read the native app's sandbox export on the snapshot worker. No RPC,
/// polling waits, provider launch, or UI activation is needed.
enum NativeAeselSession {
    static let bundleID = "computer.aesthetic.aesel.native"
    private static let lock = NSLock()

    static func decode(_ data: Data, now: Date = Date(),
                       ownsPID: (Int) -> Bool) -> (marker: [String: Any], layout: [String: Any])? {
        guard data.count <= 16384,
              let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              object["schema"] as? Int == 1,
              let marker = object["marker"] as? [String: Any],
              let layout = object["layout"] as? [String: Any],
              let id = marker["session_id"] as? String, id.hasPrefix("aesel-native-"),
              UUID(uuidString: String(id.dropFirst("aesel-native-".count))) != nil,
              ["easel", "aesel"].contains(marker["agent_type"] as? String ?? ""), // Aesel was Easel
              marker["host_app"] as? String == bundleID,
              let pid = marker["agent_pid"] as? Int, pid > 0, pid <= Int(Int32.max),
              marker["host_pid"] as? Int == pid, ownsPID(pid),
              let window = marker["host_window_id"] as? Int, window > 0,
              let updated = marker["updated"] as? String,
              let date = ISO8601DateFormatter().date(from: updated),
              now.timeIntervalSince(date) >= -5, now.timeIntervalSince(date) < 30,
              let state = marker["state"] as? String,
              ["blank", "working", "complete", "awaiting", "interrupted"].contains(state),
              layout["sessionId"] as? String == id,
              layout["visible"] is Bool,
              let x = layout["x"] as? Double, let y = layout["y"] as? Double,
              let size = layout["size"] as? Double,
              let width = layout["windowWidth"] as? Double, let height = layout["windowHeight"] as? Double,
              [x, y, size, width, height].allSatisfy({ $0.isFinite }),
              x >= 0, y >= 0, size >= 16, size <= 128,
              x + size <= width, y + size <= height else { return nil }
        // Copy only the public status contract, never arbitrary app state.
        let fields: Set<String> = ["session_id", "agent_type", "agent_pid", "host_app", "host_pid",
            "host_window_id", "subject", "summary", "piece", "piece_version", "state", "updated", "started_at"]
        var clean = marker.filter { fields.contains($0.key) }
        clean["cwd"] = ""
        clean["tty"] = ""
        return (clean, layout)
    }

    // LaunchServices can stall while applications register. Validate the live
    // executable and start time directly instead of making a synchronous XPC
    // request from Slab's snapshot worker.
    private static func ownsProcess(_ pid: Int, modified: Date) -> Bool {
        var path = [CChar](repeating: 0, count: 4096)
        guard proc_pidpath(pid_t(pid), &path, UInt32(path.count)) > 0 else { return false }
        let executable = URL(fileURLWithPath: String(cString: path))
        guard executable.deletingLastPathComponent().lastPathComponent == "MacOS" else { return false }
        let infoFile = executable.deletingLastPathComponent().deletingLastPathComponent().appendingPathComponent("Info.plist")
        guard let data = try? Data(contentsOf: infoFile),
              let info = try? PropertyListSerialization.propertyList(from: data, format: nil) as? [String: Any],
              info["CFBundleIdentifier"] as? String == bundleID else { return false }
        var process = proc_bsdinfo()
        guard proc_pidinfo(pid_t(pid), PROC_PIDTBSDINFO, 0, &process, Int32(MemoryLayout<proc_bsdinfo>.size))
            == Int32(MemoryLayout<proc_bsdinfo>.size) else { return false }
        return Double(process.pbi_start_tvsec) <= modified.timeIntervalSince1970
    }

    static func refresh() {
        lock.lock()
        defer { lock.unlock() }
        let fm = FileManager.default
        let directory = URL(fileURLWithPath: NSHomeDirectory())
            .appendingPathComponent("Library/Containers/\(bundleID)/Data/Library/Application Support/\(bundleID)/slab/windows")
        let active = URL(fileURLWithPath: Paths.activePromptsDir)
        let layouts = active.deletingLastPathComponent().appendingPathComponent("easel-layout")
        var keep = Set<String>()
        var liveLayouts = Set<String>()
        let files = ((try? fm.contentsOfDirectory(at: directory, includingPropertiesForKeys: [.contentModificationDateKey])) ?? [])
            .filter { $0.pathExtension == "json" && UUID(uuidString: $0.deletingPathExtension().lastPathComponent) != nil }
            .sorted { ((try? $0.resourceValues(forKeys: [.contentModificationDateKey]).contentModificationDate) ?? .distantPast)
                > ((try? $1.resourceValues(forKeys: [.contentModificationDateKey]).contentModificationDate) ?? .distantPast) }
        for file in files.prefix(64) {
        guard let attrs = try? fm.attributesOfItem(atPath: file.path),
           attrs[.type] as? FileAttributeType == .typeRegular,
           (attrs[.ownerAccountID] as? NSNumber)?.uint32Value == getuid(),
           let size = attrs[.size] as? NSNumber, size.intValue <= 16384,
           let data = try? Data(contentsOf: file),
           let record = decode(data, ownsPID: { pid in
               ownsProcess(pid, modified: attrs[.modificationDate] as? Date ?? .distantPast)
           }),
           let id = record.marker["session_id"] as? String,
           let pid = record.marker["host_pid"] as? Int,
           let window = record.marker["host_window_id"] as? Int else { continue }
            do {
                for dir in [active, layouts] {
                    try fm.createDirectory(at: dir, withIntermediateDirectories: true, attributes: [.posixPermissions: 0o700])
                }
                for (value, target) in [(record.layout, layouts.appendingPathComponent("\(pid)-\(window).json")),
                                        (record.marker, active.appendingPathComponent(id))] {
                    try JSONSerialization.data(withJSONObject: value).write(to: target, options: .atomic)
                    try fm.setAttributes([.posixPermissions: 0o600], ofItemAtPath: target.path)
                }
                keep.insert(id)
                liveLayouts.insert("\(pid)-\(window).json")
            } catch { NSLog("Native aesel prox import failed: %@", error.localizedDescription) }
        }
        // A closed app, replaced instance, or stalled heartbeat cannot leave a
        // ghost prox. Only this bridge's namespace is eligible for removal.
        for name in (try? fm.contentsOfDirectory(atPath: active.path)) ?? []
            where name.hasPrefix("aesel-native-") && !keep.contains(name) {
            let marker = active.appendingPathComponent(name)
            if let data = try? Data(contentsOf: marker),
               let old = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
               let pid = old["host_pid"] as? Int, let window = old["host_window_id"] as? Int {
                let layout = "\(pid)-\(window).json"
                if !liveLayouts.contains(layout) { try? fm.removeItem(at: layouts.appendingPathComponent(layout)) }
            }
            try? fm.removeItem(at: marker)
        }
    }
}
