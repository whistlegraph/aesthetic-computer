import Foundation

/// The overlay reads memory; active time and atomic saves run off the UI thread.
/// Closed sessions keep their files. Reopening the same identity keeps its creature.
final class ProxCreatures {
    static let shared = ProxCreatures()
    static var directory: URL {
        URL(fileURLWithPath: "\(Paths.home)/.config/slab/creatures", isDirectory: true)
    }
    private let lock = NSLock()
    private let saves = DispatchQueue(label: "computer.slab.creatures", qos: .utility)
    private var records: [String: ProxCreature] = [:]
    private var ticks: [String: (at: Date, working: Bool)] = [:]
    private var lastSaved: [String: Date] = [:]
    private var dirty = Set<String>()

    func appearance(for id: String) -> ProxCreature? {
        lock.lock(); defer { lock.unlock() }
        return records[id]
    }

    func observe(_ sessions: [ClaudeSession], now: Date = Date()) {
        lock.lock(); defer { lock.unlock() }
        let local = sessions.filter { !$0.isRemote }
        let live = Set(local.map(\.sessionId))
        ticks = ticks.filter { live.contains($0.key) }
        for session in local {
            let id = session.sessionId
            var creature = records[id] ?? load(id: id) ?? ProxCreature.egg(
                id: id, name: SigilRenderer.name(for: session), seed: SigilRenderer.seed(for: id), now: now)
            let oldKey = creature.appearanceKey
            let working = session.state == .working || session.state == .rendering
            if let previous = ticks[id], working && previous.working {
                let delta = now.timeIntervalSince(previous.at)
                // Neither idle time, a sleeping Mac, nor a stopped daemon ages a creature.
                if delta > 0 && delta <= 30 { creature.grow(by: delta) }
            }
            ticks[id] = (now, working)
            let changed = records[id] != creature
            records[id] = creature
            if changed { dirty.insert(id) }
            if lastSaved[id] == nil || creature.appearanceKey != oldKey
                || (dirty.contains(id) && now.timeIntervalSince(lastSaved[id] ?? .distantPast) >= 60) {
                save(creature)
                lastSaved[id] = now
                dirty.remove(id)
            }
        }
    }

    func acquire(_ feature: ProxCreature.Feature, for id: String, provider: ProxCreature.Provider) {
        lock.lock(); defer { lock.unlock() }
        guard var creature = records[id], creature.acquire(feature, provider: provider) else { return }
        records[id] = creature
        save(creature)
        lastSaved[id] = Date()
        dirty.remove(id)
    }

    private func load(id: String) -> ProxCreature? {
        let seed = String(format: "%016llx", SigilRenderer.seed(for: id))
        let url = Self.directory.appendingPathComponent(seed + ".json")
        guard let data = try? Data(contentsOf: url),
              let creature = try? JSONDecoder().decode(ProxCreature.self, from: data),
              creature.isValid, creature.id == id, creature.seed == seed else { return nil }
        return creature
    }

    private func save(_ creature: ProxCreature) {
        saves.async {
            do {
                try FileManager.default.createDirectory(at: Self.directory, withIntermediateDirectories: true)
                let encoder = JSONEncoder()
                encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
                try encoder.encode(creature).write(
                    to: Self.directory.appendingPathComponent(creature.seed + ".json"), options: .atomic)
            } catch { NSLog("[creature] could not save appearance: %@", error.localizedDescription) }
        }
    }
}
