import Foundation

/// Will's sample library at cc.oooooooooooooooo.cc as a drum kit. The site
/// root serves a Strudel sample map — bank name → file paths on the same
/// host — and its drum banks follow Strudel's naming (`tr909_bd`,
/// `tr909_sd`, `noe_hh` …), so a "family" is every bank sharing a prefix.
/// One family at a time fills the five TrackDrum pads.
///
/// Nothing ships in the app: the index is folded into a small family table
/// and the chosen family's hits are fetched once, then played from
/// Application Support (offline after the first pick). Until a family is on
/// disk the kit reports unavailable and drums fall back to the membrane.
enum MenuBandCCDrums {
    typealias Role = MenuBandPercussion.Role

    static let host = URL(string: "https://cc.oooooooooooooooo.cc")!
    static let didChange = Notification.Name("MenuBandCCDrumsDidChange")
    private static let familyKey = "notepat.ccDrumFamily"
    private static let defaultFamily = "noe"
    /// The index is ~8 MB; refetch it at most daily.
    private static let indexMaxAge: TimeInterval = 24 * 60 * 60
    /// One hit per pad: a key always plays the same sound. (Round-robin
    /// across a bank's first files swapped instruments on mixed banks — a
    /// perc bank alternated high and low conga on one key.)
    private static let hitsPerPad = 1

    /// Bank suffixes per pad, best first. Tom and click borrow whatever
    /// percussion the family has (clap, perc, rim) when it has no toms.
    private static let suffixes: [(Role, [String])] = [
        (.kick, ["bd", "kick", "kicks"]),
        (.tom, ["tom", "toms", "lt", "mt", "ht", "perc", "cp"]),
        (.snare, ["sd", "snare", "snares"]),
        (.hat, ["hh", "hats", "hat", "ch"]),
        (.click, ["cp", "clap", "claps", "rim", "rs", "cb", "perc"]),
    ]

    /// The keyboard's twelve drum keys, in `Drum` order, spread over the
    /// standard Strudel/Tidal drum-machine names (bd sd rim cp hh oh cr rd
    /// lt mt ht cb tb sh perc). Each list ends on a bank every family has,
    /// so a missing name falls back to its nearest relative. Keys that land
    /// on the same bank take successive files of it.
    private static let keySuffixes: [(Drum, Role, [String])] = [
        (.kick, .kick, ["bd", "kick", "kicks"]),
        (.crash, .hat, ["cr", "crash", "cy", "oh", "hh", "hats"]),
        (.snare, .snare, ["sd", "snare", "snares"]),
        (.splash, .hat, ["cr", "cy", "crash", "oh", "hh", "hats"]),
        (.clap, .snare, ["cp", "clap", "claps", "sd", "snares"]),
        (.snap, .click, ["rim", "rs", "snap", "cp", "claps", "sd", "snares"]),
        (.cowbell, .click, ["cb", "cowbell", "perc", "rim", "cp", "hh", "hats"]),
        (.hatClosed, .hat, ["hh", "ch", "hats", "hat"]),
        (.block, .tom, ["lt", "mt", "ht", "tom", "toms", "perc", "sd", "snares"]),
        (.hatOpen, .hat, ["oh", "hh", "hats"]),
        (.tambo, .hat, ["tb", "sh", "perc", "misc", "hh", "hats"]),
        (.ride, .hat, ["rd", "ride", "cr", "cy", "oh", "hh", "hats"]),
    ]
    typealias Drum = MenuBandPercussion.Drum

    /// One pad's source: the first few files of a bank.
    struct Pad: Codable {
        let bank: String
        let paths: [String]
    }

    /// `pads` in TrackDrum zone order: kick, tom, snare, hat, click.
    /// `keys` in `Drum` order, one per keyboard drum key.
    struct Family: Codable {
        let name: String
        let pads: [Pad]
        let keys: [Pad]
    }

    private static let queue = DispatchQueue(label: "computer.aestheticcomputer.menuband.cc-drums",
                                             qos: .utility)
    private static let lock = NSLock()
    private static var loadedSpec: MenuBandPercussion.KitSpec?
    private static var loadedName: String?
    private static var table: [Family] = []
    private static var loading: String?

    // MARK: State (any thread)

    /// The family the user picked, persisted.
    static var family: String {
        UserDefaults.standard.string(forKey: familyKey) ?? defaultFamily
    }

    /// Every complete family in the cached index, alphabetical.
    static var families: [Family] {
        lock.lock(); defer { lock.unlock() }
        return table
    }

    /// The decoded kit, once the picked family's hits are on disk.
    static var spec: MenuBandPercussion.KitSpec? {
        lock.lock(); defer { lock.unlock() }
        return loadedSpec
    }

    /// Name of the family `spec` was built from (keys the loudness cache).
    static var loadedFamily: String? {
        lock.lock(); defer { lock.unlock() }
        return loadedName
    }

    static var loadingFamily: String? {
        lock.lock(); defer { lock.unlock() }
        return loading
    }

    // MARK: Loading

    /// Read the cached family table and the picked family's hits, then
    /// refresh the index from the site when it is missing or stale. Safe to
    /// call on every launch.
    static func start() {
        queue.async {
            // A table from an older layout doesn't decode; refetch it.
            let cached = readTable()
            if let cached { setTable(cached) }
            // The picked family may gain files (a new layout, a new key):
            // fetching only ever touches what it's missing.
            loadFamily(family, fetch: true)
            if cached == nil || indexIsStale() { refreshIndex() }
        }
    }

    /// Pick a family: persist it, fetch what's missing, and call `done` on
    /// main with whether its kit is ready to play.
    static func select(_ name: String, done: @escaping (Bool) -> Void) {
        UserDefaults.standard.set(name, forKey: familyKey)
        queue.async {
            let ok = loadFamily(name, fetch: true)
            DispatchQueue.main.async { done(ok) }
        }
    }

    @discardableResult
    private static func loadFamily(_ name: String, fetch: Bool) -> Bool {
        guard let entry = families.first(where: { $0.name == name }) else { return false }
        lock.lock(); loading = name; lock.unlock()
        defer {
            lock.lock(); if loading == name { loading = nil }; lock.unlock()
            post()
        }
        // Pads and keys share files; decode each once.
        var decoded: [String: MenuBandPercussion.RecordedHit] = [:]
        func kitPad(_ pad: Pad, name label: String, role: Role) -> MenuBandPercussion.KitPad? {
            let hits = pad.paths.prefix(hitsPerPad).compactMap { path -> MenuBandPercussion.RecordedHit? in
                if let hit = decoded[path] { return hit }
                guard let file = cachedFile(for: path, fetch: fetch),
                      let hit = MenuBandPercussion.RecordedHit.load(url: file) else { return nil }
                decoded[path] = hit
                return hit
            }
            // A pad that can't play makes the whole family fall back, never
            // a silent zone.
            guard !hits.isEmpty else {
                NSLog("MenuBand cc drums: %@ has no playable hits in %@", name, pad.bank)
                return nil
            }
            return MenuBandPercussion.KitPad(name: label, role: role, layers: [], recordings: hits)
        }
        var pads: [MenuBandPercussion.KitPad] = []
        for ((role, _), pad) in zip(suffixes, entry.pads) {
            guard let built = kitPad(pad, name: label(for: role), role: role) else { return false }
            pads.append(built)
        }
        var keys: [MenuBandPercussion.KitPad] = []
        for ((drum, role, _), pad) in zip(keySuffixes, entry.keys) {
            guard let built = kitPad(pad, name: drum.label, role: role) else { return false }
            keys.append(built)
        }
        // The user may have moved on while this downloaded.
        guard family == name else { return false }
        lock.lock()
        loadedSpec = MenuBandPercussion.KitSpec(pads: pads, keys: keys)
        loadedName = name
        lock.unlock()
        NSLog("MenuBand cc drums: %@ ready (keys %@)", name,
              entry.keys.map(\.bank).joined(separator: " "))
        return true
    }

    private static func refreshIndex() {
        var request = URLRequest(url: host, timeoutInterval: 60)
        request.cachePolicy = .reloadIgnoringLocalCacheData
        guard let data = fetch(request),
              let index = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
            NSLog("MenuBand cc drums: index fetch failed")
            return
        }
        let banks = index.compactMapValues { $0 as? [String] }
        let built = buildFamilies(from: banks)
        guard !built.isEmpty else { return }
        writeTable(built)
        setTable(built)
        loadFamily(family, fetch: true)
        post()
    }

    /// Fold the sample map into complete families: a kick, a snare and a
    /// hat bank are required; tom and click take the best remaining bank.
    static func buildFamilies(from banks: [String: [String]]) -> [Family] {
        var prefixes: [String: [String: String]] = [:]   // family → suffix → bank
        for bank in banks.keys {
            guard let cut = bank.lastIndex(of: "_") else { continue }
            let prefix = String(bank[..<cut])
            let suffix = String(bank[bank.index(after: cut)...])
            prefixes[prefix, default: [:]][suffix] = bank
        }
        var result: [Family] = []
        for (name, owned) in prefixes {
            var used = Set<String>()
            var pads: [Pad] = []
            for (role, choices) in suffixes {
                let pick = choices.lazy.compactMap { owned[$0] }
                    .first { !used.contains($0) && !(banks[$0] ?? []).isEmpty }
                guard let bank = pick ?? fallbackBank(for: role, in: pads) else { break }
                used.insert(bank)
                let files = banks[bank] ?? []
                // A borrowed bank plays the hits its owner pad doesn't, when
                // it has any to spare.
                let skip = pads.contains { $0.bank == bank } ? hitsPerPad : 0
                let spare = Array(files.dropFirst(skip).prefix(hitsPerPad))
                let paths = spare.isEmpty ? Array(files.prefix(hitsPerPad)) : spare
                guard !paths.isEmpty else { break }
                pads.append(Pad(bank: bank, paths: paths))
            }
            guard pads.count == suffixes.count,
                  let keys = spreadKeys(owned: owned, banks: banks) else { continue }
            result.append(Family(name: name, pads: pads, keys: keys))
        }
        return result.sorted { $0.name.localizedStandardCompare($1.name) == .orderedAscending }
    }

    /// One bank per keyboard key, by standard name. Keys sharing a bank
    /// (crash and splash on one `cr`) step through its files, and a spent
    /// bank hands the key to the next name, so each key keeps a sound of
    /// its own while the family has enough.
    private static func spreadKeys(owned: [String: String],
                                   banks: [String: [String]]) -> [Pad]? {
        var uses: [String: Int] = [:]
        var keys: [Pad] = []
        for (_, _, choices) in keySuffixes {
            let present = choices.compactMap { owned[$0] }
                .filter { !(banks[$0] ?? []).isEmpty }
            // Prefer a bank with a file no other key took yet; repeat a
            // sound only when every relative is spent.
            guard let bank = present.first(where: { uses[$0, default: 0] < banks[$0]!.count })
                    ?? present.first,
                  let files = banks[bank] else { return nil }
            let index = uses[bank, default: 0]
            uses[bank] = index + 1
            keys.append(Pad(bank: bank, paths: [files[index % files.count]]))
        }
        return keys
    }

    /// A family with only kick/snare/hat still plays: the tom reuses the
    /// kick bank's later hits and the click the hat's.
    private static func fallbackBank(for role: Role, in pads: [Pad]) -> String? {
        switch role {
        case .tom: return pads.count > 0 ? pads[0].bank : nil
        case .click: return pads.count > 3 ? pads[3].bank : nil
        default: return nil
        }
    }

    private static func label(for role: Role) -> String {
        switch role {
        case .kick: return "KICK"
        case .tom: return "TOM"
        case .snare: return "SNARE"
        case .hat: return "HAT"
        case .click: return "CLICK"
        }
    }

    private static func setTable(_ families: [Family]) {
        lock.lock(); table = families; lock.unlock()
    }

    private static func post() {
        DispatchQueue.main.async {
            NotificationCenter.default.post(name: didChange, object: nil)
        }
    }

    // MARK: Disk

    private static var root: URL {
        FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("MenuBand/cc-drums", isDirectory: true)
    }

    private static var tableURL: URL { root.appendingPathComponent("families.json") }

    private static func readTable() -> [Family]? {
        guard let data = try? Data(contentsOf: tableURL) else { return nil }
        return try? JSONDecoder().decode([Family].self, from: data)
    }

    private static func writeTable(_ families: [Family]) {
        do {
            try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
            try JSONEncoder().encode(families).write(to: tableURL, options: .atomic)
        } catch {
            NSLog("MenuBand cc drums: family table write failed: %@", String(describing: error))
        }
    }

    private static func indexIsStale() -> Bool {
        guard let modified = (try? tableURL.resourceValues(forKeys: [.contentModificationDateKey]))?
            .contentModificationDate else { return true }
        return Date().timeIntervalSince(modified) > indexMaxAge
    }

    /// The local copy of a library file, downloading it when `fetch` allows.
    private static func cachedFile(for path: String, fetch allowFetch: Bool) -> URL? {
        let relative = path.split(separator: "/").map(String.init)
        guard !relative.isEmpty, !relative.contains("..") else { return nil }
        let local = relative.reduce(root.appendingPathComponent("samples", isDirectory: true)) {
            $0.appendingPathComponent($1)
        }
        if FileManager.default.fileExists(atPath: local.path) { return local }
        guard allowFetch,
              var parts = URLComponents(url: host, resolvingAgainstBaseURL: false) else { return nil }
        parts.path = path
        guard let remote = parts.url,
              let data = fetch(URLRequest(url: remote, timeoutInterval: 30)) else { return nil }
        do {
            try FileManager.default.createDirectory(at: local.deletingLastPathComponent(),
                                                    withIntermediateDirectories: true)
            try data.write(to: local, options: .atomic)
            return local
        } catch {
            NSLog("MenuBand cc drums: cache write failed: %@", String(describing: error))
            return nil
        }
    }

    /// Blocking fetch for the utility queue; nil unless HTTP 200.
    private static func fetch(_ request: URLRequest) -> Data? {
        let done = DispatchSemaphore(value: 0)
        var result: Data?
        URLSession.shared.dataTask(with: request) { data, response, _ in
            if (response as? HTTPURLResponse)?.statusCode == 200 { result = data }
            done.signal()
        }.resume()
        done.wait()
        return result
    }
}
