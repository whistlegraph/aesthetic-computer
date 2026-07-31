import AppKit
import Foundation

/// Fleet half of WindowNav. Geometry comes from Deskflow's installed `links`
/// graph, availability comes from the prox ledger, and the currently active
/// controller is the only host allowed to synthesize edge motion.
enum DeskflowSpatialNav {
    private struct Geometry {
        var links: [String: [WindowNav.Direction: String]]

        func positions(from source: String) -> [String: (x: Int, y: Int)] {
            var result: [String: (x: Int, y: Int)] = [source: (0, 0)]
            var queue = [source]
            while !queue.isEmpty {
                let here = queue.removeFirst()
                guard let origin = result[here] else { continue }
                for (dir, next) in links[here] ?? [:] where result[next] == nil {
                    let step = dir.step
                    result[next] = (origin.x + step.x, origin.y + step.y)
                    queue.append(next)
                }
            }
            return result
        }

        func path(from source: String, to target: String) -> [WindowNav.Direction]? {
            var queue: [(String, [WindowNav.Direction])] = [(source, [])]
            var seen = Set([source])
            while !queue.isEmpty {
                let (here, path) = queue.removeFirst()
                if here == target { return path }
                for (dir, next) in links[here] ?? [:] where seen.insert(next).inserted {
                    queue.append((next, path + [dir]))
                }
            }
            return nil
        }
    }

    private struct LedgerHost {
        let host: String
        let ip: String
        let updatedAt: Double
        let promptCount: Int
    }

    static func cross(from direction: WindowNav.Direction, alignment: CGFloat) {
        guard let source = localScreenName(), let geometry = geometry(),
              let target = targetScreen(from: source, direction: direction, geometry: geometry),
              let path = geometry.path(from: source, to: target), !path.isEmpty,
              let targetLedger = ledgers().first(where: {
                  screenToken($0.host) == screenToken(target) && $0.promptCount > 0
              }) else { return }

        DispatchQueue.global(qos: .userInteractive).async {
            // The controller acknowledges as soon as its edge moves are queued.
            // Queue the destination immediately too, with only enough delay for
            // Deskflow to traverse the actual intermediate edges. This removes
            // the old conservative sleep from every cross-host arrow.
            guard routeOnActiveController(path) else { return }
            post(ip: targetLedger.ip, endpoint: "/navigate", body: [
                "direction": (path.last ?? direction).rawValue,
                "alignment": Double(alignment),
                "delayMs": max(10, (path.count - 1) * 28 + 10),
            ])
        }
    }

    /// Decode a tailnet request on the destination machine. The HTTP listener
    /// is off-main; AX and Core Animation are deliberately handed to AppKit.
    static func receiveNavigate(_ body: [String: Any]) -> Bool {
        guard let raw = body["direction"] as? String,
              let direction = WindowNav.Direction(rawValue: raw) else { return false }
        let alignment = CGFloat((body["alignment"] as? NSNumber)?.doubleValue ?? 0.5)
        let delay = min(max((body["delayMs"] as? NSNumber)?.doubleValue ?? 0, 0), 250) / 1000
        DispatchQueue.main.asyncAfter(deadline: .now() + delay) {
            WindowNav.accept(direction, alignment: alignment)
        }
        return true
    }

    /// Decode a route only on the current controller. Clients refuse it, which
    /// prevents two machines from injecting the same traversal after a role
    /// handoff race.
    static func receiveRoute(_ body: [String: Any]) -> Bool {
        guard localRole() == "server" else {
            NSLog("slab fleet nav: refused route on non-controller role %@", localRole() ?? "unknown")
            return false
        }
        guard let values = body["path"] as? [Any] else {
            NSLog("slab fleet nav: route payload has no path")
            return false
        }
        let raws = values.compactMap { $0 as? String }
        let path = raws.compactMap(WindowNav.Direction.init(rawValue:))
        guard path.count == values.count, !path.isEmpty else {
            NSLog("slab fleet nav: invalid route path %@", String(describing: values))
            return false
        }
        DispatchQueue.main.async { WindowNav.routeDeskflow(path) }
        return true
    }

    private static func routeOnActiveController(_ path: [WindowNav.Direction]) -> Bool {
        if localRole() == "server" {
            DispatchQueue.main.async { WindowNav.routeDeskflow(path) }
            return true
        }
        guard let ip = activeControllerAddress() else { return false }
        return post(ip: ip, endpoint: "/deskflow-route",
                    body: ["path": path.map(\.rawValue)])
    }

    private static func targetScreen(from source: String, direction: WindowNav.Direction,
                                     geometry: Geometry) -> String? {
        let positions = geometry.positions(from: source)
        var live: [String: LedgerHost] = [:]
        for ledger in ledgers() where ledger.promptCount > 0
            && Date().timeIntervalSince1970 * 1000 - ledger.updatedAt < 60_000 {
            live[screenToken(ledger.host)] = ledger
        }
        var best: (screen: String, score: Int)?
        for (screen, pos) in positions where screen != source && live[screenToken(screen)] != nil {
            let score: Int
            switch direction {
            case .left: guard pos.x < 0 else { continue }; score = -pos.x + 3 * abs(pos.y)
            case .right: guard pos.x > 0 else { continue }; score = pos.x + 3 * abs(pos.y)
            case .up: guard pos.y < 0 else { continue }; score = -pos.y + 3 * abs(pos.x)
            case .down: guard pos.y > 0 else { continue }; score = pos.y + 3 * abs(pos.x)
            }
            if best == nil || score < best!.score { best = (screen, score) }
        }
        return best?.screen
    }

    private static func geometry() -> Geometry? {
        guard let text = try? String(contentsOfFile: Paths.deskflowServerConfig,
                                     encoding: .utf8) else { return nil }
        var inLinks = false
        var current = ""
        var links: [String: [WindowNav.Direction: String]] = [:]
        for raw in text.split(separator: "\n", omittingEmptySubsequences: false) {
            let line = raw.trimmingCharacters(in: .whitespaces)
            if line == "section: links" { inLinks = true; continue }
            if inLinks && line == "end" { break }
            guard inLinks, !line.isEmpty else { continue }
            if line.hasSuffix(":"), !line.contains(" = ") {
                current = String(line.dropLast())
                links[current, default: [:]] = links[current, default: [:]]
                continue
            }
            let parts = line.split(separator: "=", maxSplits: 1).map {
                $0.trimmingCharacters(in: .whitespaces)
            }
            guard parts.count == 2, !current.isEmpty,
                  let dir = WindowNav.Direction(rawValue: parts[0]) else { continue }
            links[current, default: [:]][dir] = parts[1]
        }
        return links.isEmpty ? nil : Geometry(links: links)
    }

    private static func ledgers() -> [LedgerHost] {
        let fm = FileManager.default
        var paths = [LedgerStore.localFile]
        if let peers = try? fm.contentsOfDirectory(atPath: LedgerStore.peersDir) {
            paths += peers.filter { $0.hasSuffix(".json") }.map { "\(LedgerStore.peersDir)/\($0)" }
        }
        return paths.compactMap { path in
            guard let data = fm.contents(atPath: path),
                  let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let host = obj["host"] as? String, let ip = obj["ip"] as? String,
                  !host.isEmpty, !ip.isEmpty else { return nil }
            return LedgerHost(host: host, ip: ip,
                              updatedAt: (obj["updatedAt"] as? NSNumber)?.doubleValue ?? 0,
                              promptCount: (obj["entries"] as? [[String: Any]])?.count ?? 0)
        }
    }

    private static func localScreenName() -> String? {
        json(Paths.deskflowHandoffConfig)?["screenName"] as? String
    }

    private static func localRole() -> String? {
        json(Paths.deskflowConfig)?["role"] as? String
    }

    private static func activeControllerAddress() -> String? {
        guard let text = try? String(contentsOfFile: Paths.deskflowClientRoleConfig,
                                     encoding: .utf8) else { return nil }
        return text.split(separator: "\n").first { $0.hasPrefix("remoteHost=") }
            .map { String($0.dropFirst("remoteHost=".count)) }
    }

    private static func json(_ path: String) -> [String: Any]? {
        guard let data = FileManager.default.contents(atPath: path) else { return nil }
        return try? JSONSerialization.jsonObject(with: data) as? [String: Any]
    }

    private static func screenToken(_ value: String) -> String {
        value.lowercased().replacingOccurrences(of: ".local", with: "")
    }

    @discardableResult
    private static func post(ip: String, endpoint: String, body: [String: Any]) -> Bool {
        guard let url = URL(string: "http://\(ip):\(LedgerStore.port)\(endpoint)"),
              let data = try? JSONSerialization.data(withJSONObject: body) else { return false }
        var request = URLRequest(url: url, timeoutInterval: 2)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = data
        let semaphore = DispatchSemaphore(value: 0)
        var succeeded = false
        URLSession.shared.dataTask(with: request) { responseData, response, _ in
            if (response as? HTTPURLResponse)?.statusCode == 200,
               let responseData,
               let object = try? JSONSerialization.jsonObject(with: responseData) as? [String: Any] {
                succeeded = (object["ok"] as? Bool) == true
            }
            semaphore.signal()
        }.resume()
        _ = semaphore.wait(timeout: .now() + 2.2)
        return succeeded
    }
}
