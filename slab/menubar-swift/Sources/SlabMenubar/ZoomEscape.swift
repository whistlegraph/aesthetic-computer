import AppKit
import Foundation

/// Escape travels over Slab's tailnet ledger, independently of which machine
/// Deskflow currently gives the keyboard. All entry points run on main.
enum ZoomEscape {
    static var cancelPendingInput: (() -> Void)?
    private(set) static var revision: UInt64 = 0
    private static var cancelledAt = Date.distantPast
    private static var lastBroadcast = Date.distantPast

    static func cancelLocal() {
        revision &+= 1
        cancelledAt = Date()
        cancelPendingInput?()
        ZoomLens.zoomOut() // no animation that can race the next pointer event
    }

    /// Also rejects a handoff already in flight when Escape was pressed.
    static func allowsRemoteZoom(startedAt: Double?, revision expected: UInt64) -> Bool {
        guard revision == expected, Date().timeIntervalSince(cancelledAt) > 3 else { return false }
        if let startedAt { return startedAt > cancelledAt.timeIntervalSince1970 }
        return true
    }

    static func cancelFleet() {
        cancelLocal()
        // Deskflow may deliver a physical Escape to both source and target;
        // key repeat and tap recovery must not flood the peer listeners.
        guard Date().timeIntervalSince(lastBroadcast) > 0.25 else { return }
        lastBroadcast = Date()
        DispatchQueue.global(qos: .userInitiated).async {
            let fm = FileManager.default
            let names = (try? fm.contentsOfDirectory(atPath: LedgerStore.peersDir)) ?? []
            var addresses = Set<String>()
            for name in names where name.hasSuffix(".json") {
                guard let data = fm.contents(atPath: "\(LedgerStore.peersDir)/\(name)"),
                      let peer = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                      let ip = peer["ip"] as? String, !ip.isEmpty else { continue }
                addresses.insert(ip)
            }
            // Do not filter by prompt count, Deskflow role, or cache freshness:
            // an idle/reconnected machine can still have stuck compositor zoom.
            for ip in addresses {
                guard let url = URL(string: "http://\(ip):\(LedgerStore.port)/zoom/reset") else { continue }
                var request = URLRequest(url: url, timeoutInterval: 2)
                request.httpMethod = "POST"
                request.httpBody = Data("{}".utf8)
                request.setValue("application/json", forHTTPHeaderField: "Content-Type")
                URLSession.shared.dataTask(with: request) { data, _, error in
                    let result = data.flatMap { try? JSONSerialization.jsonObject(with: $0) as? [String: Any] }
                    if error != nil || result?["ok"] as? Bool != true {
                        NSLog("slab zoom escape: peer %@ did not acknowledge reset", ip)
                    }
                }.resume()
            }
        }
    }
}
