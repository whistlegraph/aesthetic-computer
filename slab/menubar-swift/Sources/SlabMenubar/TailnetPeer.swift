import Foundation

struct TailnetPeer {
    let hostname: String
    let online: Bool
    let ip: String

    static func query() -> [TailnetPeer] {
        guard let ts = Tools.resolve("tailscale") else { return [] }
        guard let out = ShellRunner.output(ts, args: ["status", "--json"], timeout: 2) else { return [] }
        guard let data = out.data(using: .utf8),
              let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let peers = json["Peer"] as? [String: [String: Any]]
        else { return [] }

        var result: [TailnetPeer] = []
        for (_, p) in peers {
            guard let host = p["HostName"] as? String else { continue }
            let online = (p["Online"] as? Bool) ?? false
            let ips = (p["TailscaleIPs"] as? [String]) ?? []
            result.append(TailnetPeer(hostname: host, online: online, ip: ips.first ?? ""))
        }
        return result.sorted { lhs, rhs in
            if lhs.online != rhs.online { return lhs.online }
            return lhs.hostname < rhs.hostname
        }
    }
}
