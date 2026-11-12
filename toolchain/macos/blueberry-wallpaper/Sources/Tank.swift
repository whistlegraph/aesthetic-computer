import Foundation
import Network

/// One aquarium across several Macs, side by side. Each machine draws its own
/// screen as a window onto a wider world; the leader runs the fish and
/// streams them, followers draw what it sends.
///
/// `~/.config/blueberry-wallpaper/tank.json`, per machine:
///
///     { "role": "leader" | "follower", "width": 5120, "offset": 0, "port": 47390 }
///
/// `width` is the whole world in points, `offset` where this screen starts in
/// it. The link is loopback only: the leader listens on 127.0.0.1 and a
/// reverse SSH tunnel brings that port to each follower's 127.0.0.1, so no
/// firewall has to admit the app. No file: one screen, no link.
struct TankConfig {
    enum Role: String { case leader, follower }
    let role: Role
    let width: CGFloat
    let offset: CGFloat
    let port: UInt16

    static let current: TankConfig? = {
        let path = NSHomeDirectory() + "/.config/blueberry-wallpaper/tank.json"
        guard let data = FileManager.default.contents(atPath: path),
              let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any],
              let role = Role(rawValue: obj["role"] as? String ?? ""),
              let width = (obj["width"] as? NSNumber)?.doubleValue, width > 0
        else { return nil }
        return TankConfig(role: role, width: CGFloat(width),
                          offset: CGFloat((obj["offset"] as? NSNumber)?.doubleValue ?? 0),
                          port: UInt16((obj["port"] as? NSNumber)?.intValue ?? 47390))
    }()
}

/// What a follower needs to place one fish; everything else it derives.
struct RemoteFish {
    var x, homeY, depth, dir, facing, alpha, glow, boost, lunge: CGFloat
    var leaving: Bool
}

/// Newline-delimited JSON over one loopback TCP connection.
///   {"k":"hello","home":"/Users/…"}            once per connection
///   {"k":"data","data":{…fishery.json…}}       when the feeder's file changes
///   {"k":"fish","at":<unix s>,"f":[[id,x,homeY,depth,dir,facing,alpha,glow,boost,lunge,leaving]…]}
final class TankLink {
    static let shared: TankLink? = TankConfig.current.map(TankLink.init)

    let config: TankConfig
    private let queue = DispatchQueue(label: "computer.aesthetic.blueberry-wallpaper.tank")

    // Leader side.
    private var listener: NWListener?
    private var clients: [NWConnection] = []
    private var lastData: Data?

    // Follower side (read and written on main).
    private var connection: NWConnection?
    private var buffer = Data()
    private var frames: [(at: Double, fish: [String: RemoteFish])] = []
    private(set) var receivedAt: Double = 0
    private(set) var dataVersion = 0
    private(set) var data: Data?
    private var leaderHome = ""

    /// Followers draw this far behind the leader, so a late packet has
    /// already arrived by the time its moment is shown.
    static let delay: Double = 0.18

    init(config: TankConfig) {
        self.config = config
        if config.role == .leader { listen() } else { connect() }
    }

    /// A follower is live while the leader's stream keeps arriving.
    var following: Bool {
        config.role == .follower && Date().timeIntervalSince1970 - receivedAt < 3
    }

    // ── leader ───────────────────────────────────────────────────────────

    private func listen() {
        let params = NWParameters.tcp
        params.requiredLocalEndpoint = .hostPort(host: "127.0.0.1", port: NWEndpoint.Port(rawValue: config.port)!)
        params.allowLocalEndpointReuse = true
        guard let l = try? NWListener(using: params) else {
            print("tank: cannot listen on \(config.port)"); fflush(stdout)
            queue.asyncAfter(deadline: .now() + 5) { [weak self] in self?.listen() }
            return
        }
        l.newConnectionHandler = { [weak self] c in
            guard let self else { return }
            c.stateUpdateHandler = { [weak self, weak c] state in
                guard let self, let c else { return }
                switch state {
                case .ready:
                    print("tank: follower joined"); fflush(stdout)
                    self.send(Self.line(["k": "hello", "home": NSHomeDirectory()]), to: c)
                    if let d = self.lastData { self.send(Self.dataLine(d), to: c) }
                case .failed, .cancelled:
                    self.clients.removeAll { $0 === c }
                default: break
                }
            }
            self.clients.append(c)
            c.start(queue: self.queue)
        }
        l.stateUpdateHandler = { [weak self] state in
            if case .failed = state {
                l.cancel()
                self?.queue.asyncAfter(deadline: .now() + 5) { self?.listen() }
            }
        }
        listener = l
        l.start(queue: queue)
    }

    private func send(_ line: Data, to c: NWConnection) {
        c.send(content: line, completion: .contentProcessed { _ in })
    }

    private func broadcast(_ line: Data) {
        queue.async { [weak self] in
            guard let self else { return }
            for c in self.clients where c.state == .ready { self.send(line, to: c) }
        }
    }

    /// The feeder's file, as the leader just read it.
    func publish(data: Data) {
        queue.async { [weak self] in self?.lastData = data }
        broadcast(Self.dataLine(data))
    }

    func publish(fish: [(String, RemoteFish)], at: Double) {
        var out = Data("{\"k\":\"fish\",\"at\":\(at),\"f\":[".utf8)
        for (i, (id, r)) in fish.enumerated() {
            if i > 0 { out.append(0x2c) }
            let n = { (v: CGFloat) in String(format: "%.2f", Double(v)) }
            out.append(Data("[\"\(id)\",\(n(r.x)),\(n(r.homeY)),\(String(format: "%.3f", Double(r.depth))),\(n(r.dir)),\(String(format: "%.3f", Double(r.facing))),\(String(format: "%.3f", Double(r.alpha))),\(String(format: "%.3f", Double(r.glow))),\(String(format: "%.3f", Double(r.boost))),\(String(format: "%.3f", Double(r.lunge))),\(r.leaving ? 1 : 0)]".utf8))
        }
        out.append(Data("]}\n".utf8))
        broadcast(out)
    }

    private static func line(_ obj: [String: Any]) -> Data {
        var d = (try? JSONSerialization.data(withJSONObject: obj)) ?? Data("{}".utf8)
        d.append(0x0a)
        return d
    }

    /// The file goes inline. JSON only has raw newlines between tokens, so
    /// turning them into spaces keeps it one line without changing it.
    private static func dataLine(_ file: Data) -> Data {
        var d = Data("{\"k\":\"data\",\"data\":".utf8)
        d.append(Data(file.map { $0 == 0x0a || $0 == 0x0d ? 0x20 : $0 }))
        d.append(Data("}\n".utf8))
        return d
    }

    // ── follower ─────────────────────────────────────────────────────────

    private func connect() {
        let c = NWConnection(host: "127.0.0.1", port: NWEndpoint.Port(rawValue: config.port)!, using: .tcp)
        c.stateUpdateHandler = { [weak self] state in
            switch state {
            case .ready:
                print("tank: following"); fflush(stdout)
            case .failed, .cancelled, .waiting:
                c.cancel()
                self?.queue.asyncAfter(deadline: .now() + 2) {
                    guard let self, self.connection === c else { return }
                    self.connection = nil
                    self.connect()
                }
            default: break
            }
        }
        connection = c
        receive(c)
        c.start(queue: queue)
    }

    private func receive(_ c: NWConnection) {
        c.receive(minimumIncompleteLength: 1, maximumLength: 1 << 20) { [weak self] chunk, _, done, error in
            guard let self else { return }
            if let chunk { DispatchQueue.main.async { self.ingest(chunk) } }
            if done || error != nil { c.cancel(); return }
            self.receive(c)
        }
    }

    private func ingest(_ chunk: Data) {
        buffer.append(chunk)
        while let nl = buffer.firstIndex(of: 0x0a) {
            let line = buffer[buffer.startIndex..<nl]
            buffer.removeSubrange(buffer.startIndex...nl)
            handle(Data(line))
        }
        if buffer.count > 8 << 20 { buffer.removeAll() }
    }

    private func handle(_ line: Data) {
        guard let obj = (try? JSONSerialization.jsonObject(with: line)) as? [String: Any] else { return }
        receivedAt = Date().timeIntervalSince1970
        switch obj["k"] as? String {
        case "hello":
            leaderHome = obj["home"] as? String ?? ""
        case "data":
            guard let inner = obj["data"],
                  var bytes = try? JSONSerialization.data(withJSONObject: inner) else { return }
            // Avatars and skins live at the same place under each home.
            if !leaderHome.isEmpty, leaderHome != NSHomeDirectory(),
               let s = String(data: bytes, encoding: .utf8) {
                let escaped = { (p: String) in p.replacingOccurrences(of: "/", with: "\\/") }
                bytes = Data(s.replacingOccurrences(of: escaped(leaderHome), with: escaped(NSHomeDirectory()))
                              .replacingOccurrences(of: leaderHome, with: NSHomeDirectory()).utf8)
            }
            data = bytes
            dataVersion += 1
        case "fish":
            guard let at = (obj["at"] as? NSNumber)?.doubleValue, let rows = obj["f"] as? [[Any]] else { return }
            var fish: [String: RemoteFish] = [:]
            for r in rows where r.count >= 11 {
                guard let id = r[0] as? String else { continue }
                let v = { (i: Int) in CGFloat((r[i] as? NSNumber)?.doubleValue ?? 0) }
                fish[id] = RemoteFish(x: v(1), homeY: v(2), depth: v(3), dir: v(4), facing: v(5), alpha: v(6),
                                      glow: v(7), boost: v(8), lunge: v(9), leaving: v(10) > 0)
            }
            frames.append((at, fish))
            if frames.count > 60 { frames.removeFirst(frames.count - 60) }
        default: break
        }
    }

    /// Fish as the leader had them `delay` ago, interpolated between the two
    /// frames around that moment. Nil until the stream is running.
    func fish(at now: Double) -> [String: RemoteFish]? {
        guard following, let last = frames.last else { return nil }
        let t = now - Self.delay
        guard let i = frames.lastIndex(where: { $0.at <= t }), i + 1 < frames.count else { return last.fish }
        let a = frames[i], b = frames[i + 1]
        let k = CGFloat((t - a.at) / max(0.001, b.at - a.at))
        let mix = { (p: CGFloat, q: CGFloat) in p + (q - p) * k }
        var out = b.fish
        for (id, q) in b.fish {
            guard let p = a.fish[id] else { continue }
            // A wrap from one edge to the other is a jump, not a glide.
            let wrapped = abs(q.x - p.x) > config.width / 2
            out[id] = RemoteFish(x: wrapped ? q.x : mix(p.x, q.x), homeY: mix(p.homeY, q.homeY),
                                 depth: mix(p.depth, q.depth), dir: q.dir, facing: mix(p.facing, q.facing),
                                 alpha: mix(p.alpha, q.alpha), glow: mix(p.glow, q.glow),
                                 boost: mix(p.boost, q.boost), lunge: mix(p.lunge, q.lunge), leaving: q.leaving)
        }
        return out
    }
}
