// Radio.swift — KPBJ 95.9 FM, the neighbourhood's community station, in the
// Netlabel tab. One press plays its public stream, another stops it. While
// it plays, the app asks the station's server now and then what is on.

import AVFoundation
import AppKit

final class Radio {
    static let name = "KPBJ"
    static let stream = URL(string: "https://stream.kpbj.fm/stream")!
    static let status = URL(string: "https://stream.kpbj.fm/status-json.xsl")!

    private var player: AVPlayer?
    private var timer: Timer?
    private(set) var title = ""
    var playing: Bool { player != nil }
    var onChange: (() -> Void)?

    func toggle() { playing ? stop() : play() }

    func play() {
        let p = AVPlayer(url: Self.stream)          // a fresh player each time: a live stream is joined, not resumed
        p.play()
        player = p
        ask()
        let t = Timer(timeInterval: 30, repeats: true) { [weak self] _ in self?.ask() }
        RunLoop.main.add(t, forMode: .common)
        timer = t
        onChange?()
    }

    func stop() {
        player?.pause()
        player = nil
        timer?.invalidate()
        timer = nil
        title = ""
        onChange?()
    }

    // What is on: the Icecast server's own status.
    private func ask() {
        var req = URLRequest(url: Self.status)
        req.timeoutInterval = 10
        req.cachePolicy = .reloadIgnoringLocalCacheData
        URLSession.shared.dataTask(with: req) { [weak self] data, _, _ in
            guard let data, let j = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let stats = j["icestats"] as? [String: Any] else { return }
            let source = (stats["source"] as? [String: Any]) ?? ((stats["source"] as? [[String: Any]])?.first ?? [:])
            var now = (source["title"] as? String) ?? ""
            if let cut = now.range(of: " - "), now.lowercased().hasPrefix("kpbj") { now = String(now[cut.upperBound...]) }
            DispatchQueue.main.async {
                guard let self, self.playing else { return }
                self.title = now
                self.onChange?()
            }
        }.resume()
    }
}
