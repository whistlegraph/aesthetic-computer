import AVFoundation
import AppKit

/// Plays @jeffrey's voiced count-in numbers ("three / two / one") for the
/// record count-in. Clips are bundled mp3s (generated via /api/say in the
/// jeffrey PVC voice). Falls back to a soft system tick if a clip is missing.
final class CountInVoice {
    static let shared = CountInVoice()
    private var players: [String: AVAudioPlayer] = [:]

    /// `n` is "3" / "2" / "1".
    func play(_ n: String) {
        if let p = loadPlayer(n) { p.currentTime = 0; p.volume = 0.4; p.play() }
        else { NSSound(named: "Tink")?.play() }
    }

    private func loadPlayer(_ n: String) -> AVAudioPlayer? {
        if let p = players[n] { return p }
        guard let url = Bundle.appResources.url(forResource: "countin-\(n)", withExtension: "mp3"),
              let p = try? AVAudioPlayer(contentsOf: url) else { return nil }
        p.prepareToPlay()
        players[n] = p
        return p
    }
}
