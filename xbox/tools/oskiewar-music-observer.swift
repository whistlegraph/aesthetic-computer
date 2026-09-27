// Read Menu Band's existing cue notifications without controlling playback.
import Foundation
let center = DistributedNotificationCenter.default()
var observers: [NSObjectProtocol] = []
for action in ["play", "stop"] {
    observers.append(center.addObserver(forName: Notification.Name("computer.aestheticcomputer.menuband." + action), object: nil, queue: .main) { note in
        let info = note.userInfo as? [String: String] ?? [:]
        let allowed = ["title", "bpm", "startEpoch", "notes", "notes2", "notes3", "notes4", "preparedId", "lyrics", "face", "captionColor"]
        let payload: [String: Any] = ["action": action, "at": Date().timeIntervalSince1970,
            "info": info.filter { allowed.contains($0.key) }]
        if let data = try? JSONSerialization.data(withJSONObject: payload),
           let text = String(data: data, encoding: .utf8) {
            print(text)
            fflush(stdout)
        }
    })
}
RunLoop.main.run()
