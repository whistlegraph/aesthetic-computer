import AppKit

/// Desktop beat bus — Menu Band's hits, published for the Pals wallpaper.
///
/// The slab wallpaper (`toolchain/macos/blueberry-wallpaper`) swells on
/// `computer.aesthetic.slab.beat.changed` — `{bpm, beatAt, energy,
/// updatedAt}` — and reads `~/.local/share/slab/wallpaper/beat.json` on
/// launch. `beat-listener` derives that from a microphone; we know the hits
/// exactly, so every drum (and, more softly, every note) is a beat with
/// `beatAt` = now. The wallpaper's own envelope does the rest: peak on the
/// hit, settle in the gaps, go still four seconds after the last one. So the
/// desktop moves with the music whenever the jam wall is down.
///
/// The tempo is a running estimate from kick/snare/clap onsets so the
/// wallpaper's between-hit swells land on the grid; it only matters while
/// the player pauses, since a fresh hit always re-anchors the phase.
///
/// Alongside the beat rides the **vibe**: a color that drifts toward what is
/// being played (the jam wall's pitch-class rainbow for notes, its pad
/// palette for drums) and a `glow` that rises with how much is going on and
/// settles over a couple of seconds. The wallpaper tints its backdrop and
/// the Pals with the color and lights its corners with the glow.
final class MenuBandBeatBus {
    static let shared = MenuBandBeatBus()

    private static let note = Notification.Name("computer.aesthetic.slab.beat.changed")
    private static let file = NSString(
        string: "~/.local/share/slab/wallpaper/beat.json").expandingTildeInPath
    private let q = DispatchQueue(label: "computer.aestheticcomputer.menuband.beatbus", qos: .utility)
    private var onsets: [Double] = []
    private var bpm: Double = 120
    private var lastFileWrite: Double = 0
    private var fileDirReady = false
    /// Vibe color (sRGB 0–1), a moving average of what's played.
    private var vibe: (r: Double, g: Double, b: Double)?
    private var glow: Double = 0
    private var glowAt: Double = 0
    private static let glowTau = 1.0

    func drum(_ d: MenuBandPercussion.Drum, velocity: UInt8) {
        let now = Date().timeIntervalSince1970
        let anchors = d == .kick || d == .snare || d == .clap
        let color = JamWallView.drumColors[d.rawValue]
        q.async {
            // Drums color the vibe lightly — a kick-heavy groove warms it
            // toward white, a hat pattern cools it violet — notes carry it.
            self.blendVibe(color, weight: 0.2)
            self.publish(now: now, energy: Double(velocity) / 127, anchorsTempo: anchors)
        }
    }

    func note(_ midi: UInt8, velocity: UInt8) {
        let now = Date().timeIntervalSince1970
        let color = JamWallView.melodyColors[Int(midi) % 12]
        q.async {
            self.blendVibe(color, weight: 0.7)
            self.publish(now: now, energy: 0.45 * Double(velocity) / 127, anchorsTempo: false)
        }
    }

    private func blendVibe(_ color: NSColor, weight: Double) {
        let c = color.usingColorSpace(.sRGB) ?? color
        let r = Double(c.redComponent), g = Double(c.greenComponent), b = Double(c.blueComponent)
        guard let v = vibe else { vibe = (r, g, b); return }
        vibe = (v.r + (r - v.r) * weight, v.g + (g - v.g) * weight, v.b + (b - v.b) * weight)
    }

    private func publish(now: Double, energy: Double, anchorsTempo: Bool) {
        if anchorsTempo {
            onsets.append(now)
            onsets.removeAll { now - $0 > 6 }
            estimateTempo()
        }
        // Glow: decays since the last event, then each event lifts it by
        // its energy plus a little for density — a busy passage saturates,
        // a single note barely stirs it.
        if glowAt > 0 { glow *= exp(-(now - glowAt) / Self.glowTau) }
        glowAt = now
        glow = min(1, max(glow, energy * 0.85) + 0.15 + energy * 0.25)
        var info: [String: Any] = [
            "bpm": bpm, "beatAt": now,
            "energy": min(1, max(0, energy)), "updatedAt": now,
            "glow": glow,
        ]
        if let v = vibe {
            info["vibeRed"] = v.r
            info["vibeGreen"] = v.g
            info["vibeBlue"] = v.b
        }
        DistributedNotificationCenter.default().postNotificationName(
            Self.note, object: nil, userInfo: info, deliverImmediately: true)
        // The file is the wallpaper's launch-time fallback; twice a second is
        // plenty, and a sandboxed build simply can't write it (fine).
        guard now - lastFileWrite > 0.5 else { return }
        lastFileWrite = now
        if !fileDirReady {
            try? FileManager.default.createDirectory(
                atPath: (Self.file as NSString).deletingLastPathComponent,
                withIntermediateDirectories: true)
            fileDirReady = true
        }
        if let data = try? JSONSerialization.data(withJSONObject: info) {
            try? data.write(to: URL(fileURLWithPath: Self.file), options: .atomic)
        }
    }

    /// Median inter-onset interval, folded into 60–180 BPM. Needs three
    /// intervals before it moves off the previous estimate.
    private func estimateTempo() {
        guard onsets.count >= 4 else { return }
        var gaps: [Double] = []
        for i in 1..<onsets.count {
            let g = onsets[i] - onsets[i - 1]
            if g > 0.2, g < 2.0 { gaps.append(g) }
        }
        guard gaps.count >= 3 else { return }
        gaps.sort()
        var period = gaps[gaps.count / 2]
        while period < 60.0 / 180.0 { period *= 2 }
        while period > 60.0 / 60.0 { period /= 2 }
        bpm = 60.0 / period
    }
}
