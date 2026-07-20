// WaveformView.swift — an audio scrub player drawn as a waveform, with
// timestamped comment markers. Click/drag anywhere to seek; option-click
// (or the ＋ button in the controller) drops a comment at that instant.
// Sibling in spirit to clip-wizard's ScrubPlayerView, but audio-first:
// peaks are read off the file with AVAudioFile and the comment markers
// are the whole point.
import AppKit
import AVFoundation

protocol WaveformViewDelegate: AnyObject {
    func waveformRequestComment(at t: Double)   // option-click → new comment
    func waveformDidFinish()                     // track ended → auto-advance
    func waveformTick()                          // ~30fps → update time label
}

final class WaveformView: NSView, AVAudioPlayerDelegate {
    weak var delegate: WaveformViewDelegate?
    private var player: AVAudioPlayer?
    private var peaks: [Float] = []
    private var peaksToken = 0
    private var timer: Timer?
    private var preferredVolume: Float = 0.8

    var comments: [Comment] = [] { didSet { needsDisplay = true } }
    var duration: Double { player?.duration ?? 0 }
    var currentTime: Double { player?.currentTime ?? 0 }
    var isPlaying: Bool { player?.isPlaying ?? false }
    var volume: Float {
        get { player?.volume ?? preferredVolume }
        set {
            preferredVolume = max(0, min(1, newValue))
            player?.volume = preferredVolume
        }
    }

    override var isFlipped: Bool { true }

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        layer?.backgroundColor = NSColor(white: 0.07, alpha: 1).cgColor
        layer?.cornerRadius = 8
        layer?.masksToBounds = true
    }
    required init?(coder: NSCoder) { fatalError() }

    // ── load / transport ────────────────────────────────────────────────
    func load(url: URL) {
        stop()
        peaks = []
        player = try? AVAudioPlayer(contentsOf: url)
        player?.volume = preferredVolume
        player?.delegate = self
        player?.prepareToPlay()
        needsDisplay = true
        computePeaks(url: url)
    }

    func play() { player?.play(); startTimer() }
    func pause() { player?.pause(); stopTimer(); needsDisplay = true }
    func togglePlay() { if isPlaying { pause() } else { play() } }
    func seek(to t: Double) {
        player?.currentTime = max(0, min(duration, t))
        needsDisplay = true
        delegate?.waveformTick()
    }
    func stop() {
        stopTimer()
        player?.stop()
        player = nil
        needsDisplay = true
    }

    private func startTimer() {
        stopTimer()
        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            self?.needsDisplay = true
            self?.delegate?.waveformTick()
        }
    }
    private func stopTimer() { timer?.invalidate(); timer = nil }

    func audioPlayerDidFinishPlaying(_ p: AVAudioPlayer, successfully flag: Bool) {
        stopTimer()
        delegate?.waveformDidFinish()
    }

    // ── waveform peaks (read on a background queue) ──────────────────────
    private func computePeaks(url: URL) {
        peaksToken += 1
        let token = peaksToken
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            guard let file = try? AVAudioFile(forReading: url) else { return }
            let fmt = file.processingFormat
            let frames = AVAudioFrameCount(file.length)
            guard frames > 0,
                  let buf = AVAudioPCMBuffer(pcmFormat: fmt, frameCapacity: frames),
                  (try? file.read(into: buf)) != nil,
                  let ch = buf.floatChannelData else { return }
            let n = Int(buf.frameLength)
            let chans = Int(fmt.channelCount)
            let bins = 1000
            var out = [Float](repeating: 0, count: bins)
            let per = max(1, n / bins)
            for b in 0..<bins {
                let start = b * per
                let end = min(n, start + per)
                var peak: Float = 0
                var i = start
                while i < end {
                    var s: Float = 0
                    for c in 0..<chans { s += abs(ch[c][i]) }
                    s /= Float(chans)
                    if s > peak { peak = s }
                    i += 1
                }
                out[b] = peak
            }
            let mx = out.max() ?? 1
            if mx > 0 { for i in 0..<bins { out[i] = out[i] / mx } }
            DispatchQueue.main.async {
                guard let self, token == self.peaksToken else { return }
                self.peaks = out
                self.needsDisplay = true
            }
        }
    }

    // ── drawing ──────────────────────────────────────────────────────────
    override func draw(_ dirtyRect: NSRect) {
        let w = bounds.width, h = bounds.height, mid = h / 2
        let frac = duration > 0 ? CGFloat(currentTime / duration) : 0
        let progX = w * frac

        if peaks.isEmpty {
            let s = "loading…" as NSString
            let attr: [NSAttributedString.Key: Any] = [
                .foregroundColor: NSColor(white: 0.5, alpha: 1),
                .font: NSFont.systemFont(ofSize: 11)]
            s.draw(at: NSPoint(x: 10, y: mid - 7), withAttributes: attr)
        } else {
            let count = peaks.count
            let bw = w / CGFloat(count)
            for i in 0..<count {
                let x = CGFloat(i) * bw
                let amp = CGFloat(peaks[i]) * (mid - 4)
                let played = x <= progX
                (played ? NSColor.systemTeal : NSColor(white: 0.32, alpha: 1)).setFill()
                NSBezierPath(rect: NSRect(x: x, y: mid - amp, width: max(1, bw - 0.5), height: amp * 2)).fill()
            }
        }

        // playhead
        NSColor.systemYellow.setStroke()
        let head = NSBezierPath()
        head.move(to: NSPoint(x: progX, y: 0))
        head.line(to: NSPoint(x: progX, y: h))
        head.lineWidth = 1.5
        head.stroke()

        // comment markers — a line + a top triangle flag
        for c in comments {
            guard duration > 0 else { break }
            let x = w * CGFloat(c.t / duration)
            NSColor.systemOrange.withAlphaComponent(0.85).setStroke()
            let p = NSBezierPath()
            p.move(to: NSPoint(x: x, y: 0)); p.line(to: NSPoint(x: x, y: h)); p.lineWidth = 1
            p.stroke()
            NSColor.systemOrange.setFill()
            let tri = NSBezierPath()
            tri.move(to: NSPoint(x: x - 4, y: 0))
            tri.line(to: NSPoint(x: x + 4, y: 0))
            tri.line(to: NSPoint(x: x, y: 7))
            tri.close(); tri.fill()
        }
    }

    // ── interaction ──────────────────────────────────────────────────────
    private func timeAt(_ event: NSEvent) -> Double {
        let x = convert(event.locationInWindow, from: nil).x
        let frac = max(0, min(1, x / bounds.width))
        return Double(frac) * duration
    }

    override func mouseDown(with event: NSEvent) {
        guard duration > 0 else { return }
        let t = timeAt(event)
        if event.modifierFlags.contains(.option) {
            delegate?.waveformRequestComment(at: t)
        } else {
            seek(to: t)
        }
    }
    override func mouseDragged(with event: NSEvent) {
        guard duration > 0, !event.modifierFlags.contains(.option) else { return }
        seek(to: timeAt(event))
    }
}
