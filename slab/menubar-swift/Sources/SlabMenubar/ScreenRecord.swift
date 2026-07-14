// ScreenRecord.swift — screen VIDEO capture, the moving-picture sibling of FrameCapture.
//
// FrameCapture grabs one still (SCScreenshotManager). This grabs a clip: an
// SCStream fed straight into SCRecordingOutput, which hardware-encodes to an
// .mp4 on disk without a single frame ever crossing into our process. That is
// the whole reason to do it here rather than in a shell tool — no ffmpeg screen
// grab, no `screencapture` subprocess, no launchd throttle, no dropped frames.
//
// It lives INSIDE the menubar app for the same reason FrameCapture does: this
// app already holds the Screen Recording (TCC) grant and runs in the GUI
// session, so it can reach the WindowServer. An ssh shell cannot.
//
// Handshake mirrors `frame` exactly, so the same ssh transport works fleet-wide:
//   controller → writes reel.req  (JSON: {action:"start"|"stop", …})
//   menubar    → writes reel.state (JSON status), reel.out.mp4, touches reel.done
//
// Cursor: DEFAULT OFF. Tutorials draw their own cursor (puppet's virtual pointer
// overlay) — eased, click-rippled, and legible at 1x. The real macOS cursor is
// jittery, tiny, and teleports when driven synthetically, which reads as broken.
// Pass cursor:true when filming a human actually driving.

import Foundation
import AVFoundation
import ScreenCaptureKit

@available(macOS 15.0, *)
final class ScreenRecord: NSObject, SCStreamDelegate, SCRecordingOutputDelegate {
    static let shared = ScreenRecord()

    private let fm = FileManager.default
    private let queue = DispatchQueue(label: "computer.slab.menubar.reel")
    private var timer: DispatchSourceTimer?

    private var stream: SCStream?
    private var output: SCRecordingOutput?
    private var outURL: URL?
    private var startedAt: Date?
    private var lastError: String?
    /// True only while we are deliberately tearing the stream down, so the
    /// delegate can tell our own stop apart from a real mid-take failure.
    private var stopping = false

    // MARK: - request loop

    func start() {
        let dir = (Paths.reelReq as NSString).deletingLastPathComponent
        try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
        let t = DispatchSource.makeTimerSource(queue: queue)
        t.schedule(deadline: .now() + .milliseconds(50), repeating: .milliseconds(30))
        t.setEventHandler { [weak self] in self?.tick() }
        t.resume()
        timer = t
        // No ScreenCaptureKit call here — permissions stay lazy, same as FrameCapture.
    }

    private func tick() {
        guard fm.fileExists(atPath: Paths.reelReq) else { return }
        let raw = (try? Data(contentsOf: URL(fileURLWithPath: Paths.reelReq))) ?? Data()
        try? fm.removeItem(atPath: Paths.reelReq)
        try? fm.removeItem(atPath: Paths.reelDone)

        let req = (try? JSONSerialization.jsonObject(with: raw)) as? [String: Any] ?? [:]
        switch req["action"] as? String ?? "" {
        case "start":  beginRecording(req)
        case "stop":   endRecording()
        case "status": writeState(); markDone()
        default:       lastError = "unknown action"; writeState(); markDone()
        }
    }

    private func markDone() { fm.createFile(atPath: Paths.reelDone, contents: nil) }

    // MARK: - start

    private func beginRecording(_ req: [String: Any]) {
        guard stream == nil else {
            lastError = "already recording"; writeState(); markDone(); return
        }
        lastError = nil
        stopping = false

        let path = req["out"] as? String ?? Paths.reelOutMp4
        let url = URL(fileURLWithPath: path)
        try? fm.removeItem(at: url)  // AVAssetWriter refuses to overwrite

        let fps = req["fps"] as? Int ?? 60
        let showCursor = req["cursor"] as? Bool ?? false
        let windowMatch = req["window"] as? String
        let maxDim = CGFloat(req["maxDim"] as? Int ?? 2560)

        let sem = DispatchSemaphore(value: 0)
        Task {
            defer { sem.signal() }
            do {
                let filter = try await buildFilter(windowMatch: windowMatch)
                let cfg = SCStreamConfiguration()
                let (w, h) = Self.encodeSize(
                    points: filter.contentRect.size,
                    scale: CGFloat(filter.pointPixelScale),
                    maxLongEdge: maxDim)
                cfg.width = w
                cfg.height = h
                cfg.scalesToFit = true
                cfg.showsCursor = showCursor
                cfg.capturesAudio = false
                cfg.minimumFrameInterval = CMTime(value: 1, timescale: CMTimeScale(fps))
                cfg.queueDepth = 8
                cfg.captureResolution = .best

                let rcfg = SCRecordingOutputConfiguration()
                rcfg.outputURL = url
                rcfg.outputFileType = .mp4
                rcfg.videoCodecType = .h264

                let s = SCStream(filter: filter, configuration: cfg, delegate: self)
                let o = SCRecordingOutput(configuration: rcfg, delegate: self)
                try s.addRecordingOutput(o)
                try await s.startCapture()

                self.stream = s
                self.output = o
                self.outURL = url
                self.startedAt = Date()
            } catch {
                self.lastError = "\(error)"
                self.stream = nil
                self.output = nil
            }
        }
        sem.wait()
        writeState()
        markDone()
    }

    /// Pick the encoded frame size.
    ///
    /// The naive answer — points × backing scale — is a trap. A 2× Retina
    /// display hands you a 4816-wide stream, and VideoToolbox's hardware H.264
    /// encoder tops out around 4096: it accepts the stream, encodes a second or
    /// so, then drops the connection mid-take ("application connection being
    /// interrupted") and leaves a headless mp4. So clamp the long edge.
    ///
    /// 2560 is the default because it is comfortably under the ceiling while
    /// still oversampling a 1920-wide deliverable — downscaling in the final
    /// ffmpeg pass is what keeps text crisp. Dimensions are rounded down to even
    /// (h264 rejects odd) via a truncating divide.
    static func encodeSize(points: CGSize, scale: CGFloat,
                           maxLongEdge: CGFloat) -> (Int, Int) {
        var w = points.width * scale
        var h = points.height * scale
        let long = max(w, h)
        if long > maxLongEdge {
            let k = maxLongEdge / long
            w *= k
            h *= k
        }
        return (Int(w / 2) * 2, Int(h / 2) * 2)
    }

    /// A window filter when we can name the window, else the main display.
    /// Window capture is what a tutorial wants: it crops to the app, follows it
    /// if it moves, and keeps the desktop/menubar/other apps out of frame.
    private func buildFilter(windowMatch: String?) async throws -> SCContentFilter {
        let content = try await SCShareableContent.excludingDesktopWindows(
            false, onScreenWindowsOnly: true)

        if let match = windowMatch, !match.isEmpty {
            let needle = match.lowercased()
            // Prefer the largest match — apps scatter tiny helper windows
            // (tooltips, panels) that also carry the app name.
            let hit = content.windows
                .filter { w in
                    let title = (w.title ?? "").lowercased()
                    let app = (w.owningApplication?.applicationName ?? "").lowercased()
                    return title.contains(needle) || app.contains(needle)
                }
                .filter { $0.frame.width > 200 && $0.frame.height > 200 }
                .max { $0.frame.width * $0.frame.height < $1.frame.width * $1.frame.height }
            guard let win = hit else {
                throw NSError(domain: "reel", code: 2, userInfo: [
                    NSLocalizedDescriptionKey: "no window matching \"\(match)\""])
            }
            return SCContentFilter(desktopIndependentWindow: win)
        }

        guard let display = content.displays.first else {
            throw NSError(domain: "reel", code: 3, userInfo: [
                NSLocalizedDescriptionKey: "no display"])
        }
        // Same belt-and-suspenders as FrameCapture: never film our own overlays.
        let myBundle = Bundle.main.bundleIdentifier
        let exclude = content.windows.filter {
            $0.owningApplication?.bundleIdentifier == myBundle
        }
        return SCContentFilter(display: display, excludingWindows: exclude)
    }

    // MARK: - stop

    private func endRecording() {
        guard let s = stream else {
            lastError = "not recording"; writeState(); markDone(); return
        }
        // Order matters, and the wrong order looks like a hardware failure.
        // stopCapture() FIRST: that is what finalizes the recording output and
        // flushes the moov atom. Pulling the recording output off a live stream
        // instead makes SCK tear the connection down underneath the muxer and
        // report "application connection being interrupted" — an alarming error
        // for what is actually a clean, complete file. We also arm `stopping`
        // so the delegate ignores the interruption we ourselves caused.
        stopping = true
        lastError = nil
        let sem = DispatchSemaphore(value: 0)
        Task {
            defer { sem.signal() }
            do {
                try await s.stopCapture()
            } catch {
                self.lastError = "\(error)"
            }
            self.stream = nil
            self.output = nil
        }
        sem.wait()
        // `stopping` deliberately stays armed until the next beginRecording.
        // The delegate's callback lands on this same serial queue, which we have
        // been blocking — so it only runs AFTER we return. Disarming here would
        // let that late callback through and report a failure for a take that
        // finished perfectly.

        // stopCapture returns before the muxer has necessarily flushed the moov
        // atom. A truncated mp4 is worse than a slow one — wait for the file to
        // stop growing before we say done, so the caller never reads a partial.
        if let url = outURL { waitForFlush(url) }
        startedAt = nil
        writeState()
        markDone()
    }

    private func fileSize(_ path: String) -> UInt64 {
        let attrs = try? fm.attributesOfItem(atPath: path)
        return (attrs?[.size] as? NSNumber)?.uint64Value ?? 0
    }

    private func waitForFlush(_ url: URL) {
        var last: UInt64 = 0
        var stable = 0
        for _ in 0..<100 {  // ~5s ceiling
            let now = fileSize(url.path)
            if now > 0 && now == last {
                stable += 1
                if stable >= 3 { return }
            } else {
                stable = 0
            }
            last = now
            Thread.sleep(forTimeInterval: 0.05)
        }
    }

    // MARK: - state

    private func writeState() {
        var st: [String: Any] = ["recording": stream != nil]
        if let a = startedAt { st["since"] = a.timeIntervalSince1970 }
        if let u = outURL {
            st["path"] = u.path
            st["bytes"] = fileSize(u.path)
        }
        if let e = lastError { st["error"] = e }
        let data = (try? JSONSerialization.data(withJSONObject: st, options: [.prettyPrinted])) ?? Data()
        try? data.write(to: URL(fileURLWithPath: Paths.reelState))
    }

    // MARK: - delegates

    func stream(_ stream: SCStream, didStopWithError error: Error) {
        queue.async {
            guard !self.stopping else { return }  // our own stopCapture — not a failure
            self.lastError = "stream stopped: \(error.localizedDescription)"
            self.stream = nil
            self.output = nil
            self.startedAt = nil
            self.writeState()
        }
    }

    func recordingOutput(_ recordingOutput: SCRecordingOutput,
                         didFailWithError error: Error) {
        queue.async {
            self.lastError = "recording failed: \(error.localizedDescription)"
            self.writeState()
        }
    }
}
