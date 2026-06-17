import AppKit
import ApplicationServices
import CoreGraphics
import ScreenCaptureKit
import Vision

/// A "frame" of this machine for fleet automation: pixels (a downscaled
/// thumbnail) + OCR'd text with click coordinates + the Accessibility element
/// tree + window/cursor/frontmost state, packed into one JSON envelope. The
/// `frame` CLI on the controlling Mac pokes a request file over SSH; this
/// watcher (running inside the menubar app, which already holds Accessibility
/// trust and lives in the GUI session) produces the envelope. It is the
/// native-capture complement to `puppet` (which only sees the browser via CDP):
/// `frame` observes, `puppet` acts.
///
/// PERMISSIONS ARE LAZY. `start()` only spins a file-watch timer — it never
/// touches ScreenCaptureKit, so no Screen Recording prompt appears at launch.
/// The first *actual* frame request is what calls ScreenCaptureKit, which is
/// when macOS surfaces the grant (auto-approves on some hosts, prompts on
/// others). If capture fails for lack of permission we say so in the envelope
/// (`capture: "permission_needed"`) so the CLI can guide the one-time grant.
/// Accessibility is already granted to the app, so the AX tree is free.
final class FrameCapture {
    static let shared = FrameCapture()
    private let queue = DispatchQueue(label: "computer.slab.frame", qos: .userInitiated)
    private var timer: DispatchSourceTimer?
    private let fm = FileManager.default

    func start() {
        let dir = (Paths.frameReq as NSString).deletingLastPathComponent
        try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
        let t = DispatchSource.makeTimerSource(queue: queue)
        t.schedule(deadline: .now() + .milliseconds(50), repeating: .milliseconds(30))
        t.setEventHandler { [weak self] in self?.tick() }
        t.resume()
        timer = t
        // No ScreenCaptureKit call here — see the type doc: permissions are lazy.
    }

    private func tick() {
        guard fm.fileExists(atPath: Paths.frameReq) else { return }
        let mode = (try? String(contentsOfFile: Paths.frameReq, encoding: .utf8)) ?? ""
        try? fm.removeItem(atPath: Paths.frameReq)
        try? fm.removeItem(atPath: Paths.frameDone)
        produce(noOCR: mode.contains("noocr"), fast: mode.contains("fast"))
        fm.createFile(atPath: Paths.frameDone, contents: nil)
    }

    // MARK: - capture (in-process; no screencapture subprocess → no launchd throttle)

    private func captureDisplay() -> CGImage? {
        guard #available(macOS 14.0, *) else { return nil }
        let sem = DispatchSemaphore(value: 0)
        var img: CGImage?
        Task {
            defer { sem.signal() }
            guard let content = try? await SCShareableContent.excludingDesktopWindows(
                    false, onScreenWindowsOnly: true),
                  let display = content.displays.first else { return }
            let filter = SCContentFilter(display: display, excludingWindows: [])
            let cfg = SCStreamConfiguration()
            cfg.width = display.width
            cfg.height = display.height
            cfg.showsCursor = true
            img = try? await SCScreenshotManager.captureImage(contentFilter: filter, configuration: cfg)
        }
        sem.wait()
        return img
    }

    // MARK: - OCR (text + click-center coords, in logical points)

    private func ocr(_ cg: CGImage, scale: Double, fast: Bool = false) -> [[String: Any]] {
        let req = VNRecognizeTextRequest()
        req.recognitionLevel = fast ? .fast : .accurate
        req.usesLanguageCorrection = false
        req.recognitionLanguages = ["en-US"]
        try? VNImageRequestHandler(cgImage: cg, options: [:]).perform([req])
        let W = Double(cg.width), H = Double(cg.height)
        var out: [[String: Any]] = []
        for obs in (req.results ?? []) {
            guard let c = obs.topCandidates(1).first else { continue }
            let bb = obs.boundingBox
            let x = bb.minX * W / scale, y = (1 - bb.maxY) * H / scale
            let w = bb.width * W / scale, h = bb.height * H / scale
            out.append(["t": c.string, "cx": Int(x + w / 2), "cy": Int(y + h / 2),
                        "r": [Int(x), Int(y), Int(w), Int(h)]])
        }
        return out
    }

    // MARK: - Accessibility element tree of the frontmost app (trust already held)

    private func axAttr(_ el: AXUIElement, _ k: String) -> AnyObject? {
        var v: AnyObject?
        return AXUIElementCopyAttributeValue(el, k as CFString, &v) == .success ? v : nil
    }
    private func axPoint(_ el: AXUIElement) -> (CGFloat, CGFloat)? {
        guard let v = axAttr(el, "AXPosition") else { return nil }
        var p = CGPoint.zero; AXValueGetValue(v as! AXValue, .cgPoint, &p); return (p.x, p.y)
    }
    private func axSize(_ el: AXUIElement) -> (CGFloat, CGFloat)? {
        guard let v = axAttr(el, "AXSize") else { return nil }
        var s = CGSize.zero; AXValueGetValue(v as! AXValue, .cgSize, &s); return (s.width, s.height)
    }
    private func axTree() -> [String: Any] {
        guard AXIsProcessTrusted() else { return ["trusted": false, "elements": []] }
        guard let front = NSWorkspace.shared.frontmostApplication else {
            return ["trusted": true, "elements": []]
        }
        let app = AXUIElementCreateApplication(front.processIdentifier)
        let want: Set<String> = ["AXButton", "AXMenuItem", "AXTextField", "AXTextArea",
            "AXCheckBox", "AXRadioButton", "AXLink", "AXPopUpButton", "AXMenuButton",
            "AXSlider", "AXComboBox", "AXToggle"]
        var out: [[String: Any]] = []
        var count = 0
        func walk(_ el: AXUIElement, _ depth: Int) {
            if count > 2000 || depth > 28 { return }
            count += 1
            let role = (axAttr(el, "AXRole") as? String) ?? ""
            if want.contains(role), let (x, y) = axPoint(el), let (w, h) = axSize(el), w > 0, h > 0 {
                let title = (axAttr(el, "AXTitle") as? String)
                    ?? (axAttr(el, "AXValue") as? String)
                    ?? (axAttr(el, "AXDescription") as? String) ?? ""
                var acts: CFArray?
                AXUIElementCopyActionNames(el, &acts)
                out.append(["role": role, "title": String(title.prefix(80)),
                    "cx": Int(x + w / 2), "cy": Int(y + h / 2),
                    "r": [Int(x), Int(y), Int(w), Int(h)],
                    "actions": (acts as? [String]) ?? []])
            }
            if let kids = axAttr(el, "AXChildren") as? [AXUIElement] {
                for k in kids { walk(k, depth + 1) }
            }
        }
        walk(app, 0)
        return ["trusted": true, "app": front.localizedName ?? "?", "elements": out]
    }

    // MARK: - meta (screen geometry/scale, cursor, frontmost, windows)

    private func meta() -> [String: Any] {
        var r: [String: Any] = [:]
        let scale = NSScreen.main?.backingScaleFactor ?? 1
        if let s = NSScreen.main {
            r["screen"] = ["w": Int(s.frame.width), "h": Int(s.frame.height), "scale": scale]
        }
        let m = NSEvent.mouseLocation
        let sh = NSScreen.main?.frame.height ?? 0
        r["cursor"] = ["x": Int(m.x), "y": Int(sh - m.y)]   // flip to top-left origin
        if let f = NSWorkspace.shared.frontmostApplication {
            r["frontmost"] = ["app": f.localizedName ?? "?",
                              "bundle": f.bundleIdentifier ?? "?", "pid": f.processIdentifier]
        }
        var wins: [[String: Any]] = []
        if let info = CGWindowListCopyWindowInfo(
                [.optionOnScreenOnly, .excludeDesktopElements], kCGNullWindowID) as? [[String: Any]] {
            for w in info {
                guard let layer = w[kCGWindowLayer as String] as? Int, layer == 0 else { continue }
                let b = w[kCGWindowBounds as String] as? [String: Any] ?? [:]
                wins.append(["app": w[kCGWindowOwnerName as String] as? String ?? "?",
                             "title": w[kCGWindowName as String] as? String ?? "",
                             "x": b["X"] ?? 0, "y": b["Y"] ?? 0,
                             "w": b["Width"] ?? 0, "h": b["Height"] ?? 0])
            }
        }
        r["windows"] = wins
        return r
    }

    // MARK: - thumbnail (downscale to 1568px — what a vision model downsamples to anyway)

    private func thumbJPEG(_ cg: CGImage, maxWidth: Int) -> Data? {
        let s = Double(maxWidth) / Double(cg.width)
        let w = maxWidth, h = max(1, Int(Double(cg.height) * s))
        guard let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: w, pixelsHigh: h,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0),
            let ctx = NSGraphicsContext(bitmapImageRep: rep) else { return nil }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = ctx
        ctx.cgContext.interpolationQuality = .high
        ctx.cgContext.draw(cg, in: CGRect(x: 0, y: 0, width: w, height: h))
        ctx.flushGraphics()
        NSGraphicsContext.restoreGraphicsState()
        return rep.representation(using: .jpeg, properties: [.compressionFactor: 0.6])
    }

    // MARK: - assemble + write the envelope

    private func produce(noOCR: Bool, fast: Bool = false) {
        func nowNs() -> UInt64 { DispatchTime.now().uptimeNanoseconds }
        func msSince(_ t: UInt64) -> Double { (Double(nowNs() - t) / 1e6 * 10).rounded() / 10 }
        var env: [String: Any] = [:]
        var tm: [String: Double] = [:]

        // The AX walk is independent of the screenshot, so run it CONCURRENTLY
        // with capture+OCR: wall-clock becomes max(ax, capture+ocr) instead of
        // the sum. AX is ~150ms and capture+OCR ~280ms, so this hides the AX
        // cost entirely. (AXUIElement calls are fine off the main thread.)
        var axResult: [String: Any] = [:]
        var axMs: Double = 0
        let grp = DispatchGroup()
        grp.enter()
        let axStart = nowNs()
        DispatchQueue.global(qos: .userInitiated).async {
            axResult = self.axTree()
            axMs = msSince(axStart)
            grp.leave()
        }

        var t = nowNs(); let mt = meta(); tm["meta"] = msSince(t)
        env["meta"] = mt
        let scale = ((mt["screen"] as? [String: Any])?["scale"] as? CGFloat).map(Double.init) ?? 1.0
        t = nowNs(); let cg = captureDisplay(); tm["capture"] = msSince(t)
        // The JPEG ships as RAW BYTES in a sidecar file (frame.out.jpg), not
        // base64 in the JSON — base64 inflates the payload +33% and burns
        // encode/decode CPU. The transport length-prefixes the two. Written
        // BEFORE the JSON + done marker so a reader that sees `done` has both.
        var jpgBytes = 0
        if let cg = cg {
            if noOCR {
                env["ocr"] = []
            } else {
                t = nowNs(); env["ocr"] = ocr(cg, scale: scale, fast: fast); tm["ocr"] = msSince(t)
            }
            t = nowNs()
            let jpg = thumbJPEG(cg, maxWidth: 1568) ?? Data()
            try? jpg.write(to: URL(fileURLWithPath: Paths.frameOutJpg))
            jpgBytes = jpg.count
            tm["thumb"] = msSince(t)
            env["capture"] = "ok"
        } else {
            env["ocr"] = []
            try? Data().write(to: URL(fileURLWithPath: Paths.frameOutJpg)) // truncate stale jpg
            // Either Screen Recording isn't granted yet, or this is < macOS 14.
            env["capture"] = "permission_needed"
            env["permission"] = "screen_recording"
        }
        env["thumb_bytes"] = jpgBytes
        grp.wait()                 // join the concurrent AX walk
        env["ax"] = axResult
        tm["ax"] = axMs
        tm["wall"] = (tm["meta"]! + tm["capture"]! + (tm["ocr"] ?? 0) + tm["thumb"]!)  // serial part; ax overlapped
        env["timings_ms"] = tm
        if let d = try? JSONSerialization.data(withJSONObject: env, options: []) {
            try? d.write(to: URL(fileURLWithPath: Paths.frameOut))
        }
    }
}
