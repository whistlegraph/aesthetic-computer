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

    // Transient overlay windows we draw (capture flash, OCR boxes). We exclude
    // them from the screen capture by windowID so they never appear in a frame
    // — that's the "doesn't interfere" guarantee. (The badge etc. still show.)
    // Capture at this multiple of the display's point size — 2x makes small,
    // dense text (terminals) physically larger in the buffer, pushing it over
    // Vision's recognition threshold. The OCR scale uses the same factor so
    // box coords still map back to screen points.
    private let captureScale: Double = 1.0
    private let overlayLock = NSLock()
    private var overlayWindowIDs = Set<Int>()
    private var ocrOverlayWindow: NSWindow?
    private func registerOverlay(_ w: NSWindow) {
        overlayLock.lock(); overlayWindowIDs.insert(w.windowNumber); overlayLock.unlock()
    }
    private func unregisterOverlay(_ w: NSWindow) {
        overlayLock.lock(); overlayWindowIDs.remove(w.windowNumber); overlayLock.unlock()
    }

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
        var cursorOverride: CGPoint?
        if let token = mode.split(separator: " ").first(where: { $0.hasPrefix("cursor=") }) {
            let xy = token.dropFirst("cursor=".count).split(separator: ",").compactMap { Double($0) }
            if xy.count == 2 { cursorOverride = CGPoint(x: xy[0], y: xy[1]) }
        }
        produce(noOCR: mode.contains("noocr"), fast: mode.contains("fast"),
                virtualCursor: mode.contains("cursor"), cursorOverride: cursorOverride)
        fm.createFile(atPath: Paths.frameDone, contents: nil)
    }

    // A subtle whole-display flash, fired AFTER the pixels are grabbed so it
    // never lands in the capture — just end-user awareness that a frame was
    // snapped. Runs on the main thread, click-through, brief and low-alpha; the
    // capture/OCR pipeline keeps going on its own queue meanwhile.
    private func flashCaptureIndicator() {
        DispatchQueue.main.async {
            for screen in NSScreen.screens {
                let win = NSWindow(contentRect: screen.frame, styleMask: .borderless,
                                   backing: .buffered, defer: false)
                win.isOpaque = false
                win.backgroundColor = .clear
                win.level = .screenSaver
                win.ignoresMouseEvents = true
                win.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]
                let view = NSView(frame: NSRect(origin: .zero, size: screen.frame.size))
                view.wantsLayer = true
                // System accent color so the whole-display pulse matches the
                // machine's chosen tint rather than a flat white wash.
                view.layer?.backgroundColor = NSColor.controlAccentColor.cgColor
                win.contentView = view
                win.alphaValue = 0.0
                win.orderFrontRegardless()
                self.registerOverlay(win)
                NSAnimationContext.runAnimationGroup({ ctx in
                    ctx.duration = 0.06
                    win.animator().alphaValue = 0.22
                }, completionHandler: {
                    NSAnimationContext.runAnimationGroup({ ctx in
                        ctx.duration = 0.34
                        win.animator().alphaValue = 0.0
                    }, completionHandler: { self.unregisterOverlay(win); win.orderOut(nil) })
                })
            }
        }
    }

    // Draw the whole-screen OCR boxes as a brief screen-wide overlay, so a
    // watcher sees what was read across the ENTIRE display — not just inside a
    // browser window (puppet's page-side scan). Click-through, excluded from
    // captures by windowID, holds ~7s then fades. Boxes are points/top-left
    // (from ocr()); CALayer is bottom-left, so Y flips against screen height.
    private func showOcrOverlay(_ boxes: [[String: Any]]) {
        guard !boxes.isEmpty else { return }
        DispatchQueue.main.async {
            guard let screen = NSScreen.main else { return }
            if let old = self.ocrOverlayWindow { self.unregisterOverlay(old); old.orderOut(nil) }
            let H = screen.frame.height
            let win = NSWindow(contentRect: screen.frame, styleMask: .borderless,
                               backing: .buffered, defer: false)
            win.isOpaque = false
            win.backgroundColor = .clear
            win.level = .screenSaver
            win.ignoresMouseEvents = true
            win.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]
            let view = NSView(frame: NSRect(origin: .zero, size: screen.frame.size))
            view.wantsLayer = true
            if let root = view.layer {
                for b in boxes {
                    guard let r = b["r"] as? [Int], r.count == 4 else { continue }
                    let box = CALayer()
                    box.frame = CGRect(x: CGFloat(r[0]), y: H - CGFloat(r[1]) - CGFloat(r[3]),
                                       width: CGFloat(r[2]), height: CGFloat(r[3]))
                    // A random hue per box, semi-transparent fill — so the whole
                    // read is vivid and every box stands out against the others.
                    let c = NSColor(hue: .random(in: 0...1), saturation: 0.8, brightness: 1.0, alpha: 1.0)
                    box.backgroundColor = c.withAlphaComponent(0.28).cgColor
                    box.borderColor = c.withAlphaComponent(0.95).cgColor
                    box.borderWidth = 1.2
                    box.cornerRadius = 2
                    root.addSublayer(box)
                }
            }
            win.contentView = view
            win.orderFrontRegardless()
            self.registerOverlay(win)
            self.ocrOverlayWindow = win
            DispatchQueue.main.asyncAfter(deadline: .now() + 7.0) {
                guard self.ocrOverlayWindow === win else { return }
                NSAnimationContext.runAnimationGroup({ ctx in
                    ctx.duration = 0.4
                    win.animator().alphaValue = 0.0
                }, completionHandler: {
                    self.unregisterOverlay(win); win.orderOut(nil); self.ocrOverlayWindow = nil
                })
            }
        }
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
            // GUARANTEE we capture UNDER everything this app draws. Belt: any
            // window we own (flash, OCR overlay, badge, previews) by bundle id.
            // Suspenders: the explicitly-tracked overlay window ids, in case a
            // window's owning app is momentarily unresolved. A frame is always
            // the machine's real content beneath our overlays — never them.
            let myBundle = Bundle.main.bundleIdentifier
            let exclude = content.windows.filter { w in
                if w.owningApplication?.bundleIdentifier == myBundle { return true }
                self.overlayLock.lock(); defer { self.overlayLock.unlock() }
                return self.overlayWindowIDs.contains(Int(w.windowID))
            }
            let filter = SCContentFilter(display: display, excludingWindows: exclude)
            let cfg = SCStreamConfiguration()
            cfg.width = Int(Double(display.width) * self.captureScale)
            cfg.height = Int(Double(display.height) * self.captureScale)
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
        req.minimumTextHeight = 0  // don't skip small/dense text (e.g. terminal monospace)
        if #available(macOS 13.0, *) { req.revision = VNRecognizeTextRequestRevision3 }
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

    // Fetch several attributes in ONE IPC round-trip instead of one call each.
    // The old walk did ~6 `AXUIElementCopyAttributeValue` calls per node (role,
    // children, title, value, desc, position, size) — each a synchronous Mach
    // round-trip into the target app's main thread, so a 300-element tree cost
    // ~2000 IPCs. `AXUIElementCopyMultipleAttributeValues` collapses that to one
    // call per node. Returns values parallel to `attrs`; a failed slot comes
    // back as an AXValue of type .axError, mapped to nil here.
    private let AX_ATTRS = ["AXRole", "AXChildren", "AXTitle", "AXValue", "AXDescription", "AXPosition", "AXSize"]
    private func axBatch(_ el: AXUIElement) -> [AnyObject?] {
        var raw: CFArray?
        let err = AXUIElementCopyMultipleAttributeValues(
            el, AX_ATTRS as CFArray, AXCopyMultipleAttributeOptions(rawValue: 0), &raw)
        guard err == .success, let arr = raw as? [AnyObject] else {
            return Array(repeating: nil, count: AX_ATTRS.count)
        }
        return arr.map { v -> AnyObject? in
            if CFGetTypeID(v) == AXValueGetTypeID(), AXValueGetType((v as! AXValue)) == .axError { return nil }
            if v is NSNull { return nil }
            return v
        }
    }
    private func axCGPoint(_ v: AnyObject?) -> (CGFloat, CGFloat)? {
        guard let v = v, CFGetTypeID(v) == AXValueGetTypeID() else { return nil }
        var p = CGPoint.zero; AXValueGetValue(v as! AXValue, .cgPoint, &p); return (p.x, p.y)
    }
    private func axCGSize(_ v: AnyObject?) -> (CGFloat, CGFloat)? {
        guard let v = v, CFGetTypeID(v) == AXValueGetTypeID() else { return nil }
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
            let v = axBatch(el) // role, children, title, value, desc, position, size
            let role = (v[0] as? String) ?? ""
            if want.contains(role),
               let (x, y) = axCGPoint(v[5]), let (w, h) = axCGSize(v[6]), w > 0, h > 0 {
                let title = (v[2] as? String) ?? (v[3] as? String) ?? (v[4] as? String) ?? ""
                var acts: CFArray?
                AXUIElementCopyActionNames(el, &acts) // only for matched elements
                out.append(["role": role, "title": String(title.prefix(80)),
                    "cx": Int(x + w / 2), "cy": Int(y + h / 2),
                    "r": [Int(x), Int(y), Int(w), Int(h)],
                    "actions": (acts as? [String]) ?? []])
            }
            if let kids = v[1] as? [AXUIElement] {
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

    private func thumbJPEG(_ cg: CGImage, maxWidth: Int, cursor: CGPoint? = nil) -> Data? {
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
        if let cursor = cursor, let screen = NSScreen.main {
            let x = cursor.x * CGFloat(w) / screen.frame.width
            let y = CGFloat(h) - cursor.y * CGFloat(h) / screen.frame.height
            let radius = max(9.0, CGFloat(w) / 110.0)
            let marker = CGRect(x: x - radius, y: y - radius,
                              width: radius * 2, height: radius * 2)
            ctx.cgContext.setFillColor(NSColor.systemPink.withAlphaComponent(0.78).cgColor)
            ctx.cgContext.fillEllipse(in: marker)
            ctx.cgContext.setStrokeColor(NSColor.white.cgColor)
            ctx.cgContext.setLineWidth(max(2.0, radius / 5.0))
            ctx.cgContext.strokeEllipse(in: marker)
            ctx.cgContext.move(to: CGPoint(x: x - radius * 1.5, y: y))
            ctx.cgContext.addLine(to: CGPoint(x: x + radius * 1.5, y: y))
            ctx.cgContext.move(to: CGPoint(x: x, y: y - radius * 1.5))
            ctx.cgContext.addLine(to: CGPoint(x: x, y: y + radius * 1.5))
            ctx.cgContext.strokePath()
        }
        ctx.flushGraphics()
        NSGraphicsContext.restoreGraphicsState()
        return rep.representation(using: .jpeg, properties: [.compressionFactor: 0.6])
    }

    // MARK: - assemble + write the envelope

    private func produce(noOCR: Bool, fast: Bool = false,
                         virtualCursor: Bool = false, cursorOverride: CGPoint? = nil) {
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
        t = nowNs(); let cg = captureDisplay(); tm["capture"] = msSince(t)
        if cg != nil { flashCaptureIndicator() }  // subtle post-capture awareness flash
        // The JPEG ships as RAW BYTES in a sidecar file (frame.out.jpg), not
        // base64 in the JSON — base64 inflates the payload +33% and burns
        // encode/decode CPU. The transport length-prefixes the two. Written
        // BEFORE the JSON + done marker so a reader that sees `done` has both.
        var jpgBytes = 0
        if let cg = cg {
            if noOCR {
                env["ocr"] = []
            } else {
                t = nowNs(); let boxes = ocr(cg, scale: captureScale, fast: fast); tm["ocr"] = msSince(t)
                env["ocr"] = boxes
                showOcrOverlay(boxes)
            }
            t = nowNs()
            let cursorMeta = (mt["cursor"] as? [String: Int]).map {
                CGPoint(x: $0["x"] ?? 0, y: $0["y"] ?? 0)
            }
            let marker = virtualCursor ? (cursorOverride ?? cursorMeta) : nil
            let jpg = thumbJPEG(cg, maxWidth: 1568, cursor: marker) ?? Data()
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
