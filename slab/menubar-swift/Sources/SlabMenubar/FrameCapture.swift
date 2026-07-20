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
    private var pendingClickWindows: [NSWindow] = []
    private var diffBaseline: (rect: CGRect, image: CGImage)?
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
        var crop: CGRect?
        if let token = mode.split(separator: " ").first(where: { $0.hasPrefix("crop=") }) {
            let v = token.dropFirst("crop=".count).split(separator: ",").compactMap { Double($0) }
            if v.count == 4 { crop = CGRect(x: v[0], y: v[1], width: v[2], height: v[3]) }
        }
        var pendingClickTarget: CGPoint?
        if let token = mode.split(separator: " ").first(where: { $0.hasPrefix("target=") }) {
            let xy = token.dropFirst("target=".count).split(separator: ",").compactMap { Double($0) }
            if xy.count == 2 { pendingClickTarget = CGPoint(x: xy[0], y: xy[1]) }
        }
        var approvedClick: (point: CGPoint, count: Int)?
        if let token = mode.split(separator: " ").first(where: { $0.hasPrefix("press=") }) {
            let values = token.dropFirst("press=".count).split(separator: ",").compactMap { Double($0) }
            if values.count >= 2 {
                approvedClick = (
                    CGPoint(x: values[0], y: values[1]),
                    values.count >= 3 ? max(1, min(3, Int(values[2]))) : 1
                )
            }
        }
        var approvedClickTitle: String?
        if let token = mode.split(separator: " ").first(where: { $0.hasPrefix("press-title=") }) {
            let encoded = String(token.dropFirst("press-title=".count))
            if let data = Data(base64Encoded: encoded) {
                approvedClickTitle = String(data: data, encoding: .utf8)
            }
        }
        if mode.split(separator: " ").contains("target-clear") {
            clearPendingClickTarget()
        }
        if let approvedClick {
            performApprovedClick(at: approvedClick.point, count: approvedClick.count,
                                 title: approvedClickTitle)
            if mode.split(separator: " ").contains("action-only") {
                writeActionAcknowledgement()
                fm.createFile(atPath: Paths.frameDone, contents: nil)
                return
            }
        }
        produce(noOCR: mode.contains("noocr"), fast: mode.contains("fast"),
                wholeScreen: mode.contains("screen"),
                virtualCursor: mode.contains("cursor"), cursorOverride: cursorOverride, crop: crop,
                saveBaseline: mode.contains("baseline"), includeDiff: mode.contains("diff"))
        if let pendingClickTarget { showPendingClickTarget(at: pendingClickTarget) }
        fm.createFile(atPath: Paths.frameDone, contents: nil)
    }

    /// Spotlight a proposed click without performing it. Accessibility expands
    /// the point to the whole control (button, field, link, etc.); when no
    /// bounded control can be resolved we fall back to a compact point target.
    /// The rest of the display is dimmed while the target outline pulses until
    /// a later `target-clear` request approves or rejects the staged action.
    /// The click-through overlay is excluded from captures, whose virtual
    /// cursor still records the exact staged coordinate.
    private func showPendingClickTarget(at point: CGPoint) {
        DispatchQueue.main.async {
            self.clearPendingClickTargetOnMain()
            guard let screen = NSScreen.main else { return }
            let displayHeight = screen.frame.height
            let resolved = self.pendingClickElementRect(at: point)
            let fallbackSize = CGSize(width: 64, height: 52)
            let targetInScreen: CGRect
            if let resolved {
                targetInScreen = CGRect(
                    x: resolved.minX - screen.frame.minX,
                    y: displayHeight - resolved.maxY,
                    width: resolved.width,
                    height: resolved.height
                )
            } else {
                targetInScreen = CGRect(
                    x: point.x - screen.frame.minX - fallbackSize.width / 2,
                    y: displayHeight - point.y - fallbackSize.height / 2,
                    width: fallbackSize.width,
                    height: fallbackSize.height
                )
            }
            let spotlight = targetInScreen
                .insetBy(dx: -7, dy: -7)
                .intersection(CGRect(origin: .zero, size: screen.frame.size))
            guard !spotlight.isNull, spotlight.width > 0, spotlight.height > 0 else { return }

            let win = NSWindow(contentRect: screen.frame,
                               styleMask: .borderless, backing: .buffered, defer: false)
            win.isOpaque = false
            win.backgroundColor = .clear
            win.level = .screenSaver
            win.ignoresMouseEvents = true
            win.setAccessibilityElement(false)
            win.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]

            let view = NSView(frame: CGRect(origin: .zero, size: screen.frame.size))
            view.wantsLayer = true

            let dim = CAShapeLayer()
            let dimPath = CGMutablePath()
            dimPath.addRect(view.bounds)
            dimPath.addPath(CGPath(roundedRect: spotlight, cornerWidth: 9,
                                   cornerHeight: 9, transform: nil))
            dim.path = dimPath
            dim.fillRule = .evenOdd
            dim.fillColor = NSColor.black.withAlphaComponent(0.42).cgColor

            let pulse = CAShapeLayer()
            pulse.path = CGPath(roundedRect: spotlight, cornerWidth: 9,
                                cornerHeight: 9, transform: nil)
            pulse.fillColor = NSColor.clear.cgColor
            pulse.strokeColor = NSColor.controlAccentColor.cgColor
            pulse.lineWidth = 4
            pulse.shadowColor = NSColor.controlAccentColor.cgColor
            pulse.shadowOpacity = 0.95
            pulse.shadowRadius = 8

            view.layer?.addSublayer(dim)
            view.layer?.addSublayer(pulse)
            self.addPendingChoiceQuestionMarks(to: view, above: spotlight)
            let blink = CABasicAnimation(keyPath: "opacity")
            blink.fromValue = 1.0
            blink.toValue = 0.28
            blink.duration = 0.48
            blink.autoreverses = true
            blink.repeatCount = .infinity
            pulse.add(blink, forKey: "pending-click-blink")

            win.contentView = view
            win.orderFrontRegardless()
            self.registerOverlay(win)
            self.pendingClickWindows = [win]
        }
    }

    /// A light, repeating “choice needed” cue. Three question marks rise and
    /// dissolve from the target's top edge at staggered intervals, leaving the
    /// control itself completely unobscured.
    private func addPendingChoiceQuestionMarks(to view: NSView, above target: CGRect) {
        guard let root = view.layer else { return }
        let count = 3
        let available = max(30, target.width)
        for index in 0..<count {
            let mark = CATextLayer()
            mark.string = "?"
            mark.alignmentMode = .center
            mark.contentsScale = NSScreen.main?.backingScaleFactor ?? 2
            mark.font = NSFont.systemFont(ofSize: 28, weight: .bold)
            mark.fontSize = 28
            mark.foregroundColor = (index == 1
                ? NSColor.controlAccentColor
                : NSColor.white).cgColor
            mark.shadowColor = NSColor.black.cgColor
            mark.shadowOpacity = 0.85
            mark.shadowRadius = 3
            let fraction = CGFloat(index + 1) / CGFloat(count + 1)
            let x = target.midX - available / 2 + available * fraction - 14
            let y = min(view.bounds.maxY - 34, target.maxY + 8)
            mark.frame = CGRect(x: x, y: y, width: 32, height: 38)
            mark.opacity = 0

            let rise = CABasicAnimation(keyPath: "transform.translation.y")
            rise.fromValue = 0
            // Travel from the pending control to the top edge instead of
            // evaporating beside it. Keeping the full path visible makes the
            // human-choice state readable even when attention is elsewhere.
            rise.toValue = max(30, view.bounds.maxY - y - mark.bounds.height)
            let drift = CAKeyframeAnimation(keyPath: "transform.translation.x")
            let direction: CGFloat = index.isMultiple(of: 2) ? -1 : 1
            drift.values = [0, 10 * direction, -7 * direction, 14 * direction]
            drift.keyTimes = [0, 0.34, 0.7, 1]
            let fade = CAKeyframeAnimation(keyPath: "opacity")
            fade.values = [0, 1, 1, 0]
            fade.keyTimes = [0, 0.08, 0.88, 1]
            let group = CAAnimationGroup()
            group.animations = [rise, drift, fade]
            group.duration = 3.2
            group.beginTime = CACurrentMediaTime() + Double(index) * 0.52
            group.repeatCount = .infinity
            group.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
            mark.add(group, forKey: "pending-choice-float")
            root.addSublayer(mark)
        }
    }

    /// Return the nearest sensibly-sized interactive AX element containing the
    /// proposed point. Browser text often sits inside an AXButton/AXLink, so we
    /// walk parents until we find an actionable control rather than outlining
    /// only the glyph under the pointer.
    private func pendingClickElementRect(at point: CGPoint) -> CGRect? {
        guard let element = pendingClickElement(at: point) else { return nil }
        return pendingClickRect(of: element)
    }

    private func pendingClickElement(at point: CGPoint) -> AXUIElement? {
        let system = AXUIElementCreateSystemWide()
        var hit: AXUIElement?
        guard AXUIElementCopyElementAtPosition(system, Float(point.x), Float(point.y), &hit) == .success,
              var current = hit else { return nil }

        for _ in 0..<8 {
            if pendingClickElementIsInteractive(current),
               let rect = pendingClickRect(of: current),
               rect.width >= 8, rect.height >= 8,
               rect.width <= 900, rect.height <= 500 {
                return current
            }
            var parentValue: CFTypeRef?
            guard AXUIElementCopyAttributeValue(current, kAXParentAttribute as CFString,
                                                &parentValue) == .success,
                  let parentValue,
                  CFGetTypeID(parentValue) == AXUIElementGetTypeID() else { break }
            current = unsafeBitCast(parentValue, to: AXUIElement.self)
        }
        return nil
    }

    /// Perform a human-approved action from the already-trusted native app.
    /// Chrome exposes AXPress for React dialog buttons but can acknowledge it
    /// without firing the page handler. Focusing the semantic control and
    /// sending its keyboard activation is the reliable accessible path.
    /// Canvas/custom targets retain the physical click fallback.
    private func pendingClickElement(matching title: String, near point: CGPoint) -> AXUIElement? {
        guard let front = NSWorkspace.shared.frontmostApplication else { return nil }
        let root = AXUIElementCreateApplication(front.processIdentifier)
        let wanted = title.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        var best: (element: AXUIElement, distance: CGFloat)?
        var visited = 0
        func walk(_ element: AXUIElement, depth: Int) {
            guard visited < 2200, depth < 28 else { return }
            visited += 1
            let values = axBatch(element)
            let role = values[0] as? String
            let rawCandidate = (values[2] as? String) ?? (values[3] as? String) ??
                (values[4] as? String)
            let candidate = rawCandidate?
                .trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
            // Frame envelopes intentionally cap titles at 80 characters; a
            // capped staged title still identifies the full live AX title.
            if candidate == wanted || (wanted.count >= 80 && candidate?.hasPrefix(wanted) == true),
               let role, ["AXButton", "AXLink", "AXCheckBox", "AXRadioButton"].contains(role),
               let rect = pendingClickRect(from: values) {
                let distance = hypot(rect.midX - point.x, rect.midY - point.y)
                if best == nil || distance < best!.distance { best = (element, distance) }
            }
            if let children = values[1] as? [AXUIElement] {
                for child in children { walk(child, depth: depth + 1) }
            }
        }
        walk(root, depth: 0)
        return best?.element
    }

    private func performApprovedClick(at point: CGPoint, count: Int, title: String?) {
        DispatchQueue.main.sync {
            // The spotlight is both mouse-transparent and absent from the AX
            // tree, but remove it synchronously before resolving/committing so
            // its WindowServer surface cannot outlive the approval boundary.
            self.clearPendingClickTargetOnMain(animated: false)
            self.yieldMainRunLoop(for: 0.012)
            let titled = title.flatMap { self.pendingClickElement(matching: $0, near: point) }
            if let element = titled ?? self.pendingClickElement(at: point) {
                var roleValue: CFTypeRef?
                let role = AXUIElementCopyAttributeValue(
                    element, kAXRoleAttribute as CFString, &roleValue
                ) == .success ? roleValue as? String : nil
                if role == "AXButton" || role == "AXLink" ||
                   role == "AXCheckBox" || role == "AXRadioButton" {
                    AXUIElementSetAttributeValue(
                        element, kAXFocusedAttribute as CFString, kCFBooleanTrue
                    )
                    // Chrome applies AX focus asynchronously. Pumping (rather
                    // than sleeping on) the main run loop lets that focus
                    // commit, and exits early as soon as AX reports it.
                    self.yieldMainRunLoopUntilFocused(element, timeout: 0.08)
                    let keyCode: CGKeyCode = (role == "AXCheckBox" || role == "AXRadioButton") ? 49 : 36
                    CGEvent(keyboardEventSource: nil, virtualKey: keyCode, keyDown: true)?.post(tap: .cghidEventTap)
                    self.yieldMainRunLoop(for: 0.012)
                    CGEvent(keyboardEventSource: nil, virtualKey: keyCode, keyDown: false)?.post(tap: .cghidEventTap)
                    return
                }
            }
            for index in 0..<count {
                let down = CGEvent(mouseEventSource: nil, mouseType: .leftMouseDown,
                                   mouseCursorPosition: point, mouseButton: .left)
                let up = CGEvent(mouseEventSource: nil, mouseType: .leftMouseUp,
                                 mouseCursorPosition: point, mouseButton: .left)
                down?.post(tap: .cghidEventTap)
                self.yieldMainRunLoop(for: 0.012)
                up?.post(tap: .cghidEventTap)
                if index + 1 < count { self.yieldMainRunLoop(for: 0.04) }
            }
        }
    }

    private func yieldMainRunLoop(for interval: TimeInterval) {
        let deadline = Date(timeIntervalSinceNow: interval)
        repeat {
            let slice = min(deadline, Date(timeIntervalSinceNow: 0.006))
            _ = RunLoop.main.run(mode: .default, before: slice)
        } while Date() < deadline
    }

    private func yieldMainRunLoopUntilFocused(_ element: AXUIElement,
                                               timeout: TimeInterval) {
        let deadline = Date(timeIntervalSinceNow: timeout)
        repeat {
            // Always yield at least one turn after AXFocused is set; checking
            // before the run-loop turn can observe the setter's local state
            // before Chrome has moved keyboard focus in the page.
            let slice = min(deadline, Date(timeIntervalSinceNow: 0.008))
            _ = RunLoop.main.run(mode: .default, before: slice)
            var focusedValue: CFTypeRef?
            if AXUIElementCopyAttributeValue(element, kAXFocusedAttribute as CFString,
                                             &focusedValue) == .success,
               focusedValue as? Bool == true { return }
        } while Date() < deadline
    }

    private func pendingClickElementIsInteractive(_ element: AXUIElement) -> Bool {
        var roleValue: CFTypeRef?
        let role: String? = AXUIElementCopyAttributeValue(
            element, kAXRoleAttribute as CFString, &roleValue
        ) == .success ? roleValue as? String : nil
        let interactiveRoles: Set<String> = [
            "AXButton", "AXCheckBox", "AXRadioButton", "AXPopUpButton",
            "AXTextField", "AXTextArea", "AXLink", "AXSlider",
        ]
        if let role, interactiveRoles.contains(role) { return true }
        var actions: CFArray?
        guard AXUIElementCopyActionNames(element, &actions) == .success,
              let names = actions as? [String] else { return false }
        return names.contains(kAXPressAction as String)
    }

    private func pendingClickRect(of element: AXUIElement) -> CGRect? {
        pendingClickRect(from: axBatch(element))
    }

    private func pendingClickRect(from values: [AnyObject?]) -> CGRect? {
        guard values.count > 6,
              let (x, y) = axCGPoint(values[5]),
              let (width, height) = axCGSize(values[6]),
              x.isFinite, y.isFinite, width.isFinite, height.isFinite,
              width > 0, height > 0 else { return nil }
        return CGRect(x: x, y: y, width: width, height: height)
    }

    private func writeActionAcknowledgement() {
        try? Data().write(to: URL(fileURLWithPath: Paths.frameOutJpg))
        let acknowledgement: [String: Any] = [
            "capture": "action",
            "capture_scope": "none",
            "ocr": [[String: Any]](),
            "visual": [[String: Any]](),
            "diff": [[String: Any]](),
            "ax": ["trusted": AXIsProcessTrusted(),
                   "elements": [[String: Any]]()] as [String: Any],
            "thumb_bytes": 0,
        ]
        if let data = try? JSONSerialization.data(withJSONObject: acknowledgement) {
            try? data.write(to: URL(fileURLWithPath: Paths.frameOut))
        }
    }

    private func clearPendingClickTarget() {
        DispatchQueue.main.async { self.clearPendingClickTargetOnMain() }
    }

    private func clearPendingClickTargetOnMain(animated: Bool = true) {
        let windows = pendingClickWindows
        pendingClickWindows.removeAll()
        for win in windows {
            guard animated else {
                unregisterOverlay(win)
                win.orderOut(nil)
                continue
            }
            NSAnimationContext.runAnimationGroup({ context in
                context.duration = 0.14
                context.timingFunction = CAMediaTimingFunction(name: .easeOut)
                win.animator().alphaValue = 0
            }, completionHandler: {
                self.unregisterOverlay(win)
                win.orderOut(nil)
            })
        }
    }

    // Frame observation must be invisible. A previous whole-display pulse made
    // automated polling blink the target and could interrupt Deskflow/filming.
    private func flashCaptureIndicator() {
        // Intentionally silent and non-activating.
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

    /// Find the frontmost app's topmost ordinary window. CGWindowList is in
    /// actual z-order. ScreenCaptureKit supplies the matched window's canonical
    /// global frame after this ID lookup, avoiding geometry drift between APIs.
    private func focusedWindowID() -> CGWindowID? {
        guard let front = NSWorkspace.shared.frontmostApplication else { return nil }
        let ownPID = ProcessInfo.processInfo.processIdentifier
        guard front.processIdentifier != ownPID,
              let info = CGWindowListCopyWindowInfo(
                [.optionOnScreenOnly, .excludeDesktopElements], kCGNullWindowID
              ) as? [[String: Any]] else { return nil }
        for window in info {
            guard (window[kCGWindowOwnerPID as String] as? Int32) == front.processIdentifier,
                  (window[kCGWindowLayer as String] as? Int) == 0,
                  let number = window[kCGWindowNumber as String] as? UInt32,
                  let bounds = window[kCGWindowBounds as String] as? [String: Any],
                  let rect = CGRect(dictionaryRepresentation: bounds as CFDictionary),
                  rect.width >= 2, rect.height >= 2 else { continue }
            return CGWindowID(number)
        }
        return nil
    }

    /// Capture either an explicit global crop, the focused window, or the
    /// whole display. The returned region is always in global screen points;
    /// OCR, visual controls, diffs, and virtual cursor markers use that origin
    /// so their coordinates remain directly click-ready even for window-only
    /// images.
    private func captureDisplay(crop: CGRect? = nil,
                                focusedWindowID: CGWindowID? = nil) ->
                                (image: CGImage?, region: CGRect, scope: String) {
        guard #available(macOS 14.0, *) else { return (nil, .zero, "screen") }
        let sem = DispatchSemaphore(value: 0)
        var img: CGImage?
        var capturedRegion = CGRect.zero
        var capturedScope = "screen"
        Task {
            defer { sem.signal() }
            guard let content = try? await SCShareableContent.excludingDesktopWindows(
                    false, onScreenWindowsOnly: true) else { return }
            if crop == nil, let focusedWindowID,
               let window = content.windows.first(where: { $0.windowID == focusedWindowID }) {
                let region = window.frame
                let filter = SCContentFilter(desktopIndependentWindow: window)
                let cfg = SCStreamConfiguration()
                cfg.width = max(1, Int(Double(region.width) * self.captureScale))
                cfg.height = max(1, Int(Double(region.height) * self.captureScale))
                cfg.showsCursor = true
                // Keep the pixel edge identical to SCWindow.frame. Shadows add
                // padding outside that global rect and would offset OCR/clicks.
                cfg.ignoreShadowsSingleWindow = true
                capturedRegion = region
                capturedScope = "window"
                img = try? await SCScreenshotManager.captureImage(
                    contentFilter: filter, configuration: cfg)
                return
            }
            guard let display = content.displays.first else { return }
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
            var region = CGRect(x: 0, y: 0, width: display.width, height: display.height)
            if let crop = crop {
                region = crop.intersection(CGRect(x: 0, y: 0, width: display.width, height: display.height))
                cfg.sourceRect = region
                capturedScope = "crop"
            }
            capturedRegion = region
            cfg.width = Int(Double(region.width) * self.captureScale)
            cfg.height = Int(Double(region.height) * self.captureScale)
            cfg.showsCursor = true
            img = try? await SCScreenshotManager.captureImage(contentFilter: filter, configuration: cfg)
        }
        sem.wait()
        return (img, capturedRegion, capturedScope)
    }

    // MARK: - OCR (text + click-center coords, in logical points)

    private func ocr(_ cg: CGImage, scale: Double, fast: Bool = false,
                     origin: CGPoint = .zero) -> [[String: Any]] {
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
            let x = origin.x + bb.minX * W / scale
            let y = origin.y + (1 - bb.maxY) * H / scale
            let w = bb.width * W / scale, h = bb.height * H / scale
            out.append(["t": c.string, "cx": Int(x + w / 2), "cy": Int(y + h / 2),
                        "r": [Int(x), Int(y), Int(w), Int(h)]])
        }
        return out
    }

    // Lightweight icon/control awareness for local reframes. OCR intentionally
    // ignores drawn glyphs such as notification × buttons; contours recover
    // compact, near-square controls without needing app-specific templates.
    private func visualControls(_ cg: CGImage, scale: Double, origin: CGPoint,
                                focus: CGPoint?) -> [[String: Any]] {
        // Contour cost grows sharply with dense desktop content. Analyze a
        // bounded local buffer and carry its scale back to global coordinates;
        // the returned frame pixels remain full quality.
        var scan = cg
        var scanScale = scale
        if cg.width > 512 {
            let w = 512, h = max(1, Int(Double(cg.height) * Double(w) / Double(cg.width)))
            if let ctx = CGContext(data: nil, width: w, height: h, bitsPerComponent: 8,
                bytesPerRow: 0, space: CGColorSpaceCreateDeviceRGB(),
                bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) {
                ctx.interpolationQuality = .medium
                ctx.draw(cg, in: CGRect(x: 0, y: 0, width: w, height: h))
                if let small = ctx.makeImage() {
                    scan = small
                    scanScale = scale * Double(w) / Double(cg.width)
                }
            }
        }
        let dark = VNDetectContoursRequest()
        dark.contrastAdjustment = 1.5
        dark.detectsDarkOnLight = true
        let light = VNDetectContoursRequest()
        light.contrastAdjustment = 1.5
        light.detectsDarkOnLight = false
        try? VNImageRequestHandler(cgImage: scan, options: [:]).perform([dark, light])
        let observations = [dark.results?.first, light.results?.first].compactMap { $0 }
        let W = Double(scan.width) / scanScale, H = Double(scan.height) / scanScale
        var out: [[String: Any]] = []
        for c in observations.flatMap({ $0.topLevelContours }) {
            let b = c.normalizedPath.boundingBox
            let x = origin.x + b.minX * W
            let y = origin.y + (1 - b.maxY) * H
            let w = b.width * W, h = b.height * H
            let ratio = w / max(h, 0.1)
            guard w >= 10, h >= 10, w <= 46, h <= 46, ratio >= 0.65, ratio <= 1.5 else { continue }
            let cx = x + w / 2, cy = y + h / 2
            let distance = focus.map { hypot(cx - $0.x, cy - $0.y) } ?? 0
            let duplicate = out.contains { abs(($0["cx"] as? Int ?? 0) - Int(cx)) < 3 && abs(($0["cy"] as? Int ?? 0) - Int(cy)) < 3 }
            if !duplicate { out.append(["kind": "compact-control", "cx": Int(cx), "cy": Int(cy),
                "r": [Int(x), Int(y), Int(w), Int(h)], "distance": Int(distance)]) }
        }
        return out.sorted { ($0["distance"] as? Int ?? 0) < ($1["distance"] as? Int ?? 0) }.prefix(24).map { $0 }
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

    private func thumbJPEG(_ cg: CGImage, maxWidth: Int, cursor: CGPoint? = nil,
                           crop: CGRect? = nil) -> Data? {
        let w = min(maxWidth, cg.width)
        let s = Double(w) / Double(cg.width)
        let h = max(1, Int(Double(cg.height) * s))
        guard let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: w, pixelsHigh: h,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0),
            let ctx = NSGraphicsContext(bitmapImageRep: rep) else { return nil }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = ctx
        ctx.cgContext.interpolationQuality = .high
        ctx.cgContext.draw(cg, in: CGRect(x: 0, y: 0, width: w, height: h))
        if let cursor = cursor, let screen = NSScreen.main {
            let region = crop ?? screen.frame
            let x = (cursor.x - region.minX) * CGFloat(w) / region.width
            let y = CGFloat(h) - (cursor.y - region.minY) * CGFloat(h) / region.height
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

    // Binary difference map for assured hover exploration. Work on a 4×4
    // occupancy grid: unchanged cells vanish; adjacent changed cells collapse
    // into global-coordinate regions that can be grepped locally or promoted
    // as tiny crops to a visual model only when meaning remains ambiguous.
    private func differenceMap(_ before: CGImage, _ after: CGImage,
                               origin: CGPoint, scale: Double) -> [[String: Any]] {
        guard before.width == after.width, before.height == after.height else { return [] }
        let w = after.width, h = after.height, stride = w * 4
        func pixels(_ image: CGImage) -> [UInt8] {
            var data = [UInt8](repeating: 0, count: h * stride)
            data.withUnsafeMutableBytes { raw in
                if let ctx = CGContext(data: raw.baseAddress, width: w, height: h,
                    bitsPerComponent: 8, bytesPerRow: stride,
                    space: CGColorSpaceCreateDeviceRGB(),
                    bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) {
                    ctx.draw(image, in: CGRect(x: 0, y: 0, width: w, height: h))
                }
            }
            return data
        }
        let a = pixels(before), b = pixels(after), cell = 4
        let gw = (w + cell - 1) / cell, gh = (h + cell - 1) / cell
        var changed = [Bool](repeating: false, count: gw * gh)
        for gy in 0..<gh { for gx in 0..<gw {
            var hits = 0
            for py in (gy * cell)..<min(h, (gy + 1) * cell) {
                for px in (gx * cell)..<min(w, (gx + 1) * cell) {
                    let i = py * stride + px * 4
                    let delta = max(abs(Int(a[i]) - Int(b[i])),
                        abs(Int(a[i + 1]) - Int(b[i + 1])), abs(Int(a[i + 2]) - Int(b[i + 2])))
                    if delta >= 24 { hits += 1 }
                }
            }
            changed[gy * gw + gx] = hits >= 2
        } }
        var seen = [Bool](repeating: false, count: changed.count)
        var out: [[String: Any]] = []
        for sy in 0..<gh { for sx in 0..<gw {
            let start = sy * gw + sx
            if !changed[start] || seen[start] { continue }
            seen[start] = true
            var q = [(sx, sy)], head = 0, minX = sx, maxX = sx, minY = sy, maxY = sy
            while head < q.count {
                let (x, y) = q[head]; head += 1
                minX = min(minX, x); maxX = max(maxX, x); minY = min(minY, y); maxY = max(maxY, y)
                for ny in max(0, y - 1)...min(gh - 1, y + 1) {
                    for nx in max(0, x - 1)...min(gw - 1, x + 1) {
                        let i = ny * gw + nx
                        if changed[i] && !seen[i] { seen[i] = true; q.append((nx, ny)) }
                    }
                }
            }
            guard q.count >= 2 else { continue }
            let px = minX * cell, pyTop = h - min(h, (maxY + 1) * cell)
            let pw = min(w, (maxX + 1) * cell) - px
            let ph = min(h, (maxY + 1) * cell) - minY * cell
            let x = origin.x + Double(px) / scale, y = origin.y + Double(pyTop) / scale
            let rw = Double(pw) / scale, rh = Double(ph) / scale
            out.append(["kind": "change", "cx": Int(x + rw / 2), "cy": Int(y + rh / 2),
                        "r": [Int(x), Int(y), Int(rw), Int(rh)], "cells": q.count])
        } }
        return out.sorted { ($0["cells"] as? Int ?? 0) > ($1["cells"] as? Int ?? 0) }.prefix(32).map { $0 }
    }

    // MARK: - assemble + write the envelope

    private func produce(noOCR: Bool, fast: Bool = false, wholeScreen: Bool = false,
                         virtualCursor: Bool = false, cursorOverride: CGPoint? = nil,
                         crop: CGRect? = nil, saveBaseline: Bool = false,
                         includeDiff: Bool = false) {
        func nowNs() -> UInt64 { DispatchTime.now().uptimeNanoseconds }
        func msSince(_ t: UInt64) -> Double { (Double(nowNs() - t) / 1e6 * 10).rounded() / 10 }
        var env: [String: Any] = [:]
        var tm: [String: Double] = [:]

        // An explicit crop always wins. Otherwise capture the topmost window
        // of the frontmost app unless the caller explicitly requested screen.
        let boundedCrop = crop.flatMap { c in NSScreen.main.map { c.intersection($0.frame) } }
        let target = (!wholeScreen && boundedCrop == nil) ? focusedWindowID() : nil

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
        t = nowNs()
        let captured = captureDisplay(crop: boundedCrop,
            focusedWindowID: wholeScreen ? nil : target)
        let cg = captured.image
        let captureRegion = captured.region
        env["capture_scope"] = captured.scope
        if captured.scope != "screen" {
            env["crop"] = ["x": Int(captureRegion.minX), "y": Int(captureRegion.minY),
                           "w": Int(captureRegion.width), "h": Int(captureRegion.height)]
        }
        tm["capture"] = msSince(t)
        if cg != nil { flashCaptureIndicator() }  // subtle post-capture awareness flash
        // The JPEG ships as RAW BYTES in a sidecar file (frame.out.jpg), not
        // base64 in the JSON — base64 inflates the payload +33% and burns
        // encode/decode CPU. The transport length-prefixes the two. Written
        // BEFORE the JSON + done marker so a reader that sees `done` has both.
        var jpgBytes = 0
        if let cg = cg {
            let region = captureRegion
            if includeDiff, let baseline = diffBaseline, baseline.rect.equalTo(region) {
                t = nowNs(); env["diff"] = differenceMap(baseline.image, cg,
                    origin: region.origin, scale: captureScale); tm["diff"] = msSince(t)
            } else { env["diff"] = [] }
            if saveBaseline { diffBaseline = (region, cg) }
            if noOCR {
                env["ocr"] = []
            } else {
                t = nowNs(); let boxes = ocr(cg, scale: captureScale, fast: fast,
                                             origin: region.origin); tm["ocr"] = msSince(t)
                env["ocr"] = boxes
                showOcrOverlay(boxes)
            }
            let cursorMeta = (mt["cursor"] as? [String: Int]).map {
                CGPoint(x: $0["x"] ?? 0, y: $0["y"] ?? 0)
            }
            let visualOrigin = region.origin
            t = nowNs(); env["visual"] = visualControls(cg, scale: captureScale,
                origin: visualOrigin, focus: cursorOverride ?? cursorMeta); tm["visual"] = msSince(t)
            t = nowNs()
            let marker = virtualCursor ? (cursorOverride ?? cursorMeta) : nil
            let jpg = thumbJPEG(cg, maxWidth: 1568, cursor: marker, crop: region) ?? Data()
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
        // Filter only after capture, against the region ScreenCaptureKit
        // actually returned. If a focused window vanished between the CG and
        // SC lookups, captureDisplay falls back to `screen` and the AX tree
        // correctly remains unfiltered instead of describing a stale crop.
        if captured.scope != "screen",
           let elements = axResult["elements"] as? [[String: Any]] {
            axResult["elements"] = elements.filter { element in
                guard let cx = element["cx"] as? Int, let cy = element["cy"] as? Int else {
                    return false
                }
                return captureRegion.contains(CGPoint(x: cx, y: cy))
            }
        }
        env["ax"] = axResult
        tm["ax"] = axMs
        tm["wall"] = (tm["meta"]! + tm["capture"]! + (tm["ocr"] ?? 0) + tm["thumb"]!)  // serial part; ax overlapped
        env["timings_ms"] = tm
        if let d = try? JSONSerialization.data(withJSONObject: env, options: []) {
            try? d.write(to: URL(fileURLWithPath: Paths.frameOut))
        }
    }
}
