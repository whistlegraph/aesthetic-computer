import AppKit
import ApplicationServices
import CoreGraphics
import ScreenCaptureKit
import Vision

@_silgen_name("_AXUIElementGetWindow")
private func _FrameAXUIElementGetWindow(_ element: AXUIElement,
                                       _ windowID: UnsafeMutablePointer<CGWindowID>) -> AXError

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
    private let visualDetector = FrameVisualControls()
    private let fm = FileManager.default
    private let nativeBindings = FrameNativeBindings()
    private let socket = FrameSocket()
    private var socketOutput: (json: Data, jpg: Data)?

    private func writeJSON(_ data: Data) {
        if socketOutput != nil { socketOutput?.json = data }
        else { try? data.write(to: URL(fileURLWithPath: Paths.frameOut)) }
    }

    private func writeJPEG(_ data: Data) {
        if socketOutput != nil { socketOutput?.jpg = data }
        else { try? data.write(to: URL(fileURLWithPath: Paths.frameOutJpg)) }
    }

    // Transient overlay windows we draw (capture flash, OCR boxes). We exclude
    // them from the screen capture by windowID so they never appear in a frame
    // — that's the "doesn't interfere" guarantee. (The badge etc. still show.)
    // Capture at this multiple of the display's point size — 2x makes small,
    // dense text (terminals) physically larger in the buffer, pushing it over
    // Vision's recognition threshold. The OCR scale uses the same factor so
    // box coords still map back to screen points.
    private let captureScale: Double = 1.0
    /// How far outside the window an overlay-inclusive frame reaches. The rock
    /// sits above the title bar and the complaint about it is its distance from
    /// the menu bar, so a shot that stops at the window edge cannot answer the
    /// question being asked of it.
    private static let overlayMargin: Double = 180
    private let overlayLock = NSLock()
    private var overlayWindowIDs = Set<Int>()
    private var ocrOverlayWindow: NSWindow?
    private var pendingClickWindows: [NSWindow] = []
    private var pendingClickMonitor: Any?
    private var pendingClickApprovalId: String?
    private var pendingClickScreenRect: CGRect?
    private var diffBaselines: [String: (rect: CGRect, image: CGImage, window: CGWindowID?, used: UInt64)] = [:]
    private let maxDiffSessions = 4 // full-resolution CGImages: bound host memory

    private func registerOverlay(_ w: NSWindow) {
        overlayLock.lock(); overlayWindowIDs.insert(w.windowNumber); overlayLock.unlock()
    }
    private func unregisterOverlay(_ w: NSWindow) {
        overlayLock.lock(); overlayWindowIDs.remove(w.windowNumber); overlayLock.unlock()
    }

    func start() {
        let dir = (Paths.frameReq as NSString).deletingLastPathComponent
        try? fm.createDirectory(atPath: dir, withIntermediateDirectories: true)
        do {
            try socket.start(path: dir + "/frame-native.sock") { [self] mode in
                queue.sync {
                    socketOutput = (Data("{\"error\":\"Native response unavailable\"}".utf8), Data())
                    process(mode)
                    let output = socketOutput!
                    socketOutput = nil
                    return (output.json, output.jpg)
                }
            }
        } catch { NSLog("Frame socket unavailable: %@", String(describing: error)) }
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
        process(mode)
        fm.createFile(atPath: Paths.frameDone, contents: nil)
    }

    private func process(_ mode: String) {
        let flags = Set(mode.split(separator: " ").map(String.init))
        if handleNativeInput(mode: mode, flags: flags) {
            return
        }
        if let token = mode.split(separator: " ").first(where: { $0.hasPrefix("manual-check=") }) {
            let approvalId = String(token.dropFirst("manual-check=".count))
            writeManualActionAcknowledgement(approvalId: approvalId)
            return
        }
        if mode.split(separator: " ").contains("overlay-clear") {
            clearTransientOverlays()
            writeActionAcknowledgement()
            return
        }
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
        var pendingClickApprovalId: String?
        if let token = mode.split(separator: " ").first(where: { $0.hasPrefix("target-id=") }) {
            pendingClickApprovalId = String(token.dropFirst("target-id=".count))
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
        if let token = mode.split(separator: " ").first(where: { $0.hasPrefix("expect-target=") }) {
            let expected = String(token.dropFirst("expect-target=".count))
            var matches = false
            DispatchQueue.main.sync { matches = self.pendingClickApprovalId == expected }
            if !matches {
                let error = ["error": "Staged target changed or was cleared; no action sent. Stage again."]
                if let data = try? JSONSerialization.data(withJSONObject: error) {
                    writeJSON(data)
                }
                writeJPEG(Data())
                    return
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
                    return
            }
        }
        let session = mode.split(separator: " ").first(where: { $0.hasPrefix("session=") })
            .map { String($0.dropFirst("session=".count)) } ?? "legacy"
        produce(session: session, noOCR: flags.contains("noocr"), noVisual: flags.contains("novisual"), fast: flags.contains("fast"),
                wholeScreen: flags.contains("screen"),
                virtualCursor: flags.contains("cursor"), cursorOverride: cursorOverride, crop: crop,
                saveBaseline: flags.contains("baseline"), includeDiff: flags.contains("diff"),
                showOverlay: !flags.contains("quiet-overlay"),
                includeOverlays: flags.contains("overlays"))
        if let pendingClickTarget {
            showPendingClickTarget(at: pendingClickTarget, approvalId: pendingClickApprovalId)
        }
    }

    /// Spotlight a proposed click without performing it. Accessibility expands
    /// the point to the whole control (button, field, link, etc.); when no
    /// bounded control can be resolved we fall back to a compact point target.
    /// The rest of the display is dimmed while the target outline pulses until
    /// the human taps it directly or a later `target-clear` request resolves it.
    /// The click-through overlay is excluded from captures, whose virtual
    /// cursor still records the exact staged coordinate.
    private func showPendingClickTarget(at point: CGPoint, approvalId: String?) {
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
            // These windows are owned strongly by FrameCapture. Letting
            // NSWindow also release itself on close can over-release during
            // the main run loop's autorelease-pool drain.
            win.isReleasedWhenClosed = false
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
            self.addPendingTapHint(to: view, near: spotlight)
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
            if let approvalId, !approvalId.isEmpty {
                // Clear a signal left by an older staged action before arming
                // this approval id. The overlay remains click-through, so the
                // same human tap still reaches the underlying application.
                try? self.fm.removeItem(atPath: Paths.frameManualAction)
                let screenRect = targetInScreen.offsetBy(
                    dx: screen.frame.minX, dy: screen.frame.minY
                ).insetBy(dx: -2, dy: -2)
                self.installPendingClickMonitor(approvalId: approvalId,
                                                screenRect: screenRect)
            }
        }
    }

    /// Explain the direct-approval gesture on the overlay itself. This matters
    /// while the staging tool is intentionally still pending and the chat has
    /// not yet had a chance to print its textual yes/no fallback.
    private func addPendingTapHint(to view: NSView, near target: CGRect) {
        guard let root = view.layer else { return }
        let text = "Tap the highlighted control to approve"
        let width: CGFloat = 310
        let height: CGFloat = 34
        let preferredY = target.minY - height - 14
        let y = preferredY >= 12
            ? preferredY
            : min(view.bounds.maxY - height - 12, target.maxY + 14)
        let frame = CGRect(
            x: min(max(12, target.midX - width / 2), view.bounds.maxX - width - 12),
            y: y, width: width, height: height
        )

        let bubble = CAShapeLayer()
        bubble.path = CGPath(roundedRect: frame, cornerWidth: 17,
                             cornerHeight: 17, transform: nil)
        bubble.fillColor = NSColor.black.withAlphaComponent(0.78).cgColor
        bubble.strokeColor = NSColor.white.withAlphaComponent(0.72).cgColor
        bubble.lineWidth = 1
        bubble.shadowColor = NSColor.black.cgColor
        bubble.shadowOpacity = 0.7
        bubble.shadowRadius = 5

        let label = CATextLayer()
        label.string = text
        label.alignmentMode = .center
        label.contentsScale = NSScreen.main?.backingScaleFactor ?? 2
        label.font = NSFont.systemFont(ofSize: 14, weight: .semibold)
        label.fontSize = 14
        label.foregroundColor = NSColor.white.cgColor
        label.frame = frame.insetBy(dx: 10, dy: 8)

        root.addSublayer(bubble)
        root.addSublayer(label)
    }

    private func installPendingClickMonitor(approvalId: String, screenRect: CGRect) {
        removePendingClickMonitor()
        pendingClickApprovalId = approvalId
        pendingClickScreenRect = screenRect
        pendingClickMonitor = NSEvent.addGlobalMonitorForEvents(matching: .leftMouseUp) {
            [weak self] _ in
            DispatchQueue.main.async { self?.observePendingHumanClick() }
        }
    }

    private func observePendingHumanClick() {
        guard let approvalId = pendingClickApprovalId,
              let screenRect = pendingClickScreenRect else { return }
        let location = NSEvent.mouseLocation
        guard screenRect.contains(location) else { return }
        let action: [String: Any] = [
            "kind": "manual-commit",
            "approval_id": approvalId,
            "x": location.x,
            "y": location.y,
            "observed_at": ISO8601DateFormatter().string(from: Date()),
        ]
        if let data = try? JSONSerialization.data(withJSONObject: action) {
            try? data.write(to: URL(fileURLWithPath: Paths.frameManualAction),
                            options: .atomic)
        }
        clearPendingClickTargetOnMain(animated: false)
    }

    private func removePendingClickMonitor() {
        if let pendingClickMonitor { NSEvent.removeMonitor(pendingClickMonitor) }
        pendingClickMonitor = nil
        pendingClickApprovalId = nil
        pendingClickScreenRect = nil
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
        writeJPEG(Data())
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
            writeJSON(data)
        }
    }

    private func writeManualActionAcknowledgement(approvalId: String) {
        writeJPEG(Data())
        var manualAction: Any = NSNull()
        if let data = try? Data(contentsOf: URL(fileURLWithPath: Paths.frameManualAction)),
           let object = try? JSONSerialization.jsonObject(with: data),
           let action = object as? [String: Any],
           action["approval_id"] as? String == approvalId {
            manualAction = action
            try? fm.removeItem(atPath: Paths.frameManualAction)
        }
        let acknowledgement: [String: Any] = [
            "capture": "manual-check",
            "capture_scope": "none",
            "manual_action": manualAction,
            "ocr": [[String: Any]](),
            "visual": [[String: Any]](),
            "diff": [[String: Any]](),
            "ax": ["trusted": AXIsProcessTrusted(),
                   "elements": [[String: Any]]()] as [String: Any],
            "thumb_bytes": 0,
        ]
        if let data = try? JSONSerialization.data(withJSONObject: acknowledgement) {
            writeJSON(data)
        }
    }

    private func clearPendingClickTarget() {
        DispatchQueue.main.async { self.clearPendingClickTargetOnMain() }
    }

    private func clearPendingClickTargetOnMain(animated: Bool = true) {
        removePendingClickMonitor()
        let windows = pendingClickWindows
        pendingClickWindows.removeAll()
        for win in windows {
            guard animated else {
                unregisterOverlay(win)
                win.orderOut(nil)
                win.close()
                continue
            }
            NSAnimationContext.runAnimationGroup({ context in
                context.duration = 0.14
                context.timingFunction = CAMediaTimingFunction(name: .easeOut)
                win.animator().alphaValue = 0
            }, completionHandler: {
                self.unregisterOverlay(win)
                win.orderOut(nil)
                win.close()
            })
        }
    }

    /// Remove every transient surface owned by FrameCapture. This is both the
    /// replacement primitive used before drawing a new overlay and a recovery
    /// path for a fleet host that was upgraded while an old overlay was live.
    private func clearTransientOverlays() {
        let clear = {
            self.clearPendingClickTargetOnMain(animated: false)
            self.clearOcrOverlayOnMain()
        }
        if Thread.isMainThread { clear() }
        else { DispatchQueue.main.sync(execute: clear) }
    }

    private func clearOcrOverlayOnMain() {
        guard let win = ocrOverlayWindow else { return }
        ocrOverlayWindow = nil
        unregisterOverlay(win)
        win.alphaValue = 0
        win.orderOut(nil)
        win.close()
    }

    // Frame observation must be invisible. A previous whole-display pulse made
    // automated polling blink the target and could interrupt Deskflow/filming.
    private func flashCaptureIndicator() {
        // Intentionally silent and non-activating.
    }

    // Draw the whole-screen OCR boxes as a brief screen-wide overlay, so a
    // watcher sees what was read across the ENTIRE display — not just inside a
    // browser window (puppet's page-side scan). Click-through, excluded from
    // captures by windowID, holds ~1s then fades. Boxes are points/top-left
    // (from ocr()); CALayer is bottom-left, so Y flips against screen height.
    private func showOcrOverlay(_ boxes: [[String: Any]]) {
        DispatchQueue.main.async {
            // Replacement is destructive by design: there can be at most one
            // OCR surface, even when several frame requests overlap.
            self.clearOcrOverlayOnMain()
            guard !boxes.isEmpty else { return }
            guard let screen = NSScreen.main else { return }
            let H = screen.frame.height
            let win = NSWindow(contentRect: screen.frame, styleMask: .borderless,
                               backing: .buffered, defer: false)
            // FrameCapture retains this window until cleanup. Disable
            // NSWindow's legacy self-release so closing a transient overlay
            // cannot race ARC at the main autorelease-pool boundary.
            win.isReleasedWhenClosed = false
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
            DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) {
                // A replacement clears and closes the old window immediately.
                // Its delayed cleanup must then become a no-op; otherwise the
                // same NSWindow receives a second animation + close sequence.
                guard self.ocrOverlayWindow === win else { return }
                NSAnimationContext.runAnimationGroup({ ctx in
                    ctx.duration = 0.18
                    win.animator().alphaValue = 0.0
                }, completionHandler: {
                    guard self.ocrOverlayWindow === win else { return }
                    self.ocrOverlayWindow = nil
                    self.unregisterOverlay(win)
                    win.orderOut(nil)
                    win.close()
                })
            }
        }
    }

    // MARK: - capture (in-process; no screencapture subprocess → no launchd throttle)

    /// Find the frontmost app's topmost ordinary window. CGWindowList is in
    /// actual z-order. ScreenCaptureKit supplies the matched window's canonical
    /// global frame after this ID lookup, avoiding geometry drift between APIs.
    private func nativeTarget() -> FrameNativeTarget? {
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
            return FrameNativeTarget(windowID: CGWindowID(number), pid: front.processIdentifier, bounds: rect)
        }
        return nil
    }

    private func focusedWindowID() -> CGWindowID? { nativeTarget()?.windowID }

    private func nativeDesktopAvailable() -> Bool {
        guard AXIsProcessTrusted(), CGPreflightScreenCaptureAccess(),
              NSWorkspace.shared.frontmostApplication?.bundleIdentifier != "com.apple.loginwindow",
              let session = CGSessionCopyCurrentDictionary() as? [String: Any],
              session[kCGSessionOnConsoleKey as String] as? Bool == true,
              session["CGSSessionScreenIsLocked"] as? Bool != true else { return false }
        return true
    }

    private func nativeReply(_ value: [String: Any]) {
        writeJPEG(Data())
        if let data = try? JSONSerialization.data(withJSONObject: value) {
            writeJSON(data)
        }
    }

    /// Hit-test one observed point; never search another window for matching text.
    private func verificationValue(_ check: FrameNativeVerification, target: FrameNativeTarget) -> String? {
        guard target.bounds.contains(CGPoint(x: check.x, y: check.y)) else { return nil }
        let system = AXUIElementCreateSystemWide()
        AXUIElementSetMessagingTimeout(system, 0.1)
        var hit: AXUIElement?
        guard AXUIElementCopyElementAtPosition(system, Float(check.x), Float(check.y), &hit) == .success,
              let hit else { return nil }
        AXUIElementSetMessagingTimeout(hit, 0.1)
        var pid: pid_t = 0
        guard AXUIElementGetPid(hit, &pid) == .success, pid == target.pid else { return nil }
        var raw: CFArray?
        guard AXUIElementCopyMultipleAttributeValues(hit,
            ["AXRole", check.attribute, "AXWindow"] as CFArray,
            AXCopyMultipleAttributeOptions(rawValue: 0), &raw) == .success,
            let attributes = raw as? [AnyObject], attributes.count == 3,
            attributes[0] as? String == check.role,
            let value = attributes[1] as? String,
            CFGetTypeID(attributes[2]) == AXUIElementGetTypeID() else { return nil }
        var windowID: CGWindowID = 0
        guard _FrameAXUIElementGetWindow(attributes[2] as! AXUIElement, &windowID) == .success,
              windowID == target.windowID else { return nil }
        return value
    }

    private func handleNativeInput(mode: String, flags: Set<String>) -> Bool {
        let tokens = mode.split(separator: " ").map(String.init)
        let clickToken = tokens.first { $0.hasPrefix("native-click=") || $0.hasPrefix("native-drag=") }
        let guardToken = tokens.first { $0.hasPrefix("native-guard=") }
        guard let token = clickToken ?? guardToken else { return false }
        let session = tokens.first { $0.hasPrefix("session=") }.map { String($0.dropFirst(8)) } ?? "legacy"
        let prefix = clickToken == nil ? "native-guard=" : (clickToken!.hasPrefix("native-drag=") ? "native-drag=" : "native-click=")
        guard let data = Data(base64Encoded: String(token.dropFirst(prefix.count))), data.count <= 2048,
              let body = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any],
              let id = body["observationId"] as? String, !id.isEmpty,
              !(clickToken != nil && guardToken != nil) else {
            nativeReply(["error": "Invalid native request; no action sent"])
            return true
        }
        let click = clickToken == nil ? nil : try? JSONDecoder().decode(FrameNativeClick.self, from: data)
        if clickToken != nil && (click?.valid != true || (prefix == "native-drag=") != (click?.drag != nil)) {
            nativeReply(["error": "Invalid native click; no action sent"])
            return true
        }
        let started = DispatchTime.now().uptimeNanoseconds
        var failure: String?
        var guardMs = 0.0
        var inputMs = 0.0
        var checkedTarget: FrameNativeTarget?
        var initialValue: String?
        // Resolve foreground identity/geometry and send input together in the
        // Accessibility-trusted process. No AX traversal, pixels, or JPEG here.
        DispatchQueue.main.sync {
            checkedTarget = self.nativeTarget()
            let point = click.map { CGPoint(x: $0.x, y: $0.y) }
            failure = self.nativeBindings.validate(session: session, id: id,
                current: checkedTarget, available: self.nativeDesktopAvailable(), point: point)
            guardMs = Double(DispatchTime.now().uptimeNanoseconds - started) / 1e6
            guard failure == nil, let click, let point else { return }
            if let check = click.verify, let target = checkedTarget {
                initialValue = self.verificationValue(check, target: target)
                guard let initialValue, initialValue != check.equals else {
                    failure = "Verification point unavailable, wrong role/window, or condition already true"
                    return
                }
                // AX IPC may yield to the target app. Check the desktop again
                // immediately before input, after resolving the condition.
                guard self.nativeTarget() == target, self.nativeDesktopAvailable() else {
                    failure = "Native target changed during verification setup"
                    return
                }
            }
            if let drag = click.drag {
                var displays = [CGDirectDisplayID](repeating: 0, count: 32)
                var displayCount: UInt32 = 0
                guard CGGetActiveDisplayList(32, &displays, &displayCount) == .success,
                      displays.prefix(Int(displayCount)).contains(where: { CGDisplayBounds($0).contains(CGPoint(x: drag.x, y: drag.y)) }) else {
                    failure = "Drag destination is outside active displays"
                    return
                }
                let end = CGPoint(x: drag.x, y: drag.y)
                let steps = max(2, Int(ceil(drag.durationMs / 8)))
                guard let down = CGEvent(mouseEventSource: nil, mouseType: .leftMouseDown, mouseCursorPosition: point, mouseButton: .left),
                      let up = CGEvent(mouseEventSource: nil, mouseType: .leftMouseUp, mouseCursorPosition: end, mouseButton: .left) else {
                    failure = "Unable to allocate drag events"; return
                }
                var moves: [CGEvent] = []
                for i in 1...steps {
                    let fraction = Double(i) / Double(steps)
                    let position = CGPoint(x: point.x + (end.x - point.x) * fraction, y: point.y + (end.y - point.y) * fraction)
                    guard let move = CGEvent(mouseEventSource: nil, mouseType: .leftMouseDragged, mouseCursorPosition: position, mouseButton: .left) else {
                        failure = "Unable to allocate drag path"; return
                    }
                    moves.append(move)
                }
                self.nativeBindings.clear(session)
                let inputStart = DispatchTime.now().uptimeNanoseconds
                // All events exist before mouse-down; every dispatch path posts
                // mouse-up. The optional destination dwell is explicit.
                do {
                    defer { up.post(tap: .cghidEventTap) }
                    down.post(tap: .cghidEventTap)
                    if click.effectiveHoldMs > 0 { self.yieldMainRunLoop(for: click.effectiveHoldMs / 1000) }
                    let pathStart = DispatchTime.now().uptimeNanoseconds
                    for (index, move) in moves.enumerated() {
                        // Absolute deadlines avoid adding main-run-loop scheduling
                        // overruns to every step of the path.
                        let dueMs = drag.durationMs * Double(index + 1) / Double(steps)
                        let elapsedMs = Double(DispatchTime.now().uptimeNanoseconds - pathStart) / 1e6
                        if dueMs > elapsedMs { self.yieldMainRunLoop(for: (dueMs - elapsedMs) / 1000) }
                        move.post(tap: .cghidEventTap)
                    }
                    if drag.effectiveReleaseMs > 0 { self.yieldMainRunLoop(for: drag.effectiveReleaseMs / 1000) }
                }
                inputMs = Double(DispatchTime.now().uptimeNanoseconds - inputStart) / 1e6
                return
            }
            var events: [(CGEvent, CGEvent)] = []
            for index in 0..<click.count {
                guard let down = CGEvent(mouseEventSource: nil, mouseType: .leftMouseDown,
                                         mouseCursorPosition: point, mouseButton: .left),
                      let up = CGEvent(mouseEventSource: nil, mouseType: .leftMouseUp,
                                       mouseCursorPosition: point, mouseButton: .left) else {
                    failure = "Unable to allocate mouse events"
                    return
                }
                down.setIntegerValueField(.mouseEventClickState, value: Int64(index + 1))
                up.setIntegerValueField(.mouseEventClickState, value: Int64(index + 1))
                events.append((down, up))
            }
            // Consume before dispatch. A lost response must never allow replay
            // of the same observation, even after another client's request.
            self.nativeBindings.clear(session)
            let inputStart = DispatchTime.now().uptimeNanoseconds
            for (index, pair) in events.enumerated() {
                pair.0.post(tap: .cghidEventTap)
                if click.effectiveHoldMs > 0 { self.yieldMainRunLoop(for: click.effectiveHoldMs / 1000) }
                pair.1.post(tap: .cghidEventTap)
                if index + 1 < events.count { self.yieldMainRunLoop(for: 0.08) }
            }
            inputMs = Double(DispatchTime.now().uptimeNanoseconds - inputStart) / 1e6
        }
        if let failure {
            nativeReply(["error": failure + "; no action sent. Capture frame again"])
            return true
        }
        var receipt: [String: Any] = ["observationId": id, "guardMs": guardMs,
            "inputMs": inputMs, "status": click == nil ? "guarded" : "dispatched"]
        guard let click else {
            nativeReply(["capture": "guard", "nativeInput": receipt])
            return true
        }
        receipt["holdMs"] = click.effectiveHoldMs
        receipt["releasePosted"] = true
        receipt["kind"] = click.drag == nil ? "click" : "drag"
        if let drag = click.drag { receipt["durationMs"] = drag.durationMs; receipt["releaseMs"] = drag.effectiveReleaseMs }
        if click.settleMs > 0 { Thread.sleep(forTimeInterval: click.settleMs / 1000) }
        if let check = click.verify, let target = checkedTarget {
            let verifyStart = DispatchTime.now().uptimeNanoseconds
            var observed: String?
            var verified = false
            repeat {
                observed = verificationValue(check, target: target)
                var unchanged = false
                DispatchQueue.main.sync { unchanged = self.nativeTarget() == target && self.nativeDesktopAvailable() }
                if !unchanged { break }
                if observed == check.equals { verified = true; break }
                if Double(DispatchTime.now().uptimeNanoseconds - verifyStart) / 1e6 >= check.timeoutMs { break }
                Thread.sleep(forTimeInterval: 0.005)
            } while true
            receipt["verification"] = ["ok": verified, "role": check.role, "attribute": check.attribute,
                "expected": check.equals, "before": initialValue ?? "", "observed": observed ?? "",
                "ms": Double(DispatchTime.now().uptimeNanoseconds - verifyStart) / 1e6]
            if verified {
                let nextID = UUID().uuidString
                nativeBindings.record(session: session, id: nextID, target: target)
                nativeReply(["capture": "verified", "capture_scope": "window", "thumb_bytes": 0,
                    "nativeCapabilities": ["target-guard-v1", "guarded-click-v1", "ax-verify-v1", "click-hold-v1", "guarded-drag-v1"],
                    "meta": ["frontmost": ["pid": target.pid]],
                    "crop": ["x": Int(target.bounds.minX), "y": Int(target.bounds.minY),
                             "w": Int(target.bounds.width), "h": Int(target.bounds.height)],
                    "observation": ["id": nextID, "session": session, "windowId": target.windowID,
                        "kind": "ax-verification", "coordinateSpace": "macos-global-points"],
                    "nativeInput": receipt])
                return true
            }
            // The click happened. A full observation is recovery evidence, never a retry.
        }
        produce(session: session, noOCR: flags.contains("noocr"), noVisual: flags.contains("novisual"),
                fast: flags.contains("fast"), virtualCursor: true,
                cursorOverride: CGPoint(x: click.drag?.x ?? click.x, y: click.drag?.y ?? click.y), showOverlay: !flags.contains("quiet-overlay"),
                nativeInput: receipt)
        return true
    }

    /// Capture either an explicit global crop, the focused window, or the
    /// whole display. The returned region is always in global screen points;
    /// OCR, visual controls, diffs, and virtual cursor markers use that origin
    /// so their coordinates remain directly click-ready even for window-only
    /// images.
    private func captureDisplay(crop: CGRect? = nil,
                                focusedWindowID: CGWindowID? = nil,
                                includeOverlays: Bool = false) ->
                                (image: CGImage?, region: CGRect, scope: String) {
        guard #available(macOS 14.0, *) else { return (nil, .zero, "screen") }
        let sem = DispatchSemaphore(value: 0)
        var img: CGImage?
        var capturedRegion = CGRect.zero
        var capturedScope = "screen"
        Task {
            defer { sem.signal() }
            let content: SCShareableContent
            do {
                content = try await SCShareableContent.excludingDesktopWindows(
                    false, onScreenWindowsOnly: true)
            } catch {
                NSLog("Frame capture: shareable content failed: %@", String(describing: error))
                return
            }
            // Asking for the overlays and then capturing a desktop-independent
            // window is a contradiction: that path renders the window alone, so
            // whatever is parked on top of it is exactly what gets dropped.
            // Widen instead — shoot the display, cropped to the window plus a
            // margin, so the rock above the title bar and the menu bar it is
            // said to be flush against both land in the same picture.
            var cropRect = crop
            if includeOverlays, crop == nil, let focusedWindowID,
               let window = content.windows.first(where: { $0.windowID == focusedWindowID }) {
                cropRect = window.frame.insetBy(dx: -Self.overlayMargin, dy: -Self.overlayMargin)
            }
            if cropRect == nil, let focusedWindowID,
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
                do {
                    img = try await SCScreenshotManager.captureImage(
                        contentFilter: filter, configuration: cfg)
                } catch {
                    NSLog("Frame capture: focused-window screenshot failed: %@", String(describing: error))
                }
                return
            }
            guard let display = content.displays.first else { return }
            // GUARANTEE we capture UNDER everything this app draws. Belt: any
            // window we own (flash, OCR overlay, badge, previews) by bundle id.
            // Suspenders: the explicitly-tracked overlay window ids, in case a
            // window's owning app is momentarily unresolved. A frame is always
            // the machine's real content beneath our overlays — never them.
            // Hiding our own overlays is right for the usual job — reading the
            // machine's real content — and exactly wrong when the overlays are
            // the subject. Adjusting the rock's padding or the preview's shadow
            // against a capture that filters both out is working blind, so the
            // exclusion is a default rather than a rule.
            let myBundle = Bundle.main.bundleIdentifier
            let exclude = includeOverlays ? [] : content.windows.filter { w in
                if w.owningApplication?.bundleIdentifier == myBundle { return true }
                self.overlayLock.lock(); defer { self.overlayLock.unlock() }
                return self.overlayWindowIDs.contains(Int(w.windowID))
            }
            let filter = SCContentFilter(display: display, excludingWindows: exclude)
            let cfg = SCStreamConfiguration()
            var region = CGRect(x: 0, y: 0, width: display.width, height: display.height)
            if let wanted = cropRect {
                region = wanted.intersection(CGRect(x: 0, y: 0, width: display.width, height: display.height))
                cfg.sourceRect = region
                capturedScope = (includeOverlays && crop == nil) ? "window+overlays" : "crop"
            }
            capturedRegion = region
            cfg.width = Int(Double(region.width) * self.captureScale)
            cfg.height = Int(Double(region.height) * self.captureScale)
            cfg.showsCursor = true
            do {
                img = try await SCScreenshotManager.captureImage(contentFilter: filter, configuration: cfg)
            } catch {
                NSLog("Frame capture: display screenshot failed: %@", String(describing: error))
            }
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
    private func axTree(windowID: CGWindowID?) -> [String: Any] {
        guard AXIsProcessTrusted() else { return ["trusted": false, "elements": []] }
        guard let front = NSWorkspace.shared.frontmostApplication else {
            return ["trusted": true, "elements": []]
        }
        let app = AXUIElementCreateApplication(front.processIdentifier)
        var root = app
        if let windowID {
            // A rectangle filter alone includes obscured controls from other
            // windows at the same coordinates. Match the captured CG window
            // before walking AX, reducing both ambiguity and IPC work.
            var raw: CFTypeRef?
            guard AXUIElementCopyAttributeValue(app, kAXWindowsAttribute as CFString, &raw) == .success,
                  let windows = raw as? [AXUIElement],
                  let window = windows.first(where: { element in
                      var id: CGWindowID = 0
                      return _FrameAXUIElementGetWindow(element, &id) == .success && id == windowID
                  }) else {
                return ["trusted": true, "app": front.localizedName ?? "?",
                        "scope": "window-unavailable", "windowId": windowID, "elements": []]
            }
            root = window
        }
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
        walk(root, 0)
        return ["trusted": true, "app": front.localizedName ?? "?", "elements": out,
                "scope": windowID == nil ? "application" : "window",
                "windowId": windowID.map { $0 as Any } ?? NSNull()]
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

    private func produce(session: String = "legacy", noOCR: Bool, noVisual: Bool = false, fast: Bool = false, wholeScreen: Bool = false,
                         virtualCursor: Bool = false, cursorOverride: CGPoint? = nil,
                         crop: CGRect? = nil, saveBaseline: Bool = false,
                         includeDiff: Bool = false, showOverlay: Bool = true,
                         includeOverlays: Bool = false, nativeInput: [String: Any]? = nil) {
        func nowNs() -> UInt64 { DispatchTime.now().uptimeNanoseconds }
        func msSince(_ t: UInt64) -> Double { (Double(nowNs() - t) / 1e6 * 10).rounded() / 10 }
        let started = nowNs()
        var env: [String: Any] = [:]
        env["nativeCapabilities"] = ["target-guard-v1", "guarded-click-v1", "ax-verify-v1", "click-hold-v1", "guarded-drag-v1"]
        if let nativeInput { env["nativeInput"] = nativeInput }
        var tm: [String: Double] = [:]
        let reelActive: Bool
        if #available(macOS 15.0, *) {
            reelActive = ScreenRecord.shared.reservesExternalProcesses
        } else {
            reelActive = false
        }

        // An explicit crop always wins. Otherwise capture the topmost window
        // of the frontmost app unless the caller explicitly requested screen.
        let boundedCrop = crop.flatMap { c in NSScreen.main.map { c.intersection($0.frame) } }
        let target = (!wholeScreen && boundedCrop == nil) ? focusedWindowID() : nil
        let targetBefore = nativeTarget()
        nativeBindings.clear(session)

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
            axResult = self.axTree(windowID: target)
            axMs = msSince(axStart)
            grp.leave()
        }

        var t = nowNs(); let mt = meta(); tm["meta"] = msSince(t)
        env["meta"] = mt
        t = nowNs()
        let captured = captureDisplay(crop: boundedCrop,
            focusedWindowID: wholeScreen ? nil : target,
            includeOverlays: includeOverlays)
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
            let diffBaseline = diffBaselines[session]
            if includeDiff {
                if let baseline = diffBaseline, baseline.rect.equalTo(region), baseline.window == target {
                    t = nowNs(); env["diff"] = differenceMap(baseline.image, cg,
                        origin: region.origin, scale: captureScale); tm["diff"] = msSince(t)
                    env["diff_baseline"] = "matched"
                } else {
                    env["diff"] = []
                    env["diff_baseline"] = diffBaseline == nil ? "missing" : "target-or-geometry-changed"
                }
            } else { env["diff"] = [] }
            if saveBaseline {
                if diffBaselines[session] == nil, diffBaselines.count >= maxDiffSessions,
                   let oldest = diffBaselines.min(by: { $0.value.used < $1.value.used })?.key {
                    diffBaselines.removeValue(forKey: oldest)
                }
                diffBaselines[session] = (region, cg, target, nowNs())
            }
            env["observation"] = ["id": UUID().uuidString, "session": session,
                "capturedAt": ISO8601DateFormatter().string(from: Date()),
                "coordinateSpace": "macos-global-points", "windowId": target.map { $0 as Any } ?? NSNull(),
                "captureScale": captureScale]
            if noOCR {
                env["ocr"] = []
            } else {
                t = nowNs(); let boxes = ocr(cg, scale: captureScale, fast: fast,
                                             origin: region.origin); tm["ocr"] = msSince(t)
                env["ocr"] = boxes
                if showOverlay && !reelActive { showOcrOverlay(boxes) }
            }
            let cursorMeta = (mt["cursor"] as? [String: Int]).map {
                CGPoint(x: $0["x"] ?? 0, y: $0["y"] ?? 0)
            }
            let visualOrigin = region.origin
            if reelActive || noVisual {
                // VNDetectContours can overlap SCRecordingOutput's
                // WindowServer/CoreImage work and has repeatedly taken the
                // host app down mid-reel. It is supplemental to OCR + AX, so
                // omit only this pass while preserving the audit screenshot.
                env["visual"] = []
                env["visual_suppressed"] = reelActive ? "screen-recording" : "requested"
                tm["visual"] = 0
            } else {
                t = nowNs(); env["visual"] = visualDetector.controls(cg, scale: captureScale,
                    origin: visualOrigin, focus: cursorOverride ?? cursorMeta)
                tm["visual"] = msSince(t)
                env["visual_cache_hit"] = visualDetector.cacheHit
            }
            t = nowNs()
            let marker = virtualCursor ? (cursorOverride ?? cursorMeta) : nil
            let jpg = thumbJPEG(cg, maxWidth: 1568, cursor: marker, crop: region) ?? Data()
            writeJPEG(jpg)
            jpgBytes = jpg.count
            tm["thumb"] = msSince(t)
            env["capture"] = "ok"
        } else {
            env["ocr"] = []
            writeJPEG(Data()) // truncate stale jpg
            // Either Screen Recording isn't granted yet, or this is < macOS 14.
            env["capture"] = "permission_needed"
            env["permission"] = "screen_recording"
        }
        env["thumb_bytes"] = jpgBytes
        grp.wait()                 // join the concurrent AX walk
        // Filter only after capture, against the region ScreenCaptureKit
        // actually returned. If a focused window vanished between the CG and
        // SC lookups, captureDisplay falls back to `screen`. AX retains its
        // explicit scope/windowId; never substitute other windows' controls.
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
        if env["capture"] as? String == "ok", captured.scope == "window",
           let targetBefore, targetBefore.windowID == target, nativeTarget() == targetBefore,
           let observation = env["observation"] as? [String: Any], let id = observation["id"] as? String {
            nativeBindings.record(session: session, id: id, target: targetBefore)
        }
        tm["ax"] = axMs
        // A denied/failed ScreenCaptureKit request has no image and therefore
        // no thumbnail timing. Keep the permission-needed envelope alive
        // instead of trapping while trying to report that failure.
        tm["wall"] = msSince(started)
        env["timings_ms"] = tm
        if let d = try? JSONSerialization.data(withJSONObject: env, options: []) {
            writeJSON(d)
        }
    }
}
