import AppKit

/// Borderless transparent NSWindow parked over a specific Terminal.app
/// window, tracking its bounds via osascript polling (~8Hz). Used as a
/// proof-of-concept "background" effect — Terminal itself doesn't expose
/// transparency or bg images via AppleScript, so we paint over it from
/// Slab instead. Mouse events pass through; the overlay never steals focus.
final class BackgroundOverlay {
    /// AppleScript window id of the Terminal window we're tracking. When the
    /// Terminal window goes away, the next poll returns "missing" and we
    /// auto-stop.
    let terminalWindowId: Int

    private let window: NSWindow
    private var pollTimer: Timer?
    /// Last bounds we pushed to the overlay window, in AppleScript coords
    /// (top-left origin). Skip setFrame when nothing changed so the window
    /// server isn't asked to redraw 8× per second on a stationary terminal.
    private var lastBounds: (Int, Int, Int, Int)?

    init(terminalWindowId: Int, image: NSImage?, tint: NSColor, alpha: CGFloat) {
        self.terminalWindowId = terminalWindowId

        let initial = NSRect(x: 0, y: 0, width: 1, height: 1)
        window = NSWindow(
            contentRect: initial,
            styleMask: [.borderless],
            backing: .buffered,
            defer: false
        )
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false
        window.ignoresMouseEvents = true
        window.collectionBehavior = [.canJoinAllSpaces, .stationary, .fullScreenAuxiliary]
        // Sit just above normal windows (terminal level + 1) so we paint over
        // the target without going into screen-saver / system-modal levels.
        window.level = NSWindow.Level(Int(CGWindowLevelForKey(.normalWindow)) + 1)

        let view = NSView(frame: initial)
        view.wantsLayer = true
        let layer = view.layer ?? CALayer()
        layer.contentsGravity = .resizeAspectFill
        if let img = image {
            layer.contents = img
        } else {
            layer.backgroundColor = tint.cgColor
        }
        layer.opacity = Float(alpha)
        view.layer = layer
        window.contentView = view
    }

    func start() {
        // 8Hz: smooth enough that drag-resize doesn't reveal too much terminal,
        // cheap enough that we don't spawn 30 osascript subprocesses/sec.
        let t = Timer.scheduledTimer(withTimeInterval: 0.125, repeats: true) { [weak self] _ in
            self?.syncBounds()
        }
        pollTimer = t
        RunLoop.main.add(t, forMode: .common)
        syncBounds()  // first paint immediately
        window.orderFront(nil)
    }

    func stop() {
        pollTimer?.invalidate()
        pollTimer = nil
        window.orderOut(nil)
    }

    /// Pull the target Terminal window's bounds via osascript and reframe
    /// the overlay to match. AppleScript bounds are {left, top, right,
    /// bottom} in top-left-origin pixel coords; AppKit windows are
    /// bottom-left, so flip Y against the screen height.
    private func syncBounds() {
        let id = terminalWindowId
        let script = """
        tell application "Terminal"
            try
                set b to bounds of window id \(id)
                return ((item 1 of b) as text) & "," & ((item 2 of b) as text) & "," & ((item 3 of b) as text) & "," & ((item 4 of b) as text)
            on error
                return "missing"
            end try
        end tell
        """
        DispatchQueue.global(qos: .userInteractive).async { [weak self] in
            let result = ShellRunner.run("/usr/bin/osascript", args: ["-e", script], timeout: 1)
            let trimmed = result.output.trimmingCharacters(in: .whitespacesAndNewlines)
            if trimmed == "missing" || result.status != 0 {
                DispatchQueue.main.async { self?.stop() }
                return
            }
            let parts = trimmed.split(separator: ",")
                .compactMap { Int($0.trimmingCharacters(in: .whitespaces)) }
            guard parts.count == 4 else { return }
            let l = parts[0], t = parts[1], r = parts[2], b = parts[3]
            DispatchQueue.main.async {
                guard let self = self else { return }
                if let last = self.lastBounds, last == (l, t, r, b) { return }
                self.lastBounds = (l, t, r, b)
                guard let screen = NSScreen.main else { return }
                let fullH = Int(screen.frame.height)
                // AppleScript top-left → AppKit bottom-left.
                let cocoaY = fullH - b
                let frame = NSRect(x: l, y: cocoaY, width: r - l, height: b - t)
                self.window.setFrame(frame, display: true)
            }
        }
    }
}

/// Per-process registry of active overlays, keyed by Terminal window id so
/// the menu action can toggle a window's overlay on/off cleanly.
final class BackgroundOverlayController {
    static let shared = BackgroundOverlayController()
    private var overlays: [Int: BackgroundOverlay] = [:]

    private init() {}

    /// Toggle a tint overlay over the front Terminal window. If one is
    /// already active for that window, this turns it off.
    func toggleFrontTerminal(tint: NSColor, image: NSImage? = nil, alpha: CGFloat = 0.35) {
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            let script = """
            tell application "Terminal"
                try
                    return (id of front window) as text
                on error
                    return "0"
                end try
            end tell
            """
            let result = ShellRunner.run("/usr/bin/osascript", args: ["-e", script], timeout: 2)
            let id = Int(result.output.trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
            guard id != 0 else { return }
            DispatchQueue.main.async {
                guard let self = self else { return }
                if let existing = self.overlays[id] {
                    existing.stop()
                    self.overlays.removeValue(forKey: id)
                    return
                }
                let overlay = BackgroundOverlay(
                    terminalWindowId: id,
                    image: image,
                    tint: tint,
                    alpha: alpha
                )
                overlay.start()
                self.overlays[id] = overlay
            }
        }
    }

    func clearAll() {
        for (_, ov) in overlays { ov.stop() }
        overlays.removeAll()
    }
}
