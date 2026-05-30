// A tiny KidLisp "television" — runs a KidLisp source through the
// in-process Swift evaluator at 30fps and draws the framebuffer
// nearest-neighbor scaled into the view. Lives inside MenuBand's
// popover (see MenuBandPopover.swift).
//
// Drawing path: KLFramebuffer (RGBA8 byte buffer) → CGImage →
// CGContext.draw with interpolationQuality = .none for the chunky
// pixelated look.

import AppKit
import Foundation

final class KidLispTVView: NSView {

    private var source: String
    private let resWidth: Int
    private let resHeight: Int
    private var timer: Timer?
    /// Fired on mouseDown inside the screen well. Owner (AppDelegate)
    /// uses this to pop a "$" pieces chooser. Nil = clicks are
    /// swallowed by NSView's default handling. didSet invalidates
    /// cursor rects so the pointing-hand cursor switches on/off
    /// immediately when the callback is attached after the view is
    /// already in a window.
    var onScreenClick: ((NSView, NSEvent) -> Void)? {
        didSet { window?.invalidateCursorRects(for: self) }
    }
    /// Per-frame audio amplitude source. Returns a value in (roughly)
    /// 0–10 to match kidlisp.mjs's `amp` scale. Polled in `tick()`
    /// before the evaluator runs the next frame so pieces that read
    /// `amp` see the freshest reading.
    var ampProvider: (() -> Double)?
    /// Returns true while the app is in a latency-critical state
    /// (notes lit, pitch-bend gesture engaged, etc.). The TV skips
    /// its render tick while busy — kidlisp.mjs treats frames as a
    /// monotonic clock so a handful of dropped frames is invisible
    /// to most pieces and the user gets the main-thread time back
    /// for audio scheduling.
    var busyProvider: (() -> Bool)?
    // Persistent across frames — kidlisp.mjs's canonical model. Pieces that
    // don't wipe accumulate, timing tokens remember their last fire time,
    // the frame counter advances. We allocate the evaluator once and call
    // `runFrame(source)` every tick on the same backing buffer.
    private let fb: KLFramebuffer
    private let evaluator: KLEvaluator
    private var running: Bool = false

    init(source: String = "purple, ink, line, blur 5",
         resWidth: Int = 192,
         resHeight: Int = 120) {
        self.source = source
        self.resWidth = max(8, resWidth)
        self.resHeight = max(8, resHeight)
        self.fb = KLFramebuffer(width: self.resWidth, height: self.resHeight)
        self.evaluator = KLEvaluator(fb: self.fb)
        super.init(frame: .zero)
        wantsLayer = true
        layer?.backgroundColor = NSColor.black.cgColor
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) not supported") }

    override var isFlipped: Bool { true }   // (0,0) top-left, matches framebuffer

    override func mouseDown(with event: NSEvent) {
        if let handler = onScreenClick {
            handler(self, event)
        } else {
            super.mouseDown(with: event)
        }
    }

    override func resetCursorRects() {
        // Pointing-hand on hover so the screen reads as a clickable
        // surface — pairs with the chooser hook above.
        if onScreenClick != nil {
            addCursorRect(bounds, cursor: .pointingHand)
        }
    }

    func setSource(_ s: String) {
        source = s
        evaluator.resetBackdrops()
        renderOneFrame()
        needsDisplay = true
    }

    /// Target frame rate when the main thread has slack. Adaptive
    /// throttling (see `tick`) drops below this when the timer is
    /// being scheduled late — which is the cheap signal that the
    /// main run loop is overcommitted.
    static let targetFPS: Double = 30.0
    /// Floor for the adaptive throttler. Below this the TV would
    /// read as a slideshow; keep it animated even at the cost of
    /// a frame or two of audio attention.
    private static let minFPS: Double = 8.0
    /// When the measured frame interval exceeds the scheduled
    /// interval by this ratio (smoothed), we're being scheduled
    /// late → step the FPS down to give the main loop slack.
    private static let lagThreshold: Double = 1.6
    /// Headroom ratio under which we step the FPS back up toward
    /// target. Slightly above 1.0 so a single fast frame doesn't
    /// undo a multi-frame throttle decision.
    private static let headroomThreshold: Double = 1.05

    private var currentFPS: Double = targetFPS
    private var lastTickAt: CFTimeInterval = 0
    private var smoothedInterval: Double = 0
    /// Sticky-decay flag for the on-canvas "BUSY" indicator. Pure
    /// busy bool flickers at the tick rate; latch for a few frames
    /// so the badge is readable.
    private var busyHoldFrames: Int = 0

    func start() {
        guard !running else { return }
        running = true
        currentFPS = Self.targetFPS
        smoothedInterval = 1.0 / currentFPS
        lastTickAt = 0
        scheduleTimer(fps: currentFPS)
    }

    func stop() {
        running = false
        timer?.invalidate()
        timer = nil
    }

    private func scheduleTimer(fps: Double) {
        timer?.invalidate()
        let t = Timer.scheduledTimer(withTimeInterval: 1.0 / fps,
                                     repeats: true) { [weak self] _ in
            self?.tick()
        }
        // Keep firing while popover scrolls/resizes.
        RunLoop.current.add(t, forMode: .common)
        timer = t
        currentFPS = fps
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if window != nil {
            renderOneFrame()
            needsDisplay = true   // paint frame 1 before the timer's first tick
            start()
        } else {
            stop()
        }
    }

    private func tick() {
        let now = CACurrentMediaTime()
        // Adaptive throttle: EMA the scheduled-to-fired interval
        // and compare to the configured tick interval. The Timer
        // class fires LATE (never early) when the main run loop is
        // saturated, so a consistently elevated ratio is the
        // cheapest possible "main thread is overcommitted" signal.
        if lastTickAt > 0 {
            let actual = now - lastTickAt
            smoothedInterval = smoothedInterval * 0.8 + actual * 0.2
            let expected = 1.0 / currentFPS
            let ratio = smoothedInterval / expected
            if ratio > Self.lagThreshold, currentFPS > Self.minFPS {
                let next = max(Self.minFPS, currentFPS - 6)
                if next < currentFPS - 0.5 {
                    scheduleTimer(fps: next)
                    smoothedInterval = 1.0 / next
                    NSLog("MenuBand TV: throttle %.1f → %.1f fps (lag %.2fx)",
                          currentFPS, next, ratio)
                }
            } else if ratio < Self.headroomThreshold, currentFPS < Self.targetFPS {
                let next = min(Self.targetFPS, currentFPS + 2)
                if next > currentFPS + 0.5 {
                    scheduleTimer(fps: next)
                    smoothedInterval = 1.0 / next
                }
            }
        }
        lastTickAt = now

        // Yield while the app is doing latency-critical work. The
        // framebuffer is left as-is — the TV reads as briefly
        // paused, which is far less distracting than the audio
        // glitching because the KidLisp eval ate the main thread.
        if busyProvider?() == true {
            busyHoldFrames = 8
            needsDisplay = true  // repaint to flip BUSY badge on
            return
        }
        if busyHoldFrames > 0 { busyHoldFrames -= 1 }

        renderOneFrame()
        needsDisplay = true
    }

    private func renderOneFrame() {
        if let provider = ampProvider {
            evaluator.amp = provider()
        }
        evaluator.runFrame(source)
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        if let img = makeCGImage(from: fb) {
            ctx.interpolationQuality = .none
            ctx.draw(img, in: bounds)
        } else {
            NSColor.black.setFill()
            bounds.fill()
        }
        drawPerformanceBadge()
    }

    /// Tiny bottom-right corner readout: current effective FPS, plus
    /// a "BUSY" pill when the TV is skipping ticks for the audio
    /// path. Latched for a few frames after the last skip so the
    /// indicator is readable instead of flickering at the tick rate.
    private func drawPerformanceBadge() {
        let fpsAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 9, weight: .semibold),
            .foregroundColor: currentFPS >= Self.targetFPS - 0.5
                ? NSColor.white.withAlphaComponent(0.55)
                : NSColor(srgbRed: 1.0, green: 0.7, blue: 0.3, alpha: 0.85),
        ]
        let fps = NSAttributedString(
            string: String(format: "%2.0f", currentFPS), attributes: fpsAttrs)
        let pad: CGFloat = 4
        let fpsSize = fps.size()
        let fpsOrigin = NSPoint(x: bounds.maxX - fpsSize.width - pad,
                                y: bounds.maxY - fpsSize.height - pad)
        // Subtle dark scrim so the digits stay legible against any
        // KidLisp output (incl. all-white frames).
        let scrim = NSRect(x: fpsOrigin.x - 2,
                           y: fpsOrigin.y - 1,
                           width: fpsSize.width + 4,
                           height: fpsSize.height + 1)
        NSColor.black.withAlphaComponent(0.5).setFill()
        NSBezierPath(roundedRect: scrim, xRadius: 2, yRadius: 2).fill()
        fps.draw(at: fpsOrigin)

        if busyHoldFrames > 0 {
            let busyAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 8, weight: .heavy),
                .foregroundColor: NSColor(srgbRed: 1.0, green: 0.4, blue: 0.4, alpha: 0.9),
            ]
            let busy = NSAttributedString(string: "BUSY", attributes: busyAttrs)
            let bSize = busy.size()
            let bOrigin = NSPoint(x: bounds.maxX - bSize.width - pad,
                                  y: fpsOrigin.y - bSize.height - 1)
            let bScrim = NSRect(x: bOrigin.x - 2,
                                y: bOrigin.y - 1,
                                width: bSize.width + 4,
                                height: bSize.height + 1)
            NSColor.black.withAlphaComponent(0.6).setFill()
            NSBezierPath(roundedRect: bScrim, xRadius: 2, yRadius: 2).fill()
            busy.draw(at: bOrigin)
        }
    }

    private func makeCGImage(from fb: KLFramebuffer) -> CGImage? {
        let data = Data(fb.pixels)
        guard let provider = CGDataProvider(data: data as CFData) else { return nil }
        let cs = CGColorSpaceCreateDeviceRGB()
        let info = CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedLast.rawValue)
        return CGImage(width: fb.width,
                       height: fb.height,
                       bitsPerComponent: 8,
                       bitsPerPixel: 32,
                       bytesPerRow: fb.width * 4,
                       space: cs,
                       bitmapInfo: info,
                       provider: provider,
                       decode: nil,
                       shouldInterpolate: false,
                       intent: .defaultIntent)
    }
}
