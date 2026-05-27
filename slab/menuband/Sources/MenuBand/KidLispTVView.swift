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

    static let targetFPS: Double = 60.0

    func start() {
        guard !running else { return }
        running = true
        timer?.invalidate()
        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / Self.targetFPS,
                                     repeats: true) { [weak self] _ in
            self?.tick()
        }
        // Keep firing while popover scrolls/resizes.
        if let t = timer {
            RunLoop.current.add(t, forMode: .common)
        }
    }

    func stop() {
        running = false
        timer?.invalidate()
        timer = nil
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
