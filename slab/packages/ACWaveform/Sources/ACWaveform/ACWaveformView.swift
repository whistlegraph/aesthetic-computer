import QuartzCore
import WebKit
#if canImport(AppKit)
import AppKit
public typealias ACWaveformPlatformView = NSView
#else
import UIKit
public typealias ACWaveformPlatformView = UIView
#endif

/// The native half: a thin vertical strip that draws the slices a preview's
/// page posts, scrolling upward, oldest at the top. It shows only while sound
/// plays and fades out on silence. It takes no input.
///
/// `attach(to:)` wires one web view's content controller to this strip; call
/// it once, before the web view is created from that configuration.
public final class ACWaveformView: ACWaveformPlatformView {
    /// Seconds of history the strip holds; see `ACWaveformHistory.span(bpm:)`.
    public var span: TimeInterval = 4
    /// Muted, fullscreen, reduced motion: the host says when to stay dark.
    public var isSuppressed = false { didSet { if isSuppressed { clear() } } }
    public var strokeColor: CGColor = CGColor(red: 1, green: 0.47, blue: 0.70, alpha: 1) {
        didSet { shape.strokeColor = strokeColor }
    }
    /// Opacity while sounding — the desktop's .65.
    public var soundingOpacity: Float = 0.65

    private let shape = CAShapeLayer()
    private var history = ACWaveformHistory()
    private var timer: Timer?

    public override init(frame: CGRect) {
        super.init(frame: frame)
        setUp()
    }

    public required init?(coder: NSCoder) {
        super.init(coder: coder)
        setUp()
    }

    deinit { timer?.invalidate() }

    public func attach(to controller: WKUserContentController) {
        controller.addUserScript(ACWaveformScript.userScript())
        controller.add(Receiver(strip: self), name: ACWaveformScript.handlerName)
    }

    /// Forget everything drawn, as when the page navigates away.
    public func clear() {
        history.removeAll()
        stop()
        shape.path = nil
        shape.opacity = 0
    }

    func receive(_ body: Any) {
        guard !isSuppressed else { return }
        let now = CACurrentMediaTime()
        if let pair = body as? [Double], pair.count == 2 {
            history.append(low: pair[0], high: pair[1], at: now)
        } else {
            history.appendSilence(at: now)
        }
        if timer == nil && history.isSounding { start() }
    }

    private func setUp() {
        #if canImport(AppKit)
        wantsLayer = true
        layer?.addSublayer(shape)
        #else
        isUserInteractionEnabled = false
        backgroundColor = .clear
        layer.addSublayer(shape)
        #endif
        shape.fillColor = nil
        shape.strokeColor = strokeColor
        shape.lineWidth = 1
        shape.lineJoin = .round
        shape.lineCap = .round
        shape.opacity = 0
        shape.actions = ["path": NSNull(), "bounds": NSNull(), "position": NSNull()]
    }

    #if canImport(AppKit)
    public override var isFlipped: Bool { true }
    public override func hitTest(_ point: NSPoint) -> NSView? { nil }
    public override func layout() { super.layout(); fit() }
    #else
    public override func layoutSubviews() { super.layoutSubviews(); fit() }
    #endif

    private func fit() {
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        shape.frame = bounds
        CATransaction.commit()
    }

    private func start() {
        shape.removeAnimation(forKey: "fade")
        shape.opacity = soundingOpacity
        let timer = Timer(timeInterval: 1.0 / 60, repeats: true) { [weak self] _ in self?.draw() }
        RunLoop.main.add(timer, forMode: .common)
        self.timer = timer
        draw()
    }

    private func stop() {
        timer?.invalidate()
        timer = nil
    }

    private func draw() {
        let now = CACurrentMediaTime()
        history.prune(now: now, span: span)
        guard !isSuppressed, let path = history.path(in: bounds.size, now: now, span: span) else {
            stop()
            let fade = CABasicAnimation(keyPath: "opacity")
            fade.fromValue = shape.opacity
            fade.toValue = 0
            fade.duration = 0.07
            shape.opacity = 0
            shape.add(fade, forKey: "fade")
            history.removeAll()
            return
        }
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        shape.path = path
        CATransaction.commit()
    }
}

/// WKUserContentController holds its handlers strongly; this keeps it from
/// holding the strip.
private final class Receiver: NSObject, WKScriptMessageHandler {
    weak var strip: ACWaveformView?
    init(strip: ACWaveformView) { self.strip = strip }
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard message.frameInfo.isMainFrame else { return }
        strip?.receive(message.body)
    }
}
