// KidLisp TV renderer backed by a live aesthetic.computer WKWebView.
//
// Drop-in alternative to the native `KidLispTVView`: same `KidLispScreen`
// surface, same screen-well frame, but it shows the REAL runtime — every
// builtin, fonts, network pieces, future language features — by loading
// `aesthetic.computer/$code` (or encoded inline source) with chrome
// stripped (`nogap`/`nolabel`).
//
// The TV is a passive display: a transparent click-catcher sits over the
// webview and forwards clicks to the "$ pieces" chooser, matching the
// native view's interaction (you pick pieces, you don't poke the canvas).

import AppKit
import WebKit

final class KidLispTVWebView: NSView, KidLispScreen {

    private let webView: WKWebView
    private let clickCatcher: ClickCatcher
    /// Last URL handed to `load` — guards against redundant reloads when
    /// `show` is called with the same selection (e.g. popover re-open).
    private var loadedURL: URL?

    // Unused by the webview (the page generates its own audio/visuals),
    // but part of `KidLispScreen` so the panel/wiring stays uniform.
    var ampProvider: (() -> Double)?
    var busyProvider: (() -> Bool)?

    var onScreenClick: ((NSView, NSEvent) -> Void)? {
        didSet {
            clickCatcher.onClick = onScreenClick
            window?.invalidateCursorRects(for: clickCatcher)
        }
    }

    init(code: String?, source: String) {
        let config = WKWebViewConfiguration()
        // Pieces start audio/animation without a click — the TV should
        // come alive on its own like the native renderer does.
        config.mediaTypesRequiringUserActionForPlayback = []
        config.preferences.javaScriptCanOpenWindowsAutomatically = false
        self.webView = WKWebView(frame: .zero, configuration: config)
        self.clickCatcher = ClickCatcher(frame: .zero)
        super.init(frame: .zero)

        wantsLayer = true
        layer?.backgroundColor = NSColor.black.cgColor

        webView.autoresizingMask = [.width, .height]
        // Transparent so the bezel's black screen well shows through any
        // alpha the piece renders (same trick as AestheticWebWindow).
        webView.setValue(false, forKey: "drawsBackground")
        webView.allowsMagnification = false
        // Hard pixels to match the chunky LCD look of the native TV.
        webView.layer?.magnificationFilter = .nearest
        addSubview(webView)

        clickCatcher.autoresizingMask = [.width, .height]
        clickCatcher.onClick = nil
        addSubview(clickCatcher)  // on top of the webview

        show(code: code, source: source)
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) not supported") }

    override func layout() {
        super.layout()
        webView.frame = bounds
        clickCatcher.frame = bounds
    }

    func show(code: String?, source: String) {
        let url = KidLispURL.url(code: code, source: source, embed: true)
        guard url != loadedURL else { return }
        loadedURL = url
        webView.load(URLRequest(url: url))
    }

    /// Transparent overlay that grabs every click (the TV canvas isn't
    /// interactive) and forwards it to the chooser callback.
    final class ClickCatcher: NSView {
        var onClick: ((NSView, NSEvent) -> Void)?

        override func mouseDown(with event: NSEvent) {
            if let cb = onClick { cb(self, event) } else { super.mouseDown(with: event) }
        }
        // Capture clicks even though a WKWebView is below us.
        override func hitTest(_ point: NSPoint) -> NSView? {
            return onClick != nil ? self : nil
        }
        override func resetCursorRects() {
            if onClick != nil { addCursorRect(bounds, cursor: .pointingHand) }
        }
    }
}
