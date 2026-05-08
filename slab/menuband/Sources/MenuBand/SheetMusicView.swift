import AppKit
import WebKit

/// Live-transcription score card backed by Verovio. The view is a
/// transparent WKWebView shell hosting Resources/verovio/sheet.html;
/// each note onset/release on the Swift side becomes a JS call that
/// rebuilds the MusicXML score and re-renders to SVG. The displayed
/// engraving is the same one that prints — NSPrintOperation runs
/// against the WebKit print engine, so the printed page is the
/// rendered SVG, not a separate code path.
final class SheetMusicView: NSView, WKNavigationDelegate {
    weak var menuBand: MenuBandController?

    private var webView: WKWebView!
    private var isLoaded = false
    /// Note events that arrived before the JS layer was ready;
    /// flushed once Verovio's WASM finishes initializing.
    private var pendingEvents: [String] = []

    /// 8.5×11 letter aspect ratio so the card looks like a real
    /// printable page. Used by the popover to size width from height.
    static let letterAspect: CGFloat = 11.0 / 8.5

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setUpWebView()
    }

    required init?(coder: NSCoder) { nil }

    private func setUpWebView() {
        let config = WKWebViewConfiguration()
        // Allow loading the bundled HTML + JS via file:// without
        // the same-origin sandbox blocking WASM compilation.
        config.preferences.setValue(true, forKey: "allowFileAccessFromFileURLs")
        let webView = WKWebView(frame: bounds, configuration: config)
        webView.autoresizingMask = [.width, .height]
        webView.setValue(false, forKey: "drawsBackground")
        webView.navigationDelegate = self
        addSubview(webView)
        self.webView = webView
        loadSheet()
    }

    private func loadSheet() {
        // SwiftPM `.process("Resources")` flattens the verovio/
        // subdirectory into the bundle root, so we look up by
        // bare filename. The HTML's `<script src="…">` is also
        // relative, so it resolves to the sibling JS at root.
        guard let htmlURL = Bundle.module.url(
            forResource: "sheet",
            withExtension: "html"
        ) else {
            NSLog("SheetMusicView: sheet.html not found in bundle")
            return
        }
        let dir = htmlURL.deletingLastPathComponent()
        webView.loadFileURL(htmlURL, allowingReadAccessTo: dir)
    }

    // MARK: - WKNavigationDelegate

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        // Verovio needs another moment to finish WASM init — the
        // page calls Sheet.ready() which sets up an
        // onRuntimeInitialized hook. We probe for the toolkit and
        // flush pending events when it's reachable.
        pollForReady(retries: 60)
    }

    private func pollForReady(retries: Int) {
        guard retries > 0 else { return }
        webView.evaluateJavaScript("typeof Sheet !== 'undefined' && Sheet && (typeof verovio !== 'undefined') && (verovio.module && verovio.module.calledRun === true)") { [weak self] result, _ in
            guard let self = self else { return }
            if (result as? Bool) == true {
                self.isLoaded = true
                let pending = self.pendingEvents
                self.pendingEvents.removeAll()
                for js in pending {
                    self.webView.evaluateJavaScript(js, completionHandler: nil)
                }
            } else {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                    self.pollForReady(retries: retries - 1)
                }
            }
        }
    }

    // MARK: - Public API (called by MenuBandPopover)

    /// Append a note onset to the score.
    func recordNote(pitch: UInt8) {
        send("Sheet.noteOn(\(pitch))")
    }

    /// Mark the most recent un-released onset of `pitch` as released
    /// so its head shape stops growing.
    func releaseNote(pitch: UInt8) {
        send("Sheet.noteOff(\(pitch))")
    }

    /// Forward a metronome beat to the JS layer so it can keep its
    /// tick clock in lockstep with the visible metronome and snap
    /// onsets to the right beat slot.
    func beat() {
        send("Sheet.beat()")
    }

    func clearScore() {
        send("Sheet.clear()")
    }

    /// Open the macOS print panel printing the rendered Verovio
    /// engraving. WKWebView's WebKit print engine paginates the
    /// page natively — what you see is what prints.
    func printScore() {
        let info = NSPrintInfo.shared.copy() as! NSPrintInfo
        info.orientation = .portrait
        info.topMargin = 36
        info.bottomMargin = 36
        info.leftMargin = 36
        info.rightMargin = 36
        let op = webView.printOperation(with: info)
        op.showsPrintPanel = true
        op.showsProgressPanel = true
        op.run()
    }

    // MARK: - Internals

    private func send(_ js: String) {
        if !isLoaded {
            pendingEvents.append(js)
            return
        }
        webView.evaluateJavaScript(js, completionHandler: nil)
    }
}
