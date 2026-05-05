import AppKit
import WebKit

/// Liquid-glass webview window that loads https://aesthetic.computer —
/// brings the AC site onto a Mac user's desktop the same way the
/// ac-electron app does, but right out of Menu Band so it works the
/// instant the menubar piano launches. Singleton: a second invocation
/// raises the existing window instead of opening a new one.
final class AestheticWebWindowController: NSWindowController, NSWindowDelegate, WKNavigationDelegate, WKUIDelegate, WKScriptMessageHandler {
    private static var shared: AestheticWebWindowController?
    private static let defaultURL = URL(string: "https://aesthetic.computer")!
    /// JS bridge name — the page calls
    /// `window.webkit.messageHandlers.acClose.postMessage(...)`
    /// (or just emits the existing `ac-close-window` postMessage that
    /// the injected userscript forwards) to dismiss the window.
    private static let closeMessageName = "acClose"

    private var acWebView: WKWebView!
    private weak var glassView: NSView?

    /// Show the AC web window, creating it on first call. Subsequent
    /// calls focus the existing window instead of layering
    /// duplicates. Pass `rightOf:` (typically the About window's
    /// frame) to anchor the panel to the right of an existing
    /// window — keeps the two side-by-side instead of stacking.
    @discardableResult
    static func showOrFocus(rightOf anchor: NSRect? = nil,
                            gap: CGFloat = 12) -> AestheticWebWindowController {
        let controller = shared ?? AestheticWebWindowController()
        if shared == nil { shared = controller }
        if let anchor = anchor, let win = controller.window {
            controller.position(rightOf: anchor, gap: gap, window: win)
        }
        controller.showWindow(nil)
        controller.window?.orderFrontRegardless()
        NSApp.activate(ignoringOtherApps: true)
        return controller
    }

    private func position(rightOf anchor: NSRect, gap: CGFloat, window: NSWindow) {
        let myFrame = window.frame
        let screenFrame = window.screen?.visibleFrame ?? NSScreen.main?.visibleFrame ?? anchor
        var x = anchor.maxX + gap
        // Don't run off the right edge — flop to the left of the
        // anchor if the screen can't fit us on the right.
        if x + myFrame.width > screenFrame.maxX {
            let leftCandidate = anchor.minX - gap - myFrame.width
            if leftCandidate >= screenFrame.minX {
                x = leftCandidate
            } else {
                x = max(screenFrame.minX,
                        screenFrame.maxX - myFrame.width)
            }
        }
        let y = anchor.minY + (anchor.height - myFrame.height) / 2
        let clampedY = min(max(y, screenFrame.minY),
                           screenFrame.maxY - myFrame.height)
        window.setFrameOrigin(NSPoint(x: x, y: clampedY))
    }

    init() {
        // No `.closable` — the AC prompt's `-` command is the way out
        // (matches the ac-electron flip-view contract). We still hand
        // the OS Cmd+W via menu so power users have an escape hatch.
        let style: NSWindow.StyleMask = [
            .titled, .miniaturizable, .resizable, .fullSizeContentView,
        ]
        let win = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 420, height: 300),
            styleMask: style,
            backing: .buffered,
            defer: false
        )
        // Tiny corner panel — sits on top of everything, follows the
        // user across Spaces, never large enough to feel like a
        // browser. Pairs with the menubar piano as a second
        // instrument-shaped surface.
        win.minSize = NSSize(width: 280, height: 200)
        win.level = .statusBar
        win.collectionBehavior = [
            .canJoinAllSpaces,
            .fullScreenAuxiliary,
            .stationary,
            .ignoresCycle,
        ]
        win.title = "aesthetic.computer"
        win.titlebarAppearsTransparent = true
        win.titleVisibility = .hidden
        win.isMovableByWindowBackground = false
        win.isReleasedWhenClosed = false
        win.center()
        win.setFrameAutosaveName("AestheticWebWindow")
        // Transparent window backing so the glass beneath the
        // webview can actually peek through where AC has alpha
        // pixels (loading screens, transparent overlays).
        win.isOpaque = false
        win.backgroundColor = .clear
        win.hasShadow = true

        super.init(window: win)
        win.delegate = self
        // Hide every traffic-light — the AC prompt's `-` command is
        // the only way out; no close, minimize, or zoom chrome to
        // distract from the embedded site.
        for kind: NSWindow.ButtonType in [.closeButton, .miniaturizeButton, .zoomButton] {
            win.standardWindowButton(kind)?.isHidden = true
        }
        installContent(in: win)
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) { fatalError() }

    // MARK: - Layout

    private func installContent(in window: NSWindow) {
        let container = NSView()
        container.translatesAutoresizingMaskIntoConstraints = false
        container.wantsLayer = true
        // Soft rounded corners so the chrome reads as a unified
        // panel even though it's a normal NSWindow underneath.
        container.layer?.cornerRadius = 14
        container.layer?.masksToBounds = true
        if #available(macOS 10.15, *) {
            container.layer?.cornerCurve = .continuous
        }

        // Glass background — only meaningful on macOS 26+. Older
        // OSes get a flat dark backdrop so the webview still has a
        // legible bed when AC is loading.
        if #available(macOS 26.0, *) {
            let glass = AestheticWebGlassEffectView()
            glass.cornerRadius = 14
            glass.translatesAutoresizingMaskIntoConstraints = false
            container.addSubview(glass)
            NSLayoutConstraint.activate([
                glass.leadingAnchor.constraint(equalTo: container.leadingAnchor),
                glass.trailingAnchor.constraint(equalTo: container.trailingAnchor),
                glass.topAnchor.constraint(equalTo: container.topAnchor),
                glass.bottomAnchor.constraint(equalTo: container.bottomAnchor),
            ])
            self.glassView = glass
        } else {
            container.layer?.backgroundColor = NSColor.black
                .withAlphaComponent(0.85).cgColor
        }

        let config = WKWebViewConfiguration()
        config.preferences.javaScriptCanOpenWindowsAutomatically = false
        // Native bridge for the AC prompt's `-` close command.
        // Mirrors ac-electron's webview-preload.js — the page posts
        // `{ type: 'ac-close-window' }` (and we also expose
        // `window.acElectron.closeWindow()` for parity), the
        // injected userscript forwards it to the native handler,
        // which closes this window.
        let userController = WKUserContentController()
        userController.add(self, name: Self.closeMessageName)
        userController.addUserScript(WKUserScript(
            source: Self.closeBridgeScript,
            injectionTime: .atDocumentStart,
            forMainFrameOnly: false
        ))
        config.userContentController = userController
        let webView = WKWebView(frame: .zero, configuration: config)
        webView.translatesAutoresizingMaskIntoConstraints = false
        webView.navigationDelegate = self
        webView.uiDelegate = self
        webView.allowsBackForwardNavigationGestures = true
        webView.allowsMagnification = false
        webView.customUserAgent = Self.userAgent()
        // Make the webview transparent so the glass shows through
        // wherever the AC page renders alpha pixels. `drawsBackground`
        // is a long-stable WKWebView KVC key on macOS.
        webView.setValue(false, forKey: "drawsBackground")
        // Lower the perceived screen density so AC packs tighter
        // into the small panel — pageZoom < 1 shrinks the page's
        // CSS-pixel footprint to match the compact window size.
        webView.pageZoom = 0.75
        container.addSubview(webView)

        NSLayoutConstraint.activate([
            webView.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            webView.trailingAnchor.constraint(equalTo: container.trailingAnchor),
            webView.topAnchor.constraint(equalTo: container.topAnchor),
            webView.bottomAnchor.constraint(equalTo: container.bottomAnchor),
        ])

        self.acWebView = webView
        window.contentView = container
        webView.load(URLRequest(url: Self.defaultURL))
    }

    private static func userAgent() -> String {
        let version = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "0"
        return "AestheticComputerMenuBand/\(version) (Macintosh)"
    }

    /// JS shim injected at document-start so AC prompts can close
    /// the window the way they already close ac-electron windows:
    /// either `window.postMessage({ type: 'ac-close-window' }, '*')`
    /// or `window.acElectron.closeWindow()`.
    private static let closeBridgeScript: String = """
    (function () {
      function relayClose() {
        try { window.webkit.messageHandlers.acClose.postMessage('close'); }
        catch (_) {}
      }
      window.addEventListener('message', function (e) {
        if (e && e.data && e.data.type === 'ac-close-window') relayClose();
      });
      // Mirror ac-electron/webview-preload.js's API surface so AC
      // pieces detect "we're embedded" and use the same close hook.
      if (!window.acElectron) {
        window.acElectron = {
          isElectron: false,
          isMenuBand: true,
          platform: 'darwin',
          closeWindow: relayClose,
        };
      } else if (typeof window.acElectron.closeWindow !== 'function') {
        window.acElectron.closeWindow = relayClose;
      }
    })();
    """

    // MARK: - WKScriptMessageHandler

    func userContentController(_ userContentController: WKUserContentController,
                               didReceive message: WKScriptMessage) {
        guard message.name == Self.closeMessageName else { return }
        // Drop the singleton + tear down explicitly — orderOut keeps
        // the close path snappy (no hide animation) and matches what
        // a closeButton click would do under `.closable` style.
        window?.orderOut(nil)
        window?.close()
    }

    // MARK: - NSWindowDelegate

    func windowWillClose(_ notification: Notification) {
        // Drop the singleton so the next showOrFocus() rebuilds the
        // window fresh — keeps state simple and avoids a half-torn
        // webview lingering after close.
        if Self.shared === self {
            Self.shared = nil
        }
    }

    // MARK: - WKUIDelegate

    /// Open links targeting `_blank` in the user's default browser
    /// rather than spawning new in-app webviews. Keeps the embedded
    /// experience focused on AC itself.
    func webView(_ webView: WKWebView,
                 createWebViewWith configuration: WKWebViewConfiguration,
                 for navigationAction: WKNavigationAction,
                 windowFeatures: WKWindowFeatures) -> WKWebView? {
        if let url = navigationAction.request.url {
            NSWorkspace.shared.open(url)
        }
        return nil
    }
}

@available(macOS 26.0, *)
final class AestheticWebGlassEffectView: NSGlassEffectView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}
