import AppKit
import GameController
import WebKit

private struct Options {
    var baseURL = ProcessInfo.processInfo.environment["AC_FIGHT_URL"] ?? "https://aesthetic.computer/fight"
    var watchURL = ProcessInfo.processInfo.environment["AC_FIGHT_RELOAD_ENDPOINT"]

    init(arguments: [String]) {
        var index = 1
        while index < arguments.count {
            switch arguments[index] {
            case "--url" where index + 1 < arguments.count:
                index += 1; baseURL = arguments[index]
            case "--reload-endpoint" where index + 1 < arguments.count:
                index += 1; watchURL = arguments[index]
            case "--help", "-h":
                print("usage: ac-fight-runner [--url URL] [--reload-endpoint URL]")
                exit(0)
            default: break
            }
            index += 1
        }
    }
}

private final class Runner: NSObject, NSApplicationDelegate, WKNavigationDelegate, WKScriptMessageHandler {
    private let options = Options(arguments: CommandLine.arguments)
    private var window: NSWindow!
    private var webView: WKWebView!
    private var controllerTimer: Timer?
    private var reloadTimer: Timer?
    private var reloadTag: String?

    func applicationDidFinishLaunching(_ notification: Notification) {
        let scripts = WKUserContentController()
        scripts.add(self, name: "acNative")
        scripts.addUserScript(WKUserScript(source: Self.hostScript, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        let configuration = WKWebViewConfiguration()
        configuration.userContentController = scripts
        configuration.websiteDataStore = .default()

        webView = WKWebView(frame: .zero, configuration: configuration)
        webView.customUserAgent = "AestheticComputerFightRunner/1 (Macintosh; native-host)"
        webView.navigationDelegate = self
        window = NSWindow(
            contentRect: NSScreen.main?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1280, height: 720),
            styleMask: [.titled, .closable, .resizable, .miniaturizable],
            backing: .buffered,
            defer: false
        )
        window.title = "Aesthetic Computer — Fight (native macOS host)"
        window.contentView = webView
        window.makeKeyAndOrderFront(nil)
        window.toggleFullScreen(nil)

        installMenu()
        observeControllers()
        load()
        if options.watchURL != nil { startReloadWatch() }
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }

    private func load() {
        guard let url = URL(string: options.baseURL) else {
            fputs("invalid --url: \(options.baseURL)\n", stderr); return
        }
        print("[fight-runner] loading \(url.absoluteString)")
        webView.load(URLRequest(url: url, cachePolicy: .reloadRevalidatingCacheData))
    }

    private func reload() {
        print("[fight-runner] reload")
        webView.reloadFromOrigin()
    }

    private func installMenu() {
        let menu = NSMenu()
        let appItem = NSMenuItem(); menu.addItem(appItem)
        let appMenu = NSMenu(); appItem.submenu = appMenu
        appMenu.addItem(withTitle: "Reload Fight", action: #selector(reloadAction), keyEquivalent: "r").target = self
        appMenu.addItem(withTitle: "Toggle Full Screen", action: #selector(NSWindow.toggleFullScreen(_:)), keyEquivalent: "f").target = window
        appMenu.addItem(.separator())
        appMenu.addItem(withTitle: "Quit", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        NSApp.mainMenu = menu
    }

    @objc private func reloadAction() { reload() }

    private func observeControllers() {
        if #available(macOS 11.3, *) { GCController.shouldMonitorBackgroundEvents = true }
        NotificationCenter.default.addObserver(forName: .GCControllerDidConnect, object: nil, queue: .main) { [weak self] note in
            guard let pad = note.object as? GCController else { return }
            print("[fight-runner] controller connected: \(pad.vendorName ?? "unknown") / \(pad.productCategory)")
            self?.publishControllers()
        }
        NotificationCenter.default.addObserver(forName: .GCControllerDidDisconnect, object: nil, queue: .main) { [weak self] note in
            guard let pad = note.object as? GCController else { return }
            print("[fight-runner] controller disconnected: \(pad.vendorName ?? "unknown")")
            self?.publishControllers()
        }
        GCController.startWirelessControllerDiscovery(completionHandler: {})
        for controller in GCController.controllers() { controller.handlerQueue = .main }
        controllerTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in self?.publishControllers() }
    }

    private func publishControllers() {
        let pads: [[String: Any]] = GCController.controllers().enumerated().map { index, controller in
            var pad: [String: Any] = [
                "index": index,
                "id": controller.vendorName ?? controller.productCategory,
                "category": controller.productCategory,
                "connected": true,
                "timestamp": ProcessInfo.processInfo.systemUptime * 1000,
            ]
            if let g = controller.extendedGamepad {
                pad["axes"] = [g.leftThumbstick.xAxis.value, g.leftThumbstick.yAxis.value,
                               g.rightThumbstick.xAxis.value, g.rightThumbstick.yAxis.value]
                pad["buttons"] = [
                    button(g.buttonA, "A"), button(g.buttonB, "B"), button(g.buttonX, "X"), button(g.buttonY, "Y"),
                    button(g.leftShoulder, "LB"), button(g.rightShoulder, "RB"),
                    button(g.leftTrigger, "LT"), button(g.rightTrigger, "RT"),
                    button(g.dpad.up, "UP"), button(g.dpad.down, "DOWN"),
                    button(g.dpad.left, "LEFT"), button(g.dpad.right, "RIGHT"),
                ]
            }
            return pad
        }
        guard JSONSerialization.isValidJSONObject(pads),
              let data = try? JSONSerialization.data(withJSONObject: pads),
              let json = String(data: data, encoding: .utf8) else { return }
        webView.evaluateJavaScript("window.__acNativeGamepads && window.__acNativeGamepads(\(json))")
    }

    private func button(_ input: GCControllerButtonInput, _ label: String) -> [String: Any] {
        ["label": label, "pressed": input.isPressed, "value": input.value]
    }

    private func startReloadWatch() {
        reloadTimer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] _ in self?.pollReloadEndpoint() }
        pollReloadEndpoint()
    }

    private func pollReloadEndpoint() {
        guard let text = options.watchURL, let url = URL(string: text) else { return }
        var request = URLRequest(url: url); request.cachePolicy = .reloadIgnoringLocalCacheData
        URLSession.shared.dataTask(with: request) { [weak self] data, response, _ in
            guard let self, let data,
                  let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode) else { return }
            let tag = http.value(forHTTPHeaderField: "ETag") ?? String(data: data, encoding: .utf8)
            DispatchQueue.main.async {
                defer { self.reloadTag = tag }
                if let old = self.reloadTag, old != tag { self.reload() }
            }
        }.resume()
    }

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        print("[fight-runner] ready: \(webView.url?.absoluteString ?? "unknown")")
        publishControllers()
    }

    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        print("[fight-web] \(message.body)")
    }

    private static let hostScript = #"""
    (() => {
      window.acNativeHost = { platform: 'macOS', runner: 'fight', version: 1 };
      window.__acNativeGamepads = (pads) => {
        window.acNativeGamepads = pads;
        window.dispatchEvent(new CustomEvent('ac-native-gamepads', { detail: pads }));
        const out = document.getElementById('ac-native-controller-overlay');
        if (out) out.textContent = pads.length ? pads.map(p => `${p.index + 1}: ${p.id}`).join('\n') : 'NO CONTROLLER';
      };
      addEventListener('DOMContentLoaded', () => {
        const out = document.createElement('pre');
        out.id = 'ac-native-controller-overlay'; out.textContent = 'NO CONTROLLER';
        out.style.cssText = 'position:fixed;right:12px;bottom:12px;z-index:2147483647;margin:0;padding:7px 9px;background:#0009;color:#fff;font:11px/1.3 monospace;pointer-events:none';
        document.documentElement.appendChild(out);
      });
      const original = console.log;
      console.log = (...args) => { original(...args); try { webkit.messageHandlers.acNative.postMessage(args.map(String).join(' ')); } catch (_) {} };
    })();
    """#
}

private let application = NSApplication.shared
private let runner = Runner()
application.delegate = runner
application.setActivationPolicy(.regular)
application.activate(ignoringOtherApps: true)
application.run()
