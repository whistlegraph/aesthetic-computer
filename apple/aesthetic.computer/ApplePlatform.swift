import SwiftUI
import WebKit

#if os(macOS)
import AppKit
typealias ACApplication = NSApplication
typealias ACApplicationDelegate = NSApplicationDelegate
typealias ACImage = NSImage

// Explicit local automation only: the normal app never reads scripts or records
// keystrokes. A probe uses an ephemeral website store and exits its own process.
@MainActor enum MacPromptLatencyProbe {
    static func argument(_ flag: String) -> String? {
        let args = ProcessInfo.processInfo.arguments
        guard let index = args.firstIndex(of: flag), args.indices.contains(index + 1) else { return nil }
        return args[index + 1]
    }
    static var enabled: Bool { argument("--ac-input-probe") != nil }
    private static var started = false

    static func configure(_ configuration: WKWebViewConfiguration) {
        guard let path = argument("--ac-input-probe"),
              let script = try? String(contentsOfFile: path, encoding: .utf8) else { return }
        configuration.websiteDataStore = .nonPersistent()
        if ProcessInfo.processInfo.arguments.contains("--ac-input-baseline") {
            configuration.userContentController.addUserScript(WKUserScript(
                source: "window.acInputLatencyBaseline = true;",
                injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        configuration.userContentController.addUserScript(WKUserScript(
            source: script, injectionTime: .atDocumentStart, forMainFrameOnly: true))
    }

    static func run(in webView: WKWebView?) {
        guard enabled, !started, let webView else { return }
        started = true
        let interval = max(8, min(1000, Int(argument("--ac-input-interval-ms") ?? "80") ?? 80))
        let resultPath = argument("--ac-input-result") ?? "/tmp/ac-input-latency.json"
        Task { @MainActor in
            do {
                try await Task.sleep(for: .seconds(1))
                guard let window = webView.window else { throw CocoaError(.coderInvalidValue) }
                window.title = "prompt · input latency test"
                window.makeKeyAndOrderFront(nil)
                NSApp.activate(ignoringOtherApps: true)
                window.makeFirstResponder(webView)
                try await Task.sleep(for: .milliseconds(300))
                var sentEpochMs: [Double] = []
                for index in 0..<60 {
                    sentEpochMs.append(Date().timeIntervalSince1970 * 1000)
                    for type in [NSEvent.EventType.keyDown, .keyUp] {
                        guard let event = NSEvent.keyEvent(
                            with: type, location: .zero, modifierFlags: [],
                            timestamp: ProcessInfo.processInfo.systemUptime,
                            windowNumber: window.windowNumber, context: nil,
                            characters: "a", charactersIgnoringModifiers: "a",
                            isARepeat: index > 0 && interval < 30, keyCode: 0) else { continue }
                        NSApp.sendEvent(event)
                    }
                    try await Task.sleep(for: .milliseconds(interval))
                }
                try await Task.sleep(for: .seconds(1))
                let result = try await webView.evaluateJavaScript("JSON.stringify(window.acInputLatencyProbe.result())")
                guard let json = result as? String, let data = json.data(using: .utf8),
                      var report = try JSONSerialization.jsonObject(with: data) as? [String: Any]
                else { throw CocoaError(.coderInvalidValue) }
                // Native send -> DOM keydown: the AppKit, input-context and
                // WebContent IPC hop that the page clock cannot see.
                if let samples = report["samples"] as? [[String: Any]], samples.count == sentEpochMs.count {
                    report["nativeToKeydown"] = zip(sentEpochMs, samples).compactMap { sent, sample in
                        (sample["keydownEpoch"] as? Double).map { $0 - sent }
                    }
                }
                // Verify editing through the same native event path. This
                // runs after timing collection and never submits a command.
                for (characters, code) in [("\u{7F}", 51), ("\u{7F}", 51), ("\u{7F}", 51),
                                           ("\u{F702}", 123), ("b", 11)] {
                    for type in [NSEvent.EventType.keyDown, .keyUp] {
                        if let event = NSEvent.keyEvent(with: type, location: .zero, modifierFlags: [],
                            timestamp: ProcessInfo.processInfo.systemUptime, windowNumber: window.windowNumber,
                            context: nil, characters: characters, charactersIgnoringModifiers: characters,
                            isARepeat: false, keyCode: UInt16(code)) { NSApp.sendEvent(event) }
                    }
                    try await Task.sleep(for: .milliseconds(150))
                }
                report["editingCheck"] = try await webView.evaluateJavaScript(
                    "document.querySelector('#software-keyboard-input').value === 'a'.repeat(56) + 'ba'")
                let reportData = try JSONSerialization.data(withJSONObject: report, options: [.prettyPrinted, .sortedKeys])
                try reportData.write(to: URL(fileURLWithPath: resultPath), options: .atomic)
                let snapshot = try await webView.takeSnapshot(configuration: nil)
                if let tiff = snapshot.tiffRepresentation,
                   let bitmap = NSBitmapImageRep(data: tiff),
                   let png = bitmap.representation(using: .png, properties: [:]) {
                    try png.write(to: URL(fileURLWithPath: resultPath + ".png"))
                }
                print("AC input benchmark: \(resultPath)")
            } catch {
                try? "{\"error\":\"Native input probe failed\"}".write(toFile: resultPath, atomically: true, encoding: .utf8)
                NSLog("AC input benchmark failed: %@", error.localizedDescription)
            }
            NSApp.terminate(nil)
        }
    }
}

// 📊 Frame meter: the runtime reports display frames, painted frames and the
// worker's frame time at 4 Hz while enabled; the title bar draws them as a bar.
@MainActor final class FrameMeter: ObservableObject {
    static let shared = FrameMeter()
    private static let key = "acFrameMeter"
    @Published var enabled = UserDefaults.standard.bool(forKey: key) {
        didSet {
            UserDefaults.standard.set(enabled, forKey: Self.key)
            if !enabled { fps = 0; paints = 0; frameMs = 0; worstMs = 0 }
            sync(AppDelegate.shared?.appWebView)
        }
    }
    @Published private(set) var fps = 0.0
    @Published private(set) var paints = 0.0
    @Published private(set) var frameMs = 0.0
    @Published private(set) var worstMs = 0.0

    func sync(_ webView: WKWebView?) {
        webView?.evaluateJavaScript("window.acFrameMeter?.(\(enabled))") { _, _ in }
    }

    func update(_ body: [String: Any]) {
        guard enabled else { return }
        func value(_ name: String) -> Double {
            let number = (body[name] as? Double) ?? 0
            return number.isFinite ? max(0, min(number, 1000)) : 0
        }
        fps = value("fps"); paints = value("paints")
        frameMs = value("frameMs"); worstMs = value("worstMs")
    }
}

// A horizontal bar in the title bar: its length is painted frames against the
// display's rate, tinted from the window's own backdrop color.
struct FrameMeterView: View {
    @ObservedObject private var meter = FrameMeter.shared
    @ObservedObject private var theme = AppleWindowTheme.shared

    var body: some View {
        let ink: Color = theme.colorScheme == .dark ? .white : .black
        let rate = Double(NSScreen.main?.maximumFramesPerSecond ?? 60)
        HStack(spacing: 6) {
            Text(verbatim: "\(Int(meter.paints.rounded()))/\(Int(meter.fps.rounded()))")
                .frame(width: 44, alignment: .trailing)
            GeometryReader { bar in
                ZStack(alignment: .leading) {
                    Capsule().fill(ink.opacity(0.12))
                    Capsule().fill(ink.opacity(0.45))
                        .frame(width: bar.size.width * min(meter.paints / rate, 1))
                }
            }
            .frame(width: 96, height: 4)
            Text(verbatim: String(format: "%.1f ms", meter.frameMs))
                .frame(width: 48, alignment: .leading)
        }
        .font(.system(size: 10, design: .monospaced))
        .foregroundStyle(ink.opacity(0.6))
        .padding(.horizontal, 10)
        .frame(height: 28)
        .opacity(meter.enabled ? 1 : 0)
        .help("Painted frames / display frames per second; worker frame time")
    }
}

// WebKit can reset AppKit's cursor when the invisible prompt editor gains
// focus, even while CSS still reports the SVG cursor. Reassert the same image
// after mouse dispatch so native focus changes cannot replace it.
final class ACNativeWebView: WKWebView {
    private var cursorMonitor: Any?
    private var cursorTrackingArea: NSTrackingArea?
    private lazy var acCursor = NSCursor(image: NSImage(named: "ACCursor")!,
                                        hotSpot: NSPoint(x: 12, y: 12))

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if let cursorMonitor { NSEvent.removeMonitor(cursorMonitor) }
        cursorMonitor = nil
        guard let window else { return }
        if !window.titlebarAccessoryViewControllers.contains(where: { $0.view is NSHostingView<FrameMeterView> }) {
            let accessory = NSTitlebarAccessoryViewController()
            accessory.view = NSHostingView(rootView: FrameMeterView())
            accessory.view.frame.size = accessory.view.fittingSize
            accessory.layoutAttribute = .trailing
            window.addTitlebarAccessoryViewController(accessory)
        }
        cursorMonitor = NSEvent.addLocalMonitorForEvents(matching: [
            .mouseMoved, .leftMouseDown, .leftMouseUp, .leftMouseDragged,
            .rightMouseDown, .rightMouseUp, .otherMouseDown, .otherMouseUp
        ]) { [weak self] event in
            guard let self, event.window === self.window else { return event }
            self.restoreACCursor()
            DispatchQueue.main.async { [weak self] in self?.restoreACCursor() }
            if event.type == .leftMouseUp || event.type == .leftMouseDown {
                // WebKit's editor focus reply crosses a process boundary and
                // may arrive after AppKit has finished dispatching the click.
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) { [weak self] in
                    self?.restoreACCursor()
                }
            }
            return event
        }
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let cursorTrackingArea { removeTrackingArea(cursorTrackingArea) }
        let area = NSTrackingArea(rect: bounds,
                                 options: [.activeInKeyWindow, .inVisibleRect, .cursorUpdate, .mouseEnteredAndExited],
                                 owner: self, userInfo: nil)
        addTrackingArea(area)
        cursorTrackingArea = area
    }

    override func cursorUpdate(with event: NSEvent) { restoreACCursor() }
    override func mouseEntered(with event: NSEvent) { restoreACCursor() }

    override func resetCursorRects() {
        super.resetCursorRects()
        addCursorRect(bounds, cursor: acCursor)
    }

    private func restoreACCursor() {
        guard let window, window.isKeyWindow,
              bounds.contains(convert(window.mouseLocationOutsideOfEventStream, from: nil)) else { return }
        acCursor.set()
    }

    deinit {
        if let cursorMonitor { NSEvent.removeMonitor(cursorMonitor) }
    }
}

protocol ACWebViewRepresentable: NSViewRepresentable where NSViewType == WKWebView {
    func makeWebView(context: Context) -> WKWebView
    func updateWebView(_ view: WKWebView, context: Context)
}
extension ACWebViewRepresentable {
    func makeNSView(context: Context) -> WKWebView { makeWebView(context: context) }
    func updateNSView(_ view: WKWebView, context: Context) { updateWebView(view, context: context) }
}
#else
import UIKit
typealias ACApplication = UIApplication
typealias ACApplicationDelegate = UIApplicationDelegate
typealias ACImage = UIImage

protocol ACWebViewRepresentable: UIViewRepresentable where UIViewType == WKWebView {
    func makeWebView(context: Context) -> WKWebView
    func updateWebView(_ view: WKWebView, context: Context)
}
extension ACWebViewRepresentable {
    func makeUIView(context: Context) -> WKWebView { makeWebView(context: context) }
    func updateUIView(_ view: WKWebView, context: Context) { updateWebView(view, context: context) }
}
#endif

extension Image {
    init(acImage: ACImage) {
        #if os(macOS)
        self.init(nsImage: acImage)
        #else
        self.init(uiImage: acImage)
        #endif
    }
}

#if os(macOS)
@MainActor final class AppleWindowTheme: ObservableObject {
    static let shared = AppleWindowTheme()
    @Published var color = NSColor(white: 32 / 255, alpha: 1)
    var colorScheme: ColorScheme {
        let rgb = color.usingColorSpace(.sRGB) ?? color
        func linear(_ value: CGFloat) -> CGFloat {
            value <= 0.04045 ? value / 12.92 : pow((value + 0.055) / 1.055, 2.4)
        }
        let luminance = 0.2126 * linear(rgb.redComponent)
            + 0.7152 * linear(rgb.greenComponent) + 0.0722 * linear(rgb.blueComponent)
        return luminance < 0.179 ? .dark : .light
    }
}
#endif

@MainActor enum ApplePlatform {
    static func colorWindow(_ rgb: [Double], webView: WKWebView) {
        #if os(macOS)
        guard rgb.count == 3, rgb.allSatisfy({ $0.isFinite && (0...255).contains($0) }),
              let window = webView.window else { return }
        let color = NSColor(srgbRed: rgb[0] / 255, green: rgb[1] / 255,
                            blue: rgb[2] / 255, alpha: 1)
        if AppleWindowTheme.shared.color != color { AppleWindowTheme.shared.color = color }
        window.titlebarAppearsTransparent = true
        window.titlebarSeparatorStyle = .none
        window.backgroundColor = color
        // Keep the native title and traffic lights legible on either backing.
        // Theme only the title bar. Changing the window appearance also changes
        // WKWebView's prefers-color-scheme and feeds its next wipe back here.
        window.standardWindowButton(.closeButton)?.superview?.appearance =
            NSAppearance(named: AppleWindowTheme.shared.colorScheme == .dark ? .darkAqua : .aqua)
        webView.underPageBackgroundColor = color
        #endif
    }

    static func openExternal(_ url: URL) {
        if let fallback = AppRoute.mimeFallback(for: url) {
            #if os(macOS)
            if !NSWorkspace.shared.open(url) { NSWorkspace.shared.open(fallback) }
            #else
            UIApplication.shared.open(url, options: [:]) { opened in
                if !opened { UIApplication.shared.open(fallback) }
            }
            #endif
            return
        }
        guard ["https", "http", "mailto", "tel"].contains(url.scheme?.lowercased() ?? "") else { return }
        #if os(macOS)
        NSWorkspace.shared.open(url)
        #else
        UIApplication.shared.open(url)
        #endif
    }
}

@MainActor final class AppNavigation: ObservableObject {
    static let shared = AppNavigation()
    @Published var url = AppRoute.prompt
    @Published var revision = 0
    @Published var windowTitle = "aesthetic.computer"

    func open(_ incoming: URL) {
        guard let destination = AppRoute.destination(for: incoming) else { return }
        url = destination
        revision += 1
    }

    func prompt() { url = AppRoute.prompt; revision += 1 }
}
