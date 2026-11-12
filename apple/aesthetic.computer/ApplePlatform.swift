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

// `--ac-reload-probe N`: after boot, alternate N soft and hard reloads, one
// second apart, print each reload → boot:ready time, then quit. Local
// benchmarking only; nothing reads it otherwise.
@MainActor enum MacReloadProbe {
    private static let active = (Int(MacPromptLatencyProbe.argument("--ac-reload-probe") ?? "") ?? 0) > 0
    private static var remaining = Int(MacPromptLatencyProbe.argument("--ac-reload-probe") ?? "") ?? 0
    private static var results: [String] = []
    private static var pending: String?
    private static var hard = false

    static func booted(_ navigation: AppNavigation) {
        guard active else { return }
        if let label = pending, let ms = navigation.lastReloadMs { results.append("\(label) \(Int(navigation.lastCommitMs ?? -1))/\(Int(ms))ms") }
        pending = nil
        guard remaining > 0 else {
            print("AC reload probe: " + results.joined(separator: ", "))
            NSApp.terminate(nil)
            return
        }
        remaining -= 1
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
            pending = hard ? "hard" : "soft"
            if hard { navigation.hardReload() } else { navigation.reload() }
            hard.toggle()
        }
    }
}

// WebKit can reset AppKit's cursor when the invisible prompt editor gains
// focus, even while CSS still reports the SVG cursor. Reassert the same image
// after mouse dispatch so native focus changes cannot replace it.
final class ACNativeWebView: WKWebView {
    weak var navigation: AppNavigation?
    /// The page hides the pointer itself (dool's world): stop reasserting the
    /// AC cursor, which fought the page's `cursor: none` and flickered on
    /// every mouse move (2026-11-08). Set by the page's `cursor:hidden`.
    var pageHidesCursor = false {
        didSet {
            guard pageHidesCursor != oldValue else { return }
            window?.invalidateCursorRects(for: self)
            if !pageHidesCursor { restoreACCursor() }
        }
    }
    private var colorObserver: NSObjectProtocol?

    private var cursorMonitor: Any?
    private var keyMonitor: Any?
    private var cursorTrackingArea: NSTrackingArea?
    private lazy var acCursor = NSCursor(image: NSImage(named: "ACCursor")!,
                                        hotSpot: NSPoint(x: 12, y: 12))

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if let cursorMonitor { NSEvent.removeMonitor(cursorMonitor) }
        cursorMonitor = nil
        guard let window else { return }
        window.tabbingMode = .disallowed
        applyWindowAppearance()
        if colorObserver == nil {
            colorObserver = NotificationCenter.default.addObserver(
                forName: NSColor.systemColorsDidChangeNotification, object: nil, queue: .main
            ) { [weak self] _ in
                Task { @MainActor in self?.applyWindowAppearance() }
            }
        }
        if let pending = AppNavigation.pendingURL {
            AppNavigation.pendingURL = nil
            navigation?.open(pending)
        }
        // ⌘R / ⇧⌘R act here, before WebKit hands the key to the page and waits
        // for its answer: a page busy painting or collecting can't delay them.
        if keyMonitor == nil {
            keyMonitor = NSEvent.addLocalMonitorForEvents(matching: .keyDown) { [weak self] event in
                guard let self, event.window === self.window,
                      event.charactersIgnoringModifiers?.lowercased() == "r" else { return event }
                let mods = event.modifierFlags.intersection(.deviceIndependentFlagsMask)
                if mods == .command { self.navigation?.reload(); return nil }
                if mods == [.command, .shift] { self.navigation?.hardReload(); return nil }
                return event
            }
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

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        applyWindowAppearance()
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        navigation?.appearanceChanged(dark: dark)
        if let rgb = navigation?.pageColor {
            underPageBackgroundColor = NSColor(srgbRed: rgb[0] / 255, green: rgb[1] / 255, blue: rgb[2] / 255, alpha: 1)
        }
    }

    private func applyWindowAppearance() {
        guard let window else { return }
        effectiveAppearance.performAsCurrentDrawingAppearance {
            let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
            window.titlebarAppearsTransparent = true
            window.titlebarSeparatorStyle = .automatic
            window.backgroundColor = NSColor.windowBackgroundColor.blended(
                withFraction: dark ? 0.16 : 0.08, of: .controlAccentColor) ?? .windowBackgroundColor
            window.standardWindowButton(.closeButton)?.superview?.appearance = nil
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
        if !pageHidesCursor { addCursorRect(bounds, cursor: acCursor) }
    }

    private func restoreACCursor() {
        guard !pageHidesCursor, let window, window.isKeyWindow,
              bounds.contains(convert(window.mouseLocationOutsideOfEventStream, from: nil)) else { return }
        acCursor.set()
    }

    deinit {
        if let colorObserver { NotificationCenter.default.removeObserver(colorObserver) }
        if let cursorMonitor { NSEvent.removeMonitor(cursorMonitor) }
        if let keyMonitor { NSEvent.removeMonitor(keyMonitor) }
    }
}

// 📊 Memory and CPU for the page's fps readout, twice a second while it's up:
// the web content process (where the page and its worker run), the GPU
// process and this app. The page has no memory API of its own in WebKit.
@MainActor final class ProcessStatsSampler {
    private let navigation: AppNavigation
    private weak var webView: WKWebView?
    private var timer: Timer?
    private var previous: [pid_t: (cpu: UInt64, at: UInt64)] = [:]

    init(navigation: AppNavigation) { self.navigation = navigation }

    func start(_ webView: WKWebView) {
        self.webView = webView
        guard timer == nil else { return }
        let timer = Timer(timeInterval: 0.5, repeats: true) { [weak self] timer in
            MainActor.assumeIsolated {
                guard let self else { timer.invalidate(); return }
                self.sample()
            }
        }
        RunLoop.main.add(timer, forMode: .common)
        self.timer = timer
        sample()
    }

    func stop() {
        timer?.invalidate()
        timer = nil
        previous.removeAll()
    }

    private func sample() {
        guard let webView else { stop(); return }
        var body: [String: Any] = [:]
        func put(_ name: String, _ pid: pid_t?) {
            guard let pid, pid > 0, let usage = usage(of: pid) else { return }
            body[name] = ["mb": (usage.mb * 10).rounded() / 10, "cpu": (usage.cpu * 10).rounded() / 10]
        }
        put("web", processID(webView, "_webProcessIdentifier"))
        put("gpu", processID(webView, "_gpuProcessIdentifier"))
        put("app", getpid())
        if let ms = navigation.lastReloadMs { body["reloadMs"] = Int(ms) }
        guard let data = try? JSONSerialization.data(withJSONObject: body),
              let json = String(data: data, encoding: .utf8) else { return }
        webView.evaluateJavaScript("window.acNativeStats?.(\(json))") { _, _ in }
    }

    /// WebKit's process identifiers are SPI; read them only if present.
    private func processID(_ webView: WKWebView, _ key: String) -> pid_t? {
        guard webView.responds(to: NSSelectorFromString(key)) else { return nil }
        return (webView.value(forKey: key) as? NSNumber)?.int32Value
    }

    private func usage(of pid: pid_t) -> (mb: Double, cpu: Double)? {
        var info = rusage_info_v4()
        let result = withUnsafeMutablePointer(to: &info) {
            $0.withMemoryRebound(to: rusage_info_t?.self, capacity: 1) {
                proc_pid_rusage(pid, RUSAGE_INFO_V4, $0)
            }
        }
        guard result == 0 else { return nil }
        // Both clocks count in mach ticks, so their ratio is the CPU share
        // (100 = one core) on Apple silicon and Intel alike.
        let cpu = info.ri_user_time + info.ri_system_time, now = mach_absolute_time()
        var share = 0.0
        if let last = previous[pid], now > last.at, cpu >= last.cpu {
            share = Double(cpu - last.cpu) / Double(now - last.at) * 100
        }
        previous[pid] = (cpu, now)
        return (Double(info.ri_phys_footprint) / 1_048_576, share)
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

@MainActor enum ApplePlatform {
    static func colorWindow(_ rgb: [Double], webView: WKWebView) {
        #if os(macOS)
        guard rgb.count == 3, rgb.allSatisfy({ $0.isFinite && (0...255).contains($0) }) else { return }
        let dark = webView.effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        (webView as? ACNativeWebView)?.navigation?.setPageColor(rgb, dark: dark)
        // Match overscroll to the page; native chrome follows macOS appearance.
        webView.underPageBackgroundColor = NSColor(srgbRed: rgb[0] / 255,
            green: rgb[1] / 255, blue: rgb[2] / 255, alpha: 1)
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
    #if os(macOS)
    static var pendingURL: URL?
    static var active: AppNavigation? {
        func find(in view: NSView?) -> AppNavigation? {
            if let webView = view as? ACNativeWebView { return webView.navigation }
            for child in view?.subviews ?? [] { if let found = find(in: child) { return found } }
            return nil
        }
        return find(in: (NSApp.keyWindow ?? NSApp.mainWindow)?.contentView)
    }
    #endif
    weak var webView: WKWebView?
    @Published var url = AppRoute.prompt
    @Published var revision = 0
    /// Bumping this rebuilds the WKWebView itself (SwiftUI `.id`), so the next
    /// load runs in a fresh web content process rather than a wedged one.
    @Published var instance = 0
    @Published var windowTitle = "aesthetic.computer"
    /// The next load skips the HTTP cache for the document (hard reload).
    var bypassCache = false
    /// The page's last colour, painted under the fresh instance so a hard
    /// reload doesn't flash grey.
    @Published var pageColor: [Double]? = AppNavigation.rememberedColor(dark: AppNavigation.systemIsDark)
    /// The last frame, held over a reload until the next page is ready so
    /// nothing flashes between them.
    @Published var handoff: ACImage?
    /// A soft reload that hasn't committed yet; a second ⌘R or a stall
    /// escalates it to a hard one.
    private var pendingReload: (id: Int, at: Date)?
    private(set) var reloadStartedAt: Date?
    var lastReloadMs: Double?

    func open(_ incoming: URL) {
        guard let destination = AppRoute.destination(for: incoming) else { return }
        url = destination
        revision += 1
    }

    func reload() {
        if pendingReload != nil { hardReload(); return } // Pressed again: insist.
        reloadStartedAt = Date()
        let id = revision + 1
        pendingReload = (id, Date())
        holdLastFrame { [weak self] in
            guard let self, self.pendingReload?.id == id else { return }
            if let current = self.webView?.url?.absoluteString { self.url = current }
            self.revision = id
        }
        // A web process that can't commit a navigation in time is busy or hung.
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.5) { [weak self] in
            guard let self, self.pendingReload?.id == id else { return }
            self.hardReload()
        }
    }

    func hardReload() {
        pendingReload = nil
        reloadStartedAt = Date()
        holdLastFrame { [weak self] in
            guard let self else { return }
            if let current = self.webView?.url?.absoluteString { self.url = current }
            self.bypassCache = true
            self.instance += 1
        }
    }

    /// Snapshot what's on screen, then go — at most 80 ms later, so a wedged
    /// page that can't answer still reloads at once (with no frame held).
    private func holdLastFrame(then go: @escaping () -> Void) {
        var gone = false
        let once = { if !gone { gone = true; go() } }
        if let webView, handoff == nil {
            webView.takeSnapshot(with: nil) { [weak self] image, _ in
                if !gone, let image { self?.handoff = image }
                once()
            }
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + (webView == nil ? 0 : 0.08), execute: once)
        // Never hold it for long if the next page doesn't come up.
        DispatchQueue.main.asyncAfter(deadline: .now() + 8) { [weak self] in self?.handoff = nil }
    }

    // The page's colour is kept per appearance across launches, so a new
    // window or a theme flip shows the right ground before the page paints.
    private static func colorKey(dark: Bool) -> String { dark ? "acPageColorDark" : "acPageColorLight" }
    static func rememberedColor(dark: Bool) -> [Double]? {
        let rgb = UserDefaults.standard.array(forKey: colorKey(dark: dark)) as? [Double]
        return rgb?.count == 3 ? rgb : nil
    }
    static var systemIsDark: Bool {
        #if os(macOS)
        NSApp?.effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        #else
        false
        #endif
    }

    func setPageColor(_ rgb: [Double], dark: Bool) {
        guard rgb != pageColor else { return }
        pageColor = rgb
        UserDefaults.standard.set(rgb, forKey: Self.colorKey(dark: dark))
    }

    func appearanceChanged(dark: Bool) {
        if let rgb = Self.rememberedColor(dark: dark) { pageColor = rgb }
    }

    func committed() {
        pendingReload = nil
        if let start = reloadStartedAt { lastCommitMs = Date().timeIntervalSince(start) * 1000 }
    }
    var lastCommitMs: Double?

    func booted() {
        handoff = nil
        if let start = reloadStartedAt { lastReloadMs = Date().timeIntervalSince(start) * 1000 }
        reloadStartedAt = nil
        #if os(macOS)
        MacReloadProbe.booted(self)
        #endif
    }

    func prompt() { url = AppRoute.prompt; revision += 1 }
}
