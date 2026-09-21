import Foundation
import WebKit
import CryptoKit
#if os(macOS)
import AppKit
import ScreenCaptureKit
#endif

/// Same-user RPC mailbox for the monorepo MCP adapter. No listening port,
/// arbitrary JavaScript, credentials, or automatic window activation.
@MainActor
final class AeselAutomation {
    var inspect: (() -> [String: Any])?
    var perform: ((String, [String: Any]) async throws -> Void)?
    weak var preview: WKWebView?
    weak var notebook: WKWebView?
    var retryPreview: (() -> Void)?
    var previewFailure: String?
    private let directory: URL
    private var timer: Timer?
    private var servicing = false
    private var sequence = 0
    private var events: [[String: Any]] = []
    private let instance = UUID().uuidString
    private let buildSha256: String

    init() {
        var hash = SHA256()
        let bundle = Bundle.main.bundleURL
        func visit(_ folder: URL) {
            let files = (try? FileManager.default.contentsOfDirectory(at: folder, includingPropertiesForKeys: [.isDirectoryKey, .isRegularFileKey])) ?? []
            for file in files.sorted(by: { $0.lastPathComponent.utf8.lexicographicallyPrecedes($1.lastPathComponent.utf8) }) {
                if file.lastPathComponent == "_CodeSignature" { continue }
                let values = try? file.resourceValues(forKeys: [.isDirectoryKey, .isRegularFileKey])
                if values?.isDirectory == true { visit(file) }
                else if values?.isRegularFile == true, let data = try? Data(contentsOf: file) {
                    hash.update(data: Data(file.path.dropFirst(bundle.path.count + 1).utf8))
                    hash.update(data: Data([0]))
                    hash.update(data: data)
                }
            }
        }
        visit(bundle)
        buildSha256 = hash.finalize().map { String(format: "%02x", $0) }.joined()
        let requested = ProcessInfo.processInfo.environment["AESEL_AUTOMATION_NAMESPACE"] ?? ""
        let namespace = requested.range(of: "^[a-z0-9-]{1,32}$", options: .regularExpression) != nil ? "-" + requested : ""
        directory = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent(Bundle.main.bundleIdentifier ?? "computer.aesthetic.aesel.native")
            .appendingPathComponent("automation" + namespace)
    }

    func start() {
        guard timer == nil else { return }
        do {
            for path in [directory, directory.appendingPathComponent("requests"), directory.appendingPathComponent("responses")] {
                try FileManager.default.createDirectory(at: path, withIntermediateDirectories: true, attributes: [.posixPermissions: 0o700])
            }
            try write(["schema": 1, "pid": ProcessInfo.processInfo.processIdentifier, "instance": instance,
                       "bundle": Bundle.main.bundleIdentifier ?? "", "startedAt": Date().timeIntervalSince1970], to: directory.appendingPathComponent("instance.json"))
        } catch { NSLog("Aesel automation startup failed: %@", error.localizedDescription); return }
        timer = Timer.scheduledTimer(withTimeInterval: 0.15, repeats: true) { [weak self] _ in
            Task { @MainActor in await self?.poll() }
        }
        record("automation.ready")
    }

    func record(_ kind: String) {
        sequence += 1
        events.append(["sequence": sequence, "at": Date().timeIntervalSince1970, "kind": String(kind.prefix(100))])
        if events.count > 256 { events.removeFirst(events.count - 256) }
    }

    private func write(_ value: [String: Any], to file: URL) throws {
        let data = try JSONSerialization.data(withJSONObject: value, options: [.sortedKeys])
        try data.write(to: file, options: .atomic)
        try FileManager.default.setAttributes([.posixPermissions: 0o600], ofItemAtPath: file.path)
    }

    private func poll() async {
        guard !servicing else { return }
        servicing = true
        defer { servicing = false }
        let files = (try? FileManager.default.contentsOfDirectory(at: directory.appendingPathComponent("requests"), includingPropertiesForKeys: [.fileSizeKey, .isSymbolicLinkKey])) ?? []
        for file in files.sorted(by: { $0.lastPathComponent < $1.lastPathComponent }).prefix(8) {
            guard file.pathExtension == "json", UUID(uuidString: file.deletingPathExtension().lastPathComponent) != nil,
                  let info = try? file.resourceValues(forKeys: [.fileSizeKey, .isSymbolicLinkKey]), info.isSymbolicLink != true,
                  (info.fileSize ?? 0) <= 65536, let data = try? Data(contentsOf: file),
                  let request = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any] else { continue }
            try? FileManager.default.removeItem(at: file)
            let id = file.deletingPathExtension().lastPathComponent
            guard request["id"] as? String == id, request["instance"] as? String == instance,
                  let created = request["createdAt"] as? Double, abs(Date().timeIntervalSince1970 - created) < 30 else { continue }
            let result: [String: Any]
            do {
                let method = request["method"] as? String ?? ""
                let params = request["params"] as? [String: Any] ?? [:]
                result = ["id": id, "result": try await handle(method, params)]
            } catch {
                result = ["id": id, "error": error.localizedDescription]
            }
            try? write(result, to: directory.appendingPathComponent("responses/\(id).json"))
        }
    }

    private func handle(_ method: String, _ params: [String: Any]) async throws -> [String: Any] {
        switch method {
        case "state", "map":
            var state = inspect?() ?? [:]
            state["instance"] = instance
            state["buildSha256"] = buildSha256
            state["bundlePath"] = Bundle.main.bundleURL.path
            state["pid"] = ProcessInfo.processInfo.processIdentifier
            state["appVersion"] = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? ""
            state["telemetrySequence"] = sequence
            state["previewFailure"] = previewFailure ?? ""
            #if os(macOS)
            if let window = NSApp.windows.first(where: { !($0 is NSPanel) && $0.contentView != nil }) {
                state["window"] = ["number": window.windowNumber, "width": window.contentView!.bounds.width, "height": window.contentView!.bounds.height, "visible": window.isVisible, "toolbarStyle": window.toolbarStyle.rawValue, "hasToolbar": window.toolbar != nil, "styleMask": window.styleMask.rawValue, "titlebarHeight": window.frame.height - window.contentLayoutRect.height]
            }
            #endif
            return state
        case "action":
            guard let action = params["id"] as? String, let perform else { throw failure("UI is not ready") }
            try await perform(action, params)
            record("action.\(action)")
            return ["accepted": true, "action": action, "state": inspect?() ?? [:]]
        case "events":
            let after = params["after"] as? Int ?? 0
            return ["events": events.filter { ($0["sequence"] as? Int ?? 0) > after }, "latest": sequence,
                    "oldest": events.first?["sequence"] ?? sequence, "capacity": 256]
        case "preview":
            guard let preview else { throw failure("Preview is not mounted") }
            let value = try await preview.evaluateJavaScript("JSON.stringify({url:location.href,ready:!!window.preloaded,continuity:window.__aeselContinuity?.inspect?.()??null,flags:Object.fromEntries(new URLSearchParams(location.search)),canvases:[...document.querySelectorAll('canvas')].map(c=>({width:c.width,height:c.height}))})")
            return ["inspection": value as? String ?? "{}"]
        case "capture":
            let targetName = params["target"] as? String ?? "app"
            guard ["app", "preview", "notebook", "window"].contains(targetName) else { throw failure("Unknown capture target") }
            if targetName == "preview" || targetName == "notebook" {
                guard let webView = targetName == "preview" ? preview : notebook else { throw failure("Requested web view is not mounted") }
                let shot = try await webView.takeSnapshot(configuration: nil)
                #if os(macOS)
                guard let tiff = shot.tiffRepresentation, let bitmap = NSBitmapImageRep(data: tiff),
                      let png = bitmap.representation(using: .png, properties: [:]) else { throw failure("Could not encode preview") }
                #else
                guard let png = shot.pngData() else { throw failure("Could not encode preview") }
                #endif
                return ["mimeType": "image/png", "data": png.base64EncodedString(), "target": targetName]
            }
            #if os(macOS)
            guard let window = NSApp.windows.first(where: { $0.identifier?.rawValue == "workspace" }) ?? NSApp.windows.first(where: { $0.isVisible && !($0 is NSPanel) }),
                  window.contentView != nil else { throw failure("No app window available") }
            if targetName == "app" {
                guard let view = window.contentView, let bitmap = view.bitmapImageRepForCachingDisplay(in: view.bounds) else { throw failure("No app content available") }
                view.cacheDisplay(in: view.bounds, to: bitmap)
                guard let png = bitmap.representation(using: .png, properties: [:]) else { throw failure("Could not encode app chrome") }
                return ["mimeType": "image/png", "data": png.base64EncodedString(), "target": "app", "scope": "native-chrome", "windowNumber": window.windowNumber]
            }
            // Never raise the macOS permission prompt from a test capture.
            guard CGPreflightScreenCaptureAccess() else { throw failure("Whole-window capture needs existing Screen Recording permission. Use app, notebook and preview captures separately.") }
            // Capture the actual window, including WebKit and native overlays.
            // AppKit cacheDisplay and hand compositing lose pixels or occlusion.
            let content = try await SCShareableContent.excludingDesktopWindows(true, onScreenWindowsOnly: false)
            guard let target = content.windows.first(where: { $0.windowID == CGWindowID(window.windowNumber) }) else { throw failure("App window is unavailable to ScreenCaptureKit") }
            let filter = SCContentFilter(desktopIndependentWindow: target)
            let config = SCStreamConfiguration()
            config.width = Int(target.frame.width * window.backingScaleFactor)
            config.height = Int(target.frame.height * window.backingScaleFactor)
            config.showsCursor = false
            config.captureResolution = .best
            let shot = try await SCScreenshotManager.captureImage(contentFilter: filter, configuration: config)
            let output = NSBitmapImageRep(cgImage: shot)
            guard let png = output.representation(using: .png, properties: [:]) else { throw failure("Could not encode app window") }
            return ["mimeType": "image/png", "data": png.base64EncodedString(), "target": "window", "windowNumber": window.windowNumber]
            #else
            throw failure("Whole-window capture is currently macOS-only; preview capture is supported")
            #endif
        default: throw failure("Unknown automation method")
        }
    }

    static func error(_ message: String) -> NSError { NSError(domain: "AeselAutomation", code: 1, userInfo: [NSLocalizedDescriptionKey: message]) }
    private func failure(_ message: String) -> NSError { Self.error(message) }
}
