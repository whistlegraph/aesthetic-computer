import SwiftUI
import WebKit

#if os(macOS)
import AppKit
typealias ACApplication = NSApplication
typealias ACApplicationDelegate = NSApplicationDelegate
typealias ACImage = NSImage

// WebKit can reset AppKit's cursor when the invisible prompt editor gains
// focus, even while CSS still reports the SVG cursor. Reassert the same image
// after mouse dispatch so native focus changes cannot replace it.
final class ACNativeWebView: WKWebView {
    private var cursorMonitor: Any?
    private lazy var acCursor = NSCursor(image: NSImage(named: "ACCursor")!,
                                        hotSpot: NSPoint(x: 12, y: 12))

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if let cursorMonitor { NSEvent.removeMonitor(cursorMonitor) }
        cursorMonitor = nil
        guard window != nil else { return }
        cursorMonitor = NSEvent.addLocalMonitorForEvents(matching: [
            .mouseMoved, .leftMouseDown, .leftMouseUp, .leftMouseDragged,
            .rightMouseDown, .rightMouseUp, .otherMouseDown, .otherMouseUp
        ]) { [weak self] event in
            guard let self, event.window === self.window else { return event }
            self.restoreACCursor()
            DispatchQueue.main.async { [weak self] in self?.restoreACCursor() }
            return event
        }
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
