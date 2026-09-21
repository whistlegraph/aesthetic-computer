import SwiftUI
import WebKit
import CoreText

#if os(macOS)
import AppKit
typealias AeselImage = NSImage
typealias AeselColor = NSColor
typealias AeselFont = NSFont

/// Keep WebKit setup and coordinators shared; only the SwiftUI adapter differs.
protocol AeselWebViewRepresentable: NSViewRepresentable where NSViewType == WKWebView {
    func makeWebView(context: Context) -> WKWebView
    func updateWebView(_ view: WKWebView, context: Context)
    static func dismantleWebView(_ view: WKWebView, coordinator: Coordinator)
}
extension AeselWebViewRepresentable {
    func makeNSView(context: Context) -> WKWebView { makeWebView(context: context) }
    func updateNSView(_ view: WKWebView, context: Context) { updateWebView(view, context: context) }
    static func dismantleNSView(_ view: WKWebView, coordinator: Coordinator) { dismantleWebView(view, coordinator: coordinator) }
    static func dismantleWebView(_ view: WKWebView, coordinator: Coordinator) {}
}
#else
import UIKit
typealias AeselImage = UIImage
typealias AeselColor = UIColor
typealias AeselFont = UIFont

protocol AeselWebViewRepresentable: UIViewRepresentable where UIViewType == WKWebView {
    func makeWebView(context: Context) -> WKWebView
    func updateWebView(_ view: WKWebView, context: Context)
    static func dismantleWebView(_ view: WKWebView, coordinator: Coordinator)
}
extension AeselWebViewRepresentable {
    func makeUIView(context: Context) -> WKWebView { makeWebView(context: context) }
    func updateUIView(_ view: WKWebView, context: Context) { updateWebView(view, context: context) }
    static func dismantleUIView(_ view: WKWebView, coordinator: Coordinator) { dismantleWebView(view, coordinator: coordinator) }
    static func dismantleWebView(_ view: WKWebView, coordinator: Coordinator) {}
}
#endif

extension Image {
    init(aeselImage: AeselImage) {
        #if os(macOS)
        self.init(nsImage: aeselImage)
        #else
        self.init(uiImage: aeselImage)
        #endif
    }
}

extension WKWebView {
    var aeselOpacity: CGFloat {
        get {
            #if os(macOS)
            alphaValue
            #else
            alpha
            #endif
        }
        set {
            #if os(macOS)
            alphaValue = newValue
            #else
            alpha = newValue
            #endif
        }
    }
}

enum ApplePlatform {
    static func setAppearance(_ view: WKWebView, colorScheme: ColorScheme) {
        #if os(macOS)
        view.appearance = NSAppearance(named: colorScheme == .dark ? .darkAqua : .aqua)
        #else
        view.overrideUserInterfaceStyle = colorScheme == .dark ? .dark : .light
        #endif
    }
    static func registerFonts() {
        #if os(macOS)
        if let url = Bundle.main.url(forResource: "ComicRelief-Bold", withExtension: "ttf") {
            CTFontManagerRegisterFontsForURL(url as CFURL, .process, nil)
        }
        #endif
    }

    static func image(size: CGSize, drawing: @escaping () -> Void) -> AeselImage {
        #if os(macOS)
        return NSImage(size: size, flipped: true) { _ in drawing(); return true }
        #else
        let format = UIGraphicsImageRendererFormat.default()
        format.opaque = false
        return UIGraphicsImageRenderer(size: size, format: format).image { _ in drawing() }
        #endif
    }

    static func cgImage(at url: URL) -> CGImage? {
        #if os(macOS)
        return NSImage(contentsOf: url)?.cgImage(forProposedRect: nil, context: nil, hints: nil)
        #else
        return UIImage(contentsOfFile: url.path)?.cgImage
        #endif
    }

    static func image(cgImage: CGImage) -> AeselImage {
        #if os(macOS)
        return NSImage(cgImage: cgImage, size: CGSize(width: cgImage.width, height: cgImage.height))
        #else
        return UIImage(cgImage: cgImage)
        #endif
    }

    static func configureEmbeddedView(_ view: WKWebView) {
        view.underPageBackgroundColor = .clear
        #if os(iOS)
        view.isOpaque = false
        view.backgroundColor = .clear
        view.scrollView.backgroundColor = .clear
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        view.isMultipleTouchEnabled = true
        #endif
    }
}

extension View {
    @ViewBuilder func aeselKeyboardScrolling() -> some View {
        #if os(iOS)
        scrollDismissesKeyboard(.interactively)
        #else
        self
        #endif
    }
    @ViewBuilder func aeselSendLabel() -> some View {
        #if os(iOS)
        submitLabel(.send)
        #else
        self
        #endif
    }
    @ViewBuilder func aeselHelpSheet() -> some View {
        #if os(iOS)
        presentationDetents([.medium])
        #else
        frame(minWidth: 420, minHeight: 340)
        #endif
    }
    @ViewBuilder func aeselSignInSheet() -> some View {
        #if os(macOS)
        frame(minWidth: 500, minHeight: 600)
        #else
        self
        #endif
    }
}
