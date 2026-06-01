// Shared plumbing for "what KidLisp piece is the TV showing" so the
// popover's KidLisp TV and the floating AestheticWebWindow can render
// the SAME piece. Two renderers conform to `KidLispScreen`:
//
//   • KidLispTVView      — native Swift evaluator (phase-1 corpus)
//   • KidLispTVWebView   — live aesthetic.computer in a WKWebView
//
// `KidLispState.shared` is the single source of truth for the current
// selection (a `$code` when one is known, plus the raw source for the
// native fallback). The chooser writes to it; both surfaces read it.

import AppKit

/// A view that can display a KidLisp piece. Adopted by both the native
/// framebuffer view and the webview so `KidLispTVPanel` can hold either
/// behind one type and `AppDelegate`'s wiring stays renderer-agnostic.
protocol KidLispScreen: AnyObject {
    /// Show a piece. `code` is the bare cache code (no `$`) when known —
    /// the webview prefers it for a short `/$code` URL; the native
    /// renderer ignores it and evaluates `source` directly.
    func show(code: String?, source: String)

    /// Click inside the screen well → owner pops the "$ pieces" chooser.
    var onScreenClick: ((NSView, NSEvent) -> Void)? { get set }
    /// 0–10 audio amplitude feed (native `amp`). Unused by the webview.
    var ampProvider: (() -> Double)? { get set }
    /// True while the app is latency-critical; native renderer skips
    /// ticks. Unused by the webview.
    var busyProvider: (() -> Bool)? { get set }
}

/// URL construction for AC KidLisp pieces. Centralized so the popover
/// (clean embed) and the window (full chrome) build the same paths.
enum KidLispURL {
    static let base = "https://aesthetic.computer/"

    /// Encode raw KidLisp source for the URL path. Mirrors the AC
    /// convention (space→`_`, newline→`~`) then percent-encodes the
    /// rest. NOTE: verify against `lib/kidlisp.mjs` before relying on
    /// exotic glyphs — this covers the common case.
    static func encodeSource(_ src: String) -> String {
        var s = src.replacingOccurrences(of: "\r\n", with: "\n")
        s = s.replacingOccurrences(of: "\n", with: "~")  // newline → tilde (decodes back)
        s = s.replacingOccurrences(of: " ", with: "_")    // space → underscore
        let allowed = CharacterSet(charactersIn:
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~()*:!,;=+$&@")
        return s.addingPercentEncoding(withAllowedCharacters: allowed) ?? s
    }

    /// Path component for a selection: `$code` when known, else the
    /// encoded inline source.
    static func path(code: String?, source: String) -> String {
        if let code = code, !code.isEmpty { return "$\(code)" }
        return encodeSource(source)
    }

    /// Full URL. `embed: true` adds chrome-stripping query params for
    /// the small TV; the window passes `embed: false` for full fidelity.
    static func url(code: String?, source: String, embed: Bool) -> URL {
        var str = base + path(code: code, source: source)
        if embed { str += "?nogap=true&nolabel=true&density=1" }
        return URL(string: str) ?? URL(string: base)!
    }
}

/// Single source of truth for the currently-selected KidLisp piece,
/// shared by the popover TV and the floating web window. Backed by
/// UserDefaults so the choice sticks across launches.
final class KidLispState {
    static let shared = KidLispState()

    private static let codeKey = "notepat.kidlispTV.code"
    /// Same key the old single-renderer path used, so existing saved
    /// raw sources migrate forward (they load with `code == nil`).
    private static let sourceKey = "notepat.kidlispTV.source"

    private(set) var code: String?
    private(set) var source: String

    private init() {
        let d = UserDefaults.standard
        self.code = d.string(forKey: Self.codeKey)
        self.source = d.string(forKey: Self.sourceKey) ?? KidLispTVPanel.defaultSource
    }

    /// True when a real cached piece is selected (vs. the default /
    /// raw source). The web window only mirrors real `$code` pieces so
    /// it isn't hijacked away from the AC homepage by the default.
    var hasCode: Bool { (code?.isEmpty == false) }

    /// Clean-chrome URL for the small TV.
    var embedURL: URL { KidLispURL.url(code: code, source: source, embed: true) }
    /// Full-chrome URL for the window.
    var pieceURL: URL { KidLispURL.url(code: code, source: source, embed: false) }

    func select(code: String?, source: String) {
        self.code = (code?.isEmpty == true) ? nil : code
        self.source = source
        let d = UserDefaults.standard
        if let c = self.code { d.set(c, forKey: Self.codeKey) }
        else { d.removeObject(forKey: Self.codeKey) }
        d.set(source, forKey: Self.sourceKey)
    }

    func reset() {
        self.code = nil
        self.source = KidLispTVPanel.defaultSource
        let d = UserDefaults.standard
        d.removeObject(forKey: Self.codeKey)
        d.removeObject(forKey: Self.sourceKey)
    }
}

// MARK: - Native renderer conformance

extension KidLispTVView: KidLispScreen {
    /// The native evaluator only knows source text; ignore the code.
    func show(code: String?, source: String) { setSource(source) }
}
