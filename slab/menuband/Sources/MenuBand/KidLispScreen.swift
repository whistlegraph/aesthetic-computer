// Shared plumbing for the currently-selected KidLisp piece. The popover's
// KidLisp TV used to render it too, but that was removed in the v1 cutoff;
// `KidLispState` survives because the floating AestheticWebWindow (opened
// from About) still mirrors a selected `$code` piece.
//
// `KidLispState.shared` is the single source of truth for the current
// selection (a `$code` when one is known, plus the raw source).

import AppKit

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

    /// Fallback raw source when no `$code` is selected. (Formerly
    /// `Self.defaultSource`, inlined here when the TV panel
    /// was removed in the v1 cutoff.)
    static let defaultSource: String = """
        wipe black
        ink rainbow
        line 0 (- h/2 (* amp 5)) w (- h/2 (* amp 5))
        line 0 (+ h/2 (* amp 5)) w (+ h/2 (* amp 5))
        """

    private(set) var code: String?
    private(set) var source: String

    private init() {
        let d = UserDefaults.standard
        self.code = d.string(forKey: Self.codeKey)
        self.source = d.string(forKey: Self.sourceKey) ?? Self.defaultSource
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
        self.source = Self.defaultSource
        let d = UserDefaults.standard
        d.removeObject(forKey: Self.codeKey)
        d.removeObject(forKey: Self.sourceKey)
    }
}
