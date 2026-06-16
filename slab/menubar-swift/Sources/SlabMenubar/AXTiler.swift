import AppKit
import ApplicationServices

/// In-process window placement via the Accessibility API. This is what
/// makes "Tile now" feel instant: the old path forked `osascript` three
/// times (two synchronous window-count probes + a bounds script that
/// `activate`d the terminal), each a process spawn plus an Apple Events
/// round-trip into the app's main thread. AX is a direct Mach call —
/// enumerating and re-framing a dozen windows lands in single-digit
/// milliseconds, steals no focus, and needs no script compilation.
///
/// Requires Accessibility trust, which the menubar app already holds for
/// its System Events font-menu clicking; `trusted` gates every caller so
/// an untrusted install falls back to the legacy osascript path instead
/// of silently doing nothing.
///
/// Known AX caveat: `kAXWindowsAttribute` only lists windows on the
/// current Space — which is the right behavior for a tiler (windows parked
/// on another Space shouldn't be yanked into this screen's grid).
enum AXTiler {
    static var trusted: Bool { AXIsProcessTrusted() }

    /// Tileable windows of `bundleId`, front-to-back: standard windows
    /// only (no panels/sheets/hotkey drawers), minimized excluded — the
    /// same filter the AppleScript tiler applied. App not running → [].
    static func windows(bundleId: String) -> [AXUIElement] {
        var out: [AXUIElement] = []
        for app in NSRunningApplication.runningApplications(withBundleIdentifier: bundleId) {
            let el = AXUIElementCreateApplication(app.processIdentifier)
            var ref: CFTypeRef?
            guard AXUIElementCopyAttributeValue(el, kAXWindowsAttribute as CFString, &ref) == .success,
                  let list = ref as? [AXUIElement] else { continue }
            for w in list {
                if boolAttr(w, kAXMinimizedAttribute) == true { continue }
                if let sub = stringAttr(w, kAXSubroleAttribute),
                   sub != kAXStandardWindowSubrole as String { continue }
                out.append(w)
            }
        }
        return out
    }

    /// Pin a window to AppleScript-style bounds (global top-left-origin
    /// pixels — AX shares that coordinate space). Position before size so
    /// a window clamped by its old frame still lands in its cell.
    static func setFrame(_ w: AXUIElement, left: Int, top: Int, right: Int, bottom: Int) {
        var pos = CGPoint(x: left, y: top)
        var size = CGSize(width: right - left, height: bottom - top)
        if let v = AXValueCreate(.cgPoint, &pos) {
            AXUIElementSetAttributeValue(w, kAXPositionAttribute as CFString, v)
        }
        if let v = AXValueCreate(.cgSize, &size) {
            AXUIElementSetAttributeValue(w, kAXSizeAttribute as CFString, v)
        }
    }

    private static func boolAttr(_ el: AXUIElement, _ attr: String) -> Bool? {
        var ref: CFTypeRef?
        guard AXUIElementCopyAttributeValue(el, attr as CFString, &ref) == .success else { return nil }
        return ref as? Bool
    }

    private static func stringAttr(_ el: AXUIElement, _ attr: String) -> String? {
        var ref: CFTypeRef?
        guard AXUIElementCopyAttributeValue(el, attr as CFString, &ref) == .success else { return nil }
        return ref as? String
    }
}
