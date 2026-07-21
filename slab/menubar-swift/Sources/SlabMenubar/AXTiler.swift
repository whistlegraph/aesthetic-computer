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
    ///
    /// `requireStandardSubrole: false` keeps frameless windows in the set —
    /// the AC Electron preview windows (slab-web) are created `frame: false`,
    /// so macOS reports a non-standard subrole and the strict filter would
    /// drop them from the grid. We still exclude minimized + zero-size junk.
    static func windows(bundleId: String, requireStandardSubrole: Bool = true) -> [AXUIElement] {
        var out: [AXUIElement] = []
        for app in NSRunningApplication.runningApplications(withBundleIdentifier: bundleId) {
            let el = AXUIElementCreateApplication(app.processIdentifier)
            var ref: CFTypeRef?
            guard AXUIElementCopyAttributeValue(el, kAXWindowsAttribute as CFString, &ref) == .success,
                  let list = ref as? [AXUIElement] else { continue }
            for w in list {
                if boolAttr(w, kAXMinimizedAttribute) == true { continue }
                if requireStandardSubrole,
                   let sub = stringAttr(w, kAXSubroleAttribute),
                   sub != kAXStandardWindowSubrole as String { continue }
                // Skip degenerate (hidden/zero-size) windows when accepting any subrole.
                if !requireStandardSubrole, let sz = sizeAttr(w), sz.width < 80 || sz.height < 80 { continue }
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

    private static func sizeAttr(_ el: AXUIElement) -> CGSize? {
        var ref: CFTypeRef?
        guard AXUIElementCopyAttributeValue(el, kAXSizeAttribute as CFString, &ref) == .success,
              let v = ref, CFGetTypeID(v) == AXValueGetTypeID() else { return nil }
        var size = CGSize.zero
        guard AXValueGetValue(v as! AXValue, .cgSize, &size) else { return nil }
        return size
    }

    private static func positionAttr(_ el: AXUIElement) -> CGPoint? {
        var ref: CFTypeRef?
        guard AXUIElementCopyAttributeValue(el, kAXPositionAttribute as CFString, &ref) == .success,
              let v = ref, CFGetTypeID(v) == AXValueGetTypeID() else { return nil }
        var point = CGPoint.zero
        guard AXValueGetValue(v as! AXValue, .cgPoint, &point) else { return nil }
        return point
    }

    /// A window's current center in AppleScript top-left-origin pixels — the
    /// same coordinate space `setFrame` writes. Used to keep tile/scatter
    /// placement spatially local (assign each window to the nearest target
    /// cell so it stays roughly where it was). nil if AX can't read the frame.
    static func center(_ w: AXUIElement) -> CGPoint? {
        guard let frame = frame(w) else { return nil }
        return CGPoint(x: frame.midX, y: frame.midY)
    }

    /// A window's frame in the global AX/CoreGraphics coordinate space
    /// (top-left origin, y down). Navigation flashes and focus outlines use the
    /// complete frame; keeping the read here avoids each overlay growing its
    /// own subtly different AX geometry helper.
    static func frame(_ w: AXUIElement) -> CGRect? {
        guard let p = positionAttr(w), let s = sizeAttr(w) else { return nil }
        return CGRect(origin: p, size: s)
    }
}
