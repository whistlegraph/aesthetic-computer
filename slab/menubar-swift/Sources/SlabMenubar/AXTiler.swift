import AppKit
import ApplicationServices
import CoreGraphics

@_silgen_name("_AXUIElementGetWindow")
private func _AXTilerGetWindow(
    _ element: AXUIElement,
    _ windowID: UnsafeMutablePointer<CGWindowID>
) -> AXError

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

    /// One uniquely identified window. AXUIElement arrays occasionally contain
    /// duplicate/transient entries while Terminal is creating or closing a
    /// window; the CG window id is the stable identity shared by AX and the
    /// Window Server, so all counting and placement is deduplicated on it.
    struct Window {
        let element: AXUIElement
        let id: CGWindowID
    }

    /// One atomic-enough census for a tile transaction. Each application is
    /// queried exactly once, and every settle pass reuses these same elements
    /// rather than accidentally changing grid size halfway through a reflow.
    struct Snapshot {
        let iterm: [Window]
        let terminal: [Window]
        let acPanes: [Window]

        var all: [Window] { iterm + terminal + acPanes }
        var signature: [CGWindowID] { all.map(\.id).sorted() }
    }

    private struct CachedWindows {
        let at: Date
        let windows: [Window]
    }

    private struct LiveWindow {
        let id: CGWindowID
        let pid: pid_t
        let frame: CGRect
    }

    private static let cacheLock = NSLock()
    private static var cache: [String: CachedWindows] = [:]
    private static var loggedAnomalies = Set<String>()
    /// Terminal can answer kAXWindows with a transient empty array while a
    /// System Events menu click is in flight. The live failure is generally
    /// one frame; a short last-good lease prevents a false 7 -> 0 -> 7 census
    /// without hiding a real close for long (the population monitor confirms
    /// changes across multiple samples before retiling).
    private static let lastGoodLease: TimeInterval = 0.45

    static func snapshot() -> Snapshot {
        let liveWindows = liveWindowList()
        return Snapshot(
            iterm: windowRefs(bundleId: "com.googlecode.iterm2", liveWindows: liveWindows),
            terminal: windowRefs(bundleId: "com.apple.Terminal", liveWindows: liveWindows),
            acPanes: windowRefs(bundleId: "computer.aesthetic.app",
                                requireStandardSubrole: false, liveWindows: liveWindows)
        )
    }

    /// Lightweight population-only read for the idle auto-tile monitor. Most
    /// windows provide a direct CG id, so this avoids the two extra AX IPCs
    /// needed to read position + size for every window every probe. A full
    /// geometry snapshot is taken only after membership actually changes.
    static func signature() -> [CGWindowID] {
        let liveWindows = liveWindowList()
        return (
            windowRefs(bundleId: "com.googlecode.iterm2", liveWindows: liveWindows,
                       requireGeometry: false)
            + windowRefs(bundleId: "com.apple.Terminal", liveWindows: liveWindows,
                         requireGeometry: false)
            + windowRefs(bundleId: "computer.aesthetic.app", requireStandardSubrole: false,
                         liveWindows: liveWindows, requireGeometry: false)
        ).map(\.id).sorted()
    }

    /// Tileable windows of `bundleId`, front-to-back: standard windows
    /// only (no panels/sheets/hotkey drawers), minimized excluded — the
    /// same filter the AppleScript tiler applied. App not running → [].
    ///
    /// `requireStandardSubrole: false` keeps frameless windows in the set —
    /// the AC Electron preview windows (slab-web) are created `frame: false`,
    /// so macOS reports a non-standard subrole and the strict filter would
    /// drop them from the grid. We still exclude minimized + zero-size junk.
    static func windows(bundleId: String, requireStandardSubrole: Bool = true) -> [AXUIElement] {
        windowRefs(bundleId: bundleId,
                   requireStandardSubrole: requireStandardSubrole,
                   liveWindows: liveWindowList()).map(\.element)
    }

    private static func windowRefs(bundleId: String,
                                   requireStandardSubrole: Bool = true,
                                   liveWindows: [LiveWindow],
                                   requireGeometry: Bool = true) -> [Window] {
        let apps = NSRunningApplication.runningApplications(withBundleIdentifier: bundleId)
        let liveIDs = Set(liveWindows.map(\.id))
        var out: [Window] = []
        var seen = Set<CGWindowID>()
        var readSucceeded = false
        var rawCount = 0
        var minimizedCount = 0
        var subroleCount = 0
        var geometryCount = 0
        var identityCount = 0
        for app in apps {
            let el = AXUIElementCreateApplication(app.processIdentifier)
            var ref: CFTypeRef?
            guard AXUIElementCopyAttributeValue(el, kAXWindowsAttribute as CFString, &ref) == .success,
                  let list = ref as? [AXUIElement] else { continue }
            readSucceeded = true
            rawCount += list.count
            for w in list {
                if boolAttr(w, kAXMinimizedAttribute) == true {
                    minimizedCount += 1
                    continue
                }
                if requireStandardSubrole,
                   let subrole = stringAttr(w, kAXSubroleAttribute),
                   subrole != kAXStandardWindowSubrole as String {
                    // Some Terminal/macOS combinations omit AXSubrole. A
                    // missing value is not evidence that this is a panel:
                    // accept it when the element also has valid geometry and
                    // a live layer-0 Window Server identity below. Exclude
                    // only an explicitly reported non-standard subrole.
                    subroleCount += 1
                    continue
                }
                // `_AXUIElementGetWindow` is private and is absent for a few
                // otherwise ordinary Terminal windows on current macOS. Keep
                // CGWindowID as the identity, but recover it by matching this
                // AX frame to a live layer-0 window owned by the same process.
                let directID = windowID(w).flatMap { liveIDs.contains($0) ? $0 : nil }
                var measuredFrame: CGRect?
                let id: CGWindowID?
                if let directID {
                    id = directID
                } else {
                    measuredFrame = frame(w)
                    if let measuredFrame, measuredFrame.width >= 80, measuredFrame.height >= 80 {
                        id = matchingLiveWindowID(
                            frame: measuredFrame, pid: app.processIdentifier,
                            liveWindows: liveWindows, excluding: seen)
                    } else {
                        id = nil
                    }
                }
                guard let id, seen.insert(id).inserted else {
                    identityCount += 1
                    continue
                }
                // The placement snapshot also rejects half-created AX objects
                // that have an id but do not yet expose useful geometry. The
                // idle signature path deliberately skips this extra AX read.
                if requireGeometry {
                    let usableFrame = measuredFrame ?? frame(w)
                    guard let usableFrame, usableFrame.width >= 80, usableFrame.height >= 80 else {
                        seen.remove(id)
                        geometryCount += 1
                        continue
                    }
                }
                out.append(Window(element: w, id: id))
            }
        }

        let appPIDs = Set(apps.map(\.processIdentifier))
        let ownedLiveIDs = liveWindows.filter { appPIDs.contains($0.pid) }.map(\.id).sorted()
        if out.isEmpty, !ownedLiveIDs.isEmpty {
            let anomaly = "\(bundleId)|\(ownedLiveIDs.map(String.init).joined(separator: ","))|\(rawCount)|\(minimizedCount)|\(subroleCount)|\(geometryCount)|\(identityCount)"
            cacheLock.lock()
            let firstReport = loggedAnomalies.insert(anomaly).inserted
            cacheLock.unlock()
            if firstReport {
                NSLog("🧩 [tile-census] bundle=%@ cg=%@ rawAX=%d minimized=%d subrole=%d geometry=%d identity=%d",
                      bundleId, ownedLiveIDs.map(String.init).joined(separator: ","), rawCount,
                      minimizedCount, subroleCount, geometryCount, identityCount)
            }
        }

        let cacheKey = "\(bundleId)|\(requireStandardSubrole ? 1 : 0)|g\(requireGeometry ? 1 : 0)"
        let now = Date()
        cacheLock.lock()
        defer { cacheLock.unlock() }
        if !out.isEmpty {
            cache[cacheKey] = CachedWindows(at: now, windows: out)
            return out
        }
        // A stopped app is an authoritative zero. For a running app, bridge a
        // momentary failed/empty AX response with the very recent last-good
        // census; genuine closes become visible as soon as the lease expires.
        if !apps.isEmpty,
           let prior = cache[cacheKey],
           now.timeIntervalSince(prior.at) <= lastGoodLease {
            // AX may momentarily forget the whole population during menu
            // automation, but the Window Server does not. Conversely, AX can
            // retain a destroyed Terminal window for seconds; intersecting the
            // lease with live Window Server ids distinguishes the two cases.
            return prior.windows.filter { liveIDs.contains($0.id) }
        }
        if apps.isEmpty || readSucceeded { cache.removeValue(forKey: cacheKey) }
        return []
    }

    static func windowID(_ w: AXUIElement) -> CGWindowID? {
        var id = CGWindowID(0)
        guard _AXTilerGetWindow(w, &id) == .success, id != 0 else { return nil }
        return id
    }

    private static func matchingLiveWindowID(frame: CGRect, pid: pid_t,
                                             liveWindows: [LiveWindow],
                                             excluding seen: Set<CGWindowID>) -> CGWindowID? {
        let candidates = liveWindows.filter { $0.pid == pid && !seen.contains($0.id) }
        let scored = candidates.map { candidate -> (CGWindowID, CGFloat) in
            let b = candidate.frame
            let score = abs(b.minX - frame.minX) + abs(b.minY - frame.minY)
                + abs(b.width - frame.width) + abs(b.height - frame.height)
            return (candidate.id, score)
        }
        guard let best = scored.min(by: { $0.1 < $1.1 }), best.1 <= 12 else { return nil }
        return best.0
    }

    private static func liveWindowList() -> [LiveWindow] {
        guard let infos = CGWindowListCopyWindowInfo(
            [.optionAll, .excludeDesktopElements], kCGNullWindowID
        ) as? [[String: Any]] else { return [] }
        return infos.compactMap { info in
            guard (info[kCGWindowLayer as String] as? Int) == 0 else { return nil }
            let id: CGWindowID?
            if let raw = info[kCGWindowNumber as String] as? UInt32 {
                id = CGWindowID(raw)
            } else if let raw = info[kCGWindowNumber as String] as? Int {
                id = CGWindowID(raw)
            } else {
                id = nil
            }
            guard let id,
                  let rawPID = info[kCGWindowOwnerPID as String] as? Int,
                  let bounds = info[kCGWindowBounds as String] as? NSDictionary,
                  let frame = CGRect(dictionaryRepresentation: bounds) else { return nil }
            return LiveWindow(id: id, pid: pid_t(rawPID), frame: frame)
        }
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

    /// Terminal quantizes AX sizes to character cells. A hidden per-window
    /// Command-Plus zoom can make a request for (say) 274 px land at 283 px,
    /// leaving one pane a row taller even though every profile has the same
    /// font size. Measure that response and compensate the requested size;
    /// two corrections are enough to land on the intended cell without the
    /// focus-stealing View -> Default Font Size menu dance.
    @discardableResult
    static func setFrameFitting(_ w: AXUIElement,
                                left: Int, top: Int, right: Int, bottom: Int) -> CGSize? {
        let targetWidth = right - left
        let targetHeight = bottom - top
        var requestWidth = targetWidth
        var requestHeight = targetHeight
        for _ in 0..<3 {
            setFrame(w, left: left, top: top,
                     right: left + requestWidth, bottom: top + requestHeight)
            guard let actual = frame(w) else { return nil }
            let widthError = Int((actual.width - CGFloat(targetWidth)).rounded())
            let heightError = Int((actual.height - CGFloat(targetHeight)).rounded())
            if abs(widthError) <= 1 && abs(heightError) <= 1 {
                return CGSize(width: widthError, height: heightError)
            }
            requestWidth = max(80, requestWidth - widthError)
            requestHeight = max(80, requestHeight - heightError)
        }
        // Resizing can nudge an edge while Terminal resolves its cell grid.
        // Finish with the canonical origin even if the size was inexact.
        var pos = CGPoint(x: left, y: top)
        if let value = AXValueCreate(.cgPoint, &pos) {
            AXUIElementSetAttributeValue(w, kAXPositionAttribute as CFString, value)
        }
        guard let actual = frame(w) else { return nil }
        return CGSize(width: actual.width - CGFloat(targetWidth),
                      height: actual.height - CGFloat(targetHeight))
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
