import AppKit
import ApplicationServices

/// ⌘⌥ + arrow → move focus to the tiled terminal window in that direction.
///
/// The spatial companion to the ⌘⌥T tiler: once a wall of Claude terminals is
/// packed into a grid, the arrows walk it without reaching for the mouse or
/// unrolling the menubar. Pure Accessibility — the same trust the tiler
/// already holds, no osascript, and no focus-steal beyond the intended raise.
///
/// "Spatial" means each arrow raises the window physically in that direction
/// from the one currently focused, not the next in some list order — so the
/// motion matches what you see on screen. Windows on another Space are invisible
/// to `kAXWindowsAttribute` (an AX quirk the tiler relies on too), which is the
/// right thing here: the arrows only ever walk the wall in front of you.
enum WindowNav {
    enum Direction { case left, right, up, down }

    /// Terminal bundles whose windows the tiler manages — the set we walk.
    /// Same two apps `tileNow` packs, so jumping and tiling agree on membership.
    private static let bundleIds = ["com.googlecode.iterm2", "com.apple.Terminal"]

    static func jump(_ dir: Direction) {
        guard AXTiler.trusted else { return }

        // Every tileable terminal window on the current Space, with its center
        // in AppleScript top-left-origin pixels (the space `AXTiler.center`
        // reports and directional scoring below assumes).
        var wins: [(el: AXUIElement, center: CGPoint)] = []
        for bid in bundleIds {
            for w in AXTiler.windows(bundleId: bid) {
                if let c = AXTiler.center(w) { wins.append((w, c)) }
            }
        }
        guard wins.count > 1 else { return }

        // Which window holds focus now? AXUIElements from separate queries
        // aren't reliably CFEqual, so match the system-wide focused window to
        // our list by nearest center rather than by identity.
        guard let focus = focusedWindowCenter() else {
            raise(wins[0].el); return   // nothing focusable up front — seed on the first
        }
        let currentIdx = wins.enumerated().min(by: {
            dist2($0.element.center, focus) < dist2($1.element.center, focus)
        })?.offset
        let current = currentIdx.map { wins[$0].center } ?? focus

        // Best candidate in `dir`: on the correct side of `current`, minimizing
        // primary-axis travel with a heavy penalty on perpendicular drift so a
        // near-aligned neighbor beats a far diagonal one. y grows downward, so
        // "up" is a smaller y and "down" a larger one.
        var best: (el: AXUIElement, score: CGFloat)?
        for (i, w) in wins.enumerated() {
            if i == currentIdx { continue }
            let dx = w.center.x - current.x
            let dy = w.center.y - current.y
            let score: CGFloat
            switch dir {
            case .left:  guard dx < -1 else { continue }; score = -dx + 3 * abs(dy)
            case .right: guard dx >  1 else { continue }; score =  dx + 3 * abs(dy)
            case .up:    guard dy < -1 else { continue }; score = -dy + 3 * abs(dx)
            case .down:  guard dy >  1 else { continue }; score =  dy + 3 * abs(dx)
            }
            if best == nil || score < best!.score { best = (w.el, score) }
        }
        // No window on that side — we're at the edge of the wall. Do nothing
        // (no wrap) so the arrows feel like moving through a physical grid.
        guard let target = best?.el else { return }
        raise(target)
    }

    private static func dist2(_ a: CGPoint, _ b: CGPoint) -> CGFloat {
        let dx = a.x - b.x, dy = a.y - b.y
        return dx * dx + dy * dy
    }

    /// Center of the system-wide focused window (top-left origin px), or nil if
    /// nothing focusable is up front.
    private static func focusedWindowCenter() -> CGPoint? {
        let sys = AXUIElementCreateSystemWide()
        var appRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(sys, kAXFocusedApplicationAttribute as CFString, &appRef) == .success,
              let appRef = appRef else { return nil }
        let appEl = appRef as! AXUIElement
        var winRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(appEl, kAXFocusedWindowAttribute as CFString, &winRef) == .success,
              let winRef = winRef else { return nil }
        return AXTiler.center(winRef as! AXUIElement)
    }

    /// Bring `w` and its owning app forward: raise the window (orders it front
    /// within its app), mark it main (so the terminal routes keys to it), and
    /// activate the owning process (brings that app ahead of everything else).
    private static func raise(_ w: AXUIElement) {
        AXUIElementPerformAction(w, kAXRaiseAction as CFString)
        AXUIElementSetAttributeValue(w, kAXMainAttribute as CFString, kCFBooleanTrue)
        var pid: pid_t = 0
        if AXUIElementGetPid(w, &pid) == .success {
            NSRunningApplication(processIdentifier: pid)?
                .activate(options: [.activateIgnoringOtherApps])
        }
    }
}
