import AppKit
import ApplicationServices
import CoreGraphics

@_silgen_name("_AXUIElementGetWindow")
private func _WindowNavAXUIElementGetWindow(
    _ element: AXUIElement,
    _ windowID: UnsafeMutablePointer<CGWindowID>
) -> AXError

/// ⌘⌥ + arrow walks prompt windows through one shared Deskflow coordinate
/// space. A local neighbor wins first. At a local edge, Deskflow's own `links`
/// graph supplies the fleet geometry and the prompt ledger tells us which
/// screens currently have somewhere useful to land.
enum WindowNav {
    enum Direction: String, CaseIterable {
        case left, right, up, down

        var opposite: Direction {
            switch self {
            case .left: return .right
            case .right: return .left
            case .up: return .down
            case .down: return .up
            }
        }

        var step: (x: Int, y: Int) {
            switch self {
            case .left: return (-1, 0)
            case .right: return (1, 0)
            case .up: return (0, -1)
            case .down: return (0, 1)
            }
        }
    }

    private struct Window {
        let el: AXUIElement
        let id: Int
        let frame: CGRect
        var center: CGPoint { CGPoint(x: frame.midX, y: frame.midY) }
    }

    private static let bundleIds = ["com.googlecode.iterm2", "com.apple.Terminal"]

    static func jump(_ dir: Direction) {
        guard AXTiler.trusted else { return }
        let wins = windows()
        guard !wins.isEmpty else {
            DeskflowSpatialNav.cross(from: dir, alignment: 0.5)
            return
        }

        guard let focusCenter = focusedWindowCenter() else {
            focus(wins[0])
            return
        }
        let currentIdx = wins.enumerated().min(by: {
            dist2($0.element.center, focusCenter) < dist2($1.element.center, focusCenter)
        })?.offset
        let current = currentIdx.map { wins[$0].center } ?? focusCenter

        if let target = directionalWindow(in: wins, from: current,
                                          excluding: currentIdx, direction: dir) {
            focus(target)
            return
        }

        // The local wall ended. Preserve the focused pane's perpendicular
        // position so the receiving host can choose the pane that continues
        // the same visual row/column.
        DeskflowSpatialNav.cross(from: dir, alignment: normalizedAlignment(
            of: current, direction: dir))
    }

    /// Fleet navigation endpoint: select the prompt nearest the edge through
    /// which the unipointer is arriving, keeping its row/column aligned with
    /// the source pane, then punctuate the focus change with the lens move.
    static func accept(_ travelDirection: Direction, alignment: CGFloat) {
        guard AXTiler.trusted else { return }
        let wins = windows()
        guard !wins.isEmpty else { return }
        let a = min(max(alignment, 0), 1)
        let target = wins.min {
            entryScore($0, travelDirection: travelDirection, alignment: a)
                < entryScore($1, travelDirection: travelDirection, alignment: a)
        } ?? wins[0]
        focus(target)
    }

    /// Called only on the active Deskflow controller. Each synthetic HID move
    /// crosses one configured screen edge; spacing the moves lets Deskflow
    /// settle on an intermediate screen before the next edge of a multi-screen
    /// route (for example Neo → Chicken → Panda).
    static func routeDeskflow(_ path: [Direction]) {
        for (index, dir) in path.enumerated() {
            DispatchQueue.main.asyncAfter(deadline: .now() + Double(index) * 0.028) {
                postDeskflowEdge(dir)
            }
        }
    }

    private static func windows() -> [Window] {
        let all = bundleIds.flatMap { bid in
            AXTiler.windows(bundleId: bid).compactMap { el -> Window? in
                var id = CGWindowID(0)
                guard _WindowNavAXUIElementGetWindow(el, &id) == .success, id != 0,
                      let frame = AXTiler.frame(el) else { return nil }
                return Window(el: el, id: Int(id), frame: frame)
            }
        }
        // The prompt overlay has already done the expensive tty→window bind.
        // Reuse it so navigation walks actual live prox panes, not an unrelated
        // shell window. During the short startup/rebind gap, retain the old
        // terminal-wide behavior rather than making the shortcut feel dead.
        let promptIDs = PromptSigilOverlayController.shared.promptWindowIDs
        return promptIDs.isEmpty ? all : all.filter { promptIDs.contains($0.id) }
    }

    private static func directionalWindow(in wins: [Window], from current: CGPoint,
                                          excluding currentIdx: Int?,
                                          direction dir: Direction) -> Window? {
        var best: (window: Window, score: CGFloat)?
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
            if best == nil || score < best!.score { best = (w, score) }
        }
        return best?.window
    }

    private static func entryScore(_ window: Window, travelDirection dir: Direction,
                                   alignment: CGFloat) -> CGFloat {
        guard let screen = screen(containingCG: window.center) else { return .greatestFiniteMagnitude }
        let sf = cgFrame(of: screen)
        let edgeDistance: CGFloat
        let perpendicular: CGFloat
        let span: CGFloat
        switch dir {
        case .right:
            edgeDistance = window.frame.minX - sf.minX
            perpendicular = (window.center.y - sf.minY) / max(sf.height, 1)
            span = sf.height
        case .left:
            edgeDistance = sf.maxX - window.frame.maxX
            perpendicular = (window.center.y - sf.minY) / max(sf.height, 1)
            span = sf.height
        case .down:
            edgeDistance = window.frame.minY - sf.minY
            perpendicular = (window.center.x - sf.minX) / max(sf.width, 1)
            span = sf.width
        case .up:
            edgeDistance = sf.maxY - window.frame.maxY
            perpendicular = (window.center.x - sf.minX) / max(sf.width, 1)
            span = sf.width
        }
        return max(0, edgeDistance) + 3 * abs(perpendicular - alignment) * span
    }

    private static func normalizedAlignment(of point: CGPoint, direction: Direction) -> CGFloat {
        guard let screen = screen(containingCG: point) else { return 0.5 }
        let f = cgFrame(of: screen)
        switch direction {
        case .left, .right: return min(max((point.y - f.minY) / max(f.height, 1), 0), 1)
        case .up, .down: return min(max((point.x - f.minX) / max(f.width, 1), 0), 1)
        }
    }

    private static func dist2(_ a: CGPoint, _ b: CGPoint) -> CGFloat {
        let dx = a.x - b.x, dy = a.y - b.y
        return dx * dx + dy * dy
    }

    private static func focusedWindowCenter() -> CGPoint? {
        guard let window = focusedWindow() else { return nil }
        return AXTiler.center(window)
    }

    static func focusedWindow() -> AXUIElement? {
        let sys = AXUIElementCreateSystemWide()
        var appRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(sys, kAXFocusedApplicationAttribute as CFString,
                                            &appRef) == .success,
              let appRef else { return nil }
        var winRef: CFTypeRef?
        guard AXUIElementCopyAttributeValue(appRef as! AXUIElement,
                                            kAXFocusedWindowAttribute as CFString,
                                            &winRef) == .success,
              let winRef else { return nil }
        return (winRef as! AXUIElement)
    }

    private static func focus(_ window: Window) {
        AXUIElementPerformAction(window.el, kAXRaiseAction as CFString)
        AXUIElementSetAttributeValue(window.el, kAXMainAttribute as CFString, kCFBooleanTrue)
        var pid: pid_t = 0
        if AXUIElementGetPid(window.el, &pid) == .success {
            NSRunningApplication(processIdentifier: pid)?
                .activate(options: [.activateIgnoringOtherApps])
        }
        // Raising and app activation complete on the next WindowServer turn;
        // the geometry is already stable, so the acquisition move can fire now.
        if let screen = screen(containingCG: window.center) {
            ZoomSpecialMove.fire(around: window.frame, on: screen)
        }
        PopSound.playTransferClick()
        PromptFocusHighlight.shared.refreshNow()
    }

    private static func postDeskflowEdge(_ dir: Direction) {
        guard let position = CGEvent(source: nil)?.location else { return }
        let magnitude: CGFloat = 4096
        let delta: CGPoint
        switch dir {
        case .left: delta = CGPoint(x: -magnitude, y: 0)
        case .right: delta = CGPoint(x: magnitude, y: 0)
        case .up: delta = CGPoint(x: 0, y: -magnitude)
        case .down: delta = CGPoint(x: 0, y: magnitude)
        }
        guard let event = CGEvent(mouseEventSource: nil, mouseType: .mouseMoved,
                                  mouseCursorPosition: CGPoint(x: position.x + delta.x,
                                                               y: position.y + delta.y),
                                  mouseButton: .left) else { return }
        event.setIntegerValueField(.mouseEventDeltaX, value: Int64(delta.x))
        event.setIntegerValueField(.mouseEventDeltaY, value: Int64(delta.y))
        event.post(tap: .cghidEventTap)
    }

    static func screen(containingCG point: CGPoint) -> NSScreen? {
        NSScreen.screens.first { cgFrame(of: $0).contains(point) } ?? NSScreen.main
    }

    static func cgFrame(of screen: NSScreen) -> CGRect {
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        return CGRect(x: screen.frame.minX, y: desktopTop - screen.frame.maxY,
                      width: screen.frame.width, height: screen.frame.height)
    }
}
