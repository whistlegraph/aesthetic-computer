import AppKit
import ApplicationServices
import CoreGraphics

/// Hold ⌘⌥ on its own and, after a beat, the wall answers: a soft chime and a
/// small arrow-key pad floating over the focused prompt, each lit keycap
/// tinted with the color of the pane that arrow would land on. Directions
/// whose next stop is another machine (Deskflow) show as hollow bright caps;
/// dead directions stay dim. Release ⌘⌥ and it fades.
///
/// The pad is a *hint*, not a mode — the ⌘⌥-arrow hotkeys (WindowNav) work
/// exactly the same whether it is visible or not. The hold delay exists so
/// ordinary ⌘⌥ chords (⌘⌥T, ⌘⌥I, a quick app shortcut) never flash it.

/// Global "bare ⌘⌥ is being held" detector. A `CGEvent.tapCreate` listener
/// for the same reason CtrlDoubleTap is one: `NSEvent` global monitors go
/// silent after codesign rebuilds, while tapCreate fails loudly when the
/// Accessibility grant is broken.
final class NavHoldTap {
    private let onHoldStart: () -> Void
    private let onHoldEnd: () -> Void
    private let onChordKey: () -> Void
    private let onPointerDown: () -> Void
    private var tap: CFMachPort?
    private var source: CFRunLoopSource?
    private var holding = false

    init(onHoldStart: @escaping () -> Void,
         onHoldEnd: @escaping () -> Void,
         onChordKey: @escaping () -> Void,
         onPointerDown: @escaping () -> Void) {
        self.onHoldStart = onHoldStart
        self.onHoldEnd = onHoldEnd
        self.onChordKey = onChordKey
        self.onPointerDown = onPointerDown
    }

    @discardableResult
    func start() -> Bool {
        guard tap == nil else { return true }
        guard ProcessInfo.processInfo.environment["SLAB_DISABLE_EVENT_TAPS"] != "1" else {
            NSLog("slab nav hint: event tap disabled for this host")
            return false
        }
        guard AXIsProcessTrusted() else {
            NSLog("slab nav hint: Accessibility not trusted; skipping event tap")
            return false
        }
        let mask: CGEventMask =
            (1 << CGEventType.flagsChanged.rawValue) |
            (1 << CGEventType.keyDown.rawValue) |
            (1 << CGEventType.leftMouseDown.rawValue) |
            (1 << CGEventType.rightMouseDown.rawValue) |
            (1 << CGEventType.otherMouseDown.rawValue)

        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon = refcon else { return Unmanaged.passUnretained(event) }
            let me = Unmanaged<NavHoldTap>.fromOpaque(refcon).takeUnretainedValue()
            me.handle(type: type, event: event)
            return Unmanaged.passUnretained(event)
        }

        guard let port = CGEvent.tapCreate(
            tap: .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .listenOnly,
            eventsOfInterest: mask,
            callback: callback,
            userInfo: Unmanaged.passUnretained(self).toOpaque()
        ) else {
            NSLog("slab nav hint: ⌘⌥ hold tap creation failed — Accessibility not trusted?")
            return false
        }
        tap = port
        let src = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, port, 0)
        source = src
        CFRunLoopAddSource(CFRunLoopGetMain(), src, .commonModes)
        CGEvent.tapEnable(tap: port, enable: true)
        return true
    }

    func stop() {
        if let src = source { CFRunLoopRemoveSource(CFRunLoopGetMain(), src, .commonModes) }
        if let port = tap { CGEvent.tapEnable(tap: port, enable: false) }
        source = nil
        tap = nil
    }

    private func handle(type: CGEventType, event: CGEvent) {
        if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
            if let port = tap { CGEvent.tapEnable(tap: port, enable: true) }
            return
        }
        switch type {
        case .flagsChanged:
            // Bare ⌘⌥ only: ⇧ or ⌃ alongside means some other chord family.
            // Fn is deliberately NOT a disqualifier — some keyboards raise it
            // alongside arrows, and it must not end the hold mid-navigation.
            let flags = event.flags
            let bare = flags.contains(.maskCommand) && flags.contains(.maskAlternate)
                && !flags.contains(.maskShift) && !flags.contains(.maskControl)
            if bare != holding {
                holding = bare
                let cb = bare ? onHoldStart : onHoldEnd
                DispatchQueue.main.async(execute: cb)
            }
        case .keyDown:
            if holding { DispatchQueue.main.async(execute: onChordKey) }
        case .leftMouseDown, .rightMouseDown, .otherMouseDown:
            // A click while holding ⌘⌥ is some other gesture entirely
            // (Finder's ⌘⌥-drag makes aliases) — stand down for this hold.
            if holding { DispatchQueue.main.async(execute: onPointerDown) }
        default:
            break
        }
    }
}

/// Owns the pad panel and the hold/reveal state machine. Main-thread only —
/// every entry point is dispatched there by NavHoldTap, and WindowNav.jump
/// already runs on main (Carbon hotkey callbacks do).
final class NavHoldHint {
    static let shared = NavHoldHint()
    private init() {}

    /// Long enough that a briskly typed ⌘⌥ chord finishes first; short enough
    /// that a genuine "which way?" pause is answered promptly.
    private static let holdDelay: TimeInterval = 0.35
    /// Raise + app activation settle on the next WindowServer turn; re-reading
    /// focus immediately after a jump would still see the old pane.
    private static let refreshDelay: TimeInterval = 0.30

    private var panel: NSPanel?
    private var pad: NavHintPadView?
    private var showItem: DispatchWorkItem?
    private var refreshItem: DispatchWorkItem?
    private var holding = false
    private var disarmed = false
    private var visible = false
    private var chimed = false
    private var fleetDirections: Set<WindowNav.Direction> = []
    private var fleetGeneration = 0

    func beginHold() {
        holding = true
        disarmed = false
        chimed = false
        fleetDirections = []
        scheduleShow()
        fetchFleetDirections()
    }

    func endHold() {
        holding = false
        showItem?.cancel()
        showItem = nil
        refreshItem?.cancel()
        refreshItem = nil
        dismiss()
    }

    /// A non-arrow key landed during the hold. Before the reveal it means a
    /// chord is in progress — push the reveal back so it never flashes over
    /// one. After the reveal (⌘⌥T retile, say) the wall may have changed —
    /// re-read it.
    func noteChordKey() {
        guard holding, !disarmed else { return }
        if visible { scheduleRefresh() } else { scheduleShow() }
    }

    func notePointerDown() {
        guard holding else { return }
        disarmed = true
        showItem?.cancel()
        showItem = nil
        dismiss()
    }

    /// WindowNav.jump reports every arrow it acted on, so the pad tracks the
    /// walk even when Carbon consumes the arrow keyDown before our tap sees
    /// it. A fleet hop hands input to another machine — the pad bows out.
    func noteJump(local: Bool) {
        guard visible else { return }
        if local {
            scheduleRefresh()
        } else {
            disarmed = true
            dismiss()
        }
    }

    private func scheduleShow() {
        showItem?.cancel()
        let item = DispatchWorkItem { [weak self] in
            self?.showItem = nil
            self?.presentOrRefresh()
        }
        showItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + Self.holdDelay, execute: item)
    }

    private func scheduleRefresh() {
        refreshItem?.cancel()
        let item = DispatchWorkItem { [weak self] in
            self?.refreshItem = nil
            self?.presentOrRefresh()
        }
        refreshItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + Self.refreshDelay, execute: item)
    }

    /// Deskflow geometry + peer ledgers are file reads — cheap, but not
    /// main-thread work. The pad paints local arrows immediately and lights
    /// the fleet edges when this lands.
    private func fetchFleetDirections() {
        fleetGeneration += 1
        let generation = fleetGeneration
        DispatchQueue.global(qos: .userInitiated).async {
            let dirs = DeskflowSpatialNav.availableCrossDirections()
            DispatchQueue.main.async { [weak self] in
                guard let self, self.holding, generation == self.fleetGeneration else { return }
                self.fleetDirections = dirs
                // If the reveal already fired but found nothing local, a
                // fleet-only wall still deserves the pad.
                if self.visible || self.showItem == nil { self.presentOrRefresh() }
            }
        }
    }

    private func presentOrRefresh() {
        guard holding, !disarmed, AXTiler.trusted else { return }
        let snapshot = WindowNav.hintSnapshot()
        guard !snapshot.targets.isEmpty || !fleetDirections.isEmpty else {
            if visible { dismiss() }
            return
        }

        // Tint each lit cap with the destination pane's own accent, so the
        // pad reads as "that green one is to the right", not just "right works".
        let colorByID = Dictionary(
            PromptSigilOverlayController.shared.promptParticleTargets
                .map { ($0.windowID, $0.color) },
            uniquingKeysWith: { first, _ in first })
        var visuals: [WindowNav.Direction: NavHintPadView.CapVisual] = [:]
        for dir in WindowNav.Direction.allCases {
            if let id = snapshot.targets[dir] {
                visuals[dir] = .init(color: colorByID[id] ?? .controlAccentColor, fleet: false)
            } else {
                visuals[dir] = .init(color: nil, fleet: fleetDirections.contains(dir))
            }
        }

        let (panel, pad) = ensurePanel()
        pad.visuals = visuals
        position(panel, over: snapshot.originFrame)
        if !visible {
            visible = true
            panel.orderFrontRegardless()
            NSAnimationContext.runAnimationGroup { ctx in
                ctx.duration = 0.14
                panel.animator().alphaValue = 1
            }
        }
        if !chimed {
            chimed = true
            PopSound.playNavReady()
        }
    }

    private func dismiss() {
        guard visible, let panel else {
            visible = false
            return
        }
        visible = false
        NSAnimationContext.runAnimationGroup({ ctx in
            ctx.duration = 0.16
            panel.animator().alphaValue = 0
        }, completionHandler: { [weak self] in
            guard let self, !self.visible else { return }
            self.panel?.orderOut(nil)
        })
    }

    private func ensurePanel() -> (NSPanel, NavHintPadView) {
        if let panel, let pad { return (panel, pad) }
        let view = NavHintPadView(frame: CGRect(origin: .zero, size: NavHintPadView.padSize))
        let newPanel = NSPanel(contentRect: CGRect(origin: .zero, size: NavHintPadView.padSize),
                               styleMask: [.borderless, .nonactivatingPanel],
                               backing: .buffered, defer: false)
        newPanel.isOpaque = false
        newPanel.backgroundColor = .clear
        newPanel.hasShadow = false
        newPanel.level = .statusBar
        newPanel.ignoresMouseEvents = true
        newPanel.hidesOnDeactivate = false
        newPanel.collectionBehavior = [.canJoinAllSpaces, .stationary, .transient,
                                       .ignoresCycle, .fullScreenAuxiliary]
        newPanel.contentView = view
        newPanel.alphaValue = 0
        panel = newPanel
        pad = view
        return (newPanel, view)
    }

    private func position(_ panel: NSPanel, over originFrame: CGRect?) {
        let size = NavHintPadView.padSize
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        var center: CGPoint
        var screen: NSScreen?
        if let f = originFrame {
            let appKit = CGRect(x: f.minX, y: desktopTop - f.maxY,
                                width: f.width, height: f.height)
            center = CGPoint(x: appKit.midX, y: appKit.midY)
            screen = NSScreen.screens.first { $0.frame.intersects(appKit) } ?? NSScreen.main
        } else {
            screen = NSScreen.main
            center = screen.map { CGPoint(x: $0.frame.midX, y: $0.frame.midY) } ?? .zero
        }
        var frame = CGRect(x: center.x - size.width / 2, y: center.y - size.height / 2,
                           width: size.width, height: size.height)
        if let visibleArea = screen?.visibleFrame {
            frame.origin.x = min(max(frame.origin.x, visibleArea.minX + 8),
                                 visibleArea.maxX - size.width - 8)
            frame.origin.y = min(max(frame.origin.y, visibleArea.minY + 8),
                                 visibleArea.maxY - size.height - 8)
        }
        panel.setFrame(frame, display: true)
    }
}

/// The inverted-T keypad itself: ← ↓ → along the bottom, ↑ centered above,
/// exactly as the keys sit under the hand. Lit = a pane is there (filled with
/// that pane's accent); hollow bright = the next stop is another machine;
/// dim = nothing that way.
final class NavHintPadView: NSView {
    struct CapVisual {
        var color: NSColor?
        var fleet: Bool
    }

    static let capSize: CGFloat = 46
    static let gap: CGFloat = 7
    static let inset: CGFloat = 13
    static var padSize: CGSize {
        CGSize(width: inset * 2 + capSize * 3 + gap * 2,
               height: inset * 2 + capSize * 2 + gap)
    }

    var visuals: [WindowNav.Direction: CapVisual] = [:] {
        didSet { needsDisplay = true }
    }

    override func draw(_ dirtyRect: NSRect) {
        let card = NSBezierPath(roundedRect: bounds.insetBy(dx: 0.5, dy: 0.5),
                                xRadius: 20, yRadius: 20)
        NSColor.black.withAlphaComponent(0.55).setFill()
        card.fill()
        NSColor.white.withAlphaComponent(0.10).setStroke()
        card.lineWidth = 1
        card.stroke()
        for dir in WindowNav.Direction.allCases {
            drawCap(dir, visual: visuals[dir] ?? CapVisual(color: nil, fleet: false))
        }
    }

    private func capRect(_ dir: WindowNav.Direction) -> CGRect {
        let s = Self.capSize, g = Self.gap, i = Self.inset
        switch dir {
        case .left: return CGRect(x: i, y: i, width: s, height: s)
        case .down: return CGRect(x: i + s + g, y: i, width: s, height: s)
        case .right: return CGRect(x: i + (s + g) * 2, y: i, width: s, height: s)
        case .up: return CGRect(x: i + s + g, y: i + s + g, width: s, height: s)
        }
    }

    private func drawCap(_ dir: WindowNav.Direction, visual: CapVisual) {
        let rect = capRect(dir)
        let cap = NSBezierPath(roundedRect: rect, xRadius: 9, yRadius: 9)
        if let color = visual.color {
            NSGraphicsContext.saveGraphicsState()
            let glow = NSShadow()
            glow.shadowColor = color.withAlphaComponent(0.9)
            glow.shadowBlurRadius = 10
            glow.set()
            color.withAlphaComponent(0.92).setFill()
            cap.fill()
            NSGraphicsContext.restoreGraphicsState()
            drawArrow(dir, in: rect, color: NSColor.black.withAlphaComponent(0.78))
        } else if visual.fleet {
            NSColor.white.withAlphaComponent(0.55).setStroke()
            cap.lineWidth = 1.5
            cap.stroke()
            drawArrow(dir, in: rect, color: NSColor.white.withAlphaComponent(0.80))
        } else {
            NSColor.white.withAlphaComponent(0.13).setStroke()
            cap.lineWidth = 1
            cap.stroke()
            drawArrow(dir, in: rect, color: NSColor.white.withAlphaComponent(0.22))
        }
    }

    private func drawArrow(_ dir: WindowNav.Direction, in rect: CGRect, color: NSColor) {
        let center = CGPoint(x: rect.midX, y: rect.midY)
        // One up-pointing triangle, rotated per direction (AppKit y grows up).
        let points = [CGPoint(x: 0, y: 8), CGPoint(x: -7, y: -5.5), CGPoint(x: 7, y: -5.5)]
        let angle: CGFloat
        switch dir {
        case .up: angle = 0
        case .down: angle = .pi
        case .left: angle = .pi / 2
        case .right: angle = -.pi / 2
        }
        func rotated(_ p: CGPoint) -> CGPoint {
            CGPoint(x: center.x + p.x * cos(angle) - p.y * sin(angle),
                    y: center.y + p.x * sin(angle) + p.y * cos(angle))
        }
        let path = NSBezierPath()
        path.move(to: rotated(points[0]))
        path.line(to: rotated(points[1]))
        path.line(to: rotated(points[2]))
        path.close()
        color.setFill()
        path.fill()
    }
}
