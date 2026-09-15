import AppKit
import ApplicationServices
import CoreGraphics

/// Hold ⌘⌥ on its own and, after a beat, the wall answers: a soft chime and
/// one large arrow centered in every reachable prompt window. Each arrow is
/// the key that will move focus to that window and wears its accent color.
/// Release ⌘⌥ and the arrows fade.
///
/// The arrows are a *hint*, not a mode — the ⌘⌥-arrow hotkeys (WindowNav) work
/// exactly the same whether it is visible or not.

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

/// Owns the destination panels and the hold/reveal state machine. Main-thread only —
/// every entry point is dispatched there by NavHoldTap, and WindowNav.jump
/// already runs on main (Carbon hotkey callbacks do).
final class NavHoldHint {
    static let shared = NavHoldHint()
    private init() {}

    private static let holdDelay: TimeInterval = 0
    /// Raise + app activation settle on the next WindowServer turn; re-reading
    /// focus immediately after a jump would still see the old pane.
    private static let refreshDelay: TimeInterval = 0.08

    private var panels: [Int: NSPanel] = [:]
    private var hints: [Int: NavHintArrowView] = [:]
    private var currentPanel: NSPanel?
    private var currentHighlight: NavCurrentHighlightView?
    private var showItem: DispatchWorkItem?
    private var refreshItem: DispatchWorkItem?
    private var holding = false
    private var disarmed = false
    private var visible = false
    private var chimed = false
    private var focusedWindowID: Int?

    func beginHold() {
        holding = true
        disarmed = false
        chimed = false
        focusedWindowID = nil
        scheduleShow()
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

    /// WindowNav.jump reports every arrow it acted on, so the hints track the
    /// walk even when Carbon consumes the arrow keyDown before our tap sees
    /// it. A fleet hop hands input to another machine — the pad bows out.
    func noteJump(local: Bool, focusedWindowID: Int?) {
        guard visible else { return }
        if local {
            self.focusedWindowID = focusedWindowID
            presentOrRefresh()
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

    private func presentOrRefresh() {
        guard holding, !disarmed, AXTiler.trusted else { return }
        let snapshot = WindowNav.hintSnapshot(focusedWindowID: focusedWindowID)
        guard let current = snapshot.current else {
            if visible { dismiss() }
            return
        }

        let colorByID = Dictionary(
            PromptSigilOverlayController.shared.promptParticleTargets
                .map { ($0.windowID, $0.color) },
            uniquingKeysWith: { first, _ in first })

        let activeIDs = Set(snapshot.targets.map(\.windowID))
        for (id, panel) in panels where !activeIDs.contains(id) {
            panel.alphaValue = 0
            panel.orderOut(nil)
        }

        for target in snapshot.targets {
            let (panel, hint) = ensurePanel(for: target.windowID)
            hint.direction = target.direction
            hint.color = colorByID[target.windowID] ?? .controlAccentColor
            position(panel, over: target.frame)
            if panel.alphaValue == 0 {
                panel.orderFrontRegardless()
                NSAnimationContext.runAnimationGroup { ctx in
                    ctx.duration = 0.14
                    panel.animator().alphaValue = 1
                }
            }
        }

        let (focusPanel, highlight) = ensureCurrentPanel()
        highlight.setActive(true)
        positionCurrent(focusPanel, over: current.frame)
        if focusPanel.alphaValue == 0 {
            focusPanel.alphaValue = 1
            focusPanel.orderFrontRegardless()
        }
        visible = true
        if !chimed {
            chimed = true
            PopSound.playNavReady()
        }
    }

    private func dismiss() {
        visible = false
        focusedWindowID = nil
        for panel in panels.values {
            panel.alphaValue = 0
            panel.orderOut(nil)
        }
        currentPanel?.alphaValue = 0
        currentPanel?.orderOut(nil)
        currentHighlight?.setActive(false)
    }

    private func ensurePanel(for windowID: Int) -> (NSPanel, NavHintArrowView) {
        if let panel = panels[windowID], let hint = hints[windowID] { return (panel, hint) }
        let size = NavHintArrowView.hintSize
        let view = NavHintArrowView(frame: CGRect(origin: .zero, size: size))
        let panel = NSPanel(contentRect: CGRect(origin: .zero, size: size),
                            styleMask: [.borderless, .nonactivatingPanel],
                            backing: .buffered, defer: false)
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = false
        panel.level = .statusBar
        panel.ignoresMouseEvents = true
        panel.hidesOnDeactivate = false
        panel.collectionBehavior = [.canJoinAllSpaces, .stationary, .transient,
                                    .ignoresCycle, .fullScreenAuxiliary]
        panel.contentView = view
        panel.alphaValue = 0
        panels[windowID] = panel
        hints[windowID] = view
        return (panel, view)
    }

    private func position(_ panel: NSPanel, over targetFrame: CGRect) {
        let size = NavHintArrowView.hintSize
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        let appKit = CGRect(x: targetFrame.minX, y: desktopTop - targetFrame.maxY,
                            width: targetFrame.width, height: targetFrame.height)
        let center = CGPoint(x: appKit.midX, y: appKit.midY)
        var frame = CGRect(x: center.x - size.width / 2, y: center.y - size.height / 2,
                           width: size.width, height: size.height)
        if let visibleArea = NSScreen.screens.first(where: { $0.frame.intersects(appKit) })?.visibleFrame {
            frame.origin.x = min(max(frame.origin.x, visibleArea.minX + 8),
                                 visibleArea.maxX - size.width - 8)
            frame.origin.y = min(max(frame.origin.y, visibleArea.minY + 8),
                                 visibleArea.maxY - size.height - 8)
        }
        panel.setFrame(frame, display: true)
    }

    private func ensureCurrentPanel() -> (NSPanel, NavCurrentHighlightView) {
        if let currentPanel, let currentHighlight { return (currentPanel, currentHighlight) }
        let view = NavCurrentHighlightView(frame: .zero)
        view.autoresizingMask = [.width, .height]
        let panel = NSPanel(contentRect: .zero, styleMask: [.borderless, .nonactivatingPanel],
                            backing: .buffered, defer: false)
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = false
        panel.level = .statusBar
        panel.ignoresMouseEvents = true
        panel.hidesOnDeactivate = false
        panel.collectionBehavior = [.canJoinAllSpaces, .stationary, .transient,
                                    .ignoresCycle, .fullScreenAuxiliary]
        panel.contentView = view
        panel.alphaValue = 0
        currentPanel = panel
        currentHighlight = view
        return (panel, view)
    }

    private func positionCurrent(_ panel: NSPanel, over targetFrame: CGRect) {
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        let frame = CGRect(x: targetFrame.minX, y: desktopTop - targetFrame.maxY,
                           width: targetFrame.width, height: targetFrame.height)
        panel.setFrame(frame, display: true)
    }
}
/// One key, centered on the window that pressing it will focus.
final class NavHintArrowView: NSView {
    static let hintSize = CGSize(width: 112, height: 112)

    var direction: WindowNav.Direction? {
        didSet { needsDisplay = true }
    }
    var color: NSColor = .controlAccentColor {
        didSet { needsDisplay = true }
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let direction else { return }
        let size: CGFloat = 88
        drawKey(direction, in: CGRect(x: bounds.midX - size / 2,
                                      y: bounds.midY - size / 2,
                                      width: size, height: size))
    }

    private func drawKey(_ direction: WindowNav.Direction, in rect: CGRect) {
        let cap = NSBezierPath(roundedRect: rect, xRadius: rect.width * 0.22,
                               yRadius: rect.height * 0.22)
        NSGraphicsContext.saveGraphicsState()
        let shadow = NSShadow()
        shadow.shadowColor = color.withAlphaComponent(0.95)
        shadow.shadowBlurRadius = 18
        shadow.set()
        NSColor.black.withAlphaComponent(0.82).setFill()
        cap.fill()
        NSGraphicsContext.restoreGraphicsState()
        color.setStroke()
        cap.lineWidth = max(3, rect.width * 0.045)
        cap.stroke()

        let glyph = direction.glyph
        let style = NSMutableParagraphStyle()
        style.alignment = .center
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: rect.height * 0.62, weight: .black),
            .foregroundColor: color,
            .paragraphStyle: style,
        ]
        let glyphRect = CGRect(x: rect.minX, y: rect.midY - rect.height * 0.42,
                               width: rect.width, height: rect.height * 0.84)
        glyph.draw(in: glyphRect, withAttributes: attrs)
    }
}

/// A strong, temporary frame around the currently selected prompt.
final class NavCurrentHighlightView: NSView {
    private var timer: Timer?
    private var yellowPhase = false

    func setActive(_ active: Bool) {
        if active {
            guard timer == nil else { return }
            yellowPhase = false
            needsDisplay = true
            timer = Timer.scheduledTimer(withTimeInterval: 0.24, repeats: true) {
                [weak self] _ in
                guard let self else { return }
                self.yellowPhase.toggle()
                self.needsDisplay = true
            }
        } else {
            timer?.invalidate()
            timer = nil
            yellowPhase = false
            needsDisplay = true
        }
    }

    override func draw(_ dirtyRect: NSRect) {
        let color = yellowPhase ? NSColor.systemYellow : NSColor.white
        let frame = NSBezierPath(roundedRect: bounds.insetBy(dx: 7, dy: 7),
                                 xRadius: 13, yRadius: 13)
        NSGraphicsContext.saveGraphicsState()
        let glow = NSShadow()
        glow.shadowColor = color
        glow.shadowBlurRadius = 18
        glow.set()
        color.withAlphaComponent(0.98).setStroke()
        frame.lineWidth = 8
        frame.stroke()
        NSGraphicsContext.restoreGraphicsState()
    }

    deinit { timer?.invalidate() }
}
