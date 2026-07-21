import AppKit
import ApplicationServices
import QuartzCore

@_silgen_name("_AXUIElementGetWindow")
private func _FocusAXUIElementGetWindow(
    _ element: AXUIElement,
    _ windowID: UnsafeMutablePointer<CGWindowID>
) -> AXError

/// A quiet, persistent green light behind the focused prompt pane.
/// The light has no visible border to betray small AX/window-frame differences;
/// instead, a few luminous drops fall from the pane's lower edge. Unlike the
/// navigation burst this follows every ordinary focus change, mouse click,
/// move, resize, Space change, and accent-colour change.
final class PromptFocusHighlight {
    static let shared = PromptFocusHighlight()

    private static let terminalBundleIds = ["com.apple.Terminal", "com.googlecode.iterm2"]
    private static let focusGreen = NSColor(
        srgbRed: 0.16, green: 1.0, blue: 0.43, alpha: 1.0)
    private static let haloPadding: CGFloat = 24
    private static let dropDepth: CGFloat = 190
    private var panel: NSPanel?
    private var backdrop: CAShapeLayer?
    private var emitter: CAEmitterLayer?
    private var timer: Timer?
    private var observers: [pid_t: AXObserver] = [:]
    private var workspaceObserver: NSObjectProtocol?
    private var lastFrame = CGRect.null
    private var lastColor: NSColor?
    private var lastWindowID: CGWindowID = 0

    private init() {}

    private static let axCallback: AXObserverCallback = { _, _, notification, _ in
        let note = notification as String
        let reorder = note == kAXFocusedWindowChangedNotification as String
            || note == kAXMainWindowChangedNotification as String
            || note == kAXApplicationActivatedNotification as String
        DispatchQueue.main.async {
            PromptFocusHighlight.shared.refreshNow(reorder: reorder)
        }
    }

    func start() {
        guard timer == nil else { return }
        workspaceObserver = NSWorkspace.shared.notificationCenter.addObserver(
            forName: NSWorkspace.didActivateApplicationNotification,
            object: nil, queue: .main
        ) { _ in PromptFocusHighlight.shared.refreshNow(reorder: true) }
        installObservers()
        let t = Timer(timeInterval: 1.0, repeats: true) { [weak self] _ in
            self?.installObservers()
            self?.refreshNow()
        }
        timer = t
        RunLoop.main.add(t, forMode: .common)
        refreshNow()
    }

    func stop() {
        timer?.invalidate(); timer = nil
        if let workspaceObserver {
            NSWorkspace.shared.notificationCenter.removeObserver(workspaceObserver)
        }
        workspaceObserver = nil
        for (_, observer) in observers {
            CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer), .commonModes)
        }
        observers.removeAll()
        panel?.orderOut(nil)
    }

    func refreshNow(reorder: Bool = false) {
        precondition(Thread.isMainThread)
        guard AXTiler.trusted, let focused = WindowNav.focusedWindow(),
              isTerminalWindow(focused), let frame = AXTiler.frame(focused),
              frame.width >= 80, frame.height >= 80,
              let windowID = windowID(of: focused) else {
            hide()
            lastFrame = .null
            lastWindowID = 0
            return
        }
        let promptIDs = PromptSigilOverlayController.shared.promptWindowIDs
        guard promptIDs.isEmpty || promptIDs.contains(Int(windowID)) else {
            hide()
            lastFrame = .null
            lastWindowID = 0
            return
        }

        // Selection is fleet-green on every host. Using the local macOS accent
        // made this disappear into Blueberry's red Terminal palette.
        let color = Self.focusGreen
        if panel == nil { makePanel() }
        guard let panel, let backdrop, let emitter else { return }

        // Give the effect its own snug window instead of drawing a path in a
        // screen-sized panel. Besides being cheaper, this keeps the emitter's
        // edge mechanically tied to the focused window during moves/resizes.
        let windowFrame = appKitFrame(for: frame)
        let panelFrame = CGRect(
            x: windowFrame.minX - Self.haloPadding,
            y: windowFrame.minY - Self.haloPadding - Self.dropDepth,
            width: windowFrame.width + Self.haloPadding * 2,
            height: windowFrame.height + Self.haloPadding * 2 + Self.dropDepth)
        if panel.frame != panelFrame {
            panel.setFrame(panelFrame, display: false)
        }
        // AppKit may constrain a tall borderless panel below the menu bar (or
        // back onto a display edge). Derive local geometry from the resulting
        // frame, not the requested one, or the source visibly misses the
        // terminal edge by exactly that constraint delta.
        let actualPanelFrame = panel.frame
        let subject = CGRect(
            x: windowFrame.minX - actualPanelFrame.minX,
            y: windowFrame.minY - actualPanelFrame.minY,
            width: windowFrame.width,
            height: windowFrame.height)

        if frame != lastFrame || color != lastColor {
            // `path` has a default implicit Core Animation transition. During
            // a drag that makes the glow perpetually ease toward geometry from
            // ~250 ms ago — the visible "sluggish" trail. Geometry is input,
            // not animation: apply it atomically on the AX frame that supplied it.
            CATransaction.begin()
            CATransaction.setDisableActions(true)
            // Only the underside is drawn in front of the window stack. A
            // full-window shape would tint the terminal; a perimeter would
            // recreate the mismatched ring this effect replaces.
            let source = CGRect(x: subject.minX + 14, y: subject.minY - 2,
                                width: max(12, subject.width - 28), height: 3)
            backdrop.path = CGPath(roundedRect: source, cornerWidth: 1.5,
                                   cornerHeight: 1.5, transform: nil)
            backdrop.fillColor = color.withAlphaComponent(0.48).cgColor
            backdrop.shadowColor = color.cgColor
            emitter.frame = panel.contentView?.bounds ?? .zero
            emitter.emitterPosition = CGPoint(x: subject.midX, y: subject.minY + 1)
            emitter.emitterSize = CGSize(width: max(24, subject.width - 24), height: 1)
            emitter.emitterCells?.forEach {
                $0.color = color.withAlphaComponent(0.90).cgColor
            }
            CATransaction.commit()
            lastFrame = frame
            lastColor = color
        }
        emitter.birthRate = 1
        // The overlay contains pixels only at and below the lower edge. Keeping
        // that small effect above the normal-window stack is reliable even when
        // another tiled Terminal sits directly behind the selected one.
        if reorder || lastWindowID != windowID || !panel.isVisible {
            panel.orderFrontRegardless()
            lastWindowID = windowID
        }
    }

    private func hide() {
        emitter?.birthRate = 0
        panel?.orderOut(nil)
    }

    private func installObservers() {
        guard AXIsProcessTrusted() else { return }
        let livePids = Set(NSWorkspace.shared.runningApplications.compactMap { app -> pid_t? in
            Self.terminalBundleIds.contains(app.bundleIdentifier ?? "")
                ? app.processIdentifier : nil
        })
        for (pid, observer) in observers where !livePids.contains(pid) {
            CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer), .commonModes)
            observers.removeValue(forKey: pid)
        }
        for pid in livePids where observers[pid] == nil {
            var observer: AXObserver?
            guard AXObserverCreate(pid, Self.axCallback, &observer) == .success,
                  let observer else { continue }
            let app = AXUIElementCreateApplication(pid)
            for note in [kAXFocusedWindowChangedNotification,
                         kAXMainWindowChangedNotification,
                         kAXApplicationActivatedNotification,
                         kAXWindowMovedNotification,
                         kAXWindowResizedNotification] {
                AXObserverAddNotification(observer, app, note as CFString, nil)
            }
            // Window drags run the main loop in event-tracking mode. Common
            // modes keep geometry callbacks flowing while the mouse is held,
            // instead of delivering one stale jump only after mouse-up.
            CFRunLoopAddSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer), .commonModes)
            observers[pid] = observer
        }
    }

    private func isTerminalWindow(_ window: AXUIElement) -> Bool {
        var pid: pid_t = 0
        guard AXUIElementGetPid(window, &pid) == .success,
              let bundle = NSRunningApplication(processIdentifier: pid)?.bundleIdentifier else {
            return false
        }
        return Self.terminalBundleIds.contains(bundle)
    }

    private func windowID(of window: AXUIElement) -> CGWindowID? {
        var id = CGWindowID(0)
        guard _FocusAXUIElementGetWindow(window, &id) == .success, id != 0 else { return nil }
        return id
    }

    private func makePanel() {
        let window = NSPanel(contentRect: .zero,
                             styleMask: [.borderless, .nonactivatingPanel],
                             backing: .buffered, defer: false)
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false
        window.level = .floating
        window.ignoresMouseEvents = true
        window.hidesOnDeactivate = false
        window.collectionBehavior = [.canJoinAllSpaces, .stationary,
                                     .ignoresCycle, .fullScreenAuxiliary]
        let view = NSView()
        view.wantsLayer = true
        view.layer?.masksToBounds = true

        let shape = CAShapeLayer()
        shape.shadowOpacity = 0.80
        shape.shadowRadius = 17
        shape.shadowOffset = .zero
        shape.actions = ["path": NSNull(), "fillColor": NSNull(),
                         "shadowColor": NSNull(), "position": NSNull(),
                         "bounds": NSNull()]
        view.layer?.addSublayer(shape)

        let drops = CAEmitterLayer()
        drops.emitterShape = .line
        drops.emitterMode = .surface
        drops.renderMode = .additive
        drops.birthRate = 0
        drops.emitterCells = [Self.dropCell(), Self.moteCell()]
        drops.actions = ["frame": NSNull(), "position": NSNull(),
                         "bounds": NSNull(), "emitterPosition": NSNull(),
                         "emitterSize": NSNull(), "birthRate": NSNull()]
        view.layer?.addSublayer(drops)

        window.contentView = view
        panel = window
        backdrop = shape
        emitter = drops
    }

    private static func dropCell() -> CAEmitterCell {
        let cell = CAEmitterCell()
        cell.contents = dropParticle
        cell.birthRate = 17
        cell.lifetime = 2.45
        cell.lifetimeRange = 0.70
        cell.velocity = 34
        cell.velocityRange = 15
        cell.yAcceleration = -34
        cell.emissionLongitude = -.pi / 2
        cell.emissionRange = .pi / 20
        cell.scale = 0.44
        cell.scaleRange = 0.18
        cell.scaleSpeed = -0.09
        cell.alphaSpeed = -0.31
        cell.spinRange = 0.22
        return cell
    }

    private static func moteCell() -> CAEmitterCell {
        let cell = CAEmitterCell()
        cell.contents = moteParticle
        cell.birthRate = 9
        cell.lifetime = 1.85
        cell.lifetimeRange = 0.55
        cell.velocity = 20
        cell.velocityRange = 11
        cell.yAcceleration = -22
        cell.emissionLongitude = -.pi / 2
        cell.emissionRange = .pi / 10
        cell.scale = 0.24
        cell.scaleRange = 0.11
        cell.scaleSpeed = -0.07
        cell.alphaSpeed = -0.43
        return cell
    }

    private static let dropParticle: CGImage? = particleImage(size: CGSize(width: 14, height: 22))
    private static let moteParticle: CGImage? = particleImage(size: CGSize(width: 10, height: 10))

    /// White radial sprites are tinted by `CAEmitterCell.color`, allowing the
    /// same drops to follow each host's live macOS accent colour.
    private static func particleImage(size: CGSize) -> CGImage? {
        let width = Int(size.width)
        let height = Int(size.height)
        guard let context = CGContext(
            data: nil, width: width, height: height, bitsPerComponent: 8,
            bytesPerRow: width * 4, space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue),
              let gradient = CGGradient(
                colorsSpace: CGColorSpaceCreateDeviceRGB(),
                colors: [NSColor.white.cgColor,
                         NSColor.white.withAlphaComponent(0.55).cgColor,
                         NSColor.clear.cgColor] as CFArray,
                locations: [0, 0.34, 1]) else { return nil }
        context.scaleBy(x: 1, y: size.height / size.width)
        let center = CGPoint(x: size.width / 2, y: size.width / 2)
        context.drawRadialGradient(
            gradient, startCenter: center, startRadius: 0,
            endCenter: center, endRadius: size.width / 2,
            options: [.drawsAfterEndLocation])
        return context.makeImage()
    }

    private func appKitFrame(for cgFrame: CGRect) -> CGRect {
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        return CGRect(x: cgFrame.minX, y: desktopTop - cgFrame.maxY,
                      width: cgFrame.width, height: cgFrame.height)
    }
}
