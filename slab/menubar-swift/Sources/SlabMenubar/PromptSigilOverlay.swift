import AppKit
import CoreGraphics
import ApplicationServices
import SceneKit

/// Private (but stable, widely-used by window managers) bridge from an AX
/// window element to its CGWindowID — lets a kAXWindowMoved callback map the
/// moved window back to the badge bound to it.
@_silgen_name("_AXUIElementGetWindow")
private func _AXUIElementGetWindow(_ element: AXUIElement, _ windowID: UnsafeMutablePointer<CGWindowID>) -> AXError

/// The global sun that lights every stone, driven by the local clock: it rises
/// in the east, arcs overhead at midday, sets in the west, and drops long
/// shadows near dawn/dusk. Deliberately simple (a daylight arc from local time,
/// no ephemeris) — enough for the light to feel like it belongs to the room's
/// time of day. `hx` is horizontal position (+1 east/right … −1 west/left),
/// `elevation` height (0 horizon … 1 overhead), `intensity` strength, and
/// `drop` the flat shadow offset (screen points, away from the sun).
enum Sun {
    static func light(at date: Date) -> (hx: CGFloat, elevation: CGFloat, intensity: CGFloat, drop: CGSize) {
        let c = Calendar.current.dateComponents([.hour, .minute], from: date)
        let h = CGFloat(c.hour ?? 12) + CGFloat(c.minute ?? 0) / 60
        let sunrise: CGFloat = 6.5, sunset: CGFloat = 19.5
        let hx: CGFloat, e: CGFloat, intensity: CGFloat
        if h < sunrise || h > sunset {
            hx = 0; e = 0.12; intensity = 0.32          // night: dim, near-overhead
        } else {
            let t = (h - sunrise) / (sunset - sunrise)  // 0 dawn … 1 dusk
            hx = cos(t * .pi)                            // +1 east → −1 west
            e = sin(t * .pi)                             // 0 horizon … 1 noon
            intensity = 0.55 + 0.45 * e
        }
        let len: CGFloat = 2 + (1 - e) * 4               // longer shadow when low
        return (hx, e, intensity, CGSize(width: -hx * len, height: -len))
    }
}

/// One borderless, click-through badge window holding a session's per-prompt
/// sigil, parked top-right under the title bar of its terminal window. The
/// sigil image is still (shape + strata = which prompt); the badge spins it
/// continuously via a CoreAnimation layer rotation whose speed and direction
/// the controller sets from the session's STATUS — so motion is the status
/// channel. The layer spin is GPU-driven (render-server side, CPU stays idle),
/// so the only recurring cost is the controller's light reposition tick.
///
/// Compositing: the window sits at the *normal* level and is ordered directly
/// above its specific terminal window (by CGWindowID), so when another window
/// covers the terminal the sigil is correctly occluded too — it rides in the
/// stack with the window it tracks rather than floating above everything.
final class PromptSigilOverlay {
    let sessionId: String
    /// Bare tty name, e.g. `ttys003` — the join key the controller binds to a
    /// CGWindowID.
    let tty: String

    /// Key of the sprite sheet currently installed (seed : dark : sun-minute),
    /// so the controller re-renders the frames only when the rock or the sun
    /// actually changes — never on a status change (status is motion + halo).
    var frameKey: String = ""
    private var motion: (period: Double, clockwise: Bool)?
    /// Spring-follow state: `target` is where the terminal wants the badge,
    /// `current` is where it's drawn. `advance` eases current → target each
    /// frame so the badge trails its window with a little inertia (reads as
    /// playful physics rather than tracking lag).
    private var targetOrigin: NSPoint?
    private var currentOrigin: NSPoint?

    let size: CGFloat = 56
    /// Padding around the rock inside the window so the offset drop shadow has
    /// room and isn't clipped by the window edge.
    private let pad: CGFloat = 9
    /// Fixed global sun: where the highlight/shadow come from (screen-space,
    /// shared by every stone). Down-right shadow ⇒ light from the upper-left.
    private let shadowDrop = CGSize(width: 3, height: -3)
    private let window: NSWindow
    private let rockLayer = CALayer()        // plays the pre-rendered rotation frames
    private let shadowLayer = CALayer()      // solid status colour, masked to the rock silhouette
    private let shadowMask = CALayer()       // plays the same frames → the shadow's tumbling shape
    private var boxCenter = CGPoint.zero
    /// Two pre-rendered sprite sheets of one full turn: `rockFrames` is the
    /// chunky low-res copy the rock plays; `shadowFrames` is the crisp high-res
    /// silhouette the shadow plays.
    private var rockFrames: [CGImage] = []
    private var shadowFrames: [CGImage] = []

    init(sessionId: String, tty: String) {
        self.sessionId = sessionId
        self.tty = tty

        let box = size + 2 * pad
        let initial = NSRect(x: -2000, y: -2000, width: box, height: box)
        window = NSWindow(contentRect: initial, styleMask: [.borderless],
                          backing: .buffered, defer: false)
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false
        window.ignoresMouseEvents = true
        window.level = .normal
        window.collectionBehavior = [.fullScreenAuxiliary]

        // Container: a flat status-colour drop-shadow disc as a backing
        // sublayer, with the rock-frame layer on top — the disc peeks out on
        // the sun-opposite side as a hard drop shadow.
        let container = NSView(frame: NSRect(origin: .zero, size: initial.size))
        container.wantsLayer = true
        container.layer?.masksToBounds = false
        boxCenter = CGPoint(x: box / 2, y: box / 2)

        // Shadow: a solid status-colour block masked to the rock's silhouette
        // (the same tumbling frames), offset by the sun. So it's the rock's
        // actual shape turning — not a circle. Position offset applied in
        // setLighting.
        shadowLayer.frame = CGRect(x: pad, y: pad, width: size, height: size)
        shadowLayer.position = boxCenter
        shadowLayer.backgroundColor = NSColor.systemGray.cgColor
        shadowLayer.opacity = 0.9
        shadowMask.frame = CGRect(x: 0, y: 0, width: size, height: size)
        shadowMask.contentsGravity = .resizeAspect
        // Crisp silhouette: the shadow plays the high-res frames, smoothed.
        shadowMask.magnificationFilter = .linear
        shadowMask.minificationFilter = .linear
        shadowMask.contentsScale = 2
        shadowLayer.mask = shadowMask
        container.layer?.addSublayer(shadowLayer)

        rockLayer.frame = CGRect(x: pad, y: pad, width: size, height: size)
        rockLayer.contentsGravity = .resizeAspect
        // Low-res frames scaled up with nearest-neighbour → chunky low-poly
        // pixels, and cheap.
        rockLayer.magnificationFilter = .nearest
        rockLayer.minificationFilter = .nearest
        rockLayer.contentsScale = 1
        container.layer?.addSublayer(rockLayer)

        window.contentView = container
    }

    /// Install freshly rendered sprite sheets (chunky rock + crisp shadow) and
    /// (re)start playback.
    func setFrames(rock: [CGImage], shadow: [CGImage]) {
        rockFrames = rock
        shadowFrames = shadow
        rockLayer.contents = rock.first
        shadowMask.contents = shadow.first
        applyPlayback()
    }

    /// Tint the flat drop-shadow disc with the session's status colour.
    private var shadowColor: NSColor?
    func setShadowColor(_ color: NSColor) {
        guard shadowColor != color else { return }
        shadowColor = color
        shadowLayer.backgroundColor = color.cgColor
    }

    /// The global sun's direction is baked into the frames (re-rendered by the
    /// controller when it moves); here we only slide the flat shadow to the
    /// sun-opposite `drop`.
    private var appliedDrop: CGSize?
    func setLighting(drop: CGSize) {
        if appliedDrop != drop {
            appliedDrop = drop
            shadowLayer.position = CGPoint(x: boxCenter.x + drop.width,
                                           y: boxCenter.y + drop.height)
        }
    }

    /// Status-driven tumble. Cheap: a discrete keyframe animation cycling the
    /// cached frames — direction picks the frame order, period sets the turn
    /// time. Playback is server-side, so a slow tumble barely touches the CPU.
    func setMotion(period: Double, clockwise: Bool) {
        if let m = motion, m.period == period, m.clockwise == clockwise { return }
        motion = (period, clockwise)
        applyPlayback()
    }

    private func applyPlayback() {
        guard rockFrames.count > 1, let (period, cw) = motion else {
            rockLayer.removeAnimation(forKey: "tumble")
            shadowMask.removeAnimation(forKey: "tumble")
            return
        }
        func tumble(_ vals: [CGImage]) -> CAKeyframeAnimation {
            let a = CAKeyframeAnimation(keyPath: "contents")
            a.values = cw ? vals : vals.reversed()
            a.calculationMode = .discrete
            a.duration = period
            a.repeatCount = .infinity
            a.isRemovedOnCompletion = false
            return a
        }
        // Rock + shadow play their own sheets at the same timing → in sync.
        rockLayer.add(tumble(rockFrames), forKey: "tumble")
        shadowMask.add(tumble(shadowFrames), forKey: "tumble")
    }

    /// This badge's CGWindowID (valid once it's been ordered on-screen), so the
    /// controller can find it in the global stacking order.
    var windowNumber: Int { window.windowNumber }

    /// Aim the badge at the top-right of its terminal window (`b` = on-screen
    /// bounds {x,y,w,h}, top-left origin), dropped below the title bar. Sets the
    /// spring TARGET only; `advance` does the actual easing. First placement
    /// snaps (no spring from off-screen). No z-order touched here.
    func place(bounds b: (CGFloat, CGFloat, CGFloat, CGFloat), screenHeight: CGFloat) {
        let titleBar: CGFloat = 30, rightInset: CGFloat = 10
        // Window is padded around the rock; shift origin by -pad so the rock
        // itself (centred in the window) still lands at the top-right spot.
        let originX = b.0 + b.2 - rightInset - size - pad
        let originY = screenHeight - (b.1 + titleBar + size + pad)
        let t = NSPoint(x: originX, y: originY)
        targetOrigin = t
        if currentOrigin == nil {                 // first appearance: snap there
            currentOrigin = t
            window.setFrameOrigin(t)
        }
    }

    /// Ease the badge toward its target by a frame-rate-independent step.
    /// Returns true while still settling (so the controller keeps the
    /// animation loop warm), false once it's arrived. `tau` is the follow time
    /// constant — smaller is snappier, larger is floatier.
    @discardableResult
    func advance(dt: CGFloat) -> Bool {
        guard let target = targetOrigin else { return false }
        guard var cur = currentOrigin else {
            currentOrigin = target; window.setFrameOrigin(target); return false
        }
        let dx = target.x - cur.x, dy = target.y - cur.y
        if abs(dx) < 0.4 && abs(dy) < 0.4 {
            if cur != target { window.setFrameOrigin(target); currentOrigin = target }
            return false
        }
        // Big time constant = very floaty, delayed follow — the rock drifts
        // lazily after its window and coasts in.
        let tau: CGFloat = 0.42
        let alpha = 1 - exp(-dt / tau)
        cur.x += dx * alpha
        cur.y += dy * alpha
        currentOrigin = cur
        window.setFrameOrigin(cur)
        return true
    }

    /// Order the badge directly above its terminal. Called only when the badge
    /// has fallen behind its terminal (a focus-raise) or isn't on screen yet —
    /// never on a stable tick — so there's no restack churn to flicker.
    func raise(above windowNumber: Int) {
        window.order(.above, relativeTo: windowNumber)
    }

    func hide() {
        if window.isVisible { window.orderOut(nil) }
    }
    func close() { window.orderOut(nil) }
}

/// Owns the live badge set, the status→motion mapping, and a light reposition
/// tick. Energy-conscious: bounds + z-order come from CGWindowList in-process
/// (no fork); osascript runs only to (re)bind each tty to its CGWindowID, on
/// membership change / when a binding goes missing / at a slow safety cadence
/// — never per frame.
final class PromptSigilOverlayController {
    static let shared = PromptSigilOverlayController()
    private init() {}

    private var overlays: [String: PromptSigilOverlay] = [:]
    private var timer: Timer?
    /// tty (bare) → CGWindowID of its terminal window.
    private var binding: [String: Int] = [:]
    private var needsRebind = false
    private var bindInFlight = false
    private var lastBind = Date.distantPast
    /// Adaptive cadence: idle slow (low energy), but ramp to display rate while
    /// a tracked window is actually moving so badges snap tight to a drag, then
    /// settle back once it stops.
    private var motionDeadline = Date.distantPast
    private var lastBoundsByNum: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)] = [:]
    private let idleInterval: TimeInterval = 0.2
    private let activeInterval: TimeInterval = 1.0 / 60.0
    /// The cadence currently scheduled, so an AX/move event can promote the
    /// loop from idle to display-rate immediately without restarting a timer
    /// that's already fast (which would starve under a stream of events).
    private var currentInterval: TimeInterval = 0.2
    private var lastTick = Date.distantPast
    private var observerInstalled = false
    /// Live AX observers per terminal app pid (kept alive by this reference).
    private var axObservers: [pid_t: AXObserver] = [:]
    /// Global-sun state, recomputed each wall-clock minute so the whole wall's
    /// light tracks the time of day together.
    private var sunMinute = -1
    private var sun: (hx: CGFloat, elevation: CGFloat, intensity: CGFloat, drop: CGSize) =
        (0.4, 0.7, 0.9, CGSize(width: -2.4, height: -2.4))
    /// Serial queue for the offscreen 3D frame renders — one rock at a time so
    /// a per-minute sun change never spins up 7 Metal renderers at once.
    private let renderQueue = DispatchQueue(label: "computer.slab.sigil-frames", qos: .userInitiated)

    /// Non-capturing AX callback → route on main. Lexically inside the class so
    /// it may reach the private singleton; captures nothing local, so it
    /// converts cleanly to the C function pointer type. A window move/resize
    /// snaps that one badge from the moved window's live AX geometry; a focus
    /// change re-raises behind badges.
    private static let axCallback: AXObserverCallback = { _, element, notification, _ in
        let note = notification as String
        DispatchQueue.main.async {
            PromptSigilOverlayController.shared.handleAX(note: note, window: element)
        }
    }

    func handleAX(note: String, window: AXUIElement) {
        if note == kAXWindowMovedNotification as String
            || note == kAXWindowResizedNotification as String {
            snapFromAX(window)
        } else {
            reposition()   // focus / activation → re-raise behind badges
        }
    }

    /// Reposition the one badge bound to `window` directly from the window's
    /// LIVE AX position/size — fires per drag-frame, so the badge moves on the
    /// same frame as the window (no CGWindowList lag). z-order is left alone
    /// (the window stays frontmost through a drag), so no restack/flicker.
    private func snapFromAX(_ window: AXUIElement) {
        var wid = CGWindowID(0)
        guard _AXUIElementGetWindow(window, &wid) == .success, wid != 0 else { return }
        let num = Int(wid)
        guard let tty = binding.first(where: { $0.value == num })?.key,
              let ov = overlays.values.first(where: { $0.tty == tty }),
              let p = axValue(window, kAXPositionAttribute, .cgPoint) as? CGPoint,
              let s = axValue(window, kAXSizeAttribute, .cgSize) as? CGSize
        else { return }
        let screenH = NSScreen.main?.frame.height ?? 0
        ov.place(bounds: (p.x, p.y, s.width, s.height), screenHeight: screenH)
        lastBoundsByNum[num] = (p.x, p.y, s.width, s.height)
        promote()   // keep the spring loop warm so the badge eases to the new target
    }

    /// Promote the animation loop to display rate now (e.g. a drag started),
    /// without restarting an already-fast timer — so a stream of AX move events
    /// doesn't continually push the next fire out and starve the loop.
    private func promote() {
        motionDeadline = Date().addingTimeInterval(0.3)
        if currentInterval > activeInterval + 1e-6 {
            scheduleTick(after: activeInterval)
        }
    }

    /// Read an AXValue geometry attribute (point or size) off an element.
    private func axValue(_ el: AXUIElement, _ attr: String, _ type: AXValueType) -> Any? {
        var ref: CFTypeRef?
        guard AXUIElementCopyAttributeValue(el, attr as CFString, &ref) == .success,
              let v = ref, CFGetTypeID(v) == AXValueGetTypeID() else { return nil }
        if type == .cgPoint {
            var p = CGPoint.zero
            return AXValueGetValue(v as! AXValue, .cgPoint, &p) ? p : nil
        } else {
            var sz = CGSize.zero
            return AXValueGetValue(v as! AXValue, .cgSize, &sz) ? sz : nil
        }
    }

    /// Re-raise behind-the-terminal badges the instant another app activates
    /// (clicking from another app onto a terminal), rather than waiting for the
    /// next poll tick — so a cross-app focus has no visible sink.
    private func installActivationObserverIfNeeded() {
        guard !observerInstalled else { return }
        observerInstalled = true
        NSWorkspace.shared.notificationCenter.addObserver(
            forName: NSWorkspace.didActivateApplicationNotification,
            object: nil, queue: .main
        ) { [weak self] _ in self?.reposition() }
    }

    /// Attach an Accessibility focus observer to each running terminal app, so
    /// a SAME-app window switch (terminal → terminal, the common tiled case)
    /// fires kAXFocusedWindowChanged and re-raises the badge instantly instead
    /// of waiting up to a poll tick. Needs AX trust (the same the tiler uses);
    /// without it we silently fall back to the poll. Re-scanned each sync() so
    /// a terminal app launched later gets observed too.
    private func installAXFocusObservers() {
        guard AXIsProcessTrusted() else { return }
        for app in NSWorkspace.shared.runningApplications
            where Self.terminalBundleIds.contains(app.bundleIdentifier ?? "") {
            let pid = app.processIdentifier
            guard axObservers[pid] == nil else { continue }
            var observer: AXObserver?
            guard AXObserverCreate(pid, Self.axCallback, &observer) == .success,
                  let obs = observer else { continue }
            let appEl = AXUIElementCreateApplication(pid)
            AXObserverAddNotification(obs, appEl, kAXFocusedWindowChangedNotification as CFString, nil)
            AXObserverAddNotification(obs, appEl, kAXMainWindowChangedNotification as CFString, nil)
            AXObserverAddNotification(obs, appEl, kAXApplicationActivatedNotification as CFString, nil)
            // Window-level move/resize on the app element fire per drag-frame
            // with the moved window passed back — this is what makes the badge
            // snap to a drag instead of lagging the CGWindowList poll.
            AXObserverAddNotification(obs, appEl, kAXWindowMovedNotification as CFString, nil)
            AXObserverAddNotification(obs, appEl, kAXWindowResizedNotification as CFString, nil)
            CFRunLoopAddSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(obs), .defaultMode)
            axObservers[pid] = obs
        }
    }

    private static let terminalBundleIds = ["com.apple.Terminal", "com.googlecode.iterm2"]

    /// The session's status colour (the same per-status `cursor` accent the
    /// menubar polygon + themed terminals use), for the badge's halo.
    private func statusColor(for state: ClaudeSession.State) -> NSColor {
        let dark = AppDelegate.isDarkAppearance()
        let cur = AppDelegate.statusDecor(for: state, dark: dark).palette.cursor
            ?? (32768, 32768, 32768)
        return NSColor(deviceRed: CGFloat(cur.0) / 65535, green: CGFloat(cur.1) / 65535,
                       blue: CGFloat(cur.2) / 65535, alpha: 1.0)
    }

    private func motion(for state: ClaudeSession.State) -> (Double, Bool) {
        switch state {
        case .working:     return (10, true)
        case .rendering:   return (7,  true)
        case .awaiting:    return (13, false)
        case .interrupted: return (26, false)
        case .complete:    return (38, true)
        case .blank:       return (80, true)
        case .stale:       return (44, false)
        }
    }

    /// Reconcile the badge set with the live sessions. Off when `enabled` is
    /// false. Only sessions with a real local tty get a badge.
    func sync(sessions: [ClaudeSession], enabled: Bool) {
        guard enabled else { teardown(); return }
        installActivationObserverIfNeeded()
        installAXFocusObservers()

        let live = sessions.filter { !$0.tty.isEmpty && $0.remoteHost.isEmpty }
        let liveIds = Set(live.map { $0.sessionId })
        let dark = AppDelegate.isDarkAppearance()

        // Recompute the global sun every 5 minutes — the sun moves slowly, and
        // each change re-renders every rock's sprite sheet, so we don't want to
        // pay that every minute. The whole wall re-lights together.
        let now = Date()
        let bucket = Int(now.timeIntervalSince1970 / 300)
        if bucket != sunMinute {
            sunMinute = bucket
            sun = Sun.light(at: now)
        }

        var membershipChanged = false
        for (sid, ov) in overlays where !liveIds.contains(sid) {
            ov.close(); overlays.removeValue(forKey: sid); membershipChanged = true
        }
        for s in live {
            let bare = (s.tty as NSString).lastPathComponent
            let ov: PromptSigilOverlay
            if let existing = overlays[s.sessionId], existing.tty == bare {
                ov = existing
            } else {
                overlays[s.sessionId]?.close()
                ov = PromptSigilOverlay(sessionId: s.sessionId, tty: bare)
                overlays[s.sessionId] = ov
                membershipChanged = true
            }
            // Seed from session id + prompt: the session id guarantees every
            // window a distinct rock even when prompts collide (trivial "..",
            // "yes", blank); the prompt makes the rock re-form as the session
            // moves to a new prompt.
            let seed = SigilRenderer.seed(for: s.sessionId + "\u{1}" + s.subject)
            // Re-render the sprite sheet only when the rock or the sun moved.
            let key = "\(seed):\(dark):\(sunMinute)"
            if ov.frameKey != key {
                ov.frameKey = key
                let (hx, e, inten) = (sun.hx, sun.elevation, sun.intensity)
                renderQueue.async { [weak ov] in
                    let hi = SigilRockFrames.render(
                        seed: seed, dark: dark, sunHx: hx, sunElevation: e, sunIntensity: inten)
                    let lo = hi.map { SigilRockFrames.downsample($0, to: 30) }
                    DispatchQueue.main.async { ov?.setFrames(rock: lo, shadow: hi) }
                }
            }
            let (period, cw) = motion(for: s.state)
            ov.setMotion(period: period, clockwise: cw)
            ov.setShadowColor(statusColor(for: s.state))
            ov.setLighting(drop: sun.drop)
        }

        if membershipChanged { needsRebind = true }
        startTimerIfNeeded()
        reposition()
    }

    private func teardown() {
        timer?.invalidate(); timer = nil
        for (_, ov) in overlays { ov.close() }
        overlays.removeAll()
        binding.removeAll()
        for (_, obs) in axObservers {
            CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(obs), .defaultMode)
        }
        axObservers.removeAll()
    }

    private func startTimerIfNeeded() {
        guard timer == nil, !overlays.isEmpty else { return }
        scheduleTick(after: idleInterval)
    }

    /// Self-rescheduling tick: reposition, then re-arm at the display rate when
    /// a tracked window moved recently (snappy drag-following) or at the idle
    /// rate otherwise (low energy when nothing's moving).
    private func scheduleTick(after interval: TimeInterval) {
        currentInterval = interval
        timer?.invalidate()
        let t = Timer(timeInterval: max(0, interval), repeats: false) { [weak self] _ in
            self?.tick()
        }
        timer = t
        RunLoop.main.add(t, forMode: .common)
    }

    private func tick() {
        let now = Date()
        // Clamp dt so a long idle gap (or first tick) doesn't teleport the
        // spring — it should always ease, never jump.
        let dt = CGFloat(min(0.1, max(0.001, now.timeIntervalSince(lastTick))))
        lastTick = now

        reposition()                       // refresh targets + z-order
        guard !overlays.isEmpty else { timer = nil; return }
        var settling = false
        for (_, ov) in overlays where ov.advance(dt: dt) { settling = true }
        // Stay at display rate while a window moved recently OR a badge is still
        // catching up to its target; otherwise drop to the idle poll.
        let active = now < motionDeadline || settling
        scheduleTick(after: active ? activeInterval : idleInterval)
    }

    /// In-process snapshot of the on-screen window stack. Returns each
    /// Terminal/iTerm2 window's bounds {x,y,w,h} AND a front-to-back index for
    /// every normal window (ours included), so we can tell whether a badge is
    /// already sitting directly in front of its terminal. No fork.
    private func snapshotWindows()
        -> (bounds: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)], index: [Int: Int]) {
        let pids = Set(NSWorkspace.shared.runningApplications
            .filter { Self.terminalBundleIds.contains($0.bundleIdentifier ?? "") }
            .map { $0.processIdentifier })
        guard let infos = CGWindowListCopyWindowInfo([.optionOnScreenOnly], kCGNullWindowID) as? [[String: Any]]
        else { return ([:], [:]) }
        var bounds: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)] = [:]
        var index: [Int: Int] = [:]
        var i = 0
        for info in infos {
            guard let layer = info[kCGWindowLayer as String] as? Int, layer == 0,
                  let num = info[kCGWindowNumber as String] as? Int
            else { continue }
            index[num] = i; i += 1   // front-to-back rank among normal windows
            if let pid = info[kCGWindowOwnerPID as String] as? pid_t, pids.contains(pid),
               let b = info[kCGWindowBounds as String] as? [String: CGFloat],
               let x = b["X"], let y = b["Y"], let w = b["Width"], let h = b["Height"] {
                bounds[num] = (x, y, w, h)
            }
        }
        return (bounds, index)
    }

    /// Reposition every badge from the in-process window snapshot. Detects
    /// window motion (to drive the adaptive cadence) and triggers a throttled
    /// rebind when bindings go stale.
    private func reposition() {
        guard !overlays.isEmpty else { timer?.invalidate(); timer = nil; return }
        let snap = snapshotWindows()
        let screenH = NSScreen.main?.frame.height ?? 0
        var seen: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)] = [:]
        for (_, ov) in overlays {
            guard let num = binding[ov.tty], let b = snap.bounds[num] else { ov.hide(); continue }
            ov.place(bounds: b, screenHeight: screenH)
            // Raise ONLY when the badge has fallen behind its terminal (focus
            // raised the terminal above it) or isn't on screen yet. When it's
            // anywhere in front, leave the stack alone — no restack, no flicker.
            let behind: Bool = {
                guard let myIdx = snap.index[ov.windowNumber] else { return true }   // off-screen
                guard let termIdx = snap.index[num] else { return false }
                return myIdx > termIdx
            }()
            if behind { ov.raise(above: num) }
            seen[num] = b
            if let prev = lastBoundsByNum[num], prev != b {
                // A tracked window moved/resized — hold display rate for a short
                // tail so the whole drag tracks tightly.
                motionDeadline = Date().addingTimeInterval(0.3)
            }
        }
        lastBoundsByNum = seen
        // Rebind ONLY on a membership change (one-shot) or the slow safety
        // cadence — never per tick, so osascript stays capped at ~once / 5s.
        if (needsRebind || Date().timeIntervalSince(lastBind) > 5), !bindInFlight {
            rebind()
        }
    }

    /// Establish tty → CGWindowID. osascript gives tty → window bounds (the
    /// only API that knows a tab's tty); we then match those bounds to the
    /// in-process CGWindowList snapshot to recover each window's CGWindowID.
    /// Runs off-main (osascript blocks) and is the sole forking path — gated to
    /// at most one in flight and a slow cadence.
    private func rebind() {
        bindInFlight = true
        lastBind = Date()
        needsRebind = false
        let running = Set(NSWorkspace.shared.runningApplications.compactMap { $0.bundleIdentifier })
        let wantTerminal = running.contains("com.apple.Terminal")
        let wantIterm = running.contains("com.googlecode.iterm2")
        guard wantTerminal || wantIterm else { bindInFlight = false; return }
        let script = boundsScript(terminal: wantTerminal, iterm: wantIterm)

        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            let result = ShellRunner.run("/usr/bin/osascript", args: ["-e", script], timeout: 2)
            var ttyBounds: [String: (CGFloat, CGFloat, CGFloat, CGFloat)] = [:]
            for line in result.output.split(separator: "\n") {
                let parts = line.split(separator: "|")
                guard parts.count == 2 else { continue }
                let dev = (parts[0].trimmingCharacters(in: .whitespaces) as NSString).lastPathComponent
                let nums = parts[1].split(separator: ",").compactMap {
                    Double($0.trimmingCharacters(in: .whitespaces)).map { CGFloat($0) }
                }
                guard nums.count == 4 else { continue }
                // osascript bounds {l,t,r,b} → {x,y,w,h}.
                ttyBounds[dev] = (nums[0], nums[1], nums[2] - nums[0], nums[3] - nums[1])
            }
            DispatchQueue.main.async {
                guard let self = self else { return }
                // Match against a fresh snapshot (windows may have moved during
                // the osascript round-trip).
                let wins = self.snapshotWindows().bounds
                var newBinding: [String: Int] = [:]
                for (tty, tb) in ttyBounds {
                    if let hit = wins.first(where: { Self.boundsMatch($0.value, tb) }) {
                        newBinding[tty] = hit.key
                    }
                }
                self.binding = newBinding
                self.bindInFlight = false
                self.reposition()
            }
        }
    }

    /// Bounds match within a couple of points (osascript ints vs CGWindow
    /// floats; the occasional 1px rounding).
    private static func boundsMatch(
        _ a: (CGFloat, CGFloat, CGFloat, CGFloat), _ b: (CGFloat, CGFloat, CGFloat, CGFloat)
    ) -> Bool {
        abs(a.0 - b.0) <= 2 && abs(a.1 - b.1) <= 2 && abs(a.2 - b.2) <= 2 && abs(a.3 - b.3) <= 2
    }

    /// AppleScript that emits `<dev-tty>|l,t,r,b` per tab/session for the
    /// running terminals only (never launches a closed one).
    private func boundsScript(terminal: Bool, iterm: Bool) -> String {
        var s = "set out to \"\"\n"
        if terminal {
            s += """
            tell application "Terminal"
                repeat with w in windows
                    try
                        set b to bounds of w
                        set bs to ((item 1 of b) as text) & "," & ((item 2 of b) as text) & "," & ((item 3 of b) as text) & "," & ((item 4 of b) as text)
                        repeat with t in tabs of w
                            try
                                set out to out & (tty of t) & "|" & bs & linefeed
                            end try
                        end repeat
                    end try
                end repeat
            end tell

            """
        }
        if iterm {
            s += """
            tell application "iTerm2"
                repeat with w in windows
                    try
                        set p to position of w
                        set sz to size of w
                        set lx to item 1 of p
                        set ty to item 2 of p
                        set rx to lx + (item 1 of sz)
                        set by to ty + (item 2 of sz)
                        set bs to (lx as text) & "," & (ty as text) & "," & (rx as text) & "," & (by as text)
                        repeat with t in tabs of w
                            repeat with ss in sessions of t
                                try
                                    set out to out & (tty of ss) & "|" & bs & linefeed
                                end try
                            end repeat
                        end repeat
                    end try
                end repeat
            end tell

            """
        }
        s += "return out"
        return s
    }
}
