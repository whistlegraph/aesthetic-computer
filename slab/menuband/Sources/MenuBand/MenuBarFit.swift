// MenuBarFit.swift — shared menu-bar real-estate negotiation for the AC
// menu-bar apps (Menu Band, DateWizard, SlabMenubar, …).
//
// ─── Why ─────────────────────────────────────────────────────────────────
// macOS gives an app exactly ONE honest signal about menu-bar crowding: when
// there's no room for a status item, the item's window gets `screen == nil`
// (it's clipped/hidden). There is no API for the total free width. So the AC
// apps coordinate among themselves: each publishes its size ladder + priority
// to a shared registry, and a leader-elected broker decides *who* yields so the
// important things stay fully visible instead of every app greedily shrinking.
//
// ─── Protocol ────────────────────────────────────────────────────────────
// Registry dir: ~/.local/share/menubar-fit/
//   state/<slug>.json   — each participant's live state (atomic write)
//   cmd/<slug>.json      — the broker's target rung for that participant
//   broker.lock          — {pid, ts}; whoever holds a fresh lock is broker
// Wakeups: DistributedNotification "computer.aestheticcomputer.menubarfit"
//   posted on any state/command change; every participant also polls (1s) as a
//   backstop and to sample its own clip state.
//
// A "rung" is one size the app can render at, ordered smallest→largest. rung 0
// is the app's minimum footprint; rungs.count-1 is its ideal/full size. The
// broker moves apps up and down their own ladders; it never assigns a width the
// app didn't offer.
//
// ─── Negotiation ─────────────────────────────────────────────────────────
// On each broker evaluation:
//   • If ANY participant is clipped → command the LOWEST-priority participant
//     that can still shrink (rung>0) to drop one rung (tie-break: widest first).
//   • Else (slack) → command the HIGHEST-priority participant that can still
//     grow (rung<max) to rise one rung — unless it's in a post-revert cooldown.
//     If growing re-clips someone, the clip branch shrinks it again and a
//     cooldown is stamped so it won't immediately bounce back (anti-oscillation).
//
// This file is the source of truth at shared/menubar-fit/MenuBarFit.swift and is
// copied verbatim into each app's Sources. Keep the copies in sync.
import AppKit

final class MenuBarFit {
    struct Rung { let name: String; let width: CGFloat }

    // ── configuration ────────────────────────────────────────────────
    private let slug: String
    private let priority: Int          // higher = keep at a larger rung longer
    private var rungs: [Rung]          // index 0 = smallest … last = largest
    private weak var statusItem: NSStatusItem?
    // The app renders itself at the given rung (set image/title + status length).
    private let applyRung: (Rung, Int) -> Void

    // ── live state ───────────────────────────────────────────────────
    private(set) var current: Int
    private var maxRung: Int { rungs.count - 1 }
    private var lastClipped = false
    private var pollTimer: Timer?
    private var isSettling = false
    private var settleWorkItem: DispatchWorkItem?
    private var spaceChangeWorkItem: DispatchWorkItem?
    // Broker bookkeeping.
    private var isBroker = false
    private var lastGrowAt: Date = .distantPast
    private var lastShrinkAt: Date = .distantPast
    private var lastGrowth: (slug: String, fromRung: Int, at: Date)?
    private var cooldownUntil: [String: Date] = [:]   // slug → don't grow before

    // ── paths ────────────────────────────────────────────────────────
    static let root = NSString(string: "~/.local/share/menubar-fit").expandingTildeInPath
    private static var stateDir: String { root + "/state" }
    private static var cmdDir: String { root + "/cmd" }
    private static var lockPath: String { root + "/broker.lock" }
    static let channel = Notification.Name("computer.aestheticcomputer.menubarfit")

    // Freshness / cadence.
    private static let staleAfter: TimeInterval = 8      // ignore states older than this
    private static let lockTTL: TimeInterval = 5         // broker lock considered dead after this
    // Control Center updates its drawn status-item replicant asynchronously.
    // A rung must settle before it can be judged; otherwise a new width has no
    // matching replicant yet and looks clipped for the first few milliseconds.
    private static let settleInterval: TimeInterval = 0.35
    // Successful probes can advance as soon as the previous rung settles. This
    // walks Menu Band's fine-grained key ladder in a few seconds, while failed
    // probes still receive the long revert cooldown below.
    private static let growInterval: TimeInterval = 0.25
    // Notification feedback can arrive before the target app marks itself as
    // settling. Keep this shorter than settleInterval so each fresh sample can
    // trigger the next shrink without waiting for the 1s heartbeat.
    private static let shrinkInterval: TimeInterval = 0.25
    private static let growthFailureWindow: TimeInterval = 1.5
    private static let revertCooldown: TimeInterval = 20 // don't regrow a just-reverted app

    // ── init ─────────────────────────────────────────────────────────
    /// - rungs: smallest→largest. - startAt: initial rung (defaults to largest).
    /// - applyRung: render at (rung, index); MUST set the status item's width.
    init(slug: String, priority: Int, rungs: [Rung], statusItem: NSStatusItem,
         startAt: Int? = nil, applyRung: @escaping (Rung, Int) -> Void) {
        precondition(!rungs.isEmpty, "MenuBarFit needs at least one rung")
        self.slug = slug
        self.priority = priority
        self.rungs = rungs
        self.statusItem = statusItem
        self.applyRung = applyRung
        self.current = min(max(0, startAt ?? rungs.count - 1), rungs.count - 1)
    }

    /// Swap the size ladder at runtime — e.g. DateWizard collapses to a single
    /// bare-wand rung when it has no upcoming event, then reopens the badge
    /// rungs when one appears. Clamps the current rung, re-renders, and keeps
    /// the broker's width model honest. New rungs must be smallest→largest.
    func updateRungs(_ newRungs: [Rung]) {
        guard !newRungs.isEmpty else { return }
        let sameShape = newRungs.count == rungs.count &&
            zip(newRungs, rungs).allSatisfy { $0.name == $1.name && $0.width == $1.width }
        if sameShape { return }
        rungs = newRungs
        current = min(current, maxRung)
        applyRung(rungs[current], current)
        beginRungSettlement()
        writeState()
        postBus()
    }

    func start() {
        ensureDirs()
        clearStaleSelf()
        applyRung(rungs[current], current)   // render at initial rung
        beginRungSettlement()
        writeState()
        DistributedNotificationCenter.default().addObserver(
            self, selector: #selector(onBusNote), name: Self.channel, object: nil)
        let workspaceNotes = NSWorkspace.shared.notificationCenter
        for name in [NSWorkspace.didActivateApplicationNotification,
                     NSWorkspace.didLaunchApplicationNotification,
                     NSWorkspace.didTerminateApplicationNotification] {
            workspaceNotes.addObserver(
                self,
                selector: #selector(onAvailableSpaceMayHaveChanged),
                name: name,
                object: nil
            )
        }
        NotificationCenter.default.addObserver(
            self,
            selector: #selector(onAvailableSpaceMayHaveChanged),
            name: NSApplication.didChangeScreenParametersNotification,
            object: nil
        )
        // 1s heartbeat: sample own clip state, keep the broker lock alive, and
        // (if broker) evaluate. A backstop even if a notification is missed.
        let t = Timer(timeInterval: 1.0, repeats: true) { [weak self] _ in self?.tick() }
        RunLoop.main.add(t, forMode: .common)
        pollTimer = t
        tick()
    }

    deinit {
        pollTimer?.invalidate()
        settleWorkItem?.cancel()
        spaceChangeWorkItem?.cancel()
        DistributedNotificationCenter.default().removeObserver(self)
        NSWorkspace.shared.notificationCenter.removeObserver(self)
        NotificationCenter.default.removeObserver(self)
        try? FileManager.default.removeItem(atPath: Self.stateDir + "/\(slug).json")
    }

    // ── heartbeat ────────────────────────────────────────────────────
    private func tick() {
        // 1) Sample own clip state. Rewrite state EVERY tick so `ts` stays
        //    fresh — an app that settles at a stable rung must keep heartbeating
        //    or the broker evicts it as stale and loses track of it. Only wake
        //    the bus (notify peers) when the clip state actually flips.
        let clipChanged = isSettling ? false : sampleVisibility()
        writeState()
        if clipChanged { postBus() }
        // 2) Apply any pending command targeted at us.
        applyCommandIfAny()
        // 3) Broker leadership + duties.
        electBroker()
        if isBroker { evaluate() }
    }

    @objc private func onBusNote() {
        applyCommandIfAny()
        electBroker()
        if isBroker { evaluate() }
    }

    /// App switches change the width of the left-side application menus;
    /// launches, quits, and display changes can alter the right-side status
    /// cluster. Those are real new space conditions, so do not wait out a
    /// failed probe's 20-second cooldown before trying the next rung again.
    @objc private func onAvailableSpaceMayHaveChanged() {
        cooldownUntil.removeAll()
        spaceChangeWorkItem?.cancel()
        let work = DispatchWorkItem { [weak self] in
            guard let self else { return }
            self.spaceChangeWorkItem = nil
            self.tick()
            self.postBus()
        }
        spaceChangeWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15, execute: work)
    }

    /// Hold negotiation while AppKit and Control Center replace the status-item
    /// window for a new width. The old implementation sampled synchronously in
    /// `applyCommandIfAny`; on Tahoe that made every growth look clipped and
    /// bounce back 10–20ms later.
    private func beginRungSettlement() {
        settleWorkItem?.cancel()
        isSettling = true
        let expectedRung = current
        let work = DispatchWorkItem { [weak self] in
            guard let self, self.current == expectedRung else { return }
            self.settleWorkItem = nil
            self.isSettling = false
            _ = self.sampleVisibility()
            self.writeState()
            // Always wake the broker: it deliberately paused while this state
            // was settling, even when the final clipped bit did not change.
            self.postBus()
            self.electBroker()
            if self.isBroker { self.evaluate() }
        }
        settleWorkItem = work
        DispatchQueue.main.asyncAfter(
            deadline: .now() + Self.settleInterval,
            execute: work
        )
    }

    @discardableResult
    private func sampleVisibility() -> Bool {
        let clipped = !isVisible()
        let changed = clipped != lastClipped
        lastClipped = clipped
        if changed {
            NSLog("MenuBarFit[%@] clipped=%d at rung %d (%@)",
                  slug, clipped ? 1 : 0, current, rungs[current].name)
        }
        return changed
    }

    // Test hook: AC_MENUBARFIT_FAKECLIP=slug1,slug2 forces those slugs to report
    // clipped, so the negotiation can be exercised without a genuinely crowded
    // bar. Read once at launch; unset in normal use → no effect.
    private static let fakeClipped: Set<String> = Set(
        (ProcessInfo.processInfo.environment["AC_MENUBARFIT_FAKECLIP"] ?? "")
            .split(separator: ",").map { $0.trimmingCharacters(in: .whitespaces) })

    // ── clip detection (the one honest macOS signal) ──────────────────
    private func isVisible() -> Bool {
        if Self.fakeClipped.contains(slug) { return false }
        guard let button = statusItem?.button, let window = button.window else { return false }
        if window.screen == nil { return false }
        if !NSScreen.screens.contains(where: { $0.frame.intersects(window.frame) }) { return false }
        return Self.replicantVisible(width: window.frame.width, preferredName: slug)
    }

    // macOS 26 (Tahoe) re-architected status items: the thing actually drawn
    // in the bar is a Control Center-owned "replicant" window at the status
    // layer, while the app-side NSWindow is a proxy that keeps a plausible
    // frame + non-nil screen even after the bar clips the item (its
    // windowNumber is the sentinel 0x100000000 — no real CG backing). The
    // screen/frame checks above therefore can't see hiding on Tahoe; the
    // honest bit is the replicant's kCGWindowIsOnscreen. Match ours by width:
    // the replicant is exactly the proxy window's width (item length + 16pt
    // of bar padding), so a ±1.5pt match is unambiguous in practice.
    // Pre-Tahoe systems have no CC-owned status-layer windows at all → no
    // replicants seen → return true and let the screen/frame checks above
    // stay authoritative, which keeps old-OS behavior unchanged.
    static func replicantVisible(width: CGFloat, preferredName: String? = nil) -> Bool {
        guard width > 0 else { return true }
        guard let cc = NSRunningApplication.runningApplications(
            withBundleIdentifier: "com.apple.controlcenter").first else { return true }
        guard let list = CGWindowListCopyWindowInfo([.optionAll], kCGNullWindowID)
            as? [[String: Any]] else { return true }
        let statusLayer = 25
        var sawStatusLayer = false
        var sawPreferredName = false
        var namedMatchOnScreen = false
        var namedWidthMatched = false
        var widthMatchOnScreen = false
        var widthMatched = false
        for info in list {
            guard let pid = (info[kCGWindowOwnerPID as String] as? NSNumber)?.int32Value,
                  pid == cc.processIdentifier,
                  let layer = (info[kCGWindowLayer as String] as? NSNumber)?.intValue,
                  layer == statusLayer,
                  let bounds = info[kCGWindowBounds as String] as? [String: Any],
                  let w = (bounds["Width"] as? NSNumber)?.doubleValue else { continue }
            sawStatusLayer = true
            let on = (info[kCGWindowIsOnscreen as String] as? NSNumber)?.boolValue ?? false
            if let preferredName,
               info[kCGWindowName as String] as? String == preferredName {
                sawPreferredName = true
                if abs(CGFloat(w) - width) <= 1.5 {
                    namedWidthMatched = true
                    if on { namedMatchOnScreen = true }
                }
            }
            if abs(CGFloat(w) - width) <= 1.5 {
                widthMatched = true
                if on { widthMatchOnScreen = true }
            }
        }
        if !sawStatusLayer { return true }   // pre-Tahoe: no replicant architecture
        // Tahoe names Menu Band's replicant "menuband". Prefer that identity
        // over width so an unrelated same-sized item cannot mask clipping.
        if sawPreferredName { return namedWidthMatched && namedMatchOnScreen }
        if !widthMatched { return false }    // replicants exist, ours isn't among them
        return widthMatchOnScreen
    }

    // ── participant state I/O ─────────────────────────────────────────
    private struct State: Codable {
        var slug: String
        var pid: Int32
        var priority: Int
        var clipped: Bool
        var rung: Int
        var rungWidths: [Double]
        var rungNames: [String]
        var settling: Bool?
        var ts: Double
    }

    private func writeState() {
        let s = State(slug: slug, pid: ProcessInfo.processInfo.processIdentifier,
                      priority: priority, clipped: lastClipped, rung: current,
                      rungWidths: rungs.map { Double($0.width) },
                      rungNames: rungs.map { $0.name },
                      settling: isSettling ? true : nil,
                      ts: Date().timeIntervalSince1970)
        atomicWrite(Self.stateDir + "/\(slug).json", s)
    }

    private func readStates() -> [State] {
        let fm = FileManager.default
        guard let names = try? fm.contentsOfDirectory(atPath: Self.stateDir) else { return [] }
        let now = Date().timeIntervalSince1970
        var out: [State] = []
        for name in names where name.hasSuffix(".json") {
            guard let data = fm.contents(atPath: Self.stateDir + "/" + name),
                  let s = try? JSONDecoder().decode(State.self, from: data) else { continue }
            if now - s.ts > Self.staleAfter { continue }          // stale
            if !pidAlive(s.pid) { continue }                       // dead process
            out.append(s)
        }
        return out
    }

    // ── command channel ──────────────────────────────────────────────
    private struct Command: Codable { var rung: Int; var ts: Double }

    private func command(_ targetSlug: String, rung: Int) {
        atomicWrite(Self.cmdDir + "/\(targetSlug).json",
                    Command(rung: rung, ts: Date().timeIntervalSince1970))
        postBus()
    }

    private func applyCommandIfAny() {
        let path = Self.cmdDir + "/\(slug).json"
        guard let data = FileManager.default.contents(atPath: path),
              let cmd = try? JSONDecoder().decode(Command.self, from: data) else { return }
        let target = min(max(0, cmd.rung), maxRung)
        guard target != current else { return }
        NSLog("MenuBarFit[%@] rung %d → %d (%@)", slug, current, target, rungs[target].name)
        current = target
        applyRung(rungs[current], current)
        beginRungSettlement()
        writeState()
        postBus()
    }

    // ── broker: leader election ───────────────────────────────────────
    private struct Lock: Codable { var pid: Int32; var ts: Double }

    private func electBroker() {
        let fm = FileManager.default
        let now = Date().timeIntervalSince1970
        let myPid = ProcessInfo.processInfo.processIdentifier
        var holder: Lock? = nil
        if let data = fm.contents(atPath: Self.lockPath) {
            holder = try? JSONDecoder().decode(Lock.self, from: data)
        }
        let fresh = holder.map { now - $0.ts < Self.lockTTL && pidAlive($0.pid) } ?? false
        if let h = holder, fresh, h.pid != myPid {
            isBroker = false
            return
        }
        // Vacant or ours or stale → (re)claim, then read back: with atomic
        // renames the last writer wins, so if two apps claim at once only the
        // one whose pid survives the readback acts as broker this tick.
        atomicWrite(Self.lockPath, Lock(pid: myPid, ts: now))
        if let data = fm.contents(atPath: Self.lockPath),
           let back = try? JSONDecoder().decode(Lock.self, from: data) {
            isBroker = (back.pid == myPid)
        } else {
            isBroker = false
        }
    }

    // ── broker: the negotiation ───────────────────────────────────────
    private func evaluate() {
        let states = readStates()
        guard !states.isEmpty else { return }
        let now = Date()

        // One layout mutation at a time. A participant publishes `settling`
        // while Control Center swaps its replicant, then wakes us with an
        // authoritative visibility sample.
        if states.contains(where: { $0.settling == true }) { return }

        // Reconcile any state whose command we already issued but that hasn't
        // taken effect yet — treat the *reported* rung as truth.
        let anyClipped = states.contains { $0.clipped }

        if anyClipped {
            // A failed growth probe returns directly to its last known fitting
            // rung. Remembering the source rung also makes larger future probe
            // jumps safe if a participant adopts them.
            if let growth = lastGrowth,
               now.timeIntervalSince(growth.at) < Self.growthFailureWindow,
               let grown = states.first(where: { $0.slug == growth.slug }),
               grown.rung > growth.fromRung {
                cooldownUntil[growth.slug] = now.addingTimeInterval(Self.revertCooldown)
                lastGrowth = nil
                lastShrinkAt = now
                command(growth.slug, rung: growth.fromRung)
                return
            }
            guard now.timeIntervalSince(lastShrinkAt) >= Self.shrinkInterval else { return }
            // Shrink the lowest-priority app that still can. Tie-break: widest
            // current rung first (frees the most pixels).
            let shrinkable = states.filter { $0.rung > 0 }
            guard let victim = shrinkable.min(by: { a, b in
                if a.priority != b.priority { return a.priority < b.priority }
                return widthOf(a) > widthOf(b)   // .min → widest wins the "smallest" slot
            }) else { return }
            // If we grew someone moments ago and now something is clipped, that
            // grow over-committed — stamp a cooldown on the widest grower so it
            // doesn't immediately bounce back up.
            if now.timeIntervalSince(lastGrowAt) < Self.growthFailureWindow {
                if let grower = states.max(by: { widthOf($0) < widthOf($1) }) {
                    cooldownUntil[grower.slug] = now.addingTimeInterval(Self.revertCooldown)
                }
            }
            lastGrowth = nil
            lastShrinkAt = now
            command(victim.slug, rung: victim.rung - 1)
            return
        }

        // Slack: grow the highest-priority app that can, respecting cooldowns and
        // a min gap so we probe gently rather than thrash.
        guard now.timeIntervalSince(lastGrowAt) >= Self.growInterval else { return }
        let growable = states.filter {
            $0.rung < ($0.rungWidths.count - 1) && (cooldownUntil[$0.slug].map { now >= $0 } ?? true)
        }
        guard let grower = growable.max(by: { a, b in
            if a.priority != b.priority { return a.priority < b.priority }
            return widthOf(a) > widthOf(b)   // .max → higher priority, then narrower
        }) else { return }
        lastGrowAt = now
        lastGrowth = (grower.slug, grower.rung, now)
        command(grower.slug, rung: grower.rung + 1)
    }

    private func widthOf(_ s: State) -> Double {
        guard s.rung >= 0 && s.rung < s.rungWidths.count else { return 0 }
        return s.rungWidths[s.rung]
    }

    // ── fs helpers ────────────────────────────────────────────────────
    private func ensureDirs() {
        let fm = FileManager.default
        for d in [Self.root, Self.stateDir, Self.cmdDir] {
            try? fm.createDirectory(atPath: d, withIntermediateDirectories: true)
        }
    }

    // Clear a leftover state/cmd file from a previous run of this same app so a
    // crashed predecessor's rung doesn't linger.
    private func clearStaleSelf() {
        try? FileManager.default.removeItem(atPath: Self.cmdDir + "/\(slug).json")
    }

    // .atomic writes to a sibling temp file and renames into place, so a reader
    // never sees a half-written JSON even with the broker writing concurrently.
    private func atomicWrite<T: Encodable>(_ path: String, _ value: T) {
        guard let data = try? JSONEncoder().encode(value) else { return }
        try? data.write(to: URL(fileURLWithPath: path), options: .atomic)
    }

    private func postBus() {
        DistributedNotificationCenter.default().postNotificationName(
            Self.channel, object: nil, userInfo: nil, deliverImmediately: true)
    }

    private func pidAlive(_ pid: Int32) -> Bool {
        if pid <= 0 { return false }
        // kill(pid, 0): 0 = alive, ESRCH = gone. EPERM still means it exists.
        return kill(pid, 0) == 0 || errno == EPERM
    }
}
