import AppKit
import ApplicationServices
import Darwin
import Foundation
import Network
import QuartzCore

private struct CaptureCommand: Codable {
    let version: Int
    let command: String
    let enabled: Bool
}

private struct WireContact: Codable {
    let identifier: Int32
    let x: Double
    let y: Double
    let state: Int32
}

private struct ContactFrame: Codable {
    let version: Int
    let timestamp: Double
    let contacts: [WireContact]
    let event: String?
}

/// Owns the system-pointer boundary while Menu Band is using the trackpad as
/// an instrument. The event tap is the preferred path; clear non-activating
/// panels remain as a permission-free click sink when Accessibility is absent.
private final class TrackpadInteractionShield {
    private var tap: CFMachPort?
    private var source: CFRunLoopSource?
    private var tapThread: Thread?
    private var tapRunLoop: CFRunLoop?
    private var panels: [NSPanel] = []
    private var cursorHidden = false
    private var exitTimer: Timer?
    private var escapeWasDown = false
    private var commandWasDown = false
    private var commandExitArmed = false
    private var commandExitArmAfter: CFTimeInterval = 0
    private var lastCommandTap: CFTimeInterval?

    private static let commandKeyCodes: Set<CGKeyCode> = [54, 55]

    /// The permission-free watchdog only receives snapshots, not key events.
    /// While the first Command tap is pending, sample the keyboard so any
    /// intervening key turns `Command, key, Command` into a cancelled run.
    private static func hasNonCommandKeyDown() -> Bool {
        let flags = CGEventSource.flagsState(.combinedSessionState)
        let otherModifiers: CGEventFlags = [
            .maskAlphaShift, .maskShift, .maskControl,
            .maskAlternate, .maskSecondaryFn,
        ]
        if !flags.intersection(otherModifiers).isEmpty { return true }

        for raw in 0..<128 {
            let key = CGKeyCode(raw)
            guard !commandKeyCodes.contains(key) else { continue }
            if CGEventSource.keyState(.combinedSessionState, key: key) {
                return true
            }
        }
        return false
    }

    var onExitRequested: (() -> Void)?
    var onCommandDoubleTap: (() -> Void)?

    func start() {
        precondition(Thread.isMainThread)
        guard !cursorHidden else { return }

        Self.setBackgroundCursorHiding(true)
        CGDisplayHideCursor(CGMainDisplayID())
        cursorHidden = true

        if !AXIsProcessTrusted() || !startEventTap() {
            startFallbackPanels()
            NSLog("TrackDrum for Menu Band: click shield using panel fallback")
        } else {
            NSLog("TrackDrum for Menu Band: click/context shield active")
        }
    }

    func stop() {
        precondition(Thread.isMainThread)
        lastCommandTap = nil
        commandExitArmed = false
        stopEventTap()
        panels.forEach { $0.orderOut(nil) }
        panels.removeAll()
        if cursorHidden {
            CGDisplayShowCursor(CGMainDisplayID())
            Self.setBackgroundCursorHiding(false)
            cursorHidden = false
        }
    }

    /// Permission-free Command-Command watcher. Reading combined-session key
    /// state does not require a key tap, so the companion can summon MenuBand
    /// even while the App Store process is not running. While capture is
    /// active, the same gesture remains the emergency exit path.
    private func startCommandWatchdog() {
        exitTimer?.invalidate()
        escapeWasDown = CGEventSource.keyState(
            .combinedSessionState, key: 53
        )
        commandWasDown = CGEventSource.flagsState(
            .combinedSessionState
        ).contains(.maskCommand)
        commandExitArmed = false
        commandExitArmAfter = CACurrentMediaTime() + 0.35
        let timer = Timer(timeInterval: 1.0 / 60.0, repeats: true) {
            [weak self] _ in
            guard let self else { return }
            let escapeDown = CGEventSource.keyState(
                .combinedSessionState, key: 53
            )
            let commandDown = CGEventSource.flagsState(
                .combinedSessionState
            ).contains(.maskCommand)
            let escapePressed = escapeDown && !self.escapeWasDown
            var commandDoubleTapped = false
            let now = CACurrentMediaTime()
            if self.lastCommandTap != nil, Self.hasNonCommandKeyDown() {
                self.lastCommandTap = nil
            }
            if !self.commandExitArmed {
                if !commandDown && now >= self.commandExitArmAfter {
                    self.commandExitArmed = true
                    self.lastCommandTap = nil
                }
            } else if commandDown && !self.commandWasDown {
                if Self.hasNonCommandKeyDown() {
                    self.lastCommandTap = nil
                } else if let prior = self.lastCommandTap, now - prior <= 0.75 {
                    commandDoubleTapped = true
                    self.lastCommandTap = nil
                } else {
                    self.lastCommandTap = now
                }
            }
            self.escapeWasDown = escapeDown
            self.commandWasDown = commandDown
            if escapePressed || commandDoubleTapped {
                if escapePressed {
                    self.onExitRequested?()
                } else {
                    self.onCommandDoubleTap?()
                }
            }
        }
        RunLoop.main.add(timer, forMode: .common)
        exitTimer = timer
    }

    private func startEventTap() -> Bool {
        if let tap { return CGEvent.tapIsEnabled(tap: tap) }
        // Mouse buttons and drags plus AppKit's complete gesture family.
        // This includes the right-click emitted by a two-finger tap.
        let types: [UInt32] = [
            1, 2, 3, 4, 6, 7, 18, 19, 20, 22, 25, 26, 27,
            29, 30, 31, 32, 33, 34,
        ]
        var mask: CGEventMask = 0
        for type in types { mask |= CGEventMask(1) << CGEventMask(type) }

        let retainedSelf = Unmanaged.passRetained(self).toOpaque()
        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon else { return Unmanaged.passUnretained(event) }
            let shield = Unmanaged<TrackpadInteractionShield>
                .fromOpaque(refcon).takeUnretainedValue()
            if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
                if let tap = shield.tap {
                    CGEvent.tapEnable(tap: tap, enable: true)
                }
                return Unmanaged.passUnretained(event)
            }
            return nil
        }
        guard let tap = CGEvent.tapCreate(
            tap: .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .defaultTap,
            eventsOfInterest: mask,
            callback: callback,
            userInfo: retainedSelf
        ) else {
            Unmanaged<TrackpadInteractionShield>
                .fromOpaque(retainedSelf).release()
            return false
        }
        self.tap = tap
        source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, tap, 0)
        let ready = DispatchSemaphore(value: 0)
        let thread = Thread { [weak self] in
            guard let self, let source = self.source, let tap = self.tap else {
                ready.signal()
                return
            }
            let runLoop = CFRunLoopGetCurrent()
            self.tapRunLoop = runLoop
            CFRunLoopAddSource(runLoop, source, .commonModes)
            CGEvent.tapEnable(tap: tap, enable: true)
            CFRunLoopPerformBlock(runLoop, CFRunLoopMode.commonModes.rawValue) {
                ready.signal()
            }
            CFRunLoopRun()
        }
        thread.qualityOfService = .userInteractive
        thread.name = "TrackDrum-InteractionShield"
        thread.start()
        tapThread = thread
        let started = ready.wait(timeout: .now() + 0.25) == .success
        guard started, CGEvent.tapIsEnabled(tap: tap) else {
            stopEventTap()
            return false
        }
        return true
    }

    private func stopEventTap() {
        if let tap { CGEvent.tapEnable(tap: tap, enable: false) }
        if let tapRunLoop { CFRunLoopStop(tapRunLoop) }
        if tap != nil {
            Unmanaged.passUnretained(self).release()
        }
        tap = nil
        source = nil
        tapThread = nil
        tapRunLoop = nil
    }

    func startGlobalCommandWatchdog() {
        precondition(Thread.isMainThread)
        guard exitTimer == nil else { return }
        startCommandWatchdog()
    }

    func shutdownGlobalCommandWatchdog() {
        precondition(Thread.isMainThread)
        exitTimer?.invalidate()
        exitTimer = nil
    }

    private func startFallbackPanels() {
        guard panels.isEmpty else { return }
        panels = NSScreen.screens.map { screen in
            let panel = ClickShieldPanel(
                contentRect: screen.frame,
                styleMask: [.borderless, .nonactivatingPanel],
                backing: .buffered,
                defer: false
            )
            panel.level = .screenSaver
            panel.backgroundColor = .clear
            panel.isOpaque = false
            panel.hasShadow = false
            panel.animationBehavior = .none
            panel.isMovable = false
            panel.isReleasedWhenClosed = false
            panel.hidesOnDeactivate = false
            panel.ignoresMouseEvents = false
            panel.sharingType = .none
            panel.collectionBehavior = [
                .canJoinAllSpaces, .fullScreenAuxiliary, .stationary,
                .ignoresCycle,
            ]
            panel.contentView = NSView(
                frame: NSRect(origin: .zero, size: screen.frame.size)
            )
            panel.orderFrontRegardless()
            return panel
        }
    }

    private static func setBackgroundCursorHiding(_ enabled: Bool) {
        typealias MainConnection = @convention(c) () -> Int32
        typealias SetProperty = @convention(c) (
            Int32, Int32, CFString, CFTypeRef
        ) -> Int32
        let symbols = UnsafeMutableRawPointer(bitPattern: -2)
        guard let connectionSymbol = dlsym(symbols, "CGSMainConnectionID"),
              let propertySymbol = dlsym(symbols, "CGSSetConnectionProperty")
        else { return }
        let connection = unsafeBitCast(connectionSymbol, to: MainConnection.self)()
        let setProperty = unsafeBitCast(propertySymbol, to: SetProperty.self)
        _ = setProperty(
            connection, connection, "SetsCursorInBackground" as CFString,
            enabled ? kCFBooleanTrue : kCFBooleanFalse
        )
    }

    deinit {
        if Thread.isMainThread { stop() }
    }
}

private final class ClickShieldPanel: NSPanel {
    override var canBecomeKey: Bool { false }
    override var canBecomeMain: Bool { false }
}

private final class TrackpadBridgeServer {
    static let port: NWEndpoint.Port = 51_983

    private let queue = DispatchQueue(
        label: "computer.aesthetic.menuband-trackpad.server",
        qos: .userInteractive
    )
    private var listener: NWListener?
    private var client: NWConnection?
    private var receiveBuffer = Data()
    private var captureEnabled = false
    private var pendingSummon = false
    private let interactionShield = TrackpadInteractionShield()

    func start() throws {
        interactionShield.onExitRequested = { [weak self] in
            self?.queue.async { [weak self] in
                guard let self, self.captureEnabled else { return }
                self.setCaptureEnabled(false)
                self.sendExit()
            }
        }
        interactionShield.onCommandDoubleTap = { [weak self] in
            self?.queue.async { [weak self] in
                guard let self else { return }
                if self.captureEnabled {
                    self.setCaptureEnabled(false)
                    self.sendExit()
                } else {
                    self.requestSummon()
                }
            }
        }
        performOnMain { self.interactionShield.startGlobalCommandWatchdog() }
        let parameters = NWParameters.tcp
        parameters.requiredLocalEndpoint = .hostPort(
            host: NWEndpoint.Host("127.0.0.1"),
            port: Self.port
        )
        let listener = try NWListener(using: parameters)
        listener.newConnectionHandler = { [weak self] connection in
            self?.accept(connection)
        }
        listener.stateUpdateHandler = { state in
            switch state {
            case .ready:
                NSLog("TrackDrum for Menu Band: ready on 127.0.0.1:%u",
                      Self.port.rawValue)
            case .failed(let error):
                NSLog("TrackDrum for Menu Band: listener failed: %@",
                      String(describing: error))
            default:
                break
            }
        }
        self.listener = listener
        listener.start(queue: queue)

        MultitouchTrackpad.shared.onFrame = { [weak self] contacts, timestamp, _ in
            self?.send(contacts: contacts, timestamp: timestamp)
        }
    }

    func stop() {
        setCaptureEnabled(false)
        performOnMain { self.interactionShield.shutdownGlobalCommandWatchdog() }
        client?.cancel()
        client = nil
        listener?.cancel()
        listener = nil
    }

    private func accept(_ connection: NWConnection) {
        client?.cancel()
        setCaptureEnabled(false)
        client = connection
        receiveBuffer.removeAll(keepingCapacity: true)
        connection.stateUpdateHandler = { [weak self, weak connection] state in
            guard let self, let connection, self.client === connection else { return }
            switch state {
            case .ready:
                self.receive(on: connection)
                self.sendPendingSummonIfReady()
            case .failed, .cancelled:
                self.client = nil
                self.setCaptureEnabled(false)
            default:
                break
            }
        }
        connection.start(queue: queue)
    }

    private func receive(on connection: NWConnection) {
        connection.receive(
            minimumIncompleteLength: 1,
            maximumLength: 4 * 1024
        ) { [weak self, weak connection] data, _, isComplete, error in
            guard let self, let connection, self.client === connection else { return }
            if let data, !data.isEmpty { self.consume(data) }
            if isComplete || error != nil {
                connection.cancel()
                return
            }
            self.receive(on: connection)
        }
    }

    private func consume(_ data: Data) {
        receiveBuffer.append(data)
        guard receiveBuffer.count <= 8 * 1024 else {
            client?.cancel()
            return
        }
        while let newline = receiveBuffer.firstIndex(of: 0x0A) {
            let line = receiveBuffer[..<newline]
            receiveBuffer.removeSubrange(...newline)
            guard line.count <= 1024,
                  let command = try? JSONDecoder().decode(
                    CaptureCommand.self, from: Data(line)
                  ),
                  command.version == 1,
                  command.command == "capture" else { continue }
            setCaptureEnabled(command.enabled)
        }
    }

    private func setCaptureEnabled(_ enabled: Bool) {
        guard enabled != captureEnabled else { return }
        captureEnabled = enabled
        if enabled {
            if !MultitouchTrackpad.shared.start() {
                captureEnabled = false
                NSLog("TrackDrum for Menu Band: no compatible trackpad")
            } else {
                performOnMain { self.interactionShield.start() }
            }
        } else {
            send(contacts: [], timestamp: CACurrentMediaTime())
            MultitouchTrackpad.shared.stop()
            performOnMain { self.interactionShield.stop() }
        }
        NSLog("TrackDrum for Menu Band: capture %@", captureEnabled ? "on" : "off")
    }

    private func requestSummon() {
        queue.async { [weak self] in
            guard let self else { return }
            self.pendingSummon = true
            if self.client != nil {
                self.sendPendingSummonIfReady()
            } else {
                self.launchMenuBandIfNeeded()
            }
        }
    }

    private func sendPendingSummonIfReady() {
        guard pendingSummon, let client,
              case .ready = client.state else { return }
        pendingSummon = false
        send(ContactFrame(
            version: 1,
            timestamp: CACurrentMediaTime(),
            contacts: [],
            event: "summon"
        ))
    }

    private func launchMenuBandIfNeeded() {
        DispatchQueue.main.async {
            let bundleID = "computer.aesthetic.menuband"
            if let running = NSRunningApplication.runningApplications(
                withBundleIdentifier: bundleID
            ).first {
                running.activate(options: [.activateAllWindows])
                return
            }
            guard let url = NSWorkspace.shared.urlForApplication(
                withBundleIdentifier: bundleID
            ) else {
                NSLog("TrackDrum for Menu Band: MenuBand app is not installed")
                return
            }
            let configuration = NSWorkspace.OpenConfiguration()
            configuration.activates = true
            // The launch itself carries the focus intent, so the first
            // global ⌘⌘ is enough even if the localhost bridge is still
            // negotiating. The pending summon frame remains as a fallback
            // for builds that do not consume this launch argument.
            configuration.arguments = ["--focus-on-launch"]
            NSWorkspace.shared.openApplication(
                at: url, configuration: configuration
            ) { _, error in
                if let error {
                    NSLog("TrackDrum for Menu Band: MenuBand launch failed: %@",
                          error.localizedDescription)
                }
            }
        }
    }

    private func performOnMain(_ work: @escaping () -> Void) {
        if Thread.isMainThread {
            work()
        } else {
            DispatchQueue.main.sync(execute: work)
        }
    }

    private func send(contacts: [TrackpadContact], timestamp: Double) {
        guard captureEnabled || contacts.isEmpty, client != nil else { return }
        let bounded = contacts.prefix(16).map {
            WireContact(
                identifier: $0.identifier,
                x: min(1, max(0, Double($0.point.x))),
                y: min(1, max(0, Double($0.point.y))),
                state: $0.state
            )
        }
        let frame = ContactFrame(
            version: 1,
            timestamp: timestamp.isFinite ? timestamp : CACurrentMediaTime(),
            contacts: bounded,
            event: nil
        )
        send(frame)
    }

    private func sendExit() {
        send(ContactFrame(
            version: 1,
            timestamp: CACurrentMediaTime(),
            contacts: [],
            event: "exit"
        ))
    }

    private func send(_ frame: ContactFrame) {
        guard let client else { return }
        guard var payload = try? JSONEncoder().encode(frame),
              payload.count <= 16 * 1024 else { return }
        payload.append(0x0A)
        client.send(content: payload, completion: .contentProcessed { error in
            if let error {
                NSLog("TrackDrum for Menu Band: send failed: %@",
                      String(describing: error))
            }
        })
    }
}

private final class TrackpadPluginAppDelegate: NSObject, NSApplicationDelegate {
    private let server = TrackpadBridgeServer()
    private let updater = TrackDrumUpdater()

    func applicationDidFinishLaunching(_ notification: Notification) {
        do {
            try server.start()
        } catch {
            NSLog("TrackDrum for Menu Band: startup failed: %@",
                  error.localizedDescription)
            NSApp.terminate(nil)
        }
        // A helper with no window can't be told about updates, so it fetches
        // its own. Started after the bridge so a bad network never delays the
        // instrument coming up.
        updater.start()
    }

    func applicationWillTerminate(_ notification: Notification) {
        updater.stop()
        server.stop()
    }

    func applicationSupportsSecureRestorableState(_ app: NSApplication) -> Bool {
        true
    }
}

let app = NSApplication.shared
private let delegate = TrackpadPluginAppDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory)
app.run()
