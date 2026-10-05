import AppKit
import ApplicationServices
import Carbon
import Darwin

/// Physical US keyboard positions mapped to Linux evdev. No text or key logging.
enum ACRemoteKeys {
    static let codes: [UInt16: Int] = [
        0:30, 1:31, 2:32, 3:33, 4:35, 5:34, 6:44, 7:45, 8:46, 9:47,
        10:86, 11:48, 12:16, 13:17, 14:18, 15:19, 16:21, 17:20, 18:2, 19:3,
        20:4, 21:5, 22:7, 23:6, 24:13, 25:10, 26:8, 27:12, 28:9, 29:11,
        30:27, 31:24, 32:22, 33:26, 34:23, 35:25, 36:28, 37:38,
        38:36, 39:40, 40:37, 41:39, 42:43, 43:51, 44:53, 45:49,
        46:50, 47:52, 48:15, 49:57, 50:41, 51:14, 53:1,
        54:126, 55:125, 56:42, 57:58, 58:56, 59:29, 60:54, 61:100, 62:97,
        65:83, 67:55, 69:78, 71:69, 75:98, 76:96, 78:74, 81:117,
        82:82, 83:79, 84:80, 85:81, 86:75, 87:76, 88:77, 89:71, 91:72, 92:73,
        96:63, 97:64, 98:65, 99:61, 100:66, 101:67, 103:87, 105:183,
        106:186, 107:184, 109:68, 111:88, 113:185, 114:110, 115:102,
        116:104, 117:111, 118:62, 119:107, 120:60, 121:109, 122:59,
        123:105, 124:106, 125:108, 126:103,
    ]
    // NX_DEVICE* masks distinguish two held Shift/Control/Option/Command keys.
    static let modifierMasks: [UInt16: UInt64] = [
        54:0x10, 55:0x08, 56:0x02, 58:0x20, 59:0x01,
        60:0x04, 61:0x40, 62:0x2000,
    ]
    static func isToggle(_ key: UInt16, _ flags: CGEventFlags) -> Bool {
        key == 37 && flags.contains([.maskCommand, .maskAlternate])
            && flags.intersection([.maskShift, .maskControl]).isEmpty
    }
}

/// An explicit, fail-open keyboard handoff over one SSH stream. The event tap
/// exists only after the receiver is READY; a dropped heartbeat returns the Mac
/// keyboard immediately. The Linux peer independently releases keys on timeout.
final class ACKeyboardRemote: NSObject {
    struct Configuration: Decodable {
        var host: String = "ac0-remote"
        var label: String = "ac0"
    }
    var onCaptureChanged: ((Bool) -> Void)?
    var canConnect: (() -> Bool)?
    private var configuration = Configuration()
    private var hotkey: GlobalHotkey?
    private var item: NSStatusItem?
    private var process: Process?
    private var input: FileHandle?
    private var output: FileHandle?
    private var timer: Timer?
    private var tap: CFMachPort?
    private var source: CFRunLoopSource?
    private var buffer = Data()
    private var generation = 0
    private var startedAt: TimeInterval = 0
    private var lastPong: TimeInterval = 0
    private var nonce = 0
    private var outstandingPings = Set<String>()
    private var pressed = Set<Int>()
    private var waitingForModifiers = true
    private var capturing = false
    private var stopping = false
    private var now: TimeInterval { ProcessInfo.processInfo.systemUptime }

    func start() {
        let status = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        item = status
        status.button?.target = self
        status.button?.action = #selector(toggle)
        status.button?.image = NSImage(systemSymbolName: "keyboard", accessibilityDescription: "AC keyboard remote")
        status.button?.imagePosition = .imageLeading
        show("AC", detail: "AC keyboard remote · ⌘⌥L to connect")
        setPerformanceFocused(!(canConnect?() ?? true))
        NSWorkspace.shared.notificationCenter.addObserver(self, selector: #selector(suspend),
            name: NSWorkspace.willSleepNotification, object: nil)
        NSWorkspace.shared.notificationCenter.addObserver(self, selector: #selector(suspend),
            name: NSWorkspace.sessionDidResignActiveNotification, object: nil)
    }

    func setPerformanceFocused(_ focused: Bool) {
        if focused {
            disconnect()
            hotkey?.unregister()
            hotkey = nil
        } else if hotkey == nil {
            let shortcut = GlobalHotkey(id: 20) { [weak self] in self?.toggle() }
            if shortcut.register(keyCode: UInt32(kVK_ANSI_L), modifiers: UInt32(cmdKey | optionKey)) {
                hotkey = shortcut
            } else {
                show("AC !", detail: "⌘⌥L is unavailable. Click to connect.")
            }
        }
    }

    func shutdown() {
        disconnect()
        hotkey?.unregister()
        hotkey = nil
        NSWorkspace.shared.notificationCenter.removeObserver(self)
        if let item { NSStatusBar.system.removeStatusItem(item) }
        item = nil
    }

    @objc private func suspend() { disconnect() }

    @objc private func toggle() {
        if process != nil { disconnect(); return }
        guard canConnect?() ?? true else {
            fail("Leave Menu Band performance mode before connecting.")
            return
        }
        guard AXIsProcessTrusted(), ProcessInfo.processInfo.environment["SLAB_DISABLE_EVENT_TAPS"] != "1" else {
            fail("Enable Slab Menubar in System Settings → Privacy & Security → Accessibility.")
            return
        }
        let url = FileManager.default.homeDirectoryForCurrentUser.appendingPathComponent(".config/slab/ac-remote.json")
        if FileManager.default.fileExists(atPath: url.path) {
            guard let data = try? Data(contentsOf: url),
                  let config = try? JSONDecoder().decode(Configuration.self, from: data),
                  !config.host.isEmpty, config.host.range(of: "^[A-Za-z0-9][A-Za-z0-9._-]*$", options: .regularExpression) != nil,
                  !config.label.isEmpty, config.label.count <= 32 else {
                fail("Invalid ~/.config/slab/ac-remote.json; expected host and label.")
                return
            }
            configuration = config
        }
        generation += 1
        let revision = generation
        let task = Process()
        let stdinPipe = Pipe(), stdoutPipe = Pipe()
        task.executableURL = URL(fileURLWithPath: "/usr/bin/ssh")
        task.arguments = ["-T", "-o", "BatchMode=yes", "-o", "ConnectTimeout=4",
            "-o", "ServerAliveInterval=2", "-o", "ServerAliveCountMax=2",
            configuration.host, "/mnt/tools/ac-keyboard-remote"]
        task.standardInput = stdinPipe
        task.standardOutput = stdoutPipe
        task.standardError = FileHandle.nullDevice // No remote output or input is logged.
        process = task
        input = stdinPipe.fileHandleForWriting
        output = stdoutPipe.fileHandleForReading
        let descriptor = stdinPipe.fileHandleForWriting.fileDescriptor
        _ = fcntl(descriptor, F_SETFL, fcntl(descriptor, F_GETFL) | O_NONBLOCK)
        _ = fcntl(descriptor, F_SETNOSIGPIPE, 1)
        stdoutPipe.fileHandleForReading.readabilityHandler = { [weak self] handle in
            let data = handle.availableData
            DispatchQueue.main.async {
                guard let self, self.generation == revision else { return }
                if data.isEmpty { self.fail("AC connection closed.") }
                else { self.receive(data) }
            }
        }
        task.terminationHandler = { [weak self] _ in
            DispatchQueue.main.async {
                guard let self, self.generation == revision else { return }
                self.fail("AC connection closed. Click or press ⌘⌥L to reconnect.")
            }
        }
        startedAt = now
        lastPong = now
        show("AC …", detail: "Connecting to \(configuration.label) · click or ⌘⌥L to cancel")
        do { try task.run() }
        catch { fail("Could not start SSH."); return }
        let heartbeat = Timer(timeInterval: 1, repeats: true) { [weak self] _ in
            guard let self else { return }
            if !self.capturing {
                if self.now - self.startedAt > 6 { self.fail("AC receiver did not become ready.") }
                return
            }
            guard self.now - self.lastPong < 3 else { self.fail("AC connection lost; keyboard returned to this Mac."); return }
            self.nonce += 1
            let token = String(self.nonce)
            self.outstandingPings.insert(token)
            self.send("P \(token)\n")
        }
        timer = heartbeat
        RunLoop.main.add(heartbeat, forMode: .common)
    }

    private func receive(_ data: Data) {
        buffer.append(data)
        guard buffer.count < 4096 else { fail("Unexpected AC receiver response."); return }
        while let end = buffer.firstIndex(of: 10) {
            let line = String(decoding: buffer[..<end], as: UTF8.self)
            buffer.removeSubrange(...end)
            if line == "READY", !capturing {
                guard canConnect?() ?? true, beginCapture() else {
                    fail("Keyboard capture unavailable; leave other keyboard modes or check Accessibility.")
                    return
                }
                lastPong = now
                show("Remote → \(configuration.label)", detail: "Keyboard goes to \(configuration.label) · click or ⌘⌥L to return")
            } else if line.hasPrefix("P "), outstandingPings.remove(String(line.dropFirst(2))) != nil {
                lastPong = now
            }
        }
    }

    private func beginCapture() -> Bool {
        let mask = (1 << CGEventType.keyDown.rawValue) | (1 << CGEventType.keyUp.rawValue)
            | (1 << CGEventType.flagsChanged.rawValue)
        guard let port = CGEvent.tapCreate(tap: .cgSessionEventTap, place: .headInsertEventTap,
            options: .defaultTap, eventsOfInterest: CGEventMask(mask), callback: { _, type, event, context in
                guard let context else { return Unmanaged.passUnretained(event) }
                let remote = Unmanaged<ACKeyboardRemote>.fromOpaque(context).takeUnretainedValue()
                return remote.handle(type, event) ? nil : Unmanaged.passUnretained(event)
            }, userInfo: Unmanaged.passUnretained(self).toOpaque()) else { return false }
        tap = port
        source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, port, 0)
        CFRunLoopAddSource(CFRunLoopGetMain(), source, .commonModes)
        waitingForModifiers = !CGEventSource.flagsState(.combinedSessionState)
            .intersection([.maskCommand, .maskAlternate, .maskShift, .maskControl]).isEmpty
        capturing = true
        onCaptureChanged?(true)
        CGEvent.tapEnable(tap: port, enable: true)
        return true
    }

    private func handle(_ type: CGEventType, _ event: CGEvent) -> Bool {
        if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
            // Never silently resume after losing key-up events.
            fail("Keyboard capture interrupted; keyboard returned to this Mac.")
            return false
        }
        guard capturing else { return false }
        let key = UInt16(event.getIntegerValueField(.keyboardEventKeycode))
        if type == .keyDown && ACRemoteKeys.isToggle(key, event.flags) {
            disconnect()
            return true
        }
        if waitingForModifiers {
            waitingForModifiers = !event.flags.intersection([.maskCommand, .maskAlternate, .maskShift, .maskControl]).isEmpty
            return true // Do not forward the shortcut's still-held modifiers.
        }
        guard let code = ACRemoteKeys.codes[key] else { return true }
        if type == .flagsChanged {
            if key == 57 { send("K 58 1\nK 58 0\n"); return true }
            guard let mask = ACRemoteKeys.modifierMasks[key] else { return true }
            let down = event.flags.rawValue & mask != 0
            if down != pressed.contains(code) {
                if down { pressed.insert(code) } else { pressed.remove(code) }
                send("K \(code) \(down ? 1 : 0)\n")
            }
        } else if type == .keyDown {
            let value = pressed.contains(code) ? 2 : 1
            pressed.insert(code)
            send("K \(code) \(value)\n")
        } else if type == .keyUp, pressed.remove(code) != nil {
            send("K \(code) 0\n")
        }
        return true
    }

    private func send(_ line: String) {
        guard let input else { return }
        let bytes = Array(line.utf8)
        // Each protocol message fits in PIPE_BUF: EAGAIN fails open, never blocks
        // the event-tap callback behind SSH/network backpressure.
        let count = bytes.withUnsafeBytes { Darwin.write(input.fileDescriptor, $0.baseAddress, $0.count) }
        if count != bytes.count && !stopping { fail("AC connection stalled; keyboard returned to this Mac.") }
    }

    func disconnect() {
        guard !stopping else { return }
        stopping = true
        generation += 1
        if let tap { CGEvent.tapEnable(tap: tap, enable: false); CFMachPortInvalidate(tap) }
        if let source { CFRunLoopRemoveSource(CFRunLoopGetMain(), source, .commonModes) }
        tap = nil
        source = nil
        let wasCapturing = capturing
        capturing = false
        send("R\n")
        pressed.removeAll()
        outstandingPings.removeAll()
        buffer.removeAll()
        timer?.invalidate()
        timer = nil
        output?.readabilityHandler = nil
        try? input?.close()
        try? output?.close()
        input = nil
        output = nil
        if let process {
            process.terminationHandler = nil
            // Closing stdin lets the peer release first. Bound an unresponsive SSH.
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
                if process.isRunning { process.terminate() }
            }
        }
        process = nil
        if wasCapturing { onCaptureChanged?(false) }
        show("AC", detail: "AC keyboard remote · ⌘⌥L to connect")
        stopping = false
    }

    private func show(_ title: String, detail: String) {
        item?.button?.title = title
        item?.button?.toolTip = detail
        item?.button?.setAccessibilityLabel(detail)
    }

    private func fail(_ reason: String) {
        disconnect()
        show("AC !", detail: reason)
        NSLog("slab AC remote: %@", reason)
    }
}
