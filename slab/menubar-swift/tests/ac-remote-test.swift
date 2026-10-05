// Appended to ACKeyboardRemote.swift in a temporary translation unit by the
// runner. Swift extensions in the same file can inspect private state without
// exposing test-only capture controls in the installed application.
extension ACKeyboardRemote {
    static func runTests() {
        func fixture() -> (ACKeyboardRemote, Pipe) {
            let remote = ACKeyboardRemote(), pipe = Pipe()
            remote.input = pipe.fileHandleForWriting
            remote.capturing = true
            remote.waitingForModifiers = false
            for fd in [pipe.fileHandleForReading.fileDescriptor, pipe.fileHandleForWriting.fileDescriptor] {
                _ = fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK)
                _ = fcntl(fd, F_SETNOSIGPIPE, 1)
            }
            return (remote, pipe)
        }
        func drain(_ pipe: Pipe) -> String {
            var bytes = [UInt8](repeating: 0, count: 16384)
            let count = Darwin.read(pipe.fileHandleForReading.fileDescriptor, &bytes, bytes.count)
            return count > 0 ? String(decoding: bytes.prefix(count), as: UTF8.self) : ""
        }
        func event(_ key: UInt16, _ type: CGEventType, _ flags: CGEventFlags = []) -> CGEvent {
            let event = CGEvent(keyboardEventSource: nil, virtualKey: key, keyDown: type != .keyUp)!
            event.type = type
            event.flags = flags
            return event
        }
        let (remote, pipe) = fixture()
        for type in [CGEventType.keyDown, .keyDown, .keyUp] {
            assert(remote.handle(type, event(0, type)))
        }
        assert(drain(pipe) == "K 30 1\nK 30 2\nK 30 0\n")
        // Two shifts: releasing the left one must leave the right one down.
        for (key, flags): (UInt16, UInt64) in [(56, 0x20002), (60, 0x20006), (56, 0x20004), (60, 0)] {
            _ = remote.handle(.flagsChanged, event(key, .flagsChanged, CGEventFlags(rawValue: flags)))
        }
        assert(drain(pipe) == "K 42 1\nK 54 1\nK 42 0\nK 54 0\n")
        _ = remote.handle(.keyDown, event(53, .keyDown))
        _ = remote.handle(.keyUp, event(53, .keyUp))
        assert(drain(pipe) == "K 1 1\nK 1 0\n", "Escape must reach the AC prompt")
        assert(ACRemoteKeys.codes[10] == 86 && ACRemoteKeys.codes[123] == 105)
        assert(!ACRemoteKeys.isToggle(37, [.maskCommand, .maskAlternate, .maskShift]))
        _ = remote.handle(.keyDown, event(37, .keyDown, [.maskCommand, .maskAlternate]))
        assert(!remote.capturing && remote.input == nil)
        assert(drain(pipe) == "R\n", "The exit shortcut must never become a remote L")
        assert(!remote.handle(.keyDown, event(0, .keyDown)), "Idle mode must pass through")

        let (arming, armPipe) = fixture()
        arming.waitingForModifiers = true
        _ = arming.handle(.keyDown, event(0, .keyDown, [.maskCommand]))
        _ = arming.handle(.flagsChanged, event(55, .flagsChanged))
        assert(drain(armPipe).isEmpty)
        _ = arming.handle(.keyDown, event(0, .keyDown))
        assert(drain(armPipe) == "K 30 1\n")
        _ = arming.handle(.tapDisabledByTimeout, event(0, .keyUp))
        assert(!arming.capturing && drain(armPipe) == "R\n")

        let (backpressure, fullPipe) = fixture()
        let fill = [UInt8](repeating: 120, count: 4096)
        while fill.withUnsafeBytes({ Darwin.write(fullPipe.fileHandleForWriting.fileDescriptor, $0.baseAddress, $0.count) }) > 0 {}
        // Atomic writes under PIPE_BUF must fail promptly instead of freezing
        // the event tap when SSH has stopped consuming its pipe.
        let start = ProcessInfo.processInfo.systemUptime
        backpressure.send("K 30 1\n")
        assert(!backpressure.capturing)
        assert(ProcessInfo.processInfo.systemUptime - start < 0.5)

        let (heartbeat, heartbeatPipe) = fixture()
        heartbeat.lastPong = 0
        heartbeat.outstandingPings = ["42"]
        heartbeat.receive(Data("P 41\nP ".utf8))
        assert(heartbeat.lastPong == 0)
        heartbeat.receive(Data("42\n".utf8))
        assert(heartbeat.lastPong > 0 && heartbeat.outstandingPings.isEmpty)
        heartbeat.disconnect()
        assert(drain(heartbeatPipe) == "R\n")
        print("AC remote tests passed: key lifecycle, modifier overlap, escape, local exit, arming, disabled tap, nonblocking backpressure, heartbeat framing.")
    }
}
ACKeyboardRemote.runTests()
