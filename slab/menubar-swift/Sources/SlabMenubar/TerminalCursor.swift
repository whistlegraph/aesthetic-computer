import AppKit
import Darwin

// Private CoreGraphics. With SetsCursorInBackground on our connection, an
// accessory app's NSCursor.set() reaches the screen while Terminal is in
// front; without it the set is silently dropped.
@_silgen_name("_CGSDefaultConnection")
private func _CGSDefaultConnection() -> Int32
@_silgen_name("CGSSetConnectionProperty")
private func CGSSetConnectionProperty(_ cid: Int32, _ target: Int32, _ key: CFString, _ value: CFTypeRef) -> Int32

/// Terminal.app ignores OSC 22, so an Aesel TUI with something clickable under
/// the mouse says so down cursor.sock and Slab shows the pointing hand for it.
/// One JSON line per change — {"cursor":"hand"|"arrow","session":…,"tty":…} —
/// plus a two-second beat while it is a hand. A stream socket rather than
/// datagrams: node has no unix datagrams, and a hung-up connection is the
/// quickest way to hear that a session is gone.
///
/// No window of ours ever sits over the terminal (an overlay once stole the
/// clicks it was decorating). Terminal puts its own cursor back on every
/// mouse move, so the hand is set again after each move instead, and only
/// while the pointer is over that session's window with Terminal in front.
final class TerminalCursor {
    static let shared = TerminalCursor()
    private init() {}

    private struct Hand { let session: String; let tty: String; var heard: Date }
    private var hands: [Int: Hand] = [:]      // by connection
    private var showing = false
    private var over: Int?                     // connection whose window holds the pointer
    private var checked = Date.distantPast
    private var monitor: Any?
    private var watchdog: Timer?
    private var background = false
    private var nextClient = 0
    private let queue = DispatchQueue(label: "computer.slab.cursor.socket")
    private static let terminals: Set<String> = ["com.apple.Terminal", "com.googlecode.iterm2"]

    func start() {
        let path = "\(Paths.slabHome)/state/cursor.sock"
        try? FileManager.default.createDirectory(atPath: (path as NSString).deletingLastPathComponent,
                                                 withIntermediateDirectories: true)
        let fd = socket(AF_UNIX, SOCK_STREAM, 0)
        guard fd >= 0 else { return }
        var address = sockaddr_un()
        address.sun_family = sa_family_t(AF_UNIX)
        let bytes = Array(path.utf8)
        guard bytes.count < MemoryLayout.size(ofValue: address.sun_path) else { close(fd); return }
        withUnsafeMutableBytes(of: &address.sun_path) { $0.copyBytes(from: bytes + [0]) }
        unlink(path)
        let bound = withUnsafePointer(to: &address) { ptr in
            ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) {
                Darwin.bind(fd, $0, socklen_t(MemoryLayout<sockaddr_un>.size))
            }
        }
        guard bound == 0, listen(fd, 16) == 0 else {
            NSLog("cursor: socket unavailable (errno %d)", errno); close(fd); return
        }
        chmod(path, 0o600)
        queue.async { [self] in
            while true {
                let client = accept(fd, nil, nil)
                if client < 0 { if errno == EINTR { continue }; break }
                nextClient += 1
                let id = nextClient
                DispatchQueue.global(qos: .userInitiated).async { self.serve(client, id: id) }
            }
        }
    }

    // One connection per TUI, held for its life; blocking reads are fine for
    // the handful of sessions a desk ever has open.
    private func serve(_ fd: Int32, id: Int) {
        var uid: uid_t = 0, gid: gid_t = 0
        var one: Int32 = 1
        setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, socklen_t(MemoryLayout<Int32>.size))
        if getpeereid(fd, &uid, &gid) == 0, uid == getuid() {
            var pending = [UInt8](), buffer = [UInt8](repeating: 0, count: 512)
            reading: while pending.count < 4096 {
                let n = read(fd, &buffer, buffer.count)
                if n < 0 && errno == EINTR { continue }
                guard n > 0 else { break reading }
                pending += buffer.prefix(n)
                while let end = pending.firstIndex(of: 10) {
                    let line = Data(pending[..<end])
                    pending.removeSubrange(...end)
                    guard let body = (try? JSONSerialization.jsonObject(with: line)) as? [String: Any],
                          let shape = body["cursor"] as? String,
                          let session = body["session"] as? String else { continue }
                    let tty = body["tty"] as? String ?? ""
                    DispatchQueue.main.async { self.heard(id, shape: shape, session: session, tty: tty) }
                }
            }
        }
        DispatchQueue.main.async { self.drop(id) }
        close(fd)
    }

    private func heard(_ id: Int, shape: String, session: String, tty: String) {
        guard shape == "hand" else { return drop(id) }
        if hands[id] == nil { checked = .distantPast }
        hands[id] = Hand(session: session, tty: tty, heard: Date())
        watch()
        assertHand()
    }

    private func drop(_ id: Int) {
        guard hands.removeValue(forKey: id) != nil else { return }
        if over == id { over = nil; checked = .distantPast }
        assertHand()
        if hands.isEmpty { unwatch() }
    }

    private func watch() {
        if monitor == nil {
            monitor = NSEvent.addGlobalMonitorForEvents(matching: [.mouseMoved]) { [weak self] _ in
                self?.assertHand()
                // Terminal may reset its cursor just after we see the move.
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.012) { self?.assertHand(recheck: false) }
            }
        }
        if watchdog == nil {
            watchdog = Timer.scheduledTimer(withTimeInterval: 1, repeats: true) { [weak self] _ in
                guard let self else { return }
                // A TUI that stopped beating (hung, or a menubar it can't
                // reach) should not leave the hand on screen.
                for (id, hand) in self.hands where Date().timeIntervalSince(hand.heard) > 6 { self.drop(id) }
            }
        }
    }

    private func unwatch() {
        if let monitor { NSEvent.removeMonitor(monitor) }
        monitor = nil
        watchdog?.invalidate()
        watchdog = nil
    }

    private func assertHand(recheck: Bool = true) {
        // A native rock, handle or preview owns its own cursor. In particular,
        // a queued Terminal retry must not paint an arrow over its hand.
        let number = windowUnderPointer()
        if NSApp.windows.contains(where: { $0.windowNumber == number && !$0.ignoresMouseEvents }) {
            showing = false; over = nil; checked = .distantPast
            return
        }
        if recheck, Date().timeIntervalSince(checked) > 0.02 {
            checked = Date()
            over = claimUnderPointer(window: number)
        }
        if let id = over, let hand = hands[id] {
            if !background {
                background = true
                _ = CGSSetConnectionProperty(_CGSDefaultConnection(), _CGSDefaultConnection(),
                                             "SetsCursorInBackground" as CFString, kCFBooleanTrue)
            }
            NSCursor.pointingHand.set()
            if !showing { showing = true; NSLog("🖐 cursor: hand over %@ (%@)", hand.tty, hand.session) }
        } else if showing {
            showing = false
            NSCursor.arrow.set()
            NSLog("🖐 cursor: arrow")
        }
    }

    /// The hand whose Terminal window is the topmost window under the pointer,
    /// with Terminal frontmost (a background terminal reports no motion, so its
    /// hover could be stale).
    private func claimUnderPointer(window number: Int) -> Int? {
        guard !hands.isEmpty,
              Self.terminals.contains(NSWorkspace.shared.frontmostApplication?.bundleIdentifier ?? "")
        else { return nil }
        guard number > 0 else { return nil }
        return hands.first { PromptSigilOverlayController.shared.terminalWindowID(tty: $0.value.tty) == number }?.key
    }

    private func windowUnderPointer() -> Int {
        let point = NSEvent.mouseLocation
        let transparent = Set(NSApp.windows.filter(\.ignoresMouseEvents).map(\.windowNumber))
        var below = 0, number = 0
        for _ in 0..<8 {
            number = NSWindow.windowNumber(at: point, belowWindowWithWindowNumber: below)
            guard transparent.contains(number), number > 0 else { break }
            below = number
        }
        return number
    }
}
