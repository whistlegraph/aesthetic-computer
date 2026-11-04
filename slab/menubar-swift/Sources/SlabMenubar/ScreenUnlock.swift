import AppKit
import ApplicationServices
import Carbon.HIToolbox
import Security

/// Unlock a fleet Mac's locked screen from this one.
///
/// macOS refuses synthetic keystrokes at its own lock screen (secure input),
/// but a Screen Sharing session forwards keys to the remote one — so unlocking
/// host X is: open `vnc://X`, wait for its session window, and type X's login
/// password into Screen Sharing, and only Screen Sharing (`postToPid`).
///
/// The password is typed once by a person into a secure field and lives in
/// this Mac's login Keychain (service `computer.slab.unlock`, account = host).
/// It is never logged, written to disk, or handed to anything but those key
/// events — agents driving the fleet never see it.
enum ScreenUnlock {
    private static let screenSharing = "com.apple.ScreenSharing"

    static func run(host: String, ip: String) {
        let target = ip.isEmpty ? host : ip
        guard let url = URL(string: "vnc://\(target)") else { return }
        DispatchQueue.global(qos: .userInitiated).async {
            let before = sessionDocuments()
            DispatchQueue.main.async { NSWorkspace.shared.open(url) }
            guard let secret = UnlockKeychain.read(host) else {
                NSLog("🔓 [unlock] %@ — no saved password; opened Screen Sharing only", host)
                return
            }
            guard let pid = waitForSession(host: host, ip: ip, before: before, timeout: 15) else {
                NSLog("🔓 [unlock] %@ — no Screen Sharing session window within 15 s", host)
                return
            }
            // Bring the session window forward so it is the one listening,
            // tap Shift to wake a sleeping remote display, then give the lock
            // screen a moment to draw its password field.
            DispatchQueue.main.sync {
                _ = NSRunningApplication(processIdentifier: pid)?.activate(options: [])
            }
            Thread.sleep(forTimeInterval: 1.0)
            tap(keyCode: CGKeyCode(kVK_Shift), pid: pid)
            Thread.sleep(forTimeInterval: 1.5)
            let typed = type(secret, pid: pid)
            tap(keyCode: CGKeyCode(kVK_Return), pid: pid)
            NSLog("🔓 [unlock] %@ — typed into Screen Sharing (%@)", host, typed ? "all keys mapped" : "some keys unmapped")
        }
    }

    // MARK: Finding the session window

    /// Every Screen Sharing window that is a live session: its AXDocument is
    /// the `.vncloc` the connection opened (panels and sheets have none).
    private static func sessionDocuments() -> [pid_t: Set<String>] {
        var found: [pid_t: Set<String>] = [:]
        for app in NSRunningApplication.runningApplications(withBundleIdentifier: screenSharing) {
            let ax = AXUIElementCreateApplication(app.processIdentifier)
            var windows: CFTypeRef?
            guard AXUIElementCopyAttributeValue(ax, kAXWindowsAttribute as CFString, &windows) == .success,
                  let list = windows as? [AXUIElement] else { continue }
            for window in list {
                var doc: CFTypeRef?
                AXUIElementCopyAttributeValue(window, kAXDocumentAttribute as CFString, &doc)
                var title: CFTypeRef?
                AXUIElementCopyAttributeValue(window, kAXTitleAttribute as CFString, &title)
                guard let path = doc as? String, path.hasSuffix(".vncloc") else { continue }
                found[app.processIdentifier, default: []].insert(path + "|" + ((title as? String) ?? ""))
            }
        }
        return found
    }

    /// The pid owning a session window for this host: one whose document or
    /// title names the host or its address, else any session that is new
    /// since the connection was opened.
    private static func waitForSession(host: String, ip: String, before: [pid_t: Set<String>],
                                       timeout: TimeInterval) -> pid_t? {
        let deadline = Date().addingTimeInterval(timeout)
        let names = [host.lowercased(), ip.lowercased()].filter { !$0.isEmpty }
        while Date() < deadline {
            for (pid, sessions) in sessionDocuments() {
                if sessions.contains(where: { s in names.contains { s.lowercased().contains($0) } }) { return pid }
                if !sessions.subtracting(before[pid] ?? []).isEmpty { return pid }
            }
            Thread.sleep(forTimeInterval: 0.5)
        }
        return nil
    }

    // MARK: Typing

    /// Screen Sharing forwards key codes, not characters, so each character is
    /// mapped back to the key (and Shift) that produces it on this layout.
    private static func type(_ text: String, pid: pid_t) -> Bool {
        let keys = keyMap()
        var complete = true
        for character in text {
            guard let (code, shift) = keys[character] else { complete = false; continue }
            if shift { press(CGKeyCode(kVK_Shift), down: true, flags: [], pid: pid) }
            let flags: CGEventFlags = shift ? .maskShift : []
            press(code, down: true, flags: flags, pid: pid)
            press(code, down: false, flags: flags, pid: pid)
            if shift { press(CGKeyCode(kVK_Shift), down: false, flags: [], pid: pid) }
            Thread.sleep(forTimeInterval: 0.03)
        }
        return complete
    }

    private static func tap(keyCode: CGKeyCode, pid: pid_t) {
        press(keyCode, down: true, flags: [], pid: pid)
        press(keyCode, down: false, flags: [], pid: pid)
    }

    private static func press(_ code: CGKeyCode, down: Bool, flags: CGEventFlags, pid: pid_t) {
        guard let event = CGEvent(keyboardEventSource: nil, virtualKey: code, keyDown: down) else { return }
        event.flags = flags
        event.postToPid(pid)
    }

    private static func keyMap() -> [Character: (CGKeyCode, Bool)] {
        var map: [Character: (CGKeyCode, Bool)] = [:]
        guard let source = TISCopyCurrentKeyboardLayoutInputSource()?.takeRetainedValue(),
              let raw = TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData) else { return map }
        let data = Unmanaged<CFData>.fromOpaque(raw).takeUnretainedValue() as Data
        data.withUnsafeBytes { buffer in
            guard let layout = buffer.baseAddress?.assumingMemoryBound(to: UCKeyboardLayout.self) else { return }
            for shift in [false, true] {
                let modifiers = UInt32(shift ? (shiftKey >> 8) & 0xFF : 0)
                for code in 0..<128 {
                    var dead: UInt32 = 0
                    var length = 0
                    var chars = [UniChar](repeating: 0, count: 4)
                    let status = UCKeyTranslate(layout, UInt16(code), UInt16(kUCKeyActionDown), modifiers,
                                                UInt32(LMGetKbdType()), OptionBits(kUCKeyTranslateNoDeadKeysBit),
                                                &dead, chars.count, &length, &chars)
                    guard status == noErr, length == 1,
                          let scalar = Unicode.Scalar(chars[0]) else { continue }
                    let character = Character(scalar)
                    if map[character] == nil { map[character] = (CGKeyCode(code), shift) }
                }
            }
        }
        return map
    }
}

/// One login password per fleet host, in this Mac's Keychain.
enum UnlockKeychain {
    private static let service = "computer.slab.unlock"

    private static func query(_ host: String) -> [String: Any] {
        [kSecClass as String: kSecClassGenericPassword,
         kSecAttrService as String: service,
         kSecAttrAccount as String: host]
    }

    /// Attributes only — says whether a password is saved without reading it.
    static func has(_ host: String) -> Bool {
        var q = query(host)
        q[kSecReturnAttributes as String] = true
        return SecItemCopyMatching(q as CFDictionary, nil) == errSecSuccess
    }

    static func read(_ host: String) -> String? {
        var q = query(host)
        q[kSecReturnData as String] = true
        q[kSecMatchLimit as String] = kSecMatchLimitOne
        var item: CFTypeRef?
        guard SecItemCopyMatching(q as CFDictionary, &item) == errSecSuccess,
              let data = item as? Data else { return nil }
        return String(data: data, encoding: .utf8)
    }

    @discardableResult
    static func save(_ host: String, _ secret: String) -> Bool {
        let data = Data(secret.utf8)
        let status = SecItemUpdate(query(host) as CFDictionary, [kSecValueData as String: data] as CFDictionary)
        if status == errSecSuccess { return true }
        var add = query(host)
        add[kSecValueData as String] = data
        add[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly
        return SecItemAdd(add as CFDictionary, nil) == errSecSuccess
    }

    static func forget(_ host: String) {
        SecItemDelete(query(host) as CFDictionary)
    }
}

/// Which online fleet Macs are sitting at a lock screen, asked over ssh at
/// most once a minute per host and cached for the Tailnet menu's 🔒.
enum LockProbe {
    private static var cache: [String: (locked: Bool, at: Date)] = [:]
    private static var probing = false
    private static let guardLock = NSLock()
    private static let queue = DispatchQueue(label: "computer.slab.lockprobe", qos: .utility)

    static func locked(_ host: String) -> Bool {
        guardLock.lock(); defer { guardLock.unlock() }
        return cache[host]?.locked ?? false
    }

    /// Called from the snapshot tick; returns at once and asks on its own
    /// queue, so a slow host never holds the tick. ioreg rather than python
    /// Quartz: a bare Mac would answer the latter with an install dialog for
    /// the command line tools.
    static func refresh(_ hosts: [String]) {
        guardLock.lock()
        if probing { guardLock.unlock(); return }
        probing = true
        guardLock.unlock()
        queue.async {
            probe(hosts)
            guardLock.lock(); probing = false; guardLock.unlock()
        }
    }

    private static func probe(_ hosts: [String]) {
        for host in hosts {
            guardLock.lock()
            let fresh = cache[host].map { Date().timeIntervalSince($0.at) < 60 } ?? false
            guardLock.unlock()
            if fresh { continue }
            let out = ShellRunner.output("/usr/bin/ssh", args: [
                "-o", "BatchMode=yes", "-o", "ConnectTimeout=3", host,
                "ioreg -n Root -d1 | grep -c '\"CGSSessionScreenIsLocked\"=Yes'",
            ], timeout: 6)
            guard let answer = out?.trimmingCharacters(in: .whitespacesAndNewlines), !answer.isEmpty else { continue }
            guardLock.lock()
            cache[host] = (answer != "0", Date())
            guardLock.unlock()
        }
    }
}
