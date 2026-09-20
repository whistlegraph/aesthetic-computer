import AppKit
import ApplicationServices

/// Listen for real window creation; retain the periodic census for missed AX events.
final class TilePopulationObserver {
    private var observers: [pid_t: AXObserver] = [:]
    private var workspaceTokens: [NSObjectProtocol] = []
    private let changed: () -> Void
    private let bundleIDs: Set<String> = ["com.apple.Terminal", "com.googlecode.iterm2", "computer.aesthetic.app", "computer.aesthetic.easel", "computer.aesthetic.aesel"]

    init(changed: @escaping () -> Void) {
        self.changed = changed
        let center = NSWorkspace.shared.notificationCenter
        for name in [NSWorkspace.didLaunchApplicationNotification, NSWorkspace.didTerminateApplicationNotification, NSWorkspace.didActivateApplicationNotification] {
            workspaceTokens.append(center.addObserver(forName: name, object: nil, queue: .main) { [weak self] note in
                guard let self, let app = note.userInfo?[NSWorkspace.applicationUserInfoKey] as? NSRunningApplication,
                      self.bundleIDs.contains(app.bundleIdentifier ?? "") else { return }
                self.refresh()
                self.changed()
            })
        }
        refresh()
    }

    func refresh() {
        guard AXIsProcessTrusted() else { return }
        let apps = NSWorkspace.shared.runningApplications.filter { bundleIDs.contains($0.bundleIdentifier ?? "") }
        let live = Set(apps.map(\.processIdentifier))
        for pid in Array(observers.keys) where !live.contains(pid) {
            if let observer = observers.removeValue(forKey: pid) {
                CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer), .commonModes)
            }
        }
        for app in apps where observers[app.processIdentifier] == nil {
            var observer: AXObserver?
            let callback: AXObserverCallback = { _, _, _, refcon in
                guard let refcon else { return }
                let owner = Unmanaged<TilePopulationObserver>.fromOpaque(refcon).takeUnretainedValue()
                owner.changed()
            }
            guard AXObserverCreate(app.processIdentifier, callback, &observer) == .success, let observer else { continue }
            let element = AXUIElementCreateApplication(app.processIdentifier)
            let context = Unmanaged.passUnretained(self).toOpaque()
            var registered = false
            for notification in [kAXWindowCreatedNotification, kAXFocusedWindowChangedNotification, kAXUIElementDestroyedNotification] {
                if AXObserverAddNotification(observer, element, notification as CFString, context) == .success { registered = true }
            }
            guard registered else { continue }
            observers[app.processIdentifier] = observer
            CFRunLoopAddSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer), .commonModes)
        }
    }

    deinit {
        for observer in observers.values { CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer), .commonModes) }
        for token in workspaceTokens { NSWorkspace.shared.notificationCenter.removeObserver(token) }
    }
}
