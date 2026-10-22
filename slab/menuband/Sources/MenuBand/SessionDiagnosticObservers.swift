import AppKit

/// Observes only our app/window state. Workspace events retain a self/other
/// boolean, never the other application's identity or document/window title.
final class SessionDiagnosticObservers {
    private var observers: [(NotificationCenter, NSObjectProtocol)] = []

    init(diagnostics: SessionDiagnostics = .shared) {
        guard diagnostics.enabled else { return }
        let center = NotificationCenter.default
        let appEvents: [(Notification.Name, SessionDiagnostics.Event)] = [
            (NSApplication.didBecomeActiveNotification, .appActive),
            (NSApplication.didResignActiveNotification, .appInactive),
            (NSWindow.didBecomeKeyNotification, .windowKey),
            (NSWindow.didResignKeyNotification, .windowResigned)
        ]
        for (name, event) in appEvents {
            let token = center.addObserver(forName: name, object: nil, queue: .main) { _ in
                diagnostics.record(event, state: NSApp.isActive)
            }
            observers.append((center, token))
        }
        let workspace = NSWorkspace.shared.notificationCenter
        let workspaceEvents: [(Notification.Name, SessionDiagnostics.Event)] = [
            (NSWorkspace.willSleepNotification, .systemSleep),
            (NSWorkspace.didWakeNotification, .systemWake),
            (NSWorkspace.sessionDidBecomeActiveNotification, .sessionActive),
            (NSWorkspace.sessionDidResignActiveNotification, .sessionInactive)
        ]
        for (name, event) in workspaceEvents {
            let token = workspace.addObserver(forName: name, object: nil, queue: .main) { _ in
                diagnostics.record(event)
            }
            observers.append((workspace, token))
        }
        let token = workspace.addObserver(forName: NSWorkspace.didActivateApplicationNotification,
                                         object: nil, queue: .main) { notification in
            let app = notification.userInfo?[NSWorkspace.applicationUserInfoKey] as? NSRunningApplication
            let isSelf = app?.processIdentifier == ProcessInfo.processInfo.processIdentifier
            diagnostics.record(.workspaceActivation, state: isSelf)
            if !isSelf {
                // Which app took the front is the one fact a defocus
                // post-mortem always needs; the ledger carries no identifiers,
                // so it goes to the debug log.
                debugLog("workspace activated: \(app?.bundleIdentifier ?? "?") pid=\(app?.processIdentifier ?? -1)")
            }
        }
        observers.append((workspace, token))
    }

    deinit { for (center, token) in observers { center.removeObserver(token) } }
}
