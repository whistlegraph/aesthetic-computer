#if MAC_APP_STORE
import Foundation
import ServiceManagement

/// Launch-at-login for the App Store build.
///
/// The direct-download build auto-starts from a LaunchAgent that `install.sh`
/// writes into `~/Library/LaunchAgents`. A sandboxed App Store app can't
/// install a LaunchAgent at all, so before this it simply never came back after
/// a reboot — a real gap for a menu-bar utility. Here we use `SMAppService`
/// (macOS 13+), the sandbox-legal successor, gated so the whole feature is
/// App-Store-only (`#if MAC_APP_STORE`) and never fights the LaunchAgent.
///
/// Menu Band's job is to be present, so login launch defaults ON — matching the
/// direct-download LaunchAgent's always-on behavior — with a visible toggle in
/// About to turn it off. On macOS 11–12 (no `SMAppService`) it's a no-op: the
/// app just won't auto-start, which is exactly how those systems behaved before.
enum MenuBandLoginItem {
    private static let prefKey = "MBOpenAtLogin"

    /// Whether SMAppService exists on this OS (macOS 13+). When false, the
    /// About toggle hides itself — there's nothing to control.
    static var isSupported: Bool {
        if #available(macOS 13, *) { return true }
        return false
    }

    /// The user's stored preference. Defaults to `true` (parity with the
    /// direct-download always-on auto-start). Setting it reconciles the OS
    /// registration immediately.
    static var isEnabled: Bool {
        get {
            if UserDefaults.standard.object(forKey: prefKey) == nil { return true }
            return UserDefaults.standard.bool(forKey: prefKey)
        }
        set {
            UserDefaults.standard.set(newValue, forKey: prefKey)
            apply()
        }
    }

    /// Reconcile the OS login-item registration with `isEnabled`. Safe to call
    /// on every launch; `register()`/`unregister()` are idempotent against the
    /// current `status`.
    static func apply() {
        guard #available(macOS 13, *) else { return }
        let service = SMAppService.mainApp
        do {
            if isEnabled {
                if service.status != .enabled { try service.register() }
            } else if service.status == .enabled {
                try service.unregister()
            }
        } catch {
            NSLog("MenuBand: login-item \(isEnabled ? "register" : "unregister") failed: \(error.localizedDescription)")
        }
    }
}
#endif
