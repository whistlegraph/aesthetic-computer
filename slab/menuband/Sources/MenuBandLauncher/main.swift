// MenuBandLauncher — tiny always-running helper.
//
// Job: watch for a same-side double-tap of either Command key, and if Menu
// Band's main process isn't running, launch it. When Menu Band IS
// running, the launcher no-ops.
//
// Left-left and right-right both summon the app. Mixed-side pairs do not, so
// ordinary two-handed Command use remains harmless.
//
// CGEventTap variant: an earlier version used
// NSEvent.addGlobalMonitorForEvents, which silently delivered zero
// events after each codesign rebuild even with AXIsProcessTrusted
// returning true. CGEventTap fails LOUDLY when Accessibility isn't
// granted (CGEvent.tapCreate returns nil), and it explicitly notifies
// us when the tap is disabled at runtime, so we can re-enable or
// surface the problem.
//
// Two install shapes, both supported:
//   - Nested at Menu Band.app/Contents/MacOS/MenuBandLauncher, alongside
//     the SwiftPM direct-download build (install.sh).
//   - Standalone at ~/Applications/MenuBandLauncher.app, for machines
//     running the Xcode/App-Store fork of Menu Band, whose target
//     deliberately excludes this helper (install-launcher.sh).
// Either way it is signed with its own identifier
// (computer.aestheticcomputer.menubandlauncher) so TCC tracks it
// separately from the main binary.

import AppKit
import ApplicationServices
import Foundation
import IOKit.hid

final class Launcher {
    // Two shipping bundle ids: the direct-download SwiftPM build and the
    // Xcode/App-Store fork, which coexist on one Mac by design.
    private static let menuBandBundleIDs = [
        "computer.aestheticcomputer.menuband",
        "computer.aesthetic.menuband",
    ]
    // Carbon virtual keycodes: kVK_Command (left, conventional) = 55,
    // kVK_RightCommand = 54. Some keyboards only emit 55 for both
    // sides, distinguishing via device-specific flag bits in the low
    // 16 bits of CGEventFlags (NX_DEVICELCMDKEYMASK = 0x8,
    // NX_DEVICERCMDKEYMASK = 0x10). Accept either keycode and use
    // the device flag to identify the side actually pressed.
    private static let leftCommandKeyCode: Int64 = 55
    private static let rightCommandKeyCode: Int64 = 54
    private static let nxDeviceLCmd: UInt64 = 0x8
    private static let nxDeviceRCmd: UInt64 = 0x10
    private static let doubleTapWindow: CFTimeInterval = 0.50

    private var lastPressAt: CFTimeInterval = 0
    private var lastPressWasRight: Bool?
    private var tap: CFMachPort?
    private var runLoopSource: CFRunLoopSource?

    /// A listen-only keyboard tap is gated on **Input Monitoring**
    /// (kTCCServiceListenEvent), not Accessibility. The distinction matters
    /// because the failure is invisible otherwise: without the grant,
    /// `CGEvent.tapCreate` still SUCCEEDS and simply never delivers a single
    /// event, so the launcher looks healthy in `launchctl list` and in its own
    /// log while ⌘⌘ does nothing at all. (`AXIsProcessTrusted` is not the
    /// right question either — it reported false on a build whose tap worked.)
    ///
    /// So ask explicitly. `IOHIDRequestAccess` is what raises the system
    /// prompt; once TCC has recorded a denial it returns false without
    /// prompting, which is the case worth logging loudly since the only way
    /// out is System Settings or `tccutil reset`.
    private func ensureInputMonitoring() {
        let access = IOHIDCheckAccess(kIOHIDRequestTypeListenEvent)
        switch access {
        case kIOHIDAccessTypeGranted:
            NSLog("MenuBandLauncher: Input Monitoring granted")
        case kIOHIDAccessTypeDenied:
            NSLog("MenuBandLauncher: Input Monitoring DENIED — the tap will install but receive ZERO events. Enable 'Menu Band Launcher' under System Settings > Privacy & Security > Input Monitoring, or run: tccutil reset ListenEvent computer.aestheticcomputer.menubandlauncher")
        default:
            NSLog("MenuBandLauncher: Input Monitoring not yet decided — requesting")
            let granted = IOHIDRequestAccess(kIOHIDRequestTypeListenEvent)
            NSLog("MenuBandLauncher: IOHIDRequestAccess returned \(granted)")
        }
    }

    func start() -> Bool {
        ensureInputMonitoring()

        let mask = (1 << CGEventType.flagsChanged.rawValue) |
                   (1 << CGEventType.keyDown.rawValue) |
                   (1 << CGEventType.tapDisabledByTimeout.rawValue) |
                   (1 << CGEventType.tapDisabledByUserInput.rawValue)

        let opaque = Unmanaged.passUnretained(self).toOpaque()
        let callback: CGEventTapCallBack = { proxy, type, event, userInfo in
            guard let userInfo = userInfo else {
                return Unmanaged.passUnretained(event)
            }
            let launcher = Unmanaged<Launcher>.fromOpaque(userInfo).takeUnretainedValue()
            launcher.handle(type: type, event: event)
            return Unmanaged.passUnretained(event)
        }

        guard let tap = CGEvent.tapCreate(
            tap: .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .listenOnly,
            eventsOfInterest: CGEventMask(mask),
            callback: callback,
            userInfo: opaque
        ) else {
            NSLog("MenuBandLauncher: CGEvent.tapCreate FAILED — grant Input Monitoring to 'Menu Band Launcher' in System Settings > Privacy & Security.")
            return false
        }
        self.tap = tap
        let src = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, tap, 0)
        CFRunLoopAddSource(CFRunLoopGetCurrent(), src, .commonModes)
        self.runLoopSource = src
        CGEvent.tapEnable(tap: tap, enable: true)
        NSLog("MenuBandLauncher: CGEventTap installed")
        return true
    }

    private func handle(type: CGEventType, event: CGEvent) {
        if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
            NSLog("MenuBandLauncher: tap disabled (\(type.rawValue)) — re-enabling")
            if let tap = tap { CGEvent.tapEnable(tap: tap, enable: true) }
            return
        }
        let keyCode = event.getIntegerValueField(.keyboardEventKeycode)

        // Any real key between the taps breaks the run, unconditionally —
        // otherwise ⌘C ⌘ reads as tap-tap and summons the app mid-copy.
        // The Command keycodes themselves are exempt: synthesized input
        // (System Events' `key code 54`) delivers them as keyDowns. No log
        // line here — this daemon's log is a plain file nothing rotates.
        if type == .keyDown {
            if keyCode != Self.leftCommandKeyCode && keyCode != Self.rightCommandKeyCode {
                lastPressAt = 0
                lastPressWasRight = nil
            }
            return
        }
        guard type == .flagsChanged else { return }
        let flags = event.flags
        let rawFlags = flags.rawValue
        let isCmdKey = (keyCode == Self.leftCommandKeyCode || keyCode == Self.rightCommandKeyCode)
        let side: String
        if (rawFlags & Self.nxDeviceRCmd) != 0 { side = "right" }
        else if (rawFlags & Self.nxDeviceLCmd) != 0 { side = "left" }
        else { side = "?" }
        // Log Command keys only. This daemon runs forever, and its log is a
        // plain file that nothing rotates — one line per shift/option/fn press
        // fills it with noise that is never the thing you came to read.
        // (A different modifier moving between the taps still breaks the run:
        // that's a chord brewing, not a double-tap.)
        guard isCmdKey else {
            lastPressAt = 0
            lastPressWasRight = nil
            return
        }
        NSLog("MenuBandLauncher: flagsChanged keyCode=\(keyCode) side=\(side) flags=0x\(String(rawFlags, radix: 16))")

        // Down edge: .maskCommand is set on press, cleared on release.
        let isDown = flags.contains(.maskCommand)
        guard isDown else { return }

        // Bare ⌘ only. Reject chords so they can't pair into a
        // future double-tap candidate.
        let chordMask: CGEventFlags = [
            .maskShift, .maskAlternate, .maskControl,
            .maskAlphaShift, .maskSecondaryFn
        ]
        if !flags.intersection(chordMask).isEmpty {
            lastPressAt = 0
            lastPressWasRight = nil
            return
        }

        // Device bits are authoritative because some keyboards report keycode
        // 55 for both physical sides. The pair below must repeat this side.
        let isRight: Bool
        if (rawFlags & Self.nxDeviceRCmd) != 0 {
            isRight = true
        } else if (rawFlags & Self.nxDeviceLCmd) != 0 {
            isRight = false
        } else {
            isRight = (keyCode == Self.rightCommandKeyCode)
        }
        let now = CACurrentMediaTime()
        if lastPressWasRight == isRight && now - lastPressAt <= Self.doubleTapWindow {
            lastPressAt = 0
            lastPressWasRight = nil
            let running = isMenuBandRunning()
            NSLog("MenuBandLauncher: double-tap \(isRight ? "right" : "left")-⌘ detected; menuband running=\(running)")
            if !running {
                launchMenuBand()
            }
        } else {
            lastPressAt = now
            lastPressWasRight = isRight
        }
    }

    private func isMenuBandRunning() -> Bool {
        // Filter on the executable file name — the launcher and the
        // main binary share a bundle identifier in NSWorkspace's view
        // even though they're signed with distinct code-sign
        // identifiers, so we can't trust the bundle ID match alone.
        let myPid = ProcessInfo.processInfo.processIdentifier
        return NSWorkspace.shared.runningApplications.contains { app in
            guard app.processIdentifier != myPid,
                  let url = app.executableURL else { return false }
            return url.lastPathComponent == "MenuBand"
        }
    }

    /// Locate the installed Menu Band. Hardcoding one path used to be enough,
    /// back when the SwiftPM build at `~/Applications/Menu Band.app` was the
    /// only shape that existed — but the Xcode fork installs as
    /// `MenuBand.app` (no space) under `/Applications`, and a Mac App Store
    /// copy lands beside it as `MenuBand 2.app`. Search, in order:
    ///   1. `MENUBAND_APP` from the environment (the launch agent can pin one).
    ///   2. The app bundle we're nested inside, if we're the embedded helper.
    ///   3. Known install locations, dev build first.
    ///   4. LaunchServices, for anything installed somewhere unexpected.
    private func resolveMenuBandBundle() -> URL? {
        var candidates: [URL] = []

        if let pinned = ProcessInfo.processInfo.environment["MENUBAND_APP"],
           !pinned.isEmpty {
            candidates.append(
                URL(fileURLWithPath: NSString(string: pinned).expandingTildeInPath))
        }

        // Bundle.main is the launcher's own bundle. When embedded, that IS
        // Menu Band.app, so this resolves the sibling with zero guessing.
        let own = Bundle.main.bundleURL
        if own.pathExtension == "app" { candidates.append(own) }

        let home = NSHomeDirectory()

        // The dev bundle mid-install (install.sh atomically swaps it) can
        // fail the executable check for a beat — and falling through would
        // summon the App Store fork BESIDE the dev install, which then
        // blocks the dev app as a "duplicate instance". If the dev path
        // exists at all it IS the choice: wait out the swap instead.
        let devPath = "\(home)/Applications/Menu Band.app"
        if FileManager.default.fileExists(atPath: devPath) {
            for _ in 0..<20 {   // up to ~2 s
                if FileManager.default.isExecutableFile(
                    atPath: devPath + "/Contents/MacOS/MenuBand") {
                    return URL(fileURLWithPath: devPath)
                }
                usleep(100_000)
            }
        }

        candidates.append(contentsOf: [
            "\(home)/Applications/Menu Band.app",
            "/Applications/MenuBand.app",
            "/Applications/MenuBand 2.app",
            "\(home)/Applications/MenuBand.app",
        ].map { URL(fileURLWithPath: $0) })

        for candidate in candidates
        where FileManager.default.isExecutableFile(
            atPath: candidate.path + "/Contents/MacOS/MenuBand") {
            return candidate
        }

        for id in Self.menuBandBundleIDs {
            if let url = NSWorkspace.shared
                .urlForApplication(withBundleIdentifier: id) {
                return url
            }
        }
        return nil
    }

    private func launchMenuBand() {
        guard let bundle = resolveMenuBandBundle() else {
            NSLog("MenuBandLauncher: no Menu Band install found — checked MENUBAND_APP, the containing bundle, ~/Applications, /Applications, and LaunchServices")
            return
        }

        // --focus-on-launch tells AppDelegate to open the popover
        // and arm focus capture immediately after init, so a
        // single ⌘⌘ relaunches AND lands in the same focused state
        // the in-process double-tap handler produces.
        let args = ["--focus-on-launch"]

        // When embedded, NSWorkspace.openApplication(at:) gets tricked into
        // returning the launcher's OWN NSRunningApplication — LaunchServices
        // treats both binaries inside the bundle as "the app from this bundle
        // is already running". Spawn the executable directly to sidestep that.
        // Standalone, there's no such confusion, and openApplication is the
        // better citizen: it registers with LaunchServices and sets up a
        // sandboxed app's container properly (the App Store fork is
        // sandboxed). detached: stdio points at /dev/null in the Process case
        // so MenuBand isn't tied to the launcher's lifetime.
        let embedded = bundle == Bundle.main.bundleURL
        if !embedded {
            let config = NSWorkspace.OpenConfiguration()
            config.arguments = args
            config.activates = true
            NSLog("MenuBandLauncher: opening \(bundle.path)")
            NSWorkspace.shared.openApplication(at: bundle, configuration: config) { app, error in
                if let error {
                    NSLog("MenuBandLauncher: openApplication failed — \(error); falling back to direct spawn")
                    self.spawnMenuBand(in: bundle, args: args)
                } else {
                    NSLog("MenuBandLauncher: opened MenuBand pid=\(app?.processIdentifier ?? -1)")
                }
            }
            return
        }
        spawnMenuBand(in: bundle, args: args)
    }

    private func spawnMenuBand(in bundle: URL, args: [String]) {
        let exePath = bundle.path + "/Contents/MacOS/MenuBand"
        NSLog("MenuBandLauncher: launching \(exePath)")
        let task = Process()
        task.executableURL = URL(fileURLWithPath: exePath)
        task.arguments = args
        task.standardInput = FileHandle.nullDevice
        task.standardOutput = FileHandle.nullDevice
        task.standardError = FileHandle.nullDevice
        do {
            try task.run()
            NSLog("MenuBandLauncher: spawned MenuBand pid=\(task.processIdentifier)")
        } catch {
            NSLog("MenuBandLauncher: spawn failed — \(error)")
        }
    }
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)  // background helper; allow event delivery

let launcher = Launcher()
if !launcher.start() {
    // tapCreate failed. Exit non-zero so launchd's ThrottleInterval
    // gates re-launch attempts at 5s rather than tight-looping.
    exit(2)
}

app.run()
