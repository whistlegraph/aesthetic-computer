// MenuBandLauncher — tiny always-running helper.
//
// Job: watch for a double-tap of the right-Command key, and if Menu
// Band's main process isn't running, launch it. When Menu Band IS
// running, the launcher no-ops because the main app has its own
// equivalent handler in AppDelegate.startRightCommandTapMonitor —
// firing both would toggle focus capture twice.
//
// CGEventTap variant: an earlier version used
// NSEvent.addGlobalMonitorForEvents, which silently delivered zero
// events after each codesign rebuild even with AXIsProcessTrusted
// returning true. CGEventTap fails LOUDLY when Accessibility isn't
// granted (CGEvent.tapCreate returns nil), and it explicitly notifies
// us when the tap is disabled at runtime, so we can re-enable or
// surface the problem.
//
// Lives at Menu Band.app/Contents/MacOS/MenuBandLauncher and is
// signed with its own identifier
// (computer.aestheticcomputer.menubandlauncher) so TCC tracks it
// separately from the main binary.

import AppKit
import ApplicationServices
import Foundation

final class Launcher {
    private static let menuBandBundleID = "computer.aestheticcomputer.menuband"
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
    private var tap: CFMachPort?
    private var runLoopSource: CFRunLoopSource?

    func start() -> Bool {
        let trusted = AXIsProcessTrusted()
        NSLog("MenuBandLauncher: start, AXIsProcessTrusted=\(trusted)")

        let mask = (1 << CGEventType.flagsChanged.rawValue) |
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
            NSLog("MenuBandLauncher: CGEvent.tapCreate FAILED — Accessibility permission missing or tap denied. Grant Accessibility to MenuBandLauncher in System Settings.")
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
        guard type == .flagsChanged else { return }

        let keyCode = event.getIntegerValueField(.keyboardEventKeycode)
        let flags = event.flags
        let rawFlags = flags.rawValue
        let isCmdKey = (keyCode == Self.leftCommandKeyCode || keyCode == Self.rightCommandKeyCode)
        let side: String
        if (rawFlags & Self.nxDeviceRCmd) != 0 { side = "right" }
        else if (rawFlags & Self.nxDeviceLCmd) != 0 { side = "left" }
        else { side = "?" }
        NSLog("MenuBandLauncher: flagsChanged keyCode=\(keyCode) side=\(side) flags=0x\(String(rawFlags, radix: 16))")

        guard isCmdKey else { return }

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
            return
        }

        let now = CACurrentMediaTime()
        if now - lastPressAt <= Self.doubleTapWindow {
            lastPressAt = 0
            let running = isMenuBandRunning()
            NSLog("MenuBandLauncher: double-tap ⌘ (\(side)) detected; menuband running=\(running)")
            if !running {
                launchMenuBand()
            }
        } else {
            lastPressAt = now
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

    private func launchMenuBand() {
        // NSWorkspace.openApplication(at:) on the bundle URL gets
        // tricked into returning the launcher's own
        // NSRunningApplication, because LaunchServices treats both
        // binaries inside the bundle as "the app from this bundle is
        // already running." Bypass it by spawning the MenuBand
        // executable directly with Process. detached: stdin/stdout/
        // stderr point at /dev/null so MenuBand isn't tied to the
        // launcher's lifetime.
        let bundlePath = NSString(string: "~/Applications/Menu Band.app")
            .expandingTildeInPath
        let exePath = bundlePath + "/Contents/MacOS/MenuBand"
        NSLog("MenuBandLauncher: launching \(exePath)")
        let task = Process()
        task.executableURL = URL(fileURLWithPath: exePath)
        // --focus-on-launch tells AppDelegate to open the popover
        // and arm focus capture immediately after init, so a
        // single ⌘⌘ relaunches AND lands in the same focused state
        // the in-process double-tap handler produces.
        task.arguments = ["--focus-on-launch"]
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
