// tap.swift — YergerSnap menubar app.
//
// Posts a synthetic click on the iPhone Mirroring window's Capture button
// in response to two triggers:
//
//   1. Dragonframe Action Script (preferred). DF runs a shell script on
//      every event (Preferences → Advanced → Action Script). On SHOOT
//      events the script calls `open yergersnap://capture`, which routes
//      to this running app via the registered URL scheme. Net result:
//      pressing Enter in Dragonframe captures BOTH the DF camera and the
//      mirrored iPhone, with no fake keystrokes.
//
//   2. The menu-bar button (manual / fallback). Left-click taps the
//      iPhone immediately. Right-click opens the menu.
//
// First launch on a clean Mac: macOS prompts for Accessibility (TCC).
// That's needed both for AX queries on iPhone Mirroring's window and for
// posting the synthetic mouse click. Grant it once.
//
// Calibration: tapX/tapY are window-relative fractions for where to
// click inside the iPhone Mirroring window. Default = bottom center.
// Tweak and rebuild via ./build.sh.

import Cocoa
import ApplicationServices
import Carbon.HIToolbox

// ---- config --------------------------------------------------------------
let tapX: CGFloat = 0.50
let tapY: CGFloat = 0.90
// --------------------------------------------------------------------------

func runningApp(named name: String) -> NSRunningApplication? {
  NSWorkspace.shared.runningApplications.first { $0.localizedName == name }
}

func iPhoneMirroringFrame() -> CGRect? {
  guard let pid = runningApp(named: "iPhone Mirroring")?.processIdentifier
  else { return nil }
  let app = AXUIElementCreateApplication(pid)
  var windowsRef: CFTypeRef?
  guard AXUIElementCopyAttributeValue(app, kAXWindowsAttribute as CFString,
                                      &windowsRef) == .success,
        let windows = windowsRef as? [AXUIElement],
        let win = windows.first else { return nil }
  var posRef: CFTypeRef?
  var sizeRef: CFTypeRef?
  guard AXUIElementCopyAttributeValue(win, kAXPositionAttribute as CFString,
                                      &posRef) == .success,
        AXUIElementCopyAttributeValue(win, kAXSizeAttribute as CFString,
                                      &sizeRef) == .success
  else { return nil }
  var pos = CGPoint.zero, size = CGSize.zero
  AXValueGetValue(posRef as! AXValue, .cgPoint, &pos)
  AXValueGetValue(sizeRef as! AXValue, .cgSize, &size)
  return CGRect(origin: pos, size: size)
}

func clickAt(_ point: CGPoint) {
  let saved = CGEvent(source: nil)?.location
  let down = CGEvent(mouseEventSource: nil, mouseType: .leftMouseDown,
                     mouseCursorPosition: point, mouseButton: .left)
  let up   = CGEvent(mouseEventSource: nil, mouseType: .leftMouseUp,
                     mouseCursorPosition: point, mouseButton: .left)
  down?.post(tap: .cghidEventTap)
  up?.post(tap: .cghidEventTap)
  if let saved = saved { CGWarpMouseCursorPosition(saved) }
}

func tapPhone() {
  guard let frame = iPhoneMirroringFrame() else {
    NSLog("YergerSnap: iPhone Mirroring window not found.")
    return
  }
  let target = CGPoint(x: frame.minX + frame.width  * tapX,
                       y: frame.minY + frame.height * tapY)
  let prev = NSWorkspace.shared.frontmostApplication
  runningApp(named: "iPhone Mirroring")?.activate(options: [])
  usleep(80_000)
  clickAt(target)
  usleep(40_000)
  prev?.activate(options: [])
}

// Action script template installed into ~/Library/Application Support
// and pointed at from Dragonframe Preferences → Advanced → Action Script.
// Dragonframe passes 8 positional args; $4 is the event name.
let actionScriptBody = """
#!/usr/bin/env bash
# YergerSnap — Dragonframe action script.
# Configured via Dragonframe Preferences → Advanced → Action Script.
#
# Args from Dragonframe:
#   $1 production   $2 scene    $3 take       $4 event
#   $5 frame        $6 exposure $7 expName    $8 filename
#
# On SHOOT (the moment the user triggers capture) we ping YergerSnap via
# its URL scheme, which makes the running app tap iPhone Mirroring.
case "$4" in
  SHOOT)
    /usr/bin/open "yergersnap://capture" >/dev/null 2>&1
    ;;
esac
exit 0
"""

func actionScriptPath() -> String {
  let support = (NSSearchPathForDirectoriesInDomains(
    .applicationSupportDirectory, .userDomainMask, true).first ?? "")
  return support + "/YergerSnap/dragonframe-action.sh"
}

func installActionScript() -> String? {
  let path = actionScriptPath()
  let dir = (path as NSString).deletingLastPathComponent
  do {
    try FileManager.default.createDirectory(atPath: dir,
      withIntermediateDirectories: true)
    try actionScriptBody.write(toFile: path, atomically: true, encoding: .utf8)
    try FileManager.default.setAttributes(
      [.posixPermissions: NSNumber(value: 0o755)], ofItemAtPath: path)
    return path
  } catch {
    NSLog("YergerSnap: install failed — \(error)")
    return nil
  }
}

class AppDelegate: NSObject, NSApplicationDelegate {
  var statusItem: NSStatusItem!
  var menu: NSMenu!
  var idleImage: NSImage?
  var firingImage: NSImage?

  func applicationDidFinishLaunching(_ note: Notification) {
    let promptKey = kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String
    _ = AXIsProcessTrustedWithOptions([promptKey: true] as CFDictionary)

    idleImage   = NSImage(systemSymbolName: "camera.aperture",
                          accessibilityDescription: "YergerSnap idle")
    firingImage = NSImage(systemSymbolName: "circle.inset.filled",
                          accessibilityDescription: "YergerSnap firing")

    statusItem = NSStatusBar.system.statusItem(
      withLength: NSStatusItem.variableLength)
    if let btn = statusItem.button {
      btn.image = idleImage
      btn.imagePosition = .imageOnly
      btn.target = self
      btn.action = #selector(handleClick(_:))
      btn.sendAction(on: [.leftMouseUp, .rightMouseUp])
      btn.toolTip = "YergerSnap — click to tap iPhone Mirroring"
    }

    menu = NSMenu()
    let header = NSMenuItem(title: "YergerSnap", action: nil, keyEquivalent: "")
    header.isEnabled = false
    menu.addItem(header)
    menu.addItem(NSMenuItem.separator())

    let tapItem = NSMenuItem(title: "Tap iPhone Now",
                             action: #selector(tapFromMenu),
                             keyEquivalent: "")
    tapItem.target = self
    menu.addItem(tapItem)

    menu.addItem(NSMenuItem.separator())
    let installItem = NSMenuItem(title: "Install Dragonframe Hook…",
                                 action: #selector(installAndReveal),
                                 keyEquivalent: "")
    installItem.target = self
    menu.addItem(installItem)

    menu.addItem(NSMenuItem.separator())
    menu.addItem(NSMenuItem(title: "Quit YergerSnap",
                            action: #selector(NSApplication.terminate(_:)),
                            keyEquivalent: "q"))
  }

  // URL scheme handler — Dragonframe's action script calls this via
  // `open yergersnap://capture` to ask us to tap iPhone Mirroring.
  func application(_ app: NSApplication, open urls: [URL]) {
    for url in urls where url.scheme == "yergersnap" {
      let host = url.host?.lowercased() ?? url.path.replacingOccurrences(
        of: "/", with: "")
      switch host {
      case "capture", "tap":
        flashIcon()
        tapPhone()
      default:
        NSLog("YergerSnap: unknown URL \(url)")
      }
    }
  }

  @objc func handleClick(_ sender: Any?) {
    let event = NSApp.currentEvent
    let isRight = event?.type == .rightMouseUp
    let isCtrl  = event?.modifierFlags.contains(.control) ?? false
    if isRight || isCtrl { showMenu(); return }
    flashIcon()
    tapPhone()
  }

  func showMenu() {
    statusItem.menu = menu
    statusItem.button?.performClick(nil)
    DispatchQueue.main.async { self.statusItem.menu = nil }
  }

  func flashIcon() {
    statusItem.button?.image = firingImage
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.18) { [weak self] in
      self?.statusItem.button?.image = self?.idleImage
    }
  }

  @objc func tapFromMenu() { flashIcon(); tapPhone() }

  @objc func installAndReveal() {
    guard let path = installActionScript() else {
      let a = NSAlert()
      a.messageText = "Couldn't write the action script."
      a.informativeText = "Check ~/Library/Application Support permissions."
      a.runModal()
      return
    }
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(path, forType: .string)
    NSWorkspace.shared.activateFileViewerSelecting(
      [URL(fileURLWithPath: path)])

    NSApp.activate(ignoringOtherApps: true)
    let a = NSAlert()
    a.messageText = "Dragonframe hook installed"
    a.informativeText = """
      Path (also copied to clipboard):
      \(path)

      Next: in Dragonframe → Preferences → Advanced → Action Script,
      choose this file. From then on, every SHOOT in Dragonframe
      will also tap iPhone Mirroring.
      """
    a.addButton(withTitle: "OK")
    a.runModal()
  }
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.accessory)
app.run()
