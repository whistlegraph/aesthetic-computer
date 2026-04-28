// tap.swift — YergerSnap menubar app.
//
// Posts a synthetic click on the iPhone Mirroring window's Capture
// button. Two triggers:
//
//   1. Dragonframe Action Script (preferred). DF runs a shell script
//      on every event (Preferences → Advanced → Action Script). On
//      SHOOT events the script calls `open yergersnap://capture`,
//      which routes here via the registered URL scheme. So pressing
//      Enter in Dragonframe captures BOTH the DF camera and the
//      mirrored iPhone, with no fake keystrokes.
//
//   2. The "snap" menu-bar button (manual). Left-click taps the
//      iPhone immediately; right-click opens the menu.
//
// First launch on a clean Mac: macOS prompts for Accessibility (TCC).
// Needed both for AX queries on iPhone Mirroring's window and for
// posting the synthetic mouse click.
//
// Calibration: tapX/tapY are window-relative fractions for where to
// click inside the iPhone Mirroring window. Default = bottom center.

import Cocoa
import ApplicationServices
import Carbon.HIToolbox
import AVFoundation

// ---- config --------------------------------------------------------------
let tapX: CGFloat = 0.50
let tapY: CGFloat = 0.90
// --------------------------------------------------------------------------

// MARK: - iPhone Mirroring helpers

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

// MARK: - Dragonframe action script install

let actionScriptBody = """
#!/usr/bin/env bash
# YergerSnap — Dragonframe action script.
# Configured via Dragonframe Preferences → Advanced → Action Script.
#
# Args from Dragonframe:
#   $1 production   $2 scene    $3 take       $4 event
#   $5 frame        $6 exposure $7 expName    $8 filename
#
# On SHOOT (the moment the user triggers capture) we ping YergerSnap
# via its URL scheme, which makes the running app tap iPhone Mirroring.
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

// MARK: - Icon renderer

enum IconState { case idle, hover, pressed }

enum IconRenderer {
  static let height: CGFloat = 22
  static let leftPad: CGFloat = 5
  static let dotSize: CGFloat = 10
  static let ringPad: CGFloat = 2
  static let gap: CGFloat = 4
  static let rightPad: CGFloat = 6
  static let labelText = "snap"
  static let font = NSFont.systemFont(ofSize: 11.5, weight: .semibold)

  static var labelSize: NSSize {
    NSAttributedString(string: labelText, attributes: [.font: font]).size()
  }

  static var imageSize: NSSize {
    let w = leftPad + (dotSize + ringPad * 2) + gap + ceil(labelSize.width) + rightPad
    return NSSize(width: ceil(w), height: height)
  }

  static func image(_ state: IconState) -> NSImage {
    let size = imageSize
    let img = NSImage(size: size)
    img.lockFocus()
    defer { img.unlockFocus() }

    // Hover / pressed background pill
    if state != .idle {
      let alpha: CGFloat = state == .pressed ? 0.28 : 0.12
      NSColor.white.withAlphaComponent(alpha).setFill()
      let pill = NSRect(x: 1, y: 1, width: size.width - 2, height: size.height - 2)
      NSBezierPath(roundedRect: pill, xRadius: 4.5, yRadius: 4.5).fill()
    }

    // Dot + ring
    let dotY = (size.height - dotSize) / 2
    let dotX = leftPad + ringPad
    let dotRect = NSRect(x: dotX, y: dotY, width: dotSize, height: dotSize)
    let ringRect = dotRect.insetBy(dx: -ringPad, dy: -ringPad)

    // Outer ring
    let ringColor: NSColor = .white.withAlphaComponent(state == .pressed ? 1.0 : 0.85)
    ringColor.setStroke()
    let ring = NSBezierPath(ovalIn: ringRect)
    ring.lineWidth = 1.3
    ring.stroke()

    // Inner dot — red, brighter on hover, white on press.
    let dotColor: NSColor = {
      switch state {
      case .idle:    return NSColor(calibratedRed: 0.92, green: 0.22, blue: 0.22, alpha: 1)
      case .hover:   return NSColor(calibratedRed: 1.00, green: 0.32, blue: 0.32, alpha: 1)
      case .pressed: return NSColor.white
      }
    }()
    dotColor.setFill()
    NSBezierPath(ovalIn: dotRect).fill()

    // "snap" label
    let textColor: NSColor = state == .pressed
      ? NSColor.white
      : NSColor.white.withAlphaComponent(0.92)
    let textX = leftPad + dotSize + ringPad * 2 + gap
    let textY = (size.height - labelSize.height) / 2 - 1
    labelText.draw(at: NSPoint(x: textX, y: textY),
                   withAttributes: [.font: font, .foregroundColor: textColor])

    return img
  }
}

// MARK: - Click sound — low sine with a tiny front-edge pop.

final class ClickSound {
  private let engine = AVAudioEngine()
  private let player = AVAudioPlayerNode()
  private var buffer: AVAudioPCMBuffer?

  init() {
    engine.attach(player)
    let format = engine.mainMixerNode.outputFormat(forBus: 0)
    engine.connect(player, to: engine.mainMixerNode, format: format)
    buffer = makeBuffer(format: format)
    do { try engine.start() }
    catch { NSLog("YergerSnap: audio engine start failed — \(error)") }
  }

  private func makeBuffer(format: AVAudioFormat) -> AVAudioPCMBuffer? {
    let sr = format.sampleRate
    let durationSec = 0.085
    let frameCount = AVAudioFrameCount(sr * durationSec)
    guard let buf = AVAudioPCMBuffer(pcmFormat: format,
                                     frameCapacity: frameCount)
    else { return nil }
    buf.frameLength = frameCount
    let channels = Int(format.channelCount)
    let f0 = 220.0
    for ch in 0..<channels {
      guard let data = buf.floatChannelData?[ch] else { continue }
      for i in 0..<Int(frameCount) {
        let t = Double(i) / sr
        let env: Double
        if      t < 0.004                { env = t / 0.004 }
        else if t < durationSec - 0.030  { env = 1.0 }
        else                             { env = max(0, (durationSec - t) / 0.030) }
        let body  = sin(2 * .pi * f0 * t)
        let click = (t < 0.0025)
          ? sin(2 * .pi * 2400 * t) * (1.0 - t / 0.0025)
          : 0
        data[i] = Float((body * 0.6 + click * 0.4) * env * 0.20)
      }
    }
    return buf
  }

  func play() {
    guard let buf = buffer else { return }
    if !engine.isRunning { try? engine.start() }
    player.scheduleBuffer(buf, at: nil, options: .interrupts,
                          completionHandler: nil)
    if !player.isPlaying { player.play() }
  }
}

// MARK: - Hover responder (NSResponder shim for tracking-area events)

final class HoverResponder: NSResponder {
  var onEnter: (() -> Void)?
  var onExit:  (() -> Void)?
  override func mouseEntered(with event: NSEvent) { onEnter?() }
  override func mouseExited(with event: NSEvent)  { onExit?()  }
}

// MARK: - AppDelegate

class AppDelegate: NSObject, NSApplicationDelegate {
  var statusItem: NSStatusItem!
  var menu: NSMenu!
  let sound = ClickSound()
  let hover = HoverResponder()
  var isHovered = false

  func applicationDidFinishLaunching(_ note: Notification) {
    let promptKey = kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String
    _ = AXIsProcessTrustedWithOptions([promptKey: true] as CFDictionary)

    let size = IconRenderer.imageSize
    statusItem = NSStatusBar.system.statusItem(withLength: size.width)
    if let btn = statusItem.button {
      btn.image = IconRenderer.image(.idle)
      btn.imagePosition = .imageOnly
      btn.target = self
      btn.action = #selector(handleClick(_:))
      btn.sendAction(on: [.leftMouseDown, .rightMouseDown])
      btn.toolTip = "YergerSnap — click to tap iPhone Mirroring"

      hover.onEnter = { [weak self] in self?.setHovered(true)  }
      hover.onExit  = { [weak self] in self?.setHovered(false) }
      let area = NSTrackingArea(
        rect: btn.bounds,
        options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
        owner: hover, userInfo: nil)
      btn.addTrackingArea(area)
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

  func setHovered(_ hovered: Bool) {
    if isHovered == hovered { return }
    isHovered = hovered
    statusItem.button?.image = IconRenderer.image(hovered ? .hover : .idle)
  }

  // URL scheme handler — Dragonframe action script calls this via
  // `open yergersnap://capture` to request a tap.
  func application(_ app: NSApplication, open urls: [URL]) {
    for url in urls where url.scheme == "yergersnap" {
      let host = url.host?.lowercased()
        ?? url.path.replacingOccurrences(of: "/", with: "")
      switch host {
      case "capture", "tap":
        flashClick(playSound: false)   // remote trigger: silent
        tapPhone()
      default:
        NSLog("YergerSnap: unknown URL \(url)")
      }
    }
  }

  @objc func handleClick(_ sender: Any?) {
    let event = NSApp.currentEvent
    let isRight = event?.type == .rightMouseDown
    let isCtrl  = event?.modifierFlags.contains(.control) ?? false
    if isRight || isCtrl { showMenu(); return }
    flashClick(playSound: true)
    tapPhone()
  }

  func showMenu() {
    statusItem.menu = menu
    statusItem.button?.performClick(nil)
    DispatchQueue.main.async { self.statusItem.menu = nil }
  }

  func flashClick(playSound: Bool) {
    if playSound { sound.play() }
    statusItem.button?.image = IconRenderer.image(.pressed)
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.13) { [weak self] in
      guard let self else { return }
      self.statusItem.button?.image =
        IconRenderer.image(self.isHovered ? .hover : .idle)
    }
  }

  @objc func tapFromMenu()      { flashClick(playSound: true); tapPhone() }
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
    NSWorkspace.shared.activateFileViewerSelecting([URL(fileURLWithPath: path)])

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
