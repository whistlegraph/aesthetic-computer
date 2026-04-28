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
//   2. The "SNAP" menu-bar button (manual). Left-click taps the
//      iPhone immediately; right-click opens the menu.
//
// Visual states the menu-bar pill shows:
//   • idle      — dim red dot + dim "SNAP"  (hovering brightens both)
//   • not ready — gray dot with X overlay   (iPhone Mirroring not running)
//   • snapping  — 1-second inactive flash while a tap is in flight
//
// First launch on a clean Mac: macOS prompts for Accessibility (TCC).
// Needed both for AX queries on iPhone Mirroring's window and for
// posting the synthetic mouse click.

import Cocoa
import ApplicationServices
import Carbon.HIToolbox
import AVFoundation

// ---- config --------------------------------------------------------------
let tapX: CGFloat = 0.50
let tapY: CGFloat = 0.90
let snapHoldSeconds: TimeInterval = 1.0     // how long the inactive look stays
let readyPollSeconds: TimeInterval = 1.5    // iPhone Mirroring presence poll
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

enum IconPhase: Equatable { case idle, snapping }

enum IconRenderer {
  static let height: CGFloat = 22
  static let leftPad: CGFloat = 5
  static let dotSize: CGFloat = 10
  static let ringPad: CGFloat = 2
  static let gap: CGFloat = 4
  static let rightPad: CGFloat = 6
  static let labelText = "SNAP"
  static let font = NSFont.systemFont(ofSize: 11.0, weight: .semibold)

  static var labelSize: NSSize {
    NSAttributedString(string: labelText, attributes: [.font: font]).size()
  }

  static var imageSize: NSSize {
    let w = leftPad + (dotSize + ringPad * 2) + gap + ceil(labelSize.width) + rightPad
    return NSSize(width: ceil(w), height: height)
  }

  static func image(ready: Bool, hover: Bool, phase: IconPhase) -> NSImage {
    let size = imageSize
    let img = NSImage(size: size)
    img.lockFocus()
    defer { img.unlockFocus() }

    // Snapping pill — subtle backdrop signalling "I'm working, ignore clicks"
    if phase == .snapping {
      NSColor.white.withAlphaComponent(0.18).setFill()
      let pill = NSRect(x: 1, y: 1, width: size.width - 2, height: size.height - 2)
      NSBezierPath(roundedRect: pill, xRadius: 4.5, yRadius: 4.5).fill()
    }

    // Dot + ring geometry
    let dotY = (size.height - dotSize) / 2
    let dotX = leftPad + ringPad
    let dotRect  = NSRect(x: dotX, y: dotY, width: dotSize, height: dotSize)
    let ringRect = dotRect.insetBy(dx: -ringPad, dy: -ringPad)

    // Outer ring
    let ringAlpha: CGFloat = phase == .snapping
      ? 1.0
      : (ready ? (hover ? 0.95 : 0.80) : 0.45)
    NSColor.white.withAlphaComponent(ringAlpha).setStroke()
    let ring = NSBezierPath(ovalIn: ringRect)
    ring.lineWidth = 1.3
    ring.stroke()

    // Inner dot — red when ready (brightens on hover), gray when not, white during snap.
    let dotColor: NSColor = {
      if phase == .snapping { return NSColor.white }
      if !ready { return NSColor(white: 0.50, alpha: 1) }
      return hover
        ? NSColor(calibratedRed: 1.00, green: 0.30, blue: 0.30, alpha: 1)
        : NSColor(calibratedRed: 0.78, green: 0.18, blue: 0.18, alpha: 1)
    }()
    dotColor.setFill()
    NSBezierPath(ovalIn: dotRect).fill()

    // X overlay when iPhone Mirroring isn't ready (doesn't show during snap).
    if !ready && phase != .snapping {
      NSColor.white.withAlphaComponent(0.95).setStroke()
      let inset: CGFloat = 2.5
      let x = NSBezierPath()
      x.move(to: NSPoint(x: dotRect.minX + inset, y: dotRect.minY + inset))
      x.line(to: NSPoint(x: dotRect.maxX - inset, y: dotRect.maxY - inset))
      x.move(to: NSPoint(x: dotRect.minX + inset, y: dotRect.maxY - inset))
      x.line(to: NSPoint(x: dotRect.maxX - inset, y: dotRect.minY + inset))
      x.lineWidth = 1.5
      x.lineCapStyle = .round
      x.stroke()
    }

    // Label — dim by default, brighter on hover, dimmest while snapping.
    let textAlpha: CGFloat = {
      if phase == .snapping { return 0.55 }
      if !ready { return hover ? 0.75 : 0.50 }
      return hover ? 1.00 : 0.78
    }()
    let textColor = NSColor.white.withAlphaComponent(textAlpha)
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
  var readyTimer: Timer?

  var isHovered  = false
  var isReady    = false
  var isSnapping = false

  func applicationDidFinishLaunching(_ note: Notification) {
    let promptKey = kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String
    _ = AXIsProcessTrustedWithOptions([promptKey: true] as CFDictionary)

    let size = IconRenderer.imageSize
    statusItem = NSStatusBar.system.statusItem(withLength: size.width)
    if let btn = statusItem.button {
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

    isReady = checkReady()
    redraw()
    readyTimer = Timer.scheduledTimer(
      withTimeInterval: readyPollSeconds, repeats: true
    ) { [weak self] _ in self?.pollReady() }
  }

  // MARK: - State

  func checkReady() -> Bool {
    runningApp(named: "iPhone Mirroring") != nil
  }

  func pollReady() {
    let r = checkReady()
    if r != isReady {
      isReady = r
      if !isSnapping { redraw() }
    }
  }

  func setHovered(_ h: Bool) {
    if isHovered == h { return }
    isHovered = h
    if !isSnapping { redraw() }
  }

  func redraw() {
    statusItem.button?.image = IconRenderer.image(
      ready: isReady,
      hover: isHovered,
      phase: isSnapping ? .snapping : .idle)
  }

  func startSnap(playSound: Bool) {
    isSnapping = true
    redraw()
    if playSound { sound.play() }

    DispatchQueue.global(qos: .userInitiated).async { tapPhone() }

    DispatchQueue.main.asyncAfter(deadline: .now() + snapHoldSeconds) {
      [weak self] in
      guard let self else { return }
      self.isSnapping = false
      self.isReady = self.checkReady()
      self.redraw()
    }
  }

  // MARK: - Triggers

  // URL scheme — Dragonframe action script invokes this via
  // `open yergersnap://capture`.
  func application(_ app: NSApplication, open urls: [URL]) {
    for url in urls where url.scheme == "yergersnap" {
      let host = url.host?.lowercased()
        ?? url.path.replacingOccurrences(of: "/", with: "")
      switch host {
      case "capture", "tap":
        if !isSnapping { startSnap(playSound: false) }
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
    if isSnapping { return }
    startSnap(playSound: true)
  }

  func showMenu() {
    statusItem.menu = menu
    statusItem.button?.performClick(nil)
    DispatchQueue.main.async { self.statusItem.menu = nil }
  }

  @objc func tapFromMenu() {
    if !isSnapping { startSnap(playSound: true) }
  }

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
