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
//   2. The "SNAP" menu-bar pill (manual). Left-click taps the iPhone
//      immediately; right-click opens the menu.
//
// Visual states the pill shows:
//   • idle      — soft "● SNAP" in system label color
//   • hover     — same shape, brighter
//   • not ready — gray dot with X overlay (iPhone Mirroring closed)
//   • snapping  — 1-second inactive flash while a tap is in flight
//
// Sounds (Apple's bundled GM DLS drum kit via AVAudioUnitSampler):
//   • hover  → closed hi-hat
//   • down   → hi wood block
//   • up     → lo wood block (when snap finishes)
//
// First launch: macOS prompts for Accessibility (TCC). Required for
// AX queries on iPhone Mirroring's window and for posting the
// synthetic mouse click.

import Cocoa
import ApplicationServices
import Carbon.HIToolbox
import AVFoundation

// ---- config --------------------------------------------------------------
let tapX: CGFloat = 0.50
let tapY: CGFloat = 0.90
let snapHoldSeconds: TimeInterval = 1.0
let readyPollSeconds: TimeInterval = 1.5
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

// MARK: - Icon renderer (no red, no native highlight)

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

    // Dot + ring geometry
    let dotY = (size.height - dotSize) / 2
    let dotX = leftPad + ringPad
    let dotRect  = NSRect(x: dotX, y: dotY, width: dotSize, height: dotSize)
    let ringRect = dotRect.insetBy(dx: -ringPad, dy: -ringPad)

    // Outer ring — system label color (adapts to light/dark menu bar).
    let ringAlpha: CGFloat = {
      if phase == .snapping { return 0.85 }
      if !ready { return hover ? 0.65 : 0.40 }
      return hover ? 0.95 : 0.75
    }()
    NSColor.labelColor.withAlphaComponent(ringAlpha).setStroke()
    let ring = NSBezierPath(ovalIn: ringRect)
    ring.lineWidth = 1.3
    ring.stroke()

    // Inner dot
    let dotColor: NSColor = {
      if phase == .snapping {
        // Hollow look during snap — fade the ring fill.
        return NSColor.labelColor.withAlphaComponent(0.0)
      }
      if !ready {
        return NSColor(white: 0.55, alpha: 0.55)
      }
      // Ready: red, brighter on hover.
      return hover
        ? NSColor(calibratedRed: 1.00, green: 0.30, blue: 0.30, alpha: 1.0)
        : NSColor(calibratedRed: 0.85, green: 0.20, blue: 0.20, alpha: 1.0)
    }()
    if phase != .snapping {
      dotColor.setFill()
      NSBezierPath(ovalIn: dotRect).fill()
    } else {
      // During snap, draw a smaller secondary ring inside as "in flight."
      NSColor.labelColor.withAlphaComponent(0.55).setStroke()
      let inner = NSBezierPath(ovalIn: dotRect.insetBy(dx: 1.5, dy: 1.5))
      inner.lineWidth = 1.0
      inner.stroke()
    }

    // X overlay when iPhone Mirroring isn't ready (skipped during snap).
    if !ready && phase != .snapping {
      NSColor.labelColor.withAlphaComponent(0.95).setStroke()
      let inset: CGFloat = 2.4
      let x = NSBezierPath()
      x.move(to: NSPoint(x: dotRect.minX + inset, y: dotRect.minY + inset))
      x.line(to: NSPoint(x: dotRect.maxX - inset, y: dotRect.maxY - inset))
      x.move(to: NSPoint(x: dotRect.minX + inset, y: dotRect.maxY - inset))
      x.line(to: NSPoint(x: dotRect.maxX - inset, y: dotRect.minY + inset))
      x.lineWidth = 1.4
      x.lineCapStyle = .round
      x.stroke()
    }

    // "SNAP" label
    let textAlpha: CGFloat = {
      if phase == .snapping { return 0.55 }
      if !ready { return hover ? 0.70 : 0.45 }
      return hover ? 1.00 : 0.78
    }()
    let textColor = NSColor.labelColor.withAlphaComponent(textAlpha)
    let textX = leftPad + dotSize + ringPad * 2 + gap
    let textY = (size.height - labelSize.height) / 2 - 1
    labelText.draw(at: NSPoint(x: textX, y: textY),
                   withAttributes: [.font: font, .foregroundColor: textColor])

    return img
  }
}

// MARK: - NoHighlightStatusBarCell — kills the system click/hover pill.

// Setting `highlightsBy = []` doesn't suppress it because the pill is
// drawn by the cell's bezel routine, not the regular highlight state.
// Swap in a cell that no-ops the bezel and refuses to enter the
// highlighted state.
final class NoHighlightStatusBarCell: NSButtonCell {
  override var isHighlighted: Bool {
    get { false }
    set { /* swallow */ }
  }
  override func drawBezel(withFrame frame: NSRect, in controlView: NSView) {
    // Intentionally empty — the menubar pill is painted here by default.
  }
  override func highlight(_ flag: Bool, withFrame cellFrame: NSRect,
                          in controlView: NSView) {
    super.highlight(false, withFrame: cellFrame, in: controlView)
  }
  override func draw(withFrame cellFrame: NSRect, in controlView: NSView) {
    guard let image = self.image else { return }
    let imgSize = image.size
    let x = cellFrame.minX + (cellFrame.width  - imgSize.width)  / 2.0
    let y = cellFrame.minY + (cellFrame.height - imgSize.height) / 2.0
    let target = NSRect(x: x, y: y, width: imgSize.width, height: imgSize.height)
    image.draw(in: target, from: .zero, operation: .sourceOver,
               fraction: 1.0, respectFlipped: true, hints: nil)
  }
}

// MARK: - DrumHits — short percussion via Apple's bundled GM DLS bank.

// gs_instruments.dls ships with every macOS install. Channel 0 of the
// AVAudioUnitSampler is loaded with the GM percussion kit (bank MSB 0x78);
// the MIDI note number selects which drum sound plays.
//   42 — closed hi-hat   (hover)
//   76 — hi wood block   (mouse down)
//   77 — lo wood block   (mouse up / snap complete)
final class DrumHits {
  private let engine = AVAudioEngine()
  private let drums  = AVAudioUnitSampler()
  private var ready  = false

  static let bankURL = URL(fileURLWithPath:
    "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls")

  init() {
    engine.attach(drums)
    engine.connect(drums, to: engine.mainMixerNode, format: nil)
    engine.prepare()
    do { try engine.start() }
    catch {
      NSLog("YergerSnap: audio engine start failed — \(error)")
      return
    }
    if FileManager.default.fileExists(atPath: DrumHits.bankURL.path) {
      try? drums.loadSoundBankInstrument(
        at: DrumHits.bankURL, program: 0,
        bankMSB: 0x78,   // GM percussion bank
        bankLSB: 0)
    }
    ready = true
    primeForLowLatency()
  }

  // Velocity-1 warmups so the sampler caches its sample data NOW rather
  // than on the user's first real click.
  private func primeForLowLatency() {
    let warmups: [UInt8] = [42, 75, 76, 77]
    for n in warmups { drums.startNote(n, withVelocity: 1, onChannel: 0) }
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) { [weak self] in
      guard let self else { return }
      for n in warmups { self.drums.stopNote(n, onChannel: 0) }
    }
  }

  func hit(_ midi: UInt8, velocity: UInt8) {
    guard ready else { return }
    drums.startNote(midi, withVelocity: velocity, onChannel: 0)
    // Drum samples are one-shots, but stop the note shortly after so
    // we don't accumulate held-note state in the sampler.
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.10) { [weak self] in
      self?.drums.stopNote(midi, onChannel: 0)
    }
  }

  func hover() { hit(42, velocity: 28) }
  func down()  { hit(76, velocity: 95) }
  func up()    { hit(77, velocity: 70) }
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
  let drums = DrumHits()
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
      // Replace the default cell so macOS stops drawing its click/hover pill.
      let cell = NoHighlightStatusBarCell()
      cell.imagePosition = .imageOnly
      cell.isBordered = false
      cell.highlightsBy = []
      btn.cell = cell
      btn.imagePosition = .imageOnly
      btn.isBordered = false

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
    if h && !isSnapping { drums.hover() }
    if !isSnapping { redraw() }
  }

  func redraw() {
    statusItem.button?.image = IconRenderer.image(
      ready: isReady,
      hover: isHovered,
      phase: isSnapping ? .snapping : .idle)
  }

  func startSnap(playSounds: Bool) {
    isSnapping = true
    redraw()
    if playSounds { drums.down() }

    DispatchQueue.global(qos: .userInitiated).async { tapPhone() }

    DispatchQueue.main.asyncAfter(deadline: .now() + snapHoldSeconds) {
      [weak self] in
      guard let self else { return }
      self.isSnapping = false
      self.isReady = self.checkReady()
      self.redraw()
      if playSounds { self.drums.up() }
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
        if !isSnapping { startSnap(playSounds: false) }
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
    startSnap(playSounds: true)
  }

  func showMenu() {
    statusItem.menu = menu
    statusItem.button?.performClick(nil)
    DispatchQueue.main.async { self.statusItem.menu = nil }
  }

  @objc func tapFromMenu() {
    if !isSnapping { startSnap(playSounds: true) }
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
