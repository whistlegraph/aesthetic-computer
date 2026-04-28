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
//   2. The "[● SNAP]" pill in the menu bar. Left-click taps the
//      iPhone immediately. The sliders icon to its right opens the
//      menu (right-click anywhere on the item also opens the menu).
//
// Visual states:
//   • idle      — red dot + "SNAP" inside a soft outlined button
//   • hover     — same shape, brighter; settings icon brightens too
//   • not ready — X overlay on the snap button (iPhone Mirroring closed)
//   • snapping  — 1-second inactive flash while a tap is in flight
//
// Sounds (Apple's bundled GM DLS drum kit via AVAudioUnitSampler):
//   • snap-button hover → closed hi-hat
//   • mouse down       → hi wood block
//   • mouse up (snap end) → lo wood block
// Settings icon hover/clicks are silent.

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

// MARK: - Icon renderer
//
// Layout (left to right):
//   [outerPad][ snap button rect ][gap][settings chip][outerPad]

enum IconPhase: Equatable { case idle, snapping }
enum HoverRegion: Equatable { case none, snap, settings }

enum IconRenderer {
  static let height: CGFloat = 22       // matches MenuBand image height

  // Snap button geometry — button rect = 21pt tall, matching MenuBand
  // white-key height (whiteH = 21).
  static let snapPadX: CGFloat = 6
  static let snapPadY: CGFloat = 0.5
  static let dotSize:  CGFloat = 11
  static let ringPad:  CGFloat = 2
  static let dotLabelGap: CGFloat = 5
  static let labelText = "SNAP"
  static let labelFont = NSFont.systemFont(ofSize: 11.5, weight: .semibold)
  static let buttonCornerRadius: CGFloat = 4.5

  // Settings chip (sliders icon)
  static let settingsSymbol = "slider.horizontal.3"
  static let settingsW: CGFloat = 18
  static let settingsH: CGFloat = 21
  static let settingsPointSize: CGFloat = 11

  // Layout padding
  static let outerPad: CGFloat = 2
  static let sectionGap: CGFloat = 4

  // ---- derived sizes -----------------------------------------------------

  static var labelSize: NSSize {
    NSAttributedString(string: labelText, attributes: [.font: labelFont]).size()
  }

  static var snapRect: NSRect {
    let innerW = snapPadX + (dotSize + ringPad * 2)
                  + dotLabelGap + ceil(labelSize.width) + snapPadX
    return NSRect(x: outerPad,
                  y: snapPadY,
                  width: innerW,
                  height: height - snapPadY * 2)
  }

  static var settingsRect: NSRect {
    NSRect(x: snapRect.maxX + sectionGap,
           y: (height - settingsH) / 2,
           width: settingsW,
           height: settingsH)
  }

  static var imageSize: NSSize {
    NSSize(width: ceil(settingsRect.maxX + outerPad), height: height)
  }

  static func region(at point: NSPoint) -> HoverRegion {
    if snapRect.contains(point) { return .snap }
    if settingsRect.contains(point) { return .settings }
    return .none
  }

  // ---- rendering ---------------------------------------------------------

  static func image(ready: Bool, hovered: HoverRegion, phase: IconPhase)
    -> NSImage
  {
    let size = imageSize
    let img = NSImage(size: size)
    img.lockFocus()
    defer { img.unlockFocus() }

    drawSnapButton(ready: ready,
                   hovered: hovered == .snap,
                   phase: phase)
    drawSettingsChip(hovered: hovered == .settings)

    return img
  }

  private static func drawSnapButton(ready: Bool, hovered: Bool,
                                     phase: IconPhase)
  {
    let rect = snapRect

    // Button outline — labelColor with alpha based on state.
    let strokeAlpha: CGFloat = {
      if phase == .snapping { return 0.85 }
      return hovered ? 0.85 : 0.55
    }()
    NSColor.white.withAlphaComponent(strokeAlpha).setStroke()
    let outline = NSBezierPath(
      roundedRect: rect.insetBy(dx: 0.5, dy: 0.5),
      xRadius: buttonCornerRadius, yRadius: buttonCornerRadius)
    outline.lineWidth = 1.0
    outline.stroke()

    // Inner dot — always red, alpha by state.
    let dotY = rect.midY - dotSize / 2
    let dotX = rect.minX + snapPadX + ringPad
    let dotRect  = NSRect(x: dotX, y: dotY, width: dotSize, height: dotSize)
    let ringRect = dotRect.insetBy(dx: -ringPad, dy: -ringPad)

    if phase == .snapping {
      // Hollow look — small inner ring suggests "in flight."
      NSColor.white.withAlphaComponent(0.55).setStroke()
      let inner = NSBezierPath(ovalIn: dotRect.insetBy(dx: 1.5, dy: 1.5))
      inner.lineWidth = 1.0
      inner.stroke()
    } else {
      let baseRed = hovered
        ? NSColor(calibratedRed: 1.00, green: 0.30, blue: 0.30, alpha: 1.0)
        : NSColor(calibratedRed: 0.92, green: 0.22, blue: 0.22, alpha: 1.0)
      let dotColor = ready
        ? baseRed
        : baseRed.withAlphaComponent(hovered ? 0.55 : 0.40)
      dotColor.setFill()
      NSBezierPath(ovalIn: dotRect).fill()
    }

    // X overlay when not ready (skipped during snap), drawn over the
    // ring inset so the red dot stays visible.
    if !ready && phase != .snapping {
      NSColor.white.withAlphaComponent(0.95).setStroke()
      let xRect = ringRect.insetBy(dx: 0.5, dy: 0.5)
      let x = NSBezierPath()
      x.move(to: NSPoint(x: xRect.minX, y: xRect.minY))
      x.line(to: NSPoint(x: xRect.maxX, y: xRect.maxY))
      x.move(to: NSPoint(x: xRect.minX, y: xRect.maxY))
      x.line(to: NSPoint(x: xRect.maxX, y: xRect.minY))
      x.lineWidth = 1.4
      x.lineCapStyle = .round
      x.stroke()
    }

    // "SNAP" label
    let textAlpha: CGFloat = {
      if phase == .snapping { return 0.55 }
      if !ready { return hovered ? 0.70 : 0.45 }
      return hovered ? 1.00 : 0.78
    }()
    let textColor = NSColor.white.withAlphaComponent(textAlpha)
    let textX = dotRect.maxX + ringPad + dotLabelGap
    let textY = rect.midY - labelSize.height / 2 - 0.5
    labelText.draw(at: NSPoint(x: textX, y: textY),
                   withAttributes: [
                     .font: labelFont,
                     .foregroundColor: textColor,
                   ])
  }

  private static func drawSettingsChip(hovered: Bool) {
    let alpha: CGFloat = hovered ? 1.0 : 0.65
    let color = NSColor.white.withAlphaComponent(alpha)
    drawTintedSymbol(settingsSymbol, in: settingsRect,
                     pointSize: settingsPointSize, color: color)
  }

  private static func drawTintedSymbol(_ name: String, in rect: NSRect,
                                       pointSize: CGFloat, color: NSColor)
  {
    guard let base = NSImage(systemSymbolName: name,
                              accessibilityDescription: nil) else { return }
    let cfg = NSImage.SymbolConfiguration(pointSize: pointSize, weight: .semibold)
    let configured = base.withSymbolConfiguration(cfg) ?? base
    let s = configured.size

    // Tint: fill rect with color, then mask to symbol's alpha.
    let tinted = NSImage(size: s)
    tinted.lockFocus()
    color.setFill()
    NSRect(origin: .zero, size: s).fill()
    configured.draw(in: NSRect(origin: .zero, size: s),
                    from: .zero, operation: .destinationIn, fraction: 1)
    tinted.unlockFocus()

    let drawRect = NSRect(
      x: rect.midX - s.width  / 2,
      y: rect.midY - s.height / 2,
      width: s.width, height: s.height)
    tinted.draw(in: drawRect, from: .zero,
                operation: .sourceOver, fraction: 1)
  }
}

// MARK: - NoHighlightStatusBarCell — kills the system click/hover pill.

final class NoHighlightStatusBarCell: NSButtonCell {
  override var isHighlighted: Bool {
    get { false }
    set { /* swallow */ }
  }
  override func drawBezel(withFrame frame: NSRect, in controlView: NSView) {}
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
        bankMSB: 0x78, bankLSB: 0)
    }
    ready = true
    primeForLowLatency()
  }

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
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.10) { [weak self] in
      self?.drums.stopNote(midi, onChannel: 0)
    }
  }

  func hover() { hit(42, velocity: 28) }
  func down()  { hit(76, velocity: 95) }
  func up()    { hit(77, velocity: 70) }
}

// MARK: - Global hotkey (Carbon RegisterEventHotKey)

final class GlobalHotkey {
  private var hotKeyRef: EventHotKeyRef?
  private var eventHandler: EventHandlerRef?
  private let onTrigger: () -> Void

  init(onTrigger: @escaping () -> Void) { self.onTrigger = onTrigger }

  @discardableResult
  func register(keyCode: UInt32, modifiers: UInt32) -> Bool {
    unregister()
    let hotKeyID = EventHotKeyID(signature: OSType(0x59455247),  // 'YERG'
                                 id: 1)
    var ref: EventHotKeyRef?
    let regOK = RegisterEventHotKey(keyCode, modifiers, hotKeyID,
                                    GetApplicationEventTarget(), 0, &ref)
    guard regOK == noErr, let ref = ref else {
      NSLog("YergerSnap hotkey registration failed: \(regOK)")
      return false
    }
    hotKeyRef = ref

    var spec = EventTypeSpec(eventClass: OSType(kEventClassKeyboard),
                             eventKind: UInt32(kEventHotKeyPressed))
    let opaque = Unmanaged.passUnretained(self).toOpaque()
    let handler: EventHandlerUPP = { _, _, userData -> OSStatus in
      guard let userData = userData else { return noErr }
      let hk = Unmanaged<GlobalHotkey>.fromOpaque(userData).takeUnretainedValue()
      DispatchQueue.main.async { hk.onTrigger() }
      return noErr
    }
    let installOK = InstallEventHandler(GetApplicationEventTarget(), handler,
                                        1, &spec, opaque, &eventHandler)
    if installOK != noErr {
      NSLog("YergerSnap event handler install failed: \(installOK)")
      unregister()
      return false
    }
    return true
  }

  func unregister() {
    if let ref = hotKeyRef { UnregisterEventHotKey(ref) }
    hotKeyRef = nil
    if let h = eventHandler { RemoveEventHandler(h) }
    eventHandler = nil
  }

  deinit { unregister() }
}

// MARK: - Hover responder

final class HoverResponder: NSResponder {
  var onMove: ((NSEvent) -> Void)?
  var onExit: (() -> Void)?
  override func mouseEntered(with event: NSEvent) { onMove?(event) }
  override func mouseMoved(with event: NSEvent)   { onMove?(event) }
  override func mouseExited(with event: NSEvent)  { onExit?()  }
}

// MARK: - AppDelegate

class AppDelegate: NSObject, NSApplicationDelegate {
  var statusItem: NSStatusItem!
  var menu: NSMenu!
  let drums = DrumHits()
  let hover = HoverResponder()
  var readyTimer: Timer?
  var hotkey: GlobalHotkey?

  var hoveredRegion: HoverRegion = .none
  var isReady    = false
  var isSnapping = false

  func applicationDidFinishLaunching(_ note: Notification) {
    let promptKey = kAXTrustedCheckOptionPrompt.takeUnretainedValue() as String
    _ = AXIsProcessTrustedWithOptions([promptKey: true] as CFDictionary)

    let size = IconRenderer.imageSize
    statusItem = NSStatusBar.system.statusItem(withLength: size.width)
    if let btn = statusItem.button {
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
      btn.toolTip = "YergerSnap"

      hover.onMove = { [weak self] event in self?.handleHover(event) }
      hover.onExit = { [weak self] in self?.handleHoverExit() }
      let area = NSTrackingArea(
        rect: btn.bounds,
        options: [.mouseEnteredAndExited, .mouseMoved,
                  .activeAlways, .inVisibleRect],
        owner: hover, userInfo: nil)
      btn.addTrackingArea(area)
    }

    buildMenu()

    isReady = checkReady()
    redraw()
    readyTimer = Timer.scheduledTimer(
      withTimeInterval: readyPollSeconds, repeats: true
    ) { [weak self] _ in self?.pollReady() }

    // Global hotkey: ⌃⌥⌘S — fires a snap from anywhere on the system.
    let hk = GlobalHotkey { [weak self] in
      guard let self = self else { return }
      if !self.isSnapping { self.startSnap(playSounds: true) }
    }
    let mods = UInt32(controlKey | optionKey | cmdKey)
    hk.register(keyCode: UInt32(kVK_ANSI_S), modifiers: mods)
    hotkey = hk
  }

  func buildMenu() {
    menu = NSMenu()
    let header = NSMenuItem(title: "YergerSnap", action: nil, keyEquivalent: "")
    header.isEnabled = false
    menu.addItem(header)
    menu.addItem(NSMenuItem.separator())
    let tapItem = NSMenuItem(title: "Tap iPhone Now",
                             action: #selector(tapFromMenu),
                             keyEquivalent: "s")
    tapItem.target = self
    tapItem.keyEquivalentModifierMask = [.control, .option, .command]
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

  func handleHover(_ event: NSEvent) {
    guard let btn = statusItem.button else { return }
    let local = btn.convert(event.locationInWindow, from: nil)
    // The cell centers the drawn image inside the button; its bounds may
    // be slightly larger. Convert to image-local coords.
    let pt = imagePoint(from: local, in: btn)
    let region = IconRenderer.region(at: pt)
    if hoveredRegion != region {
      let was = hoveredRegion
      hoveredRegion = region
      if region == .snap && was != .snap && !isSnapping { drums.hover() }
      if !isSnapping { redraw() }
    }
  }

  func handleHoverExit() {
    if hoveredRegion != .none {
      hoveredRegion = .none
      if !isSnapping { redraw() }
    }
  }

  /// Translate a button-local point into the image's coordinate system,
  /// since NoHighlightStatusBarCell centers the image in the cell.
  func imagePoint(from buttonLocal: NSPoint, in button: NSView) -> NSPoint {
    let imgSize = IconRenderer.imageSize
    let bb = button.bounds
    let xOff = (bb.width  - imgSize.width)  / 2.0
    let yOff = (bb.height - imgSize.height) / 2.0
    let yLocal = button.isFlipped ? (bb.height - buttonLocal.y) : buttonLocal.y
    return NSPoint(x: buttonLocal.x - xOff, y: yLocal - yOff)
  }

  func redraw() {
    statusItem.button?.image = IconRenderer.image(
      ready: isReady,
      hovered: hoveredRegion,
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

    // Hit-test by where the click landed inside the button.
    if let event = event, let btn = statusItem.button {
      let local = btn.convert(event.locationInWindow, from: nil)
      let pt = imagePoint(from: local, in: btn)
      let region = IconRenderer.region(at: pt)
      if region == .settings { showMenu(); return }
      // .none happens between regions — just ignore.
      if region == .none { return }
    }

    if isSnapping { return }
    startSnap(playSounds: true)
  }

  func showMenu() {
    guard let btn = statusItem.button else { return }
    // Pop the menu directly. Setting `statusItem.menu` and calling
    // `performClick(nil)` would re-fire our action handler and recurse
    // until the stack overflows.
    let origin = NSPoint(x: 0, y: btn.bounds.height + 2)
    menu.popUp(positioning: nil, at: origin, in: btn)
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
