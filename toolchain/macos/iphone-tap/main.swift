// iphone-tap — a tiny macOS CLI for driving the iPhone Mirroring window.
//
// It is the reusable "hands and eyes" primitive underneath higher-level
// iPhone automations (the first of which is Instagram grid archiving). The
// tap + window-frame code is lifted straight from YergerSnap (slab/yergersnap),
// which has driven Julia Yerger's stop-motion rig in production; this version
// adds screen capture (`shot`) and on-device OCR (`ocr`) so an orchestrator
// can SEE the screen, not just tap blindly.
//
// Commands (all print JSON to stdout):
//
//   iphone-tap frame [--window NAME]
//       → {"x":..,"y":..,"w":..,"h":..}  window bounds in screen points
//
//   iphone-tap shot OUT.png [--window NAME]
//       → {"x":..,"y":..,"w":..,"h":..,"path":"OUT.png"}
//       Captures the window's region to a PNG. NOTE: iPhone Mirroring may
//       capture as solid black (Continuity privacy). If so, point --window
//       at a QuickTime "Movie Recording" of the phone instead — the frame
//       maths are identical because both are scaled views of one screen.
//
//   iphone-tap ocr IN.png
//       → {"w":W,"h":H,"lines":[{"text":..,"conf":..,"x":..,"y":..,"w":..,"h":..}]}
//       Coordinates are normalized [0,1] with a TOP-LEFT origin, relative to
//       the image — so they drop straight into `tap FX FY`.
//
//   iphone-tap tap FX FY [--window NAME] [--no-activate]
//       Clicks at fractional (FX,FY) of the window. Restores the cursor and
//       prior frontmost app, exactly like YergerSnap, so it stays invisible.
//
// Default --window is "iPhone Mirroring". Exit code is non-zero on any error,
// with a JSON {"error":"..."} on stdout, so callers can branch cleanly.

import Cocoa
import ApplicationServices
import Vision

// ---- output helpers ------------------------------------------------------

func emit(_ obj: [String: Any]) {
  let data = try! JSONSerialization.data(withJSONObject: obj, options: [.sortedKeys])
  print(String(data: data, encoding: .utf8)!)
}

func fail(_ message: String) -> Never {
  emit(["error": message])
  exit(1)
}

// ---- arg parsing ---------------------------------------------------------

var rawArgs = Array(CommandLine.arguments.dropFirst())

func takeOption(_ name: String) -> String? {
  guard let i = rawArgs.firstIndex(of: name), i + 1 < rawArgs.count else { return nil }
  let value = rawArgs[i + 1]
  rawArgs.removeSubrange(i...(i + 1))
  return value
}

func takeFlag(_ name: String) -> Bool {
  guard let i = rawArgs.firstIndex(of: name) else { return false }
  rawArgs.remove(at: i)
  return true
}

let windowName = takeOption("--window") ?? "iPhone Mirroring"
let noActivate = takeFlag("--no-activate")
// By default a tap leaves the mirror frontmost (so you can watch the run on its
// Space). --restore yanks focus back to the previously-active app after the tap
// (YergerSnap's original "stay invisible" behavior) for unattended use.
let restoreFocus = takeFlag("--restore")
// Swipe flick duration (ms). Short = fast flick (scrolls far); longer = a
// controlled drag. 350ms is a comfortable one-screen scroll.
let durationMs = Int(takeOption("--duration") ?? "") ?? 350

// ---- window resolution ---------------------------------------------------
//
// Everything keys off the LIVE window's CGWindowID, looked up fresh on every
// command from the window-server's full list (.optionAll → includes windows on
// other Spaces and behind other windows). That's what lets the user drag the
// mirror anywhere — even to another desktop — while we keep tracking it:
//   • `shot` captures by window id (screencapture -l), so Space/occlusion don't
//     matter — we always get the window's own live pixels.
//   • `frame`/`tap` re-resolve the bounds each call, so a moved window never
//     desyncs.
// Bounds come back in global screen points, top-left origin — the same space
// CGEvent taps use — so a fraction maps straight to a click point.

struct WinRef { let id: CGWindowID; let pid: pid_t; let frame: CGRect }

func resolveWindow(named name: String) -> WinRef? {
  guard let raw = CGWindowListCopyWindowInfo([.optionAll], kCGNullWindowID)
          as? [[String: Any]] else { return nil }

  struct Cand { let id: CGWindowID; let pid: pid_t; let rect: CGRect; let exactName: Bool }
  var cands: [Cand] = []
  for w in raw {
    let owner = w[kCGWindowOwnerName as String] as? String ?? ""
    let wname = w[kCGWindowName as String] as? String ?? ""
    // Match the app (iPhone Mirroring) OR a window title (QuickTime's
    // "Movie Recording" lives under owner "QuickTime Player").
    guard owner.contains(name) || wname.contains(name) else { continue }
    guard let b = w[kCGWindowBounds as String] as? [String: Any],
          let x = b["X"] as? CGFloat, let y = b["Y"] as? CGFloat,
          let cw = b["Width"] as? CGFloat, let ch = b["Height"] as? CGFloat,
          let id = w[kCGWindowNumber as String] as? Int,
          let pid = w[kCGWindowOwnerPID as String] as? Int else { continue }
    // Drop menubar strips / tiny helper windows — keep real content windows.
    guard cw >= 150, ch >= 300 else { continue }
    cands.append(Cand(id: CGWindowID(id), pid: pid_t(pid),
                      rect: CGRect(x: x, y: y, width: cw, height: ch),
                      exactName: wname == name))
  }
  if cands.isEmpty { return nil }
  // Prefer an exact window-title match (the live mirror's title is exactly
  // "iPhone Mirroring"; the stale onboarding window is "Welcome to iPhone
  // Mirroring"). Otherwise fall back to the largest matching window.
  let pick = cands.sorted { a, b in
    if a.exactName != b.exactName { return a.exactName }
    return a.rect.width * a.rect.height > b.rect.width * b.rect.height
  }.first!
  return WinRef(id: pick.id, pid: pick.pid, frame: pick.rect)
}

func windowFrame(named name: String) -> CGRect? { resolveWindow(named: name)?.frame }

// A bare activate() can return before the mirror is truly frontmost. In that
// state macOS consumes the next click to focus the window instead of sending a
// touch to iOS. Keep activation and input in this process and wait for AppKit's
// active-state confirmation.
func activateForInput(_ app: NSRunningApplication) {
  for _ in 0..<20 {
    app.activate(options: [.activateAllWindows, .activateIgnoringOtherApps])
    if app.isActive { break }
    usleep(100_000)
  }
  usleep(200_000)
}

func clickAt(_ point: CGPoint) {
  let saved = CGEvent(source: nil)?.location
  let down = CGEvent(mouseEventSource: nil, mouseType: .leftMouseDown,
                     mouseCursorPosition: point, mouseButton: .left)
  let up = CGEvent(mouseEventSource: nil, mouseType: .leftMouseUp,
                   mouseCursorPosition: point, mouseButton: .left)
  down?.post(tap: .cghidEventTap)
  up?.post(tap: .cghidEventTap)
  if let saved = saved { CGWarpMouseCursorPosition(saved) }
}

// A touch-drag (mouse down → stepped moves → up). iPhone Mirroring maps this to
// a finger swipe, so it scrolls lists (the profile grid) and advances posts.
// `steps` intermediate moves over `durationMs` make it read as a flick, not a
// teleport, which the gesture recognizer needs to pick up velocity.
func dragFromTo(_ a: CGPoint, _ b: CGPoint, steps: Int, durationMs: Int) {
  let saved = CGEvent(source: nil)?.location
  CGEvent(mouseEventSource: nil, mouseType: .leftMouseDown,
          mouseCursorPosition: a, mouseButton: .left)?.post(tap: .cghidEventTap)
  let n = max(steps, 1)
  let perStep = useconds_t(max(durationMs, 1) * 1000 / n)
  for i in 1...n {
    let t = CGFloat(i) / CGFloat(n)
    let p = CGPoint(x: a.x + (b.x - a.x) * t, y: a.y + (b.y - a.y) * t)
    CGEvent(mouseEventSource: nil, mouseType: .leftMouseDragged,
            mouseCursorPosition: p, mouseButton: .left)?.post(tap: .cghidEventTap)
    usleep(perStep)
  }
  CGEvent(mouseEventSource: nil, mouseType: .leftMouseUp,
          mouseCursorPosition: b, mouseButton: .left)?.post(tap: .cghidEventTap)
  if let saved = saved { CGWarpMouseCursorPosition(saved) }
}

// Scroll-wheel gesture. iPhone Mirroring maps a Mac trackpad/scroll-wheel to
// iOS scrolling (a click-drag does NOT scroll, it just touches). The event
// lands on whatever is under the cursor, so we park the cursor in the window
// first. Negative `ticks` scrolls the content UP (reveals items further down).
func scrollWheel(in win: WinRef, ticks: Int, atFx: Double, atFy: Double) {
  let f = win.frame
  let here = CGPoint(x: f.minX + f.width * CGFloat(atFx), y: f.minY + f.height * CGFloat(atFy))
  let saved = CGEvent(source: nil)?.location
  CGWarpMouseCursorPosition(here)
  usleep(60_000)
  let direction = ticks < 0 ? -1 : 1
  let weights = [2, 5, 12, 24, 34, 32, 26, 18, 10, 5, 2, 1]
  let weightTotal = weights.reduce(0, +)
  let distance = max(abs(ticks) * 40, 40)
  let hidSource = CGEventSource(stateID: .hidSystemState)
  for (index, weight) in weights.enumerated() {
    let delta = direction * max(1, Int(round(Double(distance * weight) / Double(weightTotal))))
    guard let event = CGEvent(scrollWheelEvent2Source: hidSource, units: .pixel, wheelCount: 1,
                              wheel1: Int32(delta), wheel2: 0, wheel3: 0) else { continue }
    event.setIntegerValueField(.scrollWheelEventIsContinuous, value: 1)
    event.setIntegerValueField(.scrollWheelEventPointDeltaAxis1, value: Int64(delta))
    event.setIntegerValueField(.scrollWheelEventScrollPhase,
                               value: index == 0 ? 1 : (index == weights.count - 1 ? 4 : 2))
    event.post(tap: .cghidEventTap)
    usleep(16_000)
  }
  usleep(400_000)
  if let saved = saved { CGWarpMouseCursorPosition(saved) }
}

// ---- commands ------------------------------------------------------------

struct OCRHit {
  let text: String
  let conf: Float
  let x: CGFloat
  let y: CGFloat
  let w: CGFloat
  let h: CGFloat
}

func captureWindow(_ win: WinRef, to outPath: String) {
  let proc = Process()
  proc.executableURL = URL(fileURLWithPath: "/usr/sbin/screencapture")
  proc.arguments = ["-x", "-o", "-l\(win.id)", outPath]
  do { try proc.run(); proc.waitUntilExit() } catch { fail("screencapture failed: \(error)") }
  if proc.terminationStatus != 0 { fail("screencapture exit \(proc.terminationStatus)") }
}

func recognize(_ inPath: String) -> (CGFloat, CGFloat, [OCRHit]) {
  guard let src = CGImageSourceCreateWithURL(URL(fileURLWithPath: inPath) as CFURL, nil),
        let cg = CGImageSourceCreateImageAtIndex(src, 0, nil) else { fail("cannot read image: \(inPath)") }
  let request = VNRecognizeTextRequest()
  request.recognitionLevel = .accurate
  request.usesLanguageCorrection = false
  let handler = VNImageRequestHandler(cgImage: cg, options: [:])
  do { try handler.perform([request]) } catch { fail("ocr failed: \(error)") }
  let hits = (request.results ?? []).compactMap { obs -> OCRHit? in
    guard let top = obs.topCandidates(1).first else { return nil }
    let b = obs.boundingBox
    return OCRHit(text: top.string, conf: top.confidence,
                  x: b.origin.x, y: 1.0 - (b.origin.y + b.height),
                  w: b.width, h: b.height)
  }
  return (CGFloat(cg.width), CGFloat(cg.height), hits)
}

func cmdFrame() {
  guard let f = windowFrame(named: windowName) else { fail("window not found: \(windowName)") }
  emit(["x": f.minX, "y": f.minY, "w": f.width, "h": f.height])
}

func cmdShot(_ outPath: String) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  // Capture by window id (-l), not screen region: grabs THIS window's own live
  // pixels even when it's on another Space or behind other windows, so the user
  // can park the mirror anywhere. -o drops the drop-shadow.
  captureWindow(win, to: outPath)
  let f = win.frame
  emit(["x": f.minX, "y": f.minY, "w": f.width, "h": f.height, "id": Int(win.id), "path": outPath])
}

func cmdOcr(_ inPath: String) {
  let (imgW, imgH, hits) = recognize(inPath)
  let lines: [[String: Any]] = hits.map { hit in
    ["text": hit.text, "conf": hit.conf, "x": hit.x, "y": hit.y,
     "w": hit.w, "h": hit.h]
  }
  emit(["w": imgW, "h": imgH, "lines": lines])
}

func cmdTap(_ fx: Double, _ fy: Double) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  let prev = NSWorkspace.shared.frontmostApplication
  // A synthetic click lands wherever the cursor coordinate is on the ACTIVE
  // Space, so to tap a window the user parked elsewhere we activate its app
  // first (brings its Space/window forward), then re-resolve in case that moved
  // it, then click. --no-activate skips this when the caller keeps it frontmost.
  if !noActivate {
    if let app = NSRunningApplication(processIdentifier: win.pid) {
      activateForInput(app)
    }
  }
  let f = (noActivate ? win : (resolveWindow(named: windowName) ?? win)).frame
  let target = CGPoint(x: f.minX + f.width * CGFloat(fx), y: f.minY + f.height * CGFloat(fy))
  clickAt(target)
  usleep(40_000)
  if !noActivate && restoreFocus { prev?.activate(options: []) }
  emit(["tapped": ["x": target.x, "y": target.y], "fx": fx, "fy": fy, "id": Int(win.id)])
}

// Run a short, timed tap transaction while iPhone Mirroring stays active.
// This matters for modal iOS sheets, which can disappear when separate CLI
// invocations briefly hand focus back to the calling terminal.
func cmdTapSequence(_ steps: [(Double, Double, Int)]) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  let prev = NSWorkspace.shared.frontmostApplication
  guard let app = NSRunningApplication(processIdentifier: win.pid) else {
    fail("application not found for window: \(windowName)")
  }
  activateForInput(app)
  let live = resolveWindow(named: windowName) ?? win
  let saved = CGEvent(source: nil)?.location
  for (fx, fy, waitMs) in steps {
    let target = CGPoint(
      x: live.frame.minX + live.frame.width * CGFloat(fx),
      y: live.frame.minY + live.frame.height * CGFloat(fy)
    )
    clickAt(target)
    usleep(useconds_t(max(waitMs, 0) * 1000))
  }
  if let saved = saved { CGWarpMouseCursorPosition(saved) }
  if restoreFocus { prev?.activate(options: []) }
  emit(["steps": steps.count, "id": Int(live.id)])
}

// Tap one known control, then locate and tap exact OCR labels without letting
// the mirror lose focus between modal-sheet steps. When duplicate labels exist
// (for example the Only you tab and sheet row), the lowest match wins.
func cmdTapTextSequence(_ fx: Double, _ fy: Double, _ waitMs: Int,
                        _ targets: [(String, Int)]) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  guard let app = NSRunningApplication(processIdentifier: win.pid) else {
    fail("application not found for window: \(windowName)")
  }
  activateForInput(app)
  var live = resolveWindow(named: windowName) ?? win
  clickAt(CGPoint(x: live.frame.minX + live.frame.width * CGFloat(fx),
                  y: live.frame.minY + live.frame.height * CGFloat(fy)))
  usleep(useconds_t(max(waitMs, 0) * 1000))

  var tapped: [String] = []
  let probe = "/tmp/iphone-tap-text-sequence.png"
  for (label, delayMs) in targets {
    live = resolveWindow(named: windowName) ?? live
    captureWindow(live, to: probe)
    let (_, _, hits) = recognize(probe)
    let matches = hits.filter {
      $0.text.trimmingCharacters(in: .whitespacesAndNewlines)
        .caseInsensitiveCompare(label) == .orderedSame
    }
    guard let hit = matches.max(by: { $0.y + $0.h / 2 < $1.y + $1.h / 2 }) else {
      fail("text not found after \(tapped.joined(separator: ", ")): \(label)")
    }
    let target = CGPoint(x: live.frame.minX + live.frame.width * (hit.x + hit.w / 2),
                         y: live.frame.minY + live.frame.height * (hit.y + hit.h / 2))
    clickAt(target)
    tapped.append(label)
    usleep(useconds_t(max(delayMs, 0) * 1000))
  }
  emit(["tappedText": tapped, "id": Int(live.id)])
}

func textMatches(_ hit: OCRHit, _ label: String) -> Bool {
  let text = hit.text.trimmingCharacters(in: .whitespacesAndNewlines)
  if label.caseInsensitiveCompare("Next") == .orderedSame {
    return text.range(of: #"^Next\s*\(\d+\)$"#, options: [.regularExpression, .caseInsensitive]) != nil
  }
  return text.caseInsensitiveCompare(label) == .orderedSame
}

// OCR and tap every step while the mirror remains active. The lowest matching
// label wins so modal controls take precedence over same-named page tabs.
func cmdTextSequence(_ targets: [(String, Int)]) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  guard let app = NSRunningApplication(processIdentifier: win.pid) else {
    fail("application not found for window: \(windowName)")
  }
  activateForInput(app)
  var live = resolveWindow(named: windowName) ?? win
  var tapped: [String] = []
  let probe = "/tmp/iphone-tap-text-sequence.png"
  for (label, delayMs) in targets {
    live = resolveWindow(named: windowName) ?? live
    captureWindow(live, to: probe)
    let (_, _, hits) = recognize(probe)
    let matches = hits.filter { textMatches($0, label) }
    guard let hit = matches.max(by: { $0.y + $0.h / 2 < $1.y + $1.h / 2 }) else {
      fail("text not found after \(tapped.joined(separator: ", ")): \(label)")
    }
    // iPhone Mirroring exposes the iOS sheet image without its 52-point Mac
    // toolbar inset, while CGWindow bounds used for input include that inset.
    // The first control (Next) is on the page; subsequent controls are in the
    // sheet and need the inset removed from their global click coordinate.
    let sheetInset: CGFloat = tapped.isEmpty ? 0 : 52
    clickAt(CGPoint(x: live.frame.minX + live.frame.width * (hit.x + hit.w / 2),
                    y: live.frame.minY + live.frame.height * (hit.y + hit.h / 2) - sheetInset))
    tapped.append(label)
    usleep(useconds_t(max(delayMs, 0) * 1000))
  }
  emit(["tappedText": tapped, "id": Int(live.id)])
}

func cmdSwipe(_ fx1: Double, _ fy1: Double, _ fx2: Double, _ fy2: Double, durationMs: Int) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  let prev = NSWorkspace.shared.frontmostApplication
  if !noActivate {
    if let app = NSRunningApplication(processIdentifier: win.pid) {
      activateForInput(app)
    }
  }
  let f = (noActivate ? win : (resolveWindow(named: windowName) ?? win)).frame
  let a = CGPoint(x: f.minX + f.width * CGFloat(fx1), y: f.minY + f.height * CGFloat(fy1))
  let b = CGPoint(x: f.minX + f.width * CGFloat(fx2), y: f.minY + f.height * CGFloat(fy2))
  dragFromTo(a, b, steps: 24, durationMs: durationMs)
  usleep(40_000)
  if !noActivate && restoreFocus { prev?.activate(options: []) }
  emit(["swiped": ["from": ["x": a.x, "y": a.y], "to": ["x": b.x, "y": b.y]], "id": Int(win.id)])
}

func cmdScroll(_ ticks: Int) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  let prev = NSWorkspace.shared.frontmostApplication
  if !noActivate {
    if let app = NSRunningApplication(processIdentifier: win.pid) {
      activateForInput(app)
    }
  }
  let live = (noActivate ? win : (resolveWindow(named: windowName) ?? win))
  scrollWheel(in: live, ticks: ticks, atFx: 0.5, atFy: 0.55)
  usleep(40_000)
  if !noActivate && restoreFocus { prev?.activate(options: []) }
  emit(["scrolled": ticks, "id": Int(live.id)])
}

final class EventLogContext {
  let file: FileHandle
  let frame: CGRect
  let started = ProcessInfo.processInfo.systemUptime
  var lastMove = 0.0

  init(file: FileHandle, frame: CGRect) {
    self.file = file
    self.frame = frame
  }

  func write(_ object: [String: Any]) {
    guard let data = try? JSONSerialization.data(withJSONObject: object),
          var line = String(data: data, encoding: .utf8) else { return }
    line.append("\n")
    file.write(line.data(using: .utf8)!)
  }
}

let recordCallback: CGEventTapCallBack = { _, type, event, userInfo in
  guard let userInfo else { return Unmanaged.passUnretained(event) }
  let context = Unmanaged<EventLogContext>.fromOpaque(userInfo).takeUnretainedValue()
  let now = ProcessInfo.processInfo.systemUptime
  if type == .mouseMoved && now - context.lastMove < 0.05 {
    return Unmanaged.passUnretained(event)
  }
  if type == .mouseMoved { context.lastMove = now }

  let point = event.location
  let frame = context.frame
  guard frame.contains(point) else { return Unmanaged.passUnretained(event) }
  let names: [CGEventType: String] = [
    .leftMouseDown: "tap-down", .leftMouseUp: "tap-up",
    .leftMouseDragged: "drag", .mouseMoved: "move", .scrollWheel: "scroll",
  ]
  var row: [String: Any] = [
    "t": now - context.started,
    "event": names[type] ?? "other",
    "x": point.x,
    "y": point.y,
    "fx": (point.x - frame.minX) / frame.width,
    "fy": (point.y - frame.minY) / frame.height,
  ]
  if type == .scrollWheel {
    row["deltaY"] = event.getIntegerValueField(.scrollWheelEventDeltaAxis1)
    row["pointDeltaY"] = event.getIntegerValueField(.scrollWheelEventPointDeltaAxis1)
    row["continuous"] = event.getIntegerValueField(.scrollWheelEventIsContinuous)
  }
  context.write(row)
  return Unmanaged.passUnretained(event)
}

func cmdRecordEvents(_ outPath: String) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  FileManager.default.createFile(atPath: outPath, contents: nil)
  guard let file = FileHandle(forWritingAtPath: outPath) else { fail("cannot write: \(outPath)") }
  let context = EventLogContext(file: file, frame: win.frame)
  context.write(["t": 0, "event": "start", "x": win.frame.minX, "y": win.frame.minY,
                 "w": win.frame.width, "h": win.frame.height])
  let retained = Unmanaged.passRetained(context)
  let types: [CGEventType] = [.leftMouseDown, .leftMouseUp, .leftMouseDragged, .mouseMoved, .scrollWheel]
  let mask = types.reduce(CGEventMask(0)) { $0 | (CGEventMask(1) << CGEventMask($1.rawValue)) }
  guard let tap = CGEvent.tapCreate(tap: .cgSessionEventTap, place: .tailAppendEventTap,
                                    options: .listenOnly, eventsOfInterest: mask,
                                    callback: recordCallback, userInfo: retained.toOpaque()) else {
    retained.release()
    fail("event tap unavailable; grant Accessibility permission to the terminal")
  }
  let source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, tap, 0)
  CFRunLoopAddSource(CFRunLoopGetCurrent(), source, .commonModes)
  CGEvent.tapEnable(tap: tap, enable: true)
  emit(["recording": outPath, "frame": ["x": win.frame.minX, "y": win.frame.minY,
                                        "w": win.frame.width, "h": win.frame.height]])
  CFRunLoopRun()
}

// ---- dispatch ------------------------------------------------------------

guard let verb = rawArgs.first else {
  fail("usage: iphone-tap <frame|shot OUT.png|ocr IN.png|tap FX FY|tap-sequence FX FY WAIT_MS ...|tap-text-sequence FX FY WAIT_MS TEXT WAIT_MS ...|text-sequence TEXT WAIT_MS ...|record-events OUT.jsonl> [--window NAME]")
}
rawArgs.removeFirst()

switch verb {
case "frame":
  cmdFrame()
case "shot":
  guard let out = rawArgs.first else { fail("shot needs OUT.png") }
  cmdShot(out)
case "ocr":
  guard let inp = rawArgs.first else { fail("ocr needs IN.png") }
  cmdOcr(inp)
case "tap":
  guard rawArgs.count >= 2, let fx = Double(rawArgs[0]), let fy = Double(rawArgs[1]) else {
    fail("tap needs FX FY (fractions 0..1)")
  }
  cmdTap(fx, fy)
case "tap-sequence":
  guard rawArgs.count >= 3, rawArgs.count % 3 == 0 else {
    fail("tap-sequence needs repeating FX FY WAIT_MS triples")
  }
  var steps: [(Double, Double, Int)] = []
  for i in stride(from: 0, to: rawArgs.count, by: 3) {
    guard let fx = Double(rawArgs[i]), let fy = Double(rawArgs[i + 1]),
          let waitMs = Int(rawArgs[i + 2]) else {
      fail("tap-sequence needs repeating FX FY WAIT_MS triples")
    }
    steps.append((fx, fy, waitMs))
  }
  cmdTapSequence(steps)
case "tap-text-sequence":
  guard rawArgs.count >= 5, (rawArgs.count - 3) % 2 == 0,
        let fx = Double(rawArgs[0]), let fy = Double(rawArgs[1]),
        let waitMs = Int(rawArgs[2]) else {
    fail("tap-text-sequence needs FX FY WAIT_MS then TEXT WAIT_MS pairs")
  }
  var targets: [(String, Int)] = []
  for i in stride(from: 3, to: rawArgs.count, by: 2) {
    guard let targetWaitMs = Int(rawArgs[i + 1]) else {
      fail("tap-text-sequence needs FX FY WAIT_MS then TEXT WAIT_MS pairs")
    }
    targets.append((rawArgs[i], targetWaitMs))
  }
  cmdTapTextSequence(fx, fy, waitMs, targets)
case "text-sequence":
  guard rawArgs.count >= 2, rawArgs.count % 2 == 0 else {
    fail("text-sequence needs repeating TEXT WAIT_MS pairs")
  }
  var targets: [(String, Int)] = []
  for i in stride(from: 0, to: rawArgs.count, by: 2) {
    guard let targetWaitMs = Int(rawArgs[i + 1]) else {
      fail("text-sequence needs repeating TEXT WAIT_MS pairs")
    }
    targets.append((rawArgs[i], targetWaitMs))
  }
  cmdTextSequence(targets)
case "swipe":
  guard rawArgs.count >= 4,
        let fx1 = Double(rawArgs[0]), let fy1 = Double(rawArgs[1]),
        let fx2 = Double(rawArgs[2]), let fy2 = Double(rawArgs[3]) else {
    fail("swipe needs FX1 FY1 FX2 FY2 (fractions 0..1) [--duration MS]")
  }
  cmdSwipe(fx1, fy1, fx2, fy2, durationMs: durationMs)
case "scroll":
  guard let n = Int(rawArgs.first ?? "") else { fail("scroll needs TICKS (negative scrolls content up)") }
  cmdScroll(n)
case "record-events":
  guard let out = rawArgs.first else { fail("record-events needs OUT.jsonl") }
  cmdRecordEvents(out)
default:
  fail("unknown command: \(verb)")
}
