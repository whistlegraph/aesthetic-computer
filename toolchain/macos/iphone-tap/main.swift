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
  let dir = ticks < 0 ? -1 : 1
  for _ in 0..<abs(ticks) {
    CGEvent(scrollWheelEvent2Source: nil, units: .pixel, wheelCount: 1,
            wheel1: Int32(dir * 40), wheel2: 0, wheel3: 0)?.post(tap: .cghidEventTap)
    usleep(12_000)
  }
  if let saved = saved { CGWarpMouseCursorPosition(saved) }
}

// ---- commands ------------------------------------------------------------

func cmdFrame() {
  guard let f = windowFrame(named: windowName) else { fail("window not found: \(windowName)") }
  emit(["x": f.minX, "y": f.minY, "w": f.width, "h": f.height])
}

func cmdShot(_ outPath: String) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  // Capture by window id (-l), not screen region: grabs THIS window's own live
  // pixels even when it's on another Space or behind other windows, so the user
  // can park the mirror anywhere. -o drops the drop-shadow.
  let proc = Process()
  proc.executableURL = URL(fileURLWithPath: "/usr/sbin/screencapture")
  proc.arguments = ["-x", "-o", "-l\(win.id)", outPath]
  do { try proc.run(); proc.waitUntilExit() } catch { fail("screencapture failed: \(error)") }
  if proc.terminationStatus != 0 { fail("screencapture exit \(proc.terminationStatus)") }
  let f = win.frame
  emit(["x": f.minX, "y": f.minY, "w": f.width, "h": f.height, "id": Int(win.id), "path": outPath])
}

func cmdOcr(_ inPath: String) {
  guard let src = CGImageSourceCreateWithURL(URL(fileURLWithPath: inPath) as CFURL, nil),
        let cg = CGImageSourceCreateImageAtIndex(src, 0, nil) else { fail("cannot read image: \(inPath)") }
  let imgW = CGFloat(cg.width), imgH = CGFloat(cg.height)
  let request = VNRecognizeTextRequest()
  request.recognitionLevel = .accurate
  request.usesLanguageCorrection = false
  let handler = VNImageRequestHandler(cgImage: cg, options: [:])
  do { try handler.perform([request]) } catch { fail("ocr failed: \(error)") }
  var lines: [[String: Any]] = []
  for obs in (request.results ?? []) {
    guard let top = obs.topCandidates(1).first else { continue }
    let b = obs.boundingBox  // normalized, BOTTOM-left origin
    lines.append([
      "text": top.string,
      "conf": top.confidence,
      "x": b.origin.x,
      "y": 1.0 - (b.origin.y + b.height),  // → TOP-left origin
      "w": b.width,
      "h": b.height,
    ])
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
    NSRunningApplication(processIdentifier: win.pid)?.activate(options: [])
    // Settle long enough to cover a Space-switch animation when the mirror was
    // parked on another desktop — otherwise the click lands mid-transition and
    // misses. Cheap insurance; taps are paced seconds apart anyway.
    usleep(450_000)
  }
  let f = (noActivate ? win : (resolveWindow(named: windowName) ?? win)).frame
  let target = CGPoint(x: f.minX + f.width * CGFloat(fx), y: f.minY + f.height * CGFloat(fy))
  clickAt(target)
  usleep(40_000)
  if !noActivate && restoreFocus { prev?.activate(options: []) }
  emit(["tapped": ["x": target.x, "y": target.y], "fx": fx, "fy": fy, "id": Int(win.id)])
}

func cmdSwipe(_ fx1: Double, _ fy1: Double, _ fx2: Double, _ fy2: Double, durationMs: Int) {
  guard let win = resolveWindow(named: windowName) else { fail("window not found: \(windowName)") }
  let prev = NSWorkspace.shared.frontmostApplication
  if !noActivate {
    NSRunningApplication(processIdentifier: win.pid)?.activate(options: [])
    usleep(450_000)
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
    NSRunningApplication(processIdentifier: win.pid)?.activate(options: [])
    usleep(450_000)
  }
  let live = (noActivate ? win : (resolveWindow(named: windowName) ?? win))
  scrollWheel(in: live, ticks: ticks, atFx: 0.5, atFy: 0.55)
  usleep(40_000)
  if !noActivate && restoreFocus { prev?.activate(options: []) }
  emit(["scrolled": ticks, "id": Int(live.id)])
}

// ---- dispatch ------------------------------------------------------------

guard let verb = rawArgs.first else {
  fail("usage: iphone-tap <frame|shot OUT.png|ocr IN.png|tap FX FY> [--window NAME]")
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
default:
  fail("unknown command: \(verb)")
}
