import AppKit
import AVFoundation
import GameController
import SpriteKit
import TrackpadBridge
import Darwin

private let fixedStep = 1.0 / 60.0
private let arenaWidth: CGFloat = 1280
private let arenaHeight: CGFloat = 720
private let windowWidth: CGFloat = 420
private let windowHeight: CGFloat = 236.25
private let cornerHoldDuration: TimeInterval = 1
private let ink = NSColor(calibratedRed: 0.035, green: 0.025, blue: 0.055, alpha: 1)
// Keep the encounter personal to the signed-in macOS account. AppKit resolves
// this dynamic color from System Settings > Appearance > Accent color.
private let accent = NSColor.controlAccentColor
private let contrastDark = NSColor.black
private let contrastLight = NSColor.white
private let lineDark: CGFloat = 4
private let lineLight: CGFloat = 2.5
private let lineSpot: CGFloat = 1
private let playWindowLevel = NSWindow.Level(rawValue: NSWindow.Level.screenSaver.rawValue + 1)
private let menuBandTrackpadOwnerURL = FileManager.default.homeDirectoryForCurrentUser
  .appendingPathComponent(".local/share/menuband/trackpad-owner")

/// Menu Band writes its PID while its global trackpad surface owns the input.
/// Validate liveness so a stale file after a crash never disables Fighter.
private func menuBandOwnsTrackpad() -> Bool {
  guard let text = try? String(contentsOf: menuBandTrackpadOwnerURL,
                               encoding: .utf8),
        let pid = pid_t(text.trimmingCharacters(in: .whitespacesAndNewlines)) else {
    return false
  }
  if kill(pid, 0) == 0 || errno == EPERM { return true }
  try? FileManager.default.removeItem(at: menuBandTrackpadOwnerURL)
  return false
}

// One append-only trace for every Trackpad Fighter process and round. A round
// that crashes or is killed simply has no terminal event, which is intentional:
// the unfinished lifetime remains visible during backtracing.
private final class RoundLifetimeLog: @unchecked Sendable {
  static let shared = RoundLifetimeLog()

  private let lock = NSLock()
  private let processID = ProcessInfo.processInfo.processIdentifier
  private let processSessionID = UUID().uuidString.lowercased()
  private let processStarted = ProcessInfo.processInfo.systemUptime
  private let fileURL: URL
  private var activeRoundID: String?
  private var roundStarted: TimeInterval?
  private var roundSequence = 0
  private var processFinished = false

  private init() {
    let root = FileManager.default.homeDirectoryForCurrentUser
      .appendingPathComponent(".local/share/trackpad-fighter", isDirectory: true)
    try? FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
    fileURL = root.appendingPathComponent("rounds.jsonl")
    emit("process_started", roundID: nil, fields: [
      "executable": URL(fileURLWithPath: CommandLine.arguments.first ?? "trackpad-fighter").lastPathComponent,
    ])
  }

  var path: String { fileURL.path }

  func event(_ name: String, roundID: String? = nil, fields: [String: Any] = [:]) {
    lock.lock(); defer { lock.unlock() }
    emitLocked(name, roundID: roundID ?? activeRoundID, fields: fields)
  }

  @discardableResult
  func beginRound(trigger: String, mode: String, launchID: String?) -> String {
    lock.lock(); defer { lock.unlock() }
    if let current = activeRoundID {
      emitLocked("round_abandoned", roundID: current, fields: [
        "reason": "superseded", "nextTrigger": trigger,
        "roundElapsedMs": elapsedMs(since: roundStarted),
      ])
    }
    roundSequence += 1
    let id = "\(processSessionID)-r\(roundSequence)"
    activeRoundID = id
    roundStarted = ProcessInfo.processInfo.systemUptime
    var fields: [String: Any] = ["trigger": trigger, "mode": mode, "sequence": roundSequence]
    if let launchID { fields["launchId"] = launchID }
    emitLocked("round_started", roundID: id, fields: fields)
    return id
  }

  func finishRound(_ outcome: String, roundID: String, fields: [String: Any] = [:]) {
    lock.lock(); defer { lock.unlock() }
    var details = fields
    details["outcome"] = outcome
    details["roundElapsedMs"] = elapsedMs(since: self.roundStarted)
    guard activeRoundID == roundID else {
      details["activeRoundId"] = activeRoundID ?? NSNull()
      emitLocked("round_finish_mismatch", roundID: roundID, fields: details)
      return
    }
    emitLocked("round_finished", roundID: roundID, fields: details)
    activeRoundID = nil
    roundStarted = nil
  }

  func finishProcess(reason: String) {
    lock.lock(); defer { lock.unlock() }
    guard !processFinished else { return }
    if let roundID = activeRoundID {
      emitLocked("round_abandoned", roundID: roundID, fields: [
        "reason": reason, "roundElapsedMs": elapsedMs(since: roundStarted),
      ])
      activeRoundID = nil
      roundStarted = nil
    }
    emitLocked("process_finished", roundID: nil, fields: ["reason": reason])
    processFinished = true
  }

  private func emit(_ name: String, roundID: String?, fields: [String: Any]) {
    lock.lock(); defer { lock.unlock() }
    emitLocked(name, roundID: roundID, fields: fields)
  }

  private func emitLocked(_ name: String, roundID: String?, fields: [String: Any]) {
    let uptime = ProcessInfo.processInfo.systemUptime
    var record: [String: Any] = [
      "at": ISO8601DateFormatter().string(from: Date()),
      "elapsedMs": Int((uptime - processStarted) * 1000),
      "event": name,
      "pid": processID,
      "processSessionId": processSessionID,
      "fields": fields,
    ]
    if let roundID { record["roundId"] = roundID }
    guard var data = try? JSONSerialization.data(withJSONObject: record, options: [.sortedKeys]) else { return }
    data.append(0x0a)
    let fd = Darwin.open(fileURL.path, O_WRONLY | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR)
    guard fd >= 0 else { return }
    data.withUnsafeBytes { bytes in
      if let base = bytes.baseAddress { _ = Darwin.write(fd, base, bytes.count) }
    }
    _ = Darwin.close(fd)
    fputs("[trackpad-fighter][lifetime] \(name) process=\(processSessionID) round=\(roundID ?? "-")\n", stderr)
  }

  private func elapsedMs(since start: TimeInterval?) -> Int {
    guard let start else { return 0 }
    return Int((ProcessInfo.processInfo.systemUptime - start) * 1000)
  }
}

private func pointerScreen() -> NSScreen? {
  let pointer = NSEvent.mouseLocation
  return NSScreen.screens.first(where: { $0.frame.contains(pointer) }) ?? NSScreen.main
}

private func trackpadInputChanged(_ x: Float, _ y: Float, _ contacts: Int32) {
  Task { @MainActor in TrackpadFlightInput.shared.update(x: CGFloat(x), y: CGFloat(y), contacts: Int(contacts)) }
}

@MainActor private final class TrackpadFlightInput {
  static let shared = TrackpadFlightInput()
  private var contacts = 0
  private var lastX: CGFloat?
  private var lastY: CGFloat?
  private var touchStarted: TimeInterval?
  private var touchTravel: CGFloat = 0
  private var direction: CGFloat?
  private var boost: CGFloat = 0
  private var firePending = false

  func update(x: CGFloat, y: CGFloat, contacts nextContacts: Int) {
    let now = ProcessInfo.processInfo.systemUptime
    if nextContacts == 1 {
      if contacts != 1 {
        touchStarted = now; touchTravel = 0
        lastX = x; lastY = y
      } else if let lastX, let lastY {
        let dx = x - lastX, dy = y - lastY
        let distance = hypot(dx, dy)
        touchTravel += distance
        if distance > 0.001 {
          direction = atan2(dy, dx)
          boost += distance * 3.5
        }
        self.lastX = x; self.lastY = y
      }
    } else {
      if contacts == 1, nextContacts == 0, let touchStarted,
         now - touchStarted <= 0.24, touchTravel < 0.045 {
        firePending = true
      }
      lastX = nil; lastY = nil; touchStarted = nil; touchTravel = 0
    }
    contacts = nextContacts
  }

  func consume() -> (direction: CGFloat?, boost: CGFloat, grabbed: Bool, fire: Bool) {
    let snapshot = (direction, min(1, boost), contacts == 1, firePending)
    direction = nil; boost = 0; firePending = false
    return snapshot
  }
}

private struct Buttons {
  var left = false, right = false, jump = false, light = false, heavy = false
}

private extension Buttons {
  var mask: Int { (left ? 1 : 0) | (right ? 2 : 0) | (jump ? 4 : 0) | (light ? 8 : 0) | (heavy ? 16 : 0) }
  init(mask: Int) { left = mask & 1 != 0; right = mask & 2 != 0; jump = mask & 4 != 0; light = mask & 8 != 0; heavy = mask & 16 != 0 }
}

private final class Tone {
  private let engine = AVAudioEngine()
  private let player = AVAudioPlayerNode()
  private let format = AVAudioFormat(standardFormatWithSampleRate: 48_000, channels: 1)!

  init() {
    engine.attach(player)
    engine.connect(player, to: engine.mainMixerNode, format: format)
    try? engine.start()
  }

  func play(frequency: Double, endFrequency: Double? = nil, duration: Double, volume: Float = 0.18) {
    let frames = AVAudioFrameCount(48_000 * duration)
    guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
          let samples = buffer.floatChannelData?[0] else { return }
    buffer.frameLength = frames
    var phase: Double = 0
    for i in 0..<Int(frames) {
      let t = Double(i) / Double(max(1, frames - 1))
      let hz = frequency + ((endFrequency ?? frequency) - frequency) * t
      phase += 2 * .pi * hz / 48_000
      let edge = min(1, Double(i) / 72) * min(1, Double(Int(frames) - i) / 240)
      let envelope = Float(edge * pow(1 - t, 0.35))
      samples[i] = sin(Float(phase)) * volume * envelope
    }
    player.scheduleBuffer(buffer)
    if !player.isPlaying { player.play() }
  }

  func cancel() { player.stop() }
}

private func cornerPoseChanged(_ active: Bool) {
  Task { @MainActor in CornerWatcher.shared.pose(active) }
}

private final class CornerFlashView: NSView {
  override func draw(_ dirtyRect: NSRect) {
    accent.withAlphaComponent(0.88).setFill(); bounds.fill()
    let center = NSPoint(x: bounds.midX, y: bounds.midY)
    let streaks = NSBezierPath(); streaks.lineWidth = 1.5
    for corner in [NSPoint(x: bounds.minX, y: bounds.minY), NSPoint(x: bounds.maxX, y: bounds.minY),
                   NSPoint(x: bounds.minX, y: bounds.maxY), NSPoint(x: bounds.maxX, y: bounds.maxY)] {
      for lane in 0..<7 {
        let spread = CGFloat(lane - 3) * 18
        let dx = corner.x - center.x, dy = corner.y - center.y
        let length = max(1, hypot(dx, dy))
        let nx = -dy / length, ny = dx / length
        let near = NSPoint(x: center.x + dx * 0.10 + nx * spread,
                           y: center.y + dy * 0.10 + ny * spread)
        let far = NSPoint(x: center.x + dx * 0.72 + nx * spread * 2.4,
                          y: center.y + dy * 0.72 + ny * spread * 2.4)
        streaks.move(to: near); streaks.line(to: far)
      }
    }
    ink.withAlphaComponent(0.58).setStroke(); streaks.stroke()

    for (x, direction) in [(bounds.midX - bounds.width * 0.22, CGFloat(1)),
                           (bounds.midX + bounds.width * 0.22, CGFloat(-1))] {
      let ship = NSBezierPath()
      ship.move(to: NSPoint(x: x + 15 * direction, y: bounds.midY))
      ship.line(to: NSPoint(x: x - 11 * direction, y: bounds.midY + 8))
      ship.line(to: NSPoint(x: x - 6 * direction, y: bounds.midY))
      ship.line(to: NSPoint(x: x - 11 * direction, y: bounds.midY - 8))
      ship.close()
      contrastDark.setStroke(); ship.lineWidth = lineDark; ship.stroke()
      contrastLight.setStroke(); ship.lineWidth = lineLight; ship.stroke()
      accent.setStroke(); ship.lineWidth = lineSpot; ship.stroke()
    }
  }
}

@MainActor private final class CornerWatcher {
  static let shared = CornerWatcher()
  private var launched = false
  private var poseIsActive = false
  private var introProcess: Process?
  private var launchID: String?
  private var flashPanels: [NSPanel] = []

  func start() {
    guard MFStartCornerPoseWatcher(cornerPoseChanged) else {
      RoundLifetimeLog.shared.event("watcher_start_failed", fields: ["reason": "trackpad_unavailable"])
      RoundLifetimeLog.shared.finishProcess(reason: "watcher_start_failed")
      fputs("trackpad-fighter: no MultitouchSupport trackpad found\n", stderr); exit(1)
    }
    RoundLifetimeLog.shared.event("watcher_started", fields: ["logPath": RoundLifetimeLog.shared.path])
    print("[trackpad-fighter] four-corner watcher ready")
  }

  func pose(_ active: Bool) {
    guard active != poseIsActive else { return }
    poseIsActive = active
    if active {
      guard !menuBandOwnsTrackpad() else {
        RoundLifetimeLog.shared.event("pose_ignored", fields: [
          "reason": "menu_band_trackpad_owner",
        ])
        print("[trackpad-fighter] four-corner pose ignored — Menu Band owns trackpad")
        return
      }
      guard !launched else {
        RoundLifetimeLog.shared.event("pose_ignored", fields: [
          "launchActive": launched,
        ])
        return
      }
      RoundLifetimeLog.shared.event("pose_started")
      print("[trackpad-fighter] four-corner pose active")
      flashNow()
      launchIntroFight()
      RoundLifetimeLog.shared.event("pose_armed", fields: [
        "launchId": launchID ?? NSNull(), "holdMs": 0,
      ])
      print("[trackpad-fighter] desktop fight armed instantly")
    } else {
      RoundLifetimeLog.shared.event("pose_released", fields: ["launchId": launchID ?? NSNull()])
    }
  }

  private func flashNow() {
    flashPanels.forEach { $0.orderOut(nil) }
    flashPanels = NSScreen.screens.map { screen in
      let panel = NSPanel(contentRect: screen.frame, styleMask: [.borderless], backing: .buffered, defer: false)
      panel.contentView = CornerFlashView(frame: NSRect(origin: .zero, size: screen.frame.size))
      panel.isOpaque = false; panel.backgroundColor = .clear; panel.hasShadow = false
      panel.level = NSWindow.Level(rawValue: NSWindow.Level.screenSaver.rawValue + 2)
      panel.ignoresMouseEvents = true
      panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]
      panel.orderFrontRegardless()
      return panel
    }
    let frame = pointerScreen()?.frame ?? NSScreen.main?.frame ?? .zero
    RoundLifetimeLog.shared.event("pose_flash_presented", fields: [
      "screenWidth": Int(frame.width), "screenHeight": Int(frame.height),
      "displayCount": flashPanels.count,
    ])
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.20) { [weak self] in
      self?.flashPanels.forEach { $0.orderOut(nil) }
      self?.flashPanels.removeAll()
    }
  }

  private func launchIntroFight() {
    let process = Process()
    let id = UUID().uuidString.lowercased()
    process.executableURL = URL(fileURLWithPath: CommandLine.arguments[0])
    process.arguments = ["--desktop", "--intro", "--launch-id", id]
    RoundLifetimeLog.shared.event("round_launch_requested", fields: ["launchId": id])
    process.terminationHandler = { [weak self] terminated in
      RoundLifetimeLog.shared.event("round_process_terminated", fields: [
        "launchId": id, "childPid": terminated.processIdentifier,
        "status": terminated.terminationStatus,
      ])
      Task { @MainActor in
        self?.introProcess = nil; self?.launchID = nil; self?.launched = false
      }
    }
    introProcess = process; launchID = id; launched = true
    do {
      try introProcess?.run()
      RoundLifetimeLog.shared.event("round_process_spawned", fields: [
        "launchId": id, "childPid": process.processIdentifier,
      ])
    } catch {
      RoundLifetimeLog.shared.event("round_launch_failed", fields: [
        "launchId": id, "error": String(describing: error),
      ])
      introProcess = nil; launchID = nil; launched = false
      print("[trackpad-fighter] could not launch intro fight: \(error)")
    }
  }
}

@MainActor private final class Fighter {
  let root = SKNode()
  private let skeleton = SKShapeNode()
  private let skeletonInk = SKShapeNode()
  private let head = SKShapeNode(circleOfRadius: 11)
  private let headInk = SKShapeNode(circleOfRadius: 11)
  private let fist = SKShapeNode(circleOfRadius: 4)
  private let shadow = SKShapeNode(ellipseOf: CGSize(width: 42, height: 7))
  let number: Int
  var velocity = CGVector.zero
  var health: CGFloat = 100
  var facing: CGFloat
  var grounded = true
  var attackFrames = 0
  var cooldown = 0
  var hitThisAttack = false
  var flashFrames = 0
  var floorY: CGFloat = 122
  var flightMode = false
  var visualScale: CGFloat = 1
  private(set) var heading: CGFloat
  private var gestureGrabbed = false
  private var gestureBoostFrames = 0
  private var attackDuration = 0
  private var lastRootScale = CGFloat.nan, lastRootAlpha = CGFloat.nan

  init(number: Int, inverted _: Bool, facing: CGFloat) {
    self.number = number; self.facing = facing
    let color = inverted ? NSColor.white : pink
    let path = CGMutablePath()
    path.move(to: CGPoint(x: 0, y: 23)); path.addLine(to: CGPoint(x: 0, y: -14))
    path.move(to: CGPoint(x: 0, y: 12)); path.addLine(to: CGPoint(x: -18, y: -1))
    path.move(to: CGPoint(x: 0, y: 12)); path.addLine(to: CGPoint(x: 18, y: -1))
    path.move(to: CGPoint(x: 0, y: -14)); path.addLine(to: CGPoint(x: -14, y: -39))
    path.move(to: CGPoint(x: 0, y: -14)); path.addLine(to: CGPoint(x: 14, y: -39))
    shadow.fillColor = ink; shadow.strokeColor = ink; shadow.alpha = 0.32
    shadow.position.y = -42; root.addChild(shadow)
    skeletonInk.path = path; skeletonInk.strokeColor = ink; skeletonInk.lineWidth = 9
    skeletonInk.lineCap = .round; skeletonInk.lineJoin = .round; root.addChild(skeletonInk)
    skeleton.path = path; skeleton.strokeColor = color; skeleton.lineWidth = 4
    skeleton.lineCap = .round; skeleton.lineJoin = .round; root.addChild(skeleton)
    headInk.fillColor = .clear; headInk.strokeColor = ink; headInk.lineWidth = 8
    headInk.position.y = 35; root.addChild(headInk)
    head.fillColor = .clear; head.strokeColor = color; head.lineWidth = 3
    head.position.y = 35; root.addChild(head)
    fist.fillColor = color; fist.strokeColor = ink; fist.lineWidth = 3
    fist.position = CGPoint(x: 20 * facing, y: 0); root.addChild(fist)
  }

  var hitbox: CGRect { CGRect(x: root.position.x - 18, y: root.position.y - 42, width: 36, height: 84) }
  var attackbox: CGRect {
    let reach: CGFloat = attackFrames > 0 ? (attackFrames > 7 ? 62 : 43) : 0
    return CGRect(x: facing > 0 ? root.position.x + 8 : root.position.x - 8 - reach,
                  y: root.position.y - 22, width: reach, height: 48)
  }

  func beginAttack(heavy: Bool) -> Bool {
    guard cooldown == 0, attackFrames == 0 else { return false }
    attackDuration = heavy ? 16 : 10
    attackFrames = attackDuration; cooldown = heavy ? 29 : 18; hitThisAttack = false
    return true
  }

  func setVisualScale(_ scale: CGFloat) {
    visualScale = scale
    root.setScale(scale)
    let compensation = 1 / max(0.001, scale)
    hullDark.lineWidth = lineDark * compensation
    hullLight.lineWidth = lineLight * compensation
    hull.lineWidth = lineSpot * compensation
  }

  var emitsThrust: Bool { flightMode && !grounded }
  var exhaustPosition: CGPoint {
    CGPoint(x: root.position.x - cos(heading) * 12,
            y: root.position.y - sin(heading) * 12)
  }

  func applyGesture(direction: CGFloat?, boost: CGFloat, grabbed: Bool) {
    guard flightMode else { return }
    gestureGrabbed = grabbed
    if let direction { heading = direction }
    if boost > 0 {
      velocity.dx += cos(heading) * boost * 2.8
      velocity.dy += sin(heading) * boost * 2.8
      gestureBoostFrames = 9
    }
  }

  func tick(buttons: Buttons, opponentX: CGFloat) -> Int? {
    var fired: Int?
    if buttons.light, beginAttack(heavy: false) { fired = 8 }
    if buttons.heavy, beginAttack(heavy: true) { fired = 15 }
    if flightMode {
      if buttons.left { heading += 0.055 }
      if buttons.right { heading -= 0.055 }
      let engineOn = buttons.jump
      if engineOn {
        velocity.dx += cos(heading) * 0.34
        velocity.dy += sin(heading) * 0.34
      }
      let speed = hypot(velocity.dx, velocity.dy)
      if speed > 4.2 {
        velocity.dx *= 4.2 / speed
        velocity.dy *= 4.2 / speed
      }
      velocity.dx *= 0.985; velocity.dy *= 0.985
      root.position.x += velocity.dx; root.position.y += velocity.dy
      if root.position.x < -45 { root.position.x = arenaWidth + 45 }
      if root.position.x > arenaWidth + 45 { root.position.x = -45 }
      if root.position.y < -45 { root.position.y = arenaHeight + 45 }
      if root.position.y > arenaHeight + 45 { root.position.y = -45 }
      facing = cos(heading) >= 0 ? 1 : -1
      grounded = !(engineOn || gestureBoostFrames > 0)
    } else {
      facing = opponentX >= root.position.x ? 1 : -1
      let desired: CGFloat = buttons.left ? -6 : buttons.right ? 6 : 0
      velocity.dx += (desired - velocity.dx) * (grounded ? 0.20 : 0.10)
      if buttons.jump && grounded { velocity.dy = 17; grounded = false }
      velocity.dy -= grounded ? 0 : 0.92
      root.position.x = min(arenaWidth - 70, max(70, root.position.x + velocity.dx))
      root.position.y += velocity.dy
      if root.position.y <= floorY, velocity.dy <= 0 {
        root.position.y = floorY; velocity.dy = 0; grounded = true
      }
    }
    if attackFrames > 0 { attackFrames -= 1 }
    if cooldown > 0 { cooldown -= 1 }
    if flashFrames > 0 { flashFrames -= 1 }
    let extensionAmount: CGFloat = attackFrames > 0 ? (attackFrames > 7 ? 39 : 27) : 0
    fist.position.x = facing * (20 + extensionAmount)
    root.xScale = visualScale * (flashFrames % 2 == 1 ? 1.08 : 1)
    root.yScale = visualScale
    root.alpha = flashFrames % 2 == 1 ? 0.55 : 1
    shadow.xScale = max(0.55, 1 - (root.position.y - floorY) / 600)
    return fired
  }

  private func updatePose() {
    let lean = max(-0.18, min(0.18, velocity.dy * 0.012))
    if flightMode { ship.xScale = 1; ship.zRotation = heading }
    else { ship.xScale = facing; ship.zRotation = lean * facing }
    ship.position = .zero
    ship.setScale(gestureGrabbed ? 1.08 : 1)
  }
}

private final class FightScene: SKScene {
  private let lifetime = RoundLifetimeLog.shared
  private let desktopMode: Bool
  private let introMode: Bool
  private let previewSling: Bool
  private let introCancelPath: String?
  private let sceneID: String
  private let launchID: String?
  private var roundID: String
  private var sceneBuilt = false
  private let tone = Tone()
  private let p1 = Fighter(number: 1, inverted: false, facing: 1)
  private let p2 = Fighter(number: 2, inverted: true, facing: -1)
  private let p1Bar = SKShapeNode(), p2Bar = SKShapeNode()
  private let title = SKLabelNode(fontNamed: "AvenirNext-Regular")
  private let status = SKLabelNode(fontNamed: "AvenirNext-Medium")
  private let menuCard = SKShapeNode(rectOf: CGSize(width: 450, height: 290), cornerRadius: 34)
  private let train = SKLabelNode(fontNamed: "AvenirNext-Heavy")
  private var keys = Set<UInt16>()
  private var previous = [Buttons(), Buttons()]
  private var accumulator = 0.0, lastTime = 0.0
  private var menuOpen = true, roundOver = false
  private var pointerX: CGFloat?
  private var pointerLight = false, pointerHeavy = false, pointerJump = false
  private let desktopStage: ClosedRange<CGFloat> = 44...1236
  private let desktopFloor: CGFloat = 78

  init(size: CGSize, desktopMode: Bool = false, introMode: Bool = false, previewSling: Bool = false,
       introCancelPath: String? = nil,
       roundID: String, sceneID: String, launchID: String? = nil) {
    self.desktopMode = desktopMode; self.introMode = introMode; self.previewSling = previewSling
    self.introCancelPath = introCancelPath
    self.roundID = roundID; self.sceneID = sceneID; self.launchID = launchID
    super.init(size: size)
    lifetime.event("scene_initialized", roundID: roundID, fields: [
      "sceneId": sceneID, "desktop": desktopMode, "intro": introMode,
      "width": Int(size.width), "height": Int(size.height),
    ])
  }

  required init?(coder aDecoder: NSCoder) { fatalError("init(coder:) has not been implemented") }

  override func didMove(to view: SKView) {
    lifetime.event("scene_attach_requested", roundID: roundID, fields: [
      "sceneId": sceneID, "alreadyBuilt": sceneBuilt, "childCount": children.count,
      "viewWidth": Int(view.bounds.width), "viewHeight": Int(view.bounds.height),
    ])
    guard !sceneBuilt else {
      lifetime.event("scene_duplicate_attach_ignored", roundID: roundID, fields: [
        "sceneId": sceneID, "childCount": children.count,
      ])
      return
    }
    sceneBuilt = true
    backgroundColor = desktopMode ? .clear : ink
    if !desktopMode { addBackdrop() }
    if desktopMode {
      physicsWorld.gravity = CGVector(dx: 0, dy: -55)
      for fighter in [p1, p2] {
        fighter.floorY = desktopFloor; fighter.platformRange = desktopStage; fighter.allowsRingOut = true
        fighter.visualScale = 0.72; fighter.root.setScale(fighter.visualScale)
      }
      addCornerVignette()
      addAsteroids()
    }
    p1.root.position = CGPoint(x: 390, y: 122); p2.root.position = CGPoint(x: 890, y: 122)
    addChild(p1.root); addChild(p2.root)
    title.text = "TRACKPAD  /  STARFIGHTER"; title.fontSize = 34; title.fontColor = accent
    title.position = CGPoint(x: arenaWidth / 2, y: arenaHeight - 70); if !desktopMode { addChild(title) }
    status.fontSize = desktopMode ? 11 : 18; status.fontColor = accent.withAlphaComponent(desktopMode ? 0.72 : 1)
    status.position = CGPoint(x: arenaWidth / 2, y: desktopMode ? desktopFloor + 82 : 48); status.zPosition = 31; addChild(status)
    menuCard.fillColor = ink.withAlphaComponent(0.94); menuCard.strokeColor = accent; menuCard.lineWidth = 2
    menuCard.position = CGPoint(x: arenaWidth / 2, y: arenaHeight / 2); menuCard.zPosition = 20; if !desktopMode { addChild(menuCard) }
    train.text = "TRAIN"; train.fontSize = 42; train.fontColor = accent; train.position = CGPoint(x: 0, y: 26); menuCard.addChild(train)
    let trainSub = SKLabelNode(fontNamed: "AvenirNext-Medium"); trainSub.text = "RETURN  •  LOCAL FREEFIGHT"; trainSub.fontSize = 15; trainSub.fontColor = accent; trainSub.position.y = -4; menuCard.addChild(trainSub)
    resetRound(trigger: "initial_scene", beginNewLifetime: false); showMenu(false)
    if introMode { beginIntro() }
    else {
      lifetime.event("combat_started", roundID: roundID, fields: ["sceneId": sceneID])
      tone.play(frequency: 784, endFrequency: 1046, duration: 0.14, volume: 0.14)
    }
    lifetime.event("scene_attached", roundID: roundID, fields: [
      "sceneId": sceneID, "childCount": children.count,
      "fighter1Children": p1.root.children.count, "fighter2Children": p2.root.children.count,
    ])
  }

  override func willMove(from view: SKView) {
    lifetime.event("scene_detaching", roundID: roundID, fields: [
      "sceneId": sceneID, "childCount": children.count,
      "viewWidth": Int(view.bounds.width), "viewHeight": Int(view.bounds.height),
    ])
  }

  private func addBackdrop() {
    let gridPath = CGMutablePath()
    gridPath.move(to: CGPoint(x: 0, y: 122)); gridPath.addLine(to: CGPoint(x: arenaWidth, y: 122))
    for x in stride(from: CGFloat(0), through: arenaWidth, by: 96) {
      gridPath.move(to: CGPoint(x: arenaWidth / 2, y: 122)); gridPath.addLine(to: CGPoint(x: x, y: 0))
    }
    for y in stride(from: CGFloat(22), through: 100, by: 22) {
      gridPath.move(to: CGPoint(x: 0, y: y)); gridPath.addLine(to: CGPoint(x: arenaWidth, y: y))
    }
    let grid = SKShapeNode(path: gridPath)
    grid.fillColor = .clear; grid.strokeColor = accent.withAlphaComponent(0.24); grid.lineWidth = 1
    addChild(grid)
    for radius in stride(from: CGFloat(90), through: 360, by: 90) {
      let orbit = SKShapeNode(ellipseOf: CGSize(width: radius * 2, height: radius * 0.72))
      orbit.fillColor = .clear; orbit.strokeColor = accent.withAlphaComponent(0.10); orbit.lineWidth = 1
      orbit.position = CGPoint(x: arenaWidth / 2, y: 410); addChild(orbit)
    }
  }

  private func addDesktopStage() {
    let platform = SKShapeNode(rectOf: CGSize(width: desktopStage.upperBound - desktopStage.lowerBound, height: 5), cornerRadius: 2.5)
    platform.fillColor = pink; platform.strokeColor = ink; platform.lineWidth = 7
    platform.position = CGPoint(x: arenaWidth / 2, y: desktopFloor - 43)
    platform.zPosition = -1; addChild(platform)
  }

  private func showMenu(_ show: Bool) {
    menuOpen = show; menuCard.isHidden = !show
    status.text = show ? "CLICK TRAIN  •  ESC closes menu" : "TRACKPAD: MOVE • TAP PUNCHES • 2-FINGER CLICK HEAVY • SWIPE UP JUMPS"
  }

  private func resetRound(trigger: String, beginNewLifetime: Bool) {
    if beginNewLifetime {
      roundID = lifetime.beginRound(trigger: trigger, mode: desktopMode ? "desktop" : "windowed", launchID: launchID)
    }
    let removedShots = shots.count
    p1.health = 100; p2.health = 100
    let y = desktopMode ? desktopFloor : 122
    p1.root.position = CGPoint(x: desktopMode ? 410 : 390, y: y)
    p2.root.position = CGPoint(x: desktopMode ? 870 : 890, y: y)
    p1.velocity = .zero; p2.velocity = .zero; roundOver = false; updateBars()
  }

  override func update(_ currentTime: TimeInterval) {
    if introMode, !introFinished {
      updateIntro(currentTime)
      lastTime = currentTime
      return
    }
    if lastTime == 0 { lastTime = currentTime; return }
    accumulator += min(0.1, currentTime - lastTime); lastTime = currentTime
    while accumulator >= fixedStep { tick(); accumulator -= fixedStep }
  }

  private func beginIntro() {
    introProgress = 0; introLastTime = 0; introFinished = false
    introCancelling = false; introNextBeep = 0; menuOpen = false
    buildIntroWarp()
    lifetime.event("intro_started", roundID: roundID, fields: [
      "sceneId": sceneID, "durationMs": Int(cornerHoldDuration * 1000),
    ])
    renderIntro()
  }

  private func updateIntro(_ currentTime: TimeInterval) {
    if let path = introCancelPath,
       FileManager.default.fileExists(atPath: path), !introCancelling {
      introCancelling = true
      lifetime.event("intro_cancel_requested", roundID: roundID, fields: [
        "sceneId": sceneID, "progress": Double(introProgress),
      ])
      tone.cancel()
      tone.play(frequency: 494, endFrequency: 294, duration: 0.12, volume: 0.12)
    }
    if introLastTime == 0 { introLastTime = currentTime }
    let delta = min(0.05, currentTime - introLastTime); introLastTime = currentTime
    introProgress = min(1, max(0, introProgress + CGFloat(delta / cornerHoldDuration) * (introCancelling ? -1 : 1)))

    if !introCancelling {
      let beepAt: [CGFloat] = [0, 0.30, 0.60]
      let notes = [392.0, 494.0, 659.0]
      while introNextBeep < beepAt.count, introProgress >= beepAt[introNextBeep] {
        let note = notes[introNextBeep]
        tone.play(frequency: note, endFrequency: note * 1.035, duration: 0.075, volume: 0.13)
        introNextBeep += 1
      }
    }
    renderIntro()

    if introCancelling, introProgress <= 0 {
      if let path = introCancelPath { try? FileManager.default.removeItem(atPath: path) }
      lifetime.finishRound("intro_cancelled", roundID: roundID, fields: ["sceneId": sceneID])
      NSApp.terminate(nil)
    } else if !introCancelling, introProgress >= 1 {
      introFinished = true
      introFlash.removeFromParent()
      settleWarpStars()
      resetRound(trigger: "intro_completed", beginNewLifetime: false); showMenu(false)
      lifetime.event("intro_completed", roundID: roundID, fields: ["sceneId": sceneID])
      lifetime.event("combat_started", roundID: roundID, fields: ["sceneId": sceneID, "trigger": "intro_completed"])
      tone.play(frequency: 784, endFrequency: 1046, duration: 0.16, volume: 0.18)
    }
  }

  private func renderIntro() {
    let stopT = smooth(min(1, introProgress / 0.82))

    introFlash.alpha = max(0, 0.34 * (1 - introProgress / 0.16))
    let center = CGPoint(x: arenaWidth / 2, y: arenaHeight / 2)
    for star in introWarpStars {
      let localT = min(1, max(0, stopT * 1.20 - star.phase * 0.20))
      let travel = pow(localT, 1.55)
      star.node.position = CGPoint(x: center.x + star.vector.dx * travel,
                                   y: center.y + star.vector.dy * travel)
      star.node.zRotation = atan2(star.vector.dy, star.vector.dx)
      star.node.xScale = 0.45 + localT * 1.45
      let arrivalFade = introProgress > 0.82 ? max(0, (1 - introProgress) / 0.18) : 1
      star.node.alpha = min(1, introProgress * 10) * arrivalFade
    }

    let destinations: [(Fighter, CGFloat, CGFloat)] = [(p1, arenaWidth / 2, -1), (p2, 1040, 1)]
    for (fighter, x, side) in destinations {
      fighter.root.position = CGPoint(x: x + side * (1 - stopT) * 18,
                                      y: desktopFloor + (1 - stopT) * 8)
      fighter.root.setScale(fighter.visualScale * (0.90 + stopT * 0.10))
      fighter.root.alpha = 0.82 + stopT * 0.18
    }
    status.alpha = 0
    status.text = ""
  }

  private func buildIntroWarp() {
    for star in introWarpStars { star.node.removeFromParent() }
    introWarpStars.removeAll(keepingCapacity: true)
    introFlash.removeFromParent()
    introFlash.position = CGPoint(x: arenaWidth / 2, y: arenaHeight / 2)
    introFlash.fillColor = accent; introFlash.strokeColor = .clear; introFlash.zPosition = -8
    addChild(introFlash)

    for index in 0..<48 {
      let corner = index % 4
      let sx: CGFloat = corner == 0 || corner == 2 ? -1 : 1
      let sy: CGFloat = corner < 2 ? -1 : 1
      let lane = CGFloat((index * 29) % 17 - 8) / 8
      let vector = CGVector(dx: sx * (arenaWidth * (0.42 + abs(lane) * 0.07)),
                            dy: sy * (arenaHeight * (0.40 + abs(lane) * 0.08)) + lane * 62)
      let path = CGMutablePath()
      path.move(to: CGPoint(x: -7, y: 0)); path.addLine(to: .zero)
      let node = SKShapeNode(path: path)
      node.strokeColor = contrastDark; node.lineWidth = lineDark
      let lightTrace = SKShapeNode(path: path)
      lightTrace.name = "warp-light"; lightTrace.strokeColor = contrastLight
      lightTrace.lineWidth = lineLight
      let accentTrace = SKShapeNode(path: path)
      accentTrace.name = "warp-accent"; accentTrace.strokeColor = accent
      accentTrace.lineWidth = lineSpot
      node.addChild(lightTrace); node.addChild(accentTrace)
      node.zPosition = -5
      addChild(node)
      introWarpStars.append((node, vector, CGFloat((index * 37) % 100) / 100))
    }
  }

  private func settleWarpStars() {
    for (index, star) in introWarpStars.enumerated() {
      let radius: CGFloat = index % 7 == 0 ? 2.2 : 1.35
      let path = CGMutablePath()
      path.move(to: CGPoint(x: -radius, y: 0)); path.addLine(to: CGPoint(x: radius, y: 0))
      path.move(to: CGPoint(x: 0, y: -radius)); path.addLine(to: CGPoint(x: 0, y: radius))
      star.node.path = path
      (star.node.childNode(withName: "warp-light") as? SKShapeNode)?.path = path
      (star.node.childNode(withName: "warp-accent") as? SKShapeNode)?.path = path
      star.node.zRotation = 0; star.node.xScale = 1
      star.node.alpha = index % 5 == 0 ? 0.90 : 0.68
    }
  }

  private func smooth(_ value: CGFloat) -> CGFloat {
    value * value * (3 - 2 * value)
  }

  private func tick() {
    guard !menuOpen, !roundOver else { return }
    let now = input()
    if online { matchmaker?.sendInput(now[0].mask) }
    if let damage = p1.tick(buttons: edges(now[0], previous[0]), opponentX: p2.root.position.x) { strike(from: p1, defender: p2, damage: damage) }
    let second = online ? remoteButtons : now[1]
    if let damage = p2.tick(buttons: edges(second, previous[1]), opponentX: p1.root.position.x) { strike(from: p2, defender: p1, damage: damage) }
    previous = [now[0], second]
    particleFrame += 1
    if particleFrame % 2 == 0 {
      emitThrust(from: p1); emitThrust(from: p2)
    }
    separate()
    checkRingOut()
    updateBars()
  }

  private func edges(_ current: Buttons, _ old: Buttons) -> Buttons {
    var out = current; out.light = current.light && !old.light; out.heavy = current.heavy && !old.heavy; return out
  }

  private func input() -> [Buttons] {
    var a = Buttons(left: keys.contains(0), right: keys.contains(2), jump: keys.contains(13), light: keys.contains(3), heavy: keys.contains(5))
    var b = Buttons(left: keys.contains(123), right: keys.contains(124), jump: keys.contains(126), light: keys.contains(44), heavy: keys.contains(47))
    if let target = pointerX, !desktopMode {
      a.left = a.left || target < p1.root.position.x - 18
      a.right = a.right || target > p1.root.position.x + 18
    }
    a.light = a.light || pointerLight; a.heavy = a.heavy || pointerHeavy; a.jump = a.jump || pointerJump
    pointerLight = false; pointerHeavy = false; pointerJump = false
    if controllerRefreshFrames <= 0 {
      cachedControllers = GCController.controllers()
      controllerRefreshFrames = 60
    }
    controllerRefreshFrames -= 1
    for (i, controller) in cachedControllers.prefix(2).enumerated() {
      guard let g = controller.extendedGamepad else { continue }
      let x = g.leftThumbstick.xAxis.value
      if i == 0 { a.left = a.left || x < -0.3 || g.dpad.left.isPressed; a.right = a.right || x > 0.3 || g.dpad.right.isPressed; a.jump = a.jump || g.buttonA.isPressed; a.light = a.light || g.buttonX.isPressed; a.heavy = a.heavy || g.buttonY.isPressed }
      else { b.left = b.left || x < -0.3 || g.dpad.left.isPressed; b.right = b.right || x > 0.3 || g.dpad.right.isPressed; b.jump = b.jump || g.buttonA.isPressed; b.light = b.light || g.buttonX.isPressed; b.heavy = b.heavy || g.buttonY.isPressed }
    }
    return [a, b]
  }

  private func separate() {
    let dx = p2.root.position.x - p1.root.position.x
    let dy = p2.root.position.y - p1.root.position.y
    let distance = max(0.001, hypot(dx, dy))
    let minimum = 82 * max(p1.visualScale, p2.visualScale)
    if distance < minimum {
      let push = (minimum - distance) / 2
      p1.root.position.x -= dx / distance * push; p1.root.position.y -= dy / distance * push
      p2.root.position.x += dx / distance * push; p2.root.position.y += dy / distance * push
    }
  }

  private func strike(from fighter: Fighter, defender: Fighter, damage: Int) {
    let heavy = damage > 8
    tone.play(frequency: heavy ? 115 : 230, duration: heavy ? 0.08 : 0.035,
              volume: heavy ? 0.22 : 0.12)
    guard fighter.attackbox.intersects(defender.hitbox), !fighter.hitThisAttack else { return }
    fighter.hitThisAttack = true
    defender.health = max(0, defender.health - CGFloat(damage))
    defender.velocity.dx = fighter.facing * (heavy ? 17 : 9)
    defender.velocity.dy = heavy ? 10 : 5
    defender.grounded = false; defender.flashFrames = 8
    tone.play(frequency: heavy ? 72 : 105, duration: 0.09, volume: 0.25)
    if defender.health == 0 {
      roundOver = true
      status.text = "PLAYER \(fighter.number) WINS  •  RETURN TO REMATCH"
      tone.play(frequency: 440, duration: 0.35, volume: 0.22)
    }
  }

  private func hitAsteroid(with shot: Shot) -> Bool {
    guard let index = asteroids.firstIndex(where: { shot.hitbox.intersects($0.hitbox) }) else { return false }
    let asteroid = asteroids[index]
    asteroid.health -= shot.damage > 8 ? 2 : 1
    shot.node.removeFromParent()
    if asteroid.health <= 0 {
      asteroid.node.removeFromParent(); asteroids.remove(at: index)
      lifetime.event("asteroid_destroyed", roundID: roundID, fields: [
        "sceneId": sceneID, "owner": shot.owner, "asteroidsRemaining": asteroids.count,
      ])
      tone.play(frequency: 220, endFrequency: 110, duration: 0.10, volume: 0.13)
    } else {
      asteroid.node.run(.sequence([.fadeAlpha(to: 0.25, duration: 0.04), .fadeAlpha(to: 1, duration: 0.08)]))
    }
    return true
  }

  private func updateAsteroids() {
    guard desktopMode else { return }
    for player in [1, 2] where (asteroidImpactCooldown[player] ?? 0) > 0 {
      asteroidImpactCooldown[player, default: 0] -= 1
    }
    for asteroid in asteroids {
      asteroid.node.position.x += asteroid.velocity.dx
      asteroid.node.position.y += asteroid.velocity.dy
      asteroid.node.zRotation += 0.0025
      if asteroid.node.position.x < -55 { asteroid.node.position.x = arenaWidth + 55 }
      if asteroid.node.position.x > arenaWidth + 55 { asteroid.node.position.x = -55 }
      if asteroid.node.position.y < -55 { asteroid.node.position.y = arenaHeight + 55 }
      if asteroid.node.position.y > arenaHeight + 55 { asteroid.node.position.y = -55 }
      for fighter in [p1, p2] where (asteroidImpactCooldown[fighter.number] ?? 0) == 0 {
        guard asteroid.hitbox.intersects(fighter.hitbox) else { continue }
        let dx = fighter.root.position.x - asteroid.node.position.x
        let dy = fighter.root.position.y - asteroid.node.position.y
        let distance = max(1, hypot(dx, dy))
        fighter.velocity.dx += dx / distance * 8
        fighter.velocity.dy += dy / distance * 8
        fighter.flashFrames = 10
        asteroidImpactCooldown[fighter.number] = 40
        lifetime.event("asteroid_collision", roundID: roundID, fields: [
          "sceneId": sceneID, "player": fighter.number,
        ])
        tone.play(frequency: 170, endFrequency: 105, duration: 0.08, volume: 0.12)
      }
    }
  }

  private func updateBars() {
    guard !desktopMode else { return }
    p1Bar.removeFromParent(); p2Bar.removeFromParent()
    let w: CGFloat = 430
    p1Bar.path = CGPath(rect: CGRect(x: 0, y: 0, width: w * p1.health / 100, height: 24), transform: nil)
    p2Bar.path = CGPath(rect: CGRect(x: 0, y: 0, width: w * p2.health / 100, height: 24), transform: nil)
    p1Bar.fillColor = accent.withAlphaComponent(0.18); p2Bar.fillColor = .clear
    p1Bar.strokeColor = accent; p2Bar.strokeColor = accent; p1Bar.lineWidth = 1.5; p2Bar.lineWidth = 1.5
    p1Bar.position = CGPoint(x: 54, y: arenaHeight - 116); p2Bar.position = CGPoint(x: arenaWidth - 54 - w, y: arenaHeight - 116); addChild(p1Bar); addChild(p2Bar)
  }

  override func keyDown(with event: NSEvent) {
    guard introFinished else { return }
    if event.keyCode == 53 {
      if desktopMode {
        if roundOver {
          lifetime.event("exit_after_round", roundID: roundID, fields: ["sceneId": sceneID, "input": "escape"])
        } else {
          lifetime.finishRound("quit", roundID: roundID, fields: ["sceneId": sceneID, "input": "escape"])
        }
        NSApp.terminate(nil)
      }
      else { showMenu(!menuOpen) }
      return
    }
    if event.keyCode == 36 && (menuOpen || roundOver) {
      startRematch(trigger: roundOver ? "return_rematch" : "return_restart")
      tone.play(frequency: 660, endFrequency: 820, duration: 0.08); return
    }
    keys.insert(event.keyCode)
  }
  override func keyUp(with event: NSEvent) { keys.remove(event.keyCode) }

  override func mouseMoved(with event: NSEvent) { updatePointer(event) }
  override func mouseDragged(with event: NSEvent) { updatePointer(event) }
  override func rightMouseDragged(with event: NSEvent) { updatePointer(event) }

  private func updatePointer(_ event: NSEvent) {
    guard introFinished else { return }
    pointerX = convertPoint(fromView: event.locationInWindow).x
  }

  override func mouseDown(with event: NSEvent) {
    guard introFinished, !previewSling else { return }
    updatePointer(event)
    if menuOpen { startRematch(trigger: "pointer_restart"); tone.play(frequency: 660, endFrequency: 820, duration: 0.08) }
    else if roundOver { startRematch(trigger: "pointer_rematch") }
    else { pointerLight = true }
  }

  override func rightMouseDown(with event: NSEvent) {
    guard introFinished, !previewSling else { return }
    updatePointer(event)
    if !menuOpen, !roundOver { pointerHeavy = true }
  }

  override func scrollWheel(with event: NSEvent) {
    guard introFinished, !previewSling else { return }
    guard !desktopMode else { return }
    updatePointer(event)
    if event.scrollingDeltaY > 2 { pointerJump = true }
  }

}

private final class FighterWindow: NSWindow {
  override var canBecomeKey: Bool { true }
  override var canBecomeMain: Bool { true }
}

private func makeFighterCursor() -> NSCursor {
  let side: CGFloat = 28
  let image = NSImage(size: NSSize(width: side, height: side), flipped: false) { rect in
    NSColor.clear.setFill(); rect.fill()
    let center = NSPoint(x: rect.midX, y: rect.midY)
    let path = NSBezierPath(ovalIn: NSRect(x: center.x - 6, y: center.y - 6, width: 12, height: 12))
    for (a, b) in [(0.0, 4.0), (10.0, 14.0)] {
      path.move(to: NSPoint(x: center.x + CGFloat(a), y: center.y))
      path.line(to: NSPoint(x: center.x + CGFloat(b), y: center.y))
      path.move(to: NSPoint(x: center.x - CGFloat(a), y: center.y))
      path.line(to: NSPoint(x: center.x - CGFloat(b), y: center.y))
      path.move(to: NSPoint(x: center.x, y: center.y + CGFloat(a)))
      path.line(to: NSPoint(x: center.x, y: center.y + CGFloat(b)))
      path.move(to: NSPoint(x: center.x, y: center.y - CGFloat(a)))
      path.line(to: NSPoint(x: center.x, y: center.y - CGFloat(b)))
    }
    path.lineCapStyle = .round
    ink.withAlphaComponent(0.92).setStroke(); path.lineWidth = 3.5; path.stroke()
    accent.setStroke(); path.lineWidth = 1.5; path.stroke()
    accent.setFill(); NSBezierPath(ovalIn: NSRect(x: center.x - 1.5, y: center.y - 1.5, width: 3, height: 3)).fill()
    return true
  }
  return NSCursor(image: image, hotSpot: NSPoint(x: side / 2, y: side / 2))
}

private final class FighterView: SKView {
  var capturesCursor = false
  private lazy var fighterCursor = makeFighterCursor()

  override func resetCursorRects() {
    super.resetCursorRects()
    if capturesCursor { addCursorRect(bounds, cursor: fighterCursor) }
  }
}

private final class EventShieldView: NSView {
  override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
  override func mouseDown(with event: NSEvent) {}
  override func mouseUp(with event: NSEvent) {}
  override func rightMouseDown(with event: NSEvent) {}
  override func rightMouseUp(with event: NSEvent) {}
  override func otherMouseDown(with event: NSEvent) {}
  override func otherMouseUp(with event: NSEvent) {}
  override func mouseMoved(with event: NSEvent) {}
  override func mouseDragged(with event: NSEvent) {}
  override func rightMouseDragged(with event: NSEvent) {}
  override func otherMouseDragged(with event: NSEvent) {}
  override func scrollWheel(with event: NSEvent) {}
}

@MainActor private final class AppDelegate: NSObject, NSApplicationDelegate {
  private let lifetime = RoundLifetimeLog.shared
  private var window: NSWindow!
  private var shieldWindows: [NSPanel] = []
  private var trackpadInputStarted = false
  private var desktopSessionActive = false
  private var cursorHidden = false
  func applicationDidFinishLaunching(_ notification: Notification) {
    let args = CommandLine.arguments
    let slingPreview = args.contains("--preview-sling")
    let previewMode = args.contains("--preview-gesture") || slingPreview
    let desktopMode = args.contains("--desktop") || previewMode
    let introMode = args.contains("--intro") || previewMode
    let cancelIndex = args.firstIndex(of: "--intro-cancel")
    let introCancelPath = cancelIndex.flatMap { $0 + 1 < args.count ? args[$0 + 1] : nil }
    let launchIndex = args.firstIndex(of: "--launch-id")
    let launchID = launchIndex.flatMap { $0 + 1 < args.count ? args[$0 + 1] : nil }
    let mode = previewMode ? "preview" : (desktopMode ? "desktop" : "windowed")
    desktopSessionActive = desktopMode
    if desktopMode && !previewMode {
      trackpadInputStarted = MFStartTrackpadInputWatcher(trackpadInputChanged)
    }
    lifetime.event("application_initialized", fields: [
      "mode": mode, "intro": introMode, "launchId": launchID ?? NSNull(),
      "logPath": lifetime.path, "trackpadInput": trackpadInputStarted,
    ])
    let roundID = lifetime.beginRound(trigger: introMode ? "intro_launch" : "application_launch",
                                      mode: mode, launchID: launchID)
    let sceneID = UUID().uuidString.lowercased()
    let screenFrame = pointerScreen()?.frame ?? NSRect(x: 0, y: 0, width: 1280, height: 720)
    let viewFrame = desktopMode
      ? NSRect(origin: .zero, size: screenFrame.size)
      : NSRect(x: 0, y: 0, width: windowWidth, height: windowHeight)
    let view = FighterView(frame: viewFrame)
    view.capturesCursor = desktopMode
    view.autoresizingMask = [.width, .height]
    view.preferredFramesPerSecond = 60; view.ignoresSiblingOrder = true
    view.allowsTransparency = desktopMode
    let scene = FightScene(size: CGSize(width: arenaWidth, height: arenaHeight), desktopMode: desktopMode,
                           introMode: introMode, previewSling: slingPreview, introCancelPath: introCancelPath,
                           roundID: roundID, sceneID: sceneID, launchID: launchID)
    // Fill the whole desktop without distorting the fighters. On displays that
    // are not 16:9 SpriteKit crops the scene's quiet outer edges instead of
    // adding letterbox bars.
    scene.scaleMode = desktopMode ? .aspectFill : .aspectFit
    view.presentScene(scene)
    window = FighterWindow(contentRect: desktopMode ? screenFrame : view.frame,
                           styleMask: desktopMode ? [.borderless] : [.titled, .closable, .miniaturizable, .resizable],
                           backing: .buffered, defer: false)
    window.title = "Trackpad Fighter"; window.contentView = view
    if desktopMode {
      window.isOpaque = false; window.backgroundColor = .clear; window.hasShadow = false
      // The transparent window is also the input shield: clicks and gestures
      // stop here instead of activating whatever happens to be underneath.
      window.level = playWindowLevel; window.ignoresMouseEvents = false
      window.acceptsMouseMovedEvents = true
      window.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]
    } else {
      window.contentAspectRatio = NSSize(width: 16, height: 9)
      window.minSize = NSSize(width: 320, height: 180)
      window.acceptsMouseMovedEvents = true
    }
    if desktopMode { window.setFrame(screenFrame, display: true) }
    else { window.center() }
    window.makeKeyAndOrderFront(nil)
    window.makeFirstResponder(view)
    NSApp.activate(ignoringOtherApps: true)
    if desktopMode {
      installInputShields(excluding: screenFrame)
      NSCursor.hide(); cursorHidden = true
    }
    window.invalidateCursorRects(for: view)
    lifetime.event("window_presented", roundID: roundID, fields: [
      "sceneId": sceneID, "mode": mode, "windowNumber": window.windowNumber,
      "screenX": Int(screenFrame.origin.x), "screenY": Int(screenFrame.origin.y),
      "screenWidth": Int(screenFrame.width), "screenHeight": Int(screenFrame.height),
      "inputShields": shieldWindows.count,
    ])
  }

  private func installInputShields(excluding gameFrame: NSRect) {
    for screen in NSScreen.screens where screen.frame != gameFrame {
      let panel = NSPanel(contentRect: screen.frame, styleMask: [.borderless, .nonactivatingPanel],
                          backing: .buffered, defer: false)
      panel.contentView = EventShieldView(frame: NSRect(origin: .zero, size: screen.frame.size))
      panel.isOpaque = false; panel.backgroundColor = .clear; panel.hasShadow = false
      panel.level = playWindowLevel; panel.ignoresMouseEvents = false
      panel.acceptsMouseMovedEvents = true
      panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]
      panel.orderFrontRegardless(); shieldWindows.append(panel)
    }
  }

  func applicationDidResignActive(_ notification: Notification) {
    guard desktopSessionActive else { return }
    window?.makeKeyAndOrderFront(nil)
    NSApp.activate(ignoringOtherApps: true)
  }

  func applicationWillTerminate(_ notification: Notification) {
    desktopSessionActive = false
    if trackpadInputStarted { MFStopCornerPoseWatcher() }
    for shield in shieldWindows { shield.orderOut(nil) }
    shieldWindows.removeAll()
    lifetime.event("application_terminating")
    lifetime.finishProcess(reason: "application_terminated")
    if cursorHidden { NSCursor.unhide(); cursorHidden = false }
    NSCursor.arrow.set()
  }
  func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

if CommandLine.arguments.count == 3, CommandLine.arguments[1] == "auth" {
  let ok = NativeMatchmaker.saveToken(CommandLine.arguments[2])
  print(ok ? "Trackpad Fighter sign-in saved in Keychain." : "Could not save Trackpad Fighter sign-in.")
  exit(ok ? 0 : 1)
}

if CommandLine.arguments.contains("--watch") {
  setbuf(stdout, nil)
  setbuf(stderr, nil)
  let watcherApp = NSApplication.shared
  watcherApp.setActivationPolicy(.accessory)
  CornerWatcher.shared.start()
  watcherApp.run()
  exit(0)
}

private let app = NSApplication.shared
private let delegate = AppDelegate(); app.delegate = delegate; app.setActivationPolicy(.regular); app.run()
