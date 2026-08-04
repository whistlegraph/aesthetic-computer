import AppKit
import AVFoundation
import GameController
import SpriteKit
import TrackpadBridge

private let fixedStep = 1.0 / 60.0
private let arenaWidth: CGFloat = 1280
private let arenaHeight: CGFloat = 720
private let windowWidth: CGFloat = 420
private let windowHeight: CGFloat = 236.25
private let ink = NSColor(calibratedRed: 0.035, green: 0.025, blue: 0.055, alpha: 1)
private let pink = NSColor(calibratedRed: 1, green: 0.22, blue: 0.49, alpha: 1)

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
  private var voicePlayer: AVAudioPlayer?

  init() {
    engine.attach(player)
    engine.connect(player, to: engine.mainMixerNode, format: format)
    try? engine.start()
  }

  func play(frequency: Double, duration: Double, volume: Float = 0.18) {
    let frames = AVAudioFrameCount(48_000 * duration)
    guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
          let samples = buffer.floatChannelData?[0] else { return }
    buffer.frameLength = frames
    for i in 0..<Int(frames) {
      let fade = min(1, Float(i) / 90) * max(0, 1 - Float(i) / Float(frames))
      samples[i] = sin(Float(i) * 2 * .pi * Float(frequency) / 48_000) * volume * fade
    }
    player.scheduleBuffer(buffer)
    if !player.isPlaying { player.play() }
  }

  func noise(duration: Double = 0.16, volume: Float) {
    let frames = AVAudioFrameCount(48_000 * duration)
    guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames), let samples = buffer.floatChannelData?[0] else { return }
    buffer.frameLength = frames
    var seed: UInt32 = 0x51ab_cafe
    for i in 0..<Int(frames) {
      seed = 1_664_525 &* seed &+ 1_013_904_223
      let white = Float(Int32(bitPattern: seed)) / Float(Int32.max)
      let edge = min(1, Float(i) / 80) * max(0, 1 - Float(i) / Float(frames))
      samples[i] = white * volume * edge
    }
    player.scheduleBuffer(buffer); if !player.isPlaying { player.play() }
  }

  func cancel() { player.stop() }

  func playJeffreyCountdown() {
    let path = NSString(string: "~/.local/share/menu-fighter/jeffrey-count-in.wav").expandingTildeInPath
    guard let audio = try? AVAudioPlayer(contentsOf: URL(fileURLWithPath: path)) else { return }
    voicePlayer = audio; audio.play()
  }

  func cancelVoice() { voicePlayer?.stop() }
}

private func cornerPoseChanged(_ active: Bool) {
  Task { @MainActor in CornerWatcher.shared.pose(active) }
}

private final class CornerCountdownView: NSView {
  var progress: CGFloat = 0 { didSet { needsDisplay = true } }
  override func draw(_ dirtyRect: NSRect) {
    NSColor.clear.setFill(); dirtyRect.fill()
    let box = bounds.insetBy(dx: 22, dy: 22)
    pink.setStroke()
    let path = NSBezierPath(); path.lineWidth = 7; let arm: CGFloat = 42
    for (x, y, sx, sy) in [(box.minX, box.minY, 1.0, 1.0), (box.maxX, box.minY, -1.0, 1.0),
                            (box.minX, box.maxY, 1.0, -1.0), (box.maxX, box.maxY, -1.0, -1.0)] {
      path.move(to: NSPoint(x: x + CGFloat(sx) * arm, y: y)); path.line(to: NSPoint(x: x, y: y)); path.line(to: NSPoint(x: x, y: y + CGFloat(sy) * arm))
    }
    path.stroke()
    let remaining = max(0, 5 - progress * 5)
    let text = progress < 0.4 ? "HOLD" : "\(max(1, Int(ceil(remaining))))"
    let attrs: [NSAttributedString.Key: Any] = [.font: NSFont.systemFont(ofSize: 46, weight: .black), .foregroundColor: pink]
    let size = text.size(withAttributes: attrs)
    text.draw(at: NSPoint(x: bounds.midX - size.width / 2, y: bounds.midY - size.height / 2), withAttributes: attrs)
  }
}

@MainActor private final class CornerWatcher {
  static let shared = CornerWatcher()
  private let tone = Tone()
  private var began: TimeInterval?
  private var timer: Timer?
  private var launched = false
  private var panel: NSPanel?
  private var countdownView: CornerCountdownView?
  private var spokeCountdown = false

  func start() {
    guard MFStartCornerPoseWatcher(cornerPoseChanged) else {
      fputs("menu-fighter: no MultitouchSupport trackpad found\n", stderr); exit(1)
    }
    print("[menu-fighter] four-corner watcher ready")
  }

  func pose(_ active: Bool) {
    if active {
      guard began == nil, !launched else { return }
      began = ProcessInfo.processInfo.systemUptime
      showCountdown(); spokeCountdown = false
      timer = Timer.scheduledTimer(withTimeInterval: 0.2, repeats: true) { [weak self] _ in
        Task { @MainActor in self?.advance() }
      }
      advance()
    } else if began != nil {
      began = nil; timer?.invalidate(); timer = nil; tone.cancel()
      panel?.orderOut(nil); tone.cancelVoice()
      print("[menu-fighter] corner hold cancelled")
    }
  }

  private func advance() {
    guard let began else { return }
    let elapsed = ProcessInfo.processInfo.systemUptime - began
    countdownView?.progress = min(1, elapsed / 5)
    if elapsed >= 2, !spokeCountdown { spokeCountdown = true; tone.playJeffreyCountdown() }
    tone.noise(volume: Float(0.018 + min(1, elapsed / 5) * 0.12))
    guard elapsed >= 5 else { return }
    self.began = nil; timer?.invalidate(); timer = nil; launched = true
    panel?.orderOut(nil)
    tone.play(frequency: 520, duration: 0.14, volume: 0.24)
    let process = Process(); process.executableURL = URL(fileURLWithPath: CommandLine.arguments[0])
    process.arguments = ["--desktop", "--searching"]
    try? process.run()
    DispatchQueue.main.asyncAfter(deadline: .now() + 1) { self.launched = false }
  }

  private func showCountdown() {
    if panel == nil {
      let rect = NSRect(x: 0, y: 0, width: 300, height: 170)
      let view = CornerCountdownView(frame: rect)
      let created = NSPanel(contentRect: rect, styleMask: [.borderless], backing: .buffered, defer: false)
      created.contentView = view; created.isOpaque = false; created.backgroundColor = .clear
      created.hasShadow = false; created.level = .floating; created.ignoresMouseEvents = true
      created.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
      panel = created; countdownView = view
    }
    countdownView?.progress = 0; panel?.center(); panel?.orderFrontRegardless()
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
  var platformRange: ClosedRange<CGFloat> = 70...(arenaWidth - 70)
  var allowsRingOut = false
  var visualScale: CGFloat = 1

  init(number: Int, inverted: Bool, facing: CGFloat) {
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
    attackFrames = heavy ? 16 : 10; cooldown = heavy ? 29 : 18; hitThisAttack = false
    return true
  }

  func tick(buttons: Buttons, opponentX: CGFloat) -> Int? {
    facing = opponentX >= root.position.x ? 1 : -1
    let desired: CGFloat = buttons.left ? -7 : buttons.right ? 7 : 0
    velocity.dx += (desired - velocity.dx) * (grounded ? 0.34 : 0.12)
    if buttons.jump && grounded { velocity.dy = 17; grounded = false }
    var fired: Int?
    if buttons.light, beginAttack(heavy: false) { fired = 8 }
    if buttons.heavy, beginAttack(heavy: true) { fired = 15 }
    if allowsRingOut, grounded, !platformRange.contains(root.position.x) { grounded = false }
    velocity.dy -= grounded ? 0 : 0.92
    let nextX = root.position.x + velocity.dx
    root.position.x = allowsRingOut ? min(arenaWidth + 80, max(-80, nextX)) : min(arenaWidth - 70, max(70, nextX))
    root.position.y += velocity.dy
    if root.position.y <= floorY, velocity.dy <= 0, platformRange.contains(root.position.x), root.position.y > floorY - 36 {
      root.position.y = floorY; velocity.dy = 0; grounded = true
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
}

private final class FightScene: SKScene {
  private let desktopMode: Bool
  private let searching: Bool
  private var matchmaker: NativeMatchmaker?
  private var online = false
  private var remoteButtons = Buttons()
  private var loginNeeded = false
  private let tone = Tone()
  private let p1 = Fighter(number: 1, inverted: false, facing: 1)
  private let p2 = Fighter(number: 2, inverted: true, facing: -1)
  private let p1Bar = SKShapeNode(), p2Bar = SKShapeNode()
  private let title = SKLabelNode(fontNamed: "AvenirNext-Heavy")
  private let status = SKLabelNode(fontNamed: "AvenirNext-Bold")
  private let loginButton = SKShapeNode(rectOf: CGSize(width: 250, height: 52), cornerRadius: 18)
  private let menuCard = SKShapeNode(rectOf: CGSize(width: 450, height: 290), cornerRadius: 34)
  private let train = SKLabelNode(fontNamed: "AvenirNext-Heavy")
  private let find = SKLabelNode(fontNamed: "AvenirNext-Heavy")
  private var keys = Set<UInt16>()
  private var previous = [Buttons(), Buttons()]
  private var accumulator = 0.0, lastTime = 0.0
  private var menuOpen = true, roundOver = false
  private var pointerX: CGFloat?
  private var pointerLight = false, pointerHeavy = false, pointerJump = false
  private let desktopStage: ClosedRange<CGFloat> = 44...1236
  private let desktopFloor: CGFloat = 78

  init(size: CGSize, desktopMode: Bool = false, searching: Bool = false) {
    self.desktopMode = desktopMode; self.searching = searching
    super.init(size: size)
  }

  required init?(coder aDecoder: NSCoder) { fatalError("init(coder:) has not been implemented") }

  override func didMove(to view: SKView) {
    backgroundColor = desktopMode ? .clear : ink
    if !desktopMode { addBackdrop() } else { addDesktopStage() }
    if desktopMode {
      for fighter in [p1, p2] {
        fighter.floorY = desktopFloor; fighter.platformRange = desktopStage; fighter.allowsRingOut = true
        fighter.visualScale = 0.72; fighter.root.setScale(fighter.visualScale)
      }
    }
    p1.root.position = CGPoint(x: 390, y: 122); p2.root.position = CGPoint(x: 890, y: 122)
    addChild(p1.root); addChild(p2.root)
    title.text = "MENU FIGHTER"; title.fontSize = 44; title.fontColor = pink
    title.position = CGPoint(x: arenaWidth / 2, y: arenaHeight - 70); if !desktopMode { addChild(title) }
    status.fontSize = desktopMode ? 16 : 22; status.fontColor = pink; status.position = CGPoint(x: arenaWidth / 2, y: desktopMode ? desktopFloor + 55 : 48); status.zPosition = 31; addChild(status)
    loginButton.fillColor = ink; loginButton.strokeColor = pink; loginButton.lineWidth = 4
    loginButton.position = status.position; loginButton.zPosition = 30; loginButton.isHidden = true; addChild(loginButton)
    menuCard.fillColor = ink; menuCard.strokeColor = pink; menuCard.lineWidth = 7
    menuCard.position = CGPoint(x: arenaWidth / 2, y: arenaHeight / 2); menuCard.zPosition = 20; if !desktopMode { addChild(menuCard) }
    train.text = "TRAIN"; train.fontSize = 42; train.fontColor = pink; train.position = CGPoint(x: 0, y: 26); menuCard.addChild(train)
    let trainSub = SKLabelNode(fontNamed: "AvenirNext-Medium"); trainSub.text = "RETURN  •  LOCAL FREEFIGHT"; trainSub.fontSize = 15; trainSub.fontColor = pink; trainSub.position.y = -4; menuCard.addChild(trainSub)
    find.text = "FIND"; find.fontSize = 34; find.fontColor = pink; find.alpha = 0.28; find.position.y = -75; menuCard.addChild(find)
    let planned = SKLabelNode(fontNamed: "AvenirNext-Medium"); planned.text = "ONLINE • COMING NEXT"; planned.fontSize = 13; planned.fontColor = pink; planned.alpha = 0.28; planned.position.y = -101; menuCard.addChild(planned)
    resetRound(); showMenu(!desktopMode)
    if searching { startMatchmaking() }
  }

  private func startMatchmaking() {
    let network = NativeMatchmaker(); matchmaker = network
    network.onStatus = { [weak self] text in
      guard let self else { return }
      status.text = text.hasPrefix("SIGN IN") ? "LOG IN" : text
      loginNeeded = text.hasPrefix("SIGN IN") || text.hasPrefix("LOGIN FAILED")
      loginButton.isHidden = !loginNeeded
    }
    network.onStart = { [weak self] _ in self?.online = true; self?.remoteButtons = Buttons(); self?.resetRound() }
    network.onInput = { [weak self] wire in self?.remoteButtons = Buttons(mask: wire.mask) }
    network.start()
  }

  private func addBackdrop() {
    let floor = SKShapeNode(rect: CGRect(x: 0, y: 0, width: arenaWidth, height: 122))
    floor.fillColor = pink; floor.strokeColor = pink; addChild(floor)
    for i in 0..<12 {
      let stripe = SKShapeNode(rectOf: CGSize(width: 42, height: 500))
      stripe.fillColor = i % 2 == 0 ? pink : ink
      stripe.strokeColor = stripe.fillColor; stripe.position = CGPoint(x: CGFloat(i) * 116, y: 370); stripe.zRotation = -0.14; addChild(stripe)
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

  private func resetRound() {
    p1.health = 100; p2.health = 100
    let y = desktopMode ? desktopFloor : 122
    p1.root.position = CGPoint(x: desktopMode ? 410 : 390, y: y)
    p2.root.position = CGPoint(x: desktopMode ? 870 : 890, y: y)
    p1.velocity = .zero; p2.velocity = .zero; roundOver = false; updateBars()
  }

  override func update(_ currentTime: TimeInterval) {
    if lastTime == 0 { lastTime = currentTime; return }
    accumulator += min(0.1, currentTime - lastTime); lastTime = currentTime
    while accumulator >= fixedStep { tick(); accumulator -= fixedStep }
  }

  private func tick() {
    guard !menuOpen, !roundOver else { return }
    let now = input()
    if online { matchmaker?.sendInput(now[0].mask) }
    if let damage = p1.tick(buttons: edges(now[0], previous[0]), opponentX: p2.root.position.x) { strike(from: p1, defender: p2, damage: damage) }
    let second = online ? remoteButtons : now[1]
    if let damage = p2.tick(buttons: edges(second, previous[1]), opponentX: p1.root.position.x) { strike(from: p2, defender: p1, damage: damage) }
    previous = [now[0], second]
    separate()
    checkRingOut()
    updateBars()
  }

  private func edges(_ current: Buttons, _ old: Buttons) -> Buttons {
    var out = current; out.jump = current.jump && !old.jump; out.light = current.light && !old.light; out.heavy = current.heavy && !old.heavy; return out
  }

  private func input() -> [Buttons] {
    var a = Buttons(left: keys.contains(0), right: keys.contains(2), jump: keys.contains(13), light: keys.contains(3), heavy: keys.contains(5))
    var b = Buttons(left: keys.contains(123), right: keys.contains(124), jump: keys.contains(126), light: keys.contains(44), heavy: keys.contains(47))
    if let target = pointerX {
      a.left = a.left || target < p1.root.position.x - 18
      a.right = a.right || target > p1.root.position.x + 18
    }
    a.light = a.light || pointerLight; a.heavy = a.heavy || pointerHeavy; a.jump = a.jump || pointerJump
    pointerLight = false; pointerHeavy = false; pointerJump = false
    for (i, controller) in GCController.controllers().prefix(2).enumerated() {
      guard let g = controller.extendedGamepad else { continue }
      let x = g.leftThumbstick.xAxis.value
      if i == 0 { a.left = a.left || x < -0.3 || g.dpad.left.isPressed; a.right = a.right || x > 0.3 || g.dpad.right.isPressed; a.jump = a.jump || g.buttonA.isPressed; a.light = a.light || g.buttonX.isPressed; a.heavy = a.heavy || g.buttonY.isPressed }
      else { b.left = b.left || x < -0.3 || g.dpad.left.isPressed; b.right = b.right || x > 0.3 || g.dpad.right.isPressed; b.jump = b.jump || g.buttonA.isPressed; b.light = b.light || g.buttonX.isPressed; b.heavy = b.heavy || g.buttonY.isPressed }
    }
    return [a, b]
  }

  private func separate() {
    let delta = p2.root.position.x - p1.root.position.x
    if abs(delta) < 82 { let push = (82 - abs(delta)) / 2; p1.root.position.x -= push * (delta >= 0 ? 1 : -1); p2.root.position.x += push * (delta >= 0 ? 1 : -1) }
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

  private func checkRingOut() {
    guard desktopMode, !roundOver else { return }
    let loser: Fighter? = p1.root.position.y < -90 ? p1 : (p2.root.position.y < -90 ? p2 : nil)
    guard let loser else { return }
    let winner = loser.number == 1 ? 2 : 1
    roundOver = true
    tone.play(frequency: 440, duration: 0.35, volume: 0.22)
    DispatchQueue.main.asyncAfter(deadline: .now() + 0.8) { NSApp.terminate(nil) }
    print("[menu-fighter] player \(winner) wins by ring-out")
  }

  private func updateBars() {
    guard !desktopMode else { return }
    p1Bar.removeFromParent(); p2Bar.removeFromParent()
    let w: CGFloat = 430
    p1Bar.path = CGPath(rect: CGRect(x: 0, y: 0, width: w * p1.health / 100, height: 24), transform: nil)
    p2Bar.path = CGPath(rect: CGRect(x: 0, y: 0, width: w * p2.health / 100, height: 24), transform: nil)
    p1Bar.fillColor = pink; p2Bar.fillColor = ink; p1Bar.strokeColor = pink; p2Bar.strokeColor = pink; p2Bar.lineWidth = 4
    p1Bar.position = CGPoint(x: 54, y: arenaHeight - 116); p2Bar.position = CGPoint(x: arenaWidth - 54 - w, y: arenaHeight - 116); addChild(p1Bar); addChild(p2Bar)
  }

  override func keyDown(with event: NSEvent) {
    if event.keyCode == 53 { showMenu(!menuOpen); return }
    if event.keyCode == 36 && (menuOpen || roundOver) { resetRound(); showMenu(false); tone.play(frequency: 330, duration: 0.09); return }
    keys.insert(event.keyCode)
  }
  override func keyUp(with event: NSEvent) { keys.remove(event.keyCode) }

  override func mouseMoved(with event: NSEvent) { updatePointer(event) }
  override func mouseDragged(with event: NSEvent) { updatePointer(event) }
  override func rightMouseDragged(with event: NSEvent) { updatePointer(event) }

  private func updatePointer(_ event: NSEvent) {
    pointerX = convertPoint(fromView: event.locationInWindow).x
  }

  override func mouseDown(with event: NSEvent) {
    updatePointer(event)
    if menuOpen { resetRound(); showMenu(false); tone.play(frequency: 330, duration: 0.09) }
    else if roundOver { resetRound() }
    else { pointerLight = true }
  }

  override func rightMouseDown(with event: NSEvent) {
    updatePointer(event)
    if !menuOpen, !roundOver { pointerHeavy = true }
  }

  override func scrollWheel(with event: NSEvent) {
    updatePointer(event)
    if event.scrollingDeltaY > 2 { pointerJump = true }
  }

  func desktopPointer(normalizedX: CGFloat) {
    let screenX = min(1, max(0, normalizedX)) * arenaWidth
    pointerX = min(desktopStage.upperBound - 20, max(desktopStage.lowerBound + 20, screenX))
  }
  func desktopShoot(heavy: Bool) { if heavy { pointerHeavy = true } else { pointerLight = true } }
  func desktopJump() { pointerJump = true }
  func desktopClick(at point: CGPoint, heavy: Bool) {
    if loginNeeded, loginButton.contains(point) {
      loginNeeded = false; loginButton.isHidden = true; matchmaker?.beginLogin()
    } else { desktopShoot(heavy: heavy) }
  }
}

private final class AppDelegate: NSObject, NSApplicationDelegate {
  private var window: NSWindow!
  private var monitors: [Any] = []
  func applicationDidFinishLaunching(_ notification: Notification) {
    let desktopMode = CommandLine.arguments.contains("--desktop")
    let screenFrame = NSScreen.main?.frame ?? NSRect(x: 0, y: 0, width: 1280, height: 720)
    let viewFrame = desktopMode ? screenFrame : NSRect(x: 0, y: 0, width: windowWidth, height: windowHeight)
    let view = SKView(frame: viewFrame)
    view.preferredFramesPerSecond = 60; view.ignoresSiblingOrder = true
    view.allowsTransparency = desktopMode
    let scene = FightScene(size: CGSize(width: arenaWidth, height: arenaHeight), desktopMode: desktopMode, searching: CommandLine.arguments.contains("--searching")); scene.scaleMode = .aspectFit
    view.presentScene(scene)
    window = NSWindow(contentRect: view.frame,
                      styleMask: desktopMode ? [.borderless] : [.titled, .closable, .miniaturizable, .resizable],
                      backing: .buffered, defer: false)
    window.title = "Menu Fighter — Native Swift"; window.contentView = view
    if desktopMode {
      window.isOpaque = false; window.backgroundColor = .clear; window.hasShadow = false
      window.level = .floating; window.ignoresMouseEvents = true
      window.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .stationary]
      installDesktopControls(scene: scene, screenFrame: screenFrame)
    } else {
      window.contentAspectRatio = NSSize(width: 16, height: 9)
      window.minSize = NSSize(width: 320, height: 180)
      window.acceptsMouseMovedEvents = true
    }
    window.center(); window.makeKeyAndOrderFront(nil)
    if !desktopMode { NSApp.activate(ignoringOtherApps: true) }
  }

  private func installDesktopControls(scene: FightScene, screenFrame: NSRect) {
    let pointerMask: NSEvent.EventTypeMask = [.mouseMoved, .leftMouseDragged, .rightMouseDragged]
    if let monitor = NSEvent.addGlobalMonitorForEvents(matching: pointerMask, handler: { event in
      let x = (NSEvent.mouseLocation.x - screenFrame.minX) / screenFrame.width
      Task { @MainActor in scene.desktopPointer(normalizedX: x) }
    }) { monitors.append(monitor) }
    if let monitor = NSEvent.addGlobalMonitorForEvents(matching: [.leftMouseDown, .rightMouseDown], handler: { event in
      let heavy = event.type == .rightMouseDown
      let screenPoint = NSEvent.mouseLocation
      let scale = min(screenFrame.width / arenaWidth, screenFrame.height / arenaHeight)
      let offsetX = (screenFrame.width - arenaWidth * scale) / 2
      let offsetY = (screenFrame.height - arenaHeight * scale) / 2
      let scenePoint = CGPoint(x: (screenPoint.x - screenFrame.minX - offsetX) / scale,
                               y: (screenPoint.y - screenFrame.minY - offsetY) / scale)
      Task { @MainActor in
        scene.desktopClick(at: scenePoint, heavy: heavy)
      }
    }) { monitors.append(monitor) }
    if let monitor = NSEvent.addGlobalMonitorForEvents(matching: .scrollWheel, handler: { event in
      if event.scrollingDeltaY > 2 { Task { @MainActor in scene.desktopJump() } }
    }) { monitors.append(monitor) }
  }

  func applicationWillTerminate(_ notification: Notification) { for monitor in monitors { NSEvent.removeMonitor(monitor) } }
  func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

if CommandLine.arguments.count == 3, CommandLine.arguments[1] == "auth" {
  let ok = NativeMatchmaker.saveToken(CommandLine.arguments[2])
  print(ok ? "Menu Fighter sign-in saved in Keychain." : "Could not save Menu Fighter sign-in.")
  exit(ok ? 0 : 1)
}

if CommandLine.arguments.contains("--watch") {
  let watcherApp = NSApplication.shared
  watcherApp.setActivationPolicy(.accessory)
  CornerWatcher.shared.start()
  watcherApp.run()
  exit(0)
}

private let app = NSApplication.shared
private let delegate = AppDelegate(); app.delegate = delegate; app.setActivationPolicy(.regular); app.run()
