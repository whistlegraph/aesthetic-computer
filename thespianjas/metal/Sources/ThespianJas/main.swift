import AppKit
import SceneKit
import GLTFKit2

final class StudioController: NSViewController {
  private let scene = SCNScene()
  private var subject = SCNNode()
  private var key = SCNLight()
  private var fill = SCNLight()
  private var rim = SCNLight()
  private var retainedGLTF: GLTFAsset?
  private let assetURL: URL

  init(assetURL: URL) {
    self.assetURL = assetURL
    super.init(nibName: nil, bundle: nil)
  }
  required init?(coder: NSCoder) { fatalError() }

  override func loadView() {
    let root = NSView(frame: NSRect(x: 0, y: 0, width: 1080, height: 1080))
    let options: [String: Any] = [SCNView.Option.preferredRenderingAPI.rawValue: SCNRenderingAPI.metal.rawValue]
    let view = SCNView(frame: root.bounds, options: options)
    view.autoresizingMask = [NSView.AutoresizingMask.width, NSView.AutoresizingMask.height]
    view.scene = scene
    view.backgroundColor = NSColor(calibratedRed: 0.09, green: 0.075, blue: 0.11, alpha: 1)
    view.allowsCameraControl = false
    view.antialiasingMode = SCNAntialiasingMode.multisampling4X
    view.rendersContinuously = true
    view.preferredFramesPerSecond = 60
    view.autoenablesDefaultLighting = false
    root.addSubview(view)

    let panel = NSStackView(frame: NSRect(x: 18, y: 18, width: 250, height: 150))
    panel.orientation = .vertical
    panel.alignment = .leading
    panel.spacing = 8
    panel.wantsLayer = true
    panel.layer?.backgroundColor = NSColor(calibratedWhite: 0.08, alpha: 0.82).cgColor
    panel.layer?.cornerRadius = 12
    panel.edgeInsets = NSEdgeInsets(top: 12, left: 12, bottom: 12, right: 12)
    panel.addArrangedSubview(label("thespianjas · Metal studio"))
    panel.addArrangedSubview(slider("key", value: 220) { [weak self] in self?.key.intensity = CGFloat($0 * 18) })
    panel.addArrangedSubview(slider("fill", value: 80) { [weak self] in self?.fill.intensity = CGFloat($0 * 12) })
    panel.addArrangedSubview(slider("rim", value: 180) { [weak self] in self?.rim.intensity = CGFloat($0 * 18) })
    root.addSubview(panel)
    self.view = root
    buildScene()
  }

  private func label(_ text: String) -> NSTextField {
    let field = NSTextField(labelWithString: text)
    field.textColor = .white
    field.font = .monospacedSystemFont(ofSize: 13, weight: .semibold)
    return field
  }

  private func slider(_ name: String, value: Double, change: @escaping (Double) -> Void) -> NSView {
    let row = NSStackView(); row.orientation = .horizontal; row.spacing = 8
    let title = label(name); title.frame.size.width = 40
    let control = NSSlider(value: value, minValue: 0, maxValue: 300, target: nil, action: nil)
    control.frame.size.width = 170
    final class Box: NSObject { let f: (Double) -> Void; init(_ f: @escaping (Double) -> Void) { self.f = f }; @objc func act(_ s: NSSlider) { f(s.doubleValue) } }
    let box = Box(change); control.target = box; control.action = #selector(Box.act(_:))
    objc_setAssociatedObject(control, "handler", box, .OBJC_ASSOCIATION_RETAIN_NONATOMIC)
    row.addArrangedSubview(title); row.addArrangedSubview(control); return row
  }

  private func buildScene() {
    subject.name = "thespianjas"
    scene.rootNode.addChildNode(subject)
    if assetURL.pathExtension.lowercased() == "glb" || assetURL.pathExtension.lowercased() == "gltf" {
      GLTFAsset.load(with: assetURL, options: [:]) { [weak self] _, status, asset, error, _ in
        guard status == .complete, let self, let asset else {
          if status == .error { DispatchQueue.main.async { self?.showLoadError(error) } }
          return
        }
        self.retainedGLTF = asset
        let loaded = SCNScene(gltfAsset: asset)
        DispatchQueue.main.async {
          for child in loaded.rootNode.childNodes { self.subject.addChildNode(child) }
          self.frameSubject()
        }
      }
    } else {
      do {
        let loaded = try SCNScene(url: assetURL, options: [.checkConsistency: true])
        for child in loaded.rootNode.childNodes { subject.addChildNode(child) }
        frameSubject()
      } catch { showLoadError(error) }
    }

    let floor = SCNFloor(); floor.reflectivity = 0.08
    floor.firstMaterial?.diffuse.contents = NSColor(calibratedRed: 0.18, green: 0.15, blue: 0.21, alpha: 1)
    let floorNode = SCNNode(geometry: floor); floorNode.position.y = 0; scene.rootNode.addChildNode(floorNode)

    key.type = .spot; key.color = NSColor(calibratedRed: 1, green: 0.91, blue: 0.80, alpha: 1); key.intensity = 3960; key.castsShadow = true
    fill.type = .omni; fill.color = NSColor(calibratedRed: 0.45, green: 0.82, blue: 1, alpha: 1); fill.intensity = 960
    rim.type = .spot; rim.color = NSColor(calibratedRed: 1, green: 0.32, blue: 0.62, alpha: 1); rim.intensity = 3240
    lightNode(key, at: SCNVector3(-3, 4, 4)); lightNode(fill, at: SCNVector3(3, 2, 3)); lightNode(rim, at: SCNVector3(2, 4, -3))

    let camera = SCNCamera(); camera.fieldOfView = 40
    let cameraNode = SCNNode(); cameraNode.camera = camera; cameraNode.position = SCNVector3(0, 1.5, 4.8)
    let look = SCNLookAtConstraint(target: subject); look.isGimbalLockEnabled = true; cameraNode.constraints = [look]
    scene.rootNode.addChildNode(cameraNode)
  }

  private func showLoadError(_ error: Error?) {
    let message = label("Could not load \(assetURL.lastPathComponent): \(error?.localizedDescription ?? "unknown error")")
    message.textColor = .systemRed; message.frame = NSRect(x: 290, y: 30, width: 760, height: 40); view.addSubview(message)
  }

  private func frameSubject() {
    // Provider assets vary between centimeters/meters and may be authored far
    // from the origin. Normalize the root while preserving the skeleton's
    // internal transforms: 2.4 scene units tall, centered, feet on the floor.
    let (min, max) = subject.boundingBox
    let height = max.y - min.y
    if height.isFinite && height > 0.0001 {
      let s = 2.4 / height
      subject.scale = SCNVector3(s, s, s)
      subject.position = SCNVector3(-((min.x + max.x) * 0.5) * s, -min.y * s, -((min.z + max.z) * 0.5) * s)
    }
    subject.enumerateChildNodes { node, _ in
      for key in node.animationKeys { node.animationPlayer(forKey: key)?.play() }
    }
  }

  private func lightNode(_ light: SCNLight, at p: SCNVector3) {
    let n = SCNNode(); n.light = light; n.position = p
    let look = SCNLookAtConstraint(target: subject); look.isGimbalLockEnabled = true; n.constraints = [look]
    scene.rootNode.addChildNode(n)
  }
}

final class AppDelegate: NSObject, NSApplicationDelegate {
  let asset: URL
  var window: NSWindow?
  init(asset: URL) { self.asset = asset }
  func applicationDidFinishLaunching(_ notification: Notification) {
    let w = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 1080, height: 1080), styleMask: [.titled, .closable, .resizable, .miniaturizable], backing: .buffered, defer: false)
    w.title = "thespianjas · Metal studio"; w.center(); w.contentViewController = StudioController(assetURL: asset); w.makeKeyAndOrderFront(nil); window = w
    NSApp.activate(ignoringOtherApps: true)
  }
  func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

let arg = CommandLine.arguments.dropFirst().first ?? "../assets/versions/v001/idle.glb"
let asset = URL(fileURLWithPath: arg, relativeTo: URL(fileURLWithPath: FileManager.default.currentDirectoryPath)).standardizedFileURL
let app = NSApplication.shared
let delegate = AppDelegate(asset: asset)
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
