import AppKit
import Metal
import SceneKit

guard CommandLine.arguments.count == 3 else {
    fputs("usage: render-sprites <model.usdc> <output-dir>\n", stderr)
    exit(2)
}

let modelURL = URL(fileURLWithPath: CommandLine.arguments[1])
let output = URL(fileURLWithPath: CommandLine.arguments[2], isDirectory: true)
let frameSize = 160
let columns = 8
let rows = 3
let frameCount = columns * rows

let palettes: [(String, [NSColor])] = [
    ("dark", [
        NSColor(srgbRed: 0.36, green: 0.82, blue: 1.00, alpha: 1),
        NSColor(srgbRed: 0.48, green: 0.58, blue: 1.00, alpha: 1),
        NSColor(srgbRed: 0.64, green: 0.52, blue: 1.00, alpha: 1),
        NSColor(srgbRed: 0.20, green: 0.64, blue: 1.00, alpha: 1),
        NSColor(srgbRed: 0.70, green: 0.86, blue: 1.00, alpha: 1),
    ]),
    ("light", [
        NSColor(srgbRed: 0.03, green: 0.29, blue: 0.66, alpha: 1),
        NSColor(srgbRed: 0.12, green: 0.37, blue: 0.78, alpha: 1),
        NSColor(srgbRed: 0.24, green: 0.24, blue: 0.67, alpha: 1),
        NSColor(srgbRed: 0.02, green: 0.43, blue: 0.63, alpha: 1),
        NSColor(srgbRed: 0.28, green: 0.36, blue: 0.58, alpha: 1),
    ]),
]

guard let imported = try? SCNScene(url: modelURL, options: nil),
      let device = MTLCreateSystemDefaultDevice() else {
    fputs("could not load \(modelURL.path)\n", stderr)
    exit(1)
}

let scene = SCNScene()
scene.background.contents = NSColor.clear
let model = SCNNode()
for child in imported.rootNode.childNodes { model.addChildNode(child) }
let (lo, hi) = model.boundingBox
model.pivot = SCNMatrix4MakeTranslation((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, (lo.z + hi.z) / 2)
let span = max(hi.x - lo.x, hi.y - lo.y)
let fit = 2.05 / max(span, 0.001)
model.scale = SCNVector3(fit, fit, fit)
model.eulerAngles.x = -0.08
scene.rootNode.addChildNode(model)

let camera = SCNNode()
camera.camera = SCNCamera()
camera.camera?.usesOrthographicProjection = true
camera.camera?.orthographicScale = 2.55
camera.camera?.wantsHDR = false
camera.position = SCNVector3(0, 0, 4)
scene.rootNode.addChildNode(camera)

let ambient = SCNNode()
ambient.light = SCNLight()
ambient.light?.type = .ambient
ambient.light?.intensity = 530
scene.rootNode.addChildNode(ambient)
let key = SCNNode()
key.light = SCNLight()
key.light?.type = .directional
key.light?.intensity = 1160
key.eulerAngles = SCNVector3(-0.55, 0.62, 0)
scene.rootNode.addChildNode(key)
let rim = SCNNode()
rim.light = SCNLight()
rim.light?.type = .omni
rim.light?.intensity = 680
rim.position = SCNVector3(-2, 2, 3)
scene.rootNode.addChildNode(rim)

let renderer = SCNRenderer(device: device, options: nil)
renderer.scene = scene
renderer.pointOfView = camera

try FileManager.default.createDirectory(at: output, withIntermediateDirectories: true)
for (appearance, colors) in palettes {
    for (variant, color) in colors.enumerated() {
        model.enumerateChildNodes { node, _ in
            for material in node.geometry?.materials ?? [] {
                material.lightingModel = .physicallyBased
                material.diffuse.contents = color
                material.multiply.contents = NSColor.white
                material.roughness.contents = 0.16
                material.metalness.contents = 0.12
                material.specular.contents = NSColor.white
            }
        }
        let sheet = NSImage(size: NSSize(width: columns * frameSize, height: rows * frameSize))
        sheet.lockFocus()
        NSColor.clear.setFill()
        NSRect(x: 0, y: 0, width: columns * frameSize, height: rows * frameSize).fill()
        for frame in 0..<frameCount {
            model.eulerAngles.y = CGFloat(frame) / CGFloat(frameCount) * .pi * 2
            let image = renderer.snapshot(atTime: 0, with: CGSize(width: frameSize, height: frameSize),
                                          antialiasingMode: .multisampling4X)
            let column = frame % columns
            let row = frame / columns
            image.draw(in: NSRect(x: column * frameSize, y: row * frameSize,
                                  width: frameSize, height: frameSize),
                       from: .zero, operation: .sourceOver, fraction: 1)
        }
        sheet.unlockFocus()
        guard let tiff = sheet.tiffRepresentation,
              let bitmap = NSBitmapImageRep(data: tiff),
              let png = bitmap.representation(using: .png, properties: [:]) else { exit(1) }
        let url = output.appendingPathComponent("pals-\(appearance)-\(variant).png")
        try png.write(to: url)
        print("✓ \(url.lastPathComponent)")
    }
}
