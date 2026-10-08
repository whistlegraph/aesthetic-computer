import AppKit
import SceneKit
import simd

/// Cached, transparent portraits of the same portable character used by the
/// overlay and exporter. A face stays toward the viewer through each breath.
enum ProxCreatureFrames {
    enum Mood: String { case working, waiting, resting, sleeping }
    private static let device = MTLCreateSystemDefaultDevice()

    static func render(_ creature: ProxCreature, dark: Bool,
                       sunHx: CGFloat, sunElevation: CGFloat, sunIntensity: CGFloat,
                       mood: Mood = .resting, luminous: Bool = false,
                       frameCount: Int = 72, px: CGFloat = 112) -> [CGImage] {
        guard creature.isValid, frameCount > 0, px > 0, let device else { return [] }
        let renderer = SCNRenderer(device: device, options: nil)
        let scene = SCNScene()
        renderer.scene = scene
        renderer.autoenablesDefaultLighting = false
        let camera = SCNCamera()
        camera.usesOrthographicProjection = true
        camera.orthographicScale = 1.6
        camera.zNear = 0.1
        camera.zFar = 20
        let view = SCNNode()
        view.camera = camera
        view.position = SCNVector3(0, 0.12, 6)
        scene.rootNode.addChildNode(view)
        renderer.pointOfView = view

        let seed = creature.numericSeed
        func unit(_ shift: Int) -> CGFloat { CGFloat((seed >> shift) & 255) / 255 }
        let hue = CGFloat(seed % 10_000) / 10_000
        let shell = NSColor(calibratedHue: hue, saturation: 0.20 + unit(16) * 0.14,
                            brightness: 1, alpha: 1)
        let accent = NSColor(calibratedHue: (hue + 0.08).truncatingRemainder(dividingBy: 1),
                             saturation: 0.52, brightness: 0.76, alpha: 1)
        let ink = NSColor(calibratedRed: 0.10, green: 0.08, blue: 0.14, alpha: 1)
        func material(_ color: NSColor, flat: Bool = false) -> SCNMaterial {
            let mat = SCNMaterial()
            mat.diffuse.contents = color
            mat.lightingModel = flat ? .constant : .blinn
            mat.specular.contents = NSColor(white: 0.55, alpha: 1)
            mat.shininess = 0.24
            mat.isDoubleSided = true
            if luminous && !flat, let rgb = color.usingColorSpace(.deviceRGB) {
                mat.emission.contents = NSColor(deviceRed: rgb.redComponent * 0.16,
                    green: rgb.greenComponent * 0.16, blue: rgb.blueComponent * 0.16, alpha: 1)
            }
            return mat
        }
        let shellMaterial = material(shell)
        let accentMaterial = material(accent)
        let inkMaterial = material(ink, flat: true)
        let creatureNode = SCNNode()
        scene.rootNode.addChildNode(creatureNode)
        let width = Float(0.72 + unit(8) * 0.12)
        let depth: Float = 0.68
        let height: Float = creature.stage == .egg ? 1.02 : 1.08
        let body = SCNNode(geometry: egg(width: width, height: height, depth: depth))
        body.geometry?.firstMaterial = shellMaterial
        creatureNode.addChildNode(body)

        @discardableResult
        func oval(_ parent: SCNNode, _ position: SCNVector3, _ scale: SCNVector3,
                  _ mat: SCNMaterial) -> SCNNode {
            let sphere = SCNSphere(radius: 1)
            sphere.segmentCount = 20
            sphere.firstMaterial = mat
            let node = SCNNode(geometry: sphere)
            node.position = position
            node.scale = scale
            parent.addChildNode(node)
            return node
        }
        func front(_ x: Float, _ y: Float) -> Float {
            let v = y / height
            let taper = 1 - 0.20 * v
            return depth * taper * sqrt(max(0, 1 - v * v - pow(x / (width * taper), 2)))
        }
        // A few permanent shell freckles; the center remains clear for the face.
        for i in 0..<7 {
            let sign: Float = i % 2 == 0 ? -1 : 1
            let x = sign * Float(0.37 + unit((i * 7) % 48) * 0.15)
            let y = Float(0.40 + unit((i * 5 + 4) % 48) * 0.32)
            let radius = Float(0.023 + unit((i * 3 + 9) % 48) * 0.025)
            oval(creatureNode, SCNVector3(x, y, front(x, y)),
                 SCNVector3(radius, radius * 0.8, 0.012), accentMaterial)
        }
        var eyes: [SCNNode] = []
        let eyeSpread = Float(0.22 + unit(24) * 0.07)
        for sign: Float in [-1, 1] {
            let x = sign * eyeSpread
            let eye = oval(creatureNode, SCNVector3(x, 0.10, front(x, 0.10) + 0.018),
                           SCNVector3(0.075, 0.105, 0.045), inkMaterial)
            oval(eye, SCNVector3(-0.22, 0.30, 0.90), SCNVector3(0.25, 0.21, 0.15),
                 material(.white, flat: true))
            eyes.append(eye)
        }
        let mouth = SCNNode()
        creatureNode.addChildNode(mouth)
        if mood == .waiting {
            oval(mouth, SCNVector3(0, -0.19, front(0, -0.19) + 0.02),
                 SCNVector3(0.044, 0.060, 0.02), inkMaterial)
        } else {
            for i in 0...12 {
                let x = Float(i - 6) / 6 * 0.09
                let y = -0.20 + 5 * x * x
                oval(mouth, SCNVector3(x, y, front(x, y) + 0.02),
                     SCNVector3(0.014, 0.014, 0.014), inkMaterial)
            }
        }
        for trait in creature.traits {
            switch trait.feature {
            case .ears:
                for sign: Float in [-1, 1] {
                    let ear = oval(creatureNode, SCNVector3(sign * 0.46, 0.97, 0),
                                   SCNVector3(0.17, 0.39, 0.17), shellMaterial)
                    ear.eulerAngles.z = CGFloat(-sign * 0.24)
                    oval(ear, SCNVector3(0, 0.12, 0.87), SCNVector3(0.49, 0.68, 0.22), accentMaterial)
                }
            case .sprout:
                oval(creatureNode, SCNVector3(0, 1.12, 0), SCNVector3(0.035, 0.22, 0.04), accentMaterial)
                for sign: Float in [-1, 1] {
                    let leaf = oval(creatureNode, SCNVector3(sign * 0.13, 1.29, 0),
                                    SCNVector3(0.18, 0.08, 0.07), accentMaterial)
                    leaf.eulerAngles.z = CGFloat(sign * 0.5)
                }
            case .fins:
                for sign: Float in [-1, 1] {
                    let fin = oval(creatureNode, SCNVector3(sign * width, -0.23, -0.05),
                                   SCNVector3(0.23, 0.37, 0.12), accentMaterial)
                    fin.eulerAngles.z = CGFloat(sign * 0.55)
                }
            case .feet:
                for sign: Float in [-1, 1] {
                    oval(creatureNode, SCNVector3(sign * 0.32, -1.01, 0.20),
                         SCNVector3(0.22, 0.13, 0.32), accentMaterial)
                }
            case .tail:
                for i in 0..<8 {
                    let t = Float(i) / 7
                    oval(creatureNode, SCNVector3(0.57 + t * 0.49, -0.62 + t * t * 0.48, -0.12),
                         SCNVector3(0.13 - t * 0.045, 0.13 - t * 0.045, 0.12), accentMaterial)
                }
            }
        }

        let sun = SCNLight()
        sun.type = .directional
        sun.intensity = 450 + 250 * sunIntensity
        let sunNode = SCNNode()
        sunNode.light = sun
        sunNode.position = SCNVector3(sunHx * 10, (0.3 + 0.7 * sunElevation) * 10, 8.5)
        sunNode.look(at: SCNVector3(0, 0, 0))
        scene.rootNode.addChildNode(sunNode)
        let ambient = SCNNode()
        ambient.light = SCNLight()
        ambient.light?.type = .ambient
        ambient.light?.intensity = dark ? 470 : 540
        scene.rootNode.addChildNode(ambient)

        var frames: [CGImage] = []
        frames.reserveCapacity(frameCount)
        for index in 0..<frameCount {
            let t = Float(index) / Float(frameCount)
            let angle = t * 2 * .pi
            let energy: Float = mood == .working ? 1 : (mood == .waiting ? 0.75 : 0.35)
            creatureNode.eulerAngles = SCNVector3(0, sin(angle) * 0.16,
                                                 sin(angle) * (mood == .waiting ? 0.10 : 0.045))
            creatureNode.position.y = CGFloat(sin(angle * 2) * 0.032 * energy)
            creatureNode.scale = SCNVector3(1 - sin(angle) * 0.012 * energy,
                                            1 + sin(angle) * 0.018 * energy, 1)
            let blinkCenter = Float(0.70 + unit(32) * 0.18)
            let blink = max(0, 1 - abs(t - blinkCenter) / 0.045)
            let openness: Float = mood == .sleeping ? 0.12 : max(0.12, 1 - blink)
            for eye in eyes { eye.scale.y = CGFloat(0.105 * openness) }
            let image = renderer.snapshot(atTime: 0, with: CGSize(width: px, height: px), antialiasingMode: .multisampling4X)
            if let cg = image.cgImage(forProposedRect: nil, context: nil, hints: nil) { frames.append(cg) }
        }
        return frames
    }

    /// Smooth egg: the upper hemisphere narrows while the base stays full.
    private static func egg(width: Float, height: Float, depth: Float) -> SCNGeometry {
        let rings = 28, sides = 40
        var positions: [SCNVector3] = [], normals: [SCNVector3] = []
        var indices: [Int32] = []
        for row in 0...rings {
            let theta = Float(row) / Float(rings) * .pi
            let y = cos(theta), radius = sin(theta), taper = 1 - 0.20 * y
            for col in 0...sides {
                let phi = Float(col) / Float(sides) * 2 * .pi
                positions.append(SCNVector3(width * radius * taper * cos(phi), height * y,
                                            depth * radius * taper * sin(phi)))
                let normal = simd_normalize(SIMD3<Float>(radius * cos(phi) / (width * taper),
                    (y + 0.20 * radius * radius / taper) / height, radius * sin(phi) / (depth * taper)))
                normals.append(SCNVector3(normal.x, normal.y, normal.z))
                if row < rings && col < sides {
                    let a = Int32(row * (sides + 1) + col), b = a + Int32(sides + 1)
                    indices += [a, a + 1, b, a + 1, b + 1, b]
                }
            }
        }
        return SCNGeometry(sources: [SCNGeometrySource(vertices: positions), SCNGeometrySource(normals: normals)],
                           elements: [SCNGeometryElement(indices: indices, primitiveType: .triangles)])
    }
}
