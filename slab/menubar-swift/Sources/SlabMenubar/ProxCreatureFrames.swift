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
        let skinMaterial = material(NSColor(calibratedHue: hue, saturation: 0.40 + unit(16) * 0.16,
                                            brightness: 0.92, alpha: 1))
        let inkMaterial = material(ink, flat: true)
        let creatureNode = SCNNode()
        scene.rootNode.addChildNode(creatureNode)
        let width = Float(0.72 + unit(8) * 0.12)
        let depth: Float = 0.68
        let height: Float = 1.02
        let enclosed = creature.stage == .egg
        let peeking = creature.stage == .stirring
        let grown = creature.stage == .familiar
        let face = SCNNode()
        creatureNode.addChildNode(face)
        let headWidth: Float = width * (grown ? 0.76 : 0.83)
        let headHeight: Float = grown ? 0.51 : 0.57
        let headDepth: Float = 0.53
        let headY: Float = grown ? 0.66 : 0.49
        var hands: [SCNNode] = []

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
        if enclosed || peeking {
            let shellNode = SCNNode(geometry: egg(width: width, height: height, depth: depth,
                                                  open: peeking))
            shellNode.geometry?.firstMaterial = shellMaterial
            creatureNode.addChildNode(shellNode)
        }
        if !enclosed {
            // Hatching is anatomy, not an inferred accessory: every creature
            // gets a distinct head, torso and hands even when inference is off.
            face.position.y = CGFloat(headY)
            oval(creatureNode, SCNVector3(0, headY, 0),
                 SCNVector3(headWidth, headHeight, headDepth), skinMaterial)
            oval(creatureNode, SCNVector3(0, grown ? -0.28 : -0.30, -0.06),
                 SCNVector3(width * 0.50, grown ? 0.62 : 0.49, 0.34), skinMaterial)
            oval(creatureNode, SCNVector3(0, -0.32, 0.275),
                 SCNVector3(width * 0.29, grown ? 0.36 : 0.26, 0.04), shellMaterial)
            for sign: Float in [-1, 1] {
                let arm = oval(creatureNode,
                    SCNVector3(sign * (peeking ? width * 0.86 : width * 0.59), peeking ? 0.09 : -0.32, 0),
                    SCNVector3(0.13, peeking ? 0.24 : 0.32, 0.14), skinMaterial)
                arm.eulerAngles.z = CGFloat(sign * (peeking ? 0.8 : 0.45))
                hands.append(oval(creatureNode,
                    SCNVector3(sign * (peeking ? width : width * 0.82), peeking ? 0.16 : -0.53, 0.22),
                    SCNVector3(0.16, 0.15, 0.17), skinMaterial))
                if !peeking {
                    oval(creatureNode, SCNVector3(sign * 0.20, -0.78, 0),
                         SCNVector3(0.12, 0.23, 0.14), skinMaterial)
                    oval(creatureNode, SCNVector3(sign * 0.24, -0.96, 0.13),
                         SCNVector3(0.23, 0.14, 0.30), accentMaterial)
                }
            }
            if peeking {
                // A discarded cap makes the broken shell legible at badge size.
                let cap = SCNNode(geometry: egg(width: width, height: height, depth: depth, open: true))
                cap.geometry?.firstMaterial = shellMaterial
                cap.scale = SCNVector3(0.35, 0.32, 0.35)
                cap.position = SCNVector3(-0.78, -0.87, 0.20)
                cap.eulerAngles.z = 0.8
                creatureNode.addChildNode(cap)
            }
        }
        func front(_ x: Float, _ y: Float) -> Float {
            if !enclosed {
                return headDepth * sqrt(max(0, 1 - pow(y / headHeight, 2) - pow(x / headWidth, 2)))
            }
            let v = y / height
            let taper = 1 - 0.20 * v
            return depth * taper * sqrt(max(0, 1 - v * v - pow(x / (width * taper), 2)))
        }
        // The same seed freckles migrate from the shell to the forehead.
        for i in 0..<7 {
            let sign: Float = i % 2 == 0 ? -1 : 1
            let x = sign * Float(0.37 + unit((i * 7) % 48) * 0.15) * (enclosed ? 1 : 0.74)
            let y = Float(0.40 + unit((i * 5 + 4) % 48) * 0.32) * (enclosed ? 1 : 0.58)
            let radius = Float(0.023 + unit((i * 3 + 9) % 48) * 0.025)
            oval(face, SCNVector3(x, y, front(x, y) + 0.008),
                 SCNVector3(radius, radius * 0.8, 0.012), accentMaterial)
        }
        var eyes: [SCNNode] = []
        let eyeSpread = Float(0.22 + unit(24) * 0.07)
        for sign: Float in [-1, 1] {
            let x = sign * eyeSpread
            let eye = oval(face, SCNVector3(x, 0.10, front(x, 0.10) + 0.018),
                           SCNVector3(0.075, 0.105, 0.045), inkMaterial)
            oval(eye, SCNVector3(-0.22, 0.30, 0.90), SCNVector3(0.25, 0.21, 0.15),
                 material(.white, flat: true))
            eyes.append(eye)
        }
        let mouth = SCNNode()
        face.addChildNode(mouth)
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
                    let ear = oval(creatureNode, SCNVector3(sign * 0.40, 1.02, 0),
                                   SCNVector3(0.17, 0.34, 0.17), skinMaterial)
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
                    let fin = oval(creatureNode, SCNVector3(sign * (enclosed ? width : width * 0.61), -0.23, -0.09),
                                   SCNVector3(0.23, 0.37, 0.12), accentMaterial)
                    fin.eulerAngles.z = CGFloat(sign * 0.55)
                }
            case .feet:
                for sign: Float in [-1, 1] {
                    oval(creatureNode, SCNVector3(sign * 0.27, -0.99, 0.28),
                         SCNVector3(0.29, 0.15, 0.38), accentMaterial)
                }
            case .tail:
                for i in 0..<8 {
                    let t = Float(i) / 7
                    oval(creatureNode, SCNVector3(0.31 + t * 0.65, -0.62 + t * t * 0.48, -0.12),
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
            for (i, hand) in hands.enumerated() {
                hand.eulerAngles.z = CGFloat(sin(angle + Float(i) * .pi) * 0.18 * energy)
            }
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
    private static func egg(width: Float, height: Float, depth: Float, open: Bool = false) -> SCNGeometry {
        let rings = 28, sides = 40
        var positions: [SCNVector3] = [], normals: [SCNVector3] = []
        var indices: [Int32] = []
        for row in 0...rings {
            for col in 0...sides {
                let phi = Float(col) / Float(sides) * 2 * .pi
                // Clip to a zigzag rim; the shell never encloses the new head.
                let rim: Float = [0.17, 0.04, -0.09, 0.04][col % 4]
                let start = open ? acos(rim) : 0
                let theta = start + Float(row) / Float(rings) * (.pi - start)
                let y = cos(theta), radius = sin(theta), taper = 1 - 0.20 * y
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
