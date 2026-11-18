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
        let species = creature.species
        let shell = NSColor(calibratedHue: hue, saturation: 0.16 + unit(16) * 0.26,
                            brightness: 1, alpha: 1)
        // The accent leaves the shell's hue by a seeded interval, so two eggs
        // of one colour still differ in their markings.
        let accentShift: CGFloat = [0.08, 0.30, 0.52, 0.88][Int((seed >> 44) & 3)]
        let accent = NSColor(calibratedHue: (hue + accentShift).truncatingRemainder(dividingBy: 1),
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
        shellMaterial.diffuse.contents = shellTexture(species, shell: shell, accent: accent, seed: seed)
        shellMaterial.diffuse.wrapS = .repeat
        let bellyMaterial = material(shell)
        let beakMaterial = material(NSColor(calibratedRed: 1, green: 0.66, blue: 0.18, alpha: 1))
        let accentMaterial = material(accent)
        let skinMaterial = material(NSColor(calibratedHue: hue, saturation: 0.40 + unit(16) * 0.16,
                                            brightness: 0.92, alpha: 1))
        let inkMaterial = material(ink, flat: true)
        let creatureNode = SCNNode()
        scene.rootNode.addChildNode(creatureNode)
        let width = Float(0.72 + unit(8) * 0.12)
        let depth: Float = 0.68
        let height = Float(0.94 + unit(48) * 0.12)
        let taper = Float(0.12 + unit(52) * 0.16)
        let enclosed = creature.stage == .egg
        let peeking = creature.stage == .stirring
        let grown = creature.stage == .familiar
        let face = SCNNode()
        creatureNode.addChildNode(face)
        // Species are proportions and a few fixed parts over one skeleton, so
        // every acquired trait still has somewhere to attach.
        let headShape: (w: Float, h: Float) = [.pip: (1, 1), .chick: (0.95, 1), .spike: (1, 0.95),
            .ghost: (1, 1), .bug: (0.9, 0.9), .frog: (1.16, 0.78)][species]!
        let torsoWidth: Float = [.pip: 0.50, .chick: 0.62, .spike: 0.54, .ghost: 0.64,
                                 .bug: 0.44, .frog: 0.60][species]!
        let headWidth: Float = width * (grown ? 0.76 : 0.83) * headShape.w
        let headHeight: Float = (grown ? 0.51 : 0.57) * headShape.h
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
                                                  taper: taper, open: peeking))
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
                 SCNVector3(width * torsoWidth, grown ? 0.62 : 0.49, 0.34), skinMaterial)
            oval(creatureNode, SCNVector3(0, -0.32, 0.275),
                 SCNVector3(width * (species == .chick ? 0.36 : 0.29), grown ? 0.36 : 0.26, 0.04),
                 species == .bug ? accentMaterial : bellyMaterial)
            let limbMaterial = species == .chick ? accentMaterial : skinMaterial
            let reach = torsoWidth / 0.50
            for sign: Float in [-1, 1] {
                let arm = oval(creatureNode,
                    SCNVector3(sign * (peeking ? width * 0.86 : width * 0.59 * reach), peeking ? 0.09 : -0.32, 0),
                    species == .chick ? SCNVector3(0.11, peeking ? 0.24 : 0.34, 0.22)
                                      : SCNVector3(0.13, peeking ? 0.24 : 0.32, 0.14), limbMaterial)
                arm.eulerAngles.z = CGFloat(sign * (peeking ? 0.8 : 0.45))
                hands.append(oval(creatureNode,
                    SCNVector3(sign * (peeking ? width : width * 0.82 * reach), peeking ? 0.16 : -0.53, 0.22),
                    species == .chick ? SCNVector3(0.11, 0.10, 0.12) : SCNVector3(0.16, 0.15, 0.17),
                    limbMaterial))
                guard !peeking else { continue }
                switch species {
                case .ghost:
                    // No legs: a scalloped hem, and it floats higher.
                    for x: Float in [0.14, 0.40] {
                        oval(creatureNode, SCNVector3(sign * x * width / 0.78, -0.80, -0.04),
                             SCNVector3(0.15, 0.17, 0.20), skinMaterial)
                    }
                case .chick:
                    oval(creatureNode, SCNVector3(sign * 0.16, -0.82, 0),
                         SCNVector3(0.045, 0.20, 0.045), beakMaterial)
                    oval(creatureNode, SCNVector3(sign * 0.18, -1.00, 0.12),
                         SCNVector3(0.18, 0.07, 0.25), beakMaterial)
                case .frog:
                    oval(creatureNode, SCNVector3(sign * 0.33, -0.74, 0.02),
                         SCNVector3(0.20, 0.18, 0.21), skinMaterial)
                    oval(creatureNode, SCNVector3(sign * 0.40, -0.94, 0.16),
                         SCNVector3(0.28, 0.10, 0.33), accentMaterial)
                default:
                    oval(creatureNode, SCNVector3(sign * 0.20, -0.78, 0),
                         SCNVector3(0.12, 0.23, 0.14), skinMaterial)
                    oval(creatureNode, SCNVector3(sign * 0.24, -0.96, 0.13),
                         SCNVector3(0.23, 0.14, 0.30), accentMaterial)
                }
                if species == .bug {
                    let lower = oval(creatureNode, SCNVector3(sign * width * 0.50, -0.50, 0.04),
                                     SCNVector3(0.09, 0.20, 0.10), skinMaterial)
                    lower.eulerAngles.z = CGFloat(sign * 0.75)
                }
            }
            // Head parts ride the face node so they turn and blink with it.
            switch species {
            case .pip: break
            case .chick:
                for dx: Float in [-0.07, 0, 0.07] {
                    let tuft = oval(face, SCNVector3(dx, headHeight + 0.05, 0),
                                    SCNVector3(0.04, 0.12, 0.04), skinMaterial)
                    tuft.eulerAngles.z = CGFloat(-dx * 5)
                }
            case .spike:
                for i in -2...2 {
                    let a = Float(i) * 0.34
                    let spike = oval(face, SCNVector3(sin(a) * headWidth, cos(a) * headHeight, -0.02),
                                     SCNVector3(0.07, 0.16, 0.07), accentMaterial)
                    spike.eulerAngles.z = CGFloat(-a)
                }
            case .ghost:
                let curl = oval(face, SCNVector3(0.05, headHeight + 0.04, 0),
                                SCNVector3(0.08, 0.14, 0.08), skinMaterial)
                curl.eulerAngles.z = -0.5
            case .bug:
                for sign: Float in [-1, 1] {
                    let stalk = oval(face, SCNVector3(sign * 0.17, headHeight + 0.13, 0),
                                     SCNVector3(0.022, 0.18, 0.022), accentMaterial)
                    stalk.eulerAngles.z = CGFloat(-sign * 0.3)
                    oval(face, SCNVector3(sign * 0.225, headHeight + 0.31, 0),
                         SCNVector3(0.065, 0.065, 0.065), accentMaterial)
                }
            case .frog: break // its eye mounds are built with the eyes below
            }
            if peeking {
                // A discarded cap makes the broken shell legible at badge size.
                let cap = SCNNode(geometry: egg(width: width, height: height, depth: depth,
                                                taper: taper, open: true))
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
            let narrow = 1 - taper * v
            return depth * narrow * sqrt(max(0, 1 - v * v - pow(x / (width * narrow), 2)))
        }
        // The same seed freckles migrate from the shell to the forehead.
        for i in 0..<7 {
            let sign: Float = i % 2 == 0 ? -1 : 1
            let x = sign * Float(0.37 + unit((i * 7) % 48) * 0.15) * (enclosed ? 1 : 0.74 * headShape.w)
            let y = Float(0.40 + unit((i * 5 + 4) % 48) * 0.32) * (enclosed ? 1 : 0.58 * headShape.h)
            let radius = Float(0.023 + unit((i * 3 + 9) % 48) * 0.025)
            oval(face, SCNVector3(x, y, front(x, y) + 0.008),
                 SCNVector3(radius, radius * 0.8, 0.012), accentMaterial)
        }
        var eyes: [SCNNode] = []
        let eyeSpread = Float(0.22 + unit(24) * 0.07)
        let hatched = !enclosed
        let eyeSize: Float = (hatched && species == .bug ? 1.3 : 1) * Float(0.9 + unit(36) * 0.25)
        let mounded = hatched && species == .frog
        let eyeY: Float = mounded ? headHeight * 0.62 : 0.10
        for sign: Float in [-1, 1] {
            let x = sign * eyeSpread
            var z = front(x, eyeY) + 0.018
            if mounded {
                // Frog eyes sit on mounds above the brow instead of in the face.
                let base = front(x, eyeY) * 0.5
                oval(face, SCNVector3(x, eyeY, base), SCNVector3(0.16, 0.16, 0.16), skinMaterial)
                z = base + 0.165
            }
            let eye = oval(face, SCNVector3(x, eyeY, z),
                           SCNVector3(0.075 * eyeSize, 0.105 * eyeSize, 0.045), inkMaterial)
            oval(eye, SCNVector3(-0.22, 0.30, 0.90), SCNVector3(0.25, 0.21, 0.15),
                 material(.white, flat: true))
            eyes.append(eye)
            if hatched && species == .ghost {
                oval(face, SCNVector3(x * 1.45, -0.07, front(x * 1.45, -0.07) + 0.006),
                     SCNVector3(0.065, 0.04, 0.012), accentMaterial)
            }
        }
        let mouth = SCNNode()
        face.addChildNode(mouth)
        if hatched && species == .chick {
            // A beak is the whole expression; it parts while it waits.
            oval(mouth, SCNVector3(0, -0.10, front(0, -0.10) + 0.03),
                 SCNVector3(0.08, mood == .waiting ? 0.075 : 0.05, 0.11), beakMaterial)
        } else if mood == .waiting {
            oval(mouth, SCNVector3(0, -0.19, front(0, -0.19) + 0.02),
                 SCNVector3(0.044, 0.060, 0.02), inkMaterial)
        } else {
            let half: Float = mounded ? 0.17 : 0.09
            let steps = mounded ? 12 : 6
            for i in -steps...steps {
                let x = Float(i) / Float(steps) * half
                let y = -0.20 + 0.04 * pow(x / half, 2)
                oval(mouth, SCNVector3(x, y, front(x, y) + 0.02),
                     SCNVector3(0.014, 0.014, 0.014), inkMaterial)
            }
        }
        let crown = enclosed ? height : headY + headHeight
        for trait in creature.traits {
            switch trait.feature {
            case .ears:
                for sign: Float in [-1, 1] {
                    let ear = oval(creatureNode, SCNVector3(sign * 0.40, crown - 0.04, 0),
                                   SCNVector3(0.17, 0.34, 0.17), skinMaterial)
                    ear.eulerAngles.z = CGFloat(-sign * 0.24)
                    oval(ear, SCNVector3(0, 0.12, 0.87), SCNVector3(0.49, 0.68, 0.22), accentMaterial)
                }
            case .sprout:
                oval(creatureNode, SCNVector3(0, crown + 0.06, 0), SCNVector3(0.035, 0.22, 0.04), accentMaterial)
                for sign: Float in [-1, 1] {
                    let leaf = oval(creatureNode, SCNVector3(sign * 0.13, crown + 0.23, 0),
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
            let lift: Float = hatched && !peeking && species == .ghost ? 0.10 : 0
            creatureNode.position.y = CGFloat(lift + sin(angle * 2) * 0.032 * energy * (lift > 0 ? 2 : 1))
            creatureNode.scale = SCNVector3(1 - sin(angle) * 0.012 * energy,
                                            1 + sin(angle) * 0.018 * energy, 1)
            for (i, hand) in hands.enumerated() {
                hand.eulerAngles.z = CGFloat(sin(angle + Float(i) * .pi) * 0.18 * energy)
            }
            let blinkCenter = Float(0.70 + unit(32) * 0.18)
            let blink = max(0, 1 - abs(t - blinkCenter) / 0.045)
            let openness: Float = mood == .sleeping ? 0.12 : max(0.12, 1 - blink)
            for eye in eyes { eye.scale.y = CGFloat(0.105 * eyeSize * openness) }
            let image = renderer.snapshot(atTime: 0, with: CGSize(width: px, height: px), antialiasingMode: .multisampling4X)
            if let cg = image.cgImage(forProposedRect: nil, context: nil, hints: nil) { frames.append(cg) }
        }
        return frames
    }

    /// Smooth egg: the upper hemisphere narrows while the base stays full.
    private static func egg(width: Float, height: Float, depth: Float, taper: Float,
                            open: Bool = false) -> SCNGeometry {
        let rings = 28, sides = 40
        var positions: [SCNVector3] = [], normals: [SCNVector3] = [], uvs: [CGPoint] = []
        var indices: [Int32] = []
        for row in 0...rings {
            for col in 0...sides {
                let phi = Float(col) / Float(sides) * 2 * .pi
                // Clip to a zigzag rim; the shell never encloses the new head.
                let rim: Float = [0.17, 0.04, -0.09, 0.04][col % 4]
                let start = open ? acos(rim) : 0
                let theta = start + Float(row) / Float(rings) * (.pi - start)
                let y = cos(theta), radius = sin(theta), narrow = 1 - taper * y
                positions.append(SCNVector3(width * radius * narrow * cos(phi), height * y,
                                            depth * radius * narrow * sin(phi)))
                // The markings wrap once around and run pole to pole, so an
                // opened shell keeps the pattern it had as a whole egg.
                uvs.append(CGPoint(x: CGFloat(col) / CGFloat(sides), y: CGFloat(theta / .pi)))
                let normal = simd_normalize(SIMD3<Float>(radius * cos(phi) / (width * narrow),
                    (y + taper * radius * radius / narrow) / height, radius * sin(phi) / (depth * narrow)))
                normals.append(SCNVector3(normal.x, normal.y, normal.z))
                if row < rings && col < sides {
                    let a = Int32(row * (sides + 1) + col), b = a + Int32(sides + 1)
                    indices += [a, a + 1, b, a + 1, b + 1, b]
                }
            }
        }
        return SCNGeometry(sources: [SCNGeometrySource(vertices: positions), SCNGeometrySource(normals: normals),
                                     SCNGeometrySource(textureCoordinates: uvs)],
                           elements: [SCNGeometryElement(indices: indices, primitiveType: .triangles)])
    }

    /// Shell markings, one family per species: the egg tells what is inside.
    /// Drawn in (u, v) with v = 0 at the pole; the face (u 0.25) stays clear.
    private static func shellTexture(_ species: ProxCreature.Species, shell: NSColor,
                                     accent: NSColor, seed: UInt64) -> CGImage? {
        let w = 256, h = 256
        guard let ctx = CGContext(data: nil, width: w, height: h, bitsPerComponent: 8, bytesPerRow: 0,
                                  space: CGColorSpace(name: CGColorSpace.sRGB)!,
                                  bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) else { return nil }
        ctx.translateBy(x: 0, y: CGFloat(h)); ctx.scaleBy(x: CGFloat(w), y: -CGFloat(h))
        ctx.setFillColor(shell.cgColor); ctx.fill(CGRect(x: 0, y: 0, width: 1, height: 1))
        var state = seed ^ 0x9E37_79B9_7F4A_7C15
        func next() -> CGFloat {
            state = state &* 6364136223846793005 &+ 1442695040888963407
            return CGFloat((state >> 33) & 0xFFFF) / 65535
        }
        let mark = accent.blended(withFraction: 0.25, of: shell) ?? accent
        ctx.setFillColor(mark.cgColor); ctx.setStrokeColor(mark.cgColor)
        // A dot's u radius is narrower than its v radius: the equator is
        // about 1.6 times longer than the pole-to-pole arc.
        func dot(_ u: CGFloat, _ v: CGFloat, _ r: CGFloat, ring: Bool = false) {
            guard abs(u - 0.25) > 0.12 + r || v < 0.36 - r || v > 0.64 + r else { return }
            for wrap: CGFloat in [-1, 0, 1] {
                let rect = CGRect(x: u + wrap - r / 1.6, y: v - r, width: r * 2 / 1.6, height: r * 2)
                if ring { ctx.setLineWidth(r * 0.3); ctx.strokeEllipse(in: rect) } else { ctx.fillEllipse(in: rect) }
            }
        }
        func band(_ v: CGFloat, _ thickness: CGFloat, zigzag: CGFloat) {
            let teeth = 16
            ctx.move(to: CGPoint(x: 0, y: v))
            for i in 0...teeth { ctx.addLine(to: CGPoint(x: CGFloat(i) / CGFloat(teeth), y: v + (i % 2 == 0 ? 0 : zigzag))) }
            for i in (0...teeth).reversed() {
                ctx.addLine(to: CGPoint(x: CGFloat(i) / CGFloat(teeth), y: v + thickness + (i % 2 == 0 ? 0 : zigzag)))
            }
            ctx.fillPath()
        }
        switch species {
        case .pip:
            for _ in 0..<26 { dot(next(), 0.16 + next() * 0.72, 0.012 + next() * 0.012) }
        case .chick:
            for _ in 0..<44 { dot(next(), 0.14 + next() * 0.76, 0.014 + next() * 0.030) }
        case .spike:
            for _ in 0..<11 { dot(next(), 0.18 + next() * 0.66, 0.06 + next() * 0.05) }
        case .ghost:
            for _ in 0..<22 { dot(next(), 0.16 + next() * 0.70, 0.035 + next() * 0.03, ring: true) }
        case .bug:
            let zig = 0.02 + next() * 0.04
            band(0.20 + next() * 0.04, 0.07, zigzag: zig)
            band(0.70 + next() * 0.04, 0.09, zigzag: zig)
            if next() > 0.5 { band(0.86, 0.14, zigzag: 0) }
        case .frog:
            band(0.66 + next() * 0.06, 0.40, zigzag: 0.05 + next() * 0.04)
            for _ in 0..<8 { dot(next(), 0.14 + next() * 0.18, 0.02 + next() * 0.02) }
        }
        return ctx.makeImage()
    }
}
