import AppKit
import SceneKit
import simd

/// Pre-renders a rock's full rotation as a sprite sheet: N frames of the real
/// 3D mesh tumbling, rendered offscreen with SceneKit. The badge then plays
/// these cached frames with a cheap Core Animation keyframe — so the expensive
/// 3D render happens a handful of times (only when the rock or the sun changes)
/// instead of live every frame in every window. Same geometry + texture +
/// sun-lighting as the live version; just captured up front.
enum SigilRockFrames {
    private static let device = MTLCreateSystemDefaultDevice()

    /// The tilted spin axis — matches the sense of the old live version.
    private static let axis = simd_normalize(SIMD3<Float>(0.25, 1.0, 0.12))

    /// Render `frameCount` rotation frames for `seed`, themed to `dark` and lit
    /// by the given sun. Runs offscreen (safe off the main thread); returns
    /// transparent-background CGImages ready for a CALayer.
    ///
    /// 108 frames per turn (~3.3° a step): the tumble reads as continuous 3D
    /// rotation while the chunky look still comes from the nearest-neighbour
    /// 30px downsample — pixels re-form smoothly on a screen-fixed grid
    /// rather than the sprite rotating as a zoomed bitmap. Playback stays
    /// cheap: these are cached sprites the render server cycles, so the cost
    /// is one offscreen render pass per sun move, not per displayed frame.
    static func render(
        seed: UInt64, dark: Bool,
        sunHx: CGFloat, sunElevation: CGFloat, sunIntensity: CGFloat,
        gem: Bool = false, frameCount: Int = 108, px: CGFloat = 96
    ) -> [CGImage] {
        guard let device = device else { return [] }
        let renderer = SCNRenderer(device: device, options: nil)
        let scene = SCNScene()
        renderer.scene = scene
        renderer.autoenablesDefaultLighting = false

        let cam = SCNCamera()
        cam.usesOrthographicProjection = true
        cam.orthographicScale = gem ? 1.48 : 1.85
        cam.zNear = 0.1
        cam.zFar = 100
        let camNode = SCNNode()
        camNode.camera = cam
        camNode.position = SCNVector3(0, 0, 6)
        scene.rootNode.addChildNode(camNode)
        renderer.pointOfView = camNode

        let geo = gem ? SigilMesh.gem(seed: seed) : SigilMesh.rock(seed: seed)
        let mat = SCNMaterial()
        mat.diffuse.contents = SigilRenderer.texture(seed: seed, dark: dark)
        mat.diffuse.wrapS = .repeat
        mat.diffuse.wrapT = .clamp
        mat.lightingModel = .blinn
        mat.specular.contents = NSColor(white: gem ? 0.92 : 0.22, alpha: 1)
        mat.shininess = gem ? 0.72 : 0.06
        if gem {
            mat.transparency = 1.0
            mat.blendMode = .replace
            mat.writesToDepthBuffer = true
            mat.readsFromDepthBuffer = true
            mat.fresnelExponent = 1.8
        }
        mat.isDoubleSided = true
        if gem {
            let baseHue = CGFloat(seed % 10_000) / 10_000
            geo.materials = [0.0, 0.13, 0.31].map { offset in
                let facet = mat.copy() as! SCNMaterial
                let hue = (baseHue + offset).truncatingRemainder(dividingBy: 1)
                let color = NSColor(calibratedHue: hue, saturation: 0.72,
                                    brightness: dark ? 0.88 : 0.96, alpha: 1)
                facet.diffuse.contents = color
                facet.emission.contents = color.blended(
                    withFraction: 0.28, of: .white) ?? color
                return facet
            }
        } else {
            geo.firstMaterial = mat
        }
        let rock = SCNNode(geometry: geo)
        if gem, let edgeGeo = geo.copy() as? SCNGeometry {
            let edge = SCNMaterial()
            edge.fillMode = .lines
            edge.lightingModel = .constant
            edge.diffuse.contents = NSColor(deviceWhite: 1.0, alpha: 0.46)
            edge.emission.contents = NSColor(deviceWhite: 1.0, alpha: 0.18)
            edge.transparency = 0.46
            edge.isDoubleSided = true
            edge.writesToDepthBuffer = false
            edgeGeo.materials = [edge, edge, edge]
            let lattice = SCNNode(geometry: edgeGeo)
            lattice.scale = SCNVector3(1.012, 1.012, 1.012)
            rock.addChildNode(lattice)
        }
        scene.rootNode.addChildNode(rock)

        let sun = SCNLight()
        sun.type = .directional
        sun.intensity = 650 + 700 * sunIntensity
        let sunNode = SCNNode()
        sunNode.light = sun
        let d = SCNVector3(sunHx, 0.3 + 0.7 * sunElevation, 0.85)
        sunNode.position = SCNVector3(d.x * 10, d.y * 10, d.z * 10)
        sunNode.look(at: SCNVector3(0, 0, 0))
        scene.rootNode.addChildNode(sunNode)

        let ambNode = SCNNode()
        let amb = SCNLight()
        amb.type = .ambient
        amb.intensity = 340
        ambNode.light = amb
        scene.rootNode.addChildNode(ambNode)

        let size = CGSize(width: px, height: px)
        var out: [CGImage] = []
        out.reserveCapacity(frameCount)
        for i in 0..<frameCount {
            let ang = Float(i) / Float(frameCount) * 2 * .pi
            let spinAxis = gem ? SIMD3<Float>(0, 1, 0) : axis
            rock.simdOrientation = simd_quatf(angle: ang, axis: spinAxis)
            let img = renderer.snapshot(atTime: 0, with: size, antialiasingMode: .multisampling4X)
            if let cg = img.cgImage(forProposedRect: nil, context: nil, hints: nil) {
                out.append(cg)
            }
        }
        return out
    }

    /// Nearest-neighbour downscale of a frame to `side`px — the chunky low-res
    /// copy the rock layer plays, while the crisp original drives the shadow.
    static func downsample(_ img: CGImage, to side: Int) -> CGImage {
        let cs = CGColorSpaceCreateDeviceRGB()
        guard let ctx = CGContext(
            data: nil, width: side, height: side, bitsPerComponent: 8, bytesPerRow: 0,
            space: cs, bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue
        ) else { return img }
        ctx.interpolationQuality = .none
        ctx.clear(CGRect(x: 0, y: 0, width: side, height: side))
        ctx.draw(img, in: CGRect(x: 0, y: 0, width: side, height: side))
        return ctx.makeImage() ?? img
    }
}
