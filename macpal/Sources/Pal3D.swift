// Pal3D.swift — a 3D avatar for MacPal.
//
// Drops into PalController as `glyphView` (an NSView) in place of the flat SVG
// NSImageView, so the surrounding charm — drag-to-corner, collapse, hover
// spring, press pop — all still wrap it unchanged (those poke `glyphView.layer`,
// which a layer-backed SCNView honors). What's specific to 3D lives here: the
// model load, a transparent-background SceneKit scene, a gentle idle spin + bob,
// and a `sing()` flourish for the Menu Band note easter egg.
//
// It loads an OBJ (Meshy → fal, see macpal/bin/gen-blueberry-3d.mjs) via
// ModelIO and applies the matching `*-base_color.png` to the material. Until
// that asset exists, it falls back to a procedurally-built blueberry so the 3D
// path is always live and testable — `--model3d` with no file shows the
// placeholder; `--model3d <path/to.obj>` shows the real berry.
import AppKit
import SceneKit
import ModelIO
import SceneKit.ModelIO

// A built scene plus the handles callers need (camera for the point of view,
// spin node for idle/animation). Shared by the live view and the offscreen
// snapshot/thumbnail path.
struct Pal3DScene {
    let scene: SCNScene
    let camera: SCNNode
    let spin: SCNNode
    let model: SCNNode
    let ambient: SCNNode    // recolored from the desktop pixels behind the pal
    let key: SCNNode
    let clips: [String: SCNNode]                  // hidden one-shot models (wave/cheer)
    let clipPlayers: [String: SCNAnimationPlayer]
}

final class Pal3DView: NSView {
    private let scnView = SCNView()
    private let spinNode: SCNNode         // gentle idle float rides here
    private let modelHolder: SCNNode      // centered, scaled-to-fit mesh
    private let ambientNode: SCNNode      // recolored from the desktop behind the pal
    private let keyNode: SCNNode
    private var ambientTimer: Timer?
    private let clips: [String: SCNNode]                  // wave / cheer (hidden)
    private let clipPlayers: [String: SCNAnimationPlayer]
    private var clipReturn: DispatchWorkItem?
    private var clipBusy = false

    init(frame: NSRect, objPath: String?, tint: NSColor) {
        let built = Pal3DView.buildScene(objPath: objPath, tint: tint)
        spinNode = built.spin
        modelHolder = built.model
        ambientNode = built.ambient
        keyNode = built.key
        clips = built.clips
        clipPlayers = built.clipPlayers
        super.init(frame: frame)
        wantsLayer = true

        scnView.frame = bounds
        scnView.autoresizingMask = [.width, .height]
        scnView.backgroundColor = .clear           // float over the desktop
        scnView.allowsCameraControl = false
        scnView.antialiasingMode = .multisampling4X
        scnView.isPlaying = true                    // keep the action loop alive
        scnView.scene = built.scene
        scnView.pointOfView = built.camera
        addSubview(scnView)

        startIdle()
        // Light the pal from his surroundings: sample the desktop pixels behind
        // him every ~1.5s and tint the ambient/key lights to match.
        ambientTimer = Timer.scheduledTimer(withTimeInterval: 1.5, repeats: true) { [weak self] _ in
            self?.sampleAmbient()
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) { [weak self] in self?.sampleAmbient() }
    }

    deinit { ambientTimer?.invalidate() }

    required init?(coder: NSCoder) { nil }

    // A light tint pulled toward the system accent — blends white→accent by k so
    // it nudges the berry's hue to match the machine's indigo without flattening
    // the texture detail (applied as a material multiply).
    static func accentTint(_ accent: NSColor, _ k: CGFloat) -> NSColor {
        let a = accent.usingColorSpace(.deviceRGB) ?? accent
        return NSColor(deviceRed: 1 - (1 - a.redComponent) * k,
                       green: 1 - (1 - a.greenComponent) * k,
                       blue: 1 - (1 - a.blueComponent) * k, alpha: 1)
    }

    // ── scene construction (camera · lights · centered mesh) ────────────────
    static func buildScene(objPath: String?, tint: NSColor) -> Pal3DScene {
        let scene = SCNScene()

        let camDist: CGFloat = 3.4
        let vFOV: CGFloat = 55
        let cam = SCNNode()
        cam.camera = SCNCamera()
        cam.camera?.fieldOfView = vFOV
        cam.camera?.projectionDirection = .vertical   // fix the VERTICAL fov so the
        // pal's size is independent of the view's aspect (square snapshot vs the
        // landscape glyph frame) — no more "looks fine offscreen, tiny live".
        cam.position = SCNVector3(x: 0, y: 0, z: camDist)
        scene.rootNode.addChildNode(cam)

        let key = SCNNode()
        key.light = SCNLight(); key.light!.type = .directional; key.light!.intensity = 850
        key.eulerAngles = SCNVector3(x: -0.6, y: 0.5, z: 0)
        scene.rootNode.addChildNode(key)
        let ambient = SCNNode()
        ambient.light = SCNLight(); ambient.light!.type = .ambient; ambient.light!.intensity = 480
        scene.rootNode.addChildNode(ambient)

        let spin = SCNNode()
        let model = SCNNode()
        spin.addChildNode(model)
        scene.rootNode.addChildNode(spin)

        if let p = objPath, FileManager.default.fileExists(atPath: p),
           let loaded = loadModel(p, tint: tint) {
            model.addChildNode(loaded)
        } else {
            model.addChildNode(placeholderBlueberry())
        }
        // ── viewport-containment constraint ─────────────────────────────────
        // The character must NEVER render outside its viewport, in any pose. So
        // we fit the REST pose small enough that its most extreme animated reach
        // (raised arms, plus the idle float) still sits inside the visible frame:
        //
        //   restHeight · ANIM_REACH  ≤  visibleHeight · VIEWPORT_FILL
        //
        // visibleHeight is the world-space height the camera sees at the model
        // plane: 2·dist·tan(vFOV/2). Solving for the fit target:
        let visibleHeight = 2 * camDist * tan(vFOV * 0.5 * .pi / 180)
        let ANIM_REACH: CGFloat = 1.7    // tallest clip (arms up) vs rest, + float headroom
        let VIEWPORT_FILL: CGFloat = 0.95
        normalizeToFit(model, target: visibleHeight * VIEWPORT_FILL / ANIM_REACH)

        // Extra one-shot clips (wave/cheer) — sibling <stem>-<clip>.usdc models,
        // loaded hidden + paused; playClip() swaps them in for a beat. (Swapping
        // whole rigged models avoids cross-file skeletal retargeting.)
        var clips: [String: SCNNode] = [:]
        var clipPlayers: [String: SCNAnimationPlayer] = [:]
        if let p = objPath {
            let dir = (p as NSString).deletingLastPathComponent
            let stem = ((p as NSString).lastPathComponent as NSString).deletingPathExtension
            for clip in ["wave", "cheer"] {
                let cp = "\(dir)/\(stem)-\(clip).usdc"
                guard FileManager.default.fileExists(atPath: cp),
                      // Clips (neo-wave/neo-cheer) carry no PBR sidecars of their
                      // own — reuse the idle model's textures (neo-base_color.png)
                      // so a hover/cheer swap stays textured, not raw-material green.
                      let n = loadUSD(cp, tint: tint, loop: false, textureStem: stem) else { continue }
                // Same rig as the idle → reuse the idle's exact fit transform so
                // swapping clips never zooms/shifts the camera framing. (Fitting
                // each independently differs because they load at different poses.)
                n.scale = model.scale
                n.pivot = model.pivot
                n.position = model.position
                n.isHidden = true
                spin.addChildNode(n)
                clips[clip] = n
                n.enumerateChildNodes { cn, _ in
                    for k in cn.animationKeys where clipPlayers[clip] == nil {
                        if let pl = cn.animationPlayer(forKey: k) { pl.stop(); clipPlayers[clip] = pl }
                    }
                }
            }
        }
        return Pal3DScene(scene: scene, camera: cam, spin: spin, model: model,
                          ambient: ambient, key: key, clips: clips, clipPlayers: clipPlayers)
    }

    // Pick a loader by extension: animated USD/USDZ (the rigged blueberry,
    // converted from Meshy's GLB via usdcat) vs a static OBJ.
    private static func loadModel(_ path: String, tint: NSColor) -> SCNNode? {
        let ext = (path as NSString).pathExtension.lowercased()
        if ["usdz", "usdc", "usda", "usd", "scn"].contains(ext) { return loadUSD(path, tint: tint) }
        return loadOBJ(path, tint: tint)
    }

    // ── load a rigged USD/USDZ and loop its skeletal animation ──────────────
    // SceneKit reads UsdSkel animation natively; .playRepeatedly auto-starts and
    // loops every embedded clip. glb→usd conversion can drop textures, so the
    // sibling PBR maps are re-applied (same as the OBJ path).
    private static func loadUSD(_ path: String, tint: NSColor, loop: Bool = true,
                                textureStem: String? = nil) -> SCNNode? {
        let url = URL(fileURLWithPath: path)
        let policy: SCNSceneSource.AnimationImportPolicy = loop ? .playRepeatedly : .doNotPlay
        guard let scene = try? SCNScene(url: url, options: [.animationImportPolicy: policy]) else { return nil }
        let holder = SCNNode()
        for child in scene.rootNode.childNodes { holder.addChildNode(child) }
        if holder.childNodes.isEmpty { return nil }

        let dir = url.deletingLastPathComponent()
        // Clips pass `textureStem` to reuse the idle model's PBR maps (their own
        // <stem>-base_color.png doesn't exist); the idle uses its own stem.
        let stem = textureStem ?? url.deletingPathExtension().lastPathComponent   // e.g. "blueberry" / "neo"
        let tintColor = accentTint(tint, 0.5)   // pull the model toward the accent indigo
        func map(_ suffix: String) -> NSImage? {
            for n in ["\(stem)-\(suffix).png", "blueberry-\(suffix).png"] {
                let p = dir.appendingPathComponent(n)
                if FileManager.default.fileExists(atPath: p.path) { return NSImage(contentsOf: p) }
            }
            return nil
        }
        let base = map("base_color"), normal = map("normal"), rough = map("roughness")
        holder.enumerateChildNodes { node, _ in
            for m in node.geometry?.materials ?? [] {
                m.lightingModel = .physicallyBased
                if let base = base { m.diffuse.contents = base }
                if let normal = normal { m.normal.contents = normal }
                if let rough = rough { m.roughness.contents = rough }
                m.multiply.contents = tintColor
            }
            // The looping idle auto-plays + repeats forever; one-shot clips are
            // left paused (caller grabs the player and triggers it on demand).
            if loop {
                for key in node.animationKeys {
                    if let player = node.animationPlayer(forKey: key) {
                        player.animation.repeatCount = .greatestFiniteMagnitude
                        player.play()
                    }
                }
            }
        }
        return holder
    }

    // ── load a Meshy OBJ + its base-color texture ──────────────────────────
    private static func loadOBJ(_ path: String, tint: NSColor) -> SCNNode? {
        let url = URL(fileURLWithPath: path)
        let asset = MDLAsset(url: url)
        asset.loadTextures()
        let scene = SCNScene(mdlAsset: asset)
        let holder = SCNNode()
        for child in scene.rootNode.childNodes { holder.addChildNode(child) }
        if holder.childNodes.isEmpty { return nil }

        // Meshy ships PBR maps as siblings (blueberry-base_color.png, …). The OBJ
        // itself may carry no material binding, so wire base_color in by hand.
        // Meshy ships PBR maps as siblings (blueberry-base_color.png, …). The OBJ
        // carries no usable material binding, so wire the maps in by hand — and
        // FORCE them on: Meshy's default material isn't nil, so a "fill only if
        // empty" check would skip base_color and leave the berry flat gray.
        let stem = url.deletingPathExtension().lastPathComponent
        let dir = url.deletingLastPathComponent()
        func map(_ suffix: String) -> NSImage? {
            for n in ["\(stem)-\(suffix).png", "blueberry-\(suffix).png"] {
                let p = dir.appendingPathComponent(n)
                if FileManager.default.fileExists(atPath: p.path) { return NSImage(contentsOf: p) }
            }
            return nil
        }
        let base = map("base_color"), normal = map("normal"), rough = map("roughness")
        let tintColor = accentTint(tint, 0.5)
        holder.enumerateChildNodes { node, _ in
            for m in node.geometry?.materials ?? [] {
                m.lightingModel = .physicallyBased
                if let base = base { m.diffuse.contents = base }
                if let normal = normal { m.normal.contents = normal }
                if let rough = rough { m.roughness.contents = rough }
                m.multiply.contents = tintColor
            }
        }
        return holder
    }

    // ── procedural stand-in: a plump berry with a calyx star + leaf ─────────
    private static func placeholderBlueberry() -> SCNNode {
        let node = SCNNode()

        let berry = SCNSphere(radius: 0.92)
        berry.segmentCount = 64
        let skin = SCNMaterial()
        skin.lightingModel = .physicallyBased
        skin.diffuse.contents = NSColor(calibratedRed: 0.21, green: 0.25, blue: 0.55, alpha: 1)
        skin.roughness.contents = 0.78
        skin.metalness.contents = 0.0
        berry.materials = [skin]
        let berryNode = SCNNode(geometry: berry)
        berryNode.scale = SCNVector3(x: 1, y: 0.96, z: 1)   // slightly squat
        node.addChildNode(berryNode)

        // calyx — a small dark dimpled disk at the top
        let calyx = SCNCylinder(radius: 0.2, height: 0.05)
        let cm = SCNMaterial(); cm.diffuse.contents = NSColor(white: 0.14, alpha: 1)
        calyx.materials = [cm]
        let calyxNode = SCNNode(geometry: calyx)
        calyxNode.position = SCNVector3(x: 0, y: 0.84, z: 0)
        node.addChildNode(calyxNode)

        // one little leaf
        let leaf = SCNPyramid(width: 0.5, height: 0.55, length: 0.06)
        let lm = SCNMaterial(); lm.diffuse.contents = NSColor.systemGreen
        leaf.materials = [lm]
        let leafNode = SCNNode(geometry: leaf)
        leafNode.position = SCNVector3(x: 0.28, y: 0.96, z: 0)
        leafNode.eulerAngles = SCNVector3(x: 0, y: 0, z: -0.6)
        node.addChildNode(leafNode)

        return node
    }

    // Center the mesh on the spin axis and scale its largest dimension to `target`
    // world units (so any model — tiny or huge — frames the same).
    private static func normalizeToFit(_ node: SCNNode, target: CGFloat) {
        // Accumulate real geometry bounds across the hierarchy. flattenedClone()'s
        // boundingBox comes back empty for SKINNED meshes (the rigged USD), which
        // left the berry unscaled + uncentered (a tiny blob in the corner). Walking
        // each geometry node's box into the parent's space works for skinned too.
        var lo = SCNVector3(CGFloat.greatestFiniteMagnitude, .greatestFiniteMagnitude, .greatestFiniteMagnitude)
        var hi = SCNVector3(-CGFloat.greatestFiniteMagnitude, -.greatestFiniteMagnitude, -.greatestFiniteMagnitude)
        var found = false
        func expand(_ p: SCNVector3) {
            lo = SCNVector3(min(lo.x, p.x), min(lo.y, p.y), min(lo.z, p.z))
            hi = SCNVector3(max(hi.x, p.x), max(hi.y, p.y), max(hi.z, p.z))
            found = true
        }
        node.enumerateHierarchy { child, _ in
            // Every node origin — for a SKINNED mesh the geometry box is tiny
            // (authored pre-skin; the skinner's bind transform scales it ~100×,
            // which a node-transform walk can't see), but the skeleton joints'
            // origins span the real character, so they set the true extent.
            expand(node.convertPosition(SCNVector3Zero, from: child))
            guard child.geometry != nil else { return }
            let (bMin, bMax) = child.boundingBox
            for x in [bMin.x, bMax.x] { for y in [bMin.y, bMax.y] { for z in [bMin.z, bMax.z] {
                expand(node.convertPosition(SCNVector3(x, y, z), from: child))
            } } }
        }
        guard found else { return }
        node.pivot = SCNMatrix4MakeTranslation((lo.x + hi.x) / 2, (lo.y + hi.y) / 2, (lo.z + hi.z) / 2)
        let maxDim = max(hi.x - lo.x, max(hi.y - lo.y, hi.z - lo.z))
        if maxDim > 0 {
            let s = target / maxDim
            node.scale = SCNVector3(s, s, s)
        }
    }

    // ── idle life ───────────────────────────────────────────────────────────
    // No turntable spin — he faces front; any turning comes from the rig's own
    // clips (e.g. a walk). Just a gentle vertical float so he feels alive.
    private func startIdle() {
        if ProcessInfo.processInfo.environment["MACPAL_NO_IDLE"] != nil { return }
        let up = SCNAction.moveBy(x: 0, y: 0.05, z: 0, duration: 1.7)
        up.timingMode = .easeInEaseOut
        let bob = SCNAction.sequence([up, up.reversed()])
        spinNode.runAction(.repeatForever(bob), forKey: "bob")
    }

    // ── ambient lighting from the desktop ───────────────────────────────────
    // Tint the ambient + key lights toward the average color of the wallpaper on
    // the screen the pal sits on — so he picks up the room's mood and shifts when
    // dragged to a screen with a different desktop. (Wallpaper needs no Screen-
    // Recording permission; sampling live window pixels would require
    // ScreenCaptureKit + that permission — a possible upgrade later.)
    private func sampleAmbient() {
        guard let screen = window?.screen ?? NSScreen.main,
              let url = NSWorkspace.shared.desktopImageURL(for: screen),
              let img = NSImage(contentsOf: url),
              let avg = Pal3DView.averageColor(img) else { return }
        let amb = Pal3DView.mix(avg, .white, 0.45)   // toned toward white so he stays readable
        let keyC = Pal3DView.mix(avg, .white, 0.20)
        SCNTransaction.begin(); SCNTransaction.animationDuration = 0.9
        ambientNode.light?.color = amb
        keyNode.light?.color = keyC
        SCNTransaction.commit()
    }

    private static func averageColor(_ image: NSImage) -> NSColor? {
        guard let tiff = image.tiffRepresentation,
              let bm = NSBitmapImageRep(data: tiff), let cg = bm.cgImage else { return nil }
        var px = [UInt8](repeating: 0, count: 4)
        guard let cs = CGColorSpace(name: CGColorSpace.sRGB),
              let ctx = CGContext(data: &px, width: 1, height: 1, bitsPerComponent: 8,
                                  bytesPerRow: 4, space: cs,
                                  bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue) else { return nil }
        ctx.interpolationQuality = .medium
        ctx.draw(cg, in: CGRect(x: 0, y: 0, width: 1, height: 1))
        return NSColor(srgbRed: CGFloat(px[0]) / 255, green: CGFloat(px[1]) / 255,
                       blue: CGFloat(px[2]) / 255, alpha: 1)
    }

    private static func mix(_ a: NSColor, _ b: NSColor, _ t: CGFloat) -> NSColor {
        let x = a.usingColorSpace(.sRGB) ?? a, y = b.usingColorSpace(.sRGB) ?? b
        return NSColor(srgbRed: x.redComponent + (y.redComponent - x.redComponent) * t,
                       green: x.greenComponent + (y.greenComponent - x.greenComponent) * t,
                       blue: x.blueComponent + (y.blueComponent - x.blueComponent) * t, alpha: 1)
    }

    // ── offscreen snapshot (QA / debugging the model itself) ────────────────
    // Renders the exact same scene (load + fit + textures) to a PNG at a mid-
    // animation time, so framing/orientation/texture can be checked without the
    // tiny corner widget in the way. Used by `MacPal --snapshot <png>`.
    static func snapshot(objPath: String?, size: CGSize, time: TimeInterval = 1.5) -> NSImage? {
        let built = buildScene(objPath: objPath, tint: accent)
        guard let device = MTLCreateSystemDefaultDevice() else { return nil }
        let r = SCNRenderer(device: device, options: nil)
        r.scene = built.scene
        r.pointOfView = built.camera
        r.scene?.background.contents = NSColor(white: 0.12, alpha: 1)
        return r.snapshot(atTime: time, with: size, antialiasingMode: .multisampling4X)
    }

    /// A quick excited squash, called on each Menu Band note. Runs on spinNode
    /// so it reads whether the idle or a one-shot clip is currently showing.
    func sing() {
        let pop = SCNAction.sequence([
            SCNAction.scale(by: 1.12, duration: 0.12),
            SCNAction.scale(by: 1 / 1.12, duration: 0.14),
        ])
        pop.timingMode = .easeInEaseOut
        spinNode.runAction(pop, forKey: "singPop")
    }

    /// A musical note that rises out of the pal and fades — rendered INSIDE the
    /// scene (billboarded toward the camera) so it composites above the metal
    /// view, unlike a CALayer overlay which the SCNView paints over.
    func floatNote(_ glyph: String, color: NSColor, up: Bool = true) {
        guard let root = scnView.scene?.rootNode else { return }
        // A billboarded plane textured with the note glyph — renders reliably
        // (SCNText at sub-point sizes comes out degenerate).
        let plane = SCNPlane(width: 0.9, height: 0.9)
        let m = SCNMaterial()
        m.diffuse.contents = Pal3DView.noteImage(glyph, color)
        m.lightingModel = .constant
        m.isDoubleSided = true
        m.blendMode = .alpha
        plane.materials = [m]
        let n = SCNNode(geometry: plane)
        let dir: CGFloat = up ? 1 : -1   // top-corner pal drifts notes DOWN (up exits the screen)
        n.position = SCNVector3(CGFloat.random(in: -0.3...0.3), 0.3 * dir, 0.9)
        n.constraints = [SCNBillboardConstraint()]
        root.addChildNode(n)
        let move = SCNAction.moveBy(x: CGFloat.random(in: -0.25...0.25), y: 1.3 * dir, z: 0, duration: 1.2)
        move.timingMode = .easeOut
        let fade = SCNAction.sequence([.wait(duration: 0.5), .fadeOut(duration: 0.7)])
        n.runAction(.sequence([.group([move, fade]), .removeFromParentNode()]))
    }

    private static func noteImage(_ glyph: String, _ color: NSColor) -> NSImage {
        let s: CGFloat = 128
        let img = NSImage(size: NSSize(width: s, height: s))
        img.lockFocus()
        let para = NSMutableParagraphStyle(); para.alignment = .center
        // Sharp dark drop shadow (no blur, offset down-right) — crisp comic pop,
        // matching the house style's hard offset shadows.
        let drop = NSShadow()
        drop.shadowColor = NSColor.black.withAlphaComponent(0.9)
        drop.shadowBlurRadius = 0
        drop.shadowOffset = NSSize(width: 2.5, height: -2.5)
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.boldSystemFont(ofSize: 86),
            .foregroundColor: color,
            .paragraphStyle: para,
            .strokeColor: NSColor.black.withAlphaComponent(0.5),
            .strokeWidth: -3.0,
            .shadow: drop,
        ]
        let str = glyph as NSString
        let sz = str.size(withAttributes: attrs)
        str.draw(at: NSPoint(x: (s - sz.width) / 2, y: (s - sz.height) / 2), withAttributes: attrs)
        img.unlockFocus()
        return img
    }

    /// Play a one-shot clip (e.g. "wave" on hover, "cheer" on a note) by swapping
    /// the visible model for that clip's rigged node for the clip's duration,
    /// then returning to the looping idle. No-op if a clip is already running.
    func playClip(_ name: String) {
        guard !clipBusy, let node = clips[name], let player = clipPlayers[name] else { return }
        clipBusy = true
        clipReturn?.cancel()
        modelHolder.isHidden = true
        node.isHidden = false
        player.stop()
        player.animation.repeatCount = 1
        player.play()
        let dur = player.animation.duration > 0.1 ? player.animation.duration : 2.5
        let work = DispatchWorkItem { [weak self] in
            guard let self = self else { return }
            node.isHidden = true
            self.modelHolder.isHidden = false
            self.clipBusy = false
        }
        clipReturn = work
        DispatchQueue.main.asyncAfter(deadline: .now() + dur, execute: work)
    }
}
