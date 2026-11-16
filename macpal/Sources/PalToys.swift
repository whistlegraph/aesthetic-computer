// PalToys.swift — procedural 3D pals: panda, chicken, neo.
//
// Soft glossy toys built from SceneKit primitives (and one cut-gem mesh), so
// they need no model files and no generator. Pal3DView asks for one with a
// `toy:<name>` path in place of a mesh path. Features carry names — "eye",
// "mouth", "jaw" — which is all blink and sing need to find them.
import AppKit
import SceneKit

enum PalToy {
    static let names: Set<String> = ["panda", "chicken", "neo"]

    static func path(_ name: String) -> String { "toy:" + name }

    static func name(_ path: String?) -> String? {
        guard let p = path, p.hasPrefix("toy:") else { return nil }
        let n = String(p.dropFirst(4))
        return names.contains(n) ? n : nil
    }

    static func build(_ path: String?) -> SCNNode? {
        guard let n = name(path) else { return nil }
        let toy = n == "panda" ? panda() : n == "chicken" ? chicken() : neo()
        let env = ProcessInfo.processInfo.environment
        let still = env["MACPAL_NO_IDLE"] != nil
            || NSWorkspace.shared.accessibilityDisplayShouldReduceMotion
        if !still { live(toy, n) }
        return toy
    }

    // ── materials ───────────────────────────────────────────────────────────
    private static func hex(_ v: Int) -> NSColor {
        NSColor(srgbRed: CGFloat((v >> 16) & 255) / 255, green: CGFloat((v >> 8) & 255) / 255,
                blue: CGFloat(v & 255) / 255, alpha: 1)
    }

    // Toy plastic: a broad soft highlight plus a pale rim, so the pal sits with
    // the wallpaper's glass bubbles without an environment map.
    private static func gloss(_ c: Int, shine: CGFloat = 0.55, glow: CGFloat = 0) -> SCNMaterial {
        let m = SCNMaterial()
        m.lightingModel = .blinn
        m.diffuse.contents = hex(c)
        m.specular.contents = NSColor(white: shine, alpha: 1)
        m.shininess = 0.35
        if glow > 0 { m.emission.contents = hex(c).blended(withFraction: 1 - glow, of: .black) }
        return m
    }

    // ── parts ───────────────────────────────────────────────────────────────
    private static func blob(_ r: CGFloat, _ m: SCNMaterial,
                             _ sx: CGFloat = 1, _ sy: CGFloat = 1, _ sz: CGFloat = 1,
                             seg: Int = 20) -> SCNNode {
        let s = SCNSphere(radius: r)
        s.segmentCount = seg
        s.materials = [m]
        let n = SCNNode(geometry: s)
        n.scale = SCNVector3(sx, sy, sz)
        return n
    }

    // Sit a feature on a round head: its local +z points out of the surface.
    private static func stick(_ n: SCNNode, on head: SCNNode,
                              yaw: CGFloat, pitch: CGFloat, r: CGFloat = 1, roll: CGFloat = 0) {
        let d = CGFloat.pi / 180
        let turn = SCNNode(), lift = SCNNode()
        turn.eulerAngles.y = yaw * d
        lift.eulerAngles.x = -pitch * d
        n.position = SCNVector3(0, 0, r)
        n.eulerAngles.z = roll * d
        lift.addChildNode(n); turn.addChildNode(lift); head.addChildNode(turn)
    }

    private static func cheek(_ r: CGFloat) -> SCNNode {
        let m = gloss(0xff9fb4, shine: 0.2)
        m.transparency = 0.75
        return blob(r, m, 1, 0.72, 0.22, seg: 14)
    }

    // A dark bead with a glint — the shared eye, so the pals read as siblings.
    private static func eye(_ r: CGFloat, _ c: Int = 0x26262b) -> SCNNode {
        let e = blob(r, gloss(c, shine: 0.9), 1, 1.12, 0.55, seg: 16)
        let glint = SCNMaterial()
        glint.lightingModel = .constant
        glint.diffuse.contents = NSColor.white
        let g = blob(r * 0.34, glint, seg: 10)
        g.position = SCNVector3(r * 0.32, r * 0.38, r * 0.8)
        e.addChildNode(g)
        e.name = "eye"
        return e
    }

    // A crescent smile, extruded a hair; sing stretches it open.
    private static func smile(_ w: CGFloat, _ c: Int) -> SCNNode {
        let h = w * 0.55
        let p = NSBezierPath()
        p.move(to: NSPoint(x: -w, y: 0))
        p.curve(to: NSPoint(x: w, y: 0), controlPoint1: NSPoint(x: -w * 0.5, y: -h * 1.7),
                controlPoint2: NSPoint(x: w * 0.5, y: -h * 1.7))
        p.curve(to: NSPoint(x: -w, y: 0), controlPoint1: NSPoint(x: w * 0.5, y: -h * 0.75),
                controlPoint2: NSPoint(x: -w * 0.5, y: -h * 0.75))
        p.close()
        p.flatness = w / 40
        let s = SCNShape(path: p, extrusionDepth: w * 0.2)
        s.materials = [gloss(c, shine: 0.3)]
        let n = SCNNode(geometry: s)
        n.name = "mouth"
        return n
    }

    // ── panda ───────────────────────────────────────────────────────────────
    private static func panda() -> SCNNode {
        let root = SCNNode()
        let white = gloss(0xf4f5f1), black = gloss(0x34343c, shine: 0.75)
        let head = blob(1, white, seg: 48)
        root.addChildNode(head)
        for s: CGFloat in [-1, 1] {
            let ear = blob(0.4, black, 1, 1, 0.62)
            ear.position = SCNVector3(0.72 * s, 0.72, -0.08)
            root.addChildNode(ear)
            stick(blob(0.34, black, 0.84, 1.14, 0.3), on: root, yaw: 24 * s, pitch: 6, r: 0.955, roll: 20 * s)
            // a white bead inside the black patch, pupil forward
            let ball = blob(0.11, gloss(0xffffff, shine: 0.9), 1, 1, 0.6, seg: 16)
            let pupil = blob(0.058, black, 1, 1, 0.5, seg: 12)
            pupil.position = SCNVector3(-0.012 * s, -0.005, 0.085)
            ball.addChildNode(pupil)
            ball.name = "eye"
            stick(ball, on: root, yaw: 23 * s, pitch: 8, r: 1.03)
            stick(cheek(0.17), on: root, yaw: 44 * s, pitch: -18, r: 0.965)
        }
        stick(blob(0.115, black, 1.25, 0.82, 0.6), on: root, yaw: 0, pitch: -8, r: 0.99)
        stick(smile(0.15, 0x2a2a30), on: root, yaw: 0, pitch: -19, r: 0.985)
        root.scale = SCNVector3(1.08, 0.95, 1)
        let hold = SCNNode()
        hold.addChildNode(root)
        return hold
    }

    // ── chicken ─────────────────────────────────────────────────────────────
    private static func chicken() -> SCNNode {
        let root = SCNNode()
        let yellow = gloss(0xffdc45, glow: 0.12), deep = gloss(0xffc71f, glow: 0.1)
        let orange = gloss(0xff9a1f), red = gloss(0xff4f4a)
        root.addChildNode(blob(1, yellow, seg: 48))
        for (x, y, r) in [(-0.24, 0.94, 0.19), (0, 1.04, 0.24), (0.24, 0.94, 0.19)] as [(CGFloat, CGFloat, CGFloat)] {
            let c = blob(r, red, 1, 1, 0.75)
            c.position = SCNVector3(x, y, 0.05)
            root.addChildNode(c)
        }
        for s: CGFloat in [-1, 1] {
            stick(eye(0.115), on: root, yaw: 24 * s, pitch: 12, r: 0.975)
            stick(cheek(0.17), on: root, yaw: 44 * s, pitch: -12, r: 0.965)
            let wing = blob(0.42, deep, 0.42, 0.9, 1)
            wing.position = SCNVector3(0.96 * s, -0.12, 0)
            wing.eulerAngles.z = 0.3 * s
            root.addChildNode(wing)
            let foot = blob(0.17, orange, 1, 0.5, 1.4, seg: 12)
            foot.position = SCNVector3(0.32 * s, -0.98, 0.22)
            root.addChildNode(foot)
        }
        // beak: two flattened cones; the lower one is the jaw that drops to sing
        let beak = SCNNode()
        for (name, sy, drop) in [("", 0.62, 0.0), ("jaw", 0.4, 0.05)] as [(String, CGFloat, CGFloat)] {
            let c = SCNCone(topRadius: 0.02, bottomRadius: 0.24, height: 0.4)
            c.radialSegmentCount = 16
            c.materials = [orange]
            let n = SCNNode(geometry: c)
            n.eulerAngles.x = .pi / 2          // point the cone out along +z
            n.position = SCNVector3(0, -drop, 0.12)
            let hinge = SCNNode()
            hinge.scale = SCNVector3(1, sy, 1)
            hinge.addChildNode(n)
            if !name.isEmpty { hinge.name = name }
            beak.addChildNode(hinge)
        }
        stick(beak, on: root, yaw: 0, pitch: -6, r: 0.93)
        return root
    }

    // ── neo: a translucent lime cut gem; the face inside stays on you ───────
    private static func neo() -> SCNNode {
        let root = SCNNode()
        let table: (y: CGFloat, r: CGFloat) = (0.62, 0.56), girdle: (y: CGFloat, r: CGFloat) = (0.2, 1)
        let (cut, edges) = gem(8, table, girdle, -1.08)

        // Two passes of the one mesh — far facets, then near — neither writing
        // depth, so the inside of the stone shows through its front.
        let turn = SCNNode()
        turn.name = "turn"
        for (i, cull) in [SCNCullMode.front, .back].enumerated() {
            let g = cut.copy() as! SCNGeometry
            g.materials = [glass(cull, body: i == 0 ? 0.42 : 0.5)]
            let pass = SCNNode(geometry: g)
            pass.renderingOrder = i
            turn.addChildNode(pass)
        }
        edges.materials = [edgeLight()]
        let wire = SCNNode(geometry: edges)
        wire.renderingOrder = 2
        turn.addChildNode(wire)
        root.addChildNode(turn)

        // The face hangs still in the middle of the stone and draws after it.
        let face = SCNNode()
        face.position = SCNVector3(0, -0.02, 0.5)
        for s: CGFloat in [-1, 1] {
            let e = eye(0.15, 0x1c3a08)
            e.position = SCNVector3(0.25 * s, 0, 0)
            face.addChildNode(e)
            let c = cheek(0.11)
            c.position = SCNVector3(0.36 * s, -0.22, 0)
            face.addChildNode(c)
        }
        let m = smile(0.12, 0x1c3a08)
        m.position = SCNVector3(0, -0.2, 0)
        face.addChildNode(m)
        face.enumerateHierarchy { n, _ in n.renderingOrder = 3 }
        root.addChildNode(face)
        return root
    }

    // Gem glass. SceneKit only blends a shader modifier's alpha when it opens
    // with `#pragma transparent`, and it reads the colour as premultiplied —
    // so the body is scaled by alpha and the rim and glints are added on top.
    // The shader does its own lighting from fixed view-space lights: facets
    // step from deep green through lime to chartreuse, and the warm and cool
    // glints hop from facet to facet as the stone turns.
    private static func glass(_ cull: SCNCullMode, body: CGFloat) -> SCNMaterial {
        let m = SCNMaterial()
        m.lightingModel = .constant
        m.cullMode = cull
        m.writesToDepthBuffer = false
        m.blendMode = .alpha
        m.shaderModifiers = [.fragment: """
        #pragma transparent
        #pragma body
        float3 v = normalize(_surface.view);
        float3 n = normalize(_surface.normal);
        n = dot(n, v) < 0.0 ? -n : n;
        float facing = dot(n, v);
        float rim = pow(1.0 - facing, 2.0);
        float d = max(dot(n, normalize(float3(0.45, 0.7, 0.55))), 0.0);
        float3 deep = float3(0.42, 0.70, 0.02), mid = float3(0.67, 0.90, 0.12), hi = float3(0.87, 1.0, 0.36);
        float3 lit = mix(mix(deep, mid, saturate(d * 2.0)), hi, saturate(d * 2.0 - 1.0));
        lit = pow(lit + mid * 0.14 * facing, float3(2.2));    // sRGB picks → linear
        float warm = pow(max(dot(n, normalize(normalize(float3(0.55, 0.75, 0.6)) + v)), 0.0), 60.0);
        float cool = pow(max(dot(n, normalize(normalize(float3(-0.75, 0.25, 0.55)) + v)), 0.0), 40.0);
        float low = pow(max(dot(n, normalize(normalize(float3(0.25, -0.8, 0.5)) + v)), 0.0), 70.0);
        float a = mix(\(body), 0.92, rim);
        float3 c = lit * a
            + rim * float3(0.8, 1.0, 0.4) * 0.3
            + warm * float3(1.0, 0.97, 0.62)
            + cool * float3(0.8, 1.0, 0.72) * 0.7
            + low * float3(0.98, 1.0, 0.4) * 0.5;
        _output.color = float4(c, min(a + warm + cool * 0.7, 1.0));
        """]
        return m
    }

    // Facet edges as hairlines, added over the glass: faint all round, and a
    // catch-light that slides along whichever edges face the upper left.
    private static func edgeLight() -> SCNMaterial {
        let m = SCNMaterial()
        m.lightingModel = .constant
        m.writesToDepthBuffer = false
        m.readsFromDepthBuffer = false
        m.blendMode = .add
        m.shaderModifiers = [.fragment: """
        #pragma transparent
        #pragma body
        float3 n = normalize(_surface.normal);
        float front = smoothstep(-0.2, 0.3, dot(n, normalize(_surface.view)));
        float glint = pow(max(dot(n, normalize(float3(-0.45, 0.75, 0.5))), 0.0), 4.0);
        float k = 0.1 + front * (0.22 + glint);
        _output.color = float4(float3(0.94, 1.0, 0.68) * k, k);
        """]
        return m
    }

    // Flat-shaded brilliant cut: every triangle owns its normal, so each facet
    // catches the light as its own tone when the gem turns.
    private static func gem(_ n: Int, _ table: (y: CGFloat, r: CGFloat), _ girdle: (y: CGFloat, r: CGFloat),
                            _ culet: CGFloat) -> (faces: SCNGeometry, edges: SCNGeometry) {
        var vs: [SCNVector3] = [], ns: [SCNVector3] = [], es: [SCNVector3] = []
        func tri(_ a: SCNVector3, _ b: SCNVector3, _ c: SCNVector3) {
            let u = SCNVector3(b.x - a.x, b.y - a.y, b.z - a.z), v = SCNVector3(c.x - a.x, c.y - a.y, c.z - a.z)
            var k = SCNVector3(u.y * v.z - u.z * v.y, u.z * v.x - u.x * v.z, u.x * v.y - u.y * v.x)
            let len = sqrt(k.x * k.x + k.y * k.y + k.z * k.z)
            k = SCNVector3(k.x / len, k.y / len, k.z / len)
            // the gem is convex around the origin, so outward = away from it
            let out = k.x * (a.x + b.x + c.x) + k.y * (a.y + b.y + c.y) + k.z * (a.z + b.z + c.z) > 0
            vs += out ? [a, b, c] : [a, c, b]
            let f = out ? k : SCNVector3(-k.x, -k.y, -k.z)
            ns += [f, f, f]
        }
        func ring(_ p: (y: CGFloat, r: CGFloat), _ i: Int) -> SCNVector3 {
            let a = (CGFloat(i) + 0.5) * 2 * .pi / CGFloat(n)    // half-step: a facet faces +z
            return SCNVector3(p.r * cos(a), p.y, p.r * sin(a))
        }
        let top = SCNVector3(0, table.y, 0), tip = SCNVector3(0, culet, 0)
        for i in 0..<n {
            let t0 = ring(table, i), t1 = ring(table, i + 1), g0 = ring(girdle, i), g1 = ring(girdle, i + 1)
            tri(top, t0, t1)
            tri(t0, g0, g1); tri(t0, g1, t1)
            tri(g0, tip, g1)
            es += [t0, t1, g0, g1, t0, g0, g0, tip]
        }
        func mesh(_ v: [SCNVector3], _ n: [SCNVector3], _ kind: SCNGeometryPrimitiveType) -> SCNGeometry {
            SCNGeometry(sources: [SCNGeometrySource(vertices: v), SCNGeometrySource(normals: n)],
                        elements: [SCNGeometryElement(indices: (0..<Int32(v.count)).map { $0 }, primitiveType: kind)])
        }
        // an edge point's normal is just its direction from the middle
        let out = es.map { p -> SCNVector3 in
            let l = sqrt(p.x * p.x + p.y * p.y + p.z * p.z)
            return SCNVector3(p.x / l, p.y / l, p.z / l)
        }
        return (mesh(vs, ns, .triangles), mesh(es, out, .line))
    }

    // ── life ────────────────────────────────────────────────────────────────
    // The round pals look slowly side to side (chicken mirrors panda, so on
    // adjacent screens they glance toward each other); the gem turns under its face.
    private static func live(_ toy: SCNNode, _ name: String) {
        if name == "neo" {
            toy.childNode(withName: "turn", recursively: false)?
                .runAction(.repeatForever(.rotateBy(x: 0, y: 2 * .pi, z: 0, duration: 16)), forKey: "turn")
        } else {
            let dir: CGFloat = name == "panda" ? 1 : -1
            toy.eulerAngles.y = -0.32 * dir
            let look = SCNAction.rotateBy(x: 0, y: 0.64 * dir, z: 0, duration: 3.4)
            look.timingMode = .easeInEaseOut
            toy.runAction(.repeatForever(.sequence([look, .wait(duration: 0.8), look.reversed(),
                                                    .wait(duration: 0.8)])), forKey: "look")
        }
        let shut = SCNAction.customAction(duration: 0.16) { n, t in
            blink(n, 1 - abs(1 - t / 0.08))
        }
        let gap = name == "panda" ? 5.3 : name == "chicken" ? 4.4 : 6.1
        toy.runAction(.repeatForever(.sequence([.wait(duration: gap), shut])), forKey: "blink")
    }

    static func blink(_ toy: SCNNode, _ k: CGFloat) {
        for e in toy.childNodes(passingTest: { n, _ in n.name == "eye" }) {
            e.scale.y = (e.value(forKey: "restY") as? CGFloat ?? remember(e)) * (1 - 0.9 * k)
        }
    }

    private static func remember(_ e: SCNNode) -> CGFloat {
        e.setValue(e.scale.y, forKey: "restY")
        return e.scale.y
    }

    // Mouth pose, 0 shut … 1 wide: smiles stretch open, a beak drops its jaw.
    static func open(_ toy: SCNNode, _ k: CGFloat) {
        for n in toy.childNodes(passingTest: { n, _ in n.name == "mouth" || n.name == "jaw" }) {
            if n.name == "jaw" { n.eulerAngles.x = 0.55 * k }
            else { n.scale = SCNVector3(1 + 0.25 * k, 1 + 1.5 * k, 1) }
        }
    }

    static func sing(_ toy: SCNNode) {
        toy.runAction(.customAction(duration: 0.26) { n, t in
            open(n, sin(.pi * t / 0.26))
        }, forKey: "sing")
    }
}
