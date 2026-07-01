import SceneKit
import simd

/// A unique low-poly rock mesh per prompt: a once-subdivided icosahedron whose
/// vertices are pushed in/out by seeded noise (so it's a lumpy boulder, not a
/// ball), emitted as flat-shaded facets with spherical UVs — so a horizontally
/// banded strata texture wraps around it as latitude layers. Double-sided with
/// outward-forced normals, so it renders correctly regardless of winding.
///
/// 80 facets — cheap to build and to draw; the whole rock is a handful of KB.
enum SigilMesh {
    private struct RNG {
        var s: UInt64
        init(_ seed: UInt64) { s = seed == 0 ? 0x9e37_79b9_7f4a_7c15 : seed }
        mutating func next() -> UInt64 {
            s = s &+ 0x9e37_79b9_7f4a_7c15
            var z = s
            z = (z ^ (z >> 30)) &* 0xbf58_476d_1ce4_e5b9
            z = (z ^ (z >> 27)) &* 0x94d0_49bb_1331_11eb
            return z ^ (z >> 31)
        }
        mutating func unit() -> Float { Float(next() >> 40) / Float(1 << 24) }
    }

    static func rock(seed: UInt64) -> SCNGeometry {
        var rng = RNG(seed)
        let t = Float((1.0 + 5.0.squareRoot()) / 2.0)

        var verts: [SIMD3<Float>] = [
            SIMD3(-1, t, 0), SIMD3(1, t, 0), SIMD3(-1, -t, 0), SIMD3(1, -t, 0),
            SIMD3(0, -1, t), SIMD3(0, 1, t), SIMD3(0, -1, -t), SIMD3(0, 1, -t),
            SIMD3(t, 0, -1), SIMD3(t, 0, 1), SIMD3(-t, 0, -1), SIMD3(-t, 0, 1),
        ].map { simd_normalize($0) }

        var faces: [(Int, Int, Int)] = [
            (0,11,5),(0,5,1),(0,1,7),(0,7,10),(0,10,11),
            (1,5,9),(5,11,4),(11,10,2),(10,7,6),(7,1,8),
            (3,9,4),(3,4,2),(3,2,6),(3,6,8),(3,8,9),
            (4,9,5),(2,4,11),(6,2,10),(8,6,7),(9,8,1),
        ]

        // Subdivision is hashed: ~a third stay at 20 raw faces (chunky, angular
        // crystals), the rest subdivide once to 80 (rounder boulders) — two
        // clearly different form families.
        let passes = rng.unit() < 0.34 ? 0 : 1
        var midCache: [UInt64: Int] = [:]
        func midpoint(_ a: Int, _ b: Int) -> Int {
            let key = a < b ? UInt64(a) << 32 | UInt64(b) : UInt64(b) << 32 | UInt64(a)
            if let m = midCache[key] { return m }
            verts.append(simd_normalize((verts[a] + verts[b]) * 0.5))
            let idx = verts.count - 1
            midCache[key] = idx
            return idx
        }
        for _ in 0..<passes {
            var next: [(Int, Int, Int)] = []
            for (a, b, c) in faces {
                let ab = midpoint(a, b), bc = midpoint(b, c), ca = midpoint(c, a)
                next += [(a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca)]
            }
            faces = next
        }

        // Radial displacement per shared vertex → lumpy boulder (amp ranges wide
        // so some are smooth, some craggy), then a hashed anisotropic
        // squash/stretch so forms span eggs, slabs, discs and blobs. Real
        // idiosyncrasy across the set.
        let amp: Float = 0.10 + 0.30 * rng.unit()
        let scale = SIMD3<Float>(0.66 + 0.64 * rng.unit(),
                                 0.66 + 0.64 * rng.unit(),
                                 0.66 + 0.64 * rng.unit())
        let displaced: [SIMD3<Float>] = verts.map { v in
            (v * (1 + (rng.unit() - 0.5) * 2 * amp)) * scale
        }

        func uv(_ p: SIMD3<Float>) -> CGPoint {
            let d = simd_normalize(p)
            let u = 0.5 + atan2(d.z, d.x) / (2 * .pi)
            let v = 0.5 - asin(max(-1, min(1, d.y))) / .pi
            return CGPoint(x: CGFloat(u), y: CGFloat(v))
        }

        var positions: [SCNVector3] = []
        var normals: [SCNVector3] = []
        var uvs: [CGPoint] = []
        var indices: [Int32] = []
        positions.reserveCapacity(faces.count * 3)

        for (a, b, c) in faces {
            let pa = displaced[a], pb = displaced[b], pc = displaced[c]
            var n = simd_normalize(simd_cross(pb - pa, pc - pa))
            if simd_dot(n, (pa + pb + pc) / 3) < 0 { n = -n }   // force outward
            var ua = uv(pa), ub = uv(pb), uc = uv(pc)
            // Seam fix: a facet straddling u=0/1 gets its low u's pushed up.
            let lo = min(ua.x, ub.x, uc.x), hi = max(ua.x, ub.x, uc.x)
            if hi - lo > 0.5 {
                if ua.x < 0.5 { ua.x += 1 }
                if ub.x < 0.5 { ub.x += 1 }
                if uc.x < 0.5 { uc.x += 1 }
            }
            let base = Int32(positions.count)
            let nv = SCNVector3(CGFloat(n.x), CGFloat(n.y), CGFloat(n.z))
            for (p, tex) in [(pa, ua), (pb, ub), (pc, uc)] {
                positions.append(SCNVector3(CGFloat(p.x), CGFloat(p.y), CGFloat(p.z)))
                normals.append(nv)
                uvs.append(tex)
            }
            indices += [base, base + 1, base + 2]
        }

        let geo = SCNGeometry(
            sources: [
                SCNGeometrySource(vertices: positions),
                SCNGeometrySource(normals: normals),
                SCNGeometrySource(textureCoordinates: uvs),
            ],
            elements: [SCNGeometryElement(indices: indices, primitiveType: .triangles)]
        )
        return geo
    }
}
