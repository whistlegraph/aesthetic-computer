// Mesh.swift — CPU-side geometry builders. Everything is baked in world space
// (model = identity) since the arena is static; only the camera moves.
//
// Two vertex formats: lit triangles (position, normal, color) and flat lines
// (position, color). Layouts are explicit float fields so the Metal vertex
// descriptor offsets are unambiguous.

import simd

struct TriVertex {
    var px: Float, py: Float, pz: Float       // position  (offset 0)
    var nx: Float, ny: Float, nz: Float       // normal    (offset 12)
    var r: Float, g: Float, b: Float, a: Float // color     (offset 24)
}

struct LineVertex {
    var px: Float, py: Float, pz: Float        // position  (offset 0)
    var r: Float, g: Float, b: Float, a: Float // color     (offset 12)
}

struct WorldMesh {
    var tris: [TriVertex] = []
    var lines: [LineVertex] = []

    mutating func addTri(_ a: SIMD3<Float>, _ b: SIMD3<Float>, _ c: SIMD3<Float>,
                         normal n: SIMD3<Float>, color col: SIMD4<Float>) {
        for p in [a, b, c] {
            tris.append(TriVertex(px: p.x, py: p.y, pz: p.z,
                                  nx: n.x, ny: n.y, nz: n.z,
                                  r: col.x, g: col.y, b: col.z, a: col.w))
        }
    }

    mutating func addLine(_ a: SIMD3<Float>, _ b: SIMD3<Float>, color col: SIMD4<Float>) {
        lines.append(LineVertex(px: a.x, py: a.y, pz: a.z, r: col.x, g: col.y, b: col.z, a: col.w))
        lines.append(LineVertex(px: b.x, py: b.y, pz: b.z, r: col.x, g: col.y, b: col.z, a: col.w))
    }

    // A flat-shaded quad (two tris) with a single normal.
    mutating func addQuad(_ a: SIMD3<Float>, _ b: SIMD3<Float>, _ c: SIMD3<Float>,
                          _ d: SIMD3<Float>, normal n: SIMD3<Float>, color col: SIMD4<Float>) {
        addTri(a, b, c, normal: n, color: col)
        addTri(a, c, d, normal: n, color: col)
    }
}

enum MeshBuilder {
    // Tessellated ground plane (checkerboard tiles) + grid lines, matching the
    // web arena's "large pre-tessellated ground".
    static func buildGround(into mesh: inout WorldMesh) {
        let b = ArenaWorld.bounds
        let y = Float(ArenaWorld.groundY)
        let step: Float = 2
        let up = SIMD3<Float>(0, 1, 0)
        let dark = SIMD4<Float>(0.10, 0.11, 0.14, 1)
        let light = SIMD4<Float>(0.15, 0.16, 0.20, 1)
        let grid = SIMD4<Float>(0.30, 0.34, 0.42, 0.5)

        var ix = 0
        var x = Float(b.xMin)
        while x < Float(b.xMax) - 1e-3 {
            var iz = 0
            var z = Float(b.zMin)
            while z < Float(b.zMax) - 1e-3 {
                let x0 = x, x1 = x + step, z0 = z, z1 = z + step
                let col = ((ix + iz) & 1) == 0 ? dark : light
                mesh.addQuad(
                    SIMD3(x0, y, z0), SIMD3(x0, y, z1),
                    SIMD3(x1, y, z1), SIMD3(x1, y, z0),
                    normal: up, color: col)
                z += step; iz += 1
            }
            x += step; ix += 1
        }

        // Grid lines, lifted slightly to avoid z-fighting with the tiles.
        let gy = y + 0.01
        var gx = Float(b.xMin)
        while gx <= Float(b.xMax) + 1e-3 {
            mesh.addLine(SIMD3(gx, gy, Float(b.zMin)), SIMD3(gx, gy, Float(b.zMax)), color: grid)
            gx += step
        }
        var gz = Float(b.zMin)
        while gz <= Float(b.zMax) + 1e-3 {
            mesh.addLine(SIMD3(Float(b.xMin), gy, gz), SIMD3(Float(b.xMax), gy, gz), color: grid)
            gz += step
        }

        // Bright perimeter outline (the platform edge).
        let edge = SIMD4<Float>(0.9, 0.7, 0.3, 1)
        let ey = y + 0.02
        let corners = [
            SIMD3<Float>(Float(b.xMin), ey, Float(b.zMin)),
            SIMD3<Float>(Float(b.xMax), ey, Float(b.zMin)),
            SIMD3<Float>(Float(b.xMax), ey, Float(b.zMax)),
            SIMD3<Float>(Float(b.xMin), ey, Float(b.zMax)),
        ]
        for i in 0..<4 { mesh.addLine(corners[i], corners[(i + 1) % 4], color: edge) }
    }

    static func buildObstacles(into mesh: inout WorldMesh) {
        for (i, o) in ArenaWorld.obstacles.enumerated() {
            let base = ArenaWorld.obstacleColors[i % ArenaWorld.obstacleColors.count]
            let col = SIMD4<Float>(base.x, base.y, base.z, 1)
            let edgeCol = SIMD4<Float>(min(1, base.x + 0.4), min(1, base.y + 0.4), min(1, base.z + 0.4), 1)
            switch o.kind {
            case .box:
                buildBox(into: &mesh,
                         xMin: Float(o.xMin), xMax: Float(o.xMax),
                         yMin: Float(o.yMin ?? 0), yMax: Float(o.yMax ?? 1),
                         zMin: Float(o.zMin), zMax: Float(o.zMax),
                         color: col, edge: edgeCol)
            case .cylinder:
                buildCylinder(into: &mesh,
                              cx: Float(o.x), cz: Float(o.z), r: Float(o.r),
                              yMin: Float(o.yMin ?? 0), yMax: Float(o.yMax ?? 1),
                              color: col, edge: edgeCol)
            }
        }
    }

    static func buildBox(into mesh: inout WorldMesh,
                         xMin: Float, xMax: Float, yMin: Float, yMax: Float,
                         zMin: Float, zMax: Float,
                         color col: SIMD4<Float>, edge: SIMD4<Float>) {
        func v(_ x: Float, _ y: Float, _ z: Float) -> SIMD3<Float> { SIMD3(x, y, z) }
        let dark = SIMD4<Float>(col.x * 0.6, col.y * 0.6, col.z * 0.6, 1)
        let top  = SIMD4<Float>(min(1, col.x * 1.2), min(1, col.y * 1.2), min(1, col.z * 1.2), 1)
        // +X / -X
        mesh.addQuad(v(xMax,yMin,zMin), v(xMax,yMax,zMin), v(xMax,yMax,zMax), v(xMax,yMin,zMax), normal: SIMD3(1,0,0), color: col)
        mesh.addQuad(v(xMin,yMin,zMax), v(xMin,yMax,zMax), v(xMin,yMax,zMin), v(xMin,yMin,zMin), normal: SIMD3(-1,0,0), color: col)
        // +Z / -Z
        mesh.addQuad(v(xMax,yMin,zMax), v(xMax,yMax,zMax), v(xMin,yMax,zMax), v(xMin,yMin,zMax), normal: SIMD3(0,0,1), color: dark)
        mesh.addQuad(v(xMin,yMin,zMin), v(xMin,yMax,zMin), v(xMax,yMax,zMin), v(xMax,yMin,zMin), normal: SIMD3(0,0,-1), color: dark)
        // top / bottom
        mesh.addQuad(v(xMin,yMax,zMin), v(xMin,yMax,zMax), v(xMax,yMax,zMax), v(xMax,yMax,zMin), normal: SIMD3(0,1,0), color: top)
        mesh.addQuad(v(xMin,yMin,zMax), v(xMin,yMin,zMin), v(xMax,yMin,zMin), v(xMax,yMin,zMax), normal: SIMD3(0,-1,0), color: dark)
        // top rim outline
        let rim = [v(xMin,yMax,zMin), v(xMax,yMax,zMin), v(xMax,yMax,zMax), v(xMin,yMax,zMax)]
        for i in 0..<4 { mesh.addLine(rim[i], rim[(i + 1) % 4], color: edge) }
    }

    static func buildCylinder(into mesh: inout WorldMesh,
                              cx: Float, cz: Float, r: Float, yMin: Float, yMax: Float,
                              color col: SIMD4<Float>, edge: SIMD4<Float>, segments: Int = 20) {
        let top = SIMD4<Float>(min(1, col.x * 1.2), min(1, col.y * 1.2), min(1, col.z * 1.2), 1)
        let n = segments
        var prev = SIMD2<Float>(cos(0) * r, sin(0) * r)
        let topCenter = SIMD3<Float>(cx, yMax, cz)
        for i in 1...n {
            let a = Float(i) / Float(n) * 2 * .pi
            let cur = SIMD2<Float>(cos(a) * r, sin(a) * r)
            let p0 = SIMD3<Float>(cx + prev.x, yMin, cz + prev.y)
            let p1 = SIMD3<Float>(cx + cur.x,  yMin, cz + cur.y)
            let p2 = SIMD3<Float>(cx + cur.x,  yMax, cz + cur.y)
            let p3 = SIMD3<Float>(cx + prev.x, yMax, cz + prev.y)
            // outward radial normal (averaged for the quad)
            let mid = simd_normalize(SIMD3<Float>((prev.x + cur.x) * 0.5, 0, (prev.y + cur.y) * 0.5))
            mesh.addQuad(p0, p1, p2, p3, normal: mid, color: col)
            // top cap fan
            mesh.addTri(topCenter, p3, p2, normal: SIMD3(0, 1, 0), color: top)
            // top rim line
            mesh.addLine(p3, p2, color: edge)
            prev = cur
        }
    }
}
