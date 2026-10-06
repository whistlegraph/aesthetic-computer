// Bake the canonical twelve-node Fuser mark — the same smooth-union field
// render-fuser-metaballs.swift ray-marches — into an indexed OBJ mesh with
// vertex normals, so SceneKit-based desktops can carry the mark as real
// geometry. Marching tetrahedra over a regular grid; vertices are shared along
// grid edges, so the surface is watertight and shades smoothly.
//
//   swiftc -O bake-fuser-mesh.swift -o bake-fuser-mesh
//   ./bake-fuser-mesh ../assets/fuser-mesh.obj [--step 0.02]

import Foundation
import simd

guard CommandLine.arguments.count >= 2 else {
    fputs("usage: bake-fuser-mesh <output.obj> [--step 0.02]\n", stderr)
    exit(2)
}
let outputPath = CommandLine.arguments[1]
let arguments = Array(CommandLine.arguments.dropFirst(2))
let step: Float = {
    guard let i = arguments.firstIndex(of: "--step"), arguments.indices.contains(i + 1),
          let v = Float(arguments[i + 1]), v > 0 else { return 0.02 }
    return v
}()

// Centers from captutor/assets/fuser-mark.svg (24×24 viewBox), normalized as
// in render-fuser-metaballs.swift, including its alternating depth.
let svgCenters: [(Float, Float)] = [
    (15.1507, 2.5838), (21.4201, 2.5838), (8.8818, 2.5838),
    (8.8818, 8.86312), (2.58145, 8.83282), (2.61171, 15.1369),
    (2.61171, 21.4162), (8.8818, 21.4162), (15.1507, 21.4162),
    (15.1507, 15.1369), (21.4201, 15.1369), (21.4201, 8.86312),
]
let centers: [SIMD3<Float>] = svgCenters.enumerated().map { index, point in
    SIMD3((point.0 - 12) / 7.75, (12 - point.1) / 7.75, sin(Float(index) * 1.71) * 0.055)
}
let radius: Float = 0.355
let blend: Float = 0.30

@inline(__always) func smoothMin(_ a: Float, _ b: Float, _ k: Float) -> Float {
    let h = max(k - abs(a - b), 0) / k
    return min(a, b) - h * h * k * 0.25
}
@inline(__always) func field(_ p: SIMD3<Float>) -> Float {
    var d = simd_length(p - centers[0]) - radius
    for c in centers.dropFirst() { d = smoothMin(d, simd_length(p - c) - radius, blend) }
    return d
}
func normal(_ p: SIMD3<Float>) -> SIMD3<Float> {
    let e: Float = 0.004
    let n = SIMD3(field(p + SIMD3(e, 0, 0)) - field(p - SIMD3(e, 0, 0)),
                  field(p + SIMD3(0, e, 0)) - field(p - SIMD3(0, e, 0)),
                  field(p + SIMD3(0, 0, e)) - field(p - SIMD3(0, 0, e)))
    return simd_normalize(n)
}

// Grid bounds: the mark plus its radius and blend, with a margin.
let lo = SIMD3<Float>(-1.75, -1.75, -0.55)
let hi = SIMD3<Float>(1.75, 1.75, 0.55)
let nx = Int(((hi.x - lo.x) / step).rounded(.up)) + 1
let ny = Int(((hi.y - lo.y) / step).rounded(.up)) + 1
let nz = Int(((hi.z - lo.z) / step).rounded(.up)) + 1
func gridPoint(_ i: Int, _ j: Int, _ k: Int) -> SIMD3<Float> {
    SIMD3(lo.x + Float(i) * step, lo.y + Float(j) * step, lo.z + Float(k) * step)
}
func gridIndex(_ i: Int, _ j: Int, _ k: Int) -> Int { (i * ny + j) * nz + k }

var values = [Float](repeating: 0, count: nx * ny * nz)
DispatchQueue.concurrentPerform(iterations: nx) { i in
    for j in 0..<ny {
        for k in 0..<nz {
            values[gridIndex(i, j, k)] = field(gridPoint(i, j, k))
        }
    }
}

// Shared vertex per grid edge: key = (lower corner index, axis) encoded.
var vertexForEdge: [Int: Int] = [:]
var positions: [SIMD3<Float>] = []
var triangles: [(Int, Int, Int)] = []

// Tetrahedra also cut cube face and body diagonals, so key on both corners.
func edgeKey(_ a: Int, _ b: Int) -> Int { a < b ? a &* (nx * ny * nz) &+ b : b &* (nx * ny * nz) &+ a }

func vertex(_ a: Int, _ b: Int, _ pa: SIMD3<Float>, _ pb: SIMD3<Float>) -> Int {
    let key = edgeKey(a, b)
    if let existing = vertexForEdge[key] { return existing }
    let va = values[a], vb = values[b]
    let t = va / (va - vb)
    positions.append(pa + (pb - pa) * t)
    let index = positions.count - 1
    vertexForEdge[key] = index
    return index
}

// Six tetrahedra per cube around the 0–7 diagonal (corner bits: x=1, y=2, z=4).
let tets: [[Int]] = [[0, 1, 3, 7], [0, 3, 2, 7], [0, 2, 6, 7], [0, 6, 4, 7], [0, 4, 5, 7], [0, 5, 1, 7]]

for i in 0..<(nx - 1) {
    for j in 0..<(ny - 1) {
        for k in 0..<(nz - 1) {
            var corner = [Int](repeating: 0, count: 8)
            var pt = [SIMD3<Float>](repeating: .zero, count: 8)
            for c in 0..<8 {
                let ci = i + (c & 1), cj = j + ((c >> 1) & 1), ck = k + ((c >> 2) & 1)
                corner[c] = gridIndex(ci, cj, ck)
                pt[c] = gridPoint(ci, cj, ck)
            }
            // Skip cubes fully inside or outside.
            var inside = 0
            for c in 0..<8 where values[corner[c]] < 0 { inside += 1 }
            if inside == 0 || inside == 8 { continue }

            for tet in tets {
                let v = tet.map { corner[$0] }
                let p = tet.map { pt[$0] }
                var mask = 0
                for t in 0..<4 where values[v[t]] < 0 { mask |= 1 << t }
                if mask == 0 || mask == 15 { continue }
                func e(_ a: Int, _ b: Int) -> Int { vertex(v[a], v[b], p[a], p[b]) }
                // Orientation is fixed afterwards from the field normal.
                switch mask {
                case 1, 14: triangles.append((e(0, 1), e(0, 2), e(0, 3)))
                case 2, 13: triangles.append((e(1, 0), e(1, 3), e(1, 2)))
                case 4, 11: triangles.append((e(2, 0), e(2, 1), e(2, 3)))
                case 8, 7:  triangles.append((e(3, 0), e(3, 2), e(3, 1)))
                case 3, 12:
                    let a = e(0, 2), b = e(0, 3), c = e(1, 3), d = e(1, 2)
                    triangles.append((a, b, c)); triangles.append((a, c, d))
                case 5, 10:
                    let a = e(0, 1), b = e(0, 3), c = e(2, 3), d = e(2, 1)
                    triangles.append((a, b, c)); triangles.append((a, c, d))
                case 6, 9:
                    let a = e(0, 1), b = e(0, 2), c = e(3, 2), d = e(3, 1)
                    triangles.append((a, b, c)); triangles.append((a, c, d))
                default: break
                }
            }
        }
    }
}

// Normals from the field; wind every triangle to face outward along them.
let normals = positions.map(normal)
var out = "# Fuser mark — twelve-node smooth-union surface, baked by captutor/bin/bake-fuser-mesh.swift\n"
out.reserveCapacity(positions.count * 60 + triangles.count * 30)
for p in positions { out += "v \(p.x) \(p.y) \(p.z)\n" }
for n in normals { out += "vn \(n.x) \(n.y) \(n.z)\n" }
out += "g fuser\n"
for (a, b, c) in triangles {
    let faceNormal = simd_cross(positions[b] - positions[a], positions[c] - positions[a])
    let outward = simd_dot(faceNormal, normals[a] + normals[b] + normals[c]) >= 0
    let (x, y, z) = outward ? (a, b, c) : (a, c, b)
    out += "f \(x + 1)//\(x + 1) \(y + 1)//\(y + 1) \(z + 1)//\(z + 1)\n"
}
try out.write(toFile: outputPath, atomically: true, encoding: .utf8)
print("✓ \(outputPath) · \(positions.count) vertices · \(triangles.count) triangles · step \(step)")
