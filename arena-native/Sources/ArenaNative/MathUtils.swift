// MathUtils.swift — right-handed camera matrices for Metal (NDC z in 0..1).

import simd

@inline(__always) func radians(_ deg: Float) -> Float { deg * .pi / 180 }

// Right-handed perspective looking down -Z, mapping depth to [0, 1] (Metal).
func perspectiveRH(fovyRadians: Float, aspect: Float, near: Float, far: Float) -> float4x4 {
    let ys = 1 / tan(fovyRadians * 0.5)
    let xs = ys / aspect
    let zs = far / (near - far)
    return float4x4(columns: (
        SIMD4<Float>(xs, 0,  0,        0),
        SIMD4<Float>(0,  ys, 0,        0),
        SIMD4<Float>(0,  0,  zs,      -1),
        SIMD4<Float>(0,  0,  zs * near, 0)
    ))
}

// Right-handed look-at view matrix.
func lookAtRH(eye: SIMD3<Float>, center: SIMD3<Float>, up: SIMD3<Float>) -> float4x4 {
    let z = simd_normalize(eye - center)      // backward (toward viewer)
    let x = simd_normalize(simd_cross(up, z)) // right
    let y = simd_cross(z, x)                  // true up
    return float4x4(columns: (
        SIMD4<Float>(x.x, y.x, z.x, 0),
        SIMD4<Float>(x.y, y.y, z.y, 0),
        SIMD4<Float>(x.z, y.z, z.z, 0),
        SIMD4<Float>(-simd_dot(x, eye), -simd_dot(y, eye), -simd_dot(z, eye), 1)
    ))
}

// Camera basis from yaw/pitch (degrees), matching pmove's convention:
//   forward = (sin(yaw)cos(pitch), sin(pitch), cos(yaw)cos(pitch))
func forwardVector(yawDeg: Float, pitchDeg: Float) -> SIMD3<Float> {
    let y = radians(yawDeg), p = radians(pitchDeg)
    return SIMD3<Float>(sin(y) * cos(p), sin(p), cos(y) * cos(p))
}
