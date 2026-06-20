// Shaders.swift — Metal shader source compiled at runtime (no .metallib step)
// plus the matching Swift-side uniforms struct.

import simd

// Must match `Uniforms` in the Metal source below (std layout): a 64-byte
// matrix followed by a 16-byte vector.
struct Uniforms {
    var viewProj: simd_float4x4
    var lightDir: SIMD4<Float> // xyz = normalized light direction, w unused
}

let metalShaderSource = """
#include <metal_stdlib>
using namespace metal;

struct Uniforms {
    float4x4 viewProj;
    float4   lightDir;
};

// --- Lit triangles ---
struct TriIn {
    float3 position [[attribute(0)]];
    float3 normal   [[attribute(1)]];
    float4 color    [[attribute(2)]];
};
struct TriOut {
    float4 position [[position]];
    float3 normal;
    float4 color;
};

vertex TriOut vertex_tri(TriIn in [[stage_in]],
                         constant Uniforms &u [[buffer(1)]]) {
    TriOut out;
    out.position = u.viewProj * float4(in.position, 1.0);
    out.normal = in.normal;
    out.color = in.color;
    return out;
}

fragment float4 fragment_tri(TriOut in [[stage_in]],
                             constant Uniforms &u [[buffer(1)]]) {
    float3 n = normalize(in.normal);
    float diff = max(dot(n, normalize(u.lightDir.xyz)), 0.0);
    float ambient = 0.35;
    float shade = ambient + (1.0 - ambient) * diff;
    return float4(in.color.rgb * shade, in.color.a);
}

// --- Flat lines ---
struct LineIn {
    float3 position [[attribute(0)]];
    float4 color    [[attribute(1)]];
};
struct LineOut {
    float4 position [[position]];
    float4 color;
};

vertex LineOut vertex_line(LineIn in [[stage_in]],
                           constant Uniforms &u [[buffer(1)]]) {
    LineOut out;
    out.position = u.viewProj * float4(in.position, 1.0);
    out.color = in.color;
    return out;
}

fragment float4 fragment_line(LineOut in [[stage_in]]) {
    return in.color;
}
"""
