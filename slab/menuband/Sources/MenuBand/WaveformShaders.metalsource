#include <metal_stdlib>
using namespace metal;

struct Uniforms {
    float viewW;
    float viewH;
    float barW;
    float stride;
    float minHeight;
    float4 color;
    float dotMatrix;
    float isLight;
};

struct VertexOut {
    float4 position [[position]];
    float level;
    float mask;
};

// Two triangles spanning the unit square (CCW), shared across all bar
// instances. The vertex shader scales each instance into a bar rect
// and converts pixel space to clip space.
constant float2 unitQuad[6] = {
    float2(0, 0), float2(1, 0), float2(0, 1),
    float2(0, 1), float2(1, 0), float2(1, 1)
};

// Segmented LED-meter look. Each bar is rendered as a full-height
// rect; the fragment shader carves it into stacked segments by
// level. Tweak NSEG / SEG_GAP to taste — old stereo VU meters
// typically had 10–14 segments with a small dim row above the lit
// ones to hint at the headroom.
constant float NSEG = 10.0;
constant float SEG_GAP = 0.32;
constant float UNLIT_ALPHA = 0.12;
// Hot-zone: above this fraction of the bar height, the lit color
// brightens toward white so the top of the meter still reads as
// "peaking" even when the instrument's base color is e.g. a deep
// navy bass. Linear ramp from HOT_AT → top.
constant float HOT_AT = 0.70;

vertex VertexOut bar_vertex(uint vid [[vertex_id]],
                            uint iid [[instance_id]],
                            constant Uniforms &u [[buffer(0)]],
                            constant float *levels [[buffer(1)]],
                            constant float *masks [[buffer(2)]])
{
    float2 local = unitQuad[vid];
    float barX = float(iid) * u.stride;
    // Full-height geometry — fragment shader masks per-segment.
    float h = u.viewH;
    float px = barX + local.x * u.barW;
    float py = local.y * h;
    // Pixel space → clip space ([-1, 1] on both axes).
    float clipX = (px / u.viewW) * 2.0 - 1.0;
    float clipY = (py / u.viewH) * 2.0 - 1.0;
    VertexOut out;
    out.position = float4(clipX, clipY, 0, 1);
    out.level = levels[iid];
    out.mask = masks[iid];
    return out;
}

fragment float4 bar_fragment(VertexOut in [[stage_in]],
                             constant Uniforms &u [[buffer(0)]])
{
    // [[position]] gives fragment pixel-space y with origin at TOP.
    // Flip so y01=0 is bottom of the bar (where amplitude starts).
    float y01 = 1.0 - (in.position.y / u.viewH);
    // Inter-segment gap: top fraction of each segment cell stays
    // transparent so the bar reads as a stack rather than a solid.
    float segPos = fract(y01 * NSEG);
    if (segPos > (1.0 - SEG_GAP)) {
        discard_fragment();
    }
    // Bar color = the instrument's chosen base hue, passed in via
    // u.color. In dark mode the top brightens toward white (LED
    // glow); in light mode it darkens toward black (ink saturation
    // at peak) — both read as "this bar is hotter at the top"
    // against their respective substrates.
    float3 base = u.color.rgb;
    float hot = max(0.0, (y01 - HOT_AT) / (1.0 - HOT_AT));
    float3 hotTarget = (u.isLight > 0.5) ? float3(0.0, 0.0, 0.0) : float3(1.0, 1.0, 1.0);
    float3 tier = mix(base, hotTarget, hot * 0.65);
    // Per-segment glow: brighter at the center of each LED cell,
    // falling off toward the gap edges. Reads as a soft bloom on
    // each lit segment without a real blur pass.
    float visibleSegPos = segPos / (1.0 - SEG_GAP);
    float segCenterDist = abs(visibleSegPos - 0.5) * 2.0;
    float bloom = pow(1.0 - segCenterDist, 1.6);
    // Lit = below the level (live VU) OR the corresponding bit
    // is set in this bar's dot-matrix mask. The mask path lets
    // us spell static text out of the LED segments — used in
    // MIDI mode to render "MIDI".
    uint segIndex = uint(floor(y01 * NSEG));
    uint mask = uint(in.mask);
    bool maskLit = (u.dotMatrix > 0.5) && (((mask >> segIndex) & 1u) != 0u);
    bool levelLit = y01 < in.level;
    bool lit = maskLit || levelLit;
    // Bloom direction also flips with the substrate so the
    // per-segment glow reinforces "hotter" instead of fighting it.
    float bloomSign = (u.isLight > 0.5) ? -1.0 : 1.0;
    float3 color = lit ? (tier + bloomSign * bloom * 0.40) : tier;
    // In light mode unlit segments fade toward the substrate (warm
    // off-white) instead of toward black — without this the
    // "off" rows show as faint colored dots, which reads as a row
    // of always-on LEDs rather than empty headroom.
    float unlitAlpha = (u.isLight > 0.5) ? 0.20 : UNLIT_ALPHA;
    float a = lit ? u.color.a : (u.color.a * unlitAlpha);
    return float4(clamp(color, float3(0.0), float3(1.0)), a);
}
