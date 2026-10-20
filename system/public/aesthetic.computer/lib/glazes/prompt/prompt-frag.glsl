#version 300 es
precision highp float;

// Only the default, bounded lighting settings use half-precision color math.
// Ray positions stay highp; customized settings retain the general highp path.
#ifndef LIGHT_PRECISION
#define LIGHT_PRECISION highp
#endif

in vec2 v_texc;
out vec4 endColor;

uniform sampler2D iTexture;
uniform sampler2D iTexturePost;

uniform float iTime;
uniform vec2 iMouse;
uniform vec2 iResolution;

uniform int fogIterations;
uniform int shadowIterations;

uniform int freezeGrain;

const bool colorMode = true;

uniform float focalLength;
uniform float screenScale;
uniform float shadowRange;
uniform float cameraDistance;
uniform float volumeRadius;
uniform float inputRadius;
uniform float innerDensity;
uniform float outerDensity;
uniform float anisotropy;
uniform float lightPower;
uniform float radialBlurAmount;
uniform vec3 lightDirection;
uniform vec3 bgColor;
uniform vec3 lightColor;

vec3 normLightDirection;

// Constants
const float PI = 3.14159265359;
const float PI_4 = PI/4.;
const float EPSILON = 1e-10;
const vec3 PLANE_NORMAL = vec3(0., 0., -1.);
const float SHADOW_SOFTNESS = 0.18; // 🌫️ shadow penumbra jitter (fuzzy shadows)
const float SWAY_PERIOD = 4.0;      // 🚶 seconds per back-and-forth (walking cadence)
const float SWAY_AMOUNT = 0.13;     // 🚶 horizontal wobble magnitude (added to lightDirection.x)
// 📼 VHS finish, applied over the volume.
const float VHS_SOURCE = 0.55;      // how much of the canvas's own colour comes back (screen blend)
const float VHS_BLEED = 0.7;        // red/blue sideways offset, in canvas pixels
const float VHS_SMEAR = 0.8;        // horizontal blur reach, in canvas pixels
const float VHS_WOBBLE = 0.35;      // per-line tracking jitter, in canvas pixels
const float VHS_SATURATION = 1.3;
const float VHS_CONTRAST = 0.85;    // 0 = none, 1 = full smoothstep S-curve
const float VHS_GAMMA = 1.7;        // >1 deepens the mid-tones
const float VHS_SCANLINE = 0.07;    // darkening on every other band
const float VHS_GRAIN = 0.05;

// Utils
float random(vec2 p)
{
    vec3 p3 = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

float sRGB(float x)
{
    if (x <= 0.00031308)
        return 12.92 * x;
    else
        return 1.055*pow(x, (1.0 / 2.4)) - 0.055;
}

float maxv(vec3 v)
{
    return max(max(v.x, v.y), v.z);
}

bool inBounds(vec2 coords)
{
    return coords.x >= 0. && coords.x <= 1. && coords.y >= 0. && coords.y <= 1.;
}

mat4 rotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;
    return mat4(oc * axis.x * axis.x + c,
            oc * axis.x * axis.y - axis.z * s,
            oc * axis.z * axis.x + axis.y * s,
            0.0,
            oc * axis.x * axis.y + axis.z * s,
            oc * axis.y * axis.y + c,
            oc * axis.y * axis.z - axis.x * s,
            0.0,
            oc * axis.z * axis.x - axis.y * s,
            oc * axis.y * axis.z + axis.x * s,
            oc * axis.z * axis.z + c,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0);
}

// Guts
float hgPhase(vec3 dirIn, vec3 dirOut)
{
    return PI_4 * (1. - anisotropy * anisotropy) / pow(1. + anisotropy * (anisotropy - 2. * dot(dirIn, dirOut)), 1.5);
}

vec2 worldToDensityMap(vec2 coords)
{
    return .5 * (screenScale/-cameraDistance) * coords / vec2(iResolution.x / iResolution.y, 1.0) - vec2(.5);
}

vec4 getColor(vec3 pos)
{
    // if (pos.z <= 0.0 && pos.z < -volumeRadius * 2.0)
    if (abs(pos.z) > volumeRadius * 1.0)
    {
      return vec4(0.);
    }
    vec4 outColor = vec4(bgColor, outerDensity);
    vec2 imgCoords = worldToDensityMap(pos.xy) + vec2(1.);
    if (abs(pos.z) < inputRadius && inBounds(imgCoords))
    {
        outColor.xyz = texture(iTexture, imgCoords).xyz;
        outColor.w = maxv(outColor.xyz) * innerDensity;
    }
    return outColor;
}

float zPlaneIntersect(vec3 ro, vec3 rd, float z)
{
    float denom = dot(rd, PLANE_NORMAL);
    if (abs(denom) < EPSILON)
    {
        denom = EPSILON;
    }
    return -(dot(ro, PLANE_NORMAL) + z) / dot(rd, PLANE_NORMAL);
}

vec3 directLight(vec3 pos, vec3 ldir, float headStart)
{
    vec3 ro = pos;
    // Multiplying per-step exp(-depth) equals exp(-sum(depth)).
    LIGHT_PRECISION vec3 opticalDepth = vec3(0.);

    float nearIntersectionDist = zPlaneIntersect(ro, ldir, 0.0);
    float farIntersectionDist = zPlaneIntersect(ro, ldir, volumeRadius * 2.0);

    float traceDist = max(nearIntersectionDist, farIntersectionDist);
    traceDist = min(traceDist, shadowRange);

    // A straight ray has equal steps; only the first includes the jitter.
    float sampleDistance = length(ldir * traceDist) / float(shadowIterations);
    for (int i = 1; i < shadowIterations + 1; i += 1)
    {
        pos = ro - ldir * (float(i) + headStart) / float(shadowIterations) * traceDist;
        // Once this monotone ray leaves the slab, all later samples are empty.
        if (abs(pos.z) > volumeRadius && pos.z * (-ldir.z * traceDist) >= 0.0) break;
        LIGHT_PRECISION vec4 colorValue = getColor(pos);
        opticalDepth += colorValue.w * sampleDistance * (i == 1 ? 1.0 + headStart : 1.0) * colorValue.xyz;
    }
    return exp(-opticalDepth);
}

vec3 post(vec3 col)
{
    col.x = sRGB(col.x);
    col.y = sRGB(col.y);
    col.z = sRGB(col.z);
    // Tame blown-out highlights: roll the brightest values off toward 1.0
    // instead of clipping to a harsh white. Neutral — no color shift.
    float hi = smoothstep(0.7, 1.0, maxv(col));
    col /= (1.0 + hi * 0.3);   // soft highlight roll-off (less harsh bright)
    return clamp(col, 0.0, 1.0);
}

void main()
{
    // 🚶 Handheld "walking" wobble — the light angle sways back and forth as if
    // the person holding the device is in motion. A primary cadence plus a
    // smaller faster bob keeps it organic rather than mechanical. Neutral white,
    // intensity unchanged — only the angle moves, so shadows are always present
    // and gently shifting (a perfectly vertical light makes some shadows vanish).
    float t = iTime * 0.001; // iTime is ms
    float wobble = sin(t * (2.0 * PI / SWAY_PERIOD))
                 + 0.3 * sin(t * (2.0 * PI / SWAY_PERIOD) * 2.3 + 1.7);
    vec3 swayLight = vec3(
        lightDirection.x + wobble * SWAY_AMOUNT,
        lightDirection.y,
        lightDirection.z);
    normLightDirection = normalize(swayLight);

    vec2 uv = v_texc * 2. - vec2(1.);
    uv.x *= iResolution.x / iResolution.y;

    vec3 ro = vec3(0., 0., cameraDistance);
    vec3 rd = normalize(vec3(uv, 1.0));

    float nearIntersectionDist = zPlaneIntersect(ro, rd, 0.0);
    float farIntersectionDist = zPlaneIntersect(ro, rd, volumeRadius * 2.0);
    float traceDist = abs(nearIntersectionDist - farIntersectionDist) + volumeRadius;

    vec3 volCol = vec3(0.);
    vec3 volAbs = vec3(1.);
    vec3 pos = ro + rd * nearIntersectionDist;
    vec3 stepAbs, stepCol;

    float offset = iTime;
    if (freezeGrain != 0)
    {
        offset = 0.0;
    }
    // Per-pixel raymarch jitter. Use per-axis pixel coordinates (v_texc *
    // iResolution) so the grain stays isotropic on wide screens, and wrap with
    // mod() so the hash input never grows large enough to lose float precision
    // at high resolutions — both were sources of edge distortion when the screen
    // got really big (the old `iResolution.x * v_texc` scaled y by the width).
    vec2 grainCoord = mod(v_texc * iResolution, 289.0);
    float headStartCam = random(grainCoord - mod(offset, 289.0));
    float headStartShadow = random(grainCoord + mod(offset, 289.0));

    vec3 refractionDir = mix(vec3(0, 0, 1), rd, radialBlurAmount);

    // 🌫️ Fuzzy shadows: jitter the shadow ray direction a touch per pixel so the
    // penumbra softens instead of casting a hard edge.
    vec3 ldir = normalize(normLightDirection + vec3(
        (headStartShadow - 0.5) * SHADOW_SOFTNESS,
        (random(refractionDir.xy + headStartShadow) - 0.5) * SHADOW_SOFTNESS,
        0.0));

    float phase = hgPhase(-ldir, refractionDir);
    float fogStepDistance = length(traceDist * refractionDir) / float(fogIterations);

    for (int i = 0; i < fogIterations; i++)
    {
        pos = ro + rd * nearIntersectionDist;
        pos += traceDist * refractionDir * (float(i)+headStartCam) / float(fogIterations);
        if (abs(pos.z) > volumeRadius && pos.z * refractionDir.z >= 0.0) break;
        LIGHT_PRECISION vec4 colorValue = getColor(pos);
        // Empty volume contributes neither absorption nor light. Skip the
        // shadow march for these samples without changing the accumulated color.
        if (colorValue.w == 0.0) continue;
        stepAbs = exp(-colorValue.w * fogStepDistance * (i == 0 ? headStartCam : 1.0) * colorValue.xyz);
        stepCol = vec3(1.) - stepAbs;
        volCol += stepCol * volAbs * (lightPower * lightColor * directLight(pos, ldir, headStartShadow) * phase);
        volAbs *= stepAbs;
    }

    vec3 color = post(volCol);

    // 📼 VHS: bring the canvas's own colour back over the fog, smeared
    // sideways with red and blue bleeding apart, then saturate, add contrast,
    // scanlines and a rolling grain.
    float tt = iTime * 0.001;
    vec2 texel = 1.0 / vec2(textureSize(iTexture, 0));
    float band = floor(v_texc.y / texel.y);
    float tracking = (random(vec2(floor(tt * 30.0), band)) - 0.5) * VHS_WOBBLE * texel.x;
    vec3 p0 = ro + rd * nearIntersectionDist;
    vec2 suv = worldToDensityMap(p0.xy) + vec2(1.0) + vec2(tracking, 0.0);
    vec2 reach = vec2(VHS_SMEAR * texel.x, 0.0);
    vec3 smear = (2.0 * texture(iTexture, suv).rgb
                + texture(iTexture, suv - reach).rgb
                + texture(iTexture, suv + reach).rgb) * 0.25;
    vec2 bleed = vec2(VHS_BLEED * texel.x, 0.0);
    vec3 src = vec3(texture(iTexture, suv - bleed).r, smear.g, texture(iTexture, suv + bleed).b);
    src = mix(smear, src, 0.65);
    color = 1.0 - (1.0 - color) * (1.0 - src * VHS_SOURCE);

    float luma = dot(color, vec3(0.299, 0.587, 0.114));
    color = clamp(mix(vec3(luma), color, VHS_SATURATION), 0.0, 1.0);
    color = pow(color, vec3(VHS_GAMMA));
    color = mix(color, color * color * (3.0 - 2.0 * color), VHS_CONTRAST);
    color *= 1.0 - VHS_SCANLINE * step(0.5, fract(gl_FragCoord.y / 3.0));
    color += (random(gl_FragCoord.xy + fract(tt * 7.0) * 289.0) - 0.5) * VHS_GRAIN;

    endColor = vec4(clamp(color, 0.0, 1.0), 1.);
}
