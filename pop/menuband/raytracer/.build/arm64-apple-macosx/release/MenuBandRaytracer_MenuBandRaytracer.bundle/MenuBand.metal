#include <metal_stdlib>
using namespace metal;

struct Uniforms {
    uint width;
    uint height;
    uint samples;
    uint frame;
    float time;
    float duration;
    float loopSeconds;
    float exposure;
};

struct DistanceHit { float distance; int material; };
struct RayHit { float distance; int material; float3 position; float3 normal; bool hit; };
struct Material { float3 base; float3 emission; float roughness; float metallic; };

constant float PI = 3.14159265358979323846;
constant float PAPER_STRAIGHT = 6.2;
constant float PAPER_RADIUS = 1.28;
constant float PAPER_HALF_WIDTH = 5.55;
constant float PAPER_THICKNESS = 0.035;
constant float PAPER_LOOP_LENGTH = PAPER_STRAIGHT * 2.0 + 2.0 * PI * PAPER_RADIUS;

uint hash32(uint value) {
    value ^= value >> 16;
    value *= 0x7feb352d;
    value ^= value >> 15;
    value *= 0x846ca68b;
    return value ^ (value >> 16);
}

float random(thread uint &state) {
    state = hash32(state + 0x9e3779b9);
    return float(state) / 4294967296.0;
}

float smooth01(float value) {
    float x = clamp(value, 0.0, 1.0);
    return x * x * (3.0 - 2.0 * x);
}

float sdRoundBox(float3 p, float3 bounds, float radius) {
    float3 q = abs(p) - bounds + radius;
    return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0) - radius;
}

float sdFiniteCylinderX(float3 p, float halfLength, float radius) {
    float2 d = abs(float2(length(p.yz), p.x)) - float2(radius, halfLength);
    return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
}

DistanceHit sceneDistance(float3 p) {
    float2 pa = float2(p.z, p.y) - float2(0.0, -PAPER_STRAIGHT * 0.5);
    float2 ba = float2(0.0, PAPER_STRAIGHT);
    float segment = length(pa - ba * clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0)) - PAPER_RADIUS;
    float paper = max(abs(segment) - PAPER_THICKNESS, abs(p.x) - PAPER_HALF_WIDTH);
    DistanceHit result = { paper, 1 };

    float keyboard = sdRoundBox(p - float3(0.0, -3.48, 1.62), float3(5.72, 0.28, 0.23), 0.11);
    if (keyboard < result.distance) result = { keyboard, 2 };
    float reader = sdRoundBox(p - float3(0.0, -3.17, 1.52), float3(5.60, 0.045, 0.08), 0.025);
    if (reader < result.distance) result = { reader, 3 };
    float topRoller = sdFiniteCylinderX(p - float3(0.0, PAPER_STRAIGHT * 0.5, 0.0), 5.72, 1.05);
    if (topRoller < result.distance) result = { topRoller, 4 };
    float bottomRoller = sdFiniteCylinderX(p - float3(0.0, -PAPER_STRAIGHT * 0.5, 0.0), 5.72, 1.05);
    if (bottomRoller < result.distance) result = { bottomRoller, 4 };
    float floorDistance = p.y + 4.18;
    if (floorDistance < result.distance) result = { floorDistance, 5 };
    return result;
}

float3 sceneNormal(float3 p) {
    const float epsilon = 0.0015;
    float2 h = float2(epsilon, 0.0);
    return normalize(float3(
        sceneDistance(p + h.xyy).distance - sceneDistance(p - h.xyy).distance,
        sceneDistance(p + h.yxy).distance - sceneDistance(p - h.yxy).distance,
        sceneDistance(p + h.yyx).distance - sceneDistance(p - h.yyx).distance
    ));
}

RayHit march(float3 origin, float3 direction, float maximumDistance = 48.0) {
    float distance = 0.02;
    int material = 0;
    for (int step = 0; step < 180; step++) {
        float3 position = origin + direction * distance;
        DistanceHit sample = sceneDistance(position);
        material = sample.material;
        if (sample.distance < 0.0016) {
            float3 normal = sceneNormal(position);
            if (dot(normal, direction) > 0.0) normal = -normal;
            return { distance, material, position, normal, true };
        }
        distance += max(0.001, sample.distance * 0.72);
        if (distance > maximumDistance) break;
    }
    return { distance, material, origin + direction * distance, float3(0.0), false };
}

float softShadow(float3 origin, float3 direction, float maximumDistance) {
    float result = 1.0;
    float distance = 0.025;
    for (int step = 0; step < 72 && distance < maximumDistance; step++) {
        float sample = sceneDistance(origin + direction * distance).distance;
        if (sample < 0.001) return 0.0;
        result = min(result, 18.0 * sample / distance);
        distance += clamp(sample, 0.012, 0.32);
    }
    return clamp(result, 0.0, 1.0);
}

float ambientOcclusion(float3 position, float3 normal) {
    float occlusion = 0.0;
    float weight = 1.0;
    for (int index = 1; index <= 5; index++) {
        float distance = 0.045 * float(index);
        occlusion += (distance - sceneDistance(position + normal * distance).distance) * weight;
        weight *= 0.65;
    }
    return clamp(1.0 - occlusion * 2.1, 0.18, 1.0);
}

float paperPath(float3 p) {
    float halfStraight = PAPER_STRAIGHT * 0.5;
    if (p.y >= -halfStraight && p.y <= halfStraight) {
        if (p.z >= 0.0) return p.y + halfStraight;
        return PAPER_STRAIGHT + PI * PAPER_RADIUS + (halfStraight - p.y);
    }
    if (p.y > halfStraight) {
        float angle = atan2(p.y - halfStraight, p.z);
        if (angle < 0.0) angle += 2.0 * PI;
        return PAPER_STRAIGHT + clamp(angle, 0.0, PI) * PAPER_RADIUS;
    }
    float angle = atan2(-(p.y + halfStraight), -p.z);
    if (angle < 0.0) angle += 2.0 * PI;
    return PAPER_STRAIGHT * 2.0 + PI * PAPER_RADIUS + clamp(angle, 0.0, PI) * PAPER_RADIUS;
}

Material materialAt(
    RayHit hit,
    constant Uniforms &uniforms,
    texture2d<float, access::sample> scoreTexture,
    texture2d<float, access::sample> keyboardTexture
) {
    constexpr sampler textureSampler(address::clamp_to_edge, filter::linear, mip_filter::linear);
    if (hit.material == 1) {
        float u = clamp(hit.position.x / (PAPER_HALF_WIDTH * 2.0) + 0.5, 0.0, 1.0);
        float scoreTime = uniforms.time + paperPath(hit.position) / PAPER_LOOP_LENGTH * uniforms.loopSeconds;
        float v = 1.0 - scoreTime / uniforms.duration;
        float4 ink = scoreTime >= 0.0 && scoreTime <= uniforms.duration
            ? scoreTexture.sample(textureSampler, float2(u, clamp(v, 0.0, 1.0)))
            : float4(0.0);
        float rear = smoothstep(0.25, -PAPER_RADIUS, hit.position.z);
        float3 paper = mix(float3(0.97, 0.94, 0.86), float3(0.16, 0.13, 0.20), rear * 0.82);
        float3 printed = mix(paper, ink.rgb, ink.a * (1.0 - rear * 0.48));
        return { printed, float3(0.0), 0.82, 0.0 };
    }
    if (hit.material == 2) {
        float2 uv = float2(
            clamp(hit.position.x / 11.44 + 0.5, 0.0, 1.0),
            clamp(1.0 - (hit.position.y + 3.76) / 0.56, 0.0, 1.0)
        );
        float4 keyboard = keyboardTexture.sample(textureSampler, uv);
        float currentV = clamp(1.0 - uniforms.time / uniforms.duration, 0.0, 1.0);
        float4 active = scoreTexture.sample(textureSampler, float2(uv.x, currentV));
        float3 base = mix(keyboard.rgb, active.rgb, active.a * 0.72);
        return { base, active.rgb * active.a * 0.28, 0.42, 0.02 };
    }
    if (hit.material == 3) return { float3(0.075, 0.055, 0.10), float3(0.0), 0.34, 0.18 };
    if (hit.material == 4) return { float3(0.31, 0.34, 0.39), float3(0.0), 0.24, 0.72 };
    return { float3(0.63, 0.48, 0.55), float3(0.0), 0.94, 0.0 };
}

float3 environment(float3 direction) {
    float vertical = clamp(direction.y * 0.5 + 0.5, 0.0, 1.0);
    return mix(float3(0.60, 0.40, 0.52), float3(1.00, 0.93, 0.74), vertical);
}

float3 trace(
    float3 origin,
    float3 direction,
    thread uint &randomState,
    constant Uniforms &uniforms,
    texture2d<float, access::sample> scoreTexture,
    texture2d<float, access::sample> keyboardTexture
) {
    RayHit hit = march(origin, direction);
    if (!hit.hit) return environment(direction);
    Material material = materialAt(hit, uniforms, scoreTexture, keyboardTexture);
    float3 normal = hit.normal;
    float ao = ambientOcclusion(hit.position, normal);
    float2 lightJitter = float2(random(randomState), random(randomState)) - 0.5;
    float3 lightPosition = float3(-3.8 + lightJitter.x * 5.8, 7.8, 7.2 + lightJitter.y * 3.2);
    float3 toLight = lightPosition - hit.position;
    float lightDistance = length(toLight);
    float3 lightDirection = toLight / lightDistance;
    float diffuse = max(0.0, dot(normal, lightDirection));
    float shadow = diffuse > 0.0 ? softShadow(hit.position + normal * 0.012, lightDirection, lightDistance) : 0.0;
    float3 halfVector = normalize(lightDirection - direction);
    float specularPower = mix(18.0, 120.0, 1.0 - material.roughness);
    float specular = pow(max(0.0, dot(normal, halfVector)), specularPower) * (0.08 + material.metallic * 0.75);
    float3 ambient = environment(normal) * (0.17 + 0.18 * ao);
    float3 direct = float3(1.00, 0.94, 0.84) * diffuse * shadow * 1.28;
    float3 color = material.base * (ambient + direct) * ao;
    color += specular * shadow * float3(1.0, 0.92, 0.80);
    color += material.emission;
    return color;
}

kernel void renderMenuBand(
    texture2d<float, access::sample> scoreTexture [[texture(0)]],
    texture2d<float, access::sample> keyboardTexture [[texture(1)]],
    texture2d<float, access::write> output [[texture(2)]],
    constant Uniforms &uniforms [[buffer(0)]],
    uint2 gid [[thread_position_in_grid]]
) {
    if (gid.x >= uniforms.width || gid.y >= uniforms.height) return;
    uint randomState = hash32(gid.x + gid.y * uniforms.width + uniforms.frame * 0x45d9f3b);
    float tourPhase = fmod(uniforms.time, 10.0) / 10.0;
    float orbitMix = tourPhase < 0.50 ? 0.0 : smooth01((tourPhase - 0.50) / 0.50);
    float directionSign = (uint(uniforms.time / 10.0) & 1u) == 0u ? 1.0 : -1.0;
    float orbit = directionSign * orbitMix * 2.0 * PI;
    float yaw = orbit;
    float pitch = sin(abs(orbit) * 0.5) * 0.29;
    float roll = sin(orbit) * 0.085;
    float distance = 15.0 + sin(abs(orbit) * 0.5) * 2.7;
    float3 target = float3(0.0, -0.45, 0.0);
    float horizontal = cos(pitch) * distance;
    float3 camera = target + float3(sin(yaw) * horizontal, sin(pitch) * distance, cos(yaw) * horizontal);
    float3 forward = normalize(target - camera);
    float3 right = normalize(cross(forward, float3(0.0, 1.0, 0.0)));
    float3 up = cross(right, forward);
    float cosine = cos(roll), sine = sin(roll);
    float3 rolledRight = right * cosine + up * sine;
    float3 rolledUp = up * cosine - right * sine;
    right = rolledRight; up = rolledUp;

    float3 accumulated = float3(0.0);
    for (uint sample = 0; sample < uniforms.samples; sample++) {
        float2 jitter = float2(random(randomState), random(randomState));
        float2 pixel = (float2(gid) + jitter) / float2(uniforms.width, uniforms.height);
        float2 screen = pixel * 2.0 - 1.0;
        screen.x *= float(uniforms.width) / float(uniforms.height);
        screen.y = -screen.y;
        // Portrait needs a wide vertical field so the 11-unit tracker bar fits
        // horizontally without pulling the camera unnaturally far away.
        float lens = 1.0 / tan(0.5 * 1.22173);
        float3 rayDirection = normalize(forward * lens + right * screen.x + up * screen.y);
        accumulated += trace(camera, rayDirection, randomState, uniforms, scoreTexture, keyboardTexture);
    }
    float3 color = accumulated / float(uniforms.samples);
    color = 1.0 - exp(-color * uniforms.exposure);
    color = pow(clamp(color, 0.0, 1.0), float3(1.0 / 2.2));
    output.write(float4(color, 1.0), gid);
}
