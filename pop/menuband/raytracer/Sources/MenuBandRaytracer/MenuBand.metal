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
struct Material { float3 base; float3 emission; float roughness; float metallic; float opacity; };

constant float PI = 3.14159265358979323846;
#if MENU_BAND_STRIP
constant float PAPER_STRAIGHT = 6.2;
constant float PAPER_RADIUS = 1.28;
#else
constant float PAPER_STRAIGHT = 0.20;
constant float PAPER_RADIUS = 3.05;
#endif
constant float PAPER_HALF_WIDTH = 4.65;
constant float PAPER_THICKNESS = 0.055;
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

float3 cosineHemisphere(float3 normal, thread uint &state) {
    float u1 = random(state);
    float u2 = random(state);
    float radius = sqrt(u1);
    float angle = 2.0 * PI * u2;
    float3 tangent = normalize(abs(normal.y) < 0.98
        ? cross(normal, float3(0.0, 1.0, 0.0))
        : cross(normal, float3(1.0, 0.0, 0.0)));
    float3 bitangent = cross(normal, tangent);
    return normalize(
        tangent * (radius * cos(angle)) +
        bitangent * (radius * sin(angle)) +
        normal * sqrt(max(0.0, 1.0 - u1))
    );
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

    constexpr int keyCount = 14;
    float keyWidth = PAPER_HALF_WIDTH * 2.0 / float(keyCount);
    for (int key = 0; key < keyCount; key++) {
        float center = -PAPER_HALF_WIDTH + (float(key) + 0.5) * keyWidth;
        float keyboard = sdRoundBox(
            p - float3(center, 0.0, PAPER_RADIUS + 0.10),
            float3(keyWidth * 0.455, 0.29, 0.28),
            0.09
        );
        if (keyboard < result.distance) result = { keyboard, 2 };
    }
    float reader = sdFiniteCylinderX(p - float3(0.0, 0.0, PAPER_RADIUS - 0.10), PAPER_HALF_WIDTH + 0.10, 0.16);
    if (reader < result.distance) result = { reader, 3 };
    float floorDistance = p.y + 4.18;
    if (floorDistance < result.distance) result = { floorDistance, 5 };
    return result;
}

DistanceHit opaqueSceneDistance(float3 p) {
    constexpr int keyCount = 14;
    float keyWidth = PAPER_HALF_WIDTH * 2.0 / float(keyCount);
    DistanceHit result = { 1e5, 0 };
    for (int key = 0; key < keyCount; key++) {
        float center = -PAPER_HALF_WIDTH + (float(key) + 0.5) * keyWidth;
        float keyboard = sdRoundBox(
            p - float3(center, 0.0, PAPER_RADIUS + 0.10),
            float3(keyWidth * 0.455, 0.29, 0.28),
            0.09
        );
        if (keyboard < result.distance) result = { keyboard, 2 };
    }
    float reader = sdFiniteCylinderX(p - float3(0.0, 0.0, PAPER_RADIUS - 0.10), PAPER_HALF_WIDTH + 0.10, 0.16);
    if (reader < result.distance) result = { reader, 3 };
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

float3 opaqueSceneNormal(float3 p) {
    const float epsilon = 0.0015;
    float2 h = float2(epsilon, 0.0);
    return normalize(float3(
        opaqueSceneDistance(p + h.xyy).distance - opaqueSceneDistance(p - h.xyy).distance,
        opaqueSceneDistance(p + h.yxy).distance - opaqueSceneDistance(p - h.yxy).distance,
        opaqueSceneDistance(p + h.yyx).distance - opaqueSceneDistance(p - h.yyx).distance
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

RayHit marchOpaque(float3 origin, float3 direction, float maximumDistance = 28.0) {
    float distance = 0.02;
    int material = 0;
    for (int step = 0; step < 120; step++) {
        float3 position = origin + direction * distance;
        DistanceHit sample = opaqueSceneDistance(position);
        material = sample.material;
        if (sample.distance < 0.0018) {
            float3 normal = opaqueSceneNormal(position);
            if (dot(normal, direction) > 0.0) normal = -normal;
            return { distance, material, position, normal, true };
        }
        distance += max(0.001, sample.distance * 0.76);
        if (distance > maximumDistance) break;
    }
    return { distance, material, origin + direction * distance, float3(0.0), false };
}

float softShadow(float3 origin, float3 direction, float maximumDistance) {
    float result = 1.0;
    float distance = 0.025;
    for (int step = 0; step < 72 && distance < maximumDistance; step++) {
        float sample = opaqueSceneDistance(origin + direction * distance).distance;
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
        occlusion += (distance - opaqueSceneDistance(position + normal * distance).distance) * weight;
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
        float readerPath = PAPER_STRAIGHT * 0.5;
        float relativePath = paperPath(hit.position) - readerPath;
        if (relativePath < 0.0) relativePath += PAPER_LOOP_LENGTH;
        float scoreTime = uniforms.time + relativePath / PAPER_LOOP_LENGTH * uniforms.loopSeconds;
        float v = 1.0 - scoreTime / uniforms.duration;
        float4 ink = scoreTime >= 0.0 && scoreTime <= uniforms.duration
            ? scoreTexture.sample(textureSampler, float2(u, clamp(v, 0.0, 1.0)))
            : float4(0.0);
        if (ink.a < 0.24) return { float3(0.0), float3(0.0), 1.0, 0.0, 0.0 };
        float rear = smoothstep(0.30, -PAPER_RADIUS, hit.position.z);
        float3 jelly = mix(ink.rgb, ink.rgb * float3(0.24, 0.20, 0.31), rear * 0.72);
        float opacity = mix(0.96, 0.30, rear);
        return { jelly, jelly * mix(0.14, 0.035, rear), mix(0.11, 0.42, rear), 0.02, opacity };
    }
    if (hit.material == 2) {
        float2 uv = float2(
            clamp(hit.position.x / (PAPER_HALF_WIDTH * 2.0) + 0.5, 0.0, 1.0),
            clamp(1.0 - (hit.position.y + 0.29) / 0.58, 0.0, 1.0)
        );
        float4 label = keyboardTexture.sample(textureSampler, uv);
        float currentV = clamp(1.0 - uniforms.time / uniforms.duration, 0.0, 1.0);
        float4 active = scoreTexture.sample(textureSampler, float2(uv.x, currentV));
        constexpr float3 palette[7] = {
            float3(0.91, 0.20, 0.20), float3(0.93, 0.50, 0.08), float3(0.91, 0.77, 0.04),
            float3(0.18, 0.67, 0.27), float3(0.10, 0.38, 0.76), float3(0.35, 0.20, 0.69),
            float3(0.61, 0.22, 0.66)
        };
        int keyIndex = clamp(int(floor(uv.x * 14.0)), 0, 13);
        float3 base = palette[keyIndex % 7];
        base = mix(base, active.rgb, active.a * 0.64);
        base = mix(base, label.rgb, label.a);
        return { base, active.rgb * active.a * 0.34, 0.12, 0.0, 0.92 };
    }
    if (hit.material == 3) return { float3(0.075, 0.055, 0.10), float3(0.0), 0.34, 0.18, 1.0 };
    if (hit.material == 4) return { float3(0.31, 0.34, 0.39), float3(0.0), 0.24, 0.72, 1.0 };
    return { float3(0.54, 0.36, 0.46), float3(0.0), 0.92, 0.0, 1.0 };
}

float3 environment(float3 direction) {
    float vertical = clamp(direction.y * 0.5 + 0.5, 0.0, 1.0);
    float3 horizon = float3(0.49, 0.36, 0.52);
    float3 sky = float3(0.95, 0.86, 0.75);
    return mix(horizon, sky, smooth01(vertical));
}

float3 acesToneMap(float3 color) {
    const float a = 2.51;
    const float b = 0.03;
    const float c = 2.43;
    const float d = 0.59;
    const float e = 0.14;
    return clamp((color * (a * color + b)) / (color * (c * color + d) + e), 0.0, 1.0);
}

float3 shadeSurface(
    RayHit hit,
    float3 direction,
    Material material,
    thread uint &randomState,
    constant Uniforms &uniforms,
    texture2d<float, access::sample> scoreTexture,
    texture2d<float, access::sample> keyboardTexture
) {
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
    float3 ambient = environment(normal) * (0.09 + 0.11 * ao);
    float3 direct = float3(1.00, 0.93, 0.82) * diffuse * shadow * 1.12;
    float3 color = material.base * (ambient + direct) * ao;
    color += specular * shadow * float3(1.0, 0.92, 0.80);
    float3 bounceDirection = cosineHemisphere(normal, randomState);
    RayHit bounce = marchOpaque(hit.position + normal * 0.018, bounceDirection);
    float3 bounceLight;
    if (bounce.hit) {
        Material bounceMaterial = materialAt(bounce, uniforms, scoreTexture, keyboardTexture);
        bounceLight = bounceMaterial.base * environment(bounce.normal) * 0.42;
    } else {
        bounceLight = environment(bounceDirection) * 0.72;
    }
    color += material.base * bounceLight * 0.23;
    float3 rimDirection = normalize(float3(4.5, 5.0, -6.5) - hit.position);
    float rim = pow(max(0.0, dot(normal, rimDirection)), 2.0) * 0.20;
    float fresnel = pow(1.0 - max(0.0, dot(normal, -direction)), 5.0);
    color += rim * float3(0.70, 0.83, 1.00) * 0.76;
    color += fresnel * mix(float3(0.12), material.base, 0.35) * (0.18 + material.opacity * 0.18);
    color += material.emission;
    return color;
}

float3 trace(
    float3 origin,
    float3 direction,
    thread uint &randomState,
    constant Uniforms &uniforms,
    texture2d<float, access::sample> scoreTexture,
    texture2d<float, access::sample> keyboardTexture
) {
    float3 color = float3(0.0);
    float throughput = 1.0;
    float3 cursor = origin;
    float lastNotePath = -1e5;
    float lastNoteX = -1e5;
    int lastKey = -1;
    for (int layer = 0; layer < 12 && throughput > 0.01; layer++) {
        RayHit hit = march(cursor, direction);
        if (!hit.hit) {
            color += environment(direction) * throughput;
            return color;
        }
        Material material = materialAt(hit, uniforms, scoreTexture, keyboardTexture);
        if (hit.material == 1 && material.opacity > 0.0) {
            float path = paperPath(hit.position);
            float pathDelta = abs(path - lastNotePath);
            pathDelta = min(pathDelta, PAPER_LOOP_LENGTH - pathDelta);
            if (pathDelta < 0.72 && abs(hit.position.x - lastNoteX) < 0.42) {
                material.opacity = 0.0;
            } else {
                lastNotePath = path;
                lastNoteX = hit.position.x;
            }
        } else if (hit.material == 2 && material.opacity > 0.0) {
            int key = clamp(int(floor((hit.position.x / (PAPER_HALF_WIDTH * 2.0) + 0.5) * 14.0)), 0, 13);
            if (key == lastKey) material.opacity = 0.0;
            else lastKey = key;
        }
        if (material.opacity > 0.0) {
            float3 surface = shadeSurface(
                hit, direction, material, randomState, uniforms, scoreTexture, keyboardTexture
            );
            color += surface * material.opacity * throughput;
            throughput *= 1.0 - material.opacity;
        }
        if (material.opacity >= 0.999) return color;
        float advance = hit.material == 1
            ? (material.opacity > 0.0 ? 0.82 : PAPER_THICKNESS * 2.35)
            : 0.60;
        cursor = hit.position + direction * advance;
    }
    return color + environment(direction) * throughput;
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
    float3 target = float3(0.0, 0.0, 0.0);
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
    color = acesToneMap(color * uniforms.exposure);
    color = pow(clamp(color, 0.0, 1.0), float3(1.0 / 2.2));
    output.write(float4(color, 1.0), gid);
}
