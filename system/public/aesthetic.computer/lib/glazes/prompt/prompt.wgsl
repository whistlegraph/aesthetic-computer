// Prompt paper. The source texture uses AC pixels, the light is sampled at
// display resolution, and the result is premultiplied over the Rax underlay.
struct Frame {
  size: vec2f,
  time: f32,
  day: f32,
  hud: vec4f, // The fps HUD's box in the image, as uv (x, y, w, h); w 0 is none.
};
@group(0) @binding(0) var image: texture_2d<f32>;
@group(0) @binding(1) var<uniform> frame: Frame;

override fogIterations: i32;
override shadowIterations: i32;
override freezeGrain: i32;
override screenScale: f32;
override shadowRange: f32;
override cameraDistance: f32;
override volumeRadius: f32;
override inputRadius: f32;
override innerDensity: f32;
override outerDensity: f32;
override anisotropy: f32;
override lightPower: f32;
override radialBlurAmount: f32;
override lightDirectionX: f32; override lightDirectionY: f32; override lightDirectionZ: f32;
override lightColorX: f32; override lightColorY: f32; override lightColorZ: f32;
override bgColorX: f32; override bgColorY: f32; override bgColorZ: f32;

@vertex fn vertex(@builtin(vertex_index) i: u32) -> @builtin(position) vec4f {
  let xy = array<vec2f, 3>(vec2f(-1,-1), vec2f(3,-1), vec2f(-1,3));
  return vec4f(xy[i], 0, 1);
}

fn paper(uv: vec2f) -> vec4f {
  let size = vec2i(textureDimensions(image));
  return textureLoad(image, clamp(vec2i(floor(uv * vec2f(size))), vec2i(0), size - 1), 0);
}
fn noise(p: vec2f) -> f32 {
  var v = fract(vec3f(p.xyx) * 0.1031);
  v += dot(v, v.yzx + 33.33);
  return fract((v.x + v.y) * v.z);
}
fn densityUV(xy: vec2f) -> vec2f {
  return 0.5 + 0.5 * (screenScale / -cameraDistance) * xy / vec2f(frame.size.x / frame.size.y, 1);
}
fn density(p: vec3f) -> vec4f {
  if abs(p.z) > volumeRadius { return vec4f(0); }
  let uv = densityUV(p.xy);
  if abs(p.z) < inputRadius && all(uv >= vec2f(0)) && all(uv <= vec2f(1)) {
    let rgb = paper(uv).rgb;
    let ink = select(rgb, 1.0 - rgb, frame.day > 0.5);
    return vec4f(ink, max(max(ink.r, ink.g), ink.b) * innerDensity);
  }
  return vec4f(bgColorX, bgColorY, bgColorZ, outerDensity);
}
fn lighting(p: vec3f, light: vec3f, jitter: f32) -> vec3f {
  let dist = min(max(-p.z / light.z, (2.0 * volumeRadius - p.z) / light.z), shadowRange);
  let stepSize = length(light * dist) / f32(shadowIterations);
  var depth = vec3f(0);
  for (var i = 1; i <= shadowIterations; i++) {
    let at = p - light * (f32(i) + jitter) / f32(shadowIterations) * dist;
    if abs(at.z) > volumeRadius && at.z * (-light.z * dist) >= 0.0 { break; }
    let c = density(at);
    depth += c.a * stepSize * select(1.0, 1.0 + jitter, i == 1) * c.rgb;
  }
  return exp(-depth);
}
fn srgb(v: vec3f) -> vec3f {
  return select(1.055 * pow(v, vec3f(1.0 / 2.4)) - 0.055, 12.92 * v, v <= vec3f(0.00031308));
}

// 🧴 The fps HUD is not part of the lit screen: it is a label under plastic
// wrap. Its pixels come through crisp, with no fog, smear or scanlines, under
// a slow crinkled sheen, a gloss toward the top and a bright edge.
fn wrapped(uv: vec2f, texel: vec2f, seconds: f32) -> vec4f {
  let src = paper(uv);
  let at = (uv - frame.hud.xy) / frame.hud.zw; // 0–1 across the box
  let px = (uv - frame.hud.xy) / texel; // in AC pixels
  let size = frame.hud.zw / texel;
  let crease = sin(px.x * 0.31 + px.y * 0.17 + 2.0 * sin(px.y * 0.11 + seconds * 0.2))
             * sin(px.x * 0.07 - px.y * 0.23 + 1.5 * sin(px.x * 0.05 - seconds * 0.13));
  let sheen = pow(max(crease, 0.0), 6.0) * 0.14;
  let gloss = 0.07 * (1.0 - at.y) * (1.0 - at.y);
  let edge = min(min(px.x, px.y), min(size.x - px.x, size.y - px.y));
  let rim = 0.22 * (1.0 - smoothstep(0.0, 1.0, edge));
  let color = saturate(src.rgb + sheen + gloss + rim);
  return vec4f(color * src.a, src.a);
}

@fragment fn fragment(@builtin(position) pos: vec4f) -> @location(0) vec4f {
  let uv = pos.xy / frame.size;
  let texel = 1.0 / vec2f(textureDimensions(image));
  if frame.hud.z > 0.0 && all(uv >= frame.hud.xy) && all(uv < frame.hud.xy + frame.hud.zw) {
    return wrapped(uv, texel, frame.time * 0.001);
  }
  let day = frame.day;
  let seconds = frame.time * 0.001;
  let ray = normalize(vec3f((uv * 2.0 - 1.0) * vec2f(frame.size.x / frame.size.y, 1.0), 1));
  let start = vec3f(0, 0, cameraDistance) + ray * (-cameraDistance / ray.z);
  let band = floor(uv.y / texel.y);
  let tracking = (noise(vec2f(floor(seconds * 30.0), band)) - 0.5) * 0.35 * mix(1.0, 0.4, day) * texel.x;
  let sampleUV = densityUV(start.xy) + vec2f(tracking, 0);
  let reach = vec2f(0.8 * mix(1.0, 0.3, day) * texel.x, 0);
  let smear = (2.0 * paper(sampleUV) + paper(sampleUV - reach) + paper(sampleUV + reach)) * 0.25;
  // No fog can contribute to a fully transparent output pixel.
  if smear.a == 0.0 { return vec4f(0); }
  let distance = abs(2.0 * volumeRadius / ray.z) + volumeRadius;
  let grain = pos.xy - floor(pos.xy / 289.0) * 289.0;
  let offset = select(frame.time % 289.0, 0.0, freezeGrain != 0);
  let camJitter = noise(grain - offset);
  let shadowJitter = noise(grain + offset);
  let refracted = mix(vec3f(0,0,1), ray, radialBlurAmount * mix(1.0, 0.25, day));
  let sway = sin(seconds * 1.5707963) + 0.3 * sin(seconds * 1.5707963 * 2.3 + 1.7);
  let direction = normalize(vec3f(lightDirectionX + sway * 0.13, lightDirectionY, lightDirectionZ));
  let light = normalize(direction + vec3f(shadowJitter - 0.5, noise(refracted.xy + shadowJitter) - 0.5, 0) * 0.18);
  let phase = 0.78539816 * (1.0 - anisotropy * anisotropy) /
    pow(1.0 + anisotropy * (anisotropy - 2.0 * dot(-light, refracted)), 1.5);
  let energy = lightPower * vec3f(lightColorX, lightColorY, lightColorZ) * phase;
  let stepSize = length(distance * refracted) / f32(fogIterations);
  var volume = vec3f(0);
  var transmission = vec3f(1);
  for (var i = 0; i < fogIterations; i++) {
    let at = start + distance * refracted * (f32(i) + camJitter) / f32(fogIterations);
    if abs(at.z) > volumeRadius && at.z * refracted.z >= 0.0 { break; }
    let c = density(at);
    if c.a == 0.0 { continue; }
    let absorbed = exp(-c.a * stepSize * select(1.0, camJitter, i == 0) * c.rgb);
    volume += (1.0 - absorbed) * transmission * energy * lighting(at, light, shadowJitter);
    transmission *= absorbed;
  }

  var color = srgb(volume);
  color = saturate(color / (1.0 + 0.3 * smoothstep(0.7, 1.0, max(max(color.r,color.g),color.b))));
  let bleed = vec2f(0.7 * mix(1.0, 0.6, day) * texel.x, 0);
  var src = mix(smear.rgb, vec3f(paper(sampleUV - bleed).r, smear.g, paper(sampleUV + bleed).b), 0.65);
  src = select(src, 1.0 - src, day > 0.5);
  color = 1.0 - (1.0 - color) * (1.0 - src * 0.55);
  color = pow(saturate(mix(vec3f(dot(color, vec3f(0.299,0.587,0.114))), color, 1.3)), vec3f(1.7));
  color = mix(color, color * color * (3.0 - 2.0 * color), 0.85);
  let fragCoord = vec2f(pos.x, frame.size.y - pos.y);
  color *= 1.0 - 0.07 * step(0.5, fract(fragCoord.y / 3.0));
  color = saturate(color + (noise(fragCoord + fract(seconds * 7.0) * 289.0) - 0.5) * 0.05);
  if day > 0.5 { color = mix(1.0 - src, 1.0 - color, 0.35); }
  let alpha = pow(smear.a, 1.1);
  return vec4f(color * alpha, alpha);
}
