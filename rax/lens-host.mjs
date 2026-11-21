// 🔭 The lens, done to the finished picture on the card.
// The world is drawn flat (a pinhole's picture, wider than the screen) into
// a texture; then one triangle over the screen looks each pixel up in it
// through a wide lens (the stereographic one: a thing at angle a from the
// middle lies 2 tan(a/2) out, not tan(a)), so the middle is as it was, the
// edges are drawn in and nothing there is pulled long. Because the picture
// is whole before it is bent, things that lie on one another stay in order:
// no holes, no shadows out of place. The HUD is drawn after, straight.
//
// This is the shared host (xbox/live/scene3d-webgpu.mjs) with two passes in
// place of its one; that file is not touched. What is HUD is told by depth:
// the view writes its overlays a whole unit nearer than anything in the
// world (z below -0.5 in the stream; project.mjs `under`).
import { WebGPUOskiewarScene3D } from './engine/scene3d-webgpu.mjs';
const MASK = 'r8unorm'; // 📺 the screens' mask: 1 on a screen's pixel, 0 elsewhere

// (zoom: 1 for the whole picture; for the middle one, drawn larger, how many times.)
const world = `
struct Layer { zoom: f32 }
@group(0) @binding(0) var<uniform> layer: Layer;
struct Vertex { @builtin(position) position: vec4f, @location(0) color: vec3f }
@vertex fn vertex(@location(0) position: vec3f, @location(1) color: vec3f) -> Vertex {
  var out: Vertex;
  out.position = vec4f(position.xy * layer.zoom, position.z * .5 + .5, 1.);
  if (position.z < -.5) { out.position = vec4f(2., 2., 2., 1.); } // the HUD's: not in the world's picture
  out.color = color;
  return out;
}
// 🫧 A red over 1.5 marks a thing seen through (view.mjs adds 2 to it, and
// draws it after everything else in the world): it is laid a little under
// half over what is already there.
// 📺 A green over 3.5 marks a screen's pixel (project.mjs lifts it by 4): its
// colour goes to the picture and a 1 to the second picture, the screens' mask,
// so the lens can glaze screens and grade the rest of the world apart.
struct Painted { @location(0) color: vec4f, @location(1) screen: vec4f }
@fragment fn fragment(in: Vertex) -> Painted {
  var c = in.color;
  var out: Painted;
  out.screen = vec4f(0.);
  if (c.g > 3.5) { c.g -= 4.; out.screen = vec4f(1.); }
  out.color = vec4f(c, 1.);
  if (c.r > 1.5) { out.color = vec4f(c.r - 2., c.gb, .42); }
  return out;
}
// 🌑 Where a shadow falls the picture's fourth number is put out (it is 1 everywhere else); the lens darkens by it.
@fragment fn shaded() -> Painted { var out: Painted; out.color = vec4f(0.); out.screen = vec4f(0.); return out; }`;
const hud = `
struct Vertex { @builtin(position) position: vec4f, @location(0) color: vec3f }
@vertex fn vertex(@location(0) position: vec3f, @location(1) color: vec3f) -> Vertex {
  var out: Vertex;
  out.position = vec4f(position.xy, (position.z + 1.) * .5 + .5, 1.);
  if (position.z >= -.5) { out.position = vec4f(2., 2., 2., 1.); } // the world's: already on the screen
  out.color = color;
  return out;
}
@fragment fn fragment(in: Vertex) -> @location(0) vec4f { return vec4f(in.color.r, select(in.color.g, in.color.g - 4., in.color.g > 3.5), in.color.b, 1.); }`;
// sx, sy: what one unit of tangent is on the screen (the lens's zoom in them);
// wide: how much wider the texture's picture is than the screen's would be flat;
// lens: 0 (none) to 1 (all of it); fovea: how much of the wide picture, from
// its middle out, the sharp texture holds (0: there is none).
const LENS = `
struct Lens { sx: f32, sy: f32, wide: f32, lens: f32, fovea: f32, soft: f32, dither: f32, time: f32, shade: vec3f, glaze: f32 }
@group(0) @binding(0) var picture: texture_2d<f32>;
@group(0) @binding(1) var nearest: sampler;
@group(0) @binding(2) var<uniform> u: Lens;
@group(0) @binding(3) var middle: texture_2d<f32>;
@group(0) @binding(4) var smoothly: sampler;
@group(0) @binding(5) var farMask: texture_2d<f32>;
@group(0) @binding(6) var middleMask: texture_2d<f32>;
// 🌑 How lit a place is, 0 (in shadow) to 1. With soft at 0 (the default) it
// is one look: the stencil's edge stays hard, as cut. Above 0 it is the mean
// of four looks that many texels apart, each itself a mean of four.
fn lit(t: texture_2d<f32>, at: vec2f) -> f32 {
  if (u.soft <= 0.) { return textureSampleLevel(t, smoothly, at, 0.).a; }
  let d = u.soft / vec2f(textureDimensions(t));
  return (textureSampleLevel(t, smoothly, at + d * vec2f(-1., -.35), 0.).a + textureSampleLevel(t, smoothly, at + d * vec2f(.35, -1.), 0.).a
    + textureSampleLevel(t, smoothly, at + d * vec2f(1., .35), 0.).a + textureSampleLevel(t, smoothly, at + d * vec2f(-.35, 1.), 0.).a) * .25;
}
fn look(at: vec2f, within: bool) -> vec3f {
  return select(textureSampleLevel(picture, nearest, at, 0.), textureSampleLevel(middle, nearest, at, 0.), within).rgb;
}
fn masked(at: vec2f, within: bool) -> f32 {
  return select(textureSampleLevel(farMask, nearest, at, 0.).r, textureSampleLevel(middleMask, nearest, at, 0.).r, within);
}
fn hash(p: vec2f) -> f32 {
  var v = fract(vec3f(p.xyx) * .1031);
  v += dot(v, v.yzx + 33.33);
  return fract((v.x + v.y) * v.z);
}
// 📺 A screen, glazed as the Prompt's paper is (lib/glazes/prompt/prompt.wgsl),
// in the picture's own space: each band of rows slips a little in time
// (tracking), the colour smears sideways and red and blue part (bleed), the
// tones are pushed and filled (contrast, colour), every third row is a touch
// darker (the lines) and there is a fine moving grain.
fn glazed(at: vec2f, px: vec2f, within: bool, pos: vec2f, here: vec3f) -> vec3f {
  let band = floor(pos.y / 3.);
  let slip = (hash(vec2f(floor(u.time * 30.), band)) - .5) * .35 * px.x;
  let a = at + vec2f(slip, 0.);
  let reach = vec2f(.8 * px.x, 0.);
  let smear = (2. * look(a, within) + look(a - reach, within) + look(a + reach, within)) * .25;
  let bleed = vec2f(.7 * px.x, 0.);
  var c = mix(smear, vec3f(look(a - bleed, within).r, smear.g, look(a + bleed, within).b), .65);
  c = mix(vec3f(dot(c, vec3f(.299, .587, .114))), c, 1.25);
  c = mix(c, c * c * (3. - 2. * c), .45);
  c *= 1. - .07 * step(.5, fract(pos.y / 3.));
  c += (hash(pos + fract(u.time * 7.) * 289.) - .5) * .05;
  return clamp(c, vec3f(0.), vec3f(1.));
}
// The glow off the screens: the mean of what eight looks, three and six
// pixels out, find on a screen.
fn spill(at: vec2f, px: vec2f, within: bool) -> vec3f {
  var sum = vec3f(0.);
  for (var i = 0; i < 8; i++) {
    let angle = f32(i) * .785398 + .3;
    let d = vec2f(cos(angle), sin(angle)) * px * select(3., 6., i % 2 == 1);
    sum += look(at + d, within) * masked(at + d, within);
  }
  return sum / 8.;
}
// 🌍 The world outside the screens: a quiet print, not a screen. The shadows
// lean cool and the lights warm, the colour is held back a little, the
// darkest ink is lifted off black and the corners fall away.
fn graded(c: vec3f, at: vec2f) -> vec3f {
  let y = dot(c, vec3f(.299, .587, .114));
  var g = mix(vec3f(y), c, .88);
  g = g * mix(vec3f(.96, .99, 1.05), vec3f(1.04, 1.0, .95), smoothstep(.15, .85, y));
  g = .035 + g * .965;
  let r = length(at * vec2f(.9, 1.));
  g *= 1. - .22 * smoothstep(.55, 1.45, r);
  return clamp(g, vec3f(0.), vec3f(1.));
}
struct Out { @builtin(position) position: vec4f, @location(0) at: vec2f }
@vertex fn vertex(@builtin(vertex_index) i: u32) -> Out {
  var out: Out;
  let corner = vec2f(f32((i << 1u) & 2u), f32(i & 2u)) * 2. - 1.;
  out.position = vec4f(corner, 0., 1.);
  out.at = corner; // the screen, -1 to 1, y up
  return out;
}
@fragment fn fragment(in: Out) -> @location(0) vec4f {
  // Where this pixel is, as a tangent off the middle of the lensed picture.
  let q = in.at / vec2f(u.sx, u.sy);
  let r = length(q);
  // How far out the flat picture has the same thing: t, where r = t k(t).
  var t = r;
  if (u.lens > 0. && r > 0.) {
    for (var n = 0; n < 6; n++) {
      let s = sqrt(1. + t * t);
      let k = 1. + (2. / (1. + s) - 1.) * u.lens;
      let dk = (-2. * t) / (s * (1. + s) * (1. + s)) * u.lens;
      t = t - (t * k - r) / (k + t * dk);
    }
  }
  let flat = select(q, q * (t / r), r > 0.);
  // The texture's picture is the screen's flat one, at u.wide times the scale.
  let ndc = flat * vec2f(u.sx, u.sy) * u.wide;
  let farAt = clamp(vec2f(ndc.x * .5 + .5, .5 - ndc.y * .5), vec2f(0.), vec2f(1.));
  let far = textureSampleLevel(picture, nearest, farAt, 0.);
  // 👁️ Near the middle the sharp texture has it (a texel in from its edge, so none of its border is read).
  let inner = ndc / max(u.fovea, .0001);
  let sharpAt = clamp(vec2f(inner.x * .5 + .5, .5 - inner.y * .5), vec2f(0.), vec2f(1.));
  let sharp = textureSampleLevel(middle, nearest, sharpAt, 0.);
  let within = u.fovea > 0. && max(abs(inner.x), abs(inner.y)) < .985;
  let light = select(lit(picture, farAt), lit(middle, sharpAt), within);
  var rgb = select(far, sharp, within).rgb;
  if (u.glaze > 0.) {
    // 📺 The screens and the world, glazed apart (glazed() and graded() below).
    let at = select(farAt, sharpAt, within);
    // One screen pixel, in the picture's measure: taken from the far picture's place, which runs on smoothly where the middle's begins (the middle is 1 / fovea times larger).
    let px = fwidth(farAt) * select(1., 1. / max(u.fovea, .0001), within);
    let screen = select(textureSampleLevel(farMask, nearest, farAt, 0.).r, textureSampleLevel(middleMask, nearest, sharpAt, 0.).r, within);
    if (screen > .5) {
      rgb = mix(rgb, glazed(at, px, within, in.position.xy, rgb), u.glaze);
    } else {
      rgb = rgb * mix(u.shade, vec3f(1.), light);
      rgb = mix(rgb, graded(rgb, in.at), u.glaze);
    }
    // The screens' light falls a little way out over what is round them.
    rgb += spill(at, px, within) * .3 * u.glaze;
  } else {
    rgb *= mix(u.shade, vec3f(1.), light);
  }
  // Quantize only mixed edge pixels. The ordered pattern stays on the paper grid.
  let cell = vec2u(in.position.xy) % 4u;
  let bayer = array<u32,16>(0,8,2,10,12,4,14,6,3,11,1,9,15,7,13,5);
  let threshold = (f32(bayer[cell.y*4u+cell.x]) + 0.5) / 16.;
  let edge = clamp(length(fwidth(rgb)) * 6., 0., 1.);
  return vec4f(mix(rgb, floor(rgb * 31. + threshold) / 31., edge * u.dither), 1.);
}`;

// How far out (as a tangent) the flat picture must reach for a place r out on
// the lensed one: the inverse of r = t (1 + (2 / (1 + sqrt(1 + t t)) - 1) lens).
export function unlens(r, lens) {
  if (!(lens > 0) || !(r > 0)) return r;
  let t = r;
  for (let i = 0; i < 8; i++) {
    const s = Math.sqrt(1 + t * t), k = 1 + (2 / (1 + s) - 1) * lens;
    const dk = (-2 * t) / (s * (1 + s) * (1 + s)) * lens;
    t -= (t * k - r) / (k + t * dk);
  }
  return t;
}
// The flat picture's scale beside the screen's: under 1 (wider) so the
// screen's corners find their places in it. sx, sy are the screen's.
export function lensWide(sx, sy, lens) {
  if (!(lens > 0)) return 1;
  const r = Math.hypot(1 / sx, 1 / sy);
  return r / unlens(r, lens);
}

export class LensScene3D extends WebGPUOskiewarScene3D {
  static async create(canvas, options = {}) {
    const adapter = await navigator.gpu?.requestAdapter({ powerPreference: 'high-performance' });
    if (!adapter) throw new Error('WebGPU adapter unavailable');
    const device = await adapter.requestDevice();
    try {
      const host = new LensScene3D(canvas, device, options);
      await host.ready;
      return host;
    } catch (error) {
      device.destroy();
      throw error;
    }
  }
  constructor(canvas, device, options = {}) {
    super(canvas, device, options);
    // 📺 `localStorage["rax:glaze"] = "off"` shows the screens and the world unglazed; `rax:soft` (texels, e.g. "1") softens the shadows' edges again.
    const stored = (key) => { try { return localStorage.getItem(key); } catch { return null; } };
    this.glaze = stored('rax:glaze') === 'off' ? 0 : 1;
    this.soft = Math.max(0, Number(stored('rax:soft')) || 0);
    const format = navigator.gpu.getPreferredCanvasFormat();
    this.format = format;
    const buffers = [{ arrayStride: 24, attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x3' }, { shaderLocation: 1, offset: 12, format: 'float32x3' }] }];
    const primitive = { topology: 'triangle-list', cullMode: 'none' };
    const depth = { format: this.depthFormat, depthWriteEnabled: true, depthCompare: 'less-equal' };
    const worldModule = device.createShaderModule({ code: world }), hudModule = device.createShaderModule({ code: hud }), lensModule = device.createShaderModule({ code: LENS });
    const made = [
      device.createRenderPipelineAsync({ layout: 'auto', vertex: { module: worldModule, entryPoint: 'vertex', buffers }, fragment: { module: worldModule, entryPoint: 'fragment', targets: [{ format, blend: { color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' }, alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' } } }, { format: MASK }] }, primitive, depthStencil: depth }),
      device.createRenderPipelineAsync({ layout: 'auto', vertex: { module: hudModule, entryPoint: 'vertex', buffers }, fragment: { module: hudModule, entryPoint: 'fragment', targets: [{ format }] }, primitive, depthStencil: depth }),
      device.createRenderPipelineAsync({
        layout: 'auto', vertex: { module: lensModule, entryPoint: 'vertex' }, fragment: { module: lensModule, entryPoint: 'fragment', targets: [{ format }] },
        primitive, depthStencil: { format: this.depthFormat, depthWriteEnabled: false, depthCompare: 'always' },
      }),
    ];
    if (this.shadows) {
      // The shadows' solids, counted as the shared host counts them, drawn at each picture's own size.
      const count = (depthFailOp) => ({ compare: 'always', failOp: 'keep', passOp: 'keep', depthFailOp });
      made.push(device.createRenderPipelineAsync({
        layout: 'auto', vertex: { module: worldModule, entryPoint: 'vertex', buffers }, fragment: { module: worldModule, entryPoint: 'fragment', targets: [{ format, writeMask: 0 }, { format: MASK, writeMask: 0 }] }, primitive,
        depthStencil: { format: this.depthFormat, depthWriteEnabled: false, depthCompare: 'less', stencilFront: count('decrement-wrap'), stencilBack: count('increment-wrap') },
      }));
      // The shade: one triangle over the picture, marking it wherever the count is not zero (the shared host darkens there and then; here the lens does, so it can soften the edge).
      const inside = { compare: 'not-equal', failOp: 'keep', passOp: 'keep', depthFailOp: 'keep' };
      made.push(device.createRenderPipelineAsync({
        layout: 'auto', vertex: { module: worldModule, entryPoint: 'vertex', buffers }, fragment: { module: worldModule, entryPoint: 'shaded', targets: [{ format, writeMask: GPUColorWrite.ALPHA }, { format: MASK, writeMask: 0 }] }, primitive,
        depthStencil: { format: this.depthFormat, depthWriteEnabled: false, depthCompare: 'less', stencilFront: inside, stencilBack: inside, stencilWriteMask: 0 },
      }));
    }
    this.uniform = device.createBuffer({ size: 48, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
    this.sampler = device.createSampler({ magFilter: 'nearest', minFilter: 'nearest' });
    this.smooth = device.createSampler({ magFilter: 'linear', minFilter: 'linear' }); // for a picture drawn finer than it is shown (aa)
    // Two pictures of the world: the whole of it, and its middle larger.
    this.layers = [0, 1].map(() => ({ zoom: device.createBuffer({ size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST }) }));
    this.ready = Promise.all([this.ready, ...made]).then(([, worldPipe, hudPipe, lensPipe, solidPipe, markPipe]) => {
      this.worldPipe = worldPipe; this.hudPipe = hudPipe; this.lensPipe = lensPipe; this.solidPipe = solidPipe || null; this.markPipe = markPipe || null;
      for (const layer of this.layers) {
        layer.world = device.createBindGroup({ layout: worldPipe.getBindGroupLayout(0), entries: [{ binding: 0, resource: { buffer: layer.zoom } }] });
        layer.mark = markPipe ? device.createBindGroup({ layout: markPipe.getBindGroupLayout(0), entries: [{ binding: 0, resource: { buffer: layer.zoom } }] }) : null;
        layer.solid = solidPipe ? device.createBindGroup({ layout: solidPipe.getBindGroupLayout(0), entries: [{ binding: 0, resource: { buffer: layer.zoom } }] }) : null;
      }
    });
  }
  // The textures the world is drawn into, each with its own depth: made again when a size changes.
  room(sizes, smooth = false) {
    const max = this.device.limits.maxTextureDimension2D;
    let changed = false;
    this.layers.forEach((layer, n) => {
      const w = Math.min(max, Math.max(1, Math.round(this.canvas.width * sizes[n]))), h = Math.min(max, Math.max(1, Math.round(this.canvas.height * sizes[n])));
      if (layer.picture && layer.picture.width === w && layer.picture.height === h) return;
      changed = true;
      layer.picture?.destroy(); layer.depth?.destroy(); layer.mask?.destroy();
      layer.picture = this.device.createTexture({ size: [w, h], format: this.format, usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING });
      layer.depth = this.device.createTexture({ size: [w, h], format: this.depthFormat, usage: GPUTextureUsage.RENDER_ATTACHMENT });
      layer.mask = this.device.createTexture({ size: [w, h], format: MASK, usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING }); // 📺 where the screens are
      layer.maskView = layer.mask.createView();
      layer.view = layer.picture.createView();
      layer.depthView = layer.depth.createView();
    });
    if (changed || !this.bind || this.bindSmooth !== smooth) this.bind = this.device.createBindGroup({ layout: this.lensPipe.getBindGroupLayout(0), entries: [
      { binding: 0, resource: this.layers[0].view }, { binding: 1, resource: smooth ? this.smooth : this.sampler }, { binding: 2, resource: { buffer: this.uniform } }, { binding: 3, resource: this.layers[1].view }, { binding: 4, resource: this.smooth },
      { binding: 5, resource: this.layers[0].maskView }, { binding: 6, resource: this.layers[1].maskView }] });
    this.bindSmooth = smooth;
  }
  // lens: { sx, sy, wide, lens, over, fovea } (project.mjs has the first four).
  // over: texture pixels to a screen pixel where the picture is sharpest.
  // 👁️ fovea (0 to 1; 0: none): the share of the wide picture, from its middle
  // out, that is drawn at that sharpness; the rest is drawn at half of it.
  // The lens draws the picture's edges in, so they were being drawn finer
  // than they are ever shown: this spends the pixels where they are seen.
  present({ clear = [0, 0, 0, 1], shadow = null, lens = null } = {}) {
    const { device, scene } = this;
    if (!this.depth) this.resize(this.canvas.width, this.canvas.height);
    const l = lens || { sx: 1, sy: 1, wide: 1, lens: 0, over: 1, fovea: 0 };
    // 🪶 aa (1, 2 or 3): the world is drawn that many times finer each way than it is shown and each screen pixel takes the mean of what falls in it, so edges are soft where they were stepped (full-screen anti-aliasing, by supersampling).
    const aa = Math.max(1, Math.min(3, Math.round(l.aa || 1)));
    const over = Math.max(1, Math.min(2, l.over || 1)) * aa, fovea = l.fovea > 0 && l.fovea < 1 ? l.fovea : 0;
    // The whole picture at half sharpness and the middle at all of it; or, with no fovea, the whole at all of it (and a middle of one pixel, unused).
    this.room(fovea ? [over * 0.5, over * fovea] : [over, 1 / Math.max(this.canvas.width, this.canvas.height)], aa > 1);
    if (scene.triangleCount) device.queue.writeBuffer(this.buffer, 0, scene.frameVertices());
    device.queue.writeBuffer(this.uniform, 0, new Float32Array([l.sx, l.sy, l.wide, l.lens, fovea, l.soft ?? this.soft, l.dither || 0, (performance.now() / 1000) % 1000, ...(shadow?.shade || [1, 1, 1]), l.glaze ?? this.glaze]));
    device.queue.writeBuffer(this.layers[0].zoom, 0, new Float32Array([1, 0, 0, 0]));
    device.queue.writeBuffer(this.layers[1].zoom, 0, new Float32Array([fovea ? 1 / fovea : 1, 0, 0, 0]));
    const stencil = this.shadows ? { stencilClearValue: 0, stencilLoadOp: 'clear', stencilStoreOp: 'discard' } : {};
    const encoder = device.createCommandEncoder();
    const cast = this.shadows && shadow?.count > 0 && shadow.first + shadow.count === scene.triangleCount ? shadow : null;
    const drawn = cast ? cast.first : scene.triangleCount;
    // 1. The world, flat and wide, with its shadows: once whole, once its middle.
    for (const layer of fovea ? this.layers : [this.layers[0]]) {
      const pass = encoder.beginRenderPass({
        colorAttachments: [{ view: layer.view, clearValue: clear, loadOp: 'clear', storeOp: 'store' }, { view: layer.maskView, clearValue: [0, 0, 0, 0], loadOp: 'clear', storeOp: 'store' }],
        depthStencilAttachment: { view: layer.depthView, depthClearValue: 1, depthLoadOp: 'clear', depthStoreOp: 'discard', ...stencil },
      });
      if (drawn) { pass.setPipeline(this.worldPipe); pass.setBindGroup(0, layer.world); pass.setVertexBuffer(0, this.buffer); pass.draw(drawn * 3); }
      if (cast && this.solidPipe && this.markPipe) {
        pass.setPipeline(this.solidPipe); pass.setBindGroup(0, layer.solid); pass.setVertexBuffer(0, this.buffer); pass.setStencilReference(0); pass.draw(cast.count * 3, 1, cast.first * 3);
        pass.setPipeline(this.markPipe); pass.setBindGroup(0, layer.mark); pass.setVertexBuffer(0, this.cover); pass.setStencilReference(0); pass.draw(3);
      }
      pass.end();
    }
    // 2. Through the lens to the screen, then the HUD over it, straight.
    const pass = encoder.beginRenderPass({
      colorAttachments: [{ view: this.context.getCurrentTexture().createView(), clearValue: clear, loadOp: 'clear', storeOp: 'store' }],
      depthStencilAttachment: { view: this.depthView, depthClearValue: 1, depthLoadOp: 'clear', depthStoreOp: 'discard', ...stencil },
    });
    pass.setPipeline(this.lensPipe); pass.setBindGroup(0, this.bind); pass.draw(3);
    if (drawn) { pass.setPipeline(this.hudPipe); pass.setVertexBuffer(0, this.buffer); pass.draw(drawn * 3); }
    pass.end();
    device.queue.submit([encoder.finish()]);
  }
  destroy() {
    for (const layer of this.layers) { layer.picture?.destroy(); layer.depth?.destroy(); layer.mask?.destroy(); layer.zoom?.destroy(); }
    this.uniform?.destroy();
    super.destroy();
  }
}
