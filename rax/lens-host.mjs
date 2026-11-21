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
@fragment fn fragment(in: Vertex) -> @location(0) vec4f {
  if (in.color.r > 1.5) { return vec4f(in.color.r - 2., in.color.gb, .42); }
  return vec4f(in.color, 1.);
}
// 🌑 Where a shadow falls the picture's fourth number is put out (it is 1 everywhere else); the lens darkens by it, softly.
@fragment fn shaded() -> @location(0) vec4f { return vec4f(0.); }`;
const hud = `
struct Vertex { @builtin(position) position: vec4f, @location(0) color: vec3f }
@vertex fn vertex(@location(0) position: vec3f, @location(1) color: vec3f) -> Vertex {
  var out: Vertex;
  out.position = vec4f(position.xy, (position.z + 1.) * .5 + .5, 1.);
  if (position.z >= -.5) { out.position = vec4f(2., 2., 2., 1.); } // the world's: already on the screen
  out.color = color;
  return out;
}
@fragment fn fragment(in: Vertex) -> @location(0) vec4f { return vec4f(in.color, 1.); }`;
// sx, sy: what one unit of tangent is on the screen (the lens's zoom in them);
// wide: how much wider the texture's picture is than the screen's would be flat;
// lens: 0 (none) to 1 (all of it); fovea: how much of the wide picture, from
// its middle out, the sharp texture holds (0: there is none).
const LENS = `
struct Lens { sx: f32, sy: f32, wide: f32, lens: f32, fovea: f32, soft: f32, dither: f32, shade: vec3f }
@group(0) @binding(0) var picture: texture_2d<f32>;
@group(0) @binding(1) var nearest: sampler;
@group(0) @binding(2) var<uniform> u: Lens;
@group(0) @binding(3) var middle: texture_2d<f32>;
@group(0) @binding(4) var smoothly: sampler;
// 🌑 How lit a place is, 0 (in shadow) to 1: the mean of four looks a texel apart, each itself a mean of four, so a shadow's stepped edge is a soft one. Only the shadows are smoothed: the picture's own pixels stay hard.
fn lit(t: texture_2d<f32>, at: vec2f) -> f32 {
  let d = u.soft / vec2f(textureDimensions(t));
  return (textureSampleLevel(t, smoothly, at + d * vec2f(-1., -.35), 0.).a + textureSampleLevel(t, smoothly, at + d * vec2f(.35, -1.), 0.).a
    + textureSampleLevel(t, smoothly, at + d * vec2f(1., .35), 0.).a + textureSampleLevel(t, smoothly, at + d * vec2f(-.35, 1.), 0.).a) * .25;
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
  let rgb = select(far, sharp, within).rgb * mix(u.shade, vec3f(1.), light);
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
    const format = navigator.gpu.getPreferredCanvasFormat();
    this.format = format;
    const buffers = [{ arrayStride: 24, attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x3' }, { shaderLocation: 1, offset: 12, format: 'float32x3' }] }];
    const primitive = { topology: 'triangle-list', cullMode: 'none' };
    const depth = { format: this.depthFormat, depthWriteEnabled: true, depthCompare: 'less-equal' };
    const worldModule = device.createShaderModule({ code: world }), hudModule = device.createShaderModule({ code: hud }), lensModule = device.createShaderModule({ code: LENS });
    const made = [
      device.createRenderPipelineAsync({ layout: 'auto', vertex: { module: worldModule, entryPoint: 'vertex', buffers }, fragment: { module: worldModule, entryPoint: 'fragment', targets: [{ format, blend: { color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' }, alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' } } }] }, primitive, depthStencil: depth }),
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
        layout: 'auto', vertex: { module: worldModule, entryPoint: 'vertex', buffers }, fragment: { module: worldModule, entryPoint: 'fragment', targets: [{ format, writeMask: 0 }] }, primitive,
        depthStencil: { format: this.depthFormat, depthWriteEnabled: false, depthCompare: 'less', stencilFront: count('decrement-wrap'), stencilBack: count('increment-wrap') },
      }));
      // The shade: one triangle over the picture, marking it wherever the count is not zero (the shared host darkens there and then; here the lens does, so it can soften the edge).
      const inside = { compare: 'not-equal', failOp: 'keep', passOp: 'keep', depthFailOp: 'keep' };
      made.push(device.createRenderPipelineAsync({
        layout: 'auto', vertex: { module: worldModule, entryPoint: 'vertex', buffers }, fragment: { module: worldModule, entryPoint: 'shaded', targets: [{ format, writeMask: GPUColorWrite.ALPHA }] }, primitive,
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
      layer.picture?.destroy(); layer.depth?.destroy();
      layer.picture = this.device.createTexture({ size: [w, h], format: this.format, usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING });
      layer.depth = this.device.createTexture({ size: [w, h], format: this.depthFormat, usage: GPUTextureUsage.RENDER_ATTACHMENT });
      layer.view = layer.picture.createView();
      layer.depthView = layer.depth.createView();
    });
    if (changed || !this.bind || this.bindSmooth !== smooth) this.bind = this.device.createBindGroup({ layout: this.lensPipe.getBindGroupLayout(0), entries: [
      { binding: 0, resource: this.layers[0].view }, { binding: 1, resource: smooth ? this.smooth : this.sampler }, { binding: 2, resource: { buffer: this.uniform } }, { binding: 3, resource: this.layers[1].view }, { binding: 4, resource: this.smooth }] });
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
    device.queue.writeBuffer(this.uniform, 0, new Float32Array([l.sx, l.sy, l.wide, l.lens, fovea, l.soft ?? 1, l.dither || 0, 0, ...(shadow?.shade || [1, 1, 1]), 0]));
    device.queue.writeBuffer(this.layers[0].zoom, 0, new Float32Array([1, 0, 0, 0]));
    device.queue.writeBuffer(this.layers[1].zoom, 0, new Float32Array([fovea ? 1 / fovea : 1, 0, 0, 0]));
    const stencil = this.shadows ? { stencilClearValue: 0, stencilLoadOp: 'clear', stencilStoreOp: 'discard' } : {};
    const encoder = device.createCommandEncoder();
    const cast = this.shadows && shadow?.count > 0 && shadow.first + shadow.count === scene.triangleCount ? shadow : null;
    const drawn = cast ? cast.first : scene.triangleCount;
    // 1. The world, flat and wide, with its shadows: once whole, once its middle.
    for (const layer of fovea ? this.layers : [this.layers[0]]) {
      const pass = encoder.beginRenderPass({
        colorAttachments: [{ view: layer.view, clearValue: clear, loadOp: 'clear', storeOp: 'store' }],
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
    for (const layer of this.layers) { layer.picture?.destroy(); layer.depth?.destroy(); layer.zoom?.destroy(); }
    this.uniform?.destroy();
    super.destroy();
  }
}
