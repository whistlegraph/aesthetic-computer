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

const world = `
struct Vertex { @builtin(position) position: vec4f, @location(0) color: vec3f }
@vertex fn vertex(@location(0) position: vec3f, @location(1) color: vec3f) -> Vertex {
  var out: Vertex;
  out.position = vec4f(position.xy, position.z * .5 + .5, 1.);
  if (position.z < -.5) { out.position = vec4f(2., 2., 2., 1.); } // the HUD's: not in the world's picture
  out.color = color;
  return out;
}
@fragment fn fragment(in: Vertex) -> @location(0) vec4f { return vec4f(in.color, 1.); }`;
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
// lens: 0 (none) to 1 (all of it).
const LENS = `
struct Lens { sx: f32, sy: f32, wide: f32, lens: f32 }
@group(0) @binding(0) var picture: texture_2d<f32>;
@group(0) @binding(1) var nearest: sampler;
@group(0) @binding(2) var<uniform> u: Lens;
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
  let uv = vec2f(ndc.x * .5 + .5, .5 - ndc.y * .5);
  return textureSampleLevel(picture, nearest, clamp(uv, vec2f(0.), vec2f(1.)), 0.);
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
    const make = (code, extra = {}) => { const module = device.createShaderModule({ code }); return device.createRenderPipelineAsync({ layout: 'auto', vertex: { module, entryPoint: 'vertex', buffers }, fragment: { module, entryPoint: 'fragment', targets: [{ format }] }, primitive, depthStencil: depth, ...extra }); };
    const lensModule = device.createShaderModule({ code: LENS });
    this.uniform = device.createBuffer({ size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
    this.sampler = device.createSampler({ magFilter: 'nearest', minFilter: 'nearest' });
    this.ready = Promise.all([this.ready, make(world), make(hud), device.createRenderPipelineAsync({
      layout: 'auto', vertex: { module: lensModule, entryPoint: 'vertex' }, fragment: { module: lensModule, entryPoint: 'fragment', targets: [{ format }] },
      primitive, depthStencil: { format: this.depthFormat, depthWriteEnabled: false, depthCompare: 'always' },
    })]).then(([, worldPipe, hudPipe, lensPipe]) => { this.worldPipe = worldPipe; this.hudPipe = hudPipe; this.lensPipe = lensPipe; });
    this.over = 1; // how many texture pixels to a screen pixel, each way
  }
  // The texture the world is drawn into, and its own depth: made again when the screen or its share changes.
  room(over) {
    const max = this.device.limits.maxTextureDimension2D;
    const w = Math.min(max, Math.max(1, Math.round(this.canvas.width * over))), h = Math.min(max, Math.max(1, Math.round(this.canvas.height * over)));
    if (this.picture && this.picture.width === w && this.picture.height === h) return;
    this.picture?.destroy(); this.pictureDepth?.destroy();
    this.picture = this.device.createTexture({ size: [w, h], format: this.format, usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING });
    this.pictureDepth = this.device.createTexture({ size: [w, h], format: this.depthFormat, usage: GPUTextureUsage.RENDER_ATTACHMENT });
    this.pictureView = this.picture.createView();
    this.pictureDepthView = this.pictureDepth.createView();
    this.bind = this.device.createBindGroup({ layout: this.lensPipe.getBindGroupLayout(0), entries: [
      { binding: 0, resource: this.pictureView }, { binding: 1, resource: this.sampler }, { binding: 2, resource: { buffer: this.uniform } }] });
  }
  // lens: { sx, sy, wide, lens, over } (project.mjs has the first four; over is the texture's share).
  present({ clear = [0, 0, 0, 1], shadow = null, lens = null } = {}) {
    const { device, scene } = this;
    if (!this.depth) this.resize(this.canvas.width, this.canvas.height);
    const l = lens || { sx: 1, sy: 1, wide: 1, lens: 0, over: 1 };
    this.room(Math.max(1, Math.min(2, l.over || 1)));
    if (scene.triangleCount) device.queue.writeBuffer(this.buffer, 0, scene.frameVertices());
    device.queue.writeBuffer(this.uniform, 0, new Float32Array([l.sx, l.sy, l.wide, l.lens]));
    const stencil = this.shadows ? { stencilClearValue: 0, stencilLoadOp: 'clear', stencilStoreOp: 'discard' } : {};
    const encoder = device.createCommandEncoder();
    const cast = this.shadows && shadow?.count > 0 && shadow.first + shadow.count === scene.triangleCount ? shadow : null;
    const drawn = cast ? cast.first : scene.triangleCount;
    // 1. The world, flat and wide, with its shadows.
    let pass = encoder.beginRenderPass({
      colorAttachments: [{ view: this.pictureView, clearValue: clear, loadOp: 'clear', storeOp: 'store' }],
      depthStencilAttachment: { view: this.pictureDepthView, depthClearValue: 1, depthLoadOp: 'clear', depthStoreOp: 'discard', ...stencil },
    });
    if (drawn) { pass.setPipeline(this.worldPipe); pass.setVertexBuffer(0, this.buffer); pass.draw(drawn * 3); }
    if (cast) {
      pass.setPipeline(this.volumes); pass.setVertexBuffer(0, this.buffer); pass.setStencilReference(0); pass.draw(cast.count * 3, 1, cast.first * 3);
      pass.setPipeline(this.shade); pass.setVertexBuffer(0, this.cover); pass.setBlendConstant([...(cast.shade || [0.6, 0.6, 0.6]), 1]); pass.draw(3);
    }
    pass.end();
    // 2. Through the lens to the screen, then the HUD over it, straight.
    pass = encoder.beginRenderPass({
      colorAttachments: [{ view: this.context.getCurrentTexture().createView(), clearValue: clear, loadOp: 'clear', storeOp: 'store' }],
      depthStencilAttachment: { view: this.depthView, depthClearValue: 1, depthLoadOp: 'clear', depthStoreOp: 'discard', ...stencil },
    });
    pass.setPipeline(this.lensPipe); pass.setBindGroup(0, this.bind); pass.draw(3);
    if (drawn) { pass.setPipeline(this.hudPipe); pass.setVertexBuffer(0, this.buffer); pass.draw(drawn * 3); }
    pass.end();
    device.queue.submit([encoder.finish()]);
  }
  destroy() {
    this.picture?.destroy(); this.pictureDepth?.destroy(); this.uniform?.destroy();
    super.destroy();
  }
}
