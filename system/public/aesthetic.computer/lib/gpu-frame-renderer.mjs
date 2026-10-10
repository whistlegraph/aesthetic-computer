// Draws a gpu-frame.mjs buffer with WebGPU: every primitive tessellated to
// triangles with per-vertex colour on the CPU (fast, no allocations across
// frames), one vertex buffer, one pipeline, one draw. Then the CPU screen
// buffer, when the frame asks for it, composited on top as a texture so
// text and anything the GPU path does not draw still show. Runs in bios on
// its own canvas, sized like the main canvas.
import { readFrame } from "./gpu-frame.mjs";
import { sceneCamera, placeMesh, projectMesh } from "./kidlisp-mesh.mjs";

const SEGMENTS = 24;                       // an oval's polygon
const FLOATS_PER_VERTEX = 6;               // x y r g b a

export async function createFrameRenderer(canvas) {
  if (typeof navigator === "undefined" || !navigator.gpu) return null;
  const adapter = await navigator.gpu.requestAdapter();
  if (!adapter) return null;
  const device = await adapter.requestDevice();
  const context = canvas.getContext("webgpu");
  if (!context) return null;
  const format = navigator.gpu.getPreferredCanvasFormat();
  let configured = { w: 0, h: 0 };
  const configure = () => {
    if (canvas.width === configured.w && canvas.height === configured.h) return;
    if (!canvas.width || !canvas.height) return;
    context.configure({ device, format, alphaMode: "opaque" });
    configured = { w: canvas.width, h: canvas.height };
  };
  const shader = device.createShaderModule({ code: `
    struct U { resolution: vec2f, _pad: vec2f }
    @group(0) @binding(0) var<uniform> u: U;
    struct VSOut { @builtin(position) pos: vec4f, @location(0) color: vec4f }
    @vertex fn vs(@location(0) p: vec2f, @location(1) c: vec4f) -> VSOut {
      var o: VSOut;
      o.pos = vec4f(p.x / u.resolution.x * 2.0 - 1.0, 1.0 - p.y / u.resolution.y * 2.0, 0.0, 1.0);
      o.color = vec4f(c.rgb * c.a, c.a);   // premultiplied for the blend below
      return o;
    }
    @fragment fn fs(@location(0) c: vec4f) -> @location(0) vec4f { return c; }
  ` });
  const blend = { color: { srcFactor: "one", dstFactor: "one-minus-src-alpha", operation: "add" }, alpha: { srcFactor: "one", dstFactor: "one-minus-src-alpha", operation: "add" } };
  const pipeline = device.createRenderPipeline({ layout: "auto", vertex: { module: shader, entryPoint: "vs", buffers: [{ arrayStride: FLOATS_PER_VERTEX * 4, attributes: [{ shaderLocation: 0, offset: 0, format: "float32x2" }, { shaderLocation: 1, offset: 8, format: "float32x4" }] }] }, fragment: { module: shader, entryPoint: "fs", targets: [{ format, blend }] }, primitive: { topology: "triangle-list" } });
  const uniform = device.createBuffer({ size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
  const bindGroup = device.createBindGroup({ layout: pipeline.getBindGroupLayout(0), entries: [{ binding: 0, resource: { buffer: uniform } }] });
  // The overlay: the CPU buffer as a texture over the scene.
  const overlayShader = device.createShaderModule({ code: `
    @group(0) @binding(0) var s: sampler;
    @group(0) @binding(1) var t: texture_2d<f32>;
    struct VSOut { @builtin(position) pos: vec4f, @location(0) uv: vec2f }
    @vertex fn vs(@builtin(vertex_index) i: u32) -> VSOut {
      var p = array<vec2f, 6>(vec2f(-1,-1), vec2f(1,-1), vec2f(-1,1), vec2f(-1,1), vec2f(1,-1), vec2f(1,1));
      var o: VSOut; o.pos = vec4f(p[i], 0.0, 1.0); o.uv = vec2f((p[i].x + 1.0) * 0.5, 1.0 - (p[i].y + 1.0) * 0.5); return o;
    }
    @fragment fn fs(@location(0) uv: vec2f) -> @location(0) vec4f { let c = textureSample(t, s, uv); return vec4f(c.rgb * c.a, c.a); }
  ` });
  const overlayPipeline = device.createRenderPipeline({ layout: "auto", vertex: { module: overlayShader, entryPoint: "vs" }, fragment: { module: overlayShader, entryPoint: "fs", targets: [{ format, blend }] }, primitive: { topology: "triangle-list" } });
  const sampler = device.createSampler({ magFilter: "nearest", minFilter: "nearest" });
  let overlayTexture = null, overlayBind = null, overlaySize = { w: 0, h: 0 };

  let vertices = new Float32Array(1 << 18), count = 0, vertexBuffer = null, vertexCapacity = 0;
  const need = (n) => { if ((count + n) * FLOATS_PER_VERTEX > vertices.length) { const next = new Float32Array(Math.max(vertices.length * 2, (count + n) * FLOATS_PER_VERTEX)); next.set(vertices.subarray(0, count * FLOATS_PER_VERTEX)); vertices = next; } };
  const vert = (x, y, r, g, b, a) => { let i = count * FLOATS_PER_VERTEX; vertices[i] = x; vertices[i + 1] = y; vertices[i + 2] = r / 255; vertices[i + 3] = g / 255; vertices[i + 4] = b / 255; vertices[i + 5] = a / 255; count++; };
  const triangle = (x1, y1, x2, y2, x3, y3, r, g, b, a) => { need(3); vert(x1, y1, r, g, b, a); vert(x2, y2, r, g, b, a); vert(x3, y3, r, g, b, a); };
  const quad = (x1, y1, x2, y2, x3, y3, x4, y4, r, g, b, a) => { triangle(x1, y1, x2, y2, x3, y3, r, g, b, a); triangle(x1, y1, x3, y3, x4, y4, r, g, b, a); };
  const strokeLine = (x1, y1, x2, y2, th, r, g, b, a) => {
    // The software rasterizer paints pixel centres; a 1-px line here is a 1-px quad along the segment.
    const dx = x2 - x1, dy = y2 - y1, len = Math.hypot(dx, dy) || 1, w = Math.max(1, th) / 2;
    const nx = -dy / len * w, ny = dx / len * w;
    const ex = dx / len * 0.5, ey = dy / len * 0.5;    // extend half a pixel so endpoints are covered
    quad(x1 - ex + nx + 0.5, y1 - ey + ny + 0.5, x2 + ex + nx + 0.5, y2 + ey + ny + 0.5, x2 + ex - nx + 0.5, y2 + ey - ny + 0.5, x1 - ex - nx + 0.5, y1 - ey - ny + 0.5, r, g, b, a);
  };
  const visit = {
    clear: null,
    line: strokeLine,
    box: (x, y, w, h, fill, r, g, b, a) => {
      if (w < 0) { x += w; w = -w; } if (h < 0) { y += h; h = -h; }
      if (fill) quad(x, y, x + w, y, x + w, y + h, x, y + h, r, g, b, a);
      else { strokeLine(x, y, x + w - 1, y, 1, r, g, b, a); strokeLine(x + w - 1, y, x + w - 1, y + h - 1, 1, r, g, b, a); strokeLine(x + w - 1, y + h - 1, x, y + h - 1, 1, r, g, b, a); strokeLine(x, y + h - 1, x, y, 1, r, g, b, a); }
    },
    oval: (cx, cy, rx, ry, fill, r, g, b, a) => {
      const n = Math.max(8, Math.min(64, Math.round(Math.max(rx, ry) * 2))) & ~1;
      cx += 0.5; cy += 0.5; rx += 0.5; ry += 0.5;
      if (fill) { need(n * 3); for (let k = 0; k < n; k++) { const t0 = (k / n) * Math.PI * 2, t1 = ((k + 1) / n) * Math.PI * 2; vert(cx, cy, r, g, b, a); vert(cx + Math.cos(t0) * rx, cy + Math.sin(t0) * ry, r, g, b, a); vert(cx + Math.cos(t1) * rx, cy + Math.sin(t1) * ry, r, g, b, a); } }
      else for (let k = 0; k < n; k++) { const t0 = (k / n) * Math.PI * 2, t1 = ((k + 1) / n) * Math.PI * 2; strokeLine(cx + Math.cos(t0) * rx - 0.5, cy + Math.sin(t0) * ry - 0.5, cx + Math.cos(t1) * rx - 0.5, cy + Math.sin(t1) * ry - 0.5, 1, r, g, b, a); }
    },
    tri: (x1, y1, x2, y2, x3, y3, fill, r, g, b, a) => {
      if (fill) triangle(x1 + 0.5, y1 + 0.5, x2 + 0.5, y2 + 0.5, x3 + 0.5, y3 + 0.5, r, g, b, a);
      else { strokeLine(x1, y1, x2, y2, 1, r, g, b, a); strokeLine(x2, y2, x3, y3, 1, r, g, b, a); strokeLine(x3, y3, x1, y1, 1, r, g, b, a); }
    },
    shape: (points, fill, r, g, b, a) => {
      const n = points.length >> 1;
      if (fill) { for (let k = 1; k < n - 1; k++) triangle(points[0] + 0.5, points[1] + 0.5, points[k * 2] + 0.5, points[k * 2 + 1] + 0.5, points[k * 2 + 2] + 0.5, points[k * 2 + 3] + 0.5, r, g, b, a); }
      else for (let k = 0; k < n; k++) { const j = (k + 1) % n; strokeLine(points[k * 2], points[k * 2 + 1], points[j * 2], points[j * 2 + 1], 1, r, g, b, a); }
    },
  };
  let clearColor = { r: 0, g: 0, b: 0, a: 1 };
  visit.clear = (r, g, b, a) => { clearColor = { r: r / 255, g: g / 255, b: b / 255, a: 1 }; count = 0; };
  // The 3D layer: the projector (kidlisp-mesh.mjs) turns placed meshes into
  // triangles, far to near, through the same vertex buffer. The camera is in
  // canvas pixels; meshes come from the frame's table.
  let camera27 = null, meshes = null, placed = 0;
  visit.camera = (x, y, z, yaw, pitch, fov, near) => { camera27 = sceneCamera(x, y, z, yaw, pitch, fov, canvas.width, canvas.height, camera27 || new Float32Array(27), near || 1); };
  visit.place = (id, x, y, z, yaw, pitch, roll, scale, a) => {
    const mesh = meshes?.get(id); if (!mesh || !camera27) return;
    placed += projectMesh(camera27, placeMesh(mesh, x, y, z, yaw, pitch, roll, scale), (x1, y1, x2, y2, x3, y3, depth, r, g, b, alpha) => triangle(x1, y1, x2, y2, x3, y3, r, g, b, alpha), a);
  };

  return {
    device,
    // overlay: {pixels, width, height} of the CPU buffer, or null.
    // meshTable: the frame builder's mesh map, for PLACE ops.
    render(frame, overlay, meshTable) {
      configure();
      if (!configured.w) return;
      count = 0; clearColor = { r: 0, g: 0, b: 0, a: 1 }; placed = 0;
      if (meshTable) meshes = meshTable;
      readFrame(frame, visit);
      const bytes = count * FLOATS_PER_VERTEX * 4;
      if (bytes > vertexCapacity) { vertexBuffer?.destroy(); vertexCapacity = Math.max(bytes, 1 << 16); vertexBuffer = device.createBuffer({ size: vertexCapacity, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST }); }
      if (bytes) device.queue.writeBuffer(vertexBuffer, 0, vertices.buffer, 0, bytes);
      device.queue.writeBuffer(uniform, 0, new Float32Array([canvas.width, canvas.height, 0, 0]));
      let drawOverlay = false;
      if (overlay?.pixels && overlay.width && overlay.height) {
        if (!overlayTexture || overlaySize.w !== overlay.width || overlaySize.h !== overlay.height) {
          overlayTexture?.destroy();
          overlayTexture = device.createTexture({ size: [overlay.width, overlay.height], format: "rgba8unorm", usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST });
          overlayBind = device.createBindGroup({ layout: overlayPipeline.getBindGroupLayout(0), entries: [{ binding: 0, resource: sampler }, { binding: 1, resource: overlayTexture.createView() }] });
          overlaySize = { w: overlay.width, h: overlay.height };
        }
        device.queue.writeTexture({ texture: overlayTexture }, overlay.pixels, { bytesPerRow: overlay.width * 4 }, [overlay.width, overlay.height]);
        drawOverlay = true;
      }
      const encoder = device.createCommandEncoder();
      const pass = encoder.beginRenderPass({ colorAttachments: [{ view: context.getCurrentTexture().createView(), loadOp: "clear", storeOp: "store", clearValue: clearColor }] });
      if (count) { pass.setPipeline(pipeline); pass.setBindGroup(0, bindGroup); pass.setVertexBuffer(0, vertexBuffer, 0, bytes); pass.draw(count); }
      if (drawOverlay) { pass.setPipeline(overlayPipeline); pass.setBindGroup(0, overlayBind); pass.draw(6); }
      pass.end();
      device.queue.submit([encoder.finish()]);
      return count;
    },
    destroy() { vertexBuffer?.destroy(); overlayTexture?.destroy(); device.destroy?.(); },
  };
}
