import { OskiewarScene3D } from './scene3d.mjs';

// A flat-face WebGPU host for the same projected triangle stream as WebGL,
// Metal and D3D. Sprites and post effects are deliberately outside this host.
const shader = `
struct Vertex { @builtin(position) position: vec4f, @location(0) color: vec3f }
@vertex fn vertex(@location(0) position: vec3f, @location(1) color: vec3f) -> Vertex {
  var out: Vertex;
  // WebGL's NDC depth [-1, 1] becomes WebGPU's [0, 1].
  out.position = vec4f(position.xy, position.z * .5 + .5, 1.);
  out.color = color;
  return out;
}
@fragment fn fragment(in: Vertex) -> @location(0) vec4f {
  return vec4f(in.color, 1.);
}`;

export class WebGPUOskiewarScene3D {
  static async create(canvas, options = {}) {
    const adapter = await navigator.gpu?.requestAdapter({ powerPreference: 'high-performance' });
    if (!adapter) throw new Error('WebGPU adapter unavailable');
    const device = await adapter.requestDevice();
    try {
      const host = new WebGPUOskiewarScene3D(canvas, device, options);
      await host.ready;
      return host;
    } catch (error) {
      device.destroy();
      throw error;
    }
  }

  constructor(canvas, device, options = {}) {
    this.canvas = canvas;
    this.device = device;
    this.scene = new OskiewarScene3D(options);
    this.context = canvas.getContext('webgpu');
    if (!this.context) throw new Error('WebGPU canvas unavailable');
    const format = navigator.gpu.getPreferredCanvasFormat();
    this.context.configure({ device, format, alphaMode: 'opaque' });
    this.buffer = device.createBuffer({
      size: this.scene.vertices.byteLength,
      usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
    });
    const module = device.createShaderModule({ code: shader });
    this.ready = device
      .createRenderPipelineAsync({
        layout: 'auto',
        vertex: {
          module,
          entryPoint: 'vertex',
          buffers: [
            {
              arrayStride: 24,
              attributes: [
                { shaderLocation: 0, offset: 0, format: 'float32x3' },
                { shaderLocation: 1, offset: 12, format: 'float32x3' },
              ],
            },
          ],
        },
        fragment: { module, entryPoint: 'fragment', targets: [{ format }] },
        primitive: { topology: 'triangle-list', cullMode: 'none' },
        depthStencil: {
          format: 'depth24plus',
          depthWriteEnabled: true,
          depthCompare: 'less-equal',
        },
      })
      .then((pipeline) => {
        this.pipeline = pipeline;
      });
    device.lost.then((info) => {
      if (info.reason !== 'destroyed') options.onDeviceLost?.(info);
    });
  }

  resize(width, height) {
    const max = this.device.limits.maxTextureDimension2D;
    width = Math.min(max, Math.max(1, Math.round(width)));
    height = Math.min(max, Math.max(1, Math.round(height)));
    if (this.depth && this.canvas.width === width && this.canvas.height === height) return;
    this.depth?.destroy();
    this.canvas.width = width;
    this.canvas.height = height;
    this.depth = this.device.createTexture({
      size: [width, height],
      format: 'depth24plus',
      usage: GPUTextureUsage.RENDER_ATTACHMENT,
    });
    this.depthView = this.depth.createView();
  }

  beginFrame() {
    this.scene.beginFrame();
  }
  triangle(...values) {
    return this.scene.triangle(...values);
  }

  present({ clear = [0, 0, 0, 1] } = {}) {
    const { device, scene } = this;
    if (!this.depth) this.resize(this.canvas.width, this.canvas.height);
    if (scene.triangleCount) device.queue.writeBuffer(this.buffer, 0, scene.frameVertices());
    const encoder = device.createCommandEncoder();
    const pass = encoder.beginRenderPass({
      colorAttachments: [
        {
          view: this.context.getCurrentTexture().createView(),
          clearValue: clear,
          loadOp: 'clear',
          storeOp: 'store',
        },
      ],
      depthStencilAttachment: {
        view: this.depthView,
        depthClearValue: 1,
        depthLoadOp: 'clear',
        depthStoreOp: 'discard',
      },
    });
    if (scene.triangleCount) {
      pass.setPipeline(this.pipeline);
      pass.setVertexBuffer(0, this.buffer);
      pass.draw(scene.triangleCount * 3);
    }
    pass.end();
    device.queue.submit([encoder.finish()]);
  }

  destroy() {
    this.depth?.destroy();
    this.buffer.destroy();
    this.context.unconfigure();
    this.device.destroy();
  }
}
