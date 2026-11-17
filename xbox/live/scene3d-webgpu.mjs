import { OskiewarScene3D } from './scene3d.mjs';

// A flat-face WebGPU host for the same projected triangle stream as WebGL,
// Metal and D3D. Sprites and post effects are deliberately outside this host.
//
// 🌑 Stencil shadows are opt-in (`options.shadows`); without it nothing here
// differs from before. With it the depth buffer carries a stencil, and
// `present({ shadow: { first, count, shade } })` treats `count` triangles
// from `first` to the end of the stream as closed shadow volumes instead of
// drawing them: it counts them where they fall behind what is on screen
// (depth-fail, so an eye inside a shadow reads right) and multiplies the
// pixels inside by `shade`. Depths at or nearer than SHADOW_NEAR are left
// alone, so a HUD drawn at the front of the depth range is never darkened.
const SHADOW_NEAR = 0.002;
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
    this.shadows = !!options.shadows;
    this.depthFormat = this.shadows ? 'depth24plus-stencil8' : 'depth24plus';
    const vertex = {
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
    };
    const primitive = { topology: 'triangle-list', cullMode: 'none' };
    const pipelines = [
      device.createRenderPipelineAsync({
        layout: 'auto',
        vertex,
        fragment: { module, entryPoint: 'fragment', targets: [{ format }] },
        primitive,
        depthStencil: {
          format: this.depthFormat,
          depthWriteEnabled: true,
          depthCompare: 'less-equal',
        },
      }),
    ];
    if (this.shadows) {
      // Volumes: no colour, no depth written; a face behind the scene counts
      // up or down by which way it faces. Wrapping makes the two directions
      // interchangeable, so the stream's handedness does not matter.
      const count = (depthFailOp) => ({ compare: 'always', failOp: 'keep', passOp: 'keep', depthFailOp });
      pipelines.push(device.createRenderPipelineAsync({
        layout: 'auto',
        vertex,
        fragment: { module, entryPoint: 'fragment', targets: [{ format, writeMask: 0 }] },
        primitive,
        depthStencil: {
          format: this.depthFormat,
          depthWriteEnabled: false,
          depthCompare: 'less',
          stencilFront: count('decrement-wrap'),
          stencilBack: count('increment-wrap'),
        },
      }));
      // The shade: one triangle over the screen, multiplying what is there
      // wherever the count is not zero and the scene is behind the HUD's depths.
      const inside = { compare: 'not-equal', failOp: 'keep', passOp: 'keep', depthFailOp: 'keep' };
      pipelines.push(device.createRenderPipelineAsync({
        layout: 'auto',
        vertex,
        fragment: { module, entryPoint: 'fragment', targets: [{ format, blend: {
          color: { srcFactor: 'zero', dstFactor: 'constant', operation: 'add' },
          alpha: { srcFactor: 'zero', dstFactor: 'one', operation: 'add' },
        } }] },
        primitive,
        depthStencil: {
          format: this.depthFormat,
          depthWriteEnabled: false,
          depthCompare: 'less',
          stencilFront: inside,
          stencilBack: inside,
          stencilWriteMask: 0,
        },
      }));
      // The shader halves depth into [0.5, 1]; SHADOW_NEAR is given in the stream's own units.
      this.cover = device.createBuffer({ size: 72, usage: GPUBufferUsage.VERTEX, mappedAtCreation: true });
      new Float32Array(this.cover.getMappedRange()).set([-1, -1, SHADOW_NEAR, 0, 0, 0, 3, -1, SHADOW_NEAR, 0, 0, 0, -1, 3, SHADOW_NEAR, 0, 0, 0]);
      this.cover.unmap();
    }
    this.ready = Promise.all(pipelines).then(([pipeline, volumes, shade]) => {
      this.pipeline = pipeline;
      this.volumes = volumes;
      this.shade = shade;
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
      format: this.depthFormat,
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

  present({ clear = [0, 0, 0, 1], shadow = null } = {}) {
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
        ...(this.shadows ? { stencilClearValue: 0, stencilLoadOp: 'clear', stencilStoreOp: 'discard' } : {}),
      },
    });
    // With shadows asked for, the stream's tail is volumes, not picture.
    const cast = this.shadows && shadow?.count > 0 && shadow.first + shadow.count === scene.triangleCount ? shadow : null;
    const drawn = cast ? cast.first : scene.triangleCount;
    if (drawn) {
      pass.setPipeline(this.pipeline);
      pass.setVertexBuffer(0, this.buffer);
      pass.draw(drawn * 3);
    }
    if (cast) {
      pass.setPipeline(this.volumes);
      pass.setVertexBuffer(0, this.buffer);
      pass.setStencilReference(0);
      pass.draw(cast.count * 3, 1, cast.first * 3);
      pass.setPipeline(this.shade);
      pass.setVertexBuffer(0, this.cover);
      pass.setBlendConstant([...(cast.shade || [0.6, 0.6, 0.6]), 1]);
      pass.draw(3);
    }
    pass.end();
    device.queue.submit([encoder.finish()]);
  }

  destroy() {
    this.depth?.destroy();
    this.buffer.destroy();
    this.cover?.destroy();
    this.context.unconfigure();
    this.device.destroy();
  }
}
