import { emitRayPlan } from "./ray-plan.mjs";
import { rayControls } from "./raytrace.mjs";

// One compiled pipeline, persistent uniform/output buffers, one invocation per
// pixel. Image readback is diagnostic only; presentation stays entirely on GPU.
export async function createRayGPU(plan, shaderHost, canvas, gpu = globalThis.navigator?.gpu) {
  if (!gpu) throw new Error("WebGPU is unavailable");
  const adapter = await gpu.requestAdapter();
  if (!adapter) throw new Error("No WebGPU adapter is available");
  const timestamps = adapter.features.has("timestamp-query");
  const device = await adapter.requestDevice({ requiredFeatures: timestamps ? ["timestamp-query"] : [] });
  let failed = null, disposed = false, busy = false;
  device.addEventListener("uncapturederror", event => { failed = event.error.message; });
  device.lost.then(info => { if (!disposed) failed = `WebGPU device lost: ${info.message || info.reason}`; });
  const context = canvas?.getContext("webgpu");
  if (canvas && !context) { device.destroy(); throw new Error("WebGPU canvas is unavailable"); }
  let uniforms, querySet, queryResolve, queryRead, texture;
  try {
    const module = device.createShaderModule({ code: `${emitRayPlan(plan, "wgsl")}\n${shaderHost}` });
    const errors = (await module.getCompilationInfo()).messages.filter(m => m.type === "error");
    if (errors.length) throw new Error(errors.map(m => `${m.lineNum}: ${m.message}`).join("\n"));
    const pipeline = await device.createComputePipelineAsync({ layout: "auto", compute: { module, entryPoint: "main" } });
    uniforms = device.createBuffer({ size: 32, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
    const format = gpu.getPreferredCanvasFormat();
    let presentation;
    if (context) {
      context.configure({ device, format, alphaMode: "opaque" });
      const blit = device.createShaderModule({ code: `
        @group(0) @binding(0) var image: texture_2d<f32>;
        @vertex fn vertex(@builtin(vertex_index) i: u32) -> @builtin(position) vec4f {
          let points = array<vec2f,3>(vec2f(-1,-1),vec2f(3,-1),vec2f(-1,3));
          return vec4f(points[i],0,1);
        }
        @fragment fn fragment(@builtin(position) p: vec4f) -> @location(0) vec4f {
          return textureLoad(image, vec2i(p.xy), 0);
        }` });
      presentation = await device.createRenderPipelineAsync({ layout: "auto", vertex: { module: blit, entryPoint: "vertex" }, fragment: { module: blit, entryPoint: "fragment", targets: [{ format }] } });
    }
    if (timestamps) {
      querySet = device.createQuerySet({ type: "timestamp", count: 2 });
      queryResolve = device.createBuffer({ size: 16, usage: GPUBufferUsage.QUERY_RESOLVE | GPUBufferUsage.COPY_SRC });
      queryRead = device.createBuffer({ size: 16, usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ });
    }
    let size = "", group, presentGroup;
    const data = new ArrayBuffer(32), uints = new Uint32Array(data), floats = new Float32Array(data);
    const info = adapter.info;
    return {
      info: { vendor: info?.vendor, architecture: info?.architecture, device: info?.device, description: info?.description, isFallbackAdapter: info?.isFallbackAdapter, timestamps },
      async render(controls = {}, { readback = false, timing = true } = {}) {
        if (disposed || failed) throw new Error(failed || "WebGPU renderer disposed");
        if (busy) throw new Error("WebGPU renderer already has a frame in flight");
        const { width, height, frame, bounces, greenX } = rayControls(controls);
        busy = true;
        let pixels;
        try {
          if (size !== `${width}x${height}`) {
            texture?.destroy();
            texture = device.createTexture({ size: [width, height], format: "rgba8unorm", usage: GPUTextureUsage.STORAGE_BINDING | GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_SRC });
            group = device.createBindGroup({ layout: pipeline.getBindGroupLayout(0), entries: [{ binding: 0, resource: { buffer: uniforms } }, { binding: 1, resource: texture.createView() }] });
            if (context) {
              canvas.width = width; canvas.height = height;
              presentGroup = device.createBindGroup({ layout: presentation.getBindGroupLayout(0), entries: [{ binding: 0, resource: texture.createView() }] });
            }
            size = `${width}x${height}`;
          }
          const start = performance.now();
          uints[0] = width; uints[1] = height; uints[2] = bounces; floats[4] = greenX;
          device.queue.writeBuffer(uniforms, 0, data);
          const encoder = device.createCommandEncoder();
          const measured = timing && timestamps;
          const pass = encoder.beginComputePass(measured ? { timestampWrites: { querySet, beginningOfPassWriteIndex: 0, endOfPassWriteIndex: 1 } } : {});
          pass.setPipeline(pipeline); pass.setBindGroup(0, group);
          pass.dispatchWorkgroups(Math.ceil(width / 8), Math.ceil(height / 8)); pass.end();
          if (context) {
            const screen = encoder.beginRenderPass({ colorAttachments: [{ view: context.getCurrentTexture().createView(), loadOp: "clear", storeOp: "store", clearValue: { r: 0, g: 0, b: 0, a: 1 } }] });
            screen.setPipeline(presentation); screen.setBindGroup(0, presentGroup); screen.draw(3); screen.end();
          }
          if (measured) { encoder.resolveQuerySet(querySet, 0, 2, queryResolve, 0); encoder.copyBufferToBuffer(queryResolve, 0, queryRead, 0, 16); }
          const bytesPerRow = Math.ceil(width * 4 / 256) * 256;
          if (readback) {
            pixels = device.createBuffer({ size: bytesPerRow * height, usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ });
            encoder.copyTextureToBuffer({ texture }, { buffer: pixels, bytesPerRow }, [width, height]);
          }
          device.queue.submit([encoder.finish()]);
          const submitMs = performance.now() - start;
          await device.queue.onSubmittedWorkDone();
          const completedMs = performance.now() - start;
          if (failed) throw new Error(failed);
          let gpuMs = null, rgba;
          if (measured) {
            await queryRead.mapAsync(GPUMapMode.READ);
            const stamps = new BigUint64Array(queryRead.getMappedRange());
            gpuMs = Number(stamps[1] - stamps[0]) / 1e6; queryRead.unmap();
          }
          if (pixels) {
            await pixels.mapAsync(GPUMapMode.READ);
            const mapped = new Uint8Array(pixels.getMappedRange());
            rgba = new Uint8ClampedArray(width * height * 4);
            for (let y = 0; y < height; y++) rgba.set(mapped.subarray(y * bytesPerRow, y * bytesPerRow + width * 4), y * width * 4);
            pixels.unmap();
          }
          return { width, height, frame, rgba, submitMs, completedMs, gpuMs };
        } finally { pixels?.destroy(); busy = false; }
      },
      dispose() { disposed = true; texture?.destroy(); uniforms.destroy(); querySet?.destroy(); queryResolve?.destroy(); queryRead?.destroy(); context?.unconfigure(); device.destroy(); },
    };
  } catch (error) {
    uniforms?.destroy(); querySet?.destroy(); queryResolve?.destroy(); queryRead?.destroy(); context?.unconfigure(); device.destroy(); throw error;
  }
}

// f32 GPU math may differ around hit boundaries. Report all differences and
// gate both total error and outliers; never relabel this as exact conformance.
export function compareRayGPU(expected, actual) {
  if (expected.length !== actual.length || !expected.length || expected.length % 4) throw new TypeError("Expected equal RGBA buffers");
  let absolute = 0, changedPixels = 0, outliers = 0, maxError = 0, alphaErrors = 0;
  for (let i = 0; i < expected.length; i += 4) {
    let changed = false, outlier = false;
    if (actual[i + 3] !== expected[i + 3]) alphaErrors++;
    for (let c = 0; c < 3; c++) {
      const error = Math.abs(expected[i + c] - actual[i + c]);
      absolute += error; maxError = Math.max(error, maxError); changed ||= error !== 0; outlier ||= error > 2;
    }
    if (changed) changedPixels++; if (outlier) outliers++;
  }
  const pixels = expected.length / 4, meanError = absolute / (pixels * 3), outlierFraction = outliers / pixels;
  return { pass: alphaErrors === 0 && meanError <= 0.5 && outlierFraction <= 0.002, meanError, maxError, changedPixels, outliers, outlierFraction, alphaErrors };
}
