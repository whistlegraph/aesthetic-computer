// The paper's source is the worker's transparent pixel canvas. Keep it on the
// GPU between updates so lighting can animate without uploading another frame.
import uniforms from "./glazes/uniforms.js";

export async function createPromptGlaze(onFailure) {
  const adapter = await navigator.gpu?.requestAdapter();
  if (!adapter) throw new Error("WebGPU adapter unavailable");
  const response = await fetch(new URL("./glazes/prompt/prompt.wgsl", import.meta.url));
  if (!response.ok) throw new Error("Prompt glaze shader unavailable");
  const code = await response.text();
  const device = await adapter.requestDevice();
  const canvas = document.createElement("canvas");
  canvas.dataset.type = "glaze";
  canvas.dataset.backend = "webgpu";
  canvas.style.opacity = "0";
  const context = canvas.getContext("webgpu");
  const format = navigator.gpu.getPreferredCanvasFormat();
  let disposed = false, frameBuffer, source, group, pipeline;
  function dispose() {
    if (disposed) return;
    disposed = true;
    canvas.remove();
    source?.destroy();
    frameBuffer?.destroy();
    context?.unconfigure();
    device.destroy();
  }
  device.lost.then(info => { if (!disposed) onFailure(new Error(info.message || "WebGPU device lost")); });
  device.addEventListener("uncapturederror", e => { if (!disposed) onFailure(e.error); });
  try {
    const constants = {};
    for (const [key, value] of Object.entries(uniforms.prompt)) {
      const [, name] = key.split(":");
      if (name === "focalLength" || name === "hud") continue; // Unused here; the HUD's box is in the frame buffer.
      if (Array.isArray(value)) value.forEach((n, i) => { constants[name + "XYZ"[i]] = n; });
      else constants[name] = value;
    }
    const module = device.createShaderModule({code});
    pipeline = await device.createRenderPipelineAsync({
      layout: "auto", vertex: {module, entryPoint: "vertex"},
      fragment: {module, entryPoint: "fragment", constants, targets: [{format}]},
    });
    context.configure({device, format, alphaMode: "premultiplied"});
    frameBuffer = device.createBuffer({size: 32, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST});
  } catch (error) {
    dispose();
    throw error;
  }
  const frame = new Float32Array(8);
  let hudBox = [0, 0, 0, 0];
  let uploaded = false, lastTime = 0, lastDay = false;
  function resize(w, h, rect, nativeWidth, nativeHeight, wrapper) {
    // Match the WebGL light pass budget, including on high-density displays.
    const scale = Math.min(1, 1920 / Math.max(1, nativeWidth, nativeHeight));
    const width = Math.max(1, Math.round(nativeWidth * scale)), height = Math.max(1, Math.round(nativeHeight * scale));
    if (canvas.width !== width) canvas.width = width;
    if (canvas.height !== height) canvas.height = height;
    canvas.style.width = rect.width + "px";
    canvas.style.height = rect.height + "px";
    canvas.style.imageRendering = "auto";
    if (!canvas.isConnected) wrapper.insertBefore(canvas, wrapper.querySelector('canvas[data-type="glaze"]'));
    if (source?.width === w && source?.height === h) return;
    source?.destroy();
    uploaded = false;
    source = device.createTexture({size: [w,h], format: "rgba8unorm",
      usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST | GPUTextureUsage.RENDER_ATTACHMENT});
    group = device.createBindGroup({layout: pipeline.getBindGroupLayout(0),
      entries: [{binding: 0, resource: source.createView()}, {binding: 1, resource: {buffer: frameBuffer}}]});
  }
  function update(input, x = 0, y = 0) {
    if (!source) return;
    device.queue.copyExternalImageToTexture({source: input}, {texture: source, origin: [x,y], premultipliedAlpha: false}, [input.width,input.height]);
    uploaded = true;
  }
  function render(time, mouse, day = false) {
    if (!uploaded) return;
    lastTime = time;
    lastDay = day;
    frame.set([canvas.width, canvas.height, time, Number(day), ...hudBox]);
    device.queue.writeBuffer(frameBuffer, 0, frame);
    const commands = device.createCommandEncoder();
    const pass = commands.beginRenderPass({colorAttachments: [{view: context.getCurrentTexture().createView(),
      clearValue: [0,0,0,0], loadOp: "clear", storeOp: "store"}]});
    pass.setPipeline(pipeline);
    pass.setBindGroup(0, group);
    pass.draw(3);
    pass.end();
    device.queue.submit([commands.finish()]);
  }
  function freeze(ctx) {
    if (!uploaded || !ctx?.canvas) return false;
    render(lastTime, null, lastDay);
    ctx.drawImage(canvas, 0, 0, ctx.canvas.width, ctx.canvas.height);
    canvas.style.opacity = "0";
    return true;
  }
  return {canvas, resize, update, render, freeze, dispose,
    hud(box) { hudBox = box; },
    clear() { uploaded = false; canvas.style.opacity = "0"; },
    unfreeze() { if (uploaded) canvas.style.removeProperty("opacity"); }};
}
