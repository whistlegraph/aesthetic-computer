import { rayKernel, renderRayFrame } from "./raytrace.mjs";
import { createRayWasm } from "./raytrace-wasm.mjs";
import { createRayGPU, compareRayGPU } from "./raytrace-gpu.mjs";

// Source, shader and verified Wasm artifact are embedded; no data requests.
const source = KIDLISP_RAY_SOURCE;
const canvas = document.querySelector("#cpu"), gpuCanvas = document.querySelector("#gpu");
const context = canvas.getContext("2d");
const backend = document.querySelector("#backend"), resolution = document.querySelector("#resolution");
const status = document.querySelector("output"), detail = document.querySelector("#detail"), pause = document.querySelector("button");
const kernels = new Map();
const kernel = name => { if (!kernels.has(name)) kernels.set(name, rayKernel(source, name)); return kernels.get(name); };
const wasm = createRayWasm(Uint8Array.from(atob(KIDLISP_RAY_WASM), c => c.charCodeAt(0)));
let gpu, frame = 0, running = true, busy = false, disposed = false, notice = "";
let samples = [], gpuSamples = [], intervals = [], previous = 0;
const resetStats = () => { samples = []; gpuSamples = []; intervals = []; previous = 0; };
const median = values => [...values].sort((a, b) => a - b)[Math.floor(values.length / 2)];
const add = (values, value) => { values.push(value); if (values.length > 30) values.shift(); };
const fallback = error => {
  notice = error.message.replace(/\.+$/, "");
  gpu?.dispose(); gpu = null;
  backend.querySelector('[value="webgpu"]').disabled = true;
  backend.value = "wasm-frame"; resetStats();
};
backend.addEventListener("change", resetStats);
resolution.addEventListener("change", resetStats);
pause.addEventListener("click", () => { running = !running; pause.textContent = running ? "Pause" : "Play"; resetStats(); });

async function render(name, controls, options) {
  if (name === "webgpu") {
    if (!gpu) throw new Error("WebGPU is unavailable");
    return gpu.render(controls, options);
  }
  const start = performance.now();
  const result = name === "wasm-frame" ? wasm.render(controls) : renderRayFrame(kernel(name), controls);
  return { ...result, completedMs: performance.now() - start, gpuMs: null };
}
async function paint(now) {
  if (disposed) return;
  if (running && !busy) {
    busy = true;
    const selected = backend.value;
    try {
      const [width, height] = resolution.value.split("x").map(Number);
      const result = await render(selected, { width, height, frame: frame++ });
      canvas.hidden = selected === "webgpu"; gpuCanvas.hidden = !canvas.hidden;
      if (result.rgba) {
        if (canvas.width !== width || canvas.height !== height) { canvas.width = width; canvas.height = height; }
        context.putImageData(new ImageData(result.rgba, width, height), 0, 0);
      }
      if (selected === backend.value) {
        add(samples, result.completedMs);
        if (result.gpuMs !== null) add(gpuSamples, result.gpuMs);
        if (previous) add(intervals, now - previous);
        const fps = intervals.length ? 1000 * intervals.length / intervals.reduce((a, b) => a + b, 0) : 0;
        const gpuTime = gpuSamples.length ? median(gpuSamples) : null;
        status.textContent = `${Math.round(fps)} fps · ${median(samples).toFixed(2)} ms ${selected === "webgpu" ? "to completion" : "CPU"}${gpuTime > 0 ? ` · ${gpuTime.toFixed(3)} ms GPU` : ""}`;
        detail.textContent = `${notice ? notice + ". " : ""}One sample per pixel · two reflections · ${selected === "webgpu" ? "f32 GPU math" : "f64 CPU math"}`;
        previous = now;
        window.raytracePreview = { backend: selected, width, height, frame, medianMs: median(samples), gpuMs: gpuSamples.length ? median(gpuSamples) : null, fps, notice };
      }
    } catch (error) {
      if (selected === "webgpu") fallback(error);
      else { running = false; pause.textContent = "Play"; status.textContent = error.message; }
    } finally { busy = false; }
  }
  requestAnimationFrame(paint);
}
async function boot() {
  try { gpu = await createRayGPU(kernel("direct-js").plan, KIDLISP_RAY_SHADER, gpuCanvas); }
  catch (error) { fallback(error); }
  if (disposed) { gpu?.dispose(); return; }
  // Explicit testing interface; pause and drain before comparing frames.
  window.raytraceHarness = { render, compareRayGPU, plan: kernel("direct-js").plan, gpuInfo: gpu?.info,
    async pause() { running = false; pause.textContent = "Play"; while (busy) await new Promise(resolve => setTimeout(resolve, 10)); resetStats(); },
  };
  requestAnimationFrame(paint);
}
window.addEventListener("pagehide", () => { disposed = true; gpu?.dispose(); });
boot();
