import { rayKernel, renderRayFrame } from "./raytrace.mjs";

// The builder embeds the canonical .lisp kernel; this preview makes no requests.
const source = KIDLISP_RAY_SOURCE;
const canvas = document.querySelector("canvas");
const context = canvas.getContext("2d");
const backend = document.querySelector("#backend");
const resolution = document.querySelector("#resolution");
const status = document.querySelector("output");
const pause = document.querySelector("button");
let kernel = rayKernel(source, backend.value), frame = 0, running = true;
let samples = [], intervals = [], previous = 0;
backend.addEventListener("change", () => { kernel = rayKernel(source, backend.value); samples = []; intervals = []; previous = 0; });
resolution.addEventListener("change", () => { samples = []; intervals = []; previous = 0; });
pause.addEventListener("click", () => { running = !running; pause.textContent = running ? "Pause" : "Play"; });
function paint(now) {
  if (running) {
    try {
      const [width, height] = resolution.value.split("x").map(Number);
      const started = performance.now();
      const result = renderRayFrame(kernel, { width, height, frame: frame++ });
      const renderMs = performance.now() - started;
      if (canvas.width !== width || canvas.height !== height) { canvas.width = width; canvas.height = height; }
      context.putImageData(new ImageData(result.rgba, width, height), 0, 0);
      samples.push(renderMs);
      if (samples.length > 30) samples.shift();
      const median = [...samples].sort((a, b) => a - b)[Math.floor(samples.length / 2)];
      if (previous) intervals.push(now - previous);
      if (intervals.length > 30) intervals.shift();
      const fps = intervals.length ? 1000 * intervals.length / intervals.reduce((a, b) => a + b, 0) : 0;
      status.textContent = `${median.toFixed(1)} ms render · ${Math.round(fps)} fps`;
      previous = now;
      window.raytracePreview = { backend: backend.value, width, height, frame, medianMs: median, fps };
    } catch (error) { running = false; status.textContent = error.message; }
  } else { previous = 0; intervals = []; }
  requestAnimationFrame(paint);
}
requestAnimationFrame(paint);
