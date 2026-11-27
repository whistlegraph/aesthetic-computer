// BIOS routes only prompt:webgpu here. Other pieces retain their glaze type.
import * as legacy from "./glaze.mjs";

let request, gpu, pending, failed = false;
function fallback(error) {
  if (failed || !request) return;
  console.warn("Prompt WebGPU glaze switched to WebGL", error);
  failed = true;
  gpu?.dispose();
  gpu = null;
  const {args, ready} = request;
  return legacy.on(...args, "prompt", ready);
}
export async function on(w, h, rect, width, height, wrapper, type, ready) {
  const args = [w, h, rect, width, height, wrapper];
  if (type !== "prompt-webgpu") {
    if (request) off();
    return legacy.on(...args, type, ready);
  }
  const next = request = {args, ready};
  if (failed) return legacy.on(...args, "prompt", ready);
  if (!pending) {
    legacy.off();
    const old = legacy.getCan();
    if (old) old.style.opacity = "0";
    const job = import("./prompt-glaze-gpu.mjs").then(({createPromptGlaze}) =>
      createPromptGlaze(error => { if (pending === job) fallback(error); }));
    pending = job;
  }
  const job = pending;
  try {
    const renderer = await job;
    if (pending !== job || failed) { renderer.dispose(); return; }
    if (request !== next) return;
    gpu = renderer;
    gpu.hud(hudBox);
    gpu.resize(...args);
    ready();
  } catch (error) {
    if (request === next) return fallback(error);
  }
}
export function off() {
  request = null;
  pending = null;
  gpu?.dispose();
  gpu = null;
  failed = false;
  legacy.off();
}
export function getCan() { return gpu?.canvas || legacy.getCan(); }
export function update(...args) {
  if (gpu) { try { gpu.update(...args); } catch (error) { fallback(error); } }
  else if (!pending || failed) legacy.update(...args);
}
export function render(...args) {
  if (gpu) { try { gpu.render(...args); } catch (error) { fallback(error); } }
  else if (!pending || failed) legacy.render(...args);
}
// The fps HUD's box in the image, as uv [x, y, w, h], or null: the shader
// leaves it out of the screen simulation and wraps it in plastic.
let hudBox = [0, 0, 0, 0];
export function hud(box) {
  hudBox = box || [0, 0, 0, 0];
  gpu?.hud(hudBox);
  legacy.hud(hudBox);
}
export function clear() { if (gpu) gpu.clear(); else legacy.clear(); }
export function unfreeze() { if (gpu) gpu.unfreeze(); else if (!pending || failed) legacy.unfreeze(); }
export function freeze(ctx) { return gpu ? gpu.freeze(ctx) : legacy.freeze(ctx); }
