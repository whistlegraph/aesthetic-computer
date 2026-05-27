// Smoke test for the wasm build. Run AFTER `make wasm` in pop/dsp/c/.
// Generates a 1 kHz sine, processes it through "eq:presence=+2" via the
// wasm module, and prints the RMS before/after as a sanity check.

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const C_DIR = resolve(HERE, "..");

const jsPath = resolve(C_DIR, "acdsp.js");
let factory;
try {
  ({ default: factory } = await import(jsPath));
} catch (e) {
  console.error(`acdsp.js not built. Run (cd pop/dsp/c && make wasm) first.`);
  process.exit(1);
}

const Module = await factory();

const sr = 44100, ch = 2, frames = sr;  // 1 s stereo
const buf = new Float32Array(frames * ch);
for (let i = 0; i < frames; i++) {
  const s = Math.sin(2 * Math.PI * 1000 * i / sr) * 0.5;
  buf[i * 2] = s; buf[i * 2 + 1] = s;
}

const before = rms(buf);

const bytes = buf.length * 4;
const ptr = Module._malloc(bytes);
Module.HEAPF32.set(buf, ptr / 4);

const chainStr = "1176:ratio=4:in=-3:out=+3 eq:presence=+2 eq:air=+1.5";
const specPtr = allocCString(Module, chainStr);
const rc = Module.ccall("acdsp_process", "number",
  ["number", "number", "number", "number", "number"],
  [ptr, frames, sr, ch, specPtr]);
if (rc !== 0) { console.error("acdsp_process failed:", rc); process.exit(1); }

const out = new Float32Array(Module.HEAPF32.buffer, ptr, frames * ch).slice();
Module._free(ptr); Module._free(specPtr);

const after = rms(out);
console.log(`wasm smoke: in RMS=${before.toFixed(4)}, out RMS=${after.toFixed(4)}`);
if (!Number.isFinite(after) || after === 0) {
  console.error("✗ wasm output is silent or NaN"); process.exit(1);
}
console.log("✓ wasm smoke ok");

function rms(a) {
  let s = 0; for (let i = 0; i < a.length; i++) s += a[i] * a[i];
  return Math.sqrt(s / a.length);
}
function allocCString(M, s) {
  const bytes = new TextEncoder().encode(s + "\0");
  const p = M._malloc(bytes.length);
  M.HEAPU8.set(bytes, p);
  return p;
}
