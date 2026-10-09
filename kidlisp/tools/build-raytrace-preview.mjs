#!/usr/bin/env node
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";
import { createHash } from "node:crypto";
import { rayKernel } from "../benchmarks/raytrace.mjs";
import { emitRayPlan } from "../benchmarks/ray-plan.mjs";
const { build } = createRequire(new URL("../../system/package.json", import.meta.url))("esbuild");

const source = await readFile(new URL("../benchmarks/ray-sphere.lisp", import.meta.url), "utf8");
const wasm = await readFile(new URL("../benchmarks/raytrace-frame.wasm", import.meta.url));
const manifest = JSON.parse(await readFile(new URL("../benchmarks/raytrace-frame.wasm.json", import.meta.url), "utf8"));
const shader = await readFile(new URL("../benchmarks/raytrace.wgsl", import.meta.url), "utf8");
const plan = rayKernel(source).plan;
const hash = value => createHash("sha256").update(value).digest("hex");
if (hash(wasm) !== manifest.sha256 || hash(source) !== manifest.sourceSha256 || hash(await readFile(new URL("../benchmarks/raytrace-frame.c", import.meta.url))) !== manifest.hostSha256 || hash(emitRayPlan(plan, "c")) !== manifest.kernelSha256) throw new Error("Ray Wasm is stale; run build-raytrace-wasm.mjs");
const result = await build({
  entryPoints: [fileURLToPath(new URL("../benchmarks/raytrace-preview.mjs", import.meta.url))],
  bundle: true, write: false, format: "iife", platform: "browser", minify: true,
  external: ["https", "url", "module"],
  define: { KIDLISP_RAY_SOURCE: JSON.stringify(source), KIDLISP_RAY_WASM: JSON.stringify(wasm.toString("base64")), KIDLISP_RAY_SHADER: JSON.stringify(shader) },
});
const out = resolve(process.argv[2] || "/tmp/kidlisp-raytrace/preview.html");
await mkdir(dirname(out), { recursive: true });
await writeFile(resolve(dirname(out), "ray-plan.json"), JSON.stringify(plan, null, 2) + "\n");
await writeFile(resolve(dirname(out), "ray-kernel.wgsl"), emitRayPlan(plan, "wgsl") + "\n");
await writeFile(resolve(dirname(out), "raytrace.wgsl"), emitRayPlan(plan, "wgsl") + "\n" + shader);
await writeFile(out, `<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>KidLisp ray tracing</title>
<style>*{box-sizing:border-box}body{margin:0;background:#0b0e16;color:#f4f5fa;font:18px system-ui}main{max-width:1100px;margin:auto;padding:20px}h1{font-size:26px;margin:0 0 16px}nav{display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin:0 0 16px}select,button{font:inherit;color:inherit;background:#242b3c;border:1px solid #576178;border-radius:6px;padding:7px}label{display:flex;gap:8px;align-items:center}output{font-variant-numeric:tabular-nums}canvas{display:block;width:100%;aspect-ratio:16/9;image-rendering:pixelated;background:#111}canvas[hidden]{display:none}p{font-size:15px;color:#bbc3d5}</style>
<main><h1>KidLisp ray tracing</h1><nav><label>Backend <select id="backend"><option value="webgpu">WebGPU</option><option value="wasm-frame">Wasm frame</option><option value="direct-js">Direct JS</option><option value="wasm">Wasm kernel</option><option value="plan">Numeric plan</option><option value="reference">KidLisp interpreter</option></select></label><label>Pixels <select id="resolution"><option>64x36</option><option>96x54</option><option>160x90</option><option selected>320x180</option><option>480x270</option></select></label><button>Pause</button><output aria-live="off">Preparing…</output></nav><canvas id="gpu" aria-label="Ray-traced reflective spheres"></canvas><canvas id="cpu" hidden aria-label="Ray-traced reflective spheres"></canvas><p id="detail"></p></main>
<script>${result.outputFiles[0].text.replaceAll("</script", "<\\/script")}</script>`);
console.log(out);
