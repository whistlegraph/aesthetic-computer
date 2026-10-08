#!/usr/bin/env node
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";
const { build } = createRequire(new URL("../../system/package.json", import.meta.url))("esbuild");

const source = await readFile(new URL("../benchmarks/ray-sphere.lisp", import.meta.url), "utf8");
const result = await build({
  entryPoints: [fileURLToPath(new URL("../benchmarks/raytrace-preview.mjs", import.meta.url))],
  bundle: true, write: false, format: "iife", platform: "browser", minify: true,
  external: ["https", "url", "module"],
  define: { KIDLISP_RAY_SOURCE: JSON.stringify(source) },
});
const out = resolve(process.argv[2] || "/tmp/kidlisp-raytrace/preview.html");
await mkdir(dirname(out), { recursive: true });
await writeFile(out, `<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>KidLisp ray tracing</title>
<style>*{box-sizing:border-box}body{margin:0;background:#0b0e16;color:#f4f5fa;font:18px system-ui}main{max-width:1100px;margin:auto;padding:20px}h1{font-size:26px;margin:0 0 16px}nav{display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin:0 0 16px}select,button{font:inherit;color:inherit;background:#242b3c;border:1px solid #576178;border-radius:6px;padding:7px}label{display:flex;gap:8px;align-items:center}output{font-variant-numeric:tabular-nums}canvas{display:block;width:100%;aspect-ratio:16/9;image-rendering:pixelated;background:#111}p{font-size:15px;color:#bbc3d5}</style>
<main><h1>KidLisp ray tracing</h1><nav><label>Backend <select id="backend"><option value="wasm">Wasm kernel</option><option value="plan">Numeric plan</option><option value="direct-js">Direct JS</option><option value="reference">KidLisp interpreter</option></select></label><label>Pixels <select id="resolution"><option>64x36</option><option selected>96x54</option><option>160x90</option></select></label><button>Pause</button><output>Rendering…</output></nav><canvas aria-label="Three reflective spheres with shadows on a checker floor"></canvas><p>KidLisp ray–sphere kernel; JavaScript scene and shading. One sample per pixel, two reflection bounces.</p></main>
<script>${result.outputFiles[0].text.replaceAll("</script", "<\\/script")}</script>`);
console.log(out);
