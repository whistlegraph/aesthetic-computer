#!/usr/bin/env node
// Measure the whole CPU frame, including JS traversal and Wasm crossings.
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { performance } from "node:perf_hooks";
import { cpus, platform, arch } from "node:os";
import { resolve, join } from "node:path";
import sharp from "sharp";
import { rayKernel, renderRayFrame } from "../benchmarks/raytrace.mjs";
import { createRayWasm } from "../benchmarks/raytrace-wasm.mjs";
import { hashPixels, comparePixelFrames } from "../conformance/pixels.mjs";

const source = await readFile(new URL("../benchmarks/ray-sphere.lisp", import.meta.url), "utf8");
const frameWasm = await readFile(new URL("../benchmarks/raytrace-frame.wasm", import.meta.url));
const out = resolve(process.argv[2] || "/tmp/kidlisp-raytrace");
await mkdir(out, { recursive: true });
const results = [];
const quantile = (samples, q) => [...samples].sort((a, b) => a - b)[Math.ceil(q * samples.length) - 1];
for (const [width, height] of [[64, 36], [96, 54], [160, 90]]) {
  const expected = [];
  for (const backend of ["direct-js", "reference", "plan", "wasm", "wasm-frame"]) {
    const start = performance.now();
    const kernel = backend === "wasm-frame" ? createRayWasm(frameWasm) : rayKernel(source, backend);
    const render = controls => backend === "wasm-frame" ? kernel.render(controls) : renderRayFrame(kernel, controls);
    const prepareMs = performance.now() - start;
    for (let i = 0; i < 3; i++) render({ width, height, frame: i });
    const samples = [];
    let rendered;
    // Reference interpretation is intentionally measured too, with the same
    // work and sample count. Direct JS prevents a misleading speedup baseline.
    for (let frame = 0; frame < 12; frame++) {
      const t = performance.now();
      rendered = render({ width, height, frame });
      samples.push(performance.now() - t);
      if (backend === "direct-js") expected.push(rendered.rgba);
      else {
        const parity = comparePixelFrames(expected[frame], rendered.rgba, width, height);
        if (!parity.equal) throw new Error(`${backend} changed ${parity.changedPixels} pixels at ${width}x${height}, frame ${frame}`);
      }
    }
    const medianMs = quantile(samples, 0.5), p95Ms = quantile(samples, 0.95);
    const row = { width, height, backend, prepareMs, medianMs, p95Ms, samples, wasmBytes: kernel.bytes, rays: rendered.rays, intersections: rendered.intersections, pixelSha256: hashPixels(rendered.rgba) };
    results.push(row);
    console.log(`${width}x${height} ${backend}: median ${medianMs.toFixed(2)}ms; p95 ${p95Ms.toFixed(2)}ms; exact pixels`);
    if (backend === "wasm") await sharp(Buffer.from(rendered.rgba), { raw: { width, height, channels: 4 } }).png().toFile(join(out, `raytrace-${width}x${height}.png`));
  }
}
await writeFile(join(out, "report.json"), JSON.stringify({ version: 2, scene: "three spheres, checker floor, hard shadows, two reflection bounces, one sample per pixel", kernel: source, scope: "KidLisp ray/sphere discriminant; JS host except wasm-frame which compiles the C host too; no display cost", host: { node: process.version, platform: platform(), arch: arch(), cpu: cpus()[0]?.model }, results }, null, 2) + "\n");
