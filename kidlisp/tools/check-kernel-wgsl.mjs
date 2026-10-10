#!/usr/bin/env node
// Validate and run a piece's kernels as WGSL on a real WebGPU device, in
// headless Chrome, against the JavaScript runner. The GPU is f32, so the
// check is tolerant (relative 1e-5); the Wasm backend is checked bit for
// bit in tests/kidlisp-kernel.test.mjs.
//
//   node kidlisp/tools/check-kernel-wgsl.mjs kidlisp/examples/whistlegraph/fia.lisp
import puppeteer from 'puppeteer';
import {readFileSync} from 'node:fs';
import {KidLisp} from '../../system/public/aesthetic.computer/lib/kidlisp.mjs';
import {compileKernel, runKernel, emitKernelWGSL} from '../../system/public/aesthetic.computer/lib/kidlisp-kernel.mjs';

const file = process.argv[2];
if (!file) { console.error('usage: check-kernel-wgsl.mjs piece.lisp'); process.exit(1); }
const forms = new KidLisp().parse(readFileSync(file, 'utf8')).filter((f) => Array.isArray(f) && f[0] === 'kernel');
if (!forms.length) { console.log('no kernels in', file); process.exit(0); }
const CHROME = process.env.CHROME_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const browser = await puppeteer.launch({headless: true, executablePath: CHROME, args: ['--no-sandbox', '--enable-unsafe-webgpu', '--enable-features=Vulkan,WebGPU', '--use-angle=metal']});
const page = await browser.newPage();
await page.goto('https://aesthetic.computer/wgtv', {waitUntil: 'domcontentloaded'});
let failed = 0;
try {
  for (const form of forms) {
    const plan = compileKernel(form);
    const wgsl = emitKernelWGSL(plan);
    const nIn = plan.inputs.length, nUni = plan.uniforms.length, nOut = plan.outputs.length, stride = nIn + nUni + nOut, n = 8;
    // deterministic probe rows and uniforms
    const rows = new Float64Array(n * stride);
    const uniforms = plan.uniforms.map((_, i) => 0.5 + (i % 5) * 0.37);
    for (let r = 0; r < n; r++) { for (let i = 0; i < nIn; i++) rows[r * stride + i] = (r + 1) * (i + 2) * 0.731 - 3; for (let i = 0; i < nUni; i++) rows[r * stride + nIn + i] = uniforms[i]; }
    const expected = rows.slice(); runKernel(plan, expected, n);
    const result = await page.evaluate(async (wgsl, rowsIn, uniforms, stride, n) => {
      if (!navigator.gpu) return {error: 'no navigator.gpu'};
      const adapter = await navigator.gpu.requestAdapter(); if (!adapter) return {error: 'no adapter'};
      const device = await adapter.requestDevice();
      const module = device.createShaderModule({code: wgsl});
      const messages = (await module.getCompilationInfo()).messages.map((m) => `${m.type} ${m.lineNum}:${m.linePos} ${m.message}`);
      if (messages.some((m) => m.startsWith('error'))) return {messages};
      const rows = new Float32Array(rowsIn);
      const rowBuf = device.createBuffer({size: rows.byteLength, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST});
      device.queue.writeBuffer(rowBuf, 0, rows);
      const uniArr = new Float32Array(Math.max(4, Math.ceil(uniforms.length / 4) * 4)); uniArr.set(uniforms);
      const uniBuf = device.createBuffer({size: uniArr.byteLength, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST});
      device.queue.writeBuffer(uniBuf, 0, uniArr);
      const pipeline = device.createComputePipeline({layout: 'auto', compute: {module, entryPoint: 'main'}});
      const bind = device.createBindGroup({layout: pipeline.getBindGroupLayout(0), entries: [{binding: 0, resource: {buffer: rowBuf}}, {binding: 1, resource: {buffer: uniBuf}}]});
      const read = device.createBuffer({size: rows.byteLength, usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST});
      const enc = device.createCommandEncoder(); const pass = enc.beginComputePass(); pass.setPipeline(pipeline); pass.setBindGroup(0, bind); pass.dispatchWorkgroups(Math.ceil(n / 64)); pass.end();
      enc.copyBufferToBuffer(rowBuf, 0, read, 0, rows.byteLength); device.queue.submit([enc.finish()]);
      await read.mapAsync(GPUMapMode.READ);
      return {messages, adapter: adapter.info?.vendor || '?', out: Array.from(new Float32Array(read.getMappedRange().slice(0)))};
    }, wgsl, Array.from(rows), uniforms, stride, n);
    if (result.error || !result.out) { console.log(`${plan.name}: ${result.error || result.messages.join('; ')}`); failed++; continue; }
    let worst = 0;
    for (let r = 0; r < n; r++) for (let i = 0; i < nOut; i++) { const a = expected[r * stride + nIn + nUni + i], b = result.out[r * stride + nIn + nUni + i]; worst = Math.max(worst, Math.abs(a - b) / Math.max(1, Math.abs(a))); }
    const ok = worst < 1e-5;
    if (!ok) failed++;
    console.log(`${plan.name}: ${ok ? 'ok' : 'MISMATCH'} on ${result.adapter}, ${n} rows, worst relative error ${worst.toExponential(2)}${result.messages.length ? ', ' + result.messages.join('; ') : ''}`);
  }
} finally { await browser.close(); }
process.exit(failed ? 1 : 0);
