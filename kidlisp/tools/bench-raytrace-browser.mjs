#!/usr/bin/env node
// Build preview first. No AC server or production jobs: this only opens a file.
import puppeteer from "puppeteer";
import sharp from "sharp";
import assert from "node:assert/strict";
import { writeFile, mkdir } from "node:fs/promises";
import { resolve, join } from "node:path";
import { pathToFileURL } from "node:url";
import { cpus, platform, arch } from "node:os";
const out = resolve(process.argv[2] || "/tmp/kidlisp-raytrace");
const url = pathToFileURL(join(out, "preview.html")).href;
await mkdir(out, { recursive: true });
const browser = await puppeteer.launch({ headless: true, executablePath: process.env.CHROME_BIN || "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" });
const errors = [];
try {
  const page = await browser.newPage();
  page.on("pageerror", error => errors.push(error.message));
  await page.setViewport({ width: 1180, height: 880 });
  // Capture the device solely to exercise browser device-loss recovery later.
  await page.evaluateOnNewDocument(() => {
    if (!navigator.gpu) return;
    const request = navigator.gpu.requestAdapter.bind(navigator.gpu);
    navigator.gpu.requestAdapter = async (...args) => {
      const adapter = await request(...args);
      if (adapter) {
        const device = adapter.requestDevice.bind(adapter);
        adapter.requestDevice = async (...args) => window.testRayDevice = await device(...args);
      }
      return adapter;
    };
  });
  await page.goto(url);
  await page.waitForFunction(() => window.raytracePreview?.frame >= 90);
  const live = await page.evaluate(() => window.raytracePreview);
  assert.equal(live.backend, "webgpu", live.notice);
  await page.evaluate(() => window.raytraceHarness.pause());
  await page.screenshot({ path: join(out, "webgpu-preview.png") });
  const report = await page.evaluate(async () => {
    const h = window.raytraceHarness;
    const quantile = (v, q) => [...v].sort((a,b)=>a-b)[Math.ceil(v.length*q)-1];
    const results = [], parity = [], images = [];
    // Pixel comparisons run separately from timings; include partial workgroups.
    for (const [width,height,frame,bounces] of [[1,1,0,0],[65,37,12,1],[96,54,48,2],[160,90,97,3],[320,180,0,2],[480,270,48,2]]) {
      const controls = {width,height,frame,bounces};
      const expected = await h.render("direct-js", controls);
      const wasm = await h.render("wasm-frame", controls);
      if (!expected.rgba.every((v,i)=>v===wasm.rgba[i])) throw new Error(`Wasm pixel mismatch: ${JSON.stringify(controls)}`);
      const gpu = await h.render("webgpu", controls, {readback:true});
      const comparison = h.compareRayGPU(expected.rgba,gpu.rgba);
      parity.push({...controls,...comparison});
      if (!comparison.pass) throw new Error(`GPU pixel mismatch: ${JSON.stringify(parity.at(-1))}`);
      if (width===480) for (const [backend,rgba] of [["direct-js",expected.rgba],["wasm-frame",wasm.rgba],["webgpu",gpu.rgba]]) images.push({backend,width,height,rgba:[...rgba]});
    }
    for (const [width,height] of [[96,54],[160,90],[320,180],[480,270]]) {
      for (const backend of ["direct-js","wasm","wasm-frame","webgpu"]) {
        for(let frame=0;frame<12;frame++) await h.render(backend,{width,height,frame});
        const samples=[],gpuSamples=[],submitSamples=[];
        for(let frame=0;frame<30;frame++) {
          const result=await h.render(backend,{width,height,frame});
          samples.push(result.completedMs);
          if(result.gpuMs!==null) {gpuSamples.push(result.gpuMs);submitSamples.push(result.submitMs);}
        }
        results.push({backend,width,height,medianMs:quantile(samples,.5),p95Ms:quantile(samples,.95),samples,gpuMedianMs:gpuSamples.length?quantile(gpuSamples,.5):null,gpuP95Ms:gpuSamples.length?quantile(gpuSamples,.95):null,gpuSamples,submitSamples});
      }
    }
    return {adapter:h.gpuInfo,results,parity,images};
  });
  for (const row of report.results) console.log(`${row.width}x${row.height} ${row.backend}: ${row.medianMs.toFixed(3)} ms median; ${row.p95Ms.toFixed(3)} ms p95${row.gpuMedianMs!==null?`; GPU ${row.gpuMedianMs.toFixed(3)} ms`:""}`);
  for (const {backend,width,height,rgba} of report.images) await sharp(Buffer.from(rgba),{raw:{width,height,channels:4}}).png().toFile(join(out,`${backend}-480x270.png`));
  delete report.images;
  // UI switching, resize, pause/resume, and continued output after device loss.
  await page.select("#backend","wasm-frame"); await page.select("#resolution","160x90"); await page.click("button");
  await page.waitForFunction(()=>window.raytracePreview?.backend==="wasm-frame" && window.raytracePreview?.width===160);
  await page.click("button"); await page.evaluate(()=>window.raytraceHarness.pause());
  const paused = await page.evaluate(()=>window.raytracePreview.frame);
  await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))));
  assert.equal(await page.evaluate(()=>window.raytracePreview.frame),paused);
  await page.select("#backend","webgpu"); await page.click("button");
  await page.waitForFunction(()=>window.raytracePreview?.backend==="webgpu");
  await page.evaluate(()=>window.testRayDevice.destroy());
  await page.waitForFunction(()=>window.raytracePreview?.backend==="wasm-frame" && window.raytracePreview?.notice.includes("lost"));
  report.deviceLoss = await page.evaluate(()=>window.raytracePreview.notice);
  await page.evaluate(()=>window.raytraceHarness.pause());
  const fallback = await browser.newPage();
  fallback.on("pageerror",error=>errors.push(error.message));
  await fallback.evaluateOnNewDocument(()=>Object.defineProperty(navigator,"gpu",{value:undefined}));
  await fallback.goto(url);
  await fallback.waitForFunction(()=>window.raytracePreview?.frame>=10);
  report.noGPU = await fallback.evaluate(()=>({state:window.raytracePreview,disabled:document.querySelector('[value="webgpu"]').disabled}));
  assert.equal(report.noGPU.state.backend,"wasm-frame"); assert.equal(report.noGPU.disabled,true);
  assert.deepEqual(errors,[]);
  await writeFile(join(out,"browser-report.json"),JSON.stringify({version:1,browser:await browser.version(),host:{cpu:cpus()[0]?.model,platform:platform(),arch:arch()},scope:"CPU: full render, excludes canvas upload. GPU: submission through completed compute + canvas blit, excludes image readback; GPU timestamps measure compute only and may quantize to zero. 12 warmups, 30 samples.",live,...report,errors},null,2)+"\n");
  console.log(`${out}/browser-report.json; pixel, controls and fallback checks passed`);
} finally { await browser.close(); }
