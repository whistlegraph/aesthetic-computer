#!/usr/bin/env node
import puppeteer from "puppeteer";
import assert from "node:assert/strict";
import {writeFile} from "node:fs/promises";
import {resolve,join} from "node:path";
import {pathToFileURL} from "node:url";
import {cpus,platform,arch} from "node:os";
const out=resolve(process.argv[2]||"/tmp/kidlisp-roz");
const browser=await puppeteer.launch({headless:process.env.ROZ_HEADED!=="1",executablePath:process.env.CHROME_BIN||"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"});
const errors=[];
try {
  const page=await browser.newPage();page.on("pageerror",e=>errors.push(e.message));
  await page.setViewport({width:1140,height:940});
  await page.evaluateOnNewDocument(()=>{
    if(!navigator.gpu)return;
    const request=navigator.gpu.requestAdapter.bind(navigator.gpu);
    navigator.gpu.requestAdapter=async(...args)=>{
      const adapter=await request(...args);if(!adapter)return adapter;
      const device=adapter.requestDevice.bind(adapter);
      adapter.requestDevice=async(...args)=>{const d=await device(...args);window.liveRozDevice ||= d;return d;};
      return adapter;
    };
  });
  await page.goto(pathToFileURL(join(out,"preview.html")).href);
  await page.waitForFunction(()=>window.rozPreview?.frame>=180);
  assert.equal(await page.evaluate(()=>window.rozPreview.backend),"webgpu");
  const cadence=await page.evaluate(()=>new Promise(resolve=>{
    const started=performance.now(),first=window.rozPreview.frame;let callbacks=0;
    function tick(){callbacks++;const elapsed=performance.now()-started;if(elapsed>=2000)resolve({elapsed,updates:window.rozPreview.frame-first,callbacks});else requestAnimationFrame(tick);}
    requestAnimationFrame(tick);
  }));
  const updatesPerSecond=cadence.updates*1000/cadence.elapsed;
  assert.ok(updatesPerSecond>55&&updatesPerSecond<65,`Simulation slowed with presentation: ${JSON.stringify(cadence)}`);
  await page.evaluate(()=>window.rozHarness.pause());
  const live=await page.evaluate(()=>({...window.rozPreview,stats:document.getElementById("stats").textContent}));
  await page.screenshot({path:join(out,"canvas.png")});
  assert.equal(await page.$("#view"),null,"Preview must only show the original 2D source");
  await page.setViewport({width:820,height:760});
  await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))));
  assert.equal(await page.evaluate(()=>window.rozPreview.frame),live.frame);
  const report=await page.evaluate(async()=>{
    const parity=[],timings=[];let adapter;
    const median=v=>[...v].sort((a,b)=>a-b)[Math.floor(v.length*.5)];
    const p95=v=>[...v].sort((a,b)=>a-b)[Math.ceil(v.length*.95)-1];
    for(const [width,height,seed,frames] of [[33,35,1,241],[128,128,42,360],[256,256,1,360],[512,512,4294967295,241]]) {
      const h=await window.rozHarness.context({width,height,seed});adapter=h.info;
      try {
        let first,ops=new Set();
        for(let frame=0;frame<frames;frame++) {
          const r=await h.step();r.commands.forEach(c=>c.nodes.forEach(n=>ops.add(n.op)));
          if(frame===0)first=r.rgba.slice();
          const i=r.rgba.findIndex((value,i)=>value!==r.expected[i]);
          if(i!==-1)throw new Error(`Pixel mismatch ${width}x${height} seed ${seed} frame ${frame}, byte ${i}: GPU ${r.rgba[i]} / CPU ${r.expected[i]}`);
        }
        parity.push({width,height,seed,frames,exact:true,ops:[...ops]});
        const before=await h.render([],{readback:true});
        let rejected=false;try{await h.render([{op:"line",color:[255,255,255,24]},{op:"spin",value:99}]);}catch{rejected=true;}
        const after=await h.render([],{readback:true});
        if(!rejected||!after.rgba.every((v,i)=>v===before.rgba[i]))throw new Error("Rejected graph changed feedback state");
        h.reset();const reset=await h.step();if(!reset.rgba.every((v,i)=>v===first[i]))throw new Error("Reset was not deterministic");
        // Four consecutive feedback updates share one submission, but retain
        // distinct uniforms, operation order and reference pixels.
        let batchedFrames=0;
        for(let i=0;i<30;i++) {
          const r=await h.step({count:4});batchedFrames+=4;
          if(!r.rgba.every((v,i)=>v===r.expected[i]))throw new Error(`Batched pixels differ at ${width}x${height}`);
        }
        parity.at(-1).batchedFrames=batchedFrames;
        await h.step({count:2,readback:false,waitForCompletion:false});
        const queued=await h.step({count:2,readback:false,waitForCompletion:false});
        await h.drain();const queuedPixels=await h.render([],{readback:true});
        if(!queuedPixels.rgba.every((v,i)=>v===queued.expected[i]))throw new Error("Queued uniform writes changed feedback pixels");
        if(width<128)continue;
        for(const count of [1,2]) {
          for(let i=0;i<12;i++)await h.step({readback:false,count});
          const samples=[];for(let i=0;i<50;i++) {
            const r=await h.step({readback:false,count});samples.push({cpuMs:r.cpuMs,controlMs:r.controlMs,completedMs:r.completedMs,submitMs:r.submitMs});
          }
          const column=key=>samples.map(s=>s[key]);
          timings.push({width,height,count,preparationMs:h.preparationMs,cpuMedianMs:median(column("cpuMs")),controlMedianMs:median(column("controlMs")),gpuMedianMs:median(column("completedMs")),gpuP95Ms:p95(column("completedMs")),submitMedianMs:median(column("submitMs")),samples});
        }
      }finally{h.dispose();}
    }
    return{adapter,parity,timings};
  });
  for(const r of report.timings)console.log(`${r.width}x${r.height} ${r.count} updates: GPU completion ${r.gpuMedianMs.toFixed(2)} ms median, ${r.gpuP95Ms.toFixed(2)} p95; CPU effects ${r.cpuMedianMs.toFixed(2)} ms; controls ${r.controlMedianMs.toFixed(3)} ms`);
  await page.click("#reset");await page.waitForFunction(()=>window.rozPreview.frame===0);
  await page.setViewport({width:820,height:760});await page.click("#pause");await page.waitForFunction(()=>window.rozPreview.frame>=10);
  await page.evaluate(()=>window.liveRozDevice.destroy());
  await page.waitForFunction(()=>window.rozPreview.backend==="cpu"&&window.rozPreview.frame>=5);
  report.deviceLoss=await page.evaluate(()=>window.rozPreview);assert.match(report.deviceLoss.notice,/lost/i);
  await page.evaluate(()=>window.rozHarness.pause());
  const fallback=await browser.newPage();fallback.on("pageerror",e=>errors.push(e.message));
  await fallback.evaluateOnNewDocument(()=>Object.defineProperty(navigator,"gpu",{value:undefined}));
  await fallback.goto(pathToFileURL(join(out,"preview.html")).href);
  await fallback.waitForFunction(()=>window.rozPreview?.backend==="cpu"&&window.rozPreview.frame>=10);
  report.noGPU=await fallback.evaluate(()=>window.rozPreview);
  assert.equal(report.noGPU.backend,"cpu");assert.deepEqual(errors,[]);
  await writeFile(join(out,"browser-report.json"),JSON.stringify({version:2,cadence,browser:await browser.version(),host:{cpu:cpus()[0]?.model,platform:platform(),arch:arch()},scope:"Pinned $roz body in a fixed-clock persistent-layer host. GPU: completed ordered graph + presentation at logical resolution; no image readback in timing samples. CPU: same commands through graph.mjs, excluding compositing/upload. Controls timed separately. 12 warmups, 50 samples per batch size. Tests compare every RGBA byte including alpha, separately from timings.",live,...report,errors},null,2)+"\n");
  console.log(`${out}/browser-report.json; exact pixels, reset, controls and fallback checks passed`);
} finally{await browser.close();}
