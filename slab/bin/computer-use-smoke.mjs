#!/usr/bin/env node
// A disposable website/profile/daemon. Only --native sends macOS input, and
// only after Frame identifies the fixture window and its observed button.
import assert from "node:assert/strict";
import { createServer } from "node:http";
import { mkdtemp, readFile, writeFile, rm, access } from "node:fs/promises";
import { join, resolve } from "node:path";
import { tmpdir } from "node:os";
import { spawn } from "node:child_process";
import { setTimeout as delay } from "node:timers/promises";
import { chromium } from "playwright-core";
import { createComputerUseClient } from "../lib/computer-use-client.mjs";
import { chooseObservedTarget } from "../lib/jev-computer-use.mjs";
import { playWordplay } from "../lib/wordplay-run.mjs";
import { playMathplay } from "../lib/mathplay-run.mjs";
import { playNativeDrag } from '../lib/native-drag-run.mjs';
import { captureFrame as nativeCapture } from './frame.mjs';

const dragBench = process.argv.includes('--drag-bench');
const holdBench = process.argv.includes('--hold-bench');
if (dragBench && holdBench) throw new Error('Run hold and drag experiments separately');
const compactBench = process.argv.includes('--compact-bench') || holdBench;
const installedFrame = process.argv.includes('--installed-frame');
const nativeClickBench = process.argv.includes("--native-click-bench") || compactBench;
const legacyInput = process.argv.includes('--legacy-input');
if (legacyInput && compactBench) throw new Error('Compact checks require resident input');
if (legacyInput && !nativeClickBench) throw new Error('--legacy-input requires --native-click-bench');
const native = process.argv.includes("--native") || nativeClickBench || dragBench, jev = process.argv.includes("--jev");
if (installedFrame && !native) throw new Error('--installed-frame requires a native run');
const wordplay = process.argv.includes("--wordplay");
const mathplay = process.argv.includes("--math");
if (mathplay && (wordplay || native || jev)) throw new Error('Use --math alone for the arithmetic fixture');
const clickBench = process.argv.includes("--click-bench");
if (clickBench && (native || wordplay || mathplay)) throw new Error('Use --click-bench alone for the isolated browser latency check');
if (wordplay && native) throw new Error('Use --wordplay for browser play or --native for the native fixture separately');
const root = resolve(import.meta.dirname, "../..");
const dir = await mkdtemp(join(tmpdir(), "computer-use-smoke-"));
const report = { at: new Date().toISOString(), browser: [], native: [], jev: [] };
report.installedFrame = installedFrame;
if (nativeClickBench) { report.nativeInputMode = legacyInput ? 'legacy' : compactBench ? 'compact' : 'resident'; report.transport = process.env.SLAB_FRAME_TRANSPORT || 'socket'; }
const children = [];
let context, site;
const text = r => (r.content || []).filter(c => c.type === "text").map(c => c.text).join("\n");
async function listen(server) { await new Promise(resolve => server.listen(0, "127.0.0.1", resolve)); return server.address().port; }
async function freePort() { const s = createServer(); const p = await listen(s); await new Promise(resolve => s.close(resolve)); return p; }
async function readyMCP(port) {
  return (await fetch(`http://127.0.0.1:${port}/mcp`, { method: "POST", headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ jsonrpc: "2.0", id: 0, method: "ping" }), signal: AbortSignal.timeout(1000) })).ok;
}
function start(script, args, env) {
  const child = spawn(process.execPath, [join(root, script), ...args], { env, stdio: ["ignore", "pipe", "pipe"] });
  child.errors = "";
  child.stdout.on("data", () => {});
  child.stderr.on("data", chunk => { child.errors = (child.errors + chunk).slice(-2000); });
  children.push(child); return child;
}
async function waitReady(check) {
  let last;
  for (let i = 0; i < 60; i++) {
    try { if (await check()) return; } catch (error) { last = error; }
    if (children.some(c => c.exitCode !== null)) throw new Error(children.map(c => c.errors).join("\n"));
    await delay(50);
  }
  throw last || new Error("Fixture startup timed out");
}
try {
  const gameDir = mathplay ? 'slab/mathplay' : wordplay ? 'slab/wordplay' : null;
  const html = await readFile(join(root, gameDir ? gameDir+'/index.html' : (dragBench ? "slab/test/fixtures/frame-drag.html" : "slab/test/fixtures/computer-use.html")));
  const game = gameDir ? await readFile(join(root, gameDir+'/game.mjs')) : null;
  site = createServer((req, res) => {
    if (gameDir && req.url === '/game.mjs') { res.setHeader('Content-Type', 'text/javascript'); res.end(game); return; }
    res.setHeader("Content-Type", "text/html; charset=utf-8");
    res.end(req.url === "/second" ? '<!doctype html><title>Second fixture page</title><h1>Navigation verified</h1>' : html);
  });
  const sitePort = await listen(site);
  context = await chromium.launchPersistentContext(join(dir, "profile"), {
    channel: "chrome", headless: !native, viewport: native ? null : { width: 1000, height: 850 },
    args: ["--remote-debugging-port=0", "--window-size=1000,850", "--force-renderer-accessibility"],
  });
  const browserPort = (await readFile(join(dir, "profile/DevToolsActivePort"), "utf8")).split("\n")[0];
  const page = context.pages()[0];
  await page.goto(`http://127.0.0.1:${sitePort}/`);
  const cdp = await context.newCDPSession(page);
  const { targetInfo } = await cdp.send("Target.getTargetInfo");
  await cdp.detach();
  const target = targetInfo.targetId;
  const config = join(dir, "machines.json"), sock = join(dir, "puppet.sock");
  await writeFile(config, JSON.stringify({ machines: { fixture: { local: true, cdpUrl: `http://127.0.0.1:${browserPort}` } } }));
  const env = { ...process.env, SLAB_PUPPET_CONFIG: config, SLAB_PUPPET_SOCK: sock,
    SLAB_INPUT_LEASE_DIR: process.env.SLAB_INPUT_LEASE_DIR || join(process.env.HOME, ".local/share/slab/input-leases") };
  // The test daemon's status file belongs to the fixture; native input leases
  // retain the real user's root so another controller still excludes us.
  start("slab/bin/puppet.mjs", ["daemon"], { ...env, HOME: dir });
  await waitReady(async () => { await access(sock); return true; });
  const puppetPort = await freePort(), framePort = installedFrame ? 7767 : await freePort();
  start("slab/bin/puppet-mcp.mjs", ["--http", String(puppetPort)], env);
  if (native && !installedFrame) start("slab/bin/frame-mcp.mjs", ["--http", String(framePort)], { ...env, SLAB_FRAME_NATIVE_INPUT: legacyInput ? 'legacy' : 'resident' });
  const servers = { puppet: `http://127.0.0.1:${puppetPort}/mcp`, ...(native ? { frame: `http://127.0.0.1:${framePort}/mcp` } : {}) };
  const tools = ["puppet_list", "puppet_snapshot", "puppet_choose", "puppet_click", "puppet_fill", "puppet_wait", "frame", "frame_click", "frame_drag", "frame_reframe"];
  await waitReady(() => readyMCP(puppetPort));
  if (native) await waitReady(() => readyMCP(framePort));
  const client = createComputerUseClient({ servers, allowedTools: tools });
  await client.discover();
  await waitReady(async () => JSON.parse(text(await client.call("puppet_list", { full: true }))).fixture.connected);
  async function call(name, args, lane = "browser") {
    const start = performance.now();
    const result = await client.call(name, args);
    assert.ok(!result.isError, text(result));
    report[lane].push({ tool: name, ...(args.locator ? { control: args.locator.name || args.locator.label } : {}), ms: Math.round(performance.now() - start) });
    return result;
  }
  const browser = { machine: "fixture", target };
  if (dragBench) {
    await playNativeDrag(call,page,context,report);
  } else if (mathplay) {
    report.game = await playMathplay(call, browser);
    assert.equal(await page.locator('#score').textContent(), 'Score: 30 / 30');
  } else if (wordplay) {
    report.game = await playWordplay(call, browser);
    // Check single delivery and round progression through the rendered score.
    if (report.game.complete) assert.equal(await page.locator('#score').textContent(), `Score: ${report.game.correct} / 8`);
  } else {
  const snap = JSON.parse(text(await call("puppet_snapshot", browser)));
  assert.match(snap.tree, /Add one/);
  async function click(name, expected) {
    const result = JSON.parse(text(await call("puppet_click", { ...browser, locator: { role: "button", name }, after: { locator: { text: expected } } })));
    assert.equal(result.performed, true); assert.equal(result.verification.ok, true);
  }
  await click("Add one", "Count: 1");
  if (clickBench) {
    const samples=[];
    for(let count=2;count<=26;count++){
      const start=performance.now();
      await click("Add one", `Count: ${count}`);
      samples.push(+(performance.now()-start).toFixed(2));
    }
    const sorted=[...samples].sort((a,b)=>a-b);
    report.clickLatency={scope:'local HTTP/MCP + daemon + browser + new counter verification; excludes model inference',
      samples:sorted.length,medianMs:sorted[12],p95Ms:sorted[23],maxMs:sorted.at(-1),
      under100ms:sorted.filter(ms=>ms<100).length,timesMs:samples};
  }
  await call("puppet_fill", { ...browser, locator: { label: "Name" }, value: "Fixture" });
  await click("Save name", "Saved: Fixture");
  await click("Open menu", "Choose blue");
  await click("Choose blue", "Selected: blue");
  await click("Start delayed update", "Ready");

  if (jev) {
    if (!process.env.OPENROUTER_API_KEY) report.jev.push({ skipped: "OPENROUTER_API_KEY unavailable" });
    else for (let i = 0; i < 3; i++) {
      const start = performance.now();
      const candidates = await page.locator("button:visible").evaluateAll(buttons => buttons.map((b, i) => ({ id: `button_${i}`, label: b.textContent, role: "button", visible: true, disabled: b.disabled })));
      const decision = await chooseObservedTarget({ goal: "Increase the counter by one", observation: { id: crypto.randomUUID(), target, capturedAt: new Date().toISOString() }, candidates });
      const sample = { ms: Math.round(performance.now() - start), action: decision.action, label: decision.candidate?.label, probability: decision.probability, reason: decision.reason };
      report.jev.push(sample);
      if (decision.action === "target") assert.equal(decision.candidate.label, "Add one");
    }
  }

  if (native) {
    // Slab may tile a newly opened window during fixture setup. Size only this
    // owned window, then observe after its bounds settle; setup is not timed.
    const geometry = await context.newCDPSession(page);
    try {
      const { windowId } = await geometry.send('Browser.getWindowForTarget');
      await geometry.send('Browser.setWindowBounds',{windowId,bounds:{left:40,top:40,width:1000,height:800}});
      await page.waitForFunction(()=>window.outerWidth===1000 && window.screenX===40,null,{timeout:3000});
      await delay(200);
    } finally { await geometry.detach(); }
    await page.bringToFront();
    const observed = await call("frame", { machine: "local", fast: false, visual: false }, "native");
    const digest = text(observed);
    assert.match(digest, /capture: ok/);
    assert.match(digest, /frontmost: .*Chrome/);
    assert.match(digest, /Computer-use check/);
    const observation = JSON.parse(digest.match(/^observation: (.+)$/m)[1]);
    const match = digest.match(/AXButton «Add one» @\((\d+),(\d+)\)/) || digest.match(/«Add one» @\((\d+),(\d+)\)/);
    if (!match) report.failedObservation = digest;
    assert.ok(match, "Frame must observe the exact fixture control before native input");
    const firstClick = await call("frame_click", { machine: "local", observationId: observation.id, x: Number(match[1]), y: Number(match[2]), fast: true, visual: false }, "native");
    try { await page.getByText("Count: 2", { exact: true }).waitFor({ timeout: 3000 }); }
    catch(error) {
      report.failedObservation=digest; report.failedClick=text(firstClick);
      report.counterAfterFailure=await page.locator('#count').textContent();
      const pixels=observed.content.find(c=>c.type==='image');
      if(pixels) await writeFile('/tmp/frame-smoke-failed-observation.jpg',Buffer.from(pixels.data,'base64'));
      throw error;
    }
    report.native.push({ verified: "native click incremented counter exactly once" });
    if (nativeClickBench) {
      report.nativeClickLatency=[];
      let current=JSON.parse(text(await call('frame',{machine:'local',ocr:false,visual:false},'native')).match(/^observation: (.+)$/m)[1]);
      const counterPoint = digest.match(/«Count: 1» @\((\d+),(\d+)\)/);
      if (compactBench) assert.ok(counterPoint, 'Frame must observe the counter before compact verification');
      const holds = (process.env.SLAB_HOLD_SWEEP || '40,10,5,2.5').split(',').map(Number);
      assert.ok(holds.length && holds.every(n=>Number.isFinite(n)&&n>=0&&n<=1000));
      const samples = holdBench ? holds.length * 100 : compactBench ? 25 : 10;
      for(let i=0;i<samples;i++){
        const holdMs = holdBench ? holds[i % holds.length] : undefined;
        const settleMs=compactBench?0:i%2===0?180:0,start=performance.now();
        const result=await call('frame_click',{machine:'local',observationId:current.id,x:Number(match[1]),y:Number(match[2]),ocr:false,visual:false,settleMs,...(holdMs !== undefined ? {holdMs} : {}),...(compactBench?{verify:{x:Number(counterPoint[1]),y:Number(counterPoint[2]),role:'AXStaticText',attribute:'AXValue',equals:`Count: ${i+3}`}}:{})},'native');
        await page.getByText(`Count: ${i+3}`,{exact:true}).waitFor({timeout:3000});
        current=JSON.parse(text(result).match(/^observation: (.+)$/m)[1]);
        const nativeInput=text(result).match(/^native input: (.+)$/m);
        if (!legacyInput) assert.ok(nativeInput, 'The resident benchmark must not silently exercise legacy input');
        if (compactBench) {
          assert.match(text(result), /capture: verified/);
          assert.equal(result.content.some(c=>c.type==='image'),false);
          assert.equal(JSON.parse(nativeInput[1]).verification.ok,true);
        }
        if (holdBench) assert.equal(JSON.parse(nativeInput[1]).holdMs, holdMs);
        report.nativeClickLatency.push({settleMs,...(holdBench?{holdMs}:{}),ms:+(performance.now()-start).toFixed(2),verifiedCount:i+3,responseBytes:Buffer.byteLength(JSON.stringify(result)),
          ...(nativeInput ? { nativeInput: JSON.parse(nativeInput[1]) } : {})});
      }
      let finalCount = samples + 2;
      if (holdBench) {
        // Explicit multi-clicks retain their original 80 ms inter-click spacing.
        report.multiClickChecks=[];
        for (const count of [2,3]) {
          finalCount += count;
          const result=await call('frame_click',{machine:'local',observationId:current.id,
            x:Number(match[1]),y:Number(match[2]),count,holdMs:Math.min(...holds),settleMs:0,ocr:false,visual:false,
            verify:{x:Number(counterPoint[1]),y:Number(counterPoint[2]),role:'AXStaticText',attribute:'AXValue',equals:`Count: ${finalCount}`}},'native');
          assert.match(text(result),/capture: verified/);
          await page.getByText(`Count: ${finalCount}`,{exact:true}).waitFor({timeout:3000});
          current=JSON.parse(text(result).match(/^observation: (.+)$/m)[1]);
          report.multiClickChecks.push({count,holdMs:Math.min(...holds),verifiedCount:finalCount});
        }
      }
      if (compactBench) {
        const verify = {x:Number(counterPoint[1]),y:Number(counterPoint[2]),role:'AXStaticText',attribute:'AXValue',equals:`Count: ${finalCount}`};
        const args = {machine:'local',observationId:current.id,x:Number(match[1]),y:Number(match[2]),ocr:false,visual:false,settleMs:0,verify};
        await assert.rejects(call('frame_click',args,'native'),/already true/);
        await assert.rejects(call('frame_click',{...args,verify:{...verify,role:'AXButton',equals:'absent'}},'native'),/wrong role/);
        assert.equal(await page.getByText(`Count: ${finalCount}`,{exact:true}).count(),1);
        const fallback = await call('frame_click',{...args,verify:{...verify,equals:'Count: impossible',timeoutMs:50}},'native');
        assert.match(text(fallback),/capture: ok/);
        assert.ok(fallback.content.some(c=>c.type==='image'));
        assert.equal(JSON.parse(text(fallback).match(/^native input: (.+)$/m)[1]).verification.ok,false);
        await page.getByText(`Count: ${++finalCount}`,{exact:true}).waitFor({timeout:3000});
        await assert.rejects(nativeCapture('local',{memory:true,session:current.session,nativeClick:{observationId:current.id,x:Number(match[1]),y:Number(match[2]),count:1,settleMs:0}}),/already consumed/);
        report.compactRecovery={alreadyTrueRejected:true,wrongRoleRejected:true,timeoutReturnedPixels:true,clickNotRepeated:true,consumedRejected:true};
      }
      if (!legacyInput) {
        const observed=await call('frame',{machine:'local',ocr:false,visual:false},'native');
        const observation=JSON.parse(text(observed).match(/^observation: (.+)$/m)[1]);
        const guardArgs={memory:true,session:observation.session,nativeGuard:{observationId:observation.id}};
        const guardStart=performance.now(),guardResult=await nativeCapture('local',guardArgs);
        assert.equal(guardResult.env.capture,'guard');assert.ok(!guardResult.jpg?.length);
        assert.equal(guardResult.env.nativeInput.status,'guarded');
        report.nativeGuard={ms:+(performance.now()-guardStart).toFixed(2),nativeMs:guardResult.env.nativeInput.guardMs,pixels:false};
        await assert.rejects(nativeCapture('local',{...guardArgs,nativeGuard:{observationId:'stale-observation'}}),/no action sent/);
        const inspector=await context.newCDPSession(page);
        try {
          const {windowId,bounds}=await inspector.send('Browser.getWindowForTarget');
          await inspector.send('Browser.setWindowBounds',{windowId,bounds:{left:bounds.left+20,top:bounds.top,width:bounds.width,height:bounds.height}});
          // Wait for the actual browser position, not a fixed UI delay.
          await page.waitForFunction(left=>window.screenX===left,bounds.left+20,{timeout:3000});
          // screenX updates before WindowServer necessarily publishes new CG geometry.
          // Probe without input until the native window itself reports the move.
          let moved = false;
          for (let attempt=0;attempt<100;attempt++) {
            try { await nativeCapture('local',guardArgs); }
            catch(error) { assert.match(error.message,/changed or moved.*no action sent/); moved=true; break; }
            await delay(5);
          }
          assert.ok(moved,'Native guard must reject once the window geometry moves');
          await inspector.send('Browser.setWindowBounds',{windowId,bounds});
          await page.waitForFunction(left=>window.screenX===left,bounds.left,{timeout:3000});
        } finally { await inspector.detach(); }
        assert.equal(await page.getByText(`Count: ${finalCount}`,{exact:true}).count(),1);
        report.nativeGuard.staleRejected=true;report.nativeGuard.movedRejected=true;report.nativeGuard.counterUnchanged=true;
      }
    }
    const reframe = await call("frame_reframe", { machine: "local", fast: true, visual: false }, "native");
    assert.ok(reframe.content.length > 0);
  }
  }
  // Only the generated fixture is retained; the browser profile is removed.
  const artifact = join(tmpdir(), mathplay ? "mathplay-verified.png" : wordplay ? "wordplay-verified.png" : "computer-use-smoke-verified.png");
  await page.screenshot({ path: artifact }); report.screenshot = artifact;
  if (!wordplay && !mathplay && !dragBench) {
  const navigation = JSON.parse(text(await call("puppet_click", { ...browser, locator: { role: "link", name: "Second page" }, after: { locator: { text: "Navigation verified" } } })));
  assert.equal(navigation.verification.ok, true);
  assert.equal(new URL(page.url()).pathname, "/second");
  }
  report.ok = (wordplay || mathplay) ? report.game.complete : true;
  if (!report.ok) process.exitCode = 1;
} catch (error) {
  report.ok = false; report.error = error.message; process.exitCode = 1;
} finally {
  for (const child of children) child.kill();
  await context?.close();
  if (site) await new Promise(resolve => site.close(resolve));
  await rm(dir, { recursive: true, force: true });
  await writeFile(join(tmpdir(), mathplay ? "mathplay-report.json" : wordplay ? "wordplay-report.json" : "computer-use-smoke-report.json"), JSON.stringify(report, null, 2));
  console.log(JSON.stringify(report, null, 2));
}
