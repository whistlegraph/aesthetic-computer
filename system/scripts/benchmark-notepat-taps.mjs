// Run from the repo root. The report is JSON on stdout, progress on stderr.
import { parseArgs } from "node:util";
import { chromium, devices } from "playwright";
import { GM_PROGRAM_NAMES } from "../public/aesthetic.computer/lib/notepat-programs.mjs";

const { values } = parseArgs({ options: {
  url: { type: "string", default: "https://aesthetic.computer/notepat" },
  worktree: { type: "boolean", default: false },
  batched: { type: "boolean", default: false },
  touchscreen: { type: "boolean", default: false },
  first: { type: "string", default: "1" }, last: { type: "string", default: "128" },
  taps: { type: "string", default: "8" },
} });
const first = Number(values.first), last = Number(values.last), taps = Number(values.taps);
if (![first, last, taps].every(Number.isInteger) || first < 1 || last > 128 ||
    first > last || taps < 1 || taps > 200) throw Error("Expected programs 1–128 and 1–200 taps per program");

function installProbe(batched) {
  window.acVisitTrackingDisabled = true;
  window.acNotepatLatency = { mode: batched ? "batched" : "fast" };
  const probe = window.tapProbe = { ready: null, active: false, starts: [] };
  const post = Worker.prototype.postMessage;
  Worker.prototype.postMessage = function(message, ...rest) {
    if (message?.type === "notepat:state") {
      probe.hasHost = message.content.hostReady;
      if (message.content.gmReady) probe.ready = message.content.instrument.program;
    }
    return post.call(this, message, ...rest);
  };
  window.addEventListener("pointerdown", () => { probe.down = performance.now(); }, true);
  const start = AudioBufferSourceNode.prototype.start;
  AudioBufferSourceNode.prototype.start = function(when, ...rest) {
    const scheduledAt = performance.now();
    const result = start.call(this, when, ...rest);
    if (probe.active && this.buffer) {
      const ctx = this.context, stamp = ctx.getOutputTimestamp();
      probe.starts.push({
        dispatch: scheduledAt - probe.down,
        output: stamp.performanceTime > 0
          ? stamp.performanceTime + 1000 * ((when || ctx.currentTime) - stamp.contextTime) - probe.down : null,
        fallback: this.loop, sampleRate: ctx.sampleRate,
      });
    }
    return result;
  };
}

function stats(values) {
  const sorted = values.filter(Number.isFinite).sort((a, b) => a - b);
  const at = p => sorted.length ? +sorted[Math.ceil(sorted.length * p) - 1].toFixed(2) : null;
  return { n: sorted.length, median: at(.5), p95: at(.95), max: at(1) };
}

function summarize(touches) {
  const singles = touches.filter(starts => starts.length === 1 && !starts[0].fallback).flat();
  return {
    taps: touches.length, missed: touches.filter(starts => !starts.length).length,
    duplicates: touches.filter(starts => starts.length > 1).length,
    fallback: touches.flat().filter(s => s.fallback).length,
    dispatchMs: stats(singles.map(s => s.dispatch)),
    estimatedOutputStartMs: stats(singles.map(s => s.output)),
  };
}

const browser = await chromium.launch({ channel: "chrome", args: ["--mute-audio"] });
const all = [], programs = [], origin = new URL(values.url).origin;
try {
  // Each selected patch preloads its full bank. Use a fresh page every eight.
  for (let batch = first; batch <= last; batch += 8) {
    const context = await browser.newContext({ ...devices["Pixel 7"], serviceWorkers: "block" });
    try {
      if (values.worktree) {
        for (const file of ["bios.mjs", "disks/notepat.mjs", "lib/notepat-layout.mjs", "lib/pen.mjs"]) {
          await context.route(url => url.origin === origin && url.pathname === "/aesthetic.computer/" + file,
            route => route.fulfill({ path: "system/public/aesthetic.computer/" + file, contentType: "application/javascript" }));
        }
      }
      await context.addInitScript(installProbe, values.batched);
      const page = await context.newPage();
      await page.goto(values.url, { waitUntil: "domcontentloaded" });
      await page.waitForFunction(() => window.tapProbe.hasHost);
      await page.touchscreen.tap(180, 390); // Unlock Web Audio.
      await page.waitForFunction(() => window.audioWorkletReady && window.tapProbe.ready === 0);
      await page.touchscreen.tap(370, 21); // Grid toggle in a fresh Pixel 7 profile.
      await page.waitForFunction(() => localStorage.getItem("notepat:layout") === '"grid"');
      for (let number = batch; number < batch + 8 && number <= last; number++) {
        const selectedAt = performance.now();
        await page.evaluate(program => {
          tapProbe.ready = null;
          acSEND({ type: "notepat:control", content: { kind: "program", program } });
        }, number - 1);
        await page.waitForFunction(p => tapProbe.ready === p, number - 1, { polling: 20, timeout: 60000 });
        const readyMs = Math.round(performance.now() - selectedAt);
        const touches = [];
        for (let i = 0; i < taps; i++) {
          // Pad centers in the fixed portrait viewport, alternating octaves.
          const point = { x: 58 + 96 * (i % 4), y: 118 + 128 * (Math.floor(i / 2) % 3 + 3 * (i % 2)) };
          await page.evaluate(() => { tapProbe.starts = []; tapProbe.active = true; });
          if (values.touchscreen) await page.touchscreen.tap(point.x, point.y);
          else await page.evaluate(async ({ x, y }) => {
            const target = document.elementFromPoint(x, y);
            const init = { pointerType: "touch", pointerId: 7, isPrimary: true, clientX: x, clientY: y, bubbles: true };
            target.dispatchEvent(new PointerEvent("pointerdown", { ...init, buttons: 1, pressure: .5 }));
            await new Promise(resolve => setTimeout(resolve, 45));
            target.dispatchEvent(new PointerEvent("pointerup", init));
          }, point);
          await page.waitForTimeout(65 + (i * 11) % 37);
          touches.push(await page.evaluate(() => { tapProbe.active = false; return tapProbe.starts; }));
        }
        all.push(...touches);
        programs.push({ number, name: GM_PROGRAM_NAMES[number - 1], readyMs, ...summarize(touches) });
      }
      console.error(`Completed through program ${programs.at(-1).number}`);
    } finally { await context.close(); }
  }
  const summary = summarize(all);
  console.log(JSON.stringify({
    measuredAt: new Date().toISOString(), url: values.url, worktree: values.worktree,
    browser: browser.version(), host: process.platform + "-" + process.arch, emulation: "Pixel 7",
    input: values.touchscreen ? "Chromium touchscreen tap" : "DOM PointerEvent, 45 ms hold",
    mode: values.batched ? "frame-batched" : "immediate input frame",
    sampleRates: [...new Set(all.flat().map(s => s.sampleRate))], summary, programs,
  }, null, 2));
  if (summary.dispatchMs.n !== summary.taps) process.exitCode = 1;
} finally { await browser.close(); }
