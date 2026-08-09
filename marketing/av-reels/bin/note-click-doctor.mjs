#!/usr/bin/env node
// note-click-doctor.mjs — measure clicks/pops in a piece's note-off tails.
//
// Tees every AudioContext's destination into a capture AudioWorklet (audio
// thread, so the capture itself can't invent discontinuities the way a
// main-thread ScriptProcessor would), plays a scripted run of notes, then
// reports the worst sample-to-sample jump in each note's release window and
// writes a WAV you can actually listen to.
//
//   node marketing/av-reels/bin/note-click-doctor.mjs
//   node .../note-click-doctor.mjs --base http://localhost:8888 --out tmp/clicks
//   node .../note-click-doctor.mjs --taps 8 --hold 400 --gap 500 --pointer
//
// Flags: --base URL   --piece NAME   --out DIR   --taps N   --hold MS
//        --gap MS     --pointer (tap the on-screen pads instead of keys)
//        --keys "asdf" (which keys to play)  --label NAME (names the wav)
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (!a.startsWith("--")) continue;
  const next = argv[i + 1];
  flags[a.slice(2)] = next === undefined || next.startsWith("--") ? true : argv[++i];
}

const BASE = flags.base || "http://localhost:8888";
const PIECE = flags.piece || "notepat";
const OUT = resolve(REPO, flags.out || "tmp/note-clicks");
const TAPS = parseInt(flags.taps || "6", 10);
const HOLD = parseInt(flags.hold || "350", 10);
const GAP = parseInt(flags.gap || "450", 10);
const LABEL = flags.label || "run";
const KEYS = (flags.keys || "asdfgh").split("");
const POINTER = !!flags.pointer;
const W = parseInt(flags.w || "432", 10);
const H = parseInt(flags.h || "768", 10);

// A jump larger than this between adjacent samples is what a click *is* —
// nothing musical moves that fast at 48kHz.
const CLICK = parseFloat(flags.threshold || "0.05");

const PUP = [
  `${REPO}/node_modules/puppeteer`,
  `${REPO}/oven/node_modules/puppeteer`,
  "/opt/oven/node_modules/puppeteer",
].find((p) => existsSync(p));
if (!PUP) throw new Error("puppeteer not found");
const puppeteer = (await import(`${PUP}/lib/esm/puppeteer/puppeteer.js`)).default;
const CHROME = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
].find((p) => p && existsSync(p));

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const browser = await puppeteer.launch({
  headless: "new",
  ...(CHROME ? { executablePath: CHROME } : {}),
  args: [
    "--no-sandbox",
    "--autoplay-policy=no-user-gesture-required",
    "--disable-background-timer-throttling",
    "--disable-backgrounding-occluded-windows",
    "--disable-renderer-backgrounding",
    `--window-size=${W},${H}`,
  ],
});
const page = await browser.newPage();
await page.setViewport({ width: W, height: H, deviceScaleFactor: 1 });
page.on("pageerror", (e) => console.log(`  [pageerror] ${e.message.slice(0, 120)}`));

// ── Capture rig: injected before any page script runs ────────────────────────
await page.evaluateOnNewDocument(() => {
  const OrigCtx = window.AudioContext || window.webkitAudioContext;
  if (!OrigCtx) return;
  // AC builds more than one AudioContext (the synth worklet, plus whatever
  // the mic/GM paths open). Pooling their chunks into one array interleaves
  // a live stream with a silent one and manufactures gaps that look exactly
  // like clicks — so keep a bucket per context and let the analyzer pick the
  // one that actually carries the synth.
  window.__capReady = false;
  window.__capBuckets = {};
  window.__capRate = 0;
  window.__capSeq = 0;

  const PROC = `
    class Cap extends AudioWorkletProcessor {
      constructor() { super(); this.acc = []; this.n = 0; }
      process(inputs) {
        const inp = inputs[0];
        const l = (inp && inp[0]) || null;
        const len = l ? l.length : 128;
        const out = new Float32Array(len);
        if (l) out.set(l);
        this.acc.push(out); this.n += len;
        if (this.n >= 2048) {
          const flat = new Float32Array(this.n);
          let o = 0;
          for (const a of this.acc) { flat.set(a, o); o += a.length; }
          this.port.postMessage(flat, [flat.buffer]);
          this.acc = []; this.n = 0;
        }
        return true;
      }
    }
    registerProcessor("ac-cap", Cap);
  `;

  const proto = window.AudioNode && window.AudioNode.prototype;
  const origConnect = proto.connect;
  window.__origConnect = origConnect;

  if (proto && !proto.__acCapTee) {
    proto.connect = function (dest, ...rest) {
      try {
        const ctx = this.context;
        if (ctx && dest === ctx.destination && !this.__isCapSink) {
          if (ctx.__acCapNode) origConnect.call(this, ctx.__acCapNode);
          else (ctx.__capPending = ctx.__capPending || []).push(this);
        }
      } catch (e) {}
      return origConnect.call(this, dest, ...rest);
    };
    proto.__acCapTee = true;
  }

  const Wrapped = function (...args) {
    const ctx = new OrigCtx(...args);
    window.__capRate = ctx.sampleRate;
    const bucket = "ctx" + window.__capSeq++;
    ctx.__capBucket = bucket;
    window.__capBuckets[bucket] = [];
    const url = URL.createObjectURL(new Blob([PROC], { type: "application/javascript" }));
    ctx.audioWorklet
      .addModule(url)
      .then(() => {
        const node = new AudioWorkletNode(ctx, "ac-cap", {
          numberOfInputs: 1,
          numberOfOutputs: 1,
          outputChannelCount: [1],
        });
        node.port.onmessage = (e) => {
          if (window.__capArmed) window.__capBuckets[bucket].push(e.data);
        };
        // Pull the node without feeding it back into the tee. The sink gain
        // is tiny rather than 0 on purpose — at exactly 0 Chrome is free to
        // treat the branch as silent and skip rendering it, which shows up
        // here as quantum-aligned holes that read as clicks.
        const sink = ctx.createGain();
        sink.gain.value = 1e-7;
        sink.__isCapSink = true;
        origConnect.call(node, sink);
        origConnect.call(sink, ctx.destination);
        ctx.__acCapNode = node;
        (ctx.__capPending || []).forEach((src) => {
          try { origConnect.call(src, node); } catch (err) {}
        });
        ctx.__capPending = [];
        window.__capReady = true;
      })
      .catch((err) => console.warn("cap worklet failed", err));
    return ctx;
  };
  Wrapped.prototype = OrigCtx.prototype;
  window.AudioContext = Wrapped;
  if (window.webkitAudioContext) window.webkitAudioContext = Wrapped;
});

const url = `${BASE}/${encodeURIComponent(PIECE)}?nolabel&nogap`;
console.log(`▶ ${url}`);
try {
  await page.goto(url, { waitUntil: "networkidle2", timeout: 60000 });
} catch (e) {
  console.log(`  ⚠ goto: ${e.message.slice(0, 80)} — continuing`);
}

// The audio graph only exists after a gesture; click the canvas to unlock.
await page
  .evaluate(() => {
    const c = document.querySelector("canvas");
    if (c) { c.focus(); c.click(); }
  })
  .catch(() => {});
await sleep(1000);
await page.mouse.click(W / 2, H / 2).catch(() => {});

// Wait for the capture worklet to actually attach.
for (let i = 0; i < 60; i++) {
  if (await page.evaluate(() => !!window.__capReady)) break;
  await sleep(250);
}
const ready = await page.evaluate(() => !!window.__capReady);
if (!ready) {
  console.log("✗ capture worklet never attached — no AudioContext?");
  await browser.close();
  process.exit(1);
}
await sleep(1500); // let the piece finish booting before we play

const rate = await page.evaluate(() => window.__capRate);
console.log(`  captured at ${rate}Hz · ${POINTER ? "pointer" : "keyboard"} · ${TAPS} taps`);

// ── Perform ─────────────────────────────────────────────────────────────────
// Note-off timestamps are recorded in capture-sample space so the analysis
// looks at the release window and not at the (deliberately sharp) attack.
await page.evaluate(() => {
  for (const k of Object.keys(window.__capBuckets)) window.__capBuckets[k].length = 0;
  window.__capArmed = true;
});
// Position is measured in the busiest bucket — the synth context.
const capturedSoFar = async () =>
  page.evaluate(() =>
    Math.max(
      0,
      ...Object.values(window.__capBuckets).map((b) =>
        b.reduce((n, c) => n + c.length, 0),
      ),
    ),
  );

// The worklet's own discontinuity counter (speaker.mjs → bios → __popTelemetry)
// runs on the real mixed output, on the audio thread, before anything the
// capture tee can distort. It is the metric that decides pass/fail here; the
// WAV is for ears. Zeroing it per note attributes each pop to a gesture.
const popsAround = async (fn, settleMs = 500) => {
  await page.evaluate(() => {
    window.__popTelemetry = { total: 0, max: 0, lastAt: 0 };
  });
  await fn();
  await sleep(settleMs);
  return page.evaluate(() => window.__popTelemetry || { total: 0, max: 0 });
};

const releases = [];
// Pad centers: notepat lays its pads across the lower half of the screen.
const padPoint = (i) => {
  const cols = 3;
  const col = i % cols;
  const row = Math.floor(i / cols) % 2;
  return [W * ((col + 0.5) / cols), H * (0.62 + row * 0.16)];
};

await sleep(200);
for (let i = 0; i < TAPS; i++) {
  const key = KEYS[i % KEYS.length];
  let at = 0;
  let how = "";
  // Only the *release* window is attributed — the attack is deliberately
  // sharp and would otherwise be scored as a defect.
  const onPops = await popsAround(async () => {
    if (POINTER) {
      const [x, y] = padPoint(i);
      await page.mouse.move(x, y);
      await page.mouse.down();
      await sleep(HOLD);
      at = await capturedSoFar();
      how = `pad@${Math.round(x)},${Math.round(y)}`;
    } else {
      await page.keyboard.down(key);
      await sleep(HOLD);
      at = await capturedSoFar();
      how = `key:${key}`;
    }
  }, 0);
  const offPops = await popsAround(async () => {
    if (POINTER) await page.mouse.up();
    else await page.keyboard.up(key);
  }, GAP);
  releases.push({ i, at, how, onPops, offPops });
}
await sleep(600);
await page.evaluate(() => { window.__capArmed = false; });

// Take exactly the context AC runs its speaker worklet on (bios parks it on
// window.audioContext), falling back to the loudest bucket. Mixing buckets
// splices two timelines together and every seam reads as a click.
const bucketReport = await page.evaluate(() => {
  const rows = [];
  for (const [k, b] of Object.entries(window.__capBuckets)) {
    let energy = 0;
    for (const c of b) for (let i = 0; i < c.length; i += 64) energy += Math.abs(c[i]);
    rows.push({ k, chunks: b.length, energy: +energy.toFixed(1) });
  }
  return { rows, ac: window.audioContext?.__capBucket ?? null };
});
console.log(
  `  buckets: ${bucketReport.rows.map((r) => `${r.k}(${r.chunks}ch e=${r.energy})`).join(" ")} · ac=${bucketReport.ac}`,
);
const chunks = await page.evaluate(() => {
  const buckets = window.__capBuckets;
  const preferred = window.audioContext?.__capBucket;
  let pick = preferred && buckets[preferred]?.length ? buckets[preferred] : null;
  if (!pick) {
    let bestEnergy = -1;
    for (const b of Object.values(buckets)) {
      let energy = 0;
      for (const c of b) for (let i = 0; i < c.length; i += 64) energy += Math.abs(c[i]);
      if (energy > bestEnergy) { bestEnergy = energy; pick = b; }
    }
  }
  return (pick || []).map((c) => Array.from(c));
});
const pops = await page.evaluate(() => window.__popTelemetry || null);
await browser.close();

// ── Analyze ─────────────────────────────────────────────────────────────────
const total = chunks.reduce((n, c) => n + c.length, 0);
const pcm = new Float32Array(total);
{
  let o = 0;
  for (const c of chunks) { pcm.set(c, o); o += c.length; }
}
if (!total) {
  console.log("✗ captured 0 samples — the tee never saw the synth output.");
  process.exit(1);
}

const peak = pcm.reduce((m, v) => Math.max(m, Math.abs(v)), 0);
console.log(`  ${total} samples (${(total / rate).toFixed(2)}s), peak ${peak.toFixed(3)}`);

// Harness self-check: a hole punched in the middle of otherwise live audio
// means the capture dropped chunks, and every "click" downstream of it is
// this script's fault rather than the piece's. Say so instead of reporting
// invented defects.
{
  let holes = 0;
  for (let s = 1; s < pcm.length - 1; s++) {
    if (pcm[s] !== 0) continue;
    let j = s;
    while (j < pcm.length && pcm[j] === 0) j++;
    if (j - s > 256 && Math.abs(pcm[s - 1]) > 0.05 && Math.abs(pcm[j]) > 0.05) holes++;
    s = j;
  }
  if (holes) console.log(`  ⚠ ${holes} capture dropouts — results below are unreliable`);
}

// A click is a *step*, and a step is what a smooth waveform can't predict.
// Third-order extrapolation residual is ~A·(ωT)³ for a tone (vanishing at
// musical pitches) but full-scale at a discontinuity — so unlike a raw
// first difference it doesn't accuse high notes of clicking.
const resid = new Float32Array(pcm.length);
for (let s = 3; s < pcm.length; s++)
  resid[s] = pcm[s] - (3 * pcm[s - 1] - 3 * pcm[s - 2] + pcm[s - 3]);

// Worst adjacent-sample jump inside each note's release window.
const WIN = Math.round(rate * 0.4); // 400ms after note-off covers any tail
let worstOverall = 0;
const rows = [];
for (const r of releases) {
  let worst = 0;
  let worstAt = 0;
  let count = 0;
  const start = Math.max(3, r.at);
  const end = Math.min(pcm.length, r.at + WIN);
  for (let s = start; s < end; s++) {
    const d = Math.abs(resid[s]);
    if (d > CLICK) count++;
    if (d > worst) { worst = d; worstAt = s; }
  }
  worstOverall = Math.max(worstOverall, worst);
  rows.push({
    note: r.i,
    how: r.how,
    onPops: r.onPops.total,
    offPops: r.offPops.total,
    offPopMax: r.offPops.max,
    capWorst: +worst.toFixed(4),
    capClicks: count,
    atMs: +(((worstAt - r.at) / rate) * 1000).toFixed(1),
  });
}

console.log("");
console.log("  note  how              note-on pops   note-off pops (max)   [capture: worst/clicks]");
for (const row of rows) {
  const flag = row.offPops > 0 ? " ⚠" : "  ";
  console.log(
    `  ${String(row.note).padEnd(5)} ${row.how.padEnd(16)} ${String(row.onPops).padEnd(14)} ${String(row.offPops + " (" + row.offPopMax + ")").padEnd(21)} ${row.capWorst}/${row.capClicks}${flag}`,
  );
}
const offTotal = rows.reduce((n, r) => n + r.offPops, 0);
const onTotal = rows.reduce((n, r) => n + r.onPops, 0);
const totalClicks = offTotal;
console.log("");
console.log(`  ▸ release pops (worklet, authoritative): ${offTotal}`);
console.log(`  ▸ attack pops (worklet):                 ${onTotal}`);
console.log(`  · capture-tee worst residual: ${worstOverall.toFixed(4)} (rig artifacts possible — see WAV)`);
if (pops) console.log(`  · session pop total since last reset: ${pops.total} max=${pops.max}`);

// ── Write a WAV you can listen to ───────────────────────────────────────────
mkdirSync(OUT, { recursive: true });
const wavPath = join(OUT, `${LABEL}.wav`);
const bytes = Buffer.alloc(44 + total * 2);
bytes.write("RIFF", 0);
bytes.writeUInt32LE(36 + total * 2, 4);
bytes.write("WAVE", 8);
bytes.write("fmt ", 12);
bytes.writeUInt32LE(16, 16);
bytes.writeUInt16LE(1, 20);
bytes.writeUInt16LE(1, 22);
bytes.writeUInt32LE(rate, 24);
bytes.writeUInt32LE(rate * 2, 28);
bytes.writeUInt16LE(2, 32);
bytes.writeUInt16LE(16, 34);
bytes.write("data", 36);
bytes.writeUInt32LE(total * 2, 40);
for (let s = 0; s < total; s++) {
  const v = Math.max(-1, Math.min(1, pcm[s]));
  bytes.writeInt16LE(Math.round(v * 32767), 44 + s * 2);
}
writeFileSync(wavPath, bytes);
writeFileSync(
  join(OUT, `${LABEL}.json`),
  JSON.stringify({ base: BASE, piece: PIECE, rate, rows, worstOverall, totalClicks, pops }, null, 2),
);
console.log(`  🎧 ${wavPath}`);

process.exit(totalClicks > 0 ? 2 : 0);
