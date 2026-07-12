#!/usr/bin/env node
// capture-av.mjs — headlessly capture a MUXED audio+video "performance" of a
// live aesthetic.computer piece, using AC's OWN synth audio.
//
// How it works (two proven halves, muxed by ffmpeg):
//   VIDEO ← CDP screencast (Page.startScreencast) → timestamped PNG frames.
//           Reliable in headless regardless of tab visibility.
//   AUDIO ← we patch AudioNode.prototype.connect (before AC boots) so every node
//           routed to ctx.destination ALSO tees into a capture
//           MediaStreamDestination; an in-page audio-only MediaRecorder records
//           that. Audio worklets run on the audio thread, so this survives the
//           headless rAF throttle that makes canvas.captureStream() emit nothing.
//   PERFORM ← AC instruments are SILENT without input, so we inject a timed
//           keyboard/mouse "performance" to actually play them.
//
//   node marketing/av-reels/bin/capture-av.mjs notepat --perform notepat-melody --duration 10
//   node .../capture-av.mjs bubble --perform bubble-taps --duration 10
//   node .../capture-av.mjs '$roz' --duration 10            # auto (no input)
//
// Output: out/<slug>/base-<slug>.mp4 (H.264 + AAC) + capture.json
//
// Flags: --duration N --fps N --density N --width N --height N --perform NAME
//        --base URL --headful --settle MS --slug NAME

import { existsSync, mkdirSync, rmSync, writeFileSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { PERFORMANCES } from "./performances.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  } else positional.push(a);
}

const PIECE = positional[0];
if (!PIECE) {
  console.error("usage: capture-av.mjs '<piece>' [--perform NAME] [--duration 10] [--fps 30] [--density 3]");
  process.exit(1);
}
const SLUG = (flags.slug || PIECE).replace(/^\$/, "").replace(/[^a-z0-9_-]+/gi, "-").replace(/^-+|-+$/g, "").toLowerCase() || "piece";

const DURATION = parseFloat(flags.duration || 10);
const FPS = parseInt(flags.fps || 30, 10);
const DENSITY = String(flags.density || 3);
const CAP_W = parseInt(flags.width || 1080, 10);
const CAP_H = parseInt(flags.height || 1920, 10);
// SETTLE is now just a short grace AFTER load detection (see waitForLoaded).
const SETTLE = parseInt(flags.settle || 500, 10);
// Performance source: a named built-in (--perform NAME), an inline JSON array
// (--perform-json '[...]'), or a JSON file (--perform-file path). Lets parallel
// callers supply bespoke scores without editing performances.mjs.
let PERFORM = null;
if (flags["perform-json"]) PERFORM = JSON.parse(flags["perform-json"]);
else if (flags["perform-file"]) PERFORM = JSON.parse(readFileSync(flags["perform-file"].replace(/^~/, process.env.HOME), "utf8"));
else if (flags.perform) PERFORM = PERFORMANCES[flags.perform] || null;
if (flags.perform && !PERFORM && !flags["perform-json"] && !flags["perform-file"]) console.log(`  ⚠ unknown --perform ${flags.perform} (running auto/no-input)`);

const OUT = resolve((flags.out || `${LANE}/out/${SLUG}`).replace(/^~/, process.env.HOME));
rmSync(OUT, { recursive: true, force: true });
const FRAMES_DIR = `${OUT}/frames`;
mkdirSync(FRAMES_DIR, { recursive: true });

const PUPPETEER_DIR = [
  `${REPO}/node_modules/puppeteer`, `${REPO}/oven/node_modules/puppeteer`, "/opt/oven/node_modules/puppeteer",
].find((p) => existsSync(p));
if (!PUPPETEER_DIR) throw new Error("puppeteer not found");
const puppeteer = (await import(`${PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;

const CHROME = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary",
].find((p) => p && existsSync(p));

const BASE = flags.base || "https://aesthetic.computer";
const isMerry = /\s/.test(PIECE) || PIECE.startsWith("merry");
// --label keeps AC's top-left piece-name label (default hides it with nolabel).
const noLabel = flags.label ? "" : "nolabel&";
const url = isMerry
  ? `${BASE}/prompt?${noLabel}nogap&density=${DENSITY}`
  : `${BASE}/${encodeURIComponent(PIECE)}?${noLabel}nogap&density=${DENSITY}`;

console.log(`▸ capture-av "${PIECE}"${PERFORM ? ` · perform ${flags.perform}` : ""} → ${OUT}`);
console.log(`  ${url}`);
console.log(`  ${CAP_W}×${CAP_H} · ${DURATION}s @ ${FPS}fps${CHROME ? `  [${CHROME.split("/").pop()}]` : ""}`);

const browser = await puppeteer.launch({
  headless: flags.headful ? false : "new",
  ...(CHROME ? { executablePath: CHROME } : {}),
  args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required",
    "--use-gl=angle", "--use-angle=metal", "--enable-gpu", "--ignore-gpu-blocklist",
    "--disable-background-timer-throttling", "--disable-backgrounding-occluded-windows",
    "--disable-renderer-backgrounding", "--disable-features=CalculateNativeWinOcclusion",
    `--window-size=${CAP_W},${CAP_H}`],
});

const page = await browser.newPage();
await page.setViewport({ width: CAP_W, height: CAP_H, deviceScaleFactor: 1 });
page.on("pageerror", (e) => console.log(`  [pageerror] ${e.message.slice(0, 120)}`));
page.on("console", (m) => { const t = m.text(); if (t.startsWith("[av]")) console.log(`  ${t}`); });

// Tee EVERY AudioContext's destination into a per-context capture sink.
await page.evaluateOnNewDocument(() => {
  const OrigCtx = window.AudioContext || window.webkitAudioContext;
  if (!OrigCtx) return;
  window.__acCtxs = [];
  const proto = window.AudioNode && window.AudioNode.prototype;
  if (proto && !proto.__acTee) {
    const origConnect = proto.connect;
    proto.connect = function (dest, ...rest) {
      try {
        const ctx = this.context;
        if (ctx && dest === ctx.destination && ctx.__acCapDest) origConnect.call(this, ctx.__acCapDest);
      } catch (e) {}
      return origConnect.call(this, dest, ...rest);
    };
    proto.__acTee = true;
  }
  const Wrapped = function (...args) {
    const ctx = new OrigCtx(...args);
    try { ctx.__acCapDest = ctx.createMediaStreamDestination(); window.__acCtxs.push(ctx); } catch (e) {}
    return ctx;
  };
  Wrapped.prototype = OrigCtx.prototype;
  window.AudioContext = Wrapped;
  if (window.webkitAudioContext) window.webkitAudioContext = Wrapped;
});

try {
  await page.goto(url, { waitUntil: "networkidle2", timeout: 45000 });
} catch (e) { console.log(`  ⚠ goto: ${e.message.slice(0, 80)} — continuing`); }

await page.evaluate(() => { const c = document.querySelector("canvas"); if (c) { c.focus(); c.click(); } }).catch(() => {});
await page.keyboard.press("Space").catch(() => {});
if (isMerry) {
  await new Promise((r) => setTimeout(r, 1200));
  await page.keyboard.type(PIECE, { delay: 45 });
  await page.keyboard.press("Enter");
}

// Wait for the piece to ACTUALLY paint (not a fixed timer) — some pieces
// (notepat) take ~4s to boot; starting the performance early loses notes on the
// black loading screen. We poll the largest canvas until it's non-black + varied
// and stable, then a short grace. --settle N sets the grace (default 500ms).
async function waitForLoaded(timeout) {
  const start = Date.now(); let ok = 0;
  while (Date.now() - start < timeout) {
    let m = null;
    try {
      m = await page.evaluate(() => {
        // Check EVERY canvas (the WebGL/glaze overlay can read blank via
        // drawImage; the 2D content canvas is the one that shows the piece) and
        // keep the most-content one.
        const s = 48, o = document.createElement("canvas"); o.width = s; o.height = s;
        const octx = o.getContext("2d");
        let best = { varr: 0, frac: 0 };
        for (const c of document.querySelectorAll("canvas")) {
          if (!c.width) continue;
          octx.clearRect(0, 0, s, s);
          try { octx.drawImage(c, 0, 0, s, s); } catch (e) { continue; }
          const d = octx.getImageData(0, 0, s, s).data; let sum = 0, sum2 = 0, nb = 0; const n = s * s;
          for (let i = 0; i < d.length; i += 4) { const l = (d[i] + d[i + 1] + d[i + 2]) / 3; sum += l; sum2 += l * l; if (l > 16) nb++; }
          const mean = sum / n, varr = sum2 / n - mean * mean, frac = nb / n;
          if (frac > best.frac) best = { varr, frac };
        }
        return best;
      });
    } catch (e) {}
    if (m && m.frac > 0.25 && (m.varr > 120 || m.frac > 0.55)) { if (++ok >= 2) return Date.now() - start; } else ok = 0;
    await new Promise((r) => setTimeout(r, 180));
  }
  return -1;
}
const loadedMs = await waitForLoaded(flags["load-timeout"] ? parseInt(flags["load-timeout"], 10) : (isMerry ? 10000 : 13000));
console.log(`  piece ${loadedMs >= 0 ? `loaded in ${(loadedMs / 1000).toFixed(1)}s` : "load not detected — proceeding"}`);
await new Promise((r) => setTimeout(r, SETTLE));

// ── Start the in-page audio-only recorder (records the teed synth output) ────
await page.evaluate(async () => {
  const log = (m) => console.log("[av] " + m);
  const ctxs = window.__acCtxs || [];
  for (const c of ctxs) { if (c.state === "suspended") { try { await c.resume(); } catch (e) {} } }
  const tracks = [];
  ctxs.forEach((c) => { if (c.__acCapDest) tracks.push(...c.__acCapDest.stream.getAudioTracks()); });
  log(`audio: ${ctxs.length} ctx · ${tracks.length} track(s)`);
  const stream = new MediaStream(tracks);
  const mime = ["audio/webm;codecs=opus", "audio/webm"].find((m) => MediaRecorder.isTypeSupported(m)) || "audio/webm";
  const rec = new MediaRecorder(stream, { mimeType: mime, audioBitsPerSecond: 192_000 });
  window.__acChunks = [];
  rec.ondataavailable = (e) => { if (e.data && e.data.size) window.__acChunks.push(e.data); };
  window.__acStop = () => new Promise((res) => { rec.onstop = res; rec.stop(); });
  rec.start(200);
});

// Video half. Two capture methods:
//   screencast (default) — CDP Page.startScreencast, smooth + cheap, but only
//     emits frames when the page REPAINTS (bad for static pieces like line/prompt).
//   --grab — a fixed-interval page.screenshot loop; steady frames regardless of
//     repaint, so multi-phase sessions on static pieces (prompt ↔ line) stay in sync.
const GRAB = flags.grab !== undefined && flags.grab !== false && flags.grab !== "0";
const stamps = [];
const gestures = [];        // {id, t(video s), x, y, kind} for the finger overlay
let wallAtFirstFrame = null; // Date.now() at first frame — the zero for gesture times
let realDuration = 0;

// ── Run the performance (timed key/mouse actions) while capturing ────────────
const perfStart = Date.now();
const elapsed = () => Date.now() - perfStart;
const videoT = () => (Date.now() - (wallAtFirstFrame ?? perfStart)) / 1000; // gesture time in video seconds
let lastPos = { x: 0.5, y: 0.5 };
// Live pointer state — snapshotted into EVERY captured frame so the cursor
// overlay is frame-exact (no time interpolation, immune to polling jitter).
const pointer = { x: 0.5, y: 0.5, down: false, tapUntil: 0 };
const pointerDownNow = () => pointer.down || Date.now() < pointer.tapUntil;
// Serialize ALL page CDP ops (input + screenshots) so grab-mode screenshots
// never overlap a mouse/keyboard event (which would silently drop the input).
let lock = Promise.resolve();
const withLock = (fn) => { const p = lock.then(fn); lock = p.then(() => {}, () => {}); return p; };
async function runPerformance() {
  if (!PERFORM) return;
  for (const act of PERFORM) {
    const wait = act.t - elapsed();
    if (wait > 0) await new Promise((r) => setTimeout(r, wait));
    // set pointer BEFORE the action so the per-frame cursor never lags the render
    if (act.type === "down" || act.type === "mdown") { pointer.x = act.x; pointer.y = act.y; pointer.down = true; }
    else if (act.type === "move") { pointer.x = act.x; pointer.y = act.y; }
    else if (act.type === "tap") { pointer.x = act.x; pointer.y = act.y; pointer.tapUntil = Date.now() + 140; }
    try {
      if (act.type === "down") await withLock(() => page.keyboard.down(act.key));
      else if (act.type === "up") await withLock(() => page.keyboard.up(act.key));
      else if (act.type === "press") await withLock(() => page.keyboard.press(act.key));
      else if (act.type === "tap") await withLock(() => page.mouse.click(Math.round(act.x * CAP_W), Math.round(act.y * CAP_H)));
      else if (act.type === "mdown") await withLock(async () => { await page.mouse.move(Math.round(act.x * CAP_W), Math.round(act.y * CAP_H)); await page.mouse.down(); });
      else if (act.type === "move") await withLock(() => page.mouse.move(Math.round(act.x * CAP_W), Math.round(act.y * CAP_H)));
      else if (act.type === "mup") await withLock(() => page.mouse.up());
      else if (act.type === "type") await withLock(() => page.keyboard.type(act.text, { delay: 40 }));
    } catch (e) {}
    // Record the gesture (in video time) for the finger overlay. `move` while
    // held = a drag: it belongs to its press id and drives the bubble's size.
    if (act.type === "down" || act.type === "mdown") { lastPos = { x: act.x, y: act.y }; gestures.push({ id: act.id, t: videoT(), x: act.x, y: act.y, kind: "down" }); }
    else if (act.type === "tap") { lastPos = { x: act.x, y: act.y }; gestures.push({ id: act.id, t: videoT(), x: act.x, y: act.y, kind: "tap" }); }
    else if (act.type === "move") { lastPos = { x: act.x, y: act.y }; gestures.push({ id: act.id, t: videoT(), x: act.x, y: act.y, kind: "move" }); }
    else if (act.type === "up" || act.type === "mup") { pointer.down = false; gestures.push({ id: act.id, t: videoT(), x: act.x ?? lastPos.x, y: act.y ?? lastPos.y, kind: "up" }); }
  }
}
let client = null;
if (GRAB) {
  // Fixed-interval screenshot loop. Runs the performance concurrently and keeps
  // grabbing until the performance COMPLETES (screenshots + locked input take
  // real time, so a scripted session runs longer than its scheduled duration) —
  // then a short tail of the final state. DURATION*2.5 is just a safety cap.
  const startWall = Date.now();
  wallAtFirstFrame = startWall;
  let perfComplete = false;
  const perfDone = runPerformance().finally(() => { perfComplete = true; });
  const interval = 1000 / FPS;
  const capMs = DURATION * 2.5 * 1000;
  let idx = 0, tailUntil = 0;
  while (true) {
    const nowWall = Date.now() - startWall;
    if (perfComplete) { if (!tailUntil) tailUntil = Date.now() + 900; if (Date.now() > tailUntil) break; }
    if (nowWall > capMs) break;
    const target = startWall + idx * interval;
    const now = Date.now();
    if (now < target) await new Promise((r) => setTimeout(r, target - now));
    const buf = await withLock(() => page.screenshot({ type: "jpeg", quality: 90 })).catch(() => null);
    const t = (Date.now() - startWall) / 1000;
    if (buf) {
      const file = `frame-${String(idx).padStart(5, "0")}.jpg`;
      writeFileSync(`${FRAMES_DIR}/${file}`, buf);
      stamps.push({ file, t, cx: pointer.x, cy: pointer.y, down: pointerDownNow() });
      if (idx % 30 === 0) console.log(`  ${idx} frames · ${t.toFixed(1)}s (grab${perfComplete ? " tail" : ""})`);
    }
    idx++;
  }
  await perfDone.catch(() => {});
  realDuration = stamps.length ? stamps[stamps.length - 1].t : DURATION;
} else {
  // CDP screencast — emits a frame whenever the page paints.
  client = await page.createCDPSession();
  let i = 0, t0 = null, stop = false;
  client.on("Page.screencastFrame", async ({ data, sessionId, metadata }) => {
    client.send("Page.screencastFrameAck", { sessionId }).catch(() => {});
    if (stop) return;
    const ts = metadata.timestamp ?? Date.now() / 1000;
    if (t0 === null) { t0 = ts; wallAtFirstFrame = Date.now(); }
    const t = ts - t0;
    if (t >= DURATION) { stop = true; realDuration = t; return; }
    const file = `frame-${String(i).padStart(5, "0")}.png`;
    writeFileSync(`${FRAMES_DIR}/${file}`, Buffer.from(data, "base64"));
    stamps.push({ file, t, cx: pointer.x, cy: pointer.y, down: pointerDownNow() });
    i++;
    if (i % 60 === 0) console.log(`  ${i} frames · ${t.toFixed(1)}/${DURATION}s`);
  });
  await client.send("Page.startScreencast", { format: "png", everyNthFrame: 1, maxWidth: CAP_W, maxHeight: CAP_H });
  const perfDone = runPerformance();
  const wallStart = Date.now();
  while (!stop && (Date.now() - wallStart) / 1000 < DURATION + 6) await new Promise((r) => setTimeout(r, 100));
  await perfDone.catch(() => {});
  await client.send("Page.stopScreencast").catch(() => {});
  if (!realDuration) realDuration = stamps.length ? stamps[stamps.length - 1].t : DURATION;
}

// Stop audio recorder + read the webm back as base64.
const audioB64 = await page.evaluate(async () => {
  if (!window.__acStop) return null;
  await window.__acStop();
  const blob = new Blob(window.__acChunks, { type: "audio/webm" });
  const buf = new Uint8Array(await blob.arrayBuffer());
  let bin = ""; const CH = 0x8000;
  for (let i = 0; i < buf.length; i += CH) bin += String.fromCharCode.apply(null, buf.subarray(i, i + CH));
  return btoa(bin);
});
await browser.close();

if (!stamps.length) { console.error("✗ no frames captured"); process.exit(1); }
const AUDIO = `${OUT}/audio-${SLUG}.webm`;
const haveAudio = audioB64 && audioB64.length > 100;
if (haveAudio) writeFileSync(AUDIO, Buffer.from(audioB64, "base64"));
console.log(`  ${stamps.length} frames over ${realDuration.toFixed(1)}s · audio ${haveAudio ? (Buffer.from(audioB64, "base64").length / 1e3).toFixed(0) + "KB" : "MISSING"}`);

// ── Assemble the base mp4: true-speed video from frame timestamps + audio ────
// concat demuxer with per-frame durations → correct playback speed.
const list = stamps.map((s, n) => {
  const dur = (n < stamps.length - 1 ? stamps[n + 1].t - s.t : 1 / FPS);
  return `file 'frames/${s.file}'\nduration ${Math.max(dur, 1 / 240).toFixed(4)}`;
}).join("\n") + `\nfile 'frames/${stamps[stamps.length - 1].file}'\n`;
writeFileSync(`${OUT}/frames.txt`, list);

const OUTMP4 = `${OUT}/base-${SLUG}.mp4`;
const vArgs = ["-y", "-f", "concat", "-safe", "0", "-i", `${OUT}/frames.txt`];
const aArgs = haveAudio ? ["-i", AUDIO] : [];
const mapArgs = haveAudio
  ? ["-map", "0:v", "-map", "1:a", "-c:a", "aac", "-b:a", "192k", "-shortest"]
  : ["-map", "0:v"];
const ff = spawnSync("ffmpeg", [...vArgs, ...aArgs,
  "-vsync", "cfr", "-r", String(FPS), "-c:v", "libx264", "-preset", "medium", "-crf", "18",
  "-pix_fmt", "yuv420p", ...mapArgs, "-movflags", "+faststart", OUTMP4], { stdio: ["ignore", "ignore", "inherit"] });
if (ff.status !== 0) { console.error("✗ ffmpeg assemble failed"); process.exit(1); }

const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "stream=codec_type,codec_name",
  "-of", "default=nw=1", OUTMP4], { encoding: "utf8" });
const hasAudio = /codec_type=audio/.test(probe.stdout || "");
gestures.sort((a, b) => a.t - b.t);
writeFileSync(`${OUT}/capture.json`, JSON.stringify({
  piece: PIECE, slug: SLUG, perform: flags.perform || null, isMerry, url,
  duration: DURATION, realDuration, fps: FPS, density: DENSITY, width: CAP_W, height: CAP_H,
  frames: stamps.length, base: OUTMP4, hasAudio, gestures,
  // frame-exact cursor track: one {t, x, y, down} per captured frame
  cursorTrack: stamps.map((s) => ({ t: s.t, x: s.cx, y: s.cy, down: !!s.down })),
}, null, 2));
console.log(`  ${gestures.length} gesture events recorded`);
console.log((hasAudio ? "✓" : "⚠") + ` base → ${OUTMP4}  [audio: ${hasAudio ? "YES" : "MISSING"}]`);
