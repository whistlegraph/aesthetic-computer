#!/usr/bin/env node
// line-vision.mjs — GPT-vision-guided drawing in AC's `line`. Between each
// stroke we screenshot the canvas and ask GPT-4o for the NEXT stroke (color +
// path), so the subject is built adaptively — a closed inference loop. The whole
// session is captured (grab) → base mp4 for a reel, and a final "our-end" GPT
// check rates the result.
//
//   node marketing/av-reels/bin/line-vision.mjs "a sailboat" --strokes 12 --density 6
//   then: node marketing/av-reels/bin/stamp-reel.mjs out/<slug>/base-<slug>.mp4 --title line

import { existsSync, mkdirSync, rmSync, writeFileSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) { const n = argv[i + 1]; if (n !== undefined && !n.startsWith("--")) { flags[a.slice(2)] = n; i++; } else flags[a.slice(2)] = true; }
  else positional.push(a);
}
const SUBJECT = positional[0];
if (!SUBJECT) { console.error('usage: line-vision.mjs "<subject>" [--strokes 12] [--density 6]'); process.exit(1); }
const SLUG = (flags.slug || SUBJECT).replace(/[^a-z0-9_-]+/gi, "-").replace(/^-+|-+$/g, "").toLowerCase();
const MAX_STROKES = parseInt(flags.strokes || 16, 10);
const MIN_STROKES = parseInt(flags["min-strokes"] || 6, 10);
const THRESHOLD = parseFloat(flags.score || 7); // keep drawing until critique >= this
const DENSITY = String(flags.density || 6);
const FPS = parseInt(flags.fps || 30, 10); // output fps (screencast source is higher)
const CAP_W = 1080, CAP_H = 1920;
const MODEL = flags.model || "gpt-4o";

const OUT = resolve((flags.out || `${LANE}/out/${SLUG}`).replace(/^~/, process.env.HOME));
rmSync(OUT, { recursive: true, force: true });
const FRAMES_DIR = `${OUT}/frames`;
mkdirSync(FRAMES_DIR, { recursive: true });

// ── OpenAI key (vault) ──────────────────────────────────────────────────────
function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) for (const l of readFileSync(vault, "utf8").split("\n")) if (l.startsWith("OPENAI_API_KEY=")) return l.slice(15).trim().replace(/^['"]|['"]$/g, "");
  throw new Error("OPENAI_API_KEY not found");
}
const KEY = loadKey();

const PALETTE = "red orange yellow green cyan blue purple white gray black";
const SYS = `You are drawing "${SUBJECT}" on a dark gray canvas by composing SHAPES, one per step. Coordinates are normalized 0..1 (x = left→right, y = top→bottom). Keep everything within x 0.12..0.88 and y 0.14..0.9. cx/cy are centers; r/rx/w are fractions of the frame WIDTH; circles are auto-corrected to look round.
Look at the current canvas and output ONE shape as STRICT JSON (no prose). Choose the type:
  {"shape":"circle","cx":C,"cy":C,"r":R,"color":"<${PALETTE} or r,g,b>","thickness":1..8,"label":".."}
  {"shape":"ellipse","cx":C,"cy":C,"rx":R,"ry":R,"color":..,"thickness":..,"label":".."}
  {"shape":"polyline","points":[[x,y],...2..14],"color":..,"thickness":..,"label":".."}   // lines & curves
  {"shape":"rect","x":X,"y":Y,"w":W,"h":H,"color":..,"thickness":..,"label":".."}
  {"shape":"triangle","points":[[x,y],[x,y],[x,y]],"color":..,"thickness":..,"label":".."}
  {"shape":"fill","at":[x,y],"color":"..","label":".."}   // FLOOD-FILL solid color inside an already-ENCLOSED region
Strategy: draw big OUTLINE shapes first (circle/ellipse/rect for the body), then FILL each enclosed region with solid color, then add smaller detail shapes. Prefer filled shapes over many thin lines.
When the drawing clearly reads as "${SUBJECT}", respond exactly: {"done":true}`;

// OpenAI chat call with retry on transient errors (rate limits, 5xx, network).
async function callOpenAI(body, attempts = 5) {
  let lastErr;
  for (let a = 0; a < attempts; a++) {
    try {
      const r = await fetch("https://api.openai.com/v1/chat/completions", { method: "POST", headers: { Authorization: `Bearer ${KEY}`, "Content-Type": "application/json" }, body: JSON.stringify(body) });
      const j = await r.json();
      if (j.error) throw new Error(j.error.message);
      return j.choices[0].message.content;
    } catch (e) { lastErr = e; await new Promise((r) => setTimeout(r, 1000 * (a + 1))); }
  }
  throw lastErr;
}
function extractJSON(txt) {
  const clean = txt.trim().replace(/^```json\s*|^```\s*|\s*```$/g, "");
  const m = clean.match(/\{[\s\S]*\}/);
  return JSON.parse(m ? m[0] : clean);
}

async function gptNextStroke(imgB64, history, critique, force = false) {
  const fb = critique && critique.missing ? `A critic scored the drawing ${critique.score ?? "?"}/10 and said it is missing: ${critique.missing}. ` : "";
  const forceLine = force ? "The drawing is NOT finished — do NOT respond done; output a stroke that adds the single most important missing element." : "";
  const body = {
    model: MODEL, max_tokens: 400, temperature: 0.4,
    messages: [
      { role: "system", content: SYS },
      { role: "user", content: [
        { type: "text", text: `Strokes so far: ${history.length ? history.join("; ") : "(none)"}. ${fb}${forceLine} Give the next stroke (or done).` },
        { type: "image_url", image_url: { url: `data:image/png;base64,${imgB64}`, detail: "low" } },
      ] },
    ],
  };
  for (let a = 0; a < 3; a++) {
    const txt = await callOpenAI(body);
    try { return extractJSON(txt); } catch (e) { if (a === 2) throw new Error("bad JSON: " + txt.slice(0, 100)); }
  }
}

async function gptCritique(imgB64) {
  const body = { model: MODEL, max_tokens: 200, messages: [{ role: "user", content: [
    { type: "text", text: `This drawing was meant to be "${SUBJECT}". Rate how well it reads as that on a 1-10 scale and name in one short sentence what's missing. JSON: {"score":N,"missing":"..."}` },
    { type: "image_url", image_url: { url: `data:image/png;base64,${imgB64}`, detail: "low" } },
  ] }] };
  try { return extractJSON(await callOpenAI(body)); } catch (e) { return { score: 5, missing: String(e.message).slice(0, 100) }; }
}

// ── browser ─────────────────────────────────────────────────────────────────
const PUP = [`${REPO}/node_modules/puppeteer`, `${REPO}/oven/node_modules/puppeteer`].find((p) => existsSync(p));
const puppeteer = (await import(`${PUP}/lib/esm/puppeteer/puppeteer.js`)).default;
const CHROME = ["/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"].find((p) => existsSync(p));
const BASE = flags.base || "https://aesthetic.computer";

console.log(`▸ line-vision "${SUBJECT}" · up to ${MAX_STROKES} strokes · ${MODEL} · density ${DENSITY}`);
const browser = await puppeteer.launch({
  headless: flags.headful ? false : "new", ...(CHROME ? { executablePath: CHROME } : {}),
  args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required", "--use-gl=angle", "--use-angle=metal", "--enable-gpu", "--ignore-gpu-blocklist",
    "--disable-background-timer-throttling", "--disable-renderer-backgrounding", `--window-size=${CAP_W},${CAP_H}`],
});
const page = await browser.newPage();
await page.setViewport({ width: CAP_W, height: CAP_H, deviceScaleFactor: 1 });
page.on("pageerror", (e) => console.log(`  [pageerror] ${e.message.slice(0, 100)}`));

// Tee every AudioContext's output into a capture sink so we can record AC's
// interface sounds (typing clicks, the line chirps) — same trick as capture-av.
await page.evaluateOnNewDocument(() => {
  const OrigCtx = window.AudioContext || window.webkitAudioContext;
  if (!OrigCtx) return;
  window.__acCtxs = [];
  const proto = window.AudioNode && window.AudioNode.prototype;
  if (proto && !proto.__acTee) {
    const origConnect = proto.connect;
    proto.connect = function (dest, ...rest) {
      try { const c = this.context; if (c && dest === c.destination && c.__acCapDest) origConnect.call(this, c.__acCapDest); } catch (e) {}
      return origConnect.call(this, dest, ...rest);
    };
    proto.__acTee = true;
  }
  const Wrapped = function (...args) { const ctx = new OrigCtx(...args); try { ctx.__acCapDest = ctx.createMediaStreamDestination(); window.__acCtxs.push(ctx); } catch (e) {} return ctx; };
  Wrapped.prototype = OrigCtx.prototype;
  window.AudioContext = Wrapped; if (window.webkitAudioContext) window.webkitAudioContext = Wrapped;
});

await page.goto(`${BASE}/prompt?nogap&density=${DENSITY}`, { waitUntil: "networkidle2", timeout: 45000 }).catch(() => {});
await page.evaluate(() => { const c = document.querySelector("canvas"); if (c) { c.focus(); c.click(); } }).catch(() => {});
await page.keyboard.press("Space").catch(() => {});
await new Promise((r) => setTimeout(r, 2600));

// start the in-page audio-only recorder over the teed interface sound
await page.evaluate(async () => {
  const ctxs = window.__acCtxs || [];
  for (const c of ctxs) { if (c.state === "suspended") { try { await c.resume(); } catch (e) {} } }
  const tracks = []; ctxs.forEach((c) => { if (c.__acCapDest) tracks.push(...c.__acCapDest.stream.getAudioTracks()); });
  const stream = new MediaStream(tracks);
  const mime = ["audio/webm;codecs=opus", "audio/webm"].find((m) => MediaRecorder.isTypeSupported(m)) || "audio/webm";
  const rec = new MediaRecorder(stream, { mimeType: mime, audioBitsPerSecond: 192_000 });
  window.__acRec = rec;
  window.__acChunks = [];
  rec.ondataavailable = (e) => { if (e.data && e.data.size) window.__acChunks.push(e.data); };
  window.__acStop = () => new Promise((res) => { rec.onstop = res; rec.stop(); });
  rec.start(200);
});
// pause/resume the recorder so the audio spans only the ACTIVE drawing time
// (matching the gap-trimmed video) — otherwise the sound drifts out of sync.
const audioPause = () => page.evaluate(() => { try { if (window.__acRec && window.__acRec.state === "recording") window.__acRec.pause(); } catch (e) {} }).catch(() => {});
const audioResume = () => page.evaluate(() => { try { if (window.__acRec && window.__acRec.state === "paused") window.__acRec.resume(); } catch (e) {} }).catch(() => {});

// serialize all page CDP ops (screenshots + input)
let lock = Promise.resolve();
const withLock = (fn) => { const p = lock.then(fn); lock = p.then(() => {}, () => {}); return p; };
// PNG frames = lossless (crisp chunky pixels, no JPEG mud on the block edges)
const shot = () => withLock(() => page.screenshot({ type: "png" }));

// ── continuous capture (grab) in the background ─────────────────────────────
const stamps = [];
const gestures = [];
const pointer = { x: 0.5, y: 0.5, down: false }; // snapshotted per frame → exact cursor
// Capture CLOCK that only advances while actively drawing — paused during GPT
// calls so the reel has no dead "thinking" time, and frames/cursor stay in sync.
let capAccum = 0, capBase = Date.now(), capPaused = false;
const videoT = () => (capAccum + (capPaused ? 0 : Date.now() - capBase)) / 1000;
const pauseCap = () => { if (!capPaused) { capAccum += Date.now() - capBase; capPaused = true; audioPause(); } };
const resumeCap = () => { if (capPaused) { capBase = Date.now(); capPaused = false; audioResume(); } };
let capturing = true, fidx = 0;
// CDP screencast: streams a frame on every PAINT (up to ~60fps while drawing/
// typing) → smooth 30fps output, far beyond page.screenshot's ~7fps. Frames are
// dropped while the clock is paused (GPT thinking, where the screen is static).
const client = await page.createCDPSession();
client.on("Page.screencastFrame", ({ data, sessionId }) => {
  client.send("Page.screencastFrameAck", { sessionId }).catch(() => {});
  if (!capturing || capPaused) return;
  const file = `frame-${String(fidx).padStart(6, "0")}.png`;
  writeFileSync(`${FRAMES_DIR}/${file}`, Buffer.from(data, "base64"));
  stamps.push({ file, t: videoT(), cx: pointer.x, cy: pointer.y, down: pointer.down });
  fidx++;
});
await client.send("Page.startScreencast", { format: "png", everyNthFrame: 1, maxWidth: CAP_W, maxHeight: CAP_H });

// Glide the (raised) pointer smoothly to a target with easing — the per-frame
// cursor records this so the reticle ANIMATES to each point instead of jumping.
async function glideTo(tx, ty, ms = 480) {
  const sx = pointer.x, sy = pointer.y;
  const dist = Math.hypot(tx - sx, ty - sy);
  const dur = Math.max(200, Math.min(ms, 300 + dist * 900)); // longer for bigger hops
  const steps = Math.max(4, Math.round(dur / 40));
  pointer.down = false;
  for (let s = 1; s <= steps; s++) {
    const u = s / steps, e = u < 0.5 ? 2 * u * u : 1 - Math.pow(-2 * u + 2, 2) / 2;
    const x = sx + (tx - sx) * e, y = sy + (ty - sy) * e;
    pointer.x = x; pointer.y = y;
    await withLock(() => page.mouse.move(Math.round(x * CAP_W), Math.round(y * CAP_H)));
    await new Promise((r) => setTimeout(r, 38));
  }
}

// draw a stroke as a continuous drag, recording gestures
async function drawStroke(pts) {
  const id = Date.now();
  const clamp = (v) => Math.max(0.04, Math.min(0.96, v));
  const P = pts.map(([x, y]) => [clamp(x), clamp(y)]);
  // glide to the start point first (visible travel), then press down
  await glideTo(P[0][0], P[0][1]);
  pointer.x = P[0][0]; pointer.y = P[0][1]; pointer.down = true;
  await withLock(async () => { await page.mouse.move(Math.round(P[0][0] * CAP_W), Math.round(P[0][1] * CAP_H)); await page.mouse.down(); });
  gestures.push({ id, t: videoT(), x: P[0][0], y: P[0][1], kind: "down" });
  for (let i = 0; i < P.length - 1; i++) {
    const steps = Math.max(3, Math.round(Math.hypot(P[i + 1][0] - P[i][0], P[i + 1][1] - P[i][1]) * 40));
    for (let s = 1; s <= steps; s++) {
      const u = s / steps, x = P[i][0] + (P[i + 1][0] - P[i][0]) * u, y = P[i][1] + (P[i + 1][1] - P[i][1]) * u;
      pointer.x = x; pointer.y = y;
      await withLock(() => page.mouse.move(Math.round(x * CAP_W), Math.round(y * CAP_H)));
      gestures.push({ id, t: videoT(), x, y, kind: "move" });
      await new Promise((r) => setTimeout(r, 24));
    }
  }
  await withLock(() => page.mouse.up());
  pointer.down = false;
  gestures.push({ id, t: videoT(), x: P[P.length - 1][0], y: P[P.length - 1][1], kind: "up" });
}

// Turn a GPT shape into a closed/open point path (WE tessellate — reliable
// geometry instead of GPT guessing coordinates). r/rx/w are width fractions;
// circle y-radius is aspect-corrected so it reads round on the 9:16 frame.
const AR = CAP_W / CAP_H; // 0.5625
function shapeToPath(s) {
  const t = (s.shape || "polyline").toLowerCase();
  if (t === "circle" || t === "ellipse") {
    const cx = s.cx, cy = s.cy, rx = s.r ?? s.rx ?? 0.15, ry = (s.r != null ? s.r * AR : (s.ry ?? 0.1));
    const p = []; const N = 30;
    for (let i = 0; i <= N; i++) { const a = (i / N) * Math.PI * 2; p.push([cx + Math.cos(a) * rx, cy + Math.sin(a) * ry]); }
    return p;
  }
  if (t === "rect") { const { x, y, w, h } = s; return [[x, y], [x + w, y], [x + w, y + h], [x, y + h], [x, y]]; }
  if (t === "triangle" && Array.isArray(s.points)) { return [...s.points, s.points[0]]; }
  if (Array.isArray(s.points)) return s.points;
  return [];
}

// fill: the flood only fires while pen.drawing is true (disk.mjs:13962), so a
// static click does nothing — we need a small DRAG at the point to trigger it.
async function fillAt([x, y]) {
  const cx = Math.max(0.04, Math.min(0.96, x)), cy = Math.max(0.04, Math.min(0.96, y));
  await glideTo(cx, cy); // visible travel to the fill point
  const px = Math.round(cx * CAP_W), py = Math.round(cy * CAP_H);
  const id = Date.now();
  pointer.x = cx; pointer.y = cy; pointer.down = true;
  gestures.push({ id, t: videoT(), x: cx, y: cy, kind: "down" });
  await withLock(async () => { await page.mouse.move(px, py); await page.mouse.down(); await page.mouse.move(px + 7, py + 5); });
  await new Promise((r) => setTimeout(r, 90));
  await withLock(() => page.mouse.move(px - 4, py - 3));
  gestures.push({ id, t: videoT(), x: cx, y: cy, kind: "move" });
  await new Promise((r) => setTimeout(r, 90));
  await withLock(() => page.mouse.up());
  pointer.down = false;
  gestures.push({ id, t: videoT(), x: cx, y: cy, kind: "up" });
}

// Go back to the prompt as a VISIBLE gesture (tutorial-like): glide the cursor
// up to the top-left piece label — AC's back affordance — tap it, then drift up.
// Escape is a silent safety net in case the label hitbox is missed.
async function backToPrompt() {
  const lx = 0.075, ly = 0.035;
  await glideTo(lx, ly);
  pointer.down = true;
  await withLock(() => page.mouse.click(Math.round(lx * CAP_W), Math.round(ly * CAP_H)));
  await new Promise((r) => setTimeout(r, 150));
  pointer.down = false;
  await new Promise((r) => setTimeout(r, 250));
  await withLock(() => page.keyboard.press("Escape"));
  commandTrack.push({ t: videoT(), cmd: "prompt", color: null, tool: "prompt" }); // back at prompt
  await glideTo(lx + 0.02, 0.012, 280); // drift up
}

// ── inference loop ──────────────────────────────────────────────────────────
const history = [];
const decisions = [];
const commandTrack = [];  // {t, cmd, color, tool} each time the active command changes → side-stamp flash
let critique = { score: 0, missing: "" };   // closed loop: guides + gates each stroke
for (let i = 0; i < MAX_STROKES; i++) {
  pauseCap(); // don't record the GPT "thinking" time
  const b64 = (await shot()).toString("base64");
  let stroke;
  try { stroke = await gptNextStroke(b64, history, critique, false); }
  catch (e) { console.log(`  ⚠ gpt error: ${e.message}`); resumeCap(); break; }
  if (stroke.done) {
    if (history.length >= MIN_STROKES && critique.score >= THRESHOLD) { console.log(`  ✓ done — score ${critique.score}/10 after ${history.length} strokes`); resumeCap(); break; }
    console.log(`  ↺ GPT said done but score ${critique.score}/10 < ${THRESHOLD} (or < ${MIN_STROKES} strokes) — pushing for more`);
    try { stroke = await gptNextStroke(b64, history, critique, true); } catch (e) { resumeCap(); break; }
    if (stroke.done) { console.log(`  GPT insists done`); resumeCap(); break; }
  }
  resumeCap(); // resume the clock for the actual drawing
  const isFill = (stroke.shape || stroke.tool) === "fill";
  const th = Math.max(1, Math.min(8, parseInt(stroke.thickness) || 2));
  const color = String(stroke.color || "white").replace(/[^a-z0-9,]/gi, "");
  const path = isFill ? null : shapeToPath(stroke);
  if (!isFill && (!path || path.length < 2)) { console.log(`  ⚠ empty shape (${stroke.shape}), skipping`); continue; }
  const cmd = isFill ? `fill ${color}` : `line:${th} ${color}`;
  console.log(`  [${i + 1}] ${stroke.label} — ${cmd} [${stroke.shape || "polyline"}]`);
  decisions.push({ i, ...stroke });
  // type the command at the prompt — slowly, so it reads as real typing
  await withLock(() => page.keyboard.press("ArrowLeft"));
  await withLock(() => page.keyboard.type(cmd, { delay: 105 }));
  await new Promise((r) => setTimeout(r, 250));
  await withLock(() => page.keyboard.press("Enter"));
  await new Promise((r) => setTimeout(r, 1500)); // let the tool load
  commandTrack.push({ t: videoT(), cmd, color, tool: isFill ? "fill" : "line" }); // command now active → side stamps flash
  if (isFill) { await fillAt(stroke.at || [stroke.cx ?? 0.5, stroke.cy ?? 0.5]); } else { await drawStroke(path); }
  await new Promise((r) => setTimeout(r, 350));
  await backToPrompt(); // visible: glide to the top-left label, tap it, drift up
  await new Promise((r) => setTimeout(r, 700));
  history.push(`${stroke.label} (${stroke.shape || (isFill ? "fill" : "line")})`);
  // ── critique the result → guide the next stroke + decide whether to stop ──
  pauseCap();
  const cb = (await shot()).toString("base64");
  critique = await gptCritique(cb);
  resumeCap();
  console.log(`      ↳ ${critique.score}/10 · missing: ${critique.missing}`);
  if (history.length >= MIN_STROKES && critique.score >= THRESHOLD) { console.log(`  ✓ reached score ${critique.score}/10`); break; }
}

capturing = false;
await client.send("Page.stopScreencast").catch(() => {});
await new Promise((r) => setTimeout(r, 300));
// stop the audio recorder + read the teed interface sound back as base64
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

// ── assemble base mp4: PNG frames (lossless, crisp) + AC interface audio ─────
const realDuration = stamps.length ? stamps[stamps.length - 1].t : 0;
const list = stamps.map((s, n) => `file 'frames/${s.file}'\nduration ${Math.max(1 / 240, (n < stamps.length - 1 ? stamps[n + 1].t - s.t : 1 / FPS)).toFixed(4)}`).join("\n") + `\nfile 'frames/${stamps[stamps.length - 1].file}'\n`;
writeFileSync(`${OUT}/frames.txt`, list);
const AUDIO = `${OUT}/audio-${SLUG}.webm`;
const haveAudio = audioB64 && audioB64.length > 100;
if (haveAudio) writeFileSync(AUDIO, Buffer.from(audioB64, "base64"));
console.log(`  audio ${haveAudio ? (Buffer.from(audioB64, "base64").length / 1e3).toFixed(0) + "KB" : "MISSING"}`);
const OUTMP4 = `${OUT}/base-${SLUG}.mp4`;
// crf 14 = higher quality (stamp re-encodes once more, so keep the base clean)
spawnSync("ffmpeg", ["-y", "-f", "concat", "-safe", "0", "-i", `${OUT}/frames.txt`,
  ...(haveAudio ? ["-i", AUDIO] : []),
  "-vsync", "cfr", "-r", String(FPS), "-c:v", "libx264", "-preset", "medium", "-crf", "14", "-pix_fmt", "yuv420p",
  // dynaudnorm lifts the sparse interface sounds so they're clearly audible
  ...(haveAudio ? ["-map", "0:v", "-map", "1:a", "-af", "dynaudnorm=p=0.9:m=10:f=250:g=15", "-c:a", "aac", "-b:a", "192k", "-shortest"] : []),
  "-movflags", "+faststart", OUTMP4], { stdio: ["ignore", "ignore", "inherit"] });

gestures.sort((a, b) => a.t - b.t);
writeFileSync(`${OUT}/capture.json`, JSON.stringify({ subject: SUBJECT, slug: SLUG, density: DENSITY, fps: FPS, width: CAP_W, height: CAP_H, frames: stamps.length, realDuration, base: OUTMP4, gestures, decisions, critique, commandTrack, cursorTrack: stamps.map((s) => ({ t: s.t, x: s.cx, y: s.cy, down: !!s.down })) }, null, 2));
console.log(`✓ ${stamps.length} frames · ${realDuration.toFixed(1)}s · ${decisions.length} strokes → ${OUTMP4}`);
console.log(`  next: node marketing/av-reels/bin/stamp-reel.mjs ${OUTMP4} --title line`);
