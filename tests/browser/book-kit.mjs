// book-kit, 2026.11.18
// The parts every "book" browser test shares: Aesthetic Spells
// (spellbook.e2e.mjs) and the No Paint brushbook (brushbook.e2e.mjs).
// A book test drives a real page, judges little, and writes down what it saw
// (results.json, screenshots, clips) for a papers generator to print.
//
// It needs only puppeteer-core, so it can sit next to a test on a fleet
// machine that has no checkout:
//   mkdir -p ~/scratch/book && cd ~/scratch/book && npm i puppeteer-core
//   scp book-kit.mjs <test>.e2e.mjs there, then: node <test>.e2e.mjs
//
// Env: CHROME (path), OUT (default ./out), VIDEO=1 for a clip of every case
//      (needs ffmpeg; FFMPEG overrides its path), W, H, DSF for the viewport.

import puppeteer from "puppeteer-core";
import { createServer } from "node:http";
import { createReadStream, mkdirSync, statSync, writeFileSync } from "node:fs";
import { extname, join, normalize, resolve, sep } from "node:path";

export const CHROME = process.env.CHROME || "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
export const OUT = process.env.OUT || "out";
export const VIDEO = process.env.VIDEO === "1";
export const FFMPEG = process.env.FFMPEG || "/opt/homebrew/bin/ffmpeg";
export const wait = (ms) => new Promise((r) => setTimeout(r, ms));
export const slug = (s) => s.replace(/[^a-z0-9]+/gi, "-").replace(/^-|-$/g, "").slice(0, 40) || "x";
export const viewport = (w, h, dsf) => ({
  W: parseInt(process.env.W || w, 10), H: parseInt(process.env.H || h, 10), DSF: parseFloat(process.env.DSF || dsf),
});

export function launch({ W, H }, args = []) {
  mkdirSync(OUT, { recursive: true });
  if (VIDEO) { mkdirSync(join(OUT, "video"), { recursive: true }); mkdirSync(join(OUT, "audio"), { recursive: true }); }
  return puppeteer.launch({ executablePath: CHROME, headless: true, args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required",
    "--use-fake-ui-for-media-stream", "--use-fake-device-for-media-stream", `--window-size=${W},${H}`, ...args] });
}

// Tape whatever reaches a Web Audio destination. Media elements that play on
// their own (a radio stream) are not caught. The site may open more than one
// context, so each gets a tape and the loudest one is kept.
function tapeAudio() {
  if (window.top !== window || !window.AudioNode || !window.MediaRecorder) return;
  const state = (window.__bookAudio = { tapes: [], error: null });
  const taps = new WeakMap();
  const connect = AudioNode.prototype.connect;
  AudioNode.prototype.connect = function (dest, ...rest) {
    try {
      if (dest instanceof AudioDestinationNode) {
        const ctx = dest.context;
        let tap = taps.get(ctx);
        if (!tap) {
          tap = ctx.createMediaStreamDestination();
          taps.set(ctx, tap);
          const tape = { blobs: [], started: Date.now() };
          tape.rec = new MediaRecorder(tap.stream, { mimeType: "audio/webm;codecs=opus", audioBitsPerSecond: 96000 });
          tape.rec.ondataavailable = (e) => { if (e.data.size) tape.blobs.push(e.data); };
          tape.rec.start(400);
          state.tapes.push(tape);
        }
        connect.call(this, tap);
      }
    } catch (e) { state.error = String(e); }
    return connect.call(this, dest, ...rest);
  };
}

// A page with its viewport set and everything that went wrong written down.
// `consoleTypes` picks which console messages are kept (errors by default).
export async function openPage(browser, { W, H, DSF }, { context = browser, consoleTypes = ["error"] } = {}) {
  const page = await context.newPage();
  await page.setViewport({ width: W, height: H, deviceScaleFactor: DSF });
  const log = { errors: [], console: [], popups: [], downloads: [], navs: [] };
  if (VIDEO) await page.evaluateOnNewDocument(tapeAudio);
  page.on("pageerror", (e) => log.errors.push(String(e?.message || e).slice(0, 200)));
  page.on("console", (m) => { if (consoleTypes.includes(m.type())) log.console.push(m.text().slice(0, 200)); });
  page.on("popup", (p) => log.popups.push(p?.url?.() || "popup"));
  page.on("framenavigated", (f) => { if (f === page.mainFrame()) log.navs.push(f.url()); });
  const client = await page.createCDPSession();
  await client.send("Browser.setDownloadBehavior", { behavior: "allow", downloadPath: join(process.cwd(), OUT, "downloads"), eventsEnabled: true }).catch(() => {});
  client.on("Browser.downloadWillBegin", (e) => log.downloads.push(e.suggestedFilename));
  return { page, log };
}

export async function shot(page, name, options = { type: "jpeg", quality: 55 }) {
  const file = `${name}.${options.type === "png" ? "png" : "jpg"}`;
  await page.screenshot({ path: join(OUT, file), ...options }).catch(() => {});
  return file;
}

// A clip of one case, with the page's sound taped beside it. Returns the
// fields to merge into the case's result once `stop` has run.
export async function startClip(page, id) {
  if (!VIDEO) return { stop: async () => ({}) };
  const r = {};
  let rec = null;
  try {
    rec = await page.screencast({ path: join(OUT, "video", `${slug(id)}.webm`), ffmpegPath: FFMPEG });
    r.video = `video/${slug(id)}.webm`;
    r.videoStarted = Date.now();
  } catch (e) { r.videoError = String(e?.message || e).slice(0, 160); }
  return {
    async stop() {
      if (!rec) return r;
      // Pull the audio tape out of the page before it closes.
      try {
        const a = await page.evaluate(async () => {
          const s = window.__bookAudio;
          if (!s || !s.tapes.length) return { started: null, error: s?.error || null };
          await Promise.all(s.tapes.map((t) => new Promise((done) => { t.rec.onstop = done; try { t.rec.stop(); } catch { done(); } setTimeout(done, 1500); })));
          let best = null;
          for (const t of s.tapes) { const size = t.blobs.reduce((n, b) => n + b.size, 0); if (!best || size > best.size) best = { t, size }; }
          const buf = new Uint8Array(await new Blob(best.t.blobs).arrayBuffer());
          let bin = ""; for (let i = 0; i < buf.length; i += 32768) bin += String.fromCharCode(...buf.subarray(i, i + 32768));
          return { started: best.t.started, error: s.error, tapes: s.tapes.length, b64: btoa(bin) };
        });
        if (a?.b64) {
          writeFileSync(join(OUT, "audio", `${slug(id)}.webm`), Buffer.from(a.b64, "base64"));
          r.audio = `audio/${slug(id)}.webm`;
          r.audioOffsetMs = a.started - r.videoStarted;
        } else r.audioNote = a?.error || "no Web Audio output was connected";
      } catch (e) { r.audioNote = "page was gone: " + String(e?.message || e).slice(0, 80); }
      await rec.stop().catch(() => {});
      return r;
    },
  };
}

// The first few of everything a page logged, for a case's result.
export const trimLog = (log, keep = 5) => ({ errors: log.errors.slice(0, keep), console: log.console.slice(0, keep),
  popups: log.popups, downloads: log.downloads, navs: log.navs.slice(-4) });

// Results are written after every case, so a run that dies keeps what it had.
export function saveResults(base, results, extra = {}) {
  writeFileSync(join(OUT, "results.json"), JSON.stringify({ base, at: new Date().toISOString(), ...extra, results }, null, 1));
}

// Serve a folder on localhost for a page that is not released yet.
const TYPES = { ".html": "text/html; charset=utf-8", ".js": "text/javascript", ".mjs": "text/javascript", ".json": "application/json",
  ".css": "text/css", ".png": "image/png", ".jpg": "image/jpeg", ".gif": "image/gif", ".svg": "image/svg+xml", ".wasm": "application/wasm",
  ".webm": "audio/webm", ".txt": "text/plain; charset=utf-8", ".ttf": "font/ttf", ".woff": "font/woff", ".woff2": "font/woff2", ".ico": "image/x-icon" };
export function serveFolder(folder) {
  const root = resolve(folder);
  const server = createServer((request, response) => {
    let path;
    try { path = normalize(join(root, decodeURIComponent(new URL(request.url, "http://x").pathname))); }
    catch { response.writeHead(400).end(); return; }
    if (path !== root && !path.startsWith(root + sep)) { response.writeHead(403).end(); return; }
    try { if (statSync(path).isDirectory()) path = join(path, "index.html"); statSync(path); }
    catch { response.writeHead(404).end(); return; }
    response.writeHead(200, { "Content-Type": TYPES[extname(path).toLowerCase()] || "application/octet-stream", "Cache-Control": "no-store" });
    createReadStream(path).pipe(response);
  });
  return new Promise((done) => server.listen(0, "127.0.0.1", () => done({ url: `http://127.0.0.1:${server.address().port}`, close: () => server.close() })));
}

// Keep a page from reaching anything but `base`: no analytics, no saves.
export async function onlyLocal(page, base, blocked = []) {
  await page.setRequestInterception(true);
  page.on("request", (request) => {
    const url = request.url();
    if (url.startsWith(base) || url.startsWith("data:") || url.startsWith("blob:")) return request.continue();
    blocked.push(url.slice(0, 120));
    request.abort();
  });
  return blocked;
}
