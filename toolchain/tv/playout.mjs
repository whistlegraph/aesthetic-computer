#!/usr/bin/env node
// playout.mjs — Whistlegraph TV, composed by ffmpeg instead of a browser.
//
// The browser rig filmed Chrome playing the archive; that meant grabbing
// video off Xvfb and audio off a separate PulseAudio sink — two clocks
// that drift, forever chasing an offset dial. But every whistlegraph is a
// finished mp4 whose A/V is already locked. Stream the file itself and
// sync is correct by construction: no screen grab, no second clock, no
// AV_OFFSET. This is that playout.
//
// Shape: a persistent CONSUMER ffmpeg holds the RTMP connection and just
// muxes an mpegts stream to FLV. A PRODUCER loop encodes one clip at a
// time — padded to 16:9 on the room color, label burned top-left, QR
// bottom-right — as constant-parameter mpegts piped into the consumer.
// Clip boundaries are mux joins, never RTMP reconnects, so the stream
// never blinks.
//
//   node playout.mjs <env-file>
//
// Env (0600):
//   RTMP_URL, STREAM_KEY          the ingest (from live.mjs ensure-stream)
//   CURATION=/home/jas/wgtv/curation.json
//   QR_DIR=/home/jas/wgtv/qr      pre-baked <code>.ppm (make-qrs.mjs)
//   CDN=https://assets.aesthetic.computer/whistlegraph/index
//   SIZE=1920x1080  FPS=30  VBITRATE=4500k  DWELL=180  FONT=<ttf path>
import { readFileSync, existsSync } from "node:fs";
import { spawn } from "node:child_process";

const ENV_FILE = process.argv[2];
if (!ENV_FILE) { console.error("usage: playout.mjs <env-file>"); process.exit(1); }
for (const line of readFileSync(ENV_FILE, "utf8").split("\n")) {
  const m = line.match(/^([A-Z_]+)=(.*)$/);
  if (m && !process.env[m[1]]) process.env[m[1]] = m[2];
}
const E = process.env;
const RTMP = `${E.RTMP_URL}/${E.STREAM_KEY}`;
const CDN = E.CDN || "https://assets.aesthetic.computer/whistlegraph/index";
const QR_DIR = E.QR_DIR || `${E.HOME}/wgtv/qr`;
const [W, H] = (E.SIZE || "1920x1080").split("x").map(Number);
const FPS = Number(E.FPS || 30);
const VBITRATE = E.VBITRATE || "4500k";
const DWELL = Number(E.DWELL || 180);
const FONT = E.FONT || "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf";
const ROOM = "0x16122a"; // whistlegraph.org's after-dark room
const log = (...a) => console.log(`[playout ${new Date().toISOString().slice(11, 19)}]`, ...a);

// ── the channel: curated works, views-weighted shuffle (as the browser) ──
// Fetch the live curation so each restart refreshes the channel; fall back
// to the local snapshot if the API is unreachable at boot.
const CURATION_URL = E.CURATION_URL ||
  "https://tv.whistlegraph.org/api/whistlegraph-admin?action=curation";
let curation;
try {
  const res = await fetch(CURATION_URL, { signal: AbortSignal.timeout(15000) });
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  curation = await res.json();
  log(`curation fetched live (${Object.keys(curation.works || {}).length} works)`);
} catch (err) {
  curation = JSON.parse(readFileSync(E.CURATION, "utf8"));
  log(`curation from local snapshot (${err.message})`);
}
const WORKS = Object.entries(curation.works).map(([code, w]) => ({
  code: w.asset || code,
  title: w.title || code,
  by: w.by || "",
  views: w.views || 0,
}));
const dead = new Set();
function nextWork(prev) {
  const live = WORKS.filter((w) => !dead.has(w.code) && w.code !== prev);
  const pool = live.length ? live : WORKS.filter((w) => !dead.has(w.code));
  if (!pool.length) return null;
  // Weight by log(views) so the classics surface more without starving the rest.
  const weights = pool.map((w) => 1 + Math.log10(1 + w.views));
  let r = weights.reduce((a, b) => a + b, 0) * pseudoRandom();
  for (let i = 0; i < pool.length; i++) { r -= weights[i]; if (r <= 0) return pool[i]; }
  return pool[pool.length - 1];
}
// Deterministic-free variety without Math.random (fine here; not a workflow).
let seed = Date.now() % 2147483647;
function pseudoRandom() { seed = (seed * 48271) % 2147483647; return seed / 2147483647; }

// drawtext is a colon/backslash minefield — escape the metacharacters.
const esc = (s) =>
  String(s).replace(/\\/g, "\\\\").replace(/:/g, "\\:").replace(/'/g, "\u2019")
    .replace(/%/g, "\\%").replace(/\n/g, " ").slice(0, 60);

function filterGraph(w) {
  // Portrait rips → fit inside 16:9 on the room color, like the browser TV.
  const pad = `scale=${W}:${H}:force_original_aspect_ratio=decrease,` +
    `pad=${W}:${H}:(ow-iw)/2:(oh-ih)/2:color=${ROOM},setsar=1,fps=${FPS}`;
  const shadow = "shadowcolor=black@0.85:shadowx=2:shadowy=2";
  const code = `drawtext=fontfile=${FONT}:text='[${esc(w.code)}]':x=28:y=24:` +
    `fontsize=44:fontcolor=0xb44887:${shadow}`;
  const title = `drawtext=fontfile=${FONT}:text='${esc(w.title)}':x=28:y=78:` +
    `fontsize=32:fontcolor=0xfffdf6:${shadow}`;
  const meta = `drawtext=fontfile=${FONT}:text='${esc(w.by)}${w.views ? "  \u00b7  " + fmtViews(w.views) + " views" : ""}':` +
    `x=28:y=120:fontsize=20:fontcolor=0xfffdf6@0.8:${shadow}`;
  return { pad, chrome: [code, title, meta].join(",") };
}
const fmtViews = (v) => v >= 1e6 ? `${(v / 1e6).toFixed(1)}M` : v >= 1e3 ? `${Math.round(v / 1e3)}K` : `${v}`;

// ── consumer: the one long-lived RTMP connection ──────────────────────
// TEST_OUT writes to a file and stops after TEST_CLIPS clips — a smoke
// test of the exact producer/consumer path without touching the channel.
const TEST_OUT = E.TEST_OUT || null;
const TEST_CLIPS = Number(E.TEST_CLIPS || 2);
let clipsPlayed = 0;
// discardcorrupt drops the packets straddling a clip join (a clean lost
// frame at a cut) instead of forwarding a glitch; +igndts smooths the seam.
const inFlags = ["-fflags", "+discardcorrupt+igndts", "-f", "mpegts", "-i", "pipe:0"];
const consumerArgs = TEST_OUT
  ? ["-hide_banner", "-loglevel", "warning", ...inFlags, "-c", "copy", "-y", TEST_OUT]
  : ["-hide_banner", "-loglevel", "warning", ...inFlags, "-c", "copy",
     "-f", "flv", "-flvflags", "no_duration_filesize", RTMP];
const consumer = spawn("ffmpeg", consumerArgs, { stdio: ["pipe", "inherit", "inherit"] });
consumer.on("exit", (c) => { log(`consumer exited (${c}) — ending playout`); process.exit(1); });
log(`consumer up → ${E.RTMP_URL}/(key)`);

// ── producer loop: one clip → mpegts → consumer.stdin ─────────────────
// Each producer's mpegts starts its own PTS at 0, so without help the
// consumer sees the clock jump backward at every join. -re paces at 1×,
// so stamping each clip with output_ts_offset = wall-seconds-since-start
// keeps the piped timeline monotonic across the whole broadcast.
let prev = null;
let startMs = 0;
function playNext() {
  const w = nextWork(prev);
  if (!w) { log("no live works — retrying in 10s"); return setTimeout(playNext, 10000); }
  if (!startMs) startMs = Date.now();
  const tsOffset = ((Date.now() - startMs) / 1000).toFixed(3);
  prev = w.code;
  const { pad, chrome } = filterGraph(w);
  const qr = `${QR_DIR}/${w.code}.ppm`;
  const hasQR = existsSync(qr);
  // [0:v] padded+chromed; [1:v] QR scaled to 150px; overlaid bottom-right.
  // A newly-curated work with no pre-baked QR still airs, just codeless.
  const filter = hasQR
    ? `[0:v]${pad},${chrome}[base];[1:v]scale=150:150[qr];[base][qr]overlay=W-w-22:H-h-22[v]`
    : `[0:v]${pad},${chrome}[v]`;
  const args = [
    "-hide_banner", "-loglevel", "error",
    "-re", "-i", `${CDN}/${w.code}.mp4`,
    ...(hasQR ? ["-i", qr] : []),
    "-filter_complex", filter,
    "-map", "[v]", "-map", "0:a?",
    "-t", String(DWELL),
    "-c:v", "libx264", "-preset", "veryfast", "-pix_fmt", "yuv420p",
    "-b:v", VBITRATE, "-maxrate", VBITRATE, "-bufsize", "9000k",
    "-g", String(FPS * 2), "-keyint_min", String(FPS * 2), "-sc_threshold", "0",
    "-c:a", "aac", "-b:a", "160k", "-ar", "48000", "-ac", "2",
    "-shortest",
    "-output_ts_offset", tsOffset, "-muxdelay", "0", "-muxpreload", "0",
    // Each clip is its own mpegts; resend PAT/PMT (and stamp them on frames)
    // so the consumer re-establishes PSI at every join instead of reading
    // the boundary as a corrupt packet.
    "-mpegts_flags", "+resend_headers+pat_pmt_at_frames",
    "-f", "mpegts", "pipe:1",
  ];
  log(`▶ [${w.code}] ${w.title}`);
  const prod = spawn("ffmpeg", args, { stdio: ["ignore", "pipe", "inherit"] });
  prod.stdout.pipe(consumer.stdin, { end: false });
  let bytes = 0;
  prod.stdout.on("data", (d) => (bytes += d.length));
  prod.on("exit", (code) => {
    if (bytes < 100000) { // clip produced nothing → treat as dead (missing twin, etc.)
      log(`✗ [${w.code}] produced ${bytes}B (code ${code}) — marking dead`);
      dead.add(w.code);
    }
    if (TEST_OUT && ++clipsPlayed >= TEST_CLIPS) {
      log(`test: ${clipsPlayed} clips → ${TEST_OUT}; closing`);
      consumer.stdin.end();
      return;
    }
    setImmediate(playNext);
  });
}
playNext();
