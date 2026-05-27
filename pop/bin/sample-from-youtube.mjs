#!/usr/bin/env node
// sample-from-youtube.mjs — download a YouTube source, slice it into
// onset-aligned mono WAV chops, and record the slice in the global
// pop sample index.
//
// Third-party audio is reference material (see pop/references/README.md).
// The downloaded m4a + chop WAVs are gitignored; the per-source
// manifest.json and the global INDEX.json are tracked so anyone with
// the repo can reproduce the chops from the URL.
//
// Usage:
//   node pop/bin/sample-from-youtube.mjs <url> [opts]
//
// Options:
//   --slug <slug>      override slug derived from YT title
//   --min-ms <n>       minimum chop length (default 250)
//   --max-ms <n>       maximum chop length (default 6000)
//   --max-chops <n>    cap total chops, keep loudest (default 0 = no cap)
//   --start <sec>      trim leading seconds from source before chopping
//   --end   <sec>      trim trailing seconds (cut source to this end)
//   --sr <hz>          analysis sample rate (default 22050) — output is 48k
//   --redo             recut even if chops/ already exists
//   --note "<text>"    free-text note saved into manifest
//
// Layout:
//   pop/samples/<slug>/
//     source.m4a           (downloaded, gitignored)
//     source.wav           (48k mono, gitignored, used for cutting)
//     onsets.json          (librosa output, tracked — cheap & reproduces chops)
//     chops/chop-NNN.wav   (gitignored)
//     manifest.json        (tracked)
//   pop/samples/INDEX.json (tracked — aggregates every source)

import { spawnSync } from "node:child_process";
import {
  existsSync, mkdirSync, readdirSync, readFileSync, rmSync,
  unlinkSync, writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE     = dirname(fileURLToPath(import.meta.url));
const POP      = resolve(HERE, "..");
const SAMP     = resolve(POP, "samples");
const INDEX    = resolve(SAMP, "INDEX.json");
const PY_VENV  = resolve(POP, ".venv/bin/python");
const DETECTOR = resolve(POP, "bin/detect_onsets.py");

// ── parse flags ──────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const url  = argv.find((a) => /^https?:/.test(a));
if (!url) {
  console.error("usage: sample-from-youtube.mjs <url> [--slug <s>] [--min-ms n] [--max-ms n] [--max-chops n] [--start sec] [--end sec] [--redo] [--note \"text\"]");
  process.exit(2);
}
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (!a.startsWith("--")) continue;
  const k = a.slice(2);
  const n = argv[i + 1];
  if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
  else flags[k] = true;
}
const MIN_MS    = +(flags["min-ms"]    ?? 250);
const MAX_MS    = +(flags["max-ms"]    ?? 6000);
const MAX_CHOPS = +(flags["max-chops"] ?? 0);
const START_S   = +(flags["start"]     ?? 0);
const END_S     = flags["end"] ? +flags.end : null;
const SR_ANA    = +(flags["sr"]        ?? 22050);
const REDO      = !!flags["redo"];
const NOTE      = typeof flags["note"] === "string" ? flags.note : "";
const SLUG_OV   = typeof flags["slug"] === "string" ? flags.slug : null;
const SR_OUT    = 48000;

// ── helpers ──────────────────────────────────────────────────────────
function run(cmd, args, opts = {}) {
  const r = spawnSync(cmd, args, { stdio: "inherit", ...opts });
  if (r.status !== 0) throw new Error(`${cmd} ${args.join(" ")} → exit ${r.status}`);
}
function runCapture(cmd, args) {
  const r = spawnSync(cmd, args, { encoding: "utf8" });
  if (r.status !== 0) throw new Error(`${cmd} ${args.join(" ")} → exit ${r.status}\n${r.stderr}`);
  return r.stdout;
}
function slugify(s) {
  return s.toLowerCase()
    .normalize("NFKD").replace(/[̀-ͯ]/g, "")
    .replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 64) || "untitled";
}
function ytIdFromUrl(u) {
  try {
    const o = new URL(u);
    if (o.hostname === "youtu.be") return o.pathname.slice(1);
    return o.searchParams.get("v") || "";
  } catch { return ""; }
}

// ── WAV reader / writer (mono) ───────────────────────────────────────
function loadWavMono(path) {
  const buf = readFileSync(path);
  let p = 12, fmt = null, dOff = 0, dLen = 0;
  while (p + 8 <= buf.length) {
    const id = buf.toString("ascii", p, p + 4);
    const sz = buf.readUInt32LE(p + 4);
    if (id === "fmt ") fmt = {
      format: buf.readUInt16LE(p + 8), channels: buf.readUInt16LE(p + 10),
      sr: buf.readUInt32LE(p + 12), bits: buf.readUInt16LE(p + 22),
    };
    else if (id === "data") { dOff = p + 8; dLen = sz; }
    p += 8 + sz + (sz & 1);
  }
  if (!fmt || !dOff) throw new Error(`bad WAV: ${path}`);
  const { format, channels, bits } = fmt;
  const fb = (bits / 8) * channels, frames = Math.floor(dLen / fb);
  const mono = new Float32Array(frames);
  for (let i = 0; i < frames; i++) {
    let acc = 0;
    for (let c = 0; c < channels; c++) {
      const o = dOff + i * fb + c * (bits / 8);
      if (format === 3 && bits === 32) acc += buf.readFloatLE(o);
      else if (bits === 16) acc += buf.readInt16LE(o) / 32768;
      else if (bits === 24)
        acc += (buf.readUInt8(o) | (buf.readUInt8(o + 1) << 8) | (buf.readInt8(o + 2) << 16)) / 8388608;
      else if (bits === 32) acc += buf.readInt32LE(o) / 2147483648;
    }
    mono[i] = acc / channels;
  }
  return { samples: mono, sr: fmt.sr };
}
function writeWavMono(path, samples, sr) {
  const n = samples.length, bps = 2, dataLen = n * bps;
  const buf = Buffer.alloc(44 + dataLen);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + dataLen, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(sr, 24);
  buf.writeUInt32LE(sr * bps, 28); buf.writeUInt16LE(bps, 32);
  buf.writeUInt16LE(16, 34);
  buf.write("data", 36); buf.writeUInt32LE(dataLen, 40);
  let o = 44;
  for (let i = 0; i < n; i++) {
    const s = Math.max(-1, Math.min(1, samples[i]));
    buf.writeInt16LE(Math.round(s * 32767), o); o += 2;
  }
  writeFileSync(path, buf);
}

// ── 1. probe YT metadata ─────────────────────────────────────────────
console.log(`→ probing ${url}`);
const metaRaw = runCapture("yt-dlp", ["-J", "--no-playlist", url]);
const meta = JSON.parse(metaRaw);
const title    = meta.title    || meta.fulltitle || "untitled";
const uploader = meta.uploader || meta.channel   || "";
const duration = meta.duration || 0;
const ytId     = meta.id       || ytIdFromUrl(url);
const slug     = SLUG_OV || slugify(title);

console.log(`  title    : ${title}`);
console.log(`  uploader : ${uploader}`);
console.log(`  duration : ${duration}s`);
console.log(`  slug     : ${slug}`);

// ── 2. set up dir ────────────────────────────────────────────────────
const SRC_DIR  = resolve(SAMP, slug);
const CHOP_DIR = resolve(SRC_DIR, "chops");
const SRC_M4A  = resolve(SRC_DIR, "source.m4a");
const SRC_WAV  = resolve(SRC_DIR, "source.wav");
const ONSETS   = resolve(SRC_DIR, "onsets.json");
const MANIFEST = resolve(SRC_DIR, "manifest.json");

mkdirSync(SRC_DIR, { recursive: true });

if (existsSync(CHOP_DIR) && !REDO) {
  const n = readdirSync(CHOP_DIR).filter((f) => f.endsWith(".wav")).length;
  if (n > 0) {
    console.log(`✓ ${n} chops already at ${SRC_DIR.replace(process.env.HOME, "~")}/chops/ — pass --redo to recut`);
    process.exit(0);
  }
}

// ── 3. download bestaudio → m4a ──────────────────────────────────────
if (!existsSync(SRC_M4A)) {
  console.log(`→ yt-dlp downloading audio…`);
  run("yt-dlp", [
    "-f", "bestaudio[ext=m4a]/bestaudio",
    "--no-playlist",
    "-o", SRC_M4A,
    url,
  ]);
} else {
  console.log(`  source.m4a present, reusing`);
}

// ── 4. decode to 48k mono WAV (with optional trim) ───────────────────
console.log(`→ ffmpeg → ${SR_OUT} Hz mono WAV…`);
if (existsSync(SRC_WAV)) try { unlinkSync(SRC_WAV); } catch {}
const ffArgs = ["-hide_banner", "-y", "-loglevel", "error"];
if (START_S > 0) ffArgs.push("-ss", String(START_S));
if (END_S != null) ffArgs.push("-to", String(END_S));
ffArgs.push("-i", SRC_M4A, "-ac", "1", "-ar", String(SR_OUT), "-c:a", "pcm_s16le", SRC_WAV);
run("ffmpeg", ffArgs);

// ── 5. librosa onset detection ───────────────────────────────────────
console.log(`→ librosa onsets…`);
run(PY_VENV, [DETECTOR, SRC_WAV, ONSETS, "--sr", String(SR_ANA)]);
const onsets = JSON.parse(readFileSync(ONSETS, "utf8")); // [{ms,strength}]
if (onsets.length === 0) {
  console.error("✗ no onsets detected — abort");
  process.exit(1);
}

// ── 6. cut chops between onsets (clamped to [MIN_MS, MAX_MS]) ────────
const { samples: pcm, sr } = loadWavMono(SRC_WAV);
if (sr !== SR_OUT) console.warn(`  ⚠ source SR ${sr} ≠ ${SR_OUT}`);
const totalMs = (pcm.length / sr) * 1000;

const cuts = [];
for (let i = 0; i < onsets.length; i++) {
  const startMs = onsets[i].ms;
  const nextMs  = i + 1 < onsets.length ? onsets[i + 1].ms : totalMs;
  let lenMs = nextMs - startMs;
  if (lenMs < MIN_MS) continue;          // too tight, swallow into the next
  if (lenMs > MAX_MS) lenMs = MAX_MS;    // clamp long tails
  cuts.push({
    idx: cuts.length,
    startMs,
    lenMs: Math.round(lenMs),
    strength: onsets[i].strength,
  });
}
console.log(`  ${cuts.length} chops between ${MIN_MS}–${MAX_MS}ms`);

// optional cap by strength
let kept = cuts;
if (MAX_CHOPS > 0 && cuts.length > MAX_CHOPS) {
  const ranked = [...cuts].sort((a, b) => b.strength - a.strength).slice(0, MAX_CHOPS);
  ranked.sort((a, b) => a.startMs - b.startMs);
  kept = ranked.map((c, i) => ({ ...c, idx: i }));
  console.log(`  capped to ${kept.length} loudest`);
}

// ── 7. write chop WAVs ───────────────────────────────────────────────
if (existsSync(CHOP_DIR)) rmSync(CHOP_DIR, { recursive: true, force: true });
mkdirSync(CHOP_DIR, { recursive: true });

const fadeN = Math.floor(sr * 0.003); // 3 ms declick
const chopMeta = [];
let totalChopSamples = 0;
for (const c of kept) {
  const s0 = Math.floor((c.startMs / 1000) * sr);
  const s1 = Math.min(pcm.length, s0 + Math.floor((c.lenMs / 1000) * sr));
  const slice = pcm.subarray(s0, s1);
  const out = new Float32Array(slice.length);
  // normalize chop to peak 0.95
  let peak = 0;
  for (let i = 0; i < slice.length; i++) {
    const a = Math.abs(slice[i]); if (a > peak) peak = a;
  }
  const g = peak > 0 ? 0.95 / peak : 1;
  for (let k = 0; k < slice.length; k++) {
    let v = slice[k] * g;
    if (k < fadeN) v *= k / fadeN;
    if (slice.length - k < fadeN) v *= (slice.length - k) / fadeN;
    out[k] = v;
  }
  const idxStr = String(c.idx + 1).padStart(3, "0");
  const lenStr = String(c.lenMs).padStart(4, "0");
  const name = `chop-${idxStr}-${lenStr}ms.wav`;
  writeWavMono(resolve(CHOP_DIR, name), out, sr);
  chopMeta.push({
    file: `chops/${name}`,
    startMs: c.startMs,
    lenMs: c.lenMs,
    strength: +c.strength.toFixed(3),
  });
  totalChopSamples += out.length;
}
console.log(`✓ wrote ${chopMeta.length} chops → ${CHOP_DIR.replace(process.env.HOME, "~")}/`);

// ── 8. manifest.json (tracked) ───────────────────────────────────────
const manifest = {
  version: 1,
  slug,
  source: {
    url,
    youtubeId: ytId,
    title,
    uploader,
    durationSec: duration,
  },
  cut: {
    tool: "pop/bin/sample-from-youtube.mjs",
    sampleRate: SR_OUT,
    minMs: MIN_MS,
    maxMs: MAX_MS,
    startSec: START_S,
    endSec: END_S,
    maxChops: MAX_CHOPS || null,
    method: "librosa.onset_strength + onset_detect (backtrack=true)",
  },
  generatedAt: new Date().toISOString(),
  note: NOTE,
  chops: chopMeta,
};
writeFileSync(MANIFEST, JSON.stringify(manifest, null, 2) + "\n");
console.log(`✓ manifest → ${MANIFEST.replace(process.env.HOME, "~")}`);

// ── 9. update global INDEX.json ──────────────────────────────────────
let index = { version: 1, sources: [] };
if (existsSync(INDEX)) {
  try { index = JSON.parse(readFileSync(INDEX, "utf8")); }
  catch { /* recreate */ }
}
if (!Array.isArray(index.sources)) index.sources = [];
index.sources = index.sources.filter((s) => s.slug !== slug);
index.sources.push({
  slug,
  title,
  uploader,
  url,
  youtubeId: ytId,
  durationSec: duration,
  chops: chopMeta.length,
  totalChopSec: +(totalChopSamples / sr).toFixed(2),
  manifest: `${slug}/manifest.json`,
  addedAt: new Date().toISOString(),
  note: NOTE,
});
index.sources.sort((a, b) => a.slug.localeCompare(b.slug));
index.generatedAt = new Date().toISOString();
writeFileSync(INDEX, JSON.stringify(index, null, 2) + "\n");
console.log(`✓ index  → ${INDEX.replace(process.env.HOME, "~")} (${index.sources.length} sources)`);
