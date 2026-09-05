#!/usr/bin/env node
// lyrictrack.mjs — canonical lyric timing for the sacred voice: the
// SyllaWizard drawn boundaries (take timebase, ground truth) carried
// through the sacredvox transform (trim at t0, one rubberband stretch —
// the ratio is read back from the rendered stem, not trusted from
// theory) and projected onto the floor's three passes. Every syllable
// gets a grid reading: bar, beat, and signed ms from the nearest 16th.
//
//   node pop/imab/bin/lyrictrack.mjs [--take <id>] [--no-video]
//   → out/imab-sacredvox.lyrics.json      syllables+words in stem time
//   → out/imab-floor-demo1.lyrics.json    absolute times per pass + grid fit
//   → out/imab-floor-lyricproof.mp4       the proof you can watch: words
//     flash over the real floor mix at their mapped moments, tinted by
//     grid offset (white ≤45ms · amber ≤90ms · red beyond), bar numbers
//     running through each pass.

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const DL = `${REPO}/toolchain/whistlegraph/downloads`;
const WORK = `${process.env.HOME}/.cache/ac/imab`;
mkdirSync(WORK, { recursive: true });
const argv = process.argv.slice(2);
const flag = (n, d) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const TAKE = flag("take", "7311159624588070175");
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });
const BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT, SIXT = BEAT / 4;
const T = (b) => b * BAR;
const PASSES = [16, 40, 56];          // floor.mjs doors
const PASS_OFF = 0;                   // stem ON the door — "i'm" starts at 16:A1

// ── the sacredvox window: same match, same t0/t1 ──────────────────────
const doc = JSON.parse(readFileSync(`${DL}/whistlegraph-${TAKE}.syllnote.json`, "utf8"));
const TMPL = "i'm a butterfly flapping for you guys just a costume i put on in my room".split(" ");
const norm = (w) => w.toLowerCase().replace(/[^a-z']/g, "");
const fuzzy = (a, b) => a === b || (a.length > 3 && b.length > 3 && (a.startsWith(b.slice(0, 4)) || b.startsWith(a.slice(0, 4))));
const seq = [];
let ti = 0;
for (const w of doc.words) {
  if (ti < TMPL.length && fuzzy(TMPL[ti], norm(w.text))) { seq.push(w); ti++; }
}
if (ti < 14) { console.error(`✗ matched ${ti}/16`); process.exit(1); }
const t0 = Math.max(0, seq[0].fromMs / 1000 - 0.15);
const t1 = seq[seq.length - 1].toMs / 1000 + 0.6;

// ── the stretch, read back from the stem itself ───────────────────────
const STEM = `${OUT}/imab-sacredvox.wav`;
if (!existsSync(STEM)) { console.error("✗ run sacredvox.mjs first"); process.exit(1); }
const dur = (f) => parseFloat(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", f], { encoding: "utf8" }).stdout);
const stemDur = dur(STEM);
// the trim can end early — the demucs stem may be shorter than t1
const SEP = `${WORK}/sep/htdemucs/whistlegraph-${TAKE}/vocals.wav`;
const trimEnd = existsSync(SEP) ? Math.min(t1, dur(SEP)) : t1;
const ratio = stemDur / (trimEnd - t0);
const onsets = seq.map((w) => w.nuclei?.[0]?.startSec ?? w.fromMs / 1000);
const iois = onsets.slice(1).map((t, i) => t - onsets[i]).filter((d) => d > 0.08).sort((a, b) => a - b);
const theory = BEAT / iois[Math.floor(iois.length / 2)];
if (Math.abs(theory - ratio) > 0.01)
  console.log(`  (note: median-IOI ratio ${theory.toFixed(4)} vs stem-measured ${ratio.toFixed(4)} — using measured)`);
console.log(`take ${TAKE}: window ${t0.toFixed(2)}–${t1.toFixed(2)}s · stretch ${ratio.toFixed(4)} · stem ${stemDur.toFixed(2)}s`);

// ── drawn boundaries → stem time ──────────────────────────────────────
// rubberband -c 6 preserves transients by warping time NON-uniformly,
// so a linear ×ratio map drifts mid-phrase. When the raw trim and the
// stretched phrase are both in the cache, DTW their energy envelopes
// (the shapebound idea) and carry each boundary through the actual path.
const HOP = 0.010, SRr = 48000;
const envOf = (wav) => {
  const raw = `${WORK}/.env.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SRr), raw]);
  const b = readFileSync(raw);
  const x = new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
  const hop = Math.round(HOP * SRr), n = Math.floor(x.length / hop);
  const e = new Float64Array(n);
  for (let i = 0; i < n; i++) {
    let s = 0;
    for (let j = i * hop; j < (i + 1) * hop; j++) s += x[j] * x[j];
    e[i] = Math.log10(1e-8 + Math.sqrt(s / hop));
  }
  const flux = new Float64Array(n);
  for (let i = 1; i < n; i++) flux[i] = Math.max(0, e[i] - e[i - 1]);
  const z = (a) => { const m = a.reduce((p, c) => p + c, 0) / a.length;
    const sd = Math.sqrt(a.reduce((p, c) => p + (c - m) ** 2, 0) / a.length) || 1;
    return a.map((v) => (v - m) / sd); };
  return { e: z([...e]), f: z([...flux]) };
};
let warp = null;                                     // rawSec → stemSec
const RAWP = `${WORK}/phrase.wav`, STRP = `${WORK}/phrase-124.wav`;
if (existsSync(RAWP) && existsSync(STRP)) {
  const A = envOf(RAWP), B = envOf(STRP);
  const N = A.e.length, M = B.e.length, BAND = 200;
  const INF = 1e18, D = new Float64Array(N * M).fill(INF);
  const cost = (i, j) => Math.abs(A.e[i] - B.e[j]) + Math.abs(A.f[i] - B.f[j]);
  for (let i = 0; i < N; i++) {
    const c = Math.round((i / N) * M);
    for (let j = Math.max(0, c - BAND); j < Math.min(M, c + BAND); j++) {
      const best = i === 0 && j === 0 ? 0 : Math.min(
        i > 0 && j > 0 ? D[(i - 1) * M + j - 1] : INF,
        i > 0 ? D[(i - 1) * M + j] : INF,
        j > 0 ? D[i * M + j - 1] : INF);
      if (best < INF) D[i * M + j] = cost(i, j) + best;
    }
  }
  const map = new Float64Array(N).fill(-1);
  let i = N - 1, j = M - 1;
  while (i > 0 || j > 0) {
    map[i] = map[i] < 0 ? j : (map[i] + j) / 2;
    const dg = i > 0 && j > 0 ? D[(i - 1) * M + j - 1] : INF;
    const up = i > 0 ? D[(i - 1) * M + j] : INF;
    const lf = j > 0 ? D[i * M + j - 1] : INF;
    if (dg <= up && dg <= lf) { i--; j--; } else if (up <= lf) i--; else j--;
  }
  map[0] = 0;
  for (let k = 1; k < N; k++) if (map[k] < 0) map[k] = map[k - 1];
  warp = (rawSec) => {
    const p = Math.min(N - 1, Math.max(0, rawSec / HOP));
    const a = Math.floor(p), b = Math.min(N - 1, a + 1);
    return (map[a] + (map[b] - map[a]) * (p - a)) * HOP;
  };
  console.log("  (DTW warp active: boundaries follow rubberband's actual time map)");
} else console.log("  (no cached phrase wavs — falling back to the linear stretch map)");
const toStem = (takeSec) => warp ? warp(takeSec - t0) : (takeSec - t0) * ratio;

const DRAWN = resolve(HERE, `../boundaries-drawn-${TAKE}.json`);
if (!existsSync(DRAWN)) { console.error(`✗ no drawn boundaries for ${TAKE} — run syllawizard`); process.exit(1); }
const drawn = JSON.parse(readFileSync(DRAWN, "utf8"));

// ── @jeffrey's dictation: lyric-overrides.json ────────────────────────
const OVR = resolve(HERE, "../lyric-overrides.json");
const ovr = existsSync(OVR) ? JSON.parse(readFileSync(OVR, "utf8")) : {};
for (const [i, fix] of Object.entries(ovr.drawnFix ?? {})) {
  Object.assign(drawn.sylls[+i], fix);
  console.log(`  drawnFix: ${drawn.sylls[+i].label} → ${JSON.stringify(fix)}`);
}

// ── dead-air trim: shrink every window to its actual utterance ────────
// (@jeffrey: "check the samples for start and stop dead time")
if (existsSync(RAWP)) {
  const raw = (() => { const f = `${WORK}/.trim.f32`;
    sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", RAWP, "-f", "f32le", "-ac", "1", "-ar", "16000", f]);
    const b = readFileSync(f); return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4)); })();
  const HOPS = Math.round(0.005 * 16000);
  const rmsAt = (i) => { let s = 0; const a = i * HOPS;
    for (let j = a; j < a + HOPS && j < raw.length; j++) s += raw[j] * raw[j];
    return Math.sqrt(s / HOPS); };
  for (const s of drawn.sylls) {
    const i0 = Math.floor((s.fromMs / 1000 - t0) / 0.005), i1 = Math.ceil((s.toMs / 1000 - t0) / 0.005);
    let peak = 0; for (let i = i0; i < i1; i++) peak = Math.max(peak, rmsAt(i));
    const th = Math.max(0.0025, 0.05 * peak);
    let a = i0; while (a < i1 && rmsAt(a) < th) a++;
    let b = i1; while (b > a && rmsAt(b - 1) < th) b--;
    const nf = Math.max(s.fromMs, Math.round((t0 + a * 0.005) * 1000) - 15);
    const nt = Math.min(s.toMs, Math.round((t0 + b * 0.005) * 1000) + 30);
    if (nf - s.fromMs > 40 || s.toMs - nt > 40)
      console.log(`  dead-trim ${s.label}: ${s.fromMs}–${s.toMs} → ${nf}–${nt} (−${(nf - s.fromMs) + (s.toMs - nt)}ms)`);
    s.fromMs = nf; s.toMs = nt;
  }
}

let sylls = drawn.sylls.map((s) => ({
  label: s.label, wi: s.wi,
  fromMs: Math.round(toStem(s.fromMs / 1000) * 1000),
  toMs: Math.round(toStem(s.toMs / 1000) * 1000),
  linDriftMs: Math.round((toStem(s.fromMs / 1000) - (s.fromMs / 1000 - t0) * ratio) * 1000),
}));
const words = [];
for (const s of sylls) {
  const w = words[s.wi] ?? (words[s.wi] = { text: TMPL[s.wi], fromMs: s.fromMs, toMs: s.toMs });
  w.fromMs = Math.min(w.fromMs, s.fromMs); w.toMs = Math.max(w.toMs, s.toMs);
}
// ── RETIME: dictated grid addresses re-cut the aesthetivox stem ───────
// address → stem-relative seconds (letter = bar off the door, beat 1–4)
const addrSec = (a) => {
  const m = /^([A-Z])(\d+(?:\.\d+)?)$/.exec(a.trim().toUpperCase());
  if (!m) { console.error(`✗ bad address ${a}`); process.exit(1); }
  return ((m[1].charCodeAt(0) - 65) * 4 + (parseFloat(m[2]) - 1)) * BEAT;
};
// ── aesthetivox note-lock: targets regenerated from the CURRENT
// boundaries every run, so drawnFix repairs feed the tuner (the 'a'
// lesson: stale targets tune the wrong windows).
const PY = `${REPO}/pop/.venv/bin/python`;
const AVOX = `${OUT}/imab-aesthetivox.wav`;
const RVOX = `${OUT}/imab-aesthetivox-retimed.wav`;
const GT = ["C4", "G4", "C4", "C4", "C4", "C4", "C5", "C4", "C4", "C4",
            "G4", "F4", "E4", "D4", "E4", "E4", "D4", "C4", "C4", "C4"];
writeFileSync(`${OUT}/imab-gt-targets.json`, JSON.stringify(sylls.map((s, i) => ({
  label: s.label, t: +(s.fromMs / 1000).toFixed(3),
  dur: +((s.toMs - s.fromMs) / 1000).toFixed(3), note: GT[i] })), null, 1));
console.log("→ aesthetivox note-lock (targets from current boundaries)");
sh(PY, [`${REPO}/pop/bin/autotune.py`, `${OUT}/imab-sacredvox.wav`, AVOX,
  "--targets", `${OUT}/imab-gt-targets.json`, "--register-fit",
  "--strength", "1.0", "--preserve", "0.25", "--glide-ms", "30"]);   // flutish: dead-on, little ride
let stemFile = "out/imab-sacredvox.wav";
const gridOvr = Object.entries(ovr.overrides ?? {});
if (gridOvr.length && existsSync(AVOX)) {
  // anchors (src → dst): every syllable start pinned unless dictated;
  // dictated ends anchored too. Monotonic or we bail.
  const anchors = [[0, 0]];
  for (let i = 0; i < sylls.length; i++) {
    const o = ovr.overrides?.[String(i)];
    const src = sylls[i].fromMs / 1000;
    anchors.push([src, o?.start ? addrSec(o.start) : src]);
    if (o?.end) anchors.push([sylls[i].toMs / 1000, addrSec(o.end)]);
  }
  const aDur = dur(AVOX);
  anchors.push([aDur, aDur + (anchors[anchors.length - 1][1] - anchors[anchors.length - 1][0])]);
  anchors.sort((x, y) => x[0] - y[0]);
  const uniq = [anchors[0]];
  for (const [s, d] of anchors.slice(1)) {
    const [ps, pd] = uniq[uniq.length - 1];
    if (Math.abs(s - ps) < 1e-6 && Math.abs(d - pd) < 1e-6) continue;
    if (s <= ps + 1e-6 || d < pd - 1e-6) {   // dst may hold still (a cut), never go back
      console.error(`✗ overrides not monotonic near ${s.toFixed(2)}s`); process.exit(1);
    }
    uniq.push([s, d]);
  }
  anchors.length = 0; anchors.push(...uniq);
  // src→dst warp for the boundary bookkeeping
  const warpDst = (s) => {
    let k = 1; while (k < anchors.length - 1 && anchors[k][0] < s) k++;
    const [s0, d0] = anchors[k - 1], [s1, d1] = anchors[k];
    return d0 + (s - s0) / Math.max(1e-9, s1 - s0) * (d1 - d0);
  };
  // render: cut each segment, rubberband those whose factor ≠ 1, concat
  const parts = [];
  for (let k = 1; k < anchors.length; k++) {
    const [s0, d0] = anchors[k - 1], [s1, d1] = anchors[k];
    const sd = s1 - s0, dd = d1 - d0;
    if (sd < 0.002) continue;
    if (dd < 0.02) { console.log(`  retime: ${s0.toFixed(2)}–${s1.toFixed(2)}s CUT`); continue; }
    const seg = `${WORK}/rt-${k}.wav`;
    sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", AVOX,
      "-ss", s0.toFixed(4), "-t", sd.toFixed(4), "-c:a", "pcm_s16le", seg]);
    const f = dd / sd;
    if (Math.abs(f - 1) > 0.02) {
      sh("rubberband", ["-t", f.toFixed(4), "-F", "-c", "6", seg, `${WORK}/rt-${k}s.wav`]);
      parts.push(`${WORK}/rt-${k}s.wav`);
      console.log(`  retime: ${s0.toFixed(2)}–${s1.toFixed(2)}s ×${f.toFixed(2)}`);
    } else parts.push(seg);
  }
  writeFileSync(`${WORK}/rt-list.txt`, parts.map((p) => `file '${p}'`).join("\n"));
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "concat", "-safe", "0",
    "-i", `${WORK}/rt-list.txt`, "-c:a", "pcm_s16le", RVOX]);
  sylls = sylls.map((s) => ({ ...s,
    fromMs: Math.round(warpDst(s.fromMs / 1000) * 1000),
    toMs: Math.round(warpDst(s.toMs / 1000) * 1000) }));
  for (const w of words) if (w) {
    w.fromMs = Math.round(warpDst(w.fromMs / 1000) * 1000);
    w.toMs = Math.round(warpDst(w.toMs / 1000) * 1000);
  }
  stemFile = "out/imab-aesthetivox-retimed.wav";
  console.log(`✓ ${RVOX} (dictation applied)`);
} else if (existsSync(AVOX)) stemFile = "out/imab-aesthetivox.wav";

writeFileSync(`${OUT}/imab-sacredvox.lyrics.json`, JSON.stringify({
  take: TAKE, source: "boundaries-drawn", bpm: BPM,
  t0Sec: +t0.toFixed(3), stretch: +ratio.toFixed(4), stem: stemFile,
  syllables: sylls, words: words.filter(Boolean),
}, null, 2));
console.log(`✓ out/imab-sacredvox.lyrics.json (${sylls.length} syllables)`);

// ── grid fit: where each syllable lands, per pass ─────────────────────
// relative placement is identical for every pass, so read it once
const grid = sylls.map((s) => {
  const rel = PASS_OFF + s.fromMs / 1000;         // seconds after the door
  const beats = rel / BEAT;
  const nearest = Math.round(rel / SIXT);
  const offMs = (rel - nearest * SIXT) * 1000;
  return { ...s, relSec: +rel.toFixed(3), barOff: Math.floor(beats / 4),
    beat: +(1 + (beats % 4)).toFixed(2), grid16: nearest, gridOffMs: Math.round(offMs) };
});
const passes = PASSES.map((door) => ({
  door, startSec: +(T(door) + PASS_OFF).toFixed(3),
  syllables: grid.map((g) => ({ label: g.label, sec: +(T(door) + g.relSec).toFixed(3),
    bar: door + g.barOff, beat: g.beat, gridOffMs: g.gridOffMs })),
}));
writeFileSync(`${OUT}/imab-floor-demo1.lyrics.json`, JSON.stringify({
  audio: "out/imab-floor-demo1.mp3", bpm: BPM, take: TAKE,
  passOffsetSec: PASS_OFF, passes,
}, null, 2));
console.log(`✓ out/imab-floor-demo1.lyrics.json (${PASSES.length} passes)`);

console.log("\nsyll        bar+beat      off 16th");
for (const g of grid) {
  const flagc = Math.abs(g.gridOffMs) <= 45 ? " " : Math.abs(g.gridOffMs) <= 90 ? "~" : "!";
  console.log(`  ${g.label.padEnd(8)} +${String(g.barOff).padStart(1)} b${g.beat.toFixed(2).padStart(5)}  ${String(g.gridOffMs).padStart(5)}ms ${flagc}`);
}

if (argv.includes("--no-video")) process.exit(0);

// ── the proof video: words over the real floor mix ────────────────────
const FLOOR = `${OUT}/imab-floor-demo1.mp3`;
if (!existsSync(FLOOR)) { console.error("✗ run floor.mjs first"); process.exit(1); }
const fdur = parseFloat(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", FLOOR], { encoding: "utf8" }).stdout);

const LBL = `${WORK}/lyrictrack-labels`;
const barsShown = [...new Set(PASSES.flatMap((d) => Array.from({ length: 10 }, (_, i) => d - 1 + i)))];
const gen = spawnSync(`${REPO}/pop/.venv/bin/python`, ["-c", `
import json, os
from PIL import Image, ImageDraw, ImageFont
W = ${JSON.stringify(LBL)}
os.makedirs(W, exist_ok=True)
grid = json.loads(${JSON.stringify(JSON.stringify(grid))})
font = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial Bold.ttf", 150)
mid = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial.ttf", 52)
small = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial.ttf", 44)
for i, g in enumerate(grid):
    off = g["gridOffMs"]
    col = (255,255,255,255) if abs(off) <= 45 else (255,204,85,255) if abs(off) <= 90 else (255,119,102,255)
    img = Image.new("RGBA", (1920, 320), (0,0,0,0))
    d = ImageDraw.Draw(img)
    wpx = d.textlength(g["label"], font=font)
    d.text(((1920-wpx)/2, 20), g["label"], font=font, fill=col)
    sub = f'{"+" if off >= 0 else ""}{off}ms · beat {g["beat"]:.2f}'
    spx = d.textlength(sub, font=mid)
    d.text(((1920-spx)/2, 210), sub, font=mid, fill=(170,170,190,220))
    img.save(f"{W}/s{i:02d}.png")
for b in json.loads(${JSON.stringify(JSON.stringify(barsShown))}):
    img = Image.new("RGBA", (300, 90), (0,0,0,0))
    d = ImageDraw.Draw(img)
    d.text((10, 10), f"bar {b}", font=mid, fill=(150,190,255,230))
    img.save(f"{W}/b{b:02d}.png")
lyr = "i\\u2019m a butterfly, flapping for you guys, just a costume, i put on, in my room"
img = Image.new("RGBA", (1920, 80), (0,0,0,0))
d = ImageDraw.Draw(img)
wpx = d.textlength(lyr, font=small)
d.text(((1920-wpx)/2, 10), lyr, font=small, fill=(255,255,255,90))
img.save(f"{W}/lyric.png")
print("labels done")
`], { stdio: ["ignore", "inherit", "inherit"] });
if (gen.status !== 0) { console.error("✗ label gen failed"); process.exit(1); }

const inputs = ["-f", "lavfi", "-i", `color=c=0x101018:s=1920x1080:r=30:d=${fdur.toFixed(2)}`,
  "-i", FLOOR, "-i", `${LBL}/lyric.png`];
grid.forEach((_, i) => inputs.push("-i", `${LBL}/s${String(i).padStart(2, "0")}.png`));
const barIdx = {};
barsShown.forEach((b, k) => { barIdx[b] = 3 + grid.length + k; inputs.push("-i", `${LBL}/b${String(b).padStart(2, "0")}.png`); });

let fc = `[0:v][2:v]overlay=0:940[b0]`;
let stage = 0;
grid.forEach((g, i) => {
  const windows = PASSES.map((door) => {
    const at = T(door) + g.relSec, off = at + Math.max((g.toMs - g.fromMs) / 1000, 0.35);
    return `between(t,${at.toFixed(3)},${off.toFixed(3)})`;
  }).join("+");
  fc += `;[b${stage}][${i + 3}:v]overlay=0:380:enable='${windows}'[b${stage + 1}]`; stage++;
});
for (const b of barsShown) {
  fc += `;[b${stage}][${barIdx[b]}:v]overlay=80:60:enable='between(t,${T(b).toFixed(3)},${T(b + 1).toFixed(3)})'[b${stage + 1}]`; stage++;
}
fc += `;[b${stage}]drawbox=x=1800:y=60:w=60:h=60:color=white@0.6:t=fill:enable='lt(mod(t,${BEAT.toFixed(4)}),0.09)',` +
  `drawbox=x=1700:y=60:w=60:h=60:color=0x77bbff@0.8:t=fill:enable='lt(mod(t,${BAR.toFixed(4)}),0.11)'[v]`;
const mp4 = `${OUT}/imab-floor-lyricproof.mp4`;
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", ...inputs,
  "-filter_complex", fc,
  "-map", "[v]", "-map", "1:a", "-c:v", "libx264", "-preset", "fast", "-crf", "20",
  "-c:a", "aac", "-b:a", "192k", "-shortest", mp4]);
console.log(`✓ ${mp4}`);
