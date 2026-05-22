#!/usr/bin/env node
// narrate.mjs — jeffrey-pvc story narrator, word-level beat-placed.
//
// Says each line of <slug>.narration.txt in jeffrey-pvc (WITH word
// timestamps), CHOPS the line into its words, and scatters the words
// across that section's BEAT GRID — so the narration ticks across the
// beats rather than dumping as one fast clip at the section start.
// Output: a full-length stem out/<slug>-narration.mp3 that render.mjs
// mixes in via --vocal-stem.
//
// Needs the local say endpoint running:  node bin/say-local.mjs
//
// Usage:
//   node bin/narrate.mjs --slug helpabeach-short
//   node bin/narrate.mjs --slug helpabeach-short --speed 0.85 --spread 0.9 --force

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import * as progress from "../../lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const n = process.argv[i + 1];
  if (n === undefined || n.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = n; i++; }
}

const SLUG   = flags.slug || "helpabeach-short";
const BPM    = Number(flags.bpm ?? 84);
const SPEED  = Number(flags.speed ?? 0.85);   // ElevenLabs delivery speed (<1 = slower)
const SPREAD = Number(flags.spread ?? 0.9);   // fraction of the section the words span
const ROBOT  = Number(flags.robot ?? 0.12);   // faint robotic ring-mod edge
const FORCE  = flags.force === true;
const SR     = 48000;
const beat   = 60 / BPM;
const HALFBEAT = beat / 2;                    // word onsets snap to the half-beat grid
const SAY_URL = process.env.SAY_ENDPOINT || "http://127.0.0.1:8899/api/say";

const NP  = `${LANE}/${SLUG}.np`;
const TXT = `${LANE}/${SLUG}.narration.txt`;
const OUT = `${LANE}/out/${SLUG}-narration.mp3`;
const TMP = `${LANE}/out/.tmp/narration`;
mkdirSync(TMP, { recursive: true });

if (!existsSync(NP) || !existsSync(TXT)) {
  console.error(`✗ need ${SLUG}.np and ${SLUG}.narration.txt`);
  process.exit(1);
}

// ── section spans (start + duration, seconds) from the .np ───────────
function sectionSpans(npPath) {
  const HEADER = /^#\s*[a-z][a-z0-9-]*(?:\s+\d+)?\s+\d+(?:\s+\[[^\]]*\])?\s*$/i;
  let pos = 0;
  const starts = [];
  for (const raw of readFileSync(npPath, "utf8").split("\n")) {
    const t = raw.trim();
    if (!t) continue;
    if (t.startsWith("#")) { if (HEADER.test(t)) starts.push(pos * beat); continue; }
    for (const tok of t.split(/\s+/)) {
      const m = tok.match(/^[A-Ga-g][#b]?\d(?:\+[A-Ga-g][#b]?\d)*:.+?(?:\*(\d+(?:\.\d+)?))?$/);
      if (m) pos += Number(m[1] ?? 1);
    }
  }
  const totalSec = pos * beat;
  return {
    totalSec,
    spans: starts.map((s, i) => ({
      start: s, dur: (i + 1 < starts.length ? starts[i + 1] : totalSec) - s,
    })),
  };
}

// ── jeffrey-pvc say WITH word timestamps (content-hash cached) ───────
async function sayLine(text) {
  const hash = createHash("sha256")
    .update(`jeffrey-pvc-ts:${SPEED}:${text}`).digest("hex").slice(0, 16);
  const mp3 = `${TMP}/${hash}.mp3`;
  const wj  = `${TMP}/${hash}.words.json`;
  if (!FORCE && existsSync(mp3) && existsSync(wj)) {
    return { mp3, words: JSON.parse(readFileSync(wj, "utf8")) };
  }
  const res = await fetch(SAY_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      from: text, provider: "jeffrey", voice: "neutral:0",
      stability: 0.6, similarity: 0.92, style: 0.2,
      speed: SPEED, withTimestamps: true,
    }),
  });
  if (!res.ok) {
    console.error(`✗ say ${res.status}: ${(await res.text()).slice(0, 240)}`);
    console.error("  is say-local running?  node bin/say-local.mjs");
    process.exit(1);
  }
  const j = await res.json();
  writeFileSync(mp3, Buffer.from(j.audio, "base64"));
  // characters → words (runs of non-space chars, [first start, last end])
  const a = j.alignment || {};
  const ch = a.characters || [];
  const st = a.character_start_times_seconds || [];
  const en = a.character_end_times_seconds || [];
  const words = [];
  let i = 0;
  while (i < ch.length) {
    if (/\s/.test(ch[i])) { i++; continue; }
    const from = st[i]; let txt = "", last = en[i];
    while (i < ch.length && !/\s/.test(ch[i])) { txt += ch[i]; last = en[i]; i++; }
    words.push({ text: txt, fromMs: Math.round(from * 1000), toMs: Math.round(last * 1000) });
  }
  writeFileSync(wj, JSON.stringify(words));
  return { mp3, words };
}

function decodeMono(mp3) {
  const raw = `${mp3}.f32`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", mp3, "-f", "f32le", "-ar", String(SR), "-ac", "1", raw]);
  if (r.status !== 0) { console.error("✗ decode failed"); process.exit(1); }
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, b.byteLength / 4);
}

// ── say each line, chop into words, scatter across the section beats ─
const lines = readFileSync(TXT, "utf8").split("\n")
  .map((l) => l.trim()).filter((l) => l && !l.startsWith("#"));
const { spans, totalSec } = sectionSpans(NP);
console.log(`▸ ${SLUG} narrator · ${lines.length} lines · ${spans.length} sections · speed ${SPEED} · word-beat-placed`);

progress.begin({ type: "audio", label: `${SLUG} narration` });
const LEN = Math.ceil((totalSec + 3) * SR);
const out = new Float32Array(LEN);
const fadeS = Math.floor(0.006 * SR);   // 6 ms declick on each word slice

for (let s = 0; s < lines.length; s++) {
  const span = spans[s] || { start: s * 8, dur: 8 };
  const { mp3, words } = await sayLine(lines[s]);
  const clip = decodeMono(mp3);
  const N = words.length;
  const usable = span.dur * SPREAD;
  for (let w = 0; w < N; w++) {
    // spread word w across the section, snap its onset to the half-beat grid
    const target = span.start + (N > 1 ? (w / N) * usable : 0);
    const atIdx = Math.floor((Math.round(target / HALFBEAT) * HALFBEAT) * SR);
    const w0 = Math.floor((words[w].fromMs / 1000) * SR);
    const w1 = Math.floor((words[w].toMs / 1000) * SR);
    const wlen = Math.max(1, w1 - w0);
    for (let j = 0; j < wlen; j++) {
      const d = atIdx + j;
      const src = w0 + j;
      if (d < 0 || d >= LEN || src >= clip.length) break;
      let v = clip[src];
      if (j < fadeS) v *= j / fadeS;
      else if (j > wlen - fadeS) v *= Math.max(0, (wlen - j) / fadeS);
      if (ROBOT > 0) {
        const ring = Math.sin(2 * Math.PI * 47 * (j / SR));
        v = v * (1 - ROBOT) + v * ring * (ROBOT * 1.5);
      }
      out[d] += v;
    }
  }
  console.log(`  §${s} @ ${span.start.toFixed(1)}s · ${N} words across ${(usable).toFixed(1)}s · "${lines[s]}"`);
  progress.update(((s + 1) / lines.length) * 100);
}

// peak-normalize + encode
let pk = 1e-6;
for (let i = 0; i < LEN; i++) { const a = Math.abs(out[i]); if (a > pk) pk = a; }
const norm = 0.94 / pk;
for (let i = 0; i < LEN; i++) out[i] *= norm;

const rawOut = `${TMP}/${SLUG}-narration.f32`;
writeFileSync(rawOut, Buffer.from(out.buffer, out.byteOffset, out.byteLength));
const enc = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawOut,
  "-c:a", "libmp3lame", "-q:a", "2", OUT]);
progress.end();
if (enc.status !== 0) { console.error("✗ encode failed"); process.exit(1); }
console.log(`✓ ${OUT.replace(LANE + "/", "")}  (${(LEN / SR).toFixed(1)}s · words scattered on the half-beat grid)`);
