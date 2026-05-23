#!/usr/bin/env node
// narrate.mjs — jeffrey-pvc story narrator, PHRASE-aligned natural.
//
// Says each line of <slug>.narration.txt in jeffrey-pvc (one natural
// read, with word timestamps), splits the line into PHRASES at its
// punctuation, and places each phrase — natural, unprocessed — at a
// BEAT-ALIGNED start time across the section. Only the start times
// snap to the beat; the phrase audio itself is never stretched,
// chopped word-by-word, or pitched. Phrases spread across the section
// so natural pauses fall between them. Output: a full-length stem
// out/<slug>-narration.mp3 that render.mjs mixes via --vocal-stem.
//
// Needs the local say endpoint running:  node bin/say-local.mjs
//
// Usage:
//   node bin/narrate.mjs --slug helpabeach-short
//   node bin/narrate.mjs --slug helpabeach-short --spread 0.9 --force

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
const SPREAD = Number(flags.spread ?? 0.92);  // fraction of the section the phrases span
const SPEED  = Number(flags.speed ?? 0.82);   // ElevenLabs read speed (<1 = slower)
const REVERB = flags["no-reverb"] !== true;   // a light reverb tail on the narration
const FORCE  = flags.force === true;
const SR     = 48000;
const beat   = 60 / BPM;
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

// split a narration line into phrases at its punctuation (, . ; ? ! —)
function splitPhrases(line) {
  return line.split(/\s*—\s*|\s*[,.;?!]+\s*/).map((p) => p.trim()).filter(Boolean);
}

// ── jeffrey-pvc say WITH word timestamps (content-hash cached) ───────
async function sayLine(text) {
  const hash = createHash("sha256").update(`jeffrey-pvc-ph:${SPEED}:${text}`).digest("hex").slice(0, 16);
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
  const a = j.alignment || {};
  const ch = a.characters || [];
  const st = a.character_start_times_seconds || [];
  const en = a.character_end_times_seconds || [];
  const words = [];
  let i = 0;
  while (i < ch.length) {
    if (/\s/.test(ch[i])) { i++; continue; }
    const from = st[i]; let last = en[i];
    while (i < ch.length && !/\s/.test(ch[i])) { last = en[i]; i++; }
    words.push({ fromMs: Math.round(from * 1000), toMs: Math.round(last * 1000) });
  }
  writeFileSync(wj, JSON.stringify(words));
  return { mp3, words };
}

function ff(args) { return spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", ...args]); }
function decodeMono(file) {
  const raw = `${file}.f32`;
  if (ff(["-i", file, "-f", "f32le", "-ar", String(SR), "-ac", "1", raw]).status !== 0) return null;
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, b.byteLength / 4);
}

// ── say each line, place each phrase at a beat-aligned start ─────────
const lines = readFileSync(TXT, "utf8").split("\n")
  .map((l) => l.trim()).filter((l) => l && !l.startsWith("#"));
const { spans, totalSec } = sectionSpans(NP);
console.log(`▸ ${SLUG} narrator · ${lines.length} lines · ${spans.length} sections · phrase-aligned natural`);

progress.begin({ type: "audio", label: `${SLUG} narration` });
const LEN = Math.ceil((totalSec + 3) * SR);
const out = new Float32Array(LEN);
const fadeS = Math.floor(0.012 * SR);
let cursor = 0;   // global playhead — phrases never overlap, one strictly after another

for (let s = 0; s < lines.length; s++) {
  const span = spans[s] || { start: s * 8, dur: 8 };
  const phrases = splitPhrases(lines[s]);
  const { mp3, words } = await sayLine(lines[s]);
  const lineClip = decodeMono(mp3);
  if (!lineClip) continue;
  // map words → phrases by word count, get each phrase's [from,to] span
  let wIdx = 0;
  const phraseSpans = phrases.map((ph) => {
    const nw = ph.split(/\s+/).filter(Boolean).length;
    const first = words[Math.min(wIdx, words.length - 1)];
    const last  = words[Math.min(wIdx + nw - 1, words.length - 1)];
    wIdx += nw;
    return first && last
      ? { from: first.fromMs / 1000, to: last.toMs / 1000 }
      : null;
  }).filter(Boolean);

  const M = phraseSpans.length;
  const usable = span.dur * SPREAD;
  for (let m = 0; m < M; m++) {
    const p0 = Math.floor(phraseSpans[m].from * SR);
    const p1 = Math.min(lineClip.length, Math.floor(phraseSpans[m].to * SR));
    const plen = Math.max(1, p1 - p0);
    // normalize each phrase to a consistent RMS so the voice sits at an
    // even level — no phrase reads louder or quieter than the rest.
    let sum = 0, cnt = 0;
    for (let j = p0; j < p1 && j < lineClip.length; j++) { sum += lineClip[j] * lineClip[j]; cnt++; }
    const rms = Math.sqrt(sum / Math.max(1, cnt));
    const gain = Math.max(0.25, Math.min(6, 0.14 / Math.max(1e-4, rms)));
    // start: a spread target, but NEVER before the previous phrase ended,
    // snapped UP to a beat — phrases come strictly one after another, no
    // overlap anywhere across the whole narration.
    const frac = M > 1 ? m / (M - 1) : 0;
    const target = span.start + frac * usable;
    const mark = Math.ceil(Math.max(target, cursor) / beat) * beat;
    const atIdx = Math.floor(mark * SR);
    for (let j = 0; j < plen; j++) {
      const d = atIdx + j;
      const src = p0 + j;
      if (d < 0 || d >= LEN || src >= lineClip.length) break;
      let v = lineClip[src] * gain;
      if (j < fadeS) v *= j / fadeS;
      else if (j > plen - fadeS) v *= Math.max(0, (plen - j) / fadeS);
      out[d] += v;
    }
    cursor = mark + plen / SR;   // the next phrase begins only after this one ends
  }
  console.log(`  §${s} @ ${span.start.toFixed(1)}s · ${M} phrases · "${lines[s]}"`);
  progress.update(((s + 1) / lines.length) * 100, { done: s + 1, total: lines.length });
}

let pk = 1e-6;
for (let i = 0; i < LEN; i++) { const a = Math.abs(out[i]); if (a > pk) pk = a; }
const norm = 0.94 / pk;
for (let i = 0; i < LEN; i++) out[i] *= norm;

const rawOut = `${TMP}/${SLUG}-narration.f32`;
writeFileSync(rawOut, Buffer.from(out.buffer, out.byteOffset, out.byteLength));
const enc = ff(["-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawOut,
  ...(REVERB ? ["-af", "aecho=0.85:0.7:55|130|240:0.4|0.24|0.12"] : []),
  "-c:a", "libmp3lame", "-q:a", "2", OUT]);
progress.end();
if (enc.status !== 0) { console.error("✗ encode failed"); process.exit(1); }
console.log(`✓ ${OUT.replace(LANE + "/", "")}  (phrases one-after-another on beat starts${REVERB ? ", reverb tail" : ""})`);
