#!/usr/bin/env node
// narrate.mjs — jeffrey-pvc story narrator for a chillwave story cut.
//
// Says each line of <slug>.narration.txt in jeffrey-pvc (via the local
// say endpoint) and BEAT-ALIGNS it to its section's start in <slug>.np,
// producing a full-length narration stem out/<slug>-narration.mp3 that
// render.mjs mixes in with --vocal-stem (--vocal-start 0).
//
// One narration line per section, in section order. A light ring-mod
// gives jeffrey's voice a faint robotic edge.
//
// Needs the local say endpoint running:  node bin/say-local.mjs
//
// Usage:
//   node bin/narrate.mjs --slug helpabeach-short
//   node bin/narrate.mjs --slug helpabeach-short --robot 0.2 --force

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

const SLUG  = flags.slug || "helpabeach-short";
const BPM   = Number(flags.bpm ?? 84);
const ROBOT = Number(flags.robot ?? 0.15);   // 0 = dry jeffrey, ~0.15 = faint robot
const FORCE = flags.force === true;
const SR    = 48000;
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

// ── section start times (seconds) from the .np ───────────────────────
const beat = 60 / BPM;
function sectionStarts(npPath) {
  // end-anchored — `# name [n] bars [flags]`, never a prose comment line
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
  return { starts, totalSec: pos * beat };
}

const lines = readFileSync(TXT, "utf8").split("\n")
  .map((l) => l.trim())
  .filter((l) => l && !l.startsWith("#"));
const { starts, totalSec } = sectionStarts(NP);
if (lines.length !== starts.length) {
  console.warn(`  ⚠ ${lines.length} narration lines vs ${starts.length} sections — pairing by index`);
}
console.log(`▸ ${SLUG} narrator · ${lines.length} lines · ${starts.length} sections · ${totalSec.toFixed(1)}s`);

// ── jeffrey-pvc say (content-hash cached — bills ElevenLabs once) ─────
async function sayLine(text) {
  const hash = createHash("sha256").update(`jeffrey-pvc:${text}`).digest("hex").slice(0, 16);
  const cached = `${TMP}/${hash}.mp3`;
  if (!FORCE && existsSync(cached)) return cached;
  const res = await fetch(SAY_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      from: text, provider: "jeffrey", voice: "neutral:0",
      stability: 0.6, similarity: 0.92, style: 0.2,
    }),
  });
  if (!res.ok) {
    console.error(`✗ say ${res.status}: ${(await res.text()).slice(0, 240)}`);
    console.error("  is say-local running?  node bin/say-local.mjs");
    process.exit(1);
  }
  writeFileSync(cached, Buffer.from(await res.arrayBuffer()));
  return cached;
}

function decodeMono(mp3) {
  const raw = `${mp3}.f32`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", mp3, "-f", "f32le", "-ar", String(SR), "-ac", "1", raw]);
  if (r.status !== 0) { console.error("✗ decode failed"); process.exit(1); }
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, b.byteLength / 4);
}

// ── say every line, beat-place it at its section start ───────────────
progress.begin({ type: "audio", label: `${SLUG} narration` });
const LEN = Math.ceil((totalSec + 3) * SR);
const out = new Float32Array(LEN);
for (let i = 0; i < lines.length; i++) {
  const clip = decodeMono(await sayLine(lines[i]));
  const at = Math.floor((starts[i] ?? (i * 8)) * SR);
  for (let j = 0; j < clip.length; j++) {
    const d = at + j;
    if (d >= LEN) break;
    let s = clip[j];
    // light ring-mod — a faint robotic edge, jeffrey still recognizable
    if (ROBOT > 0) {
      const ring = Math.sin(2 * Math.PI * 47 * (j / SR));
      s = s * (1 - ROBOT) + s * ring * (ROBOT * 1.5);
    }
    out[d] += s;
  }
  console.log(`  §${i} @ ${(starts[i] ?? 0).toFixed(1)}s · "${lines[i]}"`);
  progress.update(((i + 1) / lines.length) * 100);
}

// peak-normalize the assembled stem
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
console.log(`✓ ${OUT.replace(LANE + "/", "")}  (${(LEN / SR).toFixed(1)}s · robot ${ROBOT})`);
console.log(`  mix:  node bin/render.mjs --slug ${SLUG} --no-chimes --no-waves --no-sweeps --no-highmel --no-voice \\`);
console.log(`        --vocal-stem out/${SLUG}-narration.mp3 --vocal-start 0 --vocal-gain 1.4 --vocal-duck 0.5`);
