#!/usr/bin/env node
// vocal.mjs — generate a jeffrey-pvc vocal stem for the dance lane.
//
// Posts the lyric to /api/say (the same ElevenLabs proxy the recap
// pipeline uses), then applies optional post-processing that's typical
// for trance vocal hooks: a soft octave-up shift, a long reverb tail,
// and a gentle highpass to keep it from muddying the supersaw.
//
// Output stem is meant to be mixed into trance.mjs via --vocal-stem.
//
// Usage:
//   node pop/dance/bin/vocal.mjs --lyric "the music is real" \
//       --out ~/Desktop/trance-vox.mp3
//
//   # with octave-up + reverb (anthemic hook)
//   node pop/dance/bin/vocal.mjs --lyric "the music is real" \
//       --pitch-shift 0 --reverb 0.6 --out ~/Desktop/trance-vox.mp3
//
//   # cached: if the lyric + voice config hash matches, reuse stem.

import {
  writeFileSync, readFileSync, existsSync, mkdirSync, unlinkSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";
import { createHash } from "node:crypto";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const LYRIC       = flags.lyric || "A. B. C. D. one. two. three. four. C. E. G. A.";
const PROVIDER    = flags.provider || "jeffrey";
const VOICE_ID    = flags.voice || "neutral:0";
const OUT_PATH    = expandHome(flags.out) || `/tmp/trance-vocal.mp3`;
const PITCH_SHIFT = Number(flags["pitch-shift"] ?? 0); // semitones
const REVERB      = Number(flags.reverb ?? 0.5);       // 0..1
const HIGHPASS_HZ = Number(flags.highpass ?? 200);
const FORCE       = !!flags.force;

// ── cache key ─────────────────────────────────────────────────────────
const inputHash = createHash("sha256")
  .update(JSON.stringify({ LYRIC, PROVIDER, VOICE_ID, PITCH_SHIFT, REVERB, HIGHPASS_HZ }))
  .digest("hex")
  .slice(0, 16);
const hashFile = `${OUT_PATH}.hash`;

if (!FORCE && existsSync(OUT_PATH) && existsSync(hashFile)) {
  const cached = readFileSync(hashFile, "utf8").trim();
  if (cached === inputHash) {
    const sz = (readFileSync(OUT_PATH).length / 1024).toFixed(0);
    console.log(`✓ ${OUT_PATH} cached (${sz} KB · ${inputHash}) — skipping /api/say`);
    process.exit(0);
  }
}

// ── POST /api/say ─────────────────────────────────────────────────────
console.log(`→ POST /api/say · "${LYRIC}" · ${PROVIDER}:${VOICE_ID}`);
const res = await fetch("https://aesthetic.computer/api/say", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({ from: LYRIC, provider: PROVIDER, voice: VOICE_ID }),
  redirect: "follow",
});
if (!res.ok) {
  console.error(`✗ /api/say returned ${res.status}: ${await res.text()}`);
  process.exit(1);
}
const raw = Buffer.from(await res.arrayBuffer());
mkdirSync(dirname(OUT_PATH), { recursive: true });
const dryPath = `${OUT_PATH}.dry.mp3`;
writeFileSync(dryPath, raw);
console.log(`  TTS · ${(raw.length / 1024).toFixed(0)} KB`);

// ── post-process: pitch shift + reverb + highpass + loudnorm ────────
// /api/say returns ~-19 dBFS peak audio. Without normalization, the
// vocal disappears under any bed. Apply loudnorm (target -8 LUFS) so
// the stem is loud on disk; trance.mjs ALSO peak-normalizes after
// decoding for belt-and-suspenders.
const filters = [];
filters.push(`highpass=f=${HIGHPASS_HZ}`);
if (PITCH_SHIFT !== 0) {
  // asetrate changes both pitch + tempo; counter with atempo to keep speed.
  const factor = Math.pow(2, PITCH_SHIFT / 12);
  // Per ffmpeg docs asetrate works on sample rate. We'll do it as a chain:
  // asetrate=R*factor,aresample=R,atempo=1/factor
  filters.push(`asetrate=48000*${factor.toFixed(6)}`);
  filters.push(`aresample=48000`);
  // atempo only supports [0.5, 2.0]; chain if needed.
  let remain = 1 / factor;
  while (remain > 2) { filters.push(`atempo=2.0`); remain /= 2; }
  while (remain < 0.5) { filters.push(`atempo=0.5`); remain *= 2; }
  filters.push(`atempo=${remain.toFixed(6)}`);
}
if (REVERB > 0) {
  const r = Math.max(0.05, Math.min(0.95, REVERB));
  filters.push(`aecho=0.8:0.9:60|180|360|720:${(r * 0.6).toFixed(2)}|${(r * 0.45).toFixed(2)}|${(r * 0.3).toFixed(2)}|${(r * 0.2).toFixed(2)}`);
}
// Loudness normalize the result — push the stem to a much higher
// average level so it sits in front of the bed.
filters.push(`loudnorm=I=-8:TP=-1.5:LRA=11`);
const filterStr = filters.join(",");

const args = [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", dryPath,
  "-af", filterStr,
  "-c:a", "libmp3lame", "-q:a", "3",
  OUT_PATH,
];
console.log(`→ post · ${filterStr}`);
const ff = spawnSync("ffmpeg", args, { stdio: "inherit" });
try { unlinkSync(dryPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }

writeFileSync(hashFile, inputHash + "\n");
const finalSize = (readFileSync(OUT_PATH).length / 1024).toFixed(0);
console.log(`✓ ${OUT_PATH} (${finalSize} KB · ${inputHash})`);
