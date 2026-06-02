#!/usr/bin/env node
// make-stamp.mjs — the "aesthetic dot computer" voice ID, in jeffrey-pvc
// (ElevenLabs), pitched UP into a bright stamp. The source phrase is
// generated once by `pop/bin/say.mjs` (paid /api/say, cached locally) into
// voice-takes/stamp/cawmpoter-jeffrey.mp3; this just pitch-shifts it — it
// never re-hits the API.
//
// At bake time the single crow CAW lands on the "com" of "computer" so the
// whole word reads as "cawmpoter" (caw + mputer). @jeffrey wanted the FULL
// word audible (an earlier cut chopped it), in jeffrey's voice, pitched up.
//
// Reads:  pop/big-pictures/voice-takes/stamp/cawmpoter-jeffrey.mp3
// Writes: pop/big-pictures/out/cawmpoter-stamp.wav  (stereo 48k, pitched)
//
// Usage: node pop/big-pictures/bin/make-stamp.mjs [--semitones 5]

import { execSync } from "node:child_process";
import { existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");

const argv = process.argv.slice(2);
function flag(name, def) {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : def;
}
const SEMITONES = parseFloat(flag("semitones", "5"));
const STRETCH   = parseFloat(flag("stretch", "2.0"));   // time-stretch — draw it out (1 = original)
const SRC = resolve(POP, "big-pictures/voice-takes/stamp/cawmpoter-jeffrey.mp3");
const OUT = resolve(POP, "big-pictures/out/cawmpoter-stamp.wav");
mkdirSync(dirname(OUT), { recursive: true });

if (!existsSync(SRC)) {
  console.error(`✗ jeffrey source missing: ${SRC}\n  generate it once:\n` +
    `  node pop/bin/say.mjs pop/big-pictures/voice-takes/stamp/cawmpoter.txt ` +
    `--provider jeffrey --voice neutral:0 --out ${SRC}`);
  process.exit(1);
}

// mono 48k → pitch up (--formant keeps it voice-like, not chipmunk) → stereo
const mono = "/tmp/cawmpoter-mono.wav";
execSync(`ffmpeg -y -loglevel error -i "${SRC}" -ar 48000 -ac 1 "${mono}"`);
const pitched = "/tmp/cawmpoter-pitched.wav";
// pitch up + time-stretch in one pass (--time = duration multiplier) so the
// stamp draws out slow and spacious over time (@jeffrey).
execSync(`rubberband --pitch ${SEMITONES} --time ${STRETCH} --formant --crisp 5 "${mono}" "${pitched}" 2>/dev/null`);
execSync(`ffmpeg -y -loglevel error -i "${pitched}" ` +
  `-af "loudnorm=I=-16:TP=-2:LRA=6,aformat=channel_layouts=stereo" ` +
  `-ar 48000 -ac 2 -c:a pcm_s16le "${OUT}"`);
const dur = parseFloat(execSync(
  `ffprobe -v error -show_entries format=duration -of csv=p=0 "${OUT}"`).toString().trim());
console.log(`✓ ${OUT.replace(POP + "/", "pop/")} (${dur.toFixed(2)}s) — jeffrey-pvc "aesthetic dot computer" +${SEMITONES}st`);
