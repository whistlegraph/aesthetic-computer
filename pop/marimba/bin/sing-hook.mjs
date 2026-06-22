#!/usr/bin/env node
// sing-hook.mjs — spread the flutterbap hook lyric SYLLABLE-BY-SYLLABLE across
// the melody. Each syllable is sliced from jeffrey's natural vocal, time-
// stretched (rubberband, formant-preserving) to its melody note's duration,
// placed at that note's beat time, then the whole assembly is pitched to the
// melody (WORLD f0-replace, octave-down into jeffrey's baritone).
//
// Why bespoke: flutterbap.np is bare-note notation that score-pitch/pitchsnap
// can't parse, so the melody map lives here explicitly.
//
//   node pop/marimba/bin/sing-hook.mjs   → pop/marimba/out/flutterbap-hookvox.mp3

import { readFileSync, writeFileSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";

const POP = "/Users/jas/aesthetic-computer/pop";
const SR = 48000;
const BPM = 124, BEAT = 60 / BPM;
const VOCAL = `${POP}/marimba/out/flutterbap-vocal.mp3`;
const WORDS = JSON.parse(readFileSync(`${POP}/marimba/out/flutterbap-vocal-words.json`, "utf8"));
const OUT = `${POP}/marimba/out/flutterbap-hookvox.mp3`;
const TRANSPOSE = -12;   // octave down → jeffrey baritone

// hook lyric, broken to syllables; each entry says which aligned WORD it comes
// from (index into WORDS) and which slice of that word (k of n).
const W = (i, k = 0, n = 1) => ({ wi: i, k, n });
const SYL = [
  // flut-ter-bap is my friend         (word 0..3)
  { s: "flut", ...W(0, 0, 3) }, { s: "ter", ...W(0, 1, 3) }, { s: "bap", ...W(0, 2, 3) },
  { s: "is", ...W(1) }, { s: "my", ...W(2) }, { s: "friend", ...W(3) },
  // flut-ter-bap thats my man         (word 4..7)
  { s: "flut", ...W(4, 0, 3) }, { s: "ter", ...W(4, 1, 3) }, { s: "bap", ...W(4, 2, 3) },
  { s: "thats", ...W(5) }, { s: "my", ...W(6) }, { s: "man", ...W(7) },
  // i'll pull up next to him          (word 8..13)
  { s: "ill", ...W(8) }, { s: "pull", ...W(9) }, { s: "up", ...W(10) },
  { s: "next", ...W(11) }, { s: "to", ...W(12) }, { s: "him", ...W(13) },
  // flut-ter-bap thats my guy         (word 14..17)
  { s: "flut", ...W(14, 0, 3) }, { s: "ter", ...W(14, 1, 3) }, { s: "bap", ...W(14, 2, 3) },
  { s: "thats", ...W(15) }, { s: "my", ...W(16) }, { s: "guy", ...W(17) },
  // right there till the end          (word 18..22)
  { s: "right", ...W(18) }, { s: "there", ...W(19) },
  { s: "till", ...W(20) }, { s: "the", ...W(21) }, { s: "end", ...W(22) },
];

// melody: first 29 notes of the hook (butterfly + palofmine + mommywow), as
// [midi, startBeat, durBeats] in body-relative beats. The vocal locks to the
// xylophone line.
const MEL = [
  [64, 0, 1], [67, 1, 1], [72, 2, 2], [72, 4, 1], [76, 5, 1], [72, 6, 2],          // butterfly 1-2
  [62, 8, 1], [67, 9, 1], [71, 10, 2], [72, 12, 2], [67, 14, 2],                    // butterfly 3-4
  [64, 16, 1], [67, 17, 1], [72, 18, 2], [69, 20, 1], [67, 21, 1], [65, 22, 2],     // palofmine 5-6
  [64, 24, 1], [65, 25, 1], [67, 26, 1], [69, 27, 1], [72, 28, 4],                  // palofmine 7-8
  [67, 32, 4], [72, 36, 1], [76, 37, 1], [79, 38, 2], [67, 40, 4], [79, 44, 1], [76, 45, 1],  // mommywow 9-12 (to 29)
];
if (SYL.length !== MEL.length) { console.error(`✗ ${SYL.length} syllables vs ${MEL.length} notes`); process.exit(1); }

const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const midiToName = (m) => NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);

const tmp = mkdtempSync(join(tmpdir(), "fbhook-"));
const sh = (cmd, args) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"] });
const readF32 = (wav) => {
  const raw = join(tmp, "r.f32");
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};

// assemble the spread (dry) vocal
const lastBeat = MEL[MEL.length - 1][1] + MEL[MEL.length - 1][2];
const master = new Float32Array(Math.ceil((lastBeat * BEAT + 1.0) * SR));
const noteStarts = [], noteNames = [];
for (let i = 0; i < SYL.length; i++) {
  const s = SYL[i], w = WORDS[s.wi], [midi, sb, db] = MEL[i];
  const wDur = (w.toMs - w.fromMs) / 1000;
  const srcStart = w.fromMs / 1000 + (s.k / s.n) * wDur;
  const srcDur = Math.max(0.05, wDur / s.n);
  const tgtDur = db * BEAT;
  // slice the syllable, then stretch to the note duration
  const clip = join(tmp, `c${i}.wav`), str = join(tmp, `s${i}.wav`);
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", srcStart.toFixed(3), "-t", srcDur.toFixed(3), "-i", VOCAL, "-ac", "1", "-ar", String(SR), clip]);
  const ratio = Math.min(6, Math.max(0.5, (tgtDur * 0.92) / srcDur));   // fill ~92% of the note, leave a breath
  sh("rubberband", ["-t", ratio.toFixed(4), "-F", "-c", "5", clip, str]);
  const seg = readF32(str);
  const off = Math.floor(sb * BEAT * SR);
  for (let j = 0; j < seg.length; j++) { const d = off + j; if (d < master.length) master[d] += seg[j]; }
  noteStarts.push((sb * BEAT).toFixed(3));
  noteNames.push(midiToName(midi + TRANSPOSE));
}

// write dry spread wav
const dry = join(tmp, "dry.f32"), dryWav = join(tmp, "dry.wav");
writeFileSync(dry, Buffer.from(master.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", dry, dryWav]);

// pitch the spread vocal to the melody (WORLD)
console.log(`→ ${SYL.length} syllables spread across the melody · WORLD pitch (transpose ${TRANSPOSE})`);
const pit = join(tmp, "pitched.wav");
const r = spawnSync(`${POP}/.venv/bin/python`, [
  `${POP}/bin/pitchsnap_world.py`, dryWav, pit,
  "--notes", noteNames.join(","), "--note-starts", noteStarts.join(","),
  "--retain", "1.0", "--xfade-ms", "30", "--voicing-ramp-ms", "20",
  "--vibrato-hz", "5.2", "--vibrato-cents", "16",
], { stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("✗ WORLD failed"); process.exit(1); }

sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", pit, "-c:a", "libmp3lame", "-q:a", "2", OUT]);
rmSync(tmp, { recursive: true, force: true });
console.log(`✓ ${OUT}`);
