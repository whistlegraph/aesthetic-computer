#!/usr/bin/env node
// sing-hook.mjs — the imab aesthetivox demo: jeffrey's TTS voice sings
// the hook (imab.np) syllable-by-syllable, spread on the grid and WORLD
// pitch-snapped to the melody, then laid over the accompaniment bed so
// melody + arrangement can be judged together.
//
// Route (the proven marimba sing-hook shape): spinging say → whisper
// align → slice each syllable → rubberband stretch to its note →
// pitchsnap_world f0-replace (−12 into jeffrey's range) → mix.
//
//   node pop/imab/bin/sing-hook.mjs          (needs gen-accomp.mjs first)
//   → out/imab-hookvox.mp3 (dry vox) + out/imab-vox-demo.mp3 (over the bed)

import { readFileSync, writeFileSync, existsSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out");
const SR = 48_000;
const BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const TRANSPOSE = -12;
const VOCAL = `${OUT}/imab-hook-vocal.mp3`;
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "inherit", "inherit"], ...opts });

// ── 1 · the spoken source (cached — never re-spent) ───────────────────
if (!existsSync(VOCAL)) {
  console.log("→ spinging say (jeffrey voice)");
  const r = sh("node", [`${REPO}/spinging/bin/spinging.mjs`, "say", `${LANE}/imab-hook.sing.txt`, "--out", VOCAL]);
  if (r.status !== 0 || !existsSync(VOCAL)) { console.error("✗ say failed"); process.exit(1); }
}
const WORDSJSON = VOCAL.replace(/\.mp3$/, "-words.json");
if (!existsSync(WORDSJSON)) {
  console.log("→ whisper align");
  const r = sh("node", [`${REPO}/spinging/bin/spinging.mjs`, "align", VOCAL]);
  if (r.status !== 0 || !existsSync(WORDSJSON)) { console.error("✗ align failed"); process.exit(1); }
}
const WORDS = JSON.parse(readFileSync(WORDSJSON, "utf8"));
console.log(`  aligned words: ${WORDS.map((w, i) => `${i}:${w.text}`).join(" ")}`);
if (WORDS.length !== 22) {
  console.error(`✗ expected 22 aligned words, got ${WORDS.length} — adjust the SYL map below`);
  process.exit(1);
}

// ── 2 · syllables onto the melody (imab.np, bar-relative → beats) ─────
const W = (wi, k = 0, n = 1) => ({ wi, k, n });
const SYL = [
  { s: "i'm", ...W(0) }, { s: "a", ...W(1) }, { s: "but", ...W(2, 0, 3) }, { s: "ter", ...W(2, 1, 3) }, { s: "fly", ...W(2, 2, 3) },
  { s: "i", ...W(3) }, { s: "mab", ...W(4) },
  { s: "i'm", ...W(5) }, { s: "a", ...W(6) }, { s: "but", ...W(7, 0, 3) }, { s: "ter", ...W(7, 1, 3) }, { s: "fly", ...W(7, 2, 3) },
  { s: "i", ...W(8) }, { s: "mab", ...W(9) },
  { s: "i'm", ...W(10) }, { s: "a", ...W(11) }, { s: "but", ...W(12, 0, 3) }, { s: "ter", ...W(12, 1, 3) }, { s: "fly", ...W(12, 2, 3) },
  { s: "fly", ...W(13, 0, 2) }, { s: "fly", ...W(13, 1, 2) },
  { s: "no", ...W(14, 0, 2) }, { s: "thing", ...W(14, 1, 2) }, { s: "holds", ...W(15) }, { s: "me", ...W(16) }, { s: "down", ...W(17) },
  { s: "i", ...W(18) }, { s: "mab", ...W(19) }, { s: "i", ...W(20) }, { s: "mab", ...W(21) },
];
const MEL = [                                    // [midi, startBeat, durBeats]
  [76, 0, 0.5], [79, 0.5, 0.5], [84, 1, 1.5], [83, 2.5, 0.5], [79, 3, 1],
  [69, 7, 0.5], [76, 7.5, 0.5],
  [74, 8, 0.5], [79, 8.5, 0.5], [83, 9, 1.5], [81, 10.5, 0.5], [79, 11, 1],
  [69, 15, 0.5], [76, 15.5, 0.5],
  [76, 16, 0.5], [79, 16.5, 0.5], [84, 17, 1.5], [86, 18.5, 0.5], [88, 19, 1],
  [88, 20, 2], [84, 22, 1.5],
  [88, 24, 0.5], [86, 24.5, 0.5], [84, 25, 0.5], [79, 25.5, 0.5], [81, 26, 1.5],
  [69, 28, 0.5], [76, 28.5, 1], [69, 30, 0.5], [76, 30.5, 1],
];
if (SYL.length !== MEL.length) { console.error(`✗ ${SYL.length} syllables vs ${MEL.length} notes`); process.exit(1); }

const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const midiToName = (m) => NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);
const tmp = mkdtempSync(join(tmpdir(), "imabhook-"));
const readF32 = (wav) => {
  const raw = join(tmp, "r.f32");
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};

const lastBeat = MEL[MEL.length - 1][1] + MEL[MEL.length - 1][2];
const master = new Float32Array(Math.ceil((lastBeat * BEAT + 1.0) * SR));
const noteStarts = [], noteNames = [];
for (let i = 0; i < SYL.length; i++) {
  const s = SYL[i], w = WORDS[s.wi], [midi, sb, db] = MEL[i];
  const wDur = (w.toMs - w.fromMs) / 1000;
  const srcStart = w.fromMs / 1000 + (s.k / s.n) * wDur;
  const srcDur = Math.max(0.05, wDur / s.n);
  const tgtDur = db * BEAT;
  const clip = join(tmp, `c${i}.wav`), str = join(tmp, `s${i}.wav`);
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-ss", srcStart.toFixed(3), "-t", srcDur.toFixed(3), "-i", VOCAL, "-ac", "1", "-ar", String(SR), clip]);
  const ratio = Math.min(6, Math.max(0.5, (tgtDur * 0.92) / srcDur));
  sh("rubberband", ["-t", ratio.toFixed(4), "-F", "-c", "5", clip, str]);
  const seg = readF32(str);
  const off = Math.floor(sb * BEAT * SR);
  for (let j = 0; j < seg.length; j++) { const d = off + j; if (d < master.length) master[d] += seg[j]; }
  noteStarts.push((sb * BEAT).toFixed(3));
  noteNames.push(midiToName(midi + TRANSPOSE));
}
const dry = join(tmp, "dry.f32"), dryWav = join(tmp, "dry.wav");
writeFileSync(dry, Buffer.from(master.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", dry, dryWav]);

console.log(`→ ${SYL.length} syllables spread · WORLD pitch (transpose ${TRANSPOSE})`);
const pit = join(tmp, "pitched.wav");
const r = spawnSync(`${REPO}/pop/.venv/bin/python`, [
  `${REPO}/pop/bin/pitchsnap_world.py`, dryWav, pit,
  "--notes", noteNames.join(","), "--note-starts", noteStarts.join(","),
  "--retain", "1.0", "--xfade-ms", "30", "--voicing-ramp-ms", "20",
  "--vibrato-hz", "5.2", "--vibrato-cents", "16",
], { stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("✗ WORLD failed"); process.exit(1); }
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", pit, "-c:a", "libmp3lame", "-q:a", "2", `${OUT}/imab-hookvox.mp3`]);
console.log(`✓ ${OUT}/imab-hookvox.mp3`);

// ── 3 · over the bed: cycle 1 instrumental, the vox sings cycles 2–4 ──
const ACC = `${OUT}/imab-accomp-124.wav`;
if (!existsSync(ACC)) { console.error("✗ run gen-accomp.mjs first"); process.exit(1); }
const acc = readF32(ACC);                        // folds stereo to mono
const vox = readF32(pit);
const rms = (a) => { let s = 0; for (let i = 0; i < a.length; i++) s += a[i] * a[i]; return Math.sqrt(s / a.length); };
const vg = Math.min(3.5, (rms(acc) * 1.35) / Math.max(1e-9, rms(vox)));
const mixb = Float32Array.from(acc);
for (let cyc = 1; cyc < 4; cyc++) {
  const off = Math.floor(cyc * 8 * BAR * SR);
  for (let j = 0; j < vox.length; j++) { const d = off + j; if (d < mixb.length) mixb[d] += vox[j] * vg; }
}
let pk = 0; for (let i = 0; i < mixb.length; i++) pk = Math.max(pk, Math.abs(mixb[i]));
if (pk > 0.9) for (let i = 0; i < mixb.length; i++) mixb[i] *= 0.9 / pk;
const st = new Float32Array(mixb.length * 2);
for (let i = 0; i < mixb.length; i++) { st[2 * i] = mixb[i]; st[2 * i + 1] = mixb[i]; }
const mraw = join(tmp, "demo.f32");
writeFileSync(mraw, Buffer.from(st.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", mraw,
  "-metadata", "title=imab-vox-demo", "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "libmp3lame", "-q:a", "2", `${OUT}/imab-vox-demo.mp3`]);
rmSync(tmp, { recursive: true, force: true });
console.log(`✓ ${OUT}/imab-vox-demo.mp3 (vox gain ${vg.toFixed(2)})`);
