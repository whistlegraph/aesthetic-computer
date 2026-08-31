#!/usr/bin/env node
// gen-accomp.mjs — the imab accompaniment bed (no click): four hook
// cycles (32 bars) of kick / hats / shaker, bass roots, vibraphone
// chords and the hook soft on xylophone. The aesthetivox demo sings
// over this. Score truth: imab.np.
//
//   node pop/imab/bin/gen-accomp.mjs

import { mixEventMarimba } from "../../marimba/synths/marimba.mjs";
import { mixKick, mixHat, mixShaker } from "../../marimba/synths/perc.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
mkdirSync(OUT, { recursive: true });
const SR = 48_000;
const BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const argvA = process.argv.slice(2);
const flagA = (n, d) => { const i = argvA.indexOf(`--${n}`); return i >= 0 && argvA[i + 1] ? Number(argvA[i + 1]) : d; };
const XPOSE = flagA("transpose", 0);   // shift the whole bed (band tunes to the singer)
const LEAD = flagA("lead", 0.42);      // xylophone hook gain (0 = vocal-only bed)
const NAME = XPOSE || LEAD !== 0.42 ? `imab-accomp-124-x${XPOSE}` : "imab-accomp-124";
const CYCLES = 4, BARS = CYCLES * 8;
const NS = Math.ceil((BARS * BAR + 1.5) * SR);

// The MEASURED melody (imab.np — corpus-derived, 35 takes): tonic chant
// on A3, octave flare on "-PING", 4th→3rd→2nd walk through "costume".
const HOOK = [
  [1, 1, 57, 0.5], [1, 1.75, 57, 0.25], [1, 2, 57, 1], [1, 3, 57, 1], [1, 4, 57, 1],
  [2, 1.5, 57, 0.5], [2, 2.5, 69, 1], [2, 3.5, 57, 0.5], [2, 4, 57, 0.5], [2, 4.5, 57, 0.5],
  [3, 2.5, 62, 0.5], [3, 4, 62, 0.5], [3, 4.5, 61, 0.5],
  [4, 1, 59, 1.5], [4, 3, 59, 0.5], [4, 3.5, 59, 0.5], [4, 4, 57, 0.5], [4, 4.5, 57, 0.5],
  [5, 1, 56, 0.5], [5, 1.5, 57, 1.5],
];
const ROOTS = [45, 45, 50, 50, 45, 45, 45, 45];
const CHORDS = [[57, 61, 64], [57, 61, 64], [57, 62, 66], [57, 62, 66],
                [57, 61, 64], [57, 61, 64], [57, 61, 64], [57, 61, 64]];

const buf = new Float32Array(NS);
const mm = (ev) => mixEventMarimba(ev, buf, { sampleRate: SR });
for (let cyc = 0; cyc < CYCLES; cyc++) {
  const full = cyc > 0;                       // cycle 1 is the lighter intro
  for (let bar = 0; bar < 8; bar++) {
    const t0 = (cyc * 8 + bar) * BAR;
    mm({ startSec: t0, midi: ROOTS[bar] + XPOSE, durSec: 2 * BEAT, gain: 0.7, preset: "bass", decayMul: 0.8 });
    for (const midi of CHORDS[bar])
      mm({ startSec: t0, midi: midi + XPOSE, durSec: 3.5 * BEAT, gain: 0.16, preset: "vibraphone", decayMul: 1.6 });
    for (let beat = 0; beat < 4; beat++) {
      mixKick({ startSec: t0 + beat * BEAT, gain: full ? 0.72 : 0.5 }, buf, { sampleRate: SR });
      mixHat({ startSec: t0 + (beat + 0.5) * BEAT, gain: 0.2 }, buf, { sampleRate: SR });
      if (full) mixHat({ startSec: t0 + (beat + 0.5) * BEAT, gain: beat === 3 ? 0.2 : 0, open: beat === 3 }, buf, { sampleRate: SR });
      for (let s = 0; s < 4; s++)
        mixShaker({ startSec: t0 + (beat + s / 4) * BEAT, gain: 0.09 + 0.05 * Math.sin((beat * 4 + s) * Math.PI / 8) }, buf, { sampleRate: SR });
    }
  }
  for (const [bar, beat, midi, durB] of HOOK)
    mm({ startSec: (cyc * 8 + bar - 1) * BAR + (beat - 1) * BEAT, midi: midi + XPOSE,
         durSec: durB * BEAT, gain: LEAD, preset: "xylophone", decayMul: 1.1 });
}
let pk = 0; for (let i = 0; i < NS; i++) pk = Math.max(pk, Math.abs(buf[i]));
if (pk > 0.9) for (let i = 0; i < NS; i++) buf[i] *= 0.9 / pk;

const st = new Float32Array(NS * 2);
for (let i = 0; i < NS; i++) { st[2 * i] = buf[i]; st[2 * i + 1] = buf[i]; }
const raw = resolve(OUT, ".accomp.raw");
writeFileSync(raw, Buffer.from(st.buffer));
for (const [ext, args] of [["wav", ["-c:a", "pcm_s16le"]], ["mp3", ["-c:a", "libmp3lame", "-q:a", "2"]]]) {
  const dest = resolve(OUT, `${NAME}.${ext}`);
  spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", raw,
    "-metadata", `title=${NAME}`, "-metadata", "artist=Whistlegraph Dot Org",
    ...args, dest], { stdio: "inherit" });
  console.log(`✓ ${dest}`);
}
unlinkSync(raw);
