#!/usr/bin/env node
// render.mjs — the booch bed builder. Mirrors the recap/trap + dance +
// hippyhayzard cli shape: read the arrangement, mix the bed in node from
// AC instruments, write one file (mp3 by default, wav with --wav).
// No Web Audio, no Suno — every sample is synthesized here (bottom-up
// posture, pop/SCORE.md).
//
// The lane is "fermented hip-hop" — Latifah golden-era boom-bap
// foundation collided with Timbaland future-funk sparseness:
//
//   • LATIFAH-FULL — warm boom-bap pocket. Boom kick on 1 & 3, bap
//                    snare on 2 & 4, swung 16th closed hats, Rhodes
//                    stab chord on 2 & 4, sub bass on chord roots,
//                    vinyl crackle bed.
//   • TIMBALAND-SPARSE — same kick + snare + sub, drop hats + Rhodes,
//                    bring single tabla-style melodic accent +
//                    hard-gated rest bars (silence-as-percussion).
//   • BRIDGE       — drop drums, just vinyl + soft Rhodes + sub
//                    pulses. Talky / breakdown space.
//   • INTRO/OUTRO  — vinyl + soft sub roll only.
//
// Voices: ../synths/vinyl.mjs, ../synths/rhodes.mjs, ../synths/sub.mjs
// (new for this lane), + ../../hippyhayzard/synths/zitar.mjs (reused for
// the Timbaland-mode tumbi accent), + inline drum synthesis (kick /
// snare / closed-hat).
//
//   node pop/booch/bin/render.mjs               # → out/visualize-my-booch.mp3
//   node pop/booch/bin/render.mjs --wav         # → out/visualize-my-booch.wav
//   node pop/booch/bin/render.mjs --bpm 96 --out ~/b.wav

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { mixVinyl }       from "../synths/vinyl.mjs";
import { mixEventRhodes } from "../synths/rhodes.mjs";
import { mixEventSub }    from "../synths/sub.mjs";
import { mixEventZitar }  from "../../hippyhayzard/synths/zitar.mjs";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const BPM = Number(flags.bpm ?? 94);
const beat = 60 / BPM;
const bar = beat * 4;
const sx = beat / 4;                 // one 16th note

// Swing: how much to delay every off-16th (2nd, 4th, 6th, ... within a
// beat). 0 = straight, 1/3 = full triplet pull. The README spec is ~63%
// pull-back from triplet — i.e. push the off-16th 16% of a 16th later
// than straight (between straight 50% and triplet 66.7%).
const SWING = 0.16;

// ── inline drum synthesis (boom-bap kit) ───────────────────────────────
function add(buf, idx, v) { if (idx >= 0 && idx < buf.length) buf[idx] += v; }

// Boom kick — fat round 808-ish thump tuned for 94 BPM pocket. Slightly
// longer pitch envelope than the hippyhayzard chunky kick (warmer, less
// punchy). Sub-octave click suppressed for the dusty SP-1200 feel.
function kick(buf, startSec, g = 1.0) {
  const dur = 0.32, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const f = 50 + (105 - 50) * Math.exp(-t * 28);      // boom drop, longer
    ph += (2 * Math.PI * f) / SR;
    const body = Math.sin(ph) * Math.exp(-t * 9);       // long body
    const click = i < SR * 0.0025 ? (Math.random() * 2 - 1) * 0.35 * (1 - i / (SR * 0.0025)) : 0;
    add(buf, s0 + i, (body + click) * g);
  }
}

// Bap snare — SP-1200 style. Filtered noise burst + tonal body, low
// pitch (185 Hz), short decay, slight room thump from a 220 Hz tone.
function snare(buf, startSec, g = 0.85) {
  const dur = 0.18, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let prev = 0, ph1 = 0, ph2 = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    ph1 += (2 * Math.PI * 185) / SR;
    ph2 += (2 * Math.PI * 220) / SR;
    const tone = (Math.sin(ph1) * 0.5 + Math.sin(ph2) * 0.3) * Math.exp(-t * 32);
    add(buf, s0 + i, (hp * Math.exp(-t * 24) + tone) * g);
  }
}

// Closed hat — short, narrow, with a slight metallic ring (filtered
// noise + a tiny bandpass tick). Quiet by default — rides under the kick.
function hat(buf, startSec, g = 0.22, len = 0.024) {
  const n = Math.floor(len * SR), s0 = Math.floor(startSec * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-(i / SR) * 260) * g);
  }
}

// Open hat — longer, breathier. Used sparingly for accents.
function openHat(buf, startSec, g = 0.18, len = 0.18) {
  const n = Math.floor(len * SR), s0 = Math.floor(startSec * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-(i / SR) * 14) * g);
  }
}

// Cross-stick / rim — short woody knock, for the Timbaland sparse mode
// (replaces the bap snare in some bars to thin the pocket).
function rim(buf, startSec, g = 0.7) {
  const dur = 0.06, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    ph += (2 * Math.PI * 1200) / SR;
    const tone = Math.sin(ph) * Math.exp(-t * 90);
    const click = i < 12 ? (Math.random() * 2 - 1) * 0.5 : 0;
    add(buf, s0 + i, (tone + click) * g);
  }
}

// ── the chord progression (D Dorian, 4-bar loop) ───────────────────────
// bar 1: Dm7  (D F A C)         — root 38 (D2)
// bar 2: G9   (G B D F A)       — root 43 (G2), Dorian B for color
// bar 3: Am7  (A C E G)         — root 45 (A2)
// bar 4: Dm7                    — root 38
//
// Voicings — for the Rhodes stab, use upper-structure 7th-chord shapes
// in the F4–C5 register so they sit above the sub bass without muddying
// the kick. (Picked to keep voice-leading minimal between chords.)
const PROG = [
  { root: 38, rhodes: [62, 65, 69, 72]      }, // Dm7:  D F A C
  { root: 43, rhodes: [62, 65, 67, 71]      }, // G9:   D F G B  (no root in voicing)
  { root: 45, rhodes: [60, 64, 67, 69]      }, // Am7:  C E G A
  { root: 38, rhodes: [62, 65, 69, 72]      }, // Dm7
];

// One-bar tumbi accent line (used in the Timbaland-sparse section) —
// short plucks in D Dorian, on the off-beats so they sit in the gaps.
// [16th-index-within-bar, midi].
const TUMBI = [
  [3,  74], [7,  77], [10, 72], [14, 79], // bar 1
  [3,  72], [7,  74], [10, 77], [14, 72], // bar 2
  [3,  74], [7,  79], [10, 77], [14, 72], // bar 3
  [3,  74], [7,  77], [10, 72], [14, 74], // bar 4
];

// ── the form ───────────────────────────────────────────────────────────
// intro 4 → hook 4 → verse1 8 → hook 4 → verse2 8 → bridge 4 → hook 4 → outro 4
// total = 40 bars @ 94 BPM ≈ 102s
const SECTIONS = [
  { name: "intro",   bars: 4,  mode: "intro"     },
  { name: "hook1",   bars: 4,  mode: "latifah"   },
  { name: "verse1",  bars: 8,  mode: "latifah"   },
  { name: "hook2",   bars: 4,  mode: "latifah"   },
  { name: "verse2",  bars: 8,  mode: "timbaland" },
  { name: "bridge",  bars: 4,  mode: "bridge"    },
  { name: "hook3",   bars: 4,  mode: "latifah"   },
  { name: "outro",   bars: 4,  mode: "outro"     },
];
const totalBars = SECTIONS.reduce((a, s) => a + s.bars, 0);
const totalSec = totalBars * bar + 1.5;       // +1.5s tail for ring-out
const N = Math.ceil(totalSec * SR);

// Three buses: melodic (ducked), drums (not ducked), vinyl (very gentle
// duck only). Sub and Rhodes ride the melodic bus.
const bus = new Float32Array(N);
const drm = new Float32Array(N);
const vin = new Float32Array(N);
const kickTimes = [];

// Vinyl crackle runs end-to-end (tape always rolling). Seed it per
// section so different sections get distinct crackle patterns instead of
// one obvious repeating loop.
{
  let t = 0;
  for (const sec of SECTIONS) {
    const len = sec.bars * bar;
    const presetByMode = {
      intro: "hiss", outro: "hiss",
      latifah: "dusty", timbaland: "dusty",
      bridge: "hiss",
    };
    const gainByMode = {
      intro: 0.7, outro: 0.7,
      latifah: 0.55, timbaland: 0.55,
      bridge: 0.8,
    };
    mixVinyl(vin, t, len, {
      sampleRate: SR,
      preset: presetByMode[sec.mode] || "dusty",
      gain: gainByMode[sec.mode] || 0.6,
      seed: `booch:${sec.name}:${BPM}`,
    });
    t += len;
  }
}

// Helper — schedule a chord on a given bar.
function rhodesStab(t0, chordIdx, gain, preset = "stab") {
  const ch = PROG[chordIdx % 4];
  // Boom-bap convention: the chord stab lands on beats 2 and 4 (the
  // snare). Short duration. Sit in the upper-mid register.
  for (const beatOffset of [1, 3]) {
    for (const m of ch.rhodes) {
      mixEventRhodes(
        { startSec: t0 + beatOffset * beat, midi: m, durSec: beat * 0.75, gain, preset },
        bus, { sampleRate: SR, preset },
      );
    }
  }
}

function rhodesPad(t0, chordIdx, gain) {
  const ch = PROG[chordIdx % 4];
  for (const m of ch.rhodes) {
    mixEventRhodes(
      { startSec: t0, midi: m, durSec: bar * 0.95, gain, preset: "mellow" },
      bus, { sampleRate: SR, preset: "mellow" },
    );
  }
}

function subOnRoot(t0, chordIdx, gain, preset = "funk") {
  const ch = PROG[chordIdx % 4];
  // Two sub hits per bar — beat 1 and beat 3 (the kick beats), like a
  // classic P-funk root + 5th walking simplification.
  mixEventSub(
    { startSec: t0, midi: ch.root, durSec: beat * 1.9, gain, preset },
    bus, { sampleRate: SR, preset },
  );
  mixEventSub(
    { startSec: t0 + beat * 2, midi: ch.root, durSec: beat * 1.9, gain, preset },
    bus, { sampleRate: SR, preset },
  );
}

function tumbiBar(t0, gain) {
  for (const [s16, midi] of TUMBI) {
    const sw = s16 % 2 === 1 ? SWING * sx : 0;
    mixEventZitar(
      { startSec: t0 + s16 * sx + sw, midi, durSec: sx * 2.5, gain, preset: "sitar" },
      bus, { sampleRate: SR },
    );
  }
}

// ── render each section ────────────────────────────────────────────────
let cursor = 0;
for (const sec of SECTIONS) {
  const t0 = cursor * bar;

  for (let b = 0; b < sec.bars; b++) {
    const bt = t0 + b * bar;
    const chordIdx = b % 4;

    if (sec.mode === "intro") {
      // Just vinyl (already added). Add a soft sub roll on bar 4 to
      // signal the drop into hook1.
      if (b === sec.bars - 1) {
        subOnRoot(bt, 0, 0.45, "clean");
        // Pickup open-hat on the last 8th.
        openHat(drm, bt + beat * 3.5, 0.20);
      }
    }

    else if (sec.mode === "latifah") {
      // Boom-bap full pocket.
      // Kick: 1 and 3 (with a syncopated pickup kick on the 4-and "of-3"
      // sometimes — adds the swung walk).
      kick(drm, bt + 0,           1.0);   kickTimes.push(bt + 0);
      kick(drm, bt + beat * 2,    1.0);   kickTimes.push(bt + beat * 2);
      // Subtle ghost-kick on the last 16th of bar — only odd bars
      if (b % 2 === 1) {
        kick(drm, bt + beat * 3 + sx * 3 + SWING * sx, 0.45);
      }
      // Snare on 2 and 4 (with a tiny micro-shift to taste — boom-bap
      // sits the snare just barely behind the grid).
      snare(drm, bt + beat * 1 + 0.005, 0.88);
      snare(drm, bt + beat * 3 + 0.005, 0.88);
      // Closed hat: swung 16ths, skip the snare locations
      for (let e = 0; e < 16; e++) {
        const sw = e % 2 === 1 ? SWING * sx : 0;
        // ghost hat skip on every 7 and 15 (the "of 2" / "of 4" with snare)
        if (e === 4 || e === 12) continue;
        hat(drm, bt + e * sx + sw, e % 2 ? 0.18 : 0.26);
      }
      // Occasional open hat on the "and" of beat 4 — Pete-Rock signature
      if (b % 4 === 3) openHat(drm, bt + beat * 3.5, 0.16);

      // Sub bass on root
      subOnRoot(bt, chordIdx, 0.62);
      // Rhodes stab on 2 & 4
      rhodesStab(bt, chordIdx, 0.36);
      // Soft Rhodes pad underneath for warmth
      rhodesPad(bt, chordIdx, 0.10);
    }

    else if (sec.mode === "timbaland") {
      // Sparse pocket. Same kick/snare placements but the hat layer
      // drops out, replaced by the tumbi accent line. Rhodes goes silent
      // except a single chord stab on bar 2 of every 4-bar loop.
      kick(drm, bt + 0,        1.0);  kickTimes.push(bt + 0);
      kick(drm, bt + beat * 2, 1.0);  kickTimes.push(bt + beat * 2);
      // Snare on 3 only (Timbaland-style sparse) — but keep a rim on 2
      // to mark the boom-bap heritage.
      rim(drm,   bt + beat * 1 + 0.004, 0.55);
      snare(drm, bt + beat * 3 + 0.005, 0.92);
      // No closed hats — the silence is the percussion choice.
      // A single open-hat tick on the "and-of-4" every other bar.
      if (b % 2 === 1) openHat(drm, bt + beat * 3.5, 0.14);

      // Sub bass on root (slightly wider preset for Timbaland punch)
      subOnRoot(bt, chordIdx, 0.66, "wide");
      // Tumbi accent line (the melodic content fills the gaps)
      tumbiBar(bt, 0.28);
      // One Rhodes chord stab on bar 2 of each 4-bar loop only — keeps
      // the harmony anchored but lets the bed breathe.
      if (chordIdx === 1) {
        const ch = PROG[chordIdx];
        for (const m of ch.rhodes) {
          mixEventRhodes(
            { startSec: bt + beat * 2, midi: m, durSec: beat * 0.9, gain: 0.30, preset: "stab" },
            bus, { sampleRate: SR, preset: "stab" },
          );
        }
      }
    }

    else if (sec.mode === "bridge") {
      // Breakdown — drop kick/snare, leave just sub pulse + soft Rhodes
      // pad + the vinyl hiss-forward bed. Talky space for the spoken
      // bridge vocal.
      mixEventSub(
        { startSec: bt, midi: PROG[chordIdx].root, durSec: bar * 0.95, gain: 0.42, preset: "clean" },
        bus, { sampleRate: SR, preset: "clean" },
      );
      rhodesPad(bt, chordIdx, 0.18);
      // A single rim knock on beat 4 every bar to keep the pulse felt
      rim(drm, bt + beat * 3 + 0.005, 0.45);
      // Pickup back into hook3 on the last bar — snare roll on the
      // "and-of-4" leading in.
      if (b === sec.bars - 1) {
        snare(drm, bt + beat * 3 + sx * 2, 0.55);
        snare(drm, bt + beat * 3 + sx * 3, 0.65);
        openHat(drm, bt + beat * 3 + sx * 3.5, 0.20);
      }
    }

    else if (sec.mode === "outro") {
      // Echo of the hook for the first 2 bars at half intensity, then
      // fade to just vinyl + a long sub.
      if (b < 2) {
        kick(drm, bt + 0,        0.7); kickTimes.push(bt + 0);
        kick(drm, bt + beat * 2, 0.7); kickTimes.push(bt + beat * 2);
        snare(drm, bt + beat * 1 + 0.005, 0.55);
        snare(drm, bt + beat * 3 + 0.005, 0.55);
        for (let e = 0; e < 16; e++) {
          if (e === 4 || e === 12) continue;
          const sw = e % 2 === 1 ? SWING * sx : 0;
          hat(drm, bt + e * sx + sw, e % 2 ? 0.10 : 0.16);
        }
        subOnRoot(bt, chordIdx, 0.45);
        rhodesPad(bt, chordIdx, 0.14);
      } else {
        // Final long sub fade on Dm
        mixEventSub(
          { startSec: bt, midi: 38, durSec: bar * 1.8, gain: 0.32, preset: "clean" },
          bus, { sampleRate: SR, preset: "clean" },
        );
        rhodesPad(bt, 0, 0.10);
      }
    }
  }

  cursor += sec.bars;
}

// ── sidechain duck on the melodic bus (under each kick) ────────────────
const duck = new Float32Array(N).fill(1);
for (const kt of kickTimes) {
  const s0 = Math.floor(kt * SR), len = Math.floor(0.13 * SR);
  for (let i = 0; i < len && s0 + i < N; i++) {
    const w = i / len;
    const d = 0.55 + 0.45 * w;   // dip to .55, recover — gentle pump
    if (d < duck[s0 + i]) duck[s0 + i] = d;
  }
}

// ── master ─────────────────────────────────────────────────────────────
const mix = new Float32Array(N);
for (let i = 0; i < N; i++) {
  const s = bus[i] * duck[i] + drm[i] + vin[i];
  mix[i] = Math.tanh(s * 1.02);                // gentle glue
}
let peak = 0;
for (let i = 0; i < N; i++) { const a = Math.abs(mix[i]); if (a > peak) peak = a; }
// 0.82 target leaves headroom; lame & PCM both stay under 0 dBFS.
if (peak > 0) { const n = 0.82 / peak; for (let i = 0; i < N; i++) mix[i] *= n; }

// ── write ──────────────────────────────────────────────────────────────
const wantWav = !!flags.wav;
const ext = wantWav ? "wav" : "mp3";
const outPath = expandHome(flags.out) || resolve(HERE, `../out/visualize-my-booch.${ext}`);
mkdirSync(dirname(outPath), { recursive: true });

const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(N * 4);
for (let i = 0; i < N; i++) b.writeFloatLE(mix[i], i * 4);
writeFileSync(rawPath, b);

console.log(
  `→ booch · ${BPM} BPM (swing ${SWING.toFixed(2)}) · ${totalBars} bars · ` +
  `${totalSec.toFixed(1)}s · intro→hook→V1→hook→[TIMBALAND V2]→bridge→hook→outro`
);

const ffArgs = wantWav
  ? [
      "-hide_banner", "-y", "-loglevel", "error",
      "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
      "-c:a", "pcm_s16le", outPath,
    ]
  : [
      "-hide_banner", "-y", "-loglevel", "error",
      "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
      "-c:a", "libmp3lame", "-q:a", "2", outPath,
    ];

const ff = spawnSync("ffmpeg", ffArgs, { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath}`);
