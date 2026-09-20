#!/usr/bin/env node
// setalign.mjs — make every vocal set speak in the LEAD's rhythm.
//
// A regulated set gets one global tempo fit, so it keeps the natural
// spacing of its own performance: take 7427 sings the hook in 9.0s while
// the lead, retimed onto the grid, takes 14.5s. Stacked, they smear.
// This warps each set piecewise so its syllable onsets land on the
// lead's — same law as lyrictrack's dictation retime, but the
// destination is the first take instead of a grid address.
//
//   node pop/imab/bin/setalign.mjs [--takes a,b] [--no-audition]
//   → out/imab-set-<take>-aligned.wav   one per set, lead-timed
//   → out/imab-sets-successive.mp3      lead then each set, in turn,
//     over a click — no layering, so every take can be judged alone.
//
// The shift table prints how far each syllable had to travel: a take
// that needs a big, lopsided pull is one whose boundaries are wrong,
// not one that merely sings loose.

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const WORK = `${process.env.HOME}/.cache/ac/imab`;
mkdirSync(WORK, { recursive: true });
const argv = process.argv.slice(2);
const flag = (n, d) => { const i = argv.indexOf(`--${n}`); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const sh = (c, a) => spawnSync(c, a, { stdio: ["ignore", "ignore", "inherit"] });
const SR = 48000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;

const dur = (f) => parseFloat(spawnSync("ffprobe",
  ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", f],
  { encoding: "utf8" }).stdout);
const readF32 = (f) => {
  const raw = `${WORK}/.sa.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", f,
    "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + Math.floor(b.length / 4) * 4));
};

// ── the lead's syllable clock ─────────────────────────────────────────
const LEADJ = `${OUT}/imab-sacredvox.lyrics.json`;
if (!existsSync(LEADJ)) { console.error("✗ run lyrictrack.mjs first"); process.exit(1); }
const lead = JSON.parse(readFileSync(LEADJ, "utf8")).syllables;
const LEADWAV = ["imab-aesthetivox-retimed.wav", "imab-aesthetivox.wav", "imab-sacredvox.wav"]
  .map((n) => `${OUT}/${n}`).find(existsSync);
console.log(`lead ${LEADWAV.split("/").pop()} · ${lead.length} syllables · ` +
  `${lead[0].label}@${lead[0].fromMs}ms → ${lead[lead.length - 1].label}@${lead[lead.length - 1].fromMs}ms`);

const sdoc = JSON.parse(readFileSync(`${LANE}/vocal-sets.json`, "utf8"));
const listed = [...new Set([...(sdoc.sets ?? []).map((s) => s.take),
  ...(sdoc.choir?.takes ?? [])])];
const TAKES = flag("takes") ? flag("takes").split(",") : listed;

// ── align one set onto the lead ───────────────────────────────────────
const aligned = [];
for (const take of TAKES) {
  const wav = `${OUT}/imab-set-${take}.wav`;
  const tj = `${OUT}/imab-set-${take}-targets.json`;
  if (!existsSync(wav) || !existsSync(tj)) {
    console.log(`  (${take.slice(0, 6)}… not regulated — skipped)`); continue;
  }
  const set = JSON.parse(readFileSync(tj, "utf8"));
  // pair by label, walking forward: a set's extra word (both alternate
  // takes sing "it's" before "just") simply carries between anchors.
  const pairs = [];
  let si = 0;
  for (const L of lead) {
    let k = si;
    while (k < set.length && set[k].label !== L.label) k++;
    if (k < set.length) { pairs.push({ label: L.label, src: set[k].t * 1000, dst: L.fromMs }); si = k + 1; }
  }
  if (pairs.length < 8) { console.log(`  ✗ ${take.slice(0, 6)}… only ${pairs.length} syllables paired — skipped`); continue; }
  const sDur = dur(wav) * 1000;
  const anchors = [[0, 0]];
  for (const p of pairs) {
    const [ps, pd] = anchors[anchors.length - 1];
    if (p.src > ps + 20 && p.dst > pd + 20) anchors.push([p.src, p.dst]);
  }
  const [ls, ld] = anchors[anchors.length - 1];
  anchors.push([sDur, ld + Math.max(120, sDur - ls)]);
  // render: cut each span, stretch the ones that move, concat
  const parts = [];
  for (let k = 1; k < anchors.length; k++) {
    const [s0, d0] = anchors[k - 1], [s1, d1] = anchors[k];
    const sd = (s1 - s0) / 1000, dd = (d1 - d0) / 1000;
    if (sd < 0.002) continue;
    const seg = `${WORK}/sa-${take}-${k}.wav`;
    sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav,
      "-ss", s0 / 1000, "-t", sd, "-ac", "1", "-ar", String(SR), "-c:a", "pcm_s16le", seg]);
    const f = dd / sd;
    if (Math.abs(f - 1) > 0.02) {
      sh("rubberband", ["-t", f.toFixed(4), "-F", "-c", "6", seg, `${WORK}/sa-${take}-${k}s.wav`]);
      parts.push(`${WORK}/sa-${take}-${k}s.wav`);
    } else parts.push(seg);
  }
  const list = `${WORK}/sa-${take}.txt`;
  writeFileSync(list, parts.map((p) => `file '${p}'`).join("\n"));
  const final = `${OUT}/imab-set-${take}-aligned.wav`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "concat",
    "-safe", "0", "-i", list, "-ac", "1", "-ar", String(SR), final]);
  const shifts = pairs.map((p) => p.dst - p.src);
  const worst = pairs.reduce((a, p) => Math.abs(p.dst - p.src) > Math.abs(a.dst - a.src) ? p : a, pairs[0]);
  const stretch = (pairs[pairs.length - 1].dst - pairs[0].dst) / Math.max(1, pairs[pairs.length - 1].src - pairs[0].src);
  console.log(`✓ ${take.slice(0, 6)}… ${pairs.length}/${lead.length} paired · phrase ×${stretch.toFixed(2)} · ` +
    `shift ${Math.round(Math.min(...shifts))}…${Math.round(Math.max(...shifts))}ms · worst ${worst.label} ${Math.round(worst.dst - worst.src)}ms`);
  aligned.push({ take, wav: final });
}

// ── successive audition: no layering, one take at a time, over a floor ─
// Still one voice at a time — but a dry click tells you nothing about
// whether a take DANCES. The real CC0 kit plays a 124 four-on-the-floor
// under it, and each take gets a whole 16-bar phrase to arrive in
// rather than being butted against the next one.
if (!argv.includes("--no-audition") && aligned.length) {
  const phrase = lead[lead.length - 1].fromMs / 1000 + 2.5;
  const SLOT_BARS = Math.max(16, Math.ceil(phrase / BAR / 8) * 8);   // 8-bar multiples
  const INTRO_BARS = 8, OUTRO_BARS = 8;
  const SLOT = SLOT_BARS * BAR;
  const order = [{ take: "LEAD", wav: LEADWAV }, ...aligned];
  const bars = INTRO_BARS + order.length * SLOT_BARS + OUTRO_BARS;
  const total = bars * BAR + 2;
  const mix = new Float32Array(Math.ceil(total * SR));

  const KIT = `${LANE}/samples/real`;
  const kit = {};
  for (const n of ["kick", "clap", "hat-closed", "hat-open", "shaker", "snare"]) {
    const p = `${KIT}/${n}.wav`;
    if (existsSync(p)) kit[n] = readF32(p);
  }
  const hit = (name, atSec, gain) => {
    const x = kit[name];
    if (!x || gain <= 0) return;
    const at = Math.floor(atSec * SR);
    for (let j = 0; j < x.length && at + j < mix.length; j++) mix[at + j] += x[j] * gain;
  };
  // a rising noise swell — the sound of a build going somewhere
  const swell = (t0s, dur, gain) => {
    const at = Math.floor(t0s * SR), n = Math.floor(dur * SR);
    let lp = 0;
    for (let i = 0; i < n && at + i < mix.length; i++) {
      const k = i / n;
      const white = Math.random() * 2 - 1;
      lp += (white - lp) * (0.02 + 0.5 * k);     // filter opens as it rises
      mix[at + i] += lp * gain * k * k;
    }
  };

  // Entries and builds: every take arrives at a phrase top, and the two
  // bars before it climb — the roll tightens, the swell opens, and the
  // kick drops out of the last bar so the downbeat lands.
  const entryBars = order.map((_, i) => INTRO_BARS + i * SLOT_BARS);
  const buildAt = (b) => (entryBars.includes(b + 1) ? 1 : entryBars.includes(b + 2) ? 0.5 : 0);

  for (let b = 0; b < bars; b++) {
    const t0 = b * BAR;
    const intro = b < INTRO_BARS;
    const lift = b % 8 === 7;                       // phrase turn
    const bl = buildAt(b);
    const busy = (b % SLOT_BARS) >= SLOT_BARS / 2;  // second half of a slot opens up
    const entry = entryBars.includes(b);
    if (entry) {                                    // the drop
      hit("hat-open", t0, 0.75);
      hit("clap", t0, 0.8);
      for (let i = 0; i < Math.floor(0.5 * SR); i++)   // sub boom
        mix[Math.floor(t0 * SR) + i] += Math.sin(2 * Math.PI * 48 * i / SR) *
          Math.exp(-i / (0.16 * SR)) * 0.5;
    }
    for (let beat = 0; beat < 4; beat++) {
      const t = t0 + beat * BEAT;
      // kick: absent from the first 4 intro bars and from the final build bar
      if (!(intro && b < 4) && bl < 1) hit("kick", t, 0.95);
      if ((beat === 1 || beat === 3) && !intro && bl === 0) hit("clap", t, 0.55);
      if (bl === 0) {
        hit("hat-closed", t + BEAT / 2, lift && beat === 3 ? 0 : 0.34);
        if (busy) hit("hat-closed", t + BEAT / 4, 0.16);   // 16ths in the busy half
      }
      if (lift && beat === 3 && bl === 0) hit("hat-open", t + BEAT / 2, 0.42);
      for (let s = 0; s < 4; s++) hit("shaker", t + (s * BEAT) / 4, s % 2 ? 0.10 : 0.16);
    }
    if (bl > 0) {                                   // the roll, accelerating
      const div = bl === 1 ? 16 : 8;                // 16ths, then 32nds
      for (let i = 0; i < div; i++) {
        const k = i / div;
        hit("snare", t0 + (i * BAR) / div, 0.16 + 0.42 * k * bl);
      }
      if (bl === 1)
        for (let i = 0; i < 8; i++)                 // final beat doubles up
          hit("snare", t0 + BAR * 0.75 + (i * BEAT) / 8, 0.3 + 0.4 * (i / 8));
      swell(t0, BAR, 0.10 + 0.14 * bl);
    }
  }

  // ── chords ──────────────────────────────────────────────────────────
  // An 8-bar loop the hook already implies: C under the chant, F where
  // "a" sings F4, G where "just" sings G4, Am for the turn. Offbeat
  // stabs carry the dance, a pad holds the bar underneath, and both ride
  // the pump so the kick keeps the floor.
  const HZ = (m) => 440 * Math.pow(2, (m - 69) / 12);
  const PROG = [
    { name: "C",  bass: 36, notes: [48, 52, 55] },
    { name: "C",  bass: 36, notes: [48, 52, 55] },
    { name: "F",  bass: 41, notes: [48, 53, 57] },
    { name: "F",  bass: 41, notes: [48, 53, 57] },
    { name: "Am", bass: 45, notes: [48, 52, 57] },
    { name: "Am", bass: 45, notes: [48, 52, 57] },
    { name: "G",  bass: 43, notes: [50, 55, 59] },
    { name: "G",  bass: 43, notes: [50, 55, 59] },
  ];
  // sidechain: everything harmonic breathes under the four-on-the-floor
  const pump = (t) => 1 - 0.55 * Math.exp(-(((t / BEAT) % 1) * BEAT) / 0.16);
  const tone = (midi, t, dur, gain, harm, tau, attack) => {
    const f = HZ(midi), at = Math.floor(t * SR), n = Math.floor(dur * SR);
    for (let i = 0; i < n && at + i < mix.length; i++) {
      const s = i / SR;
      const e = Math.exp(-s / tau) * (1 - Math.exp(-s / attack));
      let v = 0;
      for (let h = 1; h <= harm; h++) v += Math.sin(2 * Math.PI * f * h * (i / SR)) / h;
      mix[at + i] += (v / harm) * e * gain * pump(t + s);
    }
  };
  for (let b = 0; b < bars; b++) {
    const c = PROG[b % PROG.length], t0 = b * BAR;
    const intro = b < 4;
    for (const m of c.notes) tone(m, t0, BAR, intro ? 0.09 : 0.13, 5, 1.3, 0.25);  // pad
    const bl2 = buildAt(b);
    if (!intro && bl2 === 0) {
      for (let beat = 0; beat < 4; beat++)                                          // offbeat stabs
        for (const m of c.notes) tone(m + 12, t0 + beat * BEAT + BEAT / 2, 0.30, 0.085, 7, 0.10, 0.003);
      // BASS with somewhere to go: root on the beat, the octave answering
      // offbeat, and the fifth pushing into the back half of the bar.
      const busy2 = (b % SLOT_BARS) >= SLOT_BARS / 2;
      for (let beat = 0; beat < 4; beat++) {
        const t = t0 + beat * BEAT;
        tone(c.bass, t, BEAT * 0.52, 0.40, 2, 0.16, 0.004);          // sub root
        tone(c.bass + 12, t + BEAT / 2, BEAT * 0.36, 0.20, 3, 0.09, 0.003);  // octave push
        if (busy2 && (beat === 1 || beat === 3))
          tone(c.bass + 7, t + BEAT * 0.75, BEAT * 0.22, 0.16, 3, 0.07, 0.003); // fifth
      }
    } else if (!intro && bl2 === 1) {
      for (const m of c.notes) tone(m + 12, t0, BAR, 0.10, 6, 1.6, 0.5);  // held under the build
    }
  }

  console.log("\nsuccessive audition (124 · four-on-the-floor · C C F F Am Am G G · " +
    `${SLOT_BARS}-bar slots · ${bars} bars ≈ ${(bars * BAR / 60).toFixed(1)}min):`);
  // ── the vocal bus: pushed, not merely placed ────────────────────────
  // Level alone reads as loud, not as POP. The force comes from holding
  // the whole phrase up (compression), opening the consonants (presence),
  // driving it (saturation), and hanging it in a room big enough to sing
  // back (plate). Reverb is a SEND, so the dry stays in front.
  const hpf = (x, fc) => {                                   // one-pole, clears the bass
    const a = Math.exp(-2 * Math.PI * fc / SR);
    const y = new Float32Array(x.length);
    let px = 0, py = 0;
    for (let i = 0; i < x.length; i++) { y[i] = a * (py + x[i] - px); px = x[i]; py = y[i]; }
    return y;
  };
  const presence = (x, f0, gainDb, q) => {                   // peaking EQ
    const A = Math.pow(10, gainDb / 40), w = 2 * Math.PI * f0 / SR;
    const al = Math.sin(w) / (2 * q), c = Math.cos(w);
    const b0 = 1 + al * A, b1 = -2 * c, b2 = 1 - al * A;
    const a0 = 1 + al / A, a1 = -2 * c, a2 = 1 - al / A;
    const y = new Float32Array(x.length);
    let x1 = 0, x2 = 0, y1 = 0, y2 = 0;
    for (let i = 0; i < x.length; i++) {
      const v = (b0 / a0) * x[i] + (b1 / a0) * x1 + (b2 / a0) * x2 - (a1 / a0) * y1 - (a2 / a0) * y2;
      x2 = x1; x1 = x[i]; y2 = y1; y1 = v; y[i] = v;
    }
    return y;
  };
  const compress = (x, thr, ratio, atkMs, relMs) => {
    const aA = Math.exp(-1 / (atkMs / 1000 * SR)), aR = Math.exp(-1 / (relMs / 1000 * SR));
    const y = new Float32Array(x.length);
    let env = 0;
    for (let i = 0; i < x.length; i++) {
      const v = Math.abs(x[i]);
      env = v > env ? aA * env + (1 - aA) * v : aR * env + (1 - aR) * v;
      let g = 1;
      if (env > thr) g = (thr + (env - thr) / ratio) / env;
      y[i] = x[i] * g;
    }
    return y;
  };
  const saturate = (x, drive) => {
    const y = new Float32Array(x.length);
    for (let i = 0; i < x.length; i++) y[i] = Math.tanh(x[i] * drive) / Math.tanh(drive);
    return y;
  };
  // Freeverb-shaped plate: parallel combs into series allpasses
  const plate = (x, decay, tailSec) => {
    const out = new Float32Array(x.length + Math.floor(tailSec * SR));
    const combs = [1557, 1617, 1491, 1422, 1277, 1356].map((d) => Math.round(d * SR / 44100));
    const g = 0.82 * decay;
    for (const d of combs) {
      const buf = new Float32Array(d);
      let idx = 0, lp = 0;
      for (let i = 0; i < out.length; i++) {
        const inp = i < x.length ? x[i] : 0;
        const y = buf[idx];
        lp += (y - lp) * 0.62;                    // damping, so it is not brittle
        buf[idx] = inp + lp * g;
        out[i] += y / combs.length;
        idx = (idx + 1) % d;
      }
    }
    for (const d of [225, 556, 441].map((v) => Math.round(v * SR / 44100))) {
      const buf = new Float32Array(d);
      let idx = 0;
      const ag = 0.7;
      for (let i = 0; i < out.length; i++) {
        const inp = out[i], y = buf[idx];
        out[i] = -ag * inp + y;
        buf[idx] = inp + ag * y;
        idx = (idx + 1) % d;
      }
    }
    return out;
  };

  order.forEach((o, i) => {
    let x = readF32(o.wav);
    let peak = 0;
    for (let j = 0; j < x.length; j++) peak = Math.max(peak, Math.abs(x[j]));
    if (peak > 0) { const n = 0.7 / peak; for (let j = 0; j < x.length; j++) x[j] *= n; }
    x = hpf(x, 95);
    x = presence(x, 3200, 6.5, 0.9);              // consonants forward
    x = presence(x, 180, -3.0, 0.8);              // clear the mud the plate would smear
    x = compress(x, 0.16, 5.0, 4, 90);            // hold the phrase up
    x = saturate(x, 2.4);                         // force
    for (let j = 0; j < x.length; j++) x[j] *= 1.55;
    const wet = plate(x, 0.92, 2.6);
    const bar = INTRO_BARS + i * SLOT_BARS;
    const at = Math.floor(bar * BAR * SR);
    // pre-delay keeps the word intelligible before the room answers
    const pre = Math.floor(0.028 * SR);
    for (let j = 0; j < x.length && at + j < mix.length; j++) mix[at + j] += x[j] * 0.92;
    for (let j = 0; j < wet.length && at + pre + j < mix.length; j++)
      mix[at + pre + j] += wet[j] * 0.42;
    console.log(`  ${(bar * BAR).toFixed(1).padStart(6)}s  bar ${String(bar + 1).padStart(3)}  ${o.take}`);
  });
  const raw = `${WORK}/successive.f32`;
  writeFileSync(raw, Buffer.from(mix.buffer));
  const mp3 = `${OUT}/imab-sets-successive.mp3`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR),
    "-ac", "1", "-i", raw, "-af", "aresample=192000,alimiter=limit=0.74,aresample=48000",
    "-b:a", "192k", mp3]);
  console.log(`✓ ${mp3}`);
}
