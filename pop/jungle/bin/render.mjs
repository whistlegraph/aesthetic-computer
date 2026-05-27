#!/usr/bin/env node
// jungle/bin/render.mjs — render a sunlit jungle .np score to mp3.
//
// Bottom-up + HYBRID break (per @jeffrey, 2026-05-16): the groove is the
// AC percussion kit (percussion.mjs — same kick/snare/hat/ride web +
// native notepat play) chopped into Amen-style syncopation, plus a
// synthesized bitcrush/flange snare-roll "break-stab" at phrase ends as
// the genre nod. No sampled Amen, no loop.
//
// Layers (rendered to mono stems, then placed in stereo):
//   1. break      — hybrid AC-kit breakbeat, chopped + shuffled
//   2. break-stab  — synth snare-roll burst (bitcrush+flange) at phrase ends
//   3. sub         — deep dub/reggae sub, ducked under the kick
//   4. pad         — warm golden chord pad (sinepower pad)
//   5. skank       — reggae offbeat chord stab (sinepower stab)
//   6. siren       — sparse dub-siren / airhorn (AC pitch-swept synth)
//
// Section flags in the .np score gate which layers fire, e.g.
//   "# drop 1 16 [break, break-stab, sub, skank, pad, siren]"
//   "# dub-break 8 [sub, siren, no-break]"
//
// Usage:
//   node bin/render.mjs --slug raggasol
//   node bin/render.mjs --slug jungleton --style jungleton --bpm 168
//   node bin/render.mjs --slug rodando --layer break      (solo a layer)
//   node bin/render.mjs all                                (all variants)
//
// Output: pop/jungle/out/<slug>.mp3

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdirSync, unlinkSync, copyFileSync, existsSync, renameSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { makeBufferSynth } from "../../dance/synths/bus.mjs";
import { mixEventSinePower } from "../../dance/synths/sinepower.mjs";
import { applyBitcrush, applyFlange, applyWobble, softClip } from "../../dance/synths/fx.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const { playPercussion } = await import(
  `${REPO}/system/public/aesthetic.computer/lib/percussion.mjs`
);

const SAMPLE_RATE = 48_000;

// ── per-variant defaults ──────────────────────────────────────────────
// style picks the break/sub character; root is the MIDI tonic the .np
// "_" notes are interpreted against (scores are written root-relative).
const DEFAULTS = {
  jungleton: { style: "jungleton", bpm: 166, transpose: 0,
               title: "jungletón · fía — dembow latina al sol" },
  raggasol:  { style: "ragga",     bpm: 165, transpose: 0,
               title: "raggasol · fía — jungle latina dorada" },
  rodando:   { style: "rollers",   bpm: 170, transpose: 0,
               title: "rodando · fía — rollers latina líquida" },
  solfia:    { style: "ragga",     bpm: 165, transpose: 0,
               wordmark: "solfía",
               title: "solfía · fía — spicy latina jungle" },
  solafiya:  { style: "ragga",     bpm: 176, transpose: 0,
               wordmark: "solafiya", artist: "fía", label: "Aesthetic.Computer",
               title: "solafiya", vibe: "hardcore-cute",
               // THREE verses, played SEQUENTIALLY (no layering/overlap) —
               // the arrangement was extended so they fit naturally.
               vocal:  "/Users/jas/Documents/Working Desktop/gens/Circuito Coto Amate 2.m4a",
               vocal2: "/Users/jas/Documents/Working Desktop/gens/Circuito Coto Amate 3.m4a",
               vocal3: "/Users/jas/Documents/Working Desktop/gens/Circuito Coto Amate 4.m4a",
               vocalBar: 0 },
};

// ── args ──────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
    else { flags[a.slice(2)] = next; i++; }
  } else positional.push(a);
}

// ── deterministic PRNG (humanizes ghosts / hats / stab) ───────────────
function makeRng(seedStr) {
  let s = 2166136261 >>> 0;
  for (let i = 0; i < seedStr.length; i++) {
    s ^= seedStr.charCodeAt(i);
    s = Math.imul(s, 16777619);
  }
  s = s >>> 0 || 1;
  return () => {
    s ^= s << 13; s >>>= 0;
    s ^= s >>> 17; s >>>= 0;
    s ^= s << 5;  s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}

// ── note utils ────────────────────────────────────────────────────────
const NOTE_BASE = { C:0,"C#":1,DB:1,D:2,"D#":3,EB:3,E:4,F:5,
                    "F#":6,GB:6,G:7,"G#":8,AB:8,A:9,"A#":10,BB:10,B:11 };
function noteToMidi(s) {
  s = s.toUpperCase();
  const oct = parseInt(s.slice(-1), 10);
  return 12 * (oct + 1) + NOTE_BASE[s.slice(0, -1)];
}
function midiToFreq(m) { return 440 * Math.pow(2, (m - 69) / 12); }

// ── stereo reverb (Freeverb-lite): mono in → spacious stereo tail.
// Used as a send for depth on drums / skank / siren / dings — the sub
// and gun stay dry so the low end punches through the room.
function stereoReverb(input, sr, room = 0.80, damp = 0.28) {
  const N = input.length;
  const L = new Float32Array(N), R = new Float32Array(N);
  const sc = sr / 44100;
  const mk = (d) => ({ b: new Float32Array(Math.max(1, Math.round(d * sc))), i: 0, lp: 0 });
  const combTune = [1116, 1188, 1277, 1356, 1422, 1491];
  const apTune = [556, 441, 341, 225];
  const spread = 23;
  const cL = combTune.map(mk), cR = combTune.map((d) => mk(d + spread));
  const aL = apTune.map(mk),   aR = apTune.map((d) => mk(d + spread));
  const apf = 0.5;
  const run = (x, combs, aps) => {
    let acc = 0;
    for (const c of combs) {
      const y = c.b[c.i];
      c.lp = y * (1 - damp) + c.lp * damp;
      c.b[c.i] = x + c.lp * room;
      c.i = c.i + 1 >= c.b.length ? 0 : c.i + 1;
      acc += y;
    }
    let s = acc * 0.20;
    for (const a of aps) {
      const bv = a.b[a.i];
      const y = -s + bv;
      a.b[a.i] = s + bv * apf;
      a.i = a.i + 1 >= a.b.length ? 0 : a.i + 1;
      s = y;
    }
    return s;
  };
  for (let n = 0; n < N; n++) {
    const x = input[n] * 0.30;
    L[n] = run(x, cL, aL);
    R[n] = run(x, cR, aR);
  }
  return { L, R };
}
// tighten: cheap downward expander. holds full gain on transients,
// then pulls fast toward `floor` as the signal falls — chokes the
// ringing percussion tails between hits so the break stays punchy and
// doesn't smear at jungle tempo.
function tighten(buf, sr, relMs = 55, floor = 0.3) {
  const aA = Math.exp(-1 / (sr * 0.002));            // 2 ms attack
  const aR = Math.exp(-1 / (sr * (relMs / 1000)));   // fast release
  const thr = 0.05;
  let env = 0;
  for (let i = 0; i < buf.length; i++) {
    const x = Math.abs(buf[i]);
    env = x > env ? aA * env + (1 - aA) * x : aR * env + (1 - aR) * x;
    const g = env >= thr ? 1 : floor + (1 - floor) * (env / thr);
    buf[i] *= g;
  }
}
// First-order shelves for the master deepen/warm pass.
function lowShelfBoost(buf, sr, fc, amount) {   // adds amount × low band
  const a = (1 / sr) / (1 / (2 * Math.PI * fc) + 1 / sr);
  let lp = 0;
  for (let i = 0; i < buf.length; i++) { lp += a * (buf[i] - lp); buf[i] += lp * amount; }
}
function highCut(buf, sr, fc, amount) {          // blends toward a lowpassed copy
  const a = (1 / sr) / (1 / (2 * Math.PI * fc) + 1 / sr);
  let lp = 0;
  for (let i = 0; i < buf.length; i++) { lp += a * (buf[i] - lp); buf[i] = buf[i] * (1 - amount) + lp * amount; }
}

// ── parse .np score with section flags ────────────────────────────────
// Each "_" note is a chord root over its duration. Section markers gate
// layers. Mirrors the chillwave / dance parser.
function parseScore(path, bpm, transpose) {
  const lines = readFileSync(path, "utf8").split("\n");
  const beat_s = 60.0 / bpm;
  let beat_pos = 0;
  const events = [];
  const sections = [];
  let curSection = null;
  const HEADER_RE = /^([a-z][a-z0-9-]*(?:\s+\d+)?)\s+(\d+)(?:\s+\[([^\]]+)\])?\s*$/i;

  for (const raw of lines) {
    const t = raw.trim();
    if (!t) continue;
    if (t.startsWith("#")) {
      const body = t.replace(/^#\s*/, "");
      const m = body.match(HEADER_RE);
      if (m) {
        if (curSection) {
          curSection.endSec = beat_pos * beat_s;
          sections.push(curSection);
        }
        curSection = {
          name: m[1].trim(),
          bars: Number(m[2]),
          startSec: beat_pos * beat_s,
          endSec: null,
          flags: new Set(
            (m[3] || "").split(",").map((s) => s.trim()).filter(Boolean),
          ),
        };
      }
      continue;
    }
    for (const tok of t.split(/\s+/)) {
      const m = tok.match(/^([A-Ga-g][#b]?\d):(.+?)(?:\*(\d+(?:\.\d+)?))?$/);
      if (!m) continue;
      const weight = Number(m[3] ?? 1);
      events.push({
        startSec: beat_pos * beat_s,
        midi: noteToMidi(m[1]) + transpose,
        durSec: weight * beat_s,
        section: curSection?.name ?? null,
      });
      beat_pos += weight;
    }
  }
  if (curSection) {
    curSection.endSec = beat_pos * beat_s;
    sections.push(curSection);
  }
  return { events, sections, totalSec: beat_pos * beat_s };
}

// ── 1-bar break patterns (16 sixteenths) per style ────────────────────
// K kick · S snare(hard backbeat) · s ghost snare · H closed hat ·
// O open hat · R ride. Bar B is a deterministic variation so the loop
// breathes over 2 bars (the Amen never repeats cleanly).
const BREAK = {
  // classic chopped Amen-leaning jungle
  ragga: {
    K: [1,0,0,0, 0,0,1,0, 0,0,1,0, 0,0,0,0],
    S: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    s: [0,0,0,1, 0,0,0,1, 0,1,0,0, 0,0,1,0],
    H: [0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0],
    O: [0,0,0,0, 0,0,0,1, 0,0,0,0, 0,0,0,0],
    R: [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
  },
  // jungletón — dembow tresillo woven into kick + snare skeleton
  jungleton: {
    K: [1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,1,0],
    S: [0,0,0,1, 0,0,1,0, 0,0,0,1, 0,0,1,0],
    s: [0,0,0,0, 0,1,0,0, 0,1,0,0, 0,0,0,1],
    H: [0,0,1,0, 0,0,1,0, 0,0,1,0, 0,0,1,0],
    O: [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1],
    R: [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0],
  },
  // liquid / rollers — even, continuous, smooth (less chopped)
  rollers: {
    K: [1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0],
    S: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0],
    s: [0,0,1,0, 0,1,0,0, 0,0,1,0, 0,1,0,0],
    H: [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0],
    O: [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1],
    R: [1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,0,0],
  },
};

// Bar-B variation: nudge a kick, add a ghost — keeps the 2-bar cell alive.
function barB(pat) {
  const c = JSON.parse(JSON.stringify(pat));
  c.K[6] = 0; c.K[10] = 1;          // displace a kick
  c.s[12] = c.s[12] ? 0 : 1;        // flip a ghost
  c.s[7]  = 1;
  return c;
}

// ── main render ───────────────────────────────────────────────────────
function render(slug) {
  const def = DEFAULTS[slug] || DEFAULTS.raggasol;
  const STYLE = String(flags.style || def.style);
  const BPM = Number(flags.bpm ?? def.bpm);
  const TRANSPOSE = Number(flags.transpose ?? def.transpose);
  // "hardcore cute": harder breaks + a womping dance-kick + growlier
  // bass, with a brighter, drier (less-airy) candy top. persistable.
  const HC = (flags.vibe ?? def.vibe) === "hardcore-cute";
  const SCORE_PATH = `${LANE}/${slug}.np`;
  const OUT_PATH = flags.out
    ? resolve(process.cwd(), String(flags.out))
    : `${LANE}/out/${slug}.mp3`;
  const SOLO = flags.layer ? String(flags.layer).toLowerCase() : null;
  const rng = makeRng(`jungle:${slug}:${STYLE}`);

  const score = parseScore(SCORE_PATH, BPM, TRANSPOSE);
  const beatSec = 60 / BPM;
  const barSec = beatSec * 4;            // 4/4
  const stepSec = barSec / 16;            // 16th-note grid
  const TAIL = 2.0;
  const totalSec = score.totalSec + TAIL;
  const LEN = Math.ceil(totalSec * SAMPLE_RATE);
  const nBars = Math.round(score.totalSec / barSec);

  // ── struct.json sidecar — section map for the multi-section
  // preview-score video (progress bar + per-section illustrations).
  if (!SOLO) {
    const structPath = `${LANE}/out/${slug}.struct.json`;
    writeFileSync(structPath, JSON.stringify({
      slug, style: STYLE, bpm: BPM, totalSec: score.totalSec,
      sections: score.sections.map((s) => ({
        name: s.name, startSec: s.startSec, endSec: s.endSec,
        flags: [...s.flags],
      })),
    }, null, 2));
  }

  // mono stems
  const stem = {
    break:  new Float32Array(LEN),
    stab:   new Float32Array(LEN),
    sub:    new Float32Array(LEN),
    pad:    new Float32Array(LEN),
    skank:  new Float32Array(LEN),
    siren:  new Float32Array(LEN),
    gun:    new Float32Array(LEN),
    ding:   new Float32Array(LEN),
    bell:   new Float32Array(LEN),
    meow:   new Float32Array(LEN),
    marimba: new Float32Array(LEN),
    sineloop: new Float32Array(LEN),
    shrill:  new Float32Array(LEN),
    throat:  new Float32Array(LEN),
    vroom:   new Float32Array(LEN),   // car engine vroom-vrooms (her E430)
    vocalAd: new Float32Array(LEN),   // up-front re-pitched vocal ad-libs + scratches
    impact:  new Float32Array(LEN),   // drop boom / riser drama
    vocalDuet: new Float32Array(LEN), // Voice B — duet partner (verse 3)
    vocal:   new Float32Array(LEN),
    vocalH:  new Float32Array(LEN),   // her stacked harmonies (own bus/space)
    vshadow: new Float32Array(LEN),
  };
  // section-name → value, rendered to a per-sample envelope with short
  // cosine ramps at the boundaries (no clicks). drives section-aware
  // harmony emphasis + reverb on the vocal.
  function sectionEnv(map, dflt = 1) {
    const e = new Float32Array(LEN).fill(dflt);
    const rampS = Math.floor(0.10 * SAMPLE_RATE);
    for (const s of score.sections) {
      const v = map[s.name] ?? dflt;
      const i0 = Math.max(0, Math.floor(s.startSec * SAMPLE_RATE));
      const i1 = Math.min(LEN, Math.floor(s.endSec * SAMPLE_RATE));
      for (let i = i0; i < i1; i++) {
        let k = 1;
        if (i - i0 < rampS) k = 0.5 - 0.5 * Math.cos(Math.PI * (i - i0) / rampS);
        else if (i1 - i < rampS) k = 0.5 - 0.5 * Math.cos(Math.PI * (i1 - i) / rampS);
        e[i] = dflt + (v - dflt) * k;
      }
    }
    return e;
  }
  const reeseBuf  = new Float32Array(LEN);  // detuned-saw bass growl, filtered below
  const shrillBuf = new Float32Array(LEN);  // skrillex screech, post-fx'd
  const throatBuf = new Float32Array(LEN);  // formant/throat bass, post-fx'd
  const noiseRng = makeRng(`jungle:${slug}:noise`);

  // ── humanize: swing the offbeat 16ths, jitter timing + velocity so
  // the hands sound played, not gridded. seeded → reproducible.
  const hrng = makeRng(`jungle:${slug}:human`);
  const SWING = HC ? 0.20 : STYLE === "rollers" ? 0.06 : 0.12;   // HC: laid-back hip-hop headnod
  function humT(t, step) {
    let o = (Math.floor(step) % 2 === 1) ? stepSec * SWING : 0;  // swing
    o += (hrng() * 2 - 1) * 0.007;                                // ±7 ms hand jitter
    return Math.max(0, t + o);
  }
  function humV(v) { return v * (0.82 + hrng() * 0.36); }         // ±velocity

  // section / root lookup by time
  const STYLE_DEFAULT = new Set(["break", "sub", "pad", "skank"]);
  function sectionAt(t) {
    for (const s of score.sections) if (t >= s.startSec && t < s.endSec) return s;
    return score.sections[score.sections.length - 1] || null;
  }
  function flagsAt(t) {
    const s = sectionAt(t);
    return (s && s.flags.size) ? s.flags : STYLE_DEFAULT;
  }
  function on(fl, name) {
    if (fl.has(`no-${name}`)) return false;
    return fl.has(name) || (fl === STYLE_DEFAULT && STYLE_DEFAULT.has(name));
  }
  function rootAt(t) {
    let r = null;
    for (const e of score.events) {
      if (t >= e.startSec) r = e.midi; else break;
    }
    return r ?? 57; // A3 fallback
  }

  // ── drum hit → break stem ───────────────────────────────────────────
  function hit(letter, t, vol) {
    const sound = makeBufferSynth(stem.break, t, SAMPLE_RATE, noiseRng);
    playPercussion(sound, letter, { volume: vol, phase: "both" });
  }
  // sharp "tingy" metallic top layered onto the snare — bright noise +
  // high triangles, very short, so the snare cuts and rings tiny+sharp.
  function snareTing(t, v) {
    const ss = makeBufferSynth(stem.break, t, SAMPLE_RATE, noiseRng);
    ss.synth({ type: "noise",    tone: 7200, duration: 0.030, volume: 0.34 * v, attack: 0.0003, decay: 0.029 });
    ss.synth({ type: "triangle", tone: 5400, duration: 0.018, volume: 0.18 * v, attack: 0.0004, decay: 0.017 });
    ss.synth({ type: "triangle", tone: 3400, duration: 0.028, volume: 0.11 * v, attack: 0.0006, decay: 0.027 });
  }
  // hardcore-cute WOMP-KICK — a fat pitched-down sine thump into the
  // bass path: a clicky 110→ glide, a deep 48 Hz body, a 60 Hz mid, so
  // it lands "WOMP" not "tick". straight-timed → a driving dance pulse.
  function kickWomp(t, v) {
    const k = makeBufferSynth(stem.sub, t, SAMPLE_RATE, noiseRng);
    k.synth({ type: "sine", tone: 118, duration: 0.026, volume: 0.85 * v, attack: 0.0004, decay: 0.025 }); // beater click
    k.synth({ type: "sine", tone: 70,  duration: 0.060, volume: 1.05 * v, attack: 0.001,  decay: 0.058 }); // punch
    k.synth({ type: "sine", tone: 47,  duration: 0.150, volume: 1.20 * v, attack: 0.002,  decay: 0.140 }); // sub womp body
  }

  const kickTimes = [];
  const basePat = BREAK[STYLE] || BREAK.ragga;
  const patB = barB(basePat);

  for (let bar = 0; bar < nBars; bar++) {
    const barStart = bar * barSec;
    const fl = flagsAt(barStart + stepSec);
    const thin     = fl.has("break-thin");
    const breakOn  = (on(fl, "break") || thin) && !fl.has("no-break");
    const pat = (bar % 2 === 1) ? patB : basePat;
    const phraseEnd = (bar % 16) === 15;   // once per 16-bar drop (sparse)

    if (breakOn) {
      const danger = !thin;                                  // full break = aggressive
      for (let st = 0; st < 16; st++) {
        const t = humT(barStart + st * stepSec, st);
        if (pat.K[st]) {
          hit("c", t, humV(st === 0 ? 1.14 : 1.0));
          kickTimes.push(t);
        }
        if (pat.S[st]) { hit("d", t, humV(thin ? 0.72 : 1.06)); if (!thin) snareTing(t, 0.95); }
        // HC = simpler boom-bap / hip-hop pocket (LESS rock): no busy
        // ghost-snare chatter, hats only on the quarter, no ride wash.
        if (!HC && danger && pat.s[st] && rng() < 0.38) hit("d", t, humV(0.18 + rng() * 0.10)); // ghost (sparse)
        if (pat.H[st] && (!HC || st % 4 === 0)) {
          const v = ((st % 4 === 0) ? (HC ? 0.30 : 0.24) : 0.15) + rng() * 0.09;
          hit("g", t, humV(thin ? v * 0.55 : v));
        }
        if (!HC && danger && pat.O[st]) hit("a", t, humV(0.40));
        if (danger && pat.R[st] && (!HC || st === 8)) hit("b", t, humV(0.28 + rng() * 0.08));
      }
      // hardcore-cute: a STRAIGHT four-on-floor womp-kick under the
      // chopped break → dancy drive + the womping bass kicks. full break
      // only (not the thin sections), so drops slam and intros breathe.
      if (HC && danger) {
        for (const st of [0, 4, 8, 12]) {
          kickWomp(barStart + st * stepSec, st === 8 ? 1.08 : st === 0 ? 1.0 : 0.86);
        }
      }
      // mid-phrase snare flam every 8th bar — a threat before the fill
      if (danger && (bar % 8) === 7) {
        const ft = barStart + stepSec * 15;
        hit("d", ft, humV(0.55));
        hit("d", ft + 0.021, humV(0.85));
      }
      // phrase-end snare roll → printed into the stab stem so the
      // bitcrush/flange break-stab only chews the roll, not the groove.
      if (phraseEnd && on(fl, "break-stab") && !SOLO) {
        const rollStart = barStart + stepSec * 12;
        let p = rollStart;
        let gap = stepSec / 2;
        while (p < barStart + barSec - 0.005) {
          const s2 = makeBufferSynth(stem.stab, p, SAMPLE_RATE, noiseRng);
          playPercussion(s2, "d", { volume: 0.95, phase: "both" });
          gap *= 0.84;                                  // accelerate harder
          p += Math.max(stepSec / 7, gap);
        }
        const r0 = rollStart, r1 = barStart + barSec + 0.14;
        applyBitcrush(stem.stab, {
          sampleRate: SAMPLE_RATE, startSec: r0, endSec: r1,
          bits: [{ time: 0, bits: 10 }, { time: r1 - r0, bits: 6 }],
          downsample: [{ time: 0, downsample: 1 }, { time: r1 - r0, downsample: 3 }],
          mix: 0.5,
        });
        applyFlange(stem.stab, {
          sampleRate: SAMPLE_RATE, startSec: r0, endSec: r1,
          rate: 0.6, depthMs: 4, baseDelayMs: 5, feedback: 0.38, mix: 0.4,
        });
      }
    }
  }
  // tighten: downward-expand the break + roll so percussion tails get
  // choked between hits — punchy, less overlap/ring — then light grit.
  tighten(stem.break, SAMPLE_RATE, 34, 0.16);   // super tight, up-close, snappy
  tighten(stem.stab,  SAMPLE_RATE, 70, 0.34);
  softClip(stem.break, HC ? 1.32 : 1.08);       // hardcore: harder-driven break
  if (HC) {                                     // a little crunchy edge/grit
    applyBitcrush(stem.break, { sampleRate: SAMPLE_RATE, bits: 9, downsample: 2, mix: 0.22 });
  }

  // ── bass: REAL layered jungle bass — deep sine floor (the low vibes,
  //   felt) + fundamental body + 2nd-harmonic translation + a filtered
  //   detuned-saw Reese growl, PLAYED as a per-style dub bassline (not a
  //   chord drone), ducked under the kick. ──────────────────────────────
  // pattern step = 16ths; deg is semitones off the bass root (0 root,
  // 7 fifth, 12 octave); len in 16th-steps.
  const BASS = {
    // punchy "BUM ba bum POW" jungle bounce — short notes, on/off-beat
    // intensity (acc), the POW landing on the half-time backbeat.
    ragga: [
      { step: 0,  deg: 0,  len: 2, acc: 1.00 },  // BUM
      { step: 3,  deg: 0,  len: 1, acc: 0.50 },  // ba
      { step: 6,  deg: 0,  len: 2, acc: 0.78 },  // bum
      { step: 8,  deg: 0,  len: 2, acc: 1.00 },  // POW (backbeat)
      { step: 11, deg: 7,  len: 1, acc: 0.55 },  // ba
      { step: 14, deg: 12, len: 2, acc: 0.88 },  // bum (octave lift)
    ],
    // jungletón — dembow tresillo, punchy, octave jumps
    jungleton: [
      { step: 0,  deg: 0,  len: 3 },
      { step: 3,  deg: 0,  len: 3 },
      { step: 6,  deg: 12, len: 2 },
      { step: 8,  deg: 0,  len: 3 },
      { step: 11, deg: 0,  len: 3 },
      { step: 14, deg: 12, len: 2 },
    ],
    // rollers — rolling, continuous, smooth moving line
    rollers: [
      { step: 0,  deg: 0,  len: 6 },
      { step: 6,  deg: 12, len: 2 },
      { step: 8,  deg: 7,  len: 4 },
      { step: 12, deg: 0,  len: 4 },
    ],
  };
  function bassNote(t, midi, durSec, accent) {
    const f = midiToFreq(midi);
    const g = accent;
    const s = makeBufferSynth(stem.sub, t, SAMPLE_RATE, noiseRng);
    // PUNCHY dub — short tight envelopes + an attack snap so it hits
    // "bum" not "bwooom". still deep, just not droning.
    s.synth({ type: "sine", tone: f * 1.0, duration: 0.020,  volume: 0.55 * g, attack: 0.0004, decay: 0.019 }); // click snap
    s.synth({ type: "sine", tone: f / 4, duration: durSec, volume: 0.52 * g, attack: 0.006, decay: durSec * 0.55 });
    s.synth({ type: "sine", tone: f / 2, duration: durSec, volume: 1.28 * g, attack: 0.005, decay: durSec * 0.58 });
    // HARMONIC bass — each partial split into a pair with a subtle Hz
    // offset + a different start phase, so they beat slowly against each
    // other (the dub "alive" shimmer that moves through the held note).
    const partials = [
      { r: 1.0, a: 1.00, dHz: 0.30 },   // fundamental
      { r: 1.5, a: 0.34, dHz: 0.45 },   // fifth
      { r: 2.0, a: 0.18, dHz: 0.60 },   // octave
      { r: 3.0, a: 0.08, dHz: 0.80 },   // twelfth
    ];
    for (const p of partials) {
      const base = f * p.r;
      s.synth({ type: "sine", tone: base,         duration: durSec, volume: p.a * 0.58 * g, attack: 0.006, decay: durSec * 0.46, phase: 0.00 });
      s.synth({ type: "sine", tone: base + p.dHz, duration: durSec, volume: p.a * 0.58 * g, attack: 0.006, decay: durSec * 0.46, phase: 0.37 });
    }
    // Reese growl — 4 detuned saws, saturated + lowpassed after the loop
    const rs = makeBufferSynth(reeseBuf, t, SAMPLE_RATE, noiseRng);
    for (const d of [0.988, 0.997, 1.004, 1.011]) {
      rs.synth({ type: "sawtooth", tone: f * d, duration: durSec, volume: 0.24 * g, attack: 0.006, decay: durSec * 0.62 });
    }
  }
  const bpat = BASS[STYLE] || BASS.ragga;
  for (let bar = 0; bar < nBars; bar++) {
    const barStart = bar * barSec;
    const fl = flagsAt(barStart + stepSec);
    if (!on(fl, "sub")) continue;
    const base = rootAt(barStart + 0.01) - 12;   // bass register (~A2)
    for (const n of bpat) {
      const t = humT(barStart + n.step * stepSec, n.step);   // jungle swing
      // per-note intensity if given, else strong on-beat / light off-beat
      const accent = n.acc ?? (n.step % 4 === 0 ? 1.0 : 0.7);
      bassNote(t, base + n.deg, n.len * stepSec * 0.92, accent);
    }
  }
  // danger: overdrive the Reese for growl/menace, then one slow darker
  // filter sweep over the whole line (deeper, bass-not-buzz, alive).
  softClip(reeseBuf, HC ? 1.62 : 1.35);             // hardcore: growlier
  applyWobble(reeseBuf, {
    sampleRate: SAMPLE_RATE, target: "filter",
    baseCutoffHz: HC ? 520 : STYLE === "rollers" ? 380 : STYLE === "jungleton" ? 520 : 460,
    // HC: a rhythmic quarter-note WOMP locked to tempo (165→2.75 Hz),
    // deep — "womp womp womp" under the dance-kick. else: slow sweep.
    rate: HC ? BPM / 60 : 0.13,
    depth: HC ? 0.86 : STYLE === "rollers" ? 0.30 : 0.46,
  });
  {
    const rg = HC ? 0.64 : STYLE === "rollers" ? 0.40 : 0.48;
    for (let i = 0; i < LEN; i++) stem.sub[i] += reeseBuf[i] * rg;
  }
  // ── shrill strings (held counterpoint), skrillex screech, throat bass
  // clean fast sine "blip" — bright pluck for the looping pattern.
  function sineloop(t, midi, gain, dur) {
    const f = midiToFreq(midi);
    const v = makeBufferSynth(stem.sineloop, t, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sine", tone: f,       duration: dur, volume: 0.85 * gain, attack: 0.004, decay: dur * 0.90 });
    v.synth({ type: "sine", tone: f * 2.0, duration: dur, volume: 0.18 * gain, attack: 0.004, decay: dur * 0.55 });
  }
  function shrill(t, midi, gain, dur) {
    const f = midiToFreq(midi);
    const v = makeBufferSynth(shrillBuf, t, SAMPLE_RATE, noiseRng);
    for (const d of [0.985, 0.997, 1.004, 1.016]) {
      v.synth({ type: "sawtooth", tone: f * d,     duration: dur, volume: 0.46 * gain, attack: 0.004, decay: dur * 0.7 });
      v.synth({ type: "square",   tone: f * d * 2, duration: dur, volume: 0.16 * gain, attack: 0.004, decay: dur * 0.55 });
    }
  }
  function throat(t, midi, gain, dur) {
    const f = midiToFreq(midi);
    const v = makeBufferSynth(throatBuf, t, SAMPLE_RATE, noiseRng);
    v.synth({ type: "sawtooth", tone: f,     duration: dur, volume: 0.70 * gain, attack: 0.012, decay: dur * 0.85 });
    v.synth({ type: "square",   tone: f,     duration: dur, volume: 0.30 * gain, attack: 0.012, decay: dur * 0.80 });
    v.synth({ type: "sawtooth", tone: f * 2, duration: dur, volume: 0.22 * gain, attack: 0.012, decay: dur * 0.55 });
  }
  // sidechain duck from kick onsets — gentle (sunlit, bass stays present)
  if (kickTimes.length) {
    const duckS = Math.floor(0.15 * SAMPLE_RATE);
    const floor = 0.58;
    const gain = new Float32Array(LEN).fill(1);
    for (const kt of kickTimes) {
      const k0 = Math.floor(kt * SAMPLE_RATE);
      for (let i = 0; i < duckS; i++) {
        const idx = k0 + i;
        if (idx < 0 || idx >= LEN) continue;
        const g = floor + (1 - floor) * (i / duckS);
        if (g < gain[idx]) gain[idx] = g;
      }
    }
    for (let i = 0; i < LEN; i++) stem.sub[i] *= gain[i];
  }

  // ── pad: warm golden chord per section (sunlit add9 voicing) ────────
  for (const s of score.sections) {
    if (!on(s.flags.size ? s.flags : STYLE_DEFAULT, "pad")) continue;
    const rootMidi = rootAt(s.startSec + 0.01);
    // shadowed: root + 5th + b7 + b3-up-an-octave — minor tension, the
    // golden warmth pulled into something more dangerous
    const voicing = [rootMidi, rootMidi + 7, rootMidi + 10, rootMidi + 15];
    const dur = Math.max(0.2, s.endSec - s.startSec - 0.02);
    for (const m of voicing) {
      mixEventSinePower(
        { startSec: s.startSec, midi: m, durSec: dur, gain: 0.32 },
        stem.pad, { sampleRate: SAMPLE_RATE, preset: "pad" },
      );
    }
  }

  // ── skank: reggae offbeat chord stab (the block-party bounce) ──────
  for (let bar = 0; bar < nBars; bar++) {
    const barStart = bar * barSec;
    const fl = flagsAt(barStart + stepSec);
    if (!on(fl, "skank")) continue;
    const rootMidi = rootAt(barStart + 0.01) + 12;
    const chord = [rootMidi, rootMidi + 7, rootMidi + 10]; // root-5-b7 skank
    // offbeats: the "&" of every beat (steps 2, 6, 10, 14)
    const offs = [6, 14];                  // just 2 offbeat skanks/bar — sparse
    for (const st of offs) {
      const t = humT(barStart + st * stepSec, st);
      for (const m of chord) {
        mixEventSinePower(
          { startSec: t, midi: m, durSec: stepSec * 1.3, gain: 0.17 },
          stem.skank, { sampleRate: SAMPLE_RATE, preset: "stab" },
        );
      }
    }
  }

  // ── jungle SFX: dub sirens, gunshots, high sine-bell dingiez ───────
  // ragga-jungle furniture, all AC synthesis (no samples). gated by
  // section flags: siren / guns / dings / airhorn. timing humanized.
  function dubSiren(t, dur, baseHz, topHz, lfoHz, gain) {
    const i0 = Math.floor(t * SAMPLE_RATE);
    const n = Math.floor(dur * SAMPLE_RATE);
    if (n <= 0) return;
    let ph = 0;
    for (let i = 0; i < n; i++) {
      const idx = i0 + i;
      if (idx < 0 || idx >= LEN) break;
      const x = i / n;
      const slow = baseHz + (topHz - baseHz) * (0.5 - 0.5 * Math.cos(2 * Math.PI * x));
      const vib = 1 + 0.05 * Math.sin(2 * Math.PI * lfoHz * (i / SAMPLE_RATE));
      ph += (2 * Math.PI * slow * vib) / SAMPLE_RATE;
      const env = Math.min(1, x * 10) * Math.min(1, (1 - x) * 7);
      const sq = Math.sin(ph) >= 0 ? 1 : -1;            // ragga edge (mostly sine = less buzz)
      stem.siren[idx] += (0.82 * Math.sin(ph) + 0.18 * sq) * env * gain;
    }
  }
  function gunshot(t, gain) {                           // "bow!" — crack + low boom
    const s = makeBufferSynth(stem.gun, t, SAMPLE_RATE, noiseRng);
    s.synth({ type: "noise", tone: 3600, duration: 0.012, volume: 1.0 * gain, attack: 0.0003, decay: 0.011 });
    const i0 = Math.floor(t * SAMPLE_RATE), n = Math.floor(0.14 * SAMPLE_RATE);
    let ph = 0;
    for (let i = 0; i < n; i++) {
      const idx = i0 + i;
      if (idx < 0 || idx >= LEN) break;
      const x = i / n;
      const f = 240 * Math.pow(42 / 240, x);            // pitch dives 240 → 42 Hz
      ph += (2 * Math.PI * f) / SAMPLE_RATE;
      stem.gun[idx] += Math.sin(ph) * Math.exp(-5.5 * x) * 1.15 * gain;
    }
    const s2 = makeBufferSynth(stem.gun, t + 0.006, SAMPLE_RATE, noiseRng);
    s2.synth({ type: "noise", tone: 900, duration: 0.10, volume: 0.45 * gain, attack: 0.001, decay: 0.098 });
  }
  function salute(t, gain) {                            // "bow — bow — bow"
    let p = t;
    for (let k = 0; k < 3; k++) { gunshot(p + (hrng() - 0.5) * 0.012, gain); p += 0.155; }
  }
  const DING_SET = [0, 3, 7, 10, 12, 15];               // pentatonic-ish off the root
  function ding(t, midi, gain) {                        // super high-pitched dingiez
    const f = midiToFreq(midi);
    const s = makeBufferSynth(stem.ding, t, SAMPLE_RATE, noiseRng);
    for (const [r, a, d] of [[1, 1.0, 1.6], [2.01, 0.42, 0.9], [3.0, 0.14, 0.5], [4.8, 0.04, 0.3]]) {
      s.synth({ type: "sine", tone: f * r, duration: d, volume: a * 0.5 * gain, attack: 0.002, decay: d * 0.95 });
    }
  }
  function airhorn(t, gain) {
    for (const det of [0, 7, 12]) {
      const s = makeBufferSynth(stem.siren, t, SAMPLE_RATE, noiseRng);
      s.synth({ type: "square", tone: 220 * Math.pow(2, det / 12), duration: 0.55,
                volume: 0.18 * gain, attack: 0.01, decay: 0.5 });
    }
  }
  // car "vroom vroom" — her E430. saw+square engine with a rev pitch
  // envelope + grit, two revs. lands in its own stem.
  function vroom(t, gain, revs = 2) {
    let p = t;
    for (let r = 0; r < revs; r++) {
      const i0 = Math.floor(p * SAMPLE_RATE), dur = 0.42 + hrng() * 0.10;
      const n = Math.floor(dur * SAMPLE_RATE);
      let ph1 = 0, ph2 = 0;
      for (let i = 0; i < n; i++) {
        const idx = i0 + i; if (idx < 0 || idx >= LEN) break;
        const x = i / n;
        const f = 58 + 150 * Math.sin(Math.PI * Math.min(1, x * 1.25)); // rev up then ease
        ph1 += (2 * Math.PI * f) / SAMPLE_RATE;
        ph2 += (2 * Math.PI * f * 1.503) / SAMPLE_RATE;
        const saw = (ph1 / Math.PI % 2) - 1;
        const sq = Math.sin(ph2) >= 0 ? 0.5 : -0.5;
        const grit = (noiseRng() * 2 - 1) * 0.22;
        const env = Math.min(1, x * 8) * Math.min(1, (1 - x) * 4);
        let s = (saw * 0.7 + sq * 0.4 + grit) * env * gain;
        s = Math.tanh(s * 2.4) * 0.7;                 // engine drive
        stem.vroom[idx] += s;
      }
      p += dur * 0.92;
    }
  }
  // riser — filtered noise + rising sine sweeping UP into a drop.
  function riser(t, dur, gain) {
    const i0 = Math.floor(t * SAMPLE_RATE), n = Math.floor(dur * SAMPLE_RATE);
    let ph = 0, lp = 0;
    for (let i = 0; i < n; i++) {
      const idx = i0 + i; if (idx < 0 || idx >= LEN) break;
      const x = i / n;
      const f = 180 * Math.pow(2200 / 180, x);        // sweep up
      ph += (2 * Math.PI * f) / SAMPLE_RATE;
      const nz = (noiseRng() * 2 - 1);
      const a = 1 - Math.exp(-2 * Math.PI * (300 + 6000 * x) / SAMPLE_RATE);
      lp += a * (nz - lp);
      const env = x * x;                               // accelerate in
      stem.impact[idx] += (Math.sin(ph) * 0.5 + lp * 0.9) * env * gain;
    }
  }
  // impact — the DROP. sub boom (pitch-drop) + a bright noise smack.
  function impact(t, gain) {
    const i0 = Math.floor(t * SAMPLE_RATE), n = Math.floor(0.55 * SAMPLE_RATE);
    let ph = 0;
    for (let i = 0; i < n; i++) {
      const idx = i0 + i; if (idx < 0 || idx >= LEN) break;
      const x = i / n;
      const f = 150 * Math.pow(34 / 150, Math.min(1, x * 2.2)); // 150→34 Hz
      ph += (2 * Math.PI * f) / SAMPLE_RATE;
      stem.impact[idx] += Math.sin(ph) * Math.exp(-3.4 * x) * 1.25 * gain;
    }
    const s = makeBufferSynth(stem.impact, t, SAMPLE_RATE, noiseRng);
    s.synth({ type: "noise", tone: 5200, duration: 0.05, volume: 0.55 * gain, attack: 0.0004, decay: 0.05 });
  }
  function churchBell(t, midi, gain) {                  // big tolling bell
    const f = midiToFreq(midi);
    // classic bell partials — the minor-third "tierce" (~1.19) is what
    // makes the ear hear *church bell*. long inharmonic decays.
    const P = [
      [0.5, 0.55, 6.5], [1.0, 1.00, 5.5], [1.19, 0.70, 4.2],
      [1.5, 0.52, 3.6], [2.0, 0.50, 3.0], [2.55, 0.26, 2.0],
      [3.0, 0.18, 1.6], [4.2, 0.10, 1.0],
    ];
    const s = makeBufferSynth(stem.bell, t, SAMPLE_RATE, noiseRng);
    for (const [r, a, d] of P) {
      s.synth({ type: "sine", tone: f * r, duration: d, volume: a * 0.42 * gain,
                attack: 0.003, decay: d * 0.97 });
    }
    // clapper strike — short metallic ting
    s.synth({ type: "noise", tone: 5200, duration: 0.02, volume: 0.16 * gain,
              attack: 0.0005, decay: 0.019 });
  }
  // ── kitten meows: pitched/length-varied slices of the AC-native zoo
  // cat sample (one sample → a chorus of kittens, no external sourcing).
  // gated by the `meows` flag — ragga jungle loves a sampled ad-lib.
  let catBuf = null;
  try {
    const cb = readFileSync(`${REPO}/fedac/native/samples/zoo/cat.raw`);
    catBuf = new Float32Array(cb.buffer, cb.byteOffset, Math.floor(cb.byteLength / 4));
  } catch { catBuf = null; }
  // semi = pitch (negative = low/long, positive = high). opts.semiEnd
  // glides the pitch across the note (real meow "mrrow" contour, never
  // a repeated identical hit). opts.stretch > 1 artificially lengthens
  // (slower read — mild artifact, fine for an FX). durMax caps length.
  function meow(t, semi, gain, durMax, opts = {}) {
    if (!catBuf || catBuf.length === 0) return;
    const stretch = opts.stretch || 1;
    const r0 = Math.pow(2, semi / 12) / stretch;
    const semiEnd = opts.semiEnd == null ? semi : opts.semiEnd;
    const r1 = Math.pow(2, semiEnd / 12) / stretch;
    const i0 = Math.floor(t * SAMPLE_RATE);
    const maxOut = Math.floor(durMax * SAMPLE_RATE);
    const att = Math.max(1, Math.floor(0.010 * SAMPLE_RATE));
    let src = 0;
    let n = 0;
    // first find true length (src exhausts the sample or hits durMax)
    for (let i = 0; i < maxOut; i++) {
      const u = i / maxOut;
      src += r0 + (r1 - r0) * u;
      if (src + 1 >= catBuf.length) { n = i; break; }
      n = i + 1;
    }
    if (n < 4) return;
    const rel = Math.max(1, Math.floor(n * 0.30));
    src = 0;
    for (let i = 0; i < n; i++) {
      const dst = i0 + i;
      if (dst < 0 || dst >= LEN) break;
      const s0 = Math.floor(src), s1 = s0 + 1;
      if (s1 >= catBuf.length) break;
      const fr = src - s0;
      const v = catBuf[s0] * (1 - fr) + catBuf[s1] * fr;
      let env = 1;
      if (i < att) env = i / att;
      else if (i > n - rel) env = Math.max(0, (n - i) / rel);
      stem.meow[dst] += v * env * gain;
      const u = i / maxOut;
      src += r0 + (r1 - r0) * u;
    }
  }
  // marimba / pitched long-woodblock — mallet sine stack (fundamental +
  // bar overtone + octave) over a warm wood body and a tiny tock. a
  // rolling pentatonic figure off the chord root.
  function marimba(t, midi, gain, dur) {
    const fM = midiToFreq(midi);
    const m = makeBufferSynth(stem.marimba, t, SAMPLE_RATE, noiseRng);
    m.synth({ type: "sine",     tone: fM,        duration: dur,        volume: 1.00 * gain, attack: 0.002,  decay: dur * 0.95 });
    m.synth({ type: "sine",     tone: fM * 3.95, duration: dur * 0.5,  volume: 0.30 * gain, attack: 0.001,  decay: dur * 0.45 });
    m.synth({ type: "sine",     tone: fM * 2,    duration: dur * 0.6,  volume: 0.17 * gain, attack: 0.002,  decay: dur * 0.55 });
    m.synth({ type: "triangle", tone: fM * 0.5,  duration: dur,        volume: 0.22 * gain, attack: 0.004,  decay: dur * 0.90 });
    m.synth({ type: "noise",    tone: 2600,      duration: 0.012,      volume: 0.10 * gain, attack: 0.0004, decay: 0.011 });
  }
  for (const s of score.sections) {
    const fl = s.flags.size ? s.flags : STYLE_DEFAULT;
    const len = s.endSec - s.startSec;
    if (on(fl, "siren")) {
      dubSiren(s.startSec + 0.05, 1.2, 460, 1400, 5, HC ? 0.17 : 0.13);  // rising whoop
      if (len > 12 && (HC || hrng() < 0.5))
        dubSiren(s.startSec + len * 0.6, 1.2, 700, 360, 6, HC ? 0.13 : 0.09); // falling wail
      if (HC) {                                                          // more sirens — peppered through
        const sbars = Math.max(1, Math.round(len / barSec));
        for (let b = 2; b < sbars; b += 3) {
          const up = hrng() < 0.5;
          dubSiren(humT(s.startSec + b * barSec + 8 * stepSec, 8), 0.9 + hrng() * 0.5,
                   up ? 520 : 1300, up ? 1500 : 420, 5 + hrng() * 3, 0.10 + hrng() * 0.05);
        }
      }
    }
    // car "vroom vroom" — revs into the drops (intro arrival removed —
    // the opening vroom read as weird; track now opens on the bed itself).
    if (HC) {
      const nextS = score.sections[score.sections.indexOf(s) + 1];
      if (s.name === "drop 1" || s.name === "drop 2") {
        vroom(s.startSec + 0.02, 0.40, 2);                               // rev on the drop
        if (len > 16) vroom(s.startSec + len * 0.5, 0.30, 1);
      }
      // DROP DRAMA: riser through the last ~1.6s of the section that
      // leads into a drop, then a big impact boom on the drop downbeat.
      if (nextS && (nextS.name === "drop 1" || nextS.name === "drop 2")) {
        riser(Math.max(s.startSec, s.endSec - 1.7), 1.7, 0.34);
        impact(nextS.startSec + 0.002, 0.95);
      }
    }
    if (fl.has("airhorn") && hrng() < 0.5) airhorn(s.startSec + 0.02, 0.9);
    if (fl.has("guns")) salute(s.startSec + 0.04, 0.85);                 // one salute at the drop
    if (fl.has("dings")) {
      const baseHi = rootAt(s.startSec + 0.01) + 24;    // two octaves up — high + glassy
      const bars = Math.max(1, Math.round(len / barSec));
      for (let b = 0; b < bars; b++) {
        if (hrng() < 0.80) continue;                    // very sparse sprinkle
        const nHits = 1;
        for (let h = 0; h < nHits; h++) {
          const st = [3, 7, 10, 14][Math.floor(hrng() * 4)];
          const t = humT(s.startSec + b * barSec + st * stepSec, st);
          const deg = DING_SET[Math.floor(hrng() * DING_SET.length)] + (hrng() < 0.3 ? 12 : 0);
          ding(t, baseHi + deg, 0.55 + hrng() * 0.4);
        }
      }
    }
    if (fl.has("bell")) {
      // BIG church bell ON THE DONK — the half-time backbeat (step 8).
      // low + huge; a toll opens the section, then one every 4 bars.
      const bm = rootAt(s.startSec + 0.01) - 12;
      churchBell(s.startSec + 0.03, bm, 0.95);
      const bbars = Math.max(1, Math.round(len / barSec));
      for (let b = 4; b < bbars; b += 8) {
        churchBell(humT(s.startSec + b * barSec + 8 * stepSec, 8), bm, 0.66);
      }
    }
    if (fl.has("meows")) {
      // a varied kitten vocabulary — squeaks, mid mews, rising "mrrow?",
      // long low drawls, deep stretched yowls. picked at random so it
      // never reads as one repeated wail.
      const mewKind = () => {
        const r = hrng();
        if (r < 0.30) return { s: 13 + hrng() * 8, d: 0.16 + hrng() * 0.12 };                                    // high squeak
        if (r < 0.58) return { s: 4 + hrng() * 7, d: 0.32 + hrng() * 0.22, e: () => 2 - hrng() * 4 };             // mid mew, slight glide
        if (r < 0.74) return { s: 3 + hrng() * 4, d: 0.42 + hrng() * 0.22, e: 9 + hrng() * 5 };                   // rising "mrrow?"
        if (r < 0.90) return { s: -5 + hrng() * 4, d: 0.85 + hrng() * 0.55, e: -9 - hrng() * 4, st: 1.2 + hrng() * 0.6 }; // long low drawl
        return { s: -9 + hrng() * 3, d: 1.1 + hrng() * 0.6, st: 1.4 + hrng() * 0.9 };                            // deep stretched yowl
      };
      const fire = (t, k, g) => meow(t, k.s, g, k.d,
        { semiEnd: typeof k.e === "function" ? k.e() : k.e, stretch: k.st });
      const mbars = Math.max(1, Math.round(len / barSec));
      // WAY more kitty meows when HC — a near-constant kitten chatter.
      const mewSlots = HC ? [2, 3, 4, 6, 7, 9, 10, 12, 13, 14] : [3, 4, 7, 10, 12, 14];
      for (let b = 0; b < mbars; b++) {
        const mewN = HC ? (3 + Math.floor(hrng() * 3))                 // 3–5 / bar
                        : (hrng() < 0.88 ? (hrng() < 0.35 ? 2 : 1) : 0);
        for (let q = 0; q < mewN; q++) {
          const st = mewSlots[Math.floor(hrng() * mewSlots.length)];
          fire(humT(s.startSec + b * barSec + st * stepSec, st), mewKind(), 0.55 + hrng() * 0.30);
        }
        // phrase-end kitten chorus — bigger + more often when HC
        if (b % (HC ? 4 : 8) === (HC ? 3 : 7)) {
          const k = (HC ? 7 : 4) + Math.floor(hrng() * 4);
          for (let j = 0; j < k; j++) {
            fire(s.startSec + b * barSec + (12 + j * 0.7) * stepSec + (hrng() - 0.5) * 0.04,
                 mewKind(), 0.45 + hrng() * 0.35);
          }
        }
      }
    }
    if (fl.has("marimba")) {
      const PENT = [0, 3, 5, 7, 10, 12];
      const baseM = rootAt(s.startSec + 0.01) + 12;
      const mb = Math.max(1, Math.round(len / barSec));
      const figure = [
        { st: 0, d: 0 }, { st: 3, d: 2 }, { st: 6, d: 4 },
        { st: 8, d: 1 }, { st: 11, d: 3 }, { st: 14, d: 5 },
      ];
      for (let b = 0; b < mb; b++) {
        for (const fnote of figure) {
          if (hrng() < 0.25) continue;                 // gaps so it grooves
          const deg = PENT[(fnote.d + b) % PENT.length];
          marimba(humT(s.startSec + b * barSec + fnote.st * stepSec, fnote.st),
                  baseM + deg + (hrng() < 0.2 ? 12 : 0),
                  0.50 + hrng() * 0.30, stepSec * 2.2);
        }
      }
    }
    if (fl.has("sineloop")) {
      // fast-changing sines in little looping patterns — a 4-note
      // pentatonic cell that mutates every bar, 16ths (32nds every 4th
      // bar). bright, high, hypnotic.
      const PENT = [0, 3, 5, 7, 10, 12, 15];
      const baseSL = rootAt(s.startSec + 0.01) + 24;
      const sb = Math.max(1, Math.round(len / barSec));
      for (let b = 0; b < sb; b++) {
        const o = (b * 2) % PENT.length;
        const cell = [PENT[o], PENT[(o + 2) % PENT.length],
                      PENT[(o + 1) % PENT.length], PENT[(o + 3) % PENT.length]];
        const steps = (b % 4 === 3) ? 32 : 16;       // double-time every 4th bar
        for (let k = 0; k < steps; k++) {
          const stepT = s.startSec + b * barSec + (k / steps) * barSec;
          const deg = cell[k % cell.length]
                    + ((k % (cell.length * 2) >= cell.length) ? 12 : 0);
          sineloop(humT(stepT, k), baseSL + deg,
                   0.30 + (k % 4 === 0 ? 0.12 : 0), (barSec / steps) * 0.95);
        }
      }
    }
    if (fl.has("shrill")) {
      // skrillex screech stabs — half-time screams + offbeat rips, high
      const baseSh = rootAt(s.startSec + 0.01) + 24;
      const shb = Math.max(1, Math.round(len / barSec));
      for (let b = 0; b < shb; b++) {
        shrill(humT(s.startSec + b * barSec, 0), baseSh, 0.9, stepSec * 6);
        if (hrng() < 0.6) shrill(humT(s.startSec + b * barSec + 8 * stepSec, 8), baseSh + 5, 0.8, stepSec * 4);
        if (hrng() < 0.4) shrill(humT(s.startSec + b * barSec + 14 * stepSec, 14), baseSh + 12, 0.7, stepSec * 2);
      }
    }
    if (fl.has("throat")) {
      // throat / formant bass — sustained, follows the bass root
      const baseT = rootAt(s.startSec + 0.01) - 12;
      const tb = Math.max(1, Math.round(len / barSec));
      for (let b = 0; b < tb; b++) {
        throat(s.startSec + b * barSec, baseT, 0.9, barSec * 0.5);
        throat(s.startSec + b * barSec + 8 * stepSec, baseT, 0.8, barSec * 0.5);
      }
    }
  }
  // shrill: skrillex screech — fast wobble bandpass + bitcrush + hard clip
  applyWobble(shrillBuf, { sampleRate: SAMPLE_RATE, target: "filter", baseCutoffHz: 2600, rate: 7.5, depth: 0.85 });
  applyBitcrush(shrillBuf, { sampleRate: SAMPLE_RATE, bits: 7, downsample: 2, mix: 0.5 });
  softClip(shrillBuf, 2.2);
  for (let i = 0; i < LEN; i++) stem.shrill[i] += shrillBuf[i] * 0.46;
  // throat: vowel-ish formant movement (slow LFO bandpass) + light grit
  applyWobble(throatBuf, { sampleRate: SAMPLE_RATE, target: "filter", baseCutoffHz: 820, rate: 3.2, depth: 0.7 });
  softClip(throatBuf, 1.5);
  for (let i = 0; i < LEN; i++) stem.throat[i] += throatBuf[i] * 0.60;
  // wobbly + flangey: the church bell swims — filter-LFO wobble + a
  // slow deep flange across the whole bell stem.
  applyWobble(stem.bell, {
    sampleRate: SAMPLE_RATE, target: "filter",
    baseCutoffHz: 2400, rate: 1.7, depth: 0.62,
  });
  applyFlange(stem.bell, {
    sampleRate: SAMPLE_RATE, rate: 0.22, depthMs: 6, baseDelayMs: 6,
    feedback: 0.62, mix: 0.5,
  });

  // ── scratch / reverse "play": a section flagged `scratch` rewinds
  // its last half-measure (break + dub sub) so it whooshes backward,
  // then the next section drops back in. gives the track a bit of play.
  function reverseRegion(buf, t0, t1) {
    let a = Math.max(0, Math.floor(t0 * SAMPLE_RATE));
    let b = Math.min(LEN, Math.floor(t1 * SAMPLE_RATE));
    if (b - a < 8) return;
    for (let lo = a, hi = b - 1; lo < hi; lo++, hi--) {
      const tmp = buf[lo]; buf[lo] = buf[hi]; buf[hi] = tmp;
    }
    const fade = Math.min(Math.floor(0.004 * SAMPLE_RATE), Math.floor((b - a) / 4));
    for (let k = 0; k < fade; k++) {
      const g = k / fade;
      buf[a + k] *= g;
      buf[b - 1 - k] *= g;
    }
  }
  if (!SOLO) {
    const halfBar = barSec / 2;
    for (const sec of score.sections) {
      const sfl = sec.flags.size ? sec.flags : STYLE_DEFAULT;
      if (!sfl.has("scratch")) continue;
      const t1 = sec.endSec, t0 = t1 - halfBar;
      reverseRegion(stem.break, t0, t1);
      reverseRegion(stem.sub,   t0, t1);   // dub swoosh under the rewind
    }
  }

  // ── fía vocal: clean+bright+compress → rubberband slow-stretch so
  // her ~32s take is held/elongated ACROSS the whole track → WORLD
  // scale-snap autotune (octave up) for a LEAD + two in-key HARMONY
  // layers (a fifth up, a fourth down). same family as the other pop
  // vocal sections (WORLD + rubberband + stacked harmonies).
  if (def.vocal && !SOLO) {
    const rsv = (p) => p && (p.startsWith("~") ? resolve(homedir(), p.slice(2)) : p);
    // ALL verses, played SEQUENTIALLY (no layering) — concatenated with
    // a short breath gap between each, before the (gentle) stretch.
    const verses = [def.vocal, def.vocal2, def.vocal3]
      .map(rsv).filter((p) => p && existsSync(p));
    const vpath = verses[0];
    if (vpath) {
      const clean = `/tmp/_solafiya_voc_clean.wav`;
      const stre  = `/tmp/_solafiya_voc_stretch.wav`;
      const CHAIN =
        "highpass=f=110," +                              // kill plosive thump/pops
        "adeclick,adeclip," +                            // detect+remove spikes/clicks
        `afftdn=nr=${HC ? 24 : 18},` +                   // HC: more air/hiss out
        "agate=threshold=0.012:ratio=3:attack=6:release=220:knee=3," + // breath between phrases
        "acompressor=threshold=-24dB:ratio=5:attack=4:release=140:makeup=8," +
        "acompressor=threshold=-14dB:ratio=4:attack=2:release=80:makeup=2," +
        `deesser=i=${HC ? 0.6 : 0.45},` +                // tame sibilance / hiss-air
        "equalizer=f=220:t=q:w=1.0:g=1.5," +             // chest body — anchor her in the mids
        "equalizer=f=2400:t=q:w=1.5:g=1.8," +            // presence (was 2700+2.6) — less airy
        `treble=g=${HC ? -7 : -5}:f=8000,` +             // HC: kill the breathy fizz harder
        "alimiter=limit=0.96";
      let ffArgs;
      if (verses.length > 1) {
        const ins = verses.flatMap((p) => ["-i", p]);
        // pad every verse but the last with a 0.40s breath, then concat.
        const labs = [];
        let pre = "";
        for (let i = 0; i < verses.length; i++) {
          if (i < verses.length - 1) { pre += `[${i}:a]apad=pad_dur=0.40[a${i}];`; labs.push(`[a${i}]`); }
          else labs.push(`[${i}:a]`);
        }
        ffArgs = ["-hide_banner", "-loglevel", "error", "-y", ...ins,
          "-filter_complex", `${pre}${labs.join("")}concat=n=${verses.length}:v=0:a=1,${CHAIN}[o]`,
          "-map", "[o]", "-ar", "48000", "-ac", "1", clean];
      } else {
        ffArgs = ["-hide_banner", "-loglevel", "error", "-y", "-i", vpath,
          "-af", CHAIN, "-ar", "48000", "-ac", "1", clean];
      }
      spawnSync("ffmpeg", ffArgs, { stdio: "ignore" });
      console.log(`  fía vocal · ${verses.length} verse(s) concatenated sequentially`);

      // slow-stretch (pitch-preserving) to span the track from bar 0
      let dur = verses.length * 30;                  // fallback only
      const pr = spawnSync("ffprobe", ["-v", "error", "-show_entries",
        "format=duration", "-of", "default=nw=1:nk=1", clean], { encoding: "utf8" });
      if (pr.status === 0) dur = parseFloat(pr.stdout.trim()) || dur;
      const targetSec = score.totalSec * 0.97;
      const ratio = Math.max(1, targetSec / dur);
      let base = clean;
      const rb = spawnSync("rubberband", ["-t", ratio.toFixed(4), "--fine", clean, stre]);
      if (rb.status === 0 && existsSync(stre)) base = stre;
      else console.warn("  ⚠ rubberband stretch failed — vocal stays short");

      const shiftLead = def.vocalShift ?? 12;       // it sang too low → octave up
      const LAYERS = [
        { name: "lead",    shift: shiftLead,      gain: 0.95, dest: "vocal"  },
        { name: "harm-5",  shift: shiftLead + 7,  gain: 0.26, dest: "vocalH" },
        { name: "harm-lo", shift: shiftLead - 5,  gain: 0.30, dest: "vocalH" },
        { name: "harm-hi", shift: shiftLead + 12, gain: 0.08, dest: "vocalH" }, // octave — pulled WAY back, this was the "airy" layer
      ];
      const startIdx = Math.floor((def.vocalBar || 0) * barSec * SAMPLE_RATE);
      let placed = 0;
      let leadV = null, leadPk = 1;                    // captured for ad-libs
      for (const L of LAYERS) {
        const outW = `/tmp/_solafiya_voc_${L.name}.wav`;
        const at = spawnSync(`${REPO}/pop/.venv/bin/python`, [
          `${REPO}/pop/bin/autotune.py`, base, outW,    // shared across pop lanes
          "--key", def.vocalKey || "A",
          "--scale", def.vocalScale || "minorpent",
          "--mode", def.vocalTuneMode || "note",        // Melodyne-style note-aware
          "--strength", String(def.vocalTuneStrength ?? 0.9),
          "--preserve", String(def.vocalPreserve ?? 0.6), // keep her vibrato/scoop
          "--shift", String(L.shift),
        ], { encoding: "utf8" });
        const src = (at.status === 0 && existsSync(outW)) ? outW : base;
        if (at.status !== 0) console.warn(`  ⚠ autotune ${L.name}: ${(at.stderr || "").trim().slice(0, 120)}`);
        // WORLD resynthesis + the big stretch lose ~8-12 dB RMS — make
        // it up: heavy comp + dynaudnorm + limiter so the vocal is dense
        // and genuinely present, not thin/distant.
        const srcMU = `${src}.mu.wav`;
        const mu = spawnSync("ffmpeg", [
          "-hide_banner", "-loglevel", "error", "-y", "-i", src,
          "-af", "acompressor=threshold=-30dB:ratio=6:attack=3:release=130:makeup=12," +
                 "dynaudnorm=f=180:g=9:p=0.92:m=18," +
                 "equalizer=f=220:t=q:w=1.0:g=1.2," +           // chest body — sit her in the bed
                 "equalizer=f=2400:t=q:w=1.6:g=1.2," +          // clarity bell (no exciter air)
                 "treble=g=-4.5:f=9500," +                      // keep the top smooth + less fizz
                 "asoftclip=type=tanh:threshold=0.72," +        // gentle round-off (smoother)
                 "alimiter=limit=0.97",
          "-ar", String(SAMPLE_RATE), "-ac", "1", srcMU,
        ], { stdio: "ignore" });
        const dec = (mu.status === 0 && existsSync(srcMU)) ? srcMU : src;
        const r = spawnSync("ffmpeg", [
          "-hide_banner", "-loglevel", "error", "-i", dec,
          "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1", "-",
        ], { encoding: "buffer", maxBuffer: 1024 * 1024 * 256 });
        if (r.status !== 0 || !r.stdout?.length) continue;
        const v = new Float32Array(r.stdout.buffer, r.stdout.byteOffset, Math.floor(r.stdout.byteLength / 4));
        let pk = 1e-6;
        for (let i = 0; i < v.length; i++) { const a = Math.abs(v[i]); if (a > pk) pk = a; }
        const g = (0.95 / pk) * L.gain;                // per-layer normalize × taper
        const dest = stem[L.dest] || stem.vocal;       // lead→vocal, harms→vocalH
        for (let i = 0; i < v.length; i++) {
          const d = startIdx + i;
          if (d < 0 || d >= LEN) continue;
          dest[d] += v[i] * g;
        }
        if (L.name === "lead") { leadV = v; leadPk = pk; }  // for ad-libs
        placed++;
      }

      // ── up-front vocal AD-LIBS — her saying "other words at other
      // pitches" + turntable SCRATCHES, scattered through the drops.
      if (HC && leadV && leadV.length > SAMPLE_RATE) {
        const arng = makeRng(`jungle:${slug}:adlib`);
        const norm = 0.95 / leadPk;
        // find word-ish windows: spans of energy in the lead take.
        const win = Math.floor(0.025 * SAMPLE_RATE);
        const words = [];
        { let on = false, s0 = 0;
          for (let i = 0; i < leadV.length; i += win) {
            let e = 0; for (let j = i; j < Math.min(leadV.length, i + win); j++) e = Math.max(e, Math.abs(leadV[j]));
            const loud = e > leadPk * 0.16;
            if (loud && !on) { on = true; s0 = i; }
            else if (!on || loud) { /* keep */ }
            if ((!loud || i + win >= leadV.length) && on) {
              on = false;
              if (i - s0 > 0.10 * SAMPLE_RATE) words.push([s0, Math.min(leadV.length, i)]);
            }
          }
        }
        const placeSlice = (atSec, src0, src1, semi, gain, rev = false) => {
          const ratio = Math.pow(2, semi / 12);          // re-pitch (resample)
          const srcLen = src1 - src0;
          const outN = Math.floor(srcLen / ratio);
          const d0 = Math.floor(atSec * SAMPLE_RATE);
          for (let k = 0; k < outN; k++) {
            const di = d0 + k; if (di < 0 || di >= LEN) break;
            const sp = rev ? (srcLen - 1 - k * ratio) : (k * ratio);
            const si = src0 + Math.floor(sp);
            if (si < 0 || si >= leadV.length) continue;
            const x = k / outN;
            const env = Math.min(1, x * 14) * Math.min(1, (1 - x) * 8);
            stem.vocalAd[di] += leadV[si] * norm * env * gain;
          }
        };
        const PITCHES = [3, 5, 7, 12, -5, -3, 8, 10];
        for (const sec of score.sections) {
          const drop = sec.name === "drop 1" || sec.name === "drop 2";
          if (!drop && sec.name !== "out") continue;
          const bars = Math.max(1, Math.round((sec.endSec - sec.startSec) / barSec));
          for (let b = 0; b < bars; b++) {
            if (arng() < (drop ? 0.45 : 0.7)) continue;        // sparse, tasteful
            const w = words[Math.floor(arng() * words.length)];
            if (!w) continue;
            const semi = PITCHES[Math.floor(arng() * PITCHES.length)];
            const st = [4, 8, 11, 14][Math.floor(arng() * 4)];
            placeSlice(sec.startSec + b * barSec + st * stepSec,
                       w[0], Math.min(w[1], w[0] + Math.floor((0.20 + arng() * 0.28) * SAMPLE_RATE)),
                       semi, 0.85 + arng() * 0.3);
          }
          // a turntable SCRATCH on a short slice at phrase ends
          if (drop) {
            const w = words[Math.floor(arng() * words.length)] || [0, SAMPLE_RATE / 2];
            const sl = Math.min(w[1] - w[0], Math.floor(0.13 * SAMPLE_RATE));
            let at = sec.endSec - 0.9;
            for (let r = 0; r < 5; r++) {
              placeSlice(at, w[0], w[0] + sl, (r % 2 ? 4 : -2), 0.8, r % 2 === 1);
              at += 0.10 + 0.02 * (r % 2);
            }
          }
        }
      }

      // ── section-aware vocal shaping ──────────────────────────────────
      // (a) more harmony in the drops, sparse harmony in intro/break;
      // (b) a slow filter-sweep through the dub-break ("whoom"); (c) a
      // tempo-synced stutter/echo in the drops → swung repeated words.
      {
        const hEnv = sectionEnv({                       // harmony emphasis
          "intro": 0.42, "roll-in": 0.70, "drop 1": 1.22,
          "dub-break": 0.55, "drop 2": 1.32, "out": 1.05,
        }, 0.9);
        for (let i = 0; i < LEN; i++) stem.vocalH[i] *= hEnv[i];

        const inSec = (name, i) => {
          for (const s of score.sections)
            if (s.name === name && i >= s.startSec * SAMPLE_RATE && i < s.endSec * SAMPLE_RATE) return true;
          return false;
        };
        // (b) filter sweep — one-pole LP whose cutoff breathes 380↔5200 Hz
        // across the dub-break, on lead + harmonies (dreamy underwater).
        const db = score.sections.find((s) => s.name === "dub-break");
        if (db) {
          const i0 = Math.floor(db.startSec * SAMPLE_RATE);
          const i1 = Math.min(LEN, Math.floor(db.endSec * SAMPLE_RATE));
          const span = Math.max(1, i1 - i0);
          for (const buf of [stem.vocal, stem.vocalH]) {
            let y = 0;
            for (let i = i0; i < i1; i++) {
              const ph = (i - i0) / span;
              const fc = 380 + (5200 - 380) * (0.5 - 0.5 * Math.cos(2 * Math.PI * ph * 1.5));
              const a = 1 - Math.exp(-2 * Math.PI * fc / SAMPLE_RATE);
              y += a * (buf[i] - y);
              buf[i] = y;
            }
          }
        }
        // (c) tempo-synced stutter/echo in the drops — a dotted-ish
        // (3×16th) feedback tap → her words repeat with the jungle swing.
        const dly = Math.max(1, Math.round(stepSec * 3 * SAMPLE_RATE));
        for (let i = dly; i < LEN; i++) {
          if (!(inSec("drop 1", i) || inSec("drop 2", i))) continue;
          stem.vocalH[i] += 0.40 * stem.vocalH[i - dly];
          stem.vocal[i]  += 0.16 * stem.vocal[i - dly];
        }
      }
      // single source of truth for the lyric train: the exact stretch
      // + start offset the words were placed at (Whisper times are in
      // the original-recording timebase → track time = start + t×stretch)
      // single source of truth for the karaoke train: the stretch +
      // start offset (clean timebase × stretch + startSec = track time).
      // ONE voice now — all verses sequential, no duet.
      writeFileSync(`${LANE}/out/${slug}-vocal-map.json`, JSON.stringify({
        stretch: ratio, startSec: startIdx / SAMPLE_RATE,
      }));
      console.log(`  fía vocal · ×${ratio.toFixed(2)} · ${placed} layers @ bar ${def.vocalBar} · ${verses.length} verses sequential`);

      // ── #2: voice-pitch shadow — analyse her stretched take, then a
      // soft synth tracks her melody an OCTAVE DOWN (+a fifth) so the
      // mix is pitched to match/counter her and the vocal glues in.
      if (def.vocalShadow !== false) {
        const vpJson = "/tmp/_solafiya_voicepitch.json";
        const vp = spawnSync(`${REPO}/pop/.venv/bin/python`, [
          `${LANE}/bin/voicepitch.py`, base, vpJson,
          "--key", def.vocalKey || "A", "--scale", def.vocalScale || "minorpent",
        ], { encoding: "utf8" });
        if (vp.status === 0 && existsSync(vpJson)) {
          console.log(`  ${(vp.stdout || "").trim()}`);
          const prof = JSON.parse(readFileSync(vpJson, "utf8"));
          for (const seg of prof.segments || []) {
            const tt = startIdx / SAMPLE_RATE + seg.t;
            const dur = Math.max(0.12, seg.dur * 0.95);
            const fLo = midiToFreq(seg.midi - 12);          // an octave below her
            const s = makeBufferSynth(stem.vshadow, tt, SAMPLE_RATE, noiseRng);
            s.synth({ type: "sine",     tone: fLo,       duration: dur, volume: 0.55, attack: 0.03, decay: dur * 0.90 });
            s.synth({ type: "sine",     tone: fLo * 1.5, duration: dur, volume: 0.18, attack: 0.04, decay: dur * 0.80 }); // fifth — counter
            s.synth({ type: "triangle", tone: fLo * 2,   duration: dur, volume: 0.10, attack: 0.05, decay: dur * 0.70 });
          }
        } else {
          console.warn(`  ⚠ voicepitch failed: ${(vp.stderr || "").trim().slice(0, 140)}`);
        }
      }
    }
  }

  // ── vocal duck: pull the (very busy) bed down wherever fía sings so
  // she sits genuinely up-front — a sidechain, not just a fader. sub +
  // kick keep their weight; the mid/high clutter ducks ~6 dB.
  if (def.vocal && !SOLO) {
    let vmax = 1e-6;
    for (let i = 0; i < LEN; i++) { const a = Math.abs(stem.vocal[i]); if (a > vmax) vmax = a; }
    if (vmax > 1e-4) {
      const aA = Math.exp(-1 / (SAMPLE_RATE * 0.008));   // 8 ms attack
      const aR = Math.exp(-1 / (SAMPLE_RATE * 0.18));    // 180 ms release
      const duck = new Float32Array(LEN);
      let env = 0;
      for (let i = 0; i < LEN; i++) {
        const a = Math.abs(stem.vocal[i]) / vmax;
        env = a > env ? aA * env + (1 - aA) * a : aR * env + (1 - aR) * a;
        duck[i] = 1 - 0.30 * Math.min(1, env * 1.4);     // ~3 dB — carve a real pocket so she sits INSIDE the bed
      }
      for (const k of ["break", "stab", "skank", "sineloop", "marimba",
                        "ding", "bell", "siren", "shrill", "throat", "pad"]) {
        const b = stem[k];
        for (let i = 0; i < LEN; i++) b[i] *= duck[i];
      }
    }
  }

  // ── mix: place mono stems into stereo ──────────────────────────────
  const outL = new Float32Array(LEN);
  const outR = new Float32Array(LEN);
  // gain, pan (-1..1), haasMs (micro-delay on one side for width)
  const BUS = {
    break: { g: 0.86, pan: 0.0,   haas: 0  },
    stab:  { g: 0.58, pan: 0.0,   haas: 0  },
    sub:   { g: HC ? 1.15 : 1.10, pan: 0.0, haas: 0 }, // pulled back — was too heavy
    pad:   { g: 0.46, pan: 0.0,   haas: 11 },
    skank: { g: HC ? 0.40 : 0.32, pan: 0.18,  haas: 6  },
    siren: { g: 0.36, pan: -0.20, haas: 0  },
    gun:   { g: 0.88, pan: 0.05,  haas: 0  },  // punchy + dry
    // HC: the candy/sparkle top comes FORWARD and bright (and drier in
    // the reverb send below) — that's the "cute" half of hardcore-cute.
    ding:  { g: HC ? 0.27 : 0.13, pan: 0.34,  haas: HC ? 8 : 16 },
    bell:  { g: HC ? 0.62 : 0.46, pan: -0.06, haas: HC ? 7 : 12 },
    meow:  { g: HC ? 0.68 : 0.52, pan: 0.12,  haas: HC ? 6 : 10 },
    marimba: { g: HC ? 0.32 : 0.15, pan: -0.14, haas: HC ? 5 : 8 },
    sineloop: { g: HC ? 0.30 : 0.13, pan: 0.16, haas: HC ? 7 : 12 },
    shrill:  { g: 0.15, pan: -0.05, haas: 0  }, // screech tucked under — supports, not fights
    throat:  { g: 0.28, pan: 0.0,   haas: 0  }, // formant bass — centred, dry; pulled back (was a low-mid hog)
    vocal:   { g: 0.92, pan: 0.0,   haas: 0  }, // fía lead — pulled DOWN; she was riding too hot
    vocalH:  { g: 0.28, pan: 0.0,   haas: 9  }, // her harmonies — tighter, less wash, sit IN the room
    vshadow: { g: 0.42, pan: 0.0,   haas: 0  }, // pitch-shadow of her, octave down
    vroom:   { g: HC ? 0.50 : 0.0, pan: -0.10, haas: 9 },  // car revs
    vocalAd: { g: HC ? 0.92 : 0.0, pan: 0.08, haas: 7 },   // UP-FRONT vocal ad-libs/scratch
    impact:  { g: HC ? 0.95 : 0.0, pan: 0.0,  haas: 0 },   // drop boom / riser drama
    vocalDuet: { g: 0.86, pan: 0.24, haas: 12 },           // Voice B — duet, opposite side
  };
  for (const [name, buf] of Object.entries(stem)) {
    if (SOLO && SOLO !== name) continue;
    const b = BUS[name];
    const lG = Math.cos((b.pan + 1) * Math.PI / 4) * Math.SQRT2 * b.g;
    const rG = Math.sin((b.pan + 1) * Math.PI / 4) * Math.SQRT2 * b.g;
    const hs = Math.floor((b.haas / 1000) * SAMPLE_RATE);
    for (let i = 0; i < LEN; i++) {
      const v = buf[i];
      if (v === 0) continue;
      outL[i] += v * lG;
      const ri = i + hs;
      if (ri < LEN) outR[ri] += v * rG;
    }
  }

  // ── depth: stereo reverb send. drums / skank / siren / dings get the
  // room; the sub + gun stay dry so the low end + cracks punch through.
  if (!SOLO) {
    const send = new Float32Array(LEN);
    // her voice sits deeper now → more reverb overall, and EXTRA wash in
    // the dub-break + the out (some parts more reverb than others).
    // less 'airy' when HC: pull the long wash back everywhere, tame the
    // dub-break bloom, keep her present rather than floating in verb.
    const vRev = sectionEnv(HC ? {
      "intro": 0.7, "roll-in": 0.85, "drop 1": 0.9,
      "dub-break": 1.25, "drop 2": 0.95, "out": 1.20,
    } : {
      "intro": 0.85, "roll-in": 1.0, "drop 1": 1.05,
      "dub-break": 1.95, "drop 2": 1.10, "out": 1.55,
    }, 1.0);
    // INTIMACY: the track OPENS bone-dry — the first ~6s is the closest,
    // most intimate space (almost no room), then it blooms open by ~9s.
    // the dub-break also pulls way in close ("sometimes super intimate").
    const intimDub = sectionEnv({ "dub-break": 0.28 }, 1.0);
    for (let i = 0; i < LEN; i++) {
      // drums stay DRY + tight + up-close; the harmonies (pad, skank,
      // marimba, bell) get the space — HC dries the candy top right out.
      send[i] = stem.break[i] * 0.06 + stem.skank[i] * (HC ? 0.55 : 1.05)
              + stem.siren[i] * 0.60 + stem.ding[i]  * (HC ? 0.40 : 1.00)
              + stem.pad[i]   * (HC ? 0.70 : 1.05) + stem.gun[i] * 0.10
              + stem.bell[i]  * (HC ? 0.32 : 0.70) + stem.meow[i] * (HC ? 0.22 : 0.42)
              + stem.marimba[i] * (HC ? 0.34 : 0.85) + stem.sineloop[i] * (HC ? 0.20 : 0.45)
              + stem.shrill[i]  * 0.18
              + stem.vocal[i]  * (HC ? 0.22 : 0.32) * vRev[i]   // less wash on her direct — sit IN the room
              + stem.vocalH[i] * (HC ? 0.30 : 0.48) * vRev[i]
              + stem.vshadow[i] * 0.10
              + stem.vroom[i] * 0.10 + stem.vocalAd[i] * 0.16 + stem.impact[i] * 0.05;
      const ts = i / SAMPLE_RATE;
      const introDry = ts < 6 ? 0.04 : ts < 9 ? 0.04 + 0.96 * ((ts - 6) / 3) : 1;
      send[i] *= introDry * intimDub[i];
    }
    const { L, R } = stereoReverb(send, SAMPLE_RATE, STYLE === "rollers" ? 0.82 : 0.74);
    const w = HC ? 0.10 : 0.20;                  // less wash = less airy (pulled HARD: 0.18→0.10 HC)
    for (let i = 0; i < LEN; i++) { outL[i] += L[i] * w; outR[i] += R[i] * w; }
  }

  // ── DROP DRAMA: a hard cut to near-silence in the last beat before
  // each drop, then everything SLAMS back (the impact boom lands on the
  // downbeat). classic "… —— DROP." HC only.
  if (HC && !SOLO) {
    const cut = 60 / BPM;                              // ~one beat of suck-out
    for (const ds of score.sections) {
      if (ds.name !== "drop 1" && ds.name !== "drop 2") continue;
      const d0 = ds.startSec;
      const c0 = Math.max(0, Math.floor((d0 - cut) * SAMPLE_RATE));
      const c1 = Math.floor(d0 * SAMPLE_RATE);
      for (let i = c0; i < c1 && i < LEN; i++) {
        const x = (i - c0) / Math.max(1, c1 - c0);     // 1 → ~0.04 into the drop
        const g = 1 - 0.96 * (x * x);
        outL[i] *= g; outR[i] *= g;
      }
    }
  }

  // ── master tone: deeper + warmer (low-shelf lift, gentle high tame) ─
  // HC: more low-shelf for the womp weight; the high-cut targets the
  // breathy 8 kHz+ FIZZ (the 'airy' part) but stays high enough to keep
  // the candy bell/ding sparkle intact — bright but not airy.
  // master low-shelf — was pumping too much weight under EVERYTHING
  // (the "low tones everywhere" feedback). HC dropped 0.42→0.20.
  lowShelfBoost(outL, SAMPLE_RATE, 120, HC ? 0.20 : 0.18);
  lowShelfBoost(outR, SAMPLE_RATE, 120, HC ? 0.20 : 0.18);
  // HC: shave a bit more high-cut — the new acdsp master will re-add air
  // at 11 k via eq:air, but only after the breathy 8–10 k fizz is gone.
  highCut(outL, SAMPLE_RATE, HC ? 7800 : 6400, HC ? 0.52 : 0.34);
  highCut(outR, SAMPLE_RATE, HC ? 7800 : 6400, HC ? 0.52 : 0.34);

  // ── per-lane downsampled |peak| buffers + struct.laneAudio for the
  // moving-string score-train (preview-spin.mjs). headerless f32 mono;
  // overwrites the early struct.json with the full one (sections + lanes).
  if (!SOLO) {
    const LANE_SR = 4000, dec = Math.round(SAMPLE_RATE / LANE_SR);
    const lanePaths = {};
    for (const [key, buf] of Object.entries(stem)) {
      const nn = Math.ceil(buf.length / dec);
      const ds = new Float32Array(nn);
      for (let o = 0; o < nn; o++) {
        let p = 0; const s0 = o * dec, s1 = Math.min(buf.length, s0 + dec);
        for (let s = s0; s < s1; s++) { const a = Math.abs(buf[s]); if (a > p) p = a; }
        ds[o] = p;
      }
      const rel = `out/${slug}.lane-${key}.raw`;
      writeFileSync(`${LANE}/${rel}`, Buffer.from(ds.buffer, ds.byteOffset, ds.byteLength));
      lanePaths[key] = rel;
    }
    writeFileSync(`${LANE}/out/${slug}.struct.json`, JSON.stringify({
      slug, style: STYLE, bpm: BPM, totalSec: score.totalSec,
      laneAudio: { sampleRate: LANE_SR, paths: lanePaths },
      sections: score.sections.map((s) => ({
        name: s.name, startSec: s.startSec, endSec: s.endSec, flags: [...s.flags],
      })),
    }, null, 2));
  }

  // ── peak normalize + grit softclip (loudness + danger glue) ────────
  let peak = 0;
  for (let i = 0; i < LEN; i++) {
    const a = Math.abs(outL[i]); if (a > peak) peak = a;
    const b = Math.abs(outR[i]); if (b > peak) peak = b;
  }
  if (peak > 0) {
    const norm = 0.80 / peak;       // leave real headroom for fía on top
    for (let i = 0; i < LEN; i++) { outL[i] *= norm; outR[i] *= norm; }
  }
  softClip(outL, 1.04);
  softClip(outR, 1.04);

  // ── write stereo f32 → mp3 ─────────────────────────────────────────
  mkdirSync(dirname(OUT_PATH), { recursive: true });
  const raw = Buffer.alloc(LEN * 2 * 4);
  for (let i = 0; i < LEN; i++) {
    raw.writeFloatLE(outL[i], i * 8);
    raw.writeFloatLE(outR[i], i * 8 + 4);
  }
  const rawPath = `${OUT_PATH}.f32.raw`;
  writeFileSync(rawPath, raw);
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "2", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "2",
    "-metadata", `title=${def.title}`,
    "-metadata", `artist=${def.artist || "fía"}`,
    "-metadata", "album=pixsies",
    "-metadata", "genre=jungle",
    "-metadata", `publisher=${def.label || "Aesthetic.Computer"}`,
    "-metadata", `copyright=${def.label || "Aesthetic.Computer"}`,
    OUT_PATH,
  ], { stdio: "inherit" });
  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }

  // ── optional: illustration → cover → embed, all in one pass ────────
  // `--cover` makes the full deliverable. The colored-pencil illy is
  // cached (gpt-image-2 is slow + paid) — `--force-art` regenerates it.
  // If the illy can't be made (no API key / offline) it falls back to
  // the geometric cover. The cover + ID3 tags are muxed in ONE ffmpeg
  // copy pass so there's no strip/re-tag dance.
  let covered = false;
  if (flags.cover && !SOLO) {
    const illy     = `${LANE}/out/${slug}.illy.png`;
    const coverPng = `${LANE}/out/${slug}-cover.png`;
    const SUBWORD  = { jungleton: "dembow", ragga: "ragga", rollers: "rollers" };

    if (!existsSync(illy) || flags["force-art"]) {
      console.log(`  illy · gpt-image-2 (cached after first run) …`);
      const g = spawnSync("node", [
        `${HERE}/gen-illy.mjs`, "--slug", slug,
        ...(flags["force-art"] ? ["--force"] : []),
      ], { stdio: "inherit" });
      if (g.status !== 0) console.warn(`  ⚠ illy gen failed — geometric cover fallback`);
    }

    const cg = spawnSync("node", [
      `${REPO}/pop/dance/bin/cover.mjs`,
      "--slug", slug, "--lane", "jungle",
      "--title-pos", "topleft",
      "--title", def.wordmark || slug,
      "--subtitle", `${SUBWORD[STYLE] || STYLE} latina jungle`,
      "--handle", "fía",
      ...(existsSync(illy) ? ["--illustration", illy] : []),
      "--waveform", OUT_PATH,
      "--out", coverPng,
    ], { stdio: "inherit" });
    if (cg.status !== 0) { console.error("✗ cover gen failed"); process.exit(1); }

    // mux cover art + re-assert ID3 in a single stream-copy pass
    const tagged = `${OUT_PATH}.cover.mp3`;
    const mux = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error",
      "-i", OUT_PATH, "-i", coverPng,
      "-map", "0:a", "-map", "1", "-c", "copy",
      "-id3v2_version", "3", "-disposition:v:0", "attached_pic",
      "-metadata", `title=${def.title}`,
      "-metadata", `artist=${def.artist || "fía"}`,
      "-metadata", "album=pixsies",
      "-metadata", "genre=jungle",
      "-metadata", `publisher=${def.label || "Aesthetic.Computer"}`,
      "-metadata", `copyright=${def.label || "Aesthetic.Computer"}`,
      tagged,
    ], { stdio: "inherit" });
    if (mux.status !== 0) { console.error("✗ cover mux failed"); process.exit(1); }
    renameSync(tagged, OUT_PATH);
    covered = true;
  }

  // ── desktop delivery: final mp3 (+ cover png when present) ─────────
  let dest = OUT_PATH;
  if (flags.desktop) {
    dest = resolve(homedir(), "Desktop", `${slug}.mp3`);
    copyFileSync(OUT_PATH, dest);
    const cp = `${LANE}/out/${slug}-cover.png`;
    if (covered && existsSync(cp)) {
      copyFileSync(cp, resolve(homedir(), "Desktop", `${slug}-cover.png`));
    }
  }

  console.log(
    `✓ ${slug} · ${STYLE} · ${BPM} bpm · ${nBars} bars · ` +
    `${score.totalSec.toFixed(1)}s · ${kickTimes.length} kicks` +
    `${covered ? " · cover" : ""} → ${dest}`,
  );
}

// ── dispatch ──────────────────────────────────────────────────────────
if (positional[0] === "all" || flags.all) {
  for (const slug of Object.keys(DEFAULTS)) render(slug);
} else {
  render(String(flags.slug || positional[0] || "raggasol"));
}
