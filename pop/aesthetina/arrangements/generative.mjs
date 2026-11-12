// arrangements/generative.mjs — the seeded version. The seed makes the song.
//
// One source of truth: makePlan(seed) draws everything — the form, the words,
// the chords, the rhythms, which layers play where — and writes it to
// generative.plan.json. play() renders that plan through the engine;
// voices/generative.py reads the same plan file (and runs this module with
// `node arrangements/generative.mjs` to write it if it is missing or stale).
// Change "seed" in generative.json and both halves follow.
//
// Rules:
//  - words: drawn from papers/jeffrey-lexicon (first-hand, a–z, 3–10 letters,
//    the ~800 most common), weighted by √count; each is typed through notepat's
//    key map as a melody, its letters spread E(k, n) over one or two bars.
//  - chords: three-letter lexicon words whose letters spell a major, minor or
//    diminished triad. A tonic is drawn, the vocabulary is narrowed to triads
//    inside its scale, and each section walks a four-chord progression that
//    prefers root motion by fourth/fifth.
//  - rhythms: per section, hats/clicks/phone blips from the Euclidean
//    catalogue and named timelines in papers/rhythm-platter/timelines.json.
//  - form: intro · build · peak · breakdown · build · peak · outro, lengths
//    drawn in fours and fitted to 140–160 bars; density and layers drawn per
//    section, with the country groove (boom-chick, bells, air, phone keys) as
//    layers the rules switch on.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const PAPERS = join(HERE, "..", "..", "..", "papers");
const META = JSON.parse(readFileSync(join(HERE, "generative.json"), "utf8"));
export const PLAN_PATH = join(HERE, "generative.plan.json");

// notepat's map (disks/notepat.mjs NOTE_TO_KEYBOARD_KEY) as MIDI
const KEY = { z: 58, x: 59, c: 60, v: 61, d: 62, s: 63, e: 64, f: 65, w: 66, g: 67, r: 68, a: 69, q: 70, b: 71,
  h: 72, t: 73, i: 74, y: 75, j: 76, k: 77, u: 78, l: 79, o: 80, m: 81, p: 82, n: 83 };

function mulberry(seed) {
  return () => {
    seed |= 0; seed = (seed + 0x6d2b79f5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
// maximally even onsets (Bresenham form of Bjorklund), starting on 0
const euclid = (k, n) => [...Array(n).keys()].filter((i) => ((i * k) % n) < k);
const boxOf = (onsets, n) => [...Array(n).keys()].map((i) => (onsets.includes(i) ? "x" : ".")).join("");

// ── the plan ───────────────────────────────────────────────────────────────
export function makePlan(seed = META.seed) {
  const R = mulberry(seed);
  const pick = (arr) => arr[Math.floor(R() * arr.length)];
  const weighted = (arr, w) => {
    const tot = arr.reduce((s, x) => s + w(x), 0);
    let r = R() * tot;
    for (const x of arr) if ((r -= w(x)) <= 0) return x;
    return arr[arr.length - 1];
  };

  // words
  const lex = JSON.parse(readFileSync(join(PAPERS, "jeffrey-lexicon", "dictionary.json"), "utf8"));
  const entries = Object.entries(lex).filter(([w, v]) => /^[a-z]+$/.test(w) && v.provenance === "first-hand");
  const melodic = entries.filter(([w]) => w.length >= 4 && w.length <= 10 && new Set(w).size >= 3)
    .sort((a, b) => b[1].count - a[1].count).slice(0, 800);
  const drawn = new Set();
  const drawWord = (lo = 4, hi = 10) => {
    for (;;) {
      const [w] = weighted(melodic, ([, v]) => Math.sqrt(v.count));
      if (w.length >= lo && w.length <= hi && !drawn.has(w)) { drawn.add(w); return w; }
    }
  };
  const words = { title: drawWord(6, 10), A: drawWord(4, 7), B: drawWord(6, 10), C: drawWord(4, 8), D: drawWord(6, 10), final: drawWord(4, 6) };

  // chords: triad words
  const QUAL = { maj: [0, 4, 7], min: [0, 3, 7], dim: [0, 3, 6] };
  const triadOf = (w) => {
    const pcs = [...new Set([...w].map((c) => KEY[c] % 12))];
    if (pcs.length !== 3) return null;
    for (const r of pcs) for (const [q, iv] of Object.entries(QUAL)) {
      const set = iv.map((x) => (r + x) % 12);
      if (pcs.every((p) => set.includes(p))) return { root: r, quality: q };
    }
    return null;
  };
  const vocab = entries.filter(([w]) => w.length === 3).sort((a, b) => b[1].count - a[1].count).slice(0, 3000)
    .map(([w, v]) => ({ word: w, count: v.count, ...triadOf(w) })).filter((c) => c.quality);
  const tonic = weighted(vocab.filter((c) => c.quality !== "dim"), (c) => Math.sqrt(c.count));
  const scale = (tonic.quality === "maj" ? [0, 2, 4, 5, 7, 9, 11] : [0, 2, 3, 5, 7, 8, 10]).map((x) => (x + tonic.root) % 12);
  const inKey = vocab.filter((c) => QUAL[c.quality].every((x) => scale.includes((c.root + x) % 12)));
  const byRoot = {};
  for (const c of inKey) if (!byRoot[c.root] || byRoot[c.root].count < c.count) byRoot[c.root] = c; // one word per root
  const chordSet = Object.values(byRoot);
  const progression = (start) => {
    const out = [start];
    while (out.length < 4) {
      const prev = out[out.length - 1];
      const next = weighted(chordSet.filter((c) => c.word !== prev.word), (c) => {
        const mv = (c.root - prev.root + 12) % 12;
        return (mv === 5 || mv === 7 ? 4 : mv === 2 || mv === 10 ? 1.5 : 1) * (c.quality === "dim" ? 0.4 : 1);
      });
      out.push(next);
    }
    return out.map((c) => c.word);
  };

  // rhythms
  const tl = JSON.parse(readFileSync(join(PAPERS, "rhythm-platter", "timelines.json"), "utf8"));
  const rows = [...tl.euclidean_catalogue.rows, ...tl.named_timelines.rows];
  const of = (n, lo, hi) => rows.filter((r) => r.n === n && r.k >= lo && r.k <= hi).map((r) => ({ id: r.id, box: r.box }));
  const hat16 = [...of(16, 5, 9), ...of(8, 3, 7).map((r) => ({ id: r.id, box: r.box + r.box }))];
  const clicks12 = of(12, 5, 11);
  const blips = [...of(8, 3, 5).map((r) => ({ id: r.id, box: r.box + r.box })), ...of(16, 5, 5)];

  // form
  const KINDS = [
    ["intro", [8, 12]], ["build", [16, 20]], ["peak", [24, 28, 32]], ["breakdown", [12, 16, 20]],
    ["rise", [8, 12]], ["peak2", [28, 32, 36]], ["outro", [12, 16]],
  ];
  let lens;
  do lens = KINDS.map(([, opts]) => pick(opts));
  while (lens.reduce((a, b) => a + b) < 140 || lens.reduce((a, b) => a + b) > 160);
  const VOWELS = ["ah", "oo", "oh", "ee", "ay"];
  let start = 0;
  const sections = KINDS.map(([kind], i) => {
    const len = lens[i], d = { intro: 0.2, build: 0.55, peak: 0.85, breakdown: 0.3, rise: 0.6, peak2: 1, outro: 0.4 }[kind];
    const density = Math.min(1, Math.max(0.1, d + (R() - 0.5) * 0.2));
    const on = (p) => R() < p;
    const peak = kind === "peak" || kind === "peak2";
    const L = {
      kick: kind !== "intro" && kind !== "breakdown", hats: kind !== "intro" && kind !== "breakdown",
      ups: peak && on(0.7), clap: peak || (kind === "build" && on(0.5)),
      boom: peak || kind === "build" || (kind === "outro" && on(0.6)), strum: peak && on(0.8),
      pad: kind === "breakdown" || kind === "intro" || (peak && on(0.5)),
      air: kind === "intro" || kind === "breakdown" || kind === "outro" || kind === "rise",
      bells: on(0.4 + 0.5 * density), toll: true, dtmf: peak ? on(0.8) : on(0.35),
      clicks: peak && on(0.6), canon2: peak && on(0.75), canon4: kind === "peak2",
      counter: peak && on(0.8), second: kind === "peak2", toots: kind !== "intro",
      choir: peak || kind === "breakdown" || (kind === "build" && on(0.5)), ladder: kind === "rise",
    };
    const prog = progression(i === 0 || peak ? chordSet.find((c) => c.word === tonic.word) ?? tonic : pick(chordSet));
    const lead = { intro: words.title, build: words.A, peak: words.title, breakdown: words.C, rise: words.A, peak2: words.title, outro: words.final }[kind];
    const sec = { kind, start, len, density: +density.toFixed(2), layers: L, prog, lead,
      counter: words.B, second: words.D, hats: pick(hat16), clicks: pick(clicks12), blips: pick(blips),
      vowel: pick(VOWELS), chordBars: peak || kind === "build" ? (on(0.5) ? 1 : 2) : 2 };
    start += len;
    return sec;
  });

  // word rhythms: letters spread maximally evenly over one bar (≤5 letters) or two
  const rhythm = (w) => {
    const n = w.length <= 5 ? 16 : 32;
    const onsets = euclid(w.length, n);
    return { n, steps: onsets, box: boxOf(onsets, n) };
  };
  const rhythms = Object.fromEntries([...new Set(Object.values(words))].map((w) => [w, rhythm(w)]));

  return { seed, bpm: META.bpm, swing: META.swing, bars: start, tonic: tonic.word, key: { root: tonic.root, quality: tonic.quality },
    chordSet: chordSet.map((c) => c.word), words, rhythms, sections };
}

export function writePlan(seed) {
  const plan = makePlan(seed);
  writeFileSync(PLAN_PATH, JSON.stringify(plan, null, 2) + "\n");
  return plan;
}

// ── rendering the plan ─────────────────────────────────────────────────────
export function play(ctx) {
  const { BEAT, BAR, S16, BARS, at, sw, KEY_MIDI, hz, rand, B, keyDown, keyUp, spacebar, ocarina, slideVoice, typeLine,
    kick, clap, roll, strum, ladder, hatsBar, air, bell, dtmf, pat } = ctx;
  const plan = writePlan(META.seed);
  if (plan.bars !== BARS) console.warn(`generative: plan has ${plan.bars} bars but generative.json says ${BARS}`);

  const tonesOf = (word) => [...new Set([...word].map((c) => KEY_MIDI[c] % 12))];
  const rootOf = (word) => {
    const pcs = tonesOf(word);
    for (const r of pcs) for (const iv of [[0, 4, 7], [0, 3, 7], [0, 3, 6]])
      if (pcs.every((p) => iv.map((x) => (r + x) % 12).includes(p))) return r;
    return pcs[0];
  };
  const bassOf = (word) => { const r = rootOf(word); return [40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51].find((m) => m % 12 === r); };
  const chordFor = (sec, bar) => sec.prog[Math.floor((bar - sec.start) / sec.chordBars) % 4];

  // boom-chick on any chord word
  function boomChick(bar, word, vel = 1) {
    const r = bassOf(word), fifth = r + 7 <= 52 ? r + 7 : r - 5;
    [r, fifth].forEach((m, k) => ocarina(B.bass, at(bar, k * 8), hz(m), BEAT * 0.8, 0.36 * vel,
      { h2: 0.5, h3: 0.25, vib: 0, scoop: -1.5, send: 0.03, attack: 0.004, release: 0.05, decay: 0.2 }));
    for (const beat of [1, 3]) {
      const t = at(bar, beat * 4);
      [...word].forEach((ch, i) => {
        keyDown(B.keys, t + i * 0.012, ch, 0.3 * vel, { send: 0.05 });
        ocarina(B.pad, t + i * 0.012, hz(KEY_MIDI[ch] - 12), BEAT * 0.28, 0.035 * vel,
          { pan: (i - 1) * 0.5, send: 0.1, attack: 0.004, release: 0.03, vib: 0, scoop: -0.8, decay: 0.06 });
      });
    }
  }
  // held chord-word pad
  function pad(bar, bars, word, amp = 0.045) {
    [...word].forEach((ch, i) => ocarina(B.pad, at(bar) + i * 0.05, hz(KEY_MIDI[ch] - 12), BAR * bars - 0.1, amp * (1 - i * 0.15),
      { pan: (i - 1) * 0.45, send: 0.5, attack: 0.6 + i * 0.15, release: 0.8, vib: 0.06 }));
  }
  // a word typed as a melody on its Euclidean rhythm
  function typeWord(word, bar, o = {}) {
    const r = plan.rhythms[word];
    typeLine(word, at(bar), r.steps, { until: r.n - 2, ...o });
  }
  // prolation: the word stretched factor × slower, octaves down, breath only
  function canon(word, bar, factor, octave, amp, bus) {
    const r = plan.rhythms[word];
    const notes = r.steps.map((s, i) => ({ t: at(bar) + sw(s * factor) * S16 + 0.008, m: KEY_MIDI[word[i]] + octave * 12 }));
    slideVoice(bus, notes, at(bar) + (r.n - 2) * factor * S16, { amp, send: 0.2, glide: 0.11 * Math.sqrt(factor), vib: 0.05, decay: 0.5 * factor, floor: 0.3 });
  }
  // intro: the title typed as switches, more letters breathing each pass
  function bootWord(word, bar, nBreath) {
    const r = plan.rhythms[word];
    r.steps.forEach((s, i) => {
      const tk = at(bar, s), next = at(bar, r.steps[i + 1] ?? r.n - 2);
      keyDown(B.keys, tk, word[i], 0.5); keyUp(B.keys, next - 0.02, word[i], 0.35);
      if (i < nBreath) ocarina(B.lead, tk + 0.008, hz(KEY_MIDI[word[i]]), next - tk - 0.06, 0.14, { send: 0.45, decay: 0.3, scoop: -1.3 });
    });
  }

  for (const sec of plan.sections) {
    const L = sec.layers, end = sec.start + sec.len;
    const hats = pat(sec.hats.box), clicks = pat(sec.clicks.box), blips = pat(sec.blips.box);
    const peak = sec.kind === "peak" || sec.kind === "peak2";
    const leadSpan = plan.rhythms[sec.lead].n / 16; // bars per statement

    if (L.air) air(at(sec.start), sec.len * BAR, tonesOf(sec.prog[0]).map((pc) => 57 + ((pc - 9 + 12) % 12)), sec.kind === "rise" ? 0.08 : 0.05);
    if (L.toll) bell(at(sec.start), bassOf(sec.prog[0]), "church", "bronze", 0.35, 0, 6);

    for (let bar = sec.start; bar < end; bar++) {
      const rel = bar - sec.start, word = chordFor(sec, bar), last = bar === end - 1;
      // cursor tick
      if (sec.kind !== "breakdown") for (let b4 = 0; b4 < 4; b4++)
        keyDown(B.keys, at(bar, b4 * 4 + 2), "cursor", sec.kind === "intro" ? 0.3 : 0.1, { pan: -0.35 });

      if (sec.kind === "intro") {
        if (rel % leadSpan === 0) bootWord(sec.lead, bar, Math.round((rel / Math.max(1, sec.len - leadSpan)) * sec.lead.length));
        if (last) roll(at(bar, 8), 8, 0.5);
        continue;
      }
      // drums
      const kickOn = L.kick && !(sec.kind === "rise" && rel < sec.len / 2);
      if (kickOn) {
        const half = sec.kind === "outro";
        for (let s = 0; s < 16; s += half ? 8 : 4) kick(at(bar, s));
        if (peak && rel % 8 === 7) kick(at(bar, 14), 0.6);
      }
      if (L.hats) hatsBar(bar, hats, L.ups, sec.kind === "rise" ? 0.5 + 0.5 * rel / sec.len : 0.6 + 0.4 * sec.density);
      if (L.clap && !last) { clap(at(bar, 4)); clap(at(bar, 12)); }
      if (L.clicks) clicks.forEach((x, j) => x && keyDown(B.keys, at(bar) + j * BAR / clicks.length, "click" + (j % 3), 0.42, { pan: 0.5, send: 0.1 }));
      if (L.dtmf) {
        const semis = bassOf(word) + 24 - 79;
        blips.forEach((x, s16) => x && s16 % 4 && dtmf(at(bar, s16), semis + (s16 >= 8 ? 7 : 0), 0.08, 0.06, 0.35));
      }
      if (L.boom) boomChick(bar, word, sec.kind === "outro" ? 0.7 : 1);

      // harmony
      if (L.strum && (bar - sec.start) % sec.chordBars === 0) strum(word, at(bar), BAR * sec.chordBars - 0.05, { amp: 0.05 });
      if (L.pad && rel % 4 === 0) pad(bar, Math.min(4, end - bar), word, sec.kind === "breakdown" ? 0.06 : 0.04);
      if (L.bells && peak && rel % 2 === 0) {
        const tn = [...word].map((c) => KEY_MIDI[c] + 12).sort((x, y) => x - y);
        [2, 6, 10, 14].forEach((s16, j) => bell(at(bar, s16), tn[j % tn.length], "handbell", "brass", 0.07, [-0.5, 0.5][j % 2], 2));
      }
      if (L.bells && sec.kind === "breakdown" && rel % 2 === 0) bell(at(bar), KEY_MIDI[sec.lead[(rel / 2) % sec.lead.length]] + 12, "glass", "glass", 0.12, 0.3, 4);

      // melody
      const slow = sec.kind === "breakdown";
      if (!last || sec.kind === "outro") {
        if (slow ? rel % (leadSpan * 2) === 0 : rel % leadSpan === 0) {
          if (slow) canon(sec.lead, bar, 2, 0, 0.15, B.lead);
          else typeWord(sec.lead, bar, { amp: sec.kind === "outro" ? 0.12 : 0.16, send: sec.kind === "outro" ? 0.5 : 0.3 });
        }
        const cSpan = plan.rhythms[sec.counter].n / 16;
        if (L.counter && rel >= 4 && rel % (cSpan * 2) === cSpan) typeWord(sec.counter, bar, { octave: -1, amp: 0.08, pan: 0.35, keyVel: 0.35 });
        const dSpan = plan.rhythms[sec.second].n / 16;
        if (L.second && rel >= 8 && rel % dSpan === 0) typeWord(sec.second, bar, { amp: 0.08, pan: -0.45, keyVel: 0.35 });
      }
      if (L.canon2 && rel % (leadSpan * 2) === 0 && rel + leadSpan * 2 <= sec.len) canon(sec.lead, bar, 2, -1, 0.08, B.pad);
      if (L.canon4 && rel % (leadSpan * 4) === 0 && rel + leadSpan * 4 <= sec.len) canon(sec.lead, bar, 4, -2, 0.15, B.bass);

      // rise: the tuplet ladder over its last two bars
      if (L.ladder && rel === sec.len - 2) ladder(at(bar));
      if (last && sec.kind !== "outro" && !L.ladder) roll(at(bar, 12), 4, 0.45);
    }
  }
  // the end: one spacebar alone
  spacebar(B.keys, at(BARS - 1, 8), 0.8);
  keyUp(B.keys, at(BARS - 1, 8) + 0.12, "space", 0.6);
}

// `node arrangements/generative.mjs [seed]` writes the plan (voices/generative.py calls this)
if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  const plan = writePlan(process.argv[2] ? Number(process.argv[2]) : META.seed);
  // keep generative.json's bar count in step with the plan (render.mjs sizes the buffer from it)
  writeFileSync(join(HERE, "generative.json"), JSON.stringify({ ...META, seed: plan.seed, bars: plan.bars }, null, 1).replace(/\n\s*/g, " ") + "\n");
  console.log(`seed ${plan.seed} · ${plan.bars} bars · tonic "${plan.tonic}" · words ${Object.values(plan.words).join(" ")}`);
  for (const s of plan.sections) console.log(`  ${String(s.start).padStart(3)} ${s.kind.padEnd(9)} ${String(s.len).padStart(2)} bars  ${s.lead.padEnd(10)} ${s.prog.join(" ")}  hats ${s.hats.id}`);
}
